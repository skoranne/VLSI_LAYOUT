// File    : thrust_boolean.cu
// Author  : Sandeep Koranne (C) 2026
// Purpose : Use CUDA/Thrust to develop boolean operation on GPU
#ifdef INSTRUCTIONS

Given type(XYTracker) type :: XYTracker
integer(kind=K_COORDINATE_KIND) :: X, Y integer(kind=int8) ::
polygonNumber end and ! Auxiliary data structure for the sweep-line
type :: Event integer(kind=K_COORDINATE_KIND) :: x, y1, y2
integer(kind=int8) :: lap_change ! +1 for left edge, -1 for right
edge integer :: owner ! 1 for Shape A, 2 for Shape B end type Event

call sort_event_trackers( trackers )
We have
a GPU with 128 GB unified memory; we can load these rectangles which
are type(Box) with integer(kind=K_COORDINATE_KIND) x1,y1,x2,y2 end
and generate either the Vertex Corner trackers as well as the Sweep
line event for x-coordinate and y1 and y2. Using CUDA devise a plan
to perform Boolean operations on 2 collections of boxes such as
UNION_MERGE, AND, OR2, XOR, NOT, SIZE (Minkowski erosion). I have
examples with coordinates such as (0,0) (10,10) and second box is
(5,5) to (20,20). Generate CUDA code using Thrust/CUB which can work
on 100 million such boxes. I have a RTree which I will use on the
Host side to do some partitioning.
#endif

#include <iostream>
#include <vector>
#include <cuda_runtime.h>
#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#include <thrust/sort.h>
#include <thrust/execution_policy.h>
#include <iostream>
#include <vector>
#include <cuda_runtime.h>
#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#include <thrust/sort.h>
#include <thrust/transform_reduce.h>
#include <thrust/functional.h>
#include <thrust/execution_policy.h>

#include <fstream>
#include <cstdint>
#include <cassert>
#include <stdexcept>

// Assuming the Box struct from our GPU code
using K_COORDINATE_KIND = int32_t;

struct Box {
    K_COORDINATE_KIND x1, y1, x2, y2;
    int owner; // 1 for Shape A, 2 for Shape B
    int id;    // To track the original box or write output IDs
};

// Struct to hold pairs of boxes that need their overlaps resolved
struct OverlapPair {
    int box_a_id;
    int box_b_id;
};


// High-performance binary reader
std::vector<Box> read_boxes_from_binary(const std::string& filename, int owner_id) {
    // Open file at the end (ate) to easily get the file size
    std::ifstream file(filename, std::ios::binary | std::ios::ate);
    
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file: " + filename);
    }

    // Determine file size
    std::streamsize file_size = file.tellg();
    file.seekg(0, std::ios::beg);

    // Each rectangle consists of 4 int32_t values (16 bytes total)
    size_t rect_size = 4 * sizeof(K_COORDINATE_KIND);
    
    // Safety check to ensure the file isn't corrupted or incomplete
    if (file_size % rect_size != 0) {
        throw std::runtime_error("File size is not a multiple of 16 bytes. Corrupt data?");
    }

    size_t num_rects = file_size / rect_size;
    
    // Allocate a raw buffer to read the entire file in one single I/O operation
    std::vector<K_COORDINATE_KIND> raw_data(num_rects * 4);
    
    std::cout << "Reading " << num_rects << " rectangles (" 
              << (file_size / (1024.0 * 1024.0)) << " MB) from " << filename << "...\n";

    if (!file.read(reinterpret_cast<char*>(raw_data.data()), file_size)) {
         throw std::runtime_error("Failed to read binary data.");
    }

    // Allocate the final Host vector
    std::vector<Box> boxes(num_rects);

    // Process the raw data, run assertions, and format into the Box struct
    for (size_t i = 0; i < num_rects; ++i) {
        K_COORDINATE_KIND x1 = raw_data[i * 4 + 0];
        K_COORDINATE_KIND y1 = raw_data[i * 4 + 1];
        K_COORDINATE_KIND x2 = raw_data[i * 4 + 2];
        K_COORDINATE_KIND y2 = raw_data[i * 4 + 3];

        // Ensure valid box geometry
        // Note: assertions are removed in Release builds (when NDEBUG is defined). 
        // If you need these checks to run in production, use standard if/throw statements.
        assert(x2 > x1 && "Assertion failed: x2 must be strictly greater than x1");
        assert(y2 > y1 && "Assertion failed: y2 must be strictly greater than y1");

        boxes[i] = {x1, y1, x2, y2, owner_id, static_cast<int>(i)};
    }
    
    return boxes;
}


// 1. Thrust Comparator: Sort Boxes by x1, then y1
struct SortByX1 {
    __host__ __device__
    bool operator()(const Box& a, const Box& b) const {
        if (a.x1 != b.x1) return a.x1 < b.x1;
        return a.y1 < b.y1;
    }
};

// 2. Thrust Functor: Extract the width of a Box
struct BoxWidth {
    __host__ __device__
    K_COORDINATE_KIND operator()(const Box& b) const {
        return b.x2 - b.x1;
    }
};

// 3. Fast Device-Side Binary Search (Lower Bound)
__device__ int lower_bound_x1(const Box* B, int n, K_COORDINATE_KIND val) {
    int left = 0, right = n;
    while (left < right) {
        int mid = left + (right - left) / 2;
        if (B[mid].x1 < val) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }
    return left;
}

// Fast Device-Side Binary Search (Upper Bound)
__device__ int upper_bound_x1(const Box* B, int n, K_COORDINATE_KIND val) {
    int left = 0, right = n;
    while (left < right) {
        int mid = left + (right - left) / 2;
        if (B[mid].x1 <= val) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }
    return left;
}

// Single-Layer Per-Segment Sweep Kernel
__global__ void pssl_self_intersection_kernel(
    const Box* sorted_boxes, 
    int num_boxes,
    OverlapPair* out_pairs, 
    int* out_count, 
    int max_out) 
{
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= num_boxes) return;

    Box a = sorted_boxes[i];

    // FORWARD-ONLY SWEEP: Start directly at i + 1
    for (int j = i + 1; j < num_boxes; ++j) {
        Box b = sorted_boxes[j];

        // EARLY EXIT: Because the array is sorted by x1, if b.x1 is past a.x2, 
        // NO subsequent box will ever overlap with 'a' on the X-axis.
        if (b.x1 >= a.x2) {
            break; 
        }

        // 2D Intersection test (Y-axis check, since X-axis overlap is guaranteed here)
        if (a.y1 < b.y2 && a.y2 > b.y1) {
            
            // Record the intersecting pair
            int write_idx = atomicAdd(out_count, 1);
            if (write_idx < max_out) {
                out_pairs[write_idx] = {a.id, b.id};
            }
        }
    }
}


// 4. The Per-Segment Plane Sweep Kernel (PSSL)
__global__ void per_segment_sweep_kernel(
    const Box* boxes_A, int num_A,
    const Box* boxes_B, int num_B,
    K_COORDINATE_KIND max_width_B,
    Box* out_boxes, int* out_count, int max_out) 
{
    // Each thread is assigned exactly one Box from Collection A
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx >= num_A) return;

    Box a = boxes_A[idx];

    // Calculate the theoretical X-axis boundary limits
    // The lowest possible x1 in B that could overlap A is A.x1 minus the widest box in B
    K_COORDINATE_KIND search_start_x = a.x1 - max_width_B;
    K_COORDINATE_KIND search_end_x   = a.x2;

    // Use Binary Search to find the exact array bounds to search
    int start_j = lower_bound_x1(boxes_B, num_B, search_start_x);
    int end_j   = upper_bound_x1(boxes_B, num_B, search_end_x);

    // Perform the localized "Sweep" only across candidate boxes
    for (int j = start_j; j < end_j; ++j) {
        Box b = boxes_B[j];

        // 2D Intersection test (exclusive bounds)
        if (a.x1 < b.x2 && a.x2 > b.x1 && a.y1 < b.y2 && a.y2 > b.y1) {
            
            // Calculate the overlapping area
            K_COORDINATE_KIND out_x1 = max(a.x1, b.x1);
            K_COORDINATE_KIND out_y1 = max(a.y1, b.y1);
            K_COORDINATE_KIND out_x2 = min(a.x2, b.x2);
            K_COORDINATE_KIND out_y2 = min(a.y2, b.y2);

            // Write output using atomic counters safely
            int write_idx = atomicAdd(out_count, 1);
            if (write_idx < max_out) {
                out_boxes[write_idx] = {out_x1, out_y1, out_x2, out_y2, 3, write_idx};
            }
        }
    }
}

int main(int argc, char* argv[]) {
    // 1. Prepare Host Data (Example)
    thrust::host_vector<Box> h_A = {
        {0, 0, 10, 10, 1, 0},
        {30, 30, 50, 50, 1, 1}
    };
    
    thrust::host_vector<Box> h_B = {
        {5, 5, 20, 20, 2, 0},
        {8, 8, 12, 12, 2, 1},
        {100, 100, 200, 200, 2, 2}
    };

    h_A =  read_boxes_from_binary(argv[1], 1);
    h_B =  read_boxes_from_binary(argv[2], 2);    
    
    // Move to 128GB Unified Memory / Device Space
    thrust::device_vector<Box> d_A = h_A;
    thrust::device_vector<Box> d_B = h_B;

    // 2. Sort Collection B by X-coordinate (Crucial for PSSL)
    thrust::sort(thrust::device, d_B.begin(), d_B.end(), SortByX1());

    if(true){
      // 2. Allocate Output Buffer for Overlap Pairs
      int expected_overlaps = d_B.size() * 5; // Empirical buffer size
      thrust::device_vector<OverlapPair> d_out_pairs(expected_overlaps);
      thrust::device_vector<int> d_out_count(1, 0);

      // 3. Launch the PSSL Self-Sweep
      int threadsPerBlock = 256;
      int blocksPerGrid = (d_B.size() + threadsPerBlock - 1) / threadsPerBlock;

      pssl_self_intersection_kernel<<<blocksPerGrid, threadsPerBlock>>>(
									thrust::raw_pointer_cast(d_B.data()), 
									d_B.size(),
									thrust::raw_pointer_cast(d_out_pairs.data()),
									thrust::raw_pointer_cast(d_out_count.data()),
									expected_overlaps
									);
      cudaDeviceSynchronize();

      // 4. Retrieve Results
      int overlap_count = d_out_count[0];
      thrust::host_vector<OverlapPair> h_out_pairs(d_out_pairs.begin(), d_out_pairs.begin() + overlap_count);
    
      std::cout << "Found " << overlap_count << " overlapping pairs.\n";
      #if 0
      for(int i = 0; i < overlap_count; i++) {
        std::cout << "Box " << h_out_pairs[i].box_a_id 
                  << " intersects Box " << h_out_pairs[i].box_b_id << "\n";
      }
      #endif
    }    

    
    // 3. Find the maximum width inside Collection B
    K_COORDINATE_KIND max_width_B = thrust::transform_reduce(
        thrust::device, 
        d_B.begin(), d_B.end(), 
        BoxWidth(), 
        0, 
        thrust::maximum<K_COORDINATE_KIND>()
    );

    // 4. Allocate Output Memory
    // Note: In 100M datasets, you size this based on empirical overlap expectations 
    int expected_max_outputs = d_A.size() * 10; 
    thrust::device_vector<Box> d_out_boxes(expected_max_outputs);
    thrust::device_vector<int> d_out_count(1, 0);

    // 5. Launch the Per-Segment Sweep Kernel
    int threadsPerBlock = 256;
    int blocksPerGrid = (d_A.size() + threadsPerBlock - 1) / threadsPerBlock;

    per_segment_sweep_kernel<<<blocksPerGrid, threadsPerBlock>>>(
        thrust::raw_pointer_cast(d_A.data()), d_A.size(),
        thrust::raw_pointer_cast(d_B.data()), d_B.size(),
        max_width_B,
        thrust::raw_pointer_cast(d_out_boxes.data()),
        thrust::raw_pointer_cast(d_out_count.data()),
        expected_max_outputs
    );
    cudaDeviceSynchronize();

    // 6. Print Results
    int out_count = d_out_count[0];
    
    // Safety check in case we overflowed our generous output buffer
    if (out_count > expected_max_outputs) {
        std::cerr << "Warning: Output buffer too small. Found " << out_count << " but only saved " << expected_max_outputs << ".\n";
        out_count = expected_max_outputs;
    }

    thrust::host_vector<Box> h_out_boxes(d_out_boxes.begin(), d_out_boxes.begin() + out_count);
    
    std::cout << "Found " << out_count << " intersecting regions.\n";
    for(int i = 0; i < out_count; i++) {
        Box b = h_out_boxes[i];
        std::cout << "Intersection: (" << b.x1 << "," << b.y1 
                  << ") to (" << b.x2 << "," << b.y2 << ")\n";
    }

    return 0;
}

#if 0
int main() {
    try {
        // Example usage: Read Shape A and Shape B from their respective binary files
        // std::vector<Box> host_boxes_A = read_boxes_from_binary("shape_A_100M.bin", 1);
        // std::vector<Box> host_boxes_B = read_boxes_from_binary("shape_B_100M.bin", 2);
        
        // From here, you can push them to the GPU:
        // thrust::device_vector<Box> d_boxes_A = host_boxes_A;
        // ...
        
        std::cout << "Binary reading and assertions completed successfully.\n";
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
#endif




struct Event {
    K_COORDINATE_KIND x;
    int8_t type;  // 1 for left edge, -1 for right edge
    int owner;         
    int box_idx;  // Pointer back to the original box

    // Sort by X coordinate. If X is tied, process Left edges (+1) before Right edges (-1)
    __host__ __device__
    bool operator<(const Event& other) const {
        if (x != other.x) return x < other.x;
        return type > other.type; 
    }
};

// 1. Generate Left and Right events, carrying the box_idx
__global__ void generate_events_kernel(const Box* boxes, Event* events, int num_boxes) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx < num_boxes) {
        Box b = boxes[idx];
        events[2 * idx]     = {b.x1,  1, b.owner, idx};
        events[2 * idx + 1] = {b.x2, -1, b.owner, idx};
    }
}

// Fixed size for active list per block (Matches RTree tile capacity)
#define MAX_ACTIVE_PER_TILE 1024

// 2. True 2D Sweep-Line Kernel
__global__ void boolean_intersection_sweep_kernel(
    const Event* sorted_events, 
    int num_events, 
    const Box* original_boxes,
    Box* out_boxes, 
    int* out_count) 
{
    // In a multi-block RTree implementation, each block gets its own shared memory active lists.
    // For this demonstration, we use local arrays for a single thread execution.
    if (threadIdx.x == 0 && blockIdx.x == 0) {
        
        int active_A[MAX_ACTIVE_PER_TILE];
        int active_B[MAX_ACTIVE_PER_TILE];
        int count_A = 0;
        int count_B = 0;
        
        for (int i = 0; i < num_events; ++i) {
            Event e = sorted_events[i];
            Box current_box = original_boxes[e.box_idx];
            
            if (e.type == 1) { 
                // --- LEFT EDGE: A new box is entering the sweep-line ---
                
                if (current_box.owner == 1) {
                    // It's Shape A. Intersect its Y-span with all active Shape B boxes.
                    for (int j = 0; j < count_B; ++j) {
                        Box b = original_boxes[active_B[j]];
                        
                        // Apply the True 2D Y-Axis Clipping Fix
                        K_COORDINATE_KIND out_y1 = max(current_box.y1, b.y1);
                        K_COORDINATE_KIND out_y2 = min(current_box.y2, b.y2);
                        
                        // The intersection starts at the current event X, and ends at the earliest right edge
                        K_COORDINATE_KIND out_x1 = current_box.x1; 
                        K_COORDINATE_KIND out_x2 = min(current_box.x2, b.x2);
                        
                        // If it forms a valid 2D rectangle, record the intersection
                        if (out_y1 < out_y2 && out_x1 < out_x2) {
                            int out_idx = atomicAdd(out_count, 1);
                            out_boxes[out_idx] = {out_x1, out_y1, out_x2, out_y2, 3, out_idx};
                        }
                    }
                    // Add current Shape A box to active list
                    if (count_A < MAX_ACTIVE_PER_TILE) active_A[count_A++] = e.box_idx;
                } 
                else {
                    // It's Shape B. Intersect its Y-span with all active Shape A boxes.
                    for (int j = 0; j < count_A; ++j) {
                        Box a = original_boxes[active_A[j]];
                        
                        K_COORDINATE_KIND out_y1 = max(current_box.y1, a.y1);
                        K_COORDINATE_KIND out_y2 = min(current_box.y2, a.y2);
                        K_COORDINATE_KIND out_x1 = current_box.x1; 
                        K_COORDINATE_KIND out_x2 = min(current_box.x2, a.x2);
                        
                        if (out_y1 < out_y2 && out_x1 < out_x2) {
                            int out_idx = atomicAdd(out_count, 1);
                            out_boxes[out_idx] = {out_x1, out_y1, out_x2, out_y2, 3, out_idx};
                        }
                    }
                    // Add current Shape B box to active list
                    if (count_B < MAX_ACTIVE_PER_TILE) active_B[count_B++] = e.box_idx;
                }
            } 
            else { 
                // --- RIGHT EDGE: A box is leaving the sweep-line ---
                
                // Find it in the active list and remove it via swap-and-pop
                if (current_box.owner == 1) {
                    for (int j = 0; j < count_A; ++j) {
                        if (active_A[j] == e.box_idx) {
                            active_A[j] = active_A[--count_A]; 
                            break;
                        }
                    }
                } 
                else {
                    for (int j = 0; j < count_B; ++j) {
                        if (active_B[j] == e.box_idx) {
                            active_B[j] = active_B[--count_B]; 
                            break;
                        }
                    }
                }
            }
        }
    }
}

int main2(int argc, char* argv[]) {
    // Input Test Case:
    // Box A: (0,0) to (10,10)
    // Box B: (5,5) to (20,20)
    thrust::host_vector<Box> h_boxes = {
        {0, 0, 10, 10, 1, 0},
        {5, 5, 20, 20, 2, 1}
    };

    h_boxes =  read_boxes_from_binary(argv[1], 1);
    int num_boxes = h_boxes.size();    
    // Move to Device
    thrust::device_vector<Box> d_boxes = h_boxes;
    thrust::device_vector<Event> d_events(num_boxes * 2);
    
    // 1. Generate Events
    int threadsPerBlock = 256;
    int blocksPerGrid = (num_boxes + threadsPerBlock - 1) / threadsPerBlock;
    generate_events_kernel<<<blocksPerGrid, threadsPerBlock>>>(
        thrust::raw_pointer_cast(d_boxes.data()), 
        thrust::raw_pointer_cast(d_events.data()), 
        num_boxes
    );
    cudaDeviceSynchronize();

    // 2. Sort Events
    thrust::sort(thrust::device, d_events.begin(), d_events.end());

    // 3. Execute Boolean Sweep
    thrust::device_vector<Box> d_out_boxes(num_boxes); // Assumes worst case 1:1 intersection ratio
    thrust::device_vector<int> d_out_count(1, 0);

    boolean_intersection_sweep_kernel<<<1, 1>>>(
        thrust::raw_pointer_cast(d_events.data()),
        d_events.size(),
        thrust::raw_pointer_cast(d_boxes.data()),
        thrust::raw_pointer_cast(d_out_boxes.data()),
        thrust::raw_pointer_cast(d_out_count.data())
    );
    cudaDeviceSynchronize();

    // 4. Retrieve and Print Results
    int out_count = d_out_count[0];
    thrust::host_vector<Box> h_out_boxes = d_out_boxes;
    
    std::cout << "Found " << out_count << " intersecting regions.\n";
    for(int i = 0; i < out_count; i++) {
        Box b = h_out_boxes[i];
        std::cout << "Intersection Box " << b.id << ": (" 
                  << b.x1 << "," << b.y1 << ") to (" 
                  << b.x2 << "," << b.y2 << ")\n";
    }
    // Correct Output: Intersection Box 0: (5,5) to (10,10)

    return 0;
}
