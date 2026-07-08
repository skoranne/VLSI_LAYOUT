// File    : klee.cu
// Author  : Sandeep Koranne (C) 2026
// Purpose : Klee's measure problem, conversion from Fortran code
#if 0
// Instructions I want you to devise a scheme which uses
Klee's measure algorithm and its segment tree and convert it
to Thrust/CUDA; use parallel algorithms for sorting, prefix
scan, etc. get as much CUDA GPU parallelism as possible and
then convert the purpose from calculating the overlap free
AREA to calculating OVERLAP free rectangles or even directly
polygon boundaries. I will tile it later using RTree so you
can assume that code fits on a GPU.
#endif

#include <iostream>
#include <vector>
#include <cuda_runtime.h>
#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#include <thrust/sort.h>
#include <thrust/unique.h>
#include <thrust/execution_policy.h>

using K_COORDINATE_KIND = int32_t;

struct Box {
    K_COORDINATE_KIND x1, y1, x2, y2;
};

struct Event {
    K_COORDINATE_KIND x, y1, y2;
    int lap_change; // +1 for left edge, -1 for right edge

    // Sort by X coordinate. If tied, process +1 (insert) before -1 (remove)
    __host__ __device__
    bool operator<(const Event& other) const {
        if (x != other.x) return x < other.x;
        return lap_change > other.lap_change; 
    }
};

// -------------------------------------------------------------------
// DEVICE FUNCTIONS: Segment Tree Logic
// -------------------------------------------------------------------

__device__ int binary_search_y(const K_COORDINATE_KIND* arr, int n, K_COORDINATE_KIND val) {
    int left = 0, right = n - 1;
    while (left <= right) {
        int mid = left + (right - left) / 2;
        if (arr[mid] == val) return mid;
        if (arr[mid] < val) left = mid + 1;
        else right = mid - 1;
    }
    return -1;
}

// Recursive Segment Tree Update (Matches Fortran update_tree)
__device__ void update_tree(int node, int start, int end, int l, int r, int val,
                            const K_COORDINATE_KIND* unique_y,
                            int32_t* tree_count, int64_t* tree_length) 
{
    // Out of bounds
    if (l > end || r < start) return;

    // Fully enclosed
    if (l <= start && end <= r) {
        tree_count[node] += val;
    } else {
        // Partial overlap - recurse to children
        int mid = start + (end - start) / 2;
        update_tree(2 * node, start, mid, l, r, val, unique_y, tree_count, tree_length);
        update_tree(2 * node + 1, mid + 1, end, l, r, val, unique_y, tree_count, tree_length);
    }

    // Recompute the length of covered Y intervals at this node
    if (tree_count[node] > 0) {
        // If this exact node is completely covered, its length is the raw Y distance
        tree_length[node] = unique_y[end + 1] - unique_y[start];
    } else {
        // If not covered, its length is the sum of its children
        if (start == end) {
            tree_length[node] = 0;
        } else {
            tree_length[node] = tree_length[2 * node] + tree_length[2 * node + 1];
        }
    }
}

// -------------------------------------------------------------------
// KERNEL: Sequential Sweep over the Segment Tree
// -------------------------------------------------------------------
// NOTE: For massive datasets, launch 1 block per RTree partition.
// Here, we demonstrate the logic running on a single thread block.

__global__ void sweep_area_kernel(
    const Event* events, int num_events,
    const K_COORDINATE_KIND* unique_y, int num_y,
    int32_t* tree_count, int64_t* tree_length,
    int64_t* total_area) 
{
    // A single thread handles the sequential sweep for this tile.
    if (threadIdx.x == 0 && blockIdx.x == 0) {
        int64_t area = 0;
        int64_t current_x = events[0].x;

        // Note: num_y represents the number of unique points. 
        // The segment tree operates on intervals, so the max index is num_y - 2.
        int tree_max_idx = num_y - 2; 

        for (int i = 0; i < num_events; ++i) {
            Event e = events[i];
            int64_t dx = (int64_t)e.x - current_x;

            if (dx > 0) {
                // O(1) Lookup: Root node (index 1) holds total covered Y
                int64_t covered_y = tree_length[1];
                area += (dx * covered_y);
                current_x = e.x;
            }

            int j1 = binary_search_y(unique_y, num_y, e.y1);
            int j2 = binary_search_y(unique_y, num_y, e.y2);

            // O(log N) Update
            if (j1 < j2) {
                // Segment tree is 1-indexed at the root node
                update_tree(1, 0, tree_max_idx, j1, j2 - 1, e.lap_change, 
                            unique_y, tree_count, tree_length);
            }
        }
        
        *total_area = area;
    }
}

// -------------------------------------------------------------------
// KERNELS: Parallel Event & Y-Coordinate Extraction
// -------------------------------------------------------------------

__global__ void extract_y_kernel(const Box* boxes, K_COORDINATE_KIND* y_vals, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        y_vals[2 * i] = min(boxes[i].y1, boxes[i].y2);
        y_vals[2 * i + 1] = max(boxes[i].y1, boxes[i].y2);
    }
}

__global__ void generate_events_kernel(const Box* boxes, Event* events, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        K_COORDINATE_KIND min_y = min(boxes[i].y1, boxes[i].y2);
        K_COORDINATE_KIND max_y = max(boxes[i].y1, boxes[i].y2);
        K_COORDINATE_KIND min_x = min(boxes[i].x1, boxes[i].x2);
        K_COORDINATE_KIND max_x = max(boxes[i].x1, boxes[i].x2);

        events[2 * i]     = {min_x, min_y, max_y, 1};
        events[2 * i + 1] = {max_x, min_y, max_y, -1};
    }
}

// -------------------------------------------------------------------
// HOST DRIVER: Orchestrating the workflow
// -------------------------------------------------------------------

int64_t calculate_union_area_fast_gpu(thrust::device_vector<Box>& d_boxes) {
    int n = d_boxes.size();
    if (n == 0) return 0;

    int threads = 256;
    int blocks = (n + threads - 1) / threads;

    // 1. Collect, Sort, and Compress Y coordinates in Parallel
    thrust::device_vector<K_COORDINATE_KIND> d_y_vals(2 * n);
    extract_y_kernel<<<blocks, threads>>>(
        thrust::raw_pointer_cast(d_boxes.data()), 
        thrust::raw_pointer_cast(d_y_vals.data()), n);
    
    thrust::sort(thrust::device, d_y_vals.begin(), d_y_vals.end());
    
    // std::unique equivalent in Thrust
    auto new_end = thrust::unique(thrust::device, d_y_vals.begin(), d_y_vals.end());
    int num_unique_y = thrust::distance(d_y_vals.begin(), new_end);

    // 2. Create and Sort Events in Parallel
    thrust::device_vector<Event> d_events(2 * n);
    generate_events_kernel<<<blocks, threads>>>(
        thrust::raw_pointer_cast(d_boxes.data()), 
        thrust::raw_pointer_cast(d_events.data()), n);
    
    thrust::sort(thrust::device, d_events.begin(), d_events.end());

    // 3. Initialize Segment Tree Memory
    int64_t tree_size = 4 * num_unique_y;
    thrust::device_vector<int32_t> d_tree_count(tree_size, 0);
    thrust::device_vector<int64_t> d_tree_length(tree_size, 0);

    // 4. Launch Sweep Line Kernel
    thrust::device_vector<int64_t> d_total_area(1, 0);

    sweep_area_kernel<<<1, 1>>>(
        thrust::raw_pointer_cast(d_events.data()), 2 * n,
        thrust::raw_pointer_cast(d_y_vals.data()), num_unique_y,
        thrust::raw_pointer_cast(d_tree_count.data()),
        thrust::raw_pointer_cast(d_tree_length.data()),
        thrust::raw_pointer_cast(d_total_area.data())
    );
    cudaDeviceSynchronize();

    return d_total_area[0];
}

int main() {
    // Overlapping boxes to test area coverage
    thrust::host_vector<Box> h_boxes = {
        {0, 0, 10, 10}, // Area = 100
        {5, 5, 15, 15}  // Area = 100, Overlap = 25. Total Union Area should be 175.
    };
    
    thrust::device_vector<Box> d_boxes = h_boxes;

    int64_t total_area = calculate_union_area_fast_gpu(d_boxes);

    std::cout << "Total Union Area: " << total_area << "\n";
    // Expected Output: 175

    return 0;
}
