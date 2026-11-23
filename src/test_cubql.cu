#include <iostream>
#include <vector>
#include <fstream>
#include <cstdint>
#include <cassert>
#include <stdexcept>
#include <cuda_runtime.h>

// Correct cuBQL Headers
#include <cuBQL/bvh.h>
#include <cuBQL/builder/cuda.h> 

using K_COORDINATE_KIND = int32_t;

struct Box {
    K_COORDINATE_KIND x1, y1, x2, y2;
    int owner; 
    int id;    
};

struct OverlapPair {
    int box_a_id;
    int box_b_id;
};

// =========================================================================
// 1. Binary Reader (Remains Unchanged)
// =========================================================================
std::vector<Box> read_boxes_from_binary(const std::string& filename, int owner_id) {
    std::ifstream file(filename, std::ios::binary | std::ios::ate);
    if (!file.is_open()) throw std::runtime_error("Failed to open file: " + filename);

    std::streamsize file_size = file.tellg();
    file.seekg(0, std::ios::beg);

    size_t rect_size = 4 * sizeof(K_COORDINATE_KIND);
    if (file_size % rect_size != 0) throw std::runtime_error("Corrupt data?");

    size_t num_rects = file_size / rect_size;
    std::vector<K_COORDINATE_KIND> raw_data(num_rects * 4);
    
    std::cout << "Reading " << num_rects << " rectangles from " << filename << "...\n";
    if (!file.read(reinterpret_cast<char*>(raw_data.data()), file_size)) {
         throw std::runtime_error("Failed to read binary data.");
    }

    std::vector<Box> boxes(num_rects);
    for (size_t i = 0; i < num_rects; ++i) {
        K_COORDINATE_KIND x1 = raw_data[i * 4 + 0];
        K_COORDINATE_KIND y1 = raw_data[i * 4 + 1];
        K_COORDINATE_KIND x2 = raw_data[i * 4 + 2];
        K_COORDINATE_KIND y2 = raw_data[i * 4 + 3];

        assert(x2 > x1 && y2 > y1);
        boxes[i] = {x1, y1, x2, y2, owner_id, static_cast<int>(i)};
    }
    return boxes;
}

// =========================================================================
// 2. Data Preparation Kernel (int32 -> float3 cuBQL Box)
// =========================================================================
__global__ void convert_to_cubql_boxes(const Box* boxes, cuBQL::box_t<float, 3>* bvh_boxes, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        bvh_boxes[i].lower.x = static_cast<float>(boxes[i].x1);
        bvh_boxes[i].lower.y = static_cast<float>(boxes[i].y1);
        bvh_boxes[i].lower.z = 0.0f;
        
        bvh_boxes[i].upper.x = static_cast<float>(boxes[i].x2);
        bvh_boxes[i].upper.y = static_cast<float>(boxes[i].y2);
        bvh_boxes[i].upper.z = 0.0f;
    }
}

// Device helper: Check if two cuBQL float boxes overlap
__device__ inline bool boxes_overlap(const cuBQL::box_t<float, 3>& a, const cuBQL::box_t<float, 3>& b) {
    return (a.lower.x <= b.upper.x && a.upper.x >= b.lower.x &&
            a.lower.y <= b.upper.y && a.upper.y >= b.lower.y);
}

// =========================================================================
// 3. Manual cuBQL Stack Traversal Kernel
// =========================================================================
__global__ void bvh_self_check_kernel(
    cuBQL::BinaryBVH<float, 3> bvh, 
    const Box* original_boxes, 
    int num_boxes,
    OverlapPair* out_pairs, 
    int* out_count, 
    int max_out) 
{
    int myID = blockIdx.x * blockDim.x + threadIdx.x;
    if (myID >= num_boxes) return;

    Box myBox = original_boxes[myID];

    // Formulate the query box in float3
    cuBQL::box_t<float, 3> queryBox;
    queryBox.lower = {static_cast<float>(myBox.x1), static_cast<float>(myBox.y1), 0.0f};
    queryBox.upper = {static_cast<float>(myBox.x2), static_cast<float>(myBox.y2), 0.0f};

    // Allocate a local stack for BVH traversal
    int stack[64];
    int stackPtr = 0;
    
    // Push root node (index 0)
    stack[stackPtr++] = 0;

    while (stackPtr > 0) {
        int nodeIdx = stack[--stackPtr];
        auto node = bvh.nodes[nodeIdx];

        // 1. Broad Phase Float Check: Does the query box overlap this BVH node?
        if (boxes_overlap(queryBox, node.bounds)) {
            
            if (node.count > 0) {
                // LEAF NODE: Iterate over the primitive IDs in this leaf
                for (int i = 0; i < node.count; ++i) {
                    int primID = bvh.primIDs[node.offset + i];
                    
                    // Prevent self-intersection and double-counting (A->B, B->A)
                    if (primID > myID) {
                        Box otherBox = original_boxes[primID];
                        
                        // 2. Exact Integer Narrow Phase
                        if (myBox.x1 < otherBox.x2 && myBox.x2 > otherBox.x1 &&
                            myBox.y1 < otherBox.y2 && myBox.y2 > otherBox.y1) {
                            
                            int write_idx = atomicAdd(out_count, 1);
                            if (write_idx < max_out) {
                                out_pairs[write_idx] = {myBox.id, otherBox.id};
                            }
                        }
                    }
                }
            } else {
                // INTERNAL NODE: Push both children to the stack
                // Note: GPU stack safety check to prevent overflow
                if (stackPtr + 2 <= 64) {
                    stack[stackPtr++] = node.offset;     // Left child
                    stack[stackPtr++] = node.offset + 1; // Right child
                }
            }
        }
    }
}

// =========================================================================
// Main Orchestration
// =========================================================================
int main() {
    try {
        std::vector<Box> h_boxes = read_boxes_from_binary("shape_A_100M.bin", 1);
        int num_boxes = h_boxes.size();

        Box* d_boxes = nullptr;
        cuBQL::box_t<float, 3>* d_cubql_boxes = nullptr;
        
        cudaMalloc(&d_boxes, num_boxes * sizeof(Box));
        cudaMalloc(&d_cubql_boxes, num_boxes * sizeof(cuBQL::box_t<float, 3>));
        cudaMemcpy(d_boxes, h_boxes.data(), num_boxes * sizeof(Box), cudaMemcpyHostToDevice);

        int threads = 256;
        int blocks = (num_boxes + threads - 1) / threads;
        convert_to_cubql_boxes<<<blocks, threads>>>(d_boxes, d_cubql_boxes, num_boxes);
        cudaDeviceSynchronize();

        // 4. FIX: Use the actual cuBQL GPU Builder
        std::cout << "Building cuBQL BVH on GPU...\n";
        cuBQL::BinaryBVH<float, 3> bvh;
        
        // gpuBuilder handles memory allocation and tree construction internally
        cuBQL::gpuBuilder(bvh, d_cubql_boxes, num_boxes);
        cudaDeviceSynchronize();

        int expected_overlaps = num_boxes * 5; 
        OverlapPair* d_out_pairs;
        int* d_out_count;
        cudaMalloc(&d_out_pairs, expected_overlaps * sizeof(OverlapPair));
        cudaMalloc(&d_out_count, sizeof(int));
        cudaMemset(d_out_count, 0, sizeof(int));

        std::cout << "Querying BVH for Self-Intersections...\n";
        bvh_self_check_kernel<<<blocks, threads>>>(
            bvh, 
            d_boxes, 
            num_boxes, 
            d_out_pairs, 
            d_out_count, 
            expected_overlaps
        );
        cudaDeviceSynchronize();

        int h_out_count = 0;
        cudaMemcpy(&h_out_count, d_out_count, sizeof(int), cudaMemcpyDeviceToHost);
        
        std::cout << "Found " << h_out_count << " overlapping pairs.\n";
        
        // Cleanup
        cudaFree(d_boxes);
        cudaFree(d_cubql_boxes);
        cudaFree(d_out_pairs);
        cudaFree(d_out_count);
        cuBQL::freeMemory(bvh); // Free the internal BVH arrays

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
    }

    return 0;
}
