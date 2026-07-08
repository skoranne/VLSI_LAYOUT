#include <iostream>
#include <vector>
#include <cuda_runtime.h>
#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#include <thrust/sort.h>
#include <thrust/unique.h>

using K_COORDINATE_KIND = int32_t;

struct Box {
    K_COORDINATE_KIND x1, y1, x2, y2;
};

struct Event {
    K_COORDINATE_KIND x, y1, y2;
    int lap_change; 

    __host__ __device__
    bool operator<(const Event& other) const {
        if (x != other.x) return x < other.x;
        return lap_change > other.lap_change; // Process insertions (+1) before removals (-1)
    }
};

// -------------------------------------------------------------------
// DEVICE FUNCTIONS: Segment Tree & Binary Search
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

// Parallel-safe Segment Tree Update using Atomics
__device__ void update_tree_atomic(int node, int start, int end, int l, int r, int val, int32_t* tree_count) {
    if (l > end || r < start) return;

    if (l <= start && end <= r) {
        atomicAdd(&tree_count[node], val);
        return;
    }

    int mid = start + (end - start) / 2;
    update_tree_atomic(2 * node, start, mid, l, r, val, tree_count);
    update_tree_atomic(2 * node + 1, mid + 1, end, l, r, val, tree_count);
}

// Point Query: Find the coverage of a specific elementary Y-interval (leaf)
__device__ int get_leaf_coverage(int leaf_idx, int max_leaf, const int32_t* tree_count) {
    int node = 1;
    int start = 0;
    int end = max_leaf;
    int coverage = 0;

    // Traverse from root to leaf, accumulating coverage counts
    while (start <= end) {
        coverage += tree_count[node];
        if (start == end) break;
        
        int mid = start + (end - start) / 2;
        if (leaf_idx <= mid) {
            node = 2 * node;
            end = mid;
        } else {
            node = 2 * node + 1;
            start = mid + 1;
        }
    }
    return coverage;
}

// -------------------------------------------------------------------
// KERNEL: Extract Overlap-Free Rectangles
// -------------------------------------------------------------------

__global__ void klee_geometry_extraction_kernel(
    const Event* events, int num_events,
    const K_COORDINATE_KIND* unique_y, int num_y,
    int32_t* tree_count,
    Box* out_boxes, int* out_count, int max_out) 
{
    // Assuming 1 Block per RTree Tile. 
    // In production, `tree_count` would be dynamically allocated in __shared__ memory.
    
    int tid = threadIdx.x;
    int num_leaves = num_y - 1; 

    // Sequential Sweep over X
    int e_idx = 0;
    while (e_idx < num_events) {
        
        K_COORDINATE_KIND current_x = events[e_idx].x;
        
        // 1. BATCHED UPDATE: All threads process events occurring at current_x in parallel
        for (int i = e_idx + tid; i < num_events && events[i].x == current_x; i += blockDim.x) {
            Event e = events[i];
            int j1 = binary_search_y(unique_y, num_y, e.y1);
            int j2 = binary_search_y(unique_y, num_y, e.y2);
            if (j1 < j2) {
                update_tree_atomic(1, 0, num_leaves - 1, j1, j2 - 1, e.lap_change, tree_count);
            }
        }
        __syncthreads();

        // Advance e_idx to the next unique X coordinate
        while (e_idx < num_events && events[e_idx].x == current_x) {
            e_idx++;
        }

        // 2. GEOMETRY EXTRACTION: If there is a horizontal gap, extract rectangles
        if (e_idx < num_events) {
            K_COORDINATE_KIND next_x = events[e_idx].x;
            if (next_x > current_x) {
                
                // Every thread checks an elementary Y-interval
                for (int leaf = tid; leaf < num_leaves; leaf += blockDim.x) {
                    
                    int my_coverage = get_leaf_coverage(leaf, num_leaves - 1, tree_count);
                    
                    // Boundary Detection: Is this leaf the bottom start of a new rectangle?
                    bool is_start = false;
                    if (my_coverage > 0) {
                        if (leaf == 0) {
                            is_start = true;
                        } else {
                            int prev_coverage = get_leaf_coverage(leaf - 1, num_leaves - 1, tree_count);
                            if (prev_coverage == 0) is_start = true;
                        }
                    }

                    if (is_start) {
                        // Scan forward to find the top edge of this continuous covered strip
                        int end_leaf = leaf;
                        while (end_leaf < num_leaves - 1) {
                            int next_cov = get_leaf_coverage(end_leaf + 1, num_leaves - 1, tree_count);
                            if (next_cov == 0) break;
                            end_leaf++;
                        }

                        // We found a fully overlap-free rectangle representing the Union!
                        int write_idx = atomicAdd(out_count, 1);
                        if (write_idx < max_out) {
                            out_boxes[write_idx] = {
                                current_x, 
                                unique_y[leaf], 
                                next_x, 
                                unique_y[end_leaf + 1]
                            };
                        }
                    }
                }
            }
        }
        __syncthreads();
    }
}