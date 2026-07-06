// File.  : gpu_morton.cu
// Author : Sandeep Koranne (C) 2026
// Purpose: NVFORTRAN OpenMP is still slower than Thrust based sort functions
//        : once you have 1B boxes, these things matter.

#ifndef USE_GPU
#include <algorithm>
#include <vector>
#include <cstdint>

extern "C"
{

    struct Box
    {
        int32_t x1, y1, x2, y2;
    };

    // Helper to compute Morton Code on CPU
    inline uint64_t compute_morton(const Box &b)
    {
        uint32_t cx = (uint32_t)(((int64_t)b.x1 + b.x2) / 2) ^ 0x80000000;
        uint32_t cy = (uint32_t)(((int64_t)b.y1 + b.y2) / 2) ^ 0x80000000;

        uint64_t mx = cx;
        mx = (mx | (mx << 16)) & 0x0000FFFF0000FFFFULL;
        mx = (mx | (mx << 8)) & 0x00FF00FF00FF00FFULL;
        mx = (mx | (mx << 4)) & 0x0F0F0F0F0F0F0F0FULL;
        mx = (mx | (mx << 2)) & 0x3333333333333333ULL;
        mx = (mx | (mx << 1)) & 0x5555555555555555ULL;

        uint64_t my = cy;
        my = (my | (my << 16)) & 0x0000FFFF0000FFFFULL;
        my = (my | (my << 8)) & 0x00FF00FF00FF00FFULL;
        my = (my | (my << 4)) & 0x0F0F0F0F0F0F0F0FULL;
        my = (my | (my << 2)) & 0x3333333333333333ULL;
        my = (my | (my << 1)) & 0x5555555555555555ULL;

        return (my << 1) | mx;
    }

    // Direct Host Sort: Sorts boxes in-place using std::sort
    void cpp_sort_boxes_direct(Box *boxes, int64_t n)
    {
        if (n <= 1)
            return;

        struct BoxWithKey
        {
            uint64_t key;
            Box box;
        };

        std::vector<BoxWithKey> temp(n);
        for (int64_t i = 0; i < n; ++i)
        {
            temp[i] = {compute_morton(boxes[i]), boxes[i]};
        }

        std::sort(temp.begin(), temp.end(), [](const BoxWithKey &a, const BoxWithKey &b)
                  { return a.key < b.key; });

        for (int64_t i = 0; i < n; ++i)
        {
            boxes[i] = temp[i].box;
        }
    }

    // Indirect Host Sort: Sorts a 1-based index array
    void cpp_sort_boxes_indirect(Box *boxes, int64_t *indices, int64_t n)
    {
        if (n <= 1)
            return;

        struct IndexWithKey
        {
            uint64_t key;
            int64_t index;
        };

        std::vector<IndexWithKey> temp(n);
        for (int64_t i = 0; i < n; ++i)
        {
            temp[i] = {compute_morton(boxes[i]), i + 1};
        }

        std::sort(temp.begin(), temp.end(), [](const IndexWithKey &a, const IndexWithKey &b)
                  { return a.key < b.key; });

        for (int64_t i = 0; i < n; ++i)
        {
            indices[i] = temp[i].index;
        }
    }
}
#else
#include <thrust/sort.h>
#include <thrust/execution_policy.h>
#include <thrust/transform.h>
#include <thrust/sequence.h>
#include <stdint.h>

extern "C"
{

    // Matches the Fortran Box type (assuming K_COORDINATE_KIND is int32_t)
    struct Box
    {
        int32_t x1, y1, x2, y2;
    };

    // Functor: Calculates 64-bit Morton Code directly from the Box
    struct ComputeMorton
    {
        __host__ __device__
            uint64_t
            operator()(const Box &b) const
        {
            // Calculate center using 64-bit math to prevent overflow.
            // XOR with 0x80000000 ensures negative coordinates monotonically
            // sort before positive ones when cast to unsigned 32-bit.
            uint32_t cx = (uint32_t)(((int64_t)b.x1 + b.x2) / 2) ^ 0x80000000;
            uint32_t cy = (uint32_t)(((int64_t)b.y1 + b.y2) / 2) ^ 0x80000000;

            // Expand X (Inline Morton Code generation matching Fortran bitmasks)
            uint64_t mx = cx;
            mx = (mx | (mx << 16)) & 0x0000FFFF0000FFFFULL;
            mx = (mx | (mx << 8)) & 0x00FF00FF00FF00FFULL;
            mx = (mx | (mx << 4)) & 0x0F0F0F0F0F0F0F0FULL;
            mx = (mx | (mx << 2)) & 0x3333333333333333ULL;
            mx = (mx | (mx << 1)) & 0x5555555555555555ULL;

            // Expand Y
            uint64_t my = cy;
            my = (my | (my << 16)) & 0x0000FFFF0000FFFFULL;
            my = (my | (my << 8)) & 0x00FF00FF00FF00FFULL;
            my = (my | (my << 4)) & 0x0F0F0F0F0F0F0F0FULL;
            my = (my | (my << 2)) & 0x3333333333333333ULL;
            my = (my | (my << 1)) & 0x5555555555555555ULL;

            // Interleave (Y in odd bits, X in even bits)
            return (my << 1) | mx;
        }
    };

    // ------------------------------------------------------------------------
    // DIRECT SORT: Physically moves the Boxes
    // ------------------------------------------------------------------------
    void thrust_sort_boxes_direct(Box *d_boxes, int64_t n)
    {
        if (n <= 1)
            return;

        uint64_t *d_keys = nullptr;
        cudaMalloc((void **)&d_keys, n * sizeof(uint64_t));

        // Generate Morton codes as keys
        thrust::transform(thrust::device, d_boxes, d_boxes + n, d_keys, ComputeMorton());

        // Radix Sort the boxes based on Morton keys
        thrust::sort_by_key(thrust::device, d_keys, d_keys + n, d_boxes);

        cudaFree(d_keys);
    }

    // ------------------------------------------------------------------------
    // INDIRECT SORT: Leaves Boxes untouched, sorts a 1-based index array
    // ------------------------------------------------------------------------
    void thrust_sort_boxes_indirect(Box *d_boxes, int64_t *d_indices, int64_t n)
    {
        if (n <= 1)
            return;

        uint64_t *d_keys = nullptr;
        cudaMalloc((void **)&d_keys, n * sizeof(uint64_t));

        // Generate Morton codes as keys
        thrust::transform(thrust::device, d_boxes, d_boxes + n, d_keys, ComputeMorton());

        // Initialize indices array with 1, 2, 3... N (Fortran 1-based indexing)
        thrust::sequence(thrust::device, d_indices, d_indices + n, 1LL);

        // Radix Sort the indices based on Morton keys (Boxes are NOT moved)
        thrust::sort_by_key(thrust::device, d_keys, d_keys + n, d_indices);

        cudaFree(d_keys);
    }
}
#endif
