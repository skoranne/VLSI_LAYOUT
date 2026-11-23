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
    void thrust_sort_boxes_direct(Box *boxes, int64_t n)
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
    void thrust_sort_boxes_indirect(Box *boxes, int64_t *indices, int64_t n)
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

    int32_t thrust_sort_boxes_direct_host(Box *h_boxes, int64_t n)
    {
        thrust_sort_boxes_direct(h_boxes, n);
        return 0;
    }

    int32_t thrust_sort_boxes_indirect_host(Box *h_boxes, int64_t *h_indices, int64_t n)
    {
        thrust_sort_boxes_indirect(h_boxes, h_indices, n);
        return 0;
    }

    int32_t thrust_exclusive_scan_i64(const int64_t *in_arr, int64_t *out_arr, int64_t n, int64_t *total_out)
    {
        int64_t acc = 0;
        for (int64_t i = 0; i < n; ++i)
        {
            out_arr[i] = acc;
            acc += in_arr[i];
        }
        if (total_out)
            *total_out = acc;
        return 0;
    }

    // No GPU in this build: report "unavailable" so the Fortran residency layer stays off.
    int magparser_gpu_mem_info(size_t *free_b, size_t *total_b)
    {
        *free_b = 0;
        *total_b = 0;
        return 1;
    }
}
#else
// ============================================================================
// GPU implementation (nvcc -DUSE_GPU)
//
// Performance notes (DGX Spark / GB10, CUDA 13):
//  * One process-wide non-blocking stream; Thrust runs with par_nosync on it and a caching
//    allocator, so repeated sorts/scans do not cudaMalloc/cudaFree Thrust's temp storage or
//    synchronise the whole device. Each entry point synchronises only its stream before
//    returning (results are consumed by host code / the next OpenMP kernel).
//  * Key/box/index scratch buffers are cached between calls up to
//    MAGPARSER_GPU_SCRATCH_CACHE_MB (default 2048 MB). On GB10 device memory is the same
//    LPDDR5x as host memory, so very large scratch is released after each call.
//  * Every CUDA call is checked; failures return a nonzero code (Fortran falls back to CPU).
// ============================================================================
#include <thrust/sort.h>
#include <thrust/scan.h>
#include <thrust/execution_policy.h>
#include <thrust/transform.h>
#include <thrust/sequence.h>
#include <cuda_runtime.h>
#include <stdint.h>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <mutex>
#include <new>

namespace
{
    std::mutex g_mutex;
    cudaStream_t g_stream = nullptr;
    size_t g_cache_cap = 0; // bytes; 0 = not yet read

    size_t cache_cap()
    {
        if (g_cache_cap == 0)
        {
            size_t mb = 2048;
            if (const char *e = std::getenv("MAGPARSER_GPU_SCRATCH_CACHE_MB"))
                mb = (size_t)std::strtoull(e, nullptr, 10);
            g_cache_cap = mb * (size_t)1024 * 1024 + 1; // +1 keeps it nonzero for mb == 0
        }
        return g_cache_cap - 1;
    }

    cudaStream_t stream()
    {
        if (!g_stream)
        {
            if (cudaStreamCreateWithFlags(&g_stream, cudaStreamNonBlocking) != cudaSuccess)
                g_stream = 0; // legacy default stream fallback
        }
        return g_stream;
    }

    // Grow-only device scratch buffer, cached up to cache_cap().
    struct Scratch
    {
        void *ptr = nullptr;
        size_t cap = 0;
        void *get(size_t bytes)
        {
            if (bytes <= cap)
                return ptr;
            release();
            if (cudaMalloc(&ptr, bytes) != cudaSuccess)
            {
                ptr = nullptr;
                return nullptr;
            }
            cap = bytes;
            return ptr;
        }
        void trim()
        {
            if (cap > cache_cap())
                release();
        }
        void release()
        {
            if (ptr)
                cudaFree(ptr);
            ptr = nullptr;
            cap = 0;
        }
    };
    Scratch g_keys, g_boxes, g_idx;

    // Thrust temporary-storage allocator that reuses freed blocks (classic Thrust example).
    struct CachedAllocator
    {
        typedef char value_type;
        std::multimap<std::ptrdiff_t, char *> free_blocks;
        std::map<char *, std::ptrdiff_t> allocated_blocks;
        char *allocate(std::ptrdiff_t n)
        {
            auto it = free_blocks.lower_bound(n);
            if (it != free_blocks.end())
            {
                char *p = it->second;
                std::ptrdiff_t sz = it->first;
                free_blocks.erase(it);
                allocated_blocks.insert(std::make_pair(p, sz));
                return p;
            }
            char *p = nullptr;
            if (cudaMalloc((void **)&p, n) != cudaSuccess)
            {
                // Release cached blocks and retry once before giving up.
                trim(0);
                if (cudaMalloc((void **)&p, n) != cudaSuccess)
                    throw std::bad_alloc();
            }
            allocated_blocks.insert(std::make_pair(p, n));
            return p;
        }
        void deallocate(char *p, size_t)
        {
            auto it = allocated_blocks.find(p);
            if (it == allocated_blocks.end())
                return;
            free_blocks.insert(std::make_pair(it->second, it->first));
            allocated_blocks.erase(it);
        }
        // Free cached (unused) blocks larger than `keep_max` bytes.
        void trim(size_t keep_max)
        {
            for (auto it = free_blocks.begin(); it != free_blocks.end();)
            {
                if ((size_t)it->first > keep_max)
                {
                    cudaFree(it->second);
                    it = free_blocks.erase(it);
                }
                else
                    ++it;
            }
        }
    };
    CachedAllocator g_alloc;

    inline auto policy() -> decltype(thrust::cuda::par_nosync(g_alloc).on(stream()))
    {
        return thrust::cuda::par_nosync(g_alloc).on(stream());
    }

    void end_call()
    {
        g_keys.trim();
        g_boxes.trim();
        g_idx.trim();
        g_alloc.trim(cache_cap());
    }

    bool is_device_pointer(const void *p)
    {
        cudaPointerAttributes a;
        if (cudaPointerGetAttributes(&a, p) != cudaSuccess)
        {
            cudaGetLastError(); // clear sticky "invalid value" for plain host pointers
            return false;
        }
        return a.type == cudaMemoryTypeDevice || a.type == cudaMemoryTypeManaged;
    }
} // namespace

extern "C"
{

    // Matches the Fortran Box type (K_COORDINATE_KIND = int32)
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
            // Center in 64-bit math (no overflow); XOR 0x80000000 maps signed to monotone unsigned.
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

            return (my << 1) | mx; // Y in odd bits, X in even bits
        }
    };

    // cudaMemGetInfo for the Fortran residency budget. 0 = success.
    int magparser_gpu_mem_info(size_t *free_b, size_t *total_b)
    {
        *free_b = 0;
        *total_b = 0;
        int ndev = 0;
        if (cudaGetDeviceCount(&ndev) != cudaSuccess || ndev <= 0)
        {
            cudaGetLastError();
            return 1;
        }
        return cudaMemGetInfo(free_b, total_b) == cudaSuccess ? 0 : 2;
    }

    // ------------------------------------------------------------------------
    // DIRECT SORT on device data: physically moves the boxes (device pointer).
    // ------------------------------------------------------------------------
    void thrust_sort_boxes_direct(Box *d_boxes, int64_t n)
    {
        if (n <= 1)
            return;
        std::lock_guard<std::mutex> lk(g_mutex);
        uint64_t *d_keys = (uint64_t *)g_keys.get((size_t)n * sizeof(uint64_t));
        if (!d_keys)
        {
            std::fprintf(stderr, "thrust_sort_boxes_direct: cudaMalloc(keys) failed\n");
            return;
        }
        thrust::transform(policy(), d_boxes, d_boxes + n, d_keys, ComputeMorton());
        thrust::sort_by_key(policy(), d_keys, d_keys + n, d_boxes);
        cudaStreamSynchronize(stream());
        end_call();
    }

    // ------------------------------------------------------------------------
    // INDIRECT SORT on device data: boxes untouched, sorts a 1-based index array.
    // ------------------------------------------------------------------------
    void thrust_sort_boxes_indirect(Box *d_boxes, int64_t *d_indices, int64_t n)
    {
        if (n <= 1)
            return;
        std::lock_guard<std::mutex> lk(g_mutex);
        uint64_t *d_keys = (uint64_t *)g_keys.get((size_t)n * sizeof(uint64_t));
        if (!d_keys)
        {
            std::fprintf(stderr, "thrust_sort_boxes_indirect: cudaMalloc(keys) failed\n");
            return;
        }
        thrust::transform(policy(), d_boxes, d_boxes + n, d_keys, ComputeMorton());
        thrust::sequence(policy(), d_indices, d_indices + n, 1LL);
        thrust::sort_by_key(policy(), d_keys, d_keys + n, d_indices);
        cudaStreamSynchronize(stream());
        end_call();
    }

    // ------------------------------------------------------------------------
    // HOST-POINTER DIRECT SORT (1D copies; avoids nvfortran cuMemcpy2DAsync pitch issues).
    // Returns 0 on success, nonzero on CUDA error (caller falls back to CPU sort).
    // ------------------------------------------------------------------------
    int32_t thrust_sort_boxes_direct_host(Box *h_boxes, int64_t n)
    {
        if (n <= 1)
            return 0;
        std::lock_guard<std::mutex> lk(g_mutex);
        const size_t box_bytes = (size_t)n * sizeof(Box);
        Box *d_boxes = (Box *)g_boxes.get(box_bytes);
        if (!d_boxes)
            return 1;
        uint64_t *d_keys = (uint64_t *)g_keys.get((size_t)n * sizeof(uint64_t));
        if (!d_keys)
        {
            end_call();
            return 2;
        }
        cudaStream_t s = stream();
        if (cudaMemcpyAsync(d_boxes, h_boxes, box_bytes, cudaMemcpyHostToDevice, s) != cudaSuccess)
        {
            end_call();
            return 3;
        }
        try
        {
            thrust::transform(policy(), d_boxes, d_boxes + n, d_keys, ComputeMorton());
            thrust::sort_by_key(policy(), d_keys, d_keys + n, d_boxes);
        }
        catch (...)
        {
            cudaStreamSynchronize(s);
            end_call();
            return 6;
        }
        cudaError_t err = cudaMemcpyAsync(h_boxes, d_boxes, box_bytes, cudaMemcpyDeviceToHost, s);
        cudaError_t serr = cudaStreamSynchronize(s);
        end_call();
        if (err != cudaSuccess)
            return 4;
        return (serr == cudaSuccess) ? 0 : 5;
    }

    int32_t thrust_sort_boxes_indirect_host(Box *h_boxes, int64_t *h_indices, int64_t n)
    {
        if (n <= 1)
        {
            if (n == 1)
                h_indices[0] = 1;
            return 0;
        }
        std::lock_guard<std::mutex> lk(g_mutex);
        const size_t box_bytes = (size_t)n * sizeof(Box);
        const size_t idx_bytes = (size_t)n * sizeof(int64_t);
        Box *d_boxes = (Box *)g_boxes.get(box_bytes);
        uint64_t *d_keys = (uint64_t *)g_keys.get((size_t)n * sizeof(uint64_t));
        int64_t *d_indices = (int64_t *)g_idx.get(idx_bytes);
        if (!d_boxes || !d_keys || !d_indices)
        {
            end_call();
            return 1;
        }
        cudaStream_t s = stream();
        if (cudaMemcpyAsync(d_boxes, h_boxes, box_bytes, cudaMemcpyHostToDevice, s) != cudaSuccess)
        {
            end_call();
            return 4;
        }
        try
        {
            thrust::transform(policy(), d_boxes, d_boxes + n, d_keys, ComputeMorton());
            thrust::sequence(policy(), d_indices, d_indices + n, 1LL);
            thrust::sort_by_key(policy(), d_keys, d_keys + n, d_indices);
        }
        catch (...)
        {
            cudaStreamSynchronize(s);
            end_call();
            return 7;
        }
        cudaError_t err = cudaMemcpyAsync(h_indices, d_indices, idx_bytes, cudaMemcpyDeviceToHost, s);
        cudaError_t serr = cudaStreamSynchronize(s);
        end_call();
        if (err != cudaSuccess)
            return 5;
        return (serr == cudaSuccess) ? 0 : 6;
    }

    // ------------------------------------------------------------------------
    // DEVICE EXCLUSIVE SCAN (int64) on OpenMP device arrays (use_device_addr).
    // Total = out[n-1] + in[n-1]; both scalars come back in one stream sync.
    // ------------------------------------------------------------------------
    int32_t thrust_exclusive_scan_i64(const int64_t *d_in, int64_t *d_out, int64_t n, int64_t *h_total)
    {
        if (n <= 0)
        {
            if (h_total)
                *h_total = 0;
            return 0;
        }
        std::lock_guard<std::mutex> lk(g_mutex);
        cudaStream_t s = stream();
        int64_t last[2] = {0, 0};
        // in[n-1] must be read before a possible in-place scan overwrites it.
        if (cudaMemcpyAsync(&last[0], d_in + (n - 1), sizeof(int64_t), cudaMemcpyDeviceToHost, s) != cudaSuccess)
            return 1;
        try
        {
            thrust::exclusive_scan(policy(), d_in, d_in + n, d_out, (int64_t)0);
        }
        catch (...)
        {
            cudaStreamSynchronize(s);
            return 4;
        }
        if (cudaMemcpyAsync(&last[1], d_out + (n - 1), sizeof(int64_t), cudaMemcpyDeviceToHost, s) != cudaSuccess)
            return 2;
        cudaError_t serr = cudaStreamSynchronize(s);
        end_call();
        if (serr != cudaSuccess)
            return 3;
        if (h_total)
            *h_total = last[0] + last[1];
        return 0;
    }

    // Used by event_sorter.cu: true if p is device/managed memory (else stage through device).
    int magparser_is_device_pointer(const void *p)
    {
        return is_device_pointer(p) ? 1 : 0;
    }
}
#endif
