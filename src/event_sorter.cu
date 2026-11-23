// File    : event_sorter.cu
// Author  : Sandeep Koranne (C) 2026
// Purpose : Compare Fortran native sort with Thrust on GPU
#ifndef USE_GPU
#include <algorithm>
#include <vector>
#include <utility>
#include <cstdint>
#endif
extern "C" {
#ifndef USE_GPU
  typedef int int32_t;
#endif
  // This struct must exactly match the memory layout of the Fortran bind(c) type.
  // Assuming K_COORDINATE_KIND maps to a 32-bit integer (adjust to int64_t if needed).
  struct XYTracker {
    int32_t X;
    int32_t Y;
    int8_t polygonNumber;
  };

  // Custom comparator functor for Thrust to sort by X, then by Y
  struct TrackerLess {
#ifdef USE_GPU
    __host__ __device__
#endif
    bool operator()(const XYTracker& a, const XYTracker& b) const {
      if (a.X != b.X) {
	return a.X < b.X;
      }
      return a.Y < b.Y;
    }
  };
}

#ifdef USE_GPU
#include <thrust/sort.h>
#include <thrust/execution_policy.h>
#include <thrust/transform.h>
#include <cuda_runtime.h>
#include <cstdio>
#include <stdint.h>

// Use extern "C" to prevent C++ name mangling so Fortran can link to it
extern "C" {
  // Functor: Packs two 32-bit integers into one 64-bit unsigned integer.
  // X becomes the upper 32 bits (Primary Key)
  // Y becomes the lower 32 bits (Secondary Key)
#ifdef USE_GPU
  struct PackKeys {
    __host__ __device__
    std::uint64_t operator()(const XYTracker& t) const {
      // We XOR with 0x80000000 to flip the sign bit. 
      // This genius bitwise trick ensures that negative coordinates correctly
      // sort before positive coordinates when cast to an unsigned integer!
      uint32_t ux = (uint32_t)t.X ^ 0x80000000;
      uint32_t uy = (uint32_t)t.Y ^ 0x80000000;

      return ((uint64_t)ux << 32) | (uint64_t)uy;
    }
  };
#endif

  int magparser_is_device_pointer(const void *p); // gpu_morton.cu

  // Called from Fortran (event_sorter_apif.f90). d_arr is normally the device address from
  // OpenMP use_device_addr; if the caller had no device mapping (offload disabled / no
  // device) it is a host pointer, which is staged through a device buffer here instead of
  // relying on the GPU being able to dereference host memory (true only with ATS/HMM).
  void device_event_sort(XYTracker* arr, int64_t n) {
    if (n <= 1) return;
    const size_t bytes = (size_t)n * sizeof(XYTracker);
    XYTracker* d_arr = arr;
    XYTracker* staged = nullptr;
    if (!magparser_is_device_pointer(arr)) {
      if (cudaMalloc((void**)&staged, bytes) != cudaSuccess ||
          cudaMemcpy(staged, arr, bytes, cudaMemcpyHostToDevice) != cudaSuccess) {
        if (staged) cudaFree(staged);
        std::fprintf(stderr, "device_event_sort: staging failed, sorting on host\n");
        thrust::sort(thrust::host, arr, arr + n, TrackerLess());
        return;
      }
      d_arr = staged;
    }
    uint64_t* d_keys = nullptr;
    if (cudaMalloc((void**)&d_keys, n * sizeof(uint64_t)) != cudaSuccess) {
      std::fprintf(stderr, "device_event_sort: cudaMalloc(keys) failed\n");
      if (staged) cudaFree(staged);
      return;
    }
    // Pack (X,Y) into one 64-bit key and radix sort the structs by it.
    thrust::transform(thrust::device, d_arr, d_arr + n, d_keys, PackKeys());
    thrust::sort_by_key(thrust::device, d_keys, d_keys + n, d_arr);
    cudaFree(d_keys);
    if (staged) {
      if (cudaMemcpy(arr, staged, bytes, cudaMemcpyDeviceToHost) != cudaSuccess)
        std::fprintf(stderr, "device_event_sort: copy-back failed\n");
      cudaFree(staged);
    }
  }
}
#else

extern "C" {
  void cpp_std_sort_trackers(XYTracker* arr, int64_t n) {
    if (n <= 1) return;

    // 1. Allocate a temporary vector of pairs to hold (64-bit Key, XYTracker)
    std::vector<std::pair<uint64_t, XYTracker>> paired_data(n);

    // 2. Map (Transform) the X and Y coordinates into 64-bit keys
    std::transform(arr, arr + n, paired_data.begin(), [](const XYTracker& t) {
      // XOR with 0x80000000 to handle negative coordinates properly
      uint32_t ux = (uint32_t)t.X ^ 0x80000000;
      uint32_t uy = (uint32_t)t.Y ^ 0x80000000;
      uint64_t key = ((uint64_t)ux << 32) | (uint64_t)uy;
            
      return std::make_pair(key, t);
    });

    // 3. Sort using std::sort based on the 64-bit primitive keys
    std::sort(paired_data.begin(), paired_data.end(), 
	      [](const std::pair<uint64_t, XYTracker>& a, const std::pair<uint64_t, XYTracker>& b) {
                return a.first < b.first;
	      }
	      );

    // 4. Transform back to the original array (extracting the sorted structs)
    std::transform(paired_data.begin(), paired_data.end(), arr, 
		   [](const std::pair<uint64_t, XYTracker>& p) {
		     return p.second;
		   }
		   );
  }
  // The wrapper function called from Fortran
  // d_arr is a raw device pointer passed from OpenMP's use_device_ptr
  void device_event_sort(XYTracker* d_arr, int64_t n) {
    if (n <= 1) return;

    std::sort(d_arr, d_arr + n, [](const XYTracker& a, const XYTracker& b) {
      if (a.X != b.X) {
	return a.X < b.X;
      }
      return a.Y < b.Y;
    });
	
    //std::sort(d_arr, d_arr + n, TrackerLess());
  }
}

#endif
