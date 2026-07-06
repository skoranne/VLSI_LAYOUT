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

  // The wrapper function called from Fortran
  // d_arr is a raw device pointer passed from OpenMP's use_device_ptr
  void device_event_sort(XYTracker* d_arr, int64_t n) {
    // 1. Allocate a temporary device array for the 64-bit primitive keys
    uint64_t* d_keys = nullptr;
    cudaMalloc((void**)&d_keys, n * sizeof(uint64_t));

    // 2. Map the X and Y coordinates into the 64-bit keys in parallel
    thrust::transform(thrust::device, d_arr, d_arr + n, d_keys, PackKeys());
    // 3. BLAST PROCESSING: Radix Sort!
    // This sorts the primitive d_keys, and physically moves the structs in 
    // d_arr to perfectly match the new sorted order.
    thrust::sort_by_key(thrust::device, d_keys, d_keys + n, d_arr);

    // 4. Clean up
    cudaFree(d_keys);
	
    // thrust::device execution policy tells Thrust the pointer is already on the GPU
    //thrust::sort(thrust::device, d_arr, d_arr + n, TrackerLess());
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
