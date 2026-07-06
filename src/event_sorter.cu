// File    : event_sorter.cu
// Author  : Sandeep Koranne (C) 2026
// Purpose : Compare Fortran native sort with Thrust on GPU
#ifndef USE_GPU
#include <algorithm>  
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
#include <stdint.h>

// Use extern "C" to prevent C++ name mangling so Fortran can link to it
extern "C" {
    // The wrapper function called from Fortran
    // d_arr is a raw device pointer passed from OpenMP's use_device_ptr
    void device_event_sort(XYTracker* d_arr, int64_t n) {
        // thrust::device execution policy tells Thrust the pointer is already on the GPU
        thrust::sort(thrust::device, d_arr, d_arr + n, TrackerLess());
    }
}
#else

extern "C" {
    // The wrapper function called from Fortran
    // d_arr is a raw device pointer passed from OpenMP's use_device_ptr
    void device_event_sort(XYTracker* d_arr, int64_t n) {
        std::sort(d_arr, d_arr + n, TrackerLess());
    }
}

#endif
