#include <thrust/host_vector.h>
#include <thrust/sort.h>
#include <thrust/execution_policy.h> // Include execution policies
#include <iostream>
// 1. Detect if we are using the NVIDIA CUDA Compiler (nvcc)
#if defined(__CUDACC__)
    #define EXEC_POLICY thrust::device  // Runs in parallel on the GPU
    template <typename T>
    using Vector = thrust::device_vector<T>;
    const char* backend = "GPU (CUDA)";
#elif defined(USE_OPENMP)
    // ----------------------------------------------------
    // CASE B: Compiling with G++ targeting Parallel CPU (OpenMP)
    // ----------------------------------------------------
    #include <omp.h>
    #include <thrust/system/omp/execution_policy.h> // <-- ADD THIS LINE HERE
    #define EXEC_POLICY thrust::omp::par
    template <typename T>
    using Vector = thrust::host_vector<T>;
    const char* backend = "CPU (Parallel OpenMP)";

#else
    #define EXEC_POLICY thrust::seq     // Runs sequentially on the CPU
    template <typename T>
    using Vector = thrust::host_vector<T>;
    const char* backend = "CPU (Sequential)";
#endif
int main() {
    Vector<int> H(4);
    H[0] = 34; H[1] = 6; H[2] = 18; H[3] = 1;

    // Explicitly pass the sequential CPU execution policy
    thrust::sort(EXEC_POLICY, H.begin(), H.end());

    for(int i = 0; i < 4; i++) {
        std::cout << H[i] << " ";
    }
    std::cout << std::endl;
    return 0;
}
