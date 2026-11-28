// NURBS GPU kernel scaffold (Phase 3 N1-N3). Compile with nvcc -std=c++17.
// Targets: degree <= 3 rational NURBS/conics, y-monotone curved-slab Booleans.
#include "nurbs_types.hpp"
#include <cuda_runtime.h>
#include <cub/cub.cuh>
#include <cstdio>
#include <cstdint>

namespace nurbs_gpu {

__device__ __forceinline__ void eval_bez_d(const BezSpan& b, double t, double& x, double& y) {
    int p=b.degree;
    double bx[4],by[4],bw[4];
    #pragma unroll
    for(int i=0;i<4;++i){ if(i<=p){bx[i]=b.cx[i]*b.cw[i]; by[i]=b.cy[i]*b.cw[i]; bw[i]=b.cw[i];} }
    for(int r=1;r<=3;++r){ if(r>p) break; for(int j=0;j<4-r;++j){ if(j>p-r) break; double u=1.0-t; bx[j]=u*bx[j]+t*bx[j+1]; by[j]=u*by[j]+t*by[j+1]; bw[j]=u*bw[j]+t*bw[j+1]; } }
    x=bx[0]/bw[0]; y=by[0]/bw[0];
}

// N1: Monotonic splitting kernel - one span per thread (scaffold; production uses warp/curve).
__global__ void k_split_y_monotone(const BezSpan* in, int n, BezSpan* out, int* out_n, int max_out) {
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    if (i>=n) return;
    // TODO: full y-extrema root finding for degree<=3 rational; emit 1..k monotone spans.
    int pos = atomicAdd(out_n, 1);
    if (pos < max_out) out[pos] = in[i];
}

// N2: Curve-curve intersection broadphase (AABB/bin) scaffold.
__global__ void k_aabb_bins(const BezSpan* spans, int n, AABB2* boxes) {
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    if (i>=n) return;
    // Conservative AABB from control polygon (convex hull property for Bezier)
    double x0=spans[i].cx[0], x1=x0, y0=spans[i].cy[0], y1=y0;
    for(int j=1;j<=spans[i].degree;++j){ x0=fmin(x0,spans[i].cx[j]); x1=fmax(x1,spans[i].cx[j]); y0=fmin(y0,spans[i].cy[j]); y1=fmax(y1,spans[i].cy[j]); }
    boxes[i] = {x0,y0,x1,y1};
}

extern "C" {

int nurbs_gpu_split_y_monotone(const BezSpan* h_in, int n, BezSpan* h_out, int max_out) {
    BezSpan *d_in=nullptr,*d_out=nullptr; int *d_n=nullptr; int h_n=0;
    cudaMalloc(&d_in, n*sizeof(BezSpan)); cudaMalloc(&d_out, max_out*sizeof(BezSpan)); cudaMalloc(&d_n, sizeof(int));
    cudaMemcpy(d_in, h_in, n*sizeof(BezSpan), cudaMemcpyHostToDevice);
    cudaMemcpy(d_n, &h_n, sizeof(int), cudaMemcpyHostToDevice);
    int bs=256; k_split_y_monotone<<<(n+bs-1)/bs,bs>>>(d_in,n,d_out,d_n,max_out);
    cudaMemcpy(&h_n, d_n, sizeof(int), cudaMemcpyDeviceToHost);
    if(h_n>max_out) h_n=max_out;
    cudaMemcpy(h_out, d_out, h_n*sizeof(BezSpan), cudaMemcpyDeviceToHost);
    cudaFree(d_in); cudaFree(d_out); cudaFree(d_n);
    return h_n;
}

void* nurbs_gpu_pool_alloc(int64_t bytes) { void* p=nullptr; return cudaMalloc(&p, bytes)==cudaSuccess?p:nullptr; }
void  nurbs_gpu_pool_free(void* p) { if(p) cudaFree(p); }

} // extern "C"
} // namespace nurbs_gpu
