// File   : nurbs_gpu.cu
// Author : Sandeep Koranne (C) 2026. All rights reserved.
// Purpose: GPU-accelerated degree<=3 rational NURBS/Bezier N1 (monotonic split) &
//          N2 (AABB broadphase overlap) kernels with stream-ordered cudaMemPool_t
//          and CPU fallback for non-CUDA builds (Makefile.ifx).
#include "nurbs_types.hpp"
#include <cmath>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <algorithm>
#include <vector>

#ifdef USE_GPU
#include <cuda_runtime.h>
#define HD __host__ __device__
#else
#define HD
#endif

namespace nurbs_gpu {

HD inline void eval_bez_hd(const BezSpan& b, double t, double& x, double& y) {
    const int p = b.degree;
    double bx[4], by[4], bw[4];
    for (int i = 0; i <= p; ++i) {
        const double w = b.cw[i];
        bx[i] = b.cx[i] * w;
        by[i] = b.cy[i] * w;
        bw[i] = w;
    }
    const double u = 1.0 - t;
    for (int r = 1; r <= p; ++r) {
        for (int j = 0; j <= p - r; ++j) {
            bx[j] = u * bx[j] + t * bx[j + 1];
            by[j] = u * by[j] + t * by[j + 1];
            bw[j] = u * bw[j] + t * bw[j + 1];
        }
    }
    x = bx[0] / bw[0];
    y = by[0] / bw[0];
}

// Exact analytical dy/dt of rational Bezier via homogeneous quotient rule: (Y'W - YW') / W^2.
HD inline double bez_dydt_hd(const BezSpan& b, double t) {
    const int p = b.degree;
    if (p <= 0) return 0.0;
    if (p == 1) {
        const double w0 = b.cw[0], w1 = b.cw[1];
        const double W = (1.0 - t) * w0 + t * w1;
        return w0 * w1 * (b.cy[1] - b.cy[0]) / (W * W);
    }
    double hy[4], hw[4];
    for (int i = 0; i <= p; ++i) {
        hy[i] = b.cy[i] * b.cw[i];
        hw[i] = b.cw[i];
    }
    const double u = 1.0 - t;
    // De Casteljau down to level p-1 gives both value and derivative!
    for (int r = 1; r < p; ++r) {
        for (int j = 0; j <= p - r; ++j) {
            hy[j] = u * hy[j] + t * hy[j + 1];
            hw[j] = u * hw[j] + t * hw[j + 1];
        }
    }
    const double Y  = u * hy[0] + t * hy[1];
    const double W  = u * hw[0] + t * hw[1];
    const double dY = (double)p * (hy[1] - hy[0]);
    const double dW = (double)p * (hw[1] - hw[0]);
    return (dY * W - Y * dW) / (W * W);
}

// Exact homogeneous de Casteljau split of rational Bezier at t in (0, 1).
HD inline void split_bez_hd(const BezSpan& b, double t, BezSpan& L, BezSpan& R) {
    const int p = b.degree;
    double hx[4][4], hy[4][4], hw[4][4];
    for (int i = 0; i <= p; ++i) {
        const double w = b.cw[i];
        hx[0][i] = b.cx[i] * w;
        hy[0][i] = b.cy[i] * w;
        hw[0][i] = w;
    }
    const double u = 1.0 - t;
    for (int r = 1; r <= p; ++r) {
        for (int j = 0; j <= p - r; ++j) {
            hx[r][j] = u * hx[r - 1][j] + t * hx[r - 1][j + 1];
            hy[r][j] = u * hy[r - 1][j] + t * hy[r - 1][j + 1];
            hw[r][j] = u * hw[r - 1][j] + t * hw[r - 1][j + 1];
        }
    }
    L = b;
    R = b;
    for (int i = 0; i <= p; ++i) {
        L.cw[i] = hw[i][0];
        L.cx[i] = hx[i][0] / hw[i][0];
        L.cy[i] = hy[i][0] / hw[i][0];
        R.cw[i] = hw[p - i][i];
        R.cx[i] = hx[p - i][i] / hw[p - i][i];
        R.cy[i] = hy[p - i][i] / hw[p - i][i];
    }
}

// Exact y-extrema roots in (0, 1) for degree <= 3 rational/non-rational Bezier.
HD inline int y_extrema_hd(const BezSpan& b, double roots[4]) {
    int n = 0;
    const int p = b.degree;
    if (p <= 1) return 0;
    if (p == 2) {
        // Exact quadratic: 1/2 (Y'W - YW') = (1-t)^2 d01 + t(1-t) d02 + t^2 d12 = 0
        const double d01 = b.cw[0] * b.cw[1] * (b.cy[1] - b.cy[0]);
        const double d02 = b.cw[0] * b.cw[2] * (b.cy[2] - b.cy[0]);
        const double d12 = b.cw[1] * b.cw[2] * (b.cy[2] - b.cy[1]);
        const double A = d01 - d02 + d12;
        const double B = d02 - 2.0 * d01;
        const double C = d01;
        if (fabs(A) <= 1e-18 * (fabs(B) + fabs(C) + 1.0)) {
            if (fabs(B) > 1e-18) {
                const double t = -C / B;
                if (t > 1e-12 && t < 1.0 - 1e-12) roots[n++] = t;
            }
        } else {
            const double disc = B * B - 4.0 * A * C;
            if (disc >= 0.0) {
                const double s = sqrt(disc);
                const double t1 = (-B - s) / (2.0 * A);
                const double t2 = (-B + s) / (2.0 * A);
                if (t1 > 1e-12 && t1 < 1.0 - 1e-12) roots[n++] = t1;
                if (t2 > 1e-12 && t2 < 1.0 - 1e-12 && fabs(t2 - t1) > 1e-11) roots[n++] = t2;
            }
        }
        if (n == 2 && roots[1] < roots[0]) { double tmp = roots[0]; roots[0] = roots[1]; roots[1] = tmp; }
        return n;
    }
    // Degree 3: exact analytical dy/dt scan + bisection (exact to 1e-14).
    const int S = 32;
    double prev = bez_dydt_hd(b, 0.0);
    for (int i = 1; i <= S; ++i) {
        const double t = (double)i / (double)S;
        const double cur = bez_dydt_hd(b, t);
        if (prev == 0.0 || prev * cur <= 0.0) {
            double lo = (double)(i - 1) / (double)S, hi = t;
            double flo = prev;
            for (int it = 0; it < 56; ++it) {
                const double mid = 0.5 * (lo + hi);
                const double fm = bez_dydt_hd(b, mid);
                if (flo * fm <= 0.0) hi = mid; else { lo = mid; flo = fm; }
            }
            const double r = 0.5 * (lo + hi);
            if (r > 1e-11 && r < 1.0 - 1e-11) {
                bool dup = false;
                for (int k = 0; k < n; ++k) if (fabs(roots[k] - r) < 1e-9) dup = true;
                if (!dup && n < 4) roots[n++] = r;
            }
        }
        prev = cur;
    }
    return n;
}

HD inline int split_monotone_hd(const BezSpan& in, BezSpan out[5]) {
    double roots[4];
    const int nr = y_extrema_hd(in, roots);
    BezSpan rem = in;
    double last = 0.0;
    int nout = 0;
    for (int i = 0; i < nr; ++i) {
        const double t = roots[i];
        if (t - last <= 1e-11) continue;
        const double loc = (t - last) / (1.0 - last);
        if (loc <= 1e-11 || loc >= 1.0 - 1e-11) continue;
        BezSpan L, R;
        split_bez_hd(rem, loc, L, R);
        L.flags |= 2;
        out[nout++] = L;
        rem = R;
        last = t;
    }
    rem.flags |= 2;
    out[nout++] = rem;
    return nout;
}

HD inline AABB2 span_aabb_hd(const BezSpan& s) {
    double x0 = s.cx[0], x1 = x0, y0 = s.cy[0], y1 = y0;
    for (int j = 1; j <= s.degree; ++j) {
        if (s.cx[j] < x0) x0 = s.cx[j];
        if (s.cx[j] > x1) x1 = s.cx[j];
        if (s.cy[j] < y0) y0 = s.cy[j];
        if (s.cy[j] > y1) y1 = s.cy[j];
    }
    return { x0, y0, x1, y1 };
}

#ifdef USE_GPU

namespace {
cudaStream_t g_nurbs_stream = nullptr;
cudaMemPool_t g_nurbs_pool = nullptr;
bool g_nurbs_pool_init = false;

cudaStream_t get_stream() {
    if (!g_nurbs_stream) {
        if (cudaStreamCreateWithFlags(&g_nurbs_stream, cudaStreamNonBlocking) != cudaSuccess)
            g_nurbs_stream = 0;
    }
    return g_nurbs_stream;
}

void init_pool() {
    if (g_nurbs_pool_init) return;
    g_nurbs_pool_init = true;
    int dev = 0, sup = 0;
    if (cudaGetDevice(&dev) == cudaSuccess &&
        cudaDeviceGetAttribute(&sup, cudaDevAttrMemoryPoolsSupported, dev) == cudaSuccess &&
        sup && cudaDeviceGetDefaultMemPool(&g_nurbs_pool, dev) == cudaSuccess && g_nurbs_pool) {
        uint64_t thresh = UINT64_MAX;
        cudaMemPoolSetAttribute(g_nurbs_pool, cudaMemPoolAttrReleaseThreshold, &thresh);
    } else {
        g_nurbs_pool = nullptr;
    }
}

cudaError_t dev_alloc(void** p, size_t bytes) {
    init_pool();
    if (g_nurbs_pool) return cudaMallocAsync(p, bytes, get_stream());
    return cudaMalloc(p, bytes);
}

void dev_free(void* p) {
    if (!p) return;
    if (g_nurbs_pool) cudaFreeAsync(p, get_stream());
    else cudaFree(p);
}
} // namespace

// N1: GPU exact monotonic splitting kernel (1 thread per input span, emits 1..5 y-monotone spans).
__global__ void k_split_y_monotone(const BezSpan* in, int n, BezSpan* out, int* out_n, int max_out) {
    const int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= n) return;
    BezSpan tmp[5];
    const int k = split_monotone_hd(in[i], tmp);
    const int pos = atomicAdd(out_n, k);
    for (int j = 0; j < k; ++j) {
        if (pos + j < max_out) out[pos + j] = tmp[j];
    }
}

// N2a: GPU control-polygon AABB kernel.
__global__ void k_aabb_bins(const BezSpan* spans, int n, AABB2* boxes) {
    const int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= n) return;
    boxes[i] = span_aabb_hd(spans[i]);
}

// N2b: GPU AABB broadphase overlap pair finder (i < j, conservative eps padding).
__global__ void k_find_overlaps(const AABB2* boxes, int n, double eps,
                                int* pair_a, int* pair_b, int* npairs, int max_pairs) {
    const int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= n) return;
    const AABB2 bi = boxes[i];
    for (int j = i + 1; j < n; ++j) {
        const AABB2 bj = boxes[j];
        if (bi.x1 + eps < bj.x0 || bj.x1 + eps < bi.x0) continue;
        if (bi.y1 + eps < bj.y0 || bj.y1 + eps < bi.y0) continue;
        const int pos = atomicAdd(npairs, 1);
        if (pos < max_pairs) {
            pair_a[pos] = i + 1; // 1-based for Fortran
            pair_b[pos] = j + 1;
        }
    }
}

extern "C" {

int nurbs_gpu_available() {
    int cnt = 0;
    return (cudaGetDeviceCount(&cnt) == cudaSuccess && cnt > 0) ? 1 : 0;
}

int nurbs_gpu_split_y_monotone(const BezSpan* h_in, int n, BezSpan* h_out, int max_out) {
    if (n <= 0 || max_out <= 0) return 0;
    BezSpan *d_in = nullptr, *d_out = nullptr;
    int *d_n = nullptr;
    int h_n = 0;
    cudaStream_t st = get_stream();
    if (dev_alloc((void**)&d_in, (size_t)n * sizeof(BezSpan)) != cudaSuccess ||
        dev_alloc((void**)&d_out, (size_t)max_out * sizeof(BezSpan)) != cudaSuccess ||
        dev_alloc((void**)&d_n, sizeof(int)) != cudaSuccess) {
        dev_free(d_in); dev_free(d_out); dev_free(d_n);
        return -1;
    }
    cudaMemcpyAsync(d_in, h_in, (size_t)n * sizeof(BezSpan), cudaMemcpyHostToDevice, st);
    cudaMemsetAsync(d_n, 0, sizeof(int), st);
    const int bs = 256;
    k_split_y_monotone<<<(n + bs - 1) / bs, bs, 0, st>>>(d_in, n, d_out, d_n, max_out);
    cudaMemcpyAsync(&h_n, d_n, sizeof(int), cudaMemcpyDeviceToHost, st);
    cudaStreamSynchronize(st);
    const int copy_n = (h_n > max_out) ? max_out : h_n;
    if (copy_n > 0) {
        cudaMemcpyAsync(h_out, d_out, (size_t)copy_n * sizeof(BezSpan), cudaMemcpyDeviceToHost, st);
        cudaStreamSynchronize(st);
    }
    dev_free(d_in); dev_free(d_out); dev_free(d_n);
    return h_n;
}

int nurbs_gpu_find_overlaps(const BezSpan* h_spans, int n, double eps,
                            int* h_pair_a, int* h_pair_b, int max_pairs) {
    if (n <= 1 || max_pairs <= 0) return 0;
    BezSpan* d_spans = nullptr;
    AABB2* d_boxes = nullptr;
    int *d_pa = nullptr, *d_pb = nullptr, *d_np = nullptr;
    int h_np = 0;
    cudaStream_t st = get_stream();
    if (dev_alloc((void**)&d_spans, (size_t)n * sizeof(BezSpan)) != cudaSuccess ||
        dev_alloc((void**)&d_boxes, (size_t)n * sizeof(AABB2)) != cudaSuccess ||
        dev_alloc((void**)&d_pa, (size_t)max_pairs * sizeof(int)) != cudaSuccess ||
        dev_alloc((void**)&d_pb, (size_t)max_pairs * sizeof(int)) != cudaSuccess ||
        dev_alloc((void**)&d_np, sizeof(int)) != cudaSuccess) {
        dev_free(d_spans); dev_free(d_boxes); dev_free(d_pa); dev_free(d_pb); dev_free(d_np);
        return -1;
    }
    cudaMemcpyAsync(d_spans, h_spans, (size_t)n * sizeof(BezSpan), cudaMemcpyHostToDevice, st);
    cudaMemsetAsync(d_np, 0, sizeof(int), st);
    const int bs = 256;
    k_aabb_bins<<<(n + bs - 1) / bs, bs, 0, st>>>(d_spans, n, d_boxes);
    k_find_overlaps<<<(n + bs - 1) / bs, bs, 0, st>>>(d_boxes, n, eps, d_pa, d_pb, d_np, max_pairs);
    cudaMemcpyAsync(&h_np, d_np, sizeof(int), cudaMemcpyDeviceToHost, st);
    cudaStreamSynchronize(st);
    const int copy_n = (h_np > max_pairs) ? max_pairs : h_np;
    if (copy_n > 0) {
        cudaMemcpyAsync(h_pair_a, d_pa, (size_t)copy_n * sizeof(int), cudaMemcpyDeviceToHost, st);
        cudaMemcpyAsync(h_pair_b, d_pb, (size_t)copy_n * sizeof(int), cudaMemcpyDeviceToHost, st);
        cudaStreamSynchronize(st);
    }
    dev_free(d_spans); dev_free(d_boxes); dev_free(d_pa); dev_free(d_pb); dev_free(d_np);
    return h_np;
}

void* nurbs_gpu_pool_alloc(int64_t bytes) {
    if (bytes <= 0) return nullptr;
    void* p = nullptr;
    return (dev_alloc(&p, (size_t)bytes) == cudaSuccess) ? p : nullptr;
}
void nurbs_gpu_pool_free(void* p) { dev_free(p); }

} // extern "C"

#else // CPU fallback (Makefile.ifx)

extern "C" {

int nurbs_gpu_available() { return 0; }

int nurbs_gpu_split_y_monotone(const BezSpan* h_in, int n, BezSpan* h_out, int max_out) {
    int out_n = 0;
    for (int i = 0; i < n; ++i) {
        BezSpan tmp[5];
        const int k = split_monotone_hd(h_in[i], tmp);
        for (int j = 0; j < k; ++j) {
            if (out_n < max_out) h_out[out_n] = tmp[j];
            ++out_n;
        }
    }
    return out_n;
}

int nurbs_gpu_find_overlaps(const BezSpan* h_spans, int n, double eps,
                            int* h_pair_a, int* h_pair_b, int max_pairs) {
    std::vector<AABB2> boxes((size_t)n);
    for (int i = 0; i < n; ++i) boxes[(size_t)i] = span_aabb_hd(h_spans[i]);
    int np = 0;
    for (int i = 0; i < n; ++i) {
        const AABB2& bi = boxes[(size_t)i];
        for (int j = i + 1; j < n; ++j) {
            const AABB2& bj = boxes[(size_t)j];
            if (bi.x1 + eps < bj.x0 || bj.x1 + eps < bi.x0) continue;
            if (bi.y1 + eps < bj.y0 || bj.y1 + eps < bi.y0) continue;
            if (np < max_pairs) { h_pair_a[np] = i + 1; h_pair_b[np] = j + 1; }
            ++np;
        }
    }
    return np;
}

void* nurbs_gpu_pool_alloc(int64_t bytes) { return bytes > 0 ? std::malloc((size_t)bytes) : nullptr; }
void  nurbs_gpu_pool_free(void* p) { if (p) std::free(p); }

} // extern "C"

#endif

} // namespace nurbs_gpu
