#pragma once
// CPU reference for curved-slab Boolean (N3 validation path).
// Degree <= 3 rational Bezier, y-monotone spans, exact-ish via adaptive subdivision + robust predicates.
#include "nurbs_bezier.hpp"
#include <vector>
#include <map>
#include <queue>
#include <limits>

namespace nurbs_gpu {

struct SpanEvent { f64 y; i32 span; bool is_top; };
struct ActiveSpan { i32 id; }; // simplified; full impl keeps x-at-scanline ordering

// Compute approximate area of curved trapezoid by sampling (validation only).
inline f64 approx_area(const std::vector<BezSpan>& spans, int nsamp=1024) {
    // Green's theorem on sampled boundary - caller provides closed oriented boundary spans
    f64 a=0; 
    for(auto& s:spans){
        f64 x0,y0,x1,y1;
        for(int i=0;i<nsamp;++i){ f64 t0=i/(f64)nsamp, t1=(i+1)/(f64)nsamp; eval_bez(s,t0,x0,y0); eval_bez(s,t1,x1,y1); a += x0*y1-x1*y0; }
    }
    return 0.5*a;
}

} // namespace nurbs_gpu
