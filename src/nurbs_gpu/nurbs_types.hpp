#pragma once
// NURBS-GPU kernel core types (degree <= 3 rational NURBS/conics)
// POD/SoA, 64-bit indexing, CUDA-friendly. Matches MAGPARSER any-angle conventions.
#include <cstdint>
#include <cstddef>

namespace nurbs_gpu {

using i32 = int32_t;
using i64 = int64_t;
using u64 = uint64_t;
using f64 = double;

// Rational quadratic/cubic Bezier span in homogeneous integer-scaled or float64 coords.
// For GPU pipeline we keep float64 control points + float64 weights for N1-N3,
// with exact int64 snap at N5 output. Degree p in {1,2,3}.
struct BezSpan {
    f64 cx[4];   // control x (p+1 used)
    f64 cy[4];   // control y
    f64 cw[4];   // weights (1.0 for non-rational)
    i32 degree;  // 1,2,3
    i32 flags;   // bit0: rational, bit1: y-monotone, bit2: x-monotone
    i64 owner;   // source curve/polygon id (for PNUM/winding)
    i64 band;    // assigned Y-band
};

// Y-monotonic curved slab edge: x_L(y), x_R(y) as Bezier or line, y in [y1,y2].
struct CurvedTrap {
    f64 y1, y2;
    BezSpan left;
    BezSpan right;
    i64 winding; // packed 2-operand winding: wA + (wB<<32) or simple winding
};

// AABB for spatial indexing
struct AABB2 {
    f64 x0,y0,x1,y1;
};

// Intersection event on a monotonic span
struct XEvent {
    f64 y;
    f64 x;
    i64 span_a;
    i64 span_b;
    i32 kind; // 0=crossing, 1=tangent-touch, 2=overlap-start/end
};

} // namespace nurbs_gpu
