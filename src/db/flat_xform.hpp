#pragma once
// Flattening transforms shared by the hierarchical iterators (layoutdb.cpp, layoutdb_hier.cpp).
//
// A placement maps a child point p to  mag * R(angle) * Mx * p + origin  (Mx: reflect about
// the x axis when mirror_x is set).  Nested placements are composed as one affine map in
// double precision, and a point is rounded to the grid exactly once, when it is emitted
// (half away from zero, as KLayout does).  Rounding the composed origin at every level
// instead would round two cells' copies of the same world point differently, opening
// 1-dbu gaps or overlaps between abutting shapes of different cells.
//
// Multiples of 90 degrees with unit magnification use exact cos/sin (0, +-1), so the
// matrix entries and the composed translations are integers held exactly in doubles
// (|coordinates| < 2^53) and the integer fast path is taken.

#include <cmath>
#include <cstdint>
#include <vector>
#include <algorithm>
#include "layoutdb_internal.hpp"
#include "fracture.hpp"

namespace ldbflat {

struct Affine {
    double a = 1, b = 0, c = 0, d = 1;   // [x'; y'] = [a b; c d] [x; y] + [tx; ty]
    double tx = 0, ty = 0;

    // Exact integer map: unit magnification, angle a multiple of 90, integral translation.
    bool orthogonal(int64_t m[4], int64_t& itx, int64_t& ity) const {
        const double e[4] = { a, b, c, d };
        for (int i = 0; i < 4; ++i) {
            if (e[i] != 0.0 && e[i] != 1.0 && e[i] != -1.0) return false;
            m[i] = (int64_t)e[i];
        }
        if (tx != std::floor(tx) || ty != std::floor(ty)) return false;
        itx = (int64_t)tx; ity = (int64_t)ty;
        return true;
    }
    void apply(int64_t x, int64_t y, int64_t& ox, int64_t& oy) const {
        ox = (int64_t)std::llround(a * (double)x + b * (double)y + tx);
        oy = (int64_t)std::llround(c * (double)x + d * (double)y + ty);
    }
};

// cos and sin of an angle in degrees, exact for multiples of 90.
inline void cos_sin_deg(double deg, double& cs, double& sn) {
    const double q = deg / 90.0;
    if (q == std::floor(q)) {
        const int k = (int)(((long long)q % 4 + 4) % 4);
        static const double c4[4] = { 1, 0, -1, 0 }, s4[4] = { 0, 1, 0, -1 };
        cs = c4[k]; sn = s4[k];
        return;
    }
    const double r = deg * M_PI / 180.0;
    cs = std::cos(r); sn = std::sin(r);
}

// Element (col,row) of placement `t` as an affine map.
inline Affine from_placement(const LdbTransform& t, int64_t col = 0, int64_t row = 0) {
    double cs, sn; cos_sin_deg(t.angle_deg, cs, sn);
    const double my = t.mirror_x ? -1.0 : 1.0;
    Affine r;
    r.a = t.mag * cs;  r.b = -t.mag * sn * my;
    r.c = t.mag * sn;  r.d =  t.mag * cs * my;
    r.tx = (double)t.ox + (double)col * (double)t.dx + (double)row * (double)t.row_dx;
    r.ty = (double)t.oy + (double)col * (double)t.dy + (double)row * (double)t.row_dy;
    return r;
}

// parent o child: first the child placement, then the parent.
inline Affine compose(const Affine& p, const Affine& ch) {
    Affine r;
    r.a = p.a * ch.a + p.b * ch.c;  r.b = p.a * ch.b + p.b * ch.d;
    r.c = p.c * ch.a + p.d * ch.c;  r.d = p.c * ch.b + p.d * ch.d;
    r.tx = p.a * ch.tx + p.b * ch.ty + p.tx;
    r.ty = p.c * ch.tx + p.d * ch.ty + p.ty;
    return r;
}

// Outline of a path (kind 1 LdbPolyElem) in its own coordinates, as one or more polygons.
// Manhattan paths: the disjoint rectangles of path_to_rects (exact). Slanted paths: one mitred
// outline polygon built like KLayout's Path::polygon (rounded offsets, rounded miter points).
// Zero-width paths have no area and give nothing.
inline void path_outline(const LdbPolyElem& p, std::vector<std::vector<LdbPoint>>& out) {
    const int64_t hw = p.width / 2;
    if (hw <= 0 || p.pts.size() < 2) return;
    std::vector<ldbgeom::Pt> v;
    for (auto& q : p.pts) if (v.empty() || v.back().x != q.x || v.back().y != q.y) v.push_back({ q.x, q.y });
    if (v.size() < 2) return;
    std::vector<ldbgeom::Rect> rects;
    if (ldbgeom::path_to_rects(v, hw, p.ext_begin, p.ext_end, rects)) {
        for (auto& r : rects) out.push_back({ { r[0], r[1] }, { r[2], r[1] }, { r[2], r[3] }, { r[0], r[3] } });
        return;
    }
    // One outline polygon, KLayout's construction: every segment's offset vector w*n is rounded
    // to integers, the offset lines of adjacent segments are intersected (miter) and the
    // intersection rounded; turns sharper than 160 degrees are bevelled. Ends are extended along
    // the end segments by the rounded extension vector.
    const size_t ns = v.size() - 1;
    const double w = (double)hw;
    std::vector<int64_t> ox(ns), oy(ns), ddx(ns), ddy(ns);
    std::vector<double> ux(ns), uy(ns);
    for (size_t i = 0; i < ns; ++i) {
        ddx[i] = v[i + 1].x - v[i].x; ddy[i] = v[i + 1].y - v[i].y;
        const double l = std::sqrt((double)ddx[i] * ddx[i] + (double)ddy[i] * ddy[i]);
        ux[i] = ddx[i] / l; uy[i] = ddy[i] / l;
        ox[i] = std::llround(-uy[i] * w); oy[i] = std::llround(ux[i] * w);
    }
    std::vector<LdbPoint> left, right;
    auto side = [&](int64_t sgn, std::vector<LdbPoint>& o) {
        const int64_t ebx = std::llround(ux[0] * (double)p.ext_begin), eby = std::llround(uy[0] * (double)p.ext_begin);
        o.push_back({ v[0].x - ebx + sgn * ox[0], v[0].y - eby + sgn * oy[0] });
        for (size_t i = 1; i < ns; ++i) {
            const double c = ux[i - 1] * ux[i] + uy[i - 1] * uy[i];
            const int64_t ax = v[i].x + sgn * ox[i - 1], ay = v[i].y + sgn * oy[i - 1];
            const int64_t bx = v[i].x + sgn * ox[i], by = v[i].y + sgn * oy[i];
            const long double den = (long double)ddx[i - 1] * ddy[i] - (long double)ddy[i - 1] * ddx[i];
            if (den != 0 && 1.0 + c > 2.0 * std::pow(std::cos(80.0 * M_PI / 180.0), 2)) {
                const long double t = ((long double)(bx - ax) * ddy[i] - (long double)(by - ay) * ddx[i]) / den;
                o.push_back({ (int64_t)std::llround((long double)ax + t * ddx[i - 1]), (int64_t)std::llround((long double)ay + t * ddy[i - 1]) });
            } else if (den == 0 && c > 0) {
                o.push_back({ ax, ay });                                   // collinear joint
            } else {
                o.push_back({ ax, ay }); o.push_back({ bx, by });
            }
        }
        const size_t k = ns - 1;
        const int64_t eex = std::llround(ux[k] * (double)p.ext_end), eey = std::llround(uy[k] * (double)p.ext_end);
        o.push_back({ v[ns].x + eex + sgn * ox[k], v[ns].y + eey + sgn * oy[k] });
    };
    side(+1, left); side(-1, right);
    std::vector<LdbPoint> poly(left);
    for (size_t i = right.size(); i-- > 0;) poly.push_back(right[i]);
    out.push_back(std::move(poly));
}

// Box (x1,y1,x2,y2) under a map that is not orthogonal: the rotated rectangle as rectangle
// strips (ldbgeom::fracture_general, LDB_SLANT_STRIPS per slab). An approximation, used by
// the box iterators only; the polygon iterators emit the exact rotated outline.
inline void rotated_box_strips(const Affine& t, int64_t x1, int64_t y1, int64_t x2, int64_t y2,
                               std::vector<ldbgeom::Rect>& out) {
    std::vector<ldbgeom::Pt> v(4);
    t.apply(x1, y1, v[0].x, v[0].y); t.apply(x2, y1, v[1].x, v[1].y);
    t.apply(x2, y2, v[2].x, v[2].y); t.apply(x1, y2, v[3].x, v[3].y);
    out.clear();
    if (ldbgeom::is_manhattan(v)) {
        out.push_back({ std::min({ v[0].x, v[1].x, v[2].x, v[3].x }), std::min({ v[0].y, v[1].y, v[2].y, v[3].y }),
                        std::max({ v[0].x, v[1].x, v[2].x, v[3].x }), std::max({ v[0].y, v[1].y, v[2].y, v[3].y }) });
        return;
    }
    ldbgeom::fracture_general(v, out);
}

} // namespace ldbflat
