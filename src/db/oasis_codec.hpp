#pragma once
// OASIS (SEMI P39) byte-level codec shared by the reader (oasis.cpp), the
// writer (oasis_writer.cpp) and the lossless record scanner (oasis_rec.cpp):
// integers, strings, reals, deltas, repetitions and point lists.

#include "fracture.hpp"
#include <cstdint>
#include <cstring>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>

namespace oasis_codec {

struct ParseError { std::string msg; };
using ldbgeom::Pt;

// ---------------------------------------------------------------------------
// Byte-level decoding
// ---------------------------------------------------------------------------
struct Reader {
    const uint8_t* p; size_t n; size_t off = 0; const char* where;
    Reader(const uint8_t* p_, size_t n_, const char* where_) : p(p_), n(n_), where(where_) {}

    [[noreturn]] void fail(const std::string& m) const {
        throw ParseError{ m + " at " + where + " offset " + std::to_string(off) };
    }
    bool eof() const { return off >= n; }
    void need(uint64_t k) const { if (k > n - off) fail("truncated record"); }
    uint8_t u8() { need(1); return p[off++]; }

    // Unsigned integer (7.2.1). Values beyond 64 bits are a fatal error (7.2.3).
    // Redundant high-order zero groups are allowed (there is no length restriction).
    uint64_t uint_() {
        uint64_t v = 0;
        for (unsigned sh = 0;;) {                    // sh saturates at 70: no overflow on long zero runs
            uint8_t b = u8();
            const uint64_t g = b & 0x7f;
            if (sh >= 64) { if (g) fail("unsigned integer exceeds 64 bits"); }
            else if (sh == 63 && (g >> 1)) fail("unsigned integer exceeds 64 bits");
            else v |= g << sh;
            if (!(b & 0x80)) return v;
            if (sh < 64) sh += 7;
        }
    }
    // Sign is the least significant bit, magnitude the remaining bits.
    int64_t sint() { uint64_t u = uint_(); int64_t m = (int64_t)(u >> 1); return (u & 1) ? -m : m; }
    std::string str() {
        uint64_t l = uint_(); need(l);
        std::string s((const char*)p + off, (size_t)l); off += (size_t)l; return s;
    }
    void skip_str() { uint64_t l = uint_(); need(l); off += (size_t)l; }

    double real_of(uint64_t type) {
        switch (type) {
        case 0: return (double)uint_();
        case 1: return -(double)uint_();
        case 2: return 1.0 / denom();
        case 3: return -1.0 / denom();
        case 4: { double a = (double)uint_(), b = denom(); return a / b; }
        case 5: { double a = (double)uint_(), b = denom(); return -a / b; }
        case 6: { need(4); float f; memcpy(&f, p + off, 4); off += 4; return f; }
        case 7: { need(8); double d; memcpy(&d, p + off, 8); off += 8; return d; }
        }
        fail("invalid real type " + std::to_string(type));
    }
    double real() { return real_of(uint_()); }
    double denom() { uint64_t d = uint_(); if (d == 0) fail("real with zero denominator"); return (double)d; }   // 7.3.3

    static void octant(unsigned dir, int64_t mag, int64_t& dx, int64_t& dy) {
        static const int ox[8] = { 1, 0, -1, 0, 1, -1, -1, 1 };
        static const int oy[8] = { 0, 1, 0, -1, 1, 1, -1, -1 };
        dx = ox[dir] * mag; dy = oy[dir] * mag;
    }
    void delta2(int64_t& dx, int64_t& dy) { uint64_t u = uint_(); octant((unsigned)(u & 3), (int64_t)(u >> 2), dx, dy); }
    void delta3(int64_t& dx, int64_t& dy) { uint64_t u = uint_(); octant((unsigned)(u & 7), (int64_t)(u >> 3), dx, dy); }
    void gdelta(int64_t& dx, int64_t& dy) {
        uint64_t u = uint_();
        if (!(u & 1)) { octant((unsigned)((u >> 1) & 7), (int64_t)(u >> 4), dx, dy); return; }
        dx = (int64_t)(u >> 2); if (u & 2) dx = -dx;
        uint64_t v = uint_();
        dy = (int64_t)(v >> 1); if (v & 1) dy = -dy;
    }
};

// String classes (7.4): n-string = printable 0x21-0x7E, length > 0; a-string adds SP.
inline bool is_nstring(const std::string& s) {
    if (s.empty()) return false;
    for (unsigned char c : s) if (c < 0x21 || c > 0x7e) return false;
    return true;
}
inline bool is_astring(const std::string& s) {
    for (unsigned char c : s) if (c < 0x20 || c > 0x7e) return false;
    return true;
}

// ---------------------------------------------------------------------------
// Repetitions
// ---------------------------------------------------------------------------
struct Repetition {
    bool regular = true;                     // cols x rows lattice
    int64_t cols = 1, rows = 1;
    int64_t dx = 0, dy = 0, rdx = 0, rdy = 0;
    std::vector<Pt> offs;              // irregular: every position, first is (0,0)

    template <class F> void for_each(F&& f) const {
        if (!regular) { for (auto& o : offs) f(o.x, o.y); return; }
        for (int64_t r = 0; r < rows; ++r)
            for (int64_t c = 0; c < cols; ++c) f(c * dx + r * rdx, c * dy + r * rdy);
    }
};

inline void read_repetition(Reader& s, Repetition& rep) {
    uint64_t type = s.uint_();
    if (type == 0) return;                   // reuse the modal repetition
    rep = Repetition();
    auto irregular = [&](bool along_x, bool has_grid, bool general) {
        uint64_t count = s.uint_() + 2;
        int64_t grid = has_grid ? (int64_t)s.uint_() : 1;
        rep.regular = false;
        rep.offs.reserve((size_t)std::min<uint64_t>(count, 1u << 20));
        int64_t x = 0, y = 0;
        rep.offs.push_back({ 0, 0 });
        for (uint64_t i = 1; i < count; ++i) {
            if (general) { int64_t ddx, ddy; s.gdelta(ddx, ddy); x += ddx * grid; y += ddy * grid; }
            else if (along_x) x += (int64_t)s.uint_() * grid;
            else y += (int64_t)s.uint_() * grid;
            rep.offs.push_back({ x, y });
        }
    };
    switch (type) {
    case 1: rep.cols = (int64_t)s.uint_() + 2; rep.rows = (int64_t)s.uint_() + 2;
            rep.dx = (int64_t)s.uint_(); rep.rdy = (int64_t)s.uint_(); break;
    case 2: rep.cols = (int64_t)s.uint_() + 2; rep.dx = (int64_t)s.uint_(); break;
    case 3: rep.rows = (int64_t)s.uint_() + 2; rep.rdy = (int64_t)s.uint_(); break;
    case 4: irregular(true, false, false); break;
    case 5: irregular(true, true, false); break;
    case 6: irregular(false, false, false); break;
    case 7: irregular(false, true, false); break;
    case 8: rep.cols = (int64_t)s.uint_() + 2; rep.rows = (int64_t)s.uint_() + 2;
            s.gdelta(rep.dx, rep.dy); s.gdelta(rep.rdx, rep.rdy); break;
    case 9: rep.cols = (int64_t)s.uint_() + 2; s.gdelta(rep.dx, rep.dy); break;
    case 10: irregular(false, false, true); break;
    case 11: irregular(false, true, true); break;
    default: s.fail("invalid repetition type " + std::to_string(type));
    }
}

// ---------------------------------------------------------------------------
// Point lists: offsets from the element origin, origin itself not included
// ---------------------------------------------------------------------------
inline void read_point_list(Reader& s, bool polygon, std::vector<Pt>& out) {
    out.clear();
    uint64_t type = s.uint_(), count = s.uint_();
    out.reserve((size_t)std::min<uint64_t>(count + 1, 1u << 20));
    int64_t x = 0, y = 0, dx = 0, dy = 0;
    switch (type) {
    case 0: case 1: {
        bool horizontal = (type == 0);
        for (uint64_t i = 0; i < count; ++i) {
            int64_t d = s.sint();
            if (horizontal) x += d; else y += d;
            out.push_back({ x, y });
            horizontal = !horizontal;
        }
        // A polygon in these forms omits the vertex that closes it at right angles.
        if (polygon) out.push_back(horizontal ? Pt{ 0, y } : Pt{ x, 0 });
    } break;
    case 2: for (uint64_t i = 0; i < count; ++i) { s.delta2(dx, dy); x += dx; y += dy; out.push_back({ x, y }); } break;
    case 3: for (uint64_t i = 0; i < count; ++i) { s.delta3(dx, dy); x += dx; y += dy; out.push_back({ x, y }); } break;
    case 4: for (uint64_t i = 0; i < count; ++i) { s.gdelta(dx, dy); x += dx; y += dy; out.push_back({ x, y }); } break;
    case 5: {
        int64_t ax = 0, ay = 0;              // each delta is a change to the previous delta
        for (uint64_t i = 0; i < count; ++i) { s.gdelta(dx, dy); ax += dx; ay += dy; x += ax; y += ay; out.push_back({ x, y }); }
    } break;
    default: s.fail("invalid point-list type " + std::to_string(type));
    }
}

static inline void skip_interval(Reader& s) {
    uint64_t t = s.uint_();
    if (t == 0) return;
    if (t > 4) s.fail("invalid interval type " + std::to_string(t));
    s.uint_(); if (t == 4) s.uint_();
}

// ---------------------------------------------------------------------------
// Byte-level encoding
// ---------------------------------------------------------------------------
struct Encoder {
    std::vector<uint8_t> b;
    void u8(uint8_t v) { b.push_back(v); }
    void bytes(const void* p, size_t n) { auto* c = (const uint8_t*)p; b.insert(b.end(), c, c + n); }
    void uint_(uint64_t v) { while (v >= 0x80) { u8(uint8_t(v | 0x80)); v >>= 7; } u8(uint8_t(v)); }
    static uint64_t mag(int64_t v) { return v < 0 ? (uint64_t)0 - (uint64_t)v : (uint64_t)v; }
    void sint(int64_t v) { uint_((mag(v) << 1) | (v < 0 ? 1u : 0u)); }      // sign in the low bit
    void str(const std::string& s) { uint_(s.size()); bytes(s.data(), s.size()); }
    void real_whole(uint64_t v) { uint_(0); uint_(v); }
    void real_double(double v) { uint_(7); bytes(&v, 8); }                  // little-endian host (7.3.2)
    // Shortest exact form (7.3): whole number (type 0/1), reciprocal (2/3), else IEEE double.
    void real(double v) {
        const double a = std::fabs(v);
        if (std::isfinite(v) && a < 9.0e15 && a == std::floor(a)) { uint_(v < 0 ? 1 : 0); uint_((uint64_t)a); return; }
        if (std::isfinite(v) && a > 0 && a <= 1.0) {
            const double r = 1.0 / a;
            if (r < 9.0e15 && r == std::floor(r) && 1.0 / r == a) { uint_(v < 0 ? 3 : 2); uint_((uint64_t)r); return; }
        }
        real_double(v);
    }
    void delta1(int64_t d) { sint(d); }                                     // 7.5.2 (same as signed-integer)
    void delta2(int64_t dx, int64_t dy) {                                   // 7.5.3, one of dx/dy is 0
        unsigned dir = dx > 0 ? 0 : dy > 0 ? 1 : dx < 0 ? 2 : 3;
        uint_(((dx ? mag(dx) : mag(dy)) << 2) | dir);
    }
    void delta3(int64_t dx, int64_t dy) { uint_((oct_mag(dx, dy) << 3) | octant_dir(dx, dy)); }   // 7.5.4
    // g-delta (7.5.5): the one-integer octangular form when it applies, else the (x,y) pair.
    void gdelta(int64_t dx, int64_t dy) {
        if (octangular(dx, dy) && oct_mag(dx, dy) < (1ull << 59)) {
            uint_((oct_mag(dx, dy) << 4) | (octant_dir(dx, dy) << 1)); return;
        }
        uint_((mag(dx) << 2) | (dx < 0 ? 2u : 0u) | 1u);
        uint_((mag(dy) << 1) | (dy < 0 ? 1u : 0u));
    }
    static bool octangular(int64_t dx, int64_t dy) { return dx == 0 || dy == 0 || mag(dx) == mag(dy); }
    static uint64_t oct_mag(int64_t dx, int64_t dy) { return dx ? mag(dx) : mag(dy); }
    // 0 E, 1 N, 2 W, 3 S, 4 NE, 5 NW, 6 SW, 7 SE (zero vector: east, magnitude 0)
    static unsigned octant_dir(int64_t dx, int64_t dy) {
        if (dy == 0) return dx < 0 ? 2 : 0;
        if (dx == 0) return dy > 0 ? 1 : 3;
        if (dx > 0) return dy > 0 ? 4 : 7;
        return dy > 0 ? 5 : 6;
    }
};

// CTRAPEZOID rules (29.6-29.8), shared by the parser (oasis_parser.hpp) and the record scanner
// (oasis_rec.cpp) so both readers accept exactly the same records.
struct CtrapRule { bool no_w, no_h; };
// 29.6: types 16-19, 22, 23 and 25 have no height; types 20 and 21 no width.
inline CtrapRule ctrap_rule(uint64_t t) {
    return { t == 20 || t == 21, (t >= 16 && t <= 19) || t == 22 || t == 23 || t == 25 };
}
// 29.7: geometry-w/h always become the bounding box of the figure.
inline void ctrap_implied_box(uint64_t t, int64_t& w, int64_t& h) {
    const CtrapRule r = ctrap_rule(t);
    if (r.no_h) h = (t == 22 || t == 23) ? 2 * w : w;
    if (r.no_w) w = 2 * h;
}
// 29.8: width/height constraints of the figure.
inline bool ctrap_consistent(uint64_t t, int64_t w, int64_t h) {
    return !(((t <= 3 || t == 6 || t == 7) && w < h) || ((t == 4 || t == 5) && w < 2 * h) ||
             (((t >= 8 && t <= 11) || t == 14 || t == 15) && h < w) || ((t == 12 || t == 13) && h < 2 * w));
}

// 29.9: vertices of CTRAPEZOID types 0-23 relative to (x, y), every slanted side at 45 degrees;
// triangles repeat their last vertex (types 24, 25 are rectangles and are not in the table).
inline void ctrap_vertices(uint64_t t, int64_t w, int64_t h, int64_t V[4][2]) {
    const int64_t T[24][4][2] = {
        { {0,0}, {w,0},   {w-h,h}, {0,h}   },   //  0  horizontal, right side slanted, top short
        { {0,0}, {w-h,0}, {w,h},   {0,h}   },   //  1  right side slanted, bottom short
        { {0,0}, {w,0},   {w,h},   {h,h}   },   //  2  left side slanted, top short
        { {h,0}, {w,0},   {w,h},   {0,h}   },   //  3  left side slanted, bottom short
        { {0,0}, {w,0},   {w-h,h}, {h,h}   },   //  4  both slanted, top short
        { {h,0}, {w-h,0}, {w,h},   {0,h}   },   //  5  both slanted, bottom short
        { {0,0}, {w-h,0}, {w,h},   {h,h}   },   //  6  parallelogram leaning right
        { {h,0}, {w,0},   {w-h,h}, {0,h}   },   //  7  parallelogram leaning left
        { {0,0}, {w,0},   {w,h-w}, {0,h}   },   //  8  vertical, top slanted, right short
        { {0,0}, {w,0},   {w,h},   {0,h-w} },   //  9  top slanted, left short
        { {0,0}, {w,w},   {w,h},   {0,h}   },   // 10  bottom slanted, right short
        { {0,w}, {w,0},   {w,h},   {0,h}   },   // 11  bottom slanted, left short
        { {0,0}, {w,w},   {w,h-w}, {0,h}   },   // 12  both slanted, right short
        { {0,w}, {w,0},   {w,h},   {0,h-w} },   // 13  both slanted, left short
        { {0,0}, {w,w},   {w,h},   {0,h-w} },   // 14  parallelogram rising
        { {0,w}, {w,0},   {w,h-w}, {0,h}   },   // 15  parallelogram falling
        { {0,0}, {w,0},   {0,w},   {0,w}   },   // 16  triangle, right angle bottom left
        { {0,0}, {w,w},   {0,w},   {0,w}   },   // 17  right angle top left
        { {0,0}, {w,0},   {w,w},   {w,w}   },   // 18  right angle bottom right
        { {0,w}, {w,0},   {w,w},   {w,w}   },   // 19  right angle top right
        { {0,0}, {2*h,0}, {h,h},   {h,h}   },   // 20  apex up
        { {0,h}, {h,0},   {2*h,h}, {2*h,h} },   // 21  apex down
        { {0,0}, {w,w},   {0,2*w}, {0,2*w} },   // 22  apex right
        { {0,w}, {w,0},   {w,2*w}, {w,2*w} },   // 23  apex left
    };
    for (int i = 0; i < 4; ++i) { V[i][0] = T[t][i][0]; V[i][1] = T[t][i][1]; }
}

} // namespace oasis_codec
