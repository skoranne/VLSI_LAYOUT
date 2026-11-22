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

    uint64_t uint_() {
        uint64_t v = 0;
        for (int sh = 0; sh < 70; sh += 7) {
            uint8_t b = u8();
            if (sh < 64) v |= uint64_t(b & 0x7f) << sh;
            if (!(b & 0x80)) return v;
        }
        fail("unsigned integer too long");
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
        case 2: return 1.0 / (double)uint_();
        case 3: return -1.0 / (double)uint_();
        case 4: { double a = (double)uint_(), b = (double)uint_(); return a / b; }
        case 5: { double a = (double)uint_(), b = (double)uint_(); return -a / b; }
        case 6: { need(4); float f; memcpy(&f, p + off, 4); off += 4; return f; }
        case 7: { need(8); double d; memcpy(&d, p + off, 8); off += 8; return d; }
        }
        fail("invalid real type " + std::to_string(type));
    }
    double real() { return real_of(uint_()); }

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
    void real_double(double v) { uint_(7); bytes(&v, 8); }
    void gdelta(int64_t dx, int64_t dy) {                                   // general form
        uint_((mag(dx) << 2) | (dx < 0 ? 2u : 0u) | 1u);
        uint_((mag(dy) << 1) | (dy < 0 ? 1u : 0u));
    }
};

} // namespace oasis_codec
