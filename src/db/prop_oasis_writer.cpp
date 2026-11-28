// File   : prop_oasis_writer.cpp
// Author : Sandeep Koranne (C) 2026. All rights reserved.
// Purpose: OASIS (SEMI P39) writer for MAGPARSER layers that carry per-polygon properties.
//
// Input per layer: the polygons of a healed (pairwise disjoint) Manhattan box set, given
// as boxes grouped by polygon, plus typed property columns (one value per polygon).
// Output, one CELL:
//   * each polygon becomes a RECTANGLE (single-box outline) or a POLYGON.  Outlines are
//     traced from the boxes (boundary = edges covered on exactly one side); holes are
//     joined to the outer contour by zero-width keyhole cuts, as GDSII/KLayout expect.
//   * every element is followed by its PROPERTY records: INT -> signed integer (type 9),
//     FLOAT -> IEEE double (type 7), STRING -> PROPSTRING reference (types 13/14/15 by
//     character class), STRUCT -> one PROPERTY whose value list is
//     [field-name (PROPSTRING ref), field-value, ...] for the fields present.
//     A record identical to the previous one is written as PROPERTY-REPEAT (29).
//   * pattern recognition: polygons with identical shape *and* identical property
//     records are grouped and their placements decomposed into OASIS repetitions
//     (type 1 grids, type 2/3 rows/columns, type 4/6/10 irregular lists).  A PROPERTY
//     after a repeated element applies to every instance, so instances that differ in
//     any property value are never merged.
//   * modal variables (layer, datatype, width, height, point list, repetition, xy in
//     XYRELATIVE mode) suppress repeated fields.
//   * the cell body is deflate-compressed into CBLOCK records (comp-type 0).
//   * STRICT mode: START offset-flag = 1, the CELLNAME/PROPNAME/PROPSTRING/LAYERNAME
//     tables are written as contiguous blocks of explicit-reference records after the
//     cell, and END carries their strict table offsets.  END is padded to 256 bytes.

#include "oasis_codec.hpp"
#include <zlib.h>
#if defined(LAYOUTDB_USE_NVCOMP)
#include <cuda_runtime.h>
#include <nvcomp.h>
#include <nvcomp/deflate.h>
#endif
#include <algorithm>
#include <array>
#include <climits>
#include <tuple>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>

namespace {

using oasis_codec::Encoder;

enum : int { PK_INT = 1, PK_FLOAT = 2, PK_STRING = 3, PK_STRUCT = 4 };

struct P { int64_t x, y; bool operator==(const P& o) const { return x == o.x && y == o.y; } };

// ---------------------------------------------------------------------------
// Outline tracing for a set of disjoint Manhattan boxes forming one polygon
// ---------------------------------------------------------------------------
struct Edge { P a, b; bool used = false; };

static void add_cover_edges(std::map<int64_t, std::vector<std::pair<int64_t, int64_t>>>& pos,
                            std::map<int64_t, std::vector<std::pair<int64_t, int64_t>>>& neg,
                            bool horizontal, std::vector<Edge>& out) {
    // pos[c]: intervals on line c covered on the "positive" side, neg[c]: on the negative side.
    std::vector<int64_t> keys;
    for (auto& kv : pos) keys.push_back(kv.first);
    for (auto& kv : neg) keys.push_back(kv.first);
    std::sort(keys.begin(), keys.end());
    keys.erase(std::unique(keys.begin(), keys.end()), keys.end());
    for (int64_t c : keys) {
        std::vector<std::pair<int64_t, int>> ev;     // coordinate, +-1 (pos) / +-2 (neg)
        auto ip = pos.find(c), in = neg.find(c);
        if (ip != pos.end()) for (auto& iv : ip->second) { ev.push_back({ iv.first, +1 }); ev.push_back({ iv.second, -1 }); }
        if (in != neg.end()) for (auto& iv : in->second) { ev.push_back({ iv.first, +2 }); ev.push_back({ iv.second, -2 }); }
        std::sort(ev.begin(), ev.end());
        int cp = 0, cn = 0;
        size_t i = 0;
        while (i < ev.size()) {
            int64_t t = ev[i].first;
            while (i < ev.size() && ev[i].first == t) {
                int d = ev[i].second;
                if (d == 1 || d == -1) cp += d; else cn += d / 2;
                ++i;
            }
            if (i >= ev.size()) break;
            int64_t t2 = ev[i].first;
            bool a = cp > 0, b = cn > 0;
            if (a == b || t2 <= t) continue;
            // Interior on the left of each directed edge (counter-clockwise outer contour).
            if (horizontal) {         // line y=c; pos = interior above, neg = interior below
                if (a) out.push_back({ { t, c }, { t2, c } });          // east
                else   out.push_back({ { t2, c }, { t, c } });          // west
            } else {                  // line x=c; pos = interior right, neg = interior left
                if (a) out.push_back({ { c, t2 }, { c, t } });          // south
                else   out.push_back({ { c, t }, { c, t2 } });          // north
            }
        }
    }
}

struct PHash { size_t operator()(const P& p) const { return std::hash<int64_t>()(p.x * 1000003LL ^ p.y); } };

static int dir_of(const P& a, const P& b) {   // 0 E, 1 N, 2 W, 3 S
    if (b.x > a.x) return 0;
    if (b.y > a.y) return 1;
    if (b.x < a.x) return 2;
    return 3;
}

static int64_t area2(const std::vector<P>& L) {
    __int128 s = 0;
    for (size_t i = 0, n = L.size(); i < n; ++i) {
        const P& a = L[i]; const P& b = L[(i + 1) % n];
        s += (__int128)a.x * b.y - (__int128)b.x * a.y;
    }
    return s > 0 ? 1 : (s < 0 ? -1 : 0);
}

static void simplify(std::vector<P>& L) {
    bool changed = true;
    while (changed && L.size() >= 3) {
        changed = false;
        std::vector<P> o;
        size_t n = L.size();
        for (size_t i = 0; i < n; ++i) {
            const P& a = L[(i + n - 1) % n]; const P& b = L[i]; const P& c = L[(i + 1) % n];
            bool col = (a.x == b.x && b.x == c.x) || (a.y == b.y && b.y == c.y);
            if (col) { changed = true; continue; }
            o.push_back(b);
        }
        if (o.size() < 3) break;
        L.swap(o);
    }
}

// Boundary loops of one polygon (disjoint boxes), interior on the left of every edge:
// the outer loop counter-clockwise, holes clockwise, corner contacts walked as right turns
// (one self-touching loop). Collinear vertices removed. *perimeter = total boundary length
// (valid even when tracing fails). Returns false on a broken boundary.
static bool trace_loops(const std::vector<std::array<int64_t, 4>>& boxes,
                        std::vector<std::vector<P>>& loops, int64_t* perimeter) {
    loops.clear();
    std::map<int64_t, std::vector<std::pair<int64_t, int64_t>>> hpos, hneg, vpos, vneg;
    for (auto& b : boxes) {
        hpos[b[1]].push_back({ b[0], b[2] });   // bottom edge: interior above
        hneg[b[3]].push_back({ b[0], b[2] });   // top edge: interior below
        vpos[b[0]].push_back({ b[1], b[3] });   // left edge: interior right
        vneg[b[2]].push_back({ b[1], b[3] });   // right edge: interior left
    }
    std::vector<Edge> E;
    add_cover_edges(hpos, hneg, true, E);
    add_cover_edges(vpos, vneg, false, E);
    if (perimeter) {
        int64_t s = 0;
        for (auto& e : E) s += std::llabs(e.b.x - e.a.x) + std::llabs(e.b.y - e.a.y);
        *perimeter = s;
    }
    // Split every edge at the vertices of the other orientation lying on it, so that each
    // vertex has matching in/out edges (corner-sharing boundaries).
    std::unordered_map<P, std::vector<size_t>, PHash> outs;
    {
        std::vector<P> verts;
        for (auto& e : E) { verts.push_back(e.a); verts.push_back(e.b); }
        std::sort(verts.begin(), verts.end(), [](const P& a, const P& b) { return a.x != b.x ? a.x < b.x : a.y < b.y; });
        verts.erase(std::unique(verts.begin(), verts.end()), verts.end());
        std::map<int64_t, std::vector<int64_t>> by_y, by_x;
        for (auto& v : verts) { by_y[v.y].push_back(v.x); by_x[v.x].push_back(v.y); }
        for (auto& kv : by_y) std::sort(kv.second.begin(), kv.second.end());
        for (auto& kv : by_x) std::sort(kv.second.begin(), kv.second.end());
        std::vector<Edge> S;
        for (auto& e : E) {
            if (e.a.y == e.b.y) {
                auto& xs = by_y[e.a.y];
                int64_t lo = std::min(e.a.x, e.b.x), hi = std::max(e.a.x, e.b.x);
                std::vector<int64_t> cut;
                for (auto it = std::upper_bound(xs.begin(), xs.end(), lo); it != xs.end() && *it < hi; ++it) cut.push_back(*it);
                if (e.b.x < e.a.x) std::reverse(cut.begin(), cut.end());
                P cur = e.a;
                for (int64_t x : cut) { S.push_back({ cur, { x, cur.y } }); cur = { x, cur.y }; }
                S.push_back({ cur, e.b });
            } else {
                auto& ys = by_x[e.a.x];
                int64_t lo = std::min(e.a.y, e.b.y), hi = std::max(e.a.y, e.b.y);
                std::vector<int64_t> cut;
                for (auto it = std::upper_bound(ys.begin(), ys.end(), lo); it != ys.end() && *it < hi; ++it) cut.push_back(*it);
                if (e.b.y < e.a.y) std::reverse(cut.begin(), cut.end());
                P cur = e.a;
                for (int64_t y : cut) { S.push_back({ cur, { cur.x, y } }); cur = { cur.x, y }; }
                S.push_back({ cur, e.b });
            }
        }
        E.swap(S);
    }
    for (size_t i = 0; i < E.size(); ++i) outs[E[i].a].push_back(i);
    for (size_t s0 = 0; s0 < E.size(); ++s0) {
        if (E[s0].used) continue;
        std::vector<P> L;
        size_t cur = s0;
        while (true) {
            E[cur].used = true;
            L.push_back(E[cur].a);
            const P& v = E[cur].b;
            if (v == E[s0].a) break;
            int din = dir_of(E[cur].a, E[cur].b);
            size_t best = SIZE_MAX; int best_rank = 99;
            for (size_t j : outs[v]) {
                if (E[j].used) continue;
                int dout = dir_of(E[j].a, E[j].b);
                // Prefer right turns: at a vertex where parts of the polygon (or a hole and the
                // outer boundary) touch only at a corner, the walk continues into the other part,
                // so a corner-connected polygon (MAGPARSER PNum joins touching corners) is one
                // closed outline that touches itself at that vertex.
                int turn = (dout - din + 4) % 4;            // 1 left, 0 straight, 3 right
                int rank = turn == 3 ? 0 : (turn == 0 ? 1 : 2);
                if (rank < best_rank) { best_rank = rank; best = j; }
            }
            if (best == SIZE_MAX) return false;             // broken boundary
            cur = best;
        }
        simplify(L);
        if (L.size() >= 4) loops.push_back(std::move(L));
    }
    return !loops.empty();
}

// Returns the outline (outer contour with holes keyholed in); empty on failure.
static std::vector<P> trace_outline(const std::vector<std::array<int64_t, 4>>& boxes) {
    std::vector<std::vector<P>> loops;
    if (!trace_loops(boxes, loops, nullptr)) return {};
    std::vector<P> outer;
    std::vector<std::vector<P>> holes;
    for (auto& L : loops) {
        if (area2(L) > 0) { if (outer.empty()) outer = L; else return {}; }   // one polygon = one outer loop
        else holes.push_back(L);
    }
    if (outer.empty()) return {};
    // Keyhole the holes in order of their leftmost (then lowest) vertex.
    auto leftmost = [](const std::vector<P>& L) {
        size_t k = 0;
        for (size_t i = 1; i < L.size(); ++i) if (L[i].x < L[k].x || (L[i].x == L[k].x && L[i].y < L[k].y)) k = i;
        return k;
    };
    std::sort(holes.begin(), holes.end(), [&](const std::vector<P>& a, const std::vector<P>& b) {
        const P& pa = a[leftmost(a)]; const P& pb = b[leftmost(b)];
        return pa.x != pb.x ? pa.x < pb.x : pa.y < pb.y;
    });
    for (auto& H : holes) {
        size_t hk = leftmost(H);
        P h = H[hk];
        // nearest vertical edge of the current contour strictly left of h spanning h.y
        size_t best = SIZE_MAX; int64_t bx = INT64_MIN;
        size_t n = outer.size();
        for (size_t i = 0; i < n; ++i) {
            const P& a = outer[i]; const P& b = outer[(i + 1) % n];
            if (a.x != b.x || a.x >= h.x) continue;
            int64_t lo = std::min(a.y, b.y), hi = std::max(a.y, b.y);
            if (h.y < lo || h.y > hi) continue;
            if (a.x > bx) { bx = a.x; best = i; }
        }
        if (best == SIZE_MAX) return {};
        P hit{ bx, h.y };
        std::vector<P> o;
        o.reserve(outer.size() + H.size() + 3);
        for (size_t i = 0; i <= best; ++i) o.push_back(outer[i]);
        if (!(hit == outer[best])) o.push_back(hit);
        for (size_t i = 0; i < H.size(); ++i) o.push_back(H[(hk + i) % H.size()]);
        o.push_back(h);
        o.push_back(hit);
        for (size_t i = best + 1; i < n; ++i) o.push_back(outer[i]);
        // drop consecutive duplicates (hit on a vertex)
        std::vector<P> d;
        for (auto& p : o) if (d.empty() || !(d.back() == p)) d.push_back(p);
        while (d.size() > 1 && d.front() == d.back()) d.pop_back();
        outer.swap(d);
    }
    return outer;
}

// Narrowest line end over all loops: an edge whose two end vertices are convex corners
// (left turns: interior is on the left of every loop, holes included) and which is strictly
// shorter than both neighbouring edges. width = its length, len = shorter neighbour,
// dir = outward normal (0 E, 1 N, 2 W, 3 S). Ties: lowest endpoint (x, then y), then dir.
// dir = -1 when the polygon has no line end.
static void line_end(const std::vector<std::vector<P>>& loops, int64_t& w, int64_t& len, int& dir) {
    w = 0; len = 0; dir = -1;
    P bp{ 0, 0 };
    for (auto& L : loops) {
        size_t n = L.size();
        if (n < 4) continue;
        auto elen = [&](size_t i) { const P& a = L[i % n]; const P& b = L[(i + 1) % n];
                                    return std::llabs(b.x - a.x) + std::llabs(b.y - a.y); };
        auto edir = [&](size_t i) { return dir_of(L[i % n], L[(i + 1) % n]); };
        for (size_t i = 0; i < n; ++i) {
            size_t ip = (i + n - 1) % n, in = (i + 1) % n;
            int dp = edir(ip), d = edir(i), dn = edir(in);
            if ((d - dp + 4) % 4 != 1 || (dn - d + 4) % 4 != 1) continue;   // both corners convex
            int64_t li = elen(i), lp = elen(ip), ln = elen(in);
            if (!(li < lp && li < ln)) continue;
            const P& a = L[i]; const P& b = L[in];
            P lo = (a.x < b.x || (a.x == b.x && a.y < b.y)) ? a : b;
            int od = (d + 3) % 4;                                            // outward normal
            bool better = dir < 0 || li < w ||
                (li == w && (lo.x < bp.x || (lo.x == bp.x && (lo.y < bp.y || (lo.y == bp.y && od < dir)))));
            if (better) { w = li; len = std::min(lp, ln); dir = od; bp = lo; }
        }
    }
}

// ---------------------------------------------------------------------------
// Writer state
// ---------------------------------------------------------------------------
struct Column {
    std::string name;
    int kind = 0;
    std::vector<uint8_t> present;
    std::vector<int64_t> iv;
    std::vector<double> rv;
    std::vector<std::string> sv;
    struct Field { std::string name; int kind; std::vector<uint8_t> present; std::vector<int64_t> iv; std::vector<double> rv; std::vector<std::string> sv; };
    std::vector<Field> fields;                   // STRUCT
};

struct Shape {
    bool rect = false;
    int64_t x = 0, y = 0;                        // rect lower-left / polygon first point
    int64_t w = 0, h = 0;
    std::vector<P> pts;                          // polygon, relative to (x,y), excluding origin
};

struct Rep {
    int type = 0;                                // 0 = none
    int64_t nx = 1, ny = 1, dx = 0, dy = 0;
    std::vector<int64_t> spaces;                 // types 4/6
    std::vector<P> deltas;                       // type 10
    bool operator==(const Rep& o) const {
        return type == o.type && nx == o.nx && ny == o.ny && dx == o.dx && dy == o.dy && spaces == o.spaces &&
               deltas.size() == o.deltas.size() && std::equal(deltas.begin(), deltas.end(), o.deltas.begin());
    }
};

struct OasisPropWriter {
    std::string path, cell;
    uint64_t grid = 1000;
    bool use_cblock = true, use_rep = true, strict = true;
    int cblock_backend = 2;                      // 0 cpu(zlib), 1 nvcomp(gpu), 2 auto
    std::string err;

    std::map<std::string, uint64_t> propnames, propstrings;
    std::vector<std::pair<std::string, std::pair<int64_t, int64_t>>> layernames;
    Encoder body;                                // cell contents (uncompressed record stream)
    std::vector<size_t> rec_starts;              // offset of every record in body (CBLOCK split points)
    size_t cblock_bytes = 4u << 20;              // uncompressed bytes per CBLOCK (FPO_CBLOCK_BYTES)

    // modal state
    bool m_layer = false, m_dt = false, m_w = false, m_h = false, m_pts = false, m_rep = false;
    uint64_t ml = 0, md = 0; int64_t mw = 0, mh = 0, mx = 0, my = 0;
    std::vector<P> mpts; Rep mrep;
    std::string last_prop;

    // current layer
    int64_t layer = 0, datatype = 0, npoly = 0;
    std::vector<int64_t> poly_start;             // 0-based, npoly+1
    std::vector<int64_t> boxes;
    std::vector<Column> cols;

    uint64_t propname_ref(const std::string& s) {
        auto it = propnames.find(s);
        if (it != propnames.end()) return it->second;
        uint64_t r = propnames.size(); propnames[s] = r; return r;
    }
    uint64_t propstring_ref(const std::string& s) {
        auto it = propstrings.find(s);
        if (it != propstrings.end()) return it->second;
        uint64_t r = propstrings.size(); propstrings[s] = r; return r;
    }
    static int str_class(const std::string& s) {         // 15 n-string, 13 a-string, 14 b-string
        bool n = !s.empty(), a = true;
        for (unsigned char c : s) {
            if (c < 0x21 || c > 0x7e) n = false;
            if (c < 0x20 || c > 0x7e) a = false;
        }
        return n ? 15 : (a ? 13 : 14);
    }
    void value_scalar(Encoder& e, int kind, int64_t i, double r, const std::string& s) {
        if (kind == PK_INT) { e.uint_(9); e.sint(i); }
        else if (kind == PK_FLOAT) { e.real_double(r); }
        else { e.uint_((uint64_t)str_class(s)); e.uint_(propstring_ref(s)); }
    }
    void property_header(Encoder& e, uint64_t name_ref, uint64_t nvalues) {
        e.u8(28);
        uint8_t info = 0x06;                             // C=1 name present, N=1 by reference
        if (nvalues < 15) info |= (uint8_t)(nvalues << 4); else info |= 0xF0;
        e.u8(info);
        e.uint_(name_ref);
        if (nvalues >= 15) e.uint_(nvalues);
    }
    // All PROPERTY records of polygon p, as one byte string of complete records.
    std::string props_of(int64_t p) {
        std::string out;
        for (auto& c : cols) {
            if (!c.present[p]) continue;
            Encoder e;
            if (c.kind == PK_STRUCT) {
                std::vector<size_t> fl;
                for (size_t j = 0; j < c.fields.size(); ++j) if (c.fields[j].present[p]) fl.push_back(j);
                property_header(e, propname_ref(c.name), 2 * fl.size());
                for (size_t j : fl) {
                    auto& f = c.fields[j];
                    e.uint_((uint64_t)str_class(f.name)); e.uint_(propstring_ref(f.name));
                    value_scalar(e, f.kind, f.kind == PK_INT ? f.iv[p] : 0, f.kind == PK_FLOAT ? f.rv[p] : 0.0,
                                 f.kind == PK_STRING ? f.sv[p] : std::string());
                }
            } else {
                property_header(e, propname_ref(c.name), 1);
                value_scalar(e, c.kind, c.kind == PK_INT ? c.iv[p] : 0, c.kind == PK_FLOAT ? c.rv[p] : 0.0,
                             c.kind == PK_STRING ? c.sv[p] : std::string());
            }
            uint32_t len = (uint32_t)e.b.size();
            out.append((const char*)&len, 4);            // length-prefixed records
            out.append((const char*)e.b.data(), e.b.size());
        }
        return out;
    }

    bool shape_of(int64_t p, Shape& s) {
        int64_t b0 = poly_start[p], b1 = poly_start[p + 1];
        if (b1 - b0 == 1) {
            const int64_t* b = &boxes[4 * b0];
            s.rect = true; s.x = b[0]; s.y = b[1]; s.w = b[2] - b[0]; s.h = b[3] - b[1];
            return s.w > 0 && s.h > 0;
        }
        std::vector<std::array<int64_t, 4>> bx;
        for (int64_t i = b0; i < b1; ++i) bx.push_back({ boxes[4 * i], boxes[4 * i + 1], boxes[4 * i + 2], boxes[4 * i + 3] });
        std::vector<P> L = trace_outline(bx);
        if (L.size() < 4) return false;
        if (L.size() == 4) {                              // union is a rectangle
            int64_t x1 = L[0].x, y1 = L[0].y, x2 = x1, y2 = y1;
            for (auto& q : L) { x1 = std::min(x1, q.x); y1 = std::min(y1, q.y); x2 = std::max(x2, q.x); y2 = std::max(y2, q.y); }
            s.rect = true; s.x = x1; s.y = y1; s.w = x2 - x1; s.h = y2 - y1;
            return true;
        }
        // canonical start: lowest-leftmost vertex
        size_t k = 0;
        for (size_t i = 1; i < L.size(); ++i) if (L[i].y < L[k].y || (L[i].y == L[k].y && L[i].x < L[k].x)) k = i;
        std::rotate(L.begin(), L.begin() + (long)k, L.end());
        s.rect = false; s.x = L[0].x; s.y = L[0].y;
        s.pts.clear();
        for (size_t i = 1; i < L.size(); ++i) s.pts.push_back({ L[i].x - s.x, L[i].y - s.y });
        return true;
    }

    static std::string shape_key(const Shape& s) {
        std::string k;
        if (s.rect) { k = "R"; k.append((const char*)&s.w, 8); k.append((const char*)&s.h, 8); }
        else { k = "P"; for (auto& q : s.pts) { k.append((const char*)&q.x, 8); k.append((const char*)&q.y, 8); } }
        return k;
    }

    // ---- repetition encoding
    void write_rep(Encoder& e, const Rep& r) {
        if (m_rep && r == mrep) { e.uint_(0); return; }
        switch (r.type) {
        case 1: e.uint_(1); e.uint_((uint64_t)(r.nx - 2)); e.uint_((uint64_t)(r.ny - 2)); e.uint_((uint64_t)r.dx); e.uint_((uint64_t)r.dy); break;
        case 2: e.uint_(2); e.uint_((uint64_t)(r.nx - 2)); e.uint_((uint64_t)r.dx); break;
        case 3: e.uint_(3); e.uint_((uint64_t)(r.ny - 2)); e.uint_((uint64_t)r.dy); break;
        case 4: e.uint_(4); e.uint_(r.spaces.size() - 1); for (auto v : r.spaces) e.uint_((uint64_t)v); break;
        case 6: e.uint_(6); e.uint_(r.spaces.size() - 1); for (auto v : r.spaces) e.uint_((uint64_t)v); break;
        case 10: e.uint_(10); e.uint_(r.deltas.size() - 1); for (auto& d : r.deltas) e.gdelta(d.x, d.y); break;
        }
        mrep = r; m_rep = true;
    }

    // Decompose a set of placements into (origin, repetition) pairs.
    static std::vector<std::pair<P, Rep>> decompose(std::vector<P> pts) {
        std::vector<std::pair<P, Rep>> out;
        std::sort(pts.begin(), pts.end(), [](const P& a, const P& b) { return a.y != b.y ? a.y < b.y : a.x < b.x; });
        pts.erase(std::unique(pts.begin(), pts.end()), pts.end());
        if (pts.size() == 1) { out.push_back({ pts[0], Rep() }); return out; }
        // full lattice?
        {
            std::vector<int64_t> xs, ys;
            for (auto& p : pts) { xs.push_back(p.x); ys.push_back(p.y); }
            std::sort(xs.begin(), xs.end()); xs.erase(std::unique(xs.begin(), xs.end()), xs.end());
            std::sort(ys.begin(), ys.end()); ys.erase(std::unique(ys.begin(), ys.end()), ys.end());
            auto uniform = [](const std::vector<int64_t>& v) {
                for (size_t i = 2; i < v.size(); ++i) if (v[i] - v[i - 1] != v[1] - v[0]) return false;
                return true;
            };
            if (xs.size() * ys.size() == pts.size() && uniform(xs) && uniform(ys)) {
                Rep r;
                r.nx = (int64_t)xs.size(); r.ny = (int64_t)ys.size();
                r.dx = xs.size() > 1 ? xs[1] - xs[0] : 0; r.dy = ys.size() > 1 ? ys[1] - ys[0] : 0;
                r.type = (r.nx > 1 && r.ny > 1) ? 1 : (r.nx > 1 ? 2 : 3);
                out.push_back({ { xs[0], ys[0] }, r });
                return out;
            }
        }
        // rows -> constant-pitch runs
        struct Run { int64_t x0, y, n, dx; };
        std::vector<Run> runs;
        for (size_t i = 0; i < pts.size();) {
            size_t j = i;
            while (j + 1 < pts.size() && pts[j + 1].y == pts[i].y) ++j;
            size_t a = i;
            while (a <= j) {
                if (a == j) { runs.push_back({ pts[a].x, pts[a].y, 1, 0 }); ++a; continue; }
                int64_t dx = pts[a + 1].x - pts[a].x;
                size_t b = a + 1;
                while (b + 1 <= j && pts[b + 1].x - pts[b].x == dx) ++b;
                runs.push_back({ pts[a].x, pts[a].y, (int64_t)(b - a + 1), dx });
                a = b + 1;
            }
            i = j + 1;
        }
        // stack identical runs with constant vertical pitch
        std::map<std::tuple<int64_t, int64_t, int64_t>, std::vector<int64_t>> stacks;
        for (auto& r : runs) stacks[{ r.x0, r.n, r.dx }].push_back(r.y);
        std::vector<P> singles;
        for (auto& kv : stacks) {
            auto& ys = kv.second;                        // ascending (rows were visited by y)
            int64_t x0 = std::get<0>(kv.first), n = std::get<1>(kv.first), dx = std::get<2>(kv.first);
            size_t a = 0;
            while (a < ys.size()) {
                size_t b = a;
                if (a + 1 < ys.size()) {
                    int64_t dy = ys[a + 1] - ys[a];
                    b = a + 1;
                    while (b + 1 < ys.size() && ys[b + 1] - ys[b] == dy) ++b;
                }
                int64_t ny = (int64_t)(b - a + 1);
                Rep r;
                r.nx = n; r.ny = ny; r.dx = dx; r.dy = ny > 1 ? ys[a + 1] - ys[a] : 0;
                if (n > 1 && ny > 1) r.type = 1;
                else if (n > 1) r.type = 2;
                else if (ny > 1) r.type = 3;
                if (r.type == 0) singles.push_back({ x0, ys[a] });
                else out.push_back({ { x0, ys[a] }, r });
                a = b + 1;
            }
        }
        if (singles.size() == 1) out.push_back({ singles[0], Rep() });
        else if (singles.size() > 1) {
            std::sort(singles.begin(), singles.end(), [](const P& a, const P& b) { return a.y != b.y ? a.y < b.y : a.x < b.x; });
            bool same_y = true, same_x = true;
            for (auto& s : singles) { same_y &= s.y == singles[0].y; same_x &= s.x == singles[0].x; }
            Rep r;
            if (same_y) {
                std::sort(singles.begin(), singles.end(), [](const P& a, const P& b) { return a.x < b.x; });
                r.type = 4;
                for (size_t i = 1; i < singles.size(); ++i) r.spaces.push_back(singles[i].x - singles[i - 1].x);
            } else if (same_x) {
                r.type = 6;
                for (size_t i = 1; i < singles.size(); ++i) r.spaces.push_back(singles[i].y - singles[i - 1].y);
            } else {
                r.type = 10;
                for (size_t i = 1; i < singles.size(); ++i) r.deltas.push_back({ singles[i].x - singles[i - 1].x, singles[i].y - singles[i - 1].y });
            }
            out.push_back({ singles[0], r });
        }
        return out;
    }

    void write_element(const Shape& s, int64_t x, int64_t y, const Rep* rep) {
        Encoder& e = body;
        rec_starts.push_back(body.b.size());
        uint64_t L = (uint64_t)layer, D = (uint64_t)datatype;
        bool wl = !(m_layer && ml == L), wd = !(m_dt && md == D);
        bool wx = x != mx, wy = y != my, wr = rep && rep->type != 0;
        if (s.rect) {
            bool sq = s.w == s.h;
            bool ww = !(m_w && mw == s.w);
            bool wh = !sq && !(m_h && mh == s.h);
            uint8_t info = (uint8_t)((sq ? 0x80 : 0) | (ww ? 0x40 : 0) | (wh ? 0x20 : 0) | (wx ? 0x10 : 0) | (wy ? 0x08 : 0) |
                                     (wr ? 0x04 : 0) | (wd ? 0x02 : 0) | (wl ? 0x01 : 0));
            e.u8(20); e.u8(info);
            if (wl) e.uint_(L);
            if (wd) e.uint_(D);
            if (ww) e.uint_((uint64_t)s.w);
            if (wh) e.uint_((uint64_t)s.h);
            if (wx) e.sint(x - mx);
            if (wy) e.sint(y - my);
            if (wr) write_rep(e, *rep);
            m_w = true; mw = s.w; m_h = true; mh = s.h;       // a square sets geometry-h too
        } else {
            bool wp = !(m_pts && mpts.size() == s.pts.size() && std::equal(mpts.begin(), mpts.end(), s.pts.begin()));
            uint8_t info = (uint8_t)((wp ? 0x20 : 0) | (wx ? 0x10 : 0) | (wy ? 0x08 : 0) | (wr ? 0x04 : 0) |
                                     (wd ? 0x02 : 0) | (wl ? 0x01 : 0));
            e.u8(21); e.u8(info);
            if (wl) e.uint_(L);
            if (wd) e.uint_(D);
            if (wp) {
                e.uint_(2); e.uint_(s.pts.size());               // Manhattan 1-deltas, closing edge implied
                P prev{ 0, 0 };
                for (auto& q : s.pts) {
                    int64_t dx = q.x - prev.x, dy = q.y - prev.y;
                    uint64_t dir, mag;
                    if (dy == 0) { dir = dx > 0 ? 0 : 2; mag = (uint64_t)(dx > 0 ? dx : -dx); }
                    else { dir = dy > 0 ? 1 : 3; mag = (uint64_t)(dy > 0 ? dy : -dy); }
                    e.uint_((mag << 2) | dir);
                    prev = q;
                }
                mpts = s.pts; m_pts = true;
            }
            if (wx) e.sint(x - mx);
            if (wy) e.sint(y - my);
            if (wr) write_rep(e, *rep);
        }
        m_layer = true; ml = L; m_dt = true; md = D; mx = x; my = y;
    }

    void write_props(const std::string& recs) {
        size_t o = 0;
        while (o < recs.size()) {
            uint32_t len; memcpy(&len, recs.data() + o, 4); o += 4;
            std::string r = recs.substr(o, len); o += len;
            rec_starts.push_back(body.b.size());
            if (r == last_prop) body.u8(29);
            else { body.bytes(r.data(), r.size()); last_prop = r; }
        }
    }

    bool end_layer() {
        if (npoly < 0 || (int64_t)poly_start.size() != npoly + 1) { err = "bad polygon index"; return false; }
        for (auto& c : cols) {
            if ((int64_t)c.present.size() != npoly) { err = "column " + c.name + " has wrong length"; return false; }
        }
        std::vector<Shape> shapes((size_t)npoly);
        std::vector<std::string> props((size_t)npoly);
        for (int64_t p = 0; p < npoly; ++p) {
            if (!shape_of(p, shapes[p])) { err = "polygon " + std::to_string(p + 1) + ": cannot trace outline (overlapping boxes?)"; return false; }
            props[p] = props_of(p);
        }
        // Group polygons with identical shape and property records (in canonical order of the
        // first member).  Members of a group are written consecutively, so even without
        // repetitions the modal geometry and PROPERTY-REPEAT apply.
        std::unordered_map<std::string, size_t> gid;
        std::vector<std::vector<int64_t>> groups;
        for (int64_t p = 0; p < npoly; ++p) {
            std::string k = shape_key(shapes[p]) + '|' + props[p];
            auto it = gid.find(k);
            if (it == gid.end()) { gid.emplace(std::move(k), groups.size()); groups.push_back({ p }); }
            else groups[it->second].push_back(p);
        }
        if (!use_rep) {
            for (auto& g : groups)
                for (int64_t p : g) {
                    write_element(shapes[p], shapes[p].x, shapes[p].y, nullptr);
                    write_props(props[p]);
                }
        } else {
            for (auto& g : groups) {
                std::vector<P> pos;
                for (int64_t p : g) pos.push_back({ shapes[p].x, shapes[p].y });
                for (auto& pr : decompose(pos)) {
                    write_element(shapes[g[0]], pr.first.x, pr.first.y, &pr.second);
                    write_props(props[g[0]]);
                }
            }
        }
        layernames.push_back({ "", { layer, datatype } });
        cols.clear(); boxes.clear(); poly_start.clear();
        return true;
    }

    static void zlib_compress_one(const uint8_t* in, size_t n, std::vector<uint8_t>& z, size_t& zn) {
        z.resize(compressBound((uLong)n) + 64);
        z_stream zs{};
        deflateInit2(&zs, 6, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY);
        zs.next_in = const_cast<Bytef*>(in); zs.avail_in = (uInt)n;
        zs.next_out = z.data(); zs.avail_out = (uInt)z.size();
        deflate(&zs, Z_FINISH);
        zn = z.size() - zs.avail_out;
        deflateEnd(&zs);
        z.resize(zn);
    }

    bool nvcomp_compress_chunks(const std::vector<std::pair<const uint8_t*, size_t>>& chunks,
                                std::vector<std::vector<uint8_t>>& out) {
#if !defined(LAYOUTDB_USE_NVCOMP)
        (void)chunks; (void)out; return false;
#else
        if (chunks.empty()) return true;
        int devcount = 0;
        if (cudaGetDeviceCount(&devcount) != cudaSuccess || devcount <= 0) return false;
        size_t n = chunks.size();
        size_t max_in = 0, total_in = 0;
        for (auto& c : chunks) { max_in = std::max(max_in, c.second); total_in += c.second; }
        nvcompBatchedDeflateCompressOpts_t opts = nvcompBatchedDeflateCompressDefaultOpts;
        nvcompAlignmentRequirements_t align{8,8,8};
        if (nvcompBatchedDeflateCompressGetRequiredAlignments(opts, &align) != nvcompSuccess) return false;

        size_t max_out = 0;
        if (nvcompBatchedDeflateCompressGetMaxOutputChunkSize(max_in, opts, &max_out) != nvcompSuccess) return false;
        size_t temp_bytes = 0;
        if (nvcompBatchedDeflateCompressGetTempSizeAsync(n, max_in, opts, &temp_bytes, total_in) != nvcompSuccess) return false;

        auto cuda_ok = [](cudaError_t e) { return e == cudaSuccess; };
        uint8_t *d_in = nullptr, *d_out = nullptr, *d_temp = nullptr;
        void **h_in_ptrs = nullptr, **h_out_ptrs = nullptr;
        size_t *h_in_bytes = nullptr, *h_out_bytes = nullptr;
        void **d_in_ptrs = nullptr, **d_out_ptrs = nullptr;
        size_t *d_in_bytes = nullptr, *d_out_bytes = nullptr;
        nvcompStatus_t *d_status = nullptr, *h_status = nullptr;
        cudaStream_t stream = 0;
        bool ok = true;

        auto cleanup = [&]() {
            if (stream) cudaStreamDestroy(stream);
            if (d_status) cudaFree(d_status);
            if (d_out_bytes) cudaFree(d_out_bytes);
            if (d_in_bytes) cudaFree(d_in_bytes);
            if (d_out_ptrs) cudaFree(d_out_ptrs);
            if (d_in_ptrs) cudaFree(d_in_ptrs);
            if (d_temp) cudaFree(d_temp);
            if (d_out) cudaFree(d_out);
            if (d_in) cudaFree(d_in);
            if (h_status) cudaFreeHost(h_status);
            if (h_out_bytes) cudaFreeHost(h_out_bytes);
            if (h_in_bytes) cudaFreeHost(h_in_bytes);
            if (h_out_ptrs) cudaFreeHost(h_out_ptrs);
            if (h_in_ptrs) cudaFreeHost(h_in_ptrs);
        };

        if (!cuda_ok(cudaStreamCreate(&stream))) { cleanup(); return false; }
        if (!cuda_ok(cudaMallocHost((void**)&h_in_ptrs, n * sizeof(void*))) ||
            !cuda_ok(cudaMallocHost((void**)&h_out_ptrs, n * sizeof(void*))) ||
            !cuda_ok(cudaMallocHost((void**)&h_in_bytes, n * sizeof(size_t))) ||
            !cuda_ok(cudaMallocHost((void**)&h_out_bytes, n * sizeof(size_t))) ||
            !cuda_ok(cudaMallocHost((void**)&h_status, n * sizeof(nvcompStatus_t)))) { cleanup(); return false; }

        if (!cuda_ok(cudaMalloc(&d_in, total_in)) ||
            !cuda_ok(cudaMalloc(&d_out, n * max_out)) ||
            !cuda_ok(cudaMalloc(&d_temp, temp_bytes)) ||
            !cuda_ok(cudaMalloc(&d_in_ptrs, n * sizeof(void*))) ||
            !cuda_ok(cudaMalloc(&d_out_ptrs, n * sizeof(void*))) ||
            !cuda_ok(cudaMalloc(&d_in_bytes, n * sizeof(size_t))) ||
            !cuda_ok(cudaMalloc(&d_out_bytes, n * sizeof(size_t))) ||
            !cuda_ok(cudaMalloc(&d_status, n * sizeof(nvcompStatus_t)))) { cleanup(); return false; }

        size_t in_off = 0;
        for (size_t i = 0; i < n; ++i) {
            h_in_ptrs[i] = d_in + in_off;
            h_out_ptrs[i] = d_out + i * max_out;
            h_in_bytes[i] = chunks[i].second;
            if (!cuda_ok(cudaMemcpyAsync(d_in + in_off, chunks[i].first, chunks[i].second, cudaMemcpyHostToDevice, stream))) ok = false;
            in_off += chunks[i].second;
        }
        if (ok) {
            ok = cuda_ok(cudaMemcpyAsync(d_in_ptrs, h_in_ptrs, n * sizeof(void*), cudaMemcpyHostToDevice, stream)) &&
                 cuda_ok(cudaMemcpyAsync(d_out_ptrs, h_out_ptrs, n * sizeof(void*), cudaMemcpyHostToDevice, stream)) &&
                 cuda_ok(cudaMemcpyAsync(d_in_bytes, h_in_bytes, n * sizeof(size_t), cudaMemcpyHostToDevice, stream));
        }
        if (ok) {
            nvcompStatus_t st = nvcompBatchedDeflateCompressAsync(
                (const void* const*)d_in_ptrs, d_in_bytes, max_in, n,
                d_temp, temp_bytes,
                d_out_ptrs, d_out_bytes,
                opts, d_status, stream);
            ok = (st == nvcompSuccess);
        }
        if (ok) ok = cuda_ok(cudaMemcpyAsync(h_out_bytes, d_out_bytes, n * sizeof(size_t), cudaMemcpyDeviceToHost, stream));
        if (ok) ok = cuda_ok(cudaMemcpyAsync(h_status, d_status, n * sizeof(nvcompStatus_t), cudaMemcpyDeviceToHost, stream));
        if (ok) ok = cuda_ok(cudaStreamSynchronize(stream));
        if (ok) {
            out.assign(n, std::vector<uint8_t>());
            for (size_t i = 0; i < n; ++i) {
                if (h_status[i] != nvcompSuccess || h_out_bytes[i] == 0 || h_out_bytes[i] > max_out) { ok = false; break; }
                out[i].resize(h_out_bytes[i]);
            }
            for (size_t i = 0; ok && i < n; ++i) {
                ok = cuda_ok(cudaMemcpy(out[i].data(), d_out + i * max_out, h_out_bytes[i], cudaMemcpyDeviceToHost));
            }
        }
        cleanup();
        return ok;
#endif
    }

    // A CBLOCK must hold whole records: split the stream only at record starts.
    void cblock_flush(Encoder& f, const std::vector<uint8_t>& raw, const std::vector<size_t>& starts) {
        std::vector<std::pair<const uint8_t*, size_t>> chunks;
        std::vector<size_t> chunk_raw;
        size_t chunk = cblock_bytes;
        size_t o = 0, si = 0;
        while (o < raw.size()) {
            size_t lim = o + chunk, cut = raw.size();
            if (lim < raw.size()) {
                while (si < starts.size() && starts[si] <= o) ++si;
                size_t best = 0;
                size_t sj = si;
                while (sj < starts.size() && starts[sj] <= lim) best = starts[sj++];
                cut = best > o ? best : (si < starts.size() ? starts[si] : raw.size());
            }
            chunks.push_back({ raw.data() + o, cut - o });
            chunk_raw.push_back(cut - o);
            o = cut;
        }

        std::vector<std::vector<uint8_t>> compressed(chunks.size());
        bool used_nvcomp = false;
        int backend = cblock_backend;
        if (use_cblock && backend != 0) {
#if defined(LAYOUTDB_USE_NVCOMP)
            if (backend == 1 || backend == 2) used_nvcomp = nvcomp_compress_chunks(chunks, compressed);
#else
            (void)backend;
#endif
        }
        if (!used_nvcomp) {
            for (size_t i = 0; i < chunks.size(); ++i) {
                size_t zn = 0;
                zlib_compress_one(chunks[i].first, chunks[i].second, compressed[i], zn);
            }
        }

        for (size_t i = 0; i < chunks.size(); ++i) {
            f.u8(34); f.uint_(0); f.uint_(chunk_raw[i]); f.uint_(compressed[i].size());
            f.bytes(compressed[i].data(), compressed[i].size());
        }
    }

    bool close() {
        Encoder f;
        f.bytes("%SEMI-OASIS\r\n", 13);
        f.u8(1); f.str("1.0"); f.real_whole(grid);
        f.uint_(strict ? 1 : 0);                             // offset-flag: tables in END
        if (!strict) for (int i = 0; i < 12; ++i) f.uint_(0);
        f.u8(13); f.uint_(0);                                // CELL refnum 0
        std::vector<uint8_t> raw;
        raw.push_back(16);                                   // XYRELATIVE
        raw.insert(raw.end(), body.b.begin(), body.b.end());
        std::vector<size_t> starts;
        starts.reserve(rec_starts.size());
        for (size_t s : rec_starts) starts.push_back(s + 1);
        if (use_cblock) cblock_flush(f, raw, starts); else f.bytes(raw.data(), raw.size());
        uint64_t off_cell = f.b.size();
        f.u8(4); f.str(cell); f.uint_(0);
        uint64_t off_pn = 0, off_ps = 0, off_ln = 0;
        if (!propnames.empty()) {
            off_pn = f.b.size();
            std::vector<std::pair<uint64_t, std::string>> v;
            for (auto& kv : propnames) v.push_back({ kv.second, kv.first });
            std::sort(v.begin(), v.end());
            for (auto& r : v) { f.u8(8); f.str(r.second); f.uint_(r.first); }
        }
        if (!propstrings.empty()) {
            off_ps = f.b.size();
            std::vector<std::pair<uint64_t, std::string>> v;
            for (auto& kv : propstrings) v.push_back({ kv.second, kv.first });
            std::sort(v.begin(), v.end());
            for (auto& r : v) { f.u8(10); f.str(r.second); f.uint_(r.first); }
        }
        if (!layernames.empty()) {
            off_ln = f.b.size();
            for (auto& ln : layernames) {
                f.u8(11); f.str(ln.first); f.uint_(3); f.uint_((uint64_t)ln.second.first); f.uint_(3); f.uint_((uint64_t)ln.second.second);
            }
        }
        size_t end_at = f.b.size();
        f.u8(2);
        if (strict) {
            f.uint_(1); f.uint_(off_cell);                   // CELLNAME
            f.uint_(1); f.uint_(0);                          // TEXTSTRING (none)
            f.uint_(1); f.uint_(off_pn);                     // PROPNAME
            f.uint_(1); f.uint_(off_ps);                     // PROPSTRING
            f.uint_(1); f.uint_(off_ln);                     // LAYERNAME
            f.uint_(1); f.uint_(0);                          // XNAME (none)
        }
        size_t used = f.b.size() - end_at;                   // record id + table info
        size_t pad = 256 - used - 1 - 1;                     // 1 byte length, 1 byte validation
        if (pad > 127) pad = 256 - used - 2 - 1;
        f.str(std::string(pad, '\0'));
        f.uint_(0);                                          // no validation
        if (f.b.size() - end_at != 256) { err = "internal: END record is not 256 bytes"; return false; }
        FILE* fp = fopen(path.c_str(), "wb");
        if (!fp) { err = "cannot open " + path; return false; }
        bool ok = fwrite(f.b.data(), 1, f.b.size(), fp) == f.b.size();
        ok = (fclose(fp) == 0) && ok;
        if (!ok) err = "write failed: " + path;
        return ok;
    }
};

void set_err(OasisPropWriter* w, char* errbuf, int errlen) {
    if (!errbuf || errlen <= 0) return;
    snprintf(errbuf, (size_t)errlen, "%s", w ? w->err.c_str() : "null writer");
}

} // namespace

// ---------------------------------------------------------------------------
// C API (called from prop_oasis.f90)
// ---------------------------------------------------------------------------
extern "C" {

// flags: bit0 CBLOCK, bit1 repetitions, bit2 strict
void* fpo_create(const char* path, const char* cell, int64_t grid_per_micron, int32_t flags) {
    auto* w = new OasisPropWriter();
    w->path = path; w->cell = (cell && *cell) ? cell : "TOP";
    w->grid = grid_per_micron > 0 ? (uint64_t)grid_per_micron : 1000;
    w->use_cblock = flags & 1; w->use_rep = flags & 2; w->strict = flags & 4;
    return w;
}

// Uncompressed bytes per CBLOCK (default 4 MiB); blocks always end on a record boundary.
// Outline measures of one polygon given as disjoint boxes (4*nbox int64 x1,y1,x2,y2).
// out[0] perimeter, out[1] vertices (simplified loop sizes summed over outer + holes, before
// keyholing; a corner-contact vertex counts once per visit), out[2] line-end width,
// out[3] line-end length; *le_dir -1 none / 0 E / 1 N / 2 W / 3 S.
// Thread-safe (no shared state). Returns 0, or 1 if the outline could not be traced
// (out[0] still valid, out[1] = -1, no line end).
int32_t fpo_poly_geom(int64_t nbox, const int64_t* boxes, int64_t* out, int32_t* le_dir) {
    out[0] = out[1] = out[2] = out[3] = 0; *le_dir = -1;
    if (nbox <= 0) return 1;
    if (nbox == 1) {
        int64_t w = boxes[2] - boxes[0], h = boxes[3] - boxes[1];
        out[0] = 2 * (w + h); out[1] = 4;
        if (w < h)      { out[2] = w; out[3] = h; *le_dir = 3; }   // vertical wire: south end
        else if (h < w) { out[2] = h; out[3] = w; *le_dir = 2; }   // horizontal wire: west end
        return 0;
    }
    std::vector<std::array<int64_t, 4>> bx((size_t)nbox);
    for (int64_t i = 0; i < nbox; ++i)
        bx[(size_t)i] = { boxes[4 * i], boxes[4 * i + 1], boxes[4 * i + 2], boxes[4 * i + 3] };
    std::vector<std::vector<P>> loops;
    int64_t perim = 0;
    bool ok = trace_loops(bx, loops, &perim);
    out[0] = perim;
    if (!ok) { out[1] = -1; return 1; }
    int64_t nv = 0;
    for (auto& L : loops) nv += (int64_t)L.size();
    out[1] = nv;
    int d; line_end(loops, out[2], out[3], d);
    *le_dir = d;
    return 0;
}

void fpo_set_cblock_bytes(void* h, int64_t n) {
    if (n > 0) ((OasisPropWriter*)h)->cblock_bytes = (size_t)n;
}

void fpo_set_cblock_backend(void* h, int32_t backend) {
    auto* w = (OasisPropWriter*)h;
    if (backend >= 0 && backend <= 2) w->cblock_backend = backend;
}

// poly_start: npoly+1 offsets, 1-based (Fortran CSR); boxes: 4*nbox int64 (x1,y1,x2,y2)
int32_t fpo_begin_layer(void* h, const char* lname, int64_t layer, int64_t datatype, int64_t npoly,
                        const int64_t* poly_start, int64_t nbox, const int64_t* boxes) {
    auto* w = (OasisPropWriter*)h;
    w->layer = layer; w->datatype = datatype; w->npoly = npoly;
    w->poly_start.assign(poly_start, poly_start + npoly + 1);
    for (auto& v : w->poly_start) v -= 1;
    w->boxes.assign(boxes, boxes + 4 * nbox);
    w->cols.clear();
    (void)lname;
    return 0;
}

// Scalar column (field == "" / NULL) or a field of the STRUCT column `name` begun by kind=4.
// present: npoly int8; ival/rval: npoly values; soff: npoly+1 0-based offsets into schars.
int32_t fpo_column(void* h, const char* name, const char* field, int32_t kind, const int8_t* present,
                   const int64_t* ival, const double* rval, const int64_t* soff, const char* schars) {
    auto* w = (OasisPropWriter*)h;
    int64_t n = w->npoly;
    bool is_field = field && *field;
    if (!is_field) {
        Column c; c.name = name; c.kind = kind;
        c.present.assign((const uint8_t*)present, (const uint8_t*)present + n);
        if (kind == PK_INT) c.iv.assign(ival, ival + n);
        if (kind == PK_FLOAT) c.rv.assign(rval, rval + n);
        if (kind == PK_STRING) for (int64_t i = 0; i < n; ++i) c.sv.emplace_back(schars + soff[i], (size_t)(soff[i + 1] - soff[i]));
        w->cols.push_back(std::move(c));
        return 0;
    }
    if (w->cols.empty() || w->cols.back().kind != PK_STRUCT || w->cols.back().name != name) {
        w->err = std::string("struct field ") + field + " without its struct column " + name;
        return 1;
    }
    Column::Field f; f.name = field; f.kind = kind;
    f.present.assign((const uint8_t*)present, (const uint8_t*)present + n);
    if (kind == PK_INT) f.iv.assign(ival, ival + n);
    if (kind == PK_FLOAT) f.rv.assign(rval, rval + n);
    if (kind == PK_STRING) for (int64_t i = 0; i < n; ++i) f.sv.emplace_back(schars + soff[i], (size_t)(soff[i + 1] - soff[i]));
    w->cols.back().fields.push_back(std::move(f));
    return 0;
}

int32_t fpo_end_layer(void* h, const char* lname, char* errbuf, int32_t errlen) {
    auto* w = (OasisPropWriter*)h;
    bool ok = w->end_layer();
    std::string nm = lname ? lname : "";
    for (auto& c : nm) if ((unsigned char)c < 0x21 || (unsigned char)c > 0x7e) c = '_';   // LAYERNAME is an n-string
    if (ok && !nm.empty()) w->layernames.back().first = nm;
    else if (ok) w->layernames.pop_back();
    if (!ok) set_err(w, errbuf, errlen);
    return ok ? 0 : 1;
}

int32_t fpo_close(void* h, char* errbuf, int32_t errlen) {
    auto* w = (OasisPropWriter*)h;
    bool ok = w->close();
    if (!ok) set_err(w, errbuf, errlen);
    delete w;
    return ok ? 0 : 1;
}

} // extern "C"
