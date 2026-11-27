// File   : trap_out.cpp
// Author : Sandeep Koranne (C) 2026. All rights reserved.
// Purpose: Exact OASIS output of a merged any-angle layer given as interior-disjoint horizontal
//          trapezoids with lattice-line sides (TrapezoidModule, trap_merge.f90).
//
// A trapezoid corner is the x of a side line at an integer row: a lattice point or a rational
// point. Records need lattice corners, so:
//   * a trapezoid whose four corners are lattice points is written as a RECTANGLE,
//     CTRAPEZOID (45-degree shapes) or TRAPEZOID record (Writer::trapezoid_h);
//   * every other trapezoid is joined with its vertical neighbours across its rational corners:
//     a rational corner lies on a boundary line that continues through the row (boundary
//     vertices are lattice points after snap rounding, invariant I1), so the trapezoid on the
//     other side of the row on the same line exists and is unique. Each group of joined
//     trapezoids is a region whose boundary vertices are all lattice points: its boundary is
//     traced (sides and the uncancelled parts of tops/bottoms, collinear points removed), holes
//     are bridged to a lattice vertex directly below them (inside the trapezoid under the hole's
//     lowest vertex, so the bridge is exact and crosses nothing), and the outlines are written
//     as POLYGON records.
// Nothing is rounded: the output covers exactly the input trapezoids. Failed internal checks
// are counted in stats[6] ("anomalies"); a correct run has 0.
#include "oasis_writer.hpp"
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstdlib>
#include <functional>
#include <thread>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <numeric>
#include <unordered_map>
#include <vector>
#include <cstring>

namespace {
using i128 = __int128;
using ldbgeom::Pt;

struct Side { int64_t ax, k, dx, dy; };
struct Trap { int64_t y1, y2; Side l, r; };
struct Q { i128 n; int64_t d; };                 // x = n / d, d > 0

Q qred(Q q) {
    if (q.d == 1) return q;
    i128 an = q.n < 0 ? -q.n : q.n;
    int64_t a = q.d, b = (int64_t)(an % q.d);           // gcd(n, d) = gcd(d, n mod d), d < 2^31
    while (b) { int64_t t = a % b; a = b; b = t; }
    if (a > 1) { q.n /= a; q.d /= a; }
    return q;
}
Q side_x(const Side& s, int64_t y1, int64_t y) { return qred({ (i128)s.ax * s.dy + (i128)(y - (y1 - s.k)) * s.dx, s.dy }); }
int qcmp(const Q& a, const Q& b) { i128 l = a.n * b.d, r = b.n * a.d; return l < r ? -1 : (l > r ? 1 : 0); }
int qcmp_int(const Q& a, int64_t x) { i128 l = a.n, r = (i128)x * a.d; return l < r ? -1 : (l > r ? 1 : 0); }

struct LineKey { int64_t dx, dy; i128 c; int64_t y; bool operator==(const LineKey& o) const { return dx == o.dx && dy == o.dy && c == o.c && y == o.y; } };
struct LineHash { size_t operator()(const LineKey& k) const {
    uint64_t h = (uint64_t)k.dx * 0x9E3779B97F4A7C15ull ^ ((uint64_t)k.dy + 0x632BE59BD9B4E019ull);
    h ^= (uint64_t)k.c * 0xBF58476D1CE4E5B9ull; h ^= (uint64_t)(k.c >> 64) * 0x94D049BB133111EBull;
    h ^= (uint64_t)k.y * 0xD6E8FEB86659FD93ull; return (size_t)(h ^ (h >> 31)); } };
LineKey line_key(const Side& s, int64_t y1, int64_t y) { return { s.dx, s.dy, (i128)s.ax * s.dy - (i128)(y1 - s.k) * s.dx, y }; }

struct VKey { int64_t y; i128 n; int64_t d; bool operator==(const VKey& o) const { return y == o.y && n == o.n && d == o.d; } };
struct VHash { size_t operator()(const VKey& k) const {
    uint64_t h = (uint64_t)k.y * 0x9E3779B97F4A7C15ull ^ (uint64_t)k.n * 0xBF58476D1CE4E5B9ull ^
                 (uint64_t)(k.n >> 64) * 0x94D049BB133111EBull ^ (uint64_t)k.d * 0xD6E8FEB86659FD93ull;
    return (size_t)(h ^ (h >> 29)); } };

// direction d relative to r: half 0 for ccw angle in [0, pi), else 1
int rel_half(int64_t rx, int64_t ry, int64_t dx, int64_t dy) {
    const i128 c = (i128)rx * dy - (i128)ry * dx, dt = (i128)rx * dx + (i128)ry * dy;
    return (c > 0 || (c == 0 && dt > 0)) ? 0 : 1;
}
// ccw angle from r: a < b ?
bool rel_less(int64_t rx, int64_t ry, int64_t ax, int64_t ay, int64_t bx, int64_t by) {
    const int ha = rel_half(rx, ry, ax, ay), hb = rel_half(rx, ry, bx, by);
    if (ha != hb) return ha < hb;
    return (i128)ax * by - (i128)ay * bx > 0;
}
// ccw angle from r strictly between 0 and the angle of b (both measured ccw from r)
bool rel_between(int64_t rx, int64_t ry, int64_t ux, int64_t uy, int64_t bx, int64_t by) {
    const bool u0 = (i128)rx * uy - (i128)ry * ux == 0 && (i128)rx * ux + (i128)ry * uy > 0;
    if (u0) return false;                                    // along r
    const bool b0 = (i128)rx * by - (i128)ry * bx == 0 && (i128)rx * bx + (i128)ry * by > 0;
    if (b0) return true;                                     // b at angle 2 pi: whole turn
    return rel_less(rx, ry, ux, uy, bx, by);
}

struct Stats { int64_t trap_records = 0, polygons = 0, poly_vertices = 0, groups = 0, holes = 0, anomalies = 0, max_group = 0; };
struct Output { oasis_writer::Writer* w; uint64_t layer, dt; Stats st; };

// Polygons produced for one chunk of groups (filled by a worker thread, written in order later).
struct ChunkOut {
    std::vector<Pt> pts;                         // all polygon vertices, back to back
    std::vector<int64_t> pstart;                 // polygon k: pts[pstart[k] .. pstart[k+1])
    Stats st;
    ChunkOut() : pstart{0} {}
};

// ---------------------------------------------------------------- one group -> polygons
// Reusable per-thread workspace: vectors keep their capacity between groups.
struct GroupPoly {
    const std::vector<Trap>& T;
    struct Seg { int a, b; int64_t dx, dy; };
    struct PRef { int64_t y; i128 n; int64_t d; int seg_end; };   // seg_end = 2*seg + (0 a / 1 b)
    struct Node { int64_t x, y; int prev, next; };
    std::vector<Seg> seg;
    std::vector<PRef> pr;
    std::vector<VKey> vk;
    std::vector<std::pair<int64_t, int64_t>> rowev;   // (y, +-(trap index in M + 1))
    std::vector<int> ostart, olist, nxt, ls, cv, loop_start;
    std::vector<char> used, seen, vis;
    std::vector<i128> loop_area;
    std::vector<Node> nd;
    GroupPoly(const std::vector<Trap>& t) : T(t) {}

    void add_seg(int64_t ya, const Q& xa, int64_t yb, const Q& xb, int64_t dx, int64_t dy) {
        if (ya == yb && xa.n == xb.n && xa.d == xb.d) return;
        const int k = (int)seg.size();
        seg.push_back({ -1, -1, dx, dy });
        pr.push_back({ ya, xa.n, xa.d, 2 * k }); pr.push_back({ yb, xb.n, xb.d, 2 * k + 1 });
    }

    void run(const int64_t* M, int64_t nm, ChunkOut& out) {
        build(M, nm, out.st);
        trace_emit(M, nm, out);
    }

    // Boundary of the trapezoid set M: directed segments (region on the left), lattice-or-
    // rational vertex ids, and nxt[] pairing every incoming segment with its outgoing one
    // (sharpest left turn: loops separate at pinch points).
    void build(const int64_t* M, int64_t nm, Stats& st) {
        seg.clear(); pr.clear(); vk.clear(); rowev.clear();
        // --- side segments (inside on the left: left sides go down, right sides go up)
        for (int64_t i = 0; i < nm; ++i) {
            const Trap& z = T[(size_t)M[i]];
            add_seg(z.y2, side_x(z.l, z.y1, z.y2), z.y1, side_x(z.l, z.y1, z.y1), -z.l.dx, -z.l.dy);
            add_seg(z.y1, side_x(z.r, z.y1, z.y1), z.y2, side_x(z.r, z.y1, z.y2), z.r.dx, z.r.dy);
            rowev.push_back({ z.y1, i + 1 }); rowev.push_back({ z.y2, -(i + 1) });
        }
        // --- horizontal parts: bottoms (rightwards) minus tops (leftwards), per row
        std::sort(rowev.begin(), rowev.end());
        struct Ev { Q x; int b, t; };
        std::vector<Ev> ev;
        for (size_t r0 = 0; r0 < rowev.size();) {
            size_t r1 = r0;
            const int64_t y = rowev[r0].first;
            ev.clear();
            while (r1 < rowev.size() && rowev[r1].first == y) {
                const int64_t code = rowev[r1].second;
                const Trap& z = T[(size_t)M[std::llabs(code) - 1]];
                Q xl = side_x(z.l, z.y1, y), xr = side_x(z.r, z.y1, y);
                if (qcmp(xl, xr) < 0) {
                    if (code > 0) { ev.push_back({ xl, +1, 0 }); ev.push_back({ xr, -1, 0 }); }
                    else { ev.push_back({ xl, 0, +1 }); ev.push_back({ xr, 0, -1 }); }
                }
                ++r1;
            }
            r0 = r1;
            std::sort(ev.begin(), ev.end(), [](const Ev& a, const Ev& b) { return qcmp(a.x, b.x) < 0; });
            int bc = 0, tc = 0;
            for (size_t i = 0; i < ev.size();) {
                size_t j = i;
                const Q x0 = ev[i].x;
                while (j < ev.size() && qcmp(ev[j].x, x0) == 0) { bc += ev[j].b; tc += ev[j].t; ++j; }
                if (j < ev.size() && bc != tc) {
                    if (bc > tc) add_seg(y, x0, y, ev[j].x, 1, 0); else add_seg(y, ev[j].x, y, x0, -1, 0);
                }
                i = j;
            }
        }
        // --- vertex ids: sort the endpoint references, equal points get one id
        std::sort(pr.begin(), pr.end(), [](const PRef& a, const PRef& b) {
            if (a.y != b.y) return a.y < b.y;
            if (a.n != b.n) return a.n < b.n;
            return a.d < b.d; });
        int nv = 0;
        for (size_t i = 0; i < pr.size(); ++i) {
            if (i == 0 || pr[i].y != pr[i - 1].y || pr[i].n != pr[i - 1].n || pr[i].d != pr[i - 1].d) {
                vk.push_back({ pr[i].y, pr[i].n, pr[i].d }); ++nv;
            }
            Seg& sg = seg[(size_t)(pr[i].seg_end >> 1)];
            if (pr[i].seg_end & 1) sg.b = nv - 1; else sg.a = nv - 1;
        }
        // --- pair incoming with outgoing at every vertex: first outgoing clockwise from the
        //     reverse of the incoming direction (sharpest left turn)
        const int ns = (int)seg.size();
        ostart.assign((size_t)nv + 1, 0); olist.assign((size_t)ns, 0);
        for (auto& sg : seg) ostart[(size_t)sg.a + 1]++;
        for (int v = 0; v < nv; ++v) ostart[(size_t)v + 1] += ostart[(size_t)v];
        { std::vector<int> pos(ostart.begin(), ostart.end() - 1);
          for (int i = 0; i < ns; ++i) olist[(size_t)pos[(size_t)seg[(size_t)i].a]++] = i; }
        nxt.assign((size_t)ns, -1); used.assign((size_t)ns, 0);
        for (int i = 0; i < ns; ++i) {
            const Seg& sg = seg[(size_t)i];
            const int64_t rx = -sg.dx, ry = -sg.dy;
            int best = -1;
            for (int p = ostart[(size_t)sg.b]; p < ostart[(size_t)sg.b + 1]; ++p) {
                const int o = olist[(size_t)p];
                if (best < 0) { best = o; continue; }
                const Seg& a = seg[(size_t)o]; const Seg& b = seg[(size_t)best];
                if (rel_less(rx, ry, b.dx, b.dy, a.dx, a.dy)) best = o;
            }
            if (best < 0 || used[(size_t)best]) { st.anomalies++; continue; }
            used[(size_t)best] = 1;
            nxt[(size_t)i] = best;
        }
    }

    // Measures of the closed region of M (one PNUM polygon): vertices (direction changes, per
    // boundary visit), holes = 1 - chi with chi = sum of loop orientations - sum over points of
    // (boundary visits - 1) (gluing m loop points into one lowers chi by m - 1), perimeter.
    void measure(const int64_t* M, int64_t nm, int64_t& nvert, int64_t& holes, double& perim, int64_t& loops, Stats& st) {
        build(M, nm, st);
        const int ns = (int)seg.size();
        nvert = 0; holes = 0; perim = 0; loops = 0;
        int64_t chi = 0;
        for (size_t v = 0; v + 1 < ostart.size(); ++v) {
            const int deg = ostart[v + 1] - ostart[v];
            if (deg > 1) chi -= deg - 1;
        }
        long double pl = 0;
        for (int i = 0; i < ns; ++i) {
            const Seg& sg = seg[(size_t)i];
            const VKey& a = vk[(size_t)sg.a]; const VKey& b = vk[(size_t)sg.b];
            if (sg.dy == 0) {
                pl += std::fabs((long double)b.n / (long double)b.d - (long double)a.n / (long double)a.d);
            } else {
                const long double h = std::fabs((long double)(b.y - a.y));
                pl += h * std::sqrt((long double)sg.dx * sg.dx + (long double)sg.dy * sg.dy) / std::fabs((long double)sg.dy);
            }
        }
        perim = (double)pl;
        seen.assign((size_t)ns, 0);
        for (int i = 0; i < ns; ++i) {
            if (seen[(size_t)i] || nxt[(size_t)i] < 0) continue;
            ls.clear();
            int s0 = i; bool ok = true;
            while (!seen[(size_t)s0]) { seen[(size_t)s0] = 1; ls.push_back(s0); s0 = nxt[(size_t)s0]; if (s0 < 0) { ok = false; break; } }
            if (!ok || s0 != i) { st.anomalies++; continue; }
            const size_t L = ls.size();
            long double a2 = 0;
            for (size_t k = 0; k < L; ++k) {
                const Seg& a = seg[(size_t)ls[(k + L - 1) % L]]; const Seg& b = seg[(size_t)ls[k]];
                if (!(a.dx == b.dx && a.dy == b.dy)) nvert++;
                const VKey& p = vk[(size_t)b.a]; const VKey& q = vk[(size_t)b.b];
                const long double px = (long double)p.n / (long double)p.d, qx = (long double)q.n / (long double)q.d;
                a2 += px * (long double)q.y - qx * (long double)p.y;
            }
            chi += a2 > 0 ? 1 : -1;
            loops++;
        }
        (void)chi;
        holes = count_holes(st);
    }

    // Unbridged oriented boundary loops of M (outer CCW, holes CW), collinear vertices dropped.
    // Every direction-change vertex of a canonical trapezoid set is on the integer lattice (d == 1).
    void extract_loops(const int64_t* M, int64_t nm, std::vector<int64_t>& loff,
                       std::vector<int64_t>& lxs, std::vector<int64_t>& lys, Stats& st) {
        build(M, nm, st);
        const int ns = (int)seg.size();
        seen.assign((size_t)ns, 0);
        for (int i = 0; i < ns; ++i) {
            if (seen[(size_t)i] || nxt[(size_t)i] < 0) continue;
            ls.clear();
            int s0 = i; bool ok = true;
            while (!seen[(size_t)s0]) {
                seen[(size_t)s0] = 1; ls.push_back(s0);
                s0 = nxt[(size_t)s0];
                if (s0 < 0) { ok = false; break; }
            }
            if (!ok || s0 != i) { st.anomalies++; continue; }
            cv.clear();
            const size_t L = ls.size();
            for (size_t k = 0; k < L; ++k) {
                const Seg& a = seg[(size_t)ls[(k + L - 1) % L]];
                const Seg& b = seg[(size_t)ls[k]];
                if (a.dx == b.dx && a.dy == b.dy) continue;
                cv.push_back(b.a);
            }
            if (cv.size() < 3) { st.anomalies++; continue; }
            const size_t base = lxs.size();
            bool lattice = true;
            for (size_t k = 0; k < cv.size(); ++k) {
                const VKey& q = vk[(size_t)cv[k]];
                if (q.d != 1) { lattice = false; break; }
                lxs.push_back((int64_t)q.n);
                lys.push_back(q.y);
            }
            if (!lattice) { st.anomalies++; lxs.resize(base); lys.resize(base); continue; }
            loff.push_back((int64_t)lxs.size() + 1);
        }
    }

    // Holes of the closed region = bounded components of its complement. (1) Boundary segments
    // are split at every lattice vertex lying in their interior (a vertex of one part touching a
    // side of another: such T contacts are not vertices of the side segments built from the
    // trapezoids). Segments are found by line: for every vertex and every direction present,
    // the line through the vertex is looked up in a hash of the segments' lines. (2) Incoming and
    // outgoing segments are paired by the sharpest right turn, which follows the complement face
    // on the right of the boundary, so every complement face is traced as one cycle (the
    // complement faces of a connected set are simply connected). (3) Bounded faces are the cycles
    // with negative orientation (region on the left).
    struct LKey { int64_t dx, dy; i128 c; bool operator==(const LKey& o) const { return dx == o.dx && dy == o.dy && c == o.c; } };
    struct LHash { size_t operator()(const LKey& k) const {
        uint64_t h = (uint64_t)k.dx * 0x9E3779B97F4A7C15ull ^ (uint64_t)k.dy * 0xBF58476D1CE4E5B9ull ^
                     (uint64_t)k.c * 0x94D049BB133111EBull ^ (uint64_t)(k.c >> 64) * 0xD6E8FEB86659FD93ull;
        return (size_t)(h ^ (h >> 31)); } };
    std::vector<std::pair<int,int>> splits;            // (segment, vertex)
    std::vector<Seg> seg2;
    std::vector<int> os2, ol2, nx2;
    std::vector<char> seen2;

    int64_t count_holes(Stats& st) {
        const int ns = (int)seg.size(), nv = (int)vk.size();
        auto norm = [](int64_t dx, int64_t dy, int64_t& ndx, int64_t& ndy) {
            int64_t g = std::gcd(dx < 0 ? -dx : dx, dy < 0 ? -dy : dy); if (g == 0) g = 1;
            ndx = dx / g; ndy = dy / g;
            if (ndy < 0 || (ndy == 0 && ndx < 0)) { ndx = -ndx; ndy = -ndy; }
        };
        std::unordered_map<LKey, std::vector<int>, LHash> lines;
        std::vector<std::pair<int64_t,int64_t>> dirs;
        auto vx_is_int = [&](int v) { return vk[(size_t)v].n % vk[(size_t)v].d == 0; };
        for (int i = 0; i < ns; ++i) {
            const Seg& sg = seg[(size_t)i];
            int64_t ndx, ndy; norm(sg.dx, sg.dy, ndx, ndy);
            // line through endpoint a: ndy*x - ndx*y = c, with x = n/d -> c*d = ndy*n - ndx*y*d
            const VKey& a = vk[(size_t)sg.a];
            const i128 cn = (i128)ndy * a.n - (i128)ndx * a.y * a.d;   // c * a.d (exact for lattice lines)
            if (cn % a.d != 0) continue;               // not a lattice line: cannot hold a lattice vertex
            const i128 c = cn / a.d;
            lines[{ ndx, ndy, c }].push_back(i);
            dirs.push_back({ ndx, ndy });
        }
        std::sort(dirs.begin(), dirs.end()); dirs.erase(std::unique(dirs.begin(), dirs.end()), dirs.end());
        splits.clear();
        for (int v = 0; v < nv; ++v) {
            if (!vx_is_int(v)) continue;
            const int64_t x = (int64_t)(vk[(size_t)v].n / vk[(size_t)v].d), y = vk[(size_t)v].y;
            for (auto& d : dirs) {
                const i128 c = (i128)d.second * x - (i128)d.first * y;
                auto it = lines.find({ d.first, d.second, c });
                if (it == lines.end()) continue;
                for (int i : it->second) {
                    const Seg& sg = seg[(size_t)i];
                    if (sg.a == v || sg.b == v) continue;
                    const VKey& a = vk[(size_t)sg.a]; const VKey& b = vk[(size_t)sg.b];
                    bool inside;
                    if (sg.dy == 0) {                  // horizontal: x strictly between (exact rationals)
                        const int ca = ((i128)x * a.d > a.n) - ((i128)x * a.d < a.n);
                        const int cb = ((i128)x * b.d > b.n) - ((i128)x * b.d < b.n);
                        inside = ca * cb < 0;
                    } else {
                        inside = (y > std::min(a.y, b.y)) && (y < std::max(a.y, b.y));
                    }
                    if (inside) splits.push_back({ i, v });
                }
            }
        }
        // rebuild the segment list with the split points
        std::sort(splits.begin(), splits.end());
        seg2.clear();
        size_t q = 0;
        std::vector<std::pair<long double,int>> pts;
        for (int i = 0; i < ns; ++i) {
            const Seg& sg = seg[(size_t)i];
            if (q < splits.size() && splits[q].first == i) {
                pts.clear();
                const VKey& a = vk[(size_t)sg.a];
                const long double ax = (long double)a.n / (long double)a.d;
                while (q < splits.size() && splits[q].first == i) {
                    const VKey& p = vk[(size_t)splits[q].second];
                    const long double t = sg.dy != 0 ? (long double)(p.y - a.y) / (long double)sg.dy
                                                     : ((long double)p.n / (long double)p.d - ax) / (long double)sg.dx;
                    pts.push_back({ t, splits[q].second });
                    ++q;
                }
                std::sort(pts.begin(), pts.end());
                int prev = sg.a;
                for (auto& pp : pts) { if (pp.second != prev) { seg2.push_back({ prev, pp.second, sg.dx, sg.dy }); prev = pp.second; } }
                seg2.push_back({ prev, sg.b, sg.dx, sg.dy });
            } else {
                seg2.push_back(sg);
            }
        }
        // sharpest right turn pairing
        const int n2 = (int)seg2.size();
        os2.assign((size_t)nv + 1, 0); ol2.assign((size_t)n2, 0);
        for (auto& sg : seg2) os2[(size_t)sg.a + 1]++;
        for (int v = 0; v < nv; ++v) os2[(size_t)v + 1] += os2[(size_t)v];
        { std::vector<int> pos(os2.begin(), os2.end() - 1);
          for (int i = 0; i < n2; ++i) ol2[(size_t)pos[(size_t)seg2[(size_t)i].a]++] = i; }
        nx2.assign((size_t)n2, -1);
        std::vector<char> used2((size_t)n2, 0);
        for (int i = 0; i < n2; ++i) {
            const Seg& sg = seg2[(size_t)i];
            const int64_t rx = -sg.dx, ry = -sg.dy;
            int best = -1;
            for (int p = os2[(size_t)sg.b]; p < os2[(size_t)sg.b + 1]; ++p) {
                const int o = ol2[(size_t)p];
                if (best < 0) { best = o; continue; }
                const Seg& a = seg2[(size_t)o]; const Seg& b = seg2[(size_t)best];
                if (rel_less(rx, ry, a.dx, a.dy, b.dx, b.dy)) best = o;      // smallest ccw angle
            }
            if (best < 0 || used2[(size_t)best]) { st.anomalies++; return -1; }
            used2[(size_t)best] = 1;
            nx2[(size_t)i] = best;
        }
        seen2.assign((size_t)n2, 0);
        int64_t neg = 0, pos = 0;
        for (int i = 0; i < n2; ++i) {
            if (seen2[(size_t)i]) continue;
            long double a2 = 0;
            int s0 = i;
            while (!seen2[(size_t)s0]) {
                seen2[(size_t)s0] = 1;
                const VKey& p = vk[(size_t)seg2[(size_t)s0].a]; const VKey& r = vk[(size_t)seg2[(size_t)s0].b];
                const long double px = (long double)p.n / (long double)p.d, rx = (long double)r.n / (long double)r.d;
                a2 += px * (long double)r.y - rx * (long double)p.y;
                s0 = nx2[(size_t)s0];
            }
            if (s0 != i) { st.anomalies++; return -1; }
            if (a2 > 0) pos++; else neg++;
        }
        if (pos != 1) st.anomalies++;
        return neg;
    }

    void trace_emit(const int64_t* M, int64_t nm, ChunkOut& out) {
        const int ns = (int)seg.size();
        // --- trace loops, drop collinear vertices, require lattice vertices
        nd.clear(); loop_start.clear(); loop_area.clear();
        seen.assign((size_t)ns, 0);
        for (int i = 0; i < ns; ++i) {
            if (seen[(size_t)i] || nxt[(size_t)i] < 0) continue;
            ls.clear();
            int s = i; bool ok = true;
            while (!seen[(size_t)s]) { seen[(size_t)s] = 1; ls.push_back(s); s = nxt[(size_t)s]; if (s < 0) { ok = false; break; } }
            if (!ok || s != i) { out.st.anomalies++; continue; }
            cv.clear();
            const size_t L = ls.size();
            for (size_t k = 0; k < L; ++k) {
                const Seg& a = seg[(size_t)ls[(k + L - 1) % L]]; const Seg& b = seg[(size_t)ls[k]];
                if (a.dx == b.dx && a.dy == b.dy) continue;
                cv.push_back(b.a);
            }
            if (cv.size() < 3) { out.st.anomalies++; continue; }
            const int base = (int)nd.size();
            bool lattice = true;
            for (size_t k = 0; k < cv.size(); ++k) {
                const VKey& q = vk[(size_t)cv[k]];
                if (q.d != 1) lattice = false;
                nd.push_back({ (int64_t)q.n, q.y, base + (int)((k + cv.size() - 1) % cv.size()), base + (int)((k + 1) % cv.size()) });
            }
            if (!lattice) { out.st.anomalies++; nd.resize((size_t)base); continue; }
            i128 a2 = 0;
            for (size_t k = 0; k < cv.size(); ++k) {
                const Node& p = nd[(size_t)base + k]; const Node& q = nd[(size_t)nd[(size_t)base + k].next];
                a2 += (i128)p.x * q.y - (i128)q.x * p.y;
            }
            loop_start.push_back(base); loop_area.push_back(a2);
        }
        // --- holes (clockwise loops): bridge each to a lattice vertex below it
        std::vector<std::pair<std::pair<int64_t, int64_t>, int>> holes;     // (lowest y, x), node
        for (size_t l = 0; l < loop_start.size(); ++l) {
            if (loop_area[l] >= 0) continue;
            int b = loop_start[l], n = b, lo = b;
            do { if (nd[(size_t)n].y < nd[(size_t)lo].y || (nd[(size_t)n].y == nd[(size_t)lo].y && nd[(size_t)n].x < nd[(size_t)lo].x)) lo = n; n = nd[(size_t)n].next; } while (n != b);
            holes.push_back({ { nd[(size_t)lo].y, nd[(size_t)lo].x }, lo });
        }
        if (!holes.empty()) bridge_holes(M, nm, holes, out);
        long double want = 0;                                 // 2A of the trapezoids
        for (int i = 0; i < nm; ++i) {
            const Trap& z = T[(size_t)M[i]];
            const Q bl = side_x(z.l, z.y1, z.y1), br = side_x(z.r, z.y1, z.y1), tl = side_x(z.l, z.y1, z.y2), tr = side_x(z.r, z.y1, z.y2);
            auto ld = [](const Q& q) { return (long double)q.n / (long double)q.d; };
            want += (long double)(z.y2 - z.y1) * (ld(br) - ld(bl) + ld(tr) - ld(tl));
        }
        const int64_t pts0 = (int64_t)out.pts.size();
        i128 got = 0;
        // --- emit every cycle that is (now) counter-clockwise
        vis.assign(nd.size(), 0);
        for (size_t i = 0; i < nd.size(); ++i) {
            if (vis[i]) continue;
            const int64_t p0 = (int64_t)out.pts.size();
            int n = (int)i; i128 a2 = 0;
            while (!vis[(size_t)n]) {
                vis[(size_t)n] = 1;
                out.pts.push_back({ nd[(size_t)n].x, nd[(size_t)n].y });
                const Node& q = nd[(size_t)nd[(size_t)n].next];
                a2 += (i128)nd[(size_t)n].x * q.y - (i128)q.x * nd[(size_t)n].y;
                n = nd[(size_t)n].next;
            }
            if (a2 <= 0) { out.st.anomalies++; out.pts.resize((size_t)p0); continue; }   // unbridged hole
            got += a2;
            out.pstart.push_back((int64_t)out.pts.size());
            out.st.polygons++; out.st.poly_vertices += (int64_t)out.pts.size() - p0;
        }
        if (std::fabs((long double)got - want) > 0.25L) {
            out.st.anomalies++;
            if (std::getenv("LDB_TRAPOUT_DEBUG")) {
                std::fprintf(stderr, "trapset: group area2 %.2Lf, polygons %.2Lf, %lld traps:", want, (long double)got, (long long)nm);
                for (int64_t q = pts0; q < (int64_t)out.pts.size(); ++q) std::fprintf(stderr, " (%lld,%lld)", (long long)out.pts[(size_t)q].x, (long long)out.pts[(size_t)q].y);
                std::fprintf(stderr, "\n");
                for (int i = 0; i < nm; ++i) { const Trap& z = T[(size_t)M[i]];
                    std::fprintf(stderr, "   y %lld..%lld L(ax %lld k %lld d %lld/%lld) R(ax %lld k %lld d %lld/%lld)\n", (long long)z.y1, (long long)z.y2,
                        (long long)z.l.ax, (long long)z.l.k, (long long)z.l.dx, (long long)z.l.dy, (long long)z.r.ax, (long long)z.r.k, (long long)z.r.dx, (long long)z.r.dy); }
            }
        }
    }

    void bridge_holes(const int64_t* M, int64_t nm, std::vector<std::pair<std::pair<int64_t, int64_t>, int>>& holes, ChunkOut& out) {
        std::sort(holes.begin(), holes.end());
        out.st.holes += (int64_t)holes.size();
        std::unordered_map<int64_t, std::vector<int64_t>> by_y2;            // row -> trapezoids ending
        std::unordered_map<int64_t, std::vector<int>> nodes_at;             // row -> nodes
        for (int64_t i = 0; i < nm; ++i) by_y2[T[(size_t)M[i]].y2].push_back(M[i]);
        for (size_t i = 0; i < nd.size(); ++i) nodes_at[nd[i].y].push_back((int)i);
        for (auto& h : holes) {
            const int vh = h.second;
            const int64_t vx = nd[(size_t)vh].x, vy = nd[(size_t)vh].y;
            // trapezoid whose top edge contains v (prefer v strictly inside the top edge)
            int64_t tb = -1;
            auto it = by_y2.find(vy);
            if (it != by_y2.end()) for (int64_t t : it->second) {
                const Trap& z = T[(size_t)t];
                const Q xl = side_x(z.l, z.y1, vy), xr = side_x(z.r, z.y1, vy);
                if (qcmp_int(xl, vx) <= 0 && qcmp_int(xr, vx) >= 0) {
                    if (tb < 0 || (qcmp_int(xl, vx) < 0 && qcmp_int(xr, vx) > 0)) tb = t;
                }
            }
            if (tb < 0) { out.st.anomalies++; continue; }
            const Trap& z = T[(size_t)tb];
            const Q bl = side_x(z.l, z.y1, z.y1), br = side_x(z.r, z.y1, z.y1);
            // a node on the bottom edge of tb whose wedge contains the direction towards v,
            // closest to vx; the bridge lies inside tb, so it is exact and crosses nothing
            int wn = -1; int64_t bestd = INT64_MAX;
            auto jt = nodes_at.find(z.y1);
            if (jt != nodes_at.end()) for (int n : jt->second) {
                const Node& c = nd[(size_t)n];
                if (qcmp_int(bl, c.x) > 0 || qcmp_int(br, c.x) < 0) continue;
                const int64_t ux = vx - c.x, uy = vy - c.y;
                const Node& p = nd[(size_t)c.prev]; const Node& q = nd[(size_t)c.next];
                if (!rel_between(q.x - c.x, q.y - c.y, ux, uy, p.x - c.x, p.y - c.y)) continue;
                const int64_t d = std::llabs(c.x - vx);
                if (d < bestd) { bestd = d; wn = n; }
            }
            if (wn < 0) { out.st.anomalies++; continue; }
            // splice: wn -> vh ... (hole) ... hp -> v' -> w' -> n1
            const int n1 = nd[(size_t)wn].next, hp = nd[(size_t)vh].prev;
            const int vp = (int)nd.size(); nd.push_back({ vx, vy, hp, -1 });
            const int wp = (int)nd.size(); nd.push_back({ nd[(size_t)wn].x, nd[(size_t)wn].y, vp, n1 });
            nd[(size_t)vp].next = wp;
            nd[(size_t)hp].next = vp;
            nd[(size_t)n1].prev = wp;
            nd[(size_t)wn].next = vh; nd[(size_t)vh].prev = wn;
            nodes_at[vy].push_back(vp); nodes_at[nd[(size_t)wp].y].push_back(wp);
        }
    }
};

int64_t find(std::vector<int64_t>& p, int64_t x) { while (p[(size_t)x] != x) { p[(size_t)x] = p[(size_t)p[(size_t)x]]; x = p[(size_t)x]; } return x; }
}

// traps: n records of 10 int32 (y1, y2, l.ax, l.k, l.dx, l.dy, r.ax, r.k, r.dx, r.dy).
// stats: trapezoid records, polygons, polygon vertices, groups, holes, max group size, anomalies.
namespace {
// Receives the exact output of trapset_run in a deterministic order.
struct TrapSink {
    virtual void trap(int64_t y1, int64_t y2, int64_t xbl, int64_t xbr, int64_t xtl, int64_t xtr) = 0;
    virtual void poly(const Pt* p, size_t n) = 0;
    virtual ~TrapSink() {}
};
struct WriterSink : TrapSink {
    oasis_writer::Writer* w; uint64_t layer, dt;
    void trap(int64_t y1, int64_t y2, int64_t bl, int64_t br, int64_t tl, int64_t tr) override {
        w->trapezoid_h(layer, dt, y1, y2, bl, br, tl, tr);
    }
    void poly(const Pt* p, size_t n) override { w->polygon(layer, dt, p, n); }
};
// Lattice polygons (CSR, 1-based offsets for Fortran); a lattice-corner trapezoid becomes a
// 3- or 4-point polygon.
struct PolySink : TrapSink {
    std::vector<int64_t> off{1}, xs, ys;
    void push(int64_t x, int64_t y) {
        const size_t b = (size_t)(off.back() - 1);
        if (xs.size() > b && xs.back() == x && ys.back() == y) return;
        xs.push_back(x); ys.push_back(y);
    }
    void close() {
        const size_t b = (size_t)(off.back() - 1);
        if (xs.size() > b + 1 && xs.back() == xs[b] && ys.back() == ys[b]) { xs.pop_back(); ys.pop_back(); }
        if (xs.size() < b + 3) { xs.resize(b); ys.resize(b); return; }
        off.push_back((int64_t)xs.size() + 1);
    }
    void trap(int64_t y1, int64_t y2, int64_t bl, int64_t br, int64_t tl, int64_t tr) override {
        push(bl, y1); push(br, y1); push(tr, y2); push(tl, y2); close();
    }
    void poly(const Pt* p, size_t n) override { for (size_t i = 0; i < n; ++i) push(p[i].x, p[i].y); close(); }
};
int trapset_run(int64_t n, const int32_t* rec, int64_t* stats, TrapSink& sink);
}

extern "C" int ldb_ow_trapset(void* h, int layer, int dt, int64_t n, const int32_t* rec, int64_t* stats) {
    WriterSink ws; ws.w = (oasis_writer::Writer*)h; ws.layer = (uint64_t)layer; ws.dt = (uint64_t)dt;
    return trapset_run(n, rec, stats, ws);
}

// Exact lattice polygons of a canonical trapezoid set (the same grouping as ldb_ow_trapset), for
// feeding a result back into the engine. Read with ldb_polys_count / ldb_polys_get (k = 0) and
// release with ldb_polys_free.
extern "C" void* ldb_trapset_polys(int64_t n, const int32_t* rec, int64_t* stats);

namespace {
int trapset_run(int64_t n, const int32_t* rec, int64_t* stats, TrapSink& sink) {
    using clk = std::chrono::steady_clock;
    const auto tA = clk::now();
    auto secs = [&](clk::time_point a) { return std::chrono::duration<double>(clk::now() - a).count(); };
    std::vector<Trap> T((size_t)n);
    for (int64_t i = 0; i < n; ++i) {
        const int32_t* r = rec + 10 * i;
        T[(size_t)i] = { r[0], r[1], { r[2], r[3], r[4], r[5] }, { r[6], r[7], r[8], r[9] } };
    }
    Output out{ nullptr, 0, 0, {} };
    int nth = (int)std::thread::hardware_concurrency();
    if (const char* e = std::getenv("OMP_NUM_THREADS")) { int v = std::atoi(e); if (v > 0) nth = v; }
    nth = std::max(1, nth);
    auto parallel_for = [&](int64_t cnt, const std::function<void(int64_t, int64_t)>& f) {
        const int k = (int)std::max<int64_t>(1, std::min<int64_t>(nth, cnt / 65536 + 1));
        std::vector<std::thread> th;
        for (int t = 0; t < k; ++t) th.emplace_back([&, t]() { f(cnt * t / k, cnt * (t + 1) / k); });
        for (auto& x : th) x.join();
    };
    // rational corners: bit 0 bl, 1 br, 2 tl, 3 tr
    std::vector<char> rat((size_t)n, 0);
    parallel_for(n, [&](int64_t a, int64_t b) {
        for (int64_t i = a; i < b; ++i) {
            const Trap& z = T[(size_t)i];
            int m = 0;
            if ((((i128)z.l.ax * z.l.dy + (i128)z.l.k * z.l.dx) % z.l.dy) != 0) m |= 1;
            if ((((i128)z.r.ax * z.r.dy + (i128)z.r.k * z.r.dx) % z.r.dy) != 0) m |= 2;
            if ((((i128)z.l.ax * z.l.dy + (i128)(z.y2 - z.y1 + z.l.k) * z.l.dx) % z.l.dy) != 0) m |= 4;
            if ((((i128)z.r.ax * z.r.dy + (i128)(z.y2 - z.y1 + z.r.k) * z.r.dx) % z.r.dy) != 0) m |= 8;
            rat[(size_t)i] = (char)m;
        }
    });
    // Join across rational corners: a bottom corner (row y1) and a top corner (row y2) of the
    // trapezoid below, on the same side line, must pair up one to one. Entries are sorted by a
    // 64-bit hash of (row, line, side); equal hashes are compared exactly.
    struct Ent { uint64_t h; int64_t id; int8_t top, right; };
    auto key_of = [&](int64_t id, int top, int right) {
        const Trap& z = T[(size_t)id];
        const Side& sd = right ? z.r : z.l;
        LineKey k = line_key(sd, z.y1, top ? z.y2 : z.y1);
        return k;
    };
    auto hash_of = [&](const LineKey& k, int right) {
        uint64_t h = LineHash()(k) * 0x9E3779B97F4A7C15ull + (uint64_t)right;
        h ^= h >> 29; h *= 0xBF58476D1CE4E5B9ull; h ^= h >> 32;
        return h;
    };
    std::vector<int64_t> ecount((size_t)n + 1, 0);
    for (int64_t i = 0; i < n; ++i) ecount[(size_t)i + 1] = ecount[(size_t)i] + __builtin_popcount((unsigned)rat[(size_t)i]);
    const int64_t ne = ecount[(size_t)n];
    std::vector<Ent> E((size_t)ne);
    parallel_for(n, [&](int64_t a, int64_t b) {
        for (int64_t i = a; i < b; ++i) {
            int64_t p = ecount[(size_t)i];
            for (int c = 0; c < 4; ++c) {
                if (!(rat[(size_t)i] & (1 << c))) continue;
                const int top = c >= 2, right = c & 1;
                E[(size_t)p++] = { hash_of(key_of(i, top, right), right), i, (int8_t)top, (int8_t)right };
            }
        }
    });
    // bucket by the top 12 hash bits, sort buckets in parallel
    {
        const int NB = 4096;
        std::vector<int64_t> bc((size_t)NB + 1, 0);
        for (auto& e : E) bc[(size_t)(e.h >> 52) + 1]++;
        for (int b = 0; b < NB; ++b) bc[(size_t)b + 1] += bc[(size_t)b];
        std::vector<Ent> S2((size_t)ne);
        { std::vector<int64_t> pos(bc.begin(), bc.end() - 1);
          for (auto& e : E) S2[(size_t)pos[(size_t)(e.h >> 52)]++] = e; }
        E.swap(S2);
        auto lt = [](const Ent& a, const Ent& b) { return a.h != b.h ? a.h < b.h : (a.top != b.top ? a.top < b.top : a.id < b.id); };
        parallel_for(NB, [&](int64_t a, int64_t b) {
            for (int64_t q = a; q < b; ++q) std::sort(E.begin() + bc[(size_t)q], E.begin() + bc[(size_t)q + 1], lt);
        });
    }
    std::vector<int64_t> par((size_t)n);
    std::iota(par.begin(), par.end(), (int64_t)0);
    for (int64_t p = 0; p < ne;) {
        int64_t q = p;
        while (q < ne && E[(size_t)q].h == E[(size_t)p].h) ++q;
        // pair every top corner with the bottom corner of the same exact key
        for (int64_t a = p; a < q; ++a) {
            if (!E[(size_t)a].top) continue;
            const LineKey ka = key_of(E[(size_t)a].id, 1, E[(size_t)a].right);
            int64_t m = -1;
            for (int64_t b = p; b < q; ++b) {
                if (E[(size_t)b].top || E[(size_t)b].right != E[(size_t)a].right || E[(size_t)b].id < 0) continue;
                if (key_of(E[(size_t)b].id, 0, E[(size_t)b].right) == ka) { m = b; break; }
            }
            if (m < 0) { out.st.anomalies++; continue; }
            par[(size_t)find(par, E[(size_t)a].id)] = find(par, E[(size_t)m].id);
            E[(size_t)m].id = -1 - E[(size_t)m].id;             // consumed
        }
        for (int64_t b = p; b < q; ++b) if (!E[(size_t)b].top && E[(size_t)b].id >= 0) out.st.anomalies++;
        p = q;
    }
    { std::vector<Ent>().swap(E); }
    // groups
    std::vector<int64_t> root((size_t)n), cnt((size_t)n, 0);
    for (int64_t i = 0; i < n; ++i) { root[(size_t)i] = find(par, i); cnt[(size_t)root[(size_t)i]]++; }
    std::vector<int64_t> start((size_t)n + 1, 0), order((size_t)n);
    for (int64_t i = 0; i < n; ++i) start[(size_t)root[(size_t)i] + 1]++;
    for (int64_t i = 0; i < n; ++i) start[(size_t)i + 1] += start[(size_t)i];
    { std::vector<int64_t> pos(start.begin(), start.end() - 1);
      for (int64_t i = 0; i < n; ++i) order[(size_t)pos[(size_t)root[(size_t)i]]++] = i; }
    const double t_join = secs(tA);
    const auto tB = clk::now();
    // groups in order: singletons with lattice corners become records, the rest polygons
    std::vector<int64_t> gl;                     // group starts (non-empty)
    for (int64_t g = 0; g < n; ++g) if (start[(size_t)g] != start[(size_t)g + 1]) gl.push_back(g);
    const int64_t ng = (int64_t)gl.size(), CH = 2048;
    const int64_t nch = (ng + CH - 1) / CH;
    std::vector<ChunkOut> chunks((size_t)nch);
    std::atomic<int64_t> next_chunk{0};
    nth = std::max(1, std::min<int>(nth, (int)std::max<int64_t>(1, nch)));
    auto worker = [&]() {
        GroupPoly gp(T);
        for (;;) {
            const int64_t c = next_chunk.fetch_add(1);
            if (c >= nch) break;
            ChunkOut& co = chunks[(size_t)c];
            for (int64_t q = c * CH; q < std::min(ng, (c + 1) * CH); ++q) {
                const int64_t g = gl[(size_t)q];
                const int64_t a = start[(size_t)g], b = start[(size_t)g + 1];
                if (b - a == 1 && rat[(size_t)order[(size_t)a]] == 0) continue;     // record, written below
                co.st.groups++;
                co.st.max_group = std::max<int64_t>(co.st.max_group, b - a);
                gp.run(order.data() + a, b - a, co);
            }
        }
    };
    std::vector<std::thread> th;
    for (int t = 1; t < nth; ++t) th.emplace_back(worker);
    worker();
    for (auto& t : th) t.join();
    const double t_poly = secs(tB);
    const auto tC = clk::now();
    FILE* dump = nullptr;
    if (const char* e = std::getenv("LDB_TRAPOUT_DUMP")) dump = std::fopen(e, "a");
    for (int64_t c = 0; c < nch; ++c) {
        ChunkOut& co = chunks[(size_t)c];
        size_t pk = 0;
        for (int64_t q = c * CH; q < std::min(ng, (c + 1) * CH); ++q) {
            const int64_t g = gl[(size_t)q];
            const int64_t a = start[(size_t)g], b = start[(size_t)g + 1];
            if (b - a == 1 && rat[(size_t)order[(size_t)a]] == 0) {
                const Trap& z = T[(size_t)order[(size_t)a]];
                const Q bl = side_x(z.l, z.y1, z.y1), br = side_x(z.r, z.y1, z.y1);
                const Q tl = side_x(z.l, z.y1, z.y2), tr = side_x(z.r, z.y1, z.y2);
                sink.trap(z.y1, z.y2, (int64_t)bl.n, (int64_t)br.n, (int64_t)tl.n, (int64_t)tr.n);
                if (dump) std::fprintf(dump, "T %lld %lld %lld %lld %lld %lld\n", (long long)z.y1, (long long)z.y2, (long long)bl.n, (long long)br.n, (long long)tl.n, (long long)tr.n);
                out.st.trap_records++;
            }
        }
        for (pk = 0; pk + 1 < co.pstart.size(); ++pk)
        {
            sink.poly(co.pts.data() + co.pstart[pk], (size_t)(co.pstart[pk + 1] - co.pstart[pk]));
            if (dump) { std::fprintf(dump, "P");
                for (int64_t q = co.pstart[pk]; q < co.pstart[pk + 1]; ++q) std::fprintf(dump, " %lld %lld", (long long)co.pts[(size_t)q].x, (long long)co.pts[(size_t)q].y);
                std::fprintf(dump, "\n"); }
        }
        out.st.polygons += co.st.polygons; out.st.poly_vertices += co.st.poly_vertices;
        out.st.groups += co.st.groups; out.st.holes += co.st.holes; out.st.anomalies += co.st.anomalies;
        out.st.max_group = std::max(out.st.max_group, co.st.max_group);
        ChunkOut().pts.swap(co.pts);
    }
    if (dump) std::fclose(dump);
    if (std::getenv("LDB_TRAPOUT_TIMING"))
        std::fprintf(stderr, "trapset: join %.3f s, polygons %.3f s (%d threads), write %.3f s\n", t_join, t_poly, nth, secs(tC));
    if (stats) {
        stats[0] = out.st.trap_records; stats[1] = out.st.polygons; stats[2] = out.st.poly_vertices;
        stats[3] = out.st.groups; stats[4] = out.st.holes; stats[5] = out.st.max_group; stats[6] = out.st.anomalies;
    }
    return out.st.anomalies == 0 ? 0 : 1;
}
}

struct TrapPolySet { std::vector<int64_t> off, xs, ys, poly_off; };
extern "C" void* ldb_trapset_polys(int64_t n, const int32_t* rec, int64_t* stats) {
    PolySink ps;
    trapset_run(n, rec, stats, ps);
    auto* r = new TrapPolySet();
    r->off.swap(ps.off); r->xs.swap(ps.xs); r->ys.swap(ps.ys);
    return r;
}
extern "C" int64_t ldb_trapset_polys_count(void* h, int64_t* nvert) {
    auto* r = (TrapPolySet*)h; *nvert = (int64_t)r->xs.size(); return (int64_t)r->off.size() - 1;
}
extern "C" void ldb_trapset_polys_get(void* h, int64_t* poff, int64_t* xs, int64_t* ys) {
    auto* r = (TrapPolySet*)h;
    std::memcpy(poff, r->off.data(), r->off.size() * sizeof(int64_t));
    if (!r->xs.empty()) { std::memcpy(xs, r->xs.data(), r->xs.size() * 8); std::memcpy(ys, r->ys.data(), r->ys.size() * 8); }
}
extern "C" void ldb_trapset_polys_free(void* h) { delete (TrapPolySet*)h; }

// Per-polygon measures of a canonical trapezoid set: lab[i] = PNUM ordinal (1..np) of trap i.
// Outputs (np each): vertices, holes, perimeter (dbu, real). stats[0] = anomalies.
extern "C" int ldb_trapset_measure(int64_t n, const int32_t* rec, const int64_t* lab, int64_t np,
                                   int64_t* nvert, int64_t* holes, double* perim, int64_t* stats) {
    std::vector<Trap> T((size_t)n);
    for (int64_t i = 0; i < n; ++i) {
        const int32_t* r = rec + 10 * i;
        T[(size_t)i] = { r[0], r[1], { r[2], r[3], r[4], r[5] }, { r[6], r[7], r[8], r[9] } };
    }
    std::vector<int64_t> start((size_t)np + 2, 0), order((size_t)n);
    for (int64_t i = 0; i < n; ++i) start[(size_t)lab[i] + 1]++;
    for (int64_t p = 0; p <= np; ++p) start[(size_t)p + 1] += start[(size_t)p];
    { std::vector<int64_t> pos(start.begin(), start.end() - 1);
      for (int64_t i = 0; i < n; ++i) order[(size_t)pos[(size_t)lab[i]]++] = i; }
    int nth = (int)std::thread::hardware_concurrency();
    if (const char* e = std::getenv("OMP_NUM_THREADS")) { int v = std::atoi(e); if (v > 0) nth = v; }
    nth = std::max(1, nth);
    std::atomic<int64_t> next{1}, anom{0};
    auto worker = [&]() {
        GroupPoly gp(T); Stats st;
        for (;;) {
            const int64_t p0 = next.fetch_add(256);
            if (p0 > np) break;
            for (int64_t p = p0; p < std::min(np + 1, p0 + 256); ++p) {
                int64_t lp = 0;
                gp.measure(order.data() + start[(size_t)p], start[(size_t)p + 1] - start[(size_t)p],
                           nvert[p - 1], holes[p - 1], perim[p - 1], lp, st);
            }
        }
        anom += st.anomalies;
    };
    std::vector<std::thread> th;
    for (int t = 1; t < nth; ++t) th.emplace_back(worker);
    worker();
    for (auto& t : th) t.join();
    if (stats) stats[0] = anom.load();
    return anom.load() == 0 ? 0 : 1;
}

// Unbridged oriented boundary loops (outer CCW, holes CW) of a canonical trapezoid set,
// grouped by PNUM ordinal lab[i] in 1..np. Returns a TrapPolySet* handle (read with
// ldb_trapset_polys_count / ldb_trapset_polys_get, release with ldb_trapset_polys_free).
extern "C" void* ldb_trapset_loops(int64_t n, const int32_t* rec, const int64_t* lab, int64_t np,
                                   int64_t* stats) {
    auto* r = new TrapPolySet();
    r->off.push_back(1);
    r->poly_off.push_back(1);
    if (n <= 0 || np <= 0) {
        if (stats) stats[0] = 0;
        return r;
    }
    std::vector<Trap> T((size_t)n);
    for (int64_t i = 0; i < n; ++i) {
        const int32_t* p = rec + 10 * i;
        T[(size_t)i] = { p[0], p[1], { p[2], p[3], p[4], p[5] }, { p[6], p[7], p[8], p[9] } };
    }
    std::vector<int64_t> start((size_t)np + 2, 0), order((size_t)n);
    for (int64_t i = 0; i < n; ++i) start[(size_t)lab[i] + 1]++;
    for (int64_t p = 0; p <= np; ++p) start[(size_t)p + 1] += start[(size_t)p];
    { std::vector<int64_t> pos(start.begin(), start.end() - 1);
      for (int64_t i = 0; i < n; ++i) order[(size_t)pos[(size_t)lab[i]]++] = i; }

    int nth = (int)std::thread::hardware_concurrency();
    if (const char* e = std::getenv("OMP_NUM_THREADS")) { int v = std::atoi(e); if (v > 0) nth = v; }
    nth = std::max(1, std::min(nth, (int)np));
    struct Chunk { std::vector<int64_t> off{1}, xs, ys, pcnt; int64_t anom = 0; };
    std::vector<Chunk> chunks((size_t)nth);
    auto worker = [&](int tid) {
        const int64_t p0 = 1 + (np * tid) / nth;
        const int64_t p1 = 1 + (np * (tid + 1)) / nth;
        GroupPoly gp(T); Stats st;
        Chunk& c = chunks[(size_t)tid];
        c.pcnt.reserve((size_t)(p1 - p0));
        for (int64_t p = p0; p < p1; ++p) {
            const size_t before = c.off.size();
            gp.extract_loops(order.data() + start[(size_t)p], start[(size_t)p + 1] - start[(size_t)p],
                             c.off, c.xs, c.ys, st);
            c.pcnt.push_back((int64_t)(c.off.size() - before));
        }
        c.anom = st.anomalies;
    };
    std::vector<std::thread> th;
    for (int t = 1; t < nth; ++t) th.emplace_back(worker, t);
    worker(0);
    for (auto& t : th) t.join();

    int64_t tot_loops = 0, tot_verts = 0, anom = 0;
    for (const auto& c : chunks) {
        tot_loops += (int64_t)c.off.size() - 1;
        tot_verts += (int64_t)c.xs.size();
        anom += c.anom;
    }
    r->off.reserve((size_t)tot_loops + 1);
    r->xs.reserve((size_t)tot_verts);
    r->ys.reserve((size_t)tot_verts);
    r->poly_off.reserve((size_t)np + 1);
    int64_t vbase = 0;
    for (const auto& c : chunks) {
        for (int64_t cnt : c.pcnt) r->poly_off.push_back(r->poly_off.back() + cnt);
        for (size_t k = 1; k < c.off.size(); ++k) r->off.push_back(vbase + c.off[k]);
        r->xs.insert(r->xs.end(), c.xs.begin(), c.xs.end());
        r->ys.insert(r->ys.end(), c.ys.begin(), c.ys.end());
        vbase += (int64_t)c.xs.size();
    }
    if (stats) stats[0] = anom;
    return r;
}

extern "C" void ldb_trapset_loops_poly_off(void* h, int64_t* poff) {
    auto* r = (TrapPolySet*)h;
    if (!r->poly_off.empty()) std::memcpy(poff, r->poly_off.data(), r->poly_off.size() * sizeof(int64_t));
}
