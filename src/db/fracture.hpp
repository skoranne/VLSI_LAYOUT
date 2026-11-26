#pragma once
// Shapes -> axis-parallel rectangles, shared by the GDSII and OASIS readers.

#include <cstdint>
#include <vector>
#include <array>
#include <algorithm>
#include <cmath>
#include <cstdlib>

namespace ldbgeom {

struct Pt { int64_t x, y; };
typedef std::array<int64_t, 4> Rect;     // x1, y1, x2, y2

// ---------------------------------------------------------------------------
// Manhattan polygon -> disjoint rectangles (horizontal slabs, even-odd fill,
// vertically adjacent slabs with the same span are merged)
// ---------------------------------------------------------------------------
inline bool is_manhattan(const std::vector<Pt>& v) {
    for (size_t i = 0, n = v.size(); i < n; ++i) {
        const Pt& a = v[i]; const Pt& b = v[(i + 1) % n];
        if (a.x != b.x && a.y != b.y) return false;
    }
    return true;
}

inline void fracture_manhattan(const std::vector<Pt>& v, std::vector<Rect>& out) {
    struct VEdge { int64_t x, ylo, yhi; };
    struct Span { int64_t x1, x2, y0; };
    std::vector<VEdge> edges; std::vector<int64_t> ys;
    for (size_t i = 0, n = v.size(); i < n; ++i) {
        const Pt& a = v[i]; const Pt& b = v[(i + 1) % n];
        if (a.x == b.x && a.y != b.y) edges.push_back({ a.x, std::min(a.y, b.y), std::max(a.y, b.y) });
        ys.push_back(a.y);
    }
    std::sort(ys.begin(), ys.end()); ys.erase(std::unique(ys.begin(), ys.end()), ys.end());
    std::sort(edges.begin(), edges.end(), [](const VEdge& a, const VEdge& b) { return a.ylo < b.ylo; });
    std::vector<VEdge> live; std::vector<int64_t> xs; std::vector<Span> open, cur;
    size_t next_edge = 0;
    for (size_t i = 0; i + 1 < ys.size(); ++i) {
        int64_t ylo = ys[i];
        live.erase(std::remove_if(live.begin(), live.end(), [&](const VEdge& e) { return e.yhi <= ylo; }), live.end());
        while (next_edge < edges.size() && edges[next_edge].ylo <= ylo) live.push_back(edges[next_edge++]);
        xs.clear(); for (auto& e : live) xs.push_back(e.x);
        std::sort(xs.begin(), xs.end());
        cur.clear();
        for (size_t k = 0; k + 1 < xs.size(); k += 2) {
            if (xs[k] == xs[k + 1]) continue;
            if (!cur.empty() && cur.back().x2 == xs[k]) cur.back().x2 = xs[k + 1];
            else cur.push_back({ xs[k], xs[k + 1], ylo });
        }
        // Spans unchanged from the slab below keep growing; the others are closed here.
        size_t a = 0, b = 0;
        while (a < open.size() || b < cur.size()) {
            if (a < open.size() && b < cur.size() && open[a].x1 == cur[b].x1 && open[a].x2 == cur[b].x2) {
                cur[b].y0 = open[a].y0; ++a; ++b;
            } else if (b >= cur.size() || (a < open.size() && open[a].x1 <= cur[b].x1)) {
                out.push_back({ open[a].x1, open[a].y0, open[a].x2, ylo }); ++a;
            } else ++b;
        }
        open.swap(cur);
    }
    if (!ys.empty()) for (auto& o : open) out.push_back({ o.x1, o.y0, o.x2, ys.back() });
}

// Strips per vertex-to-vertex slab used for shapes with slanted edges.  More
// strips follow the edge more closely but multiply the box count of every
// instance of the shape.  Default 4; override with LDB_SLANT_STRIPS=1..64.
inline int64_t slant_strips() {
    static const int64_t n = [] {
        const char* e = std::getenv("LDB_SLANT_STRIPS");
        const long v = e ? std::strtol(e, nullptr, 10) : 4;
        return (int64_t)std::min(64L, std::max(1L, v));
    }();
    return n;
}

// Any simple polygon -> rectangle strips.  Each slab between consecutive vertex
// ordinates is cut into at most slant_strips() strips; a strip spans the
// polygon's even-odd interior at its mid height, rounded to the grid.
inline void fracture_general(const std::vector<Pt>& v, std::vector<Rect>& out) {
    std::vector<int64_t> ys; std::vector<double> xs;
    for (auto& q : v) ys.push_back(q.y);
    std::sort(ys.begin(), ys.end()); ys.erase(std::unique(ys.begin(), ys.end()), ys.end());
    const size_t n = v.size();
    for (size_t i = 0; i + 1 < ys.size(); ++i) {
        const int64_t height = ys[i + 1] - ys[i];
        const int64_t strips = std::min<int64_t>(slant_strips(), height);
        for (int64_t k = 0; k < strips; ++k) {
            const int64_t y0 = ys[i] + height * k / strips, y1 = ys[i] + height * (k + 1) / strips;
            const double ym = 0.5 * ((double)y0 + (double)y1);
            xs.clear();
            for (size_t e = 0; e < n; ++e) {
                const Pt& a = v[e]; const Pt& b = v[(e + 1) % n];
                if (a.y == b.y) continue;
                if (ym < (double)std::min(a.y, b.y) || ym > (double)std::max(a.y, b.y)) continue;
                xs.push_back((double)a.x + ((double)b.x - (double)a.x) * (ym - (double)a.y) / ((double)b.y - (double)a.y));
            }
            std::sort(xs.begin(), xs.end());
            for (size_t j = 0; j + 1 < xs.size(); j += 2) {
                const int64_t x0 = (int64_t)std::llround(xs[j]), x1 = (int64_t)std::llround(xs[j + 1]);
                if (x1 > x0) out.push_back({ x0, y0, x1, y1 });
            }
        }
    }
}

// Manhattan path (centre line `v`, half width `hw`, end extensions) -> one
// rectangle per segment, pairwise disjoint along the path.  Returns false and
// leaves `out` untouched when a segment is slanted.
inline bool path_to_rects(const std::vector<Pt>& v, int64_t hw, int64_t ext_start, int64_t ext_end, std::vector<Rect>& out) {
    for (size_t i = 0; i + 1 < v.size(); ++i)
        if (v[i].x != v[i + 1].x && v[i].y != v[i + 1].y) return false;
    const size_t nseg = v.empty() ? 0 : v.size() - 1;
    for (size_t i = 0; i < nseg; ++i) {
        const Pt& a = v[i]; const Pt& b = v[i + 1];
        // The joint square belongs to the incoming segment, so a segment
        // starts half a width after its first point unless it is the first.
        const int64_t ea = (i == 0) ? ext_start : -hw, eb = (i + 1 == nseg) ? ext_end : hw;
        const int64_t dir = (a.x < b.x || a.y < b.y) ? 1 : -1;
        if (a.y == b.y) {
            const int64_t x1 = a.x - dir * ea, x2 = b.x + dir * eb;
            if ((x2 - x1) * dir > 0) out.push_back({ std::min(x1, x2), a.y - hw, std::max(x1, x2), a.y + hw });
        } else {
            const int64_t y1 = a.y - dir * ea, y2 = b.y + dir * eb;
            if ((y2 - y1) * dir > 0) out.push_back({ a.x - hw, std::min(y1, y2), a.x + hw, std::max(y1, y2) });
        }
    }
    return true;
}

// Bounding box of a path outline of any direction: each segment is a rectangle of half-width
// hw around the centreline, the first/last segment extended by ext_start/ext_end along its
// direction, and interior joints mitred (edges extended to meet; 27.10 leaves joints to the
// application, this is the common GDSII/KLayout reading). The miter is limited to 1/cos(80 deg)
// times hw for very sharp turns. Coordinates are rounded outward. false if v has no segment.
inline bool path_bbox(const std::vector<Pt>& v0, int64_t hw, int64_t ext_start, int64_t ext_end,
                      int64_t& x1, int64_t& y1, int64_t& x2, int64_t& y2) {
    std::vector<Pt> v;
    for (auto& q : v0) if (v.empty() || v.back().x != q.x || v.back().y != q.y) v.push_back(q);
    if (v.size() < 2) return false;
    double bx1 = 1e300, by1 = 1e300, bx2 = -1e300, by2 = -1e300;
    auto add = [&](double x, double y) { bx1 = std::min(bx1, x); by1 = std::min(by1, y); bx2 = std::max(bx2, x); by2 = std::max(by2, y); };
    const size_t ns = v.size() - 1;
    const double w = (double)hw;
    std::vector<double> ux(ns), uy(ns);
    for (size_t i = 0; i < ns; ++i) {
        const double dx = (double)(v[i + 1].x - v[i].x), dy = (double)(v[i + 1].y - v[i].y), l = std::sqrt(dx * dx + dy * dy);
        ux[i] = dx / l; uy[i] = dy / l;
    }
    for (size_t i = 0; i < ns; ++i) {
        double ax = (double)v[i].x, ay = (double)v[i].y, bx = (double)v[i + 1].x, by = (double)v[i + 1].y;
        if (i == 0) { ax -= ux[i] * (double)ext_start; ay -= uy[i] * (double)ext_start; }
        if (i + 1 == ns) { bx += ux[i] * (double)ext_end; by += uy[i] * (double)ext_end; }
        const double nx = -uy[i] * w, ny = ux[i] * w;
        add(ax + nx, ay + ny); add(ax - nx, ay - ny); add(bx + nx, by + ny); add(bx - nx, by - ny);
    }
    for (size_t i = 1; i < ns; ++i) {                         // miter points at joint v[i]
        const double c = ux[i - 1] * ux[i] + uy[i - 1] * uy[i]; // cos of the turn
        const double half = std::sqrt(std::max(0.0, (1.0 + c) / 2.0));   // cos(turn/2)
        const double f = 1.0 / std::max(half, std::cos(80.0 * M_PI / 180.0));
        double mx = -uy[i - 1] - uy[i], my = ux[i - 1] + ux[i];  // sum of the two left normals
        const double ml = std::sqrt(mx * mx + my * my);
        if (ml < 1e-12) continue;
        mx = mx / ml * w * f; my = my / ml * w * f;
        add((double)v[i].x + mx, (double)v[i].y + my); add((double)v[i].x - mx, (double)v[i].y - my);
    }
    const double eps = 1e-9;
    x1 = (int64_t)std::floor(bx1 + eps); y1 = (int64_t)std::floor(by1 + eps);
    x2 = (int64_t)std::ceil(bx2 - eps); y2 = (int64_t)std::ceil(by2 - eps);
    return true;
}

} // namespace ldbgeom
