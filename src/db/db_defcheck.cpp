// File   : db_defcheck.cpp
// Author : Sandeep Koranne (C) 2026. All rights reserved.
// Purpose: Cross-check a layout (OASIS or GDSII, read with layoutdb) against the DEF it was
//          generated from, optionally with the LEF libraries (tech LEF + cell LEFs).
//
//   db_defcheck [options] design.def[.gz] layout.{oas,gds}
//
// The DEF is streamed through zlib (gzopen/gzread read plain files transparently), so a
// multi-GB .def.gz never has to be decompressed on disk. Nothing from the DEF is stored except
// the COMPONENTS list; every routing shape is turned into point probes and tested at once.
//
// Checks (each is reported; the exit status is 1 if any fails):
//   DIE        DIEAREA equals the top cell's boundary (prBoundary layer, else its extent).
//   COMPONENTS every placed DEF component is a top-cell instance of the same cell, at the same
//              lower-left corner and orientation, and vice versa (multiset compare).
//   ROUTING    every regular/special wire segment (end points, mid point and, when the tech
//              LEF gives the layer width, the inset corners of the wire rectangle), every via
//              (rectangles of its DEF VIAS / LEF VIA definition, or the layer stack rule),
//              every RECT / + RECT / + POLYGON and every PIN shape lands on top-cell geometry
//              of the mapped layer.
//   XOR        the DEF geometry rebuilt as rectangles (wires with the tech LEF WIDTH, extended
//              by half width for regular nets or by the explicit extension, flush for special
//              nets; via rectangles; RECTs; PIN shapes) is XORed per layer with the top-cell
//              geometry (union-area sweep, 50 um tiles, differing tiles refined to ~1 um
//              windows). A layer is skipped when any of its DEF shapes has no exact geometry.
//              PIN shapes are also XORed against the pin datatype.
//   LEF        (with --lef) every PIN/OBS RECT of every LEF macro used by the layout is
//              covered by that cell's own geometry on the drawing datatype of its layer
//              (centre + inset corners); pin-datatype-only hits are reported separately.
//
// Layer stack (routing / cut alternating, bottom up). Default is SKY130:
//   li1:67/20:67/16 mcon:67/44 met1:68/20:68/16 via:68/44 met2:69/20:69/16 via2:69/44
//   met3:70/20:70/16 via3:70/44 met4:71/20:71/16 via4:71/44 met5:72/20:72/16
// (name:drawL/drawD[:pinL/pinD]); override with --stack "<same syntax>".
#include "layoutdb.h"
#include "layoutdb_hier.h"
#include "layoutdb_internal.hpp"
#include <zlib.h>
#include <algorithm>
#include <cinttypes>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <array>
#include <map>
#include <memory>
#include <tuple>
#include <string>
#include <unordered_map>
#include <vector>

#include "def_reader.hpp"

namespace {
using namespace defr;


// ------------------------------------------------------------------ layout shape index
struct ShapeIndex {
    std::vector<Rect> s;
    std::vector<uint8_t> hit;
    int64_t x0 = 0, y0 = 0, gs = 4096;
    int64_t nx = 0, ny = 0;
    std::vector<uint32_t> start, items;

    void build() {
        hit.assign(s.size(), 0);
        if (s.empty()) return;
        int64_t xa = INT64_MAX, ya = INT64_MAX, xb = INT64_MIN, yb = INT64_MIN;
        for (auto& r : s) { xa = std::min(xa, r.x1); ya = std::min(ya, r.y1); xb = std::max(xb, r.x2); yb = std::max(yb, r.y2); }
        x0 = xa; y0 = ya;
        // aim for ~4 shapes per bucket, bounded grid
        double area = (double)(xb - xa + 1) * (double)(yb - ya + 1);
        gs = (int64_t)std::sqrt(area / std::max<double>(1.0, (double)s.size() / 4.0));
        gs = std::max<int64_t>(gs, 256);
        nx = (xb - xa) / gs + 1; ny = (yb - ya) / gs + 1;
        while (nx * ny > (int64_t)8e6) { gs *= 2; nx = (xb - xa) / gs + 1; ny = (yb - ya) / gs + 1; }
        std::vector<uint32_t> cnt((size_t)(nx * ny) + 1, 0);
        for (auto& r : s)
            for (int64_t gy = (r.y1 - y0) / gs; gy <= (r.y2 - y0) / gs; ++gy)
                for (int64_t gx = (r.x1 - x0) / gs; gx <= (r.x2 - x0) / gs; ++gx) ++cnt[(size_t)(gy * nx + gx)];
        start.assign(cnt.size() + 1, 0);
        for (size_t k = 0; k < cnt.size(); ++k) start[k + 1] = start[k] + cnt[k];
        items.resize(start.back());
        std::vector<uint32_t> fill(start.begin(), start.end() - 1);
        for (uint32_t i = 0; i < s.size(); ++i) {
            auto& r = s[i];
            for (int64_t gy = (r.y1 - y0) / gs; gy <= (r.y2 - y0) / gs; ++gy)
                for (int64_t gx = (r.x1 - x0) / gs; gx <= (r.x2 - x0) / gs; ++gx) items[fill[(size_t)(gy * nx + gx)]++] = i;
        }
    }
    // inclusive point test; marks every shape containing the point
    bool probe(int64_t x, int64_t y, bool mark = true) {
        if (s.empty()) return false;
        int64_t gx = (x - x0) / gs, gy = (y - y0) / gs;
        if (x < x0 || y < y0 || gx >= nx || gy >= ny) return false;
        size_t b = (size_t)(gy * nx + gx);
        bool any = false;
        for (uint32_t k = start[b]; k < start[b + 1]; ++k) {
            const Rect& r = s[items[k]];
            if (x >= r.x1 && x <= r.x2 && y >= r.y1 && y <= r.y2) {
                any = true;
                if (!mark) return true;
                hit[items[k]] = 1;
            }
        }
        return any;
    }
};

// small per-cell geometry for LEF checks (linear scan is fine: std cells are tiny)
struct CellGeom { std::map<std::pair<int, int>, std::vector<Rect>> layers; };
bool cell_probe(const CellGeom& g, int l, int d, int64_t x, int64_t y) {
    auto it = g.layers.find({l, d});
    if (it == g.layers.end()) return false;
    for (auto& r : it->second) if (x >= r.x1 && x <= r.x2 && y >= r.y1 && y <= r.y2) return true;
    return false;
}

// ------------------------------------------------------------------ the checker
struct Sample { std::string what; int64_t x, y; };

struct Checker : DefReader {
    std::map<std::pair<int, int>, ShapeIndex> top;       // (L,D) -> top-cell shapes
    int max_samples = 10;
    struct Stat { int64_t n = 0, hit = 0, pin_only = 0; };
    std::map<std::pair<int, int>, Stat> stat;            // (src, stack layer) -> stat
    std::vector<Sample> miss_samples;
    int64_t n_miss = 0;

    void probe_point(int src, int li, int64_t x, int64_t y, const char* what) override {
        const StackLayer& sl = stack[li];
        Stat& st = stat[{src, li}];
        ++st.n;
        int64_t lx = L(x), ly = L(y);
        bool ok = false;
        auto it = top.find({sl.dl, sl.dd});
        if (it != top.end()) ok = it->second.probe(lx, ly);
        if (ok) { ++st.hit; return; }
        if (src == PIN && sl.pl >= 0) {
            auto ip = top.find({sl.pl, sl.pd});
            if (ip != top.end() && ip->second.probe(lx, ly)) { ++st.hit; ++st.pin_only; return; }
        }
        ++n_miss;
        if ((int)miss_samples.size() < max_samples)
            miss_samples.push_back({std::string(src_name(src)) + " " + sl.name + " " + what, lx, ly});
    }
    // DEF-implied geometry per stack layer (layout dbu) for the XOR check; incomplete[li]
    // counts DEF shapes on li that could not be turned into exact rectangles.
    std::vector<std::vector<Rect>> dgeom, dpin;
    std::vector<int64_t> incomplete;
    void add_geom(int li, const Rect& r, bool pin) {
        if (dgeom.empty()) { dgeom.resize(stack.size()); dpin.resize(stack.size()); incomplete.assign(stack.size(), 0); }
        Rect q = {L(r.x1), L(r.y1), L(r.x2), L(r.y2)};
        if (q.x2 <= q.x1 || q.y2 <= q.y1) return;
        (pin ? dpin : dgeom)[li].push_back(q);
    }
    void mark_incomplete(int li) override {
        if (dgeom.empty()) { dgeom.resize(stack.size()); dpin.resize(stack.size()); incomplete.assign(stack.size(), 0); }
        if (li >= 0) ++incomplete[li];
    }
    // centre + four corners inset by 1 layout dbu (DEF coordinates in, rectangle may be degenerate);
    // the rectangle is also DEF geometry for the XOR check (pins go to drawing and pin datatype)
    void probe_rect(int src, int li, const Rect& r, const char* what) override {
        add_geom(li, r, false);
        if (src == PIN) add_geom(li, r, true);
        probe_point(src, li, (r.x1 + r.x2) / 2, (r.y1 + r.y2) / 2, what);
        if (r.x2 - r.x1 < 2 || r.y2 - r.y1 < 2) return;
        // inset in DEF units: at least one layout dbu
        int64_t in = std::max<int64_t>(1, (int64_t)std::ceil(1.0 / scale));
        probe_point(src, li, r.x1 + in, r.y1 + in, what);
        probe_point(src, li, r.x2 - in, r.y1 + in, what);
        probe_point(src, li, r.x1 + in, r.y2 - in, what);
        probe_point(src, li, r.x2 - in, r.y2 - in, what);
    }
};

// ------------------------------------------------------------------ layout units
double gds_units_per_um(const char* path) {
    FILE* fp = fopen(path, "rb");
    if (!fp) return 0;
    unsigned char h[4];
    double r = 0;
    while (fread(h, 1, 4, fp) == 4) {
        int len = (h[0] << 8) | h[1];
        if (len < 4) break;
        std::vector<unsigned char> d((size_t)(len - 4));
        if (!d.empty() && fread(d.data(), 1, d.size(), fp) != d.size()) break;
        if (h[2] == 0x03 && d.size() >= 16) {   // UNITS: 2 x 8-byte GDS reals
            auto real8 = [](const unsigned char* p) {
                int sign = p[0] & 0x80 ? -1 : 1;
                int exp = (p[0] & 0x7f) - 64;
                double m = 0;
                for (int i = 1; i < 8; ++i) m = m * 256 + p[i];
                return sign * m / std::pow(2.0, 56) * std::pow(16.0, exp);
            };
            double meters = real8(d.data() + 8);
            if (meters > 0) r = 1e-6 / meters;
            break;
        }
    }
    fclose(fp);
    return r;
}

// ------------------------------------------------------------------ XOR by union areas
// area(A xor B) = 2 area(A u B) - area(A) - area(B); union area by a y-compressed sweep with a
// segment tree. The die is cut into tiles so that differences can be located; for the worst
// tiles the window is refined by quadrants down to ~1 um to give sample locations.
int64_t union_area(const std::vector<const Rect*>& rs) {
    if (rs.empty()) return 0;
    std::vector<int64_t> ys;
    ys.reserve(rs.size() * 2);
    for (auto* r : rs) { ys.push_back(r->y1); ys.push_back(r->y2); }
    std::sort(ys.begin(), ys.end());
    ys.erase(std::unique(ys.begin(), ys.end()), ys.end());
    size_t m = ys.size() - 1;
    if (m == 0) return 0;
    struct Ev { int64_t x; int32_t a, b, d; };
    std::vector<Ev> ev;
    ev.reserve(rs.size() * 2);
    for (auto* r : rs) {
        if (r->x2 <= r->x1 || r->y2 <= r->y1) continue;
        int32_t a = (int32_t)(std::lower_bound(ys.begin(), ys.end(), r->y1) - ys.begin());
        int32_t b = (int32_t)(std::lower_bound(ys.begin(), ys.end(), r->y2) - ys.begin());
        ev.push_back({r->x1, a, b, +1});
        ev.push_back({r->x2, a, b, -1});
    }
    std::sort(ev.begin(), ev.end(), [](const Ev& p, const Ev& q) { return p.x < q.x; });
    std::vector<int32_t> cnt(4 * m, 0);
    std::vector<int64_t> len(4 * m, 0);
    std::function<void(size_t, size_t, size_t, int32_t, int32_t, int32_t)> upd =
        [&](size_t n, size_t lo, size_t hi, int32_t a, int32_t b, int32_t d) {
            if ((size_t)b <= lo || hi <= (size_t)a) return;
            if ((size_t)a <= lo && hi <= (size_t)b) cnt[n] += d;
            else { size_t mid = (lo + hi) / 2; upd(2 * n, lo, mid, a, b, d); upd(2 * n + 1, mid, hi, a, b, d); }
            if (cnt[n] > 0) len[n] = ys[hi] - ys[lo];
            else if (hi - lo == 1) len[n] = 0;
            else len[n] = len[2 * n] + len[2 * n + 1];
        };
    int64_t area = 0;
    for (size_t i = 0; i < ev.size();) {
        int64_t x = ev[i].x;
        while (i < ev.size() && ev[i].x == x) { upd(1, 0, m, ev[i].a, ev[i].b, ev[i].d); ++i; }
        if (i < ev.size()) area += len[1] * (ev[i].x - x);
    }
    return area;
}

struct XorResult { int64_t area_a = 0, area_b = 0, xor_area = 0, diff_tiles = 0; };

// clip to window w; returns clipped copies
void clip_to(const std::vector<const Rect*>& in, const Rect& w, std::vector<Rect>& store) {
    store.clear();
    for (auto* r : in) {
        Rect q = {std::max(r->x1, w.x1), std::max(r->y1, w.y1), std::min(r->x2, w.x2), std::min(r->y2, w.y2)};
        if (q.x2 > q.x1 && q.y2 > q.y1) store.push_back(q);
    }
}
int64_t xor_in(const std::vector<const Rect*>& A, const std::vector<const Rect*>& B, const Rect& w,
               int64_t* aa = nullptr, int64_t* ab = nullptr) {
    std::vector<Rect> ca, cb;
    clip_to(A, w, ca); clip_to(B, w, cb);
    std::vector<const Rect*> pa, pb, pab;
    for (auto& r : ca) { pa.push_back(&r); pab.push_back(&r); }
    for (auto& r : cb) { pb.push_back(&r); pab.push_back(&r); }
    int64_t ua = union_area(pa), ub = union_area(pb), uab = union_area(pab);
    if (aa) *aa = ua;
    if (ab) *ab = ub;
    return 2 * uab - ua - ub;
}
void refine(const std::vector<const Rect*>& A, const std::vector<const Rect*>& B, const Rect& w, int64_t min_w,
            int limit, std::vector<Rect>& out) {
    if ((int)out.size() >= limit) return;
    if (w.x2 - w.x1 <= min_w && w.y2 - w.y1 <= min_w) { out.push_back(w); return; }
    int64_t mx = (w.x1 + w.x2) / 2, my = (w.y1 + w.y2) / 2;
    Rect q[4] = {{w.x1, w.y1, mx, my}, {mx, w.y1, w.x2, my}, {w.x1, my, mx, w.y2}, {mx, my, w.x2, w.y2}};
    for (auto& c : q) {
        if ((int)out.size() >= limit) return;
        if (c.x2 <= c.x1 || c.y2 <= c.y1) continue;
        std::vector<const Rect*> sa, sb;
        for (auto* r : A) if (r->x2 > c.x1 && r->x1 < c.x2 && r->y2 > c.y1 && r->y1 < c.y2) sa.push_back(r);
        for (auto* r : B) if (r->x2 > c.x1 && r->x1 < c.x2 && r->y2 > c.y1 && r->y1 < c.y2) sb.push_back(r);
        if (xor_in(sa, sb, c) > 0) refine(sa, sb, c, min_w, limit, out);
    }
}

XorResult xor_layer(const std::vector<Rect>& A, const std::vector<Rect>& B, int64_t tile, int samples,
                    std::vector<std::string>& smp, const std::string& label) {
    XorResult res;
    int64_t xa = INT64_MAX, ya = INT64_MAX, xb = INT64_MIN, yb = INT64_MIN;
    for (auto* v : {&A, &B}) for (auto& r : *v) { xa = std::min(xa, r.x1); ya = std::min(ya, r.y1); xb = std::max(xb, r.x2); yb = std::max(yb, r.y2); }
    if (xa >= xb) return res;
    int64_t nx = (xb - xa) / tile + 1, ny = (yb - ya) / tile + 1;
    std::vector<std::vector<const Rect*>> ta((size_t)(nx * ny)), tb((size_t)(nx * ny));
    auto bin = [&](const std::vector<Rect>& v, std::vector<std::vector<const Rect*>>& t) {
        for (auto& r : v)
            for (int64_t gy = (r.y1 - ya) / tile; gy <= std::min(ny - 1, (r.y2 - ya) / tile); ++gy)
                for (int64_t gx = (r.x1 - xa) / tile; gx <= std::min(nx - 1, (r.x2 - xa) / tile); ++gx) t[(size_t)(gy * nx + gx)].push_back(&r);
    };
    bin(A, ta); bin(B, tb);
    std::vector<std::pair<int64_t, size_t>> worst;
    for (size_t t = 0; t < ta.size(); ++t) {
        if (ta[t].empty() && tb[t].empty()) continue;
        int64_t gx = (int64_t)t % nx, gy = (int64_t)t / nx;
        Rect w = {xa + gx * tile, ya + gy * tile, xa + (gx + 1) * tile, ya + (gy + 1) * tile};
        int64_t aa = 0, ab = 0;
        int64_t x = xor_in(ta[t], tb[t], w, &aa, &ab);
        res.area_a += aa; res.area_b += ab; res.xor_area += x;
        if (x > 0) { ++res.diff_tiles; worst.push_back({x, t}); }
    }
    std::sort(worst.rbegin(), worst.rend());
    int shown = 0;
    for (auto& wt : worst) {
        if ((int)smp.size() >= samples || shown >= 3) break;
        ++shown;
        size_t t = wt.second;
        int64_t gx = (int64_t)t % nx, gy = (int64_t)t / nx;
        Rect w = {xa + gx * tile, ya + gy * tile, xa + (gx + 1) * tile, ya + (gy + 1) * tile};
        std::vector<Rect> spots;
        refine(ta[t], tb[t], w, 1000, 3, spots);
        for (auto& sp : spots) {
            std::vector<const Rect*> sa, sb;
            for (auto* r : ta[t]) if (r->x2 > sp.x1 && r->x1 < sp.x2 && r->y2 > sp.y1 && r->y1 < sp.y2) sa.push_back(r);
            for (auto* r : tb[t]) if (r->x2 > sp.x1 && r->x1 < sp.x2 && r->y2 > sp.y1 && r->y1 < sp.y2) sb.push_back(r);
            int64_t aa = 0, ab = 0;
            int64_t x = xor_in(sa, sb, sp, &aa, &ab);
            char b[320];
            snprintf(b, sizeof b, "  xor %s window (%" PRId64 " %" PRId64 ") (%" PRId64 " %" PRId64 "): DEF area %" PRId64 " layout area %" PRId64 " XOR %" PRId64,
                     label.c_str(), sp.x1, sp.y1, sp.x2, sp.y2, aa, ab, x);
            smp.push_back(b);
        }
    }
    return res;
}

void usage(const char* p) {
    fprintf(stderr,
        "usage: %s [options] design.def[.gz] layout.{oas,gds}\n"
        "  --lef FILE        tech or cell LEF (repeatable; .gz ok): wire widths, via definitions,\n"
        "                    macro SIZE and the LEF PIN/OBS vs cell geometry check\n"
        "  --stack SPEC      layer stack, e.g. \"li1:67/20:67/16 mcon:67/44 met1:68/20:68/16 ...\"\n"
        "                    (routing/cut alternating bottom up; default SKY130)\n"
        "  --top CELL        top cell (default: the DEF DESIGN name, else the only top cell)\n"
        "  --boundary L/D    cell boundary layer for placements (default 235/4)\n"
        "  --samples N       failure samples printed per check (default 10)\n"
        "  --xor-tile UM     XOR tile size in microns (default 50)\n", p);
}

}  // namespace

int main(int argc, char** argv) {
    std::vector<std::string> lefs, pos;
    std::string stack_spec = kSky130Stack, top_name;
    int bl = 235, bd = 4, samples = 10;
    double xor_tile_um = 50;
    for (int i = 1; i < argc; ++i) {
        std::string a = argv[i];
        auto val = [&]() -> std::string { if (i + 1 >= argc) { usage(argv[0]); exit(2); } return argv[++i]; };
        if (a == "--lef") lefs.push_back(val());
        else if (a == "--stack") stack_spec = val();
        else if (a == "--top") top_name = val();
        else if (a == "--boundary") { if (!parse_ld(val(), bl, bd)) { usage(argv[0]); return 2; } }
        else if (a == "--samples") samples = atoi(val().c_str());
        else if (a == "--xor-tile") xor_tile_um = atof(val().c_str());
        else if (a == "-h" || a == "--help") { usage(argv[0]); return 0; }
        else if (!a.empty() && a[0] == '-') { usage(argv[0]); return 2; }
        else pos.push_back(a);
    }
    if (pos.size() != 2) { usage(argv[0]); return 2; }
    const std::string def_path = pos[0], lay_path = pos[1];

    Checker ck;
    ck.max_samples = samples;
    if (!parse_stack(stack_spec, ck.stack)) { fprintf(stderr, "bad --stack\n"); return 2; }
    for (size_t i = 0; i < ck.stack.size(); ++i) ck.lidx[ck.stack[i].name] = (int)i;

    std::string err;
    for (auto& l : lefs) if (!parse_lef(l, ck.lef, err)) { fprintf(stderr, "ERROR %s\n", err.c_str()); return 2; }
    if (!lefs.empty())
        printf("LEF files %zu: macros %zu vias %zu layer widths %zu\n", lefs.size(), ck.lef.macros.size(), ck.lef.vias.size(), ck.lef.layer_width.size());

    // ---- layout
    char ebuf[1024] = {0};
    ldb_file_t* fh = ldb_open(lay_path.c_str(), ebuf, sizeof(ebuf));
    if (!fh) { fprintf(stderr, "ERROR cannot open layout %s: %s\n", lay_path.c_str(), ebuf); return 2; }
    LdbFile* lf = (LdbFile*)fh;
    double lay_upm = 0;
    if (ldb_format(fh) == 2) {
        ldb_oas_info_t* oi = ldb_oas_info_open(lay_path.c_str(), ebuf, sizeof(ebuf));
        if (oi) { lay_upm = ldb_oas_info_unit(oi); ldb_oas_info_close(oi); }
    } else {
        lay_upm = gds_units_per_um(lay_path.c_str());
    }

    // ---- DEF header pass needs the stack only; the shape index must exist before routing is
    //      streamed, and needs the top cell name, which comes from DESIGN. Read the top cell
    //      from the layout first (DESIGN name preferred when it is a cell).
    // We do a cheap pre-scan of the DEF for DESIGN/UNITS (first few hundred tokens).
    std::string design_name;
    int64_t def_units = 0;
    {
        Tokenizer tk;
        if (!tk.open(def_path)) { fprintf(stderr, "ERROR cannot open DEF %s\n", def_path.c_str()); return 2; }
        std::string t;
        for (int n = 0; n < 4000 && tk.next(t); ++n) {
            if (t == "DESIGN") tk.next(design_name);
            else if (t == "UNITS") { std::string a, b, v; tk.next(a); tk.next(b); tk.next(v); def_units = atoll(v.c_str()); }
            else if (t == "COMPONENTS" || t == "NETS" || t == "ROW") break;
        }
    }
    if (def_units <= 0) { fprintf(stderr, "ERROR DEF has no UNITS DISTANCE MICRONS\n"); return 2; }
    if (lay_upm <= 0) { lay_upm = (double)def_units; printf("WARNING layout units unknown; assuming %g/um\n", lay_upm); }
    ck.scale = lay_upm / (double)def_units;

    if (top_name.empty()) {
        if (!design_name.empty() && lf->cell_index.count(design_name)) top_name = design_name;
        else if (ldb_top_cell_count(fh) == 1) top_name = ldb_top_cell_name(fh, 0);
    }
    if (top_name.empty() || !lf->cell_index.count(top_name)) {
        fprintf(stderr, "ERROR top cell not found (DESIGN '%s'); use --top\n", design_name.c_str());
        return 2;
    }
    printf("DEF %s (DESIGN %s, %" PRId64 " units/um)\nLAYOUT %s (top %s, %g units/um, scale %g)\n",
           def_path.c_str(), design_name.c_str(), def_units, lay_path.c_str(), top_name.c_str(), lay_upm, ck.scale);

    // cell-local geometry for every cell (needed for top shapes, boundaries, LEF check)
    std::unordered_map<std::string, CellGeom> cells;
    struct Ctx { std::unordered_map<std::string, CellGeom>* cells; };
    Ctx ctx{&cells};
    ldb_iterate_boxes(fh, [](void* ud, const char* cell, int l, int d, int kind, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
        if (kind > 2) return;
        auto* c = (Ctx*)ud;
        (*c->cells)[cell].layers[{l, d}].push_back({std::min(x1, x2), std::min(y1, y2), std::max(x1, x2), std::max(y1, y2)});
    }, &ctx);

    {
        CellGeom& tg = cells[top_name];
        for (auto& sl : ck.stack) {
            for (int k = 0; k < 2; ++k) {
                int l = k ? sl.pl : sl.dl, d = k ? sl.pd : sl.dd;
                if (l < 0) continue;
                ShapeIndex& si = ck.top[{l, d}];
                auto it = tg.layers.find({l, d});
                if (it != tg.layers.end()) si.s = it->second;
                si.build();
            }
        }
    }

    // ---- stream the DEF
    if (!parse_def(def_path, ck, err)) { fprintf(stderr, "ERROR %s\n", err.c_str()); return 2; }

    int fails = 0;
    const int64_t xor_tile = std::max<int64_t>(1000, (int64_t)std::llround(xor_tile_um * lay_upm));
    auto LL = [&](int64_t v) { return ck.L(v); };

    // ---- DIE
    {
        auto& lc = lf->cells[lf->cell_index[top_name]];
        ldb_extent_t b{0, 0, 0, 0, 0};
        auto ib = lc.layer_ext.find({bl, bd});
        const char* src = "boundary layer";
        if (ib != lc.layer_ext.end() && ib->second.valid) b = ib->second;
        else { ldb_extent_whole_hier(fh, top_name.c_str(), &b); src = "extent"; }
        bool ok = ck.has_die && b.valid && LL(ck.die.x1) == b.xmin && LL(ck.die.y1) == b.ymin && LL(ck.die.x2) == b.xmax && LL(ck.die.y2) == b.ymax;
        printf("\nCHECK DIE        %s  DIEAREA (%" PRId64 " %" PRId64 ") (%" PRId64 " %" PRId64 ")  top %s (%" PRId64 " %" PRId64 ") (%" PRId64 " %" PRId64 ")\n",
               ok ? "PASS" : "FAIL", ck.die.x1, ck.die.y1, ck.die.x2, ck.die.y2, src, (int64_t)b.xmin, (int64_t)b.ymin, (int64_t)b.xmax, (int64_t)b.ymax);
        if (!ok) ++fails;
    }

    // ---- COMPONENTS vs top-cell instances
    {
        auto refbox = [&](const std::string& cell, Rect& r) -> int {
            auto ci = lf->cell_index.find(cell);
            if (ci != lf->cell_index.end()) {
                auto& c = lf->cells[ci->second];
                auto ib = c.layer_ext.find({bl, bd});
                if (ib != c.layer_ext.end() && ib->second.valid) { r = {ib->second.xmin, ib->second.ymin, ib->second.xmax, ib->second.ymax}; return 1; }
            }
            auto im = ck.lef.macros.find(cell);
            if (im != ck.lef.macros.end() && im->second.has_size) {
                r = {(int64_t)std::llround(-im->second.ox * lay_upm), (int64_t)std::llround(-im->second.oy * lay_upm),
                     (int64_t)std::llround((im->second.w - im->second.ox) * lay_upm), (int64_t)std::llround((im->second.h - im->second.oy) * lay_upm)};
                return 2;
            }
            if (ci != lf->cell_index.end() && lf->cells[ci->second].cell_ext.valid) {
                auto& e = lf->cells[ci->second].cell_ext; r = {e.xmin, e.ymin, e.xmax, e.ymax}; return 3;
            }
            return 0;
        };
        typedef std::tuple<std::string, int64_t, int64_t, int> Key;
        std::map<Key, int64_t> lay, def;
        std::map<std::string, Rect> rb_cache;
        int64_t n_inst = 0, n_nonmanhattan = 0, n_noref = 0;
        int64_t by_src[4] = {0, 0, 0, 0};
        auto& tc = lf->cells[lf->cell_index[top_name]];
        for (auto& in : tc.instances) {
            const LdbTransform& tr = in.tr;
            int a = (int)std::llround(tr.angle_deg / 90.0) & 3;
            if (std::fabs(tr.angle_deg - 90.0 * std::llround(tr.angle_deg / 90.0)) > 1e-6 || std::fabs(tr.mag - 1.0) > 1e-9) { ++n_nonmanhattan; continue; }
            int code = (tr.mirror_x ? 4 : 0) + a;
            Rect r;
            int s = refbox(in.ref_name, r);
            if (!s) { ++n_noref; continue; }
            ++by_src[s];
            for (int64_t row = 0; row < std::max<int64_t>(1, tr.rows); ++row)
                for (int64_t col = 0; col < std::max<int64_t>(1, tr.cols); ++col) {
                    int64_t ox = tr.ox + col * tr.dx + row * tr.row_dx, oy = tr.oy + col * tr.dy + row * tr.row_dy;
                    Rect p = orient_rect(code, r, ox, oy);
                    ++lay[Key(in.ref_name, p.x1, p.y1, code)];
                    ++n_inst;
                }
        }
        for (auto& c : ck.comps) ++def[Key(c.macro, LL(c.x), LL(c.y), c.orient)];
        int64_t only_def = 0, only_lay = 0;
        std::vector<std::string> smp;
        for (auto& kv : def) {
            auto it = lay.find(kv.first);
            int64_t have = it == lay.end() ? 0 : it->second;
            if (kv.second > have) {
                only_def += kv.second - have;
                if ((int)smp.size() < samples) { char b[512]; snprintf(b, sizeof b, "  DEF-only   %s at (%" PRId64 " %" PRId64 ") %s", std::get<0>(kv.first).c_str(), std::get<1>(kv.first), std::get<2>(kv.first), orient_name(std::get<3>(kv.first))); smp.push_back(b); }
            }
        }
        for (auto& kv : lay) {
            auto it = def.find(kv.first);
            int64_t have = it == def.end() ? 0 : it->second;
            if (kv.second > have) {
                only_lay += kv.second - have;
                if ((int)smp.size() < 2 * samples) { char b[512]; snprintf(b, sizeof b, "  layout-only %s at (%" PRId64 " %" PRId64 ") %s", std::get<0>(kv.first).c_str(), std::get<1>(kv.first), std::get<2>(kv.first), orient_name(std::get<3>(kv.first))); smp.push_back(b); }
            }
        }
        bool ok = only_def == 0 && only_lay == 0 && n_nonmanhattan == 0 && n_noref == 0;
        // a cell extent is not the LEF box (overhangs), so lower-left corners cannot match:
        // without a boundary layer or LEF SIZE the check is skipped, not failed
        const bool skip = by_src[3] > 0;
        printf("CHECK COMPONENTS %s  DEF placed %zu (unplaced %" PRId64 ")  layout instances %" PRId64 "  DEF-only %" PRId64 "  layout-only %" PRId64
               "  (ref box: boundary %" PRId64 " LEF %" PRId64 " extent %" PRId64 "; no ref %" PRId64 ", non-manhattan %" PRId64 ")\n",
               skip ? "SKIP" : (ok ? "PASS" : "FAIL"), ck.comps.size(), ck.n_comp_unplaced, n_inst, only_def, only_lay, by_src[1], by_src[2], by_src[3], n_noref, n_nonmanhattan);
        if (skip) printf("  SKIP: %" PRId64 " instances have no boundary layer %d/%d and no LEF SIZE (give the cell LEFs with --lef)\n", by_src[3], bl, bd);
        else for (auto& s : smp) printf("%s\n", s.c_str());
        if (!ok && !skip) ++fails;
    }

    // ---- ROUTING probes
    {
        printf("CHECK ROUTING    %s  wires %" PRId64 "  special wires %" PRId64 "  vias %" PRId64 " (stack-rule %" PRId64 ")  rects %" PRId64 "  pin shapes %" PRId64 "  polygons %" PRId64 "  missed probes %" PRId64 "\n",
               ck.n_miss == 0 ? "PASS" : "FAIL", ck.n_wires, ck.n_swires, ck.n_vias, ck.n_via_fallback, ck.n_rects, ck.n_pins, ck.n_spolys, ck.n_miss);
        printf("  %-13s %-6s %12s %12s %10s\n", "source", "layer", "probes", "hit", "pin-dt");
        for (auto& kv : ck.stat)
            printf("  %-13s %-6s %12" PRId64 " %12" PRId64 " %10" PRId64 "\n", ck.src_name(kv.first.first), ck.stack[kv.first.second].name.c_str(), kv.second.n, kv.second.hit, kv.second.pin_only);
        printf("  via usage:");
        for (auto& kv : ck.via_count) printf(" %s=%" PRId64, kv.first.c_str(), kv.second);
        printf("\n");
        for (auto& kv : ck.unknown_layers) printf("  WARNING DEF layer '%s' not in the stack: %" PRId64 " shapes not checked\n", kv.first.c_str(), kv.second);
        for (auto& kv : ck.unknown_vias) printf("  NOTE via '%s' has no DEF/LEF definition, stack rule used (%" PRId64 ")\n", kv.first.c_str(), kv.second);
        for (auto& s : ck.miss_samples) printf("  miss %s at (%" PRId64 " %" PRId64 ")\n", s.what.c_str(), s.x, s.y);
        if (ck.n_miss) ++fails;
    }

    // ---- XOR: DEF-implied geometry vs top-cell geometry, per layer
    //      (needs every DEF shape on the layer as an exact rectangle: tech LEF widths and vias)
    {
        if (ck.dgeom.empty()) { ck.dgeom.resize(ck.stack.size()); ck.dpin.resize(ck.stack.size()); ck.incomplete.assign(ck.stack.size(), 0); }
        std::vector<std::string> lines, smp;
        int64_t xor_total = 0, n_checked = 0, n_skipped = 0;
        for (size_t li = 0; li < ck.stack.size(); ++li) {
            const StackLayer& sl = ck.stack[li];
            for (int k = 0; k < 2; ++k) {
                int l = k ? sl.pl : sl.dl, d = k ? sl.pd : sl.dd;
                if (l < 0) continue;
                const std::vector<Rect>& A = k ? ck.dpin[li] : ck.dgeom[li];
                const std::vector<Rect>& B = ck.top[{l, d}].s;
                if (A.empty() && B.empty()) continue;
                char b[320];
                if (ck.incomplete[li] && !k) {   // DEF PIN shapes are always exact rectangles
                    ++n_skipped;
                    snprintf(b, sizeof b, "  %-6s %3d/%-3d %-7s SKIP  %" PRId64 " DEF shapes have no exact geometry (no LEF width / via definition)",
                             sl.name.c_str(), l, d, k ? "pin" : (sl.cut ? "cut" : "drawing"), ck.incomplete[li]);
                    lines.push_back(b);
                    continue;
                }
                XorResult x = xor_layer(A, B, xor_tile, samples, smp, sl.name + " " + std::to_string(l) + "/" + std::to_string(d));
                ++n_checked;
                xor_total += x.xor_area;
                snprintf(b, sizeof b, "  %-6s %3d/%-3d %-7s DEF area %16" PRId64 "  layout area %16" PRId64 "  XOR %12" PRId64 "  (DEF rects %zu, layout rects %zu, diff tiles %" PRId64 ")",
                         sl.name.c_str(), l, d, k ? "pin" : (sl.cut ? "cut" : "drawing"), x.area_a, x.area_b, x.xor_area, A.size(), B.size(), x.diff_tiles);
                lines.push_back(b);
            }
        }
        const char* verdict = n_checked == 0 ? "SKIP" : (xor_total == 0 ? "PASS" : "FAIL");
        printf("CHECK XOR        %s  layers checked %" PRId64 "  skipped %" PRId64 "  total XOR area %" PRId64 " dbu^2%s\n", verdict, n_checked, n_skipped, xor_total,
               n_checked == 0 ? "  (give the tech LEF with --lef)" : "");
        for (auto& s : lines) printf("%s\n", s.c_str());
        for (auto& s : smp) printf("%s\n", s.c_str());
        if (xor_total) ++fails;
    }

    // ---- LEF macros vs cell geometry
    if (!ck.lef.macros.empty()) {
        struct LS { int64_t rects = 0, ok = 0, pin_only = 0, miss = 0; };
        std::map<std::string, LS> per_layer;
        std::vector<std::string> smp;
        int64_t n_macros = 0, n_missing_cells = 0, n_poly = 0;
        std::map<std::string, int64_t> used;
        for (auto& in : lf->cells[lf->cell_index[top_name]].instances) ++used[in.ref_name];
        for (auto& u : used) {
            auto im = ck.lef.macros.find(u.first);
            if (im == ck.lef.macros.end()) { ++n_missing_cells; continue; }
            ++n_macros;
            const LefMacro& m = im->second;
            n_poly += m.n_polygons;
            const CellGeom& g = cells[u.first];
            for (auto& r : m.rects) {
                auto li = ck.lidx.find(r.layer);
                if (li == ck.lidx.end()) continue;
                const StackLayer& sl = ck.stack[li->second];
                LS& s = per_layer[r.layer];
                ++s.rects;
                Rect q = {(int64_t)std::llround((r.x1 + m.ox) * lay_upm), (int64_t)std::llround((r.y1 + m.oy) * lay_upm),
                          (int64_t)std::llround((r.x2 + m.ox) * lay_upm), (int64_t)std::llround((r.y2 + m.oy) * lay_upm)};
                int64_t px[5] = {(q.x1 + q.x2) / 2, q.x1 + 1, q.x2 - 1, q.x1 + 1, q.x2 - 1};
                int64_t py[5] = {(q.y1 + q.y2) / 2, q.y1 + 1, q.y1 + 1, q.y2 - 1, q.y2 - 1};
                bool draw = true, any = true;
                for (int k = 0; k < 5; ++k) {
                    bool d = cell_probe(g, sl.dl, sl.dd, px[k], py[k]);
                    bool p = sl.pl >= 0 && cell_probe(g, sl.pl, sl.pd, px[k], py[k]);
                    draw = draw && d;
                    any = any && (d || p);
                }
                if (draw) ++s.ok;
                else if (any) ++s.pin_only;
                else {
                    ++s.miss;
                    if ((int)smp.size() < samples) { char b[512]; snprintf(b, sizeof b, "  LEF %s %s %s RECT %.3f %.3f %.3f %.3f not in %d/%d", u.first.c_str(), r.pin ? "PIN" : "OBS", r.layer.c_str(), r.x1, r.y1, r.x2, r.y2, sl.dl, sl.dd); smp.push_back(b); }
                }
            }
        }
        int64_t miss = 0, pin_only = 0;
        for (auto& kv : per_layer) { miss += kv.second.miss; pin_only += kv.second.pin_only; }
        printf("CHECK LEF        %s  macros used %" PRId64 " (not in LEF %" PRId64 ")  RECTs missing %" PRId64 "  pin-datatype only %" PRId64 "  (LEF polygons skipped %" PRId64 ")\n",
               (miss == 0 && n_missing_cells == 0) ? "PASS" : "FAIL", n_macros, n_missing_cells, miss, pin_only, n_poly);
        for (auto& kv : per_layer) {
            auto& sl = ck.stack[ck.lidx[kv.first]];
            printf("  %-6s %3d/%-3d LEF rects %8" PRId64 "  on drawing %8" PRId64 "  pin-dt only %6" PRId64 "  missing %6" PRId64 "\n", kv.first.c_str(), sl.dl, sl.dd,
                   kv.second.rects, kv.second.ok, kv.second.pin_only, kv.second.miss);
        }
        for (auto& s : smp) printf("%s\n", s.c_str());
        if (miss || n_missing_cells) ++fails;
    }

    printf("\nDEFCHECK %s (%d failing check%s)\n", fails ? "FAIL" : "PASS", fails, fails == 1 ? "" : "s");
    ldb_close(fh);
    return fails ? 1 : 0;
}
