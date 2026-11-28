// File   : anyangle_capi.cpp
// Author : Sandeep Koranne (C) 2026. All rights reserved.
// Purpose: C API for the any-angle engine (Fortran, trap_merge.f90):
//   * ldb_polys_*: exact flattened polygons of selected layers (paths as outlines, boxes as
//     4-point polygons), as CSR arrays (1-based offsets for Fortran);
//   * ldb_ow_*:    streaming OASIS output of rectangles, horizontal trapezoids (RECTANGLE /
//     CTRAPEZOID / TRAPEZOID records chosen per shape) and polygons (oasis_writer::Writer).
#include "layoutdb.h"
#include "oasis_writer.hpp"
#include <cstdio>
#include <cstring>
#include <map>
#include <string>
#include <vector>

namespace {
struct LayerPolys { std::vector<int64_t> off{1}; std::vector<int64_t> xs, ys; };
struct PolySet {
    std::map<std::pair<int, int>, int> sel;
    std::vector<LayerPolys> L;
    double unit = 0;
};
}

extern "C" void* ldb_polys_load(const char* path, int nsel, const int* layers, const int* dts,
                                char* err, size_t errsz) {
    ldb_open_opts_t o{};
    o.flags = LDB_OPEN_SHAPES;
    ldb_file_t* f = ldb_open_opts(path, &o, err, errsz);
    if (!f) return nullptr;
    auto* P = new PolySet();
    P->L.resize((size_t)nsel);
    for (int i = 0; i < nsel; ++i) P->sel[{layers[i], dts[i]}] = i;
    P->unit = ldb_dbu_per_micron(f);
    ldb_iterate_polygons_flat(f, [](void* ud, const char*, int layer, int dt, int, int n,
                                    const int64_t* xs, const int64_t* ys) {
        auto* P = (PolySet*)ud;
        auto it = P->sel.find({layer, dt});
        if (it == P->sel.end() || n < 3) return;
        LayerPolys& L = P->L[(size_t)it->second];
        L.xs.insert(L.xs.end(), xs, xs + n);
        L.ys.insert(L.ys.end(), ys, ys + n);
        L.off.push_back((int64_t)L.xs.size() + 1);
    }, P);
    ldb_close(f);
    return P;
}

extern "C" int64_t ldb_polys_count(void* h, int k, int64_t* nvert) {
    auto* P = (PolySet*)h;
    const LayerPolys& L = P->L[(size_t)k];
    *nvert = (int64_t)L.xs.size();
    return (int64_t)L.off.size() - 1;
}

extern "C" void ldb_polys_get(void* h, int k, int64_t* poff, int64_t* xs, int64_t* ys) {
    auto* P = (PolySet*)h;
    LayerPolys& L = P->L[(size_t)k];
    std::memcpy(poff, L.off.data(), L.off.size() * sizeof(int64_t));
    if (!L.xs.empty()) {
        std::memcpy(xs, L.xs.data(), L.xs.size() * sizeof(int64_t));
        std::memcpy(ys, L.ys.data(), L.ys.size() * sizeof(int64_t));
    }
    L = LayerPolys();                 // release
}

extern "C" double ldb_polys_unit(void* h) { return ((PolySet*)h)->unit; }
extern "C" void ldb_polys_free(void* h) { delete (PolySet*)h; }

extern "C" void* ldb_ow_open(const char* path, double unit) {
    auto* w = new oasis_writer::Writer();
    oasis_writer::Options o;
    if (unit > 0) o.unit = unit;
    if (!w->open(path, o)) { std::fprintf(stderr, "ldb_ow_open: %s\n", w->error().c_str()); delete w; return nullptr; }
    return w;
}
extern "C" void ldb_ow_begin_cell(void* h, const char* name) {
    auto* w = (oasis_writer::Writer*)h; w->add_top_cell(name); w->begin_cell(name);
}
extern "C" void ldb_ow_end_cell(void* h) { ((oasis_writer::Writer*)h)->end_cell(); }
extern "C" void ldb_ow_rect(void* h, int l, int d, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
    ((oasis_writer::Writer*)h)->rectangle((uint64_t)l, (uint64_t)d, x1, y1, x2 - x1, y2 - y1);
}
extern "C" void ldb_ow_trap(void* h, int l, int d, int64_t y1, int64_t y2, int64_t xbl, int64_t xbr,
                            int64_t xtl, int64_t xtr) {
    ((oasis_writer::Writer*)h)->trapezoid_h((uint64_t)l, (uint64_t)d, y1, y2, xbl, xbr, xtl, xtr);
}
extern "C" void ldb_ow_polygon(void* h, int l, int d, int n, const int64_t* xs, const int64_t* ys) {
    std::vector<ldbgeom::Pt> p((size_t)n);
    for (int i = 0; i < n; ++i) { p[(size_t)i].x = xs[i]; p[(size_t)i].y = ys[i]; }
    ((oasis_writer::Writer*)h)->polygon((uint64_t)l, (uint64_t)d, p.data(), p.size());
}
// stats: rectangles, trapezoids, ctrapezoids, polygons, degenerate, bytes. Returns 0 on success.
extern "C" int ldb_ow_close(void* h, int64_t* stats) {
    auto* w = (oasis_writer::Writer*)h;
    const bool ok = w->close();
    const auto& s = w->stats();
    if (stats) {
        stats[0] = (int64_t)s.rectangles; stats[1] = (int64_t)s.trapezoids; stats[2] = (int64_t)s.ctrapezoids;
        stats[3] = (int64_t)s.polygons; stats[4] = (int64_t)s.degenerate; stats[5] = (int64_t)s.bytes;
    }
    if (!ok) std::fprintf(stderr, "ldb_ow_close: %s\n", w->error().c_str());
    delete w;
    return ok ? 0 : 1;
}
