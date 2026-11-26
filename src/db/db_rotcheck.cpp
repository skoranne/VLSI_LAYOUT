// db_rotcheck: does the reader's flatten apply placement rotations correctly?
//
//   db_rotcheck [--mode poly|box] [--angle A] BASE ROTATED
//
// BASE is a layout, ROTATED the same layout under one extra top-cell placement rotated by A
// degrees about the origin (regress/rotation/gen_rotated.py writes both). The flattened shapes
// of ROTATED are compared with the flattened shapes of BASE rotated here by A, with every
// vertex rounded once, half away from zero (what an exact flatten followed by one rounding
// gives, and what KLayout does).
//
//   poly  ldb_iterate_polygons_flat with LDB_OPEN_SHAPES: exact outlines. Expected to match
//         polygon for polygon at every angle.
//   box   ldb_iterate_boxes_hier, the iterator MAGPARSER loads designs with. Exact for
//         multiples of 90 degrees; other angles are approximated by rectangle strips, so only
//         the area ratio is reported there.
//
// Each polygon is normalised before hashing: repeated and collinear vertices dropped (exact
// 128-bit cross products), counter-clockwise, starting at the lowest-leftmost vertex. Per
// (layer, datatype) the multisets of normalised polygons are compared.
// Prints LAYER lines, then ROTCHECK PASS|FAIL ...; exit status 0 on PASS.

#include "layoutdb.h"
#include "layoutdb_hier.h"
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <string>
#include <vector>
#include <algorithm>
#include <chrono>

typedef __int128 i128;

struct LayerStats {
    std::vector<uint64_t> hashes;
    int64_t vertices = 0, degenerate = 0;
    i128 area2 = 0;           // sum of twice the polygon areas (unmerged)
};
typedef std::map<std::pair<int, int>, LayerStats> Stats;

struct Ctx {
    Stats* st;
    double cs = 1, sn = 0;
    bool rotate = false;
    std::vector<int64_t> x, y;
};

static void cos_sin_deg(double deg, double& cs, double& sn) {
    const double q = deg / 90.0;
    if (q == std::floor(q)) {
        const int k = (int)(((long long)q % 4 + 4) % 4);
        const double c4[4] = { 1, 0, -1, 0 }, s4[4] = { 0, 1, 0, -1 };
        cs = c4[k]; sn = s4[k];
    } else { cs = std::cos(deg * M_PI / 180.0); sn = std::sin(deg * M_PI / 180.0); }
}

static inline uint64_t mix(uint64_t h, uint64_t v) {
    h ^= v + 0x9e3779b97f4a7c15ULL + (h << 6) + (h >> 2);
    h *= 0xff51afd7ed558ccdULL; h ^= h >> 33;
    return h;
}

static void add_polygon(Ctx& c, int layer, int dt, int n, const int64_t* xs, const int64_t* ys) {
    std::vector<int64_t>& X = c.x; std::vector<int64_t>& Y = c.y;
    X.clear(); Y.clear();
    for (int i = 0; i < n; ++i) {
        int64_t px = xs[i], py = ys[i];
        if (c.rotate) {
            const double rx = c.cs * (double)px - c.sn * (double)py, ry = c.sn * (double)px + c.cs * (double)py;
            px = (int64_t)std::llround(rx); py = (int64_t)std::llround(ry);
        }
        if (!X.empty() && X.back() == px && Y.back() == py) continue;
        X.push_back(px); Y.push_back(py);
    }
    while (X.size() > 1 && X.front() == X.back() && Y.front() == Y.back()) { X.pop_back(); Y.pop_back(); }
    // drop collinear vertices (repeat until stable: removing one can make a neighbour collinear)
    bool changed = true;
    while (changed && X.size() >= 3) {
        changed = false;
        std::vector<int64_t> nx, ny;
        const size_t m = X.size();
        for (size_t i = 0; i < m; ++i) {
            const size_t p = (i + m - 1) % m, q = (i + 1) % m;
            const i128 cr = (i128)(X[i] - X[p]) * (Y[q] - Y[i]) - (i128)(Y[i] - Y[p]) * (X[q] - X[i]);
            if (cr == 0) { changed = true; continue; }
            nx.push_back(X[i]); ny.push_back(Y[i]);
        }
        if (changed) { X.swap(nx); Y.swap(ny); }
    }
    LayerStats& ls = (*c.st)[{ layer, dt }];
    if (X.size() < 3) { ++ls.degenerate; return; }
    const size_t m = X.size();
    i128 a2 = 0;
    for (size_t i = 0; i < m; ++i) { const size_t q = (i + 1) % m; a2 += (i128)X[i] * Y[q] - (i128)X[q] * Y[i]; }
    if (a2 < 0) { std::reverse(X.begin(), X.end()); std::reverse(Y.begin(), Y.end()); a2 = -a2; }
    size_t s = 0;
    for (size_t i = 1; i < m; ++i) if (Y[i] < Y[s] || (Y[i] == Y[s] && X[i] < X[s])) s = i;
    uint64_t h = 0x1234567ULL + m;
    for (size_t k = 0; k < m; ++k) { const size_t i = (s + k) % m; h = mix(h, (uint64_t)X[i]); h = mix(h, (uint64_t)Y[i]); }
    ls.hashes.push_back(h);
    ls.vertices += (int64_t)m;
    ls.area2 += a2;
}

static void poly_cb(void* ud, const char*, int layer, int dt, int, int n, const int64_t* xs, const int64_t* ys) {
    add_polygon(*(Ctx*)ud, layer, dt, n, xs, ys);
}
static void box_cb(void* ud, const char*, int layer, int dt, int, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
    if (x1 == x2 || y1 == y2) return;
    const int64_t xs[4] = { x1, x2, x2, x1 }, ys[4] = { y1, y1, y2, y2 };
    add_polygon(*(Ctx*)ud, layer, dt, 4, xs, ys);
}

static std::string i128s(i128 v) {
    if (v == 0) return "0";
    bool neg = v < 0; if (neg) v = -v;
    std::string s;
    while (v > 0) { s.push_back(char('0' + (int)(v % 10))); v /= 10; }
    if (neg) s.push_back('-');
    std::reverse(s.begin(), s.end());
    return s;
}

static bool load(const char* path, bool poly, double angle, bool rotate, Stats& st) {
    char err[512] = { 0 };
    ldb_open_opts_t oo{ poly ? LDB_OPEN_SHAPES : LDB_OPEN_DEFAULT, 0 };
    auto t0 = std::chrono::steady_clock::now();
    ldb_file_t* f = ldb_open_opts(path, &oo, err, sizeof err);
    if (!f) { fprintf(stderr, "ERROR: open %s: %s\n", path, err); return false; }
    Ctx c; c.st = &st; c.rotate = rotate;
    cos_sin_deg(angle, c.cs, c.sn);
    int rc = poly ? ldb_iterate_polygons_flat(f, poly_cb, &c) : ldb_iterate_boxes_hier(f, "", -1, box_cb, &c);
    ldb_close(f);
    double s = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
    int64_t n = 0; for (auto& kv : st) n += (int64_t)kv.second.hashes.size();
    printf("LOAD %s polygons=%lld %.1fs\n", path, (long long)n, s);
    fflush(stdout);
    return rc == 0;
}

int main(int argc, char** argv) {
    std::string mode = "poly"; double angle = 0; int a = 1;
    for (; a < argc && argv[a][0] == '-' && argv[a][1] == '-'; ++a) {
        if (!strcmp(argv[a], "--mode") && a + 1 < argc) mode = argv[++a];
        else if (!strcmp(argv[a], "--angle") && a + 1 < argc) angle = atof(argv[++a]);
        else { fprintf(stderr, "unknown option %s\n", argv[a]); return 2; }
    }
    if (argc - a != 2 || (mode != "poly" && mode != "box")) {
        fprintf(stderr, "usage: db_rotcheck [--mode poly|box] [--angle A] BASE ROTATED\n");
        return 2;
    }
    const bool poly = mode == "poly";
    const double q = angle / 90.0;
    const bool ortho = q == std::floor(q);
    Stats sb, sr;
    if (!load(argv[a], poly, angle, true, sb) || !load(argv[a + 1], poly, angle, false, sr)) return 2;

    std::map<std::pair<int, int>, int> keys;
    for (auto& kv : sb) keys[kv.first] = 1;
    for (auto& kv : sr) keys[kv.first] = 1;
    int64_t tot_missing = 0, tot_extra = 0, tot = 0;
    i128 ab = 0, ar = 0;
    for (auto& k : keys) {
        LayerStats& b = sb[k.first]; LayerStats& r = sr[k.first];
        std::sort(b.hashes.begin(), b.hashes.end()); std::sort(r.hashes.begin(), r.hashes.end());
        int64_t missing = 0, extra = 0; size_t i = 0, j = 0;
        while (i < b.hashes.size() || j < r.hashes.size()) {
            if (j >= r.hashes.size() || (i < b.hashes.size() && b.hashes[i] < r.hashes[j])) { ++missing; ++i; }
            else if (i >= b.hashes.size() || r.hashes[j] < b.hashes[i]) { ++extra; ++j; }
            else { ++i; ++j; }
        }
        tot_missing += missing; tot_extra += extra; tot += (int64_t)b.hashes.size();
        ab += b.area2; ar += r.area2;
        printf("LAYER %d/%d expected=%zu got=%zu missing=%lld extra=%lld vertices=%lld/%lld area2=%s/%s degenerate=%lld/%lld\n",
               k.first.first, k.first.second, b.hashes.size(), r.hashes.size(), (long long)missing, (long long)extra,
               (long long)b.vertices, (long long)r.vertices, i128s(b.area2).c_str(), i128s(r.area2).c_str(),
               (long long)b.degenerate, (long long)r.degenerate);
    }
    const double ratio = ab != 0 ? (double)ar / (double)ab : 1.0;
    const bool exact_expected = poly || ortho;
    const bool pass = exact_expected ? (tot_missing == 0 && tot_extra == 0) : true;
    printf("ROTCHECK %s mode=%s angle=%g polygons=%lld missing=%lld extra=%lld area_ratio=%.9f%s\n",
           pass ? "PASS" : "FAIL", mode.c_str(), angle, (long long)tot, (long long)tot_missing, (long long)tot_extra,
           ratio, exact_expected ? "" : " (approximate: box iterator under a non-orthogonal placement)");
    return pass ? 0 : 1;
}
