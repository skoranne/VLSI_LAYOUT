// db_mvi: build / inspect / verify the MAGVIEWER sidecar index of a flat OASIS file.
//   db_mvi build file.oas [out.mvi] [--grid N]   build (or rebuild) the sidecar
//   db_mvi info  file.oas [file.mvi]             header, layers, density summary
//   db_mvi check file.oas [file.mvi]             decode every unit per layer and compare count
//                                                and an order-free box hash with the streaming parser
//   db_mvi bench file.oas [frac] [n]             decode the units of n random windows (frac of extent)
#include "layoutdb.h"
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <random>
#include <string>
#include <vector>

static double now() { return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count(); }
static uint64_t mix(uint64_t x) { x ^= x >> 33; x *= 0xff51afd7ed558ccdULL; x ^= x >> 33; x *= 0xc4ceb9fe1a85ec53ULL; x ^= x >> 33; return x; }
static uint64_t box_hash(int64_t a, int64_t b, int64_t c, int64_t d) {
    return mix((uint64_t)(uint32_t)a ^ mix((uint64_t)(uint32_t)b ^ mix((uint64_t)(uint32_t)c ^ mix((uint64_t)(uint32_t)d))));
}

struct Ref { std::map<std::pair<int, int>, std::pair<int64_t, uint64_t>> per; };
static void ref_cb(void* ud, const char*, int l, int d, int, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
    auto& e = ((Ref*)ud)->per[{ l, d }];
    ++e.first; e.second += box_hash(x1, y1, x2, y2);
}

static ldb_mvi_t* open_side(const char* oas, const char* mvi) {
    char err[512] = { 0 }; int built = 0;
    const double t0 = now();
    ldb_mvi_t* m = ldb_mvi_open_or_build(oas, mvi, 0, &built, err, sizeof err);
    if (!m) { std::fprintf(stderr, "db_mvi: %s\n", err); std::exit(1); }
    std::printf("SIDECAR %s %s in %.3f s%s%s\n", ldb_mvi_path(m), built ? "built" : "opened", now() - t0,
                built ? "  " : "", built ? err : "");
    return m;
}

int main(int argc, char** argv) {
    if (argc < 3) { std::fprintf(stderr, "usage: db_mvi build|info|check|bench file.oas [...]\n"); return 2; }
    const std::string cmd = argv[1];
    const char* oas = argv[2];
    if (cmd == "build") {
        int grid = 0; const char* out = nullptr;
        for (int i = 3; i < argc; ++i) {
            if (!std::strcmp(argv[i], "--grid") && i + 1 < argc) grid = std::atoi(argv[++i]);
            else out = argv[i];
        }
        std::string op = out ? out : std::string(oas) + ".mvi";
        char err[512] = { 0 };
        const int rc = ldb_mvi_build(oas, op.c_str(), grid, err, sizeof err);
        std::printf("%s %s\n", rc == 0 ? "OK" : "FAIL", err);
        return rc == 0 ? 0 : 1;
    }
    ldb_mvi_t* m = open_side(oas, argc > 3 && cmd != "bench" ? argv[3] : nullptr);
    double unit, bs; int64_t bb[4], tile, nu, tot, ndep; int nx, ny, nlev, nl;
    ldb_mvi_info(m, &unit, bb, &tile, &nx, &ny, &nlev, &nl, &nu, &tot, &ndep, &bs);
    if (cmd == "info") {
        std::printf("UNIT %g  BBOX %lld %lld %lld %lld  GRID %dx%d tile %lld  LEVELS %d\n", unit, (long long)bb[0],
                    (long long)bb[1], (long long)bb[2], (long long)bb[3], nx, ny, (long long)tile, nlev);
        std::printf("UNITS %lld  DEPENDENT %lld  BOXES %lld  BUILD %.2f s\n", (long long)nu, (long long)ndep, (long long)tot, bs);
        for (int i = 0; i < nl; ++i) {
            int64_t l, d, c, lb[4]; ldb_mvi_layer(m, i, &l, &d, &c, lb);
            int ax, ay; const uint32_t* cnt; const float* cov;
            double occ = 0; int64_t nz = 0;
            if (ldb_mvi_density(m, i, 0, &ax, &ay, &cnt, &cov) == 0)
                for (int64_t k = 0; k < (int64_t)ax * ay; ++k) { if (cnt[k]) ++nz; occ += cov[k]; }
            std::printf("LAYER %d %lld/%lld boxes %lld units %lld occupied_tiles %lld mean_cov %.3f\n", i, (long long)l,
                        (long long)d, (long long)c, (long long)ldb_mvi_units_in(m, i, INT64_MIN, INT64_MIN, INT64_MAX, INT64_MAX, nullptr, 0),
                        (long long)nz, occ / std::max<int64_t>(1, (int64_t)ax * ay));
        }
    } else if (cmd == "check") {
        Ref ref; char err[512] = { 0 };
        double t0 = now();
        int rc = ldb_stream_boxes(oas, ref_cb, &ref, err, sizeof err);
        std::printf("STREAM rc=%d %.2f s\n", rc, now() - t0);
        int bad = 0;
        std::vector<int64_t> ids((size_t)nu);
        for (int i = 0; i < nl; ++i) {
            int64_t l, d, c; ldb_mvi_layer(m, i, &l, &d, &c, nullptr);
            const int64_t n = ldb_mvi_units_in(m, i, INT64_MIN, INT64_MIN, INT64_MAX, INT64_MAX, ids.data(), nu);
            // in batches of units, so memory stays at a few hundred MB
            std::vector<int32_t> xy;
            int64_t got = 0; uint64_t h = 0; double dt = 0;
            for (int64_t k0 = 0; k0 < n; k0 += 32) {
                const int64_t kn = std::min<int64_t>(32, n - k0);
                int64_t need = 0;
                for (int64_t k = 0; k < kn; ++k) need += ldb_mvi_unit_boxes(m, ids[(size_t)(k0 + k)], i, nullptr);
                if ((int64_t)xy.size() < 4 * need + 4) xy.resize((size_t)(4 * need + 4));
                t0 = now();
                const int64_t g = ldb_mvi_decode(m, ids.data() + k0, kn, i, xy.data(), need, nullptr, err, sizeof err);
                dt += now() - t0;
                if (g < 0) { std::printf("DECODE ERROR %s\n", err); got = -1; break; }
                for (int64_t k = 0; k < g; ++k) h += box_hash(xy[4 * k], xy[4 * k + 1], xy[4 * k + 2], xy[4 * k + 3]);
                got += g;
            }
            auto& r = ref.per[{ (int)l, (int)d }];
            const bool ok = got == r.first && h == r.second;
            bad += !ok;
            std::printf("LAYER %lld/%lld units %lld decoded %lld (%.2f s, %.0f Mbox/s) stream %lld hash %s %s\n", (long long)l,
                        (long long)d, (long long)n, (long long)got, dt, got / std::max(dt, 1e-9) / 1e6, (long long)r.first,
                        h == r.second ? "same" : "DIFF", ok ? "OK" : "MISMATCH");
        }
        if (ref.per.size() != (size_t)nl) { std::printf("LAYER COUNT stream %zu sidecar %d MISMATCH\n", ref.per.size(), nl); ++bad; }
        std::printf("CHECK %s\n", bad ? "FAIL" : "OK");
        ldb_mvi_close(m);
        return bad ? 1 : 0;
    } else if (cmd == "bench") {
        const double frac = argc > 3 ? std::atof(argv[3]) : 0.05;
        const int nwin = argc > 4 ? std::atoi(argv[4]) : 20;
        std::mt19937_64 rng(12345);
        const int64_t W = (int64_t)((bb[2] - bb[0]) * frac), H = (int64_t)((bb[3] - bb[1]) * frac);
        std::vector<int64_t> ids((size_t)nu);
        std::vector<int32_t> xy;
        double tsum = 0; int64_t bsum = 0, usum = 0;
        for (int w = 0; w < nwin; ++w) {
            const int64_t x = bb[0] + (int64_t)(rng() % (uint64_t)std::max<int64_t>(1, bb[2] - bb[0] - W));
            const int64_t y = bb[1] + (int64_t)(rng() % (uint64_t)std::max<int64_t>(1, bb[3] - bb[1] - H));
            const double t0 = now();
            int64_t wb = 0, wu = 0;
            for (int i = 0; i < nl; ++i) {
                const int64_t n = ldb_mvi_units_in(m, i, x, y, x + W, y + H, ids.data(), nu);
                int64_t need = 0;
                for (int64_t k = 0; k < n; ++k) need += ldb_mvi_unit_boxes(m, ids[(size_t)k], i, nullptr);
                if ((int64_t)xy.size() < 4 * need) xy.resize((size_t)(4 * need));
                char err[256];
                wb += ldb_mvi_decode(m, ids.data(), n, i, xy.data(), need, nullptr, err, sizeof err);
                wu += n;
            }
            const double dt = now() - t0;
            tsum += dt; bsum += wb; usum += wu;
            std::printf("WIN %2d units %4lld boxes %11lld  %.3f s\n", w, (long long)wu, (long long)wb, dt);
        }
        std::printf("BENCH windows %d mean %.3f s  mean units %.1f  mean boxes %.0f  %.0f Mbox/s\n", nwin, tsum / nwin,
                    (double)usum / nwin, (double)bsum / nwin, bsum / std::max(tsum, 1e-9) / 1e6);
    }
    ldb_mvi_close(m);
    return 0;
}
