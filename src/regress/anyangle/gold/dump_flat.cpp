// Dump the flattened polygons (db reader, ldb_polys_load) of layers that touch a window.
// Usage: dump_flat IN.oas L/D[,L/D] x1 y1 x2 y2
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <vector>
#include <string>
extern "C" void* ldb_polys_load(const char*, int, const int*, const int*, char*, size_t);
extern "C" int64_t ldb_polys_count(void*, int, int64_t*);
extern "C" void ldb_polys_get(void*, int, int64_t*, int64_t*, int64_t*);
extern "C" void ldb_polys_free(void*);
int main(int argc, char** argv) {
    std::vector<int> L, D; std::string s = argv[2];
    for (size_t p = 0; p < s.size();) { size_t q = s.find(',', p); if (q == std::string::npos) q = s.size();
        std::string t = s.substr(p, q - p); L.push_back(atoi(t.c_str())); D.push_back(atoi(t.substr(t.find('/') + 1).c_str())); p = q + 1; }
    long x1 = atol(argv[3]), y1 = atol(argv[4]), x2 = atol(argv[5]), y2 = atol(argv[6]);
    char err[512] = {0};
    void* h = ldb_polys_load(argv[1], (int)L.size(), L.data(), D.data(), err, sizeof err);
    if (!h) { fprintf(stderr, "load failed: %s\n", err); return 1; }
    for (size_t k = 0; k < L.size(); ++k) {
        int64_t nv; int64_t np = ldb_polys_count(h, (int)k, &nv);
        std::vector<int64_t> off(np + 1), xs(nv ? nv : 1), ys(nv ? nv : 1);
        ldb_polys_get(h, (int)k, off.data(), xs.data(), ys.data());
        for (int64_t i = 0; i < np; ++i) {
            long bx1 = 1L << 60, by1 = bx1, bx2 = -bx1, by2 = -bx1;
            for (int64_t j = off[i] - 1; j < off[i + 1] - 1; ++j) { bx1 = std::min<long>(bx1, xs[j]); bx2 = std::max<long>(bx2, xs[j]); by1 = std::min<long>(by1, ys[j]); by2 = std::max<long>(by2, ys[j]); }
            if (bx2 < x1 || bx1 > x2 || by2 < y1 || by1 > y2) continue;
            printf("%d/%d", L[k], D[k]);
            for (int64_t j = off[i] - 1; j < off[i + 1] - 1; ++j) printf(" %ld,%ld", (long)xs[j], (long)ys[j]);
            printf("\n");
        }
    }
    ldb_polys_free(h);
}
