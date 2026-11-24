// db_oasinfo: print what an OASIS file's tables say, without decoding geometry.
//   db_oasinfo [--verify-strict] file.oas
// Prints unit, top cell, strictness, open time, the LAYERNAME table and every cell with its
// S_CELL_OFFSET / S_BOUNDING_BOX. --verify-strict exits 1 unless the CELLNAME table is
// strict, every cell has both properties, and every S_CELL_OFFSET points at its CELL record.
#include "layoutdb.h"
#include <cstdio>
#include <cstring>

int main(int argc, char** argv) {
    bool verify = false; const char* path = nullptr;
    for (int i = 1; i < argc; ++i) {
        if (!strcmp(argv[i], "--verify-strict")) verify = true; else path = argv[i];
    }
    if (!path) { fprintf(stderr, "usage: db_oasinfo [--verify-strict] file.oas\n"); return 2; }
    char err[512] = {0};
    ldb_oas_info_t* I = ldb_oas_info_open(path, err, sizeof err);
    if (!I) { fprintf(stderr, "db_oasinfo: %s\n", err); return 2; }
    char top[256] = "";
    ldb_oas_info_top(I, top, sizeof top);
    printf("FILE %s\nUNIT %.6g steps/um\nTOP %s\nSTRICT %d\nOFFSETS_OK %d\nOPEN_MS %.3f\n", path,
           ldb_oas_info_unit(I), top[0] ? top : "(none)", ldb_oas_info_strict(I), ldb_oas_info_offsets_ok(I),
           ldb_oas_info_open_ms(I));
    for (int i = 0; i < ldb_oas_info_n_layernames(I); ++i) {
        int64_t a, b, c, d; int t; char nm[256];
        ldb_oas_info_layername(I, i, &a, &b, &c, &d, &t, nm, sizeof nm);
        printf("LAYERNAME %s %lld:%lld %lld:%lld '%s'\n", t ? "text" : "geom", (long long)a, (long long)b,
               (long long)c, (long long)d, nm);
    }
    int missing = 0;
    for (int i = 0; i < ldb_oas_info_n_cells(I); ++i) {
        char nm[512]; int64_t off, bb[4]; int fl;
        ldb_oas_info_cell(I, i, nm, sizeof nm, &off, &fl, bb);
        if (off < 0 || fl < 0) ++missing;
        if (i < 50) printf("CELL '%s' OFFSET %lld BBOX_FLAGS %d BBOX %lld %lld %lld %lld\n", nm, (long long)off, fl,
                           (long long)bb[0], (long long)bb[1], (long long)bb[2], (long long)bb[3]);
    }
    printf("CELLS %d (without S_CELL_OFFSET or S_BOUNDING_BOX: %d)\n", ldb_oas_info_n_cells(I), missing);
    int rc = 0;
    if (verify && !(ldb_oas_info_strict(I) && ldb_oas_info_offsets_ok(I) == 1 && missing == 0)) {
        printf("VERIFY_STRICT FAIL\n"); rc = 1;
    } else if (verify) printf("VERIFY_STRICT OK\n");
    ldb_oas_info_close(I);
    return rc;
}
