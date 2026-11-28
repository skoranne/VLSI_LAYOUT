// db_oas_stats: print basic layer, bbox, text/property statistics for a GDSII/OASIS file.
// Output is a stable, line-oriented text format meant to be converted to a CTR by a script.
//
// Usage: db_oas_stats [--flat] file.oas
//   --flat  flatten hierarchy (SREF/AREF) when counting boxes/extents (default: on)
//
// Output lines:
//   FILE <path>
//   FORMAT <GDSII|OASIS|UNKNOWN>
//   UNITS_PER_MICRON <f>
//   TOP <name>
//   EXTENT <xmin> <ymin> <xmax> <ymax>
//   LAYER <layer> <datatype> <name|-> <count> <xmin> <ymin> <xmax> <ymax>
//   TEXT <layer> <texttype> <x> <y> <string...>
//   TEXT_COUNT <n>
//   PROPERTY <kind> <cell> <name> <value>
//   END
#include "layoutdb.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cinttypes>
#include <map>
#include <string>
#include <vector>
#include <algorithm>

struct Key {
    int layer;
    int datatype;
    bool operator<(const Key& o) const {
        if (layer != o.layer) return layer < o.layer;
        return datatype < o.datatype;
    }
};

struct Accum {
    uint64_t count = 0;
    int64_t xmin = 0, ymin = 0, xmax = 0, ymax = 0;
    bool valid = false;
    void add(int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
        if (!valid) { xmin=x1; ymin=y1; xmax=x2; ymax=y2; valid=true; }
        else {
            if (x1 < xmin) xmin = x1;
            if (y1 < ymin) ymin = y1;
            if (x2 > xmax) xmax = x2;
            if (y2 > ymax) ymax = y2;
        }
        ++count;
    }
};

struct Ctx {
    std::map<Key, Accum> layers;
};

static void on_box(void* ud, const char* /*cell*/, int layer, int datatype, int /*kind*/,
                   int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
    auto* ctx = static_cast<Ctx*>(ud);
    ctx->layers[{layer, datatype}].add(x1, y1, x2, y2);
}

struct TextCtx { FILE* fp; uint64_t count; };
static void on_text(void* ud, const char* /*cell*/, int layer, int texttype,
                    int64_t x, int64_t y, const char* str) {
    auto* ctx = static_cast<TextCtx*>(ud);
    fprintf(ctx->fp, "TEXT %d %d %" PRId64 " %" PRId64 " %s\n", layer, texttype, x, y, str ? str : "");
    ++ctx->count;
}

static void on_prop(void* ud, int target_kind, const char* cell, const char* propname, const char* propvalue) {
    auto* fp = static_cast<FILE*>(ud);
    const char* kind = "unknown";
    if (target_kind == 0) kind = "file";
    else if (target_kind == 1) kind = "cell";
    else if (target_kind == 2) kind = "element";
    else if (target_kind == 3) kind = "instance";
    fprintf(fp, "PROPERTY %s %s %s %s\n", kind, cell ? cell : "-", propname ? propname : "-", propvalue ? propvalue : "-");
}

static std::string layer_name_for(const ldb_oas_info_t* info, int layer, int datatype) {
    if (!info) return "-";
    char nm[256]; nm[0] = 0;
    if (ldb_oas_info_layer_name(info, layer, datatype, nm, sizeof(nm)) && nm[0]) return std::string(nm);
    return "-";
}

int main(int argc, char** argv) {
    bool flat = true;
    const char* path = nullptr;
    for (int i = 1; i < argc; ++i) {
        if (std::strcmp(argv[i], "--flat") == 0) flat = true;
        else if (std::strcmp(argv[i], "--no-flat") == 0) flat = false;
        else if (argv[i][0] == '-') { fprintf(stderr, "unknown option: %s\n", argv[i]); return 2; }
        else path = argv[i];
    }
    if (!path) { fprintf(stderr, "usage: db_oas_stats [--flat|--no-flat] file.gds|file.oas\n"); return 2; }

    char err[512] = {0};
    ldb_file_t* f = ldb_open(path, err, sizeof(err));
    if (!f) { fprintf(stderr, "db_oas_stats: open failed: %s\n", err); return 1; }

    printf("FILE %s\n", path);
    int fmt = ldb_format(f);
    printf("FORMAT %s\n", fmt == 1 ? "GDSII" : (fmt == 2 ? "OASIS" : "UNKNOWN"));
    printf("UNITS_PER_MICRON %.10g\n", ldb_dbu_per_micron(f));

    ldb_oas_info_t* info = nullptr;
    if (fmt == 2) {
        char e2[512] = {0};
        info = ldb_oas_info_open(path, e2, sizeof(e2));
        if (info) {
            char top[256] = "";
            ldb_oas_info_top(info, top, sizeof(top));
            printf("TOP %s\n", top[0] ? top : "-");
        } else {
            printf("TOP -\n");
        }
    } else {
        printf("TOP -\n");
    }

    ldb_extent_t e;
    if (ldb_extent_whole(f, &e) == 0 && e.valid)
        printf("EXTENT %" PRId64 " %" PRId64 " %" PRId64 " %" PRId64 "\n", (int64_t)e.xmin, (int64_t)e.ymin, (int64_t)e.xmax, (int64_t)e.ymax);
    else
        printf("EXTENT INVALID\n");

    Ctx ctx;
    int rc = flat ? ldb_iterate_boxes_flat(f, on_box, &ctx) : ldb_iterate_boxes(f, on_box, &ctx);
    if (rc != 0) {
        fprintf(stderr, "db_oas_stats: iterate boxes failed: %d\n", rc);
        if (info) ldb_oas_info_close(info);
        ldb_close(f);
        return 1;
    }

    for (const auto& kv : ctx.layers) {
        const Key& k = kv.first;
        const Accum& a = kv.second;
        std::string nm = layer_name_for(info, k.layer, k.datatype);
        // sanitize name for single-field output
        for (char& c : nm) if (c == ' ' || c == '\t' || c == '\n' || c == '\r') c = '_';
        printf("LAYER %d %d %s %" PRIu64 " %" PRId64 " %" PRId64 " %" PRId64 " %" PRId64 "\n",
               k.layer, k.datatype, nm.c_str(), a.count,
               (int64_t)a.xmin, (int64_t)a.ymin, (int64_t)a.xmax, (int64_t)a.ymax);
    }

    TextCtx tctx{stdout, 0};
    int trc = flat ? ldb_iterate_texts_flat(f, on_text, &tctx) : ldb_iterate_texts(f, on_text, &tctx);
    if (trc != 0) fprintf(stderr, "db_oas_stats: iterate texts failed: %d\n", trc);
    printf("TEXT_COUNT %" PRIu64 "\n", tctx.count);

    ldb_iterate_properties(f, on_prop, stdout);

    printf("END\n");
    if (info) ldb_oas_info_close(info);
    ldb_close(f);
    return 0;
}
