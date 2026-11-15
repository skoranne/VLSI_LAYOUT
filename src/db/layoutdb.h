#ifndef LAYOUTDB_H
#define LAYOUTDB_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ldb_file ldb_file_t;

typedef struct {
    int64_t xmin, ymin, xmax, ymax;
    int valid;
} ldb_extent_t;

ldb_file_t* ldb_open(const char* path, char* errbuf, size_t errbuf_sz);
void ldb_close(ldb_file_t* f);
int ldb_format(const ldb_file_t* f); /* 1=GDSII, 2=OASIS, 0=unknown */
size_t ldb_memory_bytes(const ldb_file_t* f);

/* 1) whole file extent */
int ldb_extent_whole(ldb_file_t* f, ldb_extent_t* out);
/* 2) extents per cell/layer: callback for each (cell,layer,datatype,extent) */
typedef void (*ldb_cell_layer_cb)(void* ud, const char* cell, int layer, int datatype, ldb_extent_t ext);
int ldb_extent_by_cell_layer(ldb_file_t* f, ldb_cell_layer_cb cb, void* ud);

/* Iterate geometry as boxes (bbox of each element). element_kind: 0=boundary/polygon,1=path,2=box/rect,3=sref,4=aref,5=other */
typedef void (*ldb_box_cb)(void* ud, const char* cell, int layer, int datatype, int element_kind,
                           int64_t x1, int64_t y1, int64_t x2, int64_t y2);
int ldb_iterate_boxes(ldb_file_t* f, ldb_box_cb cb, void* ud);
/* 3) print cell hierarchy to FILE* as text */
int ldb_print_hierarchy(ldb_file_t* f, void* out_fp);
/* 4) print property name table */
int ldb_print_propnames(ldb_file_t* f, void* out_fp);

/* lossless record-stream roundtrip for GDSII: returns 0 on success */
int ldb_write_gdsii(const ldb_file_t* f, const char* out_path, char* errbuf, size_t errbuf_sz);
/* OASIS writer (basic/MULTIGON support) */
int ldb_write_oasis(const ldb_file_t* f, const char* out_path, char* errbuf, size_t errbuf_sz);
/* create synthetic GEAR-like curvilinear MULTIGON OASIS for 3DIC test */
int ldb_write_oasis_gear_multigon(const char* out_path, char* errbuf, size_t errbuf_sz);

#ifdef __cplusplus
}
#endif

#endif
