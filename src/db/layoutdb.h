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

/* Open options: flags bitmask */
#define LDB_OPEN_DEFAULT      0u
#define LDB_OPEN_STRICT       1u  /* OASIS: enforce/expect STRICT mode semantics; strict validation */
#define LDB_OPEN_PROPERTY     2u  /* parse and retain properties (PROPERTY mode) */
#define LDB_OPEN_LOW_MEMORY   4u  /* keep large-cell geometry in disk-access/on-demand storage */

typedef struct {
    uint32_t flags;
    uint64_t large_cell_box_threshold; /* if >0, cells with more boxes than this use disk/on-demand storage in LOW_MEMORY mode */
} ldb_open_opts_t;

ldb_file_t* ldb_open(const char* path, char* errbuf, size_t errbuf_sz);
ldb_file_t* ldb_open_opts(const char* path, const ldb_open_opts_t* opts, char* errbuf, size_t errbuf_sz);
void ldb_close(ldb_file_t* f);
int ldb_format(const ldb_file_t* f); /* 1=GDSII, 2=OASIS, 0=unknown */
int ldb_is_strict(const ldb_file_t* f); /* 1 if OASIS STRICT mode indicated/enforced */
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
/* Iterate geometry as boxes with hierarchy flattened (SREF/AREF expanded). */
int ldb_iterate_boxes_flat(ldb_file_t* f, ldb_box_cb cb, void* ud);
/* Streaming flat load (OASIS): boxes are passed to cb while parsing, nothing is stored.
   Returns 0 ok, 2 hierarchical file (fall back to ldb_open), 3 not OASIS, -1 error. */
int ldb_stream_boxes(const char* path, ldb_box_cb cb, void* ud, char* errbuf, size_t errbuf_sz);

/* Iterate geometry as polygons with explicit vertices (DBU coordinates).
   xs/ys have npts entries. For bbox-only elements, a 4-point closed rectangle is emitted.
   element_kind: 0=boundary/polygon,1=path,2=box/rect */
typedef void (*ldb_poly_cb)(void* ud, const char* cell, int layer, int datatype, int element_kind,
                            int npts, const int64_t* xs, const int64_t* ys);
int ldb_iterate_polygons(ldb_file_t* f, ldb_poly_cb cb, void* ud);
int ldb_iterate_polygons_flat(ldb_file_t* f, ldb_poly_cb cb, void* ud);
/* 3) print cell hierarchy to FILE* as text */
int ldb_print_hierarchy(ldb_file_t* f, void* out_fp);
/* 4) print property name table */
int ldb_print_propnames(ldb_file_t* f, void* out_fp);

/* Property callback: target_kind 0=file,1=cell,2=element,3=instance */
typedef void (*ldb_property_cb)(void* ud, int target_kind, const char* cell, const char* propname, const char* propvalue);
int ldb_iterate_properties(ldb_file_t* f, ldb_property_cb cb, void* ud);

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
