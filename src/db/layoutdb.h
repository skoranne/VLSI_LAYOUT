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
#define LDB_OPEN_SHAPES       8u  /* OASIS: also keep exact polygons, paths and texts next to the boxes
                                     (lossless model re-encoding, ldb_iterate_polygons); can take
                                     several times the box memory on flat polygon-heavy files.
                                     GDSII always keeps them (except with LOW_MEMORY). */

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

/* Iterate TEXT elements (DBU coordinates). str is UTF-8 (OASIS TEXT) or ASCII (GDSII STRING). */
typedef void (*ldb_text_cb)(void* ud, const char* cell, int layer, int texttype,
                            int64_t x, int64_t y, const char* str);
int ldb_iterate_texts(ldb_file_t* f, ldb_text_cb cb, void* ud);
int ldb_iterate_texts_flat(ldb_file_t* f, ldb_text_cb cb, void* ud);
/* 3) print cell hierarchy to FILE* as text */
int ldb_print_hierarchy(ldb_file_t* f, void* out_fp);
/* 4) print property name table */
int ldb_print_propnames(ldb_file_t* f, void* out_fp);

/* Property callback: target_kind 0=file,1=cell,2=element,3=instance */
typedef void (*ldb_property_cb)(void* ud, int target_kind, const char* cell, const char* propname, const char* propvalue);
int ldb_iterate_properties(ldb_file_t* f, ldb_property_cb cb, void* ud);

/* GDSII writer: a file read from GDSII is written back record for record (lossless);
   any other model (e.g. read from OASIS) is encoded as GDSII from the model. 0 on success. */
int ldb_write_gdsii(const ldb_file_t* f, const char* out_path, char* errbuf, size_t errbuf_sz);
/* Database units per micron (OASIS START unit, GDSII UNITS); 0 if unknown. */
double ldb_dbu_per_micron(const ldb_file_t* f);

/* OASIS writer options (oasis_writer.cpp, streaming writer oasis_writer.hpp). */
typedef struct {
    int strict;              /* 1: strict name tables, references by number, S_* properties (default 1) */
    int cblock_kb;           /* CBLOCK size in KiB of uncompressed records; 0 = no compression (default 4096) */
    int repetitions;         /* 1: find rectangle arrays and write them as repetitions (default 1) */
    int validation;          /* 0 none, 1 CRC32, 2 CHECKSUM32 (default 1) */
    int reencode;            /* 1: re-encode a model read from OASIS instead of copying its bytes (default 0) */
    int independent_cblocks; /* 1: every CBLOCK decodes without the previous one's modal state (default 0) */
    int deflate_level;       /* zlib level 1..9 (default 6) */
} ldb_oasis_write_opts_t;
void ldb_oasis_write_opts_default(ldb_oasis_write_opts_t* o);
int ldb_write_oasis_opts(const ldb_file_t* f, const char* out_path, const ldb_oasis_write_opts_t* o,
                         char* errbuf, size_t errbuf_sz);
/* OASIS writer with the default options */
int ldb_write_oasis(const ldb_file_t* f, const char* out_path, char* errbuf, size_t errbuf_sz);
/* create synthetic GEAR-like curvilinear MULTIGON OASIS for 3DIC test */
int ldb_write_oasis_gear_multigon(const char* out_path, char* errbuf, size_t errbuf_sz);


/* Fast open of an OASIS file by its tables only (oasis_tables.cpp): unit, S_TOP_CELL,
   CELLNAME table with S_CELL_OFFSET / S_BOUNDING_BOX, LAYERNAME table. No geometry decode. */
typedef struct ldb_oas_info ldb_oas_info_t;
ldb_oas_info_t* ldb_oas_info_open(const char* path, char* errbuf, size_t errbuf_sz);
void   ldb_oas_info_close(ldb_oas_info_t* info);
int    ldb_oas_info_strict(const ldb_oas_info_t* info);      /* 1: strict CELLNAME table present */
int    ldb_oas_info_offsets_ok(const ldb_oas_info_t* info);  /* 1: every S_CELL_OFFSET hits its CELL */
double ldb_oas_info_unit(const ldb_oas_info_t* info);        /* database steps per micron */
double ldb_oas_info_open_ms(const ldb_oas_info_t* info);
int    ldb_oas_info_top(const ldb_oas_info_t* info, char* name, int len);
int    ldb_oas_info_n_layernames(const ldb_oas_info_t* info);
/* hi = -1 means unbounded */
int    ldb_oas_info_layername(const ldb_oas_info_t* info, int i, int64_t* l_lo, int64_t* l_hi,
                              int64_t* d_lo, int64_t* d_hi, int* is_text, char* name, int len);
int    ldb_oas_info_layer_name(const ldb_oas_info_t* info, int layer, int datatype, char* name, int len);
int    ldb_oas_info_n_cells(const ldb_oas_info_t* info);
/* bbox = x1,y1,x2,y2; bbox_flags = -1 when the cell has no S_BOUNDING_BOX; offset = -1 when no S_CELL_OFFSET */
int    ldb_oas_info_cell(const ldb_oas_info_t* info, int i, char* name, int len, int64_t* offset,
                         int* bbox_flags, int64_t* bbox);

/* MAGVIEWER sidecar index (oasis_sidecar.cpp) for flat OASIS files: random access by CBLOCK
   ("unit"), per-unit per-layer box counts and bboxes, modal-state snapshots where a unit
   depends on what came before it, and a per-layer density pyramid (box count + covered area
   per tile, 2x2 reduced per level). Layer and unit indices are 0-based. */
typedef struct ldb_mvi ldb_mvi_t;
/* 0 ok, 2 hierarchical file (not supported yet), -1 error; errbuf gets a one-line summary on success */
int        ldb_mvi_build(const char* oas_path, const char* mvi_path, int grid, char* errbuf, size_t errbuf_sz);
/* NULL when missing, stale (OASIS size/mtime changed) or invalid */
ldb_mvi_t* ldb_mvi_open(const char* oas_path, const char* mvi_path, char* errbuf, size_t errbuf_sz);
/* mvi_path NULL/"": <oas>.mvi, or $MAGVIEWER_CACHE_DIR / $TMPDIR when the directory is read-only */
ldb_mvi_t* ldb_mvi_open_or_build(const char* oas_path, const char* mvi_path, int grid, int* built,
                                 char* errbuf, size_t errbuf_sz);
void       ldb_mvi_close(ldb_mvi_t* m);
const char* ldb_mvi_path(const ldb_mvi_t* m);
int        ldb_mvi_info(const ldb_mvi_t* m, double* unit, int64_t* bbox, int64_t* tile, int* nx, int* ny,
                        int* nlevels, int* nlayers, int64_t* nunits, int64_t* total_boxes, int64_t* n_dependent,
                        double* build_s);
int        ldb_mvi_layer(const ldb_mvi_t* m, int i, int64_t* layer, int64_t* datatype, int64_t* count, int64_t* bbox);
/* level 0 = finest; counts/coverage point into the mapped sidecar (row-major, ax*ay);
   coverage = covered area / tile area (can exceed 1 where shapes overlap) */
int        ldb_mvi_density(const ldb_mvi_t* m, int i, int level, int* ax, int* ay,
                           const uint32_t** counts, const float** coverage);
/* units with boxes of layer i (i < 0: any) meeting the window; returns the count, writes up to cap ids */
int64_t    ldb_mvi_units_in(const ldb_mvi_t* m, int i, int64_t x1, int64_t y1, int64_t x2, int64_t y2,
                            int64_t* ids, int64_t cap);
/* boxes of layer i (i < 0: all) in unit u, and their bbox */
int64_t    ldb_mvi_unit_boxes(const ldb_mvi_t* m, int64_t u, int i, int64_t* bbox);
/* parallel decode of units -> int32 x1,y1,x2,y2 per box; returns boxes, -(needed) if cap too small,
   INT64_MIN on error; unit_off (n+1, optional) = first box of each unit */
int64_t    ldb_mvi_decode(const ldb_mvi_t* m, const int64_t* units, int64_t n, int i, int32_t* xyxy, int64_t cap,
                          int64_t* unit_off, char* errbuf, size_t errbuf_sz);

#ifdef __cplusplus
}
#endif

#endif
