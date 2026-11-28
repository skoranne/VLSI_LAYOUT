#ifndef LAYOUTDB_HIER_H
#define LAYOUTDB_HIER_H

#include "layoutdb.h"
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    int64_t ox, oy;      /* placement origin */
    double mag;          /* magnification (1.0 default) */
    double angle_deg;    /* rotation degrees */
    int mirror_x;        /* reflect about x axis before rotation */
    int64_t cols, rows;  /* array repetition (>=1) */
    int64_t dx, dy;      /* array step x/y in first direction */
    int64_t row_dx, row_dy; /* array step for rows (second direction) */
} ldb_transform_t;

/* Iterate geometry with hierarchical flattening (applies SREF/AREF/placement transforms).
 * depth < 0 => unlimited. top_cell NULL/empty => auto-detect top(s).
 * Boxes emitted are axis-aligned bboxes in top-cell coordinates.
 */
int ldb_iterate_boxes_hier(ldb_file_t* f, const char* top_cell, int depth, ldb_box_cb cb, void* ud);

/* Whole design hierarchical extent (flattened). */
int ldb_extent_whole_hier(ldb_file_t* f, const char* top_cell, ldb_extent_t* out);

/* Get number of top-level cells (no parent references) */
int ldb_top_cell_count(ldb_file_t* f);
/* Get i-th top cell name (0-based). Returns pointer valid while file open. */
const char* ldb_top_cell_name(ldb_file_t* f, int i);


/* 1 when any cell places another cell (SREF/AREF/PLACEMENT), else 0. */
int ldb_is_hierarchical(ldb_file_t* f);

/* Cell view: the cell's own geometry in its coordinates (no flattening), plus one box per
 * placement (arrays expanded) giving the placed cell's flattened bbox, emitted on layer
 * LDB_INSTANCE_LAYER, datatype 0, element_kind 3. cell NULL/empty => first top cell.
 * Returns 0, or -2 when the cell does not exist. */
#define LDB_INSTANCE_LAYER (-1)
int ldb_iterate_cell_view(ldb_file_t* f, const char* cell, ldb_box_cb cb, void* ud);

/* Per (layer, datatype) found anywhere below `cell` (NULL/empty => first top cell):
 * own  = boxes in the cell itself, flat = boxes after flattening the hierarchy,
 * cells = number of distinct cells below (and including) `cell` with that layer.
 * Called in (layer, datatype) order. */
typedef void (*ldb_layer_summary_cb)(void* ud, int layer, int datatype, int64_t own, int64_t flat, int64_t cells);
int ldb_layer_summary_hier(ldb_file_t* f, const char* cell, ldb_layer_summary_cb cb, void* ud);
/* Placements of `cell` (arrays expanded), each with its transform (p -> R(angle) Mx p * mag + o),
 * the placed cell's flattened extent in its own coordinates (l*) and in `cell` coordinates (w*),
 * and the placement name (DEF component; "" when the file has none). */
typedef void (*ldb_placement_cb)(void* ud, const char* ref_cell, const char* inst_name,
                                 int64_t ox, int64_t oy, double angle_deg, int mirror_x, double mag,
                                 int64_t lx1, int64_t ly1, int64_t lx2, int64_t ly2,
                                 int64_t wx1, int64_t wy1, int64_t wx2, int64_t wy2);
int ldb_iterate_placements(ldb_file_t* f, const char* cell, ldb_placement_cb cb, void* ud);
/* Properties of placement `index` (0-based, in ldb_iterate_placements order before array
 * expansion is applied: the k-th LdbInstance of the cell). cb(ud, name, value). */
typedef void (*ldb_kv_cb)(void* ud, const char* name, const char* value);
int ldb_placement_properties(ldb_file_t* f, const char* cell, int64_t index, ldb_kv_cb cb, void* ud);
/* Placements in `cell` with arrays expanded (NULL/empty => first top cell). */
int64_t ldb_cell_placements(ldb_file_t* f, const char* cell);

#ifdef __cplusplus
}
#endif

#endif
