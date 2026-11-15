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

#ifdef __cplusplus
}
#endif

#endif
