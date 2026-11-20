/*
 * File   : boost_polygon_api.h
 * Author : Sandeep Koranne (C) 2026.
 * Purpose: Use Boost Polygon library in Fortran
 */
#ifndef BOOST_POLYGON_MERGE_API_H
#define BOOST_POLYGON_MERGE_API_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Maps perfectly to integer(c_int32_t) in Fortran
typedef int32_t coord_t;

struct c_box {
    coord_t x1;
    coord_t y1;
    coord_t x2;
    coord_t y2;
};

// C interface for the Boost implementation
void PerformBoostPolygonMerge(const struct c_box* input,
                              unsigned long N,
                              struct c_box* output,
                              unsigned long* outN);
void PerformBoostPolygonOperation(const struct c_box* input_A,
                                  unsigned long AN,
                                  const struct c_box* input_B,
                                  unsigned long BN,
                                  struct c_box* output,
                                  unsigned long* outN,
                                  unsigned long control_parameter,
                                  long control_value);

// Exact-sized allocating C interface (caller must free via FreeBoostPolygonBuffer)
struct c_box* PerformBoostPolygonMergeAlloc(const struct c_box* input,
                                            unsigned long N,
                                            unsigned long* outN);
struct c_box* PerformBoostPolygonOperationAlloc(const struct c_box* input_A,
                                                unsigned long AN,
                                                const struct c_box* input_B,
                                                unsigned long BN,
                                                unsigned long* outN,
                                                unsigned long control_parameter,
                                                long control_value);
void FreeBoostPolygonBuffer(struct c_box* ptr);

#ifdef __cplusplus
}
#endif

#endif // MERGE_API_H
