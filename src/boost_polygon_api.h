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
void PerformMerge(const struct c_box* input, 
                  unsigned long N, 
                  struct c_box* output, 
                  unsigned long* outN);

#ifdef __cplusplus
}
#endif

#endif // MERGE_API_H
