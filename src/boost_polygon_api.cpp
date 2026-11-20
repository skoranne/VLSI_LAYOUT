// 
//  File   : boost_polygon_api.h
//  Author : Sandeep Koranne (C) 2026.
//  Purpose: Use Boost Polygon library in Fortran
// 
#include "boost_polygon_api.h"
#include <boost/polygon/polygon.hpp>
#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <vector>

namespace bp = boost::polygon;
typedef bp::polygon_90_set_data<coord_t> Polygon90Set;

// 1. Register c_box as a Boost.Polygon Rectangle Concept
namespace boost { namespace polygon {
    template <>
    struct geometry_concept<c_box> { 
      typedef rectangle_concept type; 
    };

    template <>
    struct rectangle_traits<c_box> {
      typedef coord_t coordinate_type;
      typedef interval_data<coordinate_type> interval_type; 

      // FIXED: Boost requires get() to take 2 arguments and return the full interval 
      // for the requested orientation (HORIZONTAL or VERTICAL).
      static inline interval_type get(const c_box& b, orientation_2d orient) {
	if (orient == HORIZONTAL) {
	  // Return the X bounds
	  return interval_type(b.x1, b.x2);
	} else {
	  // Return the Y bounds
	  return interval_type(b.y1, b.y2);
	}
      }
    };
  }} // namespace boost::polygon

extern "C" {

  void FreeBoostPolygonBuffer(struct c_box* ptr) {
    std::free(ptr);
  }

  struct c_box* PerformBoostPolygonMergeAlloc(const struct c_box* input,
                                              unsigned long N,
                                              unsigned long* outN)
  {
    if (N == 0 || input == nullptr) {
      *outN = 0;
      return nullptr;
    }

    std::vector<bp::rectangle_data<coord_t>> merged_rects;
    {
      Polygon90Set poly_set;
      // Zero-Copy Ingestion
      poly_set.insert(input, input + N);
      // Extract the merged rectangles; poly_set is destroyed at scope exit to free memory immediately
      poly_set.get_rectangles(merged_rects);
    }

    const size_t count = merged_rects.size();
    *outN = static_cast<unsigned long>(count);
    if (count == 0) {
      return nullptr;
    }

    struct c_box* out_buf = static_cast<struct c_box*>(std::malloc(count * sizeof(struct c_box)));
    if (out_buf == nullptr) {
      return nullptr;
    }
    for (size_t i = 0; i < count; ++i) {
      out_buf[i].x1 = bp::xl(merged_rects[i]);
      out_buf[i].y1 = bp::yl(merged_rects[i]);
      out_buf[i].x2 = bp::xh(merged_rects[i]);
      out_buf[i].y2 = bp::yh(merged_rects[i]);
    }
    return out_buf;
  }

  void PerformBoostPolygonMerge(const struct c_box* input,
				unsigned long N,
				struct c_box* output,
				unsigned long* outN)
  {
    struct c_box* buf = PerformBoostPolygonMergeAlloc(input, N, outN);
    if (buf != nullptr) {
      if (output != nullptr && *outN > 0) {
        std::memcpy(output, buf, (*outN) * sizeof(struct c_box));
      }
      std::free(buf);
    }
  }

  static const int64_t K_MAXIMUM_WIDTH  = 100000;
  static const int64_t K_MAXIMUM_HEIGHT = 100000;
  typedef bp::rectangle_data<coord_t> Rectangle;

  struct c_box* PerformBoostPolygonOperationAlloc(const struct c_box* input_A,
                                                  unsigned long AN,
                                                  const struct c_box* input_B,
                                                  unsigned long BN,
                                                  unsigned long* outN,
                                                  unsigned long control_parameter,
                                                  long control_value)
  {
    using namespace boost::polygon::operators;
    if ((AN == 0) && (BN == 0)) {
      *outN = 0;
      return nullptr;
    }

    std::vector<Rectangle> merged_rects_pre_sd;
    {
      Polygon90Set set1;
      if (AN > 0 && input_A != nullptr) {
        set1.insert(input_A, input_A + AN);
      }

      switch (control_parameter) {
      case 0: { // XOR
        Polygon90Set set2;
        if (BN > 0 && input_B != nullptr) set2.insert(input_B, input_B + BN);
        set1 ^= set2;
        break;
      }
      case 1: { // OR
        Polygon90Set set2;
        if (BN > 0 && input_B != nullptr) set2.insert(input_B, input_B + BN);
        set1 |= set2;
        break;
      }
      case 2: { // AND
        Polygon90Set set2;
        if (BN > 0 && input_B != nullptr) set2.insert(input_B, input_B + BN);
        set1 &= set2;
        break;
      }
      case 3: { // NOT (A - B)
        Polygon90Set set2;
        if (BN > 0 && input_B != nullptr) set2.insert(input_B, input_B + BN);
        set1 -= set2;
        break;
      }
      case 4: // MERGE (unary on A)
        break;
      case 5: { // SIZE (unary on A)
        coord_t size_amount = static_cast<coord_t>(control_value);
        boost::polygon::resize(set1, size_amount);
        break;
      }
      default:
        *outN = 0;
        return nullptr;
      }
      // set2 (if any) is already destroyed here, freeing its memory before extraction.
      set1.get_rectangles(merged_rects_pre_sd);
      // set1 is destroyed at scope exit, freeing its memory before subdivision/allocation.
    }

    // Pass 1: count exact number of subdivided rectangles using 64-bit arithmetic
    // (avoids int32_t overflow near INT32_MAX and avoids intermediate vector copy).
    size_t total_out = 0;
    for (const auto& rect : merged_rects_pre_sd) {
      const int64_t x_min = static_cast<int64_t>(bp::xl(rect));
      const int64_t y_min = static_cast<int64_t>(bp::yl(rect));
      const int64_t x_max = static_cast<int64_t>(bp::xh(rect));
      const int64_t y_max = static_cast<int64_t>(bp::yh(rect));
      if (x_max <= x_min || y_max <= y_min) continue;
      const int64_t nx = (x_max - x_min + K_MAXIMUM_WIDTH  - 1) / K_MAXIMUM_WIDTH;
      const int64_t ny = (y_max - y_min + K_MAXIMUM_HEIGHT - 1) / K_MAXIMUM_HEIGHT;
      total_out += static_cast<size_t>(nx) * static_cast<size_t>(ny);
    }

    *outN = static_cast<unsigned long>(total_out);
    if (total_out == 0) {
      return nullptr;
    }

    // Pass 2: allocate exact output buffer and tile directly into it
    struct c_box* out_buf = static_cast<struct c_box*>(std::malloc(total_out * sizeof(struct c_box)));
    if (out_buf == nullptr) {
      return nullptr;
    }

    size_t idx = 0;
    for (const auto& rect : merged_rects_pre_sd) {
      const int64_t x_min = static_cast<int64_t>(bp::xl(rect));
      const int64_t y_min = static_cast<int64_t>(bp::yl(rect));
      const int64_t x_max = static_cast<int64_t>(bp::xh(rect));
      const int64_t y_max = static_cast<int64_t>(bp::yh(rect));
      if (x_max <= x_min || y_max <= y_min) continue;

      for (int64_t x = x_min; x < x_max; x += K_MAXIMUM_WIDTH) {
        const int64_t current_x_max = std::min(x + K_MAXIMUM_WIDTH, x_max);
        for (int64_t y = y_min; y < y_max; y += K_MAXIMUM_HEIGHT) {
          const int64_t current_y_max = std::min(y + K_MAXIMUM_HEIGHT, y_max);
          out_buf[idx].x1 = static_cast<coord_t>(x);
          out_buf[idx].y1 = static_cast<coord_t>(y);
          out_buf[idx].x2 = static_cast<coord_t>(current_x_max);
          out_buf[idx].y2 = static_cast<coord_t>(current_y_max);
          ++idx;
        }
      }
    }
    return out_buf;
  }

  void PerformBoostPolygonOperation(const struct c_box* input_A,
				    unsigned long AN,
				    const struct c_box* input_B,
				    unsigned long BN,
				    struct c_box* output,
				    unsigned long* outN,
				    unsigned long control_parameter,
				    long control_value)
  {
    struct c_box* buf = PerformBoostPolygonOperationAlloc(
        input_A, AN, input_B, BN, outN, control_parameter, control_value);
    if (buf != nullptr) {
      if (output != nullptr && *outN > 0) {
        std::memcpy(output, buf, (*outN) * sizeof(struct c_box));
      }
      std::free(buf);
    }
  }

} // extern "C"
