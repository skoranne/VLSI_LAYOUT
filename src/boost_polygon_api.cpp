// 
//  File   : boost_polygon_api.h
//  Author : Sandeep Koranne (C) 2026.
//  Purpose: Use Boost Polygon library in Fortran
// 
#include "boost_polygon_api.h"
#include <boost/polygon/polygon.hpp>
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

  void PerformBoostPolygonMerge(const struct c_box* input, 
				unsigned long N, 
				struct c_box* output, 
				unsigned long* outN) 
  {
    if (N == 0) {
      *outN = 0;
      return;
    }

    Polygon90Set poly_set;

    // 2. Zero-Copy Ingestion
    // Pass the raw C-array pointers. Boost will iterate over the Fortran memory 
    // directly and use the traits defined above to extract the coordinates.
    poly_set.insert(input, input + N);

    // 3. Extract the merged rectangles
    std::vector<bp::rectangle_data<coord_t>> merged_rects;
    poly_set.get_rectangles(merged_rects);
    poly_set.clear();
    // 4. Write back to the pre-allocated output buffer
    *outN = merged_rects.size();
    for (size_t i = 0; i < merged_rects.size(); ++i) {
      output[i].x1 = bp::xl(merged_rects[i]);
      output[i].y1 = bp::yl(merged_rects[i]);
      output[i].x2 = bp::xh(merged_rects[i]);
      output[i].y2 = bp::yh(merged_rects[i]);
    }
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
    using namespace boost::polygon::operators;
    if ((AN == 0) && (BN == 0)) {
      *outN = 0;
      return;
    }
    coord_t size_amount = (coord_t) control_value;    
    Polygon90Set set1, set2;
    if( AN > 0 ) set1.insert(input_A, input_A + AN);
    if( BN > 0 ) set2.insert(input_B, input_B + BN);
    Polygon90Set result;
    switch( control_parameter ) {
    case 0:
      result = set1 ^ set2;
      break;
    case 1:
      result = set1 | set2;
      break;
    case 2:
      result = set1 & set2;
      break;
    case 3:
      result = set1 - set2;
      break;
    case 4:
      result = set1 ;
      break;
    case 5:
      result = set1;
      boost::polygon::resize(result, size_amount);
      break;
    default:
      *outN = 0;
      return;
    }
    // 3. Extract the merged rectangles
    std::vector<bp::rectangle_data<coord_t>> merged_rects;
    result.get_rectangles(merged_rects);
    result.clear();
    // 4. Write back to the pre-allocated output buffer
    *outN = merged_rects.size();
    for (size_t i = 0; i < merged_rects.size(); ++i) {
      output[i].x1 = bp::xl(merged_rects[i]);
      output[i].y1 = bp::yl(merged_rects[i]);
      output[i].x2 = bp::xh(merged_rects[i]);
      output[i].y2 = bp::yh(merged_rects[i]);
    }
    merged_rects.clear();
  }

  
} // extern "C"
