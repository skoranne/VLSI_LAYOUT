// File   : boost_xor_checker.cpp
// Author : Sandeep Koranne (C) 2026.
// Purpose: Use Boost polygon API to check Boolean operations
#include <iostream>
#include <fstream>
#include <vector>
#include <stdlib.h>
#include <boost/polygon/polygon.hpp>

namespace bp = boost::polygon;

// Define Boost.Polygon types for 32-bit signed integers
typedef int32_t coord_t;
typedef bp::rectangle_data<coord_t> Rectangle;

// FIX 1: Use polygon_90_set_data specifically for Manhattan (axis-aligned) geometry
typedef bp::polygon_90_set_data<coord_t> Polygon90Set;

// Helper structure matching the 4-byte int binary file layout
struct BinaryRect {
  coord_t x1, y1, x2, y2;
};

// Reads rectangles from a binary file and pushes them into a vector for bulk loading
bool read_rectangles_binary(const std::string& filename, std::vector<Rectangle>& rects) {
  std::ifstream infile(filename, std::ios::binary);
  if (!infile) {
    std::cerr << "Error: Cannot open input file " << filename << "\n";
    return false;
  }

  BinaryRect rect;
  // Read chunks of 16 bytes (4 coordinates * 4 bytes each)
  while (infile.read(reinterpret_cast<char*>(&rect), sizeof(BinaryRect))) {
    // Construct and normalize the rectangle (ensures x1 <= x2 and y1 <= y2)
    Rectangle r = bp::construct<Rectangle>(rect.x1, rect.y1, rect.x2, rect.y2);
    rects.push_back(r);
  }
  return true;
}
const coord_t K_MAXIMUM_WIDTH = 10000;
const coord_t K_MAXIMUM_HEIGHT = 10000;
typedef bp::rectangle_data<coord_t> Rectangle;
void subdivide_rectangles(const std::vector<Rectangle>& input_rects, 
			  std::vector<Rectangle>& output_rects) {
    
  for (const auto& rect : input_rects) {
    coord_t x_min = bp::xl(rect);
    coord_t y_min = bp::yl(rect);
    coord_t x_max = bp::xh(rect);
    coord_t y_max = bp::yh(rect);

    // Tile the rectangle along the X and Y axes
    for (coord_t x = x_min; x < x_max; x += K_MAXIMUM_WIDTH) {
      coord_t current_x_max = std::min(x + K_MAXIMUM_WIDTH, x_max);
            
      for (coord_t y = y_min; y < y_max; y += K_MAXIMUM_HEIGHT) {
	coord_t current_y_max = std::min(y + K_MAXIMUM_HEIGHT, y_max);
                
	// Construct the bounded chunk safely using Boost's API
	Rectangle chunk(x, y, current_x_max, current_y_max);
	output_rects.push_back(chunk);
      }
    }
  }
}

int main(int argc, char* argv[]) {
  using namespace boost::polygon::operators;
  int control_parameter = 0;
  if (argc != 5) {
    std::cerr << "Usage: " << argv[0] << " <input1.bin> <input2.bin> <output.bin> CONTROL\n";
    std::cerr << "CONTROL 0 : XOR\n"
	      << "CONTROL 1 : OR\n"
	      << "CONTROL 2 : AND\n"
	      << "CONTROL 3 : NOT\n"
	      << "CONTROL 4 : MERGE\n";
    return 1;
  }

  std::string file1 = argv[1];
  std::string file2 = argv[2];
  std::string outfile = argv[3];
  control_parameter = atoi( argv[4] );
  std::vector<Rectangle> rects1;
  std::vector<Rectangle> rects2;

  // 1. Read binary files into vectors
  if (!read_rectangles_binary(file1, rects1) || !read_rectangles_binary(file2, rects2)) {
    return 1;
  }

  // 2. Bulk load the vectors of rectangles into the Polygon90Sets
  Polygon90Set set1;
  set1.insert(rects1.begin(), rects1.end());

  Polygon90Set set2;
  set2.insert(rects2.begin(), rects2.end());


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
  
  default:
    std::cerr << "ERROR: invalid control parameter.\n";
  }

  // 3. Extract the merged rectangles
  std::vector<bp::rectangle_data<coord_t>> merged_rects_pre_sd;
  result.get_rectangles(merged_rects_pre_sd);
  result.clear();
  std::vector<bp::rectangle_data<coord_t>> output_rects;    
  subdivide_rectangles( merged_rects_pre_sd, output_rects );
  merged_rects_pre_sd.clear();
  

  // 5. Write the resulting rectangles to the output file in binary format
  std::ofstream out_file(outfile, std::ios::binary);
  if (!out_file) {
    std::cerr << "Error: Cannot open output file " << outfile << "\n";
    return 1;
  }

  for (const auto& r : output_rects) {
    BinaryRect rect;
    rect.x1 = bp::xl(r); // Left x
    rect.y1 = bp::yl(r); // Bottom y
    rect.x2 = bp::xh(r); // Right x
    rect.y2 = bp::yh(r); // Top y

    out_file.write(reinterpret_cast<const char*>(&rect), sizeof(BinaryRect));
  }

  std::cout << "Output file written with " << output_rects.size() << " rectangles.\n";

  return output_rects.empty() ? 0 : -1;
}
