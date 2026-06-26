// File   : boost_xor_checker.cpp
// Author : Sandeep Koranne (C) 2026.
// Purpose: Use Boost polygon API to check Boolean operations
#include <iostream>
#include <fstream>
#include <vector>
#include <boost/polygon/polygon.hpp>
#include <boost/polygon/polygon_set_data.hpp>      
#include <boost/polygon/rectangle_data.hpp>        

namespace bp = boost::polygon;

// Define Boost.Polygon types for 32-bit signed integers
typedef int32_t coord_t;
typedef bp::rectangle_data<coord_t> Rectangle;
// FIX 1: Use polygon_data as the container for extracted trapezoids
typedef bp::polygon_data<coord_t> Polygon; 
typedef bp::polygon_set_data<coord_t> PolygonSet;

// Helper structure matching the 4-byte int binary file layout
struct BinaryRect {
  coord_t x1, y1, x2, y2;
};

// Reads rectangles from a binary file and inserts them into a PolygonSet
bool read_rectangles_binary(const std::string& filename, PolygonSet& poly_set) {
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
    poly_set.insert(r);
  }
  return true;
}

int main(int argc, char* argv[]) {
  using namespace boost::polygon::operators;
  if (argc != 4) {
    std::cerr << "Usage: " << argv[0] << " <input1.bin> <input2.bin> <output.bin>\n";
    return 1;
  }

  std::string file1 = argv[1];
  std::string file2 = argv[2];
  std::string outfile = argv[3];

  PolygonSet set1;
  PolygonSet set2;

  // 1. Read binary files into PolygonSets
  if (!read_rectangles_binary(file1, set1) || !read_rectangles_binary(file2, set2)) {
    return 1;
  }

  // 2. Perform the XOR operation
  PolygonSet xor_result = set1 ^ set2;

  // 3. Extract the result back into rectangles
  std::vector<Rectangle> output_rects;
  std::vector<Polygon> trapezoids; // Now using polygon_data
  
  // Decompose the XOR result into a vector of trapezoids
  xor_result.get_trapezoids(trapezoids);
  
  // FIX 2: Removed #if 0 so the extraction logic actually runs
  output_rects.reserve(trapezoids.size());

  for (const auto& trap : trapezoids) {
      Rectangle bbox;
      // Compute the minimum bounding rectangle (bounding box) of the trapezoid
      boost::polygon::extents(bbox, trap);
      output_rects.push_back(bbox);
  }

  // 4. Write the resulting rectangles to the output file in binary format
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

  std::cout << "Successfully performed XOR.\n";
  std::cout << "Output file written with " << output_rects.size() << " rectangles.\n";

  return output_rects.empty() ? 0 : -1;
}
