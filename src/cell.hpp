////////////////////////////////////////////////////////////////////////////////
// File   : cell.hpp
// Author : Sandeep Koranne
//
//#if 0
//Using C++ write a simple scanner for GDSII files which does mmap in DIRECT
//mode of the file, and then scans the mmap for STRUCTURE NAME;
//populate a std::map<std::string, Cell*> where Cell* is a pointer to a
//Cell class containing its name, file offset, and a number. We will
//  add instances and polygons into this as we go.
//#endif
#pragma once
#include <string>
#include <vector>
#include <cstdint>
#include <iostream>
#include <utility>

namespace VLSILayout {


  /// Simple description of an instance (SREF) of another cell
  struct Instance
  {
    std::string   cellName;          // name from the SNAME record
    std::int64_t  x = 0, y = 0;      // location taken from the XY record
    std::uint16_t strans = 0;             // transformation flags (STRANS)
    double        mag    = 1.0;           // magnification (MAG)
    double        angle  = 0.0;           // rotation angle (ANGLE)

    Instance(std::string n, std::int64_t xx, std::int64_t yy)
      : cellName(std::move(n)), x(xx), y(yy) {}
  };

  /// Simple polygon (BOUNDARY) – just a list of points and a layer number
  struct Polygon
  {
    std::uint16_t layer = 0;                       // from the LAYER record
    std::uint16_t datatype = 0;                    // from the DATATYPE record
    int width = 0;
    using Coordinate = std::pair<std::int64_t,std::int64_t>;
    std::vector<Coordinate> points; // (x,y) pairs from XY
    void Clear() { points.clear(); layer = datatype = -1; width = 0;}
    explicit Polygon(std::uint16_t l = 0) : layer(l) {}
  };
  struct Text
  {
    std::string   txt;          // the actual character string (STRING record)
    std::uint16_t layer   = 0;  // layer on which the text is drawn
    std::uint16_t texttype   = 0;  // layer on which the text is drawn
    std::uint16_t presentation = 0;
    std::int64_t  x = 0, y = 0; // location (first XY pair after TEXT)
    double        mag        = 1.0;
    double        angle      = 0.0;
    explicit Text() = default;
  };
  /*--------------------------------------------------------------
   *  AREF (array reference) description
   *--------------------------------------------------------------*/
  struct ArrayInstance
  {
    std::string   cellName;               // name from the following SNAME
    std::uint16_t nCols = 0, nRows = 0;    // from COLROW record
    // The three coordinate pairs that follow the AREF:
    //   (x0,y0) – lower‑left corner of the array
    //   (dx,dy) – column step vector
    //   (dxRow,dyRow) – row step vector
    std::int64_t  x0 = 0, y0 = 0;
    std::int64_t  dx = 0, dy = 0;
    std::int64_t  dxRow = 0, dyRow = 0;

    std::uint16_t strans = 0;             // transformation flags (STRANS)
    double        mag    = 1.0;           // magnification (MAG)
    double        angle  = 0.0;           // rotation angle (ANGLE)

    explicit ArrayInstance(std::string name = "")
      : cellName(std::move(name)) {}
  };
  /// Simple container that represents a GDSII structure (cell)
  struct Cell
  {
    std::string name;               ///< Structure name (STRNAME)
    std::uint64_t offset;           ///< File offset of the BGNSTR record
    std::size_t   index;            ///< Sequential number (0‑based)
    Polygon d_polygon;
    Text    d_text;
    // Containers for later use – you can push objects as you parse them
    std::vector<Instance*> instances;
    std::vector<Polygon*>  polygons;
    std::vector<ArrayInstance*> arrays;
    Cell(std::string n, std::uint64_t off, std::size_t i)
      : name(std::move(n)), offset(off), index(i) {}

    ~Cell()
    {
      // If you allocate Instance/Polygon objects dynamically,
      // delete them here (or use smart pointers instead).
      for (auto *p : instances) delete p;
      for (auto *p : polygons ) delete p;
    }
    Polygon* getPolygon() { return &d_polygon; }
    Text* getText() { return &d_text; }
    void ProcessPolygon();
    // Example helper to print a cell – handy for debugging
    void dump(std::ostream& os = std::cout) const
    {
      os << "Cell[" << index << "] name='" << name
	 << "' offset=0x" << std::hex << offset << std::dec
	 << "  instances=" << instances.size()
	 << "  polygons=" << polygons.size() << '\n';
    }
  };
} // end of VLSILayout namespace

