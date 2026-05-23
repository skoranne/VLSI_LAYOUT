////////////////////////////////////////////////////////////////////////////////
// File    : MagicLayoutFileParser.cpp
// Author  : Sandeep Koranne (C) 1997-onwards. All rights reserved.
// Purpose : Analysis and generation of Magic VLSI layout examples
//
////////////////////////////////////////////////////////////////////////////////

#if 0
magic
tech sky130A
timestamp 1733796373
<< nwell >>
rect -25 65 90 185
<< nmos >>
rect 20 -50 40 25
<< metal1 >>
rect -25 145 90 160
rect -30 -45 102 -30
<< labels >>
rlabel metal1 -30 -45 102 -30 1 VGND
rlabel metal1 -25 145 90 160 1 VPWR
flabel locali s 164 289 198 323 0 FreeSans 340 0 0 0 Y
port 6 nsew signal output
<< properties >>
string FIXED_BBOX 0 0 276 544
string GDS_END 2223360
string GDS_FILE $PDKPATH/libs.ref/sky130_fd_sc_hd/gds/sky130_fd_sc_hd.gds
string GDS_START 2219866
string LEFclass CORE
string LEFsite unithd
string LEFsymmetry X Y R90
string path 0.000 0.000 6.900 0.000 
<< end >>
#endif

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point.hpp>
#include <boost/geometry/geometries/box.hpp>
#include <boost/geometry/index/rtree.hpp>

namespace bg = boost::geometry;
namespace bgi = boost::geometry::index;

// Define Boost.Geometry types for the spatial index.
typedef bg::model::point<long, 2, bg::cs::cartesian> point_type;
typedef bg::model::box<point_type> box_type;
typedef std::pair<box_type, unsigned long> rtree_value;
namespace MagicLayout {

  class CellParseAction;

  class CellParseAction {
    //void InsertCell( bool parseMagicFile(const std::string& filename, bgi::rtree<rtree_value, bgi::rstar<16>>& rtree, std::vector<Shape>& shapes) {

  };

  void ParseGeometry();
  
}; // end of MagicLayout namespace

void MagicLayout::ParseGeometry()
{
    bgi::rtree<rtree_value, bgi::rstar<16>> rtree;
    rtree.insert( std::make_pair<box_type, long>( box_type(), 0 ) );
}

