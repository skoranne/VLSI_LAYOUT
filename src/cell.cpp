////////////////////////////////////////////////////////////////////////////////
// File   : cell.cpp
// Author : Sandeep Koranne
// Purpose: VLSI Layout Region processing experiment
////////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include <iterator>
#include <utility>
#include <algorithm>
#include <cassert>
#include "cell.hpp"

namespace VLSILayout
{
  std::ostream& operator<<( std::ostream& os, const VLSILayout::Polygon::Coordinate& C) {
    os << C.first << " " << C.second << " ";
    return os;
  }
}

void VLSILayout::Cell::ProcessPolygon()
{
  if( d_polygon.points.empty() ) return;  
  if( d_polygon.points.size() < 5 ) {
    assert( d_polygon.width > 0 );
    d_polygon.Clear();
    return; // most likely a TEXT (x,y)
  }
  if( d_polygon.points.front() == d_polygon.points.back() ) {
    d_polygon.Clear();
    return;
  }
  std::for_each( d_polygon.points.begin(), d_polygon.points.end(), []( const Polygon::Coordinate& C) {
    std::cout << C.first << " " << C.second << " ";
  });
  std::cout << std::endl;
  assert( d_polygon.points.front() == d_polygon.points.back() );      
  if( d_polygon.points.size() == 5   ) {
    std::cout << __PRETTY_FUNCTION__ << "\t BOX     "
	      << d_polygon.layer  << "\t"
	      << d_polygon.datatype << std::endl;
  } else {
    std::cout << __PRETTY_FUNCTION__ << "\t POLYGON "
	      << d_polygon.layer  << "\t"
	      << d_polygon.datatype << std::endl;
  }
  d_polygon.Clear();
}
