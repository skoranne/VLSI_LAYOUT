////////////////////////////////////////////////////////////////////////////////
// File  : gds_scanner.hpp
// Author: Sandeep Koranne
//
////////////////////////////////////////////////////////////////////////////////
#pragma once
#include <map>
#include <string>
#include <memory>
#include <iostream>
#include <iomanip>
#include <cassert>
#include "cell.hpp"
#include "utils.hpp"
namespace VLSILayout
{

  /* -------------------------------------------------------------
   *  GDSII record type constants
   *  (the values are taken directly from the GDSII specification)
   * ------------------------------------------------------------- */
  enum GdsRecordType : std::uint8_t
    {
      /* ----- top‑level structure delimiters ----- */
      HEADER   = 0x00,   // file header
      BGNLIB   = 0x01,   // begin library
      LIBNAME  = 0x02,   // library name
      UNITS    = 0x03,   // unit definitions
      ENDLIB   = 0x04,   // end library

      BGNSTR   = 0x05,   // begin structure (cell)
      STRNAME  = 0x06,   // structure name (string)
      ENDSTR   = 0x07,   // end structure

      /* ----- geometry & hierarchy ----- */
      BOUNDARY = 0x08,   // start of a polygon (filled area)
      PATH     = 0x09,   // start of a path (stroke)
      SREF     = 0x0A,   // structure reference (instance)
      AREF     = 0x0B,   // array reference
      TEXT     = 0x0C,   // text element
      LAYER    = 0x0D,   // layer number (used by many elements)
      DATATYPE = 0x0E,   // datatype (used by many elements)
      WIDTH    = 0x0F,   // path width (used by PATH)

      XY       = 0x10,   // list of coordinate pairs (8‑byte signed ints)
      ENDEL    = 0x11,   // end of the current element (SREF, BOUNDARY, PATH …)

      SNAME    = 0x12,   // name of a referenced structure (used after SREF/AREF)
      COLROW   = 0x13,   // column/row count for AREF
      TEXTNODE = 0x14,   // text node
      NODE     = 0x15,
      TEXTTYPE = 0x16,
      PRESENTATION = 0x17, // text presentation attributes
      SPACING  = 0x18,   // text spacing
      STRING   = 0x19,   // generic string (e.g., for TEXT)
      STRANS   = 0x1A,   // structure transformation flags
      MAG      = 0x1B,   // magnification factor
      ANGLE    = 0x1C,   // rotation angle
      UINTEGER = 0x1D,   // not used
      USTRING  = 0x1E,   // not used
      REFLIBS  = 0x1F,   // reference libs
      FONTS    = 0x20,
      PATHTYPE = 0x21,
      GENERATIONS = 0x22,
      ATTRTABLE = 0x23,
      /* ----- other useful records (you may ignore for now) ----- */
      BOX      = 0x2D,   // box (rectangular area)
      NODETYPE = 0x2E,   // node type
      PROPATTR = 0x2F,   // property attribute
      PROPVALUE= 0x30,   // property value
      PROPVALUE2=0x31,   // extended property value (rare)
      ENDPROPS = 0x32,   // end of property list
      LIBDIRSIZE = 0x33, // library directory size (rare)

      /* ----- end of file ----- */
    };
  inline const std::uint8_t* parseCellContent(const MappedFile& mf,
					      Cell* cell,
					      const std::uint8_t* startPtr);
  
  /// Scan a memory‑mapped GDSII file and fill `cells`
  ///
  /// `cells` will be populated with pointers that belong to the caller
  /// (they are allocated with `new` and must be deleted later, or you can
  /// replace the raw pointer with `std::unique_ptr<Cell>` if you prefer).
  inline void scan_gds(const MappedFile& mf,
		       std::map<std::string, Cell*>& cells)
  {
    const std::uint8_t* p   = mf.data;
    const std::uint8_t* end = mf.data + mf.size;

    std::uint64_t curOffset   = 0;          // file offset of the current record
    std::uint64_t bgnStrOffset = 0;        // offset of the last BGNSTR we saw
    std::size_t   cellIndex    = 0;        // sequential number for cells

    while (p + 4 <= end)          // we need at least the 4‑byte header
      {
        // ---- read the header ------------------------------------------------
        std::uint16_t recLen = be16toh(p);          // total length, inc. header
        std::uint8_t  recTyp = p[2];                // record type
        //std::uint8_t  recDat = p[3];                // data type (ignored here)

        if (recLen < 4) {
	  throw std::runtime_error("invalid GDSII record length < 4");
        }
        if (p + recLen > end) {
	  throw std::runtime_error("record overruns end of file");
        }

        // ---- process the record ---------------------------------------------
        switch (recTyp)
	  {
	  case BGNSTR:               // start of a new structure
	    bgnStrOffset = curOffset;   // remember where the structure starts
	    break;

	  case STRNAME:              // structure name follows
            {
	      // The string length is recLen‑4 bytes, padded to an even number.
	      std::size_t strLen = recLen - 4;
	      // GDSII strings are ASCII (often UTF‑8 works fine)
	      std::string name(reinterpret_cast<const char*>(p + 4), strLen);
	      // Trim possible trailing NUL padding
	      while (!name.empty() && name.back() == '\0')
		name.pop_back();

	      // Create the Cell object and store it in the map
	      Cell* cell = new Cell(std::move(name), bgnStrOffset, cellIndex++);
	      cells.emplace(cell->name, cell);
	      const std::uint8_t* afterStrName = p + recLen;
	      p = parseCellContent(mf, cell, afterStrName);
	      continue;
            }

            // other record types are ignored by this simple scanner
	  default:
	    break;
	  }

        // ---- advance ---------------------------------------------------------
        curOffset += recLen;
        p += recLen;
      }

    // If we stopped before reaching the exact end, that usually means we
    // encountered a truncated file – report it (but not fatal for the demo).
    if (p != end) {
      std::cerr << "[warning] GDS scanner stopped at offset "
		<< curOffset << " (expected " << mf.size << ")\n";
    }
  }



  /* ---------------------------------------------------------------------- */
  /*  Inner parser – walks from `startPtr` up to (and including) ENDSTR       */
  inline const std::uint8_t* parseCellContent(const MappedFile& mf,
					      Cell* cell,
					      const std::uint8_t* startPtr)
  {
    const std::uint8_t* p   = startPtr;
    const std::uint8_t* end = mf.data + mf.size;

    // Temporary storage while we are building an element
    Instance*   curInst   = nullptr;
    Polygon*    curPoly   = nullptr;
    Text*       curText   = nullptr;
    ArrayInstance*   curArray  = nullptr;
    bool isPath = false;	    
    while (p + 4 <= end)
      {
        std::uint16_t recLen = be16toh(p);
        std::uint8_t  recTyp = p[2];
        // std::uint8_t recDat = p[3];   // not needed for this demo

        if (recLen < 4 || p + recLen > end)
	  throw std::runtime_error("malformed GDSII record inside cell");

        const std::uint8_t* payload = p + 4;          // data bytes (if any)
        std::size_t dataBytes = recLen - 4;
	bool isBox = false;
	//std::cout << __LINE__ << " " << (p-startPtr) << " " << std::hex << std::setw(2) << (int)recTyp << std::dec << " " << recLen << std::endl;
        switch (recTyp)
	  {
            /* --------------------------------------------------------------
             *  SREF – start of an instance (reference to another cell)
             * ------------------------------------------------------------ */
	  case SREF:          // 0x0A
	    // clean any previous partial instance
	    delete curInst;
	    curInst = nullptr;
	    // the instance data (name, coordinates) will follow in SNAME/XY
	    break;

	  case SNAME:         // 0x12 – name of the cell referenced by the SREF
	    if (!curInst) curInst = new Instance("", 0, 0);
	    curInst->cellName.assign(reinterpret_cast<const char*>(payload),dataBytes);
	    // trim possible NUL padding
	    while (!curInst->cellName.empty() && curInst->cellName.back() == '\0')
	      curInst->cellName.pop_back();
	    if (curArray) {
	      curArray->cellName.assign(reinterpret_cast<const char*>(payload),	dataBytes);
	      while (!curArray->cellName.empty() && curArray->cellName.back() == '\0')
		curArray->cellName.pop_back();
	    }
	    break;
	  case STRANS:               // 0x1A – transformation flags (2‑byte)
	    {
	      std::uint16_t flags = be16toh(payload);
	      if (curInst)   curInst->strans = flags;   // (you may want a flag field in Instance)
	      if (curArray)  curArray->strans = flags;
	    }
	    break;
	    
	  case MAG:                  // 0x1B – magnification (8‑byte double)
	    {
	      double mag = ibm_to_ieee(payload);
	      if (curInst)   curInst->mag = mag;      // (add a `mag` member to Instance if you need it)
	      if (curArray)  curArray->mag = mag;
	    }
	    break;
	    
	  case ANGLE:                // 0x1C – rotation angle (8‑byte double, degrees)
	    {
	      double ang = ibm_to_ieee(payload);
	      if (curInst)   curInst->angle = ang;    // (add an `angle` member to Instance if you need it)
	      if (curArray)  curArray->angle = ang;
	    }
	    break;
	  case BOUNDARY:      // 0x08 – start of a polygon
	    curPoly = cell->getPolygon();   // layer will be filled later
	    curText = nullptr;
	    break;
	  case PATH:
	    isPath = true;
	    curPoly = cell->getPolygon();
	    break;
	  case WIDTH:
	    {
	      assert( isPath );
	      isPath = false;
	      assert( curPoly );
	      int width = read_be32(const_cast<const std::uint8_t*>( payload ) );
	      curPoly->width = (width << 2);
	      //std::cout << "Assigning a WIDTH of " << width << " to PATH." << std::endl;
	    }
	    break;
	  case BOX:       //
	    assert( false );
	    isBox = true;
	    curPoly = cell->getPolygon();   // layer will be filled later
	    break;
	  case TEXT:
	    curText = cell->getText();
	    curPoly = nullptr;
	    break;
	  case LAYER:         // 0x0D – layer number for the polygon or text
	    assert( curPoly || curText );
	    assert( ( curPoly == nullptr ) || ( curText == nullptr ) );
	    if( curPoly ) {
	      //std::cout << "Assigning to POLY layer: " << (int)payload[0] << "\t" << (int)payload[1] << std::endl;
	      curPoly->layer = static_cast<std::uint16_t>(payload[0] << 8 | payload[1]);
	    } else {
	      //std::cout << "Assigning to TEXT layer: " << (int)payload[0] << "\t" << (int)payload[1] << std::endl;
	      curText->layer = static_cast<std::uint16_t>(payload[0] << 8 | payload[1]);
	    }
	    break;
	  case DATATYPE:         // 0x0E – datatype number for the polygon
	    assert( curPoly );
	    //std::cout << "Assigning to POLY datatype: " << (int)payload[0] << "\t" << (int)payload[1] << std::endl;
	    curPoly->datatype = static_cast<std::uint16_t>(payload[0] << 8 | payload[1]);
	    break;
	  case TEXTTYPE:         // 0x16 – datatype number for the polygon
	    assert( curText );
	    //std::cout << "Assigning to TEXT texttype: " << (int)payload[0] << "\t" << (int)payload[1] << std::endl;
	    curText->texttype = static_cast<std::uint16_t>(payload[0] << 8 | payload[1]);
	    break;	    
	  case XY:            // 0x10 – list of coordinate pairs
	    // XY may appear after SREF (instance location) or after
	    // BOUNDARY (polygon vertices).  We treat both cases.
	    if( isBox ) {
	      std::size_t off = 0;
	      for( int xi=0; xi < 5; ++xi ) {
		off += 8;
		std::int64_t x = read_be32(const_cast<const std::uint8_t*>( payload + off) );
		std::int64_t y = read_be32(const_cast<const std::uint8_t*>(payload + off + 4) );
		//std::cout << __LINE__ << " X " << x << " " << y << std::endl;
		curPoly->points.emplace_back(x, y);		
	      }
	    } else {
	      for (std::size_t off = 0; off + 8 <= dataBytes; off += 8)
		{
		  std::int64_t x = read_be32(const_cast<const std::uint8_t*>( payload + off) );
		  std::int64_t y = read_be32(const_cast<const std::uint8_t*>(payload + off + 4) );
		  if (curInst && !curPoly && !curArray && !curText) {
		    // this XY belongs to an SREF (instance location)
		    curInst->x = x;
		    curInst->y = y;
		  }
		  else if (curPoly && !curArray && !curText) {
		    //std::cout << "Reading X = " << x << " Y = " << y << std::endl;		    
		    curPoly->points.emplace_back(x, y);
		  } else if (curArray && !curText) {
		    // The three pairs appear consecutively; we just assign them
		    // in the order they are encountered.
		    static int pairIndex = 0;   // reset for each new AREF
		    switch (pairIndex)
		      {
		      case 0: curArray->x0 = x; curArray->y0 = y; break;
		      case 1: curArray->dx = x; curArray->dy = y; break;
		      case 2: curArray->dxRow = x; curArray->dyRow = y; break;
		      }
		    ++pairIndex;
		    // When we have consumed the third pair we reset the counter
		    // for the next possible AREF (the next ENDEL will also reset it).
		    if (pairIndex == 3) pairIndex = 0;
		  }
		  else if( curText ) {
		    curText->x = x;
		    curText->y = y;
		  }
		  else {
		    assert( false && "Dont know how we can get here. UNSUPPORTED yet.");
		  }
		}
	    } // no box
	    break;
	  
	  case ENDEL:         // 0x11 – end of the current element
	    if (curInst) {
	      cell->instances.push_back(curInst);
	      curInst = nullptr;
	    }
	    if (curPoly) {
	      cell->ProcessPolygon();
	      curPoly = nullptr;
	    }
	    if (curArray) {
	      cell->arrays.push_back(curArray);
	      curArray = nullptr;
	    }
	    if (curText ) {
	      curText = nullptr;
	    }
	    break;

	  case ENDSTR:        // 0x07 – finished this cell
	    // Clean up any half‑finished element (should not happen in a
	    // well‑formed file, but be defensive).
	    delete curInst; curInst = nullptr;
	    delete curPoly; curPoly = nullptr;
	    return p + recLen;   // return pointer *after* ENDSTR

	  default:
	    // All other record types are ignored for this simple demo.
	    break;
	  }

        p += recLen;   // advance to next record inside the cell
      }

    // If we fall out of the loop we never saw ENDSTR – the file is broken.
    throw std::runtime_error("reached EOF before ENDSTR");
  }

} // end of VLSILayout namespace

