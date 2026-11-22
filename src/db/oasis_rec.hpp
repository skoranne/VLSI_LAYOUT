#pragma once
// Lossless OASIS (SEMI P39) record scanner.
//
// Unlike oasis.cpp, which converts shapes into boxes, this scanner hands every
// record to a callback as its exact bytes, together with the location of each
// reference field inside them (cell, text-string, prop-name and prop-string
// reference numbers, and cell names given inline in PLACEMENT records).  A
// client can therefore re-emit a record unchanged except for those fields,
// which is what merging files with different name tables needs.  CBLOCKs are
// decompressed and their records delivered in order, as if inline.
//
// With geometry enabled, each shape record also carries its bounding box in
// cell coordinates (repetitions included, as the box of all positions), and
// each PLACEMENT its decoded transform, so a client can compute hierarchical
// extents without expanding anything.

#include "oasis_codec.hpp"
#include <cstdint>
#include <functional>
#include <limits>
#include <string>
#include <vector>

namespace oasis_rec {

using oasis_codec::ParseError;

enum RefKind : uint8_t {
    REF_CELL_NUM,      // cell reference number (CELL 13, PLACEMENT with N bit)
    REF_CELL_STR,      // cell name inline (CELL 14, PLACEMENT without N bit); span is the n-string
    REF_TEXT_NUM,      // TEXT text-string reference number
    REF_PROPNAME_NUM,  // PROPERTY prop-name reference number
    REF_PROPSTR_NUM    // PROPERTY value of type 13..15 (prop-string reference number)
};

struct RefSpan {
    uint32_t off, len;       // byte range inside the record
    RefKind kind;
    uint64_t num = 0;        // *_NUM kinds
    std::string str;         // REF_CELL_STR
};

struct BBox {
    int64_t x1 = std::numeric_limits<int64_t>::max(), y1 = std::numeric_limits<int64_t>::max();
    int64_t x2 = std::numeric_limits<int64_t>::min(), y2 = std::numeric_limits<int64_t>::min();
    bool valid() const { return x1 <= x2 && y1 <= y2; }
    void add(int64_t x, int64_t y) { if (x < x1) x1 = x; if (x > x2) x2 = x; if (y < y1) y1 = y; if (y > y2) y2 = y; }
    void add(const BBox& b) { if (b.valid()) { add(b.x1, b.y1); add(b.x2, b.y2); } }
};

struct Placement {
    bool by_number = false; uint64_t num = 0; std::string name;   // child cell
    double mag = 1.0, angle = 0.0; bool mirror = false;           // mirror about x, then rotate, then scale
    int64_t x = 0, y = 0;
    int64_t rx1 = 0, ry1 = 0, rx2 = 0, ry2 = 0;                   // box of the repetition offsets
};

struct Record {
    uint8_t id = 0;
    const uint8_t* data = nullptr; size_t len = 0;   // the record's bytes, id byte included
    bool in_cell = false;                            // inside a CELL's contents (CELL record itself included)
    bool in_cblock = false;
    std::vector<RefSpan> refs;
    // Name records 3..10 and CELL 13/14: the name/number defined or opened.
    std::string name; bool explicit_ref = false; uint64_t ref = 0;
    // Geometry (only when scanning with geometry): shapes fill `box`; PLACEMENT fills `pl`.
    bool is_shape = false; uint64_t layer = 0, datatype = 0; BBox box;
    bool is_placement = false; Placement pl;
};

struct FileInfo {
    std::string version;
    double unit = 0.0;               // grid steps per micron
    uint64_t offset_flag = 0;
    size_t cblocks = 0, cblock_comp_bytes = 0, cblock_uncomp_bytes = 0;
    size_t records = 0;
};

// Scan an OASIS file image (magic included).  Throws ParseError on any
// malformed record.  `geometry` enables bounding boxes and placement decoding.
void scan(const uint8_t* data, size_t n, bool geometry,
          const std::function<void(const Record&)>& cb, FileInfo& info);

// Apply the transform of a placement (without its repetition) to a box.
BBox transform_box(const BBox& b, const Placement& p);

} // namespace oasis_rec
