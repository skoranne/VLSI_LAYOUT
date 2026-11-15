#pragma once
#include <cstdint>
#include <vector>
#include <string>
#include <variant>
#include <map>

namespace oasis {

struct VarInt { uint64_t u; int64_t s; };
struct Point { int64_t x,y; };

enum RecordID : uint8_t {
    PAD=0, START=1, END=2, CELLNAME=3, CELLNAME_REF=4, TEXTSTRING=5, PROPNAME=6, PROPSTRING=7,
    PROPNAME_REF=8, PROPSTRING_REF=9, CELL=13, CELL_REF=14, XYABSOLUTE=15, XYRELATIVE=16,
    PLACEMENT=17, PLACEMENT_TRANSFORM=18, TEXT=19, RECTANGLE=20, POLYGON=21, PATH=22,
    TRAPEZOID=23, CTRAPEZOID=24, CIRCLE=25, PROPERTY=28, XNAME=29, XELEMENT=30, XGEOMETRY=31, CBLOCK=34
};

struct Record {
    uint8_t id=0;
    size_t src_off=0;
    size_t src_len=0; // length in source/compressed stream
    std::vector<uint8_t> raw; // exact bytes of this record (for lossless)
    bool in_cblock=false;
};

// Full in-memory OASIS document: preserves all records in order (including from decompressed CBLOCKs)
// and retains original file bytes for lossless round-trip.
struct Document {
    std::vector<uint8_t> original; // mmap copy / original bytes
    std::vector<Record> records;   // all parsed records in logical order
    // tables
    std::vector<std::string> cellname_table;
    std::vector<std::string> textstring_table;
    std::vector<std::string> propname_table;
    std::vector<std::string> propstring_table;
    std::vector<std::string> xname_table;
    // stats
    size_t cblock_count=0;
    size_t cblock_comp_bytes=0;
    size_t cblock_uncomp_bytes=0;
    bool used_gpu_decompress=false;
    std::string decompress_info;
};

} // namespace oasis
