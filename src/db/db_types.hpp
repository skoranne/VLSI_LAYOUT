#pragma once
// GPU-friendly, memory-optimized POD/SoA-oriented layout DB structures.
// Designed for mmap-backed zero-copy where possible and explicit device transfer.

#include <cstdint>
#include <cstddef>
#include <vector>
#include <string>
#include <unordered_map>
#include <map>

namespace db {

// Compact 2D point/box in database units (integer, OASIS/GDSII native)
struct Point { int64_t x; int64_t y; };
struct BBox  { int64_t xmin,ymin,xmax,ymax; uint8_t valid; };

// Element kinds (unified)
enum class ElemKind : uint8_t {
    Boundary=0, Path, SRef, ARef, Box, Node, Text,
    ORectangle, OPolygon, OPath, OTrapezoid, OCTrapezoid, OCircle, OXElement, OXGeometry
};

// Compact stored element (CPU). Variable payloads stored in pools to avoid per-element allocs.
struct Elem {
    ElemKind kind;
    uint16_t layer;
    uint16_t datatype; // or texttype
    uint32_t prop_off; uint32_t prop_count; // into property pool
    uint32_t data_off; uint32_t data_count; // into int64/point pool depending on kind
    int32_t ref_cell;   // for refs, index into cell table (-1 if none)
    // transform (for refs)
    int64_t ox, oy;
    int32_t mag_num, mag_den; // magnification rational
    int32_t ang_num, ang_den; // angle rational (degrees units) or fixed-point
    uint8_t xform_flags; // reflect etc
};

// Property (compact)
struct Prop { int32_t name_id; int32_t value_id; uint8_t value_type; };

// Cell in SoA-friendly form
struct Cell {
    std::string name;
    std::vector<Elem> elems; // could be split to SoA later
    BBox bbox{0,0,0,0,0};
    std::vector<int32_t> child_refs; // cell indices
};

// Name tables (string interning)
struct StringTable {
    std::vector<std::string> v;
    std::unordered_map<std::string,int32_t> m;
    int32_t intern(const std::string& s){ auto it=m.find(s); if(it!=m.end()) return it->second; int32_t id=(int32_t)v.size(); v.push_back(s); m[s]=id; return id; }
    const char* get(int32_t id) const { return (id>=0 && (size_t)id<v.size())? v[id].c_str() : ""; }
};

// Raw record for lossless round-trip (GDSII always; OASIS as fallback/supplement)
struct RawRec {
    uint32_t offset; // in source buffer
    uint32_t length;
    uint16_t rtype;
    uint8_t dtype; uint8_t flags;
};

// OASIS CBLOCK descriptor for nvComp
struct CBlock {
    uint64_t src_offset; // compressed data offset in file
    uint64_t comp_size;
    uint64_t uncomp_size;
    uint8_t comp_type; // OASIS: 0 = DEFLATE (RFC 1951, raw); other values undefined by the spec
    std::vector<uint8_t> host_out; // decompressed bytes (pinned/GPU-friendly alloc ideal)
    void* dev_in=nullptr; void* dev_out=nullptr; // device buffers if nvcomp used
};

struct Database {
    int format=0; // 1 GDSII, 2 OASIS
    // mmap source
    const uint8_t* src=nullptr; size_t src_size=0; int fd=-1; void* map=nullptr;
    // lossless streams
    std::vector<RawRec> raw_recs; // GDSII records / OASIS raw segments
    std::vector<uint8_t> oasis_raw; // full raw for exact round-trip
    // parsed model
    std::vector<Cell> cells;
    std::unordered_map<std::string,int32_t> cell_map;
    StringTable strings;
    StringTable prop_names;
    StringTable prop_strings;
    std::vector<Prop> props;
    std::vector<int64_t> int_pool; // xy/coords/repetition params
    std::vector<CBlock> cblocks;
    // stats
    double cpu_parse_s=0, cpu_write_s=0;
    size_t mem_bytes=0;
    size_t gpu_bytes_alloc=0, gpu_bytes_used=0;
    bool used_gpu=false;
};

} // namespace db
