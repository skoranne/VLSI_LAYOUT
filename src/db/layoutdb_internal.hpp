#pragma once
#include "layoutdb.h"
#include <string>
#include <vector>
#include <unordered_map>
#include <map>
#include <set>
#include <cstdio>
#include <algorithm>
#include <limits>
#include <cstdint>

struct LdbRawRecord { std::vector<uint8_t> bytes; uint8_t rtype=0, dtype=0; };

struct LdbBoxElem {
    int layer=0, datatype=0, kind=0;
    int64_t x1=0,y1=0,x2=0,y2=0;
};

struct LdbTransform {
    int64_t ox=0, oy=0;      // origin/translation
    double mag=1.0;
    double angle_deg=0.0;
    int mirror_x=0;          // reflect about x axis before rotation
    int64_t cols=1, rows=1;  // AREF/repetition array counts
    int64_t dx=0, dy=0;      // step vector for columns (col spacing in x,y)
    int64_t row_dx=0, row_dy=0; // step vector for rows
};

struct LdbInstance {
    std::string ref_name;
    LdbTransform tr;
};

struct LdbProperty {
    int target_kind = 0; // 0=file,1=cell,2=element,3=instance
    std::string cell;
    std::string name;
    std::string value;
};

struct LdbCell {
    std::string name;
    std::map<std::pair<int,int>, ldb_extent_t> layer_ext;
    ldb_extent_t cell_ext{0,0,0,0,0};
    std::set<std::string> refs;
    std::vector<LdbBoxElem> boxes; // element bboxes (local), may be cleared after compaction
    std::vector<uint8_t> boxes_compact; // delta+varint entropy-encoded box stream
    std::string boxes_spill_path;       // low-memory mode: disk-backed compact box storage
    bool boxes_on_disk = false;
    std::vector<LdbInstance> instances; // hierarchical placements (repeated layout preserved)
    std::vector<LdbProperty> properties;
};

struct LdbFile {
    int format = 0;
    void* map = nullptr;
    size_t size = 0;
    int fd = -1;
    bool strict_mode = false;   // OASIS START strict flag or strict parsing requested
    uint32_t open_flags = 0;
    uint64_t large_cell_box_threshold = 0;
    std::vector<LdbCell> cells;
    std::unordered_map<std::string, size_t> cell_index;
    std::map<int, std::string> propnames;
    std::map<int, std::string> propstrings;
    std::vector<LdbRawRecord> gds_records; // lossless GDSII stream
    std::vector<uint8_t> oasis_raw; // captured/constructed for roundtrip experiments
    std::vector<LdbProperty> file_properties;
    std::string error;
    size_t mem_bytes_extra = 0;
};

static inline void ldb_extent_add_point(ldb_extent_t& e, int64_t x, int64_t y) {
    if (!e.valid) { e.xmin=e.xmax=x; e.ymin=e.ymax=y; e.valid=1; }
    else { if(x<e.xmin)e.xmin=x; if(x>e.xmax)e.xmax=x; if(y<e.ymin)e.ymin=y; if(y>e.ymax)e.ymax=y; }
}
static inline void ldb_extent_merge(ldb_extent_t& dst, const ldb_extent_t& src) {
    if (!src.valid) return;
    if (!dst.valid) { dst = src; return; }
    dst.xmin = std::min(dst.xmin, src.xmin); dst.ymin = std::min(dst.ymin, src.ymin);
    dst.xmax = std::max(dst.xmax, src.xmax); dst.ymax = std::max(dst.ymax, src.ymax);
}
