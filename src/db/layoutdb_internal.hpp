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

struct LdbCell {
    std::string name;
    std::map<std::pair<int,int>, ldb_extent_t> layer_ext;
    ldb_extent_t cell_ext{0,0,0,0,0};
    std::set<std::string> refs;
    std::vector<LdbBoxElem> boxes; // element bboxes for loading
};

struct LdbFile {
    int format = 0;
    void* map = nullptr;
    size_t size = 0;
    int fd = -1;
    std::vector<LdbCell> cells;
    std::unordered_map<std::string, size_t> cell_index;
    std::map<int, std::string> propnames;
    std::vector<LdbRawRecord> gds_records; // lossless GDSII stream
    std::vector<uint8_t> oasis_raw; // captured/constructed for roundtrip experiments
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
