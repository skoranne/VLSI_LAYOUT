#include "layoutdb_internal.hpp"
#include "layoutdb_hier.h"
#include <cmath>
#include <functional>
#include <set>
#include <algorithm>
#include <unordered_map>

static void transform_point(const LdbTransform& t, int64_t ix, int64_t iy, int64_t& ox, int64_t& oy){
    double x = (double)ix * t.mag;
    double y = (double)iy * t.mag;
    if(t.mirror_x) y = -y;
    double a = t.angle_deg * M_PI / 180.0;
    double ca = cos(a), sa = sin(a);
    // rotate
    double rx = x*ca - y*sa;
    double ry = x*sa + y*ca;
    ox = t.ox + (int64_t)llround(rx);
    oy = t.oy + (int64_t)llround(ry);
}

static void transform_bbox(const LdbTransform& t, int64_t x1,int64_t y1,int64_t x2,int64_t y2,
                           int64_t& ox1,int64_t& oy1,int64_t& ox2,int64_t& oy2){
    int64_t px[4], py[4];
    transform_point(t, x1,y1, px[0],py[0]);
    transform_point(t, x2,y1, px[1],py[1]);
    transform_point(t, x2,y2, px[2],py[2]);
    transform_point(t, x1,y2, px[3],py[3]);
    ox1=ox2=px[0]; oy1=oy2=py[0];
    for(int i=1;i<4;i++){ if(px[i]<ox1)ox1=px[i]; if(px[i]>ox2)ox2=px[i]; if(py[i]<oy1)oy1=py[i]; if(py[i]>oy2)oy2=py[i]; }
}

static void compose_transform(const LdbTransform& parent, const LdbTransform& inst, LdbTransform& out){
    // apply inst in parent context: for non-orthogonal/complex cases approximate by transforming origin and accumulating
    // For general case, we handle translation+mag+mirror+angle by transforming the instance origin and adding angles/mags.
    // Array expansion is handled separately.
    out = inst;
    // transform inst origin by parent
    int64_t tox, toy;
    transform_point(parent, inst.ox, inst.oy, tox, toy);
    out.ox = tox; out.oy = toy;
    out.mag = parent.mag * inst.mag;
    out.angle_deg = parent.angle_deg + inst.angle_deg;
    out.mirror_x = parent.mirror_x ^ inst.mirror_x; // simplified composition
    out.cols = 1; out.rows = 1; out.dx=out.dy=out.row_dx=out.row_dy=0;
}

// Compute bbox of an axis-aligned repetition array given one element's bbox and repetition params.
// For general rotation/mirror this is approximate via corners; for orthogonal (common) it's exact.
static void repetition_array_bbox(const LdbTransform& rep, int64_t x1,int64_t y1,int64_t x2,int64_t y2,
                                  int64_t& ax1,int64_t& ay1,int64_t& ax2,int64_t& ay2){
    int64_t cols = rep.cols < 1 ? 1 : rep.cols;
    int64_t rows = rep.rows < 1 ? 1 : rep.rows;
    if(cols==1 && rows==1){ ax1=x1; ay1=y1; ax2=x2; ay2=y2; return; }
    // compute bbox over all 4 corners of the array grid of element bboxes
    ax1=ax2=0; ay1=ay2=0; bool first=true;
    int64_t lastc = cols-1, lastr = rows-1;
    auto add_pt=[&](int64_t px,int64_t py){
        if(first){ ax1=ax2=px; ay1=ay2=py; first=false; }
        else { if(px<ax1)ax1=px; if(px>ax2)ax2=px; if(py<ay1)ay1=py; if(py>ay2)ay2=py; }
    };
    // corners of element bbox translated by array offsets at (0,0),(lastc,0),(0,lastr),(lastc,lastr)
    int64_t corners_c[2] = {0, lastc};
    int64_t corners_r[2] = {0, lastr};
    for(int ir=0; ir<2; ++ir){
        for(int ic=0; ic<2; ++ic){
            int64_t c=corners_c[ic], r=corners_r[ir];
            int64_t ox = c*rep.dx + r*rep.row_dx;
            int64_t oy = c*rep.dy + r*rep.row_dy;
            add_pt(x1+ox, y1+oy); add_pt(x2+ox, y1+oy);
            add_pt(x2+ox, y2+oy); add_pt(x1+ox, y2+oy);
        }
    }
}

// Repetition-aware hierarchical extent with memoization: computes flattened bbox of a cell once.
static bool cell_extent_flat(LdbFile* f, const std::string& cname,
                             std::unordered_map<std::string, ldb_extent_t>& memo,
                             std::set<std::string>& visiting,
                             ldb_extent_t& out){
    auto mit = memo.find(cname);
    if(mit != memo.end()){ out = mit->second; return out.valid; }
    auto it = f->cell_index.find(cname);
    if(it == f->cell_index.end()){ out.valid=0; memo[cname]=out; return false; }
    // cycle guard
    if(visiting.count(cname)){ out.valid=0; return false; }
    visiting.insert(cname);
    auto& cell = f->cells[it->second];
    ldb_extent_t ext{0,0,0,0,0};
    // local geometry bbox
    for(auto& b : cell.boxes){
        ldb_extent_add_point(ext, b.x1,b.y1); ldb_extent_add_point(ext, b.x2,b.y2);
    }
    // instances
    for(auto& inst : cell.instances){
        ldb_extent_t child_ext{0,0,0,0,0};
        if(!cell_extent_flat(f, inst.ref_name, memo, visiting, child_ext)) continue;
        // transform child bbox by instance transform (single)
        LdbTransform tr = inst.tr; tr.cols=1; tr.rows=1;
        int64_t x1,y1,x2,y2;
        transform_bbox(tr, child_ext.xmin, child_ext.ymin, child_ext.xmax, child_ext.ymax, x1,y1,x2,y2);
        // apply repetition array analytically (compressed/repeated layout)
        int64_t ax1,ay1,ax2,ay2;
        repetition_array_bbox(inst.tr, x1,y1,x2,y2, ax1,ay1,ax2,ay2);
        ldb_extent_t aext{ax1,ay1,ax2,ay2,1};
        ldb_extent_merge(ext, aext);
    }
    visiting.erase(cname);
    out = ext; memo[cname]=ext;
    return ext.valid;
}

// For hierarchical iteration, emit boxes. Use repetition expansion but avoid per-call overhead where possible.
static void hier_iter_cell(LdbFile* f, const std::string& cname, const LdbTransform& cur_tr,
                           int depth, int max_depth, ldb_box_cb cb, void* ud,
                           std::set<std::string>& visiting){
    auto it = f->cell_index.find(cname);
    if(it == f->cell_index.end()) return;
    if(visiting.count(cname)) return; // cycle guard
    visiting.insert(cname);
    auto& cell = f->cells[it->second];
    // emit local geometry transformed
    for(auto& b : cell.boxes){
        int64_t x1,y1,x2,y2;
        transform_bbox(cur_tr, b.x1,b.y1,b.x2,b.y2, x1,y1,x2,y2);
        cb(ud, cname.c_str(), b.layer, b.datatype, b.kind, x1,y1,x2,y2);
    }
    // recurse into instances if depth allows
    if(max_depth < 0 || depth < max_depth){
        for(auto& inst : cell.instances){
            // expand array/repetition (OASIS repetition-aware)
            int64_t cols = inst.tr.cols < 1 ? 1 : inst.tr.cols;
            int64_t rows = inst.tr.rows < 1 ? 1 : inst.tr.rows;
            for(int64_t r=0; r<rows; ++r){
                for(int64_t c=0; c<cols; ++c){
                    LdbTransform tr = inst.tr;
                    tr.cols = 1; tr.rows = 1;
                    tr.ox = inst.tr.ox + c*inst.tr.dx + r*inst.tr.row_dx;
                    tr.oy = inst.tr.oy + c*inst.tr.dy + r*inst.tr.row_dy;
                    LdbTransform child;
                    compose_transform(cur_tr, tr, child);
                    hier_iter_cell(f, inst.ref_name, child, depth+1, max_depth, cb, ud, visiting);
                }
            }
        }
    }
    visiting.erase(cname);
}

extern "C" int ldb_iterate_boxes_hier(ldb_file_t* fh, const char* top_cell, int max_depth, ldb_box_cb cb, void* ud){
    auto* f=(LdbFile*)fh; if(!f||!cb) return -1;
    std::string top = (top_cell && top_cell[0]) ? std::string(top_cell) : std::string();
    LdbTransform idtr{};
    std::set<std::string> visiting;
    if(!top.empty()){
        hier_iter_cell(f, top, idtr, 0, max_depth, cb, ud, visiting);
        return 0;
    }
    // auto-detect tops: indegree 0
    std::map<std::string,int> indeg;
    for(auto& c: f->cells){ indeg[c.name]; for(auto& r: c.refs) indeg[r]++; for(auto& ins: c.instances) indeg[ins.ref_name]++; }
    bool any=false;
    for(auto& c: f->cells){
        if(indeg[c.name]==0){ any=true; visiting.clear(); hier_iter_cell(f, c.name, idtr, 0, max_depth, cb, ud, visiting); }
    }
    if(!any){ for(auto& c: f->cells){ visiting.clear(); hier_iter_cell(f, c.name, idtr, 0, max_depth, cb, ud, visiting); } }
    return 0;
}

struct ExtHierCtx { ldb_extent_t ext{0,0,0,0,0}; };
static void ext_hier_cb(void* ud, const char*, int, int, int, int64_t x1,int64_t y1,int64_t x2,int64_t y2){
    auto* c=(ExtHierCtx*)ud;
    ldb_extent_add_point(c->ext,x1,y1); ldb_extent_add_point(c->ext,x2,y2);
}

extern "C" int ldb_extent_whole_hier(ldb_file_t* fh, const char* top_cell, ldb_extent_t* out){
    if(!fh||!out) return -1;
    auto* f=(LdbFile*)fh;
    std::string top = (top_cell && top_cell[0]) ? std::string(top_cell) : std::string();
    std::unordered_map<std::string, ldb_extent_t> memo;
    memo.reserve(f->cells.size()*2);
    ldb_extent_t result{0,0,0,0,0};
    auto compute_one=[&](const std::string& cname){
        std::set<std::string> visiting;
        ldb_extent_t e{0,0,0,0,0};
        if(cell_extent_flat(f, cname, memo, visiting, e)) ldb_extent_merge(result, e);
    };
    if(!top.empty()){
        compute_one(top);
    } else {
        std::map<std::string,int> indeg;
        for(auto& c: f->cells){ indeg[c.name]; for(auto& r: c.refs) indeg[r]++; for(auto& ins: c.instances) indeg[ins.ref_name]++; }
        bool any=false;
        for(auto& c: f->cells){ if(indeg[c.name]==0){ any=true; compute_one(c.name); } }
        if(!any){ for(auto& c: f->cells) compute_one(c.name); }
    }
    if(result.valid){ *out = result; return 0; }
    // fallback to box iteration if memoized extent unavailable
    ExtHierCtx ctx;
    if(ldb_iterate_boxes_hier(fh, top_cell, -1, ext_hier_cb, &ctx) != 0) return -1;
    *out = ctx.ext; return ctx.ext.valid?0:1;
}

extern "C" int ldb_top_cell_count(ldb_file_t* fh){
    auto* f=(LdbFile*)fh; if(!f) return 0;
    std::map<std::string,int> indeg;
    for(auto& c: f->cells){ indeg[c.name]; for(auto& r: c.refs) indeg[r]++; for(auto& ins: c.instances) indeg[ins.ref_name]++; }
    int n=0; for(auto& c: f->cells) if(indeg[c.name]==0) n++;
    return n==0 ? (int)f->cells.size() : n;
}

extern "C" const char* ldb_top_cell_name(ldb_file_t* fh, int i){
    auto* f=(LdbFile*)fh; if(!f) return nullptr;
    std::map<std::string,int> indeg;
    for(auto& c: f->cells){ indeg[c.name]; for(auto& r: c.refs) indeg[r]++; for(auto& ins: c.instances) indeg[ins.ref_name]++; }
    int n=0;
    for(auto& c: f->cells){ if(indeg[c.name]==0){ if(n==i) return c.name.c_str(); n++; } }
    if(n==0 && i>=0 && i<(int)f->cells.size()) return f->cells[i].name.c_str();
    return nullptr;
}
