#include "layoutdb_internal.hpp"
#include "layoutdb_hier.h"
#include <cmath>
#include <functional>
#include <set>
#include <algorithm>

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
            // expand array/repetition
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
