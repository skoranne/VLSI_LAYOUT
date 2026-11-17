#include "layoutdb_internal.hpp"
#include "oasis_ir.hpp"
#include "nvcomp_support.hpp"
#include "compact_boxes.hpp"
#include <cstring>
#include <cstdio>
#include <functional>
#include <algorithm>

// Comprehensive OASIS parser: captures ALL records in IR (logical order, including decompressed CBLOCK contents),
// decompresses CBLOCKs (nvComp GPU if available, else zlib CPU), preserves original bytes for lossless round-trip,
// and extracts semantic info (cells, hierarchy refs, prop names, geometry extents) best-effort.

namespace {

struct Stream {
    const uint8_t* p; size_t n; size_t off; bool in_cblock;
    bool ok=true; std::string err;
    Stream(const uint8_t* p_, size_t n_, bool cb=false):p(p_),n(n_),off(0),in_cblock(cb){}
    bool need(size_t k){ if(off+k>n){ ok=false; if(err.empty()) err="truncated"; return false; } return true; }
    uint8_t u8(){ if(!need(1)) return 0; return p[off++]; }
    uint64_t uvar(){ uint64_t v=0; int sh=0; for(int i=0;i<10;i++){ if(!need(1)) return 0; uint8_t b=p[off++]; v |= uint64_t(b&0x7f)<<sh; if(!(b&0x80)) return v; sh+=7; } ok=false; err="bad varint"; return 0; }
    int64_t svar(){ uint64_t u=uvar(); return (u&1)? -(int64_t)(u>>1)-1 : (int64_t)(u>>1); }
    std::string astr(){ uint64_t l=uvar(); if(!need((size_t)l)) return {}; std::string s((const char*)p+off,(size_t)l); off+=(size_t)l; return s; }
    void skip(size_t k){ if(need(k)) off+=k; }
    // OASIS real: type in high bits/byte; simplified parser
    double real(){ if(!need(1)) return 0; uint8_t t=p[off++];
        if(t==0){ return (double)uvar(); }
        if(t==1){ return -(double)uvar(); }
        if(t==2){ double m=(double)uvar(); int e=(int)(int64_t)svar(); return m; /* scale by 10^e simplified */ }
        if(t==3){ double m=(double)uvar(); int e=(int)(int64_t)svar(); return -m; }
        if(t==4){ double m=(double)uvar(); int e=(int)(int64_t)svar(); return m; }
        if(t==5){ double m=(double)uvar(); int e=(int)(int64_t)svar(); return -m; }
        if(t==6){ uint64_t n=uvar(), d=uvar(); return d?(double)n/(double)d:0; }
        if(t==7){ uint64_t n=uvar(), d=uvar(); return d?-(double)n/(double)d:0; }
        // float/double stored as IEEE in some implementations? fallback read 4/8
        if(t==0x08){ if(!need(4)) return 0; float f; memcpy(&f,p+off,4); off+=4; return f; }
        if(t==0x09){ if(!need(8)) return 0; double d; memcpy(&d,p+off,8); off+=8; return d; }
        // unknown: try consume as unsigned
        uvar(); return 0;
    }
};

struct Modal {
    bool abs_xy=true;
    uint64_t layer=0, datatype=0, textlayer=0, texttype=0;
    int64_t x=0,y=0;
    int64_t geom_x=0, geom_y=0; // for geometry
    uint64_t placement_cell=0;
    uint64_t text_string=0;
    int64_t half_width=0;
    int64_t ctrap_type=0;
    uint64_t circle_radius=0;
    uint64_t prop_name=0, prop_value=0;
    double path_ext_start=0, path_ext_end=0;
};

struct Ctx {
    LdbFile* f;
    oasis::Document doc;
    std::vector<std::string> cellnames, textstrings, propnames, propstrings, xnames;
    std::map<size_t, size_t> geom_idx; // cell ref -> f->cells index
    size_t cur_cell = (size_t)-1;
    Modal m;
    db::NvcompStatus nvst;
};

static void ensure_table(std::vector<std::string>& t, uint64_t idx, const std::string& v=""){ if(t.size()<=idx) t.resize(idx+1); if(!v.empty()) t[idx]=v; }
static std::string tget(const std::vector<std::string>& t, uint64_t idx){ return (idx<t.size())?t[idx]:std::string(); }

static void rec_raw(Stream& s, size_t start, Ctx& ctx){
    oasis::Record r; r.id = s.p[start]; r.src_off=start; r.src_len=s.off-start; r.in_cblock=s.in_cblock;
    r.raw.assign(s.p+start, s.p+s.off);
    ctx.doc.records.push_back(std::move(r));
}

static void parse_point_list(Stream& s, uint8_t type, std::vector<std::pair<int64_t,int64_t>>& out, int64_t& cx, int64_t& cy){
    // OASIS point-list formats 0-5
    // type bits in info; we receive the point-list type value
    if(type==0){ // Manhattan horizontal/vertical
        uint64_t count = s.uvar();
        for(uint64_t i=0;i<count && s.ok;i++){ int64_t d=s.svar(); if(i%2==0) cx+=d; else cy+=d; out.push_back({cx,cy}); }
    } else if(type==1){ // Manhattan with first dir specified
        uint64_t dir=s.uvar(); uint64_t count=s.uvar();
        for(uint64_t i=0;i<count && s.ok;i++){ int64_t d=s.svar(); if((i+dir)%2==0) cx+=d; else cy+=d; out.push_back({cx,cy}); }
    } else if(type==2){ // Octangular
        uint64_t count=s.uvar();
        for(uint64_t i=0;i<count && s.ok;i++){ int64_t d=s.svar(); /* dir encoded in d? simplified: treat as delta-x */ cx+=d; int64_t d2=s.svar(); cy+=d2; out.push_back({cx,cy}); }
    } else if(type==3){ // All-delta
        uint64_t count=s.uvar();
        for(uint64_t i=0;i<count && s.ok;i++){ int64_t dx=s.svar(), dy=s.svar(); cx+=dx; cy+=dy; out.push_back({cx,cy}); }
    } else if(type==4){ // Double-delta
        uint64_t count=s.uvar();
        for(uint64_t i=0;i<count && s.ok;i++){ int64_t dx=s.svar(), dy=s.svar(); cx+=dx; cy+=dy; out.push_back({cx,cy}); }
    } else { // 5 or other: general
        uint64_t count=s.uvar();
        for(uint64_t i=0;i<count && s.ok;i++){ int64_t dx=s.svar(), dy=s.svar(); cx+=dx; cy+=dy; out.push_back({cx,cy}); }
    }
}

struct RepInfo {
    int64_t cols = 1, rows = 1;
    int64_t dx = 0, dy = 0;
    int64_t row_dx = 0, row_dy = 0;
    bool reuse = false;
};

static RepInfo parse_repetition(Stream& s, uint8_t rep_type){
    RepInfo r;
    // OASIS repetition: 0=reuse previous, 1=uniform x*y, 2=uniform x, 3=uniform y,
    // 4=non-uniform x with uniform y? 5=uniform x with non-uniform y? 6=non-uniform both? 7,8=general
    // We parse common uniform grid forms fully and consume others for alignment.
    switch(rep_type){
        case 0:
            r.reuse = true;
            break;
        case 1: { // rectangular matrix: xdim, ydim, xspace, yspace
            uint64_t xdim = s.uvar() + 2;
            uint64_t ydim = s.uvar() + 2;
            uint64_t xsp  = s.uvar();
            uint64_t ysp  = s.uvar();
            r.cols = (int64_t)xdim; r.rows = (int64_t)ydim;
            r.dx = (int64_t)xsp; r.dy = 0;
            r.row_dx = 0; r.row_dy = (int64_t)ysp;
        } break;
        case 2: { // uniform x: xdim, xspace
            uint64_t xdim = s.uvar() + 2;
            uint64_t xsp  = s.uvar();
            r.cols = (int64_t)xdim; r.rows = 1;
            r.dx = (int64_t)xsp; r.dy = 0;
        } break;
        case 3: { // uniform y: ydim, yspace
            uint64_t ydim = s.uvar() + 2;
            uint64_t ysp  = s.uvar();
            r.cols = 1; r.rows = (int64_t)ydim;
            r.row_dx = 0; r.row_dy = (int64_t)ysp;
        } break;
        case 4: { // x-dimension with explicit x displacements, y-dimension with uniform y space
            uint64_t xdim = s.uvar() + 1;
            for(uint64_t i=0;i<xdim && s.ok;i++) s.uvar();
            uint64_t ydim = s.uvar() + 2;
            uint64_t ysp  = s.uvar();
            r.cols = (int64_t)xdim; r.rows = (int64_t)ydim;
            r.row_dx = 0; r.row_dy = (int64_t)ysp;
            // approximate dx from first displacement not stored; keep 0
        } break;
        case 5: { // uniform x with explicit y displacements
            uint64_t xdim = s.uvar() + 2;
            uint64_t xsp  = s.uvar();
            uint64_t ydim = s.uvar() + 1;
            for(uint64_t i=0;i<ydim && s.ok;i++) s.uvar();
            r.cols = (int64_t)xdim; r.rows = (int64_t)ydim;
            r.dx = (int64_t)xsp; r.dy = 0;
        } break;
        case 6: { // explicit x and explicit y
            uint64_t xdim = s.uvar() + 1;
            for(uint64_t i=0;i<xdim && s.ok;i++) s.uvar();
            uint64_t ydim = s.uvar() + 1;
            for(uint64_t i=0;i<ydim && s.ok;i++) s.uvar();
            r.cols = (int64_t)xdim; r.rows = (int64_t)ydim;
        } break;
        case 7: case 8: { // explicit displacement pairs (grid or arbitrary)
            uint64_t n = s.uvar() + 1;
            // type 8 in some refs is grid of n*m; but general form: n displacements
            // try to consume as displacement pairs; for alignment be conservative
            for(uint64_t i=0;i<n && s.ok;i++){ s.uvar(); s.uvar(); }
            r.cols = (int64_t)n; r.rows = 1;
        } break;
        case 9: case 10: case 11: {
            uint64_t n = s.uvar() + 1;
            for(uint64_t i=0;i<n && s.ok;i++){ s.uvar(); s.uvar(); }
            r.cols = (int64_t)n; r.rows = 1;
        } break;
        default: {
            // fallback: try to consume as a count + pairs
            uint64_t n = s.uvar() + 1;
            for(uint64_t i=0;i<n && s.ok;i++){ s.uvar(); s.uvar(); }
            r.cols = (int64_t)n; r.rows = 1;
        } break;
    }
    return r;
}

static void add_geom(Ctx& ctx, uint64_t layer, uint64_t datatype, int64_t x, int64_t y){
    if(ctx.cur_cell==(size_t)-1) return;
    auto it = ctx.geom_idx.find(ctx.cur_cell);
    if(it==ctx.geom_idx.end()) return;
    auto& cell = ctx.f->cells[it->second];
    auto key = std::make_pair((int)layer,(int)datatype);
    ldb_extent_add_point(cell.layer_ext[key], x,y);
    ldb_extent_add_point(cell.cell_ext, x,y);
}

static void add_box(Ctx& ctx, uint64_t layer, uint64_t datatype, int kind, int64_t x1,int64_t y1,int64_t x2,int64_t y2){
    if(ctx.cur_cell==(size_t)-1) return;
    auto it = ctx.geom_idx.find(ctx.cur_cell);
    if(it==ctx.geom_idx.end()) return;
    auto& cell = ctx.f->cells[it->second];
    LdbBoxElem be; be.layer=(int)layer; be.datatype=(int)datatype; be.kind=kind; be.x1=x1; be.y1=y1; be.x2=x2; be.y2=y2;
    if(be.x2<be.x1) std::swap(be.x1,be.x2); if(be.y2<be.y1) std::swap(be.y1,be.y2);
    cell.boxes.push_back(be);
    add_geom(ctx,layer,datatype,x1,y1); add_geom(ctx,layer,datatype,x2,y2);
}

static void parse_records(Stream& s, Ctx& ctx);

static void handle_cblock(Stream& s, Ctx& ctx, size_t rec_start){
    uint8_t comp_type = s.u8();
    uint64_t uncomp_len = s.uvar();
    uint64_t comp_len = s.uvar();
    if(!s.need((size_t)comp_len)) return;
    const uint8_t* comp_data = s.p + s.off;
    s.skip((size_t)comp_len);
    rec_raw(s, rec_start, ctx);
    ctx.doc.cblock_count++;
    ctx.doc.cblock_comp_bytes += (size_t)comp_len;
    ctx.doc.cblock_uncomp_bytes += (size_t)uncomp_len;
    // decompress
    std::vector<uint8_t> out;
    bool ok = db::decompress_cblock(comp_type, comp_data, (size_t)comp_len, out, (size_t)uncomp_len, ctx.nvst);
    if(ok || !out.empty()){
        // parse decompressed records
        if(ok && out.size() == (size_t)uncomp_len){
            Stream ds(out.data(), out.size(), true);
            parse_records(ds, ctx);
        } else if(!ok) {
            // if decompression failed but we got compressed bytes back, can't parse inner; records already preserved via raw CBLOCK
        }
    }
    if(ctx.nvst.used) ctx.doc.used_gpu_decompress = true;
    ctx.doc.decompress_info = ctx.nvst.detail;
}

static void parse_records(Stream& s, Ctx& ctx){
    // if at file start, consume magic
    if(!s.in_cblock && s.off==0 && s.n>=13 && memcmp(s.p, "%SEMI-OASIS\r\n", 13)==0) s.off=13;
    while(s.ok && s.off < s.n){
        size_t rec_start = s.off;
        uint8_t id = s.u8(); if(!s.ok) break;
        switch(id){
        case 0: rec_raw(s,rec_start,ctx); break; // PAD
        case 1: { // START
            s.astr(); // version
            s.real(); // unit
            // After unit: offset-flag byte in modern OASIS. Bits 0-5 indicate present offset-table values, bit 6 strict, bit 7 reserved.
            if(s.ok && s.off < s.n){
                uint8_t oflags = s.p[s.off];
                // Heuristic: if oflags looks like a plausible offset-flag (not a common record start like 0,2,3,6,13 etc in that position), consume it and entries
                bool looks_record = (oflags==0||oflags==2||oflags==3||oflags==6||oflags==7||oflags==13||oflags==34);
                if(!looks_record){
                    s.off++; // consume offset-flag
                    for(int i=0;i<6;i++) if(oflags & (1<<i)) s.uvar();
                    // if bit 7 set, some files include additional offsets/validation info; try to consume up to 6 more if present
                    if(oflags & 0x80){ for(int i=0;i<6;i++) s.uvar(); }
                }
                // Resync: scan for next plausible record start. Prefer CBLOCK(34) if present (KLayout files often start with offset/CBLOCK data).
                size_t found = s.n;
                size_t first_cblock = s.n;
                for(size_t k=s.off; k < s.n; ++k){ if(s.p[k]==34){ first_cblock = k; break; } }
                if(first_cblock < s.n && first_cblock <= s.off + 4096){
                    found = first_cblock;
                } else {
                    for(size_t j=s.off; j < s.n && j < s.off + 4096; ++j){
                        uint8_t nb = s.p[j];
                        if(nb==0){ found=j; break; }
                        if(nb==2){ if(j + 8 >= s.n){ found=j; break; } continue; }
                        if(nb==3||nb==6||nb==7||nb==13||nb==1||nb==4||nb==5||nb==8||nb==9||nb==14||nb==15||nb==16||nb==17||nb==18||nb==19||nb==20||nb==21||nb==22||nb==28||nb==34){
                            if(nb==3||nb==6||nb==7||nb==5){
                                if(j+1 < s.n){ uint8_t len = s.p[j+1]; if(len>0 && len < 128 && j+2+len < s.n) { found=j; break; } }
                            } else { found=j; break; }
                        }
                    }
                }
                s.off = found;
            }
            rec_raw(s,rec_start,ctx);
        } break;
        case 2: rec_raw(s,rec_start,ctx); s.off = s.n; break; // END
        case 3: { std::string nm=s.astr(); uint64_t ref=s.uvar(); ensure_table(ctx.cellnames,ref,nm); rec_raw(s,rec_start,ctx);} break;
        case 4: { uint64_t ref=s.uvar(); ensure_table(ctx.cellnames,ref); rec_raw(s,rec_start,ctx);} break; // CELLNAME reference
        case 5: { std::string nm=s.astr(); uint64_t ref=s.uvar(); ensure_table(ctx.textstrings,ref,nm); rec_raw(s,rec_start,ctx);} break;
        case 6: { std::string nm=s.astr(); uint64_t ref=s.uvar(); ensure_table(ctx.propnames,ref,nm); ctx.f->propnames[(int)ref]=nm; rec_raw(s,rec_start,ctx);} break;
        case 7: { std::string nm=s.astr(); uint64_t ref=s.uvar(); ensure_table(ctx.propstrings,ref,nm); rec_raw(s,rec_start,ctx);} break;
        case 8: { uint64_t ref=s.uvar(); ensure_table(ctx.propnames,ref); rec_raw(s,rec_start,ctx);} break;
        case 9: { uint64_t ref=s.uvar(); ensure_table(ctx.propstrings,ref); rec_raw(s,rec_start,ctx);} break;
        case 13: { uint64_t ref=s.uvar(); ensure_table(ctx.cellnames,ref);
            std::string name = tget(ctx.cellnames,ref); if(name.empty()) name="__cell_"+std::to_string(ref);
            if(!ctx.f->cell_index.count(name)){ ctx.f->cell_index[name]=ctx.f->cells.size(); LdbCell c; c.name=name; ctx.f->cells.push_back(std::move(c)); }
            ctx.cur_cell = ref; ctx.geom_idx[ref]=ctx.f->cell_index[name];
            rec_raw(s,rec_start,ctx);} break;
        case 14: { uint64_t ref=s.uvar(); ctx.cur_cell=ref; rec_raw(s,rec_start,ctx);} break;
        case 15: ctx.m.abs_xy=true; rec_raw(s,rec_start,ctx); break;
        case 16: ctx.m.abs_xy=false; rec_raw(s,rec_start,ctx); break;
        case 17: case 18: { // PLACEMENT / PLACEMENT_TRANSFORM
            uint8_t info=s.u8();
            if(info & 0x80){} else { ctx.m.placement_cell = s.uvar(); }
            uint64_t cref = ctx.m.placement_cell;
            int64_t px = ctx.m.x, py = ctx.m.y;
            if(info & 0x01){ int64_t dx=s.svar(), dy=s.svar(); if(ctx.m.abs_xy){ px=dx; py=dy; } else { px+=dx; py+=dy; } ctx.m.x=px; ctx.m.y=py; }
            // best-effort: for placement-transform (18) some fields may follow; avoid breaking stream by not consuming unknowns here
            // repetition
            RepInfo rep;
            if(info & 0x20){ uint8_t rtype=s.u8(); rep = parse_repetition(s,rtype); }
            // add hierarchy ref and instance (with repetition for compressed/repeated layout)
            if(ctx.cur_cell!=(size_t)-1){
                auto it=ctx.geom_idx.find(ctx.cur_cell);
                if(it!=ctx.geom_idx.end()){
                    std::string rn=tget(ctx.cellnames,cref); if(rn.empty()) rn="__cell_"+std::to_string(cref);
                    ctx.f->cells[it->second].refs.insert(rn);
                    LdbInstance inst; inst.ref_name = rn; inst.tr.ox = px; inst.tr.oy = py;
                    if(!rep.reuse){
                        inst.tr.cols = rep.cols; inst.tr.rows = rep.rows;
                        inst.tr.dx = rep.dx; inst.tr.dy = rep.dy;
                        inst.tr.row_dx = rep.row_dx; inst.tr.row_dy = rep.row_dy;
                    }
                    ctx.f->cells[it->second].instances.push_back(inst);
                }
            }
            rec_raw(s,rec_start,ctx);
        } break;
        case 19: { // TEXT
            uint8_t info=s.u8();
            if(info & 0x40) ctx.m.textlayer=s.uvar();
            if(info & 0x20) ctx.m.texttype=s.uvar();
            if(info & 0x10) ctx.m.text_string=s.uvar();
            if(info & 0x01){ int64_t dx=s.svar(), dy=s.svar(); if(ctx.m.abs_xy){ ctx.m.x=dx; ctx.m.y=dy; } else { ctx.m.x+=dx; ctx.m.y+=dy; } }
            // text transform/repetition
            if(info & 0x08){ /* transform */ }
            if(info & 0x04){ uint8_t rtype=s.u8(); (void)parse_repetition(s,rtype); }
            add_geom(ctx, ctx.m.textlayer, ctx.m.texttype, ctx.m.x, ctx.m.y);
            rec_raw(s,rec_start,ctx);
        } break;
        case 20: { // RECTANGLE
            uint8_t info=s.u8();
            if(info & 0x01){ int64_t dx=s.svar(), dy=s.svar(); if(ctx.m.abs_xy){ ctx.m.x=dx; ctx.m.y=dy; } else { ctx.m.x+=dx; ctx.m.y+=dy; } }
            if(info & 0x20) ctx.m.layer=s.uvar();
            if(info & 0x40) ctx.m.datatype=s.uvar();
            int64_t w=0,h=0;
            if((info&0x80)&&(info&0x04)){ w=h=s.uvar(); }
            else if(info&0x80){ w=s.uvar(); h=s.uvar(); }
            else if(info&0x04){ h=s.uvar(); }
            else if(info&0x02){ w=s.uvar(); }
            // but exact SWH bits: in spec 20: D,W,H,S,X,Y,R -> info bits [R,Y,X,S,H,W,D]=[0..6]? varies. handle common
            if(w==0 && h==0){ w=s.uvar(); h=s.uvar(); }
            if(info & 0x08){ uint8_t rtype=s.u8(); (void)parse_repetition(s,rtype); }
            add_box(ctx, ctx.m.layer, ctx.m.datatype, 2, ctx.m.x, ctx.m.y, ctx.m.x+w, ctx.m.y+h);
            rec_raw(s,rec_start,ctx);
        } break;
        case 21: { // POLYGON
            uint8_t info=s.u8();
            if(info & 0x01){ int64_t dx=s.svar(), dy=s.svar(); if(ctx.m.abs_xy){ ctx.m.x=dx; ctx.m.y=dy; } else { ctx.m.x+=dx; ctx.m.y+=dy; } }
            if(info & 0x20) ctx.m.layer=s.uvar();
            if(info & 0x40) ctx.m.datatype=s.uvar();
            uint8_t ptinfo=s.u8(); uint8_t ptype = ptinfo & 0x07;
            std::vector<std::pair<int64_t,int64_t>> pts; int64_t cx=ctx.m.x, cy=ctx.m.y; pts.push_back({cx,cy});
            parse_point_list(s, ptype, pts, cx, cy);
            int64_t bx1=cx,by1=cy,bx2=cx,by2=cy;
            for(auto& pt: pts){ add_geom(ctx,ctx.m.layer,ctx.m.datatype,pt.first,pt.second); if(pt.first<bx1)bx1=pt.first; if(pt.first>bx2)bx2=pt.first; if(pt.second<by1)by1=pt.second; if(pt.second>by2)by2=pt.second; }
            add_box(ctx, ctx.m.layer, ctx.m.datatype, 0, bx1,by1,bx2,by2);
            if(info & 0x08){ uint8_t rtype=s.u8(); (void)parse_repetition(s,rtype); }
            rec_raw(s,rec_start,ctx);
        } break;
        case 22: { // PATH
            uint8_t info=s.u8();
            if(info & 0x01){ int64_t dx=s.svar(), dy=s.svar(); if(ctx.m.abs_xy){ ctx.m.x=dx; ctx.m.y=dy; } else { ctx.m.x+=dx; ctx.m.y+=dy; } }
            if(info & 0x20) ctx.m.layer=s.uvar();
            if(info & 0x40) ctx.m.datatype=s.uvar();
            if(info & 0x80) ctx.m.half_width = s.uvar();
            if(info & 0x10){ s.real(); } // extension start
            if(info & 0x08){ s.real(); } // extension end
            uint8_t ptinfo=s.u8(); uint8_t ptype=ptinfo&0x07;
            std::vector<std::pair<int64_t,int64_t>> pts; int64_t cx=ctx.m.x, cy=ctx.m.y; pts.push_back({cx,cy});
            parse_point_list(s, ptype, pts, cx, cy);
            for(auto& pt: pts) add_geom(ctx,ctx.m.layer,ctx.m.datatype,pt.first,pt.second);
            if(info & 0x04){ uint8_t rtype=s.u8(); (void)parse_repetition(s,rtype); }
            rec_raw(s,rec_start,ctx);
        } break;
        case 23: { // TRAPEZOID
            uint8_t info=s.u8();
            if(info & 0x01){ int64_t dx=s.svar(), dy=s.svar(); if(ctx.m.abs_xy){ ctx.m.x=dx; ctx.m.y=dy; } else { ctx.m.x+=dx; ctx.m.y+=dy; } }
            if(info & 0x20) ctx.m.layer=s.uvar();
            if(info & 0x40) ctx.m.datatype=s.uvar();
            uint64_t delta_a=s.uvar(); uint64_t delta_b=s.uvar();
            (void)delta_a;(void)delta_b;
            add_geom(ctx,ctx.m.layer,ctx.m.datatype,ctx.m.x,ctx.m.y);
            if(info & 0x08){ uint8_t rtype=s.u8(); (void)parse_repetition(s,rtype); }
            rec_raw(s,rec_start,ctx);
        } break;
        case 24: { // CTRAPEZOID
            uint8_t info=s.u8();
            if(info & 0x01){ int64_t dx=s.svar(), dy=s.svar(); if(ctx.m.abs_xy){ ctx.m.x=dx; ctx.m.y=dy; } else { ctx.m.x+=dx; ctx.m.y+=dy; } }
            if(info & 0x20) ctx.m.layer=s.uvar();
            if(info & 0x40) ctx.m.datatype=s.uvar();
            if(info & 0x80){ ctx.m.ctrap_type = s.uvar(); }
            uint64_t w=s.uvar(); uint64_t h=s.uvar(); (void)w;(void)h;
            add_geom(ctx,ctx.m.layer,ctx.m.datatype,ctx.m.x,ctx.m.y);
            if(info & 0x08){ uint8_t rtype=s.u8(); (void)parse_repetition(s,rtype); }
            rec_raw(s,rec_start,ctx);
        } break;
        case 25: { // CIRCLE
            uint8_t info=s.u8();
            if(info & 0x01){ int64_t dx=s.svar(), dy=s.svar(); if(ctx.m.abs_xy){ ctx.m.x=dx; ctx.m.y=dy; } else { ctx.m.x+=dx; ctx.m.y+=dy; } }
            if(info & 0x20) ctx.m.layer=s.uvar();
            if(info & 0x40) ctx.m.datatype=s.uvar();
            if(info & 0x10) ctx.m.circle_radius = s.uvar();
            int64_t r=(int64_t)ctx.m.circle_radius;
            add_geom(ctx,ctx.m.layer,ctx.m.datatype,ctx.m.x-r,ctx.m.y-r);
            add_geom(ctx,ctx.m.layer,ctx.m.datatype,ctx.m.x+r,ctx.m.y+r);
            if(info & 0x08){ uint8_t rtype=s.u8(); (void)parse_repetition(s,rtype); }
            rec_raw(s,rec_start,ctx);
        } break;
        case 28: { // PROPERTY
            uint8_t info=s.u8();
            if(info & 0x04){ ctx.m.prop_name = s.uvar(); } else { std::string ns=s.astr(); (void)ns; }
            if(info & 0x02){ /* prop value list by ref */ s.uvar(); }
            int nv = info & 0x03; // not exact; simplified
            // values: type byte + value
            // safer: try to read 1 value
            if(s.ok && s.off < s.n){ uint8_t vt=s.u8(); if(vt==0x0f){ /* ref */ s.uvar(); } else if(vt==0x08||vt==0x09||vt==0x0a||vt==0x0b||vt==0x0c||vt==0x0d||vt==0x0e) s.astr(); else if(vt<=7) s.real(); else s.uvar(); }
            rec_raw(s,rec_start,ctx);
        } break;
        case 29: { std::string nm=s.astr(); uint64_t ref=s.uvar(); ensure_table(ctx.xnames,ref,nm); rec_raw(s,rec_start,ctx);} break; // XNAME
        case 30: { // XELEMENT
            uint8_t info=s.u8(); s.uvar(); // attribute ref
            // payload depends; consume as string/values heuristically
            if(info & 0x01){ s.astr(); }
            rec_raw(s,rec_start,ctx);
        } break;
        case 31: { // XGEOMETRY
            uint8_t info=s.u8(); if(s.ok) s.uvar(); // attribute
            if(info & 0x20) ctx.m.layer=s.uvar();
            if(info & 0x40) ctx.m.datatype=s.uvar();
            rec_raw(s,rec_start,ctx);
            // payload size unknown; resync
            bool found=false;
            for(size_t j = s.off; j < s.n && j < s.off + 4096; ++j){
                uint8_t nb=s.p[j];
                if(nb==0||nb==2||nb==3||nb==13||nb==19||nb==20||nb==21||nb==22||nb==34||nb==28){ s.off=j; s.ok=true; found=true; break; }
            }
            if(!found){ s.ok=false; s.err="XGEOMETRY payload size unknown in tolerant parser"; }
        } break;
        case 34: { // CBLOCK
            handle_cblock(s, ctx, rec_start);
        } break;
        default: {
            // Unknown/unsupported record in tolerant mode: record raw byte and resync forward
            rec_raw(s,rec_start,ctx);
            // try to resync to a plausible next record id
            bool found=false;
            for(size_t j = rec_start + 1; j < s.n && j < rec_start + 4096; ++j){
                uint8_t nb = s.p[j];
                if(nb==0||nb==1||nb==2||nb==3||nb==4||nb==5||nb==6||nb==7||nb==8||nb==9||
                   nb==13||nb==14||nb==15||nb==16||nb==17||nb==18||nb==19||nb==20||nb==21||
                   nb==22||nb==23||nb==24||nb==25||nb==28||nb==29||nb==30||nb==31||nb==34){
                    s.off = j; s.ok = true; found = true; break;
                }
            }
            if(!found){ s.ok=false; s.err="unhandled OASIS record "+std::to_string((int)id)+" at "+std::to_string(rec_start); }
        } break;
        }
    }
}

} // anon

bool parse_oasis(LdbFile* f) {
    Ctx ctx; ctx.f=f;
    ctx.nvst = db::nvcomp_init();
    ctx.doc.original.assign((const uint8_t*)f->map, (const uint8_t*)f->map + f->size);
    // Always preserve original raw for lossless round-trip
    f->oasis_raw = ctx.doc.original;
    Stream s((const uint8_t*)f->map, f->size, false);
    parse_records(s, ctx);
    // Entropy/compact encoding of box streams: delta+varint compression per cell
    size_t raw_box_bytes = 0, compact_box_bytes = 0;
    for(auto& cell : f->cells){
        raw_box_bytes += cell.boxes.size() * sizeof(LdbBoxElem);
        if(!cell.boxes.empty()){
            cell.boxes_compact = compact::encode_boxes(cell.boxes);
            compact_box_bytes += cell.boxes_compact.size();
            // keep decoded boxes for current consumers; compact form available for memory-efficient storage/transfer
        }
    }
    // (heuristic name scan omitted - too many false positives on binary/compressed data; rely on structured parse + CBLOCK decompression)
    f->mem_bytes_extra = f->oasis_raw.capacity() + ctx.doc.records.size()*sizeof(oasis::Record);
    for(auto& r: ctx.doc.records) f->mem_bytes_extra += r.raw.capacity();
    for(auto& cell : f->cells) f->mem_bytes_extra += cell.boxes_compact.capacity();
    if(raw_box_bytes > 0){
        char buf[256];
        snprintf(buf, sizeof(buf), "BOX_ENC raw=%zu compact=%zu (%.1fx)", raw_box_bytes, compact_box_bytes,
                 compact_box_bytes ? (double)raw_box_bytes/(double)compact_box_bytes : 0.0);
        f->error = (f->error.empty()?"":f->error+"; ") + std::string(buf);
    }
    if(!ctx.doc.decompress_info.empty()) f->error = (f->error.empty()?"":f->error+"; ") + std::string("CBLOCK:")+ctx.doc.decompress_info;
    if(f->cells.empty() && !s.err.empty() && f->error.empty()) f->error = s.err;
    return true;
}
