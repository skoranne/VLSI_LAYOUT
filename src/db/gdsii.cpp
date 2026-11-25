#include "layoutdb_internal.hpp"
#include <cstring>
#include <cmath>
#include "fracture.hpp"

// GDSII stream parser: records [2-byte len][1-byte type][1-byte dtype][data]
// Extracts BGNSTR/STRNAME/ENDSTR, BOUNDARY/BOX/PATH geometry, SREF/AREF with
// transforms, LAYER/DATATYPE/BOXTYPE, WIDTH/PATHTYPE/BGNEXTN/ENDEXTN and
// PROPATTR/PROPVALUE.  Geometry is stored as rectangles: Manhattan boundaries
// and paths exactly, slanted boundaries as rectangle strips, slanted paths as
// their bounding box.  TEXT and NODE elements carry no area and are skipped.

namespace {
enum : uint8_t {
    ENDLIB = 0x04, BGNSTR = 0x05, STRNAME = 0x06, ENDSTR = 0x07, BOUNDARY = 0x08, PATH = 0x09,
    SREF = 0x0A, AREF = 0x0B, TEXT = 0x0C, LAYER = 0x0D, DATATYPE = 0x0E, WIDTH = 0x0F, XY = 0x10,
    ENDEL = 0x11, SNAME = 0x12, COLROW = 0x13, NODE = 0x15, STRANS = 0x1A, MAG = 0x1B, ANGLE = 0x1C,
    PATHTYPE = 0x21, PROPATTR = 0x2B, PROPVALUE = 0x2C, BOX = 0x2D, BOXTYPE = 0x2E,
    BGNEXTN = 0x30, ENDEXTN = 0x31
};
}

static inline uint16_t be16(const uint8_t* p){ return (uint16_t(p[0])<<8)|p[1]; }
static inline int16_t be16s(const uint8_t* p){ return (int16_t)be16(p); }
static inline int32_t be32s(const uint8_t* p){ return (int32_t)((uint32_t(p[0])<<24)|(uint32_t(p[1])<<16)|(uint32_t(p[2])<<8)|p[3]); }

static double be_real8(const uint8_t* p){
    // GDSII 8-byte real: 1 sign, 7 exponent (excess-64, base 16), 56 mantissa
    uint8_t b0 = p[0];
    int sign = (b0 & 0x80) ? -1 : 1;
    int exp = (b0 & 0x7f) - 64;
    uint64_t mant = 0;
    for(int i=1;i<8;i++) mant = (mant<<8) | p[i];
    if(mant == 0) return 0.0;
    double m = (double)mant / pow(2.0, 56.0);
    return sign * m * pow(16.0, (double)exp);
}

bool parse_gdsii(LdbFile* f) {
    const uint8_t* p=(const uint8_t*)f->map; size_t n=f->size; size_t off=0;
    LdbCell* cur=nullptr; int cur_layer=0, cur_dtype=0; bool in_elem=false; int elem_type=0;
    f->gds_records.clear();

    // per-element state for SREF/AREF
    std::string sname;
    bool has_strans=false, strans_mirror=false;
    bool has_mag=false, has_angle=false;
    double mag=1.0, angle=0.0;
    int16_t cols=0, rows=0;
    std::vector<std::pair<int64_t,int64_t>> xypts;
    // PROPATTR/PROPVALUE pairs of the current element (attached to SREF/AREF at ENDEL)
    std::vector<std::pair<int,std::string>> elem_props; int prop_attr = -1;
    // per-element state for PATH
    int64_t width=0, bgn_extn=0, end_extn=0; int pathtype=0;
    std::vector<ldbgeom::Pt> verts; std::vector<ldbgeom::Rect> rects;

    auto reset_elem = [&](){
        in_elem=false; cur_layer=0; cur_dtype=0; elem_type=0;
        sname.clear(); has_strans=false; strans_mirror=false; has_mag=false; has_angle=false;
        mag=1.0; angle=0.0; cols=0; rows=0; xypts.clear(); elem_props.clear(); prop_attr=-1;
        width=0; bgn_extn=0; end_extn=0; pathtype=0;
    };

    while (off+4 <= n) {
        uint16_t reclen = be16(p+off);
        if (reclen < 4 || off+reclen > n) { f->error="truncated GDSII record"; return false; }
        uint8_t rtype=p[off+2], dtype=p[off+3];
        const uint8_t* data=p+off+4; size_t dlen=reclen-4;
        LdbRawRecord rr; rr.rtype=rtype; rr.dtype=dtype; rr.bytes.assign(p+off, p+off+reclen);
        f->gds_records.push_back(std::move(rr));
        if (rtype==BGNSTR) {
            f->cells.push_back(LdbCell{}); cur=&f->cells.back(); in_elem=false; reset_elem();
        } else if (rtype==STRNAME && cur) {
            cur->name.assign((const char*)data,(const char*)data+dlen);
            while(!cur->name.empty() && cur->name.back()==0) cur->name.pop_back();
            f->cell_index[cur->name]=f->cells.size()-1;
        } else if (rtype==ENDSTR) {
            reset_elem();
        } else if (rtype==ENDEL) {
            // finalize element
            if(cur && in_elem && (elem_type==SREF || elem_type==AREF) && !sname.empty() && !xypts.empty()){
                LdbInstance inst; inst.ref_name = sname;
                inst.tr.ox = xypts[0].first; inst.tr.oy = xypts[0].second;
                inst.tr.mag = has_mag ? mag : 1.0;
                inst.tr.angle_deg = has_angle ? angle : 0.0;
                inst.tr.mirror_x = (has_strans && strans_mirror) ? 1 : 0;
                inst.tr.cols = 1; inst.tr.rows = 1; inst.tr.dx=0; inst.tr.dy=0; inst.tr.row_dx=0; inst.tr.row_dy=0;
                if(elem_type==AREF && cols>0 && rows>0){
                    inst.tr.cols = cols; inst.tr.rows = rows;
                    // AREF XY: (ref pt), (col pt = ref + cols*dx_vec), (row pt = ref + rows*dy_vec)
                    if(xypts.size() >= 3){
                        int64_t cx = xypts[1].first - xypts[0].first;
                        int64_t cy = xypts[1].second - xypts[0].second;
                        int64_t rx = xypts[2].first - xypts[0].first;
                        int64_t ry = xypts[2].second - xypts[0].second;
                        inst.tr.dx = (cols!=0)? cx/cols : 0;
                        inst.tr.dy = (cols!=0)? cy/cols : 0;
                        inst.tr.row_dx = (rows!=0)? rx/rows : 0;
                        inst.tr.row_dy = (rows!=0)? ry/rows : 0;
                    } else if(xypts.size() >= 2){
                        inst.tr.dx = (xypts[1].first - xypts[0].first);
                        inst.tr.dy = (xypts[1].second - xypts[0].second);
                    }
                }
                for(auto& pv : elem_props){
                    std::vector<std::string> vals{std::to_string(pv.first), pv.second};
                    std::string nm = ldb_instance_name_from_property("S_GDS_PROPERTY", vals);
                    if(!nm.empty() && inst.name.empty()) inst.name = nm;
                    inst.props.push_back({"S_GDS_PROPERTY", vals[0] + " " + pv.second});
                }
                cur->instances.push_back(inst);
            }
            reset_elem();
        } else if (rtype==BOUNDARY || rtype==PATH || rtype==SREF || rtype==AREF || rtype==TEXT || rtype==NODE || rtype==BOX) {
            in_elem=true; elem_type=rtype;
        } else if (rtype==LAYER && dlen>=2) { cur_layer = be16s(data);
        } else if ((rtype==DATATYPE || rtype==BOXTYPE) && dlen>=2) { cur_dtype = be16s(data);
        } else if (rtype==WIDTH && dlen>=4) { width = be32s(data); if(width<0) width=-width;
        } else if (rtype==PATHTYPE && dlen>=2) { pathtype = be16s(data);
        } else if (rtype==BGNEXTN && dlen>=4) { bgn_extn = be32s(data);
        } else if (rtype==ENDEXTN && dlen>=4) { end_extn = be32s(data);
        } else if (rtype==XY && dlen>=4) {
            if (cur && in_elem) {
                xypts.clear();
                for (size_t i=0; i+8<=dlen; i+=8) xypts.push_back({be32s(data+i), be32s(data+i+4)});
                if(elem_type==BOUNDARY || elem_type==PATH || elem_type==BOX){
                    const int kind=(elem_type==BOUNDARY)?0:(elem_type==PATH?1:2);
                    verts.clear(); rects.clear();
                    for(auto& q: xypts) verts.push_back({q.first, q.second});
                    if(elem_type==PATH){
                        const int64_t hw = width/2;
                        // 0: flush ends, 1: round (taken as square), 2: square, 4: explicit extensions
                        const int64_t e0 = (pathtype==4) ? bgn_extn : (pathtype==0 ? 0 : hw);
                        const int64_t e1 = (pathtype==4) ? end_extn : (pathtype==0 ? 0 : hw);
                        if(hw>0 && verts.size()>=2 && !ldbgeom::path_to_rects(verts, hw, e0, e1, rects)){
                            int64_t x1=verts[0].x,x2=x1,y1=verts[0].y,y2=y1;
                            for(auto& q: verts){ x1=std::min(x1,q.x); x2=std::max(x2,q.x); y1=std::min(y1,q.y); y2=std::max(y2,q.y); }
                            rects.push_back({x1-hw,y1-hw,x2+hw,y2+hw});
                        }
                    } else {
                        if(verts.size()>1 && verts.front().x==verts.back().x && verts.front().y==verts.back().y) verts.pop_back();
                        if(verts.size()>=3){
                            if(ldbgeom::is_manhattan(verts)) ldbgeom::fracture_manhattan(verts, rects);
                            else ldbgeom::fracture_general(verts, rects);
                        }
                    }
                    auto& lext = cur->layer_ext[std::make_pair(cur_layer,cur_dtype)];
                    for(auto& r: rects){
                        LdbBoxElem be; be.layer=cur_layer; be.datatype=cur_dtype; be.kind=kind;
                        be.x1=r[0]; be.y1=r[1]; be.x2=r[2]; be.y2=r[3]; cur->boxes.push_back(be);
                        ldb_extent_add_point(lext, r[0],r[1]); ldb_extent_add_point(lext, r[2],r[3]);
                        ldb_extent_add_point(cur->cell_ext, r[0],r[1]); ldb_extent_add_point(cur->cell_ext, r[2],r[3]);
                    }
                    {
                        LdbPolyElem pe; pe.layer=cur_layer; pe.datatype=cur_dtype; pe.kind=kind;
                        pe.pts.reserve(xypts.size());
                        for(auto& pxy: xypts) pe.pts.push_back({pxy.first, pxy.second});
                        if(!pe.pts.empty()) cur->polys.push_back(std::move(pe));
                    }
                }
            }
        } else if (rtype==SNAME && dlen>0) {
            if (cur && in_elem && (elem_type==SREF || elem_type==AREF)) {
                sname.assign((const char*)data,(const char*)data+dlen);
                while(!sname.empty() && sname.back()==0) sname.pop_back();
                cur->refs.insert(sname);
            }
        } else if (rtype==STRANS && dlen>=2) {
            has_strans = true;
            uint16_t bits = be16(data);
            strans_mirror = (bits & 0x8000) != 0;
        } else if (rtype==MAG && dlen>=8) {
            has_mag = true; mag = be_real8(data);
        } else if (rtype==ANGLE && dlen>=8) {
            has_angle = true; angle = be_real8(data);
        } else if (rtype==COLROW && dlen>=4) {
            cols = be16s(data); rows = be16s(data+2);
        } else if (rtype==PROPATTR && dlen>=2) {
            int id = be16s(data); if (!f->propnames.count(id)) f->propnames[id]="";
            prop_attr = id;
        } else if (rtype==PROPVALUE && dlen>0) {
            if (in_elem && prop_attr >= 0) {
                std::string v((const char*)data,(const char*)data+dlen); while(!v.empty()&&v.back()==0) v.pop_back();
                elem_props.push_back({prop_attr, v}); prop_attr = -1;
            }
            for (auto it=f->propnames.rbegin(); it!=f->propnames.rend(); ++it) {
                if (it->second.empty()) { it->second.assign((const char*)data,(const char*)data+dlen); while(!it->second.empty()&&it->second.back()==0) it->second.pop_back(); break; }
            }
        } else if (rtype==ENDLIB) {
            off += reclen; break;
        }
        off += reclen;
    }
    for(size_t i=0;i<f->cells.size();++i) if(f->cells[i].name.empty()){ f->cells[i].name="__cell_"+std::to_string(i); f->cell_index[f->cells[i].name]=i; }
    f->mem_bytes_extra = f->gds_records.size()*sizeof(LdbRawRecord);
    for(auto& r: f->gds_records) f->mem_bytes_extra += r.bytes.capacity();
    return true;
}
