#include "layoutdb_internal.hpp"
#include <cstring>
#include <cmath>

// GDSII stream parser: records [2-byte len][1-byte type][1-byte dtype][data]
// Extracts BGNSTR/STRNAME/ENDEL, BOUNDARY/BOX/PATH XY, SREF/AREF with transforms,
// LAYER/DATATYPE, PROPATTR/PROPVALUE. Computes local geometry extents per cell and stores
// hierarchical instances (SREF/AREF) for flattening.

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

    auto reset_elem = [&](){
        in_elem=false; cur_layer=0; cur_dtype=0; elem_type=0;
        sname.clear(); has_strans=false; strans_mirror=false; has_mag=false; has_angle=false;
        mag=1.0; angle=0.0; cols=0; rows=0; xypts.clear();
    };

    while (off+4 <= n) {
        uint16_t reclen = be16(p+off);
        if (reclen < 4 || off+reclen > n) { f->error="truncated GDSII record"; return false; }
        uint8_t rtype=p[off+2], dtype=p[off+3];
        const uint8_t* data=p+off+4; size_t dlen=reclen-4;
        LdbRawRecord rr; rr.rtype=rtype; rr.dtype=dtype; rr.bytes.assign(p+off, p+off+reclen);
        f->gds_records.push_back(std::move(rr));
        if (rtype==0x05) { // BGNSTR
            f->cells.push_back(LdbCell{}); cur=&f->cells.back(); in_elem=false; reset_elem();
        } else if (rtype==0x06 && cur) { // STRNAME
            cur->name.assign((const char*)data,(const char*)data+dlen);
            while(!cur->name.empty() && cur->name.back()==0) cur->name.pop_back();
            f->cell_index[cur->name]=f->cells.size()-1;
        } else if (rtype==0x07) { // ENDEL
            // finalize element
            if(cur && in_elem && (elem_type==0x0A || elem_type==0x0B) && !sname.empty() && !xypts.empty()){
                LdbInstance inst; inst.ref_name = sname;
                inst.tr.ox = xypts[0].first; inst.tr.oy = xypts[0].second;
                inst.tr.mag = has_mag ? mag : 1.0;
                inst.tr.angle_deg = has_angle ? angle : 0.0;
                inst.tr.mirror_x = (has_strans && strans_mirror) ? 1 : 0;
                inst.tr.cols = 1; inst.tr.rows = 1; inst.tr.dx=0; inst.tr.dy=0; inst.tr.row_dx=0; inst.tr.row_dy=0;
                if(elem_type==0x0B && cols>0 && rows>0){
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
                cur->instances.push_back(inst);
            }
            reset_elem();
        } else if (rtype==0x08 || rtype==0x09 || rtype==0x0A || rtype==0x0B || rtype==0x0C || rtype==0x0F || rtype==0x2D) {
            in_elem=true; elem_type=rtype;
        } else if (rtype==0x0D && dlen>=2) { cur_layer = be16s(data);
        } else if (rtype==0x0E && dlen>=2) { cur_dtype = be16s(data);
        } else if (rtype==0x10 && dlen>=4) { // XY
            if (cur && in_elem) {
                xypts.clear();
                int64_t bx1=0,by1=0,bx2=0,by2=0; bool bvalid=false;
                for (size_t i=0; i+8<=dlen; i+=8) {
                    int64_t x = be32s(data+i), y = be32s(data+i+4);
                    xypts.push_back({x,y});
                    if(elem_type==0x08 || elem_type==0x09 || elem_type==0x2D || elem_type==0x0C || elem_type==0x0F){
                        auto key=std::make_pair(cur_layer,cur_dtype);
                        ldb_extent_add_point(cur->layer_ext[key], x,y);
                        ldb_extent_add_point(cur->cell_ext, x,y);
                        if(!bvalid){ bx1=bx2=x; by1=by2=y; bvalid=true; }
                        else { if(x<bx1)bx1=x; if(x>bx2)bx2=x; if(y<by1)by1=y; if(y>by2)by2=y; }
                    }
                }
                if(bvalid && (elem_type==0x08 || elem_type==0x09 || elem_type==0x2D || elem_type==0x0C || elem_type==0x0F)){
                    LdbBoxElem be; be.layer=cur_layer; be.datatype=cur_dtype; be.kind=(elem_type==0x08)?0:(elem_type==0x09?1:2);
                    be.x1=bx1; be.y1=by1; be.x2=bx2; be.y2=by2; cur->boxes.push_back(be);
                }
            }
        } else if (rtype==0x12 && dlen>0) { // SNAME
            if (cur && in_elem && (elem_type==0x0A || elem_type==0x0B)) {
                sname.assign((const char*)data,(const char*)data+dlen);
                while(!sname.empty() && sname.back()==0) sname.pop_back();
                cur->refs.insert(sname);
            }
        } else if (rtype==0x1A && dlen>=2) { // STRANS
            has_strans = true;
            uint16_t bits = be16(data);
            strans_mirror = (bits & 0x8000) != 0;
        } else if (rtype==0x1B && dlen>=8) { // MAG
            has_mag = true; mag = be_real8(data);
        } else if (rtype==0x1C && dlen>=8) { // ANGLE
            has_angle = true; angle = be_real8(data);
        } else if (rtype==0x13 && dlen>=4) { // COLROW
            cols = be16s(data); rows = be16s(data+2);
        } else if (rtype==0x2B && dlen>=2) { // PROPATTR
            int id = be16s(data); if (!f->propnames.count(id)) f->propnames[id]="";
        } else if (rtype==0x2C && dlen>0) { // PROPVALUE
            for (auto it=f->propnames.rbegin(); it!=f->propnames.rend(); ++it) {
                if (it->second.empty()) { it->second.assign((const char*)data,(const char*)data+dlen); while(!it->second.empty()&&it->second.back()==0) it->second.pop_back(); break; }
            }
        } else if (rtype==0x04) { // ENDLIB
            off += reclen; break;
        }
        off += reclen;
    }
    for(size_t i=0;i<f->cells.size();++i) if(f->cells[i].name.empty()){ f->cells[i].name="__cell_"+std::to_string(i); f->cell_index[f->cells[i].name]=i; }
    f->mem_bytes_extra = f->gds_records.size()*sizeof(LdbRawRecord);
    for(auto& r: f->gds_records) f->mem_bytes_extra += r.bytes.capacity();
    return true;
}
