#include "layoutdb_internal.hpp"
#include <cstring>
#include <cmath>
#include <ctime>
#include <cstdio>
#include "fracture.hpp"

// GDSII stream parser: records [2-byte len][1-byte type][1-byte dtype][data]
// Extracts BGNSTR/STRNAME/ENDSTR, BOUNDARY/BOX/PATH geometry, SREF/AREF with
// transforms, LAYER/DATATYPE/BOXTYPE, WIDTH/PATHTYPE/BGNEXTN/ENDEXTN and
// PROPATTR/PROPVALUE.  Geometry is stored as rectangles: Manhattan boundaries
// and paths exactly, slanted boundaries as rectangle strips, slanted paths as
// their bounding box.  TEXT and NODE elements carry no area and are skipped.

namespace {
enum : uint8_t {
    HEADER = 0x00, BGNLIB = 0x01, LIBNAME = 0x02, UNITS = 0x03, ENDLIB = 0x04, BGNSTR = 0x05, STRNAME = 0x06, ENDSTR = 0x07, BOUNDARY = 0x08, PATH = 0x09,
    SREF = 0x0A, AREF = 0x0B, TEXT = 0x0C, LAYER = 0x0D, DATATYPE = 0x0E, WIDTH = 0x0F, XY = 0x10,
    ENDEL = 0x11, SNAME = 0x12, COLROW = 0x13, NODE = 0x15, STRANS = 0x1A, MAG = 0x1B, ANGLE = 0x1C,
    PATHTYPE = 0x21, PROPATTR = 0x2B, PROPVALUE = 0x2C, BOX = 0x2D, BOXTYPE = 0x2E,
    BGNEXTN = 0x30, ENDEXTN = 0x31, TEXTTYPE = 0x16, PRESENTATION = 0x17, STRING = 0x19
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
    // TEXT (kept in the model unless low-memory)
    int texttype=0; std::string text_str;
    const bool keep_texts = (f->open_flags & LDB_OPEN_LOW_MEMORY) == 0;
    std::vector<ldbgeom::Pt> verts; std::vector<ldbgeom::Rect> rects;

    auto reset_elem = [&](){
        in_elem=false; cur_layer=0; cur_dtype=0; elem_type=0;
        sname.clear(); has_strans=false; strans_mirror=false; has_mag=false; has_angle=false;
        mag=1.0; angle=0.0; cols=0; rows=0; xypts.clear(); elem_props.clear(); prop_attr=-1;
        width=0; bgn_extn=0; end_extn=0; pathtype=0;
        texttype=0; text_str.clear();
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
            if(cur && in_elem && elem_type==TEXT && keep_texts && !xypts.empty()){
                LdbTextElem t; t.layer=cur_layer; t.texttype=texttype; t.x=xypts[0].first; t.y=xypts[0].second; t.str=text_str;
                cur->texts.push_back(std::move(t));
            }
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
        } else if (rtype==TEXTTYPE && dlen>=2) { texttype = be16s(data);
        } else if (rtype==STRING) {
            text_str.assign((const char*)data,(const char*)data+dlen);
            while(!text_str.empty() && text_str.back()==0) text_str.pop_back();
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
                            int64_t x1,y1,x2,y2;                   // any-angle: box of the mitred outline
                            if(ldbgeom::path_bbox(verts, hw, e0, e1, x1, y1, x2, y2)) rects.push_back({x1,y1,x2,y2});
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
                        if(elem_type==PATH){
                            pe.width=width; pe.pathtype=pathtype;
                            const int64_t hw=width/2;
                            pe.ext_begin=(pathtype==4)?bgn_extn:(pathtype==0?0:hw);
                            pe.ext_end  =(pathtype==4)?end_extn:(pathtype==0?0:hw);
                        }
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
        } else if (rtype==UNITS && dlen>=16) {
            // user units per database unit, meters per database unit
            const double m = be_real8(data+8);
            if (m > 0) {
                double d = 1e-6 / m;
                const double r = std::round(d);
                if (r > 0 && std::fabs(d - r) <= 1e-9 * r) d = r;
                f->dbu_per_micron = d;
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


// ---------------------------------------------------------------------------
// GDSII writer from the format-neutral model (used when the model was not read from
// GDSII). Polygons/paths are written as BOUNDARY/PATH when the source kept them, else every
// box as a 5-point BOUNDARY; placements as SREF, arrays as AREF. Limits of the format
// (16-bit layers and datatypes, 32-bit coordinates, 8191 points per XY record, 32767
// columns/rows) are checked and reported, never silently wrapped.
// ---------------------------------------------------------------------------
namespace {

void real8(double v, uint8_t* out) {
    std::memset(out, 0, 8);
    if (v == 0 || !std::isfinite(v)) return;
    uint8_t sign = 0;
    if (v < 0) { sign = 0x80; v = -v; }
    int e = 0;
    while (v >= 1.0) { v /= 16.0; ++e; }
    while (v < 1.0 / 16.0) { v *= 16.0; --e; }
    uint64_t mant = (uint64_t)std::llround(std::ldexp(v, 56));
    if (mant >= (1ull << 56)) { mant >>= 4; ++e; }
    if (e + 64 < 0) return;                                  // underflow: zero
    if (e + 64 > 127) { e = 63; mant = (1ull << 56) - 1; }    // overflow: largest value
    out[0] = (uint8_t)(sign | (e + 64));
    for (int i = 7; i >= 1; --i) { out[i] = (uint8_t)(mant & 0xff); mant >>= 8; }
}

struct GdsOut {
    FILE* fp; std::string err; std::vector<uint8_t> b;
    void rec(uint8_t type, uint8_t dt, const uint8_t* data, size_t n) {
        if (!err.empty()) return;
        if (n + 4 > 65534) { err = "GDSII record too long"; return; }
        const size_t len = 4 + n + (n & 1);
        b.clear();
        b.push_back((uint8_t)(len >> 8)); b.push_back((uint8_t)len); b.push_back(type); b.push_back(dt);
        b.insert(b.end(), data, data + n);
        if (n & 1) b.push_back(0);
        if (fwrite(b.data(), 1, b.size(), fp) != b.size()) err = "write failed";
    }
    void none(uint8_t type) { rec(type, 0, nullptr, 0); }
    void i16(uint8_t type, const std::vector<int64_t>& v) {
        std::vector<uint8_t> d;
        for (int64_t x : v) {
            // signed 2-byte integer, as every GDSII reader (this one included: be16s) decodes it;
            // layer, datatype and texttype must also be non-negative
            const bool layerlike = type == LAYER || type == DATATYPE || type == TEXTTYPE;
            if (x < (layerlike ? 0 : -32768) || x > 32767) {
                err = "value " + std::to_string(x) + " does not fit a GDSII 2-byte integer field"; return;
            }
            d.push_back((uint8_t)((uint64_t)x >> 8)); d.push_back((uint8_t)x);
        }
        rec(type, 2, d.data(), d.size());
    }
    void i32(uint8_t type, const std::vector<int64_t>& v) {
        std::vector<uint8_t> d;
        for (int64_t x : v) {
            if (x < INT32_MIN || x > INT32_MAX) { err = "coordinate " + std::to_string(x) + " does not fit a GDSII 4-byte integer"; return; }
            const uint32_t u = (uint32_t)(int32_t)x;
            d.push_back((uint8_t)(u >> 24)); d.push_back((uint8_t)(u >> 16)); d.push_back((uint8_t)(u >> 8)); d.push_back((uint8_t)u);
        }
        rec(type, 3, d.data(), d.size());
    }
    void r8(uint8_t type, const std::vector<double>& v) {
        std::vector<uint8_t> d(8 * v.size());
        for (size_t i = 0; i < v.size(); ++i) real8(v[i], d.data() + 8 * i);
        rec(type, 5, d.data(), d.size());
    }
    void str(uint8_t type, const std::string& s) { rec(type, 6, (const uint8_t*)s.data(), s.size()); }
    void bits(uint8_t type, uint16_t v) { const uint8_t d[2] = { (uint8_t)(v >> 8), (uint8_t)v }; rec(type, 1, d, 2); }   // bit array
};

enum : uint8_t { W_STRANS = 0x1A, W_ENDSTR = 0x07 };

} // namespace

bool write_gdsii_model(const LdbFile* f, const char* path, std::string& err) {
    FILE* fp = fopen(path, "wb");
    if (!fp) { err = "open for write failed"; return false; }
    GdsOut o{ fp, "", {} };
    const double dbu = f->dbu_per_micron > 0 ? f->dbu_per_micron : 1000.0;
    std::time_t now = std::time(nullptr);
    std::tm tmv{}; localtime_r(&now, &tmv);
    const std::vector<int64_t> stamp = { tmv.tm_year + 1900, tmv.tm_mon + 1, tmv.tm_mday, tmv.tm_hour, tmv.tm_min, tmv.tm_sec };
    std::vector<int64_t> two = stamp; two.insert(two.end(), stamp.begin(), stamp.end());
    o.i16(HEADER, { 600 });
    o.i16(BGNLIB, two);
    o.str(LIBNAME, "LIB");
    o.r8(UNITS, { 1.0 / dbu, 1e-6 / dbu });               // user unit = 1 micron
    size_t skipped = 0, truncated_texts = 0;
    auto xy_closed = [&](const std::vector<LdbPoint>& pts) {
        std::vector<int64_t> v;
        for (auto& q : pts) { v.push_back(q.x); v.push_back(q.y); }
        if (!pts.empty() && (pts.front().x != pts.back().x || pts.front().y != pts.back().y)) { v.push_back(pts[0].x); v.push_back(pts[0].y); }
        return v;
    };
    for (const auto& c : f->cells) {
        o.i16(BGNSTR, two);
        o.str(STRNAME, c.name);
        for (const auto& inst : c.instances) {
            const LdbTransform& t = inst.tr;
            const bool aref = t.cols * t.rows > 1;
            o.none(aref ? AREF : SREF);
            o.str(SNAME, inst.ref_name);
            if (t.mirror_x || t.mag != 1.0 || t.angle_deg != 0.0) {
                o.bits(W_STRANS, t.mirror_x ? 0x8000 : 0);
                if (t.mag != 1.0) o.r8(MAG, { t.mag });
                if (t.angle_deg != 0.0) o.r8(ANGLE, { t.angle_deg });
            }
            if (aref) {
                if (t.cols > 32767 || t.rows > 32767) { o.err = "array of " + std::to_string(t.cols) + " x " + std::to_string(t.rows) + " exceeds GDSII COLROW"; break; }
                o.i16(COLROW, { t.cols, t.rows });
                o.i32(XY, { t.ox, t.oy, t.ox + t.cols * t.dx, t.oy + t.cols * t.dy, t.ox + t.rows * t.row_dx, t.oy + t.rows * t.row_dy });
            } else o.i32(XY, { t.ox, t.oy });
            o.none(ENDEL);
        }
        if (!c.polys.empty()) {
            for (const auto& p : c.polys) {
                if (p.kind == 1) {
                    if (p.pts.size() > 8191) { ++skipped; continue; }
                    o.none(PATH);
                    o.i16(LAYER, { p.layer }); o.i16(DATATYPE, { p.datatype });
                    o.i16(PATHTYPE, { p.pathtype });
                    if (p.width) o.i32(WIDTH, { p.width });
                    if (p.pathtype == 4) { o.i32(BGNEXTN, { p.ext_begin }); o.i32(ENDEXTN, { p.ext_end }); }
                    std::vector<int64_t> v;
                    for (auto& q : p.pts) { v.push_back(q.x); v.push_back(q.y); }
                    o.i32(XY, v);
                } else {
                    std::vector<int64_t> v = xy_closed(p.pts);
                    if (v.size() / 2 > 8191 || v.size() / 2 < 4) { ++skipped; continue; }
                    o.none(BOUNDARY);
                    o.i16(LAYER, { p.layer }); o.i16(DATATYPE, { p.datatype });
                    o.i32(XY, v);
                }
                o.none(ENDEL);
            }
        }
        if (c.polys.empty() || c.polys_extra) {
            std::vector<LdbBoxElem> decoded;
            const std::vector<LdbBoxElem>* boxes = &c.boxes;
            if (boxes->empty() && (!c.boxes_compact.empty() || c.boxes_on_disk)) {
                extern std::vector<LdbBoxElem> ldb_load_boxes_on_demand(LdbCell& c);
                decoded = ldb_load_boxes_on_demand(const_cast<LdbCell&>(c)); boxes = &decoded;
            }
            for (const auto& b : *boxes) {
                if (c.polys_extra && b.kind != 2) continue;
                o.none(BOUNDARY);
                o.i16(LAYER, { b.layer }); o.i16(DATATYPE, { b.datatype });
                o.i32(XY, { b.x1, b.y1, b.x2, b.y1, b.x2, b.y2, b.x1, b.y2, b.x1, b.y1 });
                o.none(ENDEL);
            }
        }
        for (const auto& t : c.texts) {
            o.none(TEXT);
            o.i16(LAYER, { t.layer }); o.i16(TEXTTYPE, { t.texttype });
            o.i32(XY, { t.x, t.y });
            if (t.str.size() > 512) { ++truncated_texts; o.str(STRING, t.str.substr(0, 512)); }    // GDSII limit 512 characters
            else o.str(STRING, t.str);
            o.none(ENDEL);
        }
        o.none(W_ENDSTR);
        if (!o.err.empty()) break;
    }
    o.none(ENDLIB);
    if (fclose(fp) != 0 && o.err.empty()) o.err = "write failed";
    if (o.err.empty() && skipped) o.err = std::to_string(skipped) + " polygon/path(s) with more than 8191 points (or fewer than 3) not written";
    if (truncated_texts)                             // written (truncated): a warning, not a failure
        fprintf(stderr, "ldb_write_gdsii: %zu text string(s) longer than 512 characters truncated\n", truncated_texts);
    err = o.err;
    return err.empty();
}
