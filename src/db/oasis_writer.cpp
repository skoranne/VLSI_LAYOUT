// OASIS (SEMI P39) writer.
//
// Emits standard-conforming files: START with an in-record (empty) table
// offset block, CELLNAME / PROPNAME / PROPSTRING tables with explicit
// reference numbers, CELL, PLACEMENT (with repetitions), RECTANGLE, POLYGON,
// PROPERTY and a 256-byte END record.  The byte encodings mirror oasis.cpp.

#include "layoutdb_internal.hpp"
#include <cmath>
#include <cstdio>
#include <vector>
#include <string>
#include <cstdint>
#include <cstring>
#include <map>
#include <utility>

std::vector<LdbBoxElem> ldb_load_boxes_on_demand(LdbCell& c);   // layoutdb.cpp

namespace {

enum Record : uint8_t {
    REC_START = 1, REC_END = 2, REC_CELLNAME_REF = 4, REC_PROPNAME_REF = 8, REC_PROPSTRING_REF = 10,
    REC_CELL_REF = 13, REC_PLACEMENT = 17, REC_PLACEMENT_GENERAL = 18, REC_RECTANGLE = 20,
    REC_POLYGON = 21, REC_PROPERTY = 28
};

struct Writer {
    std::vector<uint8_t> b;
    void u8(uint8_t v) { b.push_back(v); }
    void bytes(const void* p, size_t n) { auto* c = (const uint8_t*)p; b.insert(b.end(), c, c + n); }
    void uint_(uint64_t v) { while (v >= 0x80) { u8(uint8_t(v | 0x80)); v >>= 7; } u8(uint8_t(v)); }
    static uint64_t mag(int64_t v) { return v < 0 ? (uint64_t)0 - (uint64_t)v : (uint64_t)v; }
    void sint(int64_t v) { uint_((mag(v) << 1) | (v < 0 ? 1u : 0u)); }      // sign in the low bit
    void str(const std::string& s) { uint_(s.size()); bytes(s.data(), s.size()); }
    void real_whole(uint64_t v) { uint_(0); uint_(v); }
    void real_double(double v) { uint_(7); bytes(&v, 8); }
    void gdelta(int64_t dx, int64_t dy) {                                   // general form
        uint_((mag(dx) << 2) | (dx < 0 ? 2u : 0u) | 1u);
        uint_((mag(dy) << 1) | (dy < 0 ? 1u : 0u));
    }

    void begin(uint64_t grid_per_micron) {
        bytes("%SEMI-OASIS\r\n", 13);
        u8(REC_START); str("1.0"); real_whole(grid_per_micron);
        uint_(0);                                       // table offsets follow, here
        for (int i = 0; i < 12; ++i) uint_(0);          // six (strict flag, offset) pairs: no tables
    }
    void end() {
        u8(REC_END);
        str(std::string(252, '\0'));                    // pads the record to 256 bytes
        uint_(0);                                       // no validation signature
    }
    // One-value property whose name is a PROPNAME reference; the value is an
    // n-string or, when value_ref >= 0, a PROPSTRING reference.
    void property(uint64_t name_ref, const std::string& value, long value_ref = -1) {
        u8(REC_PROPERTY); u8(0x16);                     // one value, name by reference number
        uint_(name_ref);
        if (value_ref >= 0) { uint_(15); uint_((uint64_t)value_ref); }
        else { uint_(12); str(value); }
    }
    void rectangle(int layer, int datatype, int64_t x, int64_t y, int64_t w, int64_t h) {
        u8(REC_RECTANGLE); u8(0x7B);                    // W H X Y D L
        uint_((uint64_t)layer); uint_((uint64_t)datatype);
        uint_((uint64_t)w); uint_((uint64_t)h); sint(x); sint(y);
    }
    void polygon(int layer, int datatype, const std::vector<std::pair<int64_t, int64_t>>& pts) {
        if (pts.size() < 3) return;
        u8(REC_POLYGON); u8(0x3B);                      // P X Y D L
        uint_((uint64_t)layer); uint_((uint64_t)datatype);
        uint_(4); uint_(pts.size() - 1);                // point list: general deltas
        for (size_t i = 1; i < pts.size(); ++i) gdelta(pts[i].first - pts[i - 1].first, pts[i].second - pts[i - 1].second);
        sint(pts[0].first); sint(pts[0].second);
    }
    void placement(uint64_t cell_ref, const LdbTransform& t) {
        const bool array = t.cols > 1 || t.rows > 1;
        const double quarter = t.angle_deg / 90.0;
        const bool simple = t.mag == 1.0 && quarter == std::floor(quarter);
        uint8_t info = 0xF0 | (array ? 0x08 : 0) | (t.mirror_x ? 0x01 : 0);   // C N X Y [R] [F]
        if (simple) {
            info |= (uint8_t)((((long long)quarter % 4 + 4) % 4) << 1);
            u8(REC_PLACEMENT); u8(info); uint_(cell_ref);
        } else {
            info |= 0x06;                               // magnification and angle follow
            u8(REC_PLACEMENT_GENERAL); u8(info); uint_(cell_ref);
            real_double(t.mag); real_double(t.angle_deg);
        }
        sint(t.ox); sint(t.oy);
        if (!array) return;
        if (t.cols > 1 && t.rows > 1) {
            uint_(8); uint_((uint64_t)t.cols - 2); uint_((uint64_t)t.rows - 2);
            gdelta(t.dx, t.dy); gdelta(t.row_dx, t.row_dy);
        } else if (t.cols > 1) { uint_(9); uint_((uint64_t)t.cols - 2); gdelta(t.dx, t.dy); }
        else { uint_(9); uint_((uint64_t)t.rows - 2); gdelta(t.row_dx, t.row_dy); }
    }
    int save(const char* path, char* errbuf, size_t errbuf_sz) const {
        FILE* fp = fopen(path, "wb");
        if (!fp) { if (errbuf) snprintf(errbuf, errbuf_sz, "open for write failed"); return -1; }
        const bool ok = fwrite(b.data(), 1, b.size(), fp) == b.size();
        fclose(fp);
        if (!ok) { if (errbuf) snprintf(errbuf, errbuf_sz, "write failed"); return -1; }
        return 0;
    }
};

} // namespace

// Synthetic gear with circular cut-outs (sampled curves written as POLYGON
// records) carrying cell properties: the 3DIC test layout.
extern "C" int ldb_write_oasis_gear_multigon(const char* out_path, char* errbuf, size_t errbuf_sz) {
    if (errbuf && errbuf_sz) errbuf[0] = '\0';
    Writer w;
    w.begin(1000);                                      // 1 nm grid
    w.u8(REC_PROPNAME_REF); w.str("GEAR_TYPE"); w.uint_(0);
    w.u8(REC_PROPNAME_REF); w.str("MATERIAL"); w.uint_(1);
    w.u8(REC_PROPSTRING_REF); w.str("3DIC_CUTOUT"); w.uint_(0);
    w.u8(REC_CELLNAME_REF); w.str("GEAR3DIC"); w.uint_(0);
    w.u8(REC_CELL_REF); w.uint_(0);
    w.property(0, "3DIC_cutout");
    w.property(1, "", 0);

    auto ring = [](double cx, double cy, double radius, int n) {
        std::vector<std::pair<int64_t, int64_t>> pts;
        for (int i = 0; i < n; ++i) {
            const double a = 2 * M_PI * i / n;
            pts.push_back({ (int64_t)llround(cx + radius * cos(a)), (int64_t)llround(cy + radius * sin(a)) });
        }
        return pts;
    };
    const int teeth = 8; const double R = 2000, r = 1500, tooth = 400;
    std::vector<std::pair<int64_t, int64_t>> gear;
    for (int i = 0; i < teeth * 4; ++i) {
        const double a = (2 * M_PI * i) / (teeth * 4), rad = (i % 4 < 2) ? (R + tooth) : r;
        gear.push_back({ (int64_t)llround(rad * cos(a)), (int64_t)llround(rad * sin(a)) });
    }
    w.polygon(10, 0, gear);
    w.polygon(11, 0, ring(0, 0, 600, 64));
    for (int k = 0; k < 4; ++k) {
        const double a0 = 2 * M_PI * k / 4;
        w.polygon(11, 0, ring(1100 * cos(a0), 1100 * sin(a0), 180, 32));
    }
    w.end();
    return w.save(out_path, errbuf, errbuf_sz);
}

// Write the in-memory model.  A file that was read from OASIS is written back
// byte for byte; anything else is synthesised: names, placements (arrays kept
// as repetitions), boxes as RECTANGLE records and cell/file properties.
extern "C" int ldb_write_oasis(const ldb_file_t* fh, const char* out_path, char* errbuf, size_t errbuf_sz) {
    auto* f = (const LdbFile*)fh; if (errbuf && errbuf_sz) errbuf[0] = '\0';
    if (!f) { if (errbuf) snprintf(errbuf, errbuf_sz, "null"); return -1; }
    if (f->format == 2 && !f->oasis_raw.empty()) {
        FILE* fp = fopen(out_path, "wb");
        if (!fp) { if (errbuf) snprintf(errbuf, errbuf_sz, "open for write failed"); return -1; }
        const bool ok = fwrite(f->oasis_raw.data(), 1, f->oasis_raw.size(), fp) == f->oasis_raw.size();
        fclose(fp);
        if (!ok) { if (errbuf) snprintf(errbuf, errbuf_sz, "write failed"); return -1; }
        return 0;
    }
    Writer w;
    w.begin(1000);
    std::map<std::string, uint64_t> prop_ref;
    auto prop_name = [&](const std::string& name) {
        auto it = prop_ref.find(name);
        if (it != prop_ref.end()) return it->second;
        const uint64_t ref = prop_ref.size();
        prop_ref[name] = ref;
        w.u8(REC_PROPNAME_REF); w.str(name); w.uint_(ref);
        return ref;
    };
    for (auto& p : f->file_properties) if (!p.name.empty()) prop_name(p.name);
    for (auto& c : f->cells) for (auto& p : c.properties) if (!p.name.empty()) prop_name(p.name);
    std::map<std::string, uint64_t> cell_ref;
    for (auto& c : f->cells) {
        const uint64_t ref = cell_ref.size();
        cell_ref[c.name] = ref;
        w.u8(REC_CELLNAME_REF); w.str(c.name); w.uint_(ref);
    }
    for (auto& p : f->file_properties) if (!p.name.empty()) w.property(prop_ref[p.name], p.value);
    for (auto& c : f->cells) {
        w.u8(REC_CELL_REF); w.uint_(cell_ref[c.name]);
        for (auto& p : c.properties) if (!p.name.empty()) w.property(prop_ref[p.name], p.value);
        for (auto& inst : c.instances) {
            auto it = cell_ref.find(inst.ref_name);
            if (it != cell_ref.end()) w.placement(it->second, inst.tr);
        }
        std::vector<LdbBoxElem> decoded;
        const std::vector<LdbBoxElem>* boxes = &c.boxes;
        if (boxes->empty() && (!c.boxes_compact.empty() || c.boxes_on_disk)) {
            decoded = ldb_load_boxes_on_demand(const_cast<LdbCell&>(c)); boxes = &decoded;
        }
        for (const auto& bx : *boxes) w.rectangle(bx.layer, bx.datatype, bx.x1, bx.y1, bx.x2 - bx.x1, bx.y2 - bx.y1);
    }
    w.end();
    return w.save(out_path, errbuf, errbuf_sz);
}
