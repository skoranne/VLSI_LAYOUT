// File   : db_def2oas.cpp
// Author : Sandeep Koranne (C) 2026. All rights reserved.
// Purpose: DEF (.def or .def.gz) + LEF + standard-cell layout -> OASIS that keeps the DEF names
//          and properties.
//
//   db_def2oas [options] design.def[.gz] out.oas
//     --lef FILE        tech / cell LEF (repeatable): wire widths, via definitions, macro SIZE,
//                       and LEF abstracts for macros missing from --cells
//     --cells FILE      GDS/OASIS holding the standard cells (repeatable; first match wins)
//     --stack SPEC      layer stack as in db_defcheck (default SKY130)
//     --boundary L/D    layer of the DIEAREA / LEF abstract boundary (default 235/4)
//     --no-net-props    do not attach DEF_NET to routing shapes
//     --no-conns        do not store the net connection lists
//     --no-cblock       write uncompressed records
//
// What the OASIS carries (property names in capitals; values are strings; KLayout keeps one
// property per name, so every name occurs once per element with a list of values):
//   placement   S_GDS_PROPERTY (61, component name): the attribute KLayout and Magic use for
//               instance names; DEF_STATUS FIXED|COVER (PLACED is the default and not written);
//               DEF_ATTR one value per other "+ ..." clause (SOURCE TIMING, WEIGHT 5, HALO ...);
//               DEF_PROPERTY name, value, name, value ... from "+ PROPERTY" (quotes removed).
//   shape       DEF_NET (net name) on every routing / via / RECT / pin shape; pin shapes and pin
//               labels also carry DEF_PIN (pin name). Shapes are grouped by layer, then net, so
//               consecutive shapes of one net share the name through PROPERTY-REPEAT (1 byte).
//   pin label   TEXT with the pin name on the pin datatype, at the centre of each pin shape.
//   top cell    DEF_NET_INFO one value per net: "name<TAB>SPECIAL|REGULAR<TAB>+ clauses<TAB>
//               ( inst pin ) ..."; DEF_PIN_INFO one value per pin: "name<TAB>+ clauses";
//               DEF_<KEYWORD> one value per statement for VERSION, DIVIDERCHAR, BUSBITCHARS,
//               TECHNOLOGY, ROW, TRACKS, GCELLGRID, HISTORY, PROPERTYDEFINITIONS and the
//               sections not modelled (REGIONS, GROUPS, BLOCKAGES, FILLS, ...); DEF_UNITS;
//               DIEAREA as a rectangle on the boundary layer.
// Not converted: DEF + POLYGON shapes, diagonal wires and wires on layers without a tech-LEF
// WIDTH are reported (WARNING) and left out; layout-only markers a P&R flow adds to its own
// GDS (e.g. an 81/14 area box) are not in the DEF.
// Check the result with db_defcheck (DEF vs layout XOR) and MAGVIEWER (hover shows names).
#include "layoutdb.h"
#include "layoutdb_hier.h"
#include "layoutdb_internal.hpp"
#include "oasis_codec.hpp"
#include "def_reader.hpp"
#include <zlib.h>
#include <cinttypes>
#include <set>
#include <functional>

using namespace defr;

namespace {

struct Shape { Rect r; int32_t net; int32_t pin; int16_t l, d; };

struct Converter : DefReader {
    std::vector<Shape> shapes;                       // top-cell geometry, layout units
    std::vector<std::string> nets{""}, pins{""};     // index 0: none
    std::unordered_map<std::string, int32_t> net_id;
    int32_t cur_net_id = 0;
    std::vector<std::pair<std::string, std::vector<std::string>>> net_info, pin_info;
    std::map<std::string, int64_t> incomplete_by_layer;
    bool keep_conns = true;

    int32_t net_index(const std::string& n) {
        if (n.empty()) return 0;
        auto it = net_id.find(n);
        if (it != net_id.end()) return it->second;
        int32_t id = (int32_t)nets.size();
        nets.push_back(n); net_id[n] = id;
        return id;
    }
    void probe_rect(int src, int li, const Rect& r, const char*) override {
        if (li < 0) return;
        Rect q{L(r.x1), L(r.y1), L(r.x2), L(r.y2)};
        if (q.x2 <= q.x1 || q.y2 <= q.y1) return;
        const StackLayer& sl = stack[li];
        int32_t net = 0, pin = 0;
        if (src == PIN) {
            if (pins.back() != cur_pin) pins.push_back(cur_pin);
            pin = (int32_t)pins.size() - 1;
        } else {
            if (cur_net_id == 0 || nets[cur_net_id] != cur_net) cur_net_id = net_index(cur_net);
            net = cur_net_id;
        }
        shapes.push_back({q, net, pin, (int16_t)sl.dl, (int16_t)sl.dd});
        if (src == PIN && sl.pl >= 0) shapes.push_back({q, net, -pin, (int16_t)sl.pl, (int16_t)sl.pd});   // -pin: label shape
    }
    void mark_incomplete(int li) override { if (li >= 0) ++incomplete_by_layer[stack[li].name]; }
    void net_end(const std::string& name, bool special, const std::vector<std::string>& attrs,
                 const std::vector<std::pair<std::string, std::string>>& props, const std::vector<std::string>& conns) override {
        std::vector<std::string> v{name, special ? "SPECIAL" : "REGULAR"};
        std::string a;
        for (auto& x : attrs) { if (!a.empty()) a += ' '; a += "+ " + x; }
        for (auto& p : props) { if (!a.empty()) a += ' '; a += "+ PROPERTY " + p.first + " " + p.second; }
        v.push_back(a);
        if (keep_conns) {
            std::string c;
            for (auto& x : conns) { if (!c.empty()) c += ' '; c += "( " + x + " )"; }
            v.push_back(c);
        }
        net_info.push_back({name, std::move(v)});
    }
    void pin_end(const std::string& name, const std::vector<std::string>& attrs) override {
        std::string a;
        for (auto& x : attrs) { if (!a.empty()) a += ' '; a += "+ " + x; }
        pin_info.push_back({name, {name, a}});
    }
};

std::string unquote(const std::string& v) {
    if (v.size() >= 2 && v.front() == '"' && v.back() == '"') return v.substr(1, v.size() - 2);
    return v;
}

// ------------------------------------------------------------------ OASIS writing
enum Rec : uint8_t {
    R_START = 1, R_END = 2, R_CELLNAME = 4, R_TEXTSTRING = 6, R_PROPNAME = 8, R_PROPSTRING = 10, R_CELL = 13,
    R_PLACEMENT = 17, R_TEXT = 19, R_RECTANGLE = 20, R_PROPERTY = 28, R_PROPERTY_REPEAT = 29, R_CBLOCK = 34
};

struct Out : oasis_codec::Encoder {
    // modal state (reset by CELL)
    int64_t m_layer = -1, m_dt = -1;
    int64_t m_tl = -1, m_tt = -1;
    std::string last_prop;                           // key of the last PROPERTY (for REPEAT)
    void cell_reset() { m_layer = m_dt = m_tl = m_tt = -1; last_prop.clear(); }
    void rect(int l, int d, const Rect& r) {
        uint8_t info = 0x78;                         // W H X Y
        if (l != m_layer) info |= 0x01;
        if (d != m_dt) info |= 0x02;
        u8(R_RECTANGLE); u8(info);
        if (info & 0x01) { uint_((uint64_t)l); m_layer = l; }
        if (info & 0x02) { uint_((uint64_t)d); m_dt = d; }
        uint_((uint64_t)(r.x2 - r.x1)); uint_((uint64_t)(r.y2 - r.y1)); sint(r.x1); sint(r.y1);
    }
    void text(uint64_t ref, int l, int d, int64_t x, int64_t y) {
        uint8_t info = 0x60 | 0x18;                  // C N(ref) X Y
        if (l != m_tl) info |= 0x01;
        if (d != m_tt) info |= 0x02;
        u8(R_TEXT); u8(info); uint_(ref);
        if (info & 0x01) { uint_((uint64_t)l); m_tl = l; }
        if (info & 0x02) { uint_((uint64_t)d); m_tt = d; }
        sint(x); sint(y);
    }
    // PROPERTY with string values given as PROPSTRING refs (>= 0) or inline strings
    struct V { int64_t ref = -1; std::string s; bool is_uint = false; uint64_t u = 0; };
    void prop(uint64_t name_ref, const std::vector<V>& vals, bool standard, const std::string& key) {
        if (!key.empty() && key == last_prop) { u8(R_PROPERTY_REPEAT); return; }
        uint8_t n = (uint8_t)std::min<size_t>(vals.size(), 15);
        u8(R_PROPERTY); u8((uint8_t)((n << 4) | 0x06 | (standard ? 1 : 0)));
        uint_(name_ref);
        if (n == 15) uint_(vals.size());
        for (auto& v : vals) {
            if (v.is_uint) { uint_(8); uint_(v.u); }
            else if (v.ref >= 0) { uint_(15); uint_((uint64_t)v.ref); }
            else { uint_(12); str(v.s); }
        }
        last_prop = key;
    }
    void placement(uint64_t cell_ref, const LdbTransform& t) {
        const bool array = t.cols > 1 || t.rows > 1;
        double q = t.angle_deg / 90.0;
        uint8_t info = 0xF0 | (array ? 0x08 : 0) | (t.mirror_x ? 0x01 : 0);
        info |= (uint8_t)((((long long)std::llround(q)) % 4 + 4) % 4 << 1);
        u8(R_PLACEMENT); u8(info); uint_(cell_ref);
        sint(t.ox); sint(t.oy);
        if (!array) return;
        if (t.cols > 1 && t.rows > 1) { uint_(8); uint_((uint64_t)t.cols - 2); uint_((uint64_t)t.rows - 2); gdelta(t.dx, t.dy); gdelta(t.row_dx, t.row_dy); }
        else if (t.cols > 1) { uint_(9); uint_((uint64_t)t.cols - 2); gdelta(t.dx, t.dy); }
        else { uint_(9); uint_((uint64_t)t.rows - 2); gdelta(t.row_dx, t.row_dy); }
    }
};

// Appends `body` to the file as CBLOCKs of about 1 MiB (raw DEFLATE), or as is.
bool flush_body(FILE* fp, std::vector<uint8_t>& body, bool cblock) {
    if (body.empty()) return true;
    if (!cblock) { bool ok = fwrite(body.data(), 1, body.size(), fp) == body.size(); body.clear(); return ok; }
    oasis_codec::Encoder e;
    std::vector<uint8_t> z(compressBound((uLong)body.size()) + 64);
    z_stream s; memset(&s, 0, sizeof s);
    if (deflateInit2(&s, 6, Z_DEFLATED, -15, 9, Z_DEFAULT_STRATEGY) != Z_OK) return false;
    s.next_in = body.data(); s.avail_in = (uInt)body.size();
    s.next_out = z.data(); s.avail_out = (uInt)z.size();
    int rc = deflate(&s, Z_FINISH);
    size_t zn = z.size() - s.avail_out;
    deflateEnd(&s);
    if (rc != Z_STREAM_END) return false;
    e.u8(R_CBLOCK); e.uint_(0); e.uint_(body.size()); e.uint_(zn);
    bool ok = fwrite(e.b.data(), 1, e.b.size(), fp) == e.b.size() && fwrite(z.data(), 1, zn, fp) == zn;
    body.clear();
    return ok;
}

}  // namespace

std::vector<LdbBoxElem> ldb_load_boxes_on_demand(LdbCell& c);   // layoutdb.cpp

int main(int argc, char** argv) {
    std::vector<std::string> lefs, libs, pos;
    std::string stack_spec = kSky130Stack;
    int bl = 235, bd = 4;
    bool net_props = true, conns = true, cblock = true;
    auto usage = [&]() {
        fprintf(stderr, "usage: %s [--lef F]... [--cells lib.{oas,gds}]... [--stack SPEC] [--boundary L/D]\n"
                        "          [--no-net-props] [--no-conns] [--no-cblock] design.def[.gz] out.oas\n", argv[0]);
        return 2;
    };
    for (int i = 1; i < argc; ++i) {
        std::string a = argv[i];
        auto val = [&]() -> std::string { if (i + 1 >= argc) { usage(); exit(2); } return argv[++i]; };
        if (a == "--lef") lefs.push_back(val());
        else if (a == "--cells") libs.push_back(val());
        else if (a == "--stack") stack_spec = val();
        else if (a == "--boundary") { if (!parse_ld(val(), bl, bd)) return usage(); }
        else if (a == "--no-net-props") net_props = false;
        else if (a == "--no-conns") conns = false;
        else if (a == "--no-cblock") cblock = false;
        else if (!a.empty() && a[0] == '-') return usage();
        else pos.push_back(a);
    }
    if (pos.size() != 2) return usage();

    Converter cv;
    cv.keep_statements = true;
    cv.keep_conns = conns;
    if (!parse_stack(stack_spec, cv.stack)) { fprintf(stderr, "bad --stack\n"); return 2; }
    for (size_t i = 0; i < cv.stack.size(); ++i) cv.lidx[cv.stack[i].name] = (int)i;
    std::string err;
    for (auto& l : lefs) if (!parse_lef(l, cv.lef, err)) { fprintf(stderr, "ERROR %s\n", err.c_str()); return 2; }

    if (!parse_def(pos[0], cv, err)) { fprintf(stderr, "ERROR %s\n", err.c_str()); return 2; }
    // pin shapes get their net from the pin's "+ NET <name>" clause: pin_info[k] = (name, {name, "+ NET n + DIRECTION ..."})
    {
        std::unordered_map<std::string, int32_t> pin_net;
        for (auto& pi : cv.pin_info) {
            const std::string& a = pi.second[1];
            size_t p = a.find("+ NET ");
            if (p == std::string::npos) continue;
            size_t s = p + 6, e = a.find(' ', s);
            pin_net[pi.first] = cv.net_index(a.substr(s, e == std::string::npos ? std::string::npos : e - s));
        }
        for (auto& sh : cv.shapes) if (sh.pin != 0) {
            auto it = pin_net.find(cv.pins[(size_t)std::abs(sh.pin)]);
            if (it != pin_net.end()) sh.net = it->second;
        }
    }
    const double upm = (double)cv.def_units * cv.scale;
    printf("DEF %s: design %s, %zu components, %zu nets, %zu pins, %zu shapes\n", pos[0].c_str(), cv.design.c_str(),
           cv.comps.size(), cv.net_info.size(), cv.pin_info.size(), cv.shapes.size());
    for (auto& kv : cv.incomplete_by_layer)
        printf("WARNING %" PRId64 " DEF shapes on %s have no exact geometry (give the tech LEF): written as nothing\n", kv.second, kv.first.c_str());

    // ---- library cells
    std::vector<ldb_file_t*> lib_fh;
    for (auto& l : libs) {
        char e[512] = {0};
        ldb_file_t* f = ldb_open(l.c_str(), e, sizeof e);
        if (!f) { fprintf(stderr, "ERROR cannot open %s: %s\n", l.c_str(), e); return 2; }
        lib_fh.push_back(f);
    }
    struct OutCell { std::string name; LdbCell* src = nullptr; const LefMacro* lef = nullptr; };
    std::vector<OutCell> out_cells;
    std::unordered_map<std::string, size_t> out_index;
    std::function<void(const std::string&)> want = [&](const std::string& name) {
        if (out_index.count(name) || name == cv.design) return;
        OutCell oc; oc.name = name;
        for (auto* fh : lib_fh) {
            LdbFile* lf = (LdbFile*)fh;
            auto it = lf->cell_index.find(name);
            if (it != lf->cell_index.end()) { oc.src = &lf->cells[it->second]; break; }
        }
        if (!oc.src) { auto im = cv.lef.macros.find(name); if (im != cv.lef.macros.end()) oc.lef = &im->second; }
        out_index[name] = out_cells.size();
        out_cells.push_back(oc);
        if (oc.src) for (auto& in : oc.src->instances) want(in.ref_name);
    };
    for (auto& c : cv.comps) want(c.macro);
    int64_t n_lib = 0, n_abs = 0, n_missing = 0;
    for (auto& oc : out_cells) { if (oc.src) ++n_lib; else if (oc.lef) ++n_abs; else { ++n_missing; fprintf(stderr, "WARNING cell %s: not in --cells nor LEF, written empty\n", oc.name.c_str()); } }
    printf("cells: %" PRId64 " from --cells, %" PRId64 " LEF abstracts, %" PRId64 " missing\n", n_lib, n_abs, n_missing);

    // reference box of a macro (lower-left convention of DEF placements), layout units
    auto refbox = [&](const OutCell& oc) -> Rect {
        auto im = cv.lef.macros.find(oc.name);
        if (im != cv.lef.macros.end() && im->second.has_size)
            return {(int64_t)std::llround(-im->second.ox * upm), (int64_t)std::llround(-im->second.oy * upm),
                    (int64_t)std::llround((im->second.w - im->second.ox) * upm), (int64_t)std::llround((im->second.h - im->second.oy) * upm)};
        if (oc.src) {
            auto ib = oc.src->layer_ext.find({bl, bd});
            if (ib != oc.src->layer_ext.end() && ib->second.valid) return {ib->second.xmin, ib->second.ymin, ib->second.xmax, ib->second.ymax};
            if (oc.src->cell_ext.valid) return {oc.src->cell_ext.xmin, oc.src->cell_ext.ymin, oc.src->cell_ext.xmax, oc.src->cell_ext.ymax};
        }
        return {0, 0, 0, 0};
    };

    // ---- write
    FILE* fp = fopen(pos[1].c_str(), "wb");
    if (!fp) { fprintf(stderr, "ERROR cannot write %s\n", pos[1].c_str()); return 2; }
    Out hdr;
    hdr.bytes("%SEMI-OASIS\r\n", 13);
    hdr.u8(R_START); hdr.str("1.0"); hdr.real_whole((uint64_t)std::llround(upm));
    hdr.uint_(0); for (int i = 0; i < 12; ++i) hdr.uint_(0);
    // names: cells, property names, net/pin strings
    uint64_t cref = 0;
    std::unordered_map<std::string, uint64_t> cell_ref;
    for (auto& oc : out_cells) { cell_ref[oc.name] = cref; hdr.u8(R_CELLNAME); hdr.str(oc.name); hdr.uint_(cref++); }
    cell_ref[cv.design] = cref; hdr.u8(R_CELLNAME); hdr.str(cv.design); hdr.uint_(cref++);
    std::map<std::string, uint64_t> pn;
    for (const char* n : {"S_GDS_PROPERTY", "DEF_STATUS", "DEF_ATTR", "DEF_PROPERTY", "DEF_NET", "DEF_PIN",
                          "DEF_NET_INFO", "DEF_PIN_INFO", "DEF_UNITS"}) { uint64_t r = pn.size(); pn[n] = r; hdr.u8(R_PROPNAME); hdr.str(n); hdr.uint_(r); }
    for (auto& st : cv.statements) {
        std::string n = "DEF_" + st.first;
        if (!pn.count(n)) { uint64_t r = pn.size(); pn[n] = r; hdr.u8(R_PROPNAME); hdr.str(n); hdr.uint_(r); }
    }
    // PROPSTRING: net names (ref = net index), then pin names (ref = nets.size() + pin index)
    std::vector<uint8_t> body;
    for (size_t i = 1; i < cv.nets.size(); ++i) { hdr.u8(R_PROPSTRING); hdr.str(cv.nets[i]); hdr.uint_(i); }
    const uint64_t pin_base = cv.nets.size();
    for (size_t i = 1; i < cv.pins.size(); ++i) { hdr.u8(R_PROPSTRING); hdr.str(cv.pins[i]); hdr.uint_(pin_base + i); }
    for (size_t i = 1; i < cv.pins.size(); ++i) { hdr.u8(R_TEXTSTRING); hdr.str(cv.pins[i]); hdr.uint_(i); }
    if (fwrite(hdr.b.data(), 1, hdr.b.size(), fp) != hdr.b.size()) { fprintf(stderr, "ERROR write\n"); return 2; }

    Out o;
    auto spill = [&]() { if (o.b.size() >= (1u << 20)) { if (!flush_body(fp, o.b, cblock)) { fprintf(stderr, "ERROR write\n"); exit(2); } } };
    // library cells
    for (auto& oc : out_cells) {
        o.u8(R_CELL); o.uint_(cell_ref[oc.name]); o.cell_reset();
        if (oc.src) {
            std::vector<LdbBoxElem> dec;
            const std::vector<LdbBoxElem>* bx = &oc.src->boxes;
            if (bx->empty() && (!oc.src->boxes_compact.empty() || oc.src->boxes_on_disk)) { dec = ldb_load_boxes_on_demand(*oc.src); bx = &dec; }
            for (auto& b : *bx) if (b.kind <= 2) o.rect(b.layer, b.datatype, {b.x1, b.y1, b.x2, b.y2});
            for (auto& in : oc.src->instances) {
                auto it = cell_ref.find(in.ref_name);
                if (it == cell_ref.end()) continue;
                o.placement(it->second, in.tr);
                for (auto& kv : in.props) {
                    if (kv.first == "S_GDS_PROPERTY" && !in.name.empty())
                        o.prop(pn["S_GDS_PROPERTY"], {Out::V{-1, "", true, 61}, Out::V{-1, in.name}}, true, "");
                }
            }
        } else if (oc.lef) {
            const LefMacro& m = *oc.lef;
            Rect rb = refbox(oc);
            o.rect(bl, bd, rb);
            for (auto& r : m.rects) {
                auto li = cv.lidx.find(r.layer);
                if (li == cv.lidx.end()) continue;
                const StackLayer& sl = cv.stack[li->second];
                Rect q{(int64_t)std::llround((r.x1 + m.ox) * upm), (int64_t)std::llround((r.y1 + m.oy) * upm),
                       (int64_t)std::llround((r.x2 + m.ox) * upm), (int64_t)std::llround((r.y2 + m.oy) * upm)};
                o.rect(sl.dl, sl.dd, q);
                if (r.pin && sl.pl >= 0) o.rect(sl.pl, sl.pd, q);
            }
        }
        spill();
    }
    // top cell
    o.u8(R_CELL); o.uint_(cell_ref[cv.design]); o.cell_reset();
    {
        std::vector<Out::V> v{Out::V{-1, std::to_string(cv.def_units)}};
        o.prop(pn["DEF_UNITS"], v, false, "");
    }
    // one property per name (readers such as KLayout keep a single property per name), one
    // value per statement / net / pin; net and pin records are tab-separated fields
    {
        std::map<std::string, std::vector<Out::V>> grouped;
        std::vector<std::string> order;
        for (auto& st : cv.statements) {
            const std::string n = "DEF_" + st.first;
            if (!grouped.count(n)) order.push_back(n);
            grouped[n].push_back(Out::V{-1, st.second});
        }
        for (auto& n : order) { o.prop(pn[n], grouped[n], false, ""); spill(); }
        auto record = [](const std::vector<std::string>& f) {
            std::string r;
            for (size_t i = 0; i < f.size(); ++i) { if (i) r += '\t'; r += f[i]; }
            return Out::V{-1, r};
        };
        std::vector<Out::V> nv, pv;
        for (auto& ni : cv.net_info) nv.push_back(record(ni.second));
        for (auto& pi : cv.pin_info) pv.push_back(record(pi.second));
        if (!nv.empty()) { o.prop(pn["DEF_NET_INFO"], nv, false, ""); spill(); }
        if (!pv.empty()) o.prop(pn["DEF_PIN_INFO"], pv, false, "");
    }
    if (cv.has_die) o.rect(bl, bd, {cv.L(cv.die.x1), cv.L(cv.die.y1), cv.L(cv.die.x2), cv.L(cv.die.y2)});
    // placements
    int64_t n_place = 0;
    for (auto& c : cv.comps) {
        auto io = out_index.find(c.macro);
        if (io == out_index.end()) continue;
        Rect rb = refbox(out_cells[io->second]);
        int code = c.orient < 0 ? 0 : c.orient;
        Rect p = orient_rect(code, rb, 0, 0);
        LdbTransform t;
        t.ox = cv.L(c.x) - p.x1; t.oy = cv.L(c.y) - p.y1;
        t.angle_deg = 90.0 * (code & 3); t.mirror_x = code >= 4;
        o.placement(cell_ref[c.macro], t);
        o.prop(pn["S_GDS_PROPERTY"], {Out::V{-1, "", true, 61}, Out::V{-1, c.name}}, true, "");
        if (c.status == "FIXED" || c.status == "COVER") o.prop(pn["DEF_STATUS"], {Out::V{-1, c.status}}, false, "");
        if (!c.attrs.empty()) {
            std::vector<Out::V> v;
            for (auto& a : c.attrs) v.push_back(Out::V{-1, a});
            o.prop(pn["DEF_ATTR"], v, false, "");
        }
        if (!c.props.empty()) {                                  // name, value, name, value, ...
            std::vector<Out::V> v;
            for (auto& pr : c.props) { v.push_back(Out::V{-1, pr.first}); v.push_back(Out::V{-1, unquote(pr.second)}); }
            o.prop(pn["DEF_PROPERTY"], v, false, "");
        }
        ++n_place;
        spill();
    }
    // routing, vias, pins: grouped by layer (readers that compact per layer expect it), then by
    // net so consecutive shapes share DEF_NET through PROPERTY-REPEAT; DEF order otherwise
    std::stable_sort(cv.shapes.begin(), cv.shapes.end(), [](const Shape& a, const Shape& b) {
        if (a.l != b.l) return a.l < b.l;
        if (a.d != b.d) return a.d < b.d;
        return a.net < b.net;
    });
    int64_t n_rect = 0, n_text = 0;
    // PROPERTY records follow the element they belong to; a lone DEF_NET equal to the previous
    // element's is a one-byte PROPERTY-REPEAT
    auto shape_props = [&](const Shape& sh) {
        const int32_t pin = std::abs(sh.pin);
        if (net_props && sh.net > 0)
            o.prop(pn["DEF_NET"], {Out::V{(int64_t)sh.net, ""}}, false, pin ? "" : "N" + std::to_string(sh.net));
        if (pin > 0) o.prop(pn["DEF_PIN"], {Out::V{(int64_t)(pin_base + (uint64_t)pin), ""}}, false, "");
    };
    for (auto& sh : cv.shapes) {
        o.rect(sh.l, sh.d, sh.r);
        shape_props(sh);
        ++n_rect;
        if (sh.pin < 0) {                                       // pin datatype copy: also the label
            o.text((uint64_t)(-sh.pin), sh.l, sh.d, (sh.r.x1 + sh.r.x2) / 2, (sh.r.y1 + sh.r.y2) / 2);
            shape_props(sh);
            ++n_text;
        }
        spill();
    }
    if (!flush_body(fp, o.b, cblock)) { fprintf(stderr, "ERROR write\n"); return 2; }
    Out tail;
    tail.u8(R_END); tail.str(std::string(252, '\0')); tail.uint_(0);
    bool ok = fwrite(tail.b.data(), 1, tail.b.size(), fp) == tail.b.size();
    long size = ftell(fp);
    fclose(fp);
    for (auto* f : lib_fh) ldb_close(f);
    if (!ok) { fprintf(stderr, "ERROR write\n"); return 2; }
    printf("wrote %s: %ld bytes, %zu cells + top %s, %" PRId64 " placements, %" PRId64 " rectangles, %" PRId64 " pin labels, %zu routed nets named\n",
           pos[1].c_str(), size, out_cells.size(), cv.design.c_str(), n_place, n_rect, n_text, cv.nets.size() - 1);
    return n_missing ? 1 : 0;
}
