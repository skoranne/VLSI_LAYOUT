// C API over the streaming OASIS writer: write the in-memory model (ldb_write_oasis,
// ldb_write_oasis_opts) and the synthetic gear test layout.
#include "oasis_writer.hpp"
#include "layoutdb_internal.hpp"
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <string>
#include <vector>

std::vector<LdbBoxElem> ldb_load_boxes_on_demand(LdbCell& c);   // layoutdb.cpp

// ===========================================================================
// C API
// ===========================================================================
using namespace oasis_writer;

// Synthetic gear with circular cut-outs (sampled curves written as POLYGON
// records) carrying cell properties: the 3DIC test layout.
extern "C" int ldb_write_oasis_gear_multigon(const char* out_path, char* errbuf, size_t errbuf_sz) {
    if (errbuf && errbuf_sz) errbuf[0] = '\0';
    Writer w;
    Options o; o.unit = 1000;
    if (!w.open(out_path, o)) { if (errbuf) snprintf(errbuf, errbuf_sz, "%s", w.error().c_str()); return -1; }
    w.add_top_cell("GEAR3DIC");
    w.begin_cell("GEAR3DIC");
    w.property("GEAR_TYPE", { PropValue::str("3DIC_cutout") });
    w.property("MATERIAL", { PropValue::str("3DIC_CUTOUT") });
    auto ring = [](double cx, double cy, double radius, int n) {
        std::vector<Pt> pts;
        for (int i = 0; i < n; ++i) {
            const double a = 2 * M_PI * i / n;
            pts.push_back({ (int64_t)llround(cx + radius * cos(a)), (int64_t)llround(cy + radius * sin(a)) });
        }
        return pts;
    };
    const int teeth = 8; const double R = 2000, r = 1500, tooth = 400;
    std::vector<Pt> gear;
    for (int i = 0; i < teeth * 4; ++i) {
        const double a = (2 * M_PI * i) / (teeth * 4), rad = (i % 4 < 2) ? (R + tooth) : r;
        gear.push_back({ (int64_t)llround(rad * cos(a)), (int64_t)llround(rad * sin(a)) });
    }
    w.polygon(10, 0, gear.data(), gear.size());
    auto c0 = ring(0, 0, 600, 64);
    w.polygon(11, 0, c0.data(), c0.size());
    for (int k = 0; k < 4; ++k) {
        const double a0 = 2 * M_PI * k / 4;
        auto ck = ring(1100 * cos(a0), 1100 * sin(a0), 180, 32);
        w.polygon(11, 0, ck.data(), ck.size());
    }
    w.end_cell();
    if (!w.close()) { if (errbuf) snprintf(errbuf, errbuf_sz, "%s", w.error().c_str()); return -1; }
    return 0;
}

extern "C" void ldb_oasis_write_opts_default(ldb_oasis_write_opts_t* o) {
    if (!o) return;
    o->strict = 1; o->cblock_kb = 4096; o->repetitions = 1; o->validation = 1;
    o->reencode = 0; o->independent_cblocks = 0; o->deflate_level = 6;
}

// Write the in-memory model. A file read from OASIS is written back byte for byte unless
// reencode is set; otherwise the model is encoded with the streaming writer: placements
// (arrays as repetitions), exact polygons/paths when the source kept them (GDSII), else the
// boxes, grouped per layer with rectangle arrays found by RectArrays; cell and file
// properties (standard S_* ones are regenerated, not copied).
extern "C" int ldb_write_oasis_opts(const ldb_file_t* fh, const char* out_path, const ldb_oasis_write_opts_t* po,
                                    char* errbuf, size_t errbuf_sz) {
    auto* f = (const LdbFile*)fh; if (errbuf && errbuf_sz) errbuf[0] = '\0';
    if (!f) { if (errbuf) snprintf(errbuf, errbuf_sz, "null"); return -1; }
    ldb_oasis_write_opts_t def; ldb_oasis_write_opts_default(&def);
    const ldb_oasis_write_opts_t& o = po ? *po : def;
    if (f->format == 2 && !f->oasis_raw.empty() && !o.reencode) {
        FILE* fp = fopen(out_path, "wb");
        if (!fp) { if (errbuf) snprintf(errbuf, errbuf_sz, "open for write failed"); return -1; }
        const bool ok = fwrite(f->oasis_raw.data(), 1, f->oasis_raw.size(), fp) == f->oasis_raw.size();
        fclose(fp);
        if (!ok) { if (errbuf) snprintf(errbuf, errbuf_sz, "write failed"); return -1; }
        return 0;
    }
    Options wo;
    wo.unit = f->dbu_per_micron > 0 ? f->dbu_per_micron : 1000.0;
    wo.strict = o.strict != 0;
    wo.cblock_bytes = o.cblock_kb > 0 ? (size_t)o.cblock_kb * 1024 : 0;
    wo.validation = o.validation;
    wo.independent_cblocks = o.independent_cblocks != 0;
    wo.deflate_level = o.deflate_level > 0 ? o.deflate_level : 6;
    Writer w;
    auto fail = [&]() { if (errbuf) snprintf(errbuf, errbuf_sz, "%s", w.error().c_str()); return -1; };
    if (!w.open(out_path, wo)) return fail();
    auto user_prop = [](const LdbProperty& p) { return !p.name.empty() && p.name.compare(0, 2, "S_") != 0 && oasis_codec::is_nstring(p.name); };
    for (auto& p : f->file_properties) if (user_prop(p)) w.file_property(p.name, { PropValue::str(p.value) });
    for (auto& c : f->cells) {
        w.begin_cell(c.name);
        for (auto& p : c.properties) if (p.target_kind == 1 && user_prop(p)) w.property(p.name, { PropValue::str(p.value) });
        for (auto& inst : c.instances) {
            Transform t; t.x = inst.tr.ox; t.y = inst.tr.oy; t.mag = inst.tr.mag; t.angle = inst.tr.angle_deg; t.mirror = inst.tr.mirror_x != 0;
            if (inst.tr.cols * inst.tr.rows > 1) {
                Repetition r = Repetition::lattice(inst.tr.cols, inst.tr.dx, inst.tr.dy, inst.tr.rows, inst.tr.row_dx, inst.tr.row_dy);
                w.placement(inst.ref_name, t, &r);
            } else w.placement(inst.ref_name, t);
        }
        if (!c.polys.empty()) {                                  // exact source shapes
            std::vector<Pt> pts;
            for (auto& p : c.polys) {
                pts.clear();
                for (auto& q : p.pts) pts.push_back({ q.x, q.y });
                if (p.kind == 1) w.path((uint64_t)p.layer, (uint64_t)p.datatype, pts.data(), pts.size(), p.width / 2, p.ext_begin, p.ext_end);
                else w.polygon((uint64_t)p.layer, (uint64_t)p.datatype, pts.data(), pts.size());
            }
        }
        if (c.polys.empty() || c.polys_extra) {                  // rectangles (all boxes, or kind 2)
            std::vector<LdbBoxElem> decoded;
            const std::vector<LdbBoxElem>* boxes = &c.boxes;
            if (boxes->empty() && (!c.boxes_compact.empty() || c.boxes_on_disk)) {
                decoded = ldb_load_boxes_on_demand(const_cast<LdbCell&>(c)); boxes = &decoded;
            }
            if (c.polys_extra) {
                std::vector<LdbBoxElem> r;
                for (auto& b : *boxes) if (b.kind == 2) r.push_back(b);
                decoded.swap(r); boxes = &decoded;
            }
            std::vector<uint32_t> idx(boxes->size());
            for (uint32_t i = 0; i < idx.size(); ++i) idx[i] = i;
            const auto& B = *boxes;
            std::sort(idx.begin(), idx.end(), [&](uint32_t a, uint32_t b) {
                const LdbBoxElem& p = B[a]; const LdbBoxElem& q = B[b];
                if (p.layer != q.layer) return p.layer < q.layer;
                if (p.datatype != q.datatype) return p.datatype < q.datatype;
                const int64_t pw = p.x2 - p.x1, qw = q.x2 - q.x1, ph = p.y2 - p.y1, qh = q.y2 - q.y1;
                if (pw != qw) return pw < qw;
                if (ph != qh) return ph < qh;
                if (p.y1 != q.y1) return p.y1 < q.y1;
                return p.x1 < q.x1;
            });
            for (size_t i = 0; i < idx.size();) {
                const int L = B[idx[i]].layer, D = B[idx[i]].datatype;
                size_t j = i;
                if (o.repetitions) {
                    RectArrays ra(w, (uint64_t)L, (uint64_t)D);
                    for (; j < idx.size() && B[idx[j]].layer == L && B[idx[j]].datatype == D; ++j) {
                        const LdbBoxElem& b = B[idx[j]];
                        ra.add(b.x1, b.y1, b.x2 - b.x1, b.y2 - b.y1);
                    }
                    ra.finish();
                } else {
                    for (; j < idx.size() && B[idx[j]].layer == L && B[idx[j]].datatype == D; ++j) {
                        const LdbBoxElem& b = B[idx[j]];
                        w.rectangle((uint64_t)L, (uint64_t)D, b.x1, b.y1, b.x2 - b.x1, b.y2 - b.y1);
                    }
                }
                i = j;
            }
        }
        for (auto& t : c.texts) w.text((uint64_t)t.layer, (uint64_t)t.texttype, t.x, t.y, t.str);
        w.end_cell();
        if (!w.ok()) return fail();
    }
    if (!w.close()) return fail();
    return 0;
}

extern "C" int ldb_write_oasis(const ldb_file_t* fh, const char* out_path, char* errbuf, size_t errbuf_sz) {
    return ldb_write_oasis_opts(fh, out_path, nullptr, errbuf, errbuf_sz);
}
