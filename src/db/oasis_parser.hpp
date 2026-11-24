#pragma once
// OASIS record decoder shared by oasis.cpp (whole-file parse / streaming) and
// oasis_sidecar.cpp (per-CBLOCK decode for the MAGVIEWER sidecar index).
// See oasis.cpp for what each record turns into.
//
// Speculative decoding (Parser::track): the sidecar builder decodes every CBLOCK in
// parallel without knowing the modal state it starts with. Each modal variable then
// starts undefined; setting one marks it defined, and the first *use* of an undefined
// one throws Parser::Dependent. Everything emitted before that point is exact. A block
// that never throws is independent of what came before it; one that throws is decoded
// again from its true start state, which the builder reconstructs in file order.
#include "layoutdb_internal.hpp"
#include "nvcomp_support.hpp"
#include "fracture.hpp"
#include "oasis_codec.hpp"
#include <cstring>
#include <cstdio>
#include <array>
#include <algorithm>
#include <cmath>
#include <chrono>
#include <cstdlib>
#include <future>
#include <memory>

namespace oasis_parse {

using oasis_codec::ParseError; using oasis_codec::Reader; using oasis_codec::Repetition;
using oasis_codec::read_repetition; using oasis_codec::read_point_list;
using ldbgeom::Pt; using ldbgeom::Rect;
using ldbgeom::is_manhattan; using ldbgeom::fracture_manhattan; using ldbgeom::fracture_general; using ldbgeom::path_to_rects;

// ---------------------------------------------------------------------------
// Parser state
// ---------------------------------------------------------------------------
struct CellRef { bool by_number = false; uint64_t number = 0; std::string name; };
struct PInst { CellRef ref; LdbTransform tr; };
struct PText { size_t index; uint64_t ref; };                    // text string by reference, resolved at finish
struct PCell { CellRef id; LdbCell cell; std::vector<PInst> insts; std::vector<PText> text_refs; };
// kind: 0 file, 1 cell, 2 element, 3 placement, 4 CELLNAME (cellname_ref: the cell it names)
struct PProp { long cell; bool name_by_number; uint64_t name_number; std::string name;
               bool value_by_number; uint64_t value_number; std::string value;
               int kind = 0; uint64_t cellname_ref = 0; };

struct NameTable {
    std::map<uint64_t, std::string> names; uint64_t next_implicit = 0;
    void add_implicit(const std::string& s) { names[next_implicit++] = s; }
    void add(uint64_t ref, const std::string& s) { names[ref] = s; }
    const std::string* find(uint64_t ref) const { auto it = names.find(ref); return it == names.end() ? nullptr : &it->second; }
};

struct Modal {
    bool relative = false;
    int64_t place_x = 0, place_y = 0, geom_x = 0, geom_y = 0, text_x = 0, text_y = 0;
    uint64_t layer = 0, datatype = 0, textlayer = 0, texttype = 0;
    int64_t geom_w = 0, geom_h = 0;
    int64_t half_width = 0, ext_start = 0, ext_end = 0;
    uint64_t ctrap_type = 0;
    int64_t radius = 0;
    CellRef place_cell;
    Repetition rep;
    std::vector<Pt> polygon_pts, path_pts;
    bool text_by_ref = false; uint64_t text_ref = 0; std::string text_str;   // modal text-string
    bool prop_by_number = false; uint64_t prop_number = 0; std::string prop_name;
    bool value_by_number = false; uint64_t value_number = 0; std::string value;
    void reset_for_cell() { relative = false; place_x = place_y = geom_x = geom_y = text_x = text_y = 0; }
};

// ---------------------------------------------------------------------------
// Batched CBLOCK decompression
//
// A scan pass (Parser with scan_only=true) records every top-level CBLOCK; CblockSource then
// inflates them in windows of LDB_CBLOCK_BATCH_MB (default 256 MB uncompressed) on the GPU
// (nvCOMP) or a CPU thread pool, prefetching window k+1 asynchronously while the parser is
// consuming window k. The real parse fetches block i by index (verifying its file position),
// so parsing order, modal state and results are identical to the serial path.
// ---------------------------------------------------------------------------
struct CblockRef { const uint8_t* src; uint64_t comp_type, uncomp_len, comp_len; };

class CblockSource {
public:
    CblockSource(std::vector<CblockRef> refs, db::CblockBackend be, db::NvcompStatus& st)
        : refs_(std::move(refs)), be_(be), st_(st) {
        const char* e = std::getenv("LDB_CBLOCK_BATCH_MB");
        size_t mb = e && *e ? (size_t)std::strtoull(e, nullptr, 10) : 256;
        window_bytes_ = std::max<size_t>(mb, 1) * 1024 * 1024;
        // GPU windows need nvCOMP's output alignment; it is fetched lazily (first GPU window)
        // so CPU-only decoding never creates a CUDA context.
    }
    ~CblockSource() { if (next_.valid()) next_.wait(); }

    // Pointer/length of decompressed block i, or nullptr if i does not match the scan
    // (caller then decodes inline). Throws nothing.
    bool get(size_t i, const uint8_t* src, uint64_t comp_len, uint64_t uncomp_len, const uint8_t*& p, size_t& n) {
        if (failed_ || i >= refs_.size()) return false;
        const CblockRef& r = refs_[i];
        if (r.src != src || r.comp_len != comp_len || r.uncomp_len != uncomp_len) { failed_ = true; return false; }
        if (!cur_ || i < cur_->first || i >= cur_->last) {
            const auto t0 = std::chrono::steady_clock::now();
            if (next_.valid()) {
                std::unique_ptr<Window> w = next_.get();
                if (w && i >= w->first && i < w->last) cur_ = std::move(w);
                else cur_ = decode(i);
            } else {
                cur_ = decode(i);
            }
            db::cblock_stats().wait_ms +=
                std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - t0).count();
            if (cur_->last < refs_.size()) {
                const size_t start = cur_->last;
                next_ = std::async(std::launch::async, [this, start]() { return decode(start); });
            }
        }
        const size_t k = i - cur_->first;
        if (!cur_->jobs[k].ok) return false;
        p = cur_->buf.p + cur_->jobs[k].out_off;
        n = (size_t)r.uncomp_len;
        return true;
    }
    size_t count() const { return refs_.size(); }

private:
    struct Window {
        size_t first = 0, last = 0;
        std::vector<db::CblockJob> jobs;
        db::HostBuffer buf;
    };
    std::unique_ptr<Window> decode(size_t first) {
        std::unique_ptr<Window> w(new Window);
        w->first = first;
        const db::CblockBackend be = choose_backend(refs_.size() - first);
        size_t align = 8;
        if (be == db::CblockBackend::Gpu) {
            if (gpu_align_ == 0) gpu_align_ = db::cblock_output_alignment(st_);
            align = gpu_align_;
        }
        size_t off = 0, i = first;
        for (; i < refs_.size(); ++i) {
            const CblockRef& r = refs_[i];
            if (i > first && off + r.uncomp_len > window_bytes_) break;
            db::CblockJob j;
            j.src = r.src; j.comp_len = (size_t)r.comp_len; j.uncomp_len = (size_t)r.uncomp_len;
            j.comp_type = (uint8_t)r.comp_type;
            j.out_off = off;
            off += (size_t)r.uncomp_len;
            off = (off + align - 1) / align * align;
            w->jobs.push_back(j);
        }
        w->last = i;
        w->buf.reserve(off, be == db::CblockBackend::Gpu);
        const auto t0 = std::chrono::steady_clock::now();
        db::decompress_cblock_batch(w->jobs, w->buf.p, be, st_);
        const double s = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
        if (be_ == db::CblockBackend::Auto && st_.available) {
            double& rate = (be == db::CblockBackend::Gpu) ? gpu_rate_ : cpu_rate_;
            rate = (double)off / std::max(s, 1e-9);           // bytes/s of decompressed output
        }
        ++windows_done_;
        return w;
    }
    // auto: calibrate on the first two windows (CPU, then GPU) and keep the faster backend.
    // On GB10 the 20 Grace cores out-run nvCOMP for typical KLayout/OpenROAD CBLOCK sizes
    // (~1-2 MB, a few hundred per file); on machines/files where the GPU wins it is chosen.
    db::CblockBackend choose_backend(size_t nblocks) {
        if (be_ != db::CblockBackend::Auto || !st_.available) return db::resolve_backend(be_, nblocks, st_);
        if (windows_done_ == 0 || refs_.size() < 2) return db::CblockBackend::Cpu;
        if (windows_done_ == 1) return db::CblockBackend::Gpu;
        return gpu_rate_ > cpu_rate_ ? db::CblockBackend::Gpu : db::CblockBackend::Cpu;
    }
    std::vector<CblockRef> refs_;
    db::CblockBackend be_;
    db::NvcompStatus& st_;
    size_t window_bytes_ = 0, gpu_align_ = 0;
    size_t windows_done_ = 0;
    double cpu_rate_ = 0.0, gpu_rate_ = 0.0;
    bool failed_ = false;
    std::unique_ptr<Window> cur_;
    std::future<std::unique_ptr<Window>> next_;
};

struct StreamHier {};                        // streaming parse met a PLACEMENT

struct Parser {
    LdbFile* f;
    bool scan_only = false;                  // CBLOCK pre-scan: decode records, emit nothing
    std::vector<CblockRef>* scan_refs = nullptr;
    CblockSource* src = nullptr;             // pre-decoded CBLOCKs (real parse)
    size_t top_cblock = 0;                   // index of the next top-level CBLOCK
    int cblock_depth = 0;
    std::vector<PCell> cells; long cur = -1;
    NameTable cellnames, textstrings, propnames, propstrings;
    std::vector<PProp> props;
    // Property association (31.8, 31.9): the closest preceding record other than PROPERTY, PAD,
    // CBLOCK (and XYABSOLUTE/XYRELATIVE). 0 file, 1 cell, 2 element, 3 placement, 4 CELLNAME,
    // 5 a record without properties in the model (other <name> records, XNAME, XELEMENT).
    int ptarget = 0; uint64_t ptarget_cellname = 0, last_cellname_ref = 0;
    Modal m;
    db::NvcompStatus nvst;
    size_t cblocks = 0, approximated = 0;
    bool saw_end = false, want_props = false;
    bool keep_polys = false;                 // also store exact non-rectangle shapes (LdbCell::polys)
    double unit = 0;                         // START unit: grid steps per micron
    std::vector<Rect> rects;                 // scratch
    std::vector<Pt> verts;             // scratch
    // Streaming mode (ldb_stream_boxes): boxes go straight to the callback, nothing is stored.
    // Valid for flat files only; a PLACEMENT aborts with StreamHier so the caller can fall back.
    ldb_box_cb stream_cb = nullptr;
    void* stream_ud = nullptr;
    size_t streamed = 0;
    size_t skip = 0;                         // drop this many boxes first (re-decode of a dependent block)

    // --- speculative decoding (see the header comment) ---------------------------
    struct Dependent {};
    enum : uint32_t {
        D_REL = 1u << 0, D_GX = 1u << 1, D_GY = 1u << 2, D_PX = 1u << 3, D_PY = 1u << 4,
        D_LAYER = 1u << 5, D_DT = 1u << 6, D_GW = 1u << 7, D_GH = 1u << 8, D_HW = 1u << 9,
        D_EXT = 1u << 10, D_CT = 1u << 11, D_RAD = 1u << 12, D_PCELL = 1u << 13, D_REP = 1u << 14,
        D_PPTS = 1u << 15, D_HPTS = 1u << 16,
        D_CELL_RESET = D_REL | D_GX | D_GY | D_PX | D_PY   // what a CELL record defines
    };
    bool track = false;
    uint32_t def = 0;                        // modal variables defined so far (track mode)
    void use(uint32_t b) const { if (track && (def & b) != b) throw Dependent{}; }
    void set(uint32_t b) { def |= b; }

    static const Repetition& single() { static const Repetition one; return one; }

    // --- geometry sinks ----------------------------------------------------
    void add_box(uint64_t layer, uint64_t dt, int kind, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
        if (stream_cb) {
            if (skip) { --skip; return; }
            stream_cb(stream_ud, "", (int)layer, (int)dt, kind, std::min(x1, x2), std::min(y1, y2), std::max(x1, x2), std::max(y1, y2));
            ++streamed;
            return;
        }
        LdbCell& c = cells[(size_t)cur].cell;
        LdbBoxElem be; be.layer = (int)layer; be.datatype = (int)dt; be.kind = kind;
        be.x1 = std::min(x1, x2); be.x2 = std::max(x1, x2); be.y1 = std::min(y1, y2); be.y2 = std::max(y1, y2);
        c.boxes.push_back(be);
        ldb_extent_t& le = c.layer_ext[std::make_pair(be.layer, be.datatype)];
        ldb_extent_add_point(le, be.x1, be.y1); ldb_extent_add_point(le, be.x2, be.y2);
        ldb_extent_add_point(c.cell_ext, be.x1, be.y1); ldb_extent_add_point(c.cell_ext, be.x2, be.y2);
    }
    void emit(Reader& s, const Repetition& rep, int kind, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
        if (scan_only) return;
        if (cur < 0) s.fail("geometry outside a cell");
        rep.for_each([&](int64_t ox, int64_t oy) { add_box(m.layer, m.datatype, kind, x1 + ox, y1 + oy, x2 + ox, y2 + oy); });
    }
    void emit_rects(Reader& s, const Repetition& rep, int kind) {
        if (scan_only) return;
        if (cur < 0) s.fail("geometry outside a cell");
        rep.for_each([&](int64_t ox, int64_t oy) {
            for (auto& r : rects) add_box(m.layer, m.datatype, kind, r[0] + ox, r[1] + oy, r[2] + ox, r[3] + oy);
        });
    }
    void emit_bbox_of(Reader& s, const Repetition& rep, int kind, const std::vector<Pt>& v, int64_t grow) {
        if (v.empty()) return;
        int64_t x1 = v[0].x, x2 = v[0].x, y1 = v[0].y, y2 = v[0].y;
        for (auto& q : v) { x1 = std::min(x1, q.x); x2 = std::max(x2, q.x); y1 = std::min(y1, q.y); y2 = std::max(y2, q.y); }
        ++approximated;
        emit(s, rep, kind, x1 - grow, y1 - grow, x2 + grow, y2 + grow);
    }

    // Exact shape for the model: `verts` as a polygon (kind 0) or path centreline (kind 1).
    void keep_poly(const Repetition& rep, int kind, int64_t hw = 0) {
        if (!keep_polys || stream_cb || scan_only || cur < 0) return;
        LdbCell& c = cells[(size_t)cur].cell;
        c.polys_extra = true;
        rep.for_each([&](int64_t ox, int64_t oy) {
            LdbPolyElem pe; pe.layer = (int)m.layer; pe.datatype = (int)m.datatype; pe.kind = kind;
            pe.pts.reserve(verts.size());
            for (auto& q : verts) pe.pts.push_back({ q.x + ox, q.y + oy });
            if (kind == 1) { pe.width = 2 * hw; pe.pathtype = 4; pe.ext_begin = m.ext_start; pe.ext_end = m.ext_end; }
            c.polys.push_back(std::move(pe));
        });
    }

    // Fill `verts` (a closed polygon) as rectangles.
    void emit_polygon(Reader& s, const Repetition& rep) {
        if (scan_only) return;
        keep_poly(rep, 0);
        rects.clear();
        if (is_manhattan(verts)) fracture_manhattan(verts, rects);
        else { fracture_general(verts, rects); ++approximated; }
        emit_rects(s, rep, 0);
    }

    // --- common element fields -----------------------------------------------
    // X = bit 4, Y = bit 3. bx/by: tracking bits of x and y (0 = not tracked: text).
    void read_xy(Reader& s, uint8_t info, int64_t& x, int64_t& y, uint32_t bx = D_GX, uint32_t by = D_GY) {
        coord(s, (info & 0x10) != 0, x, bx);
        coord(s, (info & 0x08) != 0, y, by);
    }
    void coord(Reader& s, bool present, int64_t& v, uint32_t b) {
        if (!present) { if (b) use(b); return; }
        const int64_t d = s.sint();
        if (b) { use(D_REL); if (m.relative) use(b); set(b); }
        v = m.relative ? v + d : d;
    }
    void modal_u(Reader& s, bool present, uint64_t& v, uint32_t b) {
        if (present) { v = s.uint_(); set(b); } else use(b);
    }
    void modal_i(Reader& s, bool present, int64_t& v, uint32_t b) {
        if (present) { v = (int64_t)s.uint_(); set(b); } else use(b);
    }
    void read_layer(Reader& s, uint8_t info) {                         // L = bit 0, D = bit 1
        modal_u(s, (info & 0x01) != 0, m.layer, D_LAYER);
        modal_u(s, (info & 0x02) != 0, m.datatype, D_DT);
    }
    void read_rep_into(Reader& s) {
        if (track && s.off < s.n && s.p[s.off] == 0) use(D_REP);       // type 0: reuse the modal one
        read_repetition(s, m.rep); set(D_REP);
    }
    const Repetition& read_rep(Reader& s, uint8_t info) {              // R = bit 2
        if (!(info & 0x04)) return single();
        read_rep_into(s); return m.rep;
    }

    // --- records -------------------------------------------------------------
    void start_cell(const CellRef& id) {
        PCell pc; pc.id = id; cells.push_back(std::move(pc));
        cur = (long)cells.size() - 1; m.reset_for_cell(); set(D_CELL_RESET);
    }

    void placement(Reader& s, bool general) {
        if (stream_cb && !scan_only) throw StreamHier{};
        if (cur < 0 && !scan_only) s.fail("PLACEMENT outside a cell");
        uint8_t info = s.u8();
        if (info & 0x80) {
            m.place_cell = CellRef();
            if (info & 0x40) { m.place_cell.by_number = true; m.place_cell.number = s.uint_(); }
            else m.place_cell.name = s.str();
            set(D_PCELL);
        } else use(D_PCELL);
        double mag = 1.0, angle;
        if (general) { if (info & 0x04) mag = s.real(); angle = (info & 0x02) ? s.real() : 0.0; }
        else angle = 90.0 * ((info >> 1) & 3);
        coord(s, (info & 0x20) != 0, m.place_x, D_PX);
        coord(s, (info & 0x10) != 0, m.place_y, D_PY);
        const Repetition* rep = &single();
        if (info & 0x08) { read_rep_into(s); rep = &m.rep; }
        if (scan_only) return;
        PInst pi; pi.ref = m.place_cell;
        pi.tr.mag = mag; pi.tr.angle_deg = angle; pi.tr.mirror_x = (info & 0x01) ? 1 : 0;
        auto& insts = cells[(size_t)cur].insts;
        if (rep->regular) {
            pi.tr.ox = m.place_x; pi.tr.oy = m.place_y;
            pi.tr.cols = rep->cols; pi.tr.rows = rep->rows;
            pi.tr.dx = rep->dx; pi.tr.dy = rep->dy; pi.tr.row_dx = rep->rdx; pi.tr.row_dy = rep->rdy;
            insts.push_back(pi);
        } else {
            for (auto& o : rep->offs) { pi.tr.ox = m.place_x + o.x; pi.tr.oy = m.place_y + o.y; insts.push_back(pi); }
        }
    }

    void text(Reader& s) {
        uint8_t info = s.u8();
        const bool store = keep_polys && !stream_cb && !scan_only && cur >= 0;
        if (info & 0x40) {
            if (info & 0x20) { m.text_by_ref = true; m.text_ref = s.uint_(); }
            else if (store) { m.text_by_ref = false; m.text_str = s.str(); }
            else s.skip_str();
        }
        if (info & 0x01) m.textlayer = s.uint_();
        if (info & 0x02) m.texttype = s.uint_();
        read_xy(s, info, m.text_x, m.text_y, 0, 0);        // texts carry no area: not tracked
        const Repetition* rep = &single();
        if (info & 0x04) {                                  // a new repetition is modal for shapes too
            const bool reuse = s.off < s.n && s.p[s.off] == 0;
            read_repetition(s, m.rep);
            if (!reuse) set(D_REP);
            rep = &m.rep;
        }
        if (!store) return;
        PCell& pc = cells[(size_t)cur];
        rep->for_each([&](int64_t ox, int64_t oy) {
            LdbTextElem t; t.layer = (int)m.textlayer; t.texttype = (int)m.texttype; t.x = m.text_x + ox; t.y = m.text_y + oy;
            if (m.text_by_ref) pc.text_refs.push_back({ pc.cell.texts.size(), m.text_ref }); else t.str = m.text_str;
            pc.cell.texts.push_back(std::move(t));
        });
    }

    void rectangle(Reader& s) {
        uint8_t info = s.u8();
        read_layer(s, info);
        if ((info & 0x80) && (info & 0x20)) s.fail("RECTANGLE with S = 1 and H = 1");   // 25.7
        modal_i(s, (info & 0x40) != 0, m.geom_w, D_GW);
        if (info & 0x80) { m.geom_h = m.geom_w; set(D_GH); }           // square
        else modal_i(s, (info & 0x20) != 0, m.geom_h, D_GH);
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        emit(s, rep, 2, m.geom_x, m.geom_y, m.geom_x + m.geom_w, m.geom_y + m.geom_h);
    }

    void polygon(Reader& s) {
        uint8_t info = s.u8();
        read_layer(s, info);
        if (info & 0x20) { read_point_list(s, true, m.polygon_pts); set(D_PPTS); } else use(D_PPTS);
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        verts.clear(); verts.push_back({ m.geom_x, m.geom_y });
        for (auto& q : m.polygon_pts) verts.push_back({ m.geom_x + q.x, m.geom_y + q.y });
        emit_polygon(s, rep);
    }

    void path(Reader& s) {
        uint8_t info = s.u8();
        read_layer(s, info);
        modal_i(s, (info & 0x40) != 0, m.half_width, D_HW);
        if (!(info & 0x80)) use(D_EXT);
        if (info & 0x80) {
            uint64_t scheme = s.uint_();
            if (((scheme >> 2) & 3) == 0 || (scheme & 3) == 0) use(D_EXT);
            if (((scheme >> 2) & 3) == 2 || (scheme & 3) == 2) use(D_HW);
            set(D_EXT);
            auto ext = [&](unsigned code, int64_t& v) {
                if (code == 1) v = 0; else if (code == 2) v = m.half_width; else if (code == 3) v = s.sint();
            };
            ext((unsigned)((scheme >> 2) & 3), m.ext_start);
            ext((unsigned)(scheme & 3), m.ext_end);
        }
        if (info & 0x20) { read_point_list(s, false, m.path_pts); set(D_HPTS); } else use(D_HPTS);
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        const int64_t hw = m.half_width;
        if (m.path_pts.empty()) return;
        verts.clear(); verts.push_back({ m.geom_x, m.geom_y });
        for (auto& q : m.path_pts) verts.push_back({ m.geom_x + q.x, m.geom_y + q.y });
        keep_poly(rep, 1, hw);                       // exact path, zero-width centrelines included
        if (hw == 0) return;                         // no area: no boxes
        rects.clear();
        if (!path_to_rects(verts, hw, m.ext_start, m.ext_end, rects)) {             // any-angle: outline box
            int64_t x1, y1, x2, y2;
            if (ldbgeom::path_bbox(verts, hw, m.ext_start, m.ext_end, x1, y1, x2, y2)) { ++approximated; emit(s, rep, 1, x1, y1, x2, y2); }
            return;
        }
        emit_rects(s, rep, 1);
    }

    void trapezoid(Reader& s, uint8_t id) {
        uint8_t info = s.u8();
        read_layer(s, info);
        modal_i(s, (info & 0x40) != 0, m.geom_w, D_GW);
        modal_i(s, (info & 0x20) != 0, m.geom_h, D_GH);
        int64_t da = (id == 23 || id == 24) ? s.sint() : 0;
        int64_t db_ = (id == 23 || id == 25) ? s.sint() : 0;
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        const int64_t x = m.geom_x, y = m.geom_y, w = m.geom_w, h = m.geom_h;
        if (da == 0 && db_ == 0) { emit(s, rep, 2, x, y, x + w, y + h); return; }   // a rectangle
        // Horizontal: delta-a = x(top left) - x(bottom left),
        //             delta-b = x(top right) - x(bottom right).
        // Vertical:   delta-a = y(bottom left) - y(bottom right),
        //             delta-b = y(top left) - y(top right).
        verts.clear();
        if (!(info & 0x80)) {       // horizontal: parallel sides at y and y + h
            verts.push_back({ x + std::max<int64_t>(-da, 0), y });
            verts.push_back({ x + w - std::max<int64_t>(db_, 0), y });
            verts.push_back({ x + w + std::min<int64_t>(db_, 0), y + h });
            verts.push_back({ x + std::max<int64_t>(da, 0), y + h });
        } else {                    // vertical: parallel sides at x and x + w
            verts.push_back({ x, y + std::max<int64_t>(da, 0) });
            verts.push_back({ x + w, y + std::max<int64_t>(-da, 0) });
            verts.push_back({ x + w, y + h - std::max<int64_t>(db_, 0) });
            verts.push_back({ x, y + h + std::min<int64_t>(db_, 0) });
        }
        emit_polygon(s, rep);
    }

    void ctrapezoid(Reader& s) {
        uint8_t info = s.u8();
        read_layer(s, info);
        modal_u(s, (info & 0x80) != 0, m.ctrap_type, D_CT);
        const uint64_t t = m.ctrap_type;
        if (t > 25) s.fail("invalid CTRAPEZOID type " + std::to_string(t));
        const oasis_codec::CtrapRule cr = oasis_codec::ctrap_rule(t);      // 29.6
        const bool no_h = cr.no_h, no_w = cr.no_w;
        if (no_h && (info & 0x20)) s.fail("CTRAPEZOID type " + std::to_string(t) + " with H = 1");
        if (no_w && (info & 0x40)) s.fail("CTRAPEZOID type " + std::to_string(t) + " with W = 1");
        if (!no_w) modal_i(s, (info & 0x40) != 0, m.geom_w, D_GW);
        if (!no_h) modal_i(s, (info & 0x20) != 0, m.geom_h, D_GH);
        oasis_codec::ctrap_implied_box(t, m.geom_w, m.geom_h);           // 29.7
        set(D_GW | D_GH);
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        const int64_t x = m.geom_x, y = m.geom_y, w = m.geom_w, h = m.geom_h;
        // 29.8: shape constraints.
        if (!oasis_codec::ctrap_consistent(t, w, h))
            s.fail("CTRAPEZOID type " + std::to_string(t) + " with inconsistent width/height");
        if (t == 24 || t == 25) { emit(s, rep, 2, x, y, x + w, y + h); return; }    // rectangle / square
        // Vertices of the 24 compact shapes, relative to (x, y); every slanted
        // side is at 45 degrees.  Triangles repeat their last vertex.
        const int64_t V[24][4][2] = {
            { {0,0}, {w,0},   {w-h,h}, {0,h}   },   //  0  horizontal, right side slanted, top short
            { {0,0}, {w-h,0}, {w,h},   {0,h}   },   //  1  right side slanted, bottom short
            { {0,0}, {w,0},   {w,h},   {h,h}   },   //  2  left side slanted, top short
            { {h,0}, {w,0},   {w,h},   {0,h}   },   //  3  left side slanted, bottom short
            { {0,0}, {w,0},   {w-h,h}, {h,h}   },   //  4  both slanted, top short
            { {h,0}, {w-h,0}, {w,h},   {0,h}   },   //  5  both slanted, bottom short
            { {0,0}, {w-h,0}, {w,h},   {h,h}   },   //  6  parallelogram leaning right
            { {h,0}, {w,0},   {w-h,h}, {0,h}   },   //  7  parallelogram leaning left
            { {0,0}, {w,0},   {w,h-w}, {0,h}   },   //  8  vertical, top slanted, right short
            { {0,0}, {w,0},   {w,h},   {0,h-w} },   //  9  top slanted, left short
            { {0,0}, {w,w},   {w,h},   {0,h}   },   // 10  bottom slanted, right short
            { {0,w}, {w,0},   {w,h},   {0,h}   },   // 11  bottom slanted, left short
            { {0,0}, {w,w},   {w,h-w}, {0,h}   },   // 12  both slanted, right short
            { {0,w}, {w,0},   {w,h},   {0,h-w} },   // 13  both slanted, left short
            { {0,0}, {w,w},   {w,h},   {0,h-w} },   // 14  parallelogram rising
            { {0,w}, {w,0},   {w,h-w}, {0,h}   },   // 15  parallelogram falling
            { {0,0}, {w,0},   {0,w},   {0,w}   },   // 16  triangle, right angle bottom left
            { {0,0}, {w,w},   {0,w},   {0,w}   },   // 17  right angle top left
            { {0,0}, {w,0},   {w,w},   {w,w}   },   // 18  right angle bottom right
            { {0,w}, {w,0},   {w,w},   {w,w}   },   // 19  right angle top right
            { {0,0}, {2*h,0}, {h,h},   {h,h}   },   // 20  apex up
            { {0,h}, {h,0},   {2*h,h}, {2*h,h} },   // 21  apex down
            { {0,0}, {w,w},   {0,2*w}, {0,2*w} },   // 22  apex right
            { {0,w}, {w,0},   {w,2*w}, {w,2*w} },   // 23  apex left
        };
        verts.clear();
        for (int i = 0; i < 4; ++i) {
            const Pt q{ x + V[t][i][0], y + V[t][i][1] };
            if (verts.empty() || verts.back().x != q.x || verts.back().y != q.y) verts.push_back(q);
        }
        emit_polygon(s, rep);
    }

    void circle(Reader& s) {
        uint8_t info = s.u8();
        read_layer(s, info);
        modal_i(s, (info & 0x20) != 0, m.radius, D_RAD);
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        if (scan_only || m.radius <= 0) return;
        verts.clear();
        for (int i = 0; i < 64; ++i) {
            const double a = 2.0 * M_PI * i / 64.0;
            verts.push_back({ m.geom_x + (int64_t)std::llround((double)m.radius * std::cos(a)),
                              m.geom_y + (int64_t)std::llround((double)m.radius * std::sin(a)) });
        }
        emit_polygon(s, rep);
    }

    void property(Reader& s) {
        uint8_t info = s.u8();
        if (info & 0x04) {
            m.prop_by_number = (info & 0x02) != 0;
            if (m.prop_by_number) m.prop_number = s.uint_(); else m.prop_name = s.str();
        }
        if (!(info & 0x08)) {
            uint64_t count = info >> 4; if (count == 15) count = s.uint_();
            m.value_by_number = false; m.value.clear();
            bool first_str = false;
            for (uint64_t i = 0; i < count; ++i) {
                uint64_t t = s.uint_();
                bool by_number = false; uint64_t number = 0; std::string v;
                char buf[64];
                if (t <= 7) { snprintf(buf, sizeof(buf), "%.17g", s.real_of(t)); v = buf; }
                else if (t == 8) v = std::to_string(s.uint_());
                else if (t == 9) v = std::to_string(s.sint());
                else if (t <= 12) v = s.str();
                else if (t <= 15) { by_number = true; number = s.uint_(); }
                else s.fail("invalid property value type " + std::to_string(t));
                // The model keeps one value: the first string-valued one (so S_GDS_PROPERTY
                // <attribute, b-string> keeps its string), else the first value.
                const bool is_str = t >= 10;
                if (i == 0 || (is_str && !first_str)) { m.value_by_number = by_number; m.value_number = number; m.value = v; }
                if (is_str) first_str = true;
            }
        }
        record_property();
    }
    void record_property() {
        if (!want_props || ptarget == 5) return;
        props.push_back({ ptarget == 0 ? -1 : cur, m.prop_by_number, m.prop_number, m.prop_name, m.value_by_number, m.value_number,
                          m.value, ptarget, ptarget_cellname });
    }

    static void skip_interval(Reader& s) { oasis_codec::skip_interval(s); }

    void cblock(Reader& s) {
        uint64_t comp_type = s.uint_(), uncomp_len = s.uint_(), comp_len = s.uint_();
        if (cblock_depth > 0) s.fail("CBLOCK nested in a CBLOCK");                       // 11.4
        if (comp_type != 0) s.fail("CBLOCK comp-type " + std::to_string(comp_type) + " (only 0 = DEFLATE is defined)");
        s.need(comp_len);
        if (scan_only) {                     // record and skip: payload is self-contained
            if (scan_refs) scan_refs->push_back(CblockRef{ s.p + s.off, comp_type, uncomp_len, comp_len });
            s.off += (size_t)comp_len;
            return;
        }
        if (src && cblock_depth == 0) {
            const uint8_t* bp = nullptr; size_t bn = 0;
            const size_t idx = top_cblock++;
            if (src->get(idx, s.p + s.off, comp_len, uncomp_len, bp, bn)) {
                s.off += (size_t)comp_len;
                ++cblocks;
                std::string where = "CBLOCK ending at file offset " + std::to_string(s.off) + ", uncompressed";
                Reader inner(bp, bn, where.c_str());
                ++cblock_depth;
                parse(inner);
                --cblock_depth;
                return;
            }
            // mismatch with the scan or undecodable in batch: decode this block inline below
        }
        std::vector<uint8_t> out;
        bool ok = db::decompress_cblock((uint8_t)comp_type, s.p + s.off, (size_t)comp_len, out, (size_t)uncomp_len, nvst);
        if (!ok || out.size() != uncomp_len) s.fail("CBLOCK decompression failed");
        s.off += (size_t)comp_len;
        ++cblocks;
        std::string where = "CBLOCK ending at file offset " + std::to_string(s.off) + ", uncompressed";
        Reader inner(out.data(), out.size(), where.c_str());
        ++cblock_depth;
        parse(inner);
        --cblock_depth;
    }

    // Called before each record outside CBLOCKs (sidecar builder: unit boundaries).
    void (*top_hook)(void* ud, size_t off, uint8_t id) = nullptr;
    void* top_hook_ud = nullptr;

    void parse(Reader& s) {
        while (!s.eof() && !saw_end) {
            if (top_hook && cblock_depth == 0) top_hook(top_hook_ud, s.off, s.p[s.off]);
            uint8_t id = s.u8();
            switch (id) {
            case 0: break;                                              // PAD
            case 1: {                                                   // START
                s.skip_str(); unit = s.real();
                if (!(unit > 0) || !std::isfinite(unit)) s.fail("START unit is not a positive number");   // 13.10
                if (s.uint_() == 0) for (int i = 0; i < 12; ++i) s.uint_();
            } break;
            case 2: saw_end = true; break;                              // END
            case 3: cellnames.add_implicit(s.str()); break;
            case 4: { std::string nm = s.str(); last_cellname_ref = s.uint_(); cellnames.add(last_cellname_ref, nm); } break;
            case 5: textstrings.add_implicit(s.str()); break;
            case 6: { std::string nm = s.str(); textstrings.add(s.uint_(), nm); } break;
            case 7: propnames.add_implicit(s.str()); break;
            case 8: { std::string nm = s.str(); propnames.add(s.uint_(), nm); } break;
            case 9: propstrings.add_implicit(s.str()); break;
            case 10: { std::string nm = s.str(); propstrings.add(s.uint_(), nm); } break;
            case 11: case 12: s.skip_str(); skip_interval(s); skip_interval(s); break;   // LAYERNAME
            case 13: { CellRef id_; id_.by_number = true; id_.number = s.uint_(); start_cell(id_); } break;
            case 14: { CellRef id_; id_.name = s.str(); start_cell(id_); } break;
            case 15: m.relative = false; set(D_REL); break;
            case 16: m.relative = true; set(D_REL); break;
            case 17: placement(s, false); break;
            case 18: placement(s, true); break;
            case 19: text(s); break;
            case 20: rectangle(s); break;
            case 21: polygon(s); break;
            case 22: path(s); break;
            case 23: case 24: case 25: trapezoid(s, id); break;
            case 26: ctrapezoid(s); break;
            case 27: circle(s); break;
            case 28: property(s); break;
            case 29: record_property(); break;                          // repeat last property
            case 30: s.uint_(); s.skip_str(); break;                    // XNAME
            case 31: s.uint_(); s.skip_str(); s.uint_(); break;
            case 32: s.uint_(); s.skip_str(); break;                    // XELEMENT
            case 33: {                                                  // XGEOMETRY
                uint8_t info = s.u8(); s.uint_();
                read_layer(s, info); s.skip_str();
                read_xy(s, info, m.geom_x, m.geom_y); (void)read_rep(s, info);
            } break;
            case 34: cblock(s); break;
            default: --s.off; s.fail("unknown OASIS record " + std::to_string((int)id));
            }
            switch (id) {                                               // property target
            case 1: ptarget = 0; break;
            case 3: ptarget = 4; ptarget_cellname = cellnames.next_implicit - 1; break;
            case 4: ptarget = 4; ptarget_cellname = last_cellname_ref; break;
            case 13: case 14: ptarget = 1; break;
            case 17: case 18: ptarget = 3; break;
            case 19: case 20: case 21: case 22: case 23: case 24: case 25: case 26: case 27: case 33: ptarget = 2; break;
            case 0: case 15: case 16: case 28: case 29: case 34: break;
            default: ptarget = 5; break;
            }
        }
    }

    // --- hand the result over to the format-neutral model ---------------------
    std::string cell_name(const CellRef& r) const {
        if (!r.by_number) return r.name;
        const std::string* nm = cellnames.find(r.number);
        return nm ? *nm : "__cell_" + std::to_string(r.number);
    }

    void finish() {
        f->dbu_per_micron = unit;
        for (auto& pc : cells) {
            std::string name = cell_name(pc.id);
            LdbCell& c = pc.cell; c.name = name;
            for (auto& tr : pc.text_refs) {
                const std::string* s = textstrings.find(tr.ref);
                // a dangling reference degrades to a placeholder, like '__cell_N' and 'propstring#N'
                c.texts[tr.index].str = s ? *s : "textstring#" + std::to_string(tr.ref);
            }
            for (auto& pi : pc.insts) {
                LdbInstance inst; inst.ref_name = cell_name(pi.ref); inst.tr = pi.tr;
                c.refs.insert(inst.ref_name);
                c.instances.push_back(std::move(inst));
            }
            if (f->cell_index.count(name)) throw ParseError{ "cell '" + name + "' is defined twice" };
            f->cell_index[name] = f->cells.size();
            f->cells.push_back(std::move(c));
        }
        for (auto& kv : propnames.names) f->propnames[(int)kv.first] = kv.second;
        for (auto& kv : propstrings.names) f->propstrings[(int)kv.first] = kv.second;
        for (auto& pp : props) {
            LdbProperty lp;
            if (pp.name_by_number) {
                const std::string* nm = propnames.find(pp.name_number);
                lp.name = nm ? *nm : "prop#" + std::to_string(pp.name_number);
            } else lp.name = pp.name;
            if (pp.value_by_number) {
                const std::string* v = propstrings.find(pp.value_number);
                lp.value = v ? *v : "propstring#" + std::to_string(pp.value_number);
            } else lp.value = pp.value;
            if (pp.kind == 4) {                                        // CELLNAME property: the named cell
                const std::string* cn = cellnames.find(pp.cellname_ref);
                auto it = cn ? f->cell_index.find(*cn) : f->cell_index.end();
                if (it == f->cell_index.end()) continue;               // external or unnamed cell
                lp.target_kind = 1; lp.cell = *cn; f->cells[it->second].properties.push_back(std::move(lp));
            } else if (pp.kind == 0 || pp.cell < 0) { lp.target_kind = 0; f->file_properties.push_back(std::move(lp)); }
            else { lp.target_kind = pp.kind; lp.cell = f->cells[(size_t)pp.cell].name; f->cells[(size_t)pp.cell].properties.push_back(std::move(lp)); }
        }
    }
};

} // namespace oasis_parse
