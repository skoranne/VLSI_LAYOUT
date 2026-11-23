// OASIS (SEMI P39) reader.
//
// Decodes every record type of the standard, including CBLOCK compression,
// all six point-list forms, all twelve repetition forms and the modal
// variables, and fills the format-neutral LdbFile model:
//
//   * RECTANGLE                     -> one exact box            (kind 2)
//   * POLYGON, Manhattan            -> exact rectangle cover    (kind 0)
//   * PATH, Manhattan segments      -> one rectangle / segment  (kind 1)
//   * TRAPEZOID / CTRAPEZOID that are rectangles -> exact box   (kind 0)
//   * TEXT, XELEMENT, XGEOMETRY, LAYERNAME, XNAME are decoded and skipped
//   * POLYGON / TRAPEZOID / CTRAPEZOID with a slanted edge, and CIRCLE ->
//     rectangle strips, each as wide as the shape at the strip's mid height
//     (LDB_SLANT_STRIPS strips per vertex-to-vertex slab, default 4);
//     a PATH with a slanted segment -> bounding box.  Boxes cannot represent
//     these shapes exactly, so they are counted and reported as APPROXIMATED
//     in the info string.
//   * element repetitions are expanded; placement repetitions stay arrays
//     when regular and become one instance per position otherwise.
//
// The parser is strict: a record it cannot decode aborts the load with the
// file offset instead of guessing a resynchronisation point.

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

namespace {

using oasis_codec::ParseError; using oasis_codec::Reader; using oasis_codec::Repetition;
using oasis_codec::read_repetition; using oasis_codec::read_point_list;
using ldbgeom::Pt; using ldbgeom::Rect;
using ldbgeom::is_manhattan; using ldbgeom::fracture_manhattan; using ldbgeom::fracture_general; using ldbgeom::path_to_rects;

// ---------------------------------------------------------------------------
// Parser state
// ---------------------------------------------------------------------------
struct CellRef { bool by_number = false; uint64_t number = 0; std::string name; };
struct PInst { CellRef ref; LdbTransform tr; };
struct PCell { CellRef id; LdbCell cell; std::vector<PInst> insts; };
struct PProp { long cell; bool name_by_number; uint64_t name_number; std::string name;
               bool value_by_number; uint64_t value_number; std::string value; };

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
    Modal m;
    db::NvcompStatus nvst;
    size_t cblocks = 0, approximated = 0;
    bool saw_end = false, want_props = false;
    std::vector<Rect> rects;                 // scratch
    std::vector<Pt> verts;             // scratch
    // Streaming mode (ldb_stream_boxes): boxes go straight to the callback, nothing is stored.
    // Valid for flat files only; a PLACEMENT aborts with StreamHier so the caller can fall back.
    ldb_box_cb stream_cb = nullptr;
    void* stream_ud = nullptr;
    size_t streamed = 0;

    static const Repetition& single() { static const Repetition one; return one; }

    // --- geometry sinks ----------------------------------------------------
    void add_box(uint64_t layer, uint64_t dt, int kind, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
        if (stream_cb) {
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

    // Fill `verts` (a closed polygon) as rectangles.
    void emit_polygon(Reader& s, const Repetition& rep) {
        if (scan_only) return;
        rects.clear();
        if (is_manhattan(verts)) fracture_manhattan(verts, rects);
        else { fracture_general(verts, rects); ++approximated; }
        emit_rects(s, rep, 0);
    }

    // --- common element fields -----------------------------------------------
    void read_xy(Reader& s, uint8_t info, int64_t& x, int64_t& y) {    // X = bit 4, Y = bit 3
        if (info & 0x10) { int64_t v = s.sint(); x = m.relative ? x + v : v; }
        if (info & 0x08) { int64_t v = s.sint(); y = m.relative ? y + v : v; }
    }
    void read_layer(Reader& s, uint8_t info) {                         // L = bit 0, D = bit 1
        if (info & 0x01) m.layer = s.uint_();
        if (info & 0x02) m.datatype = s.uint_();
    }
    const Repetition& read_rep(Reader& s, uint8_t info) {              // R = bit 2
        if (!(info & 0x04)) return single();
        read_repetition(s, m.rep); return m.rep;
    }

    // --- records -------------------------------------------------------------
    void start_cell(const CellRef& id) {
        PCell pc; pc.id = id; cells.push_back(std::move(pc));
        cur = (long)cells.size() - 1; m.reset_for_cell();
    }

    void placement(Reader& s, bool general) {
        if (stream_cb && !scan_only) throw StreamHier{};
        if (cur < 0 && !scan_only) s.fail("PLACEMENT outside a cell");
        uint8_t info = s.u8();
        if (info & 0x80) {
            m.place_cell = CellRef();
            if (info & 0x40) { m.place_cell.by_number = true; m.place_cell.number = s.uint_(); }
            else m.place_cell.name = s.str();
        }
        double mag = 1.0, angle;
        if (general) { if (info & 0x04) mag = s.real(); angle = (info & 0x02) ? s.real() : 0.0; }
        else angle = 90.0 * ((info >> 1) & 3);
        if (info & 0x20) { int64_t v = s.sint(); m.place_x = m.relative ? m.place_x + v : v; }
        if (info & 0x10) { int64_t v = s.sint(); m.place_y = m.relative ? m.place_y + v : v; }
        const Repetition* rep = &single();
        if (info & 0x08) { read_repetition(s, m.rep); rep = &m.rep; }
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
        if (info & 0x40) { if (info & 0x20) s.uint_(); else s.skip_str(); }
        if (info & 0x01) m.textlayer = s.uint_();
        if (info & 0x02) m.texttype = s.uint_();
        read_xy(s, info, m.text_x, m.text_y);
        (void)read_rep(s, info);             // text carries no area: nothing to store
    }

    void rectangle(Reader& s) {
        uint8_t info = s.u8();
        read_layer(s, info);
        if (info & 0x40) m.geom_w = (int64_t)s.uint_();
        if (info & 0x20) m.geom_h = (int64_t)s.uint_();
        if (info & 0x80) m.geom_h = m.geom_w;                          // square
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        emit(s, rep, 2, m.geom_x, m.geom_y, m.geom_x + m.geom_w, m.geom_y + m.geom_h);
    }

    void polygon(Reader& s) {
        uint8_t info = s.u8();
        read_layer(s, info);
        if (info & 0x20) read_point_list(s, true, m.polygon_pts);
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        verts.clear(); verts.push_back({ m.geom_x, m.geom_y });
        for (auto& q : m.polygon_pts) verts.push_back({ m.geom_x + q.x, m.geom_y + q.y });
        emit_polygon(s, rep);
    }

    void path(Reader& s) {
        uint8_t info = s.u8();
        read_layer(s, info);
        if (info & 0x40) m.half_width = (int64_t)s.uint_();
        if (info & 0x80) {
            uint64_t scheme = s.uint_();
            auto ext = [&](unsigned code, int64_t& v) {
                if (code == 1) v = 0; else if (code == 2) v = m.half_width; else if (code == 3) v = s.sint();
            };
            ext((unsigned)((scheme >> 2) & 3), m.ext_start);
            ext((unsigned)(scheme & 3), m.ext_end);
        }
        if (info & 0x20) read_point_list(s, false, m.path_pts);
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        const int64_t hw = m.half_width;
        if (hw == 0 || m.path_pts.empty()) return;                     // no area
        verts.clear(); verts.push_back({ m.geom_x, m.geom_y });
        for (auto& q : m.path_pts) verts.push_back({ m.geom_x + q.x, m.geom_y + q.y });
        rects.clear();
        if (!path_to_rects(verts, hw, m.ext_start, m.ext_end, rects)) { emit_bbox_of(s, rep, 1, verts, hw); return; }
        emit_rects(s, rep, 1);
    }

    void trapezoid(Reader& s, uint8_t id) {
        uint8_t info = s.u8();
        read_layer(s, info);
        if (info & 0x40) m.geom_w = (int64_t)s.uint_();
        if (info & 0x20) m.geom_h = (int64_t)s.uint_();
        int64_t da = (id == 23 || id == 24) ? s.sint() : 0;
        int64_t db_ = (id == 23 || id == 25) ? s.sint() : 0;
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        const int64_t x = m.geom_x, y = m.geom_y, w = m.geom_w, h = m.geom_h;
        if (da == 0 && db_ == 0) { emit(s, rep, 0, x, y, x + w, y + h); return; }
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
        if (info & 0x80) m.ctrap_type = s.uint_();
        if (info & 0x40) m.geom_w = (int64_t)s.uint_();
        if (info & 0x20) m.geom_h = (int64_t)s.uint_();
        read_xy(s, info, m.geom_x, m.geom_y);
        const Repetition& rep = read_rep(s, info);
        const int64_t x = m.geom_x, y = m.geom_y, w = m.geom_w, h = m.geom_h;
        const uint64_t t = m.ctrap_type;
        if (t > 25) s.fail("invalid CTRAPEZOID type " + std::to_string(t));
        if (t == 24) { emit(s, rep, 0, x, y, x + w, y + h); return; }
        if (t == 25) { emit(s, rep, 0, x, y, x + w, y + w); return; }
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
        if (info & 0x20) m.radius = (int64_t)s.uint_();
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
                if (i == 0) { m.value_by_number = by_number; m.value_number = number; m.value = v; }
            }
        }
        record_property();
    }
    void record_property() {
        if (!want_props) return;
        props.push_back({ cur, m.prop_by_number, m.prop_number, m.prop_name, m.value_by_number, m.value_number, m.value });
    }

    static void skip_interval(Reader& s) { oasis_codec::skip_interval(s); }

    void cblock(Reader& s) {
        uint64_t comp_type = s.uint_(), uncomp_len = s.uint_(), comp_len = s.uint_();
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

    void parse(Reader& s) {
        while (!s.eof() && !saw_end) {
            uint8_t id = s.u8();
            switch (id) {
            case 0: break;                                              // PAD
            case 1: {                                                   // START
                s.skip_str(); s.real();
                if (s.uint_() == 0) for (int i = 0; i < 12; ++i) s.uint_();
            } break;
            case 2: saw_end = true; break;                              // END
            case 3: cellnames.add_implicit(s.str()); break;
            case 4: { std::string nm = s.str(); cellnames.add(s.uint_(), nm); } break;
            case 5: textstrings.add_implicit(s.str()); break;
            case 6: { std::string nm = s.str(); textstrings.add(s.uint_(), nm); } break;
            case 7: propnames.add_implicit(s.str()); break;
            case 8: { std::string nm = s.str(); propnames.add(s.uint_(), nm); } break;
            case 9: propstrings.add_implicit(s.str()); break;
            case 10: { std::string nm = s.str(); propstrings.add(s.uint_(), nm); } break;
            case 11: case 12: s.skip_str(); skip_interval(s); skip_interval(s); break;   // LAYERNAME
            case 13: { CellRef id_; id_.by_number = true; id_.number = s.uint_(); start_cell(id_); } break;
            case 14: { CellRef id_; id_.name = s.str(); start_cell(id_); } break;
            case 15: m.relative = false; break;
            case 16: m.relative = true; break;
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
        }
    }

    // --- hand the result over to the format-neutral model ---------------------
    std::string cell_name(const CellRef& r) const {
        if (!r.by_number) return r.name;
        const std::string* nm = cellnames.find(r.number);
        return nm ? *nm : "__cell_" + std::to_string(r.number);
    }

    void finish() {
        for (auto& pc : cells) {
            std::string name = cell_name(pc.id);
            LdbCell& c = pc.cell; c.name = name;
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
            if (pp.cell < 0) { lp.target_kind = 0; f->file_properties.push_back(std::move(lp)); }
            else { lp.target_kind = 1; lp.cell = f->cells[(size_t)pp.cell].name; f->cells[(size_t)pp.cell].properties.push_back(std::move(lp)); }
        }
    }
};

} // namespace

static int parse_oasis_impl(LdbFile* f, ldb_box_cb stream_cb, void* stream_ud);
bool parse_oasis(LdbFile* f) { return parse_oasis_impl(f, nullptr, nullptr) == 0; }
// Flat streaming parse: every box is passed to cb as it is decoded, with no per-box storage.
// Returns 0 ok, 2 file has placements (hierarchical, nothing usable was streamed), -1 error (f->error).
int parse_oasis_stream(LdbFile* f, ldb_box_cb cb, void* ud) { return parse_oasis_impl(f, cb, ud); }

static int parse_oasis_impl(LdbFile* f, ldb_box_cb stream_cb, void* stream_ud) {
    static const char magic[] = "%SEMI-OASIS\r\n";
    const uint8_t* data = (const uint8_t*)f->map;
    if (f->size < 13 || memcmp(data, magic, 13) != 0) { f->error = "not an OASIS file (bad magic)"; return -1; }
    Parser ps; ps.f = f;
    ps.stream_cb = stream_cb; ps.stream_ud = stream_ud;
    ps.nvst = db::nvcomp_init();
    ps.want_props = (f->open_flags & LDB_OPEN_PROPERTY) != 0 || f->strict_mode;
    const db::CblockBackend backend = db::cblock_backend_from_env();
    std::unique_ptr<CblockSource> source;
    double scan_ms = 0.0;
    if (backend != db::CblockBackend::Serial) {
        // Pre-scan for CBLOCKs (cheap: geometry sinks are skipped, CBLOCK payloads jumped over).
        // Any problem here simply disables batching; the real parse reports real errors.
        const auto t0 = std::chrono::steady_clock::now();
        std::vector<CblockRef> refs;
        try {
            Parser scan; scan.f = f; scan.scan_only = true; scan.scan_refs = &refs;
            Reader s(data + 13, f->size - 13, "file (CBLOCK scan)");
            scan.parse(s);
        } catch (...) { refs.clear(); }
        scan_ms = std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - t0).count();
        if (refs.size() >= 2) {
            source.reset(new CblockSource(std::move(refs), backend, ps.nvst));
            ps.src = source.get();
        }
    }
    try {
        Reader s(data + 13, f->size - 13, "file (after the 13-byte magic)");
        ps.parse(s);
        if (!ps.saw_end) throw ParseError{ "OASIS file has no END record" };
        ps.finish();
    } catch (const ParseError& e) {
        f->cells.clear(); f->cell_index.clear();
        f->error = "OASIS parse error: " + e.msg;
        return -1;
    } catch (const StreamHier&) {
        f->cells.clear(); f->cell_index.clear();
        f->error = "hierarchical OASIS (PLACEMENT): streaming not possible";
        return 2;
    }
    if (stream_cb) {
        f->error = "STREAMED BOXES=" + std::to_string(ps.streamed) + " CBLOCKS=" + std::to_string(ps.cblocks);
        return 0;
    }
    // Original bytes are kept for the lossless write-back path.
    f->oasis_raw.assign(data, data + f->size);
    f->mem_bytes_extra = f->oasis_raw.capacity();
    size_t nboxes = 0;
    for (auto& c : f->cells) nboxes += c.boxes.size();
    char buf[400];
    const db::CblockStats& cs = db::cblock_stats();
    snprintf(buf, sizeof(buf),
             "CELLS=%zu BOXES=%zu CBLOCKS=%zu APPROXIMATED=%zu CBLOCK_BACKEND=%s CBLOCK_GPU=%zu CBLOCK_CPU=%zu "
             "CBLOCK_RETRY=%zu CBLOCK_INFLATE_MS=%.1f CBLOCK_WAIT_MS=%.1f CBLOCK_SCAN_MS=%.1f",
             f->cells.size(), nboxes, ps.cblocks, ps.approximated,
             source ? db::cblock_backend_name(backend) : "serial", cs.blocks_gpu, cs.blocks_cpu, cs.blocks_retry,
             cs.inflate_ms, cs.wait_ms, scan_ms);
    f->error = buf;    // informational on success (read by db_hier_extent)
    if (const char* v = std::getenv("LDB_CBLOCK_VERBOSE")) {
        if (*v && *v != '0') std::fprintf(stderr, "LDB_OASIS %s\n", buf);
    }
    return 0;
}
