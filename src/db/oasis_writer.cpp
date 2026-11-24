// OASIS (SEMI P39-1105) streaming writer (see oasis_writer.hpp) and the rectangle
// repetition finder. Self-contained (zlib only): bin2oas links it directly; the C API that
// writes the in-memory model is in oasis_model_writer.cpp.

#include "oasis_writer.hpp"
#include <zlib.h>
#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <functional>
#include <map>
#include <string>
#include <utility>
#include <vector>

namespace oasis_writer {

using oasis_codec::Encoder;
using oasis_codec::is_nstring;
using oasis_codec::is_astring;

namespace {

enum : uint8_t {
    R_PAD = 0, R_START = 1, R_END = 2, R_CELLNAME = 3, R_TEXTSTRING = 5, R_PROPNAME = 7, R_PROPSTRING = 9,
    R_LAYERNAME = 11, R_LAYERNAME_TEXT = 12, R_CELL_REF = 13, R_CELL_NAME = 14, R_XYABSOLUTE = 15,
    R_XYRELATIVE = 16, R_PLACEMENT = 17, R_PLACEMENT_X = 18, R_TEXT = 19, R_RECTANGLE = 20, R_POLYGON = 21,
    R_PATH = 22, R_CIRCLE = 27, R_PROPERTY = 28, R_PROPERTY_REPEAT = 29, R_CBLOCK = 34
};

size_t uint_len(uint64_t v) { size_t n = 1; while (v >= 0x80) { v >>= 7; ++n; } return n; }
uint64_t umag(int64_t v) { return v < 0 ? (uint64_t)0 - (uint64_t)v : (uint64_t)v; }
uint64_t gcd64(uint64_t a, uint64_t b) { while (b) { uint64_t t = a % b; a = b; b = t; } return a; }

bool same_rep(const Repetition& a, const Repetition& b) {
    if (a.regular != b.regular) return false;
    if (a.regular) return a.cols == b.cols && a.rows == b.rows && a.dx == b.dx && a.dy == b.dy && a.rdx == b.rdx && a.rdy == b.rdy;
    if (a.offs.size() != b.offs.size()) return false;
    for (size_t i = 0; i < a.offs.size(); ++i) if (a.offs[i].x != b.offs[i].x || a.offs[i].y != b.offs[i].y) return false;
    return true;
}

// Box of the repetition offsets, (0,0) included.
void rep_box(const Repetition& r, int64_t& x1, int64_t& y1, int64_t& x2, int64_t& y2) {
    x1 = y1 = x2 = y2 = 0;
    auto add = [&](int64_t x, int64_t y) { x1 = std::min(x1, x); y1 = std::min(y1, y); x2 = std::max(x2, x); y2 = std::max(y2, y); };
    if (!r.regular) { for (auto& o : r.offs) add(o.x, o.y); return; }
    const int64_t c = r.cols - 1, rr = r.rows - 1;
    add(c * r.dx, c * r.dy); add(rr * r.rdx, rr * r.rdy); add(c * r.dx + rr * r.rdx, c * r.dy + rr * r.rdy);
}

// Canonical form of a repetition: every lattice vector points into the upper half plane
// (dx > 0 or dx == 0 and dy >= 0), a lattice with a vertical column vector and horizontal
// row vector is transposed (so type 1 applies), irregular offsets are sorted and start at
// (0,0), and evenly spaced collinear offsets become a lattice. The element origin moves by
// (ox, oy); the set of positions is unchanged. Returns false for a single position.
bool normalize(const Repetition& in, Repetition& r, int64_t& ox, int64_t& oy) {
    ox = oy = 0;
    if (in.regular) {
        r = in; r.offs.clear();
        if (r.cols < 1 || r.rows < 1) return false;
        if (r.cols == 1) { r.cols = r.rows; r.dx = r.rdx; r.dy = r.rdy; r.rows = 1; r.rdx = r.rdy = 0; }
        if (r.rows == 1) r.rdx = r.rdy = 0;
        if (r.cols * r.rows <= 1) return false;
        auto flip = [&](int64_t n, int64_t& vx, int64_t& vy) {
            if (vx < 0 || (vx == 0 && vy < 0)) { ox += (n - 1) * vx; oy += (n - 1) * vy; vx = -vx; vy = -vy; }
        };
        flip(r.cols, r.dx, r.dy);
        if (r.rows > 1) {
            flip(r.rows, r.rdx, r.rdy);
            if (r.dx == 0 && r.rdy == 0) { std::swap(r.cols, r.rows); std::swap(r.dx, r.rdx); std::swap(r.dy, r.rdy); }
        }
        return true;
    }
    if (in.offs.empty()) return false;
    std::vector<Pt> v = in.offs;
    std::sort(v.begin(), v.end(), [](const Pt& a, const Pt& b) { return a.y != b.y ? a.y < b.y : a.x < b.x; });
    ox = v[0].x; oy = v[0].y;
    for (auto& p : v) { p.x -= ox; p.y -= oy; }
    if (v.size() == 1) return false;
    // evenly spaced on a line -> 1-D lattice
    bool even = true;
    for (size_t i = 2; i < v.size() && even; ++i)
        even = v[i].x - v[i - 1].x == v[1].x && v[i].y - v[i - 1].y == v[1].y;
    if (even && (v[1].x != 0 || v[1].y != 0)) {
        r = Repetition::lattice((int64_t)v.size(), v[1].x, v[1].y);
        int64_t ax, ay; Repetition t = r;
        normalize(t, r, ax, ay); ox += ax; oy += ay;
        return true;
    }
    r = Repetition(); r.regular = false; r.offs = std::move(v);
    return true;
}

} // namespace

// ---------------------------------------------------------------------------
// Writer implementation
// ---------------------------------------------------------------------------
struct Writer::Impl {
    Options o;
    FILE* fp = nullptr;
    std::vector<uint8_t> obuf;
    uint64_t pos = 0;                        // file offset of the next byte
    // validation over all bytes; the reserved header region is excluded and added at close
    uint32_t crc = 0, crc_pre = 0, sum = 0, sum_pre = 0;
    uint64_t seg_len = 0;
    bool reserved = false; uint64_t reserve_off = 0, reserve_len = 0;
    std::vector<uint8_t> patch;
    // records
    Encoder blk;                             // current CBLOCK contents (or one record, uncompressed)
    z_stream zs{}; bool zinit = false;
    std::vector<uint8_t> zbuf;
    bool header_done = false, in_cell = false, closed = false;
    long cur_cell = -1;
    // names
    std::unordered_map<std::string, uint64_t> cell_ref, text_ref, pname_ref, pstr_ref;
    std::vector<std::string> cell_names, texts, pnames, pstrs;
    struct Inst { uint64_t child; Transform t; int64_t rx1, ry1, rx2, ry2; };
    struct CellInfo {
        bool defined = false, placed = false;
        uint64_t offset = 0;
        bool has_box = false; int64_t x1 = 0, y1 = 0, x2 = 0, y2 = 0;
        std::vector<Inst> insts;
    };
    std::vector<CellInfo> cells;
    struct LayerName { std::string name; uint64_t l, d; bool text; };
    std::vector<LayerName> layers;
    std::vector<std::string> tops;
    struct FileProp { std::string name; std::vector<PropValue> v; bool standard; };
    std::vector<FileProp> file_props;
    // modal state (10.1)
    struct Modal {
        bool layer_k, dt_k, tl_k, tt_k, w_k, h_k, g_k, p_k, t_k, pcell_k, tstr_k, rep_k, ppl_k, hpl_k, hw_k, es_k, ee_k, rad_k, pn_k, pv_k;
        int xy;                              // 0 absolute, 1 relative, -1 unknown
        uint64_t layer, dt, tl, tt, pcell, tstr, pn;
        std::string pcell_s, tstr_s, pn_s;
        int64_t w, h, gx, gy, px, py, tx, ty, hw, es, ee, rad;
        Repetition rep;
        std::vector<uint8_t> ppl, hpl;
        std::vector<PropValue> pv;
        bool pn_std;
        void unknown() {
            layer_k = dt_k = tl_k = tt_k = w_k = h_k = g_k = p_k = t_k = pcell_k = tstr_k = rep_k = ppl_k = hpl_k = false;
            hw_k = es_k = ee_k = rad_k = pn_k = pv_k = false;
            xy = -1;
        }
        void cell_start() {                  // 21.2: absolute mode, positions 0
            unknown();
            g_k = p_k = t_k = true; gx = gy = px = py = tx = ty = 0; xy = 0;
        }
    } m;

    Impl() { m.unknown(); }
    ~Impl() { if (zinit) deflateEnd(&zs); if (fp) fclose(fp); }

    // --- output ---------------------------------------------------------------
    void out(const uint8_t* p, size_t n) {
        if (o.validation == 1) crc = (uint32_t)crc32(crc, p, (uInt)n);
        else if (o.validation == 2) for (size_t i = 0; i < n; ++i) sum += p[i];
        seg_len += n;
        obuf.insert(obuf.end(), p, p + n);
        pos += n;
        if (obuf.size() >= (1u << 20)) drain();
    }
    void out(const Encoder& e) { out(e.b.data(), e.b.size()); }
    bool io_failed = false;                          // latched: a failed mid-stream chunk is reported by close()
    bool drain() {
        if (obuf.empty() || !fp) return !io_failed;
        if (fwrite(obuf.data(), 1, obuf.size(), fp) != obuf.size()) io_failed = true;
        obuf.clear();
        return !io_failed;
    }
    bool compressing() const { return o.cblock_bytes > 0; }
};

Writer::Writer() : d_(new Impl) {}
Writer::~Writer() { delete d_; }

bool Writer::open(const std::string& path, const Options& opt) {
    Impl& I = *d_;
    I.o = opt;
    if (!(I.o.unit > 0) || !std::isfinite(I.o.unit)) { err_ = "unit must be a positive number"; return false; }
    I.fp = fopen(path.c_str(), "wb");
    if (!I.fp) { err_ = "cannot open " + path + " for writing"; return false; }
    if (I.o.validation == 1) I.crc = (uint32_t)crc32(0L, Z_NULL, 0);
    if (I.compressing()) {
        if (deflateInit2(&I.zs, I.o.deflate_level, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY) != Z_OK) { err_ = "deflateInit2 failed"; return false; }
        I.zinit = true;
    }
    Encoder e;
    e.bytes("%SEMI-OASIS\r\n", 13);
    e.u8(R_START); e.str("1.0"); e.real(I.o.unit);
    if (I.o.strict) e.uint_(1);                                   // table offsets in END
    else { e.uint_(0); for (int i = 0; i < 12; ++i) e.uint_(0); } // no tables
    I.out(e);
    return true;
}

namespace {
uint64_t intern(std::unordered_map<std::string, uint64_t>& m, std::vector<std::string>& v, const std::string& s) {
    auto it = m.find(s);
    if (it != m.end()) return it->second;
    const uint64_t r = v.size();
    m.emplace(s, r); v.push_back(s);
    return r;
}
}

void Writer::add_top_cell(const std::string& name) {
    if (!ok()) return;
    if (d_->header_done) { err_ = "add_top_cell after the first cell"; return; }
    if (!is_nstring(name)) { err_ = "cell name '" + name + "' is not an n-string"; return; }
    d_->tops.push_back(name);
}

void Writer::file_property(const std::string& name, const std::vector<PropValue>& v, bool standard) {
    if (!ok()) return;
    if (d_->header_done) { err_ = "file_property after the first cell"; return; }
    if (!is_nstring(name)) { err_ = "property name '" + name + "' is not an n-string"; return; }
    d_->file_props.push_back({ name, v, standard });
}

void Writer::layer_name(const std::string& name, uint64_t layer, uint64_t datatype, bool text) {
    if (!ok()) return;
    if (!is_nstring(name)) { err_ = "layer name '" + name + "' is not an n-string"; return; }
    d_->layers.push_back({ name, layer, datatype, text });
}

// ---------------------------------------------------------------------------
// Record plumbing
// ---------------------------------------------------------------------------
namespace {

struct Ctx {
    Writer::Impl& I; std::string& err; Stats& st;

    // Called once before each element (and before any XY-mode record it needs).
    void begin() {
        if (I.compressing() && I.o.independent_cblocks && I.blk.b.empty()) I.m.unknown();
    }
    void end() {
        if (!I.compressing()) { st.record_bytes += I.blk.b.size(); I.out(I.blk); I.blk.b.clear(); }
        else if (I.blk.b.size() >= I.o.cblock_bytes) flush();
    }
    void flush() {
        if (I.blk.b.empty()) return;
        if (!I.compressing()) { st.record_bytes += I.blk.b.size(); I.out(I.blk); I.blk.b.clear(); return; }
        const size_t n = I.blk.b.size();
        deflateReset(&I.zs);
        I.zbuf.resize(deflateBound(&I.zs, n) + 64);
        I.zs.next_in = I.blk.b.data(); I.zs.avail_in = (uInt)n;
        I.zs.next_out = I.zbuf.data(); I.zs.avail_out = (uInt)I.zbuf.size();
        if (deflate(&I.zs, Z_FINISH) != Z_STREAM_END) { if (err.empty()) err = "deflate failed"; I.blk.b.clear(); return; }
        const size_t zn = I.zs.total_out;
        Encoder h; h.u8(R_CBLOCK); h.uint_(0); h.uint_(n); h.uint_(zn);   // comp-type 0 = DEFLATE
        I.out(h); I.out(I.zbuf.data(), zn);
        st.record_bytes += n; st.cblocks++;
        I.blk.b.clear();
    }
    Encoder& e() { return I.blk; }

    // Plan an (x,y) for a position modal pair; may emit XYABSOLUTE / XYRELATIVE first.
    // Returns the X/Y presence and the values to write.
    struct XY { bool X, Y; int64_t vx, vy; };
    XY xy(int64_t x, int64_t y, bool& known, int64_t& mx, int64_t& my) {
        XY r;
        if (!known) {
            if (I.m.xy != 0) { e().u8(R_XYABSOLUTE); I.m.xy = 0; }
            r = { true, true, x, y };
        } else {
            if (I.m.xy != 1) { e().u8(R_XYRELATIVE); I.m.xy = 1; }
            r = { x != mx, y != my, x - mx, y - my };
        }
        mx = x; my = y; known = true;
        return r;
    }
    void put_xy(const XY& p) { if (p.X) e().sint(p.vx); if (p.Y) e().sint(p.vy); }

    // Write a normalised repetition (types 0-11), updating the modal repetition.
    void rep(const Repetition& r) {
        Encoder& E = e();
        st.repetitions++; st.shapes_in_repetitions += r.count();
        if (I.m.rep_k && same_rep(r, I.m.rep)) { E.uint_(0); return; }
        if (r.regular) {
            if (r.rows == 1) {
                if (r.dy == 0) { E.uint_(2); E.uint_((uint64_t)r.cols - 2); E.uint_((uint64_t)r.dx); }
                else if (r.dx == 0) { E.uint_(3); E.uint_((uint64_t)r.cols - 2); E.uint_((uint64_t)r.dy); }
                else { E.uint_(9); E.uint_((uint64_t)r.cols - 2); E.gdelta(r.dx, r.dy); }
            } else if (r.dy == 0 && r.rdx == 0 && r.rdy >= 0) {
                E.uint_(1); E.uint_((uint64_t)r.cols - 2); E.uint_((uint64_t)r.rows - 2); E.uint_((uint64_t)r.dx); E.uint_((uint64_t)r.rdy);
            } else {
                E.uint_(8); E.uint_((uint64_t)r.cols - 2); E.uint_((uint64_t)r.rows - 2); E.gdelta(r.dx, r.dy); E.gdelta(r.rdx, r.rdy);
            }
        } else {
            const auto& v = r.offs;                              // sorted by (y, x), v[0] = (0,0)
            bool on_x = true, on_y = true;
            for (auto& p : v) { on_x &= p.y == 0; on_y &= p.x == 0; }
            if (on_x || on_y) {                                  // types 4/5 (x) or 6/7 (y)
                uint64_t g = 0;
                std::vector<uint64_t> sp(v.size() - 1);
                for (size_t i = 1; i < v.size(); ++i) { sp[i - 1] = on_x ? (uint64_t)(v[i].x - v[i - 1].x) : (uint64_t)(v[i].y - v[i - 1].y); g = gcd64(g, sp[i - 1]); }
                size_t plain = 0, gridded = uint_len(g);
                for (uint64_t s : sp) { plain += uint_len(s); gridded += uint_len(g ? s / g : 0); }
                const bool use_grid = g > 1 && gridded < plain;
                E.uint_(on_x ? (use_grid ? 5 : 4) : (use_grid ? 7 : 6));
                E.uint_(v.size() - 2);
                if (use_grid) E.uint_(g);
                for (uint64_t s : sp) E.uint_(use_grid ? s / g : s);
            } else {                                             // types 10/11
                uint64_t g = 0;
                for (size_t i = 1; i < v.size(); ++i) { g = gcd64(g, umag(v[i].x - v[i - 1].x)); g = gcd64(g, umag(v[i].y - v[i - 1].y)); }
                const bool use_grid = g > 1;
                E.uint_(use_grid ? 11 : 10);
                E.uint_(v.size() - 2);
                if (use_grid) E.uint_(g);
                const int64_t gg = use_grid ? (int64_t)g : 1;
                for (size_t i = 1; i < v.size(); ++i) E.gdelta((v[i].x - v[i - 1].x) / gg, (v[i].y - v[i - 1].y) / gg);
            }
        }
        I.m.rep = r; I.m.rep_k = true;
    }

    // layer/datatype bits (L = 0x01, D = 0x02) for geometry
    uint8_t ld_bits(uint64_t l, uint64_t d) const {
        return (uint8_t)((!I.m.layer_k || I.m.layer != l ? 0x01 : 0) | (!I.m.dt_k || I.m.dt != d ? 0x02 : 0));
    }
    void put_ld(uint8_t info, uint64_t l, uint64_t d) {
        if (info & 0x01) e().uint_(l);
        if (info & 0x02) e().uint_(d);
        I.m.layer = l; I.m.dt = d; I.m.layer_k = I.m.dt_k = true;
    }

    // local bbox bookkeeping for S_BOUNDING_BOX
    void bbox_add(int64_t x1, int64_t y1, int64_t x2, int64_t y2, const Repetition* r) {
        if (!I.o.cell_bboxes || I.cur_cell < 0) return;
        int64_t a = 0, b = 0, c = 0, d = 0;
        if (r) rep_box(*r, a, b, c, d);
        auto& ci = I.cells[(size_t)I.cur_cell];
        x1 += a; y1 += b; x2 += c; y2 += d;
        if (!ci.has_box) { ci.x1 = x1; ci.y1 = y1; ci.x2 = x2; ci.y2 = y2; ci.has_box = true; return; }
        ci.x1 = std::min(ci.x1, x1); ci.y1 = std::min(ci.y1, y1); ci.x2 = std::max(ci.x2, x2); ci.y2 = std::max(ci.y2, y2);
    }
};

// --- point lists (7.7) -------------------------------------------------------
inline bool axis(int64_t dx, int64_t dy) { return (dx == 0) != (dy == 0); }
inline bool octo(int64_t dx, int64_t dy) { return dx == 0 || dy == 0 || umag(dx) == umag(dy); }

// b is between a and c on a straight line, continuing in the same direction.
inline bool straight(const Pt& a, const Pt& b, const Pt& c) {
    const __int128 ux = b.x - a.x, uy = b.y - a.y, vx = c.x - b.x, vy = c.y - b.y;
    return ux * vy - uy * vx == 0 && ux * vx + uy * vy > 0;
}

// Encode the point list of a polygon (closed, implicit closing edge) or a path, relative to
// v[0]. v has no repeated consecutive vertices and (polygon) no straight-through vertices.
void encode_points(const std::vector<Pt>& v, bool polygon, std::vector<uint8_t>& out) {
    const size_t n = v.size();
    const size_t ne = polygon ? n : n - 1;                       // edges incl. the closing one
    auto edx = [&](size_t i) { return v[(i + 1) % n].x - v[i].x; };
    auto edy = [&](size_t i) { return v[(i + 1) % n].y - v[i].y; };
    bool manh = true, oct = true, alt = true;
    for (size_t i = 0; i < ne; ++i) {
        const int64_t dx = edx(i), dy = edy(i);
        manh &= axis(dx, dy); oct &= octo(dx, dy);
        if (i > 0) alt &= ((dx == 0) != (edx(i - 1) == 0));
    }
    if (polygon) alt &= n >= 4 && n % 2 == 0;
    Encoder E;
    if (manh && alt && ne > 0) {                                 // types 0/1
        E.uint_(edx(0) != 0 ? 0 : 1);
        const size_t cnt = polygon ? n - 2 : n - 1;
        E.uint_(cnt);
        for (size_t i = 0; i < cnt; ++i) E.sint(edx(i) != 0 ? edx(i) : edy(i));
    } else if (manh || oct) {                                    // types 2/3
        const size_t cnt = polygon ? n - 1 : n - 1;
        E.uint_(manh ? 2 : 3); E.uint_(cnt);
        for (size_t i = 0; i < cnt; ++i) { if (manh) E.delta2(edx(i), edy(i)); else E.delta3(edx(i), edy(i)); }
    } else {                                                     // types 4/5, whichever is shorter
        const size_t cnt = n - 1;
        Encoder A, B;
        A.uint_(4); A.uint_(cnt);
        B.uint_(5); B.uint_(cnt);
        int64_t px = 0, py = 0;
        for (size_t i = 0; i < cnt; ++i) {
            A.gdelta(edx(i), edy(i));
            B.gdelta(edx(i) - px, edy(i) - py); px = edx(i); py = edy(i);
        }
        E.b = (B.b.size() < A.b.size()) ? B.b : A.b;
    }
    out.swap(E.b);
}

} // namespace

std::vector<uint8_t> encode_point_list_for_test(const std::vector<Pt>& polygon) {
    std::vector<uint8_t> out;
    if (polygon.size() >= 3) encode_points(polygon, true, out);
    return out;
}

#define CTX Ctx c{ *d_, err_, st_ }

namespace {
void put_values(Writer::Impl& I, Encoder& E, const std::vector<PropValue>& v);
}

// PROPERTY values (7.8): strings are n-, a- or b-strings by content; strict mode stores them
// as PROPSTRING references (types 13-15), otherwise inline (10-12).
namespace {
void put_values(Writer::Impl& I, Encoder& E, const std::vector<PropValue>& v) {
    if (v.size() >= 15) E.uint_(v.size());
    for (auto& pv : v) {
        switch (pv.kind) {
        case PropValue::UINT: E.uint_(8); E.uint_(pv.u); break;
        case PropValue::SINT: E.uint_(9); E.sint(pv.i); break;
        case PropValue::REAL: E.real(pv.r); break;
        case PropValue::STRING: {
            const int cls = is_nstring(pv.s) ? 2 : is_astring(pv.s) ? 0 : 1;   // a / b / n
            if (I.o.strict) { E.uint_(13 + cls); E.uint_(intern(I.pstr_ref, I.pstrs, pv.s)); }
            else { E.uint_(10 + cls); E.str(pv.s); }
        } break;
        }
    }
}

// File properties right after START (A2-1.1), then (if the top cells are not known yet)
// the reserved region that close() fills with S_TOP_CELL.
void write_header(Writer::Impl& I, Stats& st, bool allow_reserve) {
    I.header_done = true;
    Encoder h;
    auto prop = [&](const std::string& nm, const std::vector<PropValue>& v, bool std_) {
        uint8_t info = (uint8_t)(0x04 | (std_ ? 0x01 : 0) | (I.o.strict ? 0x02 : 0));
        info |= v.size() < 15 ? (uint8_t)(v.size() << 4) : 0xF0;
        h.u8(R_PROPERTY); h.u8(info);
        if (I.o.strict) h.uint_(intern(I.pname_ref, I.pnames, nm)); else h.str(nm);
        put_values(I, h, v);
        st.properties++;
    };
    if (I.o.strict && I.o.cell_bboxes) prop("S_BOUNDING_BOXES_AVAILABLE", { PropValue::uint_(2) }, true);
    for (auto& t : I.tops) prop("S_TOP_CELL", { PropValue::str(t) }, true);
    for (auto& fp : I.file_props) prop(fp.name, fp.v, fp.standard);
    I.out(h);
    if (allow_reserve && I.tops.empty() && I.o.header_reserve > 0 && I.fp && ftello(I.fp) >= 0) {
        I.reserved = true; I.reserve_off = I.pos; I.reserve_len = I.o.header_reserve;
        I.crc_pre = I.crc; I.sum_pre = I.sum;
        std::vector<uint8_t> z(I.reserve_len, 0);                // PAD records until patched (12.2)
        I.out(z.data(), z.size());
        I.crc = (uint32_t)crc32(0L, Z_NULL, 0); I.sum = 0; I.seg_len = 0;
    }
    I.file_props.clear();
}
}

void Writer::begin_cell(const std::string& name) {
    if (!ok()) return;
    Impl& I = *d_;
    if (I.in_cell) end_cell();
    if (!is_nstring(name)) { err_ = "cell name '" + name + "' is not an n-string"; return; }
    CTX;
    if (!I.header_done) write_header(I, st_, true);
    c.flush();                                                   // CELL is never compressed (35.4)
    const uint64_t ref = intern(I.cell_ref, I.cell_names, name);
    if (I.cells.size() <= ref) I.cells.resize(ref + 1);
    auto& ci = I.cells[ref];
    if (ci.defined) { err_ = "cell '" + name + "' is defined twice"; return; }
    ci.defined = true; ci.offset = I.pos;
    Encoder h;
    if (I.o.strict) { h.u8(R_CELL_REF); h.uint_(ref); } else { h.u8(R_CELL_NAME); h.str(name); }
    st_.record_bytes += h.b.size();
    I.out(h);
    I.m.cell_start();
    I.in_cell = true; I.cur_cell = (long)ref;
    st_.cells++;
}

void Writer::end_cell() {
    if (!ok()) return;
    CTX;
    c.flush();
    d_->in_cell = false; d_->cur_cell = -1;
}

void Writer::flush_cblock() { if (!ok()) return; CTX; c.flush(); }

void Writer::rectangle(uint64_t layer, uint64_t datatype, int64_t x, int64_t y, int64_t w, int64_t h, const Repetition* rp) {
    if (!ok()) return;
    if (!d_->in_cell) { err_ = "RECTANGLE outside a cell"; return; }
    if (w < 0) { x += w; w = -w; }
    if (h < 0) { y += h; h = -h; }
    Impl& I = *d_; CTX;
    Repetition r; int64_t ox = 0, oy = 0;
    const bool has_rep = rp && normalize(*rp, r, ox, oy);
    x += ox; y += oy;
    c.begin();
    const auto p = c.xy(x, y, I.m.g_k, I.m.gx, I.m.gy);
    uint8_t info = c.ld_bits(layer, datatype);
    const bool sq = (w == h);
    if (sq) { info |= 0x80; if (!I.m.w_k || I.m.w != w) info |= 0x40; }
    else { if (!I.m.w_k || I.m.w != w) info |= 0x40; if (!I.m.h_k || I.m.h != h) info |= 0x20; }
    if (p.X) info |= 0x10;
    if (p.Y) info |= 0x08;
    if (has_rep) info |= 0x04;
    Encoder& E = c.e();
    E.u8(R_RECTANGLE); E.u8(info);
    c.put_ld(info, layer, datatype);
    if (info & 0x40) E.uint_((uint64_t)w);
    if (info & 0x20) E.uint_((uint64_t)h);
    I.m.w = w; I.m.h = h; I.m.w_k = I.m.h_k = true;
    c.put_xy(p);
    if (has_rep) c.rep(r);
    c.bbox_add(x, y, x + w, y + h, has_rep ? &r : nullptr);
    st_.rectangles++;
    c.end();
}

void Writer::polygon(uint64_t layer, uint64_t datatype, const Pt* pts, size_t n, const Repetition* rp) {
    if (!ok()) return;
    if (!d_->in_cell) { err_ = "POLYGON outside a cell"; return; }
    // normalise: drop repeated and straight-through vertices (7.7.8), closing vertex included
    std::vector<Pt> v; v.reserve(n);
    for (size_t i = 0; i < n; ++i) {
        const Pt& q = pts[i];
        if (!v.empty() && v.back().x == q.x && v.back().y == q.y) continue;
        while (v.size() >= 2 && straight(v[v.size() - 2], v.back(), q)) v.pop_back();
        v.push_back(q);
    }
    while (v.size() >= 2 && v.back().x == v.front().x && v.back().y == v.front().y) v.pop_back();
    for (bool changed = true; changed && v.size() >= 3;) {
        changed = false;
        const size_t k = v.size();
        if (straight(v[k - 2], v[k - 1], v[0])) { v.pop_back(); changed = true; }
        else if (straight(v[k - 1], v[0], v[1])) { v.erase(v.begin()); changed = true; }
    }
    if (v.size() < 3) { st_.degenerate++; return; }
    // start at the lowest, then leftmost vertex: identical shapes share a point list
    size_t s = 0;
    for (size_t i = 1; i < v.size(); ++i) if (v[i].y < v[s].y || (v[i].y == v[s].y && v[i].x < v[s].x)) s = i;
    std::rotate(v.begin(), v.begin() + (long)s, v.end());
    if (v.size() == 4 && axis(v[1].x - v[0].x, v[1].y - v[0].y)) {           // rectangle?
        bool rect = true;
        for (size_t i = 0; i < 4; ++i) rect &= axis(v[(i + 1) % 4].x - v[i].x, v[(i + 1) % 4].y - v[i].y);
        if (rect) {
            int64_t x1 = v[0].x, y1 = v[0].y, x2 = x1, y2 = y1;
            for (auto& q : v) { x1 = std::min(x1, q.x); y1 = std::min(y1, q.y); x2 = std::max(x2, q.x); y2 = std::max(y2, q.y); }
            rectangle(layer, datatype, x1, y1, x2 - x1, y2 - y1, rp);
            return;
        }
    }
    Impl& I = *d_; CTX;
    std::vector<uint8_t> pl;
    encode_points(v, true, pl);
    Repetition r; int64_t ox = 0, oy = 0;
    const bool has_rep = rp && normalize(*rp, r, ox, oy);
    const int64_t x = v[0].x + ox, y = v[0].y + oy;
    c.begin();
    const auto p = c.xy(x, y, I.m.g_k, I.m.gx, I.m.gy);
    uint8_t info = c.ld_bits(layer, datatype);
    const bool same_pl = I.m.ppl_k && I.m.ppl == pl;
    if (!same_pl) info |= 0x20;
    if (p.X) info |= 0x10;
    if (p.Y) info |= 0x08;
    if (has_rep) info |= 0x04;
    Encoder& E = c.e();
    E.u8(R_POLYGON); E.u8(info);
    c.put_ld(info, layer, datatype);
    if (!same_pl) { E.bytes(pl.data(), pl.size()); I.m.ppl.swap(pl); I.m.ppl_k = true; }
    c.put_xy(p);
    if (has_rep) c.rep(r);
    int64_t x1 = v[0].x, y1 = v[0].y, x2 = x1, y2 = y1;
    for (auto& q : v) { x1 = std::min(x1, q.x); y1 = std::min(y1, q.y); x2 = std::max(x2, q.x); y2 = std::max(y2, q.y); }
    c.bbox_add(x1 + ox, y1 + oy, x2 + ox, y2 + oy, has_rep ? &r : nullptr);
    st_.polygons++;
    c.end();
}

void Writer::path(uint64_t layer, uint64_t datatype, const Pt* pts, size_t n, int64_t hw, int64_t es, int64_t ee, const Repetition* rp) {
    if (!ok()) return;
    if (!d_->in_cell) { err_ = "PATH outside a cell"; return; }
    if (hw < 0) hw = -hw;
    std::vector<Pt> v; v.reserve(n);
    for (size_t i = 0; i < n; ++i) {
        const Pt& q = pts[i];
        if (!v.empty() && v.back().x == q.x && v.back().y == q.y) continue;
        while (v.size() >= 2 && straight(v[v.size() - 2], v.back(), q)) v.pop_back();
        v.push_back(q);
    }
    if (v.empty()) { st_.degenerate++; return; }
    Impl& I = *d_; CTX;
    std::vector<uint8_t> pl;
    encode_points(v, false, pl);
    Repetition r; int64_t ox = 0, oy = 0;
    const bool has_rep = rp && normalize(*rp, r, ox, oy);
    const int64_t x = v[0].x + ox, y = v[0].y + oy;
    c.begin();
    const auto p = c.xy(x, y, I.m.g_k, I.m.gx, I.m.gy);
    uint8_t info = c.ld_bits(layer, datatype);
    if (!I.m.hw_k || I.m.hw != hw) info |= 0x40;
    // extension scheme 0000SSEE (27.8): 0 modal, 1 flush, 2 half-width, 3 explicit
    auto code = [&](int64_t ext, bool known, int64_t modal) -> unsigned {
        if (known && modal == ext) return 0;
        if (ext == 0) return 1;
        if (ext == hw) return 2;
        return 3;
    };
    const unsigned ss = code(es, I.m.es_k, I.m.es), eec = code(ee, I.m.ee_k, I.m.ee);
    if (ss || eec) info |= 0x80;
    const bool same_pl = I.m.hpl_k && I.m.hpl == pl;
    if (!same_pl) info |= 0x20;
    if (p.X) info |= 0x10;
    if (p.Y) info |= 0x08;
    if (has_rep) info |= 0x04;
    Encoder& E = c.e();
    E.u8(R_PATH); E.u8(info);
    c.put_ld(info, layer, datatype);
    if (info & 0x40) E.uint_((uint64_t)hw);
    I.m.hw = hw; I.m.hw_k = true;
    if (info & 0x80) {
        E.uint_((ss << 2) | eec);
        if (ss == 3) E.sint(es);
        if (eec == 3) E.sint(ee);
    }
    I.m.es = es; I.m.ee = ee; I.m.es_k = I.m.ee_k = true;
    if (!same_pl) { E.bytes(pl.data(), pl.size()); I.m.hpl.swap(pl); I.m.hpl_k = true; }
    c.put_xy(p);
    if (has_rep) c.rep(r);
    // bbox: exact for Manhattan centrelines, else the mitred outline (fracture.hpp path_bbox)
    std::vector<ldbgeom::Rect> rects;
    int64_t x1, y1, x2, y2;
    if (hw > 0 && ldbgeom::path_to_rects(v, hw, es, ee, rects) && !rects.empty()) {
        x1 = rects[0][0]; y1 = rects[0][1]; x2 = rects[0][2]; y2 = rects[0][3];
        for (auto& q : rects) { x1 = std::min(x1, q[0]); y1 = std::min(y1, q[1]); x2 = std::max(x2, q[2]); y2 = std::max(y2, q[3]); }
    } else if (!ldbgeom::path_bbox(v, hw, es, ee, x1, y1, x2, y2)) {      // one point
        const int64_t g = std::max<int64_t>(hw, std::max<int64_t>(0, std::max(es, ee)));
        x1 = x2 = v[0].x; y1 = y2 = v[0].y;
        for (auto& q : v) { x1 = std::min(x1, q.x); y1 = std::min(y1, q.y); x2 = std::max(x2, q.x); y2 = std::max(y2, q.y); }
        x1 -= g; y1 -= g; x2 += g; y2 += g;
    }
    c.bbox_add(x1 + ox, y1 + oy, x2 + ox, y2 + oy, has_rep ? &r : nullptr);
    st_.paths++;
    c.end();
}

void Writer::circle(uint64_t layer, uint64_t datatype, int64_t x, int64_t y, int64_t radius, const Repetition* rp) {
    if (!ok()) return;
    if (!d_->in_cell) { err_ = "CIRCLE outside a cell"; return; }
    if (radius < 0) radius = -radius;
    Impl& I = *d_; CTX;
    Repetition r; int64_t ox = 0, oy = 0;
    const bool has_rep = rp && normalize(*rp, r, ox, oy);
    x += ox; y += oy;
    c.begin();
    const auto p = c.xy(x, y, I.m.g_k, I.m.gx, I.m.gy);
    uint8_t info = c.ld_bits(layer, datatype);
    if (!I.m.rad_k || I.m.rad != radius) info |= 0x20;
    if (p.X) info |= 0x10;
    if (p.Y) info |= 0x08;
    if (has_rep) info |= 0x04;
    Encoder& E = c.e();
    E.u8(R_CIRCLE); E.u8(info);
    c.put_ld(info, layer, datatype);
    if (info & 0x20) E.uint_((uint64_t)radius);
    I.m.rad = radius; I.m.rad_k = true;
    c.put_xy(p);
    if (has_rep) c.rep(r);
    c.bbox_add(x - radius, y - radius, x + radius, y + radius, has_rep ? &r : nullptr);
    st_.circles++;
    c.end();
}

void Writer::text(uint64_t tl, uint64_t tt, int64_t x, int64_t y, const std::string& s0, const Repetition* rp) {
    if (!ok()) return;
    if (!d_->in_cell) { err_ = "TEXT outside a cell"; return; }
    std::string s = s0;                                          // a-string (7.4): printable + SP
    for (char& ch : s) if ((unsigned char)ch < 0x20 || (unsigned char)ch > 0x7e) ch = '?';
    Impl& I = *d_; CTX;
    Repetition r; int64_t ox = 0, oy = 0;
    const bool has_rep = rp && normalize(*rp, r, ox, oy);
    x += ox; y += oy;
    c.begin();
    const auto p = c.xy(x, y, I.m.t_k, I.m.tx, I.m.ty);
    uint8_t info = 0;
    uint64_t ref = 0;
    bool same;
    if (I.o.strict) { ref = intern(I.text_ref, I.texts, s); same = I.m.tstr_k && I.m.tstr == ref; }
    else same = I.m.tstr_k && I.m.tstr_s == s;
    if (!same) info |= (uint8_t)(0x40 | (I.o.strict ? 0x20 : 0));
    if (!I.m.tl_k || I.m.tl != tl) info |= 0x01;
    if (!I.m.tt_k || I.m.tt != tt) info |= 0x02;
    if (p.X) info |= 0x10;
    if (p.Y) info |= 0x08;
    if (has_rep) info |= 0x04;
    Encoder& E = c.e();
    E.u8(R_TEXT); E.u8(info);
    if (!same) { if (I.o.strict) E.uint_(ref); else E.str(s); }
    I.m.tstr = ref; I.m.tstr_s = s; I.m.tstr_k = true;
    if (info & 0x01) E.uint_(tl);
    if (info & 0x02) E.uint_(tt);
    I.m.tl = tl; I.m.tt = tt; I.m.tl_k = I.m.tt_k = true;
    c.put_xy(p);
    if (has_rep) c.rep(r);
    c.bbox_add(x, y, x, y, has_rep ? &r : nullptr);
    st_.texts++;
    c.end();
}

void Writer::placement(const std::string& cell, const Transform& t, const Repetition* rp) {
    if (!ok()) return;
    if (!d_->in_cell) { err_ = "PLACEMENT outside a cell"; return; }
    if (!is_nstring(cell)) { err_ = "cell name '" + cell + "' is not an n-string"; return; }
    if (!(t.mag > 0) || !std::isfinite(t.mag) || !std::isfinite(t.angle)) { err_ = "PLACEMENT with an invalid magnification or angle"; return; }
    Impl& I = *d_; CTX;
    const uint64_t ref = intern(I.cell_ref, I.cell_names, cell);
    if (I.cells.size() <= ref) I.cells.resize(ref + 1);
    I.cells[ref].placed = true;
    Repetition r; int64_t ox = 0, oy = 0;
    const bool has_rep = rp && normalize(*rp, r, ox, oy);
    const int64_t x = t.x + ox, y = t.y + oy;
    c.begin();
    const auto p = c.xy(x, y, I.m.p_k, I.m.px, I.m.py);
    const double q = t.angle / 90.0;
    const bool simple = t.mag == 1.0 && q == std::floor(q);
    uint8_t info = 0;
    const bool same = I.o.strict ? (I.m.pcell_k && I.m.pcell == ref) : (I.m.pcell_k && I.m.pcell_s == cell);
    if (!same) info |= (uint8_t)(0x80 | (I.o.strict ? 0x40 : 0));
    if (p.X) info |= 0x20;
    if (p.Y) info |= 0x10;
    if (has_rep) info |= 0x08;
    if (t.mirror) info |= 0x01;
    if (simple) info |= (uint8_t)((((long long)q % 4 + 4) % 4) << 1);
    else { if (t.mag != 1.0) info |= 0x04; if (t.angle != 0.0) info |= 0x02; }
    Encoder& E = c.e();
    E.u8(simple ? R_PLACEMENT : R_PLACEMENT_X); E.u8(info);
    if (!same) { if (I.o.strict) E.uint_(ref); else E.str(cell); }
    I.m.pcell = ref; I.m.pcell_s = cell; I.m.pcell_k = true;
    if (!simple) { if (info & 0x04) E.real(t.mag); if (info & 0x02) E.real(t.angle); }
    c.put_xy(p);
    if (has_rep) c.rep(r);
    if (I.o.cell_bboxes && I.cur_cell >= 0) {
        Impl::Inst in{ ref, t, 0, 0, 0, 0 };
        in.t.x = x; in.t.y = y;
        if (has_rep) rep_box(r, in.rx1, in.ry1, in.rx2, in.ry2);
        I.cells[(size_t)I.cur_cell].insts.push_back(in);
    }
    st_.placements++;
    c.end();
}

void Writer::property(const std::string& name, const std::vector<PropValue>& v, bool standard) {
    if (!ok()) return;
    Impl& I = *d_;
    if (!I.header_done) { file_property(name, v, standard); return; }
    if (!I.in_cell) { err_ = "element PROPERTY outside a cell"; return; }
    if (!is_nstring(name)) { err_ = "property name '" + name + "' is not an n-string"; return; }
    CTX;
    c.begin();
    uint64_t ref = 0;
    bool same_name;
    if (I.o.strict) { ref = intern(I.pname_ref, I.pnames, name); same_name = I.m.pn_k && I.m.pn == ref; }
    else same_name = I.m.pn_k && I.m.pn_s == name;
    same_name = same_name && I.m.pn_std == standard;
    const bool same_vals = I.m.pv_k && I.m.pv == v;
    Encoder& E = c.e();
    if (same_name && same_vals) E.u8(R_PROPERTY_REPEAT);         // record 29 (31.2)
    else {
        uint8_t info = standard ? 0x01 : 0;
        if (!same_name) info |= (uint8_t)(0x04 | (I.o.strict ? 0x02 : 0));
        if (same_vals) info |= 0x08;
        else if (v.size() < 15) info |= (uint8_t)(v.size() << 4);
        else info |= 0xF0;
        E.u8(R_PROPERTY); E.u8(info);
        if (!same_name) { if (I.o.strict) E.uint_(ref); else E.str(name); }
        if (!same_vals) put_values(I, E, v);
    }
    I.m.pn = ref; I.m.pn_s = name; I.m.pn_std = standard; I.m.pn_k = true;
    I.m.pv = v; I.m.pv_k = true;
    st_.properties++;
    c.end();
}

bool Writer::close() {
    Impl& I = *d_;
    if (I.closed) return ok();
    I.closed = true;
    if (!ok()) return false;
    if (I.in_cell) end_cell();
    CTX;
    if (!I.header_done) write_header(I, st_, false);           // a file without cells
    c.flush();

    // --- S_TOP_CELL into the reserved header region -------------------------------
    if (I.reserved) {
        std::vector<std::string> tops;
        for (size_t i = 0; i < I.cells.size(); ++i) if (I.cells[i].defined && !I.cells[i].placed) tops.push_back(I.cell_names[i]);
        Encoder p;
        for (auto& t : tops) {
            Encoder one;
            one.u8(R_PROPERTY);
            one.u8((uint8_t)(0x10 | 0x04 | 0x01 | (I.o.strict ? 0x02 : 0)));
            if (I.o.strict) { one.uint_(intern(I.pname_ref, I.pnames, "S_TOP_CELL")); one.uint_(15); one.uint_(intern(I.pstr_ref, I.pstrs, t)); }
            else { one.str("S_TOP_CELL"); one.uint_(12); one.str(t); }
            if (p.b.size() + one.b.size() > I.reserve_len) break;
            p.bytes(one.b.data(), one.b.size());
            st_.top_cells++;
        }
        I.patch = p.b;
        I.patch.resize(I.reserve_len, 0);                         // remainder stays PAD
    } else st_.top_cells = I.tops.size();

    // --- strict name tables ----------------------------------------------------------
    uint64_t toff[6] = { 0, 0, 0, 0, 0, 0 };                      // CELLNAME TEXTSTRING PROPNAME PROPSTRING LAYERNAME XNAME
    auto table = [&](int slot, const std::function<void(Encoder&)>& fill) {
        Encoder& E = I.blk;
        fill(E);
        if (E.b.empty()) return;
        toff[slot] = I.pos;                                      // first byte of its CBLOCK / first record
        c.flush();
    };
    auto std_prop = [&](Encoder& E, const char* nm, const std::vector<std::pair<int, uint64_t>>& vals) {
        E.u8(R_PROPERTY);
        E.u8((uint8_t)((vals.size() << 4) | 0x04 | 0x02 | 0x01));
        E.uint_(intern(I.pname_ref, I.pnames, nm));
        for (auto& v : vals) { E.uint_((uint64_t)v.first); if (v.first == 9) E.sint((int64_t)v.second); else E.uint_(v.second); }
    };
    if (I.o.strict) {
        // hierarchical bounding boxes (A2-2.2)
        struct BB { int state = 0; bool known = false, empty = true, ext = false; int64_t x1 = 0, y1 = 0, x2 = 0, y2 = 0; };
        std::vector<BB> bb(I.cells.size());
        std::function<void(size_t)> solve = [&](size_t i) {
            BB& b = bb[i];
            if (b.state == 2) return;
            if (b.state == 1) { b.known = false; return; }       // recursion: unknown
            b.state = 1;
            const auto& ci = I.cells[i];
            if (!ci.defined) { b.known = false; b.ext = true; b.state = 2; return; }
            b.known = true;
            auto add = [&](int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
                if (b.empty) { b.x1 = x1; b.y1 = y1; b.x2 = x2; b.y2 = y2; b.empty = false; return; }
                b.x1 = std::min(b.x1, x1); b.y1 = std::min(b.y1, y1); b.x2 = std::max(b.x2, x2); b.y2 = std::max(b.y2, y2);
            };
            if (ci.has_box) add(ci.x1, ci.y1, ci.x2, ci.y2);
            for (const auto& in : ci.insts) {
                solve((size_t)in.child);
                const BB& cb = bb[(size_t)in.child];
                if (cb.ext) b.ext = true;
                if (!cb.known || cb.state != 2) { b.known = false; continue; }
                if (cb.empty) continue;
                const double q = in.t.angle / 90.0;
                const bool ortho = in.t.mag == 1.0 && q == std::floor(q);
                const int quarter = ortho ? (int)(((long long)q % 4 + 4) % 4) : 0;
                const double rad = in.t.angle * M_PI / 180.0, ca = std::cos(rad), sa = std::sin(rad);
                int64_t tx1 = INT64_MAX, ty1 = INT64_MAX, tx2 = INT64_MIN, ty2 = INT64_MIN;
                const int64_t xs[2] = { cb.x1, cb.x2 }, ys[2] = { cb.y1, cb.y2 };
                for (int a = 0; a < 2; ++a) for (int bq = 0; bq < 2; ++bq) {
                    const int64_t px = xs[a], py = in.t.mirror ? -ys[bq] : ys[bq];
                    int64_t lo_x, lo_y, hi_x, hi_y;
                    if (ortho) {
                        int64_t rx = px, ry = py;
                        if (quarter == 1) { rx = -py; ry = px; } else if (quarter == 2) { rx = -px; ry = -py; } else if (quarter == 3) { rx = py; ry = -px; }
                        lo_x = hi_x = rx; lo_y = hi_y = ry;
                    } else {
                        const double fx = in.t.mag * (ca * px - sa * py), fy = in.t.mag * (sa * px + ca * py);
                        lo_x = (int64_t)std::floor(fx); hi_x = (int64_t)std::ceil(fx); lo_y = (int64_t)std::floor(fy); hi_y = (int64_t)std::ceil(fy);
                    }
                    tx1 = std::min(tx1, lo_x); ty1 = std::min(ty1, lo_y); tx2 = std::max(tx2, hi_x); ty2 = std::max(ty2, hi_y);
                }
                add(tx1 + in.t.x + in.rx1, ty1 + in.t.y + in.ry1, tx2 + in.t.x + in.rx2, ty2 + in.t.y + in.ry2);
            }
            b.state = 2;
        };
        if (I.o.cell_bboxes) for (size_t i = 0; i < I.cells.size(); ++i) solve(i);
        table(0, [&](Encoder& E) {
            for (size_t i = 0; i < I.cell_names.size(); ++i) {
                E.u8(R_CELLNAME); E.str(I.cell_names[i]);        // implicit reference numbers 0, 1, ...
                const auto& ci = I.cells[i];
                std_prop(E, "S_CELL_OFFSET", { { 8, ci.defined ? ci.offset : 0 } });   // 0 = external cell
                if (I.o.cell_bboxes) {
                    const BB& b = bb[i];
                    uint64_t flags = b.known ? (b.empty ? 2 : 0) : 1;
                    if (b.ext) flags |= 4;
                    const bool have = b.known && !b.empty;
                    std_prop(E, "S_BOUNDING_BOX", { { 8, flags }, { 9, (uint64_t)(have ? b.x1 : 0) }, { 9, (uint64_t)(have ? b.y1 : 0) },
                                                    { 8, have ? (uint64_t)(b.x2 - b.x1) : 0 }, { 8, have ? (uint64_t)(b.y2 - b.y1) : 0 } });
                }
            }
        });
        table(1, [&](Encoder& E) { for (auto& s : I.texts) { E.u8(R_TEXTSTRING); E.str(s); } });
        table(2, [&](Encoder& E) { for (auto& s : I.pnames) { E.u8(R_PROPNAME); E.str(s); } });
        table(3, [&](Encoder& E) { for (auto& s : I.pstrs) { E.u8(R_PROPSTRING); E.str(s); } });
    }
    table(4, [&](Encoder& E) {
        for (auto& l : I.layers) {
            E.u8(l.text ? R_LAYERNAME_TEXT : R_LAYERNAME); E.str(l.name);
            E.uint_(3); E.uint_(l.l); E.uint_(3); E.uint_(l.d);   // exact layer / datatype
        }
    });
    if (!I.o.strict) toff[4] = 0;                                 // non-strict: no table offsets at all

    // --- END: exactly 256 bytes (14.2) -------------------------------------------------
    Encoder e;
    e.u8(R_END);
    if (I.o.strict) for (int i = 0; i < 6; ++i) { e.uint_(1); e.uint_(toff[i]); }
    const size_t sig = I.o.validation ? 4 : 0;
    const size_t fixed = e.b.size() + 1 + sig;                   // + validation-scheme byte + signature
    size_t pad;
    if (256 - fixed - 1 < 128) { pad = 256 - fixed - 1; e.uint_(pad); }          // 1-byte length
    else { pad = 256 - fixed - 2; e.u8((uint8_t)(0x80 | (pad & 0x7f))); e.u8((uint8_t)(pad >> 7)); }   // 2 bytes
                                                                 // (127 then has a redundant zero group: legal, 7.2.1)
    e.b.insert(e.b.end(), pad, 0);
    e.uint_((uint64_t)I.o.validation);
    if (e.b.size() + sig != 256) { err_ = "internal: END record is " + std::to_string(e.b.size() + sig) + " bytes"; return false; }
    I.out(e);
    if (I.o.validation) {
        uint32_t v;
        if (I.o.validation == 1) {
            v = I.crc;
            if (I.reserved) {
                const uint32_t cp = (uint32_t)crc32(crc32(0L, Z_NULL, 0), I.patch.data(), (uInt)I.patch.size());
                v = (uint32_t)crc32_combine(crc32_combine(I.crc_pre, cp, (z_off_t)I.patch.size()), I.crc, (z_off_t)I.seg_len);
            }
        } else {
            v = I.sum;
            if (I.reserved) { v += I.sum_pre; for (uint8_t b : I.patch) v += b; }
        }
        uint8_t s4[4] = { (uint8_t)v, (uint8_t)(v >> 8), (uint8_t)(v >> 16), (uint8_t)(v >> 24) };
        I.obuf.insert(I.obuf.end(), s4, s4 + 4); I.pos += 4;
    }
    st_.bytes = I.pos;
    bool okw = I.drain();
    if (okw && I.reserved && !I.patch.empty()) {
        okw = fseeko(I.fp, (off_t)I.reserve_off, SEEK_SET) == 0 && fwrite(I.patch.data(), 1, I.patch.size(), I.fp) == I.patch.size();
    }
    if (fclose(I.fp) != 0) okw = false;
    I.fp = nullptr;
    if (!okw) { err_ = "write failed"; return false; }
    return true;
}

// ---------------------------------------------------------------------------
// RectArrays
// ---------------------------------------------------------------------------
void RectArrays::add(int64_t x, int64_t y, int64_t w, int64_t h) {
    if (!have_ || w != cw_ || h != ch_ || y != cy_) {
        if (have_) {
            row_done();
            if (w != cw_ || h != ch_) { close_all(); flush(INT64_MAX); }
        }
        have_ = true; cw_ = w; ch_ = h; cy_ = y;
    }
    row_.push_back(x);
}

// A stack "blocks" output while it holds fewer than min_elems rectangles: it may still be
// expanded into single rectangles, which must go out in (y, x) order with their neighbours.
void RectArrays::unblock(const Stack& s) {
    auto it = blocking_.find(s.y0);
    if (it != blocking_.end() && --it->second == 0) blocking_.erase(it);
}

void RectArrays::row_done() {
    while (!due_.empty() && due_.top().expiry < cy_) {          // stacks that cannot grow any more
        const Due d = due_.top(); due_.pop();
        auto it = open_.find(d.k);
        if (it != open_.end() && it->second.gen == d.gen) { close_stack(it->first, it->second); open_.erase(it); }
    }
    const int64_t window = std::max<int64_t>(1, wf_ * std::max(cw_, ch_));
    const size_t n = row_.size();
    for (size_t i = 0; i < n;) {
        size_t j = i + 1; int64_t p = 0;
        if (j < n && row_[j] > row_[i]) { p = row_[j] - row_[i]; while (j < n && row_[j] - row_[j - 1] == p) ++j; }
        const Key k{ row_[i], (int64_t)(j - i), p };
        auto it = open_.find(k);
        bool placed = false;
        if (it != open_.end()) {
            Stack& s = it->second;
            const int64_t dy = cy_ - s.last_y;
            const bool was_blocking = k.n * s.ny < me_;
            if ((s.ny == 1 && dy > 0 && dy <= window) || (s.ny >= 2 && dy == s.py)) {
                if (s.ny == 1) s.py = dy;
                s.ny++; s.last_y = cy_; s.gen = ++gen_; due_.push({ cy_ + s.py, k, s.gen }); placed = true;
                if (was_blocking && k.n * s.ny >= me_) unblock(s);
            } else { close_stack(it->first, s); open_.erase(it); }
        }
        if (!placed) {
            Stack s{ cy_, 0, 1, cy_, ++gen_ };
            if (k.n < me_) blocking_[cy_]++;
            open_.emplace(k, s);
            due_.push({ cy_ + window, k, s.gen });
        }
        i = j;
    }
    row_.clear();
    if (out_.size() >= 4096) flush(blocking_.empty() ? INT64_MAX : blocking_.begin()->first);
}

void RectArrays::close_stack(const Key& k, const Stack& s) {
    if (k.n * s.ny < me_) {                                     // too small for a repetition
        unblock(s);
        for (int64_t j = 0; j < s.ny; ++j)
            for (int64_t i = 0; i < k.n; ++i) out_.push_back({ s.y0 + j * s.py, k.x0 + i * k.p, 1, 0, 1, 0 });
        return;
    }
    out_.push_back({ s.y0, k.x0, k.n, k.p, s.ny, s.py });
}

// Write the buffered items with y < limit in (y, x) order.
void RectArrays::flush(int64_t limit) {
    std::sort(out_.begin(), out_.end(), [](const Item& a, const Item& b) { return a.y != b.y ? a.y < b.y : a.x < b.x; });
    size_t k = 0;
    for (; k < out_.size() && out_[k].y < limit; ++k) {
        const Item& it = out_[k];
        if (it.n * it.ny == 1) { w_.rectangle(layer_, dt_, it.x, it.y, cw_, ch_); continue; }
        Repetition r = Repetition::lattice(it.n, it.p, 0, it.ny, 0, it.py);
        w_.rectangle(layer_, dt_, it.x, it.y, cw_, ch_, &r);
        ++arrays_;
    }
    out_.erase(out_.begin(), out_.begin() + (ptrdiff_t)k);
}

void RectArrays::close_all() {
    for (auto& kv : open_) close_stack(kv.first, kv.second);
    open_.clear();
    blocking_.clear();
    due_ = std::priority_queue<Due>();
}

void RectArrays::finish() {
    if (!have_) return;
    row_done();
    close_all();
    flush(INT64_MAX);
    have_ = false;
}

} // namespace oasis_writer

