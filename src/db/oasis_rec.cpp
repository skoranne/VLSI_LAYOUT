// Lossless OASIS record scanner: see oasis_rec.hpp.
//
// Field order and encodings follow SEMI P39 and match oasis.cpp, whose byte
// decoders (oasis_codec.hpp) are shared.

#include "oasis_rec.hpp"
#include "nvcomp_support.hpp"
#include <cmath>
#include <cstring>

namespace oasis_rec {

using oasis_codec::Reader;
using oasis_codec::Repetition;
using oasis_codec::Pt;

namespace {

struct Modal {
    bool relative = false;
    int64_t place_x = 0, place_y = 0, geom_x = 0, geom_y = 0, text_x = 0, text_y = 0;
    uint64_t layer = 0, datatype = 0, textlayer = 0, texttype = 0;
    int64_t geom_w = 0, geom_h = 0, half_width = 0, ext_start = 0, ext_end = 0, radius = 0;
    uint64_t ctrap_type = 0;
    bool cell_by_number = false; uint64_t cell_num = 0; std::string cell_name;
    Repetition rep;
    std::vector<Pt> polygon_pts, path_pts;
    void reset_for_cell() { relative = false; place_x = place_y = geom_x = geom_y = text_x = text_y = 0; }
};

struct Scanner {
    bool geometry;
    const std::function<void(const Record&)>& cb;
    FileInfo& info;
    Modal m;
    Record r;
    bool in_cell = false, saw_end = false, in_cblock = false;
    db::NvcompStatus nvst;
    std::vector<ldbgeom::Rect> rects;
    std::vector<Pt> verts;

    Scanner(bool g, const std::function<void(const Record&)>& c, FileInfo& i) : geometry(g), cb(c), info(i) {
        nvst = db::nvcomp_init();
    }

    // --- helpers --------------------------------------------------------------
    size_t rec_start = 0;
    void ref_num(Reader& s, RefKind k, uint64_t* out = nullptr) {
        size_t a = s.off; uint64_t v = s.uint_();
        RefSpan sp; sp.off = (uint32_t)(a - rec_start); sp.len = (uint32_t)(s.off - a); sp.kind = k; sp.num = v;
        r.refs.push_back(std::move(sp));
        if (out) *out = v;
    }
    void ref_str(Reader& s, RefKind k, std::string* out = nullptr) {
        size_t a = s.off; std::string v = s.str();
        RefSpan sp; sp.off = (uint32_t)(a - rec_start); sp.len = (uint32_t)(s.off - a); sp.kind = k; sp.str = v;
        if (out) *out = v;
        r.refs.push_back(std::move(sp));
    }
    void read_xy(Reader& s, uint8_t inf, int64_t& x, int64_t& y) {   // X = bit 4, Y = bit 3
        if (inf & 0x10) { int64_t v = s.sint(); x = m.relative ? x + v : v; }
        if (inf & 0x08) { int64_t v = s.sint(); y = m.relative ? y + v : v; }
    }
    void read_layer(Reader& s, uint8_t inf) {
        if (inf & 0x01) m.layer = s.uint_();
        if (inf & 0x02) m.datatype = s.uint_();
    }
    // Box of all repetition offsets (0,0 included); `has` = repetition present.
    static void rep_box(const Repetition& rep, int64_t& x1, int64_t& y1, int64_t& x2, int64_t& y2) {
        x1 = y1 = x2 = y2 = 0;
        auto add = [&](int64_t x, int64_t y) { x1 = std::min(x1, x); y1 = std::min(y1, y); x2 = std::max(x2, x); y2 = std::max(y2, y); };
        if (!rep.regular) { for (auto& o : rep.offs) add(o.x, o.y); return; }
        const int64_t c = rep.cols - 1, rr = rep.rows - 1;
        add(c * rep.dx, c * rep.dy); add(rr * rep.rdx, rr * rep.rdy);
        add(c * rep.dx + rr * rep.rdx, c * rep.dy + rr * rep.rdy);
    }
    bool read_rep(Reader& s, uint8_t inf, uint8_t bit) {
        if (!(inf & bit)) return false;
        oasis_codec::read_repetition(s, m.rep); return true;
    }
    void shape(int64_t x1, int64_t y1, int64_t x2, int64_t y2, bool has_rep, uint64_t layer, uint64_t dt) {
        r.is_shape = true; r.layer = layer; r.datatype = dt;
        if (!geometry) return;
        int64_t a = 0, b = 0, c = 0, d = 0;
        if (has_rep) rep_box(m.rep, a, b, c, d);
        r.box.add(std::min(x1, x2) + a, std::min(y1, y2) + b);
        r.box.add(std::max(x1, x2) + c, std::max(y1, y2) + d);
    }
    void shape_pts(const std::vector<Pt>& v, int64_t grow, bool has_rep) {
        if (v.empty()) return;
        int64_t x1 = v[0].x, x2 = v[0].x, y1 = v[0].y, y2 = v[0].y;
        for (auto& q : v) { x1 = std::min(x1, q.x); x2 = std::max(x2, q.x); y1 = std::min(y1, q.y); y2 = std::max(y2, q.y); }
        shape(x1 - grow, y1 - grow, x2 + grow, y2 + grow, has_rep, m.layer, m.datatype);
    }

    // --- records ----------------------------------------------------------------
    void placement(Reader& s, bool general) {
        uint8_t inf = s.u8();
        if (inf & 0x80) {
            m.cell_by_number = (inf & 0x40) != 0;
            if (m.cell_by_number) ref_num(s, REF_CELL_NUM, &m.cell_num);
            else ref_str(s, REF_CELL_STR, &m.cell_name);
        }
        Placement& p = r.pl;
        p.mag = 1.0; p.angle = 0.0;
        if (general) { if (inf & 0x04) p.mag = s.real(); if (inf & 0x02) p.angle = s.real(); }
        else p.angle = 90.0 * ((inf >> 1) & 3);
        p.mirror = (inf & 0x01) != 0;
        if (inf & 0x20) { int64_t v = s.sint(); m.place_x = m.relative ? m.place_x + v : v; }
        if (inf & 0x10) { int64_t v = s.sint(); m.place_y = m.relative ? m.place_y + v : v; }
        bool has_rep = read_rep(s, inf, 0x08);
        r.is_placement = true;
        p.by_number = m.cell_by_number; p.num = m.cell_num; p.name = m.cell_name;
        p.x = m.place_x; p.y = m.place_y;
        p.rx1 = p.ry1 = p.rx2 = p.ry2 = 0;
        if (has_rep && geometry) rep_box(m.rep, p.rx1, p.ry1, p.rx2, p.ry2);
    }

    void text(Reader& s) {
        uint8_t inf = s.u8();
        if (inf & 0x40) { if (inf & 0x20) ref_num(s, REF_TEXT_NUM); else s.skip_str(); }
        if (inf & 0x01) m.textlayer = s.uint_();
        if (inf & 0x02) m.texttype = s.uint_();
        read_xy(s, inf, m.text_x, m.text_y);
        read_rep(s, inf, 0x04);
        // Texts have no area: not a shape for extents, but the layer counts for statistics.
        r.layer = m.textlayer; r.datatype = m.texttype;
    }

    void rectangle(Reader& s) {
        uint8_t inf = s.u8();
        read_layer(s, inf);
        if (inf & 0x40) m.geom_w = (int64_t)s.uint_();
        if (inf & 0x20) m.geom_h = (int64_t)s.uint_();
        if (inf & 0x80) m.geom_h = m.geom_w;
        read_xy(s, inf, m.geom_x, m.geom_y);
        bool rep = read_rep(s, inf, 0x04);
        shape(m.geom_x, m.geom_y, m.geom_x + m.geom_w, m.geom_y + m.geom_h, rep, m.layer, m.datatype);
    }

    void polygon(Reader& s) {
        uint8_t inf = s.u8();
        read_layer(s, inf);
        if (inf & 0x20) oasis_codec::read_point_list(s, true, m.polygon_pts);
        read_xy(s, inf, m.geom_x, m.geom_y);
        bool rep = read_rep(s, inf, 0x04);
        verts.clear(); verts.push_back({ m.geom_x, m.geom_y });
        for (auto& q : m.polygon_pts) verts.push_back({ m.geom_x + q.x, m.geom_y + q.y });
        shape_pts(verts, 0, rep);
    }

    void path(Reader& s) {
        uint8_t inf = s.u8();
        read_layer(s, inf);
        if (inf & 0x40) m.half_width = (int64_t)s.uint_();
        if (inf & 0x80) {
            uint64_t scheme = s.uint_();
            auto ext = [&](unsigned code, int64_t& v) {
                if (code == 1) v = 0; else if (code == 2) v = m.half_width; else if (code == 3) v = s.sint();
            };
            ext((unsigned)((scheme >> 2) & 3), m.ext_start);
            ext((unsigned)(scheme & 3), m.ext_end);
        }
        if (inf & 0x20) oasis_codec::read_point_list(s, false, m.path_pts);
        read_xy(s, inf, m.geom_x, m.geom_y);
        bool rep = read_rep(s, inf, 0x04);
        verts.clear(); verts.push_back({ m.geom_x, m.geom_y });
        for (auto& q : m.path_pts) verts.push_back({ m.geom_x + q.x, m.geom_y + q.y });
        r.is_shape = true; r.layer = m.layer; r.datatype = m.datatype;
        if (!geometry) return;
        rects.clear();
        if (m.half_width > 0 && ldbgeom::path_to_rects(verts, m.half_width, m.ext_start, m.ext_end, rects)) {
            int64_t a = 0, b = 0, c = 0, d = 0;
            if (rep) rep_box(m.rep, a, b, c, d);
            for (auto& q : rects) { r.box.add(q[0] + a, q[1] + b); r.box.add(q[2] + c, q[3] + d); }
            if (!rects.empty()) return;
        }
        // Any-angle centreline: mitred outline (ldbgeom::path_bbox); zero width or one point:
        // the points grown by the half-width and extensions.
        int64_t bx1, by1, bx2, by2;
        if (ldbgeom::path_bbox(verts, m.half_width, m.ext_start, m.ext_end, bx1, by1, bx2, by2)) {
            int64_t a = 0, b = 0, c = 0, d = 0;
            if (rep) rep_box(m.rep, a, b, c, d);
            r.box.add(bx1 + a, by1 + b); r.box.add(bx2 + c, by2 + d);
            return;
        }
        int64_t grow = std::max<int64_t>(m.half_width, std::max<int64_t>(0, std::max(m.ext_start, m.ext_end)));
        shape_pts(verts, grow, rep);
    }

    void trapezoid(Reader& s, uint8_t id) {
        uint8_t inf = s.u8();
        read_layer(s, inf);
        if (inf & 0x40) m.geom_w = (int64_t)s.uint_();
        if (inf & 0x20) m.geom_h = (int64_t)s.uint_();
        if (id == 23 || id == 24) s.sint();
        if (id == 23 || id == 25) s.sint();
        read_xy(s, inf, m.geom_x, m.geom_y);
        bool rep = read_rep(s, inf, 0x04);
        // A trapezoid lies inside its w x h box.
        shape(m.geom_x, m.geom_y, m.geom_x + m.geom_w, m.geom_y + m.geom_h, rep, m.layer, m.datatype);
    }

    void ctrapezoid(Reader& s) {
        uint8_t inf = s.u8();
        read_layer(s, inf);
        if (inf & 0x80) m.ctrap_type = s.uint_();
        const uint64_t t = m.ctrap_type;
        if (t > 25) s.fail("invalid CTRAPEZOID type " + std::to_string(t));
        const oasis_codec::CtrapRule cr = oasis_codec::ctrap_rule(t);      // 29.6
        if (cr.no_h && (inf & 0x20)) s.fail("CTRAPEZOID type " + std::to_string(t) + " with H = 1");
        if (cr.no_w && (inf & 0x40)) s.fail("CTRAPEZOID type " + std::to_string(t) + " with W = 1");
        if (inf & 0x40) m.geom_w = (int64_t)s.uint_();
        if (inf & 0x20) m.geom_h = (int64_t)s.uint_();
        oasis_codec::ctrap_implied_box(t, m.geom_w, m.geom_h);           // 29.7
        if (!oasis_codec::ctrap_consistent(t, m.geom_w, m.geom_h))       // 29.8
            s.fail("CTRAPEZOID type " + std::to_string(t) + " with inconsistent width/height");
        read_xy(s, inf, m.geom_x, m.geom_y);
        bool rep = read_rep(s, inf, 0x04);
        shape(m.geom_x, m.geom_y, m.geom_x + m.geom_w, m.geom_y + m.geom_h, rep, m.layer, m.datatype);
    }

    void circle(Reader& s) {
        uint8_t inf = s.u8();
        read_layer(s, inf);
        if (inf & 0x20) m.radius = (int64_t)s.uint_();
        read_xy(s, inf, m.geom_x, m.geom_y);
        bool rep = read_rep(s, inf, 0x04);
        shape(m.geom_x - m.radius, m.geom_y - m.radius, m.geom_x + m.radius, m.geom_y + m.radius, rep, m.layer, m.datatype);
    }

    void property(Reader& s) {
        uint8_t inf = s.u8();
        if (inf & 0x04) {
            if (inf & 0x02) ref_num(s, REF_PROPNAME_NUM); else s.skip_str();
        }
        if (inf & 0x08) return;                                  // reuse the modal value list
        uint64_t count = inf >> 4; if (count == 15) count = s.uint_();
        for (uint64_t i = 0; i < count; ++i) {
            uint64_t t = s.uint_();
            if (t <= 7) (void)s.real_of(t);
            else if (t == 8) s.uint_();
            else if (t == 9) s.sint();
            else if (t <= 12) s.skip_str();
            else if (t <= 15) ref_num(s, REF_PROPSTR_NUM);
            else s.fail("invalid property value type " + std::to_string(t));
        }
    }

    void cblock(Reader& s) {
        uint64_t comp_type = s.uint_(), uncomp_len = s.uint_(), comp_len = s.uint_();
        if (comp_type != 0) s.fail("CBLOCK comp-type " + std::to_string(comp_type) + " (only 0 = DEFLATE is defined)");
        if (in_cblock) s.fail("CBLOCK nested in a CBLOCK");
        s.need(comp_len);
        std::vector<uint8_t> out;
        bool ok = db::decompress_cblock((uint8_t)comp_type, s.p + s.off, (size_t)comp_len, out, (size_t)uncomp_len, nvst);
        if (!ok || out.size() != uncomp_len) s.fail("CBLOCK decompression failed");
        s.off += (size_t)comp_len;
        info.cblocks++; info.cblock_comp_bytes += comp_len; info.cblock_uncomp_bytes += uncomp_len;
        std::string where = "CBLOCK ending at file offset " + std::to_string(s.off) + ", uncompressed";
        Reader inner(out.data(), out.size(), where.c_str());
        in_cblock = true;
        parse(inner, true);
        in_cblock = false;
    }

    static bool is_name_record(uint8_t id) { return (id >= 3 && id <= 12) || id == 30 || id == 31; }

    void parse(Reader& s, bool cblk) {
        while (!s.eof() && !saw_end) {
            rec_start = s.off;
            uint8_t id = s.u8();
            if (id == 34) { cblock(s); continue; }
            r.id = id; r.refs.clear(); r.name.clear(); r.explicit_ref = false; r.ref = 0;
            r.is_shape = false; r.is_placement = false; r.box = BBox(); r.layer = r.datatype = 0;
            if (is_name_record(id) || id == 2) in_cell = false;   // END and name records close a cell
            switch (id) {
            case 0: break;                                              // PAD
            case 1: {                                                   // START
                info.version = s.str(); info.unit = s.real();
                info.offset_flag = s.uint_();
                if (info.offset_flag == 0) for (int i = 0; i < 12; ++i) s.uint_();
            } break;
            case 2: saw_end = true; s.off = s.n; break;                 // END (rest is padding/validation)
            case 3: case 5: case 7: case 9: r.name = s.str(); break;
            case 4: case 6: case 8: case 10: r.name = s.str(); r.explicit_ref = true; r.ref = s.uint_(); break;
            case 11: case 12: s.skip_str(); oasis_codec::skip_interval(s); oasis_codec::skip_interval(s); break;
            case 13: in_cell = true; m.reset_for_cell(); r.explicit_ref = true; ref_num(s, REF_CELL_NUM, &r.ref); break;
            case 14: in_cell = true; m.reset_for_cell(); ref_str(s, REF_CELL_STR, &r.name); break;
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
            case 29: break;                                             // repeat last property
            case 30: s.uint_(); s.skip_str(); break;                    // XNAME (implicit)
            case 31: s.uint_(); s.skip_str(); s.uint_(); break;         // XNAME (explicit)
            case 32: s.uint_(); s.skip_str(); break;                    // XELEMENT
            case 33: {                                                  // XGEOMETRY
                uint8_t inf = s.u8(); s.uint_();
                read_layer(s, inf); s.skip_str();
                read_xy(s, inf, m.geom_x, m.geom_y); read_rep(s, inf, 0x04);
            } break;
            default: --s.off; s.fail("unknown OASIS record " + std::to_string((int)id));
            }
            r.data = s.p + rec_start; r.len = s.off - rec_start;
            if (id == 2) r.len = 1;
            r.in_cell = in_cell; r.in_cblock = cblk;
            info.records++;
            cb(r);
        }
    }
};

} // namespace

void scan(const uint8_t* data, size_t n, bool geometry,
          const std::function<void(const Record&)>& cb, FileInfo& info) {
    static const char magic[] = "%SEMI-OASIS\r\n";
    if (n < 13 || memcmp(data, magic, 13) != 0) throw ParseError{ "not an OASIS file (bad magic)" };
    Scanner sc(geometry, cb, info);
    Reader s(data + 13, n - 13, "file (after the 13-byte magic)");
    sc.parse(s, false);
    if (!sc.saw_end) throw ParseError{ "OASIS file has no END record" };
}

BBox transform_box(const BBox& b, const Placement& p) {
    BBox out;
    if (!b.valid()) return out;
    const int64_t xs[2] = { b.x1, b.x2 }, ys[2] = { b.y1, b.y2 };
    const double q = p.angle / 90.0;
    const bool ortho = q == std::floor(q);
    const int quarter = ortho ? (int)(((long long)q % 4 + 4) % 4) : 0;
    const double rad = p.angle * M_PI / 180.0, ca = std::cos(rad), sa = std::sin(rad);
    for (int i = 0; i < 2; ++i)
        for (int j = 0; j < 2; ++j) {
            int64_t x = xs[i], y = p.mirror ? -ys[j] : ys[j];
            if (ortho && p.mag == 1.0) {
                int64_t tx = x, ty = y;
                switch (quarter) {
                case 1: tx = -y; ty = x; break;
                case 2: tx = -x; ty = -y; break;
                case 3: tx = y; ty = -x; break;
                default: break;
                }
                out.add(tx + p.x, ty + p.y);
            } else {
                double tx = p.mag * (ca * x - sa * y), ty = p.mag * (sa * x + ca * y);
                out.add((int64_t)std::floor(tx) + p.x, (int64_t)std::floor(ty) + p.y);
                out.add((int64_t)std::ceil(tx) + p.x, (int64_t)std::ceil(ty) + p.y);
            }
        }
    return out;
}

} // namespace oasis_rec
