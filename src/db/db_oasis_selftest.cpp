// db_oasis_selftest OUTDIR
//
// Conformance checks of the OASIS (SEMI P39-1105) and GDSII code in src/db:
//
//  A. byte-level codec against the examples printed in the standard: Tables 1, 2, 4
//     (integers, reals), Table 5 (1-, 2-, 3- and g-deltas), Table 8 (the six point-list
//     encodings of the Figure 6 polygon), the CRC32 / CHECKSUM32 reference values of 14.4.3
//     and 14.5.2, and malformed-input rejection;
//  B. reader: two hand-assembled files using every record type 0-34 (implicit and explicit
//     reference numbers in turn), every repetition type 0-11, every point-list type,
//     all 26 CTRAPEZOID types, modal variables, CBLOCK, PAD, PROPERTY 28/29 with all value
//     types, XNAME/XELEMENT/XGEOMETRY; END with and without CRC32;
//  C. writer: a random hierarchical layout written by the streaming writer in several
//     configurations, plus the same layout (repetitions expanded) through the GDSII model
//     writer, for an independent comparison in KLayout (regressions/oasis_conformance.py);
//     CRC32/CHECKSUM32 of the written files are recomputed here.
//
// For every OASIS file it writes NAME.boxes (our reader, hierarchy flattened: layer dt x1 y1
// x2 y2) and NAME.rec (per-layer bounding boxes from the lossless scanner), and for strict
// files NAME.cells (S_CELL_OFFSET check and S_BOUNDING_BOX per cell from oasis_tables).
// Exit status: number of failed checks here (the KLayout comparison reports separately).
#include "oasis_codec.hpp"
#include "oasis_rec.hpp"
#include "oasis_writer.hpp"
#include "layoutdb_internal.hpp"
#include <zlib.h>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <map>
#include <random>
#include <set>
#include <string>
#include <vector>

bool write_gdsii_model(const LdbFile* f, const char* path, std::string& err);   // gdsii.cpp

using oasis_codec::Encoder;
using oasis_codec::Reader;
using oasis_writer::Pt;
using oasis_writer::Repetition;
using oasis_writer::PropValue;
using oasis_writer::Transform;

static int g_fail = 0, g_pass = 0;
static void check(bool ok, const std::string& what) {
    if (ok) ++g_pass; else { ++g_fail; printf("FAIL  %s\n", what.c_str()); }
}
static std::vector<uint8_t> B(std::initializer_list<int> v) { std::vector<uint8_t> r; for (int x : v) r.push_back((uint8_t)x); return r; }
static std::string hex(const std::vector<uint8_t>& v) { std::string s; char b[4]; for (auto c : v) { snprintf(b, 4, "%02x ", c); s += b; } return s; }

// ===========================================================================
// A. codec
// ===========================================================================
static void part_a() {
    // Table 1: unsigned integers
    const std::pair<uint64_t, std::vector<uint8_t>> t1[] = {
        { 0, B({ 0x00 }) }, { 127, B({ 0x7f }) }, { 128, B({ 0x80, 0x01 }) }, { 16383, B({ 0xff, 0x7f }) },
        { 16384, B({ 0x80, 0x80, 0x01 }) } };
    for (auto& t : t1) {
        Encoder e; e.uint_(t.first);
        check(e.b == t.second, "Table 1 encode " + std::to_string(t.first) + ": " + hex(e.b));
        Reader r(t.second.data(), t.second.size(), "t1");
        check(r.uint_() == t.first && r.eof(), "Table 1 decode " + std::to_string(t.first));
    }
    // Table 2: signed integers
    const std::pair<int64_t, std::vector<uint8_t>> t2[] = {
        { 0, B({ 0x00 }) }, { 1, B({ 0x02 }) }, { -1, B({ 0x03 }) }, { 63, B({ 0x7e }) }, { -64, B({ 0x81, 0x01 }) },
        { 8191, B({ 0xfe, 0x7f }) }, { -8192, B({ 0x81, 0x80, 0x01 }) } };
    for (auto& t : t2) {
        Encoder e; e.sint(t.first);
        check(e.b == t.second, "Table 2 encode " + std::to_string(t.first) + ": " + hex(e.b));
        Reader r(t.second.data(), t.second.size(), "t2");
        check(r.sint() == t.first && r.eof(), "Table 2 decode " + std::to_string(t.first));
    }
    // Table 4: reals, rational and IEEE-4 forms
    struct R4 { double v; std::vector<uint8_t> rat, ieee; };
    const R4 t4[] = {
        { 0.0, B({ 0x00, 0x00 }), B({ 0x06, 0x00, 0x00, 0x00, 0x00 }) },
        { 1.0, B({ 0x00, 0x01 }), B({ 0x06, 0x00, 0x00, 0x80, 0x3f }) },
        { -0.5, B({ 0x03, 0x02 }), B({ 0x06, 0x00, 0x00, 0x00, 0xbf }) },
        { 0.3125, B({ 0x04, 0x05, 0x10 }), B({ 0x06, 0x00, 0x00, 0xa0, 0x3e }) },
        { 1.0 / 3, B({ 0x02, 0x03 }), B({ 0x06, 0xab, 0xaa, 0xaa, 0x3e }) },
        { -2.0 / 13, B({ 0x05, 0x02, 0x0d }), B({ 0x06, 0xd9, 0x89, 0x1d, 0xbe }) } };
    for (auto& t : t4) {
        Reader a(t.rat.data(), t.rat.size(), "t4r"), b(t.ieee.data(), t.ieee.size(), "t4i");
        const double ra = a.real(), rb = b.real();
        check(std::fabs(ra - t.v) < 1e-15 && a.eof(), "Table 4 rational " + std::to_string(t.v));
        check(std::fabs(rb - t.v) < 1e-7 && b.eof(), "Table 4 IEEE-4 " + std::to_string(t.v));
    }
    { Encoder e; e.real(1.0); check(e.b == B({ 0x00, 0x01 }), "real encode 1.0"); }
    { Encoder e; e.real(-0.5); check(e.b == B({ 0x03, 0x02 }), "real encode -0.5"); }
    { Encoder e; e.real(1.0 / 3); check(e.b == B({ 0x02, 0x03 }), "real encode 1/3: " + hex(e.b)); }
    { Encoder e; e.real(2.5); Reader r(e.b.data(), e.b.size(), "r"); check(r.real() == 2.5 && e.b[0] == 7, "real encode 2.5 (IEEE-8)"); }
    // Table 5: deltas
    {
        auto v = B({ 0xf9, 0x23 }); Reader r(v.data(), v.size(), "d1"); check(r.sint() == -2300, "Table 5 1-delta -2300");
        Encoder e; e.delta1(-2300); check(e.b == v, "Table 5 1-delta encode -2300");
    }
    {
        auto v = B({ 0xf8, 0x23 }); Reader r(v.data(), v.size(), "d1"); check(r.sint() == 2300, "Table 5 1-delta +2300");
    }
    struct D { std::vector<uint8_t> b; int kind; int64_t dx, dy; };     // kind 2, 3, 'g'
    const D t5[] = {
        { B({ 0x98, 0x2a }), 2, 1350, 0 }, { B({ 0x9b, 0x2a }), 2, 0, -1350 },
        { B({ 0xcd, 0x01 }), 3, -25, 25 }, { B({ 0xd7, 0x07 }), 3, 122, -122 },
        { B({ 0xe9, 0x03, 0x7a }), 'g', 122, 61 }, { B({ 0xec, 0x05 }), 'g', -46, -46 },
        { B({ 0xbb, 0x01, 0xb7, 0x0f }), 'g', -46, -987 } };
    for (auto& t : t5) {
        Reader r(t.b.data(), t.b.size(), "d");
        int64_t dx, dy;
        if (t.kind == 2) r.delta2(dx, dy); else if (t.kind == 3) r.delta3(dx, dy); else r.gdelta(dx, dy);
        char nm[96]; snprintf(nm, sizeof nm, "Table 5 %c-delta (%lld,%lld)", t.kind == 'g' ? 'g' : '0' + t.kind, (long long)t.dx, (long long)t.dy);
        check(dx == t.dx && dy == t.dy && r.eof(), std::string(nm) + " decode");
        Encoder e;
        if (t.kind == 2) e.delta2(t.dx, t.dy); else if (t.kind == 3) e.delta3(t.dx, t.dy); else e.gdelta(t.dx, t.dy);
        check(e.b == t.b, std::string(nm) + " encode: " + hex(e.b));
    }
    // Table 8: the Figure 6 polygon in all six point-list types
    const std::vector<uint8_t> t8[] = {
        B({ 0x00, 0x04, 0x0c, 0x08, 0x11, 0x05 }),
        B({ 0x01, 0x04, 0x11, 0x04, 0x04, 0x04 }),
        B({ 0x02, 0x05, 0x20, 0x19, 0x12, 0x0b, 0x12 }),
        B({ 0x03, 0x04, 0x15, 0x21, 0x30, 0x13 }),
        B({ 0x04, 0x02, 0x44, 0x09, 0x0d }),
        B({ 0x05, 0x09, 0x01, 0x03, 0x29, 0x00, 0x01, 0x04, 0x01, 0x03, 0x01, 0x03, 0x2b, 0x04, 0x2b, 0x00, 0x01, 0x03, 0x01, 0x03 }) };
    // Figure 6 shows six different polygons; expected vertices decoded by hand from the bit
    // patterns (start point (x,y) first) and checked against the drawing: type 0 notch at the
    // lower left, type 1 notch at the bottom right, type 2 notch at the top left, type 3 with a
    // 45-degree implicit closing edge, type 4 a downward triangle, type 5 a thin band.
    const std::vector<std::vector<Pt>> expect = {
        { { 0, 0 }, { 6, 0 }, { 6, 4 }, { -2, 4 }, { -2, 2 }, { 0, 2 } },
        { { 0, 0 }, { 0, -8 }, { 2, -8 }, { 2, -6 }, { 4, -6 }, { 4, 0 } },
        { { 0, 0 }, { 8, 0 }, { 8, 6 }, { 4, 6 }, { 4, 4 }, { 0, 4 } },
        { { 0, 0 }, { -2, 2 }, { -2, 6 }, { 4, 6 }, { 4, 4 } },
        { { 0, 0 }, { -4, 0 }, { -2, -6 } },
        { { 0, 0 }, { 0, -1 }, { 10, -2 }, { 20, -1 }, { 30, -1 }, { 40, -2 }, { 40, -1 }, { 30, 0 }, { 20, 0 }, { 10, -1 } } };
    for (int k = 0; k < 6; ++k) {
        Reader r(t8[k].data(), t8[k].size(), "t8");
        std::vector<Pt> pl;
        bool ok = true;
        try { oasis_codec::read_point_list(r, true, pl); } catch (const oasis_codec::ParseError& e) { ok = false; printf("      %s\n", e.msg.c_str()); }
        check(ok && r.eof(), "Table 8 type " + std::to_string(k) + " decodes completely");
        // read_point_list returns the vertices after the start point, with the implicit
        // vertices of types 0/1 added; the start point is implicit (0,0)
        std::vector<Pt> poly = { { 0, 0 } };
        for (auto& q : pl) poly.push_back(q);
        while (poly.size() > 1 && poly.back().x == 0 && poly.back().y == 0) poly.pop_back();
        bool same = poly.size() == expect[k].size();
        for (size_t i = 0; same && i < poly.size(); ++i) same = poly[i].x == expect[k][i].x && poly[i].y == expect[k][i].y;
        std::string got; for (auto& q : poly) got += "(" + std::to_string(q.x) + "," + std::to_string(q.y) + ")";
        check(same, "Table 8 type " + std::to_string(k) + " vertices = Figure 6: " + got);
        // and our encoder chooses a form at least as short as the printed one
        std::vector<uint8_t> enc = oasis_writer::encode_point_list_for_test(poly);
        check(!enc.empty() && enc.size() <= t8[k].size(), "Table 8 type " + std::to_string(k) + " re-encoded in " + std::to_string(enc.size()) + " <= " + std::to_string(t8[k].size()) + " bytes");
        Reader r2(enc.data(), enc.size(), "re");
        std::vector<Pt> pl2; oasis_codec::read_point_list(r2, true, pl2);
        std::vector<Pt> poly2 = { { 0, 0 } }; for (auto& q : pl2) poly2.push_back(q);
        while (poly2.size() > 1 && poly2.back().x == 0 && poly2.back().y == 0) poly2.pop_back();
        std::set<std::pair<int64_t, int64_t>> s1, s2;
        for (auto& q : poly) s1.insert({ q.x, q.y });
        for (auto& q : poly2) s2.insert({ q.x, q.y });
        check(s1 == s2, "Table 8 type " + std::to_string(k) + " re-encoding round-trips");
    }
    // validation reference values
    const char* hw = "Hello, World!";
    check((uint32_t)crc32(crc32(0L, Z_NULL, 0), (const Bytef*)hw, 13) == 0xEC4AC3D0u, "14.4.3 CRC32(Hello, World!) = EC4AC3D0");
    uint32_t sum = 0; for (int i = 0; i < 13; ++i) sum += (uint8_t)hw[i];
    check(sum == 0x469, "14.5.2 CHECKSUM32(Hello, World!) = 00000469");
    // rejection of malformed values
    auto rejects = [](std::vector<uint8_t> v, int what) {
        Reader r(v.data(), v.size(), "bad");
        try {
            if (what == 0) r.uint_(); else if (what == 1) r.real(); else { Repetition dummy; oasis_codec::Repetition rr; oasis_codec::read_repetition(r, rr); }
        } catch (const oasis_codec::ParseError&) { return true; }
        return false;
    };
    check(rejects(B({ 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f }), 0), "reject 70-bit unsigned integer");
    check(!rejects(B({ 0x81, 0x80, 0x80, 0x00 }), 0), "accept redundant zero groups");
    {   // a long run of zero continuation groups: accepted, and the shift count never overflows
        std::vector<uint8_t> v(100000, 0x80); v[0] = 0x85; v.push_back(0x00);
        Reader r(v.data(), v.size(), "long"); uint64_t x = 99;
        try { x = r.uint_(); } catch (const oasis_codec::ParseError&) {}
        check(x == 5, "accept 100000 redundant zero groups");
        v.back() = 0x01;                                          // a set bit far beyond 64 bits
        check(rejects(v, 0), "reject a set bit after 100000 groups");
    }
    check(rejects(B({ 0x02, 0x00 }), 1), "reject real with zero denominator (7.3.3)");
    check(rejects(B({ 0x08 }), 1), "reject real type 8");
    check(rejects(B({ 0x0c }), 2), "reject repetition type 12");
}

// ===========================================================================
// B. hand-assembled reader files
// ===========================================================================
struct Asm : Encoder {
    void rec(int id) { u8((uint8_t)id); }
    void end_record(bool crc, bool offsets) {
        // END: [table-offsets] padding validation-scheme [signature], 256 bytes total
        Encoder e; e.u8(2);
        if (offsets) for (int i = 0; i < 12; ++i) e.uint_(0);
        const size_t fixed = e.b.size() + 1 + (crc ? 4 : 0);
        size_t pad = 256 - fixed - 2;                            // 2-byte length (>= 128 here)
        e.u8((uint8_t)(0x80 | (pad & 0x7f))); e.u8((uint8_t)(pad >> 7));
        e.b.insert(e.b.end(), pad, 0);
        e.uint_(crc ? 1 : 0);
        b.insert(b.end(), e.b.begin(), e.b.end());
        if (crc) {
            uint32_t c = (uint32_t)crc32(crc32(0L, Z_NULL, 0), b.data(), (uInt)b.size());
            for (int i = 0; i < 4; ++i) u8((uint8_t)(c >> (8 * i)));
        }
    }
};

static void cblock(Asm& out, const Encoder& inner) {
    std::vector<uint8_t> z(compressBound(inner.b.size()) + 64);
    z_stream zs{};
    deflateInit2(&zs, 9, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY);
    zs.next_in = const_cast<uint8_t*>(inner.b.data()); zs.avail_in = (uInt)inner.b.size();
    zs.next_out = z.data(); zs.avail_out = (uInt)z.size();
    deflate(&zs, Z_FINISH);
    const size_t zn = zs.total_out; deflateEnd(&zs);
    out.rec(34); out.uint_(0); out.uint_(inner.b.size()); out.uint_(zn); out.bytes(z.data(), zn);
}

// explicit: use record types 4/6/8/10/31 and CELL by number; else 3/5/7/9/30 and CELL by name.
static std::vector<uint8_t> reader_file(bool explicit_refs, bool xrecords) {
    Asm a;
    a.bytes("%SEMI-OASIS\r\n", 13);
    a.rec(1); a.str("1.0"); a.uint_(0); a.uint_(1000);           // unit 1000 as real type 0
    if (explicit_refs) a.uint_(1); else { a.uint_(0); for (int i = 0; i < 12; ++i) a.uint_(0); }
    a.rec(0);                                                    // PAD
    // file property with all value types 0-12
    a.rec(28); a.u8(0xD4); a.str("FILEPROP");                    // 13 values, C=1 N=0
    a.uint_(0); a.uint_(5);                                      // 5
    a.uint_(1); a.uint_(5);                                      // -5
    a.uint_(2); a.uint_(4);                                      // 1/4
    a.uint_(3); a.uint_(4);                                      // -1/4
    a.uint_(4); a.uint_(3); a.uint_(2);                          // 3/2
    a.uint_(5); a.uint_(3); a.uint_(2);                          // -3/2
    { a.uint_(6); float f = 2.5f; a.bytes(&f, 4); }
    { a.uint_(7); double d = -0.125; a.bytes(&d, 8); }
    a.uint_(8); a.uint_(77);
    a.uint_(9); a.sint(-77);
    a.uint_(10); a.str("a string");
    a.uint_(11); a.str(std::string("b\0str", 5));
    a.uint_(12); a.str("nstring");
    a.rec(29);                                                   // repeat it
    // names
    if (explicit_refs) {
        a.rec(4); a.str("TOP"); a.uint_(7);
        a.rec(4); a.str("CHILD"); a.uint_(3);
        a.rec(6); a.str("hello world"); a.uint_(2);
        a.rec(8); a.str("PN"); a.uint_(5);
        a.rec(10); a.str("ps value"); a.uint_(9);
        if (xrecords) { a.rec(31); a.uint_(1); a.str("xn"); a.uint_(0); }
    } else {
        a.rec(3); a.str("CHILD");                                // 0
        a.rec(3); a.str("TOP");                                  // 1
        a.rec(5); a.str("hello world");                          // 0
        a.rec(7); a.str("PN");                                   // 0
        a.rec(9); a.str("ps value");                             // 0
        if (xrecords) { a.rec(30); a.uint_(1); a.str("xn"); }
    }
    const uint64_t child = explicit_refs ? 3 : 0, txt = explicit_refs ? 2 : 0, pn = explicit_refs ? 5 : 0, ps = explicit_refs ? 9 : 0;
    a.rec(11); a.str("metal1"); a.uint_(3); a.uint_(1); a.uint_(3); a.uint_(0);
    a.rec(11); a.str("many"); a.uint_(4); a.uint_(10); a.uint_(20); a.uint_(0);
    a.rec(12); a.str("txt"); a.uint_(1); a.uint_(10); a.uint_(2); a.uint_(1);
    // ---- CHILD ----
    if (explicit_refs) { a.rec(13); a.uint_(child); } else { a.rec(14); a.str("CHILD"); }
    a.rec(20); a.u8(0x7B); a.uint_(1); a.uint_(0); a.uint_(100); a.uint_(50); a.sint(0); a.sint(0);     // L1 (0,0) 100x50
    a.rec(20); a.u8(0xD8); a.uint_(30); a.sint(200); a.sint(0);                                       // square 30 at (200,0)
    a.rec(16);                                                                                        // XYRELATIVE
    a.rec(20); a.u8(0x08); a.sint(100);                                                               // modal 30x30, dy 100 -> (200,100)
    a.rec(20); a.u8(0x7C); a.uint_(5); a.uint_(5); a.sint(800); a.sint(-100);                         // (1000,0) 5x5, rep 1
    a.uint_(1); a.uint_(0); a.uint_(0); a.uint_(10); a.uint_(20);
    a.rec(0);
    a.rec(15);                                                                                        // XYABSOLUTE
    // polygons: Table 8 point lists, scaled by placement at different x
    const std::vector<uint8_t> t8[] = {
        B({ 0x00, 0x04, 0x0c, 0x08, 0x11, 0x05 }), B({ 0x01, 0x04, 0x11, 0x04, 0x04, 0x04 }),
        B({ 0x02, 0x05, 0x20, 0x19, 0x12, 0x0b, 0x12 }), B({ 0x03, 0x04, 0x15, 0x21, 0x30, 0x13 }),
        B({ 0x04, 0x02, 0x44, 0x09, 0x0d }),
        B({ 0x05, 0x09, 0x01, 0x03, 0x29, 0x00, 0x01, 0x04, 0x01, 0x03, 0x01, 0x03, 0x2b, 0x04, 0x2b, 0x00, 0x01, 0x03, 0x01, 0x03 }) };
    for (int k = 0; k < 6; ++k) {
        a.rec(21); a.u8(k == 0 ? 0x3B : 0x38); if (k == 0) { a.uint_(2); a.uint_(0); }
        a.bytes(t8[k].data(), t8[k].size()); a.sint(2000 + 100 * k); a.sint(0);
    }
    a.rec(21); a.u8(0x10); a.sint(2700);                                                              // modal point list, X only
    // paths
    a.rec(22); a.u8(0xFB); a.uint_(3); a.uint_(0); a.uint_(5); a.uint_((1 << 2) | 2);                 // flush start, half-width end
    a.uint_(0); a.uint_(2); a.sint(100); a.sint(50); a.sint(3000); a.sint(0);
    a.rec(22); a.u8(0x1C); a.sint(3000); a.sint(200); a.uint_(2); a.uint_(1); a.uint_(200);           // all modal, rep 2 (3 x 200)
    a.rec(22); a.u8(0xF8); a.uint_(4); a.uint_(0xF); a.sint(7); a.sint(-3);                           // explicit extensions
    a.uint_(2); a.uint_(1); a.uint_((60 << 2) | 1); a.sint(3500); a.sint(0);
    a.rec(22); a.u8(0xF8); a.uint_(6); a.uint_(0xA); a.uint_(3); a.uint_(2); a.uint_((40 << 3) | 4); a.uint_((40 << 3) | 0);
    a.sint(3700); a.sint(0);                                                                          // octangular, half-width ends
    // trapezoids on L4
    a.rec(23); a.u8(0x7B); a.uint_(4); a.uint_(0); a.uint_(100); a.uint_(40); a.sint(-20); a.sint(10); a.sint(4000); a.sint(0);
    a.rec(24); a.u8(0xF8); a.uint_(40); a.uint_(100); a.sint(20); a.sint(4200); a.sint(0);
    a.rec(25); a.u8(0x78); a.uint_(100); a.uint_(40); a.sint(-15); a.sint(4400); a.sint(0);
    // CTRAPEZOIDs 0..25 on layers 100+t; RECTANGLE with modal w/h after types 22 and 20 (29.7)
    for (int t = 0; t <= 25; ++t) {
        int64_t w = 60, h = 20;
        bool W = true, H = true;
        if ((t >= 8 && t <= 15)) { w = 20; h = 60; }
        if ((t >= 16 && t <= 19) || t == 22 || t == 23 || t == 25) { w = 30; H = false; }
        if (t == 20 || t == 21) { h = 25; W = false; }
        if (t == 24) { w = 50; h = 20; }
        a.rec(26); a.u8((uint8_t)(0x80 | (W ? 0x40 : 0) | (H ? 0x20 : 0) | 0x18 | 0x03));
        a.uint_(100 + t); a.uint_(0); a.uint_(t);
        if (W) a.uint_(w);
        if (H) a.uint_(h);
        a.sint(5000 + 100 * t); a.sint(0);
        if (t == 22) { a.rec(20); a.u8(0x1B); a.uint_(99); a.uint_(0); a.sint(5000 + 100 * t); a.sint(500); }   // 30 x 60
        if (t == 20) { a.rec(20); a.u8(0x1B); a.uint_(98); a.uint_(0); a.sint(5000 + 100 * t); a.sint(500); }   // 50 x 25
    }
    // circles on L5
    a.rec(27); a.u8(0x3B); a.uint_(5); a.uint_(0); a.uint_(50); a.sint(8000); a.sint(0);
    a.rec(27); a.u8(0x1C); a.sint(8000); a.sint(300); a.uint_(3); a.uint_(0); a.uint_(200);
    // texts
    a.rec(19); a.u8(0x7B); a.uint_(txt); a.uint_(10); a.uint_(0); a.sint(0); a.sint(0);
    a.rec(19); a.u8(0x5B); a.str("inline text"); a.uint_(10); a.uint_(1); a.sint(100); a.sint(100);
    a.rec(28); a.u8(0x26); a.uint_(pn); a.uint_(8); a.uint_(7); a.uint_(13); a.uint_(ps);              // 2 values: 7, PROPSTRING
    a.rec(29);
    if (xrecords) {
        a.rec(32); a.uint_(5); a.str("blob");                                                         // XELEMENT
        a.rec(33); a.u8(0x1B); a.uint_(2); a.uint_(7); a.uint_(0); a.str("xg"); a.sint(9000); a.sint(0);   // XGEOMETRY
    }
    // CBLOCK with repetition types 4-11 and 0 on L6
    {
        Encoder in;
        in.u8(20); in.u8(0x7B); in.uint_(6); in.uint_(0); in.uint_(10); in.uint_(10); in.sint(10000); in.sint(0);
        in.u8(20); in.u8(0x1C); in.sint(10000); in.sint(100); in.uint_(4); in.uint_(1); in.uint_(15); in.uint_(25);
        in.u8(20); in.u8(0x1C); in.sint(10000); in.sint(200); in.uint_(5); in.uint_(0); in.uint_(5); in.uint_(3);
        in.u8(20); in.u8(0x1C); in.sint(10200); in.sint(0); in.uint_(6); in.uint_(1); in.uint_(20); in.uint_(30);
        in.u8(20); in.u8(0x1C); in.sint(10300); in.sint(0); in.uint_(7); in.uint_(0); in.uint_(4); in.uint_(10);
        in.u8(20); in.u8(0x1C); in.sint(10400); in.sint(0); in.uint_(8); in.uint_(0); in.uint_(0); in.gdelta(20, 5); in.gdelta(-5, 20);
        in.u8(20); in.u8(0x1C); in.sint(10500); in.sint(0); in.uint_(9); in.uint_(1); in.gdelta(15, 15);
        in.u8(20); in.u8(0x1C); in.sint(10600); in.sint(0); in.uint_(10); in.uint_(1); in.gdelta(12, 3); in.gdelta(-4, 20);
        in.u8(20); in.u8(0x1C); in.sint(10700); in.sint(0); in.uint_(11); in.uint_(0); in.uint_(3); in.gdelta(5, 7);
        in.u8(20); in.u8(0x1C); in.sint(10800); in.sint(0); in.uint_(0);                                // reuse type 11
        in.u8(0);
        cblock(a, in);
    }
    // ---- TOP ----
    if (explicit_refs) { a.rec(13); a.uint_(7); } else { a.rec(14); a.str("TOP"); }
    a.rec(17); a.u8(0xF0); a.uint_(child); a.sint(0); a.sint(0);
    a.rec(17); a.u8(0x33); a.sint(0); a.sint(20000);                                                  // modal cell, 90 deg, flip
    a.rec(18); a.u8(0xBE); a.str("CHILD"); a.uint_(0); a.uint_(2); a.uint_(0); a.uint_(180);          // by name, mag 2, 180 deg
    a.sint(50000); a.sint(0); a.uint_(2); a.uint_(0); a.uint_(30000);
    a.rec(28); a.u8(0x16); a.uint_(pn); a.uint_(12); a.str("on_placement");
    a.rec(29);
    a.rec(18); a.u8(0x32); a.uint_(7); { double d = 270.0; a.bytes(&d, 8); } a.sint(0); a.sint(-20000);   // 270 as IEEE-8
    a.rec(18); a.u8(0x3E); a.uint_(4); a.uint_(1); a.uint_(1); a.uint_(0); a.uint_(90); a.sint(-30000); a.sint(0);   // mag 1/1, 90
    a.uint_(8); a.uint_(0); a.uint_(1); a.gdelta(0, 40000); a.gdelta(-40000, 0);                     // 2 x 3 lattice
    a.end_record(explicit_refs, explicit_refs);
    return a.b;
}

// ===========================================================================
// C. writer: random layout, OASIS (streaming writer) and GDSII twin (model writer)
// ===========================================================================
struct SShape { int kind; uint64_t l, d; std::vector<Pt> pts; int64_t hw = 0, es = 0, ee = 0; bool has_rep = false; Repetition rep; };
struct SInst { std::string child; Transform t; bool has_rep = false; Repetition rep; };
struct SText { uint64_t l, t; int64_t x, y; std::string s; };
struct SCell { std::string name; std::vector<SShape> shapes; std::vector<SInst> insts; std::vector<SText> texts; std::vector<std::pair<std::string, std::vector<PropValue>>> props; };

static std::vector<Pt> rep_offsets(const Repetition& r) {
    std::vector<Pt> v;
    if (!r.regular) return r.offs;
    for (int64_t j = 0; j < r.rows; ++j) for (int64_t i = 0; i < r.cols; ++i) v.push_back({ i * r.dx + j * r.rdx, i * r.dy + j * r.rdy });
    return v;
}

static std::vector<SCell> make_scene(unsigned seed) {
    std::mt19937_64 rng(seed);
    auto U = [&](int64_t a, int64_t b) { return std::uniform_int_distribution<int64_t>(a, b)(rng); };
    auto rand_rep = [&](int which) {
        Repetition r;
        switch (which % 9) {
        case 0: r = Repetition::lattice(U(2, 6), U(20, 90), 0); break;                         // type 2
        case 1: r = Repetition::lattice(U(2, 5), 0, -U(20, 90)); break;                        // type 3 (negative: rebased)
        case 2: r = Repetition::lattice(U(2, 4), U(30, 60), 0, U(2, 4), 0, U(30, 60)); break;   // type 1
        case 3: r = Repetition::lattice(U(2, 4), 0, U(30, 60), U(2, 4), -U(30, 60), 0); break;  // transposed, negative
        case 4: r = Repetition::lattice(U(2, 4), U(20, 40), U(5, 15), U(2, 3), -U(5, 15), U(30, 50)); break;   // type 8
        case 5: r = Repetition::lattice(U(2, 6), U(10, 40), U(10, 40)); break;                // type 9
        case 6: { r.regular = false; int64_t x = 0; int n = (int)U(3, 6); for (int i = 0; i < n; ++i) { r.offs.push_back({ x, 0 }); x += 6 * U(5, 20); } } break;   // 4/5
        case 7: { r.regular = false; int64_t y = 0; int n = (int)U(3, 6); for (int i = 0; i < n; ++i) { r.offs.push_back({ 0, y }); y -= U(25, 70); } } break;     // 6
        case 8: { r.regular = false; int n = (int)U(3, 6); for (int i = 0; i < n; ++i) r.offs.push_back({ 4 * U(-40, 40), 4 * U(-40, 40) }); } break;            // 10/11
        }
        return r;
    };
    std::vector<SCell> cells;
    for (int k = 0; k < 4; ++k) {
        SCell c; c.name = "LEAF" + std::to_string(k);
        const int64_t ox = 0, oy = 0;
        for (int i = 0; i < 40; ++i) {                                                         // rectangles
            SShape s; s.kind = 0; s.l = (uint64_t)U(1, 3); s.d = (uint64_t)U(0, 1);
            int64_t x = ox + U(-500, 500), y = oy + U(-500, 500), w = U(1, 6) * 10, h = (i % 5 == 0) ? w : U(1, 6) * 10;
            s.pts = { { x, y }, { x + w, y }, { x + w, y + h }, { x, y + h } };
            if (i % 3 == 0) { s.has_rep = true; s.rep = rand_rep(i / 3 + k); }
            c.shapes.push_back(s);
        }
        // the same rectangle repeated identically: modal reuse of the repetition (type 0)
        for (int i = 0; i < 3; ++i) {
            SShape s; s.kind = 0; s.l = 1; s.d = 0;
            const int64_t x = 2000 + 300 * i, y = 2000;
            s.pts = { { x, y }, { x + 20, y }, { x + 20, y + 20 }, { x, y + 20 } };
            s.has_rep = true; s.rep = Repetition::lattice(3, 40, 0, 2, 0, 40);
            c.shapes.push_back(s);
        }
        {   // Manhattan polygons (alternating -> point list 0/1), with spike -> type 2
            SShape s; s.kind = 1; s.l = 4; s.d = 0;
            s.pts = { { 0, 0 }, { 300, 0 }, { 300, 100 }, { 200, 100 }, { 200, 250 }, { 0, 250 } };
            c.shapes.push_back(s);
            s.pts = { { 1000, 0 }, { 1000, 200 }, { 1100, 200 }, { 1100, 100 }, { 1300, 100 }, { 1300, 0 } };   // vertical first
            s.has_rep = true; s.rep = rand_rep(k + 2); c.shapes.push_back(s); s.has_rep = false;
            s.pts = { { 0, 600 }, { 200, 600 }, { 200, 700 }, { 200, 650 }, { 200, 800 }, { 0, 800 } };     // collinear backtrack
            c.shapes.push_back(s);
            // octangular (type 3)
            s.pts = { { 0, 1000 }, { 200, 1000 }, { 300, 1100 }, { 300, 1200 }, { 100, 1200 }, { 0, 1100 } };
            c.shapes.push_back(s);
            // all-angle (type 4) and a fine curve (type 5 usually shorter)
            s.l = 5;
            s.pts = { { 500, 1000 }, { 731, 1043 }, { 650, 1290 }, { 512, 1201 } };
            c.shapes.push_back(s);
            s.pts.clear();
            for (int i = 0; i < 48; ++i) { const double a = 2 * M_PI * i / 48; s.pts.push_back({ 1500 + (int64_t)llround(300 * cos(a)), 1500 + (int64_t)llround(200 * sin(a)) }); }
            c.shapes.push_back(s);
            // same shape again elsewhere: modal point list
            for (auto& q : s.pts) q.x += 900;
            c.shapes.push_back(s);
            // closing vertex repeated, and a rectangle given as polygon
            s.l = 4; s.pts = { { 3000, 0 }, { 3100, 0 }, { 3100, 80 }, { 3000, 80 }, { 3000, 0 } };
            c.shapes.push_back(s);
        }
        {   // paths
            SShape s; s.kind = 2; s.l = 6; s.d = 0;
            s.pts = { { 0, -1000 }, { 400, -1000 }, { 400, -800 }, { 700, -800 } }; s.hw = 10; s.es = 0; s.ee = 10;
            c.shapes.push_back(s);
            s.pts = { { 0, -1200 }, { 0, -1400 }, { 300, -1400 } }; s.hw = 8; s.es = 13; s.ee = -3;
            s.has_rep = true; s.rep = rand_rep(k); c.shapes.push_back(s); s.has_rep = false;
            s.pts = { { 1000, -1000 }, { 1200, -1000 } }; s.hw = 25; s.es = 25; s.ee = 25;       // half-width ends
            c.shapes.push_back(s);
            s.l = 8;                                                                            // octangular, own layer
            s.pts = { { 1500, -1000 }, { 1700, -800 }, { 1900, -800 } }; s.hw = 10; s.es = 0; s.ee = 0;
            c.shapes.push_back(s);
        }
        c.texts.push_back({ 10, 0, 5, 5, "label_" + std::to_string(k) });
        c.texts.push_back({ 10, 1, 50, 5, "label with space" });
        c.props.push_back({ "LEAFPROP", { PropValue::uint_((uint64_t)k), PropValue::sint(-k), PropValue::real(0.5 + k), PropValue::str("v a l"), PropValue::str(std::string("bin\x01", 4)) } });
        cells.push_back(c);
    }
    // MID cells: orthogonal placements of leaves, with repetitions
    for (int m = 0; m < 2; ++m) {
        SCell c; c.name = "MID" + std::to_string(m);
        for (int i = 0; i < 6; ++i) {
            SInst in; in.child = "LEAF" + std::to_string((i + m) % 4);
            in.t.x = U(-5000, 5000); in.t.y = U(-5000, 5000); in.t.angle = 90.0 * (double)U(0, 3); in.t.mirror = U(0, 1) == 1;
            if (i % 2 == 0) { in.has_rep = true; in.rep = rand_rep(i + m); for (auto& o : in.rep.offs) { o.x *= 30; o.y *= 30; } in.rep.dx *= 30; in.rep.dy *= 30; in.rep.rdx *= 30; in.rep.rdy *= 30; }
            c.insts.push_back(in);
        }
        SShape s; s.kind = 0; s.l = 7; s.d = 0; s.pts = { { -6000, -6000 }, { 6000, -6000 }, { 6000, -5990 }, { -6000, -5990 } };
        c.shapes.push_back(s);
        cells.push_back(c);
    }
    {   // general transforms (mag, arbitrary angle)
        SCell c; c.name = "ROT";
        SInst in; in.child = "LEAF0"; in.t.x = 100; in.t.y = 200; in.t.angle = 30.0; in.t.mag = 1.5; c.insts.push_back(in);
        in.t.angle = 0; in.t.mag = 2.0; in.t.mirror = true; in.t.x = 9000; c.insts.push_back(in);
        cells.push_back(c);
    }
    {   // top
        SCell c; c.name = "TOP";
        SInst in; in.child = "MID0"; c.insts.push_back(in);
        in.child = "MID1"; in.t.x = 20000; in.t.angle = 180; in.has_rep = true; in.rep = Repetition::lattice(2, 0, 20000); c.insts.push_back(in);
        in = SInst(); in.child = "ROT"; in.t.y = -20000; c.insts.push_back(in);
        c.props.push_back({ "TOPPROP", { PropValue::str("top") } });
        c.props.push_back({ "TOPPROP", { PropValue::str("top") } });                          // record 29
        std::vector<PropValue> many; for (int i = 0; i < 20; ++i) many.push_back(PropValue::uint_((uint64_t)i));
        c.props.push_back({ "MANY", many });                                                   // >= 15 values
        cells.push_back(c);
    }
    return cells;
}

static bool write_scene_oasis(const std::vector<SCell>& sc, const std::string& path, const oasis_writer::Options& o, oasis_writer::Stats* st) {
    oasis_writer::Writer w;
    if (!w.open(path, o)) { printf("      %s\n", w.error().c_str()); return false; }
    w.layer_name("ACTIVE", 1, 0); w.layer_name("POLY", 2, 0); w.layer_name("TXT", 10, 0, true);
    w.file_property("WRITER", { PropValue::str("db_oasis_selftest") });
    for (auto& c : sc) {
        w.begin_cell(c.name);
        for (auto& p : c.props) w.property(p.first, p.second);
        for (auto& in : c.insts) w.placement(in.child, in.t, in.has_rep ? &in.rep : nullptr);
        for (auto& s : c.shapes) {
            const Repetition* r = s.has_rep ? &s.rep : nullptr;
            if (s.kind == 0) w.rectangle(s.l, s.d, s.pts[0].x, s.pts[0].y, s.pts[2].x - s.pts[0].x, s.pts[2].y - s.pts[0].y, r);
            else if (s.kind == 1) w.polygon(s.l, s.d, s.pts.data(), s.pts.size(), r);
            else w.path(s.l, s.d, s.pts.data(), s.pts.size(), s.hw, s.es, s.ee, r);
            if (&s == &c.shapes[0]) w.property("SHAPEPROP", { PropValue::str("first") });
        }
        for (auto& t : c.texts) { w.text(t.l, t.t, t.x, t.y, t.s); w.property("TEXTPROP", { PropValue::sint(t.x) }); }
        w.end_cell();
    }
    const bool ok = w.close();
    if (!ok) printf("      %s\n", w.error().c_str());
    if (st) *st = w.stats();
    return ok;
}

static bool write_scene_gds(const std::vector<SCell>& sc, const std::string& path) {
    LdbFile f; f.format = 0; f.dbu_per_micron = 1000;
    for (auto& c : sc) {
        LdbCell lc; lc.name = c.name;
        for (auto& in : c.insts) {
            auto add = [&](int64_t dx, int64_t dy) {
                LdbInstance li; li.ref_name = in.child;
                li.tr.ox = in.t.x + dx; li.tr.oy = in.t.y + dy; li.tr.mag = in.t.mag; li.tr.angle_deg = in.t.angle; li.tr.mirror_x = in.t.mirror;
                lc.instances.push_back(li);
            };
            if (!in.has_rep) add(0, 0);
            else for (auto& o : rep_offsets(in.rep)) add(o.x, o.y);
        }
        for (auto& s : c.shapes) {
            std::vector<Pt> offs = s.has_rep ? rep_offsets(s.rep) : std::vector<Pt>{ { 0, 0 } };
            for (auto& o : offs) {
                LdbPolyElem pe; pe.layer = (int)s.l; pe.datatype = (int)s.d; pe.kind = s.kind == 2 ? 1 : 0;
                for (auto& q : s.pts) pe.pts.push_back({ q.x + o.x, q.y + o.y });
                if (s.kind == 2) { pe.width = 2 * s.hw; pe.pathtype = 4; pe.ext_begin = s.es; pe.ext_end = s.ee; }
                lc.polys.push_back(pe);
            }
        }
        for (auto& t : c.texts) { LdbTextElem te; te.layer = (int)t.l; te.texttype = (int)t.t; te.x = t.x; te.y = t.y; te.str = t.s; lc.texts.push_back(te); }
        f.cells.push_back(lc);
    }
    std::string err;
    if (!write_gdsii_model(&f, path.c_str(), err)) { printf("      GDSII: %s\n", err.c_str()); return false; }
    return true;
}

// ===========================================================================
// D. dumps of our reader / scanner / tables for the KLayout comparison
// ===========================================================================
static void box_cb(void* ud, const char*, int l, int d, int, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
    fprintf((FILE*)ud, "%d %d %lld %lld %lld %lld\n", l, d, (long long)x1, (long long)y1, (long long)x2, (long long)y2);
}

static bool dump(const std::string& path, bool strict) {
    char err[512] = "";
    ldb_open_opts_t oo{ LDB_OPEN_PROPERTY | LDB_OPEN_SHAPES, 0 };
    ldb_file_t* f = ldb_open_opts(path.c_str(), &oo, err, sizeof err);
    check(f != nullptr, "our reader opens " + path + (f ? "" : std::string(": ") + err));
    if (!f) return false;
    // Flattened boxes, without the subtree of cell ROT (30-degree / mag 1.5 placements: our flat
    // iterator reports bounding boxes there); KLayout compares with ROT unselected too.
    for (auto& c : ((LdbFile*)f)->cells) if (c.name == "ROT") { c.instances.clear(); c.boxes.clear(); c.polys.clear(); c.boxes_compact.clear(); }
    FILE* o = fopen((path + ".boxes").c_str(), "w");
    ldb_iterate_boxes_flat(f, box_cb, o);
    fclose(o);
    ldb_close(f);
    // lossless scanner: per-layer bbox of shapes (repetitions included), per cell
    std::vector<uint8_t> data;
    { std::ifstream in(path, std::ios::binary); data.assign(std::istreambuf_iterator<char>(in), {}); }
    std::map<std::string, std::map<std::pair<uint64_t, uint64_t>, oasis_rec::BBox>> per;
    std::string cell; std::map<uint64_t, std::string> cname; uint64_t implicit = 0;
    oasis_rec::FileInfo fi;
    try {
        oasis_rec::scan(data.data(), data.size(), true, [&](const oasis_rec::Record& r) {
            if (r.id == 3) cname[implicit++] = r.name;
            if (r.id == 4) cname[r.ref] = r.name;
            if (r.id == 13) cell = "#" + std::to_string(r.ref);
            if (r.id == 14) cell = r.name;
            if (r.is_shape && r.box.valid()) per[cell][{ r.layer, r.datatype }].add(r.box);
        }, fi);
    } catch (const oasis_codec::ParseError& e) { check(false, "scanner " + path + ": " + e.msg); return false; }
    o = fopen((path + ".rec").c_str(), "w");
    for (auto& c : per) {
        std::string nm = c.first;
        if (!nm.empty() && nm[0] == '#') { auto it = cname.find(std::stoull(nm.substr(1))); if (it != cname.end()) nm = it->second; }
        for (auto& l : c.second) fprintf(o, "%s %llu %llu %lld %lld %lld %lld\n", nm.c_str(), (unsigned long long)l.first.first, (unsigned long long)l.first.second,
                                         (long long)l.second.x1, (long long)l.second.y1, (long long)l.second.x2, (long long)l.second.y2);
    }
    fclose(o);
    if (strict) {
        ldb_oas_info_t* I = ldb_oas_info_open(path.c_str(), err, sizeof err);
        check(I != nullptr, "oasis_tables opens " + path);
        if (!I) return false;
        check(ldb_oas_info_strict(I) == 1, "strict CELLNAME table: " + path);
        check(ldb_oas_info_offsets_ok(I) == 1, "every S_CELL_OFFSET points at its CELL: " + path);
        char top[256] = "";
        check(ldb_oas_info_top(I, top, sizeof top) == 1 && std::string(top) == "TOP", "S_TOP_CELL = TOP: " + path + " (got '" + top + "')");
        o = fopen((path + ".cells").c_str(), "w");
        for (int i = 0; i < ldb_oas_info_n_cells(I); ++i) {
            char nm[256]; int64_t off; int fl; int64_t bb[4];
            ldb_oas_info_cell(I, i, nm, sizeof nm, &off, &fl, bb);
            fprintf(o, "%s %d %lld %lld %lld %lld %lld\n", nm, fl, (long long)bb[0], (long long)bb[1], (long long)bb[2], (long long)bb[3], (long long)off);
        }
        fclose(o);
        ldb_oas_info_close(I);
    }
    return true;
}

// Property association (31.8, 31.9) as seen through ldb_iterate_properties: counts of
// (kind, cell, name) -> n.
static std::map<std::string, int> prop_census(const std::string& path) {
    std::map<std::string, int> m;
    char err[512] = "";
    ldb_open_opts_t oo{ LDB_OPEN_PROPERTY, 0 };
    ldb_file_t* f = ldb_open_opts(path.c_str(), &oo, err, sizeof err);
    if (!f) return m;
    ldb_iterate_properties(f, [](void* ud, int kind, const char* cell, const char* name, const char*) {
        auto& mm = *(std::map<std::string, int>*)ud;
        mm[std::to_string(kind) + ":" + (kind ? cell : "") + ":" + name]++;
    }, &m);
    ldb_close(f);
    return m;
}

static void check_props(const std::string& path, const std::vector<std::pair<std::string, int>>& want) {
    auto m = prop_census(path);
    for (auto& w : want) {
        auto it = m.find(w.first);
        const int got = it == m.end() ? 0 : it->second;
        check(got == w.second, "property " + w.first + " x" + std::to_string(w.second) + " in " + path + " (got " + std::to_string(got) + ")");
    }
}

// END validation signature recomputed over the whole file (14.4.2, 14.5.1).
static void check_validation(const std::string& path) {
    std::vector<uint8_t> d;
    { std::ifstream in(path, std::ios::binary); d.assign(std::istreambuf_iterator<char>(in), {}); }
    if (d.size() < 256) { check(false, "file too short: " + path); return; }
    const uint8_t* end = d.data() + d.size() - 256;
    check(end[0] == 2, "END record is the last 256 bytes: " + path);
    // scheme: the byte before the signature (4 bytes) or the last byte
    const uint8_t s4 = d[d.size() - 5], s0 = d.back();
    if (s4 == 1 || s4 == 2) {
        const uint32_t stored = (uint32_t)d[d.size() - 4] | ((uint32_t)d[d.size() - 3] << 8) | ((uint32_t)d[d.size() - 2] << 16) | ((uint32_t)d[d.size() - 1] << 24);
        uint32_t v = 0;
        if (s4 == 1) v = (uint32_t)crc32(crc32(0L, Z_NULL, 0), d.data(), (uInt)(d.size() - 4));
        else for (size_t i = 0; i + 4 < d.size(); ++i) v += d[i];
        check(v == stored, std::string(s4 == 1 ? "CRC32" : "CHECKSUM32") + " signature matches: " + path);
    } else check(s0 == 0, "validation scheme 0: " + path);
}

// ===========================================================================
// D. review regressions: zero-width paths, dangling TEXTSTRING refs, GDSII layer range
// ===========================================================================
static void count_cb(void* ud, const char*, int, int, int kind, int npts, const int64_t*, const int64_t*) {
    auto* c = static_cast<std::map<int, int>*>(ud); (*c)[kind] += 1; (void)npts;
}
static void part_d(const std::string& dir) {
    Asm a;
    a.bytes("%SEMI-OASIS\r\n", 13);
    a.rec(1); a.str("1.0"); a.uint_(0); a.uint_(1000); a.uint_(0); for (int i = 0; i < 12; ++i) a.uint_(0);
    a.rec(14); a.str("TOP");                                     // CELL by name
    a.rec(22); a.u8(0x7B); a.uint_(1); a.uint_(0); a.uint_(0);   // PATH W P X Y D L: layer 1, hw 0
    a.uint_(0); a.uint_(1); a.sint(100);                         //   point list type 0, one delta
    a.sint(0); a.sint(0);
    a.rec(19); a.u8(0x7B); a.uint_(99); a.uint_(5); a.uint_(0);  // TEXT by ref 99 (undefined)
    a.sint(10); a.sint(20);
    a.rec(20); a.u8(0x7B); a.uint_(40000); a.uint_(0);           // RECTANGLE on layer 40000
    a.uint_(10); a.uint_(10); a.sint(0); a.sint(0);
    a.end_record(false, false);
    const std::string p = dir + "/rv_regress.oas";
    FILE* o = fopen(p.c_str(), "wb"); fwrite(a.b.data(), 1, a.b.size(), o); fclose(o);
    char err[512] = "";
    ldb_open_opts_t so{ LDB_OPEN_SHAPES, 0 };
    ldb_file_t* f = ldb_open_opts(p.c_str(), &so, err, sizeof err);
    check(f != nullptr, std::string("SHAPES open with a dangling TEXTSTRING reference ") + err);
    if (!f) return;
    std::map<int, int> kinds;
    ldb_iterate_polygons(f, count_cb, &kinds);
    check(kinds[1] == 1, "zero-width PATH kept by LDB_OPEN_SHAPES");
    check(ldb_write_gdsii(f, (dir + "/rv_regress.gds").c_str(), err, sizeof err) != 0 &&
          std::string(err).find("40000") != std::string::npos, std::string("GDSII rejects layer 40000: ") + err);
    ldb_close(f);
}

int main(int argc, char** argv) {
    if (argc < 2) { fprintf(stderr, "usage: db_oasis_selftest OUTDIR\n"); return 2; }
    const std::string dir = argv[1];
    printf("A. codec vs. SEMI P39 tables\n");
    part_a();
    printf("B. reader files (every record type)\n");
    std::vector<std::string> files;
    for (int k = 0; k < 2; ++k) {
        const std::string p = dir + (k ? "/rd_explicit.oas" : "/rd_implicit.oas");
        auto v = reader_file(k == 1, false);
        FILE* o = fopen(p.c_str(), "wb"); fwrite(v.data(), 1, v.size(), o); fclose(o);
        files.push_back(p);
        check_validation(p);
        dump(p, false);
    }
    for (auto& p : files)
        check_props(p, { { "0::FILEPROP", 2 }, { "2:CHILD:PN", 2 }, { "3:TOP:prop#5", 0 }, { "3:TOP:PN", 2 } });
    {   // the X records: our reader only (KLayout does not take XGEOMETRY)
        const std::string p = dir + "/rd_xrecords.oas";
        auto v = reader_file(false, true);
        FILE* o = fopen(p.c_str(), "wb"); fwrite(v.data(), 1, v.size(), o); fclose(o);
        dump(p, false);
    }
    printf("C. writer (random hierarchical layout)\n");
    const auto scene = make_scene(12345);
    check(write_scene_gds(scene, dir + "/wr_ref.gds"), "GDSII model writer: wr_ref.gds");
    struct V { const char* name; oasis_writer::Options o; };
    std::vector<V> vars;
    { V v{ "wr_strict", {} }; vars.push_back(v); }
    { V v{ "wr_tiny_cblocks", {} }; v.o.cblock_bytes = 256; v.o.independent_cblocks = true; vars.push_back(v); }
    { V v{ "wr_nostrict", {} }; v.o.strict = false; v.o.cblock_bytes = 0; vars.push_back(v); }
    { V v{ "wr_plain_checksum", {} }; v.o.cblock_bytes = 0; v.o.validation = 2; vars.push_back(v); }
    { V v{ "wr_noreserve", {} }; v.o.header_reserve = 0; v.o.validation = 0; vars.push_back(v); }
    for (auto& v : vars) {
        const std::string p = dir + "/" + v.name + ".oas";
        oasis_writer::Stats st;
        check(write_scene_oasis(scene, p, v.o, &st), std::string("writer: ") + v.name);
        printf("      %-18s %6llu bytes, %llu CBLOCKs, %llu rect %llu poly %llu path %llu text %llu place %llu prop, %llu repetitions (%llu elements)\n",
               v.name, (unsigned long long)st.bytes, (unsigned long long)st.cblocks, (unsigned long long)st.rectangles, (unsigned long long)st.polygons,
               (unsigned long long)st.paths, (unsigned long long)st.texts, (unsigned long long)st.placements, (unsigned long long)st.properties,
               (unsigned long long)st.repetitions, (unsigned long long)st.shapes_in_repetitions);
        check_validation(p);
        dump(p, v.o.strict && !(std::string(v.name) == "wr_noreserve"));
    }
    // file / cell / element / CELLNAME association in the writer's strict file
    check_props(dir + "/wr_strict.oas", { { "0::WRITER", 1 }, { "1:TOP:TOPPROP", 2 }, { "1:TOP:MANY", 1 }, { "1:LEAF2:LEAFPROP", 1 },
                                          { "2:LEAF2:SHAPEPROP", 1 }, { "2:LEAF2:TEXTPROP", 2 }, { "1:LEAF2:SHAPEPROP", 0 },
                                          { "1:LEAF2:S_CELL_OFFSET", 1 }, { "1:MID0:S_BOUNDING_BOX", 1 }, { "0::S_TOP_CELL", 1 } });
    {   // model round trips: GDSII -> model -> OASIS (re-encoded) and OASIS -> model -> GDSII
        char err[512] = "";
        ldb_file_t* g = ldb_open((dir + "/wr_ref.gds").c_str(), err, sizeof err);
        check(g != nullptr, "open wr_ref.gds");
        if (g) {
            check(ldb_dbu_per_micron(g) == 1000.0, "GDSII UNITS -> 1000 dbu/um");
            ldb_oasis_write_opts_t o; ldb_oasis_write_opts_default(&o);
            check(ldb_write_oasis_opts(g, (dir + "/rt_gds2oas.oas").c_str(), &o, err, sizeof err) == 0, std::string("GDSII model -> OASIS ") + err);
            ldb_close(g);
            check_validation(dir + "/rt_gds2oas.oas");
            dump(dir + "/rt_gds2oas.oas", true);
        }
        ldb_open_opts_t so{ LDB_OPEN_PROPERTY | LDB_OPEN_SHAPES, 0 };
        ldb_file_t* s = ldb_open_opts((dir + "/wr_strict.oas").c_str(), &so, err, sizeof err);
        if (s) {
            check(ldb_dbu_per_micron(s) == 1000.0, "OASIS START unit -> 1000");
            check(ldb_write_gdsii(s, (dir + "/rt_oas2gds.gds").c_str(), err, sizeof err) == 0, std::string("OASIS model -> GDSII ") + err);
            ldb_oasis_write_opts_t o; ldb_oasis_write_opts_default(&o); o.reencode = 1;
            check(ldb_write_oasis_opts(s, (dir + "/rt_oas2oas.oas").c_str(), &o, err, sizeof err) == 0, std::string("OASIS model -> OASIS re-encoded ") + err);
            ldb_close(s);
            dump(dir + "/rt_oas2oas.oas", true);
        }
    }
    printf("D. review regressions\n");
    part_d(dir);
    printf("selftest: %d passed, %d failed\n", g_pass, g_fail);
    return g_fail;
}
