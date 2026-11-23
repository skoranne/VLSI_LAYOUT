// oasis_tables.cpp: open an OASIS file by its name tables only (no geometry decode).
//
// Reads START (unit, offset-flag), the table offsets (in START or END), and then only the
// CELLNAME table (with the standard properties S_CELL_OFFSET and S_BOUNDING_BOX attached to
// each CELLNAME), the PROPNAME table (when property names are given by reference), the
// LAYERNAME table and the file properties after START (S_TOP_CELL). A table may sit inside a
// CBLOCK; that CBLOCK alone is inflated. The cost is independent of the geometry size, so a
// strict-mode file of any size opens in milliseconds; this is what the viewer uses for its
// first frame, layer names and cell bounding boxes.
#include "layoutdb.h"
#include "oasis_codec.hpp"
#include <zlib.h>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

using oasis_codec::Reader;
using oasis_codec::ParseError;

namespace {

struct LayerName { std::string name; uint64_t l_lo, l_hi, d_lo, d_hi; bool text; };
struct CellInfo {
    std::string name; uint64_t refnum = 0;
    int64_t offset = -1;                         // S_CELL_OFFSET, -1 if absent
    int bbox_flags = -1;                         // S_BOUNDING_BOX flags, -1 if absent
    int64_t x = 0, y = 0, w = 0, h = 0;
};
struct PropVal { int type; uint64_t u; int64_t i; std::string s; };

}

struct ldb_oas_info {
    std::string path, top, error;
    double unit = 0, open_ms = 0;
    int offset_flag = 0;
    uint64_t table_flag[6] = {}, table_off[6] = {};
    uint64_t file_size = 0;
    std::vector<LayerName> layers;
    std::vector<CellInfo> cells;
    std::vector<std::pair<uint64_t, std::string>> propnames;   // refnum -> name
    int verified = -1;                                         // ldb_oas_info_verify result
};

namespace {

void read_interval(Reader& s, uint64_t& lo, uint64_t& hi) {
    const uint64_t inf = ~0ull;
    switch (s.uint_()) {
    case 0: lo = 0; hi = inf; break;
    case 1: lo = 0; hi = s.uint_(); break;
    case 2: lo = s.uint_(); hi = inf; break;
    case 3: lo = hi = s.uint_(); break;
    case 4: lo = s.uint_(); hi = s.uint_(); break;
    default: s.fail("invalid interval type");
    }
}

// PROPERTY record body (after the record id). Returns the name ("" when given by an unknown
// reference) and the values. Keeps the modal name/values for record 29.
struct PropModal { std::string name; std::vector<PropVal> vals; };
void read_property(Reader& s, ldb_oas_info* I, PropModal& m) {
    uint8_t info = s.u8();
    if (info & 0x04) {
        if (info & 0x02) {
            uint64_t r = s.uint_();
            m.name.clear();
            for (auto& p : I->propnames) if (p.first == r) { m.name = p.second; break; }
        } else m.name = s.str();
    }
    if (!(info & 0x08)) {
        uint64_t count = info >> 4; if (count == 15) count = s.uint_();
        m.vals.clear();
        for (uint64_t i = 0; i < count; ++i) {
            PropVal v{}; v.type = (int)s.uint_();
            if (v.type <= 7) (void)s.real_of((uint64_t)v.type);
            else if (v.type == 8) v.u = s.uint_();
            else if (v.type == 9) v.i = s.sint();
            else if (v.type <= 12) v.s = s.str();
            else if (v.type <= 15) v.u = s.uint_();
            else s.fail("invalid property value type");
            m.vals.push_back(v);
        }
    }
}

int64_t as_int(const PropVal& v) { return v.type == 9 ? v.i : (int64_t)v.u; }

// Bytes of the table starting at file offset off: the file itself, or the inflated CBLOCK
// that starts there. Returns false on error.
bool table_bytes(const uint8_t* data, uint64_t size, uint64_t off, std::vector<uint8_t>& buf,
                 const uint8_t*& p, uint64_t& n) {
    if (off >= size) return false;
    if (data[off] != 34) { p = data + off; n = size - off; return true; }
    Reader s(data + off + 1, size - off - 1, "table CBLOCK");
    uint64_t comp_type = s.uint_(), ulen = s.uint_(), clen = s.uint_();
    if (comp_type != 0) return false;
    s.need(clen);
    buf.resize((size_t)ulen);
    z_stream zs{};
    if (inflateInit2(&zs, -15) != Z_OK) return false;
    zs.next_in = const_cast<uint8_t*>(s.p + s.off); zs.avail_in = (uInt)clen;
    zs.next_out = buf.data(); zs.avail_out = (uInt)ulen;
    int rc = inflate(&zs, Z_FINISH);
    inflateEnd(&zs);
    if (rc != Z_STREAM_END) return false;
    p = buf.data(); n = buf.size();
    return true;
}

void parse_propname_table(ldb_oas_info* I, const uint8_t* data, uint64_t size) {
    if (!I->table_off[2]) return;
    std::vector<uint8_t> buf; const uint8_t* p; uint64_t n;
    if (!table_bytes(data, size, I->table_off[2], buf, p, n)) return;
    Reader s(p, n, "PROPNAME table");
    uint64_t implicit = 0;
    while (!s.eof()) {
        uint8_t id = s.u8();
        if (id == 0) continue;
        if (id == 7) I->propnames.push_back({ implicit++, s.str() });
        else if (id == 8) { std::string nm = s.str(); I->propnames.push_back({ s.uint_(), nm }); }
        else break;
    }
}

void parse_cellname_table(ldb_oas_info* I, const uint8_t* data, uint64_t size) {
    if (!I->table_off[0]) return;
    std::vector<uint8_t> buf; const uint8_t* p; uint64_t n;
    if (!table_bytes(data, size, I->table_off[0], buf, p, n)) { I->error = "cannot read CELLNAME table"; return; }
    Reader s(p, n, "CELLNAME table");
    uint64_t implicit = 0;
    PropModal pm;
    auto apply = [&](CellInfo& c) {
        if (pm.name == "S_CELL_OFFSET" && pm.vals.size() >= 1) c.offset = as_int(pm.vals[0]);
        else if (pm.name == "S_BOUNDING_BOX" && pm.vals.size() >= 5) {
            c.bbox_flags = (int)as_int(pm.vals[0]);
            c.x = as_int(pm.vals[1]); c.y = as_int(pm.vals[2]);
            c.w = as_int(pm.vals[3]); c.h = as_int(pm.vals[4]);
        }
    };
    while (!s.eof()) {
        uint8_t id = s.u8();
        if (id == 0) continue;
        if (id == 3) { CellInfo c; c.name = s.str(); c.refnum = implicit++; I->cells.push_back(c); }
        else if (id == 4) { CellInfo c; c.name = s.str(); c.refnum = s.uint_(); I->cells.push_back(c); }
        else if (id == 28) { read_property(s, I, pm); if (!I->cells.empty()) apply(I->cells.back()); }
        else if (id == 29) { if (!I->cells.empty()) apply(I->cells.back()); }
        else break;
    }
}

void parse_layername_table(ldb_oas_info* I, const uint8_t* data, uint64_t size) {
    if (!I->table_off[4]) return;
    std::vector<uint8_t> buf; const uint8_t* p; uint64_t n;
    if (!table_bytes(data, size, I->table_off[4], buf, p, n)) return;
    Reader s(p, n, "LAYERNAME table");
    PropModal pm;
    while (!s.eof()) {
        uint8_t id = s.u8();
        if (id == 0) continue;
        if (id == 11 || id == 12) {
            LayerName l; l.name = s.str(); l.text = (id == 12);
            read_interval(s, l.l_lo, l.l_hi); read_interval(s, l.d_lo, l.d_hi);
            I->layers.push_back(l);
        } else if (id == 28) read_property(s, I, pm);        // properties of a LAYERNAME: ignored
        else if (id == 29) {}
        else break;
    }
}

} // namespace

extern "C" {

ldb_oas_info_t* ldb_oas_info_open(const char* path, char* errbuf, size_t errbuf_sz) {
    auto t0 = std::chrono::steady_clock::now();
    auto fail = [&](const std::string& m) -> ldb_oas_info_t* {
        if (errbuf && errbuf_sz) snprintf(errbuf, errbuf_sz, "%s", m.c_str());
        return nullptr;
    };
    int fd = ::open(path, O_RDONLY);
    if (fd < 0) return fail(std::string("cannot open ") + path);
    struct stat st; fstat(fd, &st);
    const uint64_t size = (uint64_t)st.st_size;
    if (size < 13 + 256) { ::close(fd); return fail("file too small for OASIS"); }
    void* map = ::mmap(nullptr, size, PROT_READ, MAP_PRIVATE, fd, 0);
    ::close(fd);
    if (map == MAP_FAILED) return fail("mmap failed");
    const uint8_t* data = (const uint8_t*)map;
    ldb_oas_info* I = new ldb_oas_info;
    I->path = path; I->file_size = size;
    try {
        if (memcmp(data, "%SEMI-OASIS\r\n", 13) != 0) throw ParseError{ "not an OASIS file (bad magic)" };
        Reader s(data + 13, size - 13, "START");
        if (s.u8() != 1) throw ParseError{ "first record is not START" };
        s.skip_str();
        I->unit = s.real();
        I->offset_flag = (int)s.uint_();
        if (I->offset_flag == 0) for (int i = 0; i < 6; ++i) { I->table_flag[i] = s.uint_(); I->table_off[i] = s.uint_(); }
        // file properties right after START (S_TOP_CELL, ...)
        PropModal pm;
        for (;;) {
            if (s.eof()) break;
            uint8_t id = s.p[s.off];
            if (id == 0) { s.off++; continue; }
            if (id != 28 && id != 29) break;
            s.off++;
            if (id == 28) read_property(s, I, pm);
            if (pm.name == "S_TOP_CELL" && !pm.vals.empty() && pm.vals[0].type >= 10 && pm.vals[0].type <= 12)
                I->top = pm.vals[0].s;
        }
        if (I->offset_flag != 0) {
            Reader e(data + size - 256, 256, "END");
            if (e.u8() != 2) throw ParseError{ "last 256 bytes are not an END record (table offsets in END expected)" };
            for (int i = 0; i < 6; ++i) { I->table_flag[i] = e.uint_(); I->table_off[i] = e.uint_(); }
        }
        parse_propname_table(I, data, size);
        parse_cellname_table(I, data, size);
        parse_layername_table(I, data, size);
        if (I->top.empty() && I->cells.size() == 1) I->top = I->cells[0].name;
    } catch (const ParseError& e) {
        ::munmap(map, size);
        std::string m = e.msg;
        delete I;
        return fail(m);
    }
    // --- verify S_CELL_OFFSET: each must point at a CELL record with the right reference ---
    int ok = 1;
    for (auto& c : I->cells) {
        if (c.offset < 0) continue;
        if ((uint64_t)c.offset >= size) { ok = 0; break; }
        const uint8_t id = data[c.offset];
        Reader r(data + c.offset + 1, size - c.offset - 1, "CELL");
        if (id == 13) { if (r.uint_() != c.refnum) { ok = 0; break; } }
        else if (id == 14) { if (r.str() != c.name) { ok = 0; break; } }
        else { ok = 0; break; }
    }
    I->verified = ok;
    ::munmap(map, size);
    I->open_ms = std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - t0).count();
    return I;
}

void ldb_oas_info_close(ldb_oas_info_t* I) { delete I; }

int ldb_oas_info_strict(const ldb_oas_info_t* I) {
    if (!I) return 0;
    return (I->table_off[0] != 0 && I->table_flag[0] == 1) ? 1 : 0;
}
int ldb_oas_info_offsets_ok(const ldb_oas_info_t* I) { return I ? I->verified : 0; }
double ldb_oas_info_unit(const ldb_oas_info_t* I) { return I ? I->unit : 0.0; }
double ldb_oas_info_open_ms(const ldb_oas_info_t* I) { return I ? I->open_ms : 0.0; }

static void copy_name(const std::string& s, char* out, int len) {
    if (!out || len <= 0) return;
    size_t k = std::min<size_t>(s.size(), (size_t)len - 1);
    memcpy(out, s.data(), k); out[k] = '\0';
}

int ldb_oas_info_top(const ldb_oas_info_t* I, char* name, int len) {
    if (!I || I->top.empty()) return 0;
    copy_name(I->top, name, len); return 1;
}

int ldb_oas_info_n_layernames(const ldb_oas_info_t* I) { return I ? (int)I->layers.size() : 0; }

int ldb_oas_info_layername(const ldb_oas_info_t* I, int i, int64_t* l_lo, int64_t* l_hi,
                           int64_t* d_lo, int64_t* d_hi, int* is_text, char* name, int len) {
    if (!I || i < 0 || i >= (int)I->layers.size()) return 0;
    const LayerName& l = I->layers[(size_t)i];
    *l_lo = (int64_t)l.l_lo; *l_hi = l.l_hi == ~0ull ? -1 : (int64_t)l.l_hi;
    *d_lo = (int64_t)l.d_lo; *d_hi = l.d_hi == ~0ull ? -1 : (int64_t)l.d_hi;
    *is_text = l.text ? 1 : 0;
    copy_name(l.name, name, len); return 1;
}

int ldb_oas_info_layer_name(const ldb_oas_info_t* I, int layer, int datatype, char* name, int len) {
    if (!I) return 0;
    for (const LayerName& l : I->layers) {
        if (l.text) continue;
        if ((uint64_t)layer < l.l_lo || (uint64_t)layer > l.l_hi) continue;
        if ((uint64_t)datatype < l.d_lo || (uint64_t)datatype > l.d_hi) continue;
        copy_name(l.name, name, len); return 1;
    }
    return 0;
}

int ldb_oas_info_n_cells(const ldb_oas_info_t* I) { return I ? (int)I->cells.size() : 0; }

int ldb_oas_info_cell(const ldb_oas_info_t* I, int i, char* name, int len, int64_t* offset,
                      int* bbox_flags, int64_t* bbox) {
    if (!I || i < 0 || i >= (int)I->cells.size()) return 0;
    const CellInfo& c = I->cells[(size_t)i];
    copy_name(c.name, name, len);
    *offset = c.offset; *bbox_flags = c.bbox_flags;
    bbox[0] = c.x; bbox[1] = c.y; bbox[2] = c.x + c.w; bbox[3] = c.y + c.h;
    return 1;
}

} // extern "C"
