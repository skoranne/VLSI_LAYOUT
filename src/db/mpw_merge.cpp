// MPW_MERGE: combine the per-slot OASIS files of a multi-project-wafer (MPW)
// shuttle into one OASIS file with the dies on a grid separated by scribe
// lanes, and report on the slots.
//
//   MPW_MERGE -o mpw.oas --dir /path/to/mpw-007            (slot-*/tapeout/outputs/oas/*.oas)
//   MPW_MERGE -o mpw.oas a.oas b.oas ...
//
// How it works (no geometry is flattened or expanded at any point):
//
//  1. Analyse every slot (in parallel).  Name tables are read first, then each
//     cell gets a body hash: the hash of its records with every reference
//     number replaced by the name it refers to, so it does not depend on the
//     slot's numbering.  A Merkle hash adds the hashes of the child cells, and
//     a hierarchical bounding box is computed from shape boxes and placement
//     transforms.
//  2. Resolve cell names.  Cells with the same name and the same Merkle hash
//     in several slots are written once.  When the contents differ, the
//     version used by the most slots keeps the name and the others become
//     S<nnn>_<name> (nnn = slot number).  Each slot's top cell is always
//     renamed S<nnn>_<top>.  --prefix-all renames every cell instead.
//  3. Write.  Global CELLNAME / TEXTSTRING / PROPNAME / PROPSTRING tables, then
//     each cell exactly once, copied record by record with only the reference
//     fields renumbered (oasis_rec.hpp), recompressed into CBLOCKs.  Then the
//     generated cells: scribe lanes, slot labels and the top cell, which places
//     every slot's die at its grid site.
//
// Properties outside cells (file properties, properties on name records such
// as S_CELL_OFFSET) describe the input files and are dropped.

#include "oasis_rec.hpp"
#include "oasis_codec.hpp"

#include <zlib.h>
#include <glob.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <unistd.h>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <cinttypes>
#include <cmath>
#include <condition_variable>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <map>
#include <mutex>
#include <set>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using oasis_rec::BBox;
using oasis_rec::Placement;
using oasis_rec::Record;
using oasis_rec::RefSpan;

namespace {

double now_s() {
    using namespace std::chrono;
    return duration<double>(steady_clock::now().time_since_epoch()).count();
}

// ---------------------------------------------------------------------------
// 128-bit content hash (two independent 64-bit lanes)
// ---------------------------------------------------------------------------
struct H128 {
    uint64_t a = 0, b = 0;
    bool operator==(const H128& o) const { return a == o.a && b == o.b; }
    bool operator!=(const H128& o) const { return !(*this == o); }
    bool operator<(const H128& o) const { return a != o.a ? a < o.a : b < o.b; }
};
struct H128Hash { size_t operator()(const H128& h) const { return (size_t)(h.a ^ (h.b * 0x9E3779B97F4A7C15ULL)); } };

inline uint64_t rotl(uint64_t x, int r) { return (x << r) | (x >> (64 - r)); }
inline uint64_t fmix(uint64_t k) {
    k ^= k >> 33; k *= 0xff51afd7ed558ccdULL; k ^= k >> 33; k *= 0xc4ceb9fe1a85ec53ULL; k ^= k >> 33; return k;
}

struct Hasher {
    uint64_t a, b; uint64_t n = 0;
    explicit Hasher(uint64_t seed = 0) : a(0x243F6A8885A308D3ULL ^ seed), b(0x13198A2E03707344ULL + seed) {}
    void word(uint64_t w) {
        a ^= fmix(w * 0x87c37b91114253d5ULL); a = rotl(a, 27) * 5 + 0x52dce729;
        b ^= fmix(w * 0x4cf5ad432745937fULL + a); b = rotl(b, 31) * 5 + 0x38495ab5;
    }
    void bytes(const uint8_t* p, size_t len) {
        n += len;
        while (len >= 8) { uint64_t w; memcpy(&w, p, 8); word(w); p += 8; len -= 8; }
        if (len) { uint64_t w = 0; memcpy(&w, p, len); word(w ^ ((uint64_t)len << 56)); }
    }
    void str(const std::string& s) { word(s.size()); bytes((const uint8_t*)s.data(), s.size()); }
    void h(const H128& x) { word(x.a); word(x.b); }
    H128 done() const { H128 r; r.a = fmix(a ^ n) ^ b; r.b = fmix(b + n * 0x9E3779B97F4A7C15ULL) ^ a; return r; }
};

// ---------------------------------------------------------------------------
// Memory-mapped input file
// ---------------------------------------------------------------------------
struct Mapped {
    const uint8_t* p = nullptr; size_t n = 0; int fd = -1;
    bool open(const std::string& path, std::string& err) {
        fd = ::open(path.c_str(), O_RDONLY);
        if (fd < 0) { err = "cannot open " + path + ": " + strerror(errno); return false; }
        struct stat st; if (fstat(fd, &st) != 0) { err = "cannot stat " + path; return false; }
        n = (size_t)st.st_size;
        void* m = mmap(nullptr, n, PROT_READ, MAP_PRIVATE, fd, 0);
        if (m == MAP_FAILED) { err = "cannot mmap " + path; n = 0; return false; }
        madvise(m, n, MADV_SEQUENTIAL);
        p = (const uint8_t*)m; return true;
    }
    ~Mapped() { if (p) munmap((void*)p, n); if (fd >= 0) ::close(fd); }
};

// ---------------------------------------------------------------------------
// Per-slot model (pass 1 results; no geometry kept)
// ---------------------------------------------------------------------------
struct Agg {                           // all placements of one child with one transform
    int child; double mag, angle; bool mirror;
    BBox org;                          // box of placement origins incl. repetition offsets
    uint64_t count = 0;
};

struct Cell {
    std::string name;
    bool defined = false;
    H128 body, merkle; bool merkle_done = false;   // named: child references hashed by name
    H128 body_anon, content;                        // anonymous: child references hashed by contents
    std::vector<int> ref_order;                     // children in order of first reference
    BBox local, hier; bool hier_done = false;
    std::vector<Agg> aggs;
    std::vector<int> children;         // unique child indices
    uint64_t records = 0, shapes = 0, placements = 0, bytes = 0;
    int parents = 0;
    int out_id = -1;                   // output cell id
    bool owner = false;                // this slot writes this cell
};

// Reference number -> string, stored as one arena plus offsets (millions of
// PROPSTRINGs per file are common: one per instance).  Dense numbers are
// indexed directly; far-away explicit numbers fall back to a map.
struct NameTable {
    static constexpr uint32_t MISSING = 0xFFFFFFFFu;
    std::string arena;
    std::vector<uint64_t> off; std::vector<uint32_t> len;
    std::map<uint64_t, std::pair<uint64_t, uint32_t>> sparse;
    uint64_t next = 0;
    void put(uint64_t r, const std::string& v) {
        const uint64_t o = arena.size(); arena += v;
        if (r < off.size() + (1u << 20)) {
            if (r >= off.size()) { off.resize(r + 1, 0); len.resize(r + 1, MISSING); }
            off[r] = o; len[r] = (uint32_t)v.size();
        } else sparse[r] = { o, (uint32_t)v.size() };
    }
    void implicit(const std::string& v) { put(next++, v); }
    void explicit_(uint64_t r, const std::string& v) { put(r, v); }
    bool get(uint64_t r, std::string_view& out) const {
        if (r < off.size()) { if (len[r] == MISSING) return false; out = std::string_view(arena.data() + off[r], len[r]); return true; }
        auto it = sparse.find(r); if (it == sparse.end()) return false;
        out = std::string_view(arena.data() + it->second.first, it->second.second); return true;
    }
    uint64_t bound() const { return sparse.empty() ? off.size() : sparse.rbegin()->first + 1; }   // max ref + 1
    template <class F> void each(F&& f) const {
        std::string_view v;
        for (uint64_t r = 0; r < off.size(); ++r) if (get(r, v)) f(r, v);
        for (auto& kv : sparse) { get(kv.first, v); f(kv.first, v); }
    }
};

struct Slot {
    std::string path; int number = 0; size_t file_bytes = 0;
    oasis_rec::FileInfo info;
    NameTable cellnames, textstrings, propnames, propstrings;
    std::vector<std::string> layernames;               // raw LAYERNAME records
    std::vector<Cell> cells; std::unordered_map<std::string, int> index;
    std::vector<int> def_order;                         // cells in definition order
    std::vector<int> tops; int top = -1;
    uint64_t rec_counts[35] = {};
    std::set<std::pair<uint64_t, uint64_t>> layers;
    size_t outside_props = 0, xnames = 0, undefined_cells = 0;
    std::string error;
    double t1 = 0, t2 = 0;
    // pass 2
    // Global reference = base + slot reference; `used` marks the ones written cells need.
    uint64_t text_base = 0, pname_base = 0, pstr_base = 0;
    std::vector<uint8_t> text_used, pname_used, pstr_used;
    std::vector<int> cellref_out;                       // slot cell reference number -> output cell id
    std::vector<uint8_t> out; size_t out_cells = 0, out_records = 0;
    // report
    size_t n_shared = 0, n_unique = 0, n_renamed = 0;

    int cell(const std::string& nm) {
        auto it = index.find(nm);
        if (it != index.end()) return it->second;
        int id = (int)cells.size(); cells.emplace_back(); cells.back().name = nm; index[nm] = id; return id;
    }
};

static const char* rec_name(int id) {
    static const char* n[35] = { "PAD", "START", "END", "CELLNAME", "CELLNAME", "TEXTSTRING", "TEXTSTRING",
        "PROPNAME", "PROPNAME", "PROPSTRING", "PROPSTRING", "LAYERNAME", "LAYERNAME", "CELL", "CELL",
        "XYABSOLUTE", "XYRELATIVE", "PLACEMENT", "PLACEMENT", "TEXT", "RECTANGLE", "POLYGON", "PATH",
        "TRAPEZOID", "TRAPEZOID", "TRAPEZOID", "CTRAPEZOID", "CIRCLE", "PROPERTY", "PROPERTY", "XNAME",
        "XNAME", "XELEMENT", "XGEOMETRY", "CBLOCK" };
    return (id >= 0 && id < 35) ? n[id] : "?";
}

// ---------------------------------------------------------------------------
// Pass 1: analyse one slot
// ---------------------------------------------------------------------------
struct AggKey {
    int child; uint64_t mag, angle; bool mirror;
    bool operator==(const AggKey& o) const { return child == o.child && mag == o.mag && angle == o.angle && mirror == o.mirror; }
};
struct AggKeyHash { size_t operator()(const AggKey& k) const {
    return (size_t)fmix((uint64_t)k.child * 0x9E3779B97F4A7C15ULL ^ k.mag ^ rotl(k.angle, 17) ^ (k.mirror ? 0x5555 : 0)); } };

static uint64_t dbits(double d) { uint64_t u; memcpy(&u, &d, 8); return u; }

static std::string_view lookup(const NameTable& t, uint64_t r, const char* what) {
    std::string_view v;
    if (!t.get(r, v)) throw oasis_codec::ParseError{ std::string("undefined ") + what + " reference " + std::to_string(r) };
    return v;
}

static void analyse(Slot& S) {
    double t0 = now_s();
    std::string err;
    Mapped f;
    if (!f.open(S.path, err)) { S.error = err; return; }
    S.file_bytes = f.n;
    try {
        // 1a: name tables (they may follow the cells that use them).
        oasis_rec::FileInfo fi;
        oasis_rec::scan(f.p, f.n, false, [&](const Record& r) {
            switch (r.id) {
            case 3: S.cellnames.implicit(r.name); break;    case 4: S.cellnames.explicit_(r.ref, r.name); break;
            case 5: S.textstrings.implicit(r.name); break;  case 6: S.textstrings.explicit_(r.ref, r.name); break;
            case 7: S.propnames.implicit(r.name); break;    case 8: S.propnames.explicit_(r.ref, r.name); break;
            case 9: S.propstrings.implicit(r.name); break;  case 10: S.propstrings.explicit_(r.ref, r.name); break;
            case 11: case 12: S.layernames.emplace_back((const char*)r.data, r.len); break;
            case 30: case 31: S.xnames++; break;
            default: break;
            }
        }, fi);

        // 1b: cells, hashes, boxes.
        int cur = -1;
        Hasher hs, ha;
        std::vector<uint8_t> canon, anon;
        std::unordered_map<int, uint32_t> ordinal;
        std::unordered_map<AggKey, size_t, AggKeyHash> aggidx;
        auto finish = [&]() {
            if (cur < 0) return;
            Cell& c = S.cells[(size_t)cur];
            c.body = hs.done(); c.body_anon = ha.done(); ordinal.clear();
            std::set<int> ch;
            for (auto& a : c.aggs) ch.insert(a.child);
            c.children.assign(ch.begin(), ch.end());
            aggidx.clear(); cur = -1;
        };
        oasis_rec::scan(f.p, f.n, true, [&](const Record& r) {
            if (r.id < 35) S.rec_counts[r.id]++;
            if (cur >= 0 && (!r.in_cell || r.id == 13 || r.id == 14)) finish();
            if (r.id == 13 || r.id == 14) {
                const std::string nm = (r.id == 13) ? std::string(lookup(S.cellnames, r.ref, "cell")) : r.name;
                cur = S.cell(nm);
                Cell& c = S.cells[(size_t)cur];
                if (c.defined) throw oasis_codec::ParseError{ "cell '" + nm + "' is defined twice" };
                c.defined = true; S.def_order.push_back(cur);
                hs = Hasher(1); ha = Hasher(4);
                return;
            }
            if (!r.in_cell) { if (r.id == 28 || r.id == 29) S.outside_props++; return; }
            S.cells[(size_t)cur].records++; S.cells[(size_t)cur].bytes += r.len;
            // canonical bytes: references replaced by the names they denote
            canon.clear(); anon.clear();
            size_t last = 0;
            for (const RefSpan& sp : r.refs) {
                canon.insert(canon.end(), r.data + last, r.data + sp.off);
                anon.insert(anon.end(), r.data + last, r.data + sp.off);
                std::string_view sv;
                switch (sp.kind) {
                case oasis_rec::REF_CELL_NUM: sv = lookup(S.cellnames, sp.num, "cell"); break;
                case oasis_rec::REF_CELL_STR: sv = sp.str; break;
                case oasis_rec::REF_TEXT_NUM: sv = lookup(S.textstrings, sp.num, "text-string"); break;
                case oasis_rec::REF_PROPNAME_NUM: sv = lookup(S.propnames, sp.num, "prop-name"); break;
                case oasis_rec::REF_PROPSTR_NUM: sv = lookup(S.propstrings, sp.num, "prop-string"); break;
                }
                const std::string_view* s = &sv;
                canon.push_back(0xFF); canon.push_back((uint8_t)sp.kind);
                uint64_t L = s->size(); canon.insert(canon.end(), (uint8_t*)&L, (uint8_t*)&L + 8);
                canon.insert(canon.end(), s->begin(), s->end());
                anon.push_back(0xFF); anon.push_back((uint8_t)sp.kind);
                if (sp.kind == oasis_rec::REF_CELL_NUM || sp.kind == oasis_rec::REF_CELL_STR) {
                    // anonymous form: the child's ordinal; its contents enter the hash later
                    int child = S.cell(std::string(*s));
                    auto it = ordinal.find(child);
                    if (it == ordinal.end()) {
                        it = ordinal.emplace(child, (uint32_t)S.cells[(size_t)cur].ref_order.size()).first;
                        S.cells[(size_t)cur].ref_order.push_back(child);
                    }
                    uint64_t o = it->second; anon.insert(anon.end(), (uint8_t*)&o, (uint8_t*)&o + 8);
                } else {
                    anon.insert(anon.end(), (uint8_t*)&L, (uint8_t*)&L + 8);
                    anon.insert(anon.end(), s->begin(), s->end());
                }
                last = sp.off + sp.len;
            }
            canon.insert(canon.end(), r.data + last, r.data + r.len);
            anon.insert(anon.end(), r.data + last, r.data + r.len);
            hs.word(canon.size()); hs.bytes(canon.data(), canon.size());
            ha.word(anon.size()); ha.bytes(anon.data(), anon.size());

            Cell& c = S.cells[(size_t)cur];             // S.cell() above may have reallocated
            if (r.is_shape) { c.shapes++; c.local.add(r.box); S.layers.insert({ r.layer, r.datatype }); }
            else if (r.id == 19) S.layers.insert({ r.layer, r.datatype });
            if (r.is_placement) {
                c.placements++;
                const Placement& p = r.pl;
                const std::string nm = p.by_number ? std::string(lookup(S.cellnames, p.num, "cell")) : p.name;
                int child = S.cell(nm);
                Cell& cc = S.cells[(size_t)cur];         // S.cell() may have reallocated
                AggKey k{ child, dbits(p.mag), dbits(p.angle), p.mirror };
                auto it = aggidx.find(k);
                if (it == aggidx.end()) {
                    it = aggidx.emplace(k, cc.aggs.size()).first;
                    Agg a; a.child = child; a.mag = p.mag; a.angle = p.angle; a.mirror = p.mirror;
                    cc.aggs.push_back(a);
                }
                Agg& a = cc.aggs[it->second];
                a.org.add(p.x + p.rx1, p.y + p.ry1); a.org.add(p.x + p.rx2, p.y + p.ry2);
                a.count++;
            }
        }, S.info);
        finish();
    } catch (const oasis_codec::ParseError& e) {
        S.error = S.path + ": " + e.msg; return;
    }

    // Parents, tops, undefined cells.
    for (auto& c : S.cells) for (int ch : c.children) S.cells[(size_t)ch].parents++;
    for (int i = 0; i < (int)S.cells.size(); ++i) {
        if (!S.cells[(size_t)i].defined) S.undefined_cells++;
        else if (S.cells[(size_t)i].parents == 0) S.tops.push_back(i);
    }

    // Merkle hashes and hierarchical boxes, children first.
    std::vector<std::pair<int, size_t>> st;
    std::vector<uint8_t> state(S.cells.size(), 0);          // 0 new, 1 open, 2 done
    for (int root = 0; root < (int)S.cells.size(); ++root) {
        if (state[(size_t)root]) continue;
        st.push_back({ root, 0 }); state[(size_t)root] = 1;
        while (!st.empty()) {
            auto& [ci, k] = st.back();
            Cell& c = S.cells[(size_t)ci];
            if (k < c.children.size()) {
                int ch = c.children[k++];
                if (state[(size_t)ch] == 1) { S.error = S.path + ": recursive hierarchy at cell " + c.name; return; }
                if (state[(size_t)ch] == 0) { state[(size_t)ch] = 1; st.push_back({ ch, 0 }); }
                continue;
            }
            Hasher h(2);
            h.word(c.defined ? 1 : 0); h.h(c.body);
            std::vector<int> kids = c.children;
            std::sort(kids.begin(), kids.end(), [&](int x, int y) { return S.cells[(size_t)x].name < S.cells[(size_t)y].name; });
            for (int ch : kids) { h.str(S.cells[(size_t)ch].name); h.h(S.cells[(size_t)ch].merkle); }
            c.merkle = h.done(); c.merkle_done = true;
            Hasher hc(5);
            if (c.defined) { hc.word(1); hc.h(c.body_anon); for (int ch : c.ref_order) hc.h(S.cells[(size_t)ch].content); }
            else { hc.word(0); hc.str(c.name); }      // a reference-only cell is known by its name alone
            c.content = hc.done();
            c.hier = c.local;
            for (auto& a : c.aggs) {
                const BBox& cb = S.cells[(size_t)a.child].hier;
                if (!cb.valid() || !a.org.valid()) continue;
                Placement p; p.mag = a.mag; p.angle = a.angle; p.mirror = a.mirror;
                BBox tb = oasis_rec::transform_box(cb, p);
                c.hier.add(a.org.x1 + tb.x1, a.org.y1 + tb.y1);
                c.hier.add(a.org.x2 + tb.x2, a.org.y2 + tb.y2);
            }
            c.hier_done = true;
            state[(size_t)ci] = 2; st.pop_back();
        }
    }
    // Main top: the largest one.
    double best = -1;
    for (int t : S.tops) {
        const BBox& b = S.cells[(size_t)t].hier;
        double area = b.valid() ? (double)(b.x2 - b.x1) * (double)(b.y2 - b.y1) : 0.0;
        if (area > best) { best = area; S.top = t; }
    }
    if (S.top < 0) S.error = S.path + ": no top cell";
    S.t1 = now_s() - t0;
}

// ---------------------------------------------------------------------------
// Output encoding helpers
// ---------------------------------------------------------------------------
struct Out : oasis_codec::Encoder {
    void cellname(const std::string& n, uint64_t id) { u8(4); str(n); uint_(id); }
    void cell(uint64_t id) { u8(13); uint_(id); }
    void rectangle(uint64_t layer, uint64_t dt, int64_t x, int64_t y, int64_t w, int64_t h) {
        u8(20); u8(0x7B); uint_(layer); uint_(dt); uint_((uint64_t)w); uint_((uint64_t)h); sint(x); sint(y);
    }
    void text(const std::string& s, uint64_t layer, uint64_t tt, int64_t x, int64_t y) {
        u8(19); u8(0x5B); str(s); uint_(layer); uint_(tt); sint(x); sint(y);
    }
    void place(uint64_t cell, int64_t x, int64_t y) { u8(17); u8(0xF0); uint_(cell); sint(x); sint(y); }
};

// Raw deflate (OASIS CBLOCK comp-type 0) of `in`, appended as a CBLOCK record.
static void emit_cblock(std::vector<uint8_t>& out, const std::vector<uint8_t>& in, int level) {
    if (in.empty()) return;
    z_stream zs; memset(&zs, 0, sizeof(zs));
    if (deflateInit2(&zs, level, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY) != Z_OK) throw std::runtime_error("deflateInit2 failed");
    std::vector<uint8_t> buf(deflateBound(&zs, (uLong)in.size()));
    zs.next_in = (Bytef*)in.data(); zs.avail_in = (uInt)in.size();
    zs.next_out = buf.data(); zs.avail_out = (uInt)buf.size();
    int rc = deflate(&zs, Z_FINISH);
    if (rc != Z_STREAM_END) { deflateEnd(&zs); throw std::runtime_error("deflate failed"); }
    size_t comp = zs.total_out; deflateEnd(&zs);
    Out o; o.u8(34); o.uint_(0); o.uint_(in.size()); o.uint_(comp);
    out.insert(out.end(), o.b.begin(), o.b.end());
    out.insert(out.end(), buf.begin(), buf.begin() + (long)comp);
}

struct Options {
    std::string out_path, dir, top_name, report, json;
    std::vector<std::string> files;
    double scribe_um = 100.0;
    int cols = 0, rows = 0;
    uint64_t scribe_layer = 255, scribe_dt = 0, label_layer = 255, label_tt = 1;
    bool prefix_all = false, by_name = false, cblock = true, analyze_only = false, verify = false;
    int threads = 0, level = 6;
    size_t cblock_bytes = 1u << 20;
};

// Re-emit `r` with references renumbered: append to `dst`.
static void rewrite(Slot& S, const Record& r, std::vector<uint8_t>& dst) {
    size_t start = dst.size(), last = 0;
    Out e;
    for (const RefSpan& sp : r.refs) {
        dst.insert(dst.end(), r.data + last, r.data + sp.off);
        e.b.clear();
        switch (sp.kind) {
        case oasis_rec::REF_CELL_NUM: e.uint_((uint64_t)S.cellref_out.at(sp.num)); break;
        case oasis_rec::REF_CELL_STR:   // inline name -> reference number: set the N bit of the info byte
            e.uint_((uint64_t)S.cells[(size_t)S.index.at(sp.str)].out_id);
            dst[start + 1] |= 0x40;
            break;
        case oasis_rec::REF_TEXT_NUM: S.text_used.at(sp.num) = 1; e.uint_(S.text_base + sp.num); break;
        case oasis_rec::REF_PROPNAME_NUM: S.pname_used.at(sp.num) = 1; e.uint_(S.pname_base + sp.num); break;
        case oasis_rec::REF_PROPSTR_NUM: S.pstr_used.at(sp.num) = 1; e.uint_(S.pstr_base + sp.num); break;
        }
        dst.insert(dst.end(), e.b.begin(), e.b.end());
        last = sp.off + sp.len;
    }
    dst.insert(dst.end(), r.data + last, r.data + r.len);
}

// ---------------------------------------------------------------------------
// Pass 2: write the cells this slot owns into S.out
// ---------------------------------------------------------------------------
static void emit_slot(Slot& S, const Options& opt) {
    double t0 = now_s();
    std::string err;
    Mapped f;
    if (!f.open(S.path, err)) { S.error = err; return; }
    std::vector<uint8_t> pending;
    std::vector<uint8_t>& sink = opt.cblock ? pending : S.out;
    auto flush = [&](bool force) {
        if (!opt.cblock) return;
        if (pending.empty() || (!force && pending.size() < opt.cblock_bytes)) return;
        emit_cblock(S.out, pending, opt.level); pending.clear();
    };
    try {
        bool emit = false;
        oasis_rec::FileInfo fi;
        oasis_rec::scan(f.p, f.n, false, [&](const Record& r) {
            if (r.id == 13 || r.id == 14) {
                const Cell& c = S.cells[(size_t)S.index.at(r.id == 13 ? std::string(lookup(S.cellnames, r.ref, "cell")) : r.name)];
                emit = c.owner;
                if (emit) { Out o; o.cell((uint64_t)c.out_id); sink.insert(sink.end(), o.b.begin(), o.b.end()); S.out_cells++; }
                return;
            }
            if (!r.in_cell || r.id <= 2) { emit = false; return; }      // PAD/START/END never belong to a cell
            if (!emit) return;
            rewrite(S, r, sink); S.out_records++;
            flush(false);
        }, fi);
        flush(true);
    } catch (const std::exception& e) {
        S.error = S.path + ": " + e.what();
    } catch (const oasis_codec::ParseError& e) {
        S.error = S.path + ": " + e.msg;
    }
    S.t2 = now_s() - t0;
}

// ---------------------------------------------------------------------------
// Thread pool helper
// ---------------------------------------------------------------------------
template <class F> void parallel_for(size_t n, int threads, F&& fn) {
    std::atomic<size_t> next{ 0 };
    std::vector<std::thread> pool;
    int t = std::max(1, std::min<int>(threads, (int)n));
    for (int i = 0; i < t; ++i)
        pool.emplace_back([&]() { for (size_t k; (k = next++) < n;) fn(k); });
    for (auto& th : pool) th.join();
}

static int slot_number_from_path(const std::string& p, int fallback) {
    size_t pos = p.rfind("slot-");
    if (pos == std::string::npos) return fallback;
    int v = atoi(p.c_str() + pos + 5);
    return v > 0 ? v : fallback;
}

static std::string um(int64_t v, double unit) { char b[64]; snprintf(b, sizeof(b), "%.3f", (double)v / unit); return b; }

static std::string json_esc(const std::string& s) {
    std::string o;
    for (char c : s) { if (c == '"' || c == '\\') { o += '\\'; o += c; } else if ((unsigned char)c < 0x20) { char b[8]; snprintf(b, sizeof(b), "\\u%04x", c); o += b; } else o += c; }
    return o;
}

static void usage(const char* a0) {
    fprintf(stderr,
        "usage: %s -o out.oas (--dir MPW_DIR | file.oas ...) [options]\n"
        "  --dir D            slots are D/slot-*/tapeout/outputs/oas/*.oas (sorted)\n"
        "  --scribe UM        scribe lane width in microns (default 100)\n"
        "  --cols N --rows M  grid (default: near square, e.g. 8x5 for 40 slots)\n"
        "  --scribe-layer L/D marker layer for the scribe lanes (default 255/0)\n"
        "  --label-layer L/T  layer for slot labels (default 255/1)\n"
        "  --top NAME         top cell name (default MPW_TOP)\n"
        "  --dedup-by-name    share a cell only when name and contents agree (default: contents alone)\n"
        "  --prefix-all       rename every cell S<nnn>_<name> instead of sharing identical cells\n"
        "  --no-cblock        write uncompressed records (default: CBLOCKs of ~1 MiB)\n"
        "  --level N          deflate level 1..9 (default 6)\n"
        "  --threads N        worker threads (default: cores, at most one per slot)\n"
        "  --report FILE      text report (default: stdout)\n"
        "  --json FILE        JSON report\n"
        "  --analyze-only     analyse and report, do not write an OASIS file\n"
        "  --verify           re-read the written file: every record decodes, every reference\n"
        "                     resolves, all cells are present and the top cell is the only top\n", a0);
}

static bool parse_ld(const char* s, uint64_t& l, uint64_t& d) {
    unsigned long a, b; if (sscanf(s, "%lu/%lu", &a, &b) != 2) return false; l = a; d = b; return true;
}

} // namespace

int main(int argc, char** argv) {
    double t_start = now_s();
    Options opt;
    for (int i = 1; i < argc; ++i) {
        std::string a = argv[i];
        auto need = [&](const char* what) -> const char* {
            if (i + 1 >= argc) { fprintf(stderr, "missing value for %s\n", what); exit(2); }
            return argv[++i];
        };
        if (a == "-o") opt.out_path = need("-o");
        else if (a == "--dir") opt.dir = need("--dir");
        else if (a == "--scribe") opt.scribe_um = atof(need("--scribe"));
        else if (a == "--cols") opt.cols = atoi(need("--cols"));
        else if (a == "--rows") opt.rows = atoi(need("--rows"));
        else if (a == "--scribe-layer") { if (!parse_ld(need(a.c_str()), opt.scribe_layer, opt.scribe_dt)) { usage(argv[0]); return 2; } }
        else if (a == "--label-layer") { if (!parse_ld(need(a.c_str()), opt.label_layer, opt.label_tt)) { usage(argv[0]); return 2; } }
        else if (a == "--top") opt.top_name = need("--top");
        else if (a == "--prefix-all") opt.prefix_all = true;
        else if (a == "--dedup-by-name") opt.by_name = true;
        else if (a == "--no-cblock") opt.cblock = false;
        else if (a == "--level") opt.level = atoi(need("--level"));
        else if (a == "--threads") opt.threads = atoi(need("--threads"));
        else if (a == "--report") opt.report = need("--report");
        else if (a == "--json") opt.json = need("--json");
        else if (a == "--analyze-only") opt.analyze_only = true;
        else if (a == "--verify") opt.verify = true;
        else if (a == "-h" || a == "--help") { usage(argv[0]); return 0; }
        else if (!a.empty() && a[0] == '-') { fprintf(stderr, "unknown option %s\n", a.c_str()); usage(argv[0]); return 2; }
        else opt.files.push_back(a);
    }
    if (!opt.dir.empty()) {
        glob_t g; std::string pat = opt.dir + "/slot-*/tapeout/outputs/oas/*.oas";
        if (glob(pat.c_str(), 0, nullptr, &g) == 0) for (size_t i = 0; i < g.gl_pathc; ++i) opt.files.push_back(g.gl_pathv[i]);
        globfree(&g);
    }
    if (opt.files.empty() || (opt.out_path.empty() && !opt.analyze_only)) { usage(argv[0]); return 2; }
    if (opt.top_name.empty()) opt.top_name = "MPW_TOP";
    if (opt.threads <= 0) opt.threads = (int)std::max(1u, std::thread::hardware_concurrency());

    std::vector<Slot> slots(opt.files.size());
    for (size_t i = 0; i < slots.size(); ++i) {
        slots[i].path = opt.files[i];
        slots[i].number = slot_number_from_path(opt.files[i], (int)i + 1);
    }

    // ---- pass 1 -----------------------------------------------------------------
    double t1 = now_s();
    parallel_for(slots.size(), opt.threads, [&](size_t k) { analyse(slots[k]); });
    t1 = now_s() - t1;
    int bad = 0;
    for (auto& S : slots) if (!S.error.empty()) { fprintf(stderr, "ERROR: %s\n", S.error.c_str()); bad++; }
    if (bad) return 1;
    const double unit = slots[0].info.unit;
    for (auto& S : slots)
        if (S.info.unit != unit) { fprintf(stderr, "ERROR: %s has unit %g, %s has %g\n", S.path.c_str(), S.info.unit, slots[0].path.c_str(), unit); return 1; }

    // ---- resolve names ------------------------------------------------------------
    // A variant is one output cell.  Its key is the content hash (default: cells
    // with identical contents merge whatever their names) or, with
    // --dedup-by-name, the named Merkle hash (cells merge only when name and
    // contents agree).  Top cells and --prefix-all variants are per slot.
    struct Variant { H128 key; std::vector<std::pair<int, int>> uses; std::string name, out_name;
                     std::set<std::string> names; int out_id = -1; bool renamed = false, per_slot = false; };
    struct Group { std::vector<Variant*> v; };
    std::deque<Variant> variants;
    std::unordered_map<H128, size_t, H128Hash> var_index;
    std::map<std::pair<int, int>, Variant*> var_of;
    std::unordered_set<std::string> original_names;
    for (int s = 0; s < (int)slots.size(); ++s) {
        Slot& S = slots[(size_t)s];
        std::vector<int> order = S.def_order;
        for (int c = 0; c < (int)S.cells.size(); ++c) if (!S.cells[(size_t)c].defined) order.push_back(c);
        for (int c : order) {
            const Cell& C = S.cells[(size_t)c];
            original_names.insert(C.name);
            const bool per_slot = opt.prefix_all || c == S.top;
            H128 key = opt.by_name ? C.merkle : C.content;
            if (opt.by_name || !C.defined) { Hasher h(6); h.h(key); h.str(C.name); key = h.done(); }
            if (per_slot) { Hasher h(3); h.h(key); h.word((uint64_t)s); key = h.done(); }
            auto it = var_index.find(key);
            if (it == var_index.end()) {
                it = var_index.emplace(key, variants.size()).first;
                variants.emplace_back(); variants.back().key = key; variants.back().name = C.name; variants.back().per_slot = per_slot;
            }
            Variant& v = variants[it->second];
            v.uses.push_back({ s, c }); v.names.insert(C.name);
            var_of[{ s, c }] = &v;
        }
    }
    // Group variants by preferred name (that of their first use).  The variant
    // used by the most slots keeps the name (ties: lowest slot); the others
    // become S<nnn>_<name>.  Per-slot variants never keep the name.
    std::map<std::string, Group> groups;
    for (auto& v : variants) groups[v.name].v.push_back(&v);
    auto slot_count = [](const Variant* v) { std::set<int> ss; for (auto& u : v->uses) ss.insert(u.first); return ss.size(); };
    std::unordered_set<std::string> used_names;
    std::vector<Variant*> to_rename;
    for (auto& [name, g] : groups) {
        Variant* keep = nullptr;
        for (Variant* v : g.v) {
            if (v->per_slot) continue;
            if (!keep || slot_count(v) > slot_count(keep)) keep = v;    // g.v is in slot order: ties keep the earlier
        }
        for (Variant* v : g.v) {
            if (v == keep) { v->out_name = name; used_names.insert(name); }
            else to_rename.push_back(v);
        }
    }
    for (Variant* v : to_rename) {
        char pre[32]; snprintf(pre, sizeof(pre), "S%03d_", slots[(size_t)v->uses[0].first].number);
        // A new name must not clash with any original cell name either.
        std::string base = pre + v->name;
        std::string n = base; int k = 2;
        while (used_names.count(n) || original_names.count(n)) n = base + "_v" + std::to_string(k++);
        used_names.insert(n);
        v->out_name = n; v->renamed = true;
    }
    if (!opt.analyze_only)
    for (const std::string& gen : { opt.top_name, std::string("MPW_SCRIBE"), std::string("MPW_LABELS") })
        if (used_names.count(gen) || original_names.count(gen)) { fprintf(stderr, "ERROR: generated cell name %s clashes with an input cell\n", gen.c_str()); return 1; }

    // Output ids: slots in order, cells in definition order; the first slot with a
    // variant writes it.  Variants defined nowhere (references only) come last.
    std::vector<std::string> out_names;
    for (int s = 0; s < (int)slots.size(); ++s)
        for (int c : slots[(size_t)s].def_order) {
            Variant* v = var_of[{ s, c }];
            if (v->out_id < 0) { v->out_id = (int)out_names.size(); out_names.push_back(v->out_name); slots[(size_t)s].cells[(size_t)c].owner = true; }
        }
    size_t ghost = 0;
    for (auto& v : variants) if (v.out_id < 0) { v.out_id = (int)out_names.size(); out_names.push_back(v.out_name); ghost++; }
    for (int s = 0; s < (int)slots.size(); ++s) {
        Slot& S = slots[(size_t)s];
        for (int c = 0; c < (int)S.cells.size(); ++c) {
            Variant* v = var_of[{ s, c }];
            S.cells[(size_t)c].out_id = v->out_id;
            if (!S.cells[(size_t)c].defined) continue;
            if (v->out_name != S.cells[(size_t)c].name) S.n_renamed++;
            if (slot_count(v) > 1) S.n_shared++; else S.n_unique++;
        }
    }

    // Global string references: slot s owns [base_s, base_s + bound_s).  No
    // global dedup: the strings are mostly unique per slot (instance names).
    {
        uint64_t tb = 0, nb = 0, sb = 0;
        for (auto& S : slots) {
            S.text_base = tb; tb += S.textstrings.bound(); S.text_used.assign(S.textstrings.bound(), 0);
            S.pname_base = nb; nb += S.propnames.bound(); S.pname_used.assign(S.propnames.bound(), 0);
            S.pstr_base = sb; sb += S.propstrings.bound(); S.pstr_used.assign(S.propstrings.bound(), 0);
            S.cellref_out.assign(S.cellnames.bound(), -1);
            S.cellnames.each([&](uint64_t r, std::string_view v) {
                auto it = S.index.find(std::string(v));
                if (it != S.index.end()) S.cellref_out[r] = S.cells[(size_t)it->second].out_id;
            });
        }
    }
    std::vector<std::string> layernames; std::unordered_set<std::string> layername_seen;
    for (auto& S : slots) for (auto& l : S.layernames) if (layername_seen.insert(l).second) layernames.push_back(l);

    // ---- grid ---------------------------------------------------------------------
    const int n = (int)slots.size();
    int64_t W = 0, H = 0;
    for (auto& S : slots) {
        const BBox& b = S.cells[(size_t)S.top].hier;
        W = std::max(W, b.x2 - b.x1); H = std::max(H, b.y2 - b.y1);
    }
    const int64_t lane = (int64_t)std::llround(opt.scribe_um * unit);
    if (opt.cols <= 0 && opt.rows <= 0) {
        double best = 1e300;
        for (int c = 1; c <= n; ++c) {
            int r = (n + c - 1) / c;
            double w = (double)c * W + (c + 1.0) * lane, h = (double)r * H + (r + 1.0) * lane;
            double score = std::max(w, h) / std::min(w, h) + 1e-3 * (c * r - n);
            if (score < best) { best = score; opt.cols = c; opt.rows = r; }
        }
    } else if (opt.cols <= 0) opt.cols = (n + opt.rows - 1) / opt.rows;
    else if (opt.rows <= 0) opt.rows = (n + opt.cols - 1) / opt.cols;
    if ((int64_t)opt.cols * opt.rows < n) { fprintf(stderr, "ERROR: grid %dx%d too small for %d slots\n", opt.cols, opt.rows, n); return 1; }
    const int64_t RX = opt.cols * W + (opt.cols + 1) * lane, RY = opt.rows * H + (opt.rows + 1) * lane;
    struct Site { int64_t px, py; int col, row; };
    std::vector<Site> sites((size_t)n);
    for (int k = 0; k < n; ++k) {
        const BBox& b = slots[(size_t)k].cells[(size_t)slots[(size_t)k].top].hier;
        int col = k % opt.cols, row = k / opt.cols;
        int64_t x0 = lane + col * (W + lane), y0 = lane + row * (H + lane);
        // centre the die in its site
        sites[(size_t)k] = { x0 + (W - (b.x2 - b.x1)) / 2 - b.x1, y0 + (H - (b.y2 - b.y1)) / 2 - b.y1, col, row };
    }

    // ---- pass 2 + write -----------------------------------------------------------------
    double t2 = now_s();
    size_t out_bytes = 0, table_entries = 0;
    const int top_id = (int)out_names.size(), scribe_id = top_id + 1, label_id = top_id + 2;
    if (!opt.analyze_only) {
        out_names.push_back(opt.top_name); out_names.push_back("MPW_SCRIBE"); out_names.push_back("MPW_LABELS");
        FILE* fp = fopen(opt.out_path.c_str(), "wb");
        if (!fp) { fprintf(stderr, "ERROR: cannot write %s\n", opt.out_path.c_str()); return 1; }
        auto put = [&](const std::vector<uint8_t>& b) {
            if (!b.empty() && fwrite(b.data(), 1, b.size(), fp) != b.size()) { fprintf(stderr, "ERROR: write failed\n"); exit(1); }
            out_bytes += b.size();
        };
        // START + file property S_TOP_CELL
        Out h;
        h.bytes("%SEMI-OASIS\r\n", 13);
        h.u8(1); h.str("1.0");
        if (unit == std::floor(unit) && unit > 0) h.real_whole((uint64_t)unit); else h.real_double(unit);
        h.uint_(0); for (int i = 0; i < 12; ++i) h.uint_(0);       // table offsets: none (not strict)
        h.u8(28); h.u8(0x15); h.str("S_TOP_CELL"); h.uint_(12); h.str(opt.top_name);
        put(h.b);
        // name tables
        Out tb;
        for (size_t i = 0; i < out_names.size(); ++i) tb.cellname(out_names[i], i);
        for (auto& l : layernames) tb.bytes(l.data(), l.size());
        if (opt.cblock) { std::vector<uint8_t> c; emit_cblock(c, tb.b, opt.level); put(c); } else put(tb.b);

        // slots: workers fill S.out, the main thread writes them in order
        std::mutex mu; std::condition_variable cv;
        std::vector<char> done((size_t)n, 0);
        std::thread writer_pool([&]() {
            parallel_for((size_t)n, opt.threads, [&](size_t k) {
                emit_slot(slots[k], opt);
                std::lock_guard<std::mutex> lk(mu); done[k] = 1; cv.notify_all();
            });
        });
        for (int k = 0; k < n; ++k) {
            std::unique_lock<std::mutex> lk(mu);
            cv.wait(lk, [&]() { return done[(size_t)k] != 0; });
            lk.unlock();
            Slot& S = slots[(size_t)k];
            if (!S.error.empty()) { fprintf(stderr, "ERROR: %s\n", S.error.c_str()); writer_pool.join(); fclose(fp); return 1; }
            put(S.out); std::vector<uint8_t>().swap(S.out);
        }
        writer_pool.join();

        // TEXTSTRING / PROPNAME / PROPSTRING tables: only entries the written cells use.
        {
            std::vector<uint8_t> pend, chunk;
            Out o;
            auto table = [&](uint8_t id, const NameTable& t, const std::vector<uint8_t>& used, uint64_t base) {
                t.each([&](uint64_t r, std::string_view v) {
                    if (r >= used.size() || !used[r]) return;
                    o.u8(id); o.uint_(v.size()); o.bytes(v.data(), v.size()); o.uint_(base + r);
                    if (o.b.size() >= opt.cblock_bytes) { if (opt.cblock) { chunk.clear(); emit_cblock(chunk, o.b, opt.level); put(chunk); } else put(o.b); o.b.clear(); }
                });
            };
            for (auto& S : slots) {
                table(6, S.textstrings, S.text_used, S.text_base);
                table(8, S.propnames, S.pname_used, S.pname_base);
                table(10, S.propstrings, S.pstr_used, S.pstr_base);
                for (auto& u : S.text_used) table_entries += u;
                for (auto& u : S.pname_used) table_entries += u;
                for (auto& u : S.pstr_used) table_entries += u;
            }
            if (opt.cblock) { chunk.clear(); emit_cblock(chunk, o.b, opt.level); put(chunk); } else put(o.b);
        }

        // generated cells
        Out g;
        g.cell((uint64_t)scribe_id);
        for (int j = 0; j <= opt.rows; ++j) g.rectangle(opt.scribe_layer, opt.scribe_dt, 0, j * (H + lane), RX, lane);
        for (int i = 0; i <= opt.cols; ++i)
            for (int j = 0; j < opt.rows; ++j) g.rectangle(opt.scribe_layer, opt.scribe_dt, i * (W + lane), j * (H + lane) + lane, lane, H);
        g.cell((uint64_t)label_id);
        for (int k = 0; k < n; ++k) {
            const Slot& S = slots[(size_t)k];
            char lbl[64]; snprintf(lbl, sizeof(lbl), "slot-%03d", S.number);
            std::string t = std::string(lbl) + " " + S.cells[(size_t)S.top].name;
            int64_t x0 = lane + sites[(size_t)k].col * (W + lane), y0 = lane + sites[(size_t)k].row * (H + lane);
            g.text(t, opt.label_layer, opt.label_tt, x0 + W / 2, y0 - lane / 2);
        }
        g.cell((uint64_t)top_id);
        g.place((uint64_t)scribe_id, 0, 0);
        g.place((uint64_t)label_id, 0, 0);
        for (int k = 0; k < n; ++k) {
            const Slot& S = slots[(size_t)k];
            g.place((uint64_t)S.cells[(size_t)S.top].out_id, sites[(size_t)k].px, sites[(size_t)k].py);
        }
        if (opt.cblock) { std::vector<uint8_t> c; emit_cblock(c, g.b, opt.level); put(c); } else put(g.b);
        // END: 256 bytes, no validation signature
        Out e; e.u8(2); e.uint_(252); for (int i = 0; i < 252; ++i) e.u8(0); e.uint_(0);
        put(e.b);
        if (fclose(fp) != 0) { fprintf(stderr, "ERROR: closing %s failed\n", opt.out_path.c_str()); return 1; }
    }
    t2 = now_s() - t2;

    // ---- optional verification of the written file --------------------------------
    std::string verify_msg;
    if (opt.verify && !opt.analyze_only) {
        double tv = now_s();
        Slot V; V.path = opt.out_path; V.number = 0;
        analyse(V);
        size_t ndef = 0; for (auto& c : V.cells) ndef += c.defined;
        std::string why;
        if (!V.error.empty()) why = V.error;
        else if (ndef + ghost != out_names.size()) why = "cells: " + std::to_string(ndef) + " defined + " + std::to_string(ghost) + " reference-only, expected " + std::to_string(out_names.size());
        else if (V.tops.size() != 1 || V.cells[(size_t)V.tops[0]].name != opt.top_name) why = "top cells: " + std::to_string(V.tops.size());
        else {
            const BBox& b = V.cells[(size_t)V.top].hier;
            if (b.x1 != 0 || b.y1 != 0 || b.x2 != RX || b.y2 != RY) why = "top extent differs from the reticle";
        }
        char buf[160];
        snprintf(buf, sizeof(buf), "%s (%zu cells, %zu records, %.2f s)", why.empty() ? "PASS" : "FAIL", ndef, V.info.records, now_s() - tv);
        verify_msg = buf;
        if (!why.empty()) verify_msg += ": " + why;
        fprintf(stderr, "verify: %s\n", verify_msg.c_str());
    }

    // ---- report -------------------------------------------------------------------
    struct rusage ru; getrusage(RUSAGE_SELF, &ru);
    FILE* rp = opt.report.empty() ? stdout : fopen(opt.report.c_str(), "w");
    if (!rp) { fprintf(stderr, "ERROR: cannot write %s\n", opt.report.c_str()); return 1; }
    size_t in_bytes = 0, defined_total = 0;
    for (auto& S : slots) { in_bytes += S.file_bytes; for (auto& c : S.cells) defined_total += c.defined; }
    fprintf(rp, "MPW_MERGE report\n================\n");
    fprintf(rp, "slots %d   unit %g/um   grid %d cols x %d rows   scribe %.3f um   die pitch %s x %s um\n",
            n, unit, opt.cols, opt.rows, opt.scribe_um, um(W + lane, unit).c_str(), um(H + lane, unit).c_str());
    fprintf(rp, "reticle extent %s x %s um   top cell %s\n", um(RX, unit).c_str(), um(RY, unit).c_str(), opt.top_name.c_str());
    fprintf(rp, "input %.1f MB in %d files, %zu cell definitions; output %zu cells (%zu generated, %zu reference-only), %.1f MB%s\n",
            in_bytes / 1e6, n, defined_total, out_names.size(), opt.analyze_only ? (size_t)0 : (size_t)3, ghost, out_bytes / 1e6,
            opt.analyze_only ? " (analyze only: nothing written)" : "");
    fprintf(rp, "time: analyse %.2f s, write %.2f s, total %.2f s; peak RSS %.0f MB; threads %d\n",
            t1, t2, now_s() - t_start, ru.ru_maxrss / 1024.0, opt.threads);
    if (!verify_msg.empty()) fprintf(rp, "verify: %s\n", verify_msg.c_str());
    fprintf(rp, "\n");
    fprintf(rp, "%-4s %-9s %-20s %8s %21s %21s %7s %7s %7s %7s %6s %8s %6s %6s\n", "slot", "site", "top cell", "MB",
            "die llx,lly (um)", "die w x h (um)", "cells", "shared", "unique", "renamd", "layers", "CBLOCKs", "t1 s", "t2 s");
    for (int k = 0; k < n; ++k) {
        const Slot& S = slots[(size_t)k];
        const BBox& b = S.cells[(size_t)S.top].hier;
        char site[32]; snprintf(site, sizeof(site), "c%d,r%d", sites[(size_t)k].col, sites[(size_t)k].row);
        size_t ndef = 0; for (auto& c : S.cells) ndef += c.defined;
        fprintf(rp, "%03d  %-9s %-20s %8.1f %10s,%-10s %10s x %-8s %7zu %7zu %7zu %7zu %6zu %8zu %6.2f %6.2f\n",
                S.number, site, S.cells[(size_t)S.top].name.c_str(), S.file_bytes / 1e6,
                um(b.x1, unit).c_str(), um(b.y1, unit).c_str(), um(b.x2 - b.x1, unit).c_str(), um(b.y2 - b.y1, unit).c_str(),
                ndef, S.n_shared, S.n_unique, S.n_renamed, S.layers.size(), S.info.cblocks, S.t1, S.t2);
    }
    // warnings
    for (auto& S : slots) {
        if (S.tops.size() > 1) fprintf(rp, "WARNING slot %03d: %zu top cells, placing %s\n", S.number, S.tops.size(), S.cells[(size_t)S.top].name.c_str());
        if (S.undefined_cells) fprintf(rp, "WARNING slot %03d: %zu referenced cells are not defined\n", S.number, S.undefined_cells);
        if (S.xnames) fprintf(rp, "WARNING slot %03d: %zu XNAME records dropped\n", S.number, S.xnames);
    }
    // record totals
    uint64_t tot[35] = {};
    for (auto& S : slots) for (int i = 0; i < 35; ++i) tot[i] += S.rec_counts[i];
    fprintf(rp, "\nrecords over all slots (CBLOCK contents included):\n");
    std::map<std::string, uint64_t> by_kind;
    for (int i = 0; i < 35; ++i) if (tot[i]) by_kind[rec_name(i)] += tot[i];
    for (auto& [k, v] : by_kind) fprintf(rp, "  %-12s %14" PRIu64 "\n", k.c_str(), v);
    // dedup
    size_t shared_vars = 0, multi_name_vars = 0, merged_defs = 0;
    std::vector<std::pair<size_t, Variant*>> by_use;
    for (auto& v : variants) {
        size_t sc = slot_count(&v);
        if (sc > 1) shared_vars++;
        if (v.names.size() > 1) multi_name_vars++;
        merged_defs += v.uses.size() - 1;
        by_use.push_back({ sc, &v });
    }
    fprintf(rp, "\ndedup (%s): %zu input cells -> %zu output cells; %zu shared by several slots; "
                "%zu merged from differently named copies\n",
            opt.prefix_all ? "--prefix-all" : opt.by_name ? "name + contents" : "contents",
            variants.size() + merged_defs, variants.size(), shared_vars, multi_name_vars);
    std::stable_sort(by_use.begin(), by_use.end(), [](auto& x, auto& y) { return x.first > y.first; });
    fprintf(rp, "most shared cells (output name: slots, input names):\n");
    for (size_t i = 0; i < by_use.size() && i < 15 && by_use[i].first > 1; ++i) {
        const Variant* v = by_use[i].second;
        fprintf(rp, "  %-48s %3zu slots, %zu name(s)", v->out_name.c_str(), by_use[i].first, v->names.size());
        size_t k = 0; for (auto& nm : v->names) { if (k++ == 3) { fprintf(rp, " ..."); break; } fprintf(rp, " %s", nm.c_str()); }
        fprintf(rp, "\n");
    }
    std::vector<std::pair<size_t, std::string>> multi;
    for (auto& [name, g] : groups) if (g.v.size() > 1) multi.push_back({ g.v.size(), name });
    std::sort(multi.begin(), multi.end(), [](auto& x, auto& y) { return x.first != y.first ? x.first > y.first : x.second < y.second; });
    fprintf(rp, "%zu names carry different contents in different slots; most variants first (output name <- slots):\n", multi.size());
    for (size_t i = 0; i < multi.size() && i < 20; ++i) {
        const Group& g = groups[multi[i].second];
        fprintf(rp, "  %-40s %zu variants\n", multi[i].second.c_str(), g.v.size());
        for (size_t j = 0; j < g.v.size() && j < 4; ++j) {
            std::set<int> ss; for (auto& u : g.v[j]->uses) ss.insert(slots[(size_t)u.first].number);
            fprintf(rp, "      %-44s <- %zu slot(s):", g.v[j]->out_name.c_str(), ss.size());
            size_t k = 0; for (int sn : ss) { if (k++ == 12) { fprintf(rp, " ..."); break; } fprintf(rp, " %03d", sn); }
            fprintf(rp, "\n");
        }
        if (g.v.size() > 4) fprintf(rp, "      ...\n");
    }
    if (rp != stdout) fclose(rp);

    if (!opt.json.empty()) {
        FILE* jp = fopen(opt.json.c_str(), "w");
        if (!jp) { fprintf(stderr, "ERROR: cannot write %s\n", opt.json.c_str()); return 1; }
        fprintf(jp, "{\n  \"unit_per_um\": %g, \"cols\": %d, \"rows\": %d, \"scribe_um\": %g,\n", unit, opt.cols, opt.rows, opt.scribe_um);
        fprintf(jp, "  \"die_pitch_um\": [%s, %s], \"reticle_um\": [%s, %s], \"top\": \"%s\", \"output\": \"%s\", \"output_bytes\": %zu,\n",
                um(W + lane, unit).c_str(), um(H + lane, unit).c_str(), um(RX, unit).c_str(), um(RY, unit).c_str(),
                json_esc(opt.top_name).c_str(), json_esc(opt.out_path).c_str(), out_bytes);
        fprintf(jp, "  \"output_cells\": %zu, \"input_bytes\": %zu, \"seconds\": %.3f, \"peak_rss_mb\": %.1f,\n",
                out_names.size(), in_bytes, now_s() - t_start, ru.ru_maxrss / 1024.0);
        fprintf(jp, "  \"slots\": [\n");
        for (int k = 0; k < n; ++k) {
            const Slot& S = slots[(size_t)k];
            const BBox& b = S.cells[(size_t)S.top].hier;
            size_t ndef = 0; for (auto& c : S.cells) ndef += c.defined;
            fprintf(jp, "    {\"slot\": %d, \"file\": \"%s\", \"bytes\": %zu, \"top\": \"%s\", \"out_top\": \"%s\", \"col\": %d, \"row\": %d,\n",
                    S.number, json_esc(S.path).c_str(), S.file_bytes, json_esc(S.cells[(size_t)S.top].name).c_str(),
                    json_esc(out_names[(size_t)S.cells[(size_t)S.top].out_id]).c_str(), sites[(size_t)k].col, sites[(size_t)k].row);
            fprintf(jp, "     \"die_bbox_um\": [%s, %s, %s, %s], \"placed_at_um\": [%s, %s], \"cells\": %zu, \"shared\": %zu, \"unique\": %zu, \"renamed\": %zu,\n",
                    um(b.x1, unit).c_str(), um(b.y1, unit).c_str(), um(b.x2, unit).c_str(), um(b.y2, unit).c_str(),
                    um(sites[(size_t)k].px, unit).c_str(), um(sites[(size_t)k].py, unit).c_str(), ndef, S.n_shared, S.n_unique, S.n_renamed);
            fprintf(jp, "     \"cblocks\": %zu, \"records\": %zu, \"layers\": [", S.info.cblocks, S.info.records);
            bool first = true;
            for (auto& [l, d] : S.layers) { fprintf(jp, "%s\"%" PRIu64 "/%" PRIu64 "\"", first ? "" : ",", l, d); first = false; }
            fprintf(jp, "],\n     \"record_counts\": {");
            first = true;
            for (int i = 0; i < 35; ++i) if (S.rec_counts[i]) { fprintf(jp, "%s\"%d\": %" PRIu64, first ? "" : ", ", i, S.rec_counts[i]); first = false; }
            fprintf(jp, "}}%s\n", k + 1 < n ? "," : "");
        }
        fprintf(jp, "  ],\n  \"variants\": {\n");
        bool firstg = true;
        for (auto& [name, g] : groups) {
            if (g.v.size() < 2) continue;
            fprintf(jp, "%s    \"%s\": [", firstg ? "" : ",\n", json_esc(name).c_str()); firstg = false;
            for (size_t j = 0; j < g.v.size(); ++j) {
                std::set<int> ss; for (auto& u : g.v[j]->uses) ss.insert(slots[(size_t)u.first].number);
                fprintf(jp, "%s{\"out\": \"%s\", \"slots\": [", j ? ", " : "", json_esc(g.v[j]->out_name).c_str());
                size_t k = 0; for (int sn : ss) fprintf(jp, "%s%d", k++ ? "," : "", sn);
                fprintf(jp, "]}");
            }
            fprintf(jp, "]");
        }
        fprintf(jp, "\n  }\n}\n");
        fclose(jp);
    }
    return (!verify_msg.empty() && verify_msg.compare(0, 4, "PASS") != 0) ? 3 : 0;
}
