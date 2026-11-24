// MAGVIEWER sidecar index (.mvi) for flat OASIS files: random access by CBLOCK, plus a
// per-layer density pyramid for drawing views with more shapes than pixels.
//
// A file is cut into *units*: every top-level CBLOCK is one unit, and the records between
// CBLOCKs form inline units (split at CELL records and every ~4 MB). For each unit the
// sidecar keeps its byte range, the boxes it produces per layer (count + bbox) and, when the
// unit depends on modal state set before it, a snapshot of that state. Any unit can then be
// decoded on its own, in parallel with the others, without reading the rest of the file.
//
// Build (ldb_mvi_build):
//   1. Top-level scan (CBLOCK payloads skipped) -> unit list.
//   2. Every unit decoded in parallel, speculatively: modal variables start undefined
//      (oasis_parser.hpp, Parser::track). A unit that reads an undefined variable stops
//      with Parser::Dependent; everything it emitted until then is exact.
//   3. In file order, the true start state of every unit is rebuilt from the end states
//      of the units before it; dependent units are finished from that state and keep it
//      as their snapshot. Files written by bin2oas are fully independent: no snapshots.
//   4. Density: box count and covered area per tile of a grid over the top cell bbox,
//      for every layer, reduced 2x2 per level up to a single tile. The grid is filled
//      during step 2 when the bbox is known up front (strict S_BOUNDING_BOX), otherwise
//      by a second parallel pass.
// The file is written to a temporary name and renamed, so readers never see a partial one.
// It records the OASIS file's size and mtime; a stale sidecar is rebuilt.

#include "oasis_parser.hpp"
#include "layoutdb.h"
#include <atomic>
#include <map>
#include <mutex>
#include <thread>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

using namespace oasis_parse;

namespace {

constexpr char kMagic[8] = { 'M', 'V', 'I', 'D', 'X', '0', '1', '\0' };
constexpr uint32_t UNIT_CBLOCK = 1, UNIT_INLINE = 2;
constexpr uint32_t UF_AFTER_CELL = 1, UF_SNAPSHOT = 2;
constexpr uint64_t FF_STRICT_BBOX = 1, FF_ALL_INDEPENDENT = 2;
constexpr size_t kInlineSplit = 4u << 20;

struct Hdr {
    char magic[8];
    uint64_t oas_size, oas_mtime_ns;
    double unit;
    int64_t bbox[4];
    int64_t tile;
    int32_t nx, ny, nlevels, nlayers;
    int64_t nunits, nentries;
    uint64_t flags;
    double build_s;
    uint64_t off_layers, off_units, off_entries, off_snap, snap_bytes, off_density, density_bytes;
    int64_t total_boxes, n_dependent;
    uint64_t reserved[8];
};
struct LayerRec { int64_t layer, datatype, count; int64_t bbox[4]; uint64_t density_off; };
struct UnitRec {
    uint64_t file_off, len;                // record bytes in the OASIS file (CBLOCK: whole record)
    uint32_t kind, flags;
    uint64_t snap_off, snap_len;
    uint64_t entry_first; uint32_t entry_count, in_cell;
    int64_t boxes;
};
struct EntryRec { int32_t layer, pad; int64_t count; int64_t bbox[4]; };

double now_s() {
    return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();
}

void level_dims(int nx, int ny, int l, int& ax, int& ay) {
    ax = std::max(1, (nx + (1 << l) - 1) >> l);
    ay = std::max(1, (ny + (1 << l) - 1) >> l);
}
uint64_t layer_density_bytes(int nx, int ny, int nlevels) {
    uint64_t b = 0;
    for (int l = 0; l < nlevels; ++l) { int ax, ay; level_dims(nx, ny, l, ax, ay); b += (uint64_t)ax * ay * 8; }
    return b;
}

int n_threads() {
    if (const char* e = std::getenv("LDB_THREADS")) { int t = std::atoi(e); if (t > 0) return t; }
    return std::max(1u, std::thread::hardware_concurrency());
}
// Dynamic parallel for over [0, n); fn(i) may throw nothing.
template <class F> void parallel_for(size_t n, F fn) {
    const int nt = (int)std::min<size_t>(n, (size_t)n_threads());
    if (nt <= 1) { for (size_t i = 0; i < n; ++i) fn(i); return; }
    std::atomic<size_t> next{ 0 };
    std::vector<std::thread> th;
    for (int t = 0; t < nt; ++t)
        th.emplace_back([&]() { for (size_t i; (i = next.fetch_add(1)) < n;) fn(i); });
    for (auto& t : th) t.join();
}

struct Mapped {
    const uint8_t* p = nullptr; size_t n = 0; uint64_t mtime_ns = 0;
    bool open(const char* path, std::string& err) {
        int fd = ::open(path, O_RDONLY);
        if (fd < 0) { err = std::string("cannot open ") + path; return false; }
        struct stat st; fstat(fd, &st);
        n = (size_t)st.st_size;
#ifdef __APPLE__
        const struct timespec& mt = st.st_mtimespec;    // macOS name of st_mtim
#else
        const struct timespec& mt = st.st_mtim;
#endif
        mtime_ns = (uint64_t)mt.tv_sec * 1000000000ull + (uint64_t)mt.tv_nsec;
        void* m = n ? mmap(nullptr, n, PROT_READ, MAP_SHARED, fd, 0) : nullptr;
        ::close(fd);
        if (n && m == MAP_FAILED) { err = std::string("cannot map ") + path; n = 0; return false; }
        p = (const uint8_t*)m;
        return true;
    }
    ~Mapped() { if (p) munmap((void*)p, n); }
};

// ---------------------------------------------------------------- modal snapshots
void put64(std::vector<uint8_t>& b, int64_t v) { const uint8_t* q = (const uint8_t*)&v; b.insert(b.end(), q, q + 8); }
int64_t get64(const uint8_t*& p) { int64_t v; std::memcpy(&v, p, 8); p += 8; return v; }
void put_pts(std::vector<uint8_t>& b, const std::vector<Pt>& v) {
    put64(b, (int64_t)v.size()); for (auto& q : v) { put64(b, q.x); put64(b, q.y); }
}
void get_pts(const uint8_t*& p, std::vector<Pt>& v) {
    v.resize((size_t)get64(p)); for (auto& q : v) { q.x = get64(p); q.y = get64(p); }
}
std::vector<uint8_t> save_modal(const Modal& m) {
    std::vector<uint8_t> b;
    for (int64_t v : { (int64_t)m.relative, m.place_x, m.place_y, m.geom_x, m.geom_y, m.text_x, m.text_y,
                       (int64_t)m.layer, (int64_t)m.datatype, (int64_t)m.textlayer, (int64_t)m.texttype,
                       m.geom_w, m.geom_h, m.half_width, m.ext_start, m.ext_end, (int64_t)m.ctrap_type, m.radius,
                       (int64_t)m.rep.regular, m.rep.cols, m.rep.rows, m.rep.dx, m.rep.dy, m.rep.rdx, m.rep.rdy,
                       (int64_t)m.place_cell.by_number, (int64_t)m.place_cell.number })
        put64(b, v);
    put_pts(b, m.rep.offs); put_pts(b, m.polygon_pts); put_pts(b, m.path_pts);
    put64(b, (int64_t)m.place_cell.name.size());
    b.insert(b.end(), m.place_cell.name.begin(), m.place_cell.name.end());
    while (b.size() % 8) b.push_back(0);
    return b;
}
void load_modal(const uint8_t* p, Modal& m) {
    m.relative = get64(p) != 0; m.place_x = get64(p); m.place_y = get64(p); m.geom_x = get64(p); m.geom_y = get64(p);
    m.text_x = get64(p); m.text_y = get64(p); m.layer = (uint64_t)get64(p); m.datatype = (uint64_t)get64(p);
    m.textlayer = (uint64_t)get64(p); m.texttype = (uint64_t)get64(p); m.geom_w = get64(p); m.geom_h = get64(p);
    m.half_width = get64(p); m.ext_start = get64(p); m.ext_end = get64(p); m.ctrap_type = (uint64_t)get64(p);
    m.radius = get64(p); m.rep.regular = get64(p) != 0; m.rep.cols = get64(p); m.rep.rows = get64(p);
    m.rep.dx = get64(p); m.rep.dy = get64(p); m.rep.rdx = get64(p); m.rep.rdy = get64(p);
    m.place_cell.by_number = get64(p) != 0; m.place_cell.number = (uint64_t)get64(p);
    get_pts(p, m.rep.offs); get_pts(p, m.polygon_pts); get_pts(p, m.path_pts);
    const size_t nl = (size_t)get64(p); m.place_cell.name.assign((const char*)p, nl);
}
// The true state after a unit that never read an undefined variable: its own definitions
// over the state it started from.
void overlay(Modal& st, const Modal& e, uint32_t def) {
    if (def & Parser::D_REL) st.relative = e.relative;
    if (def & Parser::D_GX) st.geom_x = e.geom_x;
    if (def & Parser::D_GY) st.geom_y = e.geom_y;
    if (def & Parser::D_PX) st.place_x = e.place_x;
    if (def & Parser::D_PY) st.place_y = e.place_y;
    if (def & Parser::D_LAYER) st.layer = e.layer;
    if (def & Parser::D_DT) st.datatype = e.datatype;
    if (def & Parser::D_GW) st.geom_w = e.geom_w;
    if (def & Parser::D_GH) st.geom_h = e.geom_h;
    if (def & Parser::D_HW) st.half_width = e.half_width;
    if (def & Parser::D_EXT) { st.ext_start = e.ext_start; st.ext_end = e.ext_end; }
    if (def & Parser::D_CT) st.ctrap_type = e.ctrap_type;
    if (def & Parser::D_RAD) st.radius = e.radius;
    if (def & Parser::D_PCELL) st.place_cell = e.place_cell;
    if (def & Parser::D_REP) st.rep = e.rep;
    if (def & Parser::D_PPTS) st.polygon_pts = e.polygon_pts;
    if (def & Parser::D_HPTS) st.path_pts = e.path_pts;
    // texts are not tracked (no geometry): take the latest values
    st.text_x = e.text_x; st.text_y = e.text_y; st.textlayer = e.textlayer; st.texttype = e.texttype;
}

// ---------------------------------------------------------------- unit decoding
struct Unit { uint64_t off = 0, len = 0; uint32_t kind = 0, flags = 0; bool in_cell = false; };

// Decode one unit into stream_cb. `start` = nullptr: speculative (track) unless a snapshot
// is given. Throws Parser::Dependent, StreamHier, ParseError.
struct UnitDecoder {
    const uint8_t* file;
    template <class Setup> void run(const Unit& u, Parser& ps, Setup setup) {
        LdbFile dummy;
        ps.f = &dummy;
        ps.cur = u.in_cell ? 0 : -1;
        if (u.flags & UF_AFTER_CELL) { ps.m.reset_for_cell(); ps.def = Parser::D_CELL_RESET; }
        setup(ps);
        Reader s(file + u.off, (size_t)u.len, "sidecar unit");
        if (u.kind == UNIT_CBLOCK) { s.u8(); ps.cblock(s); }
        else ps.parse(s);
    }
};

// ---------------------------------------------------------------- build
struct Grid {                       // base level of one layer's density
    std::vector<uint32_t> count;
    std::vector<uint64_t> area;
};

struct Builder {
    const uint8_t* file = nullptr; size_t size = 0;
    std::vector<Unit> units;
    // layer registry
    std::mutex mu;
    std::map<std::pair<uint64_t, uint64_t>, int> layer_ids;
    std::vector<std::pair<uint64_t, uint64_t>> layer_keys;
    std::vector<std::unique_ptr<Grid>> grids;        // sized up front (kMaxLayers) when density is on
    static constexpr int kMaxLayers = 65536;
    bool density = false;
    int64_t gx = 0, gy = 0, tile = 1; int nx = 1, ny = 1;

    int layer_index(uint64_t l, uint64_t d) {
        std::lock_guard<std::mutex> g(mu);
        auto it = layer_ids.find({ l, d });
        if (it != layer_ids.end()) return it->second;
        const int id = (int)layer_keys.size();
        if (id >= kMaxLayers) throw ParseError{ "too many layers for the sidecar" };
        layer_ids[{ l, d }] = id; layer_keys.push_back({ l, d });
        if (density) { grids[(size_t)id].reset(new Grid); grids[(size_t)id]->count.assign((size_t)nx * ny, 0);
                       grids[(size_t)id]->area.assign((size_t)nx * ny, 0); }
        return id;
    }
    void add_density(int li, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
        Grid& g = *grids[(size_t)li];
        auto ix = [&](int64_t x) { return (int)std::min<int64_t>(nx - 1, std::max<int64_t>(0, (x - gx) / tile)); };
        auto iy = [&](int64_t y) { return (int)std::min<int64_t>(ny - 1, std::max<int64_t>(0, (y - gy) / tile)); };
        const int i1 = ix(x1), i2 = ix(x2 > x1 ? x2 - 1 : x1), j1 = iy(y1), j2 = iy(y2 > y1 ? y2 - 1 : y1);
        const size_t c = (size_t)iy((y1 + y2) / 2) * nx + ix((x1 + x2) / 2);
        __atomic_fetch_add(&g.count[c], 1u, __ATOMIC_RELAXED);
        if (i1 == i2 && j1 == j2) {
            __atomic_fetch_add(&g.area[(size_t)j1 * nx + i1], (uint64_t)((x2 - x1) * (y2 - y1)), __ATOMIC_RELAXED);
            return;
        }
        for (int j = j1; j <= j2; ++j) {
            const int64_t ty1 = gy + (int64_t)j * tile, h = std::min(y2, ty1 + tile) - std::max(y1, ty1);
            if (h <= 0) continue;
            for (int i = i1; i <= i2; ++i) {
                const int64_t tx1 = gx + (int64_t)i * tile, w = std::min(x2, tx1 + tile) - std::max(x1, tx1);
                if (w > 0) __atomic_fetch_add(&g.area[(size_t)j * nx + i], (uint64_t)(w * h), __ATOMIC_RELAXED);
            }
        }
    }
};

struct Stat { int64_t count = 0; int64_t bbox[4] = { INT64_MAX, INT64_MAX, INT64_MIN, INT64_MIN }; };
struct UnitWork {
    std::map<int, Stat> stats;               // per layer index
    int64_t emitted = 0;
    bool dependent = false;
    Modal end; uint32_t def = 0;
    std::vector<uint8_t> snap;
};

// Box sink used while building: per-unit statistics and (optionally) density.
struct BuildSink {
    Builder* b; UnitWork* w; bool stats = true, dens = false;
    uint64_t last_l = UINT64_MAX, last_d = UINT64_MAX; int last_i = -1;
    static void cb(void* ud, const char*, int layer, int dt, int, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
        auto* s = (BuildSink*)ud;
        if ((uint64_t)layer != s->last_l || (uint64_t)dt != s->last_d) {
            s->last_l = (uint64_t)layer; s->last_d = (uint64_t)dt; s->last_i = s->b->layer_index(s->last_l, s->last_d);
        }
        if (s->stats) {
            Stat& st = s->w->stats[s->last_i];
            ++st.count; ++s->w->emitted;
            st.bbox[0] = std::min(st.bbox[0], x1); st.bbox[1] = std::min(st.bbox[1], y1);
            st.bbox[2] = std::max(st.bbox[2], x2); st.bbox[3] = std::max(st.bbox[3], y2);
        }
        if (s->dens) s->b->add_density(s->last_i, x1, y1, x2, y2);
    }
};

struct TopScan {
    std::vector<Unit>* units; size_t last_off = 0; uint8_t last_id = 255; bool open_inline = false;
    bool after_cell = false; long* cur = nullptr;
    void close_prev(size_t off) { if (!units->empty() && units->back().len == 0) units->back().len = off - units->back().off; }
    static void hook(void* ud, size_t off, uint8_t id) {
        auto* t = (TopScan*)ud;
        const size_t foff = off + 13;                      // reader starts after the magic
        const bool is_cell = id == 13 || id == 14;
        bool start = false;
        if (id == 34 || t->units->empty() || t->last_id == 34 || is_cell) start = true;
        else if (foff - t->units->back().off >= kInlineSplit) start = true;
        if (start) {
            t->close_prev(foff);
            Unit u; u.off = foff; u.kind = id == 34 ? UNIT_CBLOCK : UNIT_INLINE;
            u.flags = (t->last_id == 13 || t->last_id == 14) ? UF_AFTER_CELL : 0;
            u.in_cell = *t->cur >= 0 || is_cell;
            t->units->push_back(u);
        }
        t->last_id = id;
    }
};

} // namespace

// ============================================================================ C API
struct ldb_mvi {
    Mapped side, oas;
    const Hdr* h = nullptr;
    const LayerRec* layers = nullptr;
    const UnitRec* units = nullptr;
    const EntryRec* entries = nullptr;
    const uint8_t* snaps = nullptr;
    const uint8_t* dens = nullptr;
    std::string path;
};

static void set_err(char* e, size_t n, const std::string& m) { if (e && n) snprintf(e, n, "%s", m.c_str()); }

extern "C" int ldb_mvi_build(const char* oas_path, const char* mvi_path, int grid, char* err, size_t errn) {
    const double t0 = now_s();
    std::string es;
    Mapped f;
    if (!f.open(oas_path, es)) { set_err(err, errn, es); return -1; }
    if (f.n < 13 || std::memcmp(f.p, "%SEMI-OASIS\r\n", 13) != 0) { set_err(err, errn, "not an OASIS file"); return -1; }

    Builder b; b.file = f.p; b.size = f.n;
    // Unit and bbox from the strict tables when present.
    double unit = 0.0; bool have_bbox = false; int64_t bb[4] = { 0, 0, 0, 0 };
    if (ldb_oas_info_t* info = ldb_oas_info_open(oas_path, nullptr, 0)) {
        unit = ldb_oas_info_unit(info);
        char top[512] = { 0 };
        const int has_top = ldb_oas_info_top(info, top, sizeof top);
        const int nc = ldb_oas_info_n_cells(info);
        for (int i = 0; i < nc; ++i) {
            char nm[512]; int64_t off; int fl; int64_t box[4];
            ldb_oas_info_cell(info, i, nm, sizeof nm, &off, &fl, box);
            if (fl >= 0 && (fl & 6) == 0 && ((has_top > 0 && std::strcmp(nm, top) == 0) || nc == 1)) {
                have_bbox = true; std::memcpy(bb, box, sizeof bb);
            }
        }
        ldb_oas_info_close(info);
    }
    if (grid <= 0) grid = 1024;
    auto setup_grid = [&](const int64_t* box) {
        const int64_t w = std::max<int64_t>(1, box[2] - box[0]), h = std::max<int64_t>(1, box[3] - box[1]);
        b.tile = std::max<int64_t>(1, (std::max(w, h) + grid - 1) / grid);
        b.nx = (int)((w + b.tile - 1) / b.tile); b.ny = (int)((h + b.tile - 1) / b.tile);
        b.gx = box[0]; b.gy = box[1];
        b.grids.resize(Builder::kMaxLayers);
        b.density = true;
    };
    if (have_bbox) setup_grid(bb);

    // 1. top-level scan
    try {
        Parser scan; LdbFile dummy; scan.f = &dummy; scan.scan_only = true;
        TopScan ts; ts.units = &b.units; ts.cur = &scan.cur;
        scan.top_hook = &TopScan::hook; scan.top_hook_ud = &ts;
        Reader s(f.p + 13, f.n - 13, "file (sidecar scan)");
        scan.parse(s);
        ts.close_prev(13 + s.off);
        if (!b.units.empty() && b.units.back().len == 0) b.units.back().len = f.n - b.units.back().off;
    } catch (const ParseError& e) { set_err(err, errn, "OASIS scan: " + e.msg); return -1; }
    const double t_scan = now_s() - t0;

    // 2. speculative parallel decode
    const size_t nu = b.units.size();
    std::vector<UnitWork> work(nu);
    std::atomic<int> hier{ 0 }; std::mutex emu; std::string perr;
    UnitDecoder dec{ f.p };
    parallel_for(nu, [&](size_t i) {
        UnitWork& w = work[i];
        BuildSink sink{ &b, &w, true, b.density };
        Parser ps;
        try {
            dec.run(b.units[i], ps, [&](Parser& p) { p.track = true; p.stream_cb = &BuildSink::cb; p.stream_ud = &sink; });
            w.end = ps.m; w.def = ps.def;
        } catch (const Parser::Dependent&) { w.dependent = true;
        } catch (const StreamHier&) { hier = 1;
        } catch (const ParseError& e) { std::lock_guard<std::mutex> g(emu); if (perr.empty()) perr = e.msg; }
    });
    if (hier) { set_err(err, errn, "hierarchical OASIS (PLACEMENT): the sidecar supports flat files"); return 2; }
    if (!perr.empty()) { set_err(err, errn, "OASIS decode: " + perr); return -1; }
    const double t_spec = now_s() - t0 - t_scan;

    // 3. true start states; finish dependent units
    int64_t ndep = 0;
    {
        Modal st;
        for (size_t i = 0; i < nu; ++i) {
            const Unit& u = b.units[i];
            if (u.flags & UF_AFTER_CELL) st.reset_for_cell();
            UnitWork& w = work[i];
            if (!w.dependent) { overlay(st, w.end, w.def); continue; }
            ++ndep;
            w.snap = save_modal(st);
            BuildSink sink{ &b, &w, true, b.density };
            Parser ps;
            try {
                Unit uu = u; uu.flags &= ~UF_AFTER_CELL;    // the snapshot already holds it
                dec.run(uu, ps, [&](Parser& p) { p.m = st; p.stream_cb = &BuildSink::cb; p.stream_ud = &sink;
                                                 p.skip = (size_t)w.emitted; });
            } catch (const StreamHier&) { set_err(err, errn, "hierarchical OASIS (PLACEMENT): the sidecar supports flat files"); return 2;
            } catch (const ParseError& e) { set_err(err, errn, "OASIS decode: " + e.msg); return -1; }
            st = ps.m;
        }
    }
    // stats emitted twice? no: the re-decode skipped what the speculative pass had counted,
    // but its sink counted the skipped ones neither (add_box drops them before the callback).

    // global bbox / layers
    const int nl = (int)b.layer_keys.size();
    std::vector<LayerRec> lrec((size_t)nl);
    for (int l = 0; l < nl; ++l) {
        lrec[(size_t)l] = LayerRec{ (int64_t)b.layer_keys[(size_t)l].first, (int64_t)b.layer_keys[(size_t)l].second, 0,
                                    { INT64_MAX, INT64_MAX, INT64_MIN, INT64_MIN }, 0 };
    }
    int64_t total = 0, gbb[4] = { INT64_MAX, INT64_MAX, INT64_MIN, INT64_MIN };
    std::vector<UnitRec> urec(nu);
    std::vector<EntryRec> erec;
    uint64_t snap_bytes = 0;
    for (size_t i = 0; i < nu; ++i) {
        UnitRec& r = urec[i];
        r = UnitRec{ b.units[i].off, b.units[i].len, b.units[i].kind, b.units[i].flags, 0, 0, erec.size(), 0, 0, 0 };
        if (b.units[i].in_cell) r.in_cell = 1;
        if (!work[i].snap.empty()) { r.flags |= UF_SNAPSHOT; r.snap_off = snap_bytes; r.snap_len = work[i].snap.size(); snap_bytes += r.snap_len; }
        for (auto& kv : work[i].stats) {
            EntryRec e{ kv.first, 0, kv.second.count, { kv.second.bbox[0], kv.second.bbox[1], kv.second.bbox[2], kv.second.bbox[3] } };
            erec.push_back(e);
            LayerRec& L = lrec[(size_t)kv.first];
            L.count += e.count;
            for (int k = 0; k < 2; ++k) { L.bbox[k] = std::min(L.bbox[k], e.bbox[k]); L.bbox[k + 2] = std::max(L.bbox[k + 2], e.bbox[k + 2]);
                                          gbb[k] = std::min(gbb[k], e.bbox[k]); gbb[k + 2] = std::max(gbb[k + 2], e.bbox[k + 2]); }
            r.boxes += e.count; total += e.count;
        }
        r.entry_count = (uint32_t)(erec.size() - r.entry_first);
    }
    if (total == 0) { for (auto& v : gbb) v = 0; }

    // 4. density by a second pass when the bbox was not known
    if (!b.density && total > 0) {
        setup_grid(gbb);
        for (int l = 0; l < nl; ++l) {
            b.grids[(size_t)l].reset(new Grid);
            b.grids[(size_t)l]->count.assign((size_t)b.nx * b.ny, 0); b.grids[(size_t)l]->area.assign((size_t)b.nx * b.ny, 0);
        }
        std::atomic<int> bad{ 0 };
        parallel_for(nu, [&](size_t i) {
            UnitWork dummy;
            BuildSink sink{ &b, &dummy, false, true };
            Parser ps;
            try {
                Unit u = b.units[i];
                if (!work[i].snap.empty()) {
                    u.flags &= ~UF_AFTER_CELL;
                    dec.run(u, ps, [&](Parser& p) { load_modal(work[i].snap.data(), p.m); p.stream_cb = &BuildSink::cb; p.stream_ud = &sink; });
                } else {
                    dec.run(u, ps, [&](Parser& p) { p.stream_cb = &BuildSink::cb; p.stream_ud = &sink; });
                }
            } catch (...) { bad = 1; }
        });
        if (bad) { set_err(err, errn, "OASIS decode failed in the density pass"); return -1; }
    }
    if (!b.density) { b.nx = b.ny = 1; b.tile = 1; }

    // levels
    int nlev = 1;
    { int ax = b.nx, ay = b.ny; while (ax > 1 || ay > 1) { ax = (ax + 1) / 2; ay = (ay + 1) / 2; ++nlev; } }
    const uint64_t ldb = layer_density_bytes(b.nx, b.ny, nlev);

    // write
    Hdr H; std::memset(&H, 0, sizeof H);
    std::memcpy(H.magic, kMagic, 8);
    H.oas_size = f.n; H.oas_mtime_ns = f.mtime_ns; H.unit = unit;
    for (int k = 0; k < 4; ++k) H.bbox[k] = have_bbox ? bb[k] : gbb[k];
    H.tile = b.tile; H.nx = b.nx; H.ny = b.ny; H.nlevels = nlev; H.nlayers = nl;
    H.nunits = (int64_t)nu; H.nentries = (int64_t)erec.size();
    H.flags = (have_bbox ? FF_STRICT_BBOX : 0) | (ndep == 0 ? FF_ALL_INDEPENDENT : 0);
    H.total_boxes = total; H.n_dependent = ndep;
    auto align8 = [](uint64_t v) { return (v + 7) & ~7ull; };
    H.off_layers = align8(sizeof H);
    H.off_units = align8(H.off_layers + sizeof(LayerRec) * (uint64_t)nl);
    H.off_entries = align8(H.off_units + sizeof(UnitRec) * nu);
    H.off_snap = align8(H.off_entries + sizeof(EntryRec) * erec.size());
    H.snap_bytes = snap_bytes;
    H.off_density = align8(H.off_snap + snap_bytes);
    H.density_bytes = b.density ? ldb * (uint64_t)nl : 0;
    for (int l = 0; l < nl; ++l) lrec[(size_t)l].density_off = b.density ? ldb * (uint64_t)l : 0;
    H.build_s = now_s() - t0;

    const std::string tmp = std::string(mvi_path) + ".tmp" + std::to_string((long)getpid());
    FILE* o = std::fopen(tmp.c_str(), "wb");
    if (!o) { set_err(err, errn, "cannot write " + tmp); return -1; }
    auto pad_to = [&](uint64_t off) { static const char z[8] = { 0 }; long cur = std::ftell(o); if ((uint64_t)cur < off) std::fwrite(z, 1, (size_t)(off - (uint64_t)cur), o); };
    std::fwrite(&H, sizeof H, 1, o);
    pad_to(H.off_layers); if (nl) std::fwrite(lrec.data(), sizeof(LayerRec), (size_t)nl, o);
    pad_to(H.off_units); if (nu) std::fwrite(urec.data(), sizeof(UnitRec), nu, o);
    pad_to(H.off_entries); if (!erec.empty()) std::fwrite(erec.data(), sizeof(EntryRec), erec.size(), o);
    pad_to(H.off_snap);
    for (size_t i = 0; i < nu; ++i) if (!work[i].snap.empty()) std::fwrite(work[i].snap.data(), 1, work[i].snap.size(), o);
    pad_to(H.off_density);
    if (b.density) {
        const double tarea = (double)b.tile * (double)b.tile;
        for (int l = 0; l < nl; ++l) {
            Grid& g = *b.grids[(size_t)l];
            std::vector<uint32_t> cnt(g.count);
            std::vector<double> ar(g.area.begin(), g.area.end());
            int ax = b.nx, ay = b.ny;
            for (int lev = 0; lev < nlev; ++lev) {
                const double ta = tarea * (double)(1ull << (2 * lev));
                std::vector<float> cov(cnt.size());
                for (size_t k = 0; k < cnt.size(); ++k) cov[k] = (float)(ar[k] / ta);
                std::fwrite(cnt.data(), 4, cnt.size(), o);
                std::fwrite(cov.data(), 4, cov.size(), o);
                if (lev + 1 < nlev) {                      // 2x2 reduction
                    const int bx = (ax + 1) / 2, by = (ay + 1) / 2;
                    std::vector<uint32_t> c2((size_t)bx * by, 0); std::vector<double> a2((size_t)bx * by, 0.0);
                    for (int j = 0; j < ay; ++j)
                        for (int i = 0; i < ax; ++i) {
                            const size_t s = (size_t)j * ax + i, d = (size_t)(j / 2) * bx + (size_t)(i / 2);
                            c2[d] += cnt[s]; a2[d] += ar[s];
                        }
                    cnt.swap(c2); ar.swap(a2); ax = bx; ay = by;
                }
            }
        }
    }
    const bool ok = std::fflush(o) == 0 && !std::ferror(o);
    std::fclose(o);
    if (!ok || std::rename(tmp.c_str(), mvi_path) != 0) { std::remove(tmp.c_str()); set_err(err, errn, std::string("cannot write ") + mvi_path); return -1; }
    char msg[256];
    snprintf(msg, sizeof msg, "MVI units=%zu dependent=%lld layers=%d boxes=%lld scan=%.2fs decode=%.2fs total=%.2fs",
             nu, (long long)ndep, nl, (long long)total, t_scan, t_spec, now_s() - t0);
    set_err(err, errn, msg);
    return 0;
}

extern "C" ldb_mvi_t* ldb_mvi_open(const char* oas_path, const char* mvi_path, char* err, size_t errn) {
    std::unique_ptr<ldb_mvi> m(new ldb_mvi);
    std::string es;
    if (!m->oas.open(oas_path, es)) { set_err(err, errn, es); return nullptr; }
    if (!m->side.open(mvi_path, es)) { set_err(err, errn, es); return nullptr; }
    if (m->side.n < sizeof(Hdr)) { set_err(err, errn, "sidecar too small"); return nullptr; }
    m->h = (const Hdr*)m->side.p;
    const Hdr& H = *m->h;
    if (std::memcmp(H.magic, kMagic, 8) != 0) { set_err(err, errn, "not a sidecar (or another version)"); return nullptr; }
    if (H.oas_size != m->oas.n || H.oas_mtime_ns != m->oas.mtime_ns) { set_err(err, errn, "sidecar is stale"); return nullptr; }
    if (H.off_density + H.density_bytes > m->side.n) { set_err(err, errn, "sidecar truncated"); return nullptr; }
    m->layers = (const LayerRec*)(m->side.p + H.off_layers);
    m->units = (const UnitRec*)(m->side.p + H.off_units);
    m->entries = (const EntryRec*)(m->side.p + H.off_entries);
    m->snaps = m->side.p + H.off_snap;
    m->dens = m->side.p + H.off_density;
    m->path = mvi_path;
    return m.release();
}

static std::string default_mvi_path(const char* oas) {
    std::string p = std::string(oas) + ".mvi";
    const char* cd = std::getenv("MAGVIEWER_CACHE_DIR");
    if (!cd || !*cd) {
        std::string dir = p.substr(0, p.find_last_of('/') == std::string::npos ? 0 : p.find_last_of('/') + 1);
        if (access(dir.empty() ? "." : dir.c_str(), W_OK) == 0) return p;
        cd = std::getenv("TMPDIR"); if (!cd || !*cd) cd = "/tmp";
    }
    std::string base = p.substr(p.find_last_of('/') == std::string::npos ? 0 : p.find_last_of('/') + 1);
    return std::string(cd) + "/" + base;
}

extern "C" ldb_mvi_t* ldb_mvi_open_or_build(const char* oas_path, const char* mvi_path, int grid, int* built,
                                            char* err, size_t errn) {
    const std::string mp = mvi_path && *mvi_path ? std::string(mvi_path) : default_mvi_path(oas_path);
    if (built) *built = 0;
    if (ldb_mvi_t* m = ldb_mvi_open(oas_path, mp.c_str(), nullptr, 0)) return m;
    const int rc = ldb_mvi_build(oas_path, mp.c_str(), grid, err, errn);
    if (rc != 0) return nullptr;
    if (built) *built = 1;
    char msg[512] = { 0 };
    if (err && errn) snprintf(msg, sizeof msg, "%s", err);
    ldb_mvi_t* m = ldb_mvi_open(oas_path, mp.c_str(), err, errn);
    if (m && err && errn) snprintf(err, errn, "%s", msg);
    return m;
}

extern "C" void ldb_mvi_close(ldb_mvi_t* m) { delete m; }

extern "C" const char* ldb_mvi_path(const ldb_mvi_t* m) { return m ? m->path.c_str() : ""; }

extern "C" int ldb_mvi_info(const ldb_mvi_t* m, double* unit, int64_t* bbox, int64_t* tile, int* nx, int* ny,
                            int* nlevels, int* nlayers, int64_t* nunits, int64_t* total_boxes, int64_t* n_dependent,
                            double* build_s) {
    if (!m) return -1;
    const Hdr& H = *m->h;
    if (unit) *unit = H.unit;
    if (bbox) for (int k = 0; k < 4; ++k) bbox[k] = H.bbox[k];
    if (tile) *tile = H.tile;
    if (nx) *nx = H.nx;
    if (ny) *ny = H.ny;
    if (nlevels) *nlevels = H.nlevels;
    if (nlayers) *nlayers = H.nlayers;
    if (nunits) *nunits = H.nunits;
    if (total_boxes) *total_boxes = H.total_boxes;
    if (n_dependent) *n_dependent = H.n_dependent;
    if (build_s) *build_s = H.build_s;
    return 0;
}

extern "C" int ldb_mvi_layer(const ldb_mvi_t* m, int i, int64_t* layer, int64_t* datatype, int64_t* count, int64_t* bbox) {
    if (!m || i < 0 || i >= m->h->nlayers) return -1;
    const LayerRec& L = m->layers[i];
    if (layer) *layer = L.layer;
    if (datatype) *datatype = L.datatype;
    if (count) *count = L.count;
    if (bbox) for (int k = 0; k < 4; ++k) bbox[k] = L.bbox[k];
    return 0;
}

extern "C" int ldb_mvi_density(const ldb_mvi_t* m, int i, int level, int* ax, int* ay,
                               const uint32_t** counts, const float** coverage) {
    if (!m || i < 0 || i >= m->h->nlayers || level < 0 || level >= m->h->nlevels || m->h->density_bytes == 0) return -1;
    const Hdr& H = *m->h;
    uint64_t off = m->layers[i].density_off;
    for (int l = 0; l < level; ++l) { int bx, by; level_dims(H.nx, H.ny, l, bx, by); off += (uint64_t)bx * by * 8; }
    int bx, by; level_dims(H.nx, H.ny, level, bx, by);
    if (ax) *ax = bx;
    if (ay) *ay = by;
    if (counts) *counts = (const uint32_t*)(m->dens + off);
    if (coverage) *coverage = (const float*)(m->dens + off + (uint64_t)bx * by * 4);
    return 0;
}

static bool meets(const int64_t* b, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
    return b[0] <= x2 && b[2] >= x1 && b[1] <= y2 && b[3] >= y1;
}

// Units holding boxes of layer i (i < 0: any layer) that meet the window. Returns the number
// found; ids are written up to cap (0-based unit indices, file order).
extern "C" int64_t ldb_mvi_units_in(const ldb_mvi_t* m, int i, int64_t x1, int64_t y1, int64_t x2, int64_t y2,
                                    int64_t* ids, int64_t cap) {
    if (!m) return -1;
    int64_t n = 0;
    for (int64_t u = 0; u < m->h->nunits; ++u) {
        const UnitRec& U = m->units[u];
        for (uint32_t k = 0; k < U.entry_count; ++k) {
            const EntryRec& e = m->entries[U.entry_first + k];
            if ((i < 0 || e.layer == i) && meets(e.bbox, x1, y1, x2, y2)) {
                if (n < cap && ids) ids[n] = u;
                ++n;
                break;
            }
        }
    }
    return n;
}

extern "C" int64_t ldb_mvi_unit_boxes(const ldb_mvi_t* m, int64_t u, int i, int64_t* bbox) {
    if (!m || u < 0 || u >= m->h->nunits) return -1;
    const UnitRec& U = m->units[u];
    int64_t n = 0;
    if (bbox) { bbox[0] = bbox[1] = INT64_MAX; bbox[2] = bbox[3] = INT64_MIN; }
    for (uint32_t k = 0; k < U.entry_count; ++k) {
        const EntryRec& e = m->entries[U.entry_first + k];
        if (i < 0 || e.layer == i) {
            n += e.count;
            if (bbox) for (int q = 0; q < 2; ++q) { bbox[q] = std::min(bbox[q], e.bbox[q]); bbox[q + 2] = std::max(bbox[q + 2], e.bbox[q + 2]); }
        }
    }
    return n;
}

namespace {
struct DecodeSink {
    int want = -1; uint64_t last_l = UINT64_MAX, last_d = UINT64_MAX; bool last_ok = false;
    const ldb_mvi* m = nullptr; int32_t* out = nullptr; int64_t cap = 0, n = 0; bool clipped = false, overflow = false;
    static void cb(void* ud, const char*, int layer, int dt, int, int64_t x1, int64_t y1, int64_t x2, int64_t y2) {
        auto* s = (DecodeSink*)ud;
        if ((uint64_t)layer != s->last_l || (uint64_t)dt != s->last_d) {
            s->last_l = (uint64_t)layer; s->last_d = (uint64_t)dt;
            s->last_ok = s->want < 0 || (s->m->layers[s->want].layer == (int64_t)layer && s->m->layers[s->want].datatype == (int64_t)dt);
        }
        if (!s->last_ok) return;
        if (s->n >= s->cap) { s->overflow = true; return; }
        auto c = [&](int64_t v) -> int32_t {
            if (v > INT32_MAX) { s->clipped = true; return INT32_MAX; }
            if (v < INT32_MIN) { s->clipped = true; return INT32_MIN; }
            return (int32_t)v;
        };
        int32_t* o = s->out + 4 * s->n++;
        o[0] = c(x1); o[1] = c(y1); o[2] = c(x2); o[3] = c(y2);
    }
};
} // namespace

// Decode units[0..n) in parallel and write the boxes of layer i (i < 0: all layers) as
// int32 x1,y1,x2,y2 into xyxy, unit after unit in the order given. cap = capacity in boxes.
// Returns the number of boxes, or -(needed) if cap is too small, or INT64_MIN on error.
// unit_off (optional, n+1 entries) receives each unit's first box index.
extern "C" int64_t ldb_mvi_decode(const ldb_mvi_t* m, const int64_t* units, int64_t n, int i, int32_t* xyxy, int64_t cap,
                                  int64_t* unit_off, char* err, size_t errn) {
    if (!m) return INT64_MIN;
    std::vector<int64_t> off((size_t)n + 1, 0);
    for (int64_t k = 0; k < n; ++k) {
        const int64_t c = ldb_mvi_unit_boxes(m, units[k], i, nullptr);
        if (c < 0) { set_err(err, errn, "bad unit index"); return INT64_MIN; }
        off[(size_t)k + 1] = off[(size_t)k] + c;
    }
    if (unit_off) for (int64_t k = 0; k <= n; ++k) unit_off[k] = off[(size_t)k];
    if (off[(size_t)n] > cap) return -off[(size_t)n];
    std::atomic<int> bad{ 0 }; std::mutex emu; std::string first;
    UnitDecoder dec{ m->oas.p };
    parallel_for((size_t)n, [&](size_t k) {
        const UnitRec& U = m->units[units[k]];
        Unit u; u.off = U.file_off; u.len = U.len; u.kind = U.kind; u.flags = U.flags; u.in_cell = U.in_cell != 0;
        DecodeSink sink; sink.want = i;
        sink.m = m; sink.out = xyxy + 4 * off[k]; sink.cap = off[k + 1] - off[k];
        Parser ps;
        try {
            if (U.flags & UF_SNAPSHOT) {
                u.flags &= ~UF_AFTER_CELL;
                dec.run(u, ps, [&](Parser& p) { load_modal(m->snaps + U.snap_off, p.m); p.stream_cb = &DecodeSink::cb; p.stream_ud = &sink; });
            } else {
                dec.run(u, ps, [&](Parser& p) { p.stream_cb = &DecodeSink::cb; p.stream_ud = &sink; });
            }
            if (sink.n != sink.cap || sink.overflow) { std::lock_guard<std::mutex> g(emu); bad = 1;
                if (first.empty()) first = "unit " + std::to_string(units[k]) + " decoded " + std::to_string(sink.n) +
                                           " boxes, sidecar says " + std::to_string(sink.cap); }
            if (sink.clipped) { std::lock_guard<std::mutex> g(emu); if (first.empty()) first = "coordinates beyond int32 were clipped"; }
        } catch (const ParseError& e) { std::lock_guard<std::mutex> g(emu); bad = 1; if (first.empty()) first = e.msg;
        } catch (...) { std::lock_guard<std::mutex> g(emu); bad = 1; if (first.empty()) first = "unit decode failed"; }
    });
    if (bad) { set_err(err, errn, first); return INT64_MIN; }
    if (!first.empty()) set_err(err, errn, first);
    return off[(size_t)n];
}
