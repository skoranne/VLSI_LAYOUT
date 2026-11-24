#pragma once
// Streaming OASIS (SEMI P39-1105) writer.
//
// Records go to the file as they are produced: the writer holds one CBLOCK buffer, the
// name tables (names only) and a few words per cell and per placement, never the
// geometry. Everything it writes is standard OASIS 1.0:
//
//   * compaction through the modal variables (layer, datatype, geometry-w/h, positions
//     in XYRELATIVE mode, placement-cell, text-string, repetition, point lists, path
//     half-width/extensions, circle radius, last property name/values, record 29);
//   * repetitions: the caller passes them (Repetition), they are normalised and written
//     in the shortest form (types 0-11); RectArrays finds them in a sorted rectangle stream;
//   * point lists in the shortest form: implicit Manhattan (0/1), Manhattan (2),
//     octangular (3), all-angle (4) or double-delta (5); squares use the S bit;
//   * CBLOCK (raw DEFLATE, RFC 1951) of cblock_bytes uncompressed; CELL, START and END
//     are never compressed (35.4); independent_cblocks makes every CBLOCK decodable without
//     the modal state of the previous one (for parallel and random-access readers);
//   * strict mode: START offset-flag 1 and the six table offsets in END; CELLNAME,
//     TEXTSTRING, PROPNAME, PROPSTRING and LAYERNAME tables after the cells, each in its own
//     CBLOCK starting at the table's first record (13.9); every cell, text string, property
//     name and property string is referenced by number (13.8);
//   * standard properties: S_TOP_CELL and S_BOUNDING_BOXES_AVAILABLE after START,
//     S_CELL_OFFSET and S_BOUNDING_BOX (hierarchical, computed at close) per CELLNAME;
//   * END padded to exactly 256 bytes, with CRC32 or CHECKSUM32 validation (14.4, 14.5).
//
// Usage: open, [layer_name / file_property / add_top_cell], then per cell begin_cell,
// elements (each optionally followed by property()), end_cell; finally close.
#include "fracture.hpp"
#include "oasis_codec.hpp"
#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <map>
#include <queue>
#include <string>
#include <unordered_map>
#include <vector>

namespace oasis_writer {

using ldbgeom::Pt;

struct Options {
    double unit = 1000.0;              // grid steps per micron (START unit)
    bool strict = true;                // strict name tables, everything by reference number
    size_t cblock_bytes = 4u << 20;    // uncompressed CBLOCK size; 0 = no compression
    int deflate_level = 6;
    bool independent_cblocks = false;  // reset the modal state at every CBLOCK start
    int validation = 1;                // 0 none, 1 CRC32, 2 CHECKSUM32
    bool cell_bboxes = true;           // S_BOUNDING_BOX per cell (strict mode)
    size_t header_reserve = 1024;      // bytes reserved after START for S_TOP_CELL when the top
                                       // cells are only known at close (0 = do not write it)
};

// A repetition as a lattice (cols x rows, column step (dx,dy), row step (rdx,rdy)) or as an
// explicit list of offsets (irregular, offs[0] = (0,0) need not hold: any set of offsets).
struct Repetition {
    bool regular = true;
    int64_t cols = 1, rows = 1, dx = 0, dy = 0, rdx = 0, rdy = 0;
    std::vector<Pt> offs;
    size_t count() const { return regular ? (size_t)(cols * rows) : offs.size(); }
    static Repetition lattice(int64_t cols, int64_t dx, int64_t dy, int64_t rows = 1, int64_t rdx = 0, int64_t rdy = 0) {
        Repetition r; r.cols = cols; r.dx = dx; r.dy = dy; r.rows = rows; r.rdx = rdx; r.rdy = rdy; return r;
    }
};

struct PropValue {
    enum Kind : uint8_t { UINT, SINT, REAL, STRING } kind = STRING;
    uint64_t u = 0; int64_t i = 0; double r = 0; std::string s;
    static PropValue uint_(uint64_t v) { PropValue p; p.kind = UINT; p.u = v; return p; }
    static PropValue sint(int64_t v) { PropValue p; p.kind = SINT; p.i = v; return p; }
    static PropValue real(double v) { PropValue p; p.kind = REAL; p.r = v; return p; }
    static PropValue str(const std::string& v) { PropValue p; p.kind = STRING; p.s = v; return p; }
    bool operator==(const PropValue& o) const {
        return kind == o.kind && u == o.u && i == o.i && (r == o.r || (r != r && o.r != o.r)) && s == o.s;
    }
};

// Placement transform (23.1): reflect about x if mirror, then rotate counterclockwise by
// angle degrees, then scale by mag, then translate by (x, y).
struct Transform { int64_t x = 0, y = 0; double mag = 1.0, angle = 0.0; bool mirror = false; };

struct Stats {
    uint64_t bytes = 0, record_bytes = 0, cblocks = 0;
    uint64_t cells = 0, rectangles = 0, polygons = 0, paths = 0, circles = 0, texts = 0;
    uint64_t placements = 0, properties = 0, repetitions = 0, shapes_in_repetitions = 0;
    uint64_t degenerate = 0;           // polygons with < 3 distinct vertices, dropped
    uint64_t top_cells = 0;
};

class Writer {
public:
    Writer();
    ~Writer();
    Writer(const Writer&) = delete;
    Writer& operator=(const Writer&) = delete;

    bool open(const std::string& path, const Options& opt = Options());
    bool ok() const { return err_.empty(); }
    const std::string& error() const { return err_; }
    const Stats& stats() const { return st_; }

    // Before the first cell.
    void add_top_cell(const std::string& name);     // S_TOP_CELL (else: unplaced cells, at close)
    void file_property(const std::string& name, const std::vector<PropValue>& v, bool standard = false);
    // Any time before close.
    void layer_name(const std::string& name, uint64_t layer, uint64_t datatype, bool text = false);

    void begin_cell(const std::string& name);
    void end_cell();

    void rectangle(uint64_t layer, uint64_t datatype, int64_t x, int64_t y, int64_t w, int64_t h,
                   const Repetition* rep = nullptr);
    // Closed polygon; the closing vertex may be repeated. Rectangles become RECTANGLE.
    void polygon(uint64_t layer, uint64_t datatype, const Pt* pts, size_t n, const Repetition* rep = nullptr);
    // Path centreline with half-width and start/end extensions (database units).
    void path(uint64_t layer, uint64_t datatype, const Pt* pts, size_t n, int64_t half_width,
              int64_t ext_start, int64_t ext_end, const Repetition* rep = nullptr);
    void circle(uint64_t layer, uint64_t datatype, int64_t x, int64_t y, int64_t radius, const Repetition* rep = nullptr);
    void text(uint64_t textlayer, uint64_t texttype, int64_t x, int64_t y, const std::string& s,
              const Repetition* rep = nullptr);
    void placement(const std::string& cell, const Transform& t, const Repetition* rep = nullptr);
    // PROPERTY for the record written last (file after START, cell after begin_cell, or element).
    void property(const std::string& name, const std::vector<PropValue>& v, bool standard = false);

    void flush_cblock();               // end the current CBLOCK (e.g. at a layer change)
    bool close();                      // tables, END, header patch; false on any error

    struct Impl;                       // opaque
private:
    Impl* d_;
    std::string err_;
    Stats st_;
};

// Repetition finder for rectangles of one layer/datatype. Feed rectangles sorted by
// (w, h, y, x); each row of equal (w, h, y) is split into runs of constant x pitch, and
// identical runs (same x0, count, pitch) in rows of constant y pitch are stacked into
// 2-D arrays (types 1, 2, 3). Arrays of fewer than min_elems rectangles are written as single
// RECTANGLEs (short repetitions cost more than modal rectangles once deflated). Output keeps
// (y, x) order within a shape, so XYRELATIVE deltas stay small. Streaming: memory is the
// current row, the open stacks (closed once they cannot grow, or when no row has extended
// them within window = window_factor * max(w, h)) and the rectangles waiting behind a stack
// that may still be expanded (at most one window of rows).
class RectArrays {
public:
    RectArrays(Writer& w, uint64_t layer, uint64_t datatype, int64_t window_factor = 64, int64_t min_elems = 8)
        : w_(w), layer_(layer), dt_(datatype), wf_(window_factor), me_(std::max<int64_t>(2, min_elems)) {}
    ~RectArrays() { finish(); }
    void add(int64_t x, int64_t y, int64_t w, int64_t h);
    void finish();
    uint64_t arrays() const { return arrays_; }
private:
    struct Key { int64_t x0, n, p; bool operator==(const Key& o) const { return x0 == o.x0 && n == o.n && p == o.p; } };
    struct KeyHash { size_t operator()(const Key& k) const {
        uint64_t h = (uint64_t)k.x0 * 0x9E3779B97F4A7C15ull ^ (uint64_t)k.n * 0xC2B2AE3D27D4EB4Full ^ (uint64_t)k.p * 0x165667B19E3779F9ull;
        return (size_t)(h ^ (h >> 29)); } };
    struct Stack { int64_t y0, py, ny, last_y; uint64_t gen; };
    struct Due { int64_t expiry; Key k; uint64_t gen; bool operator<(const Due& o) const { return expiry > o.expiry; } };
    struct Item { int64_t y, x, n, p, ny, py; };
    void row_done();
    void close_stack(const Key& k, const Stack& s);
    void close_all();
    void unblock(const Stack& s);
    void flush(int64_t limit);
    Writer& w_;
    uint64_t layer_, dt_;
    int64_t wf_, me_;
    std::map<int64_t, int> blocking_;   // y0 of stacks with < min_elems rectangles -> count
    std::vector<Item> out_;
    bool have_ = false;
    int64_t cw_ = 0, ch_ = 0, cy_ = 0;
    std::vector<int64_t> row_;
    std::unordered_map<Key, Stack, KeyHash> open_;
    std::priority_queue<Due> due_;
    uint64_t gen_ = 0, arrays_ = 0;
};

// The point list (7.7) the writer emits for a closed polygon given from its start vertex
// (no repeated or straight-through vertices); for tests.
std::vector<uint8_t> encode_point_list_for_test(const std::vector<Pt>& polygon);

} // namespace oasis_writer
