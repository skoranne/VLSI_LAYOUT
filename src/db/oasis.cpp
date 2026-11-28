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
#include "oasis_parser.hpp"
#include <cstdlib>

using namespace oasis_parse;

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
    // LDB_OPEN_SHAPES: exact polygons/paths/texts next to the fractured boxes (lossless
    // re-encoding, ldb_iterate_polygons); not in low-memory mode.
    ps.keep_polys = (f->open_flags & LDB_OPEN_SHAPES) != 0 && (f->open_flags & LDB_OPEN_LOW_MEMORY) == 0;
    ps.keep_texts = (f->open_flags & LDB_OPEN_LOW_MEMORY) == 0;
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
