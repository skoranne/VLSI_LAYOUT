// bin2oas: convert a KLBIN layer set (Magic .mag whose sections point at KLBIN files) into
// one flat, compact, STRICT-mode OASIS file.
//
//   bin2oas [opts] in.mag out.oas
//   bin2oas [opts] out.oas L:D file.bin [L:D file.bin ...]
//
//   --grid N          grid steps per micron in START (default 1000)
//   --dedup           drop exact duplicate boxes
//   --cblock-mb M     CBLOCK size, uncompressed (default 8)
//   --cell NAME       cell name (default TOP)
//   --largest K       keep only the K largest input layers (by box count), in input order
//   --tile NXxNY      place the layer set NX x NY times (flat copies), e.g. --tile 2x2
//   --tile-gap G      gap between tiles in database units (default 0)
//   --bin B           spatial order: bins of B x B database units in Morton order, each bin
//                     sorted by shape then row (0 = whole layer sorted by shape then row)
//   --no-strict       old layout: no name tables, no table offsets, no S_* properties
//
// Encoding (all standard SEMI P39, readable by KLayout and src/db/oasis.cpp):
//   * RECTANGLE records with modal layer/datatype/width/height and XYRELATIVE deltas; boxes of
//     one layer are sorted by shape (w,h), then row (y1) and x1 (per spatial bin with --bin),
//     so a typical record is 3-5 bytes before compression;
//   * records are wrapped in deflate CBLOCKs. Every CBLOCK holds one layer and starts with
//     XYABSOLUTE and a fully specified RECTANGLE, so it decodes without the modal state of the
//     previous CBLOCK (independent CBLOCKs, for parallel and random-access readers);
//   * strict mode: START offset-flag 1, table offsets in END; CELL by reference number; a
//     strict CELLNAME table after the geometry whose entry carries S_CELL_OFFSET (byte offset
//     of the CELL record) and S_BOUNDING_BOX (flags 0, x, y, w, h); a strict LAYERNAME table
//     (names from the .mag sections, e.g. "li1" for 67/20); file property S_TOP_CELL.
// Memory: one input layer's boxes at a time (16 B/box) plus one CBLOCK buffer; tiles are
// emitted as translated copies of the same sorted array.
#include "oasis_codec.hpp"
#include <zlib.h>
#include <algorithm>
#include <chrono>
#include <climits>
#include <parallel/algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <regex>
#include <string>
#include <vector>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

namespace {

struct Box { int32_t x1, y1, x2, y2; };

struct Input { int layer, datatype; std::string path, name; size_t nbox = 0; };

struct Out {
    FILE* fp = nullptr;
    oasis_codec::Encoder blk;        // records of the current CBLOCK
    size_t cblock_bytes = 8u << 20;
    uint64_t raw = 0, written = 0, ncblock = 0;
    void put(const oasis_codec::Encoder& e) {
        fwrite(e.b.data(), 1, e.b.size(), fp);
        written += e.b.size();
    }
    void flush_cblock() {
        if (blk.b.empty()) return;
        std::vector<uint8_t> z(compressBound(blk.b.size()) + 64);
        z_stream zs{};
        if (deflateInit2(&zs, 6, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY) != Z_OK) { fprintf(stderr, "deflateInit2 failed\n"); exit(2); }
        zs.next_in = blk.b.data(); zs.avail_in = (uInt)blk.b.size();
        zs.next_out = z.data(); zs.avail_out = (uInt)z.size();
        if (deflate(&zs, Z_FINISH) != Z_STREAM_END) { fprintf(stderr, "deflate failed\n"); exit(2); }
        size_t zn = zs.total_out;
        deflateEnd(&zs);
        oasis_codec::Encoder h;
        h.u8(34); h.uint_(0); h.uint_(blk.b.size()); h.uint_(zn);   // CBLOCK, comp-type 0 = deflate
        put(h);
        fwrite(z.data(), 1, zn, fp);
        written += zn; raw += blk.b.size(); ++ncblock;
        blk.b.clear();
    }
    void maybe_flush() { if (blk.b.size() >= cblock_bytes) flush_cblock(); }
};

bool parse_ld_from_name(const std::string& p, int& l, int& d) {
    static const std::regex re("L([0-9]+)_D([0-9]+)[^/]*\\.bin$");
    std::smatch m;
    if (!std::regex_search(p, m, re)) return false;
    l = std::stoi(m[1]); d = std::stoi(m[2]);
    return true;
}

std::vector<Input> read_mag(const std::string& mag) {
    std::ifstream in(mag);
    if (!in) { fprintf(stderr, "cannot open %s\n", mag.c_str()); exit(1); }
    std::vector<Input> v;
    std::string line, section;
    while (std::getline(in, line)) {
        if (line.rfind("<< ", 0) == 0) {
            size_t e = line.find(" >>");
            section = e == std::string::npos ? "" : line.substr(3, e - 3);
            continue;
        }
        if (line.rfind("KLBIN ", 0) != 0) continue;
        std::string p = line.substr(6);
        while (!p.empty() && (p.back() == ' ' || p.back() == '\r')) p.pop_back();
        Input x; x.path = p;
        if (!parse_ld_from_name(p, x.layer, x.datatype)) { fprintf(stderr, "skip (no L<l>_D<d> in name): %s\n", p.c_str()); continue; }
        static const std::regex generic("L[0-9]+_D[0-9]+");
        if (!section.empty() && section != "end" && !std::regex_match(section, generic)) x.name = section;
        v.push_back(x);
    }
    return v;
}

void usage() {
    fprintf(stderr, "usage: bin2oas [--grid N] [--dedup] [--cblock-mb M] [--cell NAME] [--largest K]\n"
                    "               [--tile NXxNY] [--tile-gap G] [--bin B] [--no-strict] in.mag out.oas\n"
                    "       bin2oas [opts] out.oas L:D file.bin [L:D file.bin ...]\n");
    exit(1);
}

uint64_t morton2(uint32_t x, uint32_t y) {
    auto spread = [](uint64_t v) {
        v &= 0xffffffffull;
        v = (v | (v << 16)) & 0x0000ffff0000ffffull; v = (v | (v << 8)) & 0x00ff00ff00ff00ffull;
        v = (v | (v << 4)) & 0x0f0f0f0f0f0f0f0full; v = (v | (v << 2)) & 0x3333333333333333ull;
        v = (v | (v << 1)) & 0x5555555555555555ull; return v;
    };
    return spread(x) | (spread(y) << 1);
}

std::vector<Box> load_bin(const std::string& path) {
    int fd = open(path.c_str(), O_RDONLY);
    if (fd < 0) { fprintf(stderr, "cannot open %s\n", path.c_str()); exit(1); }
    struct stat st; fstat(fd, &st);
    size_t n = (size_t)st.st_size / sizeof(Box);
    std::vector<Box> v(n);
    if (n) {
        void* m = mmap(nullptr, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
        if (m == MAP_FAILED) { perror("mmap"); exit(1); }
        memcpy(v.data(), m, n * sizeof(Box));
        munmap(m, st.st_size);
    }
    close(fd);
    return v;
}

// PROPERTY record with an inline standard-property name (info: U count, C=1, N=0, S=1).
void std_prop_head(oasis_codec::Encoder& e, const char* name, unsigned nval) {
    e.u8(28); e.u8(uint8_t((nval << 4) | 0x04 | 0x01)); e.str(name);
}

} // namespace

int main(int argc, char** argv) {
    uint64_t grid = 1000;
    bool dedup = false;
    size_t cblock_mb = 8;
    std::string cell = "TOP";
    size_t largest = 0;
    int tnx = 1, tny = 1;
    int64_t tile_gap = 0, bin = 0;
    bool strict = true;
    std::vector<std::string> pos;
    for (int i = 1; i < argc; ++i) {
        std::string a = argv[i];
        if (a == "--grid" && i + 1 < argc) grid = std::stoull(argv[++i]);
        else if (a == "--dedup") dedup = true;
        else if (a == "--cblock-mb" && i + 1 < argc) cblock_mb = std::stoull(argv[++i]);
        else if (a == "--cell" && i + 1 < argc) cell = argv[++i];
        else if (a == "--largest" && i + 1 < argc) largest = std::stoull(argv[++i]);
        else if (a == "--tile" && i + 1 < argc) { if (sscanf(argv[++i], "%dx%d", &tnx, &tny) != 2 || tnx < 1 || tny < 1) usage(); }
        else if (a == "--tile-gap" && i + 1 < argc) tile_gap = std::stoll(argv[++i]);
        else if (a == "--bin" && i + 1 < argc) bin = std::stoll(argv[++i]);
        else if (a == "--no-strict") strict = false;
        else if (a.rfind("--", 0) == 0) usage();
        else pos.push_back(a);
    }
    std::vector<Input> inputs;
    std::string out_path;
    if (pos.size() == 2 && pos[0].size() > 4 && pos[0].substr(pos[0].size() - 4) == ".mag") {
        inputs = read_mag(pos[0]); out_path = pos[1];
    } else if (pos.size() >= 3 && pos.size() % 2 == 1) {
        out_path = pos[0];
        for (size_t i = 1; i < pos.size(); i += 2) {
            Input x; x.path = pos[i + 1];
            if (sscanf(pos[i].c_str(), "%d:%d", &x.layer, &x.datatype) != 2) usage();
            inputs.push_back(x);
        }
    } else usage();

    // Box counts, optional --largest selection, and the bounding box of the layer set.
    for (Input& in : inputs) {
        struct stat st;
        if (stat(in.path.c_str(), &st) != 0) { fprintf(stderr, "cannot stat %s\n", in.path.c_str()); return 1; }
        in.nbox = (size_t)st.st_size / sizeof(Box);
    }
    inputs.erase(std::remove_if(inputs.begin(), inputs.end(), [](const Input& x) { return x.nbox == 0; }), inputs.end());
    if (largest > 0 && largest < inputs.size()) {
        std::vector<size_t> ord(inputs.size());
        for (size_t i = 0; i < ord.size(); ++i) ord[i] = i;
        std::stable_sort(ord.begin(), ord.end(), [&](size_t a, size_t b) { return inputs[a].nbox > inputs[b].nbox; });
        std::vector<bool> keep(inputs.size(), false);
        for (size_t i = 0; i < largest; ++i) keep[ord[i]] = true;
        std::vector<Input> sel;
        for (size_t i = 0; i < inputs.size(); ++i) if (keep[i]) sel.push_back(inputs[i]);
        inputs.swap(sel);
    }
    int64_t bx1 = INT64_MAX, by1 = INT64_MAX, bx2 = INT64_MIN, by2 = INT64_MIN;
    if (tnx * tny > 1 || bin > 0 || strict) {
        for (const Input& in : inputs) {
            std::vector<Box> v = load_bin(in.path);
            int64_t a1 = INT64_MAX, b1 = INT64_MAX, a2 = INT64_MIN, b2 = INT64_MIN;
            #pragma omp parallel for reduction(min:a1,b1) reduction(max:a2,b2)
            for (size_t k = 0; k < v.size(); ++k) {
                if (v[k].x2 < v[k].x1 || v[k].y2 < v[k].y1) continue;
                a1 = std::min<int64_t>(a1, v[k].x1); b1 = std::min<int64_t>(b1, v[k].y1);
                a2 = std::max<int64_t>(a2, v[k].x2); b2 = std::max<int64_t>(b2, v[k].y2);
            }
            bx1 = std::min(bx1, a1); by1 = std::min(by1, b1); bx2 = std::max(bx2, a2); by2 = std::max(by2, b2);
        }
    }
    const int64_t px = (bx2 - bx1) + tile_gap, py = (by2 - by1) + tile_gap;
    if (tnx * tny > 1) {
        const int64_t ex = (int64_t)(tnx - 1) * px + bx2, ey = (int64_t)(tny - 1) * py + by2;
        if (ex > INT32_MAX || ey > INT32_MAX) { fprintf(stderr, "tiled extent exceeds 32-bit coordinates\n"); return 1; }
        fprintf(stderr, "tiling %dx%d, pitch %lld x %lld\n", tnx, tny, (long long)px, (long long)py);
    }

    Out o;
    o.cblock_bytes = cblock_mb << 20;
    o.fp = fopen(out_path.c_str(), "wb");
    if (!o.fp) { fprintf(stderr, "cannot write %s\n", out_path.c_str()); return 1; }
    uint64_t cell_offset = 0;
    {
        oasis_codec::Encoder e;
        e.bytes("%SEMI-OASIS\r\n", 13);
        e.u8(1); e.str("1.0"); e.real_whole(grid);       // START
        if (strict) {
            e.uint_(1);                                  // offset-flag 1: table offsets in END
            std_prop_head(e, "S_TOP_CELL", 1); e.uint_(12); e.str(cell);
        } else {
            e.uint_(0); for (int i = 0; i < 12; ++i) e.uint_(0);
        }
        o.put(e);
        cell_offset = o.written;
        oasis_codec::Encoder c;
        if (strict) { c.u8(13); c.uint_(0); }            // CELL by reference number 0
        else { c.u8(14); c.str(cell); }                  // CELL by name
        c.u8(16);                                        // XYRELATIVE
        o.put(c);
    }
    uint64_t total = 0, total_in = 0;
    int64_t mx = 0, my = 0;            // modal geometry-x/y: 0 after CELL
    auto t0 = std::chrono::steady_clock::now();
    for (const Input& in : inputs) {
        std::vector<Box> v = load_bin(in.path);
        const size_t n = v.size();
        total_in += n * (size_t)tnx * (size_t)tny;
        if (bin > 0) {
            auto key = [&](const Box& b) {
                return morton2((uint32_t)((b.x1 - bx1) / bin), (uint32_t)((b.y1 - by1) / bin));
            };
            __gnu_parallel::sort(v.begin(), v.end(), [&](const Box& a, const Box& b) {
                uint64_t ka = key(a), kb = key(b);
                if (ka != kb) return ka < kb;
                int64_t wa = (int64_t)a.x2 - a.x1, wb = (int64_t)b.x2 - b.x1;
                if (wa != wb) return wa < wb;
                int64_t ha = (int64_t)a.y2 - a.y1, hb = (int64_t)b.y2 - b.y1;
                if (ha != hb) return ha < hb;
                if (a.y1 != b.y1) return a.y1 < b.y1;
                return a.x1 < b.x1;
            }, __gnu_parallel::balanced_quicksort_tag());
        } else {
            __gnu_parallel::sort(v.begin(), v.end(), [](const Box& a, const Box& b) {
                int64_t wa = (int64_t)a.x2 - a.x1, wb = (int64_t)b.x2 - b.x1;
                if (wa != wb) return wa < wb;
                int64_t ha = (int64_t)a.y2 - a.y1, hb = (int64_t)b.y2 - b.y1;
                if (ha != hb) return ha < hb;
                if (a.y1 != b.y1) return a.y1 < b.y1;
                return a.x1 < b.x1;
            }, __gnu_parallel::balanced_quicksort_tag());
        }
        if (dedup) v.erase(std::unique(v.begin(), v.end(), [](const Box& a, const Box& b) {
            return a.x1 == b.x1 && a.y1 == b.y1 && a.x2 == b.x2 && a.y2 == b.y2; }), v.end());
        int64_t mw = -1, mh = -1;
        size_t nbad = 0;
        for (int ty = 0; ty < tny; ++ty) {
            for (int k = 0; k < tnx; ++k) {
                const int tx = (ty % 2 == 0) ? k : tnx - 1 - k;          // serpentine tile order
                const int64_t ox = (int64_t)tx * px, oy = (int64_t)ty * py;
                for (const Box& b : v) {
                    int64_t w = (int64_t)b.x2 - b.x1, h = (int64_t)b.y2 - b.y1;
                    if (w < 0 || h < 0) { if (tx == 0 && ty == 0) ++nbad; continue; }
                    const int64_t x = b.x1 + ox, y = b.y1 + oy;
                    const bool fresh = o.blk.b.empty();              // new CBLOCK: state everything
                    uint8_t info = 0;
                    if (fresh) { info |= 0x03 | 0x40 | 0x20 | 0x10 | 0x08; o.blk.u8(15); }   // XYABSOLUTE
                    if (w != mw) info |= 0x40;
                    if (h != mh) info |= 0x20;
                    if (x != mx) info |= 0x10;
                    if (y != my) info |= 0x08;
                    o.blk.u8(20); o.blk.u8(info);
                    if (info & 0x01) o.blk.uint_((uint64_t)in.layer);
                    if (info & 0x02) o.blk.uint_((uint64_t)in.datatype);
                    if (info & 0x40) o.blk.uint_((uint64_t)w);
                    if (info & 0x20) o.blk.uint_((uint64_t)h);
                    if (fresh) {
                        if (info & 0x10) o.blk.sint(x);
                        if (info & 0x08) o.blk.sint(y);
                        o.blk.u8(16);                                    // back to XYRELATIVE
                    } else {
                        if (info & 0x10) o.blk.sint(x - mx);
                        if (info & 0x08) o.blk.sint(y - my);
                    }
                    mw = w; mh = h; mx = x; my = y;
                    o.maybe_flush();
                }
            }
        }
        o.flush_cblock();                                    // CBLOCKs never span layers
        total += (v.size() - nbad) * (size_t)tnx * (size_t)tny;
        double dt = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
        fprintf(stderr, "L%d_D%d %s: %zu boxes x %d tiles (%zu dup removed, %zu invalid)  out so far %.1f MB  %.1f s\n",
                in.layer, in.datatype, in.name.c_str(), v.size() - nbad, tnx * tny, dedup ? n - v.size() : (size_t)0, nbad,
                o.written / 1e6, dt);
    }
    o.flush_cblock();
    uint64_t cellname_off = 0, layername_off = 0;
    if (strict) {
        oasis_codec::Encoder t;
        cellname_off = o.written;
        t.u8(4); t.str(cell); t.uint_(0);                    // CELLNAME with explicit reference 0
        std_prop_head(t, "S_CELL_OFFSET", 1); t.uint_(8); t.uint_(cell_offset);
        const int64_t ux2 = (int64_t)(tnx - 1) * px + bx2, uy2 = (int64_t)(tny - 1) * py + by2;
        std_prop_head(t, "S_BOUNDING_BOX", 5);
        t.uint_(8); t.uint_(total ? 0 : 1);                  // flags: bit0 = empty cell
        t.uint_(9); t.sint(total ? bx1 : 0); t.uint_(9); t.sint(total ? by1 : 0);
        t.uint_(8); t.uint_(total ? (uint64_t)(ux2 - bx1) : 0); t.uint_(8); t.uint_(total ? (uint64_t)(uy2 - by1) : 0);
        o.put(t);
        t.b.clear();
        bool any = false;
        std::vector<std::pair<int, int>> seen;
        for (const Input& in : inputs) {
            std::string nm = in.name.empty() ? std::to_string(in.layer) + "/" + std::to_string(in.datatype) : in.name;
            if (std::find(seen.begin(), seen.end(), std::make_pair(in.layer, in.datatype)) != seen.end()) continue;
            seen.push_back({ in.layer, in.datatype });
            if (!any) { layername_off = o.written; any = true; }
            t.u8(11); t.str(nm);                             // LAYERNAME (geometry), exact layer/datatype
            t.uint_(3); t.uint_((uint64_t)in.layer); t.uint_(3); t.uint_((uint64_t)in.datatype);
        }
        o.put(t);
    }
    {
        oasis_codec::Encoder e;                              // END: exactly 256 bytes
        e.u8(2);
        if (strict) {
            const uint64_t offs[6] = { cellname_off, 0, 0, 0, layername_off, 0 };   // CELLNAME TEXTSTRING PROPNAME PROPSTRING LAYERNAME XNAME
            for (uint64_t off : offs) { e.uint_(1); e.uint_(off); }
        }
        size_t used = e.b.size() + 1;                        // + validation scheme (uint 0)
        size_t pad = 256 - used - 2;                         // 2-byte length prefix when pad >= 128
        if (pad < 128) pad = 256 - used - 1;
        e.str(std::string(pad, '\0'));
        e.uint_(0);
        if (e.b.size() != 256) { fprintf(stderr, "internal: END record is %zu bytes\n", e.b.size()); return 2; }
        o.put(e);
    }
    fclose(o.fp);
    fprintf(stderr, "wrote %s: %llu boxes (%llu in), %.1f MB (%.2f B/box), %llu CBLOCKs, raw records %.1f MB%s\n",
            out_path.c_str(), (unsigned long long)total, (unsigned long long)total_in, o.written / 1e6,
            (double)o.written / std::max<uint64_t>(total, 1), (unsigned long long)o.ncblock, o.raw / 1e6,
            strict ? ", strict" : "");
    return 0;
}
