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
//   --no-rep          do not look for rectangle arrays (repetitions)
//   --rep-min N       smallest array written as a repetition (default 8 rectangles)
//   --no-crc          no CRC32 validation signature in END
//   --no-strict       no name tables, no table offsets, no S_* properties
//
// Encoding: the streaming writer of oasis_writer.hpp (standard SEMI P39, readable by KLayout
// and src/db/oasis.cpp):
//   * boxes of one layer are sorted by shape (w,h), then row (y1) and x1 (per spatial bin with
//     --bin); RectArrays turns runs of equal pitch in a row, and identical runs stacked at a
//     constant row pitch, into RECTANGLE repetitions (types 1, 2, 3); the other boxes are
//     RECTANGLEs with modal layer/datatype/width/height and XYRELATIVE deltas;
//   * records are wrapped in deflate CBLOCKs; every CBLOCK holds one layer and is independent
//     (it starts with XYABSOLUTE and fully specified records), for parallel and random-access
//     readers (db_mvi);
//   * strict mode: START offset-flag 1, table offsets in END; CELL by reference number; strict
//     CELLNAME table with S_CELL_OFFSET and S_BOUNDING_BOX; PROPNAME and PROPSTRING tables; a
//     strict LAYERNAME table (names from the .mag sections, e.g. "li1" for 67/20); file
//     properties S_TOP_CELL and S_BOUNDING_BOXES_AVAILABLE; CRC32 in END.
// Memory: one input layer's boxes at a time (16 B/box), the open repetition stacks and one
// CBLOCK buffer; tiles are emitted as translated copies of the same sorted array.
#include "oasis_writer.hpp"
#include <algorithm>
#include <chrono>
#include <climits>
#include <parallel/algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <memory>
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
                    "               [--tile NXxNY] [--tile-gap G] [--bin B] [--no-rep] [--rep-min N] [--no-crc] [--no-strict]\n"
                    "               in.mag out.oas\n"
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

} // namespace

int main(int argc, char** argv) {
    uint64_t grid = 1000;
    bool dedup = false;
    size_t cblock_mb = 8;
    std::string cell = "TOP";
    size_t largest = 0;
    int tnx = 1, tny = 1;
    int64_t tile_gap = 0, bin = 0;
    bool strict = true, reps = true, crc = true;
    int64_t rep_min = 8;
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
        else if (a == "--no-rep") reps = false;
        else if (a == "--rep-min" && i + 1 < argc) rep_min = atoll(argv[++i]);
        else if (a == "--no-crc") crc = false;
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
    if (tnx * tny > 1 || bin > 0) {
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

    oasis_writer::Options wo;
    wo.unit = (double)grid;
    wo.strict = strict;
    wo.cblock_bytes = cblock_mb << 20;
    wo.independent_cblocks = true;
    wo.validation = crc ? 1 : 0;
    oasis_writer::Writer w;
    if (!w.open(out_path, wo)) { fprintf(stderr, "%s\n", w.error().c_str()); return 1; }
    if (strict) {
        std::vector<std::pair<int, int>> seen;
        for (const Input& in : inputs) {
            if (std::find(seen.begin(), seen.end(), std::make_pair(in.layer, in.datatype)) != seen.end()) continue;
            seen.push_back({ in.layer, in.datatype });
            const std::string nm = in.name.empty() ? std::to_string(in.layer) + "/" + std::to_string(in.datatype) : in.name;
            w.layer_name(nm, (uint64_t)in.layer, (uint64_t)in.datatype);
        }
    }
    w.add_top_cell(cell);
    w.begin_cell(cell);
    uint64_t total = 0, total_in = 0, arrays = 0;
    auto t0 = std::chrono::steady_clock::now();
    for (const Input& in : inputs) {
        std::vector<Box> v = load_bin(in.path);
        const size_t n = v.size();
        total_in += n * (size_t)tnx * (size_t)tny;
        auto shape_row = [](const Box& a, const Box& b) {
            int64_t wa = (int64_t)a.x2 - a.x1, wb = (int64_t)b.x2 - b.x1;
            if (wa != wb) return wa < wb;
            int64_t ha = (int64_t)a.y2 - a.y1, hb = (int64_t)b.y2 - b.y1;
            if (ha != hb) return ha < hb;
            if (a.y1 != b.y1) return a.y1 < b.y1;
            return a.x1 < b.x1;
        };
        std::vector<uint64_t> binkey;                    // --bin: Morton bin of each box (kept with v)
        if (bin > 0) {
            auto key = [&](const Box& b) {
                return morton2((uint32_t)((b.x1 - bx1) / bin), (uint32_t)((b.y1 - by1) / bin));
            };
            __gnu_parallel::sort(v.begin(), v.end(), [&](const Box& a, const Box& b) {
                uint64_t ka = key(a), kb = key(b);
                if (ka != kb) return ka < kb;
                return shape_row(a, b);
            }, __gnu_parallel::balanced_quicksort_tag());
            binkey.resize(v.size());
            for (size_t k = 0; k < v.size(); ++k) binkey[k] = key(v[k]);
        } else {
            __gnu_parallel::sort(v.begin(), v.end(), shape_row, __gnu_parallel::balanced_quicksort_tag());
        }
        if (dedup) {
            size_t o = 0;
            for (size_t k = 0; k < v.size(); ++k) {
                if (o > 0 && v[k].x1 == v[o - 1].x1 && v[k].y1 == v[o - 1].y1 && v[k].x2 == v[o - 1].x2 && v[k].y2 == v[o - 1].y2) continue;
                if (!binkey.empty()) binkey[o] = binkey[k];
                v[o++] = v[k];
            }
            v.resize(o); if (!binkey.empty()) binkey.resize(o);
        }
        size_t nbad = 0;
        for (const Box& b : v) if (b.x2 < b.x1 || b.y2 < b.y1) ++nbad;
        for (int ty = 0; ty < tny; ++ty) {
            for (int k = 0; k < tnx; ++k) {
                const int tx = (ty % 2 == 0) ? k : tnx - 1 - k;          // serpentine tile order
                const int64_t ox = (int64_t)tx * px, oy = (int64_t)ty * py;
                if (reps) {
                    // one finder per tile (and per spatial bin): its input must be sorted by (w,h,y,x)
                    std::unique_ptr<oasis_writer::RectArrays> ra;
                    uint64_t cur_bin = ~0ull;
                    for (size_t q = 0; q < v.size(); ++q) {
                        const Box& b = v[q];
                        if (b.x2 < b.x1 || b.y2 < b.y1) continue;
                        if (!ra || (!binkey.empty() && binkey[q] != cur_bin)) {
                            if (ra) { ra->finish(); arrays += ra->arrays(); }
                            ra.reset(new oasis_writer::RectArrays(w, (uint64_t)in.layer, (uint64_t)in.datatype, 64, rep_min));
                            if (!binkey.empty()) cur_bin = binkey[q];
                        }
                        ra->add(b.x1 + ox, b.y1 + oy, (int64_t)b.x2 - b.x1, (int64_t)b.y2 - b.y1);
                    }
                    if (ra) { ra->finish(); arrays += ra->arrays(); }
                } else {
                    for (const Box& b : v) {
                        if (b.x2 < b.x1 || b.y2 < b.y1) continue;
                        w.rectangle((uint64_t)in.layer, (uint64_t)in.datatype, b.x1 + ox, b.y1 + oy, (int64_t)b.x2 - b.x1, (int64_t)b.y2 - b.y1);
                    }
                }
                if (!w.ok()) { fprintf(stderr, "%s\n", w.error().c_str()); return 2; }
            }
        }
        w.flush_cblock();                                    // CBLOCKs never span layers
        total += (v.size() - nbad) * (size_t)tnx * (size_t)tny;
        double dt = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
        fprintf(stderr, "L%d_D%d %s: %zu boxes x %d tiles (%zu dup removed, %zu invalid)  arrays so far %llu  %.1f s\n",
                in.layer, in.datatype, in.name.c_str(), v.size() - nbad, tnx * tny, dedup ? n - v.size() : (size_t)0, nbad,
                (unsigned long long)arrays, dt);
    }
    w.end_cell();
    if (!w.close()) { fprintf(stderr, "%s\n", w.error().c_str()); return 2; }
    const auto& st = w.stats();
    fprintf(stderr, "wrote %s: %llu boxes (%llu in), %.1f MB (%.3f B/box), %llu RECTANGLE records (%llu repetitions "
                    "covering %llu boxes), %llu CBLOCKs, raw records %.1f MB%s\n",
            out_path.c_str(), (unsigned long long)total, (unsigned long long)total_in, st.bytes / 1e6,
            (double)st.bytes / std::max<uint64_t>(total, 1), (unsigned long long)st.rectangles,
            (unsigned long long)st.repetitions, (unsigned long long)st.shapes_in_repetitions,
            (unsigned long long)st.cblocks, st.record_bytes / 1e6, strict ? ", strict" : "");
    return 0;
}
