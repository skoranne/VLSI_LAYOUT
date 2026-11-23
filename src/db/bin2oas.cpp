// bin2oas: convert a KLBIN layer set (Magic .mag whose sections point at KLBIN files) into
// one flat, compact OASIS file.
//
//   bin2oas [--grid N] [--dedup] [--cblock-mb M] [--cell NAME] in.mag out.oas
//   bin2oas [...] out.oas L:D file.bin [L:D file.bin ...]
//
// Layer/datatype come from the KLBIN file name (..._L<layer>_D<datatype>.bin) or the L:D arg.
// Encoding (all standard SEMI P39, readable by KLayout and src/db/oasis.cpp):
//   * boxes of one layer are sorted by shape (w,h), then by row (y1) and x1, so that within a
//     shape run RECTANGLE records carry only the changed coordinate: modal layer/datatype/
//     width/height and XYRELATIVE mode make a typical record 3-5 bytes before compression;
//   * the record stream is wrapped in deflate CBLOCKs (default 8 MB uncompressed each).
// Memory: one layer's boxes at a time (16 B/box) plus one CBLOCK buffer.
#include "oasis_codec.hpp"
#include <zlib.h>
#include <algorithm>
#include <chrono>
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

struct Input { int layer, datatype; std::string path; };

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
    std::string line;
    while (std::getline(in, line)) {
        if (line.rfind("KLBIN ", 0) != 0) continue;
        std::string p = line.substr(6);
        while (!p.empty() && (p.back() == ' ' || p.back() == '\r')) p.pop_back();
        Input x; x.path = p;
        if (!parse_ld_from_name(p, x.layer, x.datatype)) { fprintf(stderr, "skip (no L<l>_D<d> in name): %s\n", p.c_str()); continue; }
        v.push_back(x);
    }
    return v;
}

void usage() {
    fprintf(stderr, "usage: bin2oas [--grid N] [--dedup] [--cblock-mb M] [--cell NAME] in.mag out.oas\n"
                    "       bin2oas [opts] out.oas L:D file.bin [L:D file.bin ...]\n");
    exit(1);
}

} // namespace

int main(int argc, char** argv) {
    uint64_t grid = 1000;
    bool dedup = false;
    size_t cblock_mb = 8;
    std::string cell = "TOP";
    std::vector<std::string> pos;
    for (int i = 1; i < argc; ++i) {
        std::string a = argv[i];
        if (a == "--grid" && i + 1 < argc) grid = std::stoull(argv[++i]);
        else if (a == "--dedup") dedup = true;
        else if (a == "--cblock-mb" && i + 1 < argc) cblock_mb = std::stoull(argv[++i]);
        else if (a == "--cell" && i + 1 < argc) cell = argv[++i];
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

    Out o;
    o.cblock_bytes = cblock_mb << 20;
    o.fp = fopen(out_path.c_str(), "wb");
    if (!o.fp) { fprintf(stderr, "cannot write %s\n", out_path.c_str()); return 1; }
    {
        oasis_codec::Encoder e;
        e.bytes("%SEMI-OASIS\r\n", 13);
        e.u8(1); e.str("1.0"); e.real_whole(grid);       // START
        e.uint_(0); for (int i = 0; i < 12; ++i) e.uint_(0);
        e.u8(14); e.str(cell);                           // CELL by name
        e.u8(16);                                        // XYRELATIVE
        o.put(e);
    }
    uint64_t total = 0, total_in = 0;
    int64_t mx = 0, my = 0;            // modal geometry-x/y: 0 after CELL, persist across layers
    auto t0 = std::chrono::steady_clock::now();
    for (const Input& in : inputs) {
        int fd = open(in.path.c_str(), O_RDONLY);
        if (fd < 0) { fprintf(stderr, "cannot open %s\n", in.path.c_str()); return 1; }
        struct stat st; fstat(fd, &st);
        size_t n = (size_t)st.st_size / sizeof(Box);
        std::vector<Box> v(n);
        if (n) {
            void* m = mmap(nullptr, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
            if (m == MAP_FAILED) { perror("mmap"); return 1; }
            memcpy(v.data(), m, n * sizeof(Box));
            munmap(m, st.st_size);
        }
        close(fd);
        total_in += n;
        std::sort(v.begin(), v.end(), [](const Box& a, const Box& b) {
            int64_t wa = (int64_t)a.x2 - a.x1, wb = (int64_t)b.x2 - b.x1;
            if (wa != wb) return wa < wb;
            int64_t ha = (int64_t)a.y2 - a.y1, hb = (int64_t)b.y2 - b.y1;
            if (ha != hb) return ha < hb;
            if (a.y1 != b.y1) return a.y1 < b.y1;
            return a.x1 < b.x1;
        });
        if (dedup) v.erase(std::unique(v.begin(), v.end(), [](const Box& a, const Box& b) {
            return a.x1 == b.x1 && a.y1 == b.y1 && a.x2 == b.x2 && a.y2 == b.y2; }), v.end());
        // Modal state is reset for every layer: first record states everything.
        bool first = true;
        int64_t mw = -1, mh = -1;
        size_t nbad = 0;
        for (const Box& b : v) {
            int64_t w = (int64_t)b.x2 - b.x1, h = (int64_t)b.y2 - b.y1;
            if (w < 0 || h < 0) { ++nbad; continue; }
            uint8_t info = 0;
            if (first) info |= 0x03;                     // D L
            if (w != mw) info |= 0x40;
            if (h != mh) info |= 0x20;
            if (b.x1 != mx) info |= 0x10;
            if (b.y1 != my) info |= 0x08;
            o.blk.u8(20); o.blk.u8(info);
            if (info & 0x01) o.blk.uint_((uint64_t)in.layer);
            if (info & 0x02) o.blk.uint_((uint64_t)in.datatype);
            if (info & 0x40) o.blk.uint_((uint64_t)w);
            if (info & 0x20) o.blk.uint_((uint64_t)h);
            // XYRELATIVE: deltas from the previous modal x/y (also across layer changes).
            if (info & 0x10) o.blk.sint((int64_t)b.x1 - mx);
            if (info & 0x08) o.blk.sint((int64_t)b.y1 - my);
            mw = w; mh = h; mx = b.x1; my = b.y1;
            first = false;
            o.maybe_flush();
        }
        total += v.size() - nbad;
        double dt = std::chrono::duration<double>(std::chrono::steady_clock::now() - t0).count();
        fprintf(stderr, "L%d_D%d: %zu boxes (%zu dup removed, %zu invalid)  out so far %.1f MB  %.1f s\n",
                in.layer, in.datatype, v.size() - nbad, (size_t)0 + (dedup ? n - v.size() : 0), nbad,
                (o.written + o.blk.b.size()) / 1e6, dt);
    }
    o.flush_cblock();
    {
        oasis_codec::Encoder e;
        e.u8(2); e.str(std::string(252, '\0')); e.uint_(0);   // END
        o.put(e);
    }
    fclose(o.fp);
    fprintf(stderr, "wrote %s: %llu boxes (%llu in), %.1f MB (%.2f B/box), %llu CBLOCKs, raw records %.1f MB\n",
            out_path.c_str(), (unsigned long long)total, (unsigned long long)total_in, o.written / 1e6,
            (double)o.written / std::max<uint64_t>(total, 1), (unsigned long long)o.ncblock, o.raw / 1e6);
    return 0;
}
