#include "flat_xform.hpp"
#include <unordered_set>
#include "layoutdb_internal.hpp"
#include "compact_boxes.hpp"
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstring>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <functional>
#include <string>
#include <vector>

static bool map_file(LdbFile* f, const char* path, char* errbuf, size_t errbuf_sz) {
    f->fd = ::open(path, O_RDONLY);
    if (f->fd < 0) { snprintf(errbuf, errbuf_sz, "open: %s", strerror(errno)); return false; }
    struct stat st{};
    if (::fstat(f->fd, &st) != 0) { snprintf(errbuf, errbuf_sz, "fstat: %s", strerror(errno)); ::close(f->fd); f->fd=-1; return false; }
    f->size = (size_t)st.st_size;
    if (f->size == 0) { f->map = nullptr; return true; }
    f->map = ::mmap(nullptr, f->size, PROT_READ, MAP_PRIVATE, f->fd, 0);
    if (f->map == MAP_FAILED) { snprintf(errbuf, errbuf_sz, "mmap: %s", strerror(errno)); f->map=nullptr; ::close(f->fd); f->fd=-1; return false; }
    return true;
}

static int detect_format(const uint8_t* p, size_t n) {
    if (n >= 4 && p[0]==0x00 && p[1]==0x06 && p[2]==0x00 && p[3]==0x02) return 1; // GDSII HEADER
    if (n >= 13 && memcmp(p, "%SEMI-OASIS\r\n", 13)==0) return 2;
    return 0;
}

// Forward decl parsers
bool parse_gdsii(LdbFile* f);
int parse_oasis_stream(LdbFile* f, ldb_box_cb cb, void* ud);
bool parse_oasis(LdbFile* f);

static std::string make_spill_path(size_t idx){
    static int seq = 0;
    char buf[256];
    snprintf(buf, sizeof(buf), "/tmp/ldb_boxes_%d_%d_%zu.bin", (int)getpid(), seq++, idx);
    return std::string(buf);
}

static bool spill_compact_to_disk(LdbCell& c, size_t idx){
    if(c.boxes_compact.empty()) return false;
    std::string path = make_spill_path(idx);
    FILE* fp = fopen(path.c_str(), "wb");
    if(!fp) return false;
    size_t n = fwrite(c.boxes_compact.data(), 1, c.boxes_compact.size(), fp);
    fclose(fp);
    if(n != c.boxes_compact.size()){ ::unlink(path.c_str()); return false; }
    c.boxes_spill_path = path;
    c.boxes_on_disk = true;
    // free in-memory copy of compact bytes (geometry kept on disk)
    std::vector<uint8_t>().swap(c.boxes_compact);
    return true;
}

std::vector<LdbBoxElem> ldb_load_boxes_on_demand(LdbCell& c){
    if(!c.boxes.empty()) return c.boxes;
    if(!c.boxes_compact.empty()) return compact::decode_boxes(c.boxes_compact);
    if(c.boxes_on_disk && !c.boxes_spill_path.empty()){
        FILE* fp = fopen(c.boxes_spill_path.c_str(), "rb");
        if(!fp) return {};
        fseek(fp, 0, SEEK_END);
        long sz = ftell(fp);
        fseek(fp, 0, SEEK_SET);
        if(sz <= 0){ fclose(fp); return {}; }
        std::vector<uint8_t> buf((size_t)sz);
        size_t n = fread(buf.data(), 1, buf.size(), fp);
        fclose(fp);
        if(n != buf.size()) return {};
        return compact::decode_boxes(buf);
    }
    return {};
}

static void apply_low_memory(LdbFile* f){
    if(!f) return;
    bool lowmem = (f->open_flags & LDB_OPEN_LOW_MEMORY) != 0;
    uint64_t thr = f->large_cell_box_threshold;
    if(!lowmem) return;
    if(thr == 0) thr = 200000; // default: keep large cells compact/on-demand
    for(size_t i=0;i<f->cells.size();++i){
        auto& c = f->cells[i];
        if(c.boxes.size() >= thr){
            // ensure compact form exists
            if(c.boxes_compact.empty() && !c.boxes.empty()){
                c.boxes_compact = compact::encode_boxes(c.boxes);
            }
            // drop expanded boxes to save memory
            std::vector<LdbBoxElem>().swap(c.boxes);
            // spill compact geometry to disk-access storage
            if(!spill_compact_to_disk(c, i)){
                // fallback: keep compact in memory (still on-demand decode)
                c.boxes_on_disk = false;
            }
        } else if(!c.boxes.empty() && (f->open_flags & LDB_OPEN_LOW_MEMORY)){
            // for smaller cells in low-memory mode, prefer compact in-memory over expanded
            if(c.boxes_compact.empty()) c.boxes_compact = compact::encode_boxes(c.boxes);
            // keep both? for small cells can drop expanded to reduce RAM; decode on demand
            if(c.boxes.size() >= 1024){
                std::vector<LdbBoxElem>().swap(c.boxes);
            }
        }
    }
}

static bool file_exists(const char* p){ struct stat st; return ::stat(p,&st)==0; }

static std::string find_klayout(){
    const char* env = getenv("KLAYOUT_BIN");
    if(env && env[0] && file_exists(env)) return env;
    const char* cands[] = {
        "/Applications/KLayout/klayout.app/Contents/MacOS/klayout",
        "/usr/bin/klayout",
        "/usr/local/bin/klayout",
        "klayout",
        nullptr
    };
    for(int i=0;cands[i];++i){
        if(i < 3){ if(file_exists(cands[i])) return cands[i]; }
        else return cands[i]; // let PATH resolve
    }
    return "";
}

static bool run_klayout_convert(const std::string& kl, const char* in, const std::string& out){
    // Write a tiny ruby script
    char rbpath[256];
    snprintf(rbpath, sizeof(rbpath), "/tmp/ldb_kl_%d.rb", (int)getpid());
    FILE* fp = fopen(rbpath, "w");
    if(!fp) return false;
    fprintf(fp, "layout = RBA::Layout.new\nlayout.read($in)\nlayout.write($out)\n");
    fclose(fp);
    std::string cmd = "\"" + kl + "\" -zz -rd in=\"" + std::string(in) + "\" -rd out=\"" + out + "\" -r " + rbpath + " >/tmp/ldb_kl_out.log 2>&1";
    int rc = system(cmd.c_str());
    ::unlink(rbpath);
    return rc == 0 && file_exists(out.c_str());
}

static ldb_file_t* do_open_native(const char* path, const ldb_open_opts_t* opts, char* errbuf, size_t errbuf_sz) {
    if (errbuf && errbuf_sz) errbuf[0]='\0';
    auto* f = new LdbFile();
    if(opts){
        f->open_flags = opts->flags;
        f->large_cell_box_threshold = opts->large_cell_box_threshold;
        if(opts->flags & LDB_OPEN_STRICT) f->strict_mode = true;
    }
    if (!map_file(f, path, errbuf, errbuf_sz)) { delete f; return nullptr; }
    auto* p = (const uint8_t*)f->map;
    f->format = detect_format(p, f->size);
    bool ok = false;
    if (f->format == 1) ok = parse_gdsii(f);
    else if (f->format == 2) ok = parse_oasis(f);
    else { snprintf(errbuf, errbuf_sz, "unknown layout format"); ok = false; }
    if (!ok) {
        if (errbuf && errbuf_sz && errbuf[0]=='\0') snprintf(errbuf, errbuf_sz, "%s", f->error.empty()?"parse error":f->error.c_str());
        ldb_close((ldb_file_t*)f); return nullptr;
    }
    apply_low_memory(f);
    return (ldb_file_t*)f;
}

static ldb_file_t* do_open(const char* path, const ldb_open_opts_t* opts, char* errbuf, size_t errbuf_sz) {
    if (errbuf && errbuf_sz) errbuf[0]='\0';
    // Native readers are the default for all formats (GDSII/OASIS).
    // Optional KLayout-assisted conversion path for OASIS can be enabled with LDB_OASIS_KLAYOUT=1.
    int fd = ::open(path, O_RDONLY);
    int fmt = 0;
    if(fd >= 0){
        uint8_t hdr[16] = {0};
        ssize_t n = ::read(fd, hdr, sizeof(hdr));
        fmt = detect_format(hdr, (size_t)(n>0?n:0));
        ::close(fd);
    }
    const char* use_kl = getenv("LDB_OASIS_KLAYOUT");
    if(fmt == 2 && use_kl && use_kl[0] && use_kl[0] != '0'){
        std::string kl = find_klayout();
        if(!kl.empty()){
            char outgds[256];
            snprintf(outgds, sizeof(outgds), "/tmp/ldb_conv_%d.gds", (int)getpid());
            // try conversion (works when klayout executable resolves)
            bool conv = run_klayout_convert(kl, path, outgds);
            if(!conv && kl.find('/') == std::string::npos){
                // try common absolute paths if PATH lookup failed
                const char* abs = "/Applications/KLayout/klayout.app/Contents/MacOS/klayout";
                if(file_exists(abs)) conv = run_klayout_convert(abs, path, outgds);
            }
            if(conv){
                // Parse converted GDS for correct geometry/hierarchy; preserve format as OASIS-origin in usage
                auto* f = (LdbFile*)do_open_native(outgds, opts, errbuf, errbuf_sz);
                ::unlink(outgds);
                if(f){
                    // Mark as OASIS source; strict flag from original if requested/detected is best-effort
                    f->format = 2; // report as OASIS (original)
                    // Try to detect strict flag from original header quickly
                    int ofd = ::open(path, O_RDONLY);
                    if(ofd>=0){
                        uint8_t buf2[64]={0}; ssize_t rn=::read(ofd,buf2,sizeof(buf2)); ::close(ofd);
                        // parse minimal START: magic(13)+recid(1)+ver(a-str)+unit(real)+offsetflag
                        if(rn>20 && buf2[13]==1){
                            size_t ii=14;
                            // ver a-string
                            uint64_t vlen=0; int sh=0; while(ii<(size_t)rn){ uint8_t b=buf2[ii++]; vlen|=(uint64_t)(b&0x7f)<<sh; if(!(b&0x80)) break; sh+=7; }
                            ii += (size_t)vlen;
                            if(ii < (size_t)rn){
                                // unit real: skip based on type
                                uint8_t rt=buf2[ii++];
                                if(rt<=1){ while(ii<(size_t)rn && (buf2[ii++]&0x80)); }
                                else if(rt<=5){ while(ii<(size_t)rn && (buf2[ii++]&0x80)); while(ii<(size_t)rn && (buf2[ii++]&0x80)); }
                                else if(rt==6){ ii+=4; }
                                else if(rt==7){ ii+=8; }
                                else { while(ii<(size_t)rn && (buf2[ii++]&0x80)); }
                                if(ii < (size_t)rn){ uint8_t oflags=buf2[ii]; if(oflags & 0x40) f->strict_mode = true; }
                            }
                        }
                    }
                    if(opts && (opts->flags & LDB_OPEN_STRICT)) f->strict_mode = true;
                    return (ldb_file_t*)f;
                }
                // fall through to native on parse failure
            }
            // fall through to native if conversion unavailable/failed
        }
    }
    return do_open_native(path, opts, errbuf, errbuf_sz);
}

extern "C" ldb_file_t* ldb_open(const char* path, char* errbuf, size_t errbuf_sz) {
    return do_open(path, nullptr, errbuf, errbuf_sz);
}

extern "C" ldb_file_t* ldb_open_opts(const char* path, const ldb_open_opts_t* opts, char* errbuf, size_t errbuf_sz) {
    return do_open(path, opts, errbuf, errbuf_sz);
}

extern "C" void ldb_close(ldb_file_t* fh) {
    auto* f = (LdbFile*)fh; if(!f) return;
    if (f->map && f->map!=MAP_FAILED) ::munmap(f->map, f->size);
    if (f->fd >= 0) ::close(f->fd);
    for(auto& c : f->cells){
        if(c.boxes_on_disk && !c.boxes_spill_path.empty()){
            ::unlink(c.boxes_spill_path.c_str());
        }
    }
    delete f;
}
extern "C" int ldb_format(const ldb_file_t* fh){ return ((const LdbFile*)fh)->format; }
extern "C" int ldb_is_strict(const ldb_file_t* fh){ return (fh && ((const LdbFile*)fh)->strict_mode) ? 1 : 0; }

extern "C" int ldb_extent_whole(ldb_file_t* fh, ldb_extent_t* out) {
    auto* f=(LdbFile*)fh; if(!f||!out) return -1;
    ldb_extent_t e{0,0,0,0,0};
    for (auto& c: f->cells) ldb_extent_merge(e, c.cell_ext);
    *out = e; return e.valid?0:1;
}

extern "C" int ldb_extent_by_cell_layer(ldb_file_t* fh, ldb_cell_layer_cb cb, void* ud) {
    auto* f=(LdbFile*)fh; if(!f||!cb) return -1;
    for (auto& c: f->cells) {
        for (auto& kv: c.layer_ext) cb(ud, c.name.c_str(), kv.first.first, kv.first.second, kv.second);
    }
    return 0;
}

static void print_tree(LdbFile* f, const std::string& name, FILE* out, std::set<std::string>& visiting, int depth) {
    for(int i=0;i<depth;i++) fprintf(out,"  ");
    fprintf(out, "%s\n", name.c_str());
    auto it=f->cell_index.find(name); if(it==f->cell_index.end()) return;
    if (visiting.count(name)) return;
    visiting.insert(name);
    for (auto& ch: f->cells[it->second].refs) print_tree(f,ch,out,visiting,depth+1);
    visiting.erase(name);
}

extern "C" int ldb_print_hierarchy(ldb_file_t* fh, void* out_fp) {
    auto* f=(LdbFile*)fh; FILE* out=(FILE*)out_fp; if(!f||!out) return -1;
    std::map<std::string,int> indeg;
    for(auto& c:f->cells) { indeg[c.name]; for(auto&r:c.refs) indeg[r]++; }
    std::set<std::string> visiting;
    bool any=false;
    for(auto& c:f->cells) if(indeg[c.name]==0){ print_tree(f,c.name,out,visiting,0); any=true; }
    if(!any) for(auto& c:f->cells){ print_tree(f,c.name,out,visiting,0); }
    return 0;
}

extern "C" int ldb_print_propnames(ldb_file_t* fh, void* out_fp) {
    auto* f=(LdbFile*)fh; FILE* out=(FILE*)out_fp; if(!f||!out) return -1;
    for (auto& kv: f->propnames) fprintf(out, "%d\t%s\n", kv.first, kv.second.c_str());
    return 0;
}

extern "C" int ldb_iterate_properties(ldb_file_t* fh, ldb_property_cb cb, void* ud) {
    auto* f=(LdbFile*)fh; if(!f||!cb) return -1;
    for(auto& p : f->file_properties) cb(ud, p.target_kind, p.cell.c_str(), p.name.c_str(), p.value.c_str());
    for(auto& c : f->cells){
        for(auto& p : c.properties) cb(ud, p.target_kind, p.cell.empty()?c.name.c_str():p.cell.c_str(), p.name.c_str(), p.value.c_str());
    }
    return 0;
}

extern "C" size_t ldb_memory_bytes(const ldb_file_t* fh){
    auto* f=(const LdbFile*)fh; if(!f) return 0;
    size_t s = sizeof(LdbFile) + f->mem_bytes_extra;
    for(auto& c: f->cells){
        s += c.name.size() + c.layer_ext.size()*64 + c.refs.size()*32;
        s += c.boxes.size()*sizeof(LdbBoxElem);
        s += c.boxes_compact.size();
        s += c.instances.size()*sizeof(LdbInstance);
        s += c.properties.size()*sizeof(LdbProperty);
    }
    return s;
}

bool write_gdsii_model(const LdbFile* f, const char* path, std::string& err);   // gdsii.cpp

extern "C" double ldb_dbu_per_micron(const ldb_file_t* fh){ return fh ? ((const LdbFile*)fh)->dbu_per_micron : 0.0; }

extern "C" int ldb_write_gdsii(const ldb_file_t* fh, const char* out_path, char* errbuf, size_t errbuf_sz){
    auto* f=(const LdbFile*)fh; if(errbuf&&errbuf_sz) errbuf[0]='\0';
    if(!f){ if(errbuf) snprintf(errbuf,errbuf_sz,"null"); return -1; }
    if(f->format!=1 || f->gds_records.empty()){
        std::string err;
        if(!write_gdsii_model(f, out_path, err)){ if(errbuf) snprintf(errbuf,errbuf_sz,"%s",err.c_str()); return -1; }
        return 0;
    }
    FILE* fp=fopen(out_path,"wb"); if(!fp){ if(errbuf) snprintf(errbuf,errbuf_sz,"open for write failed"); return -1; }
    for(auto& r: f->gds_records){ if(fwrite(r.bytes.data(),1,r.bytes.size(),fp)!=r.bytes.size()){ fclose(fp); if(errbuf) snprintf(errbuf,errbuf_sz,"write failed"); return -1; } }
    fclose(fp); return 0;
}

extern "C" int ldb_iterate_boxes(ldb_file_t* fh, ldb_box_cb cb, void* ud){
    auto* f=(LdbFile*)fh; if(!f||!cb) return -1;
    for(auto& c: f->cells){
        if(!c.boxes.empty()){
            for(auto& b: c.boxes){ cb(ud, c.name.c_str(), b.layer, b.datatype, b.kind, b.x1,b.y1,b.x2,b.y2); }
        } else if(!c.boxes_compact.empty() || c.boxes_on_disk){
            // low-memory/compressed view: disk-access/on-demand decode
            auto decoded = ldb_load_boxes_on_demand(c);
            for(auto& b: decoded){ cb(ud, c.name.c_str(), b.layer, b.datatype, b.kind, b.x1,b.y1,b.x2,b.y2); }
        }
    }
    return 0;
}

// Top cells (indegree 0) of the file; all cells when there is none (a cycle).
static std::vector<size_t> ldb_flat_roots(LdbFile* f){
    std::unordered_set<std::string> referenced;
    for(auto& c: f->cells){ for(auto& r: c.refs) referenced.insert(r); for(auto& i: c.instances) referenced.insert(i.ref_name); }
    std::vector<size_t> roots;
    for(size_t ci=0; ci<f->cells.size(); ci++) if(!referenced.count(f->cells[ci].name)) roots.push_back(ci);
    if(roots.empty()) for(size_t ci=0; ci<f->cells.size(); ci++) roots.push_back(ci);
    return roots;
}

// Walk the hierarchy below `ci` with the composed map `acc` (see flat_xform.hpp): `leaf` is
// called for every cell reached, with the map of that cell into the root.
template <class Leaf>
static void ldb_flat_walk(LdbFile* f, size_t ci, const ldbflat::Affine& acc, std::vector<char>& on_stack, Leaf& leaf){
    if(ci >= f->cells.size() || on_stack[ci]) return;      // cycle guard
    on_stack[ci] = 1;
    auto& c = f->cells[ci];
    leaf(c, acc);
    for(const auto& inst: c.instances){
        auto it = f->cell_index.find(inst.ref_name);
        if(it == f->cell_index.end()) continue;
        for(int64_t row=0; row<std::max<int64_t>(1,inst.tr.rows); row++)
            for(int64_t col=0; col<std::max<int64_t>(1,inst.tr.cols); col++)
                ldb_flat_walk(f, it->second, ldbflat::compose(acc, ldbflat::from_placement(inst.tr, col, row)), on_stack, leaf);
    }
    on_stack[ci] = 0;
}

// Flattened boxes from the top cell(s), labelled with the top cell name. Orthogonal
// placements are exact; under a non-orthogonal one a box becomes rectangle strips of the
// rotated rectangle (approximation; ldb_iterate_polygons_flat gives the exact outline).
extern "C" int ldb_iterate_boxes_flat(ldb_file_t* fh, ldb_box_cb cb, void* ud){
    auto* f=(LdbFile*)fh; if(!f||!cb) return -1;
    int64_t approx = 0;
    std::vector<char> on_stack(f->cells.size(), 0);
    std::vector<ldbgeom::Rect> strips;
    for(size_t root: ldb_flat_roots(f)){
        const char* top_cell = f->cells[root].name.c_str();
        auto leaf = [&](LdbCell& c, const ldbflat::Affine& acc){
            std::vector<LdbBoxElem> decoded;
            const std::vector<LdbBoxElem>* bs = &c.boxes;
            if(c.boxes.empty() && (!c.boxes_compact.empty() || c.boxes_on_disk)){ decoded = ldb_load_boxes_on_demand(c); bs = &decoded; }
            int64_t m[4], tx, ty;
            const bool ortho = acc.orthogonal(m, tx, ty);
            for(const auto& b: *bs){
                if(ortho){
                    const int64_t ax = m[0]*b.x1 + m[1]*b.y1, ay = m[2]*b.x1 + m[3]*b.y1;
                    const int64_t bx = m[0]*b.x2 + m[1]*b.y2, by = m[2]*b.x2 + m[3]*b.y2;
                    cb(ud, top_cell, b.layer, b.datatype, b.kind, tx + std::min(ax,bx), ty + std::min(ay,by),
                       tx + std::max(ax,bx), ty + std::max(ay,by));
                } else {
                    ldbflat::rotated_box_strips(acc, b.x1,b.y1,b.x2,b.y2, strips); ++approx;
                    for(auto& r: strips) cb(ud, top_cell, b.layer, b.datatype, b.kind, r[0],r[1],r[2],r[3]);
                }
            }
        };
        ldb_flat_walk(f, root, ldbflat::Affine(), on_stack, leaf);
    }
    if(approx > 0)
        fprintf(stderr, "WARNING: ldb_iterate_boxes_flat: %lld boxes under non-orthogonal placements "
                "approximated by rectangle strips\n", (long long)approx);
    return 0;
}

static void ldb_emit_poly_as_rect(ldb_poly_cb cb, void* ud, const char* cell,
                                  int layer,int datatype,int kind,
                                  int64_t x1,int64_t y1,int64_t x2,int64_t y2){
    int64_t xs[4]={x1,x2,x2,x1};
    int64_t ys[4]={y1,y1,y2,y2};
    cb(ud, cell, layer, datatype, kind, 4, xs, ys);
}

extern "C" int ldb_iterate_polygons(ldb_file_t* fh, ldb_poly_cb cb, void* ud){
    auto* f=(LdbFile*)fh; if(!f||!cb) return -1;
    for(auto& c: f->cells){
        if(!c.polys.empty()){
            for(auto& p: c.polys){
                if(p.pts.empty()) continue;
                std::vector<int64_t> xs, ys;
                xs.reserve(p.pts.size()); ys.reserve(p.pts.size());
                for(auto& pt: p.pts){ xs.push_back(pt.x); ys.push_back(pt.y); }
                cb(ud, c.name.c_str(), p.layer, p.datatype, p.kind, (int)xs.size(), xs.data(), ys.data());
            }
            if(c.polys_extra)
                for(auto& b: c.boxes) if(b.kind==2) ldb_emit_poly_as_rect(cb,ud,c.name.c_str(),b.layer,b.datatype,b.kind,b.x1,b.y1,b.x2,b.y2);
        } else if(!c.boxes.empty()){
            for(auto& b: c.boxes){ ldb_emit_poly_as_rect(cb,ud,c.name.c_str(),b.layer,b.datatype,b.kind,b.x1,b.y1,b.x2,b.y2); }
        } else if(!c.boxes_compact.empty() || c.boxes_on_disk){
            auto decoded = ldb_load_boxes_on_demand(c);
            for(auto& b: decoded){ ldb_emit_poly_as_rect(cb,ud,c.name.c_str(),b.layer,b.datatype,b.kind,b.x1,b.y1,b.x2,b.y2); }
        }
    }
    return 0;
}

// Flattened exact shapes from the top cell(s), labelled with the top cell name. Every vertex
// is mapped by the composed placement map and rounded once (flat_xform.hpp), so rotated,
// mirrored and magnified placements give the exact transformed outline. Paths (kind 1) are
// emitted as their outline polygons (ldbflat::path_outline), not as centrelines.
extern "C" int ldb_iterate_polygons_flat(ldb_file_t* fh, ldb_poly_cb cb, void* ud){
    auto* f=(LdbFile*)fh; if(!f||!cb) return -1;
    std::vector<char> on_stack(f->cells.size(), 0);
    std::vector<int64_t> xs, ys;
    std::vector<std::vector<LdbPoint>> outl;
    for(size_t root: ldb_flat_roots(f)){
        const char* top_cell = f->cells[root].name.c_str();
        auto emit_pts = [&](const ldbflat::Affine& acc, int layer, int datatype, int kind, const std::vector<LdbPoint>& pts){
            xs.resize(pts.size()); ys.resize(pts.size());
            for(size_t i=0;i<pts.size();i++) acc.apply(pts[i].x, pts[i].y, xs[i], ys[i]);
            cb(ud, top_cell, layer, datatype, kind, (int)xs.size(), xs.data(), ys.data());
        };
        auto emit_box = [&](const ldbflat::Affine& acc, const LdbBoxElem& b){
            const std::vector<LdbPoint> pts = { {b.x1,b.y1}, {b.x2,b.y1}, {b.x2,b.y2}, {b.x1,b.y2} };
            emit_pts(acc, b.layer, b.datatype, b.kind, pts);
        };
        auto leaf = [&](LdbCell& c, const ldbflat::Affine& acc){
            if(!c.polys.empty()){
                for(auto& p: c.polys){
                    if(p.pts.empty()) continue;
                    if(p.kind == 1){
                        outl.clear(); ldbflat::path_outline(p, outl);
                        for(auto& o: outl) emit_pts(acc, p.layer, p.datatype, 1, o);
                    } else emit_pts(acc, p.layer, p.datatype, p.kind, p.pts);
                }
                if(c.polys_extra) for(auto& b: c.boxes) if(b.kind==2) emit_box(acc, b);
            } else if(!c.boxes.empty()){
                for(auto& b: c.boxes) emit_box(acc, b);
            } else if(!c.boxes_compact.empty() || c.boxes_on_disk){
                for(auto& b: ldb_load_boxes_on_demand(c)) emit_box(acc, b);
            }
        };
        ldb_flat_walk(f, root, ldbflat::Affine(), on_stack, leaf);
    }
    return 0;
}

extern "C" int ldb_iterate_texts(ldb_file_t* fh, ldb_text_cb cb, void* ud) {
    auto* f = (LdbFile*)fh; if (!f || !cb) return -1;
    for (auto& c : f->cells)
        for (auto& t : c.texts)
            cb(ud, c.name.c_str(), t.layer, t.texttype, t.x, t.y, t.str.c_str());
    return 0;
}

extern "C" int ldb_iterate_texts_flat(ldb_file_t* fh, ldb_text_cb cb, void* ud) {
    auto* f = (LdbFile*)fh; if (!f || !cb) return -1;
    std::vector<char> on_stack(f->cells.size(), 0);
    for (size_t root : ldb_flat_roots(f)) {
        const char* top_cell = f->cells[root].name.c_str();
        auto leaf = [&](LdbCell& c, const ldbflat::Affine& acc) {
            for (auto& t : c.texts) {
                int64_t x, y;
                acc.apply(t.x, t.y, x, y);
                cb(ud, top_cell, t.layer, t.texttype, x, y, t.str.c_str());
            }
        };
        ldb_flat_walk(f, root, ldbflat::Affine(), on_stack, leaf);
    }
    return 0;
}

// Flat streaming load: OASIS boxes go straight to cb while parsing, no in-memory copy of the
// geometry. Returns 0 ok, 2 hierarchical file (use ldb_open + ldb_iterate_boxes*),
// 3 format not supported for streaming (GDSII), -1 error (errbuf).
extern "C" int ldb_stream_boxes(const char* path, ldb_box_cb cb, void* ud, char* errbuf, size_t errbuf_sz) {
    if (errbuf && errbuf_sz) errbuf[0] = '\0';
    auto* f = new LdbFile();
    if (!map_file(f, path, errbuf, errbuf_sz)) { delete f; return -1; }
    int rc;
    if (detect_format((const uint8_t*)f->map, f->size) != 2) rc = 3;
    else {
        rc = parse_oasis_stream(f, cb, ud);
        if (errbuf && errbuf_sz) snprintf(errbuf, errbuf_sz, "%s", f->error.c_str());
    }
    ldb_close((ldb_file_t*)f);
    return rc;
}
