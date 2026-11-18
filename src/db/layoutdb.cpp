#include "layoutdb_internal.hpp"
#include "compact_boxes.hpp"
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstring>
#include <cerrno>
#include <cstdio>

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

static std::vector<LdbBoxElem> load_boxes_on_demand(LdbCell& c){
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

static ldb_file_t* do_open(const char* path, const ldb_open_opts_t* opts, char* errbuf, size_t errbuf_sz) {
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

extern "C" int ldb_write_gdsii(const ldb_file_t* fh, const char* out_path, char* errbuf, size_t errbuf_sz){
    auto* f=(const LdbFile*)fh; if(errbuf&&errbuf_sz) errbuf[0]='\0';
    if(!f || f->format!=1 || f->gds_records.empty()){ if(errbuf) snprintf(errbuf,errbuf_sz,"no GDSII record stream loaded"); return -1; }
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
            auto decoded = load_boxes_on_demand(c);
            for(auto& b: decoded){ cb(ud, c.name.c_str(), b.layer, b.datatype, b.kind, b.x1,b.y1,b.x2,b.y2); }
        }
    }
    return 0;
}
