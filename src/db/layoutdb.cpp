#include "layoutdb_internal.hpp"
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstring>
#include <cerrno>

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

extern "C" ldb_file_t* ldb_open(const char* path, char* errbuf, size_t errbuf_sz) {
    if (errbuf && errbuf_sz) errbuf[0]='\0';
    auto* f = new LdbFile();
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
    return (ldb_file_t*)f;
}

extern "C" void ldb_close(ldb_file_t* fh) {
    auto* f = (LdbFile*)fh; if(!f) return;
    if (f->map && f->map!=MAP_FAILED) ::munmap(f->map, f->size);
    if (f->fd >= 0) ::close(f->fd);
    delete f;
}
extern "C" int ldb_format(const ldb_file_t* fh){ return ((const LdbFile*)fh)->format; }

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
    // roots: cells not referenced? compute in-degree
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

extern "C" size_t ldb_memory_bytes(const ldb_file_t* fh){
    auto* f=(const LdbFile*)fh; if(!f) return 0;
    size_t s = sizeof(LdbFile) + f->mem_bytes_extra;
    for(auto& c: f->cells){ s += c.name.size() + c.layer_ext.size()*64 + c.refs.size()*32; }
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
        for(auto& b: c.boxes){ cb(ud, c.name.c_str(), b.layer, b.datatype, b.kind, b.x1,b.y1,b.x2,b.y2); }
    }
    return 0;
}
