#include "layoutdb.h"
#include "layoutdb_hier.h"
#include "layoutdb_internal.hpp"
#include <cstdio>
#include <string>

int main(int argc, char** argv){
    if(argc < 2){
        fprintf(stderr, "usage: %s <file.gds|file.oas> [top_cell]\n", argv[0]);
        fprintf(stderr, "  Computes hierarchical (flattened, transform-aware) extent.\n");
        return 2;
    }
    const char* top = (argc >= 3) ? argv[2] : "";
    char err[512];
    ldb_file_t* f = ldb_open(argv[1], err, sizeof(err));
    if(!f){ fprintf(stderr, "open failed: %s\n", err); return 1; }
    int ntop = ldb_top_cell_count(f);
    fprintf(stdout, "TOP_CELLS %d\n", ntop);
    for(int i=0;i<ntop;i++){
        const char* n = ldb_top_cell_name(f,i);
        if(n) fprintf(stdout, "TOP[%d]=%s\n", i, n);
    }
    ldb_extent_t e;
    int rc = ldb_extent_whole_hier(f, top[0]?top:nullptr, &e);
    if(rc==0 && e.valid){
        fprintf(stdout, "HIER_EXTENT %lld %lld %lld %lld\n",
            (long long)e.xmin,(long long)e.ymin,(long long)e.xmax,(long long)e.ymax);
    } else {
        fprintf(stdout, "HIER_EXTENT invalid\n");
    }
    // also print flat extent for comparison
    ldb_extent_t fe;
    if(ldb_extent_whole(f, &fe)==0 && fe.valid){
        fprintf(stdout, "FLAT_EXTENT %lld %lld %lld %lld\n",
            (long long)fe.xmin,(long long)fe.ymin,(long long)fe.xmax,(long long)fe.ymax);
    }
    const char* info = ((LdbFile*)f)->error.c_str();
    if(info && info[0]){
        // Print concise info: BOX_ENC and decompression status summary
        std::string s(info);
        size_t pos = s.find("BOX_ENC");
        if(pos != std::string::npos){
            size_t end = s.find(';', pos);
            if(end == std::string::npos) end = s.size();
            fprintf(stdout, "INFO: %s\n", s.substr(pos, end-pos).c_str());
        }
        size_t fails = 0, okdef = 0;
        for(size_t p=0; (p=s.find("zlib inflate failed", p))!=std::string::npos; ++p) fails++;
        for(size_t p=0; (p=s.find("cpu-raw-deflate", p))!=std::string::npos; ++p) okdef++;
        for(size_t p=0; (p=s.find("cpu-zlib", p))!=std::string::npos; ++p) okdef++;
        for(size_t p=0; (p=s.find("cpu-auto-deflate", p))!=std::string::npos; ++p) okdef++;
        fprintf(stdout, "INFO: CBLOCK_DECOMPRESS ok=%zu fail=%zu\n", okdef, fails);
    }
    ldb_close(f);
    return 0;
}
