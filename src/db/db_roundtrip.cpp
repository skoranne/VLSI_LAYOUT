#include "layoutdb.h"
#include "db_stats.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include <fstream>

static bool files_equal(const char* a,const char* b){
    std::ifstream f1(a,std::ios::binary), f2(b,std::ios::binary);
    if(!f1||!f2) return false;
    std::vector<char> b1((std::istreambuf_iterator<char>(f1)),{}), b2((std::istreambuf_iterator<char>(f2)),{});
    return b1==b2;
}

int main(int argc,char**argv){
    if(argc<3){
        fprintf(stderr,"usage: %s readwrite <in.gds> [out.gds]\n       %s gear_oasis <out.oas>\n",argv[0],argv[0]);
        return 2;
    }
    std::string mode=argv[1];
    if(mode=="gear_oasis"){
        char err[512]; db_stats_begin();
        int rc=ldb_write_oasis_gear_multigon(argv[2],err,sizeof(err));
        db_stats_t st{}; db_stats_end(&st,0);
        if(rc!=0){ fprintf(stderr,"gear_oasis failed: %s\n",err); return 1; }
        printf("Wrote GEAR/MULTIGON OASIS: %s\n",argv[2]);
        printf("CPU(s)=%.6f MaxRSS(kB)=%ld Mem(bytes)=%zu GPU=%s util=%u%% gpu_mem=%zu\n", st.cpu_seconds, st.max_rss_kb, st.in_memory_bytes, st.gpu_name, st.gpu_util_percent, st.gpu_mem_total_bytes);
        return 0;
    }
    if(mode=="readwrite"){
        const char* in=argv[2];
        std::string out = (argc>=4)? argv[3] : (std::string(in)+".rt");
        char err[512]; db_stats_begin();
        ldb_file_t* f=ldb_open(in,err,sizeof(err));
        if(!f){ fprintf(stderr,"open failed: %s\n",err); return 1; }
        size_t mem=ldb_memory_bytes(f);
        int fmt=ldb_format(f); int rc=-1;
        if(fmt==1) rc=ldb_write_gdsii(f,out.c_str(),err,sizeof(err));
        else if(fmt==2) rc=ldb_write_oasis(f,out.c_str(),err,sizeof(err));
        else { fprintf(stderr,"unknown format\n"); ldb_close(f); return 1; }
        db_stats_t st{}; db_stats_end(&st,mem);
        if(rc!=0){ fprintf(stderr,"write failed: %s\n",err); ldb_close(f); return 1; }
        // read back
        char err2[512]; ldb_file_t* f2=ldb_open(out.c_str(),err2,sizeof(err2));
        if(!f2){ fprintf(stderr,"read-back failed: %s\n",err2); ldb_close(f); return 1; }
        bool same = files_equal(in,out.c_str());
        ldb_extent_t e1{},e2{}; ldb_extent_whole(f,&e1); ldb_extent_whole(f2,&e2);
        printf("Format=%s\n", fmt==1?"GDSII":"OASIS");
        printf("Byte-identical: %s\n", same?"YES":"NO");
        printf("Extent1: %s\n", e1.valid?"valid":"invalid");
        printf("Extent2: %s\n", e2.valid?"valid":"invalid");
        printf("CPU(s)=%.6f MaxRSS(kB)=%ld Mem(bytes)=%zu GPU=%s util=%u%% gpu_mem=%zu\n", st.cpu_seconds, st.max_rss_kb, st.in_memory_bytes, st.gpu_name, st.gpu_util_percent, st.gpu_mem_total_bytes);
#if defined(LAYOUTDB_USE_NVCOMP)
        printf("nvComp: linked; CBLOCK backend LDB_CBLOCK_DECOMP=%s (auto|gpu|cpu|serial)\n",
               getenv("LDB_CBLOCK_DECOMP") ? getenv("LDB_CBLOCK_DECOMP") : "auto");
#else
        printf("nvComp: not linked in this build; CBLOCKs decoded on the CPU (LDB_CBLOCK_DECOMP=%s)\n",
               getenv("LDB_CBLOCK_DECOMP") ? getenv("LDB_CBLOCK_DECOMP") : "auto");
#endif
        ldb_close(f2); ldb_close(f);
        return same?0:3;
    }
    fprintf(stderr,"unknown mode\n"); return 2;
}
