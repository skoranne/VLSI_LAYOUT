#include "db_stats.h"
#include <sys/resource.h>
#include <time.h>
#include <cstdio>
#include <cstring>
#include <string>

static struct timespec t0;
void db_stats_begin(void){ clock_gettime(CLOCK_MONOTONIC, &t0); }
static void query_gpu(db_stats_t* out){
    out->gpu_name[0]='\0'; out->gpu_mem_total_bytes=0; out->gpu_util_percent=0;
    FILE* p=popen("nvidia-smi --query-gpu=name,memory.total,utilization.gpu --format=csv,noheader,nounits 2>/dev/null","r");
    if(!p) return;
    char line[512]; if(fgets(line,sizeof(line),p)){
        // name, memMiB, util%
        char name[128]={0}; double mem=0; unsigned util=0;
        if(sscanf(line, " %127[^,], %lf, %u", name, &mem, &util)>=1){
            strncpy(out->gpu_name,name,sizeof(out->gpu_name)-1);
            out->gpu_mem_total_bytes=(size_t)(mem*1024.0*1024.0);
            out->gpu_util_percent=util;
        }
    }
    pclose(p);
}
void db_stats_end(db_stats_t* out, size_t in_memory_bytes){
    struct timespec t1; clock_gettime(CLOCK_MONOTONIC,&t1);
    out->cpu_seconds = (t1.tv_sec-t0.tv_sec) + (t1.tv_nsec-t0.tv_nsec)/1e9;
    struct rusage ru{}; getrusage(RUSAGE_SELF,&ru);
    out->max_rss_kb = ru.ru_maxrss; // kB on Linux
    out->in_memory_bytes = in_memory_bytes;
    query_gpu(out);
}
