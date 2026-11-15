#ifndef DB_STATS_H
#define DB_STATS_H
#include <stdint.h>
#include <stddef.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef struct {
    double cpu_seconds;
    long max_rss_kb;
    size_t in_memory_bytes;
    char gpu_name[128];
    size_t gpu_mem_total_bytes;
    unsigned gpu_util_percent;
} db_stats_t;
void db_stats_begin(void);
void db_stats_end(db_stats_t* out, size_t in_memory_bytes);
#ifdef __cplusplus
}
#endif
#endif
