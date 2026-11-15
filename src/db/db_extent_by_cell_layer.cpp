#include "layoutdb.h"
#include <cstdio>
#include <cinttypes>
static void cb(void*, const char* cell,int layer,int dt, ldb_extent_t e){
    if(e.valid) printf("%s\t%d\t%d\t%" PRId64 "\t%" PRId64 "\t%" PRId64 "\t%" PRId64 "\n",cell,layer,dt,(int64_t)e.xmin,(int64_t)e.ymin,(int64_t)e.xmax,(int64_t)e.ymax);
}
int main(int argc,char**argv){
    if(argc<2){fprintf(stderr,"usage: %s <file>\n",argv[0]);return 2;}
    char err[512]; ldb_file_t* f=ldb_open(argv[1],err,sizeof(err)); if(!f){fprintf(stderr,"open failed: %s\n",err);return 1;}
    ldb_extent_by_cell_layer(f,cb,nullptr); ldb_close(f); return 0;
}
