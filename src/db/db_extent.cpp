#include "layoutdb.h"
#include <cstdio>
#include <cstdlib>
#include <cinttypes>
int main(int argc, char** argv){
    if(argc<2){ fprintf(stderr,"usage: %s <file.gds|file.oas>\n",argv[0]); return 2; }
    char err[512]; ldb_file_t* f=ldb_open(argv[1],err,sizeof(err));
    if(!f){ fprintf(stderr,"open failed: %s\n",err); return 1; }
    ldb_extent_t e; int rc=ldb_extent_whole(f,&e);
    if(rc==0 && e.valid) printf("EXTENT %" PRId64 " %" PRId64 " %" PRId64 " %" PRId64 "\n",(int64_t)e.xmin,(int64_t)e.ymin,(int64_t)e.xmax,(int64_t)e.ymax);
    else printf("EXTENT invalid\n");
    ldb_close(f); return 0;
}
