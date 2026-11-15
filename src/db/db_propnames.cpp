#include "layoutdb.h"
#include <cstdio>
int main(int argc,char**argv){
    if(argc<2){fprintf(stderr,"usage: %s <file>\n",argv[0]);return 2;}
    char err[512]; ldb_file_t* f=ldb_open(argv[1],err,sizeof(err)); if(!f){fprintf(stderr,"open failed: %s\n",err);return 1;}
    ldb_print_propnames(f, stdout); ldb_close(f); return 0;
}
