#include "oasis_ir.hpp"
#include <cstdio>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc,char**argv){
    if(argc<2) return 1;
    int fd=open(argv[1],O_RDONLY); struct stat st{}; fstat(fd,&st);
    void* m=mmap(nullptr,st.st_size,PROT_READ,MAP_PRIVATE,fd,0);
    // can't easily use internal parser; just print header
    unsigned char* p=(unsigned char*)m;
    printf("size=%ld\n",(long)st.st_size);
    for(int i=0;i<32 && i<st.st_size;i++) printf("%02x ",p[i]);
    printf("\n");
    munmap(m,st.st_size); close(fd);
    return 0;
}
