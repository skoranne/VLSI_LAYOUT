#include "nvcomp_support.hpp"
#include <cstring>

#if defined(LAYOUTDB_USE_NVCOMP)
#include <nvcomp.h>
#include <nvcomp/deflate.h>
#include <cuda_runtime.h>
#endif
#if defined(LAYOUTDB_USE_ZLIB)
#include <zlib.h>
#endif

namespace db {

NvcompStatus nvcomp_init(){
    NvcompStatus s;
#if defined(LAYOUTDB_USE_NVCOMP)
    int count=0; cudaError_t e=cudaGetDeviceCount(&count);
    if(e==cudaSuccess && count>0){ s.available=true; s.detail="nvcomp+cuda available"; }
    else { s.detail="cuda device not available"; }
#else
    s.available=false; s.detail="built without nvCOMP (LAYOUTDB_USE_NVCOMP not set)";
#endif
    return s;
}

bool nvcomp_decompress_deflate(const uint8_t* comp, size_t comp_size, uint8_t* out, size_t uncomp_size, std::string& err){
#if defined(LAYOUTDB_USE_NVCOMP)
    // nvComp deflate decompress (batched API simplified for single chunk)
    void* d_comp=nullptr; void* d_out=nullptr;
    cudaStream_t stream=0; cudaStreamCreate(&stream);
    if(cudaMalloc(&d_comp, comp_size)!=cudaSuccess || cudaMalloc(&d_out, uncomp_size)!=cudaSuccess){ err="cudaMalloc failed"; return false; }
    cudaMemcpyAsync(d_comp, comp, comp_size, cudaMemcpyHostToDevice, stream);
    // Use nvcomp deflate manager/low-level API depending on version; try low-level batched
    size_t* d_comp_bytes=nullptr; size_t* d_uncomp_bytes=nullptr; void** d_comp_ptrs=nullptr; void** d_out_ptrs=nullptr;
    cudaMalloc(&d_comp_bytes,sizeof(size_t)); cudaMalloc(&d_uncomp_bytes,sizeof(size_t));
    cudaMalloc(&d_comp_ptrs,sizeof(void*)); cudaMalloc(&d_out_ptrs,sizeof(void*));
    cudaMemcpyAsync(d_comp_bytes,&comp_size,sizeof(size_t),cudaMemcpyHostToDevice,stream);
    cudaMemcpyAsync(d_uncomp_bytes,&uncomp_size,sizeof(size_t),cudaMemcpyHostToDevice,stream);
    cudaMemcpyAsync(d_comp_ptrs,&d_comp,sizeof(void*),cudaMemcpyHostToDevice,stream);
    cudaMemcpyAsync(d_out_ptrs,&d_out,sizeof(void*),cudaMemcpyHostToDevice,stream);
    nvcompStatus_t st = nvcompBatchedDeflateDecompressAsync(d_comp_ptrs,d_comp_bytes,d_uncomp_bytes,nullptr,1,d_out_ptrs,nullptr,nullptr,0,stream);
    if(st!=nvcompSuccess){ err="nvcompBatchedDeflateDecompressAsync failed"; cudaFree(d_comp); cudaFree(d_out); return false; }
    cudaMemcpyAsync(out, d_out, uncomp_size, cudaMemcpyDeviceToHost, stream);
    cudaStreamSynchronize(stream);
    cudaFree(d_comp); cudaFree(d_out); cudaFree(d_comp_bytes); cudaFree(d_uncomp_bytes); cudaFree(d_comp_ptrs); cudaFree(d_out_ptrs);
    cudaStreamDestroy(stream);
    return true;
#else
    (void)comp;(void)comp_size;(void)out;(void)uncomp_size; err="nvCOMP not enabled at build time"; return false;
#endif
}

bool decompress_cblock(uint8_t comp_type, const uint8_t* comp, size_t comp_size, std::vector<uint8_t>& out, size_t uncomp_size, NvcompStatus& st){
    out.resize(uncomp_size);

#if defined(LAYOUTDB_USE_NVCOMP)
    if(comp_type != 0 && st.available){
        std::string e; if(nvcomp_decompress_deflate(comp,comp_size,out.data(),uncomp_size,e)){ st.used=true; return true; }
        st.detail = e;
    }
#endif

    if(comp_type==0){
        // Some writers mark CBLOCK as uncompressed but store deflate-compressed payload; try decompression first.
#if defined(LAYOUTDB_USE_ZLIB)
        {
            std::vector<uint8_t> tmp(uncomp_size);
            uLongf dst = (uLongf)uncomp_size;
            int zr = uncompress((Bytef*)tmp.data(), &dst, (const Bytef*)comp, (uLong)comp_size);
            if(zr == Z_OK){ out.swap(tmp); out.resize((size_t)dst); st.detail += "; cpu-zlib(type0)"; return true; }
            z_stream zs{}; zs.next_in=(Bytef*)comp; zs.avail_in=(uInt)comp_size; zs.next_out=(Bytef*)tmp.data(); zs.avail_out=(uInt)uncomp_size;
            if(inflateInit2(&zs,-15)==Z_OK){
                int r=inflate(&zs,Z_FINISH); inflateEnd(&zs);
                if(r==Z_STREAM_END){ out.swap(tmp); out.resize(uncomp_size - zs.avail_out); st.detail += "; cpu-raw-deflate(type0)"; return true; }
            }
        }
#endif
        if(comp_size==uncomp_size) { memcpy(out.data(),comp,comp_size); return true; }
        out.assign(comp,comp+comp_size); return true;
    }

#if defined(LAYOUTDB_USE_NVCOMP)
    if(st.available){
        std::string e; if(nvcomp_decompress_deflate(comp,comp_size,out.data(),uncomp_size,e)){ st.used=true; return true; }
        st.detail = e;
    }
#endif
#if defined(LAYOUTDB_USE_ZLIB)
    {
        uLongf dst = (uLongf)uncomp_size;
        int zr = uncompress((Bytef*)out.data(), &dst, (const Bytef*)comp, (uLong)comp_size);
        if(zr == Z_OK){ out.resize((size_t)dst); st.detail += "; cpu-zlib"; return true; }
        z_stream zs{}; zs.next_in=(Bytef*)comp; zs.avail_in=(uInt)comp_size; zs.next_out=(Bytef*)out.data(); zs.avail_out=(uInt)uncomp_size;
        if(inflateInit2(&zs,-15)==Z_OK){
            int r=inflate(&zs,Z_FINISH); inflateEnd(&zs);
            if(r==Z_STREAM_END){ out.resize(uncomp_size - zs.avail_out); st.detail += "; cpu-raw-deflate"; return true; }
        }
        st.detail += "; zlib inflate failed";
    }
#endif
    st.detail += "; decompression unavailable - kept compressed payload";
    out.assign(comp,comp+comp_size);
    return false;
}

} // namespace db
