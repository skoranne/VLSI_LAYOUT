#pragma once
#include <cstdint>
#include <vector>
#include <cstring>
#include "layoutdb_internal.hpp"

// Compact/entropy-style encoding for box streams:
// - Delta-encode coordinates (sorted-ish by emission order, locality helps)
// - Variable-byte (LEB128-like unsigned, zigzag for signed) encoding
// - Repeated layout: store (base boxes + repetition params) conceptually; here we compress the materialized stream compactly.

namespace compact {

inline void enc_uvar(std::vector<uint8_t>& out, uint64_t v){
    while(v >= 0x80){ out.push_back((uint8_t)(v | 0x80)); v >>= 7; }
    out.push_back((uint8_t)v);
}
inline void enc_svar(std::vector<uint8_t>& out, int64_t v){
    uint64_t zz = (uint64_t)((v << 1) ^ (v >> 63));
    enc_uvar(out, zz);
}
inline uint64_t dec_uvar(const uint8_t*& p, const uint8_t* end){
    uint64_t v=0; int sh=0;
    while(p<end){ uint8_t b=*p++; v |= uint64_t(b&0x7f)<<sh; if(!(b&0x80)) return v; sh+=7; if(sh>63) break; }
    return v;
}
inline int64_t dec_svar(const uint8_t*& p, const uint8_t* end){
    uint64_t zz = dec_uvar(p,end);
    return (int64_t)(zz>>1) ^ -(int64_t)(zz&1);
}

inline std::vector<uint8_t> encode_boxes(const std::vector<LdbBoxElem>& boxes){
    std::vector<uint8_t> out;
    out.reserve(boxes.size() * 8); // typically much less than raw 36 bytes/box
    enc_uvar(out, (uint64_t)boxes.size());
    int64_t px1=0, py1=0, px2=0, py2=0;
    int pl=0, pd=0, pk=0;
    for(const auto& b : boxes){
        enc_svar(out, (int64_t)b.layer - pl); pl = b.layer;
        enc_svar(out, (int64_t)b.datatype - pd); pd = b.datatype;
        enc_svar(out, (int64_t)b.kind - pk); pk = b.kind;
        enc_svar(out, b.x1 - px1); px1 = b.x1;
        enc_svar(out, b.y1 - py1); py1 = b.y1;
        enc_svar(out, b.x2 - px2); px2 = b.x2;
        enc_svar(out, b.y2 - py2); py2 = b.y2;
    }
    return out;
}

inline std::vector<LdbBoxElem> decode_boxes(const std::vector<uint8_t>& in){
    std::vector<LdbBoxElem> boxes;
    if(in.empty()) return boxes;
    const uint8_t* p = in.data();
    const uint8_t* end = p + in.size();
    uint64_t n = dec_uvar(p,end);
    boxes.resize((size_t)n);
    int64_t px1=0, py1=0, px2=0, py2=0;
    int pl=0, pd=0, pk=0;
    for(size_t i=0;i<(size_t)n && p<end;i++){
        pl += (int)dec_svar(p,end);
        pd += (int)dec_svar(p,end);
        pk += (int)dec_svar(p,end);
        px1 += dec_svar(p,end);
        py1 += dec_svar(p,end);
        px2 += dec_svar(p,end);
        py2 += dec_svar(p,end);
        auto& b = boxes[i];
        b.layer=pl; b.datatype=pd; b.kind=pk;
        b.x1=px1; b.y1=py1; b.x2=px2; b.y2=py2;
    }
    return boxes;
}

inline size_t encoded_size(const std::vector<uint8_t>& in){ return in.size(); }

} // namespace compact
