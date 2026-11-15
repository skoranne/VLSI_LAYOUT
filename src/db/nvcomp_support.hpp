#pragma once
// Optional nvComp integration for OASIS CBLOCK decompression.
// Compiles without nvComp; enables GPU path when LAYOUTDB_USE_NVCOMP is defined and libs linked.

#include <cstddef>
#include <cstdint>
#include <vector>
#include <string>

namespace db {

struct NvcompStatus {
    bool available=false;
    bool used=false;
    std::string detail;
};

NvcompStatus nvcomp_init();
bool nvcomp_decompress_deflate(const uint8_t* comp, size_t comp_size, uint8_t* out, size_t uncomp_size, std::string& err);
// Generic: try GPU if available else CPU fallback (zlib)
bool decompress_cblock(uint8_t comp_type, const uint8_t* comp, size_t comp_size, std::vector<uint8_t>& out, size_t uncomp_size, NvcompStatus& st);

} // namespace db
