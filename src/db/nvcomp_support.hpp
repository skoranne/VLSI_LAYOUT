#pragma once
// OASIS CBLOCK decompression: serial zlib, multi-threaded zlib, or batched nvCOMP (GPU).
//
// OASIS CBLOCK comp-type 0 is raw DEFLATE (RFC 1951) -- exactly nvCOMP's Deflate format.
// The batched API decompresses many CBLOCKs per call; oasis.cpp pre-scans the record stream
// for CBLOCK offsets and feeds windows of blocks here (double-buffered against parsing).
//
// Backend selection: LDB_CBLOCK_DECOMP = auto | gpu | cpu | serial (default auto).
//   serial : one block at a time with zlib, inline while parsing (the original behaviour)
//   cpu    : windows of blocks inflated by a std::thread pool (LDB_CBLOCK_THREADS, default all cores)
//   gpu    : windows of blocks inflated by nvcompBatchedDeflateDecompressAsync; any block whose
//            device status/size is wrong is retried on the CPU (so results never depend on GPU)
//   auto   : gpu when built with nvCOMP, a device exists and the window is large enough
//            (>= LDB_CBLOCK_GPU_MIN_BLOCKS blocks, default 64), otherwise cpu.
// Builds without nvCOMP (LAYOUTDB_USE_NVCOMP undefined) or zlib still compile and run.

#include <cstddef>
#include <cstdint>
#include <vector>
#include <string>

namespace db {

struct NvcompStatus {
    bool available=false;      // nvCOMP compiled in and a CUDA device is present
    bool used=false;           // at least one block was decompressed on the GPU
    std::string detail;
};

NvcompStatus nvcomp_init();

// Single block (serial path). out is resized to the decompressed size.
bool decompress_cblock(uint8_t comp_type, const uint8_t* comp, size_t comp_size, std::vector<uint8_t>& out,
                       size_t uncomp_size, NvcompStatus& st);

enum class CblockBackend { Serial, Cpu, Gpu, Auto };
CblockBackend cblock_backend_from_env();
const char* cblock_backend_name(CblockBackend b);

// One block of a batch: compressed bytes and where its output goes inside the window buffer.
struct CblockJob {
    const uint8_t* src = nullptr;
    size_t comp_len = 0;
    size_t uncomp_len = 0;
    uint8_t comp_type = 0;
    size_t out_off = 0;        // byte offset of this block inside the window output buffer
    bool ok = false;           // set by decompress_cblock_batch
};

// Host buffer for a window of decompressed blocks (page-locked when nvCOMP is available so
// device-to-host copies are DMA; plain heap otherwise). Grow-only.
struct HostBuffer {
    uint8_t* p = nullptr;
    size_t cap = 0;
    bool pinned = false;
    void reserve(size_t bytes, bool want_pinned);
    ~HostBuffer();
    HostBuffer() = default;
    HostBuffer(const HostBuffer&) = delete;
    HostBuffer& operator=(const HostBuffer&) = delete;
};

// Cumulative statistics (process-wide; reported in the OASIS INFO line).
struct CblockStats {
    size_t blocks_gpu = 0, blocks_cpu = 0, blocks_retry = 0, blocks_failed = 0, windows = 0;
    double inflate_ms = 0.0;    // time spent inside decompress_cblock_batch
    double wait_ms = 0.0;       // time the parser waited for a window (not overlapped)
    size_t bytes_in = 0, bytes_out = 0;
};
CblockStats& cblock_stats();

// Decompress every job into out + job.out_off. `be` must be Cpu or Gpu (Auto is resolved
// by the caller via resolve_backend). Returns the number of blocks that could not be decoded.
size_t decompress_cblock_batch(std::vector<CblockJob>& jobs, uint8_t* out, CblockBackend be, NvcompStatus& st);

// Alignment (bytes) callers must use for CblockJob::out_off so that host and device windows
// share one layout (lets the GPU path return a whole window with a single D2H copy).
size_t cblock_output_alignment(const NvcompStatus& st);

// Resolve Auto for a window of `nblocks` blocks.
CblockBackend resolve_backend(CblockBackend requested, size_t nblocks, const NvcompStatus& st);

} // namespace db
