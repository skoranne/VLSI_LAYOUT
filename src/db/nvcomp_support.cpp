// OASIS CBLOCK decompression backends (see nvcomp_support.hpp).
#include "nvcomp_support.hpp"
#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <new>
#include <string>
#include <thread>
#include <algorithm>

#if defined(LAYOUTDB_USE_NVCOMP)
#include <cuda_runtime.h>
#include <nvcomp.h>
#include <nvcomp/deflate.h>
#endif
#if defined(LAYOUTDB_USE_ZLIB)
#include <zlib.h>
#endif

namespace db {

namespace {

using clk = std::chrono::steady_clock;
double ms_since(clk::time_point t0) { return std::chrono::duration<double, std::milli>(clk::now() - t0).count(); }

size_t env_size(const char* name, size_t dflt) {
    const char* e = std::getenv(name);
    if (!e || !*e) return dflt;
    return (size_t)std::strtoull(e, nullptr, 10);
}

// Inflate one stream into dst[0..cap). wbits: -15 raw DEFLATE (OASIS), 15 zlib, 31 gzip.
bool inflate_with(const uint8_t* src, size_t n, uint8_t* dst, size_t cap, size_t& outlen, int wbits) {
#if defined(LAYOUTDB_USE_ZLIB)
    if (cap == 0 || n > 0xFFFFFFFFull || cap > 0xFFFFFFFFull) return false;
    z_stream zs{};
    zs.next_in = (Bytef*)src;
    zs.avail_in = (uInt)n;
    zs.next_out = (Bytef*)dst;
    zs.avail_out = (uInt)cap;
    if (inflateInit2(&zs, wbits) != Z_OK) return false;
    int r = inflate(&zs, Z_FINISH);
    inflateEnd(&zs);
    outlen = cap - zs.avail_out;
    if (r == Z_STREAM_END) return true;
    // Some writers end without a final-block marker / with trailing padding: accept if all
    // input was consumed (legacy behaviour of this reader).
    return (r == Z_OK || r == Z_BUF_ERROR) && zs.avail_in == 0;
#else
    (void)src; (void)n; (void)dst; (void)cap; (void)outlen; (void)wbits;
    return false;
#endif
}

// Decode one CBLOCK into dst (exactly uncomp_len bytes expected). OASIS comp-type 0 is raw
// DEFLATE, so that is tried first; zlib/gzip wrappers and "stored" are tolerated for
// non-conforming writers. Returns true only if exactly uncomp_len bytes were produced.
bool inflate_into(const CblockJob& j, uint8_t* dst) {
    size_t got = 0;
    const int order0[] = { -15, 15, 31 };
    for (int wb : order0) {
        if (inflate_with(j.src, j.comp_len, dst, j.uncomp_len, got, wb) && got == j.uncomp_len) return true;
    }
    if (j.comp_len == j.uncomp_len) {          // writer marked compressed but stored raw
        std::memcpy(dst, j.src, j.uncomp_len);
        return true;
    }
    return false;
}

std::mutex g_stats_mutex;

} // namespace

CblockStats& cblock_stats() {
    static CblockStats s;
    return s;
}

const char* cblock_backend_name(CblockBackend b) {
    switch (b) {
    case CblockBackend::Serial: return "serial";
    case CblockBackend::Cpu: return "cpu";
    case CblockBackend::Gpu: return "gpu";
    default: return "auto";
    }
}

CblockBackend cblock_backend_from_env() {
    const char* e = std::getenv("LDB_CBLOCK_DECOMP");
    if (!e || !*e) return CblockBackend::Auto;
    std::string v(e);
    if (v == "serial") return CblockBackend::Serial;
    if (v == "cpu") return CblockBackend::Cpu;
    if (v == "gpu" || v == "nvcomp") return CblockBackend::Gpu;
    return CblockBackend::Auto;
}

// ---------------------------------------------------------------------------
// HostBuffer
// ---------------------------------------------------------------------------
static void host_buffer_free(uint8_t*& p, bool pinned) {
    if (!p) return;
#if defined(LAYOUTDB_USE_NVCOMP)
    if (pinned) { cudaFreeHost(p); p = nullptr; return; }
#endif
    (void)pinned;
    std::free(p);
    p = nullptr;
}

void HostBuffer::reserve(size_t bytes, bool want_pinned) {
    if (bytes <= cap && (pinned || !want_pinned)) return;
    host_buffer_free(p, pinned);
    cap = 0; pinned = false;
    size_t n = std::max<size_t>(bytes, 1);
#if defined(LAYOUTDB_USE_NVCOMP)
    if (want_pinned && cudaHostAlloc((void**)&p, n, cudaHostAllocDefault) == cudaSuccess) {
        pinned = true; cap = n; return;
    }
    cudaGetLastError();
#endif
    (void)want_pinned;
    p = (uint8_t*)std::malloc(n);
    if (!p) throw std::bad_alloc();
    cap = n;
}

HostBuffer::~HostBuffer() { host_buffer_free(p, pinned); }

// ---------------------------------------------------------------------------
// GPU context (nvCOMP)
// ---------------------------------------------------------------------------
#if defined(LAYOUTDB_USE_NVCOMP)
namespace {
struct GpuCtx {
    std::mutex mu;
    bool init = false, ok = false;
    cudaStream_t stream = nullptr;
    nvcompAlignmentRequirements_t al{1, 1, 1};
    void* d_in = nullptr;   size_t cap_in = 0;
    void* d_out = nullptr;  size_t cap_out = 0;
    void* d_temp = nullptr; size_t cap_temp = 0;
    void* d_meta = nullptr; size_t cap_meta = 0;   // [in ptrs][out ptrs][in bytes][out bytes][actual][status]
    HostBuffer h_in, h_meta;

    bool grow(void*& p, size_t& cap, size_t need) {
        if (need <= cap) return true;
        if (p) cudaFree(p);
        p = nullptr; cap = 0;
        if (cudaMalloc(&p, std::max<size_t>(need, 1)) != cudaSuccess) { p = nullptr; return false; }
        cap = need;
        return true;
    }
    bool setup() {
        if (init) return ok;
        init = true;
        int ndev = 0;
        if (cudaGetDeviceCount(&ndev) != cudaSuccess || ndev <= 0) { cudaGetLastError(); return ok = false; }
        if (cudaStreamCreateWithFlags(&stream, cudaStreamNonBlocking) != cudaSuccess) return ok = false;
        if (nvcompBatchedDeflateDecompressGetRequiredAlignments(nvcompBatchedDeflateDecompressDefaultOpts, &al) != nvcompSuccess)
            al = nvcompAlignmentRequirements_t{8, 8, 8};
        return ok = true;
    }
};
GpuCtx& gpu() { static GpuCtx g; return g; }

inline size_t align_up(size_t v, size_t a) { return a <= 1 ? v : (v + a - 1) / a * a; }

// Returns false if the whole batch must go to the CPU (launch/setup failure).
bool gpu_batch(std::vector<CblockJob>& jobs, uint8_t* out, size_t& n_gpu) {
    GpuCtx& g = gpu();
    std::lock_guard<std::mutex> lk(g.mu);
    if (!g.setup()) return false;
    const size_t n = jobs.size();
    std::vector<size_t> in_off(n);
    size_t total_in = 0, total_out = 0, max_out = 0;
    for (size_t i = 0; i < n; ++i) {
        total_in = align_up(total_in, g.al.input);
        in_off[i] = total_in;
        total_in += jobs[i].comp_len;
        total_out = std::max(total_out, jobs[i].out_off + jobs[i].uncomp_len);
        max_out = std::max(max_out, jobs[i].uncomp_len);
    }
    size_t temp = 0;
    if (nvcompBatchedDeflateDecompressGetTempSizeAsync(n, max_out, nvcompBatchedDeflateDecompressDefaultOpts, &temp, total_out)
        != nvcompSuccess) return false;
    const size_t meta_bytes = n * (4 * sizeof(size_t) + sizeof(size_t) + sizeof(nvcompStatus_t)) + 64;
    if (!g.grow(g.d_in, g.cap_in, total_in) || !g.grow(g.d_out, g.cap_out, total_out) ||
        !g.grow(g.d_temp, g.cap_temp, align_up(temp, 256)) || !g.grow(g.d_meta, g.cap_meta, meta_bytes))
        return false;
    try {
        g.h_in.reserve(total_in, true);
        g.h_meta.reserve(meta_bytes, true);
    } catch (...) { return false; }

    // Stage compressed chunks (from the mmap'd file) into one pinned buffer.
    for (size_t i = 0; i < n; ++i) std::memcpy(g.h_in.p + in_off[i], jobs[i].src, jobs[i].comp_len);
    // Metadata, laid out identically on host and device.
    uint8_t* hm = g.h_meta.p;
    uint8_t* dm = (uint8_t*)g.d_meta;
    void** h_in_ptrs = (void**)hm;                 void** d_in_ptrs = (void**)dm;
    void** h_out_ptrs = h_in_ptrs + n;             void** d_out_ptrs = d_in_ptrs + n;
    size_t* h_in_bytes = (size_t*)(h_out_ptrs + n);   size_t* d_in_bytes = (size_t*)(d_out_ptrs + n);
    size_t* h_out_bytes = h_in_bytes + n;             size_t* d_out_bytes = d_in_bytes + n;
    size_t* h_actual = h_out_bytes + n;               size_t* d_actual = d_out_bytes + n;
    nvcompStatus_t* h_status = (nvcompStatus_t*)(h_actual + n);
    nvcompStatus_t* d_status = (nvcompStatus_t*)(d_actual + n);
    for (size_t i = 0; i < n; ++i) {
        h_in_ptrs[i] = (uint8_t*)g.d_in + in_off[i];
        h_out_ptrs[i] = (uint8_t*)g.d_out + jobs[i].out_off;
        h_in_bytes[i] = jobs[i].comp_len;
        h_out_bytes[i] = jobs[i].uncomp_len;
    }
    cudaStream_t s = g.stream;
    const size_t meta_in_bytes = 4 * n * sizeof(size_t);
    if (cudaMemcpyAsync(g.d_in, g.h_in.p, total_in, cudaMemcpyHostToDevice, s) != cudaSuccess) return false;
    if (cudaMemcpyAsync(dm, hm, meta_in_bytes, cudaMemcpyHostToDevice, s) != cudaSuccess) return false;
    nvcompStatus_t st = nvcompBatchedDeflateDecompressAsync(
        (const void* const*)d_in_ptrs, d_in_bytes, d_out_bytes, d_actual, n, g.d_temp, temp,
        (void* const*)d_out_ptrs, nvcompBatchedDeflateDecompressDefaultOpts, d_status, s);
    if (st != nvcompSuccess) { cudaStreamSynchronize(s); return false; }
    // Whole window back with one copy (job offsets share the device layout).
    if (cudaMemcpyAsync(out, g.d_out, total_out, cudaMemcpyDeviceToHost, s) != cudaSuccess) return false;
    if (cudaMemcpyAsync(h_actual, d_actual, n * (sizeof(size_t) + sizeof(nvcompStatus_t)) + 0, cudaMemcpyDeviceToHost, s)
        != cudaSuccess) return false;
    if (cudaStreamSynchronize(s) != cudaSuccess) return false;
    n_gpu = 0;
    for (size_t i = 0; i < n; ++i) {
        jobs[i].ok = (h_status[i] == nvcompSuccess && h_actual[i] == jobs[i].uncomp_len);
        if (jobs[i].ok) ++n_gpu;
    }
    return true;
}
} // namespace
#endif

NvcompStatus nvcomp_init() {
    NvcompStatus s;
#if defined(LAYOUTDB_USE_NVCOMP)
    int count = 0;
    cudaError_t e = cudaGetDeviceCount(&count);
    if (e == cudaSuccess && count > 0) { s.available = true; s.detail = "nvcomp+cuda available"; }
    else { cudaGetLastError(); s.detail = "cuda device not available"; }
#else
    s.available = false; s.detail = "built without nvCOMP (LAYOUTDB_USE_NVCOMP not set)";
#endif
    return s;
}

size_t cblock_output_alignment(const NvcompStatus& st) {
#if defined(LAYOUTDB_USE_NVCOMP)
    if (st.available) {
        GpuCtx& g = gpu();
        std::lock_guard<std::mutex> lk(g.mu);
        if (g.setup()) return std::max<size_t>(g.al.output, 8);
    }
#endif
    (void)st;
    return 8;
}

CblockBackend resolve_backend(CblockBackend requested, size_t nblocks, const NvcompStatus& st) {
    if (requested == CblockBackend::Serial || requested == CblockBackend::Cpu) return requested;
    if (!st.available) return CblockBackend::Cpu;
    if (requested == CblockBackend::Gpu) return CblockBackend::Gpu;
    static const size_t min_blocks = env_size("LDB_CBLOCK_GPU_MIN_BLOCKS", 64);
    return nblocks >= min_blocks ? CblockBackend::Gpu : CblockBackend::Cpu;
}

size_t decompress_cblock_batch(std::vector<CblockJob>& jobs, uint8_t* out, CblockBackend be, NvcompStatus& st) {
    const auto t0 = clk::now();
    size_t n_gpu = 0, n_retry = 0;
    for (auto& j : jobs) j.ok = false;
    bool gpu_done = false;
#if defined(LAYOUTDB_USE_NVCOMP)
    if (be == CblockBackend::Gpu && st.available) {
        // Only comp-type 0 (DEFLATE) goes to nvCOMP; anything else is decoded on the CPU below.
        std::vector<size_t> idx;
        std::vector<CblockJob> gj;
        for (size_t i = 0; i < jobs.size(); ++i)
            if (jobs[i].comp_type == 0 && jobs[i].uncomp_len > 0) { idx.push_back(i); gj.push_back(jobs[i]); }
        if (!gj.empty() && gpu_batch(gj, out, n_gpu)) {
            for (size_t k = 0; k < idx.size(); ++k) jobs[idx[k]].ok = gj[k].ok;
            gpu_done = true;
            if (n_gpu) st.used = true;
        } else if (!gj.empty()) {
            st.detail = "nvCOMP batch failed; CPU fallback";
        }
    }
#endif
    // CPU: every block not already decoded (all of them for the cpu backend; GPU rejects otherwise).
    std::vector<size_t> todo;
    for (size_t i = 0; i < jobs.size(); ++i) if (!jobs[i].ok) todo.push_back(i);
    if (gpu_done) n_retry = todo.size();
    if (!todo.empty()) {
        static const size_t hw = std::max<size_t>(1, std::thread::hardware_concurrency());
        const size_t want = env_size("LDB_CBLOCK_THREADS", hw);
        const size_t nthreads = std::max<size_t>(1, std::min(want, todo.size()));
        std::atomic<size_t> next{0};
        auto worker = [&]() {
            for (;;) {
                size_t k = next.fetch_add(1);
                if (k >= todo.size()) break;
                CblockJob& j = jobs[todo[k]];
                j.ok = inflate_into(j, out + j.out_off);
            }
        };
        if (nthreads == 1) worker();
        else {
            std::vector<std::thread> pool;
            pool.reserve(nthreads - 1);
            for (size_t t = 1; t < nthreads; ++t) pool.emplace_back(worker);
            worker();
            for (auto& th : pool) th.join();
        }
    }
    size_t failed = 0, bin = 0, bout = 0;
    for (auto& j : jobs) { if (!j.ok) ++failed; bin += j.comp_len; bout += j.uncomp_len; }
    {
        std::lock_guard<std::mutex> lk(g_stats_mutex);
        CblockStats& cs = cblock_stats();
        cs.blocks_gpu += n_gpu;
        cs.blocks_cpu += jobs.size() - n_gpu - failed;
        cs.blocks_retry += n_retry;
        cs.blocks_failed += failed;
        cs.windows += 1;
        cs.bytes_in += bin;
        cs.bytes_out += bout;
        cs.inflate_ms += ms_since(t0);
    }
    return failed;
}

// ---------------------------------------------------------------------------
// Serial single-block path (also used for nested / unscanned CBLOCKs).
// ---------------------------------------------------------------------------
bool decompress_cblock(uint8_t comp_type, const uint8_t* comp, size_t comp_size, std::vector<uint8_t>& out,
                       size_t uncomp_size, NvcompStatus& st) {
    CblockJob j;
    j.src = comp; j.comp_len = comp_size; j.uncomp_len = uncomp_size; j.comp_type = comp_type;
    out.assign(uncomp_size, 0);
    if (uncomp_size > 0 && inflate_into(j, out.data())) {
        std::lock_guard<std::mutex> lk(g_stats_mutex);
        cblock_stats().blocks_cpu += 1;
        cblock_stats().bytes_in += comp_size;
        cblock_stats().bytes_out += uncomp_size;
        return true;
    }
    char buf[96];
    snprintf(buf, sizeof(buf), "; inflate failed(type%u,csz=%zu,usz=%zu)", (unsigned)comp_type, comp_size, uncomp_size);
    st.detail += buf;
    out.assign(comp, comp + comp_size);
    return false;
}

} // namespace db
