////////////////////////////////////////////////////////////////////////////////
// File   : utils.hpp
// Author : Sandeep Koranne
////////////////////////////////////////////////////////////////////////////////
#ifndef VLSI_LAYOUT_UTILS_HPP
#define VLSI_LAYOUT_UTILS_HPP
#include <cstdint>
#include <cstring>
#include <stdexcept>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <cmath>    // std::ldexp, std::pow
namespace VLSILayout
{
  /// Convert a 16‑bit big‑endian value to host order
  inline std::uint16_t be16toh(const std::uint8_t* p)
  {
    return (static_cast<std::uint16_t>(p[0]) << 8) |
      static_cast<std::uint16_t>(p[1]);
  }

  /// Convert a 32‑bit big‑endian value to host order
  inline std::uint32_t be32toh(const std::uint8_t* p)
  {
    return (static_cast<std::uint32_t>(p[0]) << 24) |
      (static_cast<std::uint32_t>(p[1]) << 16) |
      (static_cast<std::uint32_t>(p[2]) <<  8) |
      static_cast<std::uint32_t>(p[3]);
  }
  inline std::int64_t read_be64(const std::uint8_t* p)
  {
    std::int64_t v = 0;
    for (int i = 0; i < 8; ++i)
      v = (v << 8) | static_cast<std::int64_t>(p[i]);
    return v;
  }
  std::int32_t read_be32(const std::uint8_t* ptr) {
    return (ptr[0] << 24) | (ptr[1] << 16) | (ptr[2] << 8) | ptr[3];
  }
  /// Convert an 8‑byte IBM‑format floating‑point value (big‑endian)
  /// to a native IEEE‑754 double.
  ///
  /// The algorithm is taken from the GDSII specification:
  ///   value = (-1)^sign × 0.fraction × 16^(exponent‑64)
  inline double ibm_to_ieee(const std::uint8_t* p)
  {
    // Assemble the 64‑bit big‑endian word
    std::uint64_t raw = 0;
    for (int i = 0; i < 8; ++i)
      raw = (raw << 8) | static_cast<std::uint64_t>(p[i]);

    if (raw == 0)                     // all‑zero is exactly 0.0
      return 0.0;

    // ---- decode the three fields ---------------------------------
    const int      sign      = (raw >> 63) & 0x1;                 // 1 bit
    const int      exponent  = (raw >> 56) & 0x7F;                // 7 bits (biased)
    const std::uint64_t fraction = raw & 0x00FFFFFFFFFFFFFFULL;   // 56 bits

    // ---- build the mantissa (0.fraction) -------------------------
    // fraction / 2^56 gives the value of the fractional part.
    const double mantissa = static_cast<double>(fraction) /
      static_cast<double>(1ULL << 56);

    // ---- apply the base‑16 exponent --------------------------------
    // 16^(e‑64) == 2^(4·(e‑64))
    const int  base16Exp = exponent - 64;
    const double value  = std::ldexp(mantissa, 4 * base16Exp); // 2‑power version

    // ---- sign ----------------------------------------------------
    return (sign ? -value : value);
  }
  inline double read_be_double(const std::uint8_t* p)
  {
    // GDSII stores doubles as big‑endian IEEE‑754 8‑byte values.
    std::uint64_t u = 0;
    for (int i = 0; i < 8; ++i) u = (u << 8) | static_cast<std::uint64_t>(p[i]);
    double d;
    std::memcpy(&d, &u, sizeof(d));   // copy bits into a double
    return d;
  }
  /// Simple RAII wrapper around `mmap`/`munmap`
  class MappedFile
  {
  public:
    const std::uint8_t* data   = nullptr;   ///< start of the mapping
    std::size_t        size   = 0;          ///< length in bytes
    int                fd     = -1;        ///< file descriptor (kept open)

    explicit MappedFile(const char* path)
    {
      fd = ::open(path, O_RDONLY | O_CLOEXEC);
      if (fd < 0) throw std::runtime_error("open() failed");

      struct stat st{};
      if (::fstat(fd, &st) < 0) {
	::close(fd);
	throw std::runtime_error("fstat() failed");
      }
      size = static_cast<std::size_t>(st.st_size);
      if (size == 0) {
	::close(fd);
	throw std::runtime_error("file is empty");
      }

      // MAP_PRIVATE + PROT_READ gives us a read‑only, copy‑on‑write view.
      // This is the usual “direct” mmap – no extra buffers are allocated.
      void* ptr = ::mmap(nullptr, size,
			 PROT_READ, MAP_PRIVATE, fd, 0);
      if (ptr == MAP_FAILED) {
	::close(fd);
	throw std::runtime_error("mmap() failed");
      }
      madvise(ptr, size, MADV_SEQUENTIAL);
      // After processing a group
      //madvise(current_chunk, current_len, MADV_DONTNEED);
      data = static_cast<const std::uint8_t*>(ptr);
    }

    // non‑copyable
    MappedFile(const MappedFile&) = delete;
    MappedFile& operator=(const MappedFile&) = delete;

    // movable
    MappedFile(MappedFile&& other) noexcept
      : data(other.data), size(other.size), fd(other.fd)
    {
      other.data = nullptr;
      other.fd   = -1;
      other.size = 0;
    }
    MappedFile& operator=(MappedFile&& other) noexcept
    {
      if (this != &other) {
	unmap();
	data = other.data;
	size = other.size;
	fd   = other.fd;
	other.data = nullptr;
	other.fd   = -1;
	other.size = 0;
      }
      return *this;
    }

    ~MappedFile() { unmap(); }

  private:
    void unmap()
    {
      if (data) {
	::munmap(const_cast<std::uint8_t*>(data), size);
	data = nullptr;
      }
      if (fd >= 0) {
	::close(fd);
	fd = -1;
      }
    }
  };
}
#endif
