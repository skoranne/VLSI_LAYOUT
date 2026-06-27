/*
 * File    : snappy_api.h
 * Author  : Sandeep Koranne (C) 2026.
 * Purpose : Use lossless compression for storage
 */
#include <cstdint>
#include <cstddef>

extern "C" {

  size_t snappy_max_compressed_length(size_t input_bytes);
  void snappy_compress_bytes(const void* input_bytes, 
			     size_t total_bytes, 
			     char* output, 
			     size_t* output_length);
  /* Returns true on success */
  bool snappy_get_uncompressed_length(const char* compressed_data, size_t compressed_length, size_t* result);
  bool snappy_uncompress_bytes(const char* compressed_data, size_t compressed_length, void* output_bytes);
}
