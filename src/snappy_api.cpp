/*
 * File    : snappy_api.cpp
 * Author  : Sandeep Koranne (C) 2026.
 * Purpose : Use lossless compression for storage
 */
#include "snappy_api.h"
#include <snappy.h>

extern "C" {

    // 1. Get worst-case compressed size based on total input bytes
    size_t snappy_max_compressed_length(size_t input_bytes) {
        return snappy::MaxCompressedLength(input_bytes);
    }

    // 2. Generic compression function accepting raw bytes (void*)
    void snappy_compress_bytes(const void* input_bytes, 
                               size_t total_bytes, 
                               char* output, 
                               size_t* output_length) {
                                
        snappy::RawCompress(reinterpret_cast<const char*>(input_bytes), 
                            total_bytes, 
                            output, 
                            output_length);
    }
  // Parses the Snappy header to get the total original uncompressed size in bytes.
  // Returns true on success, false if the metadata is corrupted.
  bool snappy_get_uncompressed_length(const char* compressed_data, size_t compressed_length, size_t* result) {
    return snappy::GetUncompressedLength(compressed_data, compressed_length, result);
  }
  
  // Uncompresses the data directly into a destination memory block.
  // Returns true on success, false if decompression failed.
  bool snappy_uncompress_bytes(const char* compressed_data, size_t compressed_length, void* output_bytes) {
    return snappy::RawUncompress(compressed_data, compressed_length, reinterpret_cast<char*>(output_bytes));
  }
}
