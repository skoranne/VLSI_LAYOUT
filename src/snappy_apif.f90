! File    : snappy_apif.f90
! Author  : Sandeep Koranne (C) 2026.
! Purpose : Use lossless compression API
module SnappyCompressionModule
  use iso_c_binding, only: c_int32_t, c_size_t, c_char, c_bool, c_ptr, c_loc, c_int, c_long, c_long_long
  use iso_fortran_env, only: int8, int32, int64
  use CommonModule
  use GeometryModule
  implicit none

  ! ---------------------------------------------------------
  ! C++ Interfaces
  ! ---------------------------------------------------------
  interface
     function c_max_compressed_length(input_bytes) bind(C, name="snappy_max_compressed_length")
       import :: c_size_t
       integer(c_size_t), value :: input_bytes
       integer(c_size_t) :: c_max_compressed_length
     end function c_max_compressed_length

     subroutine c_compress_bytes(input_bytes, total_bytes, output, output_length) bind(C, name="snappy_compress_bytes")
       import :: c_ptr, c_size_t, c_char
       ! CRITICAL FIX 1: Must be scalar (no (*)) and passed by value (no intent)
       type(c_ptr), value :: input_bytes                 
       integer(c_size_t), value :: total_bytes
       character(kind=c_char), intent(out) :: output(*)
       integer(c_size_t), intent(inout) :: output_length
     end subroutine c_compress_bytes

     function c_get_uncompressed_length(compressed_data, compressed_length, result_bytes) bind(C, name="snappy_get_uncompressed_length")
       import :: c_char, c_size_t, c_bool
       character(kind=c_char), intent(in) :: compressed_data(*)
       integer(c_size_t), value :: compressed_length
       integer(c_size_t), intent(out) :: result_bytes
       logical(c_bool) :: c_get_uncompressed_length
     end function c_get_uncompressed_length

     function c_uncompress_bytes(compressed_data, compressed_length, output_bytes) bind(C, name="snappy_uncompress_bytes")
       import :: c_char, c_size_t, c_bool, c_ptr
       character(kind=c_char), intent(in) :: compressed_data(*)
       integer(c_size_t), value :: compressed_length
       ! CRITICAL FIX 2: Must be scalar (no (*)) and passed by value (no intent)
       type(c_ptr), value :: output_bytes                
       logical(c_bool) :: c_uncompress_bytes
     end function c_uncompress_bytes

     ! ---------------------------------------------------------
     ! ZLIB Native C Interfaces (zlib.h)
     ! ---------------------------------------------------------

     ! Equivalent to: uLong compressBound(uLong sourceLen);
     function zlib_compress_bound(source_len) bind(C, name="compressBound")
       import :: c_long
       integer(c_long), value :: source_len
       integer(c_long) :: zlib_compress_bound
     end function zlib_compress_bound

     ! Equivalent to: int compress(Bytef *dest, uLongf *destLen, const Bytef *source, uLong sourceLen);
     function zlib_compress(dest, dest_len, source, source_len) bind(C, name="compress")
       import :: c_ptr, c_long, c_int
       ! CRITICAL FIX: Passed by value as c_ptr to avoid array descriptors
       type(c_ptr), value :: dest
       integer(c_long), intent(inout) :: dest_len
       type(c_ptr), value :: source
       integer(c_long), value :: source_len
       integer(c_int) :: zlib_compress
     end function zlib_compress

     ! Equivalent to: int uncompress(Bytef *dest, uLongf *destLen, const Bytef *source, uLong sourceLen);
     function zlib_uncompress(dest, dest_len, source, source_len) bind(C, name="uncompress")
       import :: c_ptr, c_long, c_int
       ! CRITICAL FIX: Passed by value as c_ptr
       type(c_ptr), value :: dest
       integer(c_long), intent(inout) :: dest_len
       type(c_ptr), value :: source
       integer(c_long), value :: source_len
       integer(c_int) :: zlib_uncompress
     end function zlib_uncompress

     ! ---------------------------------------------------------
     ! ZSTD Native C Interfaces (zstd.h)
     ! ---------------------------------------------------------
     ! Equivalent to: size_t ZSTD_compressBound(size_t srcSize);
     function zstd_compress_bound(src_size) bind(C, name="ZSTD_compressBound")
       import :: c_size_t
       integer(c_size_t), value :: src_size
       integer(c_size_t) :: zstd_compress_bound
     end function zstd_compress_bound

     ! Equivalent to: size_t ZSTD_compress(void* dst, size_t dstCapacity, const void* src, size_t srcSize, int compressionLevel);
     function zstd_compress(dst, dst_capacity, src, src_size, comp_level) bind(C, name="ZSTD_compress")
       import :: c_ptr, c_size_t, c_int
       ! CRITICAL FIX: Passed by value as c_ptr
       type(c_ptr), value :: dst
       integer(c_size_t), value :: dst_capacity
       type(c_ptr), value :: src
       integer(c_size_t), value :: src_size
       integer(c_int), value :: comp_level
       integer(c_size_t) :: zstd_compress
     end function zstd_compress

     ! Equivalent to: unsigned long long ZSTD_getFrameContentSize(const void *src, size_t srcSize);
     function zstd_get_uncompressed_length(src, src_size) bind(C, name="ZSTD_getFrameContentSize")
       import :: c_ptr, c_size_t, c_long_long
       type(c_ptr), value :: src
       integer(c_size_t), value :: src_size
       integer(c_long_long) :: zstd_get_uncompressed_length
     end function zstd_get_uncompressed_length

     ! Equivalent to: size_t ZSTD_decompress(void* dst, size_t dstCapacity, const void* src, size_t compressedSize);
     function zstd_decompress(dst, dst_capacity, src, compressed_size) bind(C, name="ZSTD_decompress")
       import :: c_ptr, c_size_t
       ! CRITICAL FIX: Passed by value as c_ptr
       type(c_ptr), value :: dst
       integer(c_size_t), value :: dst_capacity
       type(c_ptr), value :: src
       integer(c_size_t), value :: compressed_size
       integer(c_size_t) :: zstd_decompress
     end function zstd_decompress

     ! Equivalent to: unsigned ZSTD_isError(size_t code);
     function zstd_is_error(code) bind(C, name="ZSTD_isError")
       import :: c_size_t, c_int
       integer(c_size_t), value :: code
       integer(c_int) :: zstd_is_error
     end function zstd_is_error

  end interface

contains

  ! ---------------------------------------------------------
  ! High-Level Compression Wrapper
  ! ---------------------------------------------------------
  function compress_box_array(boxes) result(compressed_bytes)
    ! TARGET and CONTIGUOUS allow c_loc() to safely grab the memory address
    type(Box), intent(in), target, contiguous :: boxes(:) 

    character(kind=c_char), allocatable :: compressed_bytes(:)
    character(kind=c_char), allocatable :: temp_buffer(:)
    integer(c_size_t) :: total_input_bytes, max_out_bytes, actual_out_bytes

    total_input_bytes = size(boxes, kind=c_size_t) * storage_size(boxes(1), kind=c_size_t) / 8_c_size_t
    max_out_bytes = c_max_compressed_length(total_input_bytes)
    allocate(compressed_bytes(max_out_bytes))

    actual_out_bytes = max_out_bytes

    ! Pass the raw pointer value of the first element
    call c_compress_bytes(c_loc(boxes(1)), total_input_bytes, compressed_bytes, actual_out_bytes)

    if (actual_out_bytes < max_out_bytes) then
       allocate(temp_buffer(actual_out_bytes))
       temp_buffer(1:actual_out_bytes) = compressed_bytes(1:actual_out_bytes)
       call move_alloc(from=temp_buffer, to=compressed_bytes)
    end if
  end function compress_box_array

  ! ---------------------------------------------------------
  ! High-Level Decompression Wrapper
  ! ---------------------------------------------------------
  subroutine decompress_box_array(compressed_bytes, output_buffer)
    character(kind=c_char), intent(in) :: compressed_bytes(:)
    ! TARGET allows c_loc() to safely grab the memory address
    type(Box), intent(inout), target :: output_buffer(:) 
    integer(c_size_t) :: comp_length, uncomp_bytes, num_boxes, box_storage_bytes
    logical(c_bool) :: success
    type(Box) :: dummy_box
    comp_length = size(compressed_bytes, kind=c_size_t)
    success = c_get_uncompressed_length(compressed_bytes, comp_length, uncomp_bytes)
    if (.not. success) error stop "Error: Failed to parse Snappy compressed header!"
    box_storage_bytes = storage_size(dummy_box, kind=c_size_t) / 8_c_size_t
    num_boxes = uncomp_bytes / box_storage_bytes
    ! Pass the raw pointer value of the first allocated element
    success = c_uncompress_bytes(compressed_bytes, comp_length, c_loc(output_buffer(1)))
    if (.not. success) error stop "Error: Snappy data decompression failed!"
  end subroutine decompress_box_array

  ! ==================================================================
  ! ZERO-ALLOCATION SNAPPY WRAPPERS
  ! ==================================================================

  subroutine snappy_compress_buffer(input, in_len, output, out_len, actual_len)
    use iso_c_binding, only: c_size_t, c_loc, c_char, c_ptr, c_f_pointer
    implicit none

    integer(int8), intent(in), target    :: input(:)
    integer(int64), intent(in)           :: in_len
    integer(int8), intent(inout), target :: output(:)  ! Pre-allocated thread buffer
    integer(int64), intent(in)           :: out_len    ! Total capacity of output
    integer(int64), intent(out)          :: actual_len ! Bytes actually written

    integer(c_size_t) :: c_in_len, c_out_len
    character(kind=c_char), pointer :: char_output(:)

    c_in_len = int(in_len, c_size_t)
    c_out_len = int(out_len, c_size_t)

    ! Zero-copy type map from int8 to c_char array
    call c_f_pointer(c_loc(output), char_output, [c_out_len])

    ! Call C function directly. c_loc(input) provides the required c_ptr
    call c_compress_bytes(c_loc(input), c_in_len, char_output, c_out_len)

    actual_len = int(c_out_len, int64)
  end subroutine snappy_compress_buffer


  subroutine snappy_uncompress_buffer(input, in_len, output, out_len, status)
    use iso_c_binding, only: c_size_t, c_loc, c_char, c_ptr, c_f_pointer, c_bool
    implicit none

    integer(int8), intent(in), target    :: input(:)
    integer(int64), intent(in)           :: in_len
    integer(int8), intent(inout), target :: output(:)  ! Pre-allocated thread buffer
    integer(int64), intent(in)           :: out_len    ! Expected uncompressed size
    integer :: status

    integer(c_size_t) :: c_in_len
    character(kind=c_char), pointer :: char_input(:)
    logical(c_bool) :: success

    c_in_len = int(in_len, c_size_t)
    ! Zero-copy type map from int8 to c_char array
    call c_f_pointer(c_loc(input), char_input, [c_in_len])

    ! Call C function directly. c_loc(output) provides the required c_ptr
    success = c_uncompress_bytes(char_input, c_in_len, c_loc(output))
    !if (size(output, kind=int64) < expected_output_length) then
    !   print *, "CRITICAL: Allocated output buffer is too small!"
    !   stop 1
    !end if

    if (success) then
       status = 0
    else
       status = 2 ! Corruption
    end if
  end subroutine snappy_uncompress_buffer
  subroutine zlib_compress_buffer(input, in_len, output, out_len, actual_len)
    use iso_c_binding, only: c_long, c_loc, c_int
    implicit none

    integer(int8), intent(in), target    :: input(:)
    integer(int64), intent(in)           :: in_len
    integer(int8), intent(inout), target :: output(:)  ! Pre-allocated thread buffer
    integer(int64), intent(in)           :: out_len    ! Total capacity of output
    integer(int64), intent(out)          :: actual_len ! Bytes actually written

    integer(c_long) :: c_in_len, c_out_len
    integer(c_int)  :: z_status

    ! ZLIB uses c_long (uLong) for sizes, not c_size_t
    c_in_len = int(in_len, c_long)
    c_out_len = int(out_len, c_long)

    ! Call ZLIB directly. c_loc handles the int8 pointer automatically.
    ! Note: c_out_len is passed by reference (intent inout in the interface) 
    ! so ZLIB can update it with the final compressed size.
    z_status = zlib_compress(c_loc(output), c_out_len, c_loc(input), c_in_len)

    ! Z_OK is 0 in zlib.h
    if (z_status == 0) then
       actual_len = int(c_out_len, int64)
    else
       ! Handle error (e.g., Z_MEM_ERROR or Z_BUF_ERROR if output is too small)
       actual_len = 0 
    end if
  end subroutine zlib_compress_buffer
  subroutine zlib_uncompress_buffer(input, in_len, output, out_len, status)
    use iso_c_binding, only: c_long, c_loc, c_int
    implicit none

    integer(int8), intent(in), target    :: input(:)
    integer(int64), intent(in)           :: in_len
    integer(int8), intent(inout), target :: output(:)  ! Pre-allocated thread buffer
    integer(int64), intent(in)           :: out_len    ! Expected uncompressed size
    integer, intent(out)                 :: status

    integer(c_long) :: c_in_len, c_out_len
    integer(c_int)  :: z_status

    c_in_len = int(in_len, c_long)
    c_out_len = int(out_len, c_long)

    ! Call ZLIB directly.
    z_status = zlib_uncompress(c_loc(output), c_out_len, c_loc(input), c_in_len)

    ! Z_OK is 0 in zlib.h
    if (z_status == 0) then
       status = 0
    else
       status = 2 ! Emulating your Snappy 'Corruption' status code
    end if
  end subroutine zlib_uncompress_buffer
  subroutine zstd_compress_buffer(input, in_len, output, out_len, actual_len)
    use iso_c_binding, only: c_size_t, c_loc, c_int
    implicit none

    integer(int8), intent(in), target    :: input(:)
    integer(int64), intent(in)           :: in_len
    integer(int8), intent(inout), target :: output(:)  ! Pre-allocated thread buffer
    integer(int64), intent(in)           :: out_len    ! Total capacity of output
    integer(int64), intent(out)          :: actual_len ! Bytes actually written

    integer(c_size_t) :: c_in_len, c_out_len, z_result
    integer(c_int), parameter :: comp_level = 20 ! Standard default compression level

    c_in_len = int(in_len, c_size_t)
    c_out_len = int(out_len, c_size_t)

    ! Call ZSTD directly. c_loc handles the int8 pointer automatically.
    z_result = zstd_compress(c_loc(output), c_out_len, c_loc(input), c_in_len, comp_level)

    ! ZSTD returns the actual compressed size, OR an error code. 
    ! We must use ZSTD_isError to check which one it is.
    if (zstd_is_error(z_result) /= 0) then
       actual_len = 0 ! Or handle specific error
    else
       actual_len = int(z_result, int64)
    end if
  end subroutine zstd_compress_buffer
  subroutine zstd_uncompress_buffer(input, in_len, output, out_len, status)
    use iso_c_binding, only: c_size_t, c_loc
    implicit none

    integer(int8), intent(in), target    :: input(:)
    integer(int64), intent(in)           :: in_len
    integer(int8), intent(inout), target :: output(:)  ! Pre-allocated thread buffer
    integer(int64), intent(in)           :: out_len    ! Expected uncompressed size
    integer, intent(out)                 :: status

    integer(c_size_t) :: c_in_len, c_out_len, z_result

    c_in_len = int(in_len, c_size_t)
    c_out_len = int(out_len, c_size_t)

    ! Call ZSTD directly. 
    z_result = zstd_decompress(c_loc(output), c_out_len, c_loc(input), c_in_len)

    ! Check if the returned size_t represents an error code
    if (zstd_is_error(z_result) /= 0) then
       status = 2 ! Emulating your Snappy 'Corruption' status code
    else
       status = 0
    end if
  end subroutine zstd_uncompress_buffer
end module SnappyCompressionModule
