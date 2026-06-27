! File    : snappy_apif.f90
! Author  : Sandeep Koranne (C) 2026.
! Purpose : Use lossless compression API
module SnappyCompressionModule
  use iso_c_binding, only: c_int32_t, c_size_t, c_char, c_bool, c_ptr, c_loc
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
  #define NEW_CODE
  #ifdef NEW_CODE
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
  #else
  function decompress_box_array(compressed_bytes) result(boxes)
    character(kind=c_char), intent(in) :: compressed_bytes(:)
    ! TARGET allows c_loc() to safely grab the memory address
    type(Box), allocatable,  target :: boxes(:)
    integer(c_size_t) :: comp_length, uncomp_bytes, num_boxes, box_storage_bytes
    logical(c_bool) :: success
    type(Box) :: dummy_box
    comp_length = size(compressed_bytes, kind=c_size_t)
    success = c_get_uncompressed_length(compressed_bytes, comp_length, uncomp_bytes)
    if (.not. success) error stop "Error: Failed to parse Snappy compressed header!"
    box_storage_bytes = storage_size(dummy_box, kind=c_size_t) / 8_c_size_t
    num_boxes = uncomp_bytes / box_storage_bytes
    allocate(boxes(num_boxes))
    ! Pass the raw pointer value of the first allocated element
    success = c_uncompress_bytes(compressed_bytes, comp_length, c_loc(boxes(1)))
    if (.not. success) error stop "Error: Snappy data decompression failed!"
  end function decompress_box_array
  #endif

end module SnappyCompressionModule
