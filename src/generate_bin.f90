! File     : generate_bin.f90
! Author   : Sandeep Koranne (C) 2026. All rights reserved.
! Puropose : Generate 3-4 boxes in bin as 4-points in int32
module CompressionChunkManagerModule
  use CommonModule
  use GeometryModule
  use SnappyCompressionModule
  use iso_c_binding, only: c_char
  use iso_fortran_env, only : int32, int64, real64
  !use snappy_mod, only: Box, compress_box_array, decompress_box_array
  implicit none

  ! ---------------------------------------------------------
  ! Hardware/R-Tree Tuning Constants
  ! ---------------------------------------------------------
  integer(kind=int64), parameter :: LEAVES_PER_CHUNK = 4096_int64
  integer(kind=int64), parameter :: BOXES_PER_CHUNK  = K_LEAF_CAPACITY * LEAVES_PER_CHUNK

  ! ---------------------------------------------------------
  ! 1. The Individual Chunk Data Structure
  ! ---------------------------------------------------------
  type :: Chunk
     integer(kind=int64) :: num_boxes          ! Exact number of boxes in this chunk
     integer(kind=int64) :: compressed_size    ! Size of the byte array
     character(kind=c_char), allocatable :: data(:) ! The Snappy compressed bytes
  end type Chunk

  ! ---------------------------------------------------------
  ! 2. The Master 'CompressedChunks' Structure
  ! ---------------------------------------------------------
  type :: CompressedChunks
     integer(kind=int64) :: total_boxes
     integer(kind=int64) :: num_chunks
     type(Chunk), allocatable :: chunks(:)
  end type CompressedChunks

  ! ---------------------------------------------------------
  ! Interfaces to match your required API
  ! ---------------------------------------------------------
  interface compress
     module procedure compress_to_chunks
  end interface compress

  interface decompress
     module procedure decompress_from_chunks
  end interface decompress

contains

  ! ---------------------------------------------------------
  ! COMPRESS: Takes a massive Box array and populates 'cchunk'
  ! ---------------------------------------------------------
  subroutine compress_to_chunks(boxes, cchunk)
    ! Note: contiguous allows us to safely slice and pass to Snappy
    type(Box), intent(in), target, contiguous :: boxes(:)
    type(CompressedChunks), intent(inout) :: cchunk

    integer(kind=int64) :: current_start, current_end, i

    cchunk%total_boxes = size(boxes, kind=8)

    ! Integer math trick to calculate the exact number of chunks needed
    ! (Equivalent to ceiling(total_boxes / BOXES_PER_CHUNK))
    cchunk%num_chunks = (cchunk%total_boxes + BOXES_PER_CHUNK - 1_int64) / BOXES_PER_CHUNK

    allocate(cchunk%chunks(cchunk%num_chunks))

    current_start = 1_int64
    !$komp parallel do default(none) &
    !$komp shared(cchunk, boxes) &
    !$komp private(i, current_start, current_end)
    do i = 1_int64, cchunk%num_chunks
       ! Ensure the last chunk doesn't overrun the array bounds
       current_end = min(current_start + BOXES_PER_CHUNK - 1_int64, cchunk%total_boxes)

       cchunk%chunks(i)%num_boxes = current_end - current_start + 1_int64

       ! Pass just the slice into your snappy_mod
       cchunk%chunks(i)%data = compress_box_array(boxes(current_start:current_end))
       cchunk%chunks(i)%compressed_size = size(cchunk%chunks(i)%data, kind=int64)

       current_start = current_end + 1_int64
    end do
    !$komp end parallel do
  end subroutine compress_to_chunks

  ! ---------------------------------------------------------
  ! DECOMPRESS: Takes 'cchunk' and restores the allocatable Box array
  ! ---------------------------------------------------------
  subroutine decompress_from_chunks(cchunk, boxes)
    type(CompressedChunks), intent(in) :: cchunk
    type(Box), allocatable, intent(out) :: boxes(:)
    integer(kind=int64) :: current_start, current_end, i
    type(Box), allocatable :: temp_boxes(:)

    ! 1. Allocate the master Fortran array to exactly the right size
    allocate(boxes(cchunk%total_boxes))
    allocate( temp_boxes( BOXES_PER_CHUNK ) )
    current_start = 1_int64

    ! 2. Loop through the memory chunks, decompressing sequentially
    !$komp parallel private(i, current_start)
    !$komp do
    do i = 1_int64, cchunk%num_chunks      
       current_start = ((i - 1_int64) * BOXES_PER_CHUNK) + 1_int64
       current_end = current_start + cchunk%chunks(i)%num_boxes - 1_int64
       if ( (current_end - current_start + 1_int64) /= cchunk%chunks(i)%num_boxes ) then
          print *, "CRITICAL MISMATCH at chunk ", i
       end if
       ! Decompress just this specific chunk into a temporary array
       call decompress_box_array(cchunk%chunks(i)%data, temp_boxes)
       ! Copy the uncompressed data directly into its place in the master array
       boxes(current_start:current_end) = temp_boxes(1:cchunk%chunks(i)%num_boxes)
    end do
    deallocate( temp_boxes )    
    !$komp end do
    !$komp end parallel
  end subroutine decompress_from_chunks

end module CompressionChunkManagerModule

module SimpleTest
  use GeometryModule
  use BoxByteStreamModule
  use BoxCompressionModule
  use MortonSortModule
  use KLDataModule
  use iso_fortran_env, only : int32, int64, real64
contains  
  subroutine Test1()
    !use HDFDataModule
    implicit none
    type(Box),allocatable :: arr(:)
    type(BoxByteStream) :: stream
    integer(kind=int64) :: num_boxes, pos,i
    logical :: ok
    pos = 1
    arr(1) = Box(0,0,2,2)
    arr(2) = Box(2,0,4,2)
    arr(3) = Box(4,0,6,2)
    stream = compress_boxes(arr)
    write(*,*) stream%data
    num_boxes = get_number_boxes( stream, pos, ok, .false. ) !> not scanning yet
    if( .not. ok .or. num_boxes == 0) then
       error stop "Decoding failed."
    end if
    write(*,*) '+++++ Number Boxes in Stream : ', num_boxes, '++++++'
    num_boxes = get_number_boxes( stream, pos, ok, .true. ) !> scanning now
    do i=1,num_boxes
       call decompress_box_stream( stream, pos, arr(i), ok )
       write(*,*) i, ' ', arr(i)
       if( .not. ok .or. .not. arr(i)%is_valid()) then
          error stop "BOX reading failed."
       end if
    end do
    write(*,*) '+++++ Number Boxes in Stream : ', num_boxes, '++++++'
    write(*,*) arr
    write(*,*) '+++++'  
    call WriteKLBin( "x.bin", arr, size(arr) )
  end subroutine Test1
end module SimpleTest

module BoxCompressionTest
  use GeometryModule
  use BoxByteStreamModule
  use BoxCompressionModule
  use KLDataModule
  use MortonSortModule
  use SnappyCompressionModule
  use iso_fortran_env, only : int32, int64, real64
  implicit none
contains
  subroutine TestBoxCompression()
    !use HDFDataModule
    implicit none
    type(Box),allocatable :: arr(:), readback(:)
    type(Box) :: bbox
    type(BoxByteStream) :: stream
    integer(kind=int64) :: num_boxes, pos,i
    logical :: ok
    pos = 1_int64
    call LoadKLBin("b.bin", arr)
    bbox = mbr_of_array( arr, size(arr) )
    write(*,*) 'Loaded : ', size(arr), ' BBOX = ', bbox
    write(*,'(4I12)') arr(1:10)  
    !call quicksort_boxes( arr, 1, size(arr) )
    call MortonSort( arr )
    write(*,*) 'Sorting complete: '
    write(*,'(4I12)') arr(1:10)
    !call WriteKLBin( "x.bin", arr(1:1000),1000 )
    !stop
    stream = compress_boxes(arr)
    write(*,*) 'Stream size = ', size( stream%data,kind=int64 )
    num_boxes = get_number_boxes( stream, pos, ok, .false. ) !> not scanning yet
    if( .not. ok .or. num_boxes == 0) then
       error stop "Decoding failed."
    end if
    write(*,*) '+++++ Number Boxes in Stream : ', num_boxes, '++++++'
    num_boxes = get_number_boxes( stream, pos, ok, .true. ) !> scanning now
    allocate(readback(num_boxes))
    do i=1,num_boxes
       call decompress_box_stream( stream, pos, readback(i), ok )
       if( .not. ok .or. .not. readback(i)%is_valid()) then
          error stop "BOX reading failed."
       end if
    end do
    write(*,*) '+++++ Number Boxes in Stream : ', num_boxes, '++++++'
    !write(*,*) arr
    write(*,*) '+++++'
    if( size(arr,kind=int64) /= size(readback,kind=int64) ) then
       error stop "SIZES do NOT match"
    end if
    do i=1,num_boxes
       if( arr(i) == readback(i) ) then
       else
          error stop "CONTENTS do NOT match"
       end if
    end do
    ! calculate the compression ratio
    write(*,*) 'Number of boxes: ', size(arr), ' took ', size(stream%data,kind=int64), ' ratio = ', &
         size(stream%data,kind=int64)*1.0/size(arr,kind=int64), ' bytes per box.'
  end subroutine TestBoxCompression
end module BoxCompressionTest

module SnappyCompressionTest
  use GeometryModule
  use BoxByteStreamModule
  use BoxCompressionModule
  use KLDataModule
  use MortonSortModule
  use SnappyCompressionModule
  use CompressionChunkManagerModule
  use iso_fortran_env, only : int32, int64, real64
  use iso_c_binding
  implicit none
contains
  subroutine TestSnappyCompression()
    !use HDFDataModule
    implicit none
    type(CompressedChunks) :: chunk_manager
    type(Box),allocatable :: original_boxes(:)
    type(Box) :: bbox
    type(BoxByteStream) :: stream
    integer(kind=int64) :: num_boxes, pos,i
    logical :: ok
    type(Box), allocatable :: restored_boxes(:)
    character(kind=c_char), allocatable :: compressed_stream(:)
    logical :: match
    real(kind=8), parameter :: BYTES_PER_GB  = 1000000000.0_8
    real(kind=8), parameter :: BYTES_PER_GIB = 1073741824.0_8
    real(kind=real64) :: size_in_gb, total_bytes
    pos = 1_int64
    call LoadKLBin("b.bin", original_boxes)
    num_boxes = size( original_boxes, kind=int64 )
    total_bytes = num_boxes * 16_8
    size_in_gb = total_bytes/BYTES_PER_GIB
    bbox = mbr_of_array( original_boxes, num_boxes )
    write(*,*) 'Loaded : ', num_boxes, ' BBOX = ', bbox, ' ', size_in_gb, ' Gb of data'
    write(*,'(4I12)') original_boxes(1:10)  
    !call quicksort_boxes( arr, 1, size(arr) )
    call MortonSort( original_boxes )
    !call omt_pack( original_boxes , K_LEAF_CAPACITY )
    write(*,*) 'Sorting complete: '
    write(*,'(4I12)') original_boxes(1:10)
    call compress_to_chunks( original_boxes, chunk_manager )
    write(*,*) 'Compressed to ', chunk_manager%num_chunks, ' chunks, total_size = ', sum( chunk_manager%chunks(:)%compressed_size )
    !compressed_stream = compress_box_array(original_boxes)
    call decompress_from_chunks( chunk_manager, restored_boxes )
    ! 3. Decompress (Dynamic allocation occurs inside the module function)
    print *, "Restored elements:     ", size(restored_boxes)

    ! 4. Validate Integrity
    match = .true.
    do i = 1, num_boxes
       if (original_boxes(i)%x1 /= restored_boxes(i)%x1 .or. &
            original_boxes(i)%y1 /= restored_boxes(i)%y1 .or. &
            original_boxes(i)%x2 /= restored_boxes(i)%x2 .or. &
            original_boxes(i)%y2 /= restored_boxes(i)%y2) then
          match = .false.
          exit
       end if
    end do
    if (match) then
       print *, "Validation RESULT: SUCCESS! Data perfectly restored."
    else
       print *, "Validation RESULT: FAILURE! Corrupted data encountered."
    end if

  end subroutine TestSnappyCompression
end module SnappyCompressionTest


program main
  use SnappyCompressionTest
  call TestSnappyCompression()
end program main
