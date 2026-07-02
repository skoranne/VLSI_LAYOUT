! File     : generate_bin.f90
! Author   : Sandeep Koranne (C) 2026. All rights reserved.
! Puropose : Generate 3-4 boxes in bin as 4-points in int32
module SimpleTest
   use GeometryModule
   use BoxByteStreamModule
   use BoxCompressionModule
   use SerializationModule
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
      call WriteKLBin( "x.bin", arr, int( size(arr),kind=int64) )
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
      bbox = mbr_of_array( arr, int( size(arr), kind=int64) )
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
   use LayoutStatisticsModule
   use SerializationModule
   use BoxCodecModule
   use iso_fortran_env, only : int8, int32, int64, real64
   use iso_c_binding
   implicit none
contains
   subroutine TestSnappyCompression(fileName, out_filename)
      !use HDFDataModule
      implicit none
      character(*), intent(in) :: fileName
      character(*), intent(in) :: out_filename
      type(CompressedChunks) :: chunk_manager
      type(Box),allocatable :: original_boxes(:)
      type(Box) :: bbox
      type(BoxByteStream) :: stream
      integer(kind=int64) :: num_boxes, pos,i
      logical :: ok
      type(Box), allocatable :: restored_boxes(:), decompressed_boxes(:)
      character(kind=c_char), allocatable :: compressed_stream(:)
      logical :: match
      real(kind=8), parameter :: BYTES_PER_GB  = 1000000000.0_8
      real(kind=8), parameter :: BYTES_PER_GIB = 1073741824.0_8
      real(kind=real64) :: size_in_gb, total_bytes
      type(BoxCodec) :: codec_state
      integer(int8), allocatable :: out_stream(:)
      integer(int64) :: bytes_written, snappy_bytes_total
      type(CompressedStream) :: snappy_stream
      pos = 1_int64
      call LoadKLBin(fileName, original_boxes)
      num_boxes = size( original_boxes, kind=int64 )
      total_bytes = num_boxes * 16_8
      size_in_gb = total_bytes/BYTES_PER_GIB
      bbox = mbr_of_array( original_boxes, num_boxes )
      write(*,*) 'Loaded : ', num_boxes, ' BBOX = ', bbox, ' ', size_in_gb, ' Gb of data'
      write(*,'(4I12)') original_boxes(1:5)
      !call quicksort_boxes( arr, 1, size(arr) )
      call MortonSort( original_boxes )
      !call omt_pack( original_boxes , K_LEAF_CAPACITY )
      write(*,*) 'Sorting complete: '
      write(*,'(4I12)') original_boxes(1:5)
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
         error stop "ERROR: DATA CORRUPTION"
      end if
      call analyze_boxes( original_boxes )
      !> try this byte stream compression as well
      !>call CompressBoxesUsingCodec(original_boxes, codec_state, out_stream, bytes_written)
      !>write(*,*) 'Codec wrote: ', bytes_written
      call CompressBoxesUsingCodec( original_boxes, codec_state, out_stream, bytes_written)
      write(*,*) 'Codec wrote: ', bytes_written
      allocate( decompressed_boxes( num_boxes ) )
      call DecompressBoxesUsingCodec( out_stream, bytes_written, num_boxes, decompressed_boxes, codec_state )
      match = .true.
      do i = 1, num_boxes
         if (original_boxes(i)%x1 /= decompressed_boxes(i)%x1 .or. &
            original_boxes(i)%y1 /= decompressed_boxes(i)%y1 .or. &
            original_boxes(i)%x2 /= decompressed_boxes(i)%x2 .or. &
            original_boxes(i)%y2 /= decompressed_boxes(i)%y2) then
            match = .false.
            exit
         end if
      end do
      if (match) then
         print *, "Validation RESULT: SUCCESS! CODEC Data perfectly restored."
      else
         print *, "Validation RESULT: FAILURE! CODEC Corrupted data encountered."
         error stop "ERROR: DATA CORRUPTION"
      end if
      deallocate( decompressed_boxes )

      !> compression
      call CompressBoxesToStream( original_boxes, snappy_stream, 1) !> since this SNAPPY test
      snappy_bytes_total = 0
      do i = 1, size(snappy_stream%chunks)
         snappy_bytes_total = snappy_bytes_total + snappy_stream%chunks(i)%compressed_size
      end do
      write(*,*) 'Stream ', size(snappy_stream%chunks), ' chunks, ', snappy_bytes_total
      call DecompressStreamToBoxes( snappy_stream, decompressed_boxes)
      if( num_boxes /= snappy_stream%total_boxes) error stop "ERROR: Stream decomp failed."
      match = .true.
      do i = 1, num_boxes
         if (original_boxes(i)%x1 /= decompressed_boxes(i)%x1 .or. &
            original_boxes(i)%y1 /= decompressed_boxes(i)%y1 .or. &
            original_boxes(i)%x2 /= decompressed_boxes(i)%x2 .or. &
            original_boxes(i)%y2 /= decompressed_boxes(i)%y2) then
            match = .false.
            exit
         end if
      end do
      if (match) then
         print *, "Validation RESULT: SUCCESS! SNAPPY Decomp Data perfectly restored."
      else
         print *, "Validation RESULT: FAILURE! SNAPPY Decomp Corrupted data encountered."
         error stop "ERROR: DATA CORRUPTION"
      end if
      !> file level serialization
      call SaveCompressedStreamToDisk( out_filename, snappy_stream )
   end subroutine TestSnappyCompression

   subroutine TestSnappyDecompression(fileName, snapfilename)
      implicit none
      character(*), intent(in) :: fileName
      character(*), intent(in) :: snapfilename
      type(CompressedChunks) :: chunk_manager
      type(Box),allocatable :: original_boxes(:)
      type(Box) :: bbox
      type(BoxByteStream) :: stream
      integer(kind=int64) :: num_boxes, pos,i
      logical :: ok
      type(Box), allocatable :: restored_boxes(:), decompressed_boxes(:)
      character(kind=c_char), allocatable :: compressed_stream(:)
      logical :: match
      real(kind=8), parameter :: BYTES_PER_GB  = 1000000000.0_8
      real(kind=8), parameter :: BYTES_PER_GIB = 1073741824.0_8
      real(kind=real64) :: size_in_gb, total_bytes
      type(BoxCodec) :: codec_state
      integer(int8), allocatable :: out_stream(:)
      integer(int64) :: bytes_written, snappy_bytes_total
      type(CompressedStream) :: snappy_stream
      pos = 1_int64
      call LoadKLBin(fileName, original_boxes)
      call MortonSort( original_boxes ) !> otherwise SNAP wont match
      num_boxes = size( original_boxes, kind=int64 )
      call RestoreCompressedStreamFromDisk( snapfilename, snappy_stream )
      call DecompressStreamToBoxes( snappy_stream, decompressed_boxes)
      if( num_boxes /= snappy_stream%total_boxes) error stop "ERROR: Stream decomp failed."
      match = .true.
      do i = 1, num_boxes
         if (original_boxes(i)%x1 /= decompressed_boxes(i)%x1 .or. &
            original_boxes(i)%y1 /= decompressed_boxes(i)%y1 .or. &
            original_boxes(i)%x2 /= decompressed_boxes(i)%x2 .or. &
            original_boxes(i)%y2 /= decompressed_boxes(i)%y2) then
            match = .false.
            exit
         end if
      end do
      if (match) then
         print *, "Validation RESULT: SUCCESS! SNAP FILE Data perfectly restored."
      else
         print *, "Validation RESULT: FAILURE! SNAP FILE Corrupted data encountered."
         error stop "ERROR: DATA CORRUPTION"
      end if

   end subroutine TestSnappyDecompression
end module SnappyCompressionTest

program main
   use SnappyCompressionTest
   use SerializationModule
   implicit none
   integer :: narg, iostat
   character(len=256)            :: filenameA, filenameB
   narg = command_argument_count()
   select case (narg)
    case (0)
      error stop "./GEN_BIN BIN-FILE BIN-SNAP-FILE"
    case (2)
      call get_command_argument(1, filenameA, status=iostat)   ! allocates automatically
      if (iostat /= 0) then
         write (*,*) "ERROR: 1st argument must be a filename."
         stop 2
      end if
      call get_command_argument(2, filenameB, status=iostat)   ! allocates automatically
      if (iostat /= 0) then
         write (*,*) "ERROR: 2nd argument must be a filename."
         stop 2
      end if

      write (*,*) 'Reading 1st filename: ', trim(filenameA)
      write (*,*) 'Writing 2nd filename: ', trim(filenameB)
      !call TestSnappyCompression( trim(filenameA), trim(filenameB) )
      !call TestSnappyDecompression( trim(filenameA), trim(filenameB) )
      call AnalyzeSnapFile( filenameB )
   end select
end program main
