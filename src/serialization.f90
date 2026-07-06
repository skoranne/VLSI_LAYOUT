! File   : serialization.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: We have a compressed stream, put it to disk to restore somewhat/somewhere

module SerializationModule
   use CommonModule
   use GeometryModule
   use DesignModule
   use BoxCodecModule
   use iso_fortran_env
   implicit none
   public:: SaveCompressedStreamToDisk, RestoreCompressedStreamFromDisk, AnalyzeSnapFile, AnalyzeUnit
contains

   subroutine SaveCompressedStreamToDisk(fileName, in_stream)
      character(len=*), intent(in) :: fileName
      type(CompressedStream), intent(in) :: in_stream
      integer :: iunit, ios, i
      integer(int64), allocatable :: chunk_offsets(:)
      integer(int64) :: current_file_pos


      allocate(chunk_offsets(in_stream%num_chunks))

      open(newunit=iunit, file=fileName, status='replace', access='stream',form='unformatted', iostat=ios)
      if (ios /= 0) stop "Error opening file for writing."

      ! 1. Write the Stream metadata
      write(iunit) CODEC_VERSION
      write(iunit) in_stream%layer_properties
      write(iunit) in_stream%total_boxes, in_stream%num_chunks, in_stream%compression_method
      if( associated( in_stream%arr ) ) write(iunit) in_stream%arr
      ! 2. Loop through chunks and write them
      do i = 1, in_stream%num_chunks
         inquire(unit=iunit, pos=current_file_pos)
         chunk_offsets(i) = current_file_pos
         write(iunit) in_stream%chunks(i)%num_boxes, &
            in_stream%chunks(i)%raw_byte_size, &
            in_stream%chunks(i)%compressed_size
         ! Write the allocatable data array
         write(iunit) in_stream%chunks(i)%data
      end do
      ! At the very end, write the offset table so the reader can find it
      inquire(unit=iunit, pos=current_file_pos)
      write(iunit) chunk_offsets

      ! as the absolute last 8 bytes of the file, so a reader can seek to
      ! EOF - 8, read the table location, jump there, read the table,
      ! and then instantly seek to any chunk.
      write(iunit) current_file_pos
      flush(iunit)
      close(iunit)
      !call execute_command_line("sync")
   end subroutine SaveCompressedStreamToDisk

   subroutine RestoreCompressedStreamFromDisk(fileName, out_stream)
      character(len=*), intent(in) :: fileName
      type(CompressedStream), intent(out) :: out_stream
      integer :: iunit, ios, i, attempt
      character(len=256) :: error_msg
      logical :: is_open
      integer :: ghost_unit
      integer :: file_codec_version
      integer(kind=int64) :: filesize

      ! 1. Interrogate the compiler's internal registry
      inquire(file=trim(fileName), opened=is_open, number=ghost_unit)

      ! 2. If the compiler claims it's open, forcefully terminate the connection
      if (is_open) then
         print *, "WARNING: nvfortran RTL reports file is still open on unit ", ghost_unit
         print *, "Executing forced purge of unit ", ghost_unit, "..."
         close(ghost_unit)
      end if
      ! Attempt to open the file up to 5 times
      do attempt = 1, 5
         open(newunit=iunit, file=trim(fileName), status='old', access='stream', &
              iomsg=error_msg,form='unformatted', action='read',iostat=ios)         
         if (ios == 0) exit ! Success, break the loop
         ! If it failed, ask the OS to pause for a fraction of a second
         call execute_command_line("sleep 0.5")
      end do
      if (ios /= 0) then
        print *, "=================================================="
        print *, "CRITICAL I/O FAILURE"
        print *, "File: ", trim(fileName)
        print *, "IOSTAT Code: ", ios
        print *, "System Message: ", trim(error_msg)
        print *, "=================================================="
        stop 1
      end if
      if (ios /= 0) then
         write(*,*) 'ERROR: cannot open file: ', fileName, ' for reading.'
         stop "Error opening file for reading."
      end if
      filesize = 0      
      inquire(unit=iunit,size=filesize)
      ! 1. Read the Stream metadata
      if( filesize == 0 ) then
         out_stream%total_boxes = 0
         out_stream%num_chunks = 0
         return
      end if
      read(iunit) file_codec_version
      read(iunit) out_stream%layer_properties
      read(iunit) out_stream%total_boxes, out_stream%num_chunks, out_stream%compression_method
      if( iand( out_stream%layer_properties, LAYER_STATE_PNUM ) /= 0 ) then
         allocate( out_stream%arr( out_stream%total_boxes ) )
         read(iunit) out_stream%arr
      end if
      write(*,'(A,I3,A,I8,A,I14,A,I18,A,I1)') '|V| = ', file_codec_version, &
           '|L| = ', out_stream%layer_properties,&
           '|N| = ', out_stream%total_boxes, ' |C| = ', out_stream%num_chunks, ' |M| =', out_stream%compression_method
      ! 2. Allocate the chunks array
      allocate(out_stream%chunks(out_stream%num_chunks))

      ! 3. Loop through and restore each chunk
      do i = 1, out_stream%num_chunks
         read(iunit) out_stream%chunks(i)%num_boxes, &
            out_stream%chunks(i)%raw_byte_size, &
            out_stream%chunks(i)%compressed_size

         ! Allocate the data array based on the size just read
         allocate(out_stream%chunks(i)%data(out_stream%chunks(i)%compressed_size))

         ! Read the array data
         read(iunit) out_stream%chunks(i)%data
      end do

      close(iunit)
   end subroutine RestoreCompressedStreamFromDisk

   !> Function to analyze a on-disk file for CHUNK blocks
   subroutine read_offset_table(iunit, chunk_offsets, num_chunks, iostat)
      use iso_fortran_env, only: int64
      implicit none

      integer, intent(in) :: iunit
      integer(int64), allocatable, intent(out) :: chunk_offsets(:)
      integer(int64), intent(out) :: num_chunks
      integer, intent(out) :: iostat

      integer(int64) :: file_size_bytes
      integer(int64) :: table_pointer_pos
      integer(int64) :: table_start_pos
      integer(int64) :: table_byte_span

      ! 1. Query the OS for the exact file size in bytes
      inquire(unit=iunit, size=file_size_bytes)

      if (file_size_bytes < 8_int64) then
         print *, "CRITICAL: File is too small to be a valid layout stream."
         iostat = -1
         return
      end if

      ! 2. Calculate 1-based position for EOF - 8 bytes
      table_pointer_pos = file_size_bytes - 7_int64

      ! 3. Seek to EOF-8 and read the 64-bit integer that points to the table
      read(iunit, pos=table_pointer_pos, iostat=iostat) table_start_pos
      if (iostat /= 0) return

      ! Sanity check: The pointer must be mathematically valid
      if (table_start_pos < 1_int64 .or. table_start_pos >= table_pointer_pos) then
         print *, "CRITICAL: Invalid offset table pointer detected: ", table_start_pos
         iostat = -2
         return
      end if

      ! 4. Calculate the exact number of chunks
      table_byte_span = table_pointer_pos - table_start_pos

      ! Strict alignment check: The span must be perfectly divisible by 8 bytes
      if (mod(table_byte_span, 8_int64) /= 0_int64) then
         print *, "CRITICAL: Offset table byte span is not aligned to 64-bit bounds. File corrupted."
         iostat = -3
         return
      end if

      num_chunks = table_byte_span / 8_int64

      ! 5. Allocate the offset array dynamically
      if (allocated(chunk_offsets)) deallocate(chunk_offsets)
      allocate(chunk_offsets(num_chunks))

      ! 6. Seek directly to the table and read the entire array in one pass
      read(iunit, pos=table_start_pos, iostat=iostat) chunk_offsets
      if (iostat /= 0) then
         print *, "CRITICAL: Failed to read the chunk offset array."
      end if

   end subroutine read_offset_table
   subroutine read_single_chunk(iunit, target_chunk_index, chunk_offsets, out_stream)
      use iso_fortran_env, only: int64
      integer, intent(in) :: iunit
      integer(int64), intent(in) :: target_chunk_index
      integer(int64), intent(in) :: chunk_offsets(:)
      type(CompressedStream), intent(inout) :: out_stream

      integer(int64) :: target_pos

      ! 1. Get the exact byte offset for this chunk
      target_pos = chunk_offsets(target_chunk_index)

      ! 2. Jump to that position and read the metadata header
      read(iunit, pos=target_pos) out_stream%chunks(target_chunk_index)%num_boxes, &
         out_stream%chunks(target_chunk_index)%raw_byte_size, &
         out_stream%chunks(target_chunk_index)%compressed_size

      ! 3. Allocate the buffer based on the exact size
      if (allocated(out_stream%chunks(target_chunk_index)%data)) then
         deallocate(out_stream%chunks(target_chunk_index)%data)
      end if
      allocate(out_stream%chunks(target_chunk_index)%data(out_stream%chunks(target_chunk_index)%compressed_size))

      ! 4. Read the compressed data.
      ! (No pos= needed here, the file pointer automatically advanced past the metadata)
      read(iunit) out_stream%chunks(target_chunk_index)%data

    end subroutine read_single_chunk
    subroutine AnalyzeUnit( iunit )
      integer, intent(in) :: iunit
      integer             :: ios, i
      integer(int64), allocatable :: chunk_offsets(:)
      integer(int64) :: num_chunks
      integer(int64) :: target_pos, N, raw_byte_size, compressed_size
      call read_offset_table( iunit, chunk_offsets, num_chunks, ios)
      write(*,*) 'NUM CHUNKS = ', num_chunks
      do i=1,num_chunks
         target_pos = chunk_offsets(i)
         read(iunit, pos=target_pos) N, raw_byte_size, compressed_size
         write(*,'(I8,4(A,I8))') i,' ',chunk_offsets(i),' ',N,' ',raw_byte_size,' ',compressed_size
      end do
    end subroutine AnalyzeUnit
    
   subroutine AnalyzeSnapFile( fileName )
      character(len=*), intent(in) :: fileName
      integer :: iunit, ios, i
      integer(int64), allocatable :: chunk_offsets(:)
      integer(int64) :: num_chunks
      integer(int64) :: target_pos, N, raw_byte_size, compressed_size
      open(newunit=iunit, file=fileName, status='old', access='stream', form='unformatted', iostat=ios)
      if (ios /= 0) stop "Error opening file for reading."
      call AnalyzeUnit( iunit )
   end subroutine

end module SerializationModule
