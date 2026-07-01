! File   : serialization.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: We have a compressed stream, put it to disk to restore somewhat/somewhere

module SerializationModule
  use CommonModule
  use GeometryModule
  use BoxCodecModule
  use iso_fortran_env
  implicit none

contains

  subroutine SaveCompressedStreamToDisk(fileName, in_stream)
    character(len=*), intent(in) :: fileName
    type(CompressedStream), intent(in) :: in_stream
    integer :: iunit, ios, i

    open(newunit=iunit, file=fileName, status='replace', form='unformatted', iostat=ios)
    if (ios /= 0) stop "Error opening file for writing."

    ! 1. Write the Stream metadata
    write(iunit) in_stream%total_boxes, in_stream%num_chunks, in_stream%compression_method

    ! 2. Loop through chunks and write them
    do i = 1, in_stream%num_chunks
       write(iunit) in_stream%chunks(i)%num_boxes, &
            in_stream%chunks(i)%raw_byte_size, &
            in_stream%chunks(i)%compressed_size
       ! Write the allocatable data array
       write(iunit) in_stream%chunks(i)%data
    end do

    close(iunit)
  end subroutine SaveCompressedStreamToDisk

  subroutine RestoreCompressedStreamFromDisk(fileName, out_stream)
    character(len=*), intent(in) :: fileName
    type(CompressedStream), intent(out) :: out_stream
    integer :: iunit, ios, i

    open(newunit=iunit, file=fileName, status='old', form='unformatted', iostat=ios)
    if (ios /= 0) stop "Error opening file for reading."

    ! 1. Read the Stream metadata
    read(iunit) out_stream%total_boxes, out_stream%num_chunks, out_stream%compression_method
    write(*,'(A,I12,A,I8,A,I1)') '|N| = ', out_stream%total_boxes, ' |C| = ', out_stream%num_chunks, ' |M| =', out_stream%compression_method
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

end module SerializationModule
