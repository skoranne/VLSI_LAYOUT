!=====================================================================
! File   : kldata.f90
! Author : Sandeep Koranne (C) All rights reserved
!  Simple demo: store an array of Box objects in an HDF5 file and read it
!  back again.  Uses the HDF5‑Fortran API (module hdf5).
!=====================================================================
module KLDataModule
  use iso_fortran_env,only: int32, int64
  use GeometryModule
  implicit none
  private
  public :: LoadKLBin, WriteKLBin
contains
  subroutine LoadKLBin(fileName,boxes)
    character(len=*), intent(in)  :: filename
    type(Box), allocatable,intent(out) :: boxes(:)
    integer(kind=int64) :: file_bytes, total_boxes, i, dot_pos
    integer, parameter  :: BOX_SIZE_BYTES = 16 ! 4 coordinates * 4 bytes
    integer :: file_unit, io_status
    ! 1. Query the filesystem for the total file size in bytes
    inquire(file=trim(filename), size=file_bytes)
    if (file_bytes <= 0) then
       write(*,*) 'Error: KLBIN File ', trim(filename), ' is empty or does not exist.'
       error stop
    end if
    ! 2. Calculate the exact number of box structs in the array
    total_boxes = file_bytes / BOX_SIZE_BYTES
    !write(*,'(A,I12,A)') 'INFO: Total file size =    ', file_bytes, ' bytes'
    !write(*,'(A,I12,A)') 'INFO: Allocating array for ', total_boxes, ' boxes.'
    ! 3. Allocate the dynamic storage
    allocate(boxes(total_boxes))
    ! 4. Open the file in raw binary stream mode
    open(newunit=file_unit, &
         file=trim(filename), &
         access='stream', &         ! Eliminates complex record headers
         form='unformatted', &      ! Tells Fortran it is binary, not text
         status='old', &
         action='read', &
         iostat=io_status)
    if (io_status /= 0) then
       print *, "Error: Could not open the binary file."
       stop
    end if
    ! 5. Read the entire file cleanly into your allocated array in one shot
    read(file_unit, iostat=io_status) boxes
    if (io_status == 0) then
       !write(*,'(A,I12,A)') 'INFO: Read successful for  ', total_boxes, ' boxes.'       
       ! Example: print the first box if it exists
       !if (total_boxes > 0) then
       !   print '(A,4I12)', "First Box: ", boxes(1)%x1, &
       !        boxes(1)%y1, &
       !        boxes(1)%x2, &
       !        boxes(1)%y2
       !end if
       do i=1,total_boxes
          if( .not. boxes(i)%is_valid() ) error stop "INVALID BOX detected on input"
       end do
    else
       print *, "Error occurred while reading the data array."
    end if
    close(file_unit)
  end subroutine LoadKLBin
  subroutine WriteKLBin(fileName, boxes, total_boxes)
    character(len=*), intent(in) :: filename
    type(Box), intent(in)        :: boxes(:)
    integer(kind=int64),intent(in) :: total_boxes
    integer(kind=int64)          :: i
    integer                      :: file_unit, io_status
    do i=1,total_boxes
       if( .not. boxes(i)%is_valid() ) then
          write(*,*) 'Box: ', i, ' ', boxes(i), ' WRONG.'
          error stop "INVALID BOX"
       end if
    end do
    write(*,'(A,I12,A)') 'INFO: Writing array of     ', total_boxes, ' boxes.'
    ! 2. Open the file in raw binary stream mode
    open(newunit=file_unit, &
         file=trim(filename), &
         access='stream', &         ! Eliminates complex record headers
         form='unformatted', &      ! Tells Fortran it is binary, not text
         status='replace', &        ! Creates a new file or overwrites existing
         action='write', &
         iostat=io_status)
    if (io_status /= 0) then
       print *, "Error: Could not open the binary file for writing."
       stop
    end if
    ! 3. Write the entire array cleanly into the file in one shot
    write(file_unit, iostat=io_status) boxes
    if (io_status == 0) then
       write(*,'(A,I12,A)') 'INFO: Write successful for ', total_boxes, ' boxes.'
    else
       print *, "Error occurred while writing the data array."
    end if
    ! 4. Close the file
    close(file_unit)
  end subroutine WriteKLBin
end module KLDataModule
