! File    : readklbin.f90
! Author  : Sandeep Koranne (C) 2026. All rights reserved
! Purpose : Read the 4 int32 coordinates for a box written by KLayout
program read_gds_bin
  use, intrinsic :: iso_fortran_env, only: int64
  use GeometryModule
  implicit none
  ! Define standard 64-bit integer types (matching Ruby's q< format)
  integer(kind=int64) :: file_bytes, total_boxes
  integer, parameter  :: BOX_SIZE_BYTES = 16 ! 4 coordinates * 4 bytes

  ! Dynamic array to store all layout shapes
  type(Box), allocatable :: box_array(:)
  integer :: file_unit, io_status, i

  character(len=100) :: filename = "square_L68_D20.bin"

  ! 1. Query the filesystem for the total file size in bytes
  inquire(file=trim(filename), size=file_bytes)

  if (file_bytes <= 0) then
     print *, "Error: File is empty or does not exist."
     stop
  end if

  ! 2. Calculate the exact number of box structs in the array
  total_boxes = file_bytes / BOX_SIZE_BYTES
  print *, "INFO: Total file size = ", file_bytes, " bytes"
  print *, "INFO: Allocating array for ", total_boxes, " boxes."

  ! 3. Allocate the dynamic storage
  allocate(box_array(total_boxes))

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
  read(file_unit, iostat=io_status) box_array

  if (io_status == 0) then
     print *, "INFO: Read successful!"
     ! Example: print the first 10 boxes if it exists
     if (total_boxes > 0) then
        do i=1,min(10,total_boxes)
           print '(A,4I12)', "First Box: ", box_array(i)%x1, &
                box_array(i)%y1, &
                box_array(i)%x2, &
                box_array(i)%y2
        end do
        do i=1,total_boxes
           if( .not. box_array(i)%is_valid() ) error stop "INVALID BOX detected on input"
        end do
        
     end if
  else
     print *, "Error occurred while reading the data array."
  end if

  close(file_unit)
  deallocate(box_array)

end program read_gds_bin
