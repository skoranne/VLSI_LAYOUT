! File   : test_repetition.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Given a BIN file and a description of a repetition, expand and write it

program main
  use CommonModule
  use GeometryModule
  use KLDataModule
  use RTreeBuilderGPU
  use MortonSortOMT
  use SystemInformationModule
  use DatastructuresModule
  use iso_fortran_env, only: int32, int64, real64
  use omp_lib
  implicit none
  character(len=256)            :: arg_string    ! temporary buffer for the 2nd argument
  type(Box), allocatable :: boxes(:)
  type(Box), allocatable :: temp_boxes(:)
  type(Box)              :: bbox
  integer(kind=int64)    :: N
  integer(kind=int64)    :: total_nodes
  integer                :: narg, i, j
  character(len=256)            :: filenameA, filenameB      
  character(len=256)            :: outFileName   
  integer                       :: control_parameter(4)
  integer                       :: iostat, file_unit
  integer(kind=K_COORDINATE_KIND) :: rows, cols,dx,dy

  narg = command_argument_count()
     call get_command_argument(1, filenameA, status=iostat)   ! allocates automatically
     if (iostat /= 0) then
        write (*,*) "ERROR: 1st argument must be a filename."
        stop 2
     end if
     write (*,*) 'Reading 1st filename: ', trim(filenameA)
     call get_command_argument(2, outFileName, status=iostat)   ! allocates automatically
     if (iostat /= 0) then
        write (*,*) "ERROR: 3rd argument must be a filename."
        stop 2
     end if
     ! ---- third argument: integer (max number of layers) ----------
     do i=1,4
        call get_command_argument(2+i, arg_string, status=iostat)
        if (iostat /= 0) then
           write (*,*) "ERROR: 4th argument must be an integer."
           stop 2
        end if
        read (arg_string, *, iostat=iostat) control_parameter(i)
        if (iostat /= 0 .or. control_parameter(i) < 0) then
           write (*,*) "ERROR: CONTROL must be a non‑negative integer."
           stop 3
        end if
     end do
     
  call LoadKLBin(filenameA, boxes)
  N = size( boxes )
  bbox = mbr_of_array( boxes, N )
  write(*,'(A,I12,A,4I12,A,I12)') 'Loaded : ', N, ' BBOX = ', bbox, ' |T| = ', total_nodes
  open(newunit=file_unit, &
       file=trim(outFileName), &
       access='stream', &         ! Eliminates complex record headers
       form='unformatted', &      ! Tells Fortran it is binary, not text
       status='replace', &        ! Creates a new file or overwrites existing
       action='write', &
       iostat=iostat)
  if (iostat /= 0) then
     print *, "Error: Could not open the binary file for writing."
     stop
  end if
  ! 3. Write the entire array cleanly into the file in one shot
  !  
  do rows = 1, control_parameter(1)
     dx = 0
     do cols = 1, control_parameter(2)
        temp_boxes = boxes !> copy original
        call ApplyTransform( temp_boxes, dx, dy )
        dx = dx + control_parameter(3)
        write(file_unit, iostat=iostat) temp_boxes
        if (iostat == 0) then
           write(*,'(A,I12,A)') 'INFO: Write successful for ', N*rows*cols, ' boxes.'
        else
           print *, "Error occurred while writing the data array."
        end if
     end do
     dy = dy + control_parameter(4)
  end do
  write(*,*) 'Written total: ', N*rows*cols, ' boxes.'
  close(file_unit)
end program main


