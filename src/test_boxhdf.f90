!=====================================================================
! File   : test_boxhdf.f90
! Author : Sandeep Koranne (C) All rights reserved
!  Simple demo: store an array of Box objects in an HDF5 file and read it
!  back again.  Uses the HDF5‑Fortran API (module hdf5).
!  HDF5_FC=ifx /scratch1/skoranne/INTELCOMPILERS/bin/h5fc -O3 disk.f90 -o disk.exe
!  h5ls and h5stat show file info, and also use Julia HDF5 package
!=====================================================================

!=====================================================================
!  Driver program – creates a few boxes, writes them, reads them back.
!=====================================================================
program demo_box_hdf5
  use iso_fortran_env, only: int32
  use hdf5
  use HDFDataModule
  use GeometryModule
  implicit none
  integer(kind=int32), allocatable :: X(:), Y(:)
  integer(kind=int32), allocatable :: polystart(:), polyend(:)
  type(Box), allocatable :: boxes(:), boxes_read(:)
  integer :: i, n

  !-----------------------------------------------------------------
  !  1) Build a small test array
  !-----------------------------------------------------------------
  n = 50
  allocate(boxes(n))

  do i = 1, n
     boxes(i)%x1 = i*10
     boxes(i)%y1 = i*10
     boxes(i)%x2 = i*10 + 2
     boxes(i)%y2 = i*10 + 3
  end do

  print *, '--- original boxes -----------------------------------'
  do i = 1, n
     print '(A,5I5)', 'Box ', i, boxes(i)%x1, boxes(i)%y1, &
          boxes(i)%x2, boxes(i)%y2
  end do

  !-----------------------------------------------------------------
  !  2) Write them to an HDF5 file
  !-----------------------------------------------------------------
  call saveToHDF('boxes.h5', boxes)
  print *, 'Data written to boxes.h5'

  !  3) Read the data back into a *different* array
  !-----------------------------------------------------------------
  call loadFromHDF('boxes.h5', boxes_read)

  print *, '--- boxes read back from HDF5 ------------------------'
  do i = 1, size(boxes_read)
     print '(A,5I5)', 'Box ', i, boxes_read(i)%x1, boxes_read(i)%y1, &
          boxes_read(i)%x2, boxes_read(i)%y2
  end do

  !-----------------------------------------------------------------
  !  4) (optional) verify that what we read equals what we wrote
  !-----------------------------------------------------------------
  if ( all(boxes == boxes_read) ) then
     print *, 'Verification succeeded – data are identical.'
  else
     print *, 'Verification failed – mismatch!'
  end if

  call LoadPolygonOffsetsHDF5("a.h5",X, Y, polystart, polyend)
  !> what does 12 polygon 84 vertices mean
  if( size(X) /= size(Y) ) then
     error stop "|X| != |Y|"
  end if
  if( size(polystart) /= size(polyend) ) then
     error stop "|PolyStart| != |PolyEnd|"
  end if
  
  do i=1,size(polystart)
     write(*,*) 'Polygon = ', i, ' has ', polyend(i) - polystart(i), ' vertices'
     write(*,*) X(polystart(i):polyend(i)-1)
     write(*,*) Y(polystart(i):polyend(i)-1)     
  end do

end program demo_box_hdf5

