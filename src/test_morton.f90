!--------------------------------------------------------------------
!*** 2️⃣  Driver program – create boxes, sort them, and display the result
!--------------------------------------------------------------------

program demo_morton_sort
  use iso_fortran_env, only: int32, int64
  use MortonSortModule
  implicit none

  integer, parameter :: nbox = 10
  type(Box)               :: boxes(nbox)
  type(BoxWithMortonCode) :: aux_sorted(nbox)
  real    :: val
  integer :: i

  !--------------------------------------------------------------
  !  1) Create a few test boxes (random coordinates for demo)
  !--------------------------------------------------------------
  call random_seed()
  do i = 1, nbox
     call random_number(val)
     boxes(i)%x1 = int(100*val)
     call random_number(val)
     boxes(i)%y1 = int(100*val)
     call random_number(val)
     boxes(i)%x2 = boxes(i)%x1 + int(10*val) + 1
     call random_number(val)
     boxes(i)%y2 = boxes(i)%y1 + int(10*val) + 1
  end do

  print *, '--- Original boxes (index, x1,y1,x2,y2) ---'
  do i = 1, nbox
     print '(I3,1X,4(I5,1X))', i, boxes(i)%x1, boxes(i)%y1, &
          boxes(i)%x2, boxes(i)%y2
  end do

  !--------------------------------------------------------------
  !  2) Build the auxiliary array and sort it by Morton code
  !--------------------------------------------------------------
  call morton_sort_boxes(boxes, aux_sorted)

  print *, ''
  print *, '--- Sorted auxiliary array (mortonCode, boxId) ---'
  do i = 1, nbox
     print '(I3,1X,Z16.16,1X,I5)', i, aux_sorted(i)%mortonCode, &
          aux_sorted(i)%boxId
  end do

  !--------------------------------------------------------------
  !  3) (Optional) Show the boxes in Morton order using   !--------------------------------------------------------------
  print *, ''
  print *, '--- Boxes in Morton order (using sorted indices) ---'
  do i = 1, nbox
     associate (b => boxes(aux_sorted(i)%boxId))
       print '(I3,1X,4(I5,1X))', aux_sorted(i)%boxId, &
            b%x1, b%y1, b%x2, b%y2
     end associate
  end do

end program demo_morton_sort
