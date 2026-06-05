!=====================================================================
!  morton_sort.f90
!  Demo: compute Morton codes for the centre of a rectangular box,
!        store (mortonCode, boxId) pairs and sort them.
!=====================================================================
module box_mod
  use iso_fortran_env, only: int32, int64
  implicit none
  private
  public :: Box, BoxWithMortonCode, morton_sort_boxes

  !-----------------------------------------------------------------
  !  Simple rectangular box – four 32‑bit integer coordinates.
  !  bind(C) guarantees a layout compatible with C (no hidden padding).
  !-----------------------------------------------------------------
  type, bind(C) :: Box
     integer(kind=int32) :: x1, y1, x2, y2
  end type Box

  !-----------------------------------------------------------------
  !  Auxiliary type that will be sorted.
  !    mortonCode – ‑order code of the box centre
  !    boxId      – 32‑bit index of the box in the original array
  !-----------------------------------------------------------------
  type :: BoxWithMortonCode
     integer(kind=int64) :: mortonCode
     integer(kind=int32) :: boxId
  end type BoxWithMortonCode

contains
  !=================================================================
  !  PUBLIC INTERFACE
  !=================================================================
  subroutine morton_sort_boxes(boxes, sorted_aux)
    type(Box),               intent(in)  :: boxes(:)
    type(BoxWithMortonCode), intent(out) :: sorted_aux(:)

    integer :: n

    n = size(boxes)
    if (size(sorted_aux) /= n) then
       stop 'morton_sort_boxes: size mismatch between input and output arrays'
    end if

    call build_aux_array(boxes, sorted_aux)
    call quicksort(sorted_aux, 1, n)      ! in‑place sort by mortonCode
  end subroutine morton_sort_boxes

  !=================================================================
  !  PRIVATE HELPERS
  !=================================================================

  !-----------------------------------------------------------------
  !  Build the auxiliary array: compute centre → morton code → store.
  !-----------------------------------------------------------------
  subroutine build_aux_array(boxes, aux)
    type(Box),               intent(in)  :: boxes(:)
    type(BoxWithMortonCode), intent(out) :: aux(:)

    integer :: i
    integer(kind=int32) :: cx, cy               ! centre coordinates
    integer(kind=int64) :: mc

    do i = 1, size(boxes)
       ! centre of the rectangle (integer division, truncates toward zero)
       cx = (boxes(i)%x1 + boxes(i)%x2) / 2
       cy = (boxes(i)%y1 + boxes(i)%y2) / 2

       mc = morton2D(cx, cy)                  ! 64‑bit Morton code
       aux(i)%mortonCode = mc
       aux(i)%boxId      = i
    end do
  end subroutine build_aux_array

  !-----------------------------------------------------------------
  !  Morton (Z‑order) code for two 32‑bit coordinates.
  !  The algorithm interleaves the bits of x and y.
  !  It works for the full 32‑bit range – the result fits into 64 bits.
  !-----------------------------------------------------------------
  function morton2D(x, y) result(code)
  integer(kind=int32), intent(in) :: x, y
  integer(kind=int64)             :: code
  integer(kind=int64) :: xx, yy, inter

  xx = int(x, kind=int64)
  yy = int(y, kind=int64)

  ! --- split each 32‑bit word so that there are empty bits between
  !     every original bit (see “Bit Twiddling Hacks” by Sean Eron Anderson)
  ! use SHIFTL(xx,16)
  xx = (xx .or. SHIFTL(xx,16)) .and. int(z'0000FFFF0000FFFF',kind=int64)
  xx = (xx .or. SHIFTL(xx, 8)) .and. int(z'00FF00FF00FF00FF',kind=int64)
  xx = (xx .or. SHIFTL(xx, 4)) .and. int(z'0F0F0F0F0F0F0F0F',kind=int64)
  xx = (xx .or. SHIFTL(xx, 2)) .and. int(z'3333333333333333',kind=int64)
  xx = (xx .or. SHIFTL(xx, 1)) .and. int(z'5555555555555555',kind=int64)

  yy = (yy .or. SHIFTL(yy,16)) .and. int(z'0000FFFF0000FFFF',kind=int64)
  yy = (yy .or. SHIFTL(yy, 8)) .and. int(z'00FF00FF00FF00FF',kind=int64)
  yy = (yy .or. SHIFTL(yy, 4)) .and. int(z'0F0F0F0F0F0F0F0F',kind=int64)
  yy = (yy .or. SHIFTL(yy, 2)) .and. int(z'3333333333333333',kind=int64)
  yy = (yy .or. SHIFTL(yy, 1)) .and. int(z'5555555555555555',kind=int64)

  inter = ior(xx, SHIFTL(yy,1))! x‑bits are even, y‑bits odd
  code = inter
end function morton2D

!-----------------------------------------------------------------
!  Simple in‑place quicksort for an array of BoxWithMortonCode.
!  It sorts by the mortonCode component (ascending).
!-----------------------------------------------------------------
recursive subroutine quicksort(a, lo, hi)
  type(BoxWithMortonCode), intent(inout) :: a(:)
  integer,                  intent(in)    :: lo, hi
  integer :: i, j
  integer(kind=int64) :: pivot

  if (lo >= hi) return

  i = lo
  j = hi
  pivot = a((lo+hi)/2)%mortonCode

  do
     do while (a(i)%mortonCode < pivot)
        i = i + 1
     end do
     do while (a(j)%mortonCode > pivot)
        j = j - 1
     end do
     if (i <= j) then
        call swap(a(i), a(j))
        i = i + 1
        j = j - 1
     end if
     if (i > j) exit
  end do

  if (lo < j) call quicksort(a, lo, j)
  if (i   < hi) call quicksort(a, i , hi)
end subroutine quicksort

!-----------------------------------------------------------------
!  Swap two elements of the auxiliary array.
!-----------------------------------------------------------------
pure subroutine swap(x, y)
  type(BoxWithMortonCode), intent(inout) :: x, y
  type(BoxWithMortonCode)                :: tmp
  tmp = x
  x   = y
  y   = tmp
end subroutine swap

end module box_mod

!--------------------------------------------------------------------
!*** 2️⃣  Driver program – create boxes, sort them, and display the result
!--------------------------------------------------------------------

program demo_morton_sort
  use iso_fortran_env, only: int32, int64
  use box_mod
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
