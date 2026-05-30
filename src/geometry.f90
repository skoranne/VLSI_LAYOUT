! File    : geometry.f90
! Author  : Sandeep Koranne (C) All rights.reserved.
! Purpose : Implementation of MAGIC VLSI format parser

module GeometryModule
  implicit none
  private
  public :: Box, CheckSortOrder, mbr_of_array, CheckBox, quicksort_boxes, box_scale

  type :: Box
     integer(kind=4) :: x1, y1, x2, y2
   contains
     procedure, pass :: reset_to_infinity
     procedure, pass :: is_valid
     procedure, pass :: print_box
     procedure, pass :: box_union
     procedure, pass :: box_intersection
     generic :: operator(+) => box_union
     generic :: operator(*) => box_intersection
  end type Box
  type :: RNode
     logical                     :: leaf      ! .true. → stores boxes
     integer                     :: n         ! number of used entries
     type(Box),  allocatable     :: bbox(:)   ! bounding box for each entry
     type(RNode),allocatable     :: child(:)  ! child pointers (only for interior)
     type(Box),  allocatable     :: data(:)   ! boxes (only for leaf nodes)
  end type RNode
contains
    ! Type procedure to reset box to [infinity,infinity,-infinity,-infinity]
  pure subroutine reset_to_infinity(this)
    class(Box), intent(inout) :: this
    this%x1 = huge(this%x1)
    this%y1 = huge(this%y1)
    this%x2 = -huge(this%x1)
    this%y2 = -huge(this%y1)
  end subroutine reset_to_infinity
  pure logical function is_valid(this)
    class(Box), intent(in) :: this
    !box is valid if x1 < x2 and y1 < y2
    is_valid = (this%x1 < this%x2 .and. this%y1 < this%y2)
  end function is_valid
  
  subroutine print_box(this)
    class(Box), intent(in) :: this    
    print *, 'Box: [', this%x1, ',', this%y1, '] to [', this%x2, ',', this%y2, ']'
  end subroutine print_box
  ! Type procedure for union of two boxes
  pure function box_union(this, other) result(union_box)
    class(Box), intent(in) :: this, other
    type(Box) :: union_box
    ! Find the bounding box that contains both boxes
    union_box%x1 = min(this%x1, other%x1)
    union_box%y1 = min(this%y1, other%y1)
    union_box%x2 = max(this%x2, other%x2)
    union_box%y2 = max(this%y2, other%y2)
  end function box_union
  pure function mbr_of_array(arr,n) result(mbr)
    type(Box), intent(in), dimension(:) :: arr
    integer(kind=8), intent(in) :: n
    type(Box)  :: mbr
    integer(kind=8)    :: i
    call mbr%reset_to_infinity()
    if( size(arr) == 0 ) then
       return
    end if
    do i = 1,n
       mbr = mbr + arr(i)
    end do
  end function mbr_of_array
  
  subroutine box_scale(this, ascale, bscale)
    class(Box), intent(inout) :: this
    integer, intent(in) :: ascale, bscale
    this%x1 = (this%x1*ascale)/bscale
    this%x2 = (this%x2*ascale)/bscale
    this%y1 = (this%y1*ascale)/bscale
    this%y2 = (this%y2*ascale)/bscale    
  end subroutine box_scale
  
  ! Type procedure for intersection of two boxes
  function box_intersection(this, other) result(intersection_box)
    class(Box), intent(in) :: this, other
    type(Box) :: intersection_box
    
    ! Find the intersection box
    intersection_box%x1 = max(this%x1, other%x1)
    intersection_box%y1 = max(this%y1, other%y1)
    intersection_box%x2 = min(this%x2, other%x2)
    intersection_box%y2 = min(this%y2, other%y2)
    
    ! Check if intersection is valid (non-empty)
    if (intersection_box%x1 > intersection_box%x2 .or. &
        intersection_box%y1 > intersection_box%y2) then
       ! Invalid intersection - set to empty box
       intersection_box%x1 = huge(this%x1)
       intersection_box%y1 = huge(this%y1)
       intersection_box%x2 = -huge(this%x1)
       intersection_box%y2 = -huge(this%y1)
    end if
  end function box_intersection
  function CheckBox(b1) result(res)
    type(Box), intent(in) :: b1
    logical :: res
    res = ( ( b1%x1 >= b1%x2) .or. ( b1%y1 >= b1%y2) )
    if( res ) then
       write(*,'(A,4I)') 'Box ', b1%x1, b1%y1, b1%x2, b1%y2
    end if
  end function CheckBox

  pure elemental function box_less_than(b1, b2) result(res)
    type(Box), intent(in) :: b1, b2
    logical :: res
    if (b1%x1 == b2%x1) then
       res = (b1%y1 < b2%y1)
    else
       res = (b1%x1 < b2%x1)
    end if
  end function box_less_than
  pure elemental function box_less_than_or_equal(b1, b2) result(res)
    type(Box), intent(in) :: b1, b2
    logical :: res
    if (b1%x1 == b2%x1) then
       res = (b1%y1 <= b2%y1)
    else
       res = (b1%x1 <= b2%x1)
    end if
  end function box_less_than_or_equal
  
  function CheckSortOrder(arr, left, right) result(res)
    type(Box), intent(inout), dimension(:) :: arr
    integer(kind=8), intent(in) :: left, right  ! Use kind=8 for 100M arrays
    integer(kind=8) :: i
    logical         :: res
    if( right <= left ) then
       res = .TRUE.
       return
    else 
       do i=left,right-1
          res = box_less_than( arr(i), arr(i+1) )
          if( .not. res ) then
             write(*,*) 'Box ',i, ' failed '
             write(*,'(A,I,A,4I)') 'Box ', i, ': ', arr(i)%x1, arr(i)%y1, arr(i)%x2, arr(i)%y2
             write(*,'(A,I,A,4I)') 'Box ', i+1, ': ', arr(i+1)%x1, arr(i+1)%y1, arr(i+1)%x2, arr(i+1)%y2             
             return
          end if
       end do
    end if
  end function CheckSortOrder
  
  recursive subroutine quicksort_boxes(arr, left, right)
    type(Box), intent(inout), dimension(:) :: arr
    integer(kind=8), intent(in) :: left, right  ! Use kind=8 for 100M arrays
    integer(kind=8) :: i, j
    type(Box) :: pivot, temp

    if (left < right) then
       pivot = arr((left + right) / 2)
       i = left
       j = right

       do while (i <= j)
          do while (box_less_than(arr(i), pivot))
             i = i + 1
          end do
          do while (box_less_than(pivot, arr(j)))
             j = j - 1
          end do
          if (i <= j) then
             ! Swap elements
             temp = arr(i)
             arr(i) = arr(j)
             arr(j) = temp
             i = i + 1
             j = j - 1
          end if
       end do

       if (left < j)  call quicksort_boxes(arr, left, j)
       if (i < right) call quicksort_boxes(arr, i, right)
    end if
  end subroutine quicksort_boxes

end module GeometryModule

