! File    : geometry.f90
! Author  : Sandeep Koranne (C) All rights.reserved.
! Purpose : Implementation of MAGIC VLSI format parser

module GeometryModule
  implicit none
  private
  public :: Box, CheckSortOrder, mbr_of_array, CheckBox, quicksort_boxes, box_scale, str_pack, omt_pack, MBRValid
  ! Enum-like constants for the sorting axis
  integer, parameter :: AXIS_X = 1
  integer, parameter :: AXIS_Y = 2
  !>   type, bind(C) :: Box
  type :: Box
     integer(kind=4) :: x1, y1, x2, y2
   contains
     procedure, pass :: reset_to_infinity
     procedure, pass :: is_valid
     procedure, pass :: print_box
     procedure, pass :: box_union
     procedure, pass :: box_intersection
     procedure, pass :: box_equal
     generic :: operator(+) => box_union
     generic :: operator(*) => box_intersection
     generic :: operator(==) => box_equal
  end type Box
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
  pure logical function MBRValid(this)
    class(Box), intent(in) :: this
    !box is valid if x1 <= x2 and y1 <= y2
    MBRValid = (this%x1 <= this%x2 .and. this%y1 <= this%y2)
  end function MBRValid
  subroutine print_box(this)
    class(Box), intent(in) :: this    
    print *, 'Box: [', this%x1, ',', this%y1, '] to [', this%x2, ',', this%y2, ']'
  end subroutine print_box
  pure elemental function box_equal(this, other) result(is_eq)
    class(Box), intent(in) :: this, other
    logical :: is_eq
    is_eq = ( ( this%x1 == other%x1) .and. ( this%y1 == other%y1) .and. &
         ( this%x2 == other%x2) .and. ( this%y2 == other%y2) )
  end function box_equal
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

  pure subroutine str_pack(arr, node_capacity)
    type(Box), intent(inout), dimension(:) :: arr
    integer, intent(in) :: node_capacity

    integer :: n, num_leaves, num_slices
    integer :: slice_size, slice_start, slice_end
    integer :: i

    n = size(arr)
    if (n <= node_capacity) return

    ! 1. Calculate STR dimensions
    ! Total number of leaf nodes required
    num_leaves = ceiling(real(n) / real(node_capacity))

    ! Number of vertical slices (S = sqrt(L))
    num_slices = ceiling(sqrt(real(num_leaves)))

    ! Number of rectangles per vertical slice
    slice_size = ceiling(real(n) / real(num_slices))

    ! 2. Sort the entire array by the X-center coordinate
    call quicksort_boxes_STR(arr, 1, n, AXIS_X)

    ! 3. Divide into S vertical slices and sort each by Y-center
    do i = 1, num_slices
       slice_start = (i - 1) * slice_size + 1
       slice_end   = min(i * slice_size, n)

       ! Only sort if the slice contains elements
       if (slice_start < slice_end) then
          call quicksort_boxes_STR(arr, slice_start, slice_end, AXIS_Y)
       end if
    end do

  end subroutine str_pack
  pure subroutine insertion_sort_boxes(arr, left, right, axis)
    type(Box), intent(inout), dimension(:) :: arr
    integer, intent(in) :: left, right, axis

    integer :: i, j
    type(Box) :: key
    real :: key_center, current_center

    do i = left + 1, right
       key = arr(i)
       
       ! Calculate the center of the key element being inserted
       if (axis == AXIS_X) then
          key_center = (key%x1 + key%x2) * 0.5
       else
          key_center = (key%y1 + key%y2) * 0.5
       end if

       j = i - 1
       
       ! Shift elements that are greater than the key to the right
       shift_loop: do while (j >= left)
          if (axis == AXIS_X) then
             current_center = (arr(j)%x1 + arr(j)%x2) * 0.5
          else
             current_center = (arr(j)%y1 + arr(j)%y2) * 0.5
          end if

          ! If the sorted element is less than or equal to the key, we found the insertion point
          if (current_center <= key_center) exit shift_loop

          arr(j + 1) = arr(j)
          j = j - 1
       end do shift_loop

       ! Insert the key at its correct position
       arr(j + 1) = key
    end do

  end subroutine insertion_sort_boxes
  pure recursive subroutine quicksort_boxes_STR(arr, left, right, axis)
    type(Box), intent(inout), dimension(:) :: arr
    integer, intent(in) :: left, right, axis
    integer, parameter :: K_SMALL_THRESHOLD = 64 !> 16 was worse than 32
    integer :: i, j
    real :: pivot_center, current_center
    type(Box) :: temp

    !if (left >= right) return
    if (right - left <= K_SMALL_THRESHOLD) then
       if (left < right) call insertion_sort_boxes(arr, left, right, axis)
       return
    end if
    i = left
    j = right

    ! Calculate the center of the pivot element
    if (axis == AXIS_X) then
       pivot_center = (arr((left + right) / 2)%x1 + arr((left + right) / 2)%x2) * 0.5
    else
       pivot_center = (arr((left + right) / 2)%y1 + arr((left + right) / 2)%y2) * 0.5
    end if

    ! Standard Quicksort Partitioning
    do while (i <= j)
       ! Get center of element i
       if (axis == AXIS_X) then
          current_center = (arr(i)%x1 + arr(i)%x2) * 0.5
       else
          current_center = (arr(i)%y1 + arr(i)%y2) * 0.5
       end if

       do while (current_center < pivot_center)
          i = i + 1
          if (axis == AXIS_X) then
             current_center = (arr(i)%x1 + arr(i)%x2) * 0.5
          else
             current_center = (arr(i)%y1 + arr(i)%y2) * 0.5
          end if
       end do

       ! Get center of element j
       if (axis == AXIS_X) then
          current_center = (arr(j)%x1 + arr(j)%x2) * 0.5
       else
          current_center = (arr(j)%y1 + arr(j)%y2) * 0.5
       end if

       do while (current_center > pivot_center)
          j = j - 1
          if (axis == AXIS_X) then
             current_center = (arr(j)%x1 + arr(j)%x2) * 0.5
          else
             current_center = (arr(j)%y1 + arr(j)%y2) * 0.5
          end if
       end do

       if (i <= j) then
          ! Swap
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
          i = i + 1
          j = j - 1
       end if
    end do

    ! Recursive calls
    if (left < j)  call quicksort_boxes_STR(arr, left, j, axis)
    if (i < right) call quicksort_boxes_STR(arr, i, right, axis)

  end subroutine quicksort_boxes_STR

  ! Wrapper subroutine to match your original interface
  pure subroutine omt_pack(arr, node_capacity)
    type(Box), intent(inout), dimension(:) :: arr
    integer, intent(in) :: node_capacity
    type(Box), allocatable, dimension(:) :: workspace

    if (size(arr) <= node_capacity) return

    ! Allocate a single workspace array to avoid repeated memory allocations
    ! during the recursive splitting steps.
    allocate(workspace(size(arr)))
    
    call omt_pack_recursive(arr, 1, size(arr), node_capacity, workspace)
    
    deallocate(workspace)
  end subroutine omt_pack


  ! Recursive Top-Down Partitioning
  pure recursive subroutine omt_pack_recursive(arr, start_idx, end_idx, node_capacity, workspace)
    type(Box), intent(inout), dimension(:) :: arr
    integer, intent(in) :: start_idx, end_idx, node_capacity
    type(Box), intent(inout), dimension(:) :: workspace

    integer :: n, num_leaves, left_leaves, split_idx
    real :: overlap_x, area_x, overlap_y, area_y

    n = end_idx - start_idx + 1
    if (n <= node_capacity) return

    ! To guarantee perfectly balanced trees, divide the required leaves in half
    num_leaves = ceiling(real(n) / real(node_capacity))
    left_leaves = num_leaves / 2
    
    ! Calculate exact array index to split at
    split_idx = start_idx + left_leaves * node_capacity - 1

    ! 1. Evaluate X-axis split
    call quicksort_boxes_STR(arr, start_idx, end_idx, AXIS_X)
    call evaluate_split(arr, start_idx, split_idx, end_idx, overlap_x, area_x)

    ! Save the X-sorted state into the workspace
    workspace(start_idx:end_idx) = arr(start_idx:end_idx)

    ! 2. Evaluate Y-axis split
    call quicksort_boxes_STR(arr, start_idx, end_idx, AXIS_Y)
    call evaluate_split(arr, start_idx, split_idx, end_idx, overlap_y, area_y)

    ! 3. Choose the best axis 
    ! (Minimize overlap first; use total area as a tie-breaker)
    if ((overlap_x < overlap_y) .or. (overlap_x == overlap_y .and. area_x < area_y)) then
        ! X was better: restore the X-sorted array from the workspace
        arr(start_idx:end_idx) = workspace(start_idx:end_idx)
    end if
    ! If Y was better, arr is already Y-sorted, so we do nothing.

    ! 4. Recursively process the left and right partitions
    call omt_pack_recursive(arr, start_idx, split_idx, node_capacity, workspace)
    call omt_pack_recursive(arr, split_idx + 1, end_idx, node_capacity, workspace)

  end subroutine omt_pack_recursive


  ! Helper Subroutine: Computes the cost (Overlap and Area) of a proposed split
  pure subroutine evaluate_split(arr, start_idx, split_idx, end_idx, overlap, total_area)
    type(Box), intent(in), dimension(:) :: arr
    integer, intent(in) :: start_idx, split_idx, end_idx
    real, intent(out) :: overlap, total_area

    integer(kind=4) :: l_xmin, l_xmax, l_ymin, l_ymax
    integer(kind=4) :: r_xmin, r_xmax, r_ymin, r_ymax
    integer(kind=4) :: over_x, over_y
    integer :: i

    ! Initialize left MBR
    l_xmin = arr(start_idx)%x1; l_xmax = arr(start_idx)%x2
    l_ymin = arr(start_idx)%y1; l_ymax = arr(start_idx)%y2
    do i = start_idx + 1, split_idx
        l_xmin = min(l_xmin, arr(i)%x1)
        l_xmax = max(l_xmax, arr(i)%x2)
        l_ymin = min(l_ymin, arr(i)%y1)
        l_ymax = max(l_ymax, arr(i)%y2)
    end do

    ! Initialize right MBR
    r_xmin = arr(split_idx + 1)%x1; r_xmax = arr(split_idx + 1)%x2
    r_ymin = arr(split_idx + 1)%y1; r_ymax = arr(split_idx + 1)%y2
    do i = split_idx + 2, end_idx
        r_xmin = min(r_xmin, arr(i)%x1)
        r_xmax = max(r_xmax, arr(i)%x2)
        r_ymin = min(r_ymin, arr(i)%y1)
        r_ymax = max(r_ymax, arr(i)%y2)
    end do

    ! Compute total area of both bounding boxes
    total_area = (l_xmax - l_xmin) * (l_ymax - l_ymin) + &
                 (r_xmax - r_xmin) * (r_ymax - r_ymin)

    ! Compute physical overlap area
    over_x = max(0, min(l_xmax, r_xmax) - max(l_xmin, r_xmin))
    over_y = max(0, min(l_ymax, r_ymax) - max(l_ymin, r_ymin))
    overlap = over_x * over_y

  end subroutine evaluate_split
end module GeometryModule

