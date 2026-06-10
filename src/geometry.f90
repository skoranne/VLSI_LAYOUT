! File    : geometry.f90
! Author  : Sandeep Koranne (C) All rights.reserved.
! Purpose : Implementation of MAGIC VLSI format parser

module GeometryModule
  use iso_fortran_env, only: int32, int64, real64
  implicit none
  private
  integer, parameter, public :: K = int32

  public :: Box, CheckSortOrder, mbr_of_array, CheckBox, quicksort_boxes, box_scale, str_pack, omt_pack, MBRValid, &
       box_not_interact, box_interact, box_area, box_perimeter, calculate_union_area, get_sort_permutation, &
       calculate_polygon_union_area, PolygonBooleanAND, heal_boxes, sort_int_array
  ! Enum-like constants for the sorting axis
  integer, parameter :: AXIS_X = 1
  integer, parameter :: AXIS_Y = 2
  !>   type, bind(C) :: Box
  type :: Box
     integer(kind=K) :: x1, y1, x2, y2
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
  type :: AugmentedBox
     type(Box) :: mbr
     integer(kind=K) :: value
  end type AugmentedBox

  ! The generic interface routes the call to the correct specific subroutine
  interface get_sort_permutation
     module procedure get_box_permutation
     !module procedure get_augmentbox_permutation
  end interface get_sort_permutation
  type, public :: ShapeCollection
     integer(int32), allocatable :: X(:), Y(:)
     integer(int32), allocatable :: poly_start(:), poly_end(:)
  end type ShapeCollection
  ! Auxiliary data structure for the sweep-line
  type :: Event
     integer(K) :: x
     integer(K) :: y1, y2
     integer(K) :: lap_change ! +1 for left edge, -1 for right edge
     integer :: owner ! 1 for Shape A, 2 for Shape B     
  end type Event

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
  !> Topological functions
  pure elemental function box_not_interact(this, other) result(retval)
    class(Box), intent(in) :: this, other
    logical :: retval
    retval = ( ( this%x1 > other%x2) .or. ( this%y1 > other%y2) .or. &
         ( this%x2 < other%x1) .or. ( this%y2 < other%y1) )
  end function box_not_interact
  pure elemental function box_interact(this, other) result(retval)
    class(Box), intent(in) :: this, other
    type(Box)              :: tempBox
    logical :: retval
    if( box_not_interact(this, other) ) then
       retval = .false.
       return
    else !> its possible that there is a corner to corner touch
       tempBox = this * other
       if( tempBox%x1 == tempBox%x2 .and. tempBox%y1 == tempBox%y2 ) then
          retval = .false. ! point intersection
       else
          retval = MBRValid( tempBox )
       end if
    end if
  end function box_interact


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

  pure subroutine box_scale(this, ascale, bscale)
    class(Box), intent(inout) :: this
    integer, intent(in) :: ascale, bscale
    this%x1 = (this%x1*ascale)/bscale
    this%x2 = (this%x2*ascale)/bscale
    this%y1 = (this%y1*ascale)/bscale
    this%y2 = (this%y2*ascale)/bscale    
  end subroutine box_scale

  ! Type procedure for intersection of two boxes
  pure function box_intersection(this, other) result(intersection_box)
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
  pure elemental function box_area(b) result(retval)
    type(Box), intent(in) :: b
    real(kind=real64)     :: retval
    if( is_valid(b) ) then
       retval = real(b%x2 - b%x1) * real(b%y2 - b%y1)
    else
       retval = 0.0
    end if
  end function box_area
  pure elemental function box_perimeter(b) result(retval)
    type(Box), intent(in) :: b
    real(kind=real64)     :: retval
    if( MBRValid(b) ) then !> because we can have interaction with line segment overlap
       retval = 2*real(b%x2 - b%x1) + 2*real(b%y2 - b%y1)
    else
       retval = 0.0
    end if
  end function box_perimeter

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

  subroutine str_pack(arr, node_capacity)
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
  subroutine insertion_sort_boxes(arr, left, right, axis)
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
  recursive subroutine quicksort_boxes_STR(arr, left, right, axis)
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
  subroutine omt_pack(arr, node_capacity)
    type(Box), intent(inout), dimension(:) :: arr
    integer, intent(in) :: node_capacity
    type(Box), allocatable, dimension(:) :: workspace

    if (size(arr) <= node_capacity) return

    ! Allocate a single workspace array to avoid repeated memory allocations
    ! during the recursive splitting steps.
    allocate(workspace(size(arr)))
    !$omp parallel default(none) shared(arr,workspace) firstprivate(node_capacity)
    !$omp single nowait
    call omt_pack_recursive(arr, 1, size(arr), node_capacity, workspace)
    !$omp end single
    !$omp end parallel

    deallocate(workspace)
  end subroutine omt_pack


  ! Recursive Top-Down Partitioning
  recursive subroutine omt_pack_recursive(arr, start_idx, end_idx, node_capacity, workspace)
    type(Box), intent(inout), dimension(:) :: arr
    integer, intent(in) :: start_idx, end_idx, node_capacity
    type(Box), intent(inout), dimension(:) :: workspace
    integer, parameter :: MIN_TASK_SIZE = 32768
    !On MW
    !Default do concurrent on outer loop took
    !Sorting/OMT completed in     10248.56 CPU seconds.     2256.00 REAL seconds.
    !OpenMP with 32768
    !Sorting/OMT completed in     39305.40 CPU seconds.     1304.20 REAL seconds.                                                  

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
    if ( split_idx - start_idx + 1 >= MIN_TASK_SIZE ) then
       !$omp task shared(arr,workspace) firstprivate(start_idx, split_idx,node_capacity) default(none)
       call omt_pack_recursive(arr, start_idx, split_idx, node_capacity, workspace)
       !$omp end task
    else
       call omt_pack_recursive(arr, start_idx, split_idx, node_capacity, workspace)
    end if
    if ( end_idx - split_idx >= MIN_TASK_SIZE ) then
       !$omp task shared(arr,workspace) firstprivate(split_idx, end_idx, node_capacity) default(none) &
       !$omp& if ( end_idx - split_idx >= MIN_TASK_SIZE )
       call omt_pack_recursive(arr, split_idx + 1, end_idx, node_capacity, workspace)
       !$omp end task
    else
       call omt_pack_recursive(arr, split_idx + 1, end_idx, node_capacity, workspace)
    end if
  end subroutine omt_pack_recursive


  ! Helper Subroutine: Computes the cost (Overlap and Area) of a proposed split
  subroutine evaluate_split(arr, start_idx, split_idx, end_idx, overlap, total_area)
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

  !> winding number and lapcount analysis
  pure function calculate_union_area(boxes) result(area)
    type(Box), intent(in) :: boxes(:)
    integer(kind=real64) :: area

    integer :: n, num_events, num_y, i, j
    type(Event), allocatable :: events(:)
    integer(K), allocatable :: y_vals(:), unique_y(:)
    integer, allocatable :: lap(:)
    integer(kind=int64) :: current_x, dx, covered_y
    integer :: j1, j2

    n = size(boxes)
    area = 0
    if (n == 0) return

    ! 1. Collect and compress Y coordinates
    allocate(y_vals(2*n))
    do i = 1, n
       y_vals(2*i - 1) = min(boxes(i)%y1, boxes(i)%y2)
       y_vals(2*i)     = max(boxes(i)%y1, boxes(i)%y2)
    end do

    call sort_int_array(y_vals)

    ! Remove duplicates to create our y-axis segments
    allocate(unique_y(2*n))
    num_y = 1
    unique_y(1) = y_vals(1)
    do i = 2, 2*n
       if (y_vals(i) /= unique_y(num_y)) then
          num_y = num_y + 1
          unique_y(num_y) = y_vals(i)
       end if
    end do

    ! 2. Create Event Queue
    allocate(events(2*n))
    do i = 1, n
       ! Left Edge Event
       events(2*i - 1)%x          = min(boxes(i)%x1, boxes(i)%x2)
       events(2*i - 1)%y1         = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%y2         = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%lap_change = 1

       ! Right Edge Event
       events(2*i)%x              = max(boxes(i)%x1, boxes(i)%x2)
       events(2*i)%y1             = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%y2             = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%lap_change     = -1
    end do

    ! Sort events primarily by X coordinate
    call sort_events(events)

    ! 3. Sweep Line Algorithm
    allocate(lap(num_y - 1))
    lap = 0
    area = 0
    current_x = events(1)%x

    do i = 1, 2*n
       dx = int(events(i)%x,int64) - current_x

       ! If we have moved horizontally, calculate area for the previous slice
       if (dx > 0) then
          covered_y = 0_int64
          do j = 1, num_y - 1
             if (lap(j) > 0) then
                covered_y = covered_y + int(unique_y(j+1) - unique_y(j),int64)
             end if
          end do

          area = area + (dx * covered_y)
          current_x = int(events(i)%x,int64)
       end if

       ! Apply the current event to our Y-scanline lap counts
       j1 = binary_search_y(unique_y, num_y, events(i)%y1)
       j2 = binary_search_y(unique_y, num_y, events(i)%y2)

       do j = j1, j2 - 1
          lap(j) = lap(j) + events(i)%lap_change
       end do
    end do

  end function calculate_union_area
  !> box_count is the output count, since memory is recycled using move_alloc
  pure subroutine old_heal_boxes(input_box_count, boxes, output_box_count)
    integer(kind=int64), intent(in) :: input_box_count    
    type(Box), allocatable, intent(inout) :: boxes(:)
    integer(kind=int64), intent(out) :: output_box_count
    type(Box), allocatable :: healed_boxes(:)

    integer :: n, num_y, i, j
    type(Event), allocatable :: events(:)
    integer(K), allocatable :: y_vals(:), unique_y(:) ! Assuming 'K' is defined in module
    integer, allocatable :: lap(:)
    integer(kind=int64) :: current_x, next_x
    integer :: j1, j2

    ! --- Variables for Output Collection ---
    type(Box), allocatable :: temp_boxes(:), resized_boxes(:)
    integer :: max_boxes
    logical :: in_segment
    integer(kind=int64) :: y_start, y_end

    n = input_box_count
    if (n == 0) then
       allocate(healed_boxes(0))
       return
    end if

    ! 1. Collect and compress Y coordinates
    allocate(y_vals(2*n))
    do i = 1, n
       y_vals(2*i - 1) = min(boxes(i)%y1, boxes(i)%y2)
       y_vals(2*i)     = max(boxes(i)%y1, boxes(i)%y2)
    end do

    call sort_int_array(y_vals)

    ! Remove duplicates to create our y-axis segments
    allocate(unique_y(2*n))
    num_y = 1
    unique_y(1) = y_vals(1)
    do i = 2, 2*n
       if (y_vals(i) /= unique_y(num_y)) then
          num_y = num_y + 1
          unique_y(num_y) = y_vals(i)
       end if
    end do

    ! 2. Create Event Queue
    allocate(events(2*n))
    do i = 1, n
       events(2*i - 1)%x          = min(boxes(i)%x1, boxes(i)%x2)
       events(2*i - 1)%y1         = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%y2         = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%lap_change = 1

       events(2*i)%x              = max(boxes(i)%x1, boxes(i)%x2)
       events(2*i)%y1             = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%y2             = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%lap_change     = -1
    end do

    call sort_events(events)

    ! 3. Sweep Line Algorithm
    allocate(lap(num_y - 1))
    lap = 0

    ! Initial allocation for output array (dynamically grows if needed)
    max_boxes = max(100, 4 * n)
    allocate(temp_boxes(max_boxes))
    output_box_count = 0

    current_x = int(events(1)%x, int64)

    do i = 1, 2*n
       next_x = int(events(i)%x, int64)

       ! If the sweep line has moved right, process the active overlapping regions
       if (next_x > current_x) then
          in_segment = .false.

          do j = 1, num_y - 1
             if (lap(j) > 0 .and. .not. in_segment) then
                ! Start of a newly merged contiguous segment
                in_segment = .true.
                y_start = unique_y(j)
             else if (lap(j) == 0 .and. in_segment) then
                ! End of the contiguous segment -> Generate a Box
                in_segment = .false.
                y_end = unique_y(j)

                output_box_count = output_box_count + 1
                ! Reallocate if we hit capacity
                if (output_box_count > max_boxes) then
                   allocate(resized_boxes(max_boxes * 2))
                   resized_boxes(1:max_boxes) = temp_boxes
                   call move_alloc(resized_boxes, temp_boxes)
                   max_boxes = max_boxes * 2
                end if

                temp_boxes(output_box_count)%x1 = current_x
                temp_boxes(output_box_count)%x2 = next_x
                temp_boxes(output_box_count)%y1 = y_start
                temp_boxes(output_box_count)%y2 = y_end
             end if
          end do

          ! Catch the edge case where the union touches the absolute top boundary
          if (in_segment) then
             y_end = unique_y(num_y)
             output_box_count = output_box_count + 1
             if (output_box_count > max_boxes) then
                allocate(resized_boxes(max_boxes * 2))
                resized_boxes(1:max_boxes) = temp_boxes
                call move_alloc(resized_boxes, temp_boxes)
                max_boxes = max_boxes * 2
             end if
             temp_boxes(output_box_count)%x1 = current_x
             temp_boxes(output_box_count)%x2 = next_x
             temp_boxes(output_box_count)%y1 = y_start
             temp_boxes(output_box_count)%y2 = y_end
          end if

          current_x = next_x
       end if

       ! Update scanline laps with current event
       j1 = binary_search_y(unique_y, num_y, events(i)%y1)
       j2 = binary_search_y(unique_y, num_y, events(i)%y2)

       do j = j1, j2 - 1
          lap(j) = lap(j) + events(i)%lap_change
       end do
    end do

    ! 4. Finalize the correctly sized output collection
    if( output_box_count == 0 ) error stop "PLEASE ANALYZE"
    if( output_box_count > 0 .and. output_box_count < size(boxes) ) then
       boxes(1:output_box_count) = temp_boxes(1:output_box_count)
    else
       allocate(healed_boxes(output_box_count))
       if (output_box_count > 0) then
          healed_boxes(1:output_box_count) = temp_boxes(1:output_box_count)
       end if
       call move_alloc(from=healed_boxes, to=boxes)
    end if
  end subroutine old_heal_boxes
  pure subroutine heal_boxes(input_box_count, boxes, output_box_count)
    integer(kind=int64), intent(in) :: input_box_count  
    type(Box), allocatable, intent(inout) :: boxes(:)
    integer(kind=int64), intent(out) :: output_box_count

    ! Internal variables
    integer :: n, num_y, i, j
    type(Event), allocatable :: events(:)
    integer(K), allocatable :: y_vals(:) 
    integer, allocatable :: lap(:)
    integer(kind=int64) :: current_x, next_x
    integer :: j1, j2

    ! Output collection
    type(Box), allocatable :: temp_boxes(:), resized_boxes(:), healed_boxes(:)
    integer :: max_boxes
    logical :: in_segment
    integer(kind=int64) :: y_start, y_end

    n = input_box_count

    ! --- FIX 1: Properly handle n == 0 ---
    if (n == 0) then
       output_box_count = 0
       allocate(healed_boxes(0))
       call move_alloc(from=healed_boxes, to=boxes)
       return
    end if

    ! 1. Collect, sort, and compress Y coordinates IN-PLACE
    allocate(y_vals(2*n))
    do i = 1, n
       y_vals(2*i - 1) = min(boxes(i)%y1, boxes(i)%y2)
       y_vals(2*i)     = max(boxes(i)%y1, boxes(i)%y2)
    end do

    call sort_int_array(y_vals)

    ! --- FIX 2: In-place unique check (eliminates unique_y entirely) ---
    num_y = 1
    do i = 2, 2*n
       if (y_vals(i) /= y_vals(num_y)) then
          num_y = num_y + 1
          y_vals(num_y) = y_vals(i)
       end if
    end do
    ! Now y_vals(1:num_y) acts as our unique y-axis boundaries.

    ! 2. Create Event Queue
    allocate(events(2*n))
    do i = 1, n
       events(2*i - 1)%x          = min(boxes(i)%x1, boxes(i)%x2)
       events(2*i - 1)%y1         = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%y2         = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%lap_change = 1

       events(2*i)%x              = max(boxes(i)%x1, boxes(i)%x2)
       events(2*i)%y1             = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%y2             = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%lap_change     = -1
    end do

    call sort_events(events)

    ! 3. Sweep Line Algorithm
    allocate(lap(num_y - 1))
    lap = 0

    ! --- FIX 3: Start with a sensible allocation, not 4*n ---
    max_boxes = max(100, n) 
    allocate(temp_boxes(max_boxes))
    output_box_count = 0

    current_x = int(events(1)%x, int64)

    do i = 1, 2*n
       next_x = int(events(i)%x, int64)

       if (next_x > current_x) then
          in_segment = .false.

          do j = 1, num_y - 1
             if (lap(j) > 0 .and. .not. in_segment) then
                in_segment = .true.
                y_start = y_vals(j)
             else if (lap(j) == 0 .and. in_segment) then
                in_segment = .false.
                y_end = y_vals(j)

                output_box_count = output_box_count + 1

                ! --- FIX 4: 1.5x scaling to reduce reallocation memory spikes ---
                if (output_box_count > max_boxes) then
                   max_boxes = max_boxes + (max_boxes / 2) + 1 
                   allocate(resized_boxes(max_boxes))
                   resized_boxes(1:output_box_count-1) = temp_boxes
                   call move_alloc(resized_boxes, temp_boxes)
                end if

                temp_boxes(output_box_count)%x1 = current_x
                temp_boxes(output_box_count)%x2 = next_x
                temp_boxes(output_box_count)%y1 = y_start
                temp_boxes(output_box_count)%y2 = y_end
             end if
          end do

          ! Catch edge case
          if (in_segment) then
             y_end = y_vals(num_y)
             output_box_count = output_box_count + 1

             if (output_box_count > max_boxes) then
                max_boxes = max_boxes + (max_boxes / 2) + 1
                allocate(resized_boxes(max_boxes))
                resized_boxes(1:output_box_count-1) = temp_boxes
                call move_alloc(resized_boxes, temp_boxes)
             end if

             temp_boxes(output_box_count)%x1 = current_x
             temp_boxes(output_box_count)%x2 = next_x
             temp_boxes(output_box_count)%y1 = y_start
             temp_boxes(output_box_count)%y2 = y_end
          end if

          current_x = next_x
       end if

       ! Update scanline laps with current event using our compressed y_vals
       j1 = binary_search_y(y_vals, num_y, events(i)%y1)
       j2 = binary_search_y(y_vals, num_y, events(i)%y2)

       do j = j1, j2 - 1
          lap(j) = lap(j) + events(i)%lap_change
       end do
    end do

    ! --- FIX 5: Explicitly free internals right before finalizing ---
    deallocate(events)
    deallocate(y_vals)
    deallocate(lap)

    ! 4. Finalize the correctly sized output collection
    if( output_box_count == 0 ) error stop "PLEASE ANALYZE"
    if( output_box_count > 0 .and. output_box_count < size(boxes) ) then
       boxes(1:output_box_count) = temp_boxes(1:output_box_count)
    else
       allocate(healed_boxes(output_box_count))
       if (output_box_count > 0) then
          healed_boxes(1:output_box_count) = temp_boxes(1:output_box_count)
       end if
       call move_alloc(from=healed_boxes, to=boxes)
    end if
  end subroutine heal_boxes

  pure function calculate_polygon_union_area(X, Y, poly_start, poly_end) result(area)
    integer(kind=int32), intent(in) :: X(:), Y(:)
    integer(kind=int32), intent(in) :: poly_start(:), poly_end(:)
    integer(K) :: area

    integer :: p, s, e, k, num_polygons, num_events, num_y, i, j
    integer :: is_ccw, j1, j2
    integer(kind=int64) :: signed_area, dy, current_x, dx, covered_y

    ! Arrays sized safely to the maximum possible vertices
    type(Event), allocatable :: events(:)
    integer(kind=int32), allocatable :: y_vals(:), unique_y(:)
    integer, allocatable :: lap(:)

    num_polygons = size(poly_start)
    area = 0
    if (num_polygons == 0) return

    allocate(events(size(X)))
    allocate(y_vals(size(X)))

    num_events = 0
    num_y = 0

    ! ==========================================
    ! 1. Event Generation & Orientation Checking
    ! ==========================================
    do p = 1, num_polygons
       s = poly_start(p)
       e = poly_end(p)

       ! A. Calculate Signed Area to determine CW/CCW
       signed_area = 0
       do k = s, e - 1
          signed_area = signed_area + X(k) * Y(k+1) - X(k+1) * Y(k)
       end do

       if (signed_area > 0) then
          is_ccw = 1
       else if (signed_area < 0) then
          is_ccw = -1
       else
          cycle ! Degenerate zero-area polygon, skip it
       end if

       ! B. Extract Vertical Edges
       do k = s, e - 1
          ! A vertical edge has same X, different Y
          if (X(k) == X(k+1) .and. Y(k) /= Y(k+1)) then
             num_events = num_events + 1
             events(num_events)%x = X(k)
             events(num_events)%y1 = min(Y(k), Y(k+1))
             events(num_events)%y2 = max(Y(k), Y(k+1))

             ! Collect Y coordinates for compression later
             num_y = num_y + 1
             y_vals(num_y) = min(Y(k), Y(k+1))
             num_y = num_y + 1
             y_vals(num_y) = max(Y(k), Y(k+1))

             ! Assign Lap Count Parity based on direction and orientation
             dy = Y(k+1) - Y(k)
             if (dy > 0) then
                ! Going UP
                events(num_events)%lap_change = -1 * is_ccw
             else
                ! Going DOWN
                events(num_events)%lap_change =  1 * is_ccw
             end if
          end if
       end do
    end do

    if (num_events == 0) return

    ! ==========================================
    ! 2. Coordinate Compression (Y-Axis)
    ! ==========================================
    call sort_int_array(y_vals(1:num_y))

    allocate(unique_y(num_y))
    j = 1
    unique_y(1) = y_vals(1)
    do i = 2, num_y
       if (y_vals(i) /= unique_y(j)) then
          j = j + 1
          unique_y(j) = y_vals(i)
       end if
    end do
    num_y = j ! num_y is now the count of UNIQUE y coordinates

    ! ==========================================
    ! 3. Sweep Line Algorithm
    ! ==========================================
    call sort_events(events(1:num_events))

    allocate(lap(num_y - 1))
    lap = 0
    area = 0
    current_x = events(1)%x

    do i = 1, num_events
       dx = events(i)%x - current_x

       if (dx > 0) then
          covered_y = 0
          do j = 1, num_y - 1
             if (lap(j) > 0) then
                covered_y = covered_y + (unique_y(j+1) - unique_y(j))
             end if
          end do

          area = area + (dx * covered_y)
          current_x = events(i)%x
       end if

       j1 = binary_search_y(unique_y, num_y, events(i)%y1)
       j2 = binary_search_y(unique_y, num_y, events(i)%y2)

       do j = j1, j2 - 1
          lap(j) = lap(j) + events(i)%lap_change
       end do
    end do

  end function calculate_polygon_union_area

  ! --- Auxiliary Subroutines ---

  ! Binary search for rapid index lookup of y-coordinates
  pure function binary_search_y(arr, n, val) result(idx)
    integer(K), intent(in) :: arr(:)
    integer, intent(in) :: n
    integer(K), intent(in) :: val
    integer :: idx, low, high, mid

    low = 1
    high = n
    idx = -1
    do while (low <= high)
       mid = (low + high) / 2
       if (arr(mid) == val) then
          idx = mid
          return
       else if (arr(mid) < val) then
          low = mid + 1
       else
          high = mid - 1
       end if
    end do
  end function binary_search_y

  ! In-place Quicksort for 64-bit integers
  pure recursive subroutine sort_int_array(arr)
    integer(K), intent(inout) :: arr(:)
    integer(K) :: pivot, temp
    integer :: i, j, left, right

    if (size(arr) <= 1) return
    left = 1
    right = size(arr)
    pivot = arr((left + right) / 2)
    i = left
    j = right

    do while (i <= j)
       do while (arr(i) < pivot)
          i = i + 1
       end do
       do while (arr(j) > pivot)
          j = j - 1
       end do
       if (i <= j) then
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
          i = i + 1
          j = j - 1
       end if
    end do

    if (left < j) call sort_int_array(arr(left:j))
    if (i < right) call sort_int_array(arr(i:right))
  end subroutine sort_int_array

  ! In-place Quicksort for Events (sorting by X-coordinate)
  pure recursive subroutine sort_events(arr)
    type(Event), intent(inout) :: arr(:)
    integer(K) :: pivot
    type(Event) :: temp
    integer :: i, j, left, right

    if (size(arr) <= 1) return
    left = 1
    right = size(arr)
    pivot = arr((left + right) / 2)%x
    i = left
    j = right

    do while (i <= j)
       do while (arr(i)%x < pivot)
          i = i + 1
       end do
       do while (arr(j)%x > pivot)
          j = j - 1
       end do
       if (i <= j) then
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
          i = i + 1
          j = j - 1
       end if
    end do

    if (left < j) call sort_events(arr(left:j))
    if (i < right) call sort_events(arr(i:right))
  end subroutine sort_events


  !Permutaion code using INDIRECT SORTING
  !Instead of shuffling this AugmentBox data, can I generate an array of integers and then sort
  !that into a permutation array ? Write a Fortran function which accepts the Box array (make
  !it general so I can pass Box or AugmentBox) and as output in the subroutie it allocates and
  !returns a permutation array of same size as Box array, but which contains the sort order.
  ! =========================================================
  ! 1. Implementation for type(Box)
  ! =========================================================
  subroutine get_box_permutation(arr, perm)
    type(Box), intent(in) :: arr(:)
    integer(int64), allocatable, intent(out) :: perm(:)
    integer(int64) :: i, n

    n = size(arr, kind=int64)
    allocate(perm(n))

    ! Initialize permutation array with 1, 2, 3... N
    do i = 1, n
       perm(i) = i
    end do

    ! Sort the indices
    if (n > 1) then
       call indirect_quicksort_box(arr, perm, 1_int64, n)
    end if
  end subroutine get_box_permutation

  pure recursive subroutine indirect_quicksort_box(arr, perm, left, right)
    type(Box), intent(in) :: arr(:)
    integer(int64), intent(inout) :: perm(:)
    integer(int64), intent(in) :: left, right

    integer(int64) :: i, j, temp
    type(Box) :: pivot_val

    if (left < right) then
       ! The pivot VALUE is looked up via the permutation array
       pivot_val = arr(perm((left + right) / 2))
       i = left
       j = right

       do while (i <= j)
          ! Compare values using indices from perm
          do while (box_less_than(arr(perm(i)), pivot_val))
             i = i + 1
          end do
          do while (box_less_than(pivot_val, arr(perm(j))))
             j = j - 1
          end do

          if (i <= j) then
             ! Swap the INDICES, not the actual Box data
             temp = perm(i)
             perm(i) = perm(j)
             perm(j) = temp
             i = i + 1
             j = j - 1
          end if
       end do

       if (left < j)  call indirect_quicksort_box(arr, perm, left, j)
       if (i < right) call indirect_quicksort_box(arr, perm, i, right)
    end if
  end subroutine indirect_quicksort_box

  !> Polygon booleans
  !Now that we have sorting for X,Y and we have   pure function
  !calculate_polygon_union_area(X, Y, poly_start, poly_end) result(area)
  !lets start adding polygon booleans for rectilinear polygons.
  !So we will construct a new type which is
  !type ShapeCollection
  !integer(kind=K), allocatable: X(:),Y(:),poly_start(:),poly_end(:)
  !and we want to write something like
  subroutine PolygonBooleanAND(A, B, C)
    type(ShapeCollection), intent(in)  :: A, B
    type(ShapeCollection), intent(out) :: C
    !integer, parameter :: K = int64
    type(Event), allocatable :: events(:)
    integer(kind=K), allocatable  :: y_vals(:), unique_y(:)
    integer, allocatable     :: lap_A(:), lap_B(:)
    logical, allocatable     :: is_inside(:)
    integer(kind=K), allocatable  :: start_x(:)

    integer :: num_events, num_y, i, j, j1, j2
    integer(kind=K) :: current_x
    logical :: new_inside

    ! Dynamic output buffers (oversized for safety, trimmed at the end)
    integer(int32), allocatable :: out_X(:), out_Y(:)
    integer(int32), allocatable :: out_start(:), out_end(:)
    integer :: out_vert_count, out_poly_count

    ! Allocate maximum possible bounds
    allocate(events(size(A%X) + size(B%X)))
    allocate(y_vals(size(A%X) + size(B%X)))
    num_events = 0
    num_y = 0

    ! ==========================================
    ! 1. Extract Events & Assign Ownership
    ! ==========================================
    call extract_shape_events(A, 1, events, num_events, y_vals, num_y)
    call extract_shape_events(B, 2, events, num_events, y_vals, num_y)

    if (num_events == 0) return

    ! ==========================================
    ! 2. Coordinate Compression
    ! ==========================================
    call sort_int_array(y_vals(1:num_y))

    allocate(unique_y(num_y))
    j = 1
    unique_y(1) = y_vals(1)
    do i = 2, num_y
       if (y_vals(i) /= unique_y(j)) then
          j = j + 1
          unique_y(j) = y_vals(i)
       end if
    end do
    num_y = j

    ! ==========================================
    ! 3. Boolean Sweep-Line Core
    ! ==========================================
    call sort_events(events(1:num_events))

    allocate(lap_A(num_y - 1), lap_B(num_y - 1))
    allocate(is_inside(num_y - 1), start_x(num_y - 1))
    lap_A = 0; lap_B = 0
    is_inside = .false.; start_x = 0

    ! Allocate temporary output buffers
    allocate(out_X(num_events * 5), out_Y(num_events * 5))
    allocate(out_start(num_events), out_end(num_events))
    out_vert_count = 0
    out_poly_count = 0

    i = 1
    do while (i <= num_events)
       current_x = events(i)%x

       ! A. Process ALL events that occur at this exact X coordinate simultaneously
       do while (i <= num_events .and. events(i)%x == current_x)
          j1 = binary_search_y(unique_y, num_y, events(i)%y1)
          j2 = binary_search_y(unique_y, num_y, events(i)%y2)

          if (events(i)%owner == 1) then
             lap_A(j1 : j2-1) = lap_A(j1 : j2-1) + events(i)%lap_change
          else
             lap_B(j1 : j2-1) = lap_B(j1 : j2-1) + events(i)%lap_change
          end if
          i = i + 1
       end do

       ! B. Check for State Changes (The Boolean AND Logic)
       do j = 1, num_y - 1
          ! THE BOOLEAN CONDITION: Change this to implement OR, NOT, XOR
          new_inside = (lap_A(j) > 0 .and. lap_B(j) > 0)

          if (new_inside .and. .not. is_inside(j)) then
             ! Segment just entered the intersection. Mark the start X.
             start_x(j) = current_x
             is_inside(j) = .true.

          else if (.not. new_inside .and. is_inside(j)) then
             ! Segment just exited the intersection. Emit a closed polygon strip.
             ! The strip goes from start_x(j) to current_x, between unique_y(j) and unique_y(j+1)

             out_poly_count = out_poly_count + 1
             out_start(out_poly_count) = out_vert_count + 1

             ! Point 1: Bottom Left
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(start_x(j), int32)
             out_Y(out_vert_count) = int(unique_y(j), int32)

             ! Point 2: Bottom Right
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(current_x, int32)
             out_Y(out_vert_count) = int(unique_y(j), int32)

             ! Point 3: Top Right
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(current_x, int32)
             out_Y(out_vert_count) = int(unique_y(j+1), int32)

             ! Point 4: Top Left
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(start_x(j), int32)
             out_Y(out_vert_count) = int(unique_y(j+1), int32)

             ! Point 5: Close the loop (Bottom Left again)
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(start_x(j), int32)
             out_Y(out_vert_count) = int(unique_y(j), int32)

             out_end(out_poly_count) = out_vert_count
             is_inside(j) = .false.
          end if
       end do
    end do

    ! ==========================================
    ! 4. Finalize Output ShapeCollection (Trim arrays)
    ! ==========================================
    allocate(C%X(out_vert_count), C%Y(out_vert_count))
    allocate(C%poly_start(out_poly_count), C%poly_end(out_poly_count))

    if (out_vert_count > 0) then
       C%X = out_X(1:out_vert_count)
       C%Y = out_Y(1:out_vert_count)
       C%poly_start = out_start(1:out_poly_count)
       C%poly_end = out_end(1:out_poly_count)
    end if

  end subroutine PolygonBooleanAND

  ! --- Helper to process orientation and populate the shared event queue ---
  subroutine extract_shape_events(shape, owner_id, events, num_events, y_vals, num_y)
    type(ShapeCollection), intent(in) :: shape
    integer, intent(in) :: owner_id
    type(Event), intent(inout) :: events(:)
    integer(kind=K), intent(inout) :: y_vals(:)
    integer, intent(inout) :: num_events, num_y

    integer :: p, s, e, kloop, is_ccw
    integer(kind=K) :: signed_area, dy

    if (.not. allocated(shape%poly_start)) return

    do p = 1, size(shape%poly_start)
       s = shape%poly_start(p)
       e = shape%poly_end(p)

       ! 1. Calculate Signed Area to determine Orientation
       signed_area = 0
       do kloop = s, e - 1
          signed_area = signed_area + &
               (int(shape%X(kloop),int64) * int(shape%Y(kloop+1),int64) - int(shape%X(kloop+1),int64) * int(shape%Y(kloop),int64))
       end do

       if (signed_area > 0) then
          is_ccw = 1
       else if (signed_area < 0) then
          is_ccw = -1
       else
          cycle 
       end if

       ! 2. Generate edges tagged with the owner_id
       do kloop = s, e - 1
          if (shape%X(kloop) == shape%X(kloop+1) .and. shape%Y(kloop) /= shape%Y(kloop+1)) then
             num_events = num_events + 1
             events(num_events)%owner = owner_id
             events(num_events)%x = int(shape%X(kloop), K)
             events(num_events)%y1 = min(int(shape%Y(kloop), K), int(shape%Y(kloop+1), K))
             events(num_events)%y2 = max(int(shape%Y(kloop), K), int(shape%Y(kloop+1), K))

             num_y = num_y + 1
             y_vals(num_y) = events(num_events)%y1
             num_y = num_y + 1
             y_vals(num_y) = events(num_events)%y2

             dy = int(shape%Y(kloop+1),int64) - int(shape%Y(kloop),int64)
             if (dy > 0) then
                events(num_events)%lap_change = -1 * is_ccw
             else
                events(num_events)%lap_change =  1 * is_ccw
             end if
          end if
       end do
    end do
  end subroutine extract_shape_events

  !ksubroutine PolygonBooleanAND( A, B, C)
  !where A B and C are of type ShapeCollection.
  !Please generate modern Fortran code for this using the previously defined functions on sorting and event processing
  !> Morton and other space filling curve sort
  !> Given an array of type(Box), how can we create a Morton index (x|y)
  !> then sort the array using that index and then use this sorted order
  !> to create an RTree and evaluate performance.
end module GeometryModule

