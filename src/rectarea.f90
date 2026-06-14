module rect_area_mod
  use iso_fortran_env, only: int64
  implicit none
  private

  ! Use 64-bit integers to prevent overflow when calculating large areas
  integer, parameter, public :: K = int64

  ! Core data structure for input
  type, public :: Box
     integer(K) :: x1, y1, x2, y2
  end type Box

  ! Auxiliary data structure for the sweep-line
  type :: Event
     integer(K) :: x
     integer(K) :: y1, y2
     integer(K) :: lap_change ! +1 for left edge, -1 for right edge
  end type Event

  public :: calculate_union_area

contains

  pure function calculate_union_area(boxes) result(area)
    type(Box), intent(in) :: boxes(:)
    integer(K) :: area

    integer :: n, num_events, num_y, i, j
    type(Event), allocatable :: events(:)
    integer(K), allocatable :: y_vals(:), unique_y(:)
    integer, allocatable :: lap(:)
    integer(K) :: current_x, dx, covered_y
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
       dx = events(i)%x - current_x

       ! If we have moved horizontally, calculate area for the previous slice
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

       ! Apply the current event to our Y-scanline lap counts
       j1 = binary_search_y(unique_y, num_y, events(i)%y1)
       j2 = binary_search_y(unique_y, num_y, events(i)%y2)

       do j = j1, j2 - 1
          lap(j) = lap(j) + events(i)%lap_change
       end do
    end do

  end function calculate_union_area

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

end module rect_area_mod

module BoxAreaCalc
  use iso_fortran_env
  implicit none

  integer, parameter :: int64 = selected_int_kind(18)
  integer, parameter :: K_COORDINATE_KIND = int32
  integer, parameter :: MAX_SKIP_LEVEL = 16

  type :: NodePtr
     type(SkipListNode), pointer :: ptr => null()
  end type NodePtr

  type :: SkipListNode
     integer(K_COORDINATE_KIND) :: y1, y2 ! The interval
     type(NodePtr) :: Forward(MAX_SKIP_LEVEL)
     ! Backward pointers are rarely needed for search, 
     ! keeping for your specific schema requirements
     type(NodePtr) :: Backward(MAX_SKIP_LEVEL) 
  end type SkipListNode

  type :: SkipList
     integer :: current_level
     type(SkipListNode), pointer :: header => null()
     type(SkipListNode), pointer :: Arena(:) => null()
     integer :: ArenaPtr
  end type SkipList

  type :: Box
     integer(K_COORDINATE_KIND) :: x1, y1, x2, y2
  end type Box

  type :: Event
     integer(K_COORDINATE_KIND) :: x
     integer :: type ! +1 for entry, -1 for exit
     integer(K_COORDINATE_KIND) :: y1, y2
  end type Event

contains

  ! Calculate Area
  function CalculateTotalArea(boxes) result(total_area)
    type(Box), intent(in) :: boxes(:)
    real(real64) :: total_area

    ! 1. Setup events (2 events per box)
    ! 2. Sort events by x coordinate
    ! 3. Iterate and use SkipList to manage active y-intervals

    ! Pseudocode for the loop:
    ! do i = 2 to 2*N_boxes
    !    width = events(i)%x - events(i-1)%x
    !    if (width > 0) then
    !        len = GetUnionLength(mySkipList)
    !        total_area = total_area + (width * len)
    !    end if
    !    update SkipList (Insert/Delete interval)
    ! end do
  end function CalculateTotalArea

  ! Efficient memory allocation from Arena
  function GetNewNode(list) result(node)
    type(SkipList), intent(inout) :: list
    type(SkipListNode), pointer :: node

    list%ArenaPtr = list%ArenaPtr + 1
    node => list%Arena(list%ArenaPtr)
  end function GetNewNode

end module BoxAreaCalc

