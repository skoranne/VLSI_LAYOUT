module mod_union_area_segtree
  implicit none
  private

  integer, parameter :: real64 = selected_real_kind(15, 307)
  integer, parameter :: int64  = selected_int_kind(18)
  integer, parameter :: K_COORDINATE_KIND = int64 

  public :: calculate_union_area_st
  public :: Box

  type :: Box
     integer(kind=K_COORDINATE_KIND) :: x1, y1, x2, y2
  end type Box

  type :: Event
     integer(kind=K_COORDINATE_KIND) :: x, y1, y2
     integer(kind=int64)             :: lap_change
  end type Event

contains

  ! =========================================================================
  ! SWEEP LINE USING STATIC SEGMENT TREE
  ! =========================================================================
  pure function calculate_union_area_st(boxes) result(area)
    type(Box), intent(in) :: boxes(:)
    integer(kind=real64)  :: area

    integer :: n, i, ev_idx, num_y
    type(Event), allocatable :: events(:)
    integer(kind=K_COORDINATE_KIND), allocatable :: y_vals(:), unique_y(:)

    ! Static Arrays backing the Segment Tree (No Pointers!)
    integer(kind=int64), allocatable :: tree_count(:)
    integer(kind=K_COORDINATE_KIND), allocatable :: tree_length(:)

    integer(kind=K_COORDINATE_KIND) :: current_x, dx

    n = size(boxes)
    area = 0.0_real64
    if (n == 0) return

    ! 1. Collect Events and Y-coordinates
    allocate(events(2*n))
    allocate(y_vals(2*n))

    do i = 1, n
       events(2*i - 1)%x          = min(boxes(i)%x1, boxes(i)%x2)
       events(2*i - 1)%y1         = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%y2         = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%lap_change = 1_int64

       events(2*i)%x          = max(boxes(i)%x1, boxes(i)%x2)
       events(2*i)%y1         = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%y2         = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%lap_change = -1_int64

       ! Collect every Y boundary for the tree construction
       y_vals(2*i - 1) = events(2*i - 1)%y1
       y_vals(2*i)     = events(2*i - 1)%y2
    end do

    ! 2. Coordinate Compression (Offline Setup)
    call quicksort_events(events, 1, 2*n)
    call quicksort_y(y_vals, 1, 2*n)

    ! Remove duplicates to define the base intervals
    allocate(unique_y(2*n))
    num_y = 1
    unique_y(1) = y_vals(1)
    do i = 2, 2*n
       if (y_vals(i) /= unique_y(num_y)) then
          num_y = num_y + 1
          unique_y(num_y) = y_vals(i)
       end if
    end do

    ! If there are no intervals, there is no area
    if (num_y <= 1) return

    ! Allocate the Segment Tree 
    ! A standard binary tree needs 4*N nodes to safely cover N leaf intervals
    allocate(tree_count(4 * (num_y - 1)))
    allocate(tree_length(4 * (num_y - 1)))
    tree_count = 0_int64
    tree_length = 0_int64

    ! 3. Sweep Line Algorithm
    current_x = events(1)%x
    ev_idx = 1

    do while (ev_idx <= 2*n)
       dx = events(ev_idx)%x - current_x

       if (dx > 0) then
          ! O(1) QUERY: The root node (index 1) instantly holds the 
          ! total covered Y-length of the entire universe!
          area = area + real(dx, real64) * real(tree_length(1), real64)
          current_x = events(ev_idx)%x
       end if

       do while (ev_idx <= 2*n)
          if (events(ev_idx)%x /= current_x) exit

          ! Apply the interval to the tree structure
          call segtree_update(1, 1, num_y - 1, &
               events(ev_idx)%y1, events(ev_idx)%y2, &
               events(ev_idx)%lap_change, &
               unique_y, tree_count, tree_length)
          ev_idx = ev_idx + 1
       end do
    end do
  end function calculate_union_area_st

  ! =========================================================================
  ! RECURSIVE SEGMENT TREE UPDATE
  ! =========================================================================
  pure recursive subroutine segtree_update(node, left, right, q_y1, q_y2, val, unique_y, count, length)
    integer, intent(in) :: node, left, right
    integer(kind=K_COORDINATE_KIND), intent(in) :: q_y1, q_y2
    integer(kind=int64), intent(in) :: val
    integer(kind=K_COORDINATE_KIND), intent(in) :: unique_y(:)
    integer(kind=int64), intent(inout) :: count(:)
    integer(kind=K_COORDINATE_KIND), intent(inout) :: length(:)

    integer :: mid
    integer(kind=K_COORDINATE_KIND) :: node_y1, node_y2

    ! Map tree boundaries to actual real-world Y coordinates
    node_y1 = unique_y(left)
    node_y2 = unique_y(right + 1)

    ! Case A: This node is completely outside the target interval
    if (node_y2 <= q_y1 .or. node_y1 >= q_y2) return

    ! Case B: This node fits entirely inside the target interval
    if (node_y1 >= q_y1 .and. node_y2 <= q_y2) then
       count(node) = count(node) + val
    else
       ! Case C: Partial overlap. Split down the middle and recurse.
       mid = left + (right - left) / 2
       call segtree_update(2 * node, left, mid, q_y1, q_y2, val, unique_y, count, length)
       call segtree_update(2 * node + 1, mid + 1, right, q_y1, q_y2, val, unique_y, count, length)
    end if

    ! RECALCULATE NODE LENGTH
    if (count(node) > 0) then
       ! Entire block is covered by at least one bounding box
       length(node) = node_y2 - node_y1
    else if (left == right) then
       ! Reached an empty leaf
       length(node) = 0_int64
    else
       ! Merge data upward from children
       length(node) = length(2 * node) + length(2 * node + 1)
    end if
  end subroutine segtree_update

  ! --- (Your existing quicksort_events goes here) ---
  ! ...

  ! --- Simple QuickSort helper for the Y-Coordinate Array ---
  pure recursive subroutine quicksort_y(arr, left, right)
    integer(kind=K_COORDINATE_KIND), intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer :: i, j
    integer(kind=K_COORDINATE_KIND) :: temp, pivot

    if (left >= right) return
    pivot = arr(left + (right - left) / 2)
    i = left
    j = right

    do while (i <= j)
       do while (arr(i) < pivot); i = i + 1; end do
          do while (arr(j) > pivot); j = j - 1; end do
             if (i <= j) then
                temp = arr(i); arr(i) = arr(j); arr(j) = temp
                i = i + 1; j = j - 1
             end if
          end do

          if (left < j)  call quicksort_y(arr, left, j)
          if (i < right) call quicksort_y(arr, i, right)
  end subroutine quicksort_y

end module mod_union_area_segtree

