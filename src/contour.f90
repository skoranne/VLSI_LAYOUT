! File   : contour.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: Given Magic VLSI boxes, construct exterior/interior cycles
! We have the heal_boxes function, we have sorting, event, etc, now write a function
! which takes as input array of interacting and touching  boxes and returns array of
! polygon contour. The outer contour should be first returned, and the next contours
! are interior cycles.
! rect 7 4 11 5
! rect 7 2 8 4
! rect 10 2 11 4
! rect 7 1 11 2
! Firsr we remove overlaps.

module ContourExtractionModule
  use iso_fortran_env, only: int64, int32
  use GeometryModule

  implicit none
  private
  public :: Point, Polygon, extract_contours
  integer, parameter :: COORDINATE_KIND = int32
  type :: Point
     integer(kind=COORDINATE_KIND) :: x, y
  end type Point

  type :: Polygon
     type(Point), allocatable :: pts(:)
     integer(int64) :: signed_area
  end type Polygon

  type :: DirectedEdge
     integer(kind=COORDINATE_KIND) :: x1, y1, x2, y2
     integer(kind=COORDINATE_KIND) :: min_x, min_y, max_x, max_y
     logical :: is_active
  end type DirectedEdge

contains
  !--------------------------------------------------------------
  ! Wrapper for the edge sorting routine
  !--------------------------------------------------------------
  pure subroutine sort_edges_by_geometry(edges, n)
    type(DirectedEdge), intent(inout) :: edges(:)
    integer, intent(in)               :: n

    if (n > 1) then
       call quicksort_edges(edges, 1, n)
    end if
  end subroutine sort_edges_by_geometry

  !--------------------------------------------------------------
  ! Recursive Quicksort for DirectedEdge types
  !--------------------------------------------------------------
  pure recursive subroutine quicksort_edges(edges, left, right)
    type(DirectedEdge), intent(inout) :: edges(:)
    integer, intent(in)               :: left, right

    integer :: i, j
    type(DirectedEdge) :: pivot, temp

    if (left >= right) return

    ! Choose the middle element as the pivot
    pivot = edges((left + right) / 2)
    i = left
    j = right

    do while (i <= j)
       ! Scan left: find an element that is NOT less than the pivot
       do while (edge_is_less_than(edges(i), pivot))
          i = i + 1
       end do

       ! Scan right: find an element that is NOT greater than the pivot
       do while (edge_is_less_than(pivot, edges(j)))
          j = j - 1
       end do

       ! Swap if they are out of order
       if (i <= j) then
          temp = edges(i)
          edges(i) = edges(j)
          edges(j) = temp
          i = i + 1
          j = j - 1
       end if
    end do

    ! Recurse on the sub-partitions
    if (left < j)  call quicksort_edges(edges, left, j)
    if (i < right) call quicksort_edges(edges, i, right)

  end subroutine quicksort_edges

  !--------------------------------------------------------------
  ! Helper: Lexicographical comparison of two edges
  ! Order: min_x -> min_y -> max_x -> max_y
  !--------------------------------------------------------------
  pure function edge_is_less_than(a, b) result(is_less)
    type(DirectedEdge), intent(in) :: a, b
    logical :: is_less

    if (a%min_x /= b%min_x) then
       is_less = a%min_x < b%min_x
    else if (a%min_y /= b%min_y) then
       is_less = a%min_y < b%min_y
    else if (a%max_x /= b%max_x) then
       is_less = a%max_x < b%max_x
    else
       is_less = a%max_y < b%max_y
    end if
  end function edge_is_less_than
  !--------------------------------------------------------------
  ! Wrapper for Integer Array Sorting
  !--------------------------------------------------------------
  pure subroutine sort_int_array32(arr)
    use iso_fortran_env, only: int32
    integer(int32), intent(inout) :: arr(:)

    if (size(arr) > 1) then
       call quicksort_int32(arr, 1, size(arr))
    end if
  end subroutine sort_int_array32

  !--------------------------------------------------------------
  ! Recursive Quicksort for int32
  !--------------------------------------------------------------
  pure recursive subroutine quicksort_int32(arr, left, right)
    use iso_fortran_env, only: int32
    integer(int32), intent(inout) :: arr(:)
    integer, intent(in)           :: left, right

    integer :: i, j
    integer(int32) :: pivot, temp

    if (left >= right) return

    ! Choose the middle element as the pivot
    pivot = arr((left + right) / 2)
    i = left
    j = right

    do while (i <= j)
       ! Scan left
       do while (arr(i) < pivot)
          i = i + 1
       end do

       ! Scan right
       do while (arr(j) > pivot)
          j = j - 1
       end do

       ! Swap
       if (i <= j) then
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
          i = i + 1
          j = j - 1
       end if
    end do

    ! Recurse
    if (left < j)  call quicksort_int32(arr, left, j)
    if (i < right) call quicksort_int32(arr, i, right)
  end subroutine quicksort_int32

  !--------------------------------------------------------------
  ! Extracts topological contours from a raw array of interacting boxes
  !--------------------------------------------------------------
  subroutine extract_contours(raw_boxes, input_count, contours, num_contours)
    type(Box), intent(inout), allocatable :: raw_boxes(:)
    integer(int64), intent(in)            :: input_count
    type(Polygon), allocatable, intent(out) :: contours(:)
    integer, intent(out)                  :: num_contours

    integer(int64) :: healed_count
    type(DirectedEdge), allocatable :: edges(:)
    integer :: num_edges, max_edges, i, j

    ! --- Atomization Variables ---
    integer(kind=COORDINATE_KIND), allocatable :: y_vals(:), uy(:)
    integer :: num_uy, idx_y1, idx_y2

    ! --- Tracing Variables ---
    integer(int64) :: start_x, start_y, curr_x, curr_y
    logical :: cycle_closed, edge_found
    type(Point), allocatable :: temp_pts(:)
    integer :: pt_count
    type(Polygon) :: temp_poly

    ! 1. Heal the interacting boxes
    call heal_boxes(input_count, raw_boxes, healed_count)
    if (healed_count == 0) then
       num_contours = 0
       allocate(contours(0))
       return
    end if
    !write(*,*) 'InputCount = ', input_count, ' Healed count = ', healed_count
    ! 2. Extract Unique Y Grid for Atomization
    allocate(y_vals(2 * healed_count))
    do i = 1, healed_count
       y_vals(2*i - 1) = raw_boxes(i)%y1
       y_vals(2*i)     = raw_boxes(i)%y2
    end do
    call sort_int_array32(y_vals)  ! Ensure you have this helper available

    allocate(uy(2 * healed_count))
    num_uy = 1
    uy(1) = y_vals(1)
    do i = 2, 2 * healed_count
       if (y_vals(i) /= uy(num_uy)) then
          num_uy = num_uy + 1
          uy(num_uy) = y_vals(i)
       end if
    end do

    ! 3. Generate Directed Atomic Edges (CCW)
    ! --- THE FIX: Pre-calculate the EXACT number of edges required ---
    num_edges = 0
    do i = 1, healed_count
       idx_y1 = binary_search_y(uy, num_uy, raw_boxes(i)%y1)
       idx_y2 = binary_search_y(uy, num_uy, raw_boxes(i)%y2)

       ! Catch binary search failures just in case
       if (idx_y1 == 0 .or. idx_y2 == 0) then
          print *, "FATAL: Y-coordinate missing from unique grid!"
          stop
       end if

       ! 2 horizontal edges (Top + Bottom)
       ! Plus (idx_y2 - idx_y1) vertical segments on the Left
       ! Plus (idx_y2 - idx_y1) vertical segments on the Right
       num_edges = num_edges + 2 + 2 * (idx_y2 - idx_y1)
    end do

    ! Allocate exact memory, zero waste, zero overflows
    allocate(edges(num_edges))
    num_edges = 0 ! Reset counter to use as an index for add_edge

    ! --- Now do the actual population ---
    do i = 1, healed_count
       ! Bottom edge
       call add_edge(edges, num_edges, raw_boxes(i)%x1, raw_boxes(i)%y1, raw_boxes(i)%x2, raw_boxes(i)%y1)

       ! Top edge
       call add_edge(edges, num_edges, raw_boxes(i)%x2, raw_boxes(i)%y2, raw_boxes(i)%x1, raw_boxes(i)%y2)

       idx_y1 = binary_search_y(uy, num_uy, raw_boxes(i)%y1)
       idx_y2 = binary_search_y(uy, num_uy, raw_boxes(i)%y2)

       ! Left edge: Split into atomic Y segments (Top to Bottom)
       do j = idx_y2 - 1, idx_y1, -1
          call add_edge(edges, num_edges, raw_boxes(i)%x1, uy(j+1), raw_boxes(i)%x1, uy(j))
       end do

       ! Right edge: Split into atomic Y segments (Bottom to Top)
       do j = idx_y1, idx_y2 - 1
          call add_edge(edges, num_edges, raw_boxes(i)%x2, uy(j), raw_boxes(i)%x2, uy(j+1))
       end do
    end do

    ! 4. Cancel Internal Shared Edges
    call sort_edges_by_geometry(edges, num_edges)

    do i = 1, num_edges - 1
       if (edges(i)%is_active .and. edges(i+1)%is_active) then
          if (edges(i)%min_x == edges(i+1)%min_x .and. edges(i)%min_y == edges(i+1)%min_y .and. &
               edges(i)%max_x == edges(i+1)%max_x .and. edges(i)%max_y == edges(i+1)%max_y) then
             edges(i)%is_active   = .false.
             edges(i+1)%is_active = .false.
          end if
       end if
    end do

    ! 5. Trace the Cycles
    allocate(contours(num_edges / 4 + 1)) 
    num_contours = 0
    allocate(temp_pts(num_edges))

    !write(*,*) 'NUM_EDGES = ', num_edges
    do i = 1, num_edges
       if (.not. edges(i)%is_active) cycle

       pt_count = 1
       temp_pts(pt_count)%x = edges(i)%x1
       temp_pts(pt_count)%y = edges(i)%y1

       start_x = edges(i)%x1
       start_y = edges(i)%y1
       curr_x  = edges(i)%x2
       curr_y  = edges(i)%y2
       edges(i)%is_active = .false.

       cycle_closed = .false.

       do while (.not. cycle_closed)
          ! Collinear vertex compression
          if (pt_count >= 2) then
             if (.not. ( (temp_pts(pt_count-1)%x == temp_pts(pt_count)%x .and. temp_pts(pt_count)%x == curr_x) .or. &
                  (temp_pts(pt_count-1)%y == temp_pts(pt_count)%y .and. temp_pts(pt_count)%y == curr_y) )) then
                pt_count = pt_count + 1
             end if
          else
             pt_count = pt_count + 1
          end if
          temp_pts(pt_count)%x = curr_x
          temp_pts(pt_count)%y = curr_y

          if (curr_x == start_x .and. curr_y == start_y) then
             cycle_closed = .true.
             exit
          end if

          ! FIX: Fail-safe flag to prevent infinite loops
          edge_found = .false.
          do j = 1, num_edges
             if (edges(j)%is_active .and. edges(j)%x1 == curr_x .and. edges(j)%y1 == curr_y) then
                curr_x = edges(j)%x2
                curr_y = edges(j)%y2
                edges(j)%is_active = .false.
                edge_found = .true.
                exit
             end if
          end do

          if (.not. edge_found) then
             print *, "WARNING: Broken contour at X:", curr_x, " Y:", curr_y
             exit ! Break out to prevent a hang
          end if
       end do

       if (cycle_closed .and. pt_count > 3) then
          num_contours = num_contours + 1
          allocate(contours(num_contours)%pts(pt_count - 1))
          contours(num_contours)%pts = temp_pts(1:pt_count-1)
          contours(num_contours)%signed_area = calculate_shoelace_area(contours(num_contours)%pts)
       end if
    end do

    ! 6. Sort contours by Area (Outer = Pos, Hole = Neg)
    do i = 1, num_contours - 1
       do j = i + 1, num_contours
          if (contours(j)%signed_area > contours(i)%signed_area) then
             temp_poly   = contours(i)
             contours(i) = contours(j)
             contours(j) = temp_poly
          end if
       end do
    end do

  end subroutine extract_contours

  !--------------------------------------------------------------
  ! Fast Binary Search for Y-Coordinate Grid Indices
  !--------------------------------------------------------------
  pure function binary_search_y(arr, n, target_val) result(idx)
    use iso_fortran_env, only: int32
    integer(kind=COORDINATE_KIND), intent(in) :: arr(:)
    integer, intent(in)        :: n
    integer(kind=COORDINATE_KIND), intent(in) :: target_val
    integer :: idx
    integer :: low, high, mid

    low = 1
    high = n
    idx = 0

    do while (low <= high)
       mid = low + (high - low) / 2
       if (arr(mid) == target_val) then
          idx = mid
          return
       else if (arr(mid) < target_val) then
          low = mid + 1
       else
          high = mid - 1
       end if
    end do
  end function binary_search_y
  !--------------------------------------------------------------
  ! Helper: Adds a directed edge to the tracking array
  !--------------------------------------------------------------
  pure subroutine add_edge(edges, count, x1, y1, x2, y2)
    type(DirectedEdge), intent(inout) :: edges(:)
    integer, intent(inout) :: count
    integer(kind=COORDINATE_KIND), intent(in) :: x1, y1, x2, y2

    count = count + 1
    edges(count)%x1 = x1
    edges(count)%y1 = y1
    edges(count)%x2 = x2
    edges(count)%y2 = y2
    edges(count)%min_x = min(x1, x2)
    edges(count)%max_x = max(x1, x2)
    edges(count)%min_y = min(y1, y2)
    edges(count)%max_y = max(y1, y2)
    edges(count)%is_active = .true.
  end subroutine add_edge

  !--------------------------------------------------------------
  ! Helper: Shoelace Area (Positive = CCW Outer, Negative = CW Hole)
  !--------------------------------------------------------------
  pure function calculate_shoelace_area(pts) result(area)
    type(Point), intent(in) :: pts(:)
    integer(int64) :: area
    integer :: i, n

    area = 0
    n = size(pts)
    if (n < 3) return

    do i = 1, n - 1
       area = area + (pts(i)%x * pts(i+1)%y) - (pts(i+1)%x * pts(i)%y)
    end do
    ! Add closing segment
    area = area + (pts(n)%x * pts(1)%y) - (pts(1)%x * pts(n)%y)

    ! Standard shoelace requires dividing by 2, but we just need it for relative sizing 
    ! and sign (+/-), so dividing by 2 is safely omitted for integer speed.
  end function calculate_shoelace_area

end module ContourExtractionModule
