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

! File   : contour.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: Given Magic VLSI boxes, construct exterior/interior cycles
! Patched: Int64 Promotion for >2 Billion Edge VLSI Layouts

module ContourExtractionModule
  use iso_fortran_env, only: int64, int32
  use GeometryModule
  use PolygonFractureModule
  implicit none
  private
  public :: Point, Polygon, extract_contours, contours_to_trackers

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
  ! Wrapper for the edge sorting routine (Upgraded to int64)
  !--------------------------------------------------------------
  pure subroutine sort_edges_by_geometry(edges, n)
    type(DirectedEdge), intent(inout) :: edges(:)
    integer(int64), intent(in)        :: n

    if (n > 1_int64) then
       call quicksort_edges(edges, 1_int64, n)
    end if
  end subroutine sort_edges_by_geometry

  !--------------------------------------------------------------
  ! Recursive Quicksort for DirectedEdge types (Upgraded to int64)
  !--------------------------------------------------------------
  pure recursive subroutine quicksort_edges(edges, left, right)
    type(DirectedEdge), intent(inout) :: edges(:)
    integer(int64), intent(in)        :: left, right

    integer(int64) :: i, j, k, m
    type(DirectedEdge) :: pivot, temp

    integer(int64), parameter :: K_SMALL_THRESHOLD = 32_int64

    if (left >= right) return

    if (right - left <= K_SMALL_THRESHOLD) then
       do k = left + 1_int64, right
          temp = edges(k)
          m = k - 1_int64
          do while (m >= left)
             if (edge_is_less_than(temp, edges(m))) then
                edges(m + 1_int64) = edges(m)
                m = m - 1_int64
             else
                exit
             end if
          end do
          edges(m + 1_int64) = temp
       end do
    else
       pivot = edges((left + right) / 2_int64)
       i = left
       j = right

       do while (i <= j)
          do while (edge_is_less_than(edges(i), pivot))
             i = i + 1_int64
          end do
          do while (edge_is_less_than(pivot, edges(j)))
             j = j - 1_int64
          end do
          if (i <= j) then
             temp = edges(i)
             edges(i) = edges(j)
             edges(j) = temp
             i = i + 1_int64
             j = j - 1_int64
          end if
       end do

       if (left < j)  call quicksort_edges(edges, left, j)
       if (i < right) call quicksort_edges(edges, i, right)
    end if
  end subroutine quicksort_edges

  !--------------------------------------------------------------
  ! Helper: Lexicographical comparison of two edges
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
  ! Wrapper for Integer Array Sorting (Upgraded to int64 bounds)
  !--------------------------------------------------------------
  pure subroutine sort_int_array32(arr)
    integer(int32), intent(inout) :: arr(:)
    if (size(arr, kind=int64) > 1_int64) then
       call quicksort_int32(arr, 1_int64, size(arr, kind=int64))
    end if
  end subroutine sort_int_array32

  !--------------------------------------------------------------
  ! Recursive Quicksort for int32 (Upgraded to int64 indices)
  !--------------------------------------------------------------
  pure recursive subroutine quicksort_int32(arr, left, right)
    integer(int32), intent(inout) :: arr(:)
    integer(int64), intent(in)    :: left, right
    integer(int64) :: i, j
    integer(int32) :: pivot, temp

    if (left >= right) return

    pivot = arr((left + right) / 2_int64)
    i = left
    j = right

    do while (i <= j)
       do while (arr(i) < pivot)
          i = i + 1_int64
       end do
       do while (arr(j) > pivot)
          j = j - 1_int64
       end do
       if (i <= j) then
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
          i = i + 1_int64
          j = j - 1_int64
       end if
    end do

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
    integer(int64), intent(out)           :: num_contours

    integer(int64) :: healed_count
    type(DirectedEdge), allocatable :: edges(:)

    ! FIX: All edge counters MUST be int64 to prevent VLSI overflow
    integer(int64) :: num_edges, i, j

    ! --- Atomization Variables ---
    integer(kind=COORDINATE_KIND), allocatable :: y_vals(:), uy(:)
    integer(int64) :: num_uy, idx_y1, idx_y2

    ! --- Tracing Variables ---
    integer(kind=COORDINATE_KIND) :: start_x, start_y, curr_x, curr_y
    logical :: cycle_closed, edge_found
    type(Point), allocatable :: temp_pts(:)
    integer(int64) :: pt_count
    type(Polygon) :: temp_poly
    ! --- ADD THIS INTEGRITY CHECK BEFORE TRACING ---
    ! Check if every vertex has an even degree
    integer(int64) :: vertex_degree
    integer(int64) :: v_x, v_y
    do i=1,input_count
       if(.not. raw_boxes(i)%is_valid() ) error stop "INPUT BOX not valid"
    end do
    
    call heal_boxes(input_count, raw_boxes, healed_count)
    do i=1,healed_count
       if(.not. raw_boxes(i)%is_valid() ) error stop "INPUT BOX not valid"
    end do
    
    if (healed_count == 0_int64) then
       num_contours = 0_int64
       allocate(contours(0))
       return
    end if

    ! 2. Extract Unique Y Grid
    allocate(y_vals(2_int64 * healed_count))
    do i = 1_int64, healed_count
       y_vals(2_int64*i - 1_int64) = raw_boxes(i)%y1
       y_vals(2_int64*i)           = raw_boxes(i)%y2
    end do
    call sort_int_array32(y_vals)

    allocate(uy(2_int64 * healed_count))
    num_uy = 1_int64
    uy(1) = y_vals(1)
    do i = 2_int64, 2_int64 * healed_count
       if (y_vals(i) /= uy(num_uy)) then
          num_uy = num_uy + 1_int64
          uy(num_uy) = y_vals(i)
       end if
    end do

    ! 3. Generate Directed Atomic Edges (CCW)
    num_edges = 0_int64
    do i = 1_int64, healed_count
       idx_y1 = binary_search_y(uy, num_uy, raw_boxes(i)%y1)
       idx_y2 = binary_search_y(uy, num_uy, raw_boxes(i)%y2)

       if (idx_y1 == 0_int64 .or. idx_y2 == 0_int64) then
          print *, "FATAL: Y-coordinate missing from unique grid!"
          stop
       end if

       num_edges = num_edges + 2_int64 + 2_int64 * (idx_y2 - idx_y1)
    end do

    allocate(edges(num_edges))
    num_edges = 0_int64 

    do i = 1_int64, healed_count
       call add_edge(edges, num_edges, raw_boxes(i)%x1, raw_boxes(i)%y1, raw_boxes(i)%x2, raw_boxes(i)%y1)
       call add_edge(edges, num_edges, raw_boxes(i)%x2, raw_boxes(i)%y2, raw_boxes(i)%x1, raw_boxes(i)%y2)

       idx_y1 = binary_search_y(uy, num_uy, raw_boxes(i)%y1)
       idx_y2 = binary_search_y(uy, num_uy, raw_boxes(i)%y2)

       do j = idx_y2 - 1_int64, idx_y1, -1_int64
          call add_edge(edges, num_edges, raw_boxes(i)%x1, uy(j+1_int64), raw_boxes(i)%x1, uy(j))
       end do

       do j = idx_y1, idx_y2 - 1_int64
          call add_edge(edges, num_edges, raw_boxes(i)%x2, uy(j), raw_boxes(i)%x2, uy(j+1_int64))
       end do
    end do

    ! 4. Cancel Internal Shared Edges
    call sort_edges_by_geometry(edges, num_edges)

    do i = 1_int64, num_edges - 1_int64
       if (edges(i)%is_active .and. edges(i+1_int64)%is_active) then
          if (edges(i)%min_x == edges(i+1_int64)%min_x .and. edges(i)%min_y == edges(i+1_int64)%min_y .and. &
               edges(i)%max_x == edges(i+1_int64)%max_x .and. edges(i)%max_y == edges(i+1_int64)%max_y) then
             edges(i)%is_active         = .false.
             edges(i+1_int64)%is_active = .false.
          end if
       end if
    end do
    ! (This is a simplified check for demonstration)
    do i = 1_int64, num_edges
       if (.not. edges(i)%is_active) cycle

       ! Count how many active edges start/end at this vertex
       vertex_degree = 0
       v_x = edges(i)%x1
       v_y = edges(i)%y1

       do j = 1_int64, num_edges
          if (edges(j)%is_active .and. &
               ((edges(j)%x1 == v_x .and. edges(j)%y1 == v_y) .or. &
               (edges(j)%x2 == v_x .and. edges(j)%y2 == v_y))) then
             vertex_degree = vertex_degree + 1
          end if
       end do

       if (mod(vertex_degree, 2_int64) /= 0) then
          print *, "TOPOLOGY ERROR: Odd degree vertex detected at (", v_x, ",", v_y, ")"
          print *, "This box set cannot form closed loops."
          stop
       end if
    end do
    ! 5. Trace the Cycles
    allocate(contours(num_edges / 4_int64 + 1_int64)) 
    num_contours = 0_int64
    allocate(temp_pts(num_edges))
    edge_found = .false.
    ! We need to search specifically for an active edge that continues the path
    do j = 1_int64, num_edges
       if (edges(j)%is_active .and. edges(j)%x1 == curr_x .and. edges(j)%y1 == curr_y) then
          ! Found a candidate
          curr_x = edges(j)%x2
          curr_y = edges(j)%y2
          edges(j)%is_active = .false.
          edge_found = .true.
          exit
       end if
    end do
    do i = 1_int64, num_edges
       if (.not. edges(i)%is_active) cycle

       pt_count = 1_int64
       temp_pts(pt_count)%x = edges(i)%x1
       temp_pts(pt_count)%y = edges(i)%y1

       start_x = edges(i)%x1
       start_y = edges(i)%y1
       curr_x  = edges(i)%x2
       curr_y  = edges(i)%y2
       edges(i)%is_active = .false.

       cycle_closed = .false.

       do while (.not. cycle_closed)
          if (pt_count >= 2_int64) then
             if (.not. ( (temp_pts(pt_count-1_int64)%x == temp_pts(pt_count)%x .and. temp_pts(pt_count)%x == curr_x) .or. &
                  (temp_pts(pt_count-1_int64)%y == temp_pts(pt_count)%y .and. temp_pts(pt_count)%y == curr_y) )) then
                pt_count = pt_count + 1_int64
             end if
          else
             pt_count = pt_count + 1_int64
          end if

          temp_pts(pt_count)%x = curr_x
          temp_pts(pt_count)%y = curr_y

          if (curr_x == start_x .and. curr_y == start_y) then
             cycle_closed = .true.
             exit
          end if

          edge_found = .false.

          ! O(log N) Edge Lookup implementation you requested earlier
          j = find_next_edge(edges, num_edges, curr_x, curr_y)

          if (j > 0_int64) then
             curr_x = edges(j)%x2
             curr_y = edges(j)%y2
             edges(j)%is_active = .false.
             edge_found = .true.
          end if

          if (.not. edge_found) then
             print *, "WARNING: Broken contour at X:", curr_x, " Y:", curr_y
             print *, "--- TOPOLOGY INTEGRITY FAILED ---"
             print *, "Tracer stalled at (", curr_x, ",", curr_y, ")"
             print *, "Attempting to find edge starting at this coordinate..."

             ! Debug: Scan for ANY edge starting here
             do j = 1_int64, num_edges
                if (edges(j)%x1 == curr_x .and. edges(j)%y1 == curr_y) then
                   print *, "Found an edge starting at this coord, but is_active = ", edges(j)%is_active
                end if
             end do
             stop "Tracer Panic" 
             exit 
          end if
       end do

       if (cycle_closed .and. pt_count > 3_int64) then
          num_contours = num_contours + 1_int64
          allocate(contours(num_contours)%pts(pt_count - 1_int64))
          contours(num_contours)%pts = temp_pts(1_int64:pt_count-1_int64)
          contours(num_contours)%signed_area = calculate_shoelace_area(contours(num_contours)%pts)
       end if
    end do

    ! 6. Sort contours by Area (Replacing the old O(N^2) bubble sort)
    ! Note: QuickSort logic can be replicated here for `contours` array if desired for max performance
    do i = 1_int64, num_contours - 1_int64
       do j = i + 1_int64, num_contours
          if (contours(j)%signed_area > contours(i)%signed_area) then
             temp_poly   = contours(i)
             contours(i) = contours(j)
             contours(j) = temp_poly
          end if
       end do
    end do

  end subroutine extract_contours

  !--------------------------------------------------------------
  ! O(log N) Next-Edge Lookup (Upgraded to int64)
  !--------------------------------------------------------------
  pure function find_next_edge(edges, n, target_x, target_y) result(idx)
    type(DirectedEdge), intent(in) :: edges(:)
    integer(int64), intent(in)     :: n
    integer(kind=COORDINATE_KIND), intent(in) :: target_x, target_y
    integer(int64) :: idx, low, high, mid

    low = 1_int64
    high = n
    idx = 0_int64

    do while (low <= high)
       mid = low + (high - low) / 2_int64

       if (edges(mid)%x1 == target_x .and. edges(mid)%y1 == target_y) then
          if (edges(mid)%is_active) then
             idx = mid
             return
          else
             ! Standard collision handling
             idx = mid 
             return
          end if
       else if (edges(mid)%x1 < target_x .or. &
            (edges(mid)%x1 == target_x .and. edges(mid)%y1 < target_y)) then
          low = mid + 1_int64
       else
          high = mid - 1_int64
       end if
    end do
  end function find_next_edge

  !--------------------------------------------------------------
  ! Fast Binary Search for Y-Coordinate Grid Indices (Upgraded to int64)
  !--------------------------------------------------------------
  pure function binary_search_y(arr, n, target_val) result(idx)
    integer(kind=COORDINATE_KIND), intent(in) :: arr(:)
    integer(int64), intent(in) :: n
    integer(kind=COORDINATE_KIND), intent(in) :: target_val
    integer(int64) :: idx, low, high, mid

    low = 1_int64
    high = n
    idx = 0_int64

    do while (low <= high)
       mid = low + (high - low) / 2_int64
       if (arr(mid) == target_val) then
          idx = mid
          return
       else if (arr(mid) < target_val) then
          low = mid + 1_int64
       else
          high = mid - 1_int64
       end if
    end do
  end function binary_search_y

  !--------------------------------------------------------------
  ! Helper: Adds a directed edge to the tracking array (Upgraded to int64)
  !--------------------------------------------------------------
  pure subroutine add_edge(edges, count, x1, y1, x2, y2)
    type(DirectedEdge), intent(inout) :: edges(:)
    integer(int64), intent(inout) :: count
    integer(kind=COORDINATE_KIND), intent(in) :: x1, y1, x2, y2

    count = count + 1_int64
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
  ! Helper: Shoelace Area (Upgraded limits)
  !--------------------------------------------------------------
  pure function calculate_shoelace_area(pts) result(area)
    type(Point), intent(in) :: pts(:)
    integer(int64) :: area
    integer(int64) :: i, n

    area = 0_int64
    n = size(pts, kind=int64)
    if (n < 3_int64) return

    do i = 1_int64, n - 1_int64
       area = area + (int(pts(i)%x, int64) * int(pts(i+1)%y, int64)) - &
            (int(pts(i+1)%x, int64) * int(pts(i)%y, int64))
    end do
    area = area + (int(pts(n)%x, int64) * int(pts(1)%y, int64)) - &
         (int(pts(1)%x, int64) * int(pts(n)%y, int64))
  end function calculate_shoelace_area

  !--------------------------------------------------------------
  ! Directly converts Polygons to Sweep-Line Trackers (Already Int64 safe)
  !--------------------------------------------------------------
  pure subroutine contours_to_trackers(contours, trackers, tracker_count)
    type(Polygon), intent(in) :: contours(:)
    type(XYTracker), allocatable, intent(out) :: trackers(:)
    integer(kind=int64), intent(out) :: tracker_count

    integer(int64) :: i, j, k, n_pts
    integer(kind=int64) :: total_vertical_edges, winding
    integer(kind=COORDINATE_KIND) :: y_start, y_end, curr_x

    total_vertical_edges = 0_int64
    do i = 1_int64, size(contours, kind=int64)
       n_pts = size(contours(i)%pts, kind=int64)
       do j = 1_int64, n_pts - 1_int64
          if (contours(i)%pts(j)%x == contours(i)%pts(j+1_int64)%x) then
             total_vertical_edges = total_vertical_edges + 1_int64
          end if
       end do
    end do

    tracker_count = total_vertical_edges * 2_int64
    if (tracker_count == 0_int64) then
       allocate(trackers(0))
       return
    end if

    allocate(trackers(tracker_count))
    k = 1_int64

    do i = 1_int64, size(contours, kind=int64)
       winding = sign(1_int64, contours(i)%signed_area)
       n_pts = size(contours(i)%pts, kind=int64)

       do j = 1_int64, n_pts - 1_int64
          if (contours(i)%pts(j)%x == contours(i)%pts(j+1_int64)%x) then
             curr_x  = contours(i)%pts(j)%x
             y_start = contours(i)%pts(j)%y
             y_end   = contours(i)%pts(j+1_int64)%y

             if (y_start < y_end) then
                trackers(k)   = XYTracker(X = curr_x, Y = y_start, polygonNumber = -winding)
                trackers(k+1) = XYTracker(X = curr_x, Y = y_end,   polygonNumber =  winding)
             else
                trackers(k)   = XYTracker(X = curr_x, Y = y_end,   polygonNumber =  winding)
                trackers(k+1) = XYTracker(X = curr_x, Y = y_start, polygonNumber = -winding)
             end if
             k = k + 2_int64
          end if
       end do
    end do
  end subroutine contours_to_trackers

end module ContourExtractionModule
