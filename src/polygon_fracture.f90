! File   : polygon_fracture.f90
! Author : Sandeep Koranne
! Purpose: See the notes in skiplist.f90.notes
!
module polygon_fracture_mod
  use CommonModule
  use GeometryModule
  use DesignModule
  use HDFDataModule
  use ContourExtractionModule

  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  implicit none
  private
  integer, parameter :: K_POLYGON_INIT_BOX_COUNT = 64

  ! --- Constants & Kinds ---
  integer, parameter :: MAX_SKIP_LEVEL = 32
  real(real64), parameter :: SKIP_PROBABILITY = 0.5_real64

  ! --- Public Exports ---
  public :: XYTracker, SkipList, SkipListNode, NodePtr
  public :: init_skiplist, insert_edge, remove_edge, find_bounding_edges, destroy_skiplist
  public :: sort_trackers, scanline_fracture, generate_trackers
  public :: K_COORDINATE_KIND
  type :: ActiveRegion
     integer(kind=K_COORDINATE_KIND) :: y1, y2, x_start
  end type ActiveRegion
  ! --- Types ---
  type :: XYTracker
     integer(kind=K_COORDINATE_KIND) :: X, Y
     integer(kind=int64) :: polygonNumber 
     ! the winding number of the vertex is the sign of polygonNumber
  end type XYTracker

  ! Fortran requires a wrapper type to have arrays of pointers
  type :: NodePtr
     type(SkipListNode), pointer :: ptr => null()
  end type NodePtr

  type :: SkipListNode
     integer(kind=K_COORDINATE_KIND) :: y_val
     integer(kind=int64) :: lap_change
     type(NodePtr), allocatable :: forward(:)
  end type SkipListNode

  type :: SkipList
     integer :: current_level
     type(SkipListNode), pointer :: header => null()
  end type SkipList

contains

  ! ==========================================
  ! Skip List Core Functions
  ! ==========================================

  subroutine init_skiplist(list)
    type(SkipList), intent(inout) :: list
    integer :: i

    list%current_level = 1
    allocate(list%header)
    ! Use a minimum sentinel value
    list%header%y_val = -huge(1_int64)
    list%header%lap_change = 0
    allocate(list%header%forward(MAX_SKIP_LEVEL))

    do i = 1, MAX_SKIP_LEVEL
       list%header%forward(i)%ptr => null()
    end do
  end subroutine init_skiplist

  subroutine destroy_skiplist(list)
    type(SkipList), intent(inout) :: list
    type(SkipListNode), pointer :: current, next_node

    current => list%header
    do while (associated(current))
       next_node => current%forward(1)%ptr
       deallocate(current%forward)
       deallocate(current)
       current => next_node
    end do
    nullify(list%header)
  end subroutine destroy_skiplist

  function random_level() result(lvl)
    integer :: lvl
    real(real64) :: r

    lvl = 1
    call random_number(r)
    do while (r < SKIP_PROBABILITY .and. lvl < MAX_SKIP_LEVEL)
       lvl = lvl + 1
       call random_number(r)
    end do
  end function random_level
  !> Accumulates lap counts for coincident Y boundaries. 
  !> Auto-deletes the node if the lap count reaches zero.
  subroutine update_active_edges(list, y_val, lap_delta)
    type(SkipList), intent(inout) :: list
    integer(kind=K_COORDINATE_KIND), intent(in) :: y_val
    integer(kind=int64), intent(in) :: lap_delta

    type(NodePtr) :: update(MAX_SKIP_LEVEL)
    type(SkipListNode), pointer :: current, target, new_node
    integer :: i, lvl

    current => list%header

    ! 1. Find positions to update
    do i = list%current_level, 1, -1
       do while (associated(current%forward(i)%ptr))
          if (current%forward(i)%ptr%y_val < y_val) then
             current => current%forward(i)%ptr
          else
             exit
          end if
       end do
       update(i)%ptr => current
    end do

    target => current%forward(1)%ptr

    ! 2. Check if the Y-coordinate already exists in the Skip List
    if (associated(target)) then
       if (target%y_val == y_val) then
          ! It exists: accumulate the winding sign
          target%lap_change = target%lap_change + lap_delta

          ! If the boundary is fully resolved, remove the node entirely
          if (target%lap_change == 0) then
             do i = 1, list%current_level
                if (.not. associated(update(i)%ptr%forward(i)%ptr)) exit
                if (.not. associated(update(i)%ptr%forward(i)%ptr, target)) exit
                update(i)%ptr%forward(i)%ptr => target%forward(i)%ptr
             end do

             deallocate(target%forward)
             deallocate(target)

             ! Reduce max level if top levels are now empty
             do while (list%current_level > 1 .and. &
                  .not. associated(list%header%forward(list%current_level)%ptr))
                list%current_level = list%current_level - 1
             end do
          end if

          return ! We are done
       end if
    end if

    ! 3. The Y-coordinate does not exist; insert a new node
    if (lap_delta == 0) return 

    lvl = random_level()

    if (lvl > list%current_level) then
       do i = list%current_level + 1, lvl
          update(i)%ptr => list%header
       end do
       list%current_level = lvl
    end if

    allocate(new_node)
    new_node%y_val = y_val
    new_node%lap_change = lap_delta
    allocate(new_node%forward(lvl))

    do i = 1, lvl
       new_node%forward(i)%ptr => update(i)%ptr%forward(i)%ptr
       update(i)%ptr%forward(i)%ptr => new_node
    end do
  end subroutine update_active_edges

  subroutine insert_edge(list, y_val, edge_id)
    type(SkipList), intent(inout) :: list
    integer(kind=K_COORDINATE_KIND), intent(in) :: y_val
    integer(kind=int64), intent(in) :: edge_id

    type(NodePtr) :: update(MAX_SKIP_LEVEL)
    type(SkipListNode), pointer :: current, new_node
    integer :: i, lvl

    current => list%header

    ! Find positions to update
    do i = list%current_level, 1, -1
       do while (associated(current%forward(i)%ptr))
          if (current%forward(i)%ptr%y_val < y_val) then
             current => current%forward(i)%ptr
          else
             exit
          end if
       end do
       update(i)%ptr => current
    end do

    ! Generate random level for new node
    lvl = random_level()

    if (lvl > list%current_level) then
       do i = list%current_level + 1, lvl
          update(i)%ptr => list%header
       end do
       list%current_level = lvl
    end if

    ! Create and splice in the new node
    allocate(new_node)
    new_node%y_val = y_val
    !>> this is to be changed <<
    !new_node%edge_id = edge_id
    allocate(new_node%forward(lvl))

    do i = 1, lvl
       new_node%forward(i)%ptr => update(i)%ptr%forward(i)%ptr
       update(i)%ptr%forward(i)%ptr => new_node
    end do
  end subroutine insert_edge

  subroutine remove_edge(list, y_val)
    type(SkipList), intent(inout) :: list
    integer(kind=K_COORDINATE_KIND), intent(in) :: y_val

    type(NodePtr) :: update(MAX_SKIP_LEVEL)
    type(SkipListNode), pointer :: current, target
    integer :: i

    current => list%header

    do i = list%current_level, 1, -1
       do while (associated(current%forward(i)%ptr))
          if (current%forward(i)%ptr%y_val < y_val) then
             current => current%forward(i)%ptr
          else
             exit
          end if
       end do
       update(i)%ptr => current
    end do

    target => current%forward(1)%ptr

    if (associated(target)) then
       if (target%y_val == y_val) then
          do i = 1, list%current_level
             if (.not. associated(update(i)%ptr%forward(i)%ptr)) exit
             if (.not. associated(update(i)%ptr%forward(i)%ptr, target)) exit
             update(i)%ptr%forward(i)%ptr => target%forward(i)%ptr
          end do

          deallocate(target%forward)
          deallocate(target)

          ! Reduce level if top levels are now empty
          do while (list%current_level > 1 .and. &
               .not. associated(list%header%forward(list%current_level)%ptr))
             list%current_level = list%current_level - 1
          end do
       end if
    end if
  end subroutine remove_edge

  ! Finds the nearest edge directly above (ceiling) and below (floor) the target Y
  subroutine find_bounding_edges(list, target_y, floor_y, ceil_y, found_floor, found_ceil)
    type(SkipList), intent(in) :: list
    integer(kind=K_COORDINATE_KIND), intent(in) :: target_y
    integer(kind=K_COORDINATE_KIND), intent(out) :: floor_y, ceil_y
    logical, intent(out) :: found_floor, found_ceil

    type(SkipListNode), pointer :: current

    found_floor = .false.
    found_ceil = .false.
    current => list%header

    ! Find the floor (greatest value < target_y)
    call traverse_to_floor(list, target_y, current)

    if (associated(current) .and. current%y_val /= -huge(1_int64)) then
       floor_y = current%y_val
       found_floor = .true.
    end if

    ! The ceiling is naturally the immediate next node at level 1
    if (associated(current%forward(1)%ptr)) then
       ceil_y = current%forward(1)%ptr%y_val
       found_ceil = .true.
    end if
  end subroutine find_bounding_edges

  subroutine traverse_to_floor(list, target_y, current)
    type(SkipList), intent(in) :: list
    integer(kind=K_COORDINATE_KIND), intent(in) :: target_y
    type(SkipListNode), pointer, intent(inout) :: current
    integer :: i

    do i = list%current_level, 1, -1
       do while (associated(current%forward(i)%ptr))
          if (current%forward(i)%ptr%y_val <= target_y) then
             current => current%forward(i)%ptr
          else
             exit
          end if
       end do
    end do
  end subroutine traverse_to_floor


  ! ==========================================
  ! Utility & Algorithm Execution
  ! ==========================================

  ! Quicksort implementation (using X, then Y for stable geometric sorting)
  pure recursive subroutine sort_trackers(arr)
    type(XYTracker), intent(inout) :: arr(:)
    integer :: n
    if (size(arr) <= 1) return
    n = size(arr)
    call quicksort(arr, 1, n)
  end subroutine sort_trackers

  pure recursive subroutine quicksort(arr, left, right)
    type(XYTracker), intent(inout) :: arr(:)
    integer, intent(in) :: left, right
    integer :: i, j
    type(XYTracker) :: pivot, temp

    if (left >= right) return
    i = left
    j = right
    pivot = arr((left + right) / 2)

    do while (i <= j)
       do while (is_less_than(arr(i), pivot))
          i = i + 1
       end do
       do while (is_less_than(pivot, arr(j)))
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

    if (left < j) call quicksort(arr, left, j)
    if (i < right) call quicksort(arr, i, right)
  end subroutine quicksort

  pure function is_less_than(a, b) result(less)
    type(XYTracker), intent(in) :: a, b
    logical :: less
    if (a%X == b%X) then
       less = a%Y < b%Y
    else
       less = a%X < b%X
    end if
  end function is_less_than
subroutine scanline_fracture(trackers, fractured_boxes)
    type(XYTracker), intent(inout) :: trackers(:)
    type(Box), allocatable, intent(out) :: fractured_boxes(:)

    type(SkipList) :: active_edges
    type(SkipListNode), pointer :: current_node

    integer :: i, n
    integer(kind=K_COORDINATE_KIND) :: current_x, y_start, y_end
    integer(kind=int64) :: winding_sign, current_lap

    ! Region History Variables
    type(ActiveRegion), allocatable :: prev_regions(:), curr_regions(:), temp_regions(:)
    integer :: n_prev, n_curr, max_regions, p, c

    ! Output collection variables
    type(Box), allocatable :: temp_boxes(:), resized_boxes(:)
    integer :: output_count, max_boxes

    n = size(trackers)
    if (n == 0) then
       allocate(fractured_boxes(0))
       return
    end if

    ! 1. Sort vertices left-to-right (X), then bottom-to-top (Y)
    call sort_trackers(trackers)
    call init_skiplist(active_edges)

    ! Initialize memory arrays
    max_boxes = max(100, n)
    allocate(temp_boxes(max_boxes))
    output_count = 0

    max_regions = max(100, n/4)
    allocate(prev_regions(max_regions), curr_regions(max_regions))
    n_prev = 0

    ! 2. Sweep Line Execution
    do i = 1, n
       winding_sign = sign(1_int64, trackers(i)%polygonNumber)

       ! Update Skip List with the current edge
       call update_active_edges(active_edges, trackers(i)%Y, winding_sign)

       ! 3. Check if we have processed all events at the current X coordinate.
       if (i == n .or. trackers(i)%X < trackers(i+1)%X) then
          current_x = trackers(i)%X
          n_curr = 0

          ! --- Extract Current Active Y-Intervals ---
          current_node => active_edges%header%forward(1)%ptr
          current_lap = 0

          do while (associated(current_node))
             if (current_lap <= 0 .and. current_lap + current_node%lap_change > 0) then
                y_start = current_node%y_val
             else if (current_lap > 0 .and. current_lap + current_node%lap_change <= 0) then
                y_end = current_node%y_val
                
                n_curr = n_curr + 1
                
                ! Expand region arrays safely if we exceed capacity
                if (n_curr > max_regions) then
                   max_regions = max_regions * 2
                   allocate(temp_regions(max_regions))
                   temp_regions(1:n_curr-1) = curr_regions(1:n_curr-1)
                   call move_alloc(from=temp_regions, to=curr_regions)
                   
                   allocate(temp_regions(max_regions))
                   if (n_prev > 0) temp_regions(1:n_prev) = prev_regions(1:n_prev)
                   call move_alloc(from=temp_regions, to=prev_regions)
                end if

                curr_regions(n_curr)%y1 = y_start
                curr_regions(n_curr)%y2 = y_end
                curr_regions(n_curr)%x_start = current_x ! Default origin is current X
             end if

             current_lap = current_lap + current_node%lap_change
             current_node => current_node%forward(1)%ptr
          end do

          ! --- Fast Two-Pointer Merge & Emit Phase ---
          p = 1
          c = 1
          do while (p <= n_prev .and. c <= n_curr)
             if (prev_regions(p)%y1 == curr_regions(c)%y1 .and. prev_regions(p)%y2 == curr_regions(c)%y2) then
                ! Exact Match: Region continues uninterrupted. Inherit the historical x_start.
                curr_regions(c)%x_start = prev_regions(p)%x_start
                p = p + 1
                c = c + 1
             else if (prev_regions(p)%y1 < curr_regions(c)%y1) then
                ! The previous region ended (interrupted by an edge). Emit it.
                if (current_x > prev_regions(p)%x_start) then
                   call emit_region(prev_regions(p), current_x)
                end if
                p = p + 1
             else if (prev_regions(p)%y1 > curr_regions(c)%y1) then
                ! The current region is completely new. It already has x_start = current_x.
                c = c + 1
             else 
                ! y1 matches but y2 differs (a split or merge happened at the upper boundary). Emit it.
                if (current_x > prev_regions(p)%x_start) then
                   call emit_region(prev_regions(p), current_x)
                end if
                p = p + 1
             end if
          end do

          ! Emit any remaining previous regions that didn't match
          do while (p <= n_prev)
             if (current_x > prev_regions(p)%x_start) then
                call emit_region(prev_regions(p), current_x)
             end if
             p = p + 1
          end do

          ! Save the current state as the previous state for the next X iteration
          if (n_curr > 0) prev_regions(1:n_curr) = curr_regions(1:n_curr)
          n_prev = n_curr
       end if
    end do

    ! 4. Cleanup and Finalize
    call destroy_skiplist(active_edges)
    write(*,*) 'In PF (Fractured Count): ', output_count
    
    allocate(fractured_boxes(output_count))
    if (output_count > 0) then
       fractured_boxes(1:output_count) = temp_boxes(1:output_count)
    end if

  contains

    ! Helper routine to cleanly handle dynamic reallocation and assignments
    subroutine emit_region(region, x_end)
       type(ActiveRegion), intent(in) :: region
       integer(kind=K_COORDINATE_KIND), intent(in) :: x_end
       
       output_count = output_count + 1
       
       if (output_count > max_boxes) then
          max_boxes = max_boxes + (max_boxes / 2) + 1
          allocate(resized_boxes(max_boxes))
          resized_boxes(1:output_count-1) = temp_boxes(1:output_count-1)
          call move_alloc(from=resized_boxes, to=temp_boxes)
       end if

       temp_boxes(output_count)%x1 = region%x_start
       temp_boxes(output_count)%x2 = x_end
       temp_boxes(output_count)%y1 = region%y1
       temp_boxes(output_count)%y2 = region%y2
    end subroutine emit_region

  end subroutine scanline_fracture
  subroutine old_scanline_fracture(trackers, fractured_boxes)
    type(XYTracker), intent(inout) :: trackers(:)
    type(Box), allocatable, intent(out) :: fractured_boxes(:)

    type(SkipList) :: active_edges
    type(SkipListNode), pointer :: current_node

    integer :: i, n
    integer(kind=K_COORDINATE_KIND) :: current_x, next_x, y_start, y_end
    integer(kind=int64) :: winding_sign, current_lap

    ! Output collection variables
    type(Box), allocatable :: temp_boxes(:), resized_boxes(:)
    integer :: output_count, max_boxes

    n = size(trackers)
    if (n == 0) then
       allocate(fractured_boxes(0))
       return
    end if

    ! 1. Sort vertices left-to-right (X), then bottom-to-top (Y)
    call sort_trackers(trackers)
    call init_skiplist(active_edges)

    ! Initialize dynamic output array
    max_boxes = max(100, n)
    allocate(temp_boxes(max_boxes))
    output_count = 0

    ! 2. Sweep Line Execution
    do i = 1, n
       winding_sign = sign(1_int64, trackers(i)%polygonNumber)

       ! Add or subtract the winding sign to the Y coordinate in the Skip List.
       ! (Assume this helper accumulates lap_change, and deletes the node if it hits 0)
       call update_active_edges(active_edges, trackers(i)%Y, winding_sign)

       ! 3. Check if we have processed all events at the current X coordinate.
       ! If the next event is further to the right, we have a continuous "slab" to emit.
       if (i < n) then
          if (trackers(i)%X < trackers(i+1)%X) then
             current_x = trackers(i)%X
             next_x    = trackers(i+1)%X

             ! Traverse Level 1 of the Skip List (Sorted Active Y Boundaries)
             current_node => active_edges%header%forward(1)%ptr
             current_lap = 0
             y_start = 0
             do while (associated(current_node))
                ! State Transition: Empty Space -> Inside a Polygon
                ! (Detects a boundary when density crosses from <= 0 to > 0)
                if (current_lap <= 0 .and. current_lap + current_node%lap_change > 0) then
                   y_start = current_node%y_val

                   ! State Transition: Inside a Polygon -> Empty Space
                   ! (Detects a boundary when density drops from > 0 to <= 0)
                else if (current_lap > 0 .and. current_lap + current_node%lap_change <= 0) then
                   y_end = current_node%y_val

                   ! --- EMIT THE FRACTURED BOX ---
                   output_count = output_count + 1

                   ! Handle dynamic memory resizing
                   if (output_count > max_boxes) then
                      max_boxes = max_boxes + (max_boxes / 2) + 1
                      allocate(resized_boxes(max_boxes))
                      resized_boxes(1:output_count-1) = temp_boxes(1:output_count-1)
                      call move_alloc(from=resized_boxes, to=temp_boxes)
                   end if

                   temp_boxes(output_count)%x1 = current_x
                   temp_boxes(output_count)%x2 = next_x
                   temp_boxes(output_count)%y1 = y_start
                   temp_boxes(output_count)%y2 = y_end
                end if

                ! Accumulate the lap count and move to the next vertical boundary
                current_lap = current_lap + current_node%lap_change
                current_node => current_node%forward(1)%ptr
             end do
          end if
       end if
    end do

    ! 4. Cleanup and Finalize
    call destroy_skiplist(active_edges)
    write(*,*) 'In PF: ', output_count
    ! Lock in the exact size for the final output array
    allocate(fractured_boxes(output_count))
    if (output_count > 0) then
       fractured_boxes(1:output_count) = temp_boxes(1:output_count)
    end if

  end subroutine old_scanline_fracture

  !> Generates an array of XYTrackers from an array of Boxes.
  !> Automatically normalizes coordinates and flags winding numbers.
  pure subroutine generate_trackers(boxes, bbox, trackers)
    type(Box), intent(in) :: boxes(:)
    type(Box), intent(in) :: bbox
    type(XYTracker), allocatable, intent(out) :: trackers(:)

    integer :: i, n, idx
    integer(kind=K_COORDINATE_KIND) :: min_x, max_x, min_y, max_y

    n = size(boxes)
    min_x = bbox%x1
    max_x = bbox%x2
    min_y = bbox%y1
    max_y = bbox%y2
    ! Each box has 4 corners, so we need exactly 4x the trackers
    allocate(trackers(4 * (n+1)))
    trackers(1) = XYTracker(X = min_x, Y = min_y, polygonNumber =  1)
    trackers(2) = XYTracker(X = min_x, Y = max_y, polygonNumber =  -1)
    trackers(3) = XYTracker(X = max_x, Y = min_y, polygonNumber =  -1)
    trackers(4) = XYTracker(X = max_x, Y = max_y, polygonNumber =  1)    
    do i = 1, n
       idx = i * 4

       ! 1. Normalize box coordinates to guarantee proper left-to-right sweeping
       min_x = min(boxes(i)%x1, boxes(i)%x2)
       max_x = max(boxes(i)%x1, boxes(i)%x2)
       min_y = min(boxes(i)%y1, boxes(i)%y2)
       max_y = max(boxes(i)%y1, boxes(i)%y2)

       ! 2. Left Vertices (Sweep line encounters these first -> Insert phase)
       ! Positive ID signifies insertion of the Y boundaries
       trackers(idx + 1) = XYTracker(X = min_x, Y = min_y, polygonNumber =  -i)
       trackers(idx + 2) = XYTracker(X = min_x, Y = max_y, polygonNumber =  i)

       ! 3. Right Vertices (Sweep line encounters these last -> Remove phase)
       ! Negative ID signifies removal of the Y boundaries
       trackers(idx + 3) = XYTracker(X = max_x, Y = min_y, polygonNumber = i)
       trackers(idx + 4) = XYTracker(X = max_x, Y = max_y, polygonNumber = -i)
    end do
  end subroutine generate_trackers

end module polygon_fracture_mod


