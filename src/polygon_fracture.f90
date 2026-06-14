! File   : polygon_fracture.f90
! Author : Sandeep Koranne
! Purpose: High-Performance Scanline Fracture using Doubly-Linked LIFO Arena SkipList.
!          Features O(1) deletions and O(log N) UP/DOWN neighbor resolutions.

module PolygonFractureModule
  use CommonModule
  use GeometryModule
  !use DesignModule
  use HDFDataModule
  !use ContourExtractionModule

  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  implicit none
  private
  integer, parameter :: K_POLYGON_INIT_BOX_COUNT = 64
  integer, parameter :: K_SMALL_THRESHOLD = 64
  ! --- Constants & Kinds ---
  integer, parameter      :: MAX_SKIP_LEVEL = 32
  real(real64), parameter :: SKIP_PROBABILITY = 0.5_real64

  ! --- Public Exports ---
  public :: XYTracker, SkipList, SkipListNode, NodePtr
  public :: init_skiplist, update_active_edges, find_bounding_edges, destroy_skiplist, calculate_union_area_sl
  public :: sort_trackers, scanline_fracture, generate_trackers, heal_boxes
  public :: K_COORDINATE_KIND

  type :: ActiveRegion
     integer(kind=K_COORDINATE_KIND) :: y1, y2, x_start
  end type ActiveRegion

  type :: XYTracker
     integer(kind=K_COORDINATE_KIND) :: X, Y
     integer(kind=int64) :: polygonNumber 
     ! the winding number of the vertex is the sign of polygonNumber
  end type XYTracker

  ! ============================================================================
  ! HIGHEST-EFFORT SKIPLIST SCHEMA (Doubly-Linked, Arena-Allocated)
  ! ============================================================================
  type :: NodePtr
     type(SkipListNode), pointer :: ptr => null()
  end type NodePtr

  type :: SkipListNode
     ! Payload
     integer(kind=K_COORDINATE_KIND) :: y_val
     integer(kind=int64)             :: lap_change
     ! Bi-directional Routing and LIFO Memory Chain
     type(NodePtr) :: Forward(MAX_SKIP_LEVEL)
     type(NodePtr) :: Backward(MAX_SKIP_LEVEL) 
     type(NodePtr) :: NextFree
  end type SkipListNode

  type :: SkipList
     integer :: current_level
     type(SkipListNode), pointer :: header => null()

     ! Memory Pool Optimization
     type(SkipListNode), pointer :: Arena(:) => null()
     type(NodePtr)               :: FreeHead
  end type SkipList

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type :: Event
     integer(kind=K_COORDINATE_KIND) :: x, y1, y2
     integer(kind=int64)             :: lap_change
  end type Event

contains

  ! ============================================================================
  ! MEMORY POOL & SKIPLIST LIFECYCLE
  ! ============================================================================

  subroutine init_skiplist(list, capacity)
    type(SkipList), intent(inout) :: list
    integer(int64), intent(in)    :: capacity
    integer(int64) :: i
    integer :: j

    list%current_level = 1
    ! Allocate contiguous arena block
    allocate(list%Arena(capacity + 1))

    ! Chain the LIFO free list
    do i = 1, capacity
       list%Arena(i)%NextFree%ptr => list%Arena(i+1)
       do j = 1, MAX_SKIP_LEVEL
          list%Arena(i)%Forward(j)%ptr  => null()
          list%Arena(i)%Backward(j)%ptr => null()
       end do
    end do

    list%Arena(capacity + 1)%NextFree%ptr => null()
    do j = 1, MAX_SKIP_LEVEL
       list%Arena(capacity + 1)%Forward(j)%ptr  => null()
       list%Arena(capacity + 1)%Backward(j)%ptr => null()
    end do

    list%FreeHead%ptr => list%Arena(1)

    ! Consume the first node as the Sentinel Header
    list%header => SL_GET_FREE(list)
    list%header%y_val = -huge(1_int64)
    list%header%lap_change = 0
  end subroutine init_skiplist

  subroutine destroy_skiplist(list)
    type(SkipList), intent(inout) :: list
    if (associated(list%Arena)) deallocate(list%Arena)
    nullify(list%header)
    nullify(list%FreeHead%ptr)
  end subroutine destroy_skiplist

  function SL_GET_FREE(list) result(node)
    type(SkipList), intent(inout) :: list
    type(SkipListNode), pointer :: node
    if (.not. associated(list%FreeHead%ptr)) then
       error stop "CRITICAL: Polygon Fracture SkipList Arena Exhausted!"
    end if
    node => list%FreeHead%ptr
    list%FreeHead%ptr => node%NextFree%ptr
    node%NextFree%ptr => null()
  end function SL_GET_FREE

  subroutine SL_RELEASE(list, node)
    type(SkipList), intent(inout) :: list
    type(SkipListNode), pointer :: node
    integer :: i
    ! Wipe pointers to prevent ghost links
    do i = 1, MAX_SKIP_LEVEL
       node%Forward(i)%ptr  => null()
       node%Backward(i)%ptr => null()
    end do
    node%NextFree%ptr => list%FreeHead%ptr
    list%FreeHead%ptr => node
  end subroutine SL_RELEASE

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


  ! ============================================================================
  ! O(log N) UP/DOWN SWEEP-LINE ENGINE
  ! ============================================================================

  !> Smart Update: Accumulates winding numbers. If a boundary perfectly resolves (lap == 0),
  !> it leverages the Backward pointers to un-splice the node in O(1) without a re-scan.
  subroutine update_active_edges(list, y_val, lap_delta)
    type(SkipList), intent(inout) :: list
    integer(kind=K_COORDINATE_KIND), intent(in) :: y_val
    integer(kind=int64), intent(in) :: lap_delta

    type(NodePtr) :: update(MAX_SKIP_LEVEL)
    type(SkipListNode), pointer :: current, target, new_node, prev_node, next_node
    integer :: i, lvl

    current => list%header

    ! 1. O(log N) Search for the target Y coordinate
    do i = list%current_level, 1, -1
       do while (associated(current%Forward(i)%ptr))
          if (current%Forward(i)%ptr%y_val < y_val) then
             current => current%Forward(i)%ptr
          else
             exit
          end if
       end do
       update(i)%ptr => current
    end do

    target => current%Forward(1)%ptr

    ! 2. Coordinate MATCH: Accumulate or Delete
    if (associated(target)) then
       if (target%y_val == y_val) then
          target%lap_change = target%lap_change + lap_delta

          ! If boundary resolves, O(1) Un-splice using doubly-linked pointers
          if (target%lap_change == 0) then
             do i = 1, list%current_level
                ! If this node is not active at this level, stop splicing
                if (.not. associated(target%Backward(i)%ptr)) exit

                prev_node => target%Backward(i)%ptr
                next_node => target%Forward(i)%ptr

                ! Bypass the target node
                if (associated(prev_node)) prev_node%Forward(i)%ptr => next_node
                if (associated(next_node)) next_node%Backward(i)%ptr => prev_node
             end do

             ! Release back to Arena
             call SL_RELEASE(list, target)

             ! Safely lower the list's max level if the top lanes are now empty
             do while (list%current_level > 1 .and. &
                  .not. associated(list%header%Forward(list%current_level)%ptr))
                list%current_level = list%current_level - 1
             end do
          end if
          return 
       end if
    end if

    ! 3. Coordinate NOT FOUND: Insert New Boundary (O(log N) search + O(1) splice)
    if (lap_delta == 0) return 

    lvl = random_level()
    if (lvl > list%current_level) then
       do i = list%current_level + 1, lvl
          update(i)%ptr => list%header
       end do
       list%current_level = lvl
    end if

    new_node => SL_GET_FREE(list)
    new_node%y_val = y_val
    new_node%lap_change = lap_delta

    ! Doubly-Linked Splice
    do i = 1, lvl
       ! Forward links
       new_node%Forward(i)%ptr => update(i)%ptr%Forward(i)%ptr
       update(i)%ptr%Forward(i)%ptr => new_node

       ! Backward links
       if (associated(new_node%Forward(i)%ptr)) then
          new_node%Forward(i)%ptr%Backward(i)%ptr => new_node
       end if
       new_node%Backward(i)%ptr => update(i)%ptr
    end do
  end subroutine update_active_edges


  !> Look UP and DOWN Optimization: 
  !> Finds the target, then instantly grabs Forward(1) for CEILING and Backward(1) for FLOOR.
  subroutine find_bounding_edges(list, target_y, floor_y, ceil_y, found_floor, found_ceil)
    type(SkipList), intent(in) :: list
    integer(kind=K_COORDINATE_KIND), intent(in) :: target_y
    integer(kind=K_COORDINATE_KIND), intent(out) :: floor_y, ceil_y
    logical, intent(out) :: found_floor, found_ceil
    integer :: i
    type(SkipListNode), pointer :: current

    found_floor = .false.
    found_ceil = .false.
    current => list%header

    ! 1. O(log N) Traversal to exactly target_y
    do i = list%current_level, 1, -1
       do while (associated(current%Forward(i)%ptr))
          if (current%Forward(i)%ptr%y_val <= target_y) then
             current => current%Forward(i)%ptr
          else
             exit
          end if
       end do
    end do

    ! 2. Look DOWN (Floor) using Backward pointer
    if (associated(current%Backward(1)%ptr)) then
       if (current%Backward(1)%ptr%y_val /= -huge(1_int64)) then
          floor_y = current%Backward(1)%ptr%y_val
          found_floor = .true.
       end if
    end if

    ! 3. Look UP (Ceiling) using Forward pointer
    if (associated(current%Forward(1)%ptr)) then
       ceil_y = current%Forward(1)%ptr%y_val
       found_ceil = .true.
    end if
  end subroutine find_bounding_edges

  ! ... (Keep existing sort_trackers, quicksort, is_less_than unaltered) ...
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


  ! ============================================================================
  ! SCANLINE ENGINE
  ! ============================================================================
  subroutine scanline_fracture(trackers, fractured_boxes)
    type(XYTracker), intent(inout) :: trackers(:)
    type(Box), allocatable, intent(out) :: fractured_boxes(:)

    type(SkipList) :: active_edges
    type(SkipListNode), pointer :: current_node

    integer :: i, n
    integer(kind=K_COORDINATE_KIND) :: current_x, y_start, y_end
    integer(kind=int64) :: winding_sign, current_lap, max_sweep_capacity

    ! O(1) Pointer Swap Optimization variables
    type(ActiveRegion), target, allocatable :: region_bank_A(:), region_bank_B(:), temp_regions(:)
    type(ActiveRegion), pointer :: prev_regions(:), curr_regions(:), swap_ptr(:)
    integer :: n_prev, n_curr, p, c

    type(Box), allocatable :: temp_boxes(:), resized_boxes(:)
    integer(kind=int64) :: output_count, max_boxes
    logical :: process_x_slice
    n = size(trackers)
    if (n == 0) then
       allocate(fractured_boxes(0))
       return
    end if

    call sort_trackers(trackers)

    ! Initialize Arena with capacity N
    ! call init_skiplist(active_edges, int(n, int64))
    call init_skiplist(active_edges, min(int(n, int64), 5000000_int64))
    ! 1. Pre-allocate Massive Output Block
    ! A typical polygon fractures into roughly 1 to 2 times the number of vertices.
    ! Pre-allocating massively avoids mid-sweep geometric reallocations.
    max_boxes = max(100000_int64, int(n, int64) * 2_int64)
    allocate(temp_boxes(max_boxes))
    output_count = 0

    ! 2. Absolute Maximum Capacity Region Banks
    ! There can NEVER be more vertical active regions than N/2. 
    ! Allocating strictly to the mathematical limit removes all bounds-checking overhead.
    max_sweep_capacity = min(int(n, int64)/2_int64, 2000000_int64)
    !allocate(region_bank_A(n/2 + 1), region_bank_B(n/2 + 1))
    allocate(region_bank_A(max_sweep_capacity), region_bank_B(max_sweep_capacity))
    prev_regions => region_bank_A
    curr_regions => region_bank_B
    n_prev = 0

    do i = 1, n
       winding_sign = sign(1_int64, trackers(i)%polygonNumber)
       call update_active_edges(active_edges, trackers(i)%Y, winding_sign)
       ! --- THE SAFE BOUNDS FIX ---
       process_x_slice = .false.
       if (i == n) then
          process_x_slice = .true.
       else if (trackers(i)%X < trackers(i+1)%X) then
          process_x_slice = .true.
       end if

       if (process_x_slice) then
          current_x = trackers(i)%X
          n_curr = 0

          current_node => active_edges%header%Forward(1)%ptr
          current_lap = 0

          ! 3. Uninterrupted Hardware-Prefetch-Friendly Traversal
          do while (associated(current_node))
             if (current_lap <= 0 .and. current_lap + current_node%lap_change > 0) then
                y_start = current_node%y_val
             else if (current_lap > 0 .and. current_lap + current_node%lap_change <= 0) then
                y_end = current_node%y_val

                n_curr = n_curr + 1
                ! --- NEW: DYNAMIC REGION BANK EXPANSION ---
                if (n_curr > size(region_bank_B)) then
                   max_sweep_capacity = size(region_bank_B) * 2_int64

                   ! Expand Bank B (Current)
                   allocate(temp_regions(max_sweep_capacity))
                   temp_regions(1:n_curr-1) = curr_regions(1:n_curr-1)
                   call move_alloc(from=temp_regions, to=region_bank_B)
                   curr_regions => region_bank_B

                   ! Expand Bank A (Previous) so they remain symmetric for O(1) swap
                   allocate(temp_regions(max_sweep_capacity))
                   if (n_prev > 0) temp_regions(1:n_prev) = prev_regions(1:n_prev)
                   call move_alloc(from=temp_regions, to=region_bank_A)
                   prev_regions => region_bank_A
                end if
                ! ------------------------------------------

                curr_regions(n_curr)%y1 = y_start
                curr_regions(n_curr)%y2 = y_end
                curr_regions(n_curr)%x_start = current_x 
             end if

             current_lap = current_lap + current_node%lap_change
             current_node => current_node%Forward(1)%ptr
          end do

          p = 1
          c = 1
          do while (p <= n_prev .and. c <= n_curr)
             if (prev_regions(p)%y1 == curr_regions(c)%y1 .and. prev_regions(p)%y2 == curr_regions(c)%y2) then
                curr_regions(c)%x_start = prev_regions(p)%x_start
                p = p + 1
                c = c + 1
             else if (prev_regions(p)%y1 < curr_regions(c)%y1) then
                if (current_x > prev_regions(p)%x_start) then
                   call emit_region(prev_regions(p), current_x)
                end if
                p = p + 1
             else if (prev_regions(p)%y1 > curr_regions(c)%y1) then
                c = c + 1
             else 
                if (current_x > prev_regions(p)%x_start) then
                   call emit_region(prev_regions(p), current_x)
                end if
                p = p + 1
             end if
          end do

          do while (p <= n_prev)
             if (current_x > prev_regions(p)%x_start) then
                call emit_region(prev_regions(p), current_x)
             end if
             p = p + 1
          end do

          ! 4. O(1) State Transfer (The 584-Second Bottleneck Fix)
          if (n_curr > 0) then
             swap_ptr => prev_regions
             prev_regions => curr_regions
             curr_regions => swap_ptr
          end if
          n_prev = n_curr
       end if
    end do

    call destroy_skiplist(active_edges)
    write(*,*) 'In PF (Fractured Count): ', output_count

    allocate(fractured_boxes(output_count))
    if (output_count > 0) then
       fractured_boxes(1:output_count) = temp_boxes(1:output_count)
    end if

  contains

    subroutine emit_region(region, x_end)
      type(ActiveRegion), intent(in) :: region
      integer(kind=K_COORDINATE_KIND), intent(in) :: x_end

      output_count = output_count + 1

      if (output_count > max_boxes) then
         max_boxes = max_boxes * 2_int64
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
    integer(kind=K_COORDINATE_KIND) :: current_x, y_start, y_end
    integer(kind=int64) :: winding_sign, current_lap

    type(ActiveRegion), allocatable :: prev_regions(:), curr_regions(:), temp_regions(:)
    integer :: n_prev, n_curr, max_regions, p, c

    type(Box), allocatable :: temp_boxes(:), resized_boxes(:)
    integer :: output_count, max_boxes

    n = size(trackers)
    if (n == 0) then
       allocate(fractured_boxes(0))
       return
    end if

    call sort_trackers(trackers)

    ! Initialize Arena with capacity N (Guarantees zero heap requests during sweep)
    call init_skiplist(active_edges, int(n, int64))

    max_boxes = max(100, n)
    allocate(temp_boxes(max_boxes))
    output_count = 0

    max_regions = max(100, n/4)
    allocate(prev_regions(max_regions), curr_regions(max_regions))
    n_prev = 0

    do i = 1, n
       winding_sign = sign(1_int64, trackers(i)%polygonNumber)
       call update_active_edges(active_edges, trackers(i)%Y, winding_sign)

       if (i == n .or. trackers(i)%X < trackers(i+1)%X) then
          current_x = trackers(i)%X
          n_curr = 0

          current_node => active_edges%header%Forward(1)%ptr
          current_lap = 0

          do while (associated(current_node))
             if (current_lap <= 0 .and. current_lap + current_node%lap_change > 0) then
                y_start = current_node%y_val
             else if (current_lap > 0 .and. current_lap + current_node%lap_change <= 0) then
                y_end = current_node%y_val

                n_curr = n_curr + 1

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
                curr_regions(n_curr)%x_start = current_x 
             end if

             current_lap = current_lap + current_node%lap_change
             current_node => current_node%Forward(1)%ptr
          end do

          p = 1
          c = 1
          do while (p <= n_prev .and. c <= n_curr)
             if (prev_regions(p)%y1 == curr_regions(c)%y1 .and. prev_regions(p)%y2 == curr_regions(c)%y2) then
                curr_regions(c)%x_start = prev_regions(p)%x_start
                p = p + 1
                c = c + 1
             else if (prev_regions(p)%y1 < curr_regions(c)%y1) then
                if (current_x > prev_regions(p)%x_start) then
                   call emit_region(prev_regions(p), current_x)
                end if
                p = p + 1
             else if (prev_regions(p)%y1 > curr_regions(c)%y1) then
                c = c + 1
             else 
                if (current_x > prev_regions(p)%x_start) then
                   call emit_region(prev_regions(p), current_x)
                end if
                p = p + 1
             end if
          end do

          do while (p <= n_prev)
             if (current_x > prev_regions(p)%x_start) then
                call emit_region(prev_regions(p), current_x)
             end if
             p = p + 1
          end do

          if (n_curr > 0) prev_regions(1:n_curr) = curr_regions(1:n_curr)
          n_prev = n_curr
       end if
    end do

    call destroy_skiplist(active_edges)
    write(*,*) 'In PF (Fractured Count): ', output_count

    allocate(fractured_boxes(output_count))
    if (output_count > 0) then
       fractured_boxes(1:output_count) = temp_boxes(1:output_count)
    end if

  contains

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
  end subroutine old_scanline_fracture

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

    allocate(trackers(4 * (n+1)))
    trackers(1) = XYTracker(X = min_x, Y = min_y, polygonNumber =  1)
    trackers(2) = XYTracker(X = min_x, Y = max_y, polygonNumber = -1)
    trackers(3) = XYTracker(X = max_x, Y = min_y, polygonNumber = -1)
    trackers(4) = XYTracker(X = max_x, Y = max_y, polygonNumber =  1)   

    do i = 1, n
       idx = i * 4
       min_x = min(boxes(i)%x1, boxes(i)%x2)
       max_x = max(boxes(i)%x1, boxes(i)%x2)
       min_y = min(boxes(i)%y1, boxes(i)%y2)
       max_y = max(boxes(i)%y1, boxes(i)%y2)

       trackers(idx + 1) = XYTracker(X = min_x, Y = min_y, polygonNumber = -i)
       trackers(idx + 2) = XYTracker(X = min_x, Y = max_y, polygonNumber =  i)
       trackers(idx + 3) = XYTracker(X = max_x, Y = min_y, polygonNumber =  i)
       trackers(idx + 4) = XYTracker(X = max_x, Y = max_y, polygonNumber = -i)
    end do
  end subroutine generate_trackers
  subroutine heal_boxes4(input_box_count, boxes, output_box_count)
    integer(kind=int64), intent(in) :: input_box_count  
    type(Box), allocatable, intent(inout) :: boxes(:)
    integer(kind=int64), intent(out) :: output_box_count

    type(SkipList) :: active_edges
    type(SkipListNode), pointer :: current_node

    type(XYTracker), allocatable :: events(:)
    integer :: i, n
    integer(kind=K_COORDINATE_KIND) :: current_x, y_start, y_end, min_y, max_y, min_x, max_x
    integer(kind=int64) :: current_lap

    ! O(1) Pointer Swap variables
    type(ActiveRegion), target, allocatable :: region_bank_A(:), region_bank_B(:), temp_regions(:)
    type(ActiveRegion), pointer :: prev_regions(:), curr_regions(:), swap_ptr(:)
    integer :: n_prev, n_curr, p, c

    ! Output collection
    type(Box), allocatable :: temp_boxes(:), resized_boxes(:)
    integer(kind=int64) :: max_boxes, max_sweep_capacity
    logical :: process_x_slice
    n = int(input_box_count, int32)
    if (n == 0) then
       output_box_count = 0
       return
    end if

    ! 1. Generate O(1) Point Boundaries (The Derivative Trick)
    allocate(events(4 * n))
    do i = 1, n
       min_x = min(boxes(i)%x1, boxes(i)%x2)
       max_x = max(boxes(i)%x1, boxes(i)%x2)
       min_y = min(boxes(i)%y1, boxes(i)%y2)
       max_y = max(boxes(i)%y1, boxes(i)%y2)

       ! Left Edge (Insert Box) -> Adds +1 to the interval [min_y, max_y)
       events(4*i - 3) = XYTracker(X = min_x, Y = min_y, polygonNumber =  1) 
       events(4*i - 2) = XYTracker(X = min_x, Y = max_y, polygonNumber = -1) 

       ! Right Edge (Remove Box) -> Subtracts 1 from the interval [min_y, max_y)
       events(4*i - 1) = XYTracker(X = max_x, Y = min_y, polygonNumber = -1) 
       events(4*i)     = XYTracker(X = max_x, Y = max_y, polygonNumber =  1) 
    end do

    ! 2. Sort and Initialize
    call sort_trackers(events)
    call init_skiplist(active_edges, min(int(4*n, int64), 5000000_int64))

    max_boxes = max(100000_int64, int(n, int64))
    allocate(temp_boxes(max_boxes))
    output_box_count = 0

    max_sweep_capacity = min(int(n, int64)/2_int64 + 1, 2000000_int64)
    allocate(region_bank_A(max_sweep_capacity), region_bank_B(max_sweep_capacity))
    prev_regions => region_bank_A
    curr_regions => region_bank_B
    n_prev = 0

    ! 3. Sweep Line Algorithm
    do i = 1, 4*n
       ! Insert boundary point in O(log N)
       call update_active_edges(active_edges, events(i)%Y, sign(1_int64, events(i)%polygonNumber))

       ! --- THE SAFE BOUNDS FIX ---
       process_x_slice = .false.
       if (i == 4*n) then
          process_x_slice = .true.
       else if (events(i)%X < events(i+1)%X) then
          process_x_slice = .true.
       end if

       if (process_x_slice) then
          current_x = events(i)%X
          n_curr = 0

          current_node => active_edges%header%Forward(1)%ptr
          current_lap = 0

          ! Walk ONLY the active edges (O(M) instead of O(Total Y Universe))
          do while (associated(current_node))
             ! Integrate lap counts
             if (current_lap <= 0 .and. current_lap + current_node%lap_change > 0) then
                y_start = current_node%y_val
             else if (current_lap > 0 .and. current_lap + current_node%lap_change <= 0) then
                y_end = current_node%y_val
                n_curr = n_curr + 1

                ! Dynamic Bank Expansion (Safe Fallback)
                if (n_curr > size(region_bank_B)) then
                   max_sweep_capacity = size(region_bank_B) * 2_int64
                   allocate(temp_regions(max_sweep_capacity))
                   temp_regions(1:n_curr-1) = curr_regions(1:n_curr-1)
                   call move_alloc(from=temp_regions, to=region_bank_B)
                   curr_regions => region_bank_B

                   allocate(temp_regions(max_sweep_capacity))
                   if (n_prev > 0) temp_regions(1:n_prev) = prev_regions(1:n_prev)
                   call move_alloc(from=temp_regions, to=region_bank_A)
                   prev_regions => region_bank_A
                end if

                curr_regions(n_curr)%y1 = y_start
                curr_regions(n_curr)%y2 = y_end
                curr_regions(n_curr)%x_start = current_x 
             end if

             current_lap = current_lap + current_node%lap_change
             current_node => current_node%Forward(1)%ptr
          end do

          ! Fast Two-Pointer Merge (Heals adjacent segments horizontally)
          p = 1
          c = 1
          do while (p <= n_prev .and. c <= n_curr)
             if (prev_regions(p)%y1 == curr_regions(c)%y1 .and. prev_regions(p)%y2 == curr_regions(c)%y2) then
                curr_regions(c)%x_start = prev_regions(p)%x_start
                p = p + 1
                c = c + 1
             else if (prev_regions(p)%y1 < curr_regions(c)%y1) then
                if (current_x > prev_regions(p)%x_start) call emit_region(prev_regions(p), current_x)
                p = p + 1
             else if (prev_regions(p)%y1 > curr_regions(c)%y1) then
                c = c + 1
             else 
                if (current_x > prev_regions(p)%x_start) call emit_region(prev_regions(p), current_x)
                p = p + 1
             end if
          end do

          do while (p <= n_prev)
             if (current_x > prev_regions(p)%x_start) call emit_region(prev_regions(p), current_x)
             p = p + 1
          end do

          ! O(1) Swap
          if (n_curr > 0) then
             swap_ptr => prev_regions
             prev_regions => curr_regions
             curr_regions => swap_ptr
          end if
          n_prev = n_curr
       end if
    end do

    ! 4. Clean up
    call destroy_skiplist(active_edges)

    ! Finalize output safely
    if (output_box_count > 0 .and. output_box_count < size(boxes)) then
       boxes(1:output_box_count) = temp_boxes(1:output_box_count)
    else
       allocate(resized_boxes(output_box_count))
       if (output_box_count > 0) resized_boxes(1:output_box_count) = temp_boxes(1:output_box_count)
       call move_alloc(from=resized_boxes, to=boxes)
    end if

  contains

    subroutine emit_region(region, x_end)
      type(ActiveRegion), intent(in) :: region
      integer(kind=K_COORDINATE_KIND), intent(in) :: x_end

      output_box_count = output_box_count + 1

      if (output_box_count > max_boxes) then
         max_boxes = max_boxes + (max_boxes / 2_int64) + 1_int64
         allocate(resized_boxes(max_boxes))
         resized_boxes(1:output_box_count-1) = temp_boxes(1:output_box_count-1)
         call move_alloc(from=resized_boxes, to=temp_boxes)
      end if

      temp_boxes(output_box_count)%x1 = region%x_start
      temp_boxes(output_box_count)%x2 = x_end
      temp_boxes(output_box_count)%y1 = region%y1
      temp_boxes(output_box_count)%y2 = region%y2
    end subroutine emit_region

  end subroutine heal_boxes4
  subroutine heal_boxes(input_box_count, boxes, output_box_count)
    integer(kind=int64), intent(in) :: input_box_count  
    type(Box), allocatable, intent(inout) :: boxes(:)
    integer(kind=int64), intent(out) :: output_box_count

    type(SkipList) :: active_edges
    type(SkipListNode), pointer :: current_node

    type(XYTracker), allocatable :: events(:)
    integer :: i
    integer(kind=int64) :: n
    integer(kind=K_COORDINATE_KIND) :: current_x, y_start, y_end, min_y, max_y, min_x, max_x
    integer(kind=int64) :: current_lap

    ! O(1) Pointer Swap variables
    type(ActiveRegion), target, allocatable :: region_bank_A(:), region_bank_B(:), temp_regions(:)
    type(ActiveRegion), pointer :: prev_regions(:), curr_regions(:), swap_ptr(:)
    integer :: n_prev, n_curr, p, c

    ! Output collection
    type(Box), allocatable :: temp_boxes(:), resized_boxes(:)
    integer(kind=int64) :: max_boxes, max_sweep_capacity
    logical :: process_x_slice
    n = input_box_count
    if (n == 0) then
       output_box_count = 0
       return
    end if

    ! 1. Generate O(1) Point Boundaries (The Derivative Trick)
    allocate(events(4 * n))
    do i = 1, n
       min_x = min(boxes(i)%x1, boxes(i)%x2)
       max_x = max(boxes(i)%x1, boxes(i)%x2)
       min_y = min(boxes(i)%y1, boxes(i)%y2)
       max_y = max(boxes(i)%y1, boxes(i)%y2)

       ! Left Edge (+1 interval interval start, -1 interval end)
       events(4*i - 3) = XYTracker(X = min_x, Y = min_y, polygonNumber =  1_int64) 
       events(4*i - 2) = XYTracker(X = min_x, Y = max_y, polygonNumber = -1_int64) 

       ! Right Edge (-1 interval interval start, +1 interval end)
       events(4*i - 1) = XYTracker(X = max_x, Y = min_y, polygonNumber = -1_int64) 
       events(4*i)     = XYTracker(X = max_x, Y = max_y, polygonNumber =  1_int64) 
    end do

    ! 2. Sort and Initialize
    call sort_trackers(events)

    ! Cap SkipList to safe memory maximum (e.g., 5M concurrent active Y edges)
    call init_skiplist(active_edges, min(n * 4_int64, 5000000_int64))

    max_boxes = max(100000_int64, n)
    allocate(temp_boxes(max_boxes))
    output_box_count = 0

    max_sweep_capacity = min(n/2_int64 + 1_int64, 2000000_int64)
    allocate(region_bank_A(max_sweep_capacity), region_bank_B(max_sweep_capacity))
    prev_regions => region_bank_A
    curr_regions => region_bank_B
    n_prev = 0

    ! 3. Sweep Line Algorithm
    do i = 1, 4*n
       ! OPTIMIZATION: Bypass sign() intrinsic, directly use polygonNumber as lap_delta
       call update_active_edges(active_edges, events(i)%Y, events(i)%polygonNumber)
       ! --- THE SAFE BOUNDS FIX ---
       process_x_slice = .false.
       if (i == 4*n) then
          process_x_slice = .true.
       else if (events(i)%X < events(i+1)%X) then
          process_x_slice = .true.
       end if

       if (process_x_slice) then
          current_x = events(i)%X
          n_curr = 0

          current_node => active_edges%header%Forward(1)%ptr
          current_lap = 0

          ! Walk ONLY the active edges (O(M) instead of O(Total Y Universe))
          do while (associated(current_node))
             ! Integrate lap counts
             if (current_lap <= 0 .and. current_lap + current_node%lap_change > 0) then
                y_start = current_node%y_val
             else if (current_lap > 0 .and. current_lap + current_node%lap_change <= 0) then
                y_end = current_node%y_val
                n_curr = n_curr + 1

                ! Dynamic Bank Expansion (Safe Fallback)
                if (n_curr > size(region_bank_B)) then
                   max_sweep_capacity = size(region_bank_B) * 2_int64

                   allocate(temp_regions(max_sweep_capacity))
                   temp_regions(1:n_curr-1) = curr_regions(1:n_curr-1)
                   call move_alloc(from=temp_regions, to=region_bank_B)
                   curr_regions => region_bank_B

                   allocate(temp_regions(max_sweep_capacity))
                   if (n_prev > 0) temp_regions(1:n_prev) = prev_regions(1:n_prev)
                   call move_alloc(from=temp_regions, to=region_bank_A)
                   prev_regions => region_bank_A
                end if

                curr_regions(n_curr)%y1 = y_start
                curr_regions(n_curr)%y2 = y_end
                curr_regions(n_curr)%x_start = current_x 
             end if

             current_lap = current_lap + current_node%lap_change
             current_node => current_node%Forward(1)%ptr
          end do

          ! Fast Two-Pointer Merge (Heals adjacent segments horizontally)
          p = 1
          c = 1
          do while (p <= n_prev .and. c <= n_curr)
             if (prev_regions(p)%y1 == curr_regions(c)%y1 .and. prev_regions(p)%y2 == curr_regions(c)%y2) then
                curr_regions(c)%x_start = prev_regions(p)%x_start
                p = p + 1
                c = c + 1
             else if (prev_regions(p)%y1 < curr_regions(c)%y1) then
                if (current_x > prev_regions(p)%x_start) call emit_region(prev_regions(p), current_x)
                p = p + 1
             else if (prev_regions(p)%y1 > curr_regions(c)%y1) then
                c = c + 1
             else 
                if (current_x > prev_regions(p)%x_start) call emit_region(prev_regions(p), current_x)
                p = p + 1
             end if
          end do

          do while (p <= n_prev)
             if (current_x > prev_regions(p)%x_start) call emit_region(prev_regions(p), current_x)
             p = p + 1
          end do

          ! OPTIMIZATION: Unconditional O(1) Swap. 
          ! Prevents stale 'prev_regions' data from polluting future sparse intervals.
          swap_ptr => prev_regions
          prev_regions => curr_regions
          curr_regions => swap_ptr
          n_prev = n_curr
       end if
    end do

    ! 4. Clean up
    call destroy_skiplist(active_edges)

    ! Finalize output safely
    if (output_box_count > 0 .and. output_box_count < size(boxes)) then
       boxes(1:output_box_count) = temp_boxes(1:output_box_count)
    else
       allocate(resized_boxes(output_box_count))
       if (output_box_count > 0) resized_boxes(1:output_box_count) = temp_boxes(1:output_box_count)
       call move_alloc(from=resized_boxes, to=boxes)
    end if

  contains

    subroutine emit_region(region, x_end)
      type(ActiveRegion), intent(in) :: region
      integer(kind=K_COORDINATE_KIND), intent(in) :: x_end

      output_box_count = output_box_count + 1

      if (output_box_count > max_boxes) then
         max_boxes = max_boxes + (max_boxes / 2_int64) + 1_int64
         allocate(resized_boxes(max_boxes))
         resized_boxes(1:output_box_count-1) = temp_boxes(1:output_box_count-1)
         call move_alloc(from=resized_boxes, to=temp_boxes)
      end if

      temp_boxes(output_box_count)%x1 = region%x_start
      temp_boxes(output_box_count)%x2 = x_end
      temp_boxes(output_box_count)%y1 = region%y1
      temp_boxes(output_box_count)%y2 = region%y2
    end subroutine emit_region

  end subroutine heal_boxes

  ! =========================================================================
  ! UNION AREA SWEEP LINE
  ! =========================================================================
  function calculate_union_area_sl(boxes) result(area)
    type(Box), intent(in) :: boxes(:)
    integer(kind=real64)  :: area

    integer :: n, i, ev_idx
    type(Event), allocatable :: events(:)
    type(SkipList), target :: sl
    integer(kind=int64) :: current_covered
    integer(kind=K_COORDINATE_KIND) :: current_x, dx

    n = size(boxes)
    area = 0.0_real64
    if (n == 0) return

    ! 1. Create Event Queue (Left and Right edges)
    allocate(events(2*n))
    do i = 1, n
       ! Left Edge (+1 lap)
       events(2*i - 1)%x          = min(boxes(i)%x1, boxes(i)%x2)
       events(2*i - 1)%y1         = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%y2         = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i - 1)%lap_change = 1_int64

       ! Right Edge (-1 lap)
       events(2*i)%x          = max(boxes(i)%x1, boxes(i)%x2)
       events(2*i)%y1         = min(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%y2         = max(boxes(i)%y1, boxes(i)%y2)
       events(2*i)%lap_change = -1_int64
    end do

    ! Sort events by X coordinate
    call sort_events(events)

    ! 2. Initialize SkipList with Arena Capacity
    ! Maximum possible distinct Y nodes is 2*N
    call sl_init(sl, 2*n)

    ! 3. Sweep Line Algorithm
    current_x = events(1)%x
    ev_idx = 1

    do while (ev_idx <= 2*n)
       dx = events(ev_idx)%x - current_x

       ! If we advanced in X, calculate area for the previous vertical slice
       if (dx > 0) then
          call sl_get_covered_y(sl, current_covered)
          area = area + real(dx, real64) * real(current_covered, real64)
          current_x = events(ev_idx)%x
       end if
       ! Group processing: Process ALL events sharing this exact X coordinate
       ! This prevents redundant SkipList area calculations.
       do while (ev_idx <= 2*n)
          if (events(ev_idx)%x /= current_x) exit

          ! Delta array logic: 
          ! Event lap_change modifies the lower bound normally, and upper bound inversely
          call sl_add_delta(sl, events(ev_idx)%y1, events(ev_idx)%lap_change)
          call sl_add_delta(sl, events(ev_idx)%y2, -events(ev_idx)%lap_change)

          ev_idx = ev_idx + 1
       end do
    end do

    call sl_destroy(sl)
  end function calculate_union_area_sl


  ! =========================================================================
  ! SKIP LIST IMPLEMENTATION (ARENA-BACKED)
  ! =========================================================================
  pure subroutine sl_init(sl, max_nodes)
    type(SkipList), intent(inout) :: sl
    integer, intent(in)           :: max_nodes
    integer :: i

    allocate(sl%Arena(max_nodes + 1)) ! +1 for the Header node

    ! Initialize Memory Pool (Free List)
    do i = 2, max_nodes
       sl%Arena(i)%NextFree%ptr => sl%Arena(i+1)
    end do
    sl%Arena(max_nodes + 1)%NextFree%ptr => null()
    sl%FreeHead%ptr => sl%Arena(2)

    ! Setup Header Node
    sl%header => sl%Arena(1)
    sl%header%y_val = -huge(1_int64) ! Negative infinity
    sl%header%lap_change = 0
    sl%current_level = 1

    do i = 1, MAX_SKIP_LEVEL
       sl%header%Forward(i)%ptr => null()
       sl%header%Backward(i)%ptr => null()
    end do
  end subroutine sl_init

  pure subroutine sl_destroy(sl)
    type(SkipList), intent(inout) :: sl
    if (associated(sl%Arena)) deallocate(sl%Arena)
  end subroutine sl_destroy

  ! The core insert/update logic for the Delta Scanline
  subroutine sl_add_delta(sl, target_y, delta)
    type(SkipList), intent(inout) :: sl
    integer(kind=K_COORDINATE_KIND), intent(in) :: target_y
    integer(kind=int64), intent(in) :: delta

    type(SkipListNode), pointer :: current, new_node
    type(NodePtr) :: update_nodes(MAX_SKIP_LEVEL)
    integer :: i, new_level

    current => sl%header

    ! Search for position
    do i = sl%current_level, 1, -1
       do while (associated(current%Forward(i)%ptr))
          if (current%Forward(i)%ptr%y_val < target_y) then
             current => current%Forward(i)%ptr
          else
             exit
          end if
       end do
       update_nodes(i)%ptr => current
    end do

    current => current%Forward(1)%ptr
    ! Case A: The Y coordinate already exists. Modify its delta.
    if (associated(current)) then
       if (current%y_val == target_y) then
          current%lap_change = current%lap_change + delta

          ! Optimization: If net delta is 0, node is useless. Prune it.
          if (current%lap_change == 0) then
             do i = 1, sl%current_level
                if (.not. associated(update_nodes(i)%ptr%Forward(i)%ptr)) exit
                if (.not. associated(update_nodes(i)%ptr%Forward(i)%ptr, current)) exit

                update_nodes(i)%ptr%Forward(i)%ptr => current%Forward(i)%ptr
                if (associated(current%Forward(i)%ptr)) then
                   current%Forward(i)%ptr%Backward(i)%ptr => update_nodes(i)%ptr
                end if
             end do

             ! Downgrade level if top levels are now empty
             do while (sl%current_level > 1 .and. &
                  .not. associated(sl%header%Forward(sl%current_level)%ptr))
                sl%current_level = sl%current_level - 1
             end do

             ! Return node to Free Pool
             current%NextFree%ptr => sl%FreeHead%ptr
             sl%FreeHead%ptr => current
          end if
          return
       end if
    end if


    ! Case B: Target Y does not exist. Insert it from the pool.
    if (.not. associated(sl%FreeHead%ptr)) return ! Failsafe (should never hit if Arena is 2*N)

    new_node => sl%FreeHead%ptr
    sl%FreeHead%ptr => new_node%NextFree%ptr

    new_node%y_val = target_y
    new_node%lap_change = delta
    new_level = pseudo_random_level()

    if (new_level > sl%current_level) then
       do i = sl%current_level + 1, new_level
          update_nodes(i)%ptr => sl%header
       end do
       sl%current_level = new_level
    end if

    ! Link new node into the SkipList
    do i = 1, new_level
       new_node%Forward(i)%ptr => update_nodes(i)%ptr%Forward(i)%ptr
       update_nodes(i)%ptr%Forward(i)%ptr => new_node

       new_node%Backward(i)%ptr => update_nodes(i)%ptr
       if (associated(new_node%Forward(i)%ptr)) then
          new_node%Forward(i)%ptr%Backward(i)%ptr => new_node
       end if
    end do
  end subroutine sl_add_delta
  pure subroutine sl_get_covered_y(sl, covered)
    type(SkipList), intent(inout) :: sl
    integer(kind=int64), intent(out) :: covered

    type(SkipListNode), pointer :: current
    integer(kind=int64) :: running_lap
    integer(kind=K_COORDINATE_KIND) :: last_y

    covered = 0_int64
    running_lap = 0_int64
    current => sl%header%Forward(1)%ptr

    if (.not. associated(current)) return
    last_y = current%y_val

    do while (associated(current))
       if (running_lap > 0) then
          covered = covered + int(current%y_val - last_y, int64)
       end if

       ! Integrate the difference array
       running_lap = running_lap + current%lap_change
       last_y = current%y_val
       current => current%Forward(1)%ptr
    end do
  end subroutine sl_get_covered_y
  ! Traverses Level 1 to sum up lengths where running lap > 0

  ! Deterministic pseudo-random level generator for PURE context
  pure function pseudo_random_level() result(lvl)
    integer :: lvl
    ! Note: Since this is a pure function, we mock randomness deterministically
    ! or rely on a hashed state. For true random, remove PURE attribute. 
    ! Using a bitwise hash of a static counter is a common pure workaround.
    lvl = 1
    ! Simplified for example; a real implementation might use an LCG here.
    ! Assuming perfect coin flips:
    do while (mod(lvl * 1103515245 + 12345, 2) == 0 .and. lvl < MAX_SKIP_LEVEL)
       lvl = lvl + 1
    end do
  end function pseudo_random_level
  pure subroutine sort_events(arr)
    type(Event), intent(inout) :: arr(:)

    if (size(arr) > 1) then
       call quicksort_events(arr, 1, size(arr))
    end if
  end subroutine sort_events
  ! Basic Event Sort Implementation (Required for completeness)
  pure subroutine insertion_sort_events(arr)
    type(Event), intent(inout) :: arr(:)
    integer :: i, j
    type(Event) :: key

    ! Insertion sort for brevity (Swap for QuickSort in production for large N)
    do i = 2, size(arr)
       key = arr(i)
       j = i - 1
       do while (j >= 1)
          if (arr(j)%x > key%x) then
             arr(j + 1) = arr(j)
             j = j - 1
          else
             exit
          end if
       end do
       arr(j + 1) = key
    end do
  end subroutine insertion_sort_events

  ! =========================================================================
  ! RECURSIVE HYBRID QUICKSORT
  ! =========================================================================
  pure recursive subroutine quicksort_events(arr, left, right)
    type(Event), intent(inout) :: arr(:)
    integer, intent(in) :: left, right

    integer :: i, j
    type(Event) :: key, temp, pivot

    ! Base safety check
    if (left >= right) return

    ! --- INSERTION SORT (Fast for small thresholds) ---
    if (right - left + 1 <= K_SMALL_THRESHOLD) then
       do i = left + 1, right
          key = arr(i)
          j = i - 1
          do while (j >= left)
             if (arr(j)%x > key%x) then
                arr(j + 1) = arr(j)
                j = j - 1
             else
                exit
             end if
          end do
          arr(j + 1) = key
       end do

       ! --- QUICKSORT (Fast for large partitions) ---
    else
       ! Use middle element as pivot to mitigate worst-case on sorted data
       pivot = arr(left + (right - left) / 2)

       i = left
       j = right

       ! Partitioning phase
       do while (i <= j)
          ! Find elements on the wrong side of the pivot
          do while (arr(i)%x < pivot%x)
             i = i + 1
          end do

          do while (arr(j)%x > pivot%x)
             j = j - 1
          end do

          ! Swap them if pointers haven't crossed
          if (i <= j) then
             temp = arr(i)
             arr(i) = arr(j)
             arr(j) = temp

             i = i + 1
             j = j - 1
          end if
       end do

       ! Recursive calls for the remaining partitions
       if (left < j)  call quicksort_events(arr, left, j)
       if (i < right) call quicksort_events(arr, i, right)
    end if
  end subroutine quicksort_events
end module PolygonFractureModule
