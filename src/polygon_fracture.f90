! File   : polygon_fracture.f90
! Author : Sandeep Koranne
! Purpose: High-Performance Scanline Fracture using Doubly-Linked LIFO Arena SkipList.
!          Features O(1) deletions and O(log N) UP/DOWN neighbor resolutions.

module PolygonFractureModule
   use CommonModule
   use GeometryModule
   use HDFDataModule
   use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
   implicit none
   private

   integer, parameter :: K_POLYGON_INIT_BOX_COUNT = 64
   integer, parameter :: K_SMALL_THRESHOLD = 64
   ! --- Constants & Kinds ---
   integer, parameter    :: MAX_SKIP_LEVEL = 24

   ! --- Public Exports ---
   public :: XYTracker
   !public: SkipList, SkipListNode, NodePtr
   !public :: init_skiplist, update_active_edges, find_bounding_edges, destroy_skiplist, calculate_union_area_sl
   public :: sort_trackers, scanline_fracture, generate_trackers, heal_boxes
   public :: K_COORDINATE_KIND

   type :: ActiveRegion
      integer(kind=K_COORDINATE_KIND) :: y1, y2, x_start
   end type ActiveRegion


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
   end type SkipListNode

   type :: SkipList
      integer :: current_level
      type(SkipListNode), pointer :: header => null()
      ! Memory Pool Optimization
      type(SkipListNode), pointer :: Arena(:) => null()
      type(NodePtr)               :: FreeHead
   end type SkipList

contains
   ! ============================================================================
   ! MEMORY POOL & SKIPLIST LIFECYCLE
   ! ============================================================================
   pure subroutine init_skiplist(list, capacity)
      type(SkipList), intent(inout), target :: list
      integer(int64), intent(in)    :: capacity
      integer(int64) :: i
      integer :: j
      type(SkipListNode), pointer :: temp_node

      list%current_level = 1
      ! Allocate contiguous arena block
      allocate(list%Arena(capacity + 1))

      ! Chain the LIFO free list
      do i = 1, capacity
         list%Arena(i)%Forward(1)%ptr => list%Arena(i+1)
         do j = 2, MAX_SKIP_LEVEL
            list%Arena(i)%Forward(j)%ptr  => null()
         end do
      end do

      list%Arena(capacity + 1)%Forward(1)%ptr => null()
      do j = 1, MAX_SKIP_LEVEL
         list%Arena(capacity + 1)%Forward(j)%ptr  => null()
      end do

      list%FreeHead%ptr => list%Arena(1)

      ! Consume the first node as the Sentinel Header via subroutine call
      call SL_GET_FREE(list, temp_node)
      list%header => temp_node
      list%header%y_val = -huge(1_K_COORDINATE_KIND)
      list%header%lap_change = 0
   end subroutine init_skiplist

   pure subroutine destroy_skiplist(list)
      type(SkipList), intent(inout), target :: list
      if (associated(list%Arena)) deallocate(list%Arena)
      nullify(list%header)
      nullify(list%FreeHead%ptr)
   end subroutine destroy_skiplist

   ! Converted to Subroutine to bypass PURE FUNCTION INTENT(IN) constraints
   pure subroutine SL_GET_FREE(list, node)
      type(SkipList), intent(inout), target :: list
      type(SkipListNode), pointer, intent(out) :: node
      integer(kind=int64) :: i
      if (.not. associated(list%FreeHead%ptr)) then
         error stop "CRITICAL: Polygon Fracture SkipList Arena Exhausted!"
      end if
      node => list%FreeHead%ptr
      list%FreeHead%ptr => node%Forward(1)%ptr
      do i = 1, MAX_SKIP_LEVEL
         node%Forward(i)%ptr => null()
      end do
   end subroutine SL_GET_FREE

   pure subroutine SL_RELEASE(list, node)
      type(SkipList), intent(inout), target :: list
      type(SkipListNode), pointer, intent(inout) :: node
      integer :: i

      ! Wipe pointers to prevent ghost links
      do i = 1, MAX_SKIP_LEVEL
         node%Forward(i)%ptr  => null()
      end do
      node%Forward(1)%ptr => list%FreeHead%ptr
      list%FreeHead%ptr => node
   end subroutine SL_RELEASE

   ! Pure, hash-based deterministic level generator
   pure function get_skip_level(y_val) result(lvl)
      integer(kind=K_COORDINATE_KIND), intent(in) :: y_val
      integer :: lvl
      integer(int32) :: hash

      hash = int(y_val, int32)
      hash = ieor(hash, ishft(hash, -16))
      hash = hash * 212519137_int32
      hash = ieor(hash, ishft(hash, -15))
      hash = hash * 401662913_int32

      lvl = 1
      do while (iand(hash, 1_int32) == 0_int32 .and. lvl < MAX_SKIP_LEVEL)
         lvl = lvl + 1
         hash = ishft(hash, -1)
      end do
   end function get_skip_level

   ! ============================================================================
   ! O(log N) UP/DOWN SWEEP-LINE ENGINE
   ! ============================================================================
   subroutine update_active_edges(list, y_val, lap_delta)
      !--------------------------------------------------------------------
      ! PURPOSE
      !   Insert a new boundary (y_val, lap_delta) into the skip‑list
      !   or, if the y‑coordinate already exists, add lap_delta to its
      !   lap_change field.  When the accumulated lap_change becomes zero
      !   the node is removed.
      !
      !   The implementation uses **forward links only** – the
      !   Backward array has been eliminated.
      !--------------------------------------------------------------------
      use, intrinsic :: iso_fortran_env, only: int8, int64
      implicit none

      type(SkipList), intent(inout), target :: list
      integer(kind=K_COORDINATE_KIND), intent(in) :: y_val
      integer(kind=int8), intent(in)              :: lap_delta

      ! Local helpers ----------------------------------------------------
      type(NodePtr)                :: update(MAX_SKIP_LEVEL)   ! predecessor per level
      type(SkipListNode), pointer :: current   => null()
      type(SkipListNode), pointer :: target_node => null()
      type(SkipListNode), pointer :: new_node    => null()
      integer                      :: i, lvl

      !--------------------------------------------------------------------
      ! 1.  Find the place where y_val would belong (standard skip‑list search)
      !--------------------------------------------------------------------
      current => list%header
      do i = list%current_level, 1, -1
         ! Walk forward while the next node on this level is still < y_val
         do
            if (.not. associated(current%Forward(i)%ptr)) exit          ! no next node
            if (current%Forward(i)%ptr%y_val >= y_val) exit              ! y_val too large
            current => current%Forward(i)%ptr                           ! advance
         end do
         update(i)%ptr => current               ! remember predecessor on level i
      end do

      ! The first node at level‑1 after the predecessor is the possible match
      target_node => current%Forward(1)%ptr

      !--------------------------------------------------------------------
      ! 2.  If we found the exact y‑coordinate, just update the payload
      !--------------------------------------------------------------------
      if (associated(target_node)) then
         if (target_node%y_val == y_val) then
            target_node%lap_change = target_node%lap_change + lap_delta

            ! If the net change is now zero we have to delete the node
            if (target_node%lap_change == 0_int64) then
               !--- splice it out on every level where it appears -----------------
               do i = 1, list%current_level
                  if (.not. associated(update(i)%ptr%Forward(i)%ptr)) exit
                  if (.not. associated(update(i)%ptr%Forward(i)%ptr, target_node)) exit
                  update(i)%ptr%Forward(i)%ptr => target_node%Forward(i)%ptr
               end do

               !--- possibly shrink the current_level -----------------------------
               do while (list%current_level > 1 .and. &
                  .not. associated(list%header%Forward(list%current_level)%ptr))
                  list%current_level = list%current_level - 1
               end do

               call SL_RELEASE(list, target_node)   ! return node to the free‑list
            end if

            return                               ! we are done
         end if
      end if

      !--------------------------------------------------------------------
      ! 3.  No existing node with this y‑value – we must insert a new one.
      !     (If the delta is zero there is nothing to insert.)
      !--------------------------------------------------------------------
      if (lap_delta == 0_int64) return

      lvl = get_skip_level(y_val)                ! randomised level for the new node

      ! If the new level is higher than anything we have seen, extend the header
      if (lvl > list%current_level) then
         do i = list%current_level + 1, lvl
            update(i)%ptr => list%header
         end do
         list%current_level = lvl
      end if

      ! Grab a fresh node from the pool (or allocate one)
      call SL_GET_FREE(list, new_node)

      new_node%y_val      = y_val
      new_node%lap_change = lap_delta

      !--------------------------------------------------------------------
      ! 4.  Forward‑only splice: link the new node into every level ≤ lvl
      !--------------------------------------------------------------------
      do i = 1, lvl
         new_node%Forward(i)%ptr          => update(i)%ptr%Forward(i)%ptr
         update(i)%ptr%Forward(i)%ptr     => new_node
      end do

   end subroutine update_active_edges

   pure subroutine get_covered_y(sl, covered)
      type(SkipList), intent(inout),target :: sl
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

         running_lap = running_lap + current%lap_change
         last_y = current%y_val
         current => current%Forward(1)%ptr
      end do
   end subroutine get_covered_y

   ! ============================================================================
   ! SORTING LOGIC
   ! ============================================================================
   pure recursive subroutine quicksort_trackers(arr, left, right)
      type(XYTracker), intent(inout) :: arr(:)
      integer, intent(in) :: left, right
      integer :: i, j, n
      type(XYTracker) :: pivot, temp

      if (left >= right) return
      n = right - left + 1

      if (n < K_SMALL_THRESHOLD) then
         call insertion_sort_trackers(arr, left, right)
         return
      end if

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

      if (left < j) call quicksort_trackers(arr, left, j)
      if (i < right) call quicksort_trackers(arr, i, right)
   end subroutine quicksort_trackers

   pure subroutine insertion_sort_trackers(arr, left, right)
      type(XYTracker), intent(inout) :: arr(:)
      integer, intent(in) :: left, right
      integer :: i, j
      type(XYTracker) :: key

      do i = left + 1, right
         key = arr(i)
         j = i - 1
         do while (j >= left)
            if (is_less_than(key, arr(j))) then
               arr(j + 1) = arr(j)
               j = j - 1
            else
               exit
            end if
         end do
         arr(j + 1) = key
      end do
   end subroutine insertion_sort_trackers

   pure recursive subroutine sort_trackers(arr)
      type(XYTracker), intent(inout) :: arr(:)
      integer :: n
      if (size(arr) <= 1) return
      n = size(arr)
      call quicksort_trackers(arr, 1, n)
   end subroutine sort_trackers

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
   ! SCANLINE & FRACTURE ENGINES
   ! On demand
   ! ============================================================================
   subroutine scanline_fracture(trackers, fractured_boxes)
      !-----------------------------------------------------------------
      !  PURPOSE
      !    Scan‑line fracture of a set of axis‑aligned boxes.  The routine
      !    now correctly handles the case where the input boxes overlap.
      !
      !  INPUT
      !    trackers(:)        – XY edge list (already sorted by X after the
      !                         call to sort_trackers)
      !
      !  OUTPUT
      !    fractured_boxes(:) – non‑overlapping boxes produced by the scan.
      !-----------------------------------------------------------------
      use iso_fortran_env, only: int8, int64
      implicit none

      type(XYTracker),               intent(inout) :: trackers(:)
      type(Box),          allocatable, intent(out) :: fractured_boxes(:)

      !-----------------------------------------------------------------
      !  Local data‑structures
      !-----------------------------------------------------------------
      type(SkipList)                     :: active_edges
      type(SkipListNode), pointer        :: current_node
      integer                            :: i, n
      integer(kind=K_COORDINATE_KIND)    :: current_x, y_start, y_end
      integer(kind=int64)                :: current_lap
      integer(kind=int8)                 :: winding_sign
      integer(kind=int64)                :: max_sweep_capacity
      type(ActiveRegion), target, allocatable :: region_bank_A(:), region_bank_B(:), temp_regions(:)
      type(ActiveRegion), pointer        :: prev_regions(:), curr_regions(:), swap_ptr(:)
      integer                            :: n_prev, n_curr, p, c
      type(Box),          allocatable    :: temp_boxes(:), resized_boxes(:)
      integer(kind=int64)                :: output_count, max_boxes
      logical                            :: process_x_slice

      !-----------------------------------------------------------------
      !  Initialisation
      !-----------------------------------------------------------------
      n = size(trackers)
      if (n == 0) then
         allocate(fractured_boxes(0))
         return
      end if

      call sort_trackers(trackers)
      call init_skiplist(active_edges, min(int(n, int64), 5000000_int64))

      max_boxes = max(100000_int64, int(n, int64) * 2_int64)
      allocate(temp_boxes(max_boxes))
      output_count = 0_int64

      max_sweep_capacity = min(int(n, int64)/2_int64, 2000000_int64)
      allocate(region_bank_A(max_sweep_capacity), region_bank_B(max_sweep_capacity))

      prev_regions => region_bank_A
      curr_regions => region_bank_B
      n_prev = 0

      !-----------------------------------------------------------------
      !  Main scan‑line loop
      !-----------------------------------------------------------------
      do i = 1, n
         winding_sign = sign(1_int8, trackers(i)%polygonNumber)
         call update_active_edges(active_edges, trackers(i)%Y, winding_sign)

         process_x_slice = .false.
         if (i == n) then
            process_x_slice = .true.
         else if (trackers(i)%X < trackers(i+1)%X) then
            process_x_slice = .true.
         end if

         if (process_x_slice) then
            current_x = trackers(i)%X
            n_curr    = 0
            current_node => active_edges%header%Forward(1)%ptr
            current_lap  = 0

            !-------------------------------------------------------------
            !  Walk the active‑edge list and build the raw Y‑intervals
            !-------------------------------------------------------------
            do while (associated(current_node))
               if (current_lap <= 0_int64 .and. &
                  current_lap + current_node%lap_change > 0_int64) then
                  y_start = current_node%y_val
               else if (current_lap > 0_int64 .and. &
                  current_lap + current_node%lap_change <= 0_int64) then
                  y_end = current_node%y_val
                  n_curr = n_curr + 1

                  !--- grow region banks if needed ---------------------------------
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
                  !-----------------------------------------------------------------
                  curr_regions(n_curr)%y1       = y_start
                  curr_regions(n_curr)%y2       = y_end
                  curr_regions(n_curr)%x_start  = current_x
               end if

               current_lap = current_lap + current_node%lap_change
               current_node => current_node%Forward(1)%ptr
            end do

            !=================================================================
            !  1) Sort the newly created Y‑intervals
            !  2) Merge any that overlap or touch
            !=================================================================
            if (n_curr > 1) call sort_and_merge_regions(curr_regions, n_curr)

            !-------------------------------------------------------------
            !  Compare with the previous sweep line and emit finished boxes
            !-------------------------------------------------------------
            p = 1
            c = 1
            do while (p <= n_prev .and. c <= n_curr)
               if (prev_regions(p)%y1 == curr_regions(c)%y1 .and. &
                  prev_regions(p)%y2 == curr_regions(c)%y2) then
                  ! same vertical span – continue the region
                  curr_regions(c)%x_start = prev_regions(p)%x_start
                  p = p + 1
                  c = c + 1
               else if (prev_regions(p)%y1 < curr_regions(c)%y1) then
                  if (current_x > prev_regions(p)%x_start) &
                     call emit_region(prev_regions(p), current_x)
                  p = p + 1
               else if (prev_regions(p)%y1 > curr_regions(c)%y1) then
                  c = c + 1
               else
                  if (current_x > prev_regions(p)%x_start) &
                     call emit_region(prev_regions(p), current_x)
                  p = p + 1
               end if
            end do

            do while (p <= n_prev)
               if (current_x > prev_regions(p)%x_start) &
                  call emit_region(prev_regions(p), current_x)
               p = p + 1
            end do

            !-------------------------------------------------------------
            !  Prepare for the next X‑slice
            !-------------------------------------------------------------
            if (n_curr > 0) then
               swap_ptr    => prev_regions
               prev_regions=> curr_regions
               curr_regions=> swap_ptr
            end if
            n_prev = n_curr
         end if
      end do

      call destroy_skiplist(active_edges)

      !-----------------------------------------------------------------
      !  Final output handling
      !-----------------------------------------------------------------
      !write(*,*) 'In PF (Fractured Count): ', output_count
      allocate(fractured_boxes(output_count))
      if (output_count > 0_int64) fractured_boxes = temp_boxes(1:output_count)

   contains

      !=====================================================================
      !  Emit a finished region as a box
      !=====================================================================
      subroutine emit_region(region, x_end)
         type(ActiveRegion), intent(in) :: region
         integer(kind=K_COORDINATE_KIND), intent(in) :: x_end

         output_count = output_count + 1_int64
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

      !=====================================================================
      !  Sort an array of ActiveRegion by y1 (ascending) and then merge
      !  any overlapping or touching intervals.
      !
      !  Arguments
      !    regs   – on‑entry: unsorted intervals, on‑exit: merged intervals
      !    nregs  – on‑entry: number of intervals, on‑exit: number after merge
      !=====================================================================
      subroutine sort_and_merge_regions(regs, nregs)
         type(ActiveRegion), intent(inout) :: regs(:)
         integer,            intent(inout) :: nregs
         integer                           :: i, j, n_merged
         type(ActiveRegion)   :: tmp

         !-------------------------------------------------------------
         !  Simple insertion sort – sufficient because nregs is usually
         !  small (order of a few hundred at most)
         !-------------------------------------------------------------
         do i = 2, nregs
            tmp = regs(i)
            j = i - 1
            do while (j >= 1 .and. (regs(j)%y1 > tmp%y1) )
               regs(j+1) = regs(j)
               j = j - 1
            end do
            regs(j+1) = tmp
         end do

         !-------------------------------------------------------------
         !  Merge overlapping / adjacent intervals.
         !-------------------------------------------------------------
         n_merged = 0
         do i = 1, nregs
            if (n_merged == 0) then
               n_merged = n_merged + 1
               regs(n_merged) = regs(i)
            else
               if (regs(i)%y1 <= regs(n_merged)%y2) then
                  ! Overlap – extend the current interval
                  regs(n_merged)%y2 = max(regs(n_merged)%y2, regs(i)%y2)
                  ! Preserve the earliest x_start (already stored)
               else
                  n_merged = n_merged + 1
                  regs(n_merged) = regs(i)
               end if
            end if
         end do

         nregs = n_merged
      end subroutine sort_and_merge_regions

   end subroutine scanline_fracture

   subroutine good_scanline_fracture(trackers, fractured_boxes)
      type(XYTracker), allocatable, intent(inout) :: trackers(:)
      type(Box), allocatable, intent(out) :: fractured_boxes(:)

      type(SkipList) :: active_edges
      type(SkipListNode), pointer :: current_node

      integer :: i, n
      integer(kind=K_COORDINATE_KIND) :: current_x, y_start, y_end
      integer(kind=int8)                 :: winding_sign
      integer(kind=int64) :: current_lap, max_sweep_capacity

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
      call PerformCancellation( trackers )
      call init_skiplist(active_edges, min(int(n, int64), 5000000_int64))

      max_boxes = max(100000_int64, int(n, int64) * 2_int64)
      allocate(temp_boxes(max_boxes))
      output_count = 0

      max_sweep_capacity = min(int(n, int64)/2_int64, 2000000_int64)
      allocate(region_bank_A(max_sweep_capacity), region_bank_B(max_sweep_capacity))
      prev_regions => region_bank_A
      curr_regions => region_bank_B
      n_prev = 0

      do i = 1, n
         winding_sign = sign(1_int8, trackers(i)%polygonNumber)
         call update_active_edges(active_edges, trackers(i)%Y, winding_sign)

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

            do while (associated(current_node))
               if (current_lap <= 0 .and. current_lap + current_node%lap_change > 0) then
                  y_start = current_node%y_val
               else if (current_lap > 0 .and. current_lap + current_node%lap_change <= 0) then
                  y_end = current_node%y_val

                  n_curr = n_curr + 1
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
         if( x_end == region%x_start ) return
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
         if( .not. temp_boxes(output_count)%is_valid() ) error stop "PUTTING INVALID BOX IN REGION"
      end subroutine emit_region
   end subroutine good_scanline_fracture

   subroutine generate_trackers(boxes, bbox, trackers)
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
         !min_x = min(boxes(i)%x1, boxes(i)%x2)
         !max_x = max(boxes(i)%x1, boxes(i)%x2)
         !min_y = min(boxes(i)%y1, boxes(i)%y2)
         !max_y = max(boxes(i)%y1, boxes(i)%y2)
         min_x = boxes(i)%x1
         min_y = boxes(i)%y1
         max_x = boxes(i)%x2
         max_y = boxes(i)%y2
         trackers(idx + 1) = XYTracker(X = min_x, Y = min_y, polygonNumber = -1) !> this can be used as i as well
         trackers(idx + 2) = XYTracker(X = min_x, Y = max_y, polygonNumber =  1)
         trackers(idx + 3) = XYTracker(X = max_x, Y = min_y, polygonNumber =  1)
         trackers(idx + 4) = XYTracker(X = max_x, Y = max_y, polygonNumber = -1)
         !write(*,*) 'At idx ', idx+1, ' added ', trackers(idx+1:idx+4)
      end do
   end subroutine generate_trackers

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
      integer(kind=int8)                 :: winding_sign
      type(ActiveRegion), target, allocatable :: region_bank_A(:), region_bank_B(:), temp_regions(:)
      type(ActiveRegion), pointer :: prev_regions(:), curr_regions(:), swap_ptr(:)
      integer :: n_prev, n_curr, p, c

      type(Box), allocatable :: temp_boxes(:), resized_boxes(:)
      integer(kind=int64) :: max_boxes, max_sweep_capacity
      logical :: process_x_slice
      n = input_box_count
      if (n == 0) then
         output_box_count = 0
         return
      end if
      if (n == 1) then
         output_box_count = 1
         return
      end if

      allocate(events(4 * n))
      do i = 1, n
         min_x = min(boxes(i)%x1, boxes(i)%x2)
         max_x = max(boxes(i)%x1, boxes(i)%x2)
         min_y = min(boxes(i)%y1, boxes(i)%y2)
         max_y = max(boxes(i)%y1, boxes(i)%y2)

         events(4*i - 3) = XYTracker(X = min_x, Y = min_y, polygonNumber =  1_int64)
         events(4*i - 2) = XYTracker(X = min_x, Y = max_y, polygonNumber = -1_int64)
         events(4*i - 1) = XYTracker(X = max_x, Y = min_y, polygonNumber = -1_int64)
         events(4*i)     = XYTracker(X = max_x, Y = max_y, polygonNumber =  1_int64)
      end do

      call sort_trackers(events)
      call init_skiplist(active_edges, min(n * 4_int64, 5000000_int64))

      max_boxes = max(100000_int64, n)
      allocate(temp_boxes(max_boxes))
      output_box_count = 0

      max_sweep_capacity = min(n/2_int64 + 1_int64, 2000000_int64)
      allocate(region_bank_A(max_sweep_capacity), region_bank_B(max_sweep_capacity))
      prev_regions => region_bank_A
      curr_regions => region_bank_B
      n_prev = 0

      do i = 1, 4*n
         winding_sign = sign(1_int8, events(i)%polygonNumber)
         call update_active_edges(active_edges, events(i)%Y, winding_sign)

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

            do while (associated(current_node))
               if (current_lap <= 0 .and. current_lap + current_node%lap_change > 0) then
                  y_start = current_node%y_val
               else if (current_lap > 0 .and. current_lap + current_node%lap_change <= 0) then
                  y_end = current_node%y_val
                  n_curr = n_curr + 1

                  if (n_curr > max_sweep_capacity) then
                     max_sweep_capacity = max_sweep_capacity * 2_int64

                     if (associated(curr_regions, region_bank_A)) then
                        ! curr_regions is looking at A, prev_regions is looking at B
                        allocate(temp_regions(max_sweep_capacity))
                        temp_regions(1:n_curr-1) = curr_regions(1:n_curr-1)
                        call move_alloc(from=temp_regions, to=region_bank_A)
                        curr_regions => region_bank_A

                        allocate(temp_regions(max_sweep_capacity))
                        if (n_prev > 0) temp_regions(1:n_prev) = prev_regions(1:n_prev)
                        call move_alloc(from=temp_regions, to=region_bank_B)
                        prev_regions => region_bank_B
                     else
                        ! curr_regions is looking at B, prev_regions is looking at A
                        allocate(temp_regions(max_sweep_capacity))
                        temp_regions(1:n_curr-1) = curr_regions(1:n_curr-1)
                        call move_alloc(from=temp_regions, to=region_bank_B)
                        curr_regions => region_bank_B

                        allocate(temp_regions(max_sweep_capacity))
                        if (n_prev > 0) temp_regions(1:n_prev) = prev_regions(1:n_prev)
                        call move_alloc(from=temp_regions, to=region_bank_A)
                        prev_regions => region_bank_A
                     end if
                  end if
#ifdef OLD_CODE
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
#endif
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

            swap_ptr => prev_regions
            prev_regions => curr_regions
            curr_regions => swap_ptr
            n_prev = n_curr
         end if
      end do

      call destroy_skiplist(active_edges)

      if (output_box_count > 0 .and. output_box_count < size(boxes)) then
         block
            type(Box), allocatable :: temp(:)
            temp = temp_boxes(1:output_box_count)
            boxes = temp
         end block ! Temp is automatically cleaned up here
         !boxes(1:output_box_count) = temp_boxes(1:output_box_count)
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

      allocate(events(2*n))
      do i = 1, n
         events(2*i - 1)%x          = min(boxes(i)%x1, boxes(i)%x2)
         events(2*i - 1)%y1         = min(boxes(i)%y1, boxes(i)%y2)
         events(2*i - 1)%y2         = max(boxes(i)%y1, boxes(i)%y2)
         events(2*i - 1)%lap_change = 1_int64

         events(2*i)%x          = max(boxes(i)%x1, boxes(i)%x2)
         events(2*i)%y1         = min(boxes(i)%y1, boxes(i)%y2)
         events(2*i)%y2         = max(boxes(i)%y1, boxes(i)%y2)
         events(2*i)%lap_change = -1_int64
      end do

      call sort_events(events)
      call init_skiplist(sl, int(2*n, int64))

      current_x = events(1)%x
      ev_idx = 1

      do while (ev_idx <= 2*n)
         dx = events(ev_idx)%x - current_x

         if (dx > 0) then
            call get_covered_y(sl, current_covered)
            area = area + real(dx, real64) * real(current_covered, real64)
            current_x = events(ev_idx)%x
         end if

         do while (ev_idx <= 2*n)
            if (events(ev_idx)%x /= current_x) exit

            call update_active_edges(sl, events(ev_idx)%y1,  events(ev_idx)%lap_change)
            call update_active_edges(sl, events(ev_idx)%y2, -events(ev_idx)%lap_change)

            ev_idx = ev_idx + 1
         end do
      end do

      call destroy_skiplist(sl)
   end function calculate_union_area_sl
   !=================================================================
   !  Subroutine: PerformCancellation
   !  Purpose   : Remove pairs of vertices that are identical (X,Y)
   !              but have opposite polygonNumber values.
   !
   !  Argument  : arr – allocatable array of XYTracker, sorted, will
   !              be overwritten with the compacted list.
   !=================================================================
   subroutine PerformCancellation(arr)
      implicit none
      type(XYTracker), allocatable, intent(inout) :: arr(:)

      ! Local data
      type(XYTracker), allocatable :: work(:)   ! temporary workspace
      integer(kind=int64)                      :: n      ! original size
      integer(kind=int64)                      :: write_index  ! next free slot in work
      integer(kind=int64)                      :: i, improved

      !----------------------------------------------------------------
      !  Guard against empty or not‑allocated input
      !----------------------------------------------------------------
      if (.not.allocated(arr)) return
      n = size(arr)
      if (n == 0) return

      !----------------------------------------------------------------
      !  Allocate a temporary buffer the same size as the input.
      !  We will copy only the survivors into it.
      !----------------------------------------------------------------
      allocate(work(n))

      write_index = 0                       ! no survivors yet

      do i = 1, n
         if (write_index > 0) then
            ! Compare current token with the last survivor that is still
            ! in the buffer.  If they are on the same point and have
            ! opposite polygon numbers, they cancel each other.
            if ( work(write_index)%X == arr(i)%X .and. &
               work(write_index)%Y == arr(i)%Y .and. &
               work(write_index)%polygonNumber == -arr(i)%polygonNumber ) then
               ! ----- CANCEL -----
               write_index = write_index - 1        ! discard the previous survivor
               cycle                    ! and discard the current one
            end if
         end if

         ! No cancellation → keep the current token
         write_index = write_index + 1
         work(write_index) = arr(i)
      end do
      write(*,*) 'Orign = ',n,' after cancellation: ', write_index
      !----------------------------------------------------------------
      !  Re‑size the original array to the number of survivors.
      !  If everything cancelled, we end up with a zero‑length array.
      !----------------------------------------------------------------
      deallocate(arr)                 ! free the original storage
      allocate(arr(write_index))            ! allocate just enough space
      if (write_index > 0) arr = work(1:write_index)

      deallocate(work)                ! tidy up temporary buffer
   end subroutine PerformCancellation

end module PolygonFractureModule
