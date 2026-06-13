! File   : boxmerge.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: We keep coming back to this scanline vs RTree debate
!Given an array of type(box) in modern Fortran with
!integer coordinates we want to remove coliear segments
!while preserving rectangular shapes. Given a r tree of
!such boxes how can I merge adjoining rectangles which
!touch or overlap to form a bigger rectangle in modern
!Fortran. Is rtree better of scanline for parallelism and
!we have 500 million boxes. Generate modern Fortran code
!to solve this along with test program and extensive
!documentation and please double check your work with
!highest effort

module BoxMergeModule
  use, intrinsic :: iso_fortran_env, only: int32, int64
  use CommonModule
  use GeometryModule
  implicit none
  private
  public :: merge_boxes_using_scanline, merge_all_boxes, print_ScanBoxes, ScanBox

  ! Define the ScanBox data type
  type :: ScanBox
     type(Box) :: pbox
     logical   :: active = .true. !> this is important
  end type ScanBox

  type :: SkipListNode
     integer(kind=K_COORDINATE_KIND) :: x1, y1, y2, x2
     integer(kind=int64)             :: src_idx    ! << Maps mutation back to source array
     type(SkipListPointer), allocatable :: forward(:)
  end type SkipListNode

  type :: SkipListPointer
     type(SkipListNode), pointer :: ptr => null()
  end type SkipListPointer

  type :: SkipList
     type(SkipListNode), pointer :: header => null()
     integer :: max_level
  end type SkipList

contains
  ! Initialize the list with a sentinel header
  subroutine init_skiplist(sl)
    type(SkipList), intent(out) :: sl
    integer :: i
    allocate(sl%header)
    sl%max_level = 32 ! Sufficient for 366M boxes (log2(N) ~ 28, but 16-20 is fine)
    allocate(sl%header%forward(sl%max_level))
    do i = 1, sl%max_level
       sl%header%forward(i)%ptr => null()
    end do
  end subroutine init_skiplist
  subroutine destroy_skiplist(sl)
    type(SkipList), intent(inout) :: sl
    type(SkipListNode), pointer :: curr, temp

    ! 1. Traverse the list at level 1 and free every node
    curr => sl%header%forward(1)%ptr
    do while (associated(curr))
       temp => curr
       curr => curr%forward(1)%ptr

       ! Free the forward pointer array of the node
       if (allocated(temp%forward)) deallocate(temp%forward)
       ! Free the node itself
       deallocate(temp)
    end do

    ! 2. Free the header
    if (allocated(sl%header%forward)) deallocate(sl%header%forward)
    deallocate(sl%header)
    nullify(sl%header)
  end subroutine destroy_skiplist
  ! 2. SAFE DELETE FUNCTION (Using native coordinates)
  subroutine delete_node(sl, target_x1, target_x2, target_y1, target_y2)
    type(SkipList), intent(inout) :: sl
    integer(kind=K_COORDINATE_KIND), intent(in) :: target_x1, target_x2, target_y1, target_y2
    type(SkipListNode), pointer :: curr, temp
    type(SkipListPointer) :: update(sl%max_level)
    integer :: i

    curr => sl%header
    do i = sl%max_level, 1, -1
       do while (associated(curr%forward(i)%ptr))
          if (curr%forward(i)%ptr%y1 < target_y1) then
             curr => curr%forward(i)%ptr
          else
             exit
          end if
       end do
       update(i)%ptr => curr
    end do

    curr => update(1)%ptr%forward(1)%ptr

    do while (associated(curr))
       if (curr%y1 > target_y1) exit

       ! Strict topological match
       if (curr%x1 == target_x1 .and. curr%x2 == target_x2 .and. &
            curr%y1 == target_y1 .and. curr%y2 == target_y2) then

          do i = 1, sl%max_level
             if (.not. associated(update(i)%ptr%forward(i)%ptr, curr)) exit
             update(i)%ptr%forward(i)%ptr => curr%forward(i)%ptr
          end do

          if (allocated(curr%forward)) deallocate(curr%forward)
          deallocate(curr)
          return
       end if
       curr => curr%forward(1)%ptr
    end do
  end subroutine delete_node
  ! 3. DUAL-SCAN OVERLAP RESOLUTION
  function find_merge_candidate(sl, x1, x2, y1, y2) result(node)
    type(SkipList), intent(in) :: sl
    integer(kind=K_COORDINATE_KIND), intent(in) :: x1, x2, y1, y2
    type(SkipListNode), pointer :: node, curr

    ! Scan the active level 1 list directly
    curr => sl%header%forward(1)%ptr
    do while (associated(curr))

       ! CONDITION 1: Horizontal Touch or Overlap
       ! Exact same Y-span, and incoming X1 is within or touches the active X2
       if (curr%y1 == y1 .and. curr%y2 == y2 .and. x1 <= curr%x2) then
          node => curr
          return
       end if

       ! CONDITION 2: Vertical Touch or Overlap
       ! Exact same X-span, and incoming Y1 is within or touches the active Y2
       if (curr%x1 == x1 .and. curr%x2 == x2 .and. y1 <= curr%y2) then
          node => curr
          return
       end if

       ! EARLY EXIT GUARANTEE:
       ! Since SkipList is ordered by y1, if curr%y1 > incoming y1, 
       ! no future nodes can possibly satisfy the conditions above.
       if (curr%y1 > y1) then
          exit
       end if

       curr => curr%forward(1)%ptr
    end do
    node => null()
  end function find_merge_candidate
  ! Find a box that matches the Y-interval [y1, y2]
  function find_matching_y_interval(sl, y1, y2) result(node)
    type(SkipList), intent(in) :: sl
    integer(kind=K_COORDINATE_KIND), intent(in) :: y1, y2
    type(SkipListNode), pointer :: node, curr
    integer :: i

    curr => sl%header
    do i = sl%max_level, 1, -1
       do while (associated(curr%forward(i)%ptr))
          if (curr%forward(i)%ptr%y1 < y1) then
             curr => curr%forward(i)%ptr
          else
             exit
          end if
       end do
    end do

    ! Scan horizontally through nodes that share the exact same y1
    node => curr%forward(1)%ptr
    do while (associated(node))
       if (node%y1 == y1 .and. node%y2 == y2) then
          return ! Exact match found
       else if (node%y1 > y1) then
          node => null() ! We have passed the valid Y-range
          return
       else
          node => node%forward(1)%ptr ! Keep checking next node
       end if
    end do
  end function find_matching_y_interval

  function random_level(sl) result(lvl)
    type(SkipList), intent(in) :: sl
    integer :: lvl
    real :: r
    lvl = 1
    call random_number(r)
    do while (r < 0.5 .and. lvl < sl%max_level)
       lvl = lvl + 1
       call random_number(r)
    end do
  end function random_level

  ! 4. MAPPED INSERTION (Including X1)
  subroutine insert_skiplist_node(sl, box_in, idx)
    type(SkipList), intent(inout) :: sl
    type(ScanBox), intent(in) :: box_in
    integer(kind=int64), intent(in) :: idx
    type(SkipListNode), pointer :: new_node, curr
    type(SkipListPointer) :: update(sl%max_level)
    integer :: i, lvl

    if (.not. associated(sl%header)) then
       print *, "CRITICAL: Header not associated!"
       error stop
    end if

    curr => sl%header
    do i = sl%max_level, 1, -1
       do while (associated(curr%forward(i)%ptr))
          if (curr%forward(i)%ptr%y1 < box_in%pbox%y1) then
             curr => curr%forward(i)%ptr
          else
             exit
          end if
       end do
       update(i)%ptr => curr
    end do

    lvl = random_level(sl)
    allocate(new_node)

    new_node%src_idx = idx
    new_node%x1 = box_in%pbox%x1
    new_node%x2 = box_in%pbox%x2
    new_node%y1 = box_in%pbox%y1
    new_node%y2 = box_in%pbox%y2

    allocate(new_node%forward(lvl))
    do i = 1, lvl
       new_node%forward(i)%ptr => update(i)%ptr%forward(i)%ptr
       update(i)%ptr%forward(i)%ptr => new_node
    end do
  end subroutine insert_skiplist_node

  subroutine prune_inactive_slabs(sl, current_x)
    type(SkipList), intent(inout) :: sl
    integer(kind=K_COORDINATE_KIND), intent(in) :: current_x
    type(SkipListNode), pointer :: curr, prev, temp
    integer :: i

    ! CRITICAL FIX: Must iterate TOP-DOWN to avoid Use-After-Free segfaults
    do i = sl%max_level, 1, -1
       curr => sl%header%forward(i)%ptr
       prev => sl%header

       do while (associated(curr))
          if (curr%x2 < current_x) then
             ! Bypass curr: prev now points to what curr pointed to
             prev%forward(i)%ptr => curr%forward(i)%ptr

             ! Only free the node when we reach the absolute bottom level.
             ! By now, it has been cleanly removed from all higher levels.
             if (i == 1) then
                temp => curr
                curr => prev%forward(i)%ptr ! Move curr before freeing temp

                if (allocated(temp%forward)) deallocate(temp%forward)
                deallocate(temp)
             else
                curr => prev%forward(i)%ptr
             end if
          else
             prev => curr
             curr => curr%forward(i)%ptr
          end if
       end do
    end do
  end subroutine prune_inactive_slabs
  subroutine merge_boxes_using_scanline(boxes)
    type(Box), allocatable, intent(inout) :: boxes(:)
    type(ScanBox), allocatable :: ScanBoxes(:)
    type(Box), allocatable :: temp_boxes(:)
    integer(kind=int64)    :: i, write_idx
    if( size(boxes) == 0 ) then
       return
       error stop "ZERO INPUT"
    end if
    allocate(ScanBoxes(size(boxes)))
    !$KOMP PARALLEL DO
    do i = 1, size(boxes)
       ScanBoxes(i)%pbox = boxes(i)
       ScanBoxes(i)%active = .true.
    end do
    !$KOMP END PARALLEL DO
    !ScanBoxes = [ (ScanBox( boxes(i) ), i = 1, size(boxes)) ]
    !call merge_all_boxes( ScanBoxes )
    call merge_all_boxes_skiplist( ScanBoxes )
    !temp_boxes = pack( ScanBoxes%pbox, mask=ScanBoxes%active ) !>> This is why Fortran is used <<!
    ! --- In-Place Compaction (Replaces PACK) ---
    allocate(temp_boxes(count(ScanBoxes%active)))
    write(*,*) 'Allocated : ', size( temp_boxes)    
    write_idx = 1
    do i = 1, size(ScanBoxes)
       if (ScanBoxes(i)%active) then
          temp_boxes(write_idx) = ScanBoxes(i)%pbox
          write_idx = write_idx + 1
       end if
    end do
    call move_alloc(from=temp_boxes, to=boxes)
  end subroutine merge_boxes_using_scanline

  !> Main subroutine to scan and merge all perfectly rectangular overlaps
  subroutine merge_all_boxes(ScanBoxes)
    type(ScanBox), allocatable,intent(inout) :: ScanBoxes(:)    
    integer(int32)           :: i, j, n, merges_this_pass
    type(ScanBox)            :: merged_ScanBox
    n = size(ScanBoxes)
    ! 1. Sort ScanBoxes by xmin, then ymin for the sweep-line
    !call quicksort_ScanBoxes(ScanBoxes, 1, n)
    call sort_ScanBoxes( ScanBoxes, 1, n )
    do
       merges_this_pass = 0
       ! 2. Sweep-line phase
       do i = 1, n
          if (.not. ScanBoxes(i)%active) cycle
          ! Look ahead in the sorted array
          do j = i + 1, n
             if (.not. ScanBoxes(j)%active) cycle
             ! SCANLINE OPTIMIZATION:
             ! Since array is sorted by xmin, if ScanBox(j) is entirely to the 
             ! right of ScanBox(i), no subsequent ScanBoxes can intersect ScanBox(i).
             if (ScanBoxes(j)%pbox%x1 > ScanBoxes(i)%pbox%x2) exit
             ! Check if they can be merged into a perfect rectangle
             if (can_merge(ScanBoxes(i), ScanBoxes(j), merged_ScanBox)) then
                if (box_area(merged_ScanBox%pbox) > (box_area(ScanBoxes(i)%pbox)+box_area(ScanBoxes(j)%pbox))) then
                   error stop "INCORRECT AREA CALCULATION"
                   cycle 
                end if
                ScanBoxes(i) = merged_ScanBox       ! Update current ScanBox
                ScanBoxes(j)%active = .false.   ! Deactivate merged ScanBox
                merges_this_pass = merges_this_pass + 1
             end if
          end do
       end do
       ! If no merges happened in this pass, the geometry is fully simplified
       if (merges_this_pass == 0) exit
    end do
  end subroutine merge_all_boxes

  !> Checks if two ScanBoxes touch/overlap and form exactly a larger rectangle
  function can_merge(a, b, res) result(mergeable)
    type(ScanBox), intent(in)  :: a, b
    type(ScanBox), intent(out) :: res
    logical                :: mergeable
    integer(int64)         :: area_a, area_b, area_intersect, area_bound
    integer(kind=K_COORDINATE_KIND)         :: ixmin, iymin, ixmax, iymax

    mergeable = .false.

    ! 1. Calculate Intersection Coordinates
    ixmin = max(a%pbox%x1, b%pbox%x1)
    iymin = max(a%pbox%y1, b%pbox%y1)
    ixmax = min(a%pbox%x2, b%pbox%x2)
    iymax = min(a%pbox%y2, b%pbox%y2)
    ! 1. PRE-CHECK: Confirm absolute non-overlap
    ! If they overlap, this function returns .false. immediately,
    ! effectively forcing your algorithm to handle the overlap as an error.
    ixmin = max(a%pbox%x1, b%pbox%x1)
    iymin = max(a%pbox%y1, b%pbox%y1)
    ixmax = min(a%pbox%x2, b%pbox%x2)
    iymax = min(a%pbox%y2, b%pbox%y2)

    ! If they have any positive intersection area, they are overlapping
    if (ixmin < ixmax .and. iymin < iymax) then
       error stop "THIS CODE ASSUMES NON-OVLP"
       return
    end if
    ! 2. Calculate Intersection Area
    ! If ixmin < ixmax and iymin < iymax, they overlap.
    ! If they are exactly equal on one axis, they touch (collinear boundary).
    if (ixmin < ixmax .and. iymin < iymax) then
       area_intersect = int(ixmax - ixmin, int64) * int(iymax - iymin, int64)
    else if (ixmin == ixmax .and. iymin <= iymax .or. &
         iymin == iymax .and. ixmin <= ixmax) then
       area_intersect = 0_int64 ! Touching borders
    else
       return ! Strictly disjoint, cannot merge
    end if

    ! 3. Calculate Overall Bounding ScanBox
    res%pbox%x1   = min(a%pbox%x1, b%pbox%x1)
    res%pbox%y1   = min(a%pbox%y1, b%pbox%y1)
    res%pbox%x2   = max(a%pbox%x2, b%pbox%x2)
    res%pbox%y2   = max(a%pbox%y2, b%pbox%y2)
    res%active = .true.

    ! 4. Area Check to preserve strictly rectangular shapes
    area_bound = int(res%pbox%x2 - res%pbox%x1, int64) * int(res%pbox%y2 - res%pbox%y1, int64)
    area_a     = int(a%pbox%x2 - a%pbox%x1, int64) * int(a%pbox%y2 - a%pbox%y1, int64)
    area_b     = int(b%pbox%x2 - b%pbox%x1, int64) * int(b%pbox%y2 - b%pbox%y1, int64)

    if (area_bound == area_a + area_b - area_intersect) then
       mergeable = .true.
    end if
  end function can_merge

  recursive subroutine sort_ScanBoxes(a, first, last)
    type(ScanBox), intent(inout) :: a(:)
    integer(int32), intent(in) :: first, last
    integer(int32), parameter  :: K_THRESHOLD = 64
    integer(int32) :: i, j
    type(ScanBox) :: temp, pivot

    ! Base case: Use insertion sort for small arrays
    if (last - first < K_THRESHOLD) then
       call insertion_sort_ScanBoxes(a, first, last)
       return
    end if

    ! Standard Quicksort partition logic
    pivot = a((first + last) / 2)
    i = first
    j = last
    do while (i <= j)
       do while (a(i)%pbox%x1 < pivot%pbox%x1 .or. &
            (a(i)%pbox%x1 == pivot%pbox%x1 .and. a(i)%pbox%y1 < pivot%pbox%y1))
          i = i + 1
       end do
       do while (a(j)%pbox%x1 > pivot%pbox%x1 .or. &
            (a(j)%pbox%x1 == pivot%pbox%x1 .and. a(j)%pbox%y1 > pivot%pbox%y1))
          j = j - 1
       end do
       if (i <= j) then
          temp = a(i); a(i) = a(j); a(j) = temp
          i = i + 1; j = j - 1
       end if
    end do

    if (first < j) call sort_ScanBoxes(a, first, j)
    if (i < last) call sort_ScanBoxes(a, i, last)
  end subroutine sort_ScanBoxes

  subroutine insertion_sort_ScanBoxes(a, first, last)
    type(ScanBox), intent(inout) :: a(:)
    integer(int32), intent(in) :: first, last
    integer(int32) :: i, j
    type(ScanBox) :: key

    do i = first + 1, last
       key = a(i)
       j = i - 1
       do while (j >= first .and. (a(j)%pbox%x1 > key%pbox%x1 .or. &
            (a(j)%pbox%x1 == key%pbox%x1 .and. a(j)%pbox%y1 > key%pbox%y1)))
          a(j + 1) = a(j)
          j = j - 1
       end do
       a(j + 1) = key
    end do
  end subroutine insertion_sort_ScanBoxes

  !> Fast recursive QuickSort optimized for Type(ScanBox)
  recursive subroutine quicksort_ScanBoxes(a, first, last)
    type(ScanBox), intent(inout) :: a(:)
    integer(int32), intent(in) :: first, last
    integer(int32) :: i, j
    type(ScanBox) :: temp, pivot
    if (first >= last) return
    pivot = a((first + last) / 2)
    i = first
    j = last
    do while (i <= j)
       ! Sort by xmin primarily, then ymin
       do while (a(i)%pbox%x1 < pivot%pbox%x1 .or. &
            (a(i)%pbox%x1 == pivot%pbox%x1 .and. a(i)%pbox%y1 < pivot%pbox%y1))
          i = i + 1
       end do
       do while (a(j)%pbox%x1 > pivot%pbox%x1 .or. &
            (a(j)%pbox%x1 == pivot%pbox%x1 .and. a(j)%pbox%y1 > pivot%pbox%y1))
          j = j - 1
       end do
       if (i <= j) then
          temp = a(i)
          a(i) = a(j)
          a(j) = temp
          i = i + 1
          j = j - 1
       end if
    end do
    if (first < j) call quicksort_ScanBoxes(a, first, j)
    if (i < last) call quicksort_ScanBoxes(a, i, last)
  end subroutine quicksort_ScanBoxes

  !> Helper utility to print active ScanBoxes
  subroutine print_ScanBoxes(ScanBoxes, title)
    type(ScanBox), intent(in) :: ScanBoxes(:)
    character(len=*), intent(in) :: title
    integer(int32) :: i, count

    print *, "--- ", trim(title), " ---"
    count = 0
    do i = 1, size(ScanBoxes)
       if (ScanBoxes(i)%active) then
          count = count + 1
          print "(A,I3,A,I5,A,I5,A,I5,A,I5,A)",  &
               " [", ScanBoxes(i)%pbox%x1, ",", ScanBoxes(i)%pbox%y1, " -> ", &
               ScanBoxes(i)%pbox%x2, ",", ScanBoxes(i)%pbox%y2, "]"
       end if
    end do
    print *, "Total Active ScanBoxes: ", count
    print *, ""
  end subroutine print_ScanBoxes
  ! 5. 2D LIVE-MAPPED MERGE LOOP
  subroutine merge_all_boxes_skiplist(ScanBoxes)
    type(ScanBox), intent(inout) :: ScanBoxes(:)
    type(SkipList)               :: active_slabs
    type(SkipListNode), pointer  :: current_node
    integer(int64)               :: i

    call sort_ScanBoxes(ScanBoxes, 1, size(ScanBoxes))
    call init_skiplist(active_slabs)

    do i = 1, size(ScanBoxes)

       current_node => find_merge_candidate(active_slabs, &
            ScanBoxes(i)%pbox%x1, ScanBoxes(i)%pbox%x2, &
            ScanBoxes(i)%pbox%y1, ScanBoxes(i)%pbox%y2)

       if (associated(current_node)) then
          ! 1. Extend whichever boundary matched (Horizontal or Vertical)
          current_node%x2 = max(current_node%x2, ScanBoxes(i)%pbox%x2)
          current_node%y2 = max(current_node%y2, ScanBoxes(i)%pbox%y2)

          ! 2. Map mutation back to original array
          ScanBoxes(current_node%src_idx)%pbox%x2 = current_node%x2
          ScanBoxes(current_node%src_idx)%pbox%y2 = current_node%y2

          ScanBoxes(i)%active = .false. 
       else
          call insert_skiplist_node(active_slabs, ScanBoxes(i), i)
       end if

       call prune_inactive_slabs(active_slabs, ScanBoxes(i)%pbox%x1)
    end do

    call destroy_skiplist(active_slabs)
  end subroutine merge_all_boxes_skiplist

end module BoxMergeModule


