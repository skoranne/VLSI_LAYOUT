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
      logical   :: active = .true. 
  end type ScanBox

  ! 1. ARENA-OPTIMIZED NODE SCHEMA
  ! Forward array is now fixed-size to prevent internal heap fragmentation.
  ! next_free pointer added to maintain the LIFO Free List.
  type :: SkipListNode
      integer(kind=K_COORDINATE_KIND) :: x1, y1, y2, x2
      integer(kind=int64)             :: src_idx    
      type(SkipListPointer)           :: forward(32) 
      type(SkipListPointer)           :: next_free   
  end type SkipListNode

  type :: SkipListPointer
      type(SkipListNode), pointer :: ptr => null()
  end type SkipListPointer

  ! SkipList now contains the contiguous Arena and the Free List anchor
  type :: SkipList
      type(SkipListNode), pointer :: header => null()
      integer :: max_level
      type(SkipListNode), allocatable, target :: arena(:)
      type(SkipListPointer) :: free_head
  end type SkipList

contains

  ! =========================================================================
  ! LIFO MEMORY POOL MANAGEMENT
  ! =========================================================================

  subroutine init_skiplist(sl, total_boxes)
    type(SkipList), intent(out) :: sl
    integer(int64), intent(in)  :: total_boxes
    integer(int64) :: pool_size, i
    integer :: j

    sl%max_level = 32 

    ! Arena Heuristic: Maximum active scanline elements rarely exceed 5% of N.
    ! We guarantee at least 1,000,000 nodes for safety.
    pool_size = max(1000000_int64, total_boxes / 20_int64)
    allocate(sl%arena(pool_size))

    ! Initialize the LIFO Free List chain
    do i = 1, pool_size - 1
       sl%arena(i)%next_free%ptr => sl%arena(i+1)
       do j = 1, sl%max_level
          sl%arena(i)%forward(j)%ptr => null()
       end do
    end do
    sl%arena(pool_size)%next_free%ptr => null()
    do j = 1, sl%max_level
       sl%arena(pool_size)%forward(j)%ptr => null()
    end do

    sl%free_head%ptr => sl%arena(1)

    ! Allocate Header from the pool
    sl%header => get_free_node(sl)
  end subroutine init_skiplist

  function get_free_node(sl) result(node)
    type(SkipList), intent(inout) :: sl
    type(SkipListNode), pointer :: node
    
    if (.not. associated(sl%free_head%ptr)) then
       error stop "CRITICAL: SkipList LIFO Arena Exhausted. Increase pool_size."
    end if
    
    ! Pop from head
    node => sl%free_head%ptr
    sl%free_head%ptr => node%next_free%ptr
    node%next_free%ptr => null()
  end function get_free_node

  subroutine release_node(sl, node)
    type(SkipList), intent(inout) :: sl
    type(SkipListNode), pointer :: node
    integer :: i
    
    ! Nullify data to prevent ghost pointers
    do i = 1, sl%max_level
       node%forward(i)%ptr => null()
    end do
    
    ! Push to LIFO head
    node%next_free%ptr => sl%free_head%ptr
    sl%free_head%ptr => node
  end subroutine release_node

  subroutine destroy_skiplist(sl)
    type(SkipList), intent(inout) :: sl
    ! Because of the arena, destruction is simply one deallocation.
    ! Fortran immediately frees the entire contiguous block.
    if (allocated(sl%arena)) deallocate(sl%arena)
    nullify(sl%header)
    nullify(sl%free_head%ptr)
  end subroutine destroy_skiplist


  ! =========================================================================
  ! SKIPLIST CORE LOGIC
  ! =========================================================================

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

  subroutine insert_skiplist_node(sl, box_in, idx)
    type(SkipList), intent(inout) :: sl
    type(ScanBox), intent(in) :: box_in
    integer(kind=int64), intent(in) :: idx
    type(SkipListNode), pointer :: new_node, curr
    type(SkipListPointer) :: update(sl%max_level)
    integer :: i, lvl

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
    
    ! Pull node from LIFO Arena instead of OS Heap
    new_node => get_free_node(sl)

    new_node%src_idx = idx
    new_node%x1 = box_in%pbox%x1
    new_node%x2 = box_in%pbox%x2
    new_node%y1 = box_in%pbox%y1
    new_node%y2 = box_in%pbox%y2

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

    do i = sl%max_level, 1, -1
       curr => sl%header%forward(i)%ptr
       prev => sl%header

       do while (associated(curr))
          if (curr%x2 < current_x) then
             ! Bypass curr
             prev%forward(i)%ptr => curr%forward(i)%ptr

             if (i == 1) then
                temp => curr
                curr => prev%forward(i)%ptr 
                ! Release back to LIFO Arena
                call release_node(sl, temp)
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

  function find_merge_candidate(sl, x1, x2, y1, y2) result(node)
    type(SkipList), intent(in) :: sl
    integer(kind=K_COORDINATE_KIND), intent(in) :: x1, x2, y1, y2
    type(SkipListNode), pointer :: node, curr

    curr => sl%header%forward(1)%ptr
    do while (associated(curr))
       
       ! CONDITION 1: Horizontal Touch or Overlap
       if (curr%y1 == y1 .and. curr%y2 == y2 .and. x1 <= curr%x2) then
          node => curr
          return
       end if

       ! CONDITION 2: Vertical Touch or Overlap
       if (curr%x1 == x1 .and. curr%x2 == x2 .and. y1 <= curr%y2) then
          node => curr
          return
       end if

       ! EARLY EXIT GUARANTEE
       if (curr%y1 > y1) exit

       curr => curr%forward(1)%ptr
    end do
    node => null()
  end function find_merge_candidate


  ! =========================================================================
  ! MAIN API EXECUTIONS
  ! =========================================================================

  subroutine merge_boxes_using_scanline(boxes)
    type(Box), allocatable, intent(inout) :: boxes(:)
    type(ScanBox), allocatable :: ScanBoxes(:)
    type(Box), allocatable :: temp_boxes(:)
    integer(kind=int64)    :: i, write_idx
    
    if( size(boxes) == 0 ) return

    allocate(ScanBoxes(size(boxes)))
    !$OMP PARALLEL DO
    do i = 1, size(boxes)
       ScanBoxes(i)%pbox = boxes(i)
       ScanBoxes(i)%active = .true.
    end do
    !$OMP END PARALLEL DO

    call merge_all_boxes_skiplist( ScanBoxes )

    ! --- In-Place Compaction ---
    allocate(temp_boxes(count(ScanBoxes%active)))
    write_idx = 1
    do i = 1, size(ScanBoxes)
       if (ScanBoxes(i)%active) then
          temp_boxes(write_idx) = ScanBoxes(i)%pbox
          write_idx = write_idx + 1
       end if
    end do
    call move_alloc(from=temp_boxes, to=boxes)
  end subroutine merge_boxes_using_scanline

  subroutine merge_all_boxes_skiplist(ScanBoxes)
    type(ScanBox), intent(inout) :: ScanBoxes(:)
    type(SkipList)               :: active_slabs
    type(SkipListNode), pointer  :: current_node
    integer(int64)               :: i, n

    n = size(ScanBoxes)
    call sort_ScanBoxes(ScanBoxes, 1, n)
    call init_skiplist(active_slabs, n)

    do i = 1, n
       
       current_node => find_merge_candidate(active_slabs, &
            ScanBoxes(i)%pbox%x1, ScanBoxes(i)%pbox%x2, &
            ScanBoxes(i)%pbox%y1, ScanBoxes(i)%pbox%y2)

       if (associated(current_node)) then
          ! 1. Extend active boundary
          current_node%x2 = max(current_node%x2, ScanBoxes(i)%pbox%x2)
          current_node%y2 = max(current_node%y2, ScanBoxes(i)%pbox%y2)

          ! 2. Project to array
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

  ! =========================================================================
  ! SORT UTILITIES (Untouched logic)
  ! =========================================================================

  recursive subroutine sort_ScanBoxes(a, first, last)
    type(ScanBox), intent(inout) :: a(:)
    integer(int32), intent(in) :: first, last
    integer(int32), parameter  :: K_THRESHOLD = 64
    integer(int32) :: i, j
    type(ScanBox) :: temp, pivot

    if (last - first < K_THRESHOLD) then
       call insertion_sort_ScanBoxes(a, first, last)
       return
    end if

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

  ! (Old Brute Force functions merge_all_boxes & can_merge can remain if needed for fallback tests)

end module BoxMergeModule
