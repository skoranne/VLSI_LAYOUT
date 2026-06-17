! File   : gpu_skiplist.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: A normal pointer heavy skiplist will not be ideal for GPU
!=====================================================================
!  gpu_skiplist.f90
!  Integer-offset (Index-based) skip list for GPU offloading
!=====================================================================
module GPUSkipListModule
  implicit none
  private
  public :: SL_INIT, SL_DESTROY, SL_FIND, SL_INSERT, SL_DELETE
  public :: Skiplist, SkiplistNode, MAX_LEVELS, NULL_IDX

  integer, parameter :: MAX_LEVELS = 16
  real,    parameter :: DEFAULT_P  = 0.5

  ! Use 0 as our "Null" pointer since Fortran arrays are 1-indexed
  integer, parameter :: NULL_IDX   = 0

  !=================================================================
  !  1) Node type - Strictly contiguous, no pointers, no allocatables
  !     Using a fixed-size forward array avoids device-side deep copies.
  !=================================================================
  type :: SkiplistNode
     integer :: data = 0
     integer :: forward(MAX_LEVELS) = NULL_IDX 
  end type SkiplistNode

  !=================================================================
  !  2) Container type - Owns the contiguous memory pool
  !=================================================================
  type :: Skiplist
     integer :: max_levels = MAX_LEVELS
     real    :: prob       = DEFAULT_P
     integer :: head       = NULL_IDX
     integer :: free_list  = NULL_IDX
     integer :: capacity   = 0
     type(SkiplistNode), allocatable :: pool(:) 
  end type Skiplist

  interface SL_INIT
     module procedure sl_init
  end interface SL_INIT
  interface SL_DESTROY
     module procedure sl_destroy
  end interface SL_DESTROY
  interface SL_FIND
     module procedure sl_find
  end interface SL_FIND
  interface SL_INSERT
     module procedure sl_insert
  end interface SL_INSERT
  interface SL_DELETE
     module procedure sl_delete
  end interface SL_DELETE

contains

  !=================================================================
  !  Random level generator
  !=================================================================
  integer function random_level(sl) result(level)
    type(Skiplist), intent(in) :: sl
    real                       :: r
    level = 1
    do while (level < sl%max_levels)
       call random_number(r)
       if (r >= sl%prob) exit
       level = level + 1
    end do
  end function random_level

  !=================================================================
  !  Expand the pool (HOST ONLY). 
  !  Do not call this inside a GPU kernel.
  !=================================================================
  subroutine expand_pool(sl)
    type(Skiplist), intent(inout) :: sl
    type(SkiplistNode), allocatable :: temp_pool(:)
    integer :: old_cap, new_cap, i

    old_cap = sl%capacity
    new_cap = max(old_cap * 2, 4096) ! Double the capacity

    allocate(temp_pool(new_cap))

    if (old_cap > 0) then
       temp_pool(1:old_cap) = sl%pool(1:old_cap)
    end if

    ! Setup the new free list chain in the expanded area
    sl%free_list = old_cap + 1
    do i = old_cap + 1, new_cap - 1
       temp_pool(i)%forward(1) = i + 1
    end do
    temp_pool(new_cap)%forward(1) = NULL_IDX

    call move_alloc(from=temp_pool, to=sl%pool)
    sl%capacity = new_cap
  end subroutine expand_pool

  !=================================================================
  !  Allocate a node from the free list
  !=================================================================
  subroutine allocate_node(sl, idx)
    type(Skiplist), intent(inout) :: sl
    integer, intent(out)          :: idx

    if (sl%free_list == NULL_IDX) then
       ! WARNING: Pool expansion must happen on the HOST before 
       ! moving the data environment to the GPU.
       call expand_pool(sl)
    end if

    idx = sl%free_list
    sl%free_list = sl%pool(idx)%forward(1)

    ! Clear the node's forward array
    sl%pool(idx)%forward(:) = NULL_IDX
  end subroutine allocate_node

  !=================================================================
  !  Return node to free list
  !=================================================================
  subroutine free_node(sl, idx)
    type(Skiplist), intent(inout) :: sl
    integer, intent(in)           :: idx

    sl%pool(idx)%forward(1) = sl%free_list
    sl%free_list = idx
  end subroutine free_node

  !=================================================================
  !  SL_INIT - create list and allocate pre-determined pool size
  !=================================================================
  subroutine sl_init(sl, expected_capacity, max_levels)
    type(Skiplist), intent(out) :: sl
    integer, intent(in)         :: expected_capacity
    integer, intent(in), optional :: max_levels

    if (present(max_levels)) sl%max_levels = max_levels
    sl%prob = DEFAULT_P
    sl%capacity = 0

    ! Expand pool to initial capacity
    do while (sl%capacity < expected_capacity + 1)
       call expand_pool(sl)
    end do

    ! Setup Sentinel Head
    call allocate_node(sl, sl%head)
    sl%pool(sl%head)%data = -huge(0)
  end subroutine sl_init

  !=================================================================
  !  SL_FIND
  !=================================================================
  subroutine sl_find(sl, key, update, found_idx)
    type(Skiplist), intent(in)  :: sl
    integer, intent(in)         :: key
    integer, intent(out)        :: update(:) ! Assumed size sl%max_levels
    integer, intent(out)        :: found_idx

    integer :: curr, nxt, i

    curr = sl%head
    do i = sl%max_levels, 1, -1
       nxt = sl%pool(curr)%forward(i)

       do while (nxt /= NULL_IDX)
          if (sl%pool(nxt)%data >= key) exit
          curr = nxt
          nxt = sl%pool(curr)%forward(i)
       end do
       update(i) = curr
    end do

    nxt = sl%pool(curr)%forward(1)
    if (nxt /= NULL_IDX) then
       if (sl%pool(nxt)%data == key) then
          found_idx = nxt
       else
          found_idx = NULL_IDX
       end if
    else
       found_idx = NULL_IDX
    end if
  end subroutine sl_find

  !=================================================================
  !  SL_INSERT
  !=================================================================
  subroutine sl_insert(sl, key)
    type(Skiplist), intent(inout) :: sl
    integer, intent(in)           :: key

    integer :: update(MAX_LEVELS)
    integer :: target, new_node
    integer :: lvl, i

    call sl_find(sl, key, update, target)

    if (target /= NULL_IDX) return ! Duplicate

    lvl = random_level(sl)
    call allocate_node(sl, new_node)
    sl%pool(new_node)%data = key

    do i = 1, lvl
       sl%pool(new_node)%forward(i) = sl%pool(update(i))%forward(i)
       sl%pool(update(i))%forward(i) = new_node
    end do
  end subroutine sl_insert

  !=================================================================
  !  SL_DELETE
  !=================================================================
  subroutine sl_delete(sl, key)
    type(Skiplist), intent(inout) :: sl
    integer, intent(in)           :: key

    integer :: update(MAX_LEVELS)
    integer :: target, i

    call sl_find(sl, key, update, target)

    if (target == NULL_IDX) return ! Not found

    do i = 1, sl%max_levels
       if (sl%pool(update(i))%forward(i) == target) then
          sl%pool(update(i))%forward(i) = sl%pool(target)%forward(i)
       end if
    end do

    call free_node(sl, target)
  end subroutine sl_delete

  !=================================================================
  !  SL_DESTROY
  !=================================================================
  subroutine sl_destroy(sl)
    type(Skiplist), intent(inout) :: sl

    if (allocated(sl%pool)) deallocate(sl%pool)
    sl%head      = NULL_IDX
    sl%free_list = NULL_IDX
    sl%capacity  = 0
  end subroutine sl_destroy
end module GPUSkipListModule
program test_skiplist
  use GPUSkipListModule
  implicit none

  type(Skiplist) :: sl
  integer :: i

  call SL_INIT(sl, expected_capacity=2000, max_levels=12)

  do i = 1, 2000000
     call SL_INSERT(sl, i*10)
  end do

  call SL_INSERT(sl, 30)   ! duplicate – ignored
  call SL_DELETE(sl, 10)   ! present
  call SL_DELETE(sl, 150)  ! absent – harmless

  !call sl_print(sl)

  call SL_DESTROY(sl)
end program test_skiplist
