! File    : datastructures.f90
! Author  : Sandeep Koranne (C) All rights.reserved.
! Purpose : Implementation of commonly used data structures
!         : hash table already implemented, this file has
!         : ring-buffer, LRU, union-find, graph, tree
!=====================================================================
!  File    : union_find_int64.f90
!  Author  : Sandeep Koranne (C) All rights reserved.
!  Purpose : Simple 64‑bit Union‑Find (disjoint‑set) container.
!            This version is stripped down to the essentials and
!            written so that NVFORTRAN accepts it (no PURE procedures,
!            no generic bindings, all symbols explicitly declared).
!=====================================================================

module DataStructuresModule
  use iso_fortran_env, only : int64
  implicit none
  private                     ! hide everything by default

  public :: UnionFind, uf_init_int64, uf_init_int64, uf_root_int64, uf_merge_int64, uf_reduce_int64,&
       uf_fullreduce_int64, uf_count_roots_int64, uf_expand_roots_int64

  !-----------------------------------------------------------------
  !  Union‑Find container (64‑bit indices)
  !-----------------------------------------------------------------
  type :: UnionFind
     integer(kind=int64), allocatable :: arr(:)   ! arr(i)=0 → singleton
   contains
     !--- basic operations -------------------------------------------------
     procedure :: init      => uf_init_int64
     procedure :: insert    => uf_insert_int64
     procedure :: root      => uf_root_int64
     procedure :: merge     => uf_merge_int64
     procedure :: reduce    => uf_reduce_int64
     procedure :: fullreduce=> uf_fullreduce_int64
     procedure :: count_roots => uf_count_roots_int64
     procedure :: expand_roots => uf_expand_roots_int64
  end type UnionFind


contains
  !=================================================================
  !  Initialise the structure.
  !  All entries are set to 0 → every element starts as a singleton.
  !=================================================================
  subroutine uf_init_int64 (self, max_n)
    class(UnionFind), intent(inout) :: self
    integer(kind=int64), intent(in) :: max_n

    if (allocated(self%arr)) deallocate(self%arr)
    allocate(self%arr(max_n))
    self%arr = 0_int64
  end subroutine uf_init_int64

  !=================================================================
  !  Insert a singleton element i (make it point to itself).
  !=================================================================
  subroutine uf_insert_int64 (self, i)
    class(UnionFind), intent(inout) :: self
    integer(kind=int64), intent(in) :: i

    if (self%arr(i) == 0_int64) then
       self%arr(i) = i
    end if
  end subroutine uf_insert_int64

  !=================================================================
  !  Find the root of element i (no path compression – pure‑like).
  !=================================================================
  function uf_root_int64 (self, i) result(r)
    class(UnionFind), intent(in) :: self
    integer(kind=int64), intent(in) :: i
    integer(kind=int64) :: r, cur

    if (self%arr(i) == 0_int64) then
       r = i
       return
    end if

    cur = i
    do while (self%arr(cur) /= cur)
       cur = self%arr(cur)
    end do
    r = cur
  end function uf_root_int64

  !=================================================================
  !  Merge the sets containing x and y.
  !=================================================================
  subroutine uf_merge_int64 (self, x, y)
    class(UnionFind), intent(inout) :: self
    integer(kind=int64), intent(in) :: x, y
    integer(kind=int64) :: rx, ry

    rx = self%root(x)
    ry = self%root(y)

    if (rx == 0_int64 .or. ry == 0_int64) then
       stop "ERROR: attempt to merge a singleton that has not been inserted."
    end if

    if (rx == ry) return                ! already in the same set

    ! Simple union – attach the higher numbered root to the lower.
    if (rx < ry) then
       self%arr(ry) = rx
       call self%reduce(ry)
    else
       self%arr(rx) = ry
       call self%reduce(rx)
    end if
  end subroutine uf_merge_int64

  !=================================================================
  !  Path‑compression: after the call arr(x) points directly to the
  !  ultimate root.
  !=================================================================
  subroutine uf_reduce_int64 (self, x)
    class(UnionFind), intent(inout) :: self
    integer(kind=int64), intent(in) :: x
    integer(kind=int64) :: r, cur, nxt

    if (self%arr(x) == 0_int64) return   ! singleton – nothing to do

    cur = x
    do while (self%arr(cur) /= cur)
       cur = self%arr(cur)
    end do
    r = cur

    cur = x
    do while (self%arr(cur) /= cur)
       nxt               = self%arr(cur)
       self%arr(cur)  = r
       cur               = nxt
    end do
  end subroutine uf_reduce_int64

  !=================================================================
  !  Full reduction – compress every path and renumber the roots
  !  sequentially from 1..Nroots.
  !=================================================================
  subroutine uf_fullreduce_int64 (self)
    class(UnionFind), intent(inout) :: self
    integer(kind=int64) :: n, i, cur_label
    integer(kind=int64), allocatable :: map(:)

    n = size(self%arr)
    allocate(map(n), source=0_int64)

    ! First compress all paths
    do i = 1, n
       if (self%arr(i) /= 0_int64) call self%reduce(i)
    end do

    ! Assign new labels to the distinct roots
    cur_label = 1_int64
    do i = 1, n
       if (self%arr(i) == i) then
          map(i) = cur_label
          cur_label = cur_label + 1_int64
       end if
    end do

    ! Replace old root numbers by the new compact ones
    do i = 1, n
       if (self%arr(i) > 0_int64) then
          self%arr(i) = map(self%arr(i))
       end if
    end do
  end subroutine uf_fullreduce_int64

  !=================================================================
  !  Count the number of distinct roots (i.e. the number of sets).
  !=================================================================
  pure function uf_count_roots_int64 (self) result(nroots)
    class(UnionFind), intent(in) :: self
    integer(kind=int64) :: nroots
    integer(kind=int64) :: i, minv, maxv
    logical, allocatable :: seen(:)

    if (size(self%arr) == 0) then
       nroots = 0_int64
       return
    end if

    minv = minval(self%arr)
    maxv = maxval(self%arr)

    allocate(seen(minv:maxv), source=.false.)

    do i = 1, size(self%arr)
       if (self%arr(i) > 0_int64) seen(self%arr(i)) = .true.
    end do

    nroots = count(seen)
  end function uf_count_roots_int64

  !=================================================================
  !  Expand the structure so that every element that is still a
  !  singleton (arr = 0) gets a fresh unique root number.
  !=================================================================
  subroutine uf_expand_roots_int64 (self)
    class(UnionFind), intent(inout) :: self
    integer(kind=int64) :: nroots, i

    nroots = self%count_roots()

    do i = 1, size(self%arr)
       if (self%arr(i) == 0_int64) then
          nroots = nroots + 1_int64
          self%arr(i) = nroots
       end if
    end do
  end subroutine uf_expand_roots_int64

end module DataStructuresModule



