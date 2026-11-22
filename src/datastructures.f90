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

  public :: UnionFind, uf_init_int64, uf_root_int64, uf_merge_int64, uf_reduce_int64,&
       uf_fullreduce_int64, uf_count_roots_int64, uf_expand_roots_int64
  public :: BucketBoundary, GetSortPermutation, get_equal_key_segments, group_by_key

  !-----------------------------------------------------------------
  !  Inclusive [start_idx, end_idx] range of one equal-key bucket
  !  inside a permutation produced by GetSortPermutation/group_by_key.
  !-----------------------------------------------------------------
  type :: BucketBoundary
     integer(int64) :: start_idx = 0
     integer(int64) :: end_idx   = 0
  end type BucketBoundary

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

  !=================================================================
  !  Key grouping.
  !
  !  GetSortPermutation returns perm such that keys(perm) is ascending
  !  and equal keys keep their original relative order (stable).
  !  Dense keys (the common case: polygon numbers in 0..N) are handled
  !  by an O(N) counting sort; sparse keys fall back to a quicksort.
  !=================================================================
  pure subroutine GetSortPermutation(arr, perm)
    integer(int64), intent(in) :: arr(:)
    integer(int64), allocatable, intent(out) :: perm(:)
    integer(int64), allocatable :: offsets(:)
    integer(int64) :: i, n, lo, hi

    n = size(arr, kind=int64)
    allocate(perm(n))
    if (n == 0) return

    lo = minval(arr)
    hi = maxval(arr)
    if (is_dense_range(lo, hi, n)) then
       call counting_sort_permutation(arr, lo, hi, perm, offsets)
    else
       do i = 1, n
          perm(i) = i
       end do
       if (n > 1) call IndirectQuicksort(arr, perm, 1_int64, n)
    end if
  end subroutine GetSortPermutation

  !=================================================================
  !  Boundaries of the equal-key runs of keys(perm).
  !=================================================================
  pure subroutine get_equal_key_segments(arr, perm, segments)
    integer(int64), intent(in)  :: arr(:)
    integer(int64), intent(in)  :: perm(:)
    type(BucketBoundary), allocatable, intent(out) :: segments(:)
    integer(int64) :: i, n, num_segments

    n = size(perm, kind=int64)
    if (n == 0) then
       allocate(segments(0))
       return
    end if

    num_segments = 1
    do i = 1, n - 1
       if (arr(perm(i)) /= arr(perm(i+1))) num_segments = num_segments + 1
    end do
    allocate(segments(num_segments))

    num_segments = 1
    segments(1)%start_idx = 1
    do i = 1, n - 1
       if (arr(perm(i)) /= arr(perm(i+1))) then
          segments(num_segments)%end_idx = i
          num_segments = num_segments + 1
          segments(num_segments)%start_idx = i + 1
       end if
    end do
    segments(num_segments)%end_idx = n
  end subroutine get_equal_key_segments

  !=================================================================
  !  One-shot equivalent of GetSortPermutation + get_equal_key_segments.
  !  For dense keys the bucket boundaries fall out of the counting-sort
  !  histogram, so no second scan over the (indirectly addressed) keys
  !  is needed.
  !=================================================================
  pure subroutine group_by_key(arr, perm, segments)
    integer(int64), intent(in) :: arr(:)
    integer(int64), allocatable, intent(out) :: perm(:)
    type(BucketBoundary), allocatable, intent(out) :: segments(:)
    integer(int64), allocatable :: offsets(:)
    integer(int64) :: n, lo, hi, k, num_segments

    n = size(arr, kind=int64)
    if (n == 0) then
       allocate(perm(0), segments(0))
       return
    end if

    lo = minval(arr)
    hi = maxval(arr)
    if (.not. is_dense_range(lo, hi, n)) then
       call GetSortPermutation(arr, perm)
       call get_equal_key_segments(arr, perm, segments)
       return
    end if

    allocate(perm(n))
    call counting_sort_permutation(arr, lo, hi, perm, offsets)
    ! offsets(k) is now the first slot of key k; offsets(hi+1) = n+1.
    num_segments = 0
    do k = lo, hi
       if (offsets(k+1) > offsets(k)) num_segments = num_segments + 1
    end do
    allocate(segments(num_segments))
    num_segments = 0
    do k = lo, hi
       if (offsets(k+1) > offsets(k)) then
          num_segments = num_segments + 1
          segments(num_segments)%start_idx = offsets(k)
          segments(num_segments)%end_idx   = offsets(k+1) - 1
       end if
    end do
  end subroutine group_by_key

  pure logical function is_dense_range(lo, hi, n)
    integer(int64), intent(in) :: lo, hi, n
    integer(int64), parameter :: K_MAX_RANGE_PER_KEY = 4_int64, K_MIN_RANGE = 1024_int64
    ! hi - lo cannot overflow for the non-negative keys we care about;
    ! guard the general case by testing the signs first.
    if (lo < 0_int64 .and. hi > huge(hi) + lo) then
       is_dense_range = .false.
    else
       is_dense_range = (hi - lo) < max(K_MIN_RANGE, K_MAX_RANGE_PER_KEY * n)
    end if
  end function is_dense_range

  !  Stable counting sort of the indices 1..N by key. On return
  !  offsets(k) is the first position of key k in perm (k = lo..hi+1).
  pure subroutine counting_sort_permutation(arr, lo, hi, perm, offsets)
    integer(int64), intent(in)  :: arr(:)
    integer(int64), intent(in)  :: lo, hi
    integer(int64), intent(out) :: perm(:)
    integer(int64), allocatable, intent(out) :: offsets(:)
    integer(int64), allocatable :: cursor(:)
    integer(int64) :: i, k, n, running, bucket_size

    n = size(arr, kind=int64)
    allocate(offsets(lo:hi+1), source=0_int64)
    do i = 1, n
       offsets(arr(i)) = offsets(arr(i)) + 1
    end do
    running = 1
    do k = lo, hi
       bucket_size = offsets(k)
       offsets(k)  = running
       running     = running + bucket_size
    end do
    offsets(hi+1) = running

    allocate(cursor(lo:hi))
    cursor(lo:hi) = offsets(lo:hi)
    do i = 1, n
       k = arr(i)
       perm(cursor(k)) = i
       cursor(k) = cursor(k) + 1
    end do
  end subroutine counting_sort_permutation

  !  Stable indirect quicksort (ties broken on the original index) with
  !  an insertion-sort cutoff. Used only for sparse key ranges.
  pure recursive subroutine IndirectQuicksort(arr, perm, left, right)
    integer(int64), intent(in)    :: arr(:)
    integer(int64), intent(inout) :: perm(:)
    integer(int64), intent(in)    :: left, right
    integer(int64), parameter :: K_INSERTION_CUTOFF = 16_int64
    integer(int64) :: i, j, temp, pivot_val, pivot_perm, key_idx, key_val

    if (left >= right) return

    if (right - left + 1 <= K_INSERTION_CUTOFF) then
       do i = left + 1, right
          key_idx = perm(i)
          key_val = arr(key_idx)
          j = i - 1
          do while (j >= left)
             if (arr(perm(j)) < key_val) exit
             if (arr(perm(j)) == key_val .and. perm(j) < key_idx) exit
             perm(j+1) = perm(j)
             j = j - 1
          end do
          perm(j+1) = key_idx
       end do
       return
    end if

    pivot_perm = perm((left + right) / 2)
    pivot_val  = arr(pivot_perm)
    i = left
    j = right
    do while (i <= j)
       do while (arr(perm(i)) < pivot_val .or. &
            (arr(perm(i)) == pivot_val .and. perm(i) < pivot_perm))
          i = i + 1
       end do
       do while (arr(perm(j)) > pivot_val .or. &
            (arr(perm(j)) == pivot_val .and. perm(j) > pivot_perm))
          j = j - 1
       end do
       if (i <= j) then
          temp = perm(i); perm(i) = perm(j); perm(j) = temp
          i = i + 1
          j = j - 1
       end if
    end do
    if (left < j)  call IndirectQuicksort(arr, perm, left, j)
    if (i < right) call IndirectQuicksort(arr, perm, i, right)
  end subroutine IndirectQuicksort

end module DataStructuresModule
