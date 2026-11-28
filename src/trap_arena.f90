! File   : trap_arena.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Pure-POD, non-polymorphic (Intel ifx OpenMP-safe) memory pool & high-water-mark
!          arena helpers for the any-angle trapezoid engine (Tier 1 consing elimination).
!
! Design rules:
!   - Zero class(...), zero type-bound procedures, zero allocatable-component UDTs passed
!     by value or assigned inside OpenMP hot loops.
!   - Backing buffers are allocated ONCE per OpenMP thread (or per persistent pool) and
!     grown geometrically only when a new high-water mark is reached.
!   - Resetting a thread-local arena between bands or operations is O(1) (top = 0) with
!     zero malloc/free calls and zero kernel minor page faults.
module TrapArenaModule
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
  use TrapezoidModule, only: Trapezoid
  implicit none
  private

  public :: arena_ensure_i64, arena_ensure_i32, arena_ensure_i8, arena_ensure_r64, arena_ensure_l
  public :: arena_ensure0_i64, arena_ensure0_i32, arena_ensure0_i8
  public :: arena_grow_i64, arena_grow_i32, arena_grow_trap
  public :: arena_alloc_i64, arena_alloc_trap

contains

  !> Ensure 1-based array a(1:cap) has at least n elements (contents discarded on grow).
  subroutine arena_ensure_i64(a, n)
    integer(int64), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = size(a, kind=int64)
       if (cap >= n) return
       deallocate(a)
       m = max(n, 2_int64 * cap)
    else
       m = max(n, 64_int64)
    end if
    allocate(a(m))
  end subroutine arena_ensure_i64

  subroutine arena_ensure_i32(a, n)
    integer(int32), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = size(a, kind=int64)
       if (cap >= n) return
       deallocate(a)
       m = max(n, 2_int64 * cap)
    else
       m = max(n, 64_int64)
    end if
    allocate(a(m))
  end subroutine arena_ensure_i32

  subroutine arena_ensure_i8(a, n)
    integer(int8), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = size(a, kind=int64)
       if (cap >= n) return
       deallocate(a)
       m = max(n, 2_int64 * cap)
    else
       m = max(n, 64_int64)
    end if
    allocate(a(m))
  end subroutine arena_ensure_i8

  subroutine arena_ensure_r64(a, n)
    real(real64), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = size(a, kind=int64)
       if (cap >= n) return
       deallocate(a)
       m = max(n, 2_int64 * cap)
    else
       m = max(n, 64_int64)
    end if
    allocate(a(m))
  end subroutine arena_ensure_r64

  subroutine arena_ensure_l(a, n)
    logical, allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = size(a, kind=int64)
       if (cap >= n) return
       deallocate(a)
       m = max(n, 2_int64 * cap)
    else
       m = max(n, 64_int64)
    end if
    allocate(a(m))
  end subroutine arena_ensure_l

  !> Ensure 0-based array a(0:cap) has upper bound >= n (contents discarded on grow).
  subroutine arena_ensure0_i64(a, n)
    integer(int64), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = ubound(a, 1, kind=int64)
       if (cap >= n) return
       deallocate(a)
       m = max(n, 2_int64 * cap)
    else
       m = max(n, 256_int64)
    end if
    allocate(a(0:m))
  end subroutine arena_ensure0_i64

  subroutine arena_ensure0_i32(a, n)
    integer(int32), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = ubound(a, 1, kind=int64)
       if (cap >= n) return
       deallocate(a)
       m = max(n, 2_int64 * cap)
    else
       m = max(n, 256_int64)
    end if
    allocate(a(0:m))
  end subroutine arena_ensure0_i32

  subroutine arena_ensure0_i8(a, n)
    integer(int8), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = ubound(a, 1, kind=int64)
       if (cap >= n) return
       deallocate(a)
       m = max(n, 2_int64 * cap)
    else
       m = max(n, 256_int64)
    end if
    allocate(a(0:m))
  end subroutine arena_ensure0_i8

  !> Grow 1-based array a preserving a(1:used).
  subroutine arena_grow_i64(a, used, need)
    integer(int64), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: used, need
    integer(int64), allocatable :: tmp(:)
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = size(a, kind=int64)
       if (cap >= need) return
       m = max(need, 2_int64 * cap)
       allocate(tmp(m))
       if (used > 0) tmp(1:used) = a(1:used)
       call move_alloc(tmp, a)
    else
       allocate(a(max(need, 4096_int64)))
    end if
  end subroutine arena_grow_i64

  subroutine arena_grow_i32(a, used, need)
    integer(int32), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: used, need
    integer(int32), allocatable :: tmp(:)
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = size(a, kind=int64)
       if (cap >= need) return
       m = max(need, 2_int64 * cap)
       allocate(tmp(m))
       if (used > 0) tmp(1:used) = a(1:used)
       call move_alloc(tmp, a)
    else
       allocate(a(max(need, 4096_int64)))
    end if
  end subroutine arena_grow_i32

  subroutine arena_grow_trap(a, used, need)
    type(Trapezoid), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: used, need
    type(Trapezoid), allocatable :: tmp(:)
    integer(int64) :: cap, m
    if (allocated(a)) then
       cap = size(a, kind=int64)
       if (cap >= need) return
       m = max(need, 2_int64 * cap)
       allocate(tmp(m))
       if (used > 0) tmp(1:used) = a(1:used)
       call move_alloc(tmp, a)
    else
       allocate(a(max(need, 4096_int64)))
    end if
  end subroutine arena_grow_trap

  !> Linear bump-arena sub-allocation of n int64 slots in buf(1:cap), returning [i0, i1].
  subroutine arena_alloc_i64(buf, cap, top, n, i0, i1)
    integer(int64), allocatable, intent(inout) :: buf(:)
    integer(int64), intent(inout) :: cap, top
    integer(int64), intent(in)    :: n
    integer(int64), intent(out)   :: i0, i1
    if (top + n > cap) then
       call arena_grow_i64(buf, top, top + n + 65536_int64)
       cap = size(buf, kind=int64)
    end if
    i0 = top + 1; i1 = top + n; top = i1
  end subroutine arena_alloc_i64

  !> Linear bump-arena sub-allocation of n Trapezoid slots in buf(1:cap), returning [i0, i1].
  subroutine arena_alloc_trap(buf, cap, top, n, i0, i1)
    type(Trapezoid), allocatable, intent(inout) :: buf(:)
    integer(int64), intent(inout) :: cap, top
    integer(int64), intent(in)    :: n
    integer(int64), intent(out)   :: i0, i1
    if (top + n > cap) then
       call arena_grow_trap(buf, top, top + n + 16384_int64)
       cap = size(buf, kind=int64)
    end if
    i0 = top + 1; i1 = top + n; top = i1
  end subroutine arena_alloc_trap

end module TrapArenaModule
