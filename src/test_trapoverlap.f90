! File   : test_trapoverlap.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: brute-force check of trap_overlap_positive (exact separating-axis test) against a
!          floating-point convex clip of the two trapezoids on small lattice coordinates, where a
!          positive overlap area is far above rounding noise.
! Usage  : test_trapoverlap.exe [ncases] [seed]
program test_trapoverlap
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use TrapezoidModule
  implicit none
  integer(int64) :: ncases, ic, nov, nfail, ntouch
  integer :: seed, ln, st
  character(len=32) :: arg
  type(Trapezoid) :: a, b
  real(real64) :: ar
  logical :: ex
  ncases = 200000; seed = 1
  call get_command_argument(1, arg, ln, st); if (st == 0 .and. ln > 0) read(arg, *) ncases
  call get_command_argument(2, arg, ln, st); if (st == 0 .and. ln > 0) read(arg, *) seed
  call rng_seed(seed)
  nov = 0; nfail = 0; ntouch = 0
  do ic = 1, ncases
     call random_trap(a)
     if (mod(ic, 3_int64) == 0) then
        call near_trap(a, b)                 ! shares rows / sides: many touching cases
     else
        call random_trap(b)
     end if
     ex = trap_overlap_positive(a, b)
     ar = clip_area(a, b)
     if (ex) nov = nov + 1
     if (.not. ex .and. ar == 0 .and. bbox_touch(a, b)) ntouch = ntouch + 1
     if (ex .neqv. (ar > 1.0e-7_real64)) then
        nfail = nfail + 1
        if (nfail <= 5) write(*, '(a,i0,a,l1,a,es12.4)') 'FAIL case ', ic, ' exact=', ex, ' clip_area=', ar
     end if
     if (trap_overlap_positive(b, a) .neqv. ex) then
        nfail = nfail + 1
        if (nfail <= 5) write(*, '(a,i0)') 'FAIL asymmetric case ', ic
     end if
  end do
  write(*, '(a,i0,a,i0,a,i0,a,i0)') 'test_trapoverlap cases=', ncases, ' overlapping=', nov, &
       ' touching=', ntouch, ' failures=', nfail
  if (nfail == 0) then
     write(*, '(a)') 'test_trapoverlap PASS'
  else
     write(*, '(a)') 'test_trapoverlap FAIL'
     error stop 1
  end if
contains
  subroutine rng_seed(s)
    integer, intent(in) :: s
    integer :: n
    integer, allocatable :: v(:)
    call random_seed(size=n)
    allocate(v(n)); v = s + 37 * [(n, n = 1, size(v))]
    call random_seed(put=v)
  end subroutine rng_seed

  integer(int64) function irand(lo, hi)
    integer(int64), intent(in) :: lo, hi
    real(real64) :: r
    call random_number(r)
    irand = lo + int(r * real(hi - lo + 1, real64), int64)
    if (irand > hi) irand = hi
  end function irand

  real(real64) function xat(s, y1, y)
    type(TrapSide), intent(in) :: s
    integer(int32), intent(in) :: y1
    integer(int64), intent(in) :: y
    xat = (real(s%ax, real64) * s%dy + real(y - y1 + s%k, real64) * s%dx) / s%dy
  end function xat

  subroutine random_trap(t)
    type(Trapezoid), intent(out) :: t
    integer(int64) :: y1, y2, p(8)
    integer :: status, k
    do
       y1 = irand(-8_int64, 7_int64); y2 = irand(y1 + 1, 9_int64)
       do k = 1, 8
          p(k) = irand(-9_int64, 9_int64)
       end do
       if (irand(0_int64, 3_int64) == 0) p(3) = p(1)            ! vertical sides now and then
       if (p(2) == p(4) .or. p(6) == p(8)) cycle
       call trap_make(y1, y2, p(1), p(2), p(3), p(4), p(5), p(6), p(7), p(8), t, status)
       if (status /= K_TRAP_OK) cycle
       if (xat(t%l, t%y1, y1) > xat(t%r, t%y1, y1) + 1e-12_real64) cycle
       if (xat(t%l, t%y1, y2) > xat(t%r, t%y1, y2) + 1e-12_real64) cycle
       if (xat(t%r, t%y1, y1) - xat(t%l, t%y1, y1) + xat(t%r, t%y1, y2) - xat(t%l, t%y1, y2) < 1e-9_real64) cycle
       exit
    end do
  end subroutine random_trap

  !> A trapezoid sharing a side line or a row with a: touching / coincident configurations.
  subroutine near_trap(a, t)
    type(Trapezoid), intent(in) :: a
    type(Trapezoid), intent(out) :: t
    type(Trapezoid) :: r
    type(TrapSide) :: sl, sr
    integer :: mode, st
    integer(int32) :: oy1
    do
       call random_trap(r)
       oy1 = r%y1
       mode = int(irand(0_int64, 3_int64))
       select case (mode)
       case (2)                                 ! stacked on a's top row
          r%y1 = a%y2; r%y2 = a%y2 + int(irand(1_int64, 4_int64), int32)
       case (3)                                 ! same rows
          r%y1 = a%y1; r%y2 = a%y2
       end select
       call side_reanchor(r%l, int(oy1, int64), int(r%y1, int64), sl, st)
       call side_reanchor(r%r, int(oy1, int64), int(r%y1, int64), sr, st)
       r%l = sl; r%r = sr
       if (mode == 0) call side_reanchor(a%r, int(a%y1, int64), int(r%y1, int64), r%l, st)  ! share a's right line
       if (mode == 1) call side_reanchor(a%l, int(a%y1, int64), int(r%y1, int64), r%r, st)  ! share a's left line
       if (r%y2 <= r%y1) cycle
       if (xat(r%l, r%y1, int(r%y1, int64)) > xat(r%r, r%y1, int(r%y1, int64)) + 1e-12_real64) cycle
       if (xat(r%l, r%y1, int(r%y2, int64)) > xat(r%r, r%y1, int(r%y2, int64)) + 1e-12_real64) cycle
       exit
    end do
    t = r
  end subroutine near_trap

  logical function bbox_touch(a, b)
    type(Trapezoid), intent(in) :: a, b
    integer(int64) :: ax1, ay1, ax2, ay2, bx1, by1, bx2, by2
    call trap_mbr(a, ax1, ay1, ax2, ay2); call trap_mbr(b, bx1, by1, bx2, by2)
    bbox_touch = max(ax1, bx1) <= min(ax2, bx2) .and. max(ay1, by1) <= min(ay2, by2)
  end function bbox_touch

  !> Area of the intersection of two trapezoids (Sutherland-Hodgman, counter-clockwise).
  real(real64) function clip_area(a, b) result(ar)
    type(Trapezoid), intent(in) :: a, b
    real(real64) :: px(16), py(16), qx(16), qy(16), cx(4), cy(4)
    integer :: n, m, e, i, j
    real(real64) :: ex, ey, s1, s2, t
    call corners(a, px, py); n = 4
    call corners(b, cx, cy)
    do e = 1, 4
       j = mod(e, 4) + 1
       ex = cx(j) - cx(e); ey = cy(j) - cy(e)
       if (abs(ex) + abs(ey) == 0) cycle                     ! degenerate (triangle) edge
       m = 0
       do i = 1, n
          s1 = ex * (py(i) - cy(e)) - ey * (px(i) - cx(e))
          s2 = ex * (py(mod(i, n) + 1) - cy(e)) - ey * (px(mod(i, n) + 1) - cx(e))
          if (s1 >= 0) then
             m = m + 1; qx(m) = px(i); qy(m) = py(i)
          end if
          if ((s1 >= 0) .neqv. (s2 >= 0)) then
             t = s1 / (s1 - s2)
             m = m + 1
             qx(m) = px(i) + t * (px(mod(i, n) + 1) - px(i)); qy(m) = py(i) + t * (py(mod(i, n) + 1) - py(i))
          end if
       end do
       n = m; px(1:n) = qx(1:n); py(1:n) = qy(1:n)
       if (n < 3) then
          ar = 0; return
       end if
    end do
    ar = 0
    do i = 1, n
       ar = ar + px(i) * py(mod(i, n) + 1) - px(mod(i, n) + 1) * py(i)
    end do
    ar = 0.5_real64 * ar
    if (ar < 0) ar = 0
  end function clip_area

  subroutine corners(t, x, y)
    type(Trapezoid), intent(in) :: t
    real(real64), intent(out) :: x(4), y(4)
    x(1) = xat(t%l, t%y1, int(t%y1, int64)); y(1) = t%y1
    x(2) = xat(t%r, t%y1, int(t%y1, int64)); y(2) = t%y1
    x(3) = xat(t%r, t%y1, int(t%y2, int64)); y(3) = t%y2
    x(4) = xat(t%l, t%y1, int(t%y2, int64)); y(4) = t%y2
  end subroutine corners
end program test_trapoverlap
