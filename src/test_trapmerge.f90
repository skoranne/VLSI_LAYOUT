! File   : test_trapmerge.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Randomised brute-force checks of TrapMergeModule (single-layer OR, any angle).
!   Per case (random rectangles, triangles, rotated rectangles, star polygons, overlapping):
!   * no internal anomalies; every trapezoid valid; area integral (invariant I1)
!   * the result is identical for 1 band and for k bands (canonical output)
!   * snap rounding: no two fragments cross, overlap, or meet at an interior point
!   * at random rational sample points (x/21, y/21): inside a trapezoid (at most one) <=>
!     nonzero winding of the snapped fragments
!   * against the unsnapped input: any disagreement lies within 0.75 dbu of an input edge
!     (snap rounding moves points by at most sqrt(2)/2)
!   * Manhattan cases: no inexact crossings and 2*area == 2 * (unit cells inside), exactly
!   usage: test_trapmerge.exe [ncases] [seed]
program test_trapmerge
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use WideIntModule
  use ExactPredicatesModule
  use TrapezoidModule
  use TrapMergeModule
  implicit none
  integer :: ncase, seed, icase, nfail, nb, mode, i, j, k, ns, nsample, nfail_case
  integer(int64) :: npoly, nv, ntrap1, ntrapk, G, cnt_cells, area2_int
  integer(int64), allocatable :: poff(:), px(:), py(:)
  type(FragSet) :: E, F
  type(Trapezoid), allocatable :: t1(:), tk(:)
  type(TrapMergeStats) :: st1, stk
  type(i128) :: a2
  logical :: isint
  character(len=32) :: arg
  integer :: istat
  real(real64) :: r
  integer(int64) :: offset, npmax
  logical :: big
  integer(int64), parameter :: gsizes(5) = [8_int64, 16_int64, 40_int64, 200_int64, 3000_int64]
  integer(int64) :: tot_traps, tot_inexact, tot_samples, tot_mismatch_unsnapped

  ncase = 3000; seed = 12345
  if (command_argument_count() >= 1) then
     call get_command_argument(1, arg); read(arg, *) ncase
  end if
  if (command_argument_count() >= 2) then
     call get_command_argument(2, arg); read(arg, *) seed
  end if
  offset = 0; npmax = 14
  if (command_argument_count() >= 3) then
     call get_command_argument(3, arg); read(arg, *) offset
  end if
  if (command_argument_count() >= 4) then
     call get_command_argument(4, arg); read(arg, *) npmax
  end if
  big = npmax > 40
  call init_rng(seed)
  nfail = 0; tot_traps = 0; tot_inexact = 0; tot_samples = 0; tot_mismatch_unsnapped = 0
  do icase = 1, ncase
     mode = mod(icase, 5)            ! 0: Manhattan only
     call random_number(r)
     G = gsizes(1 + int(r*5))
     call make_case(mode, G, npoly, poff, px, py)
     px = px + offset; py = py - offset
     nfail_case = 0
     st1 = TrapMergeStats(); stk = TrapMergeStats()
     call TrapBuildEdges(npoly, poff, px, py, E, istat)
     if (istat /= 0) call fail('build status')
     call TrapSnapRound(E, F, st1)
     call TrapSweepUnion(F, 1, t1, ntrap1, st1)
     call random_number(r); nb = 2 + int(r*9)
     stk%anomalies = 0
     call TrapSweepUnion(F, nb, tk, ntrapk, stk)
     if (st1%anomalies /= 0) call fail('anomalies nb=1')
     if (stk%anomalies /= 0) call fail('anomalies nb=k')
     call trap_sort(t1, ntrap1); call trap_sort(tk, ntrapk)
     if (ntrap1 /= ntrapk) then
        call fail('band count changes the result')
     else
        do i = 1, int(ntrap1)
           if (.not. trap_equal(t1(i), tk(i))) then
              call fail('band count changes the result'); exit
           end if
        end do
     end if
     do i = 1, int(ntrap1)
        if (.not. trap_valid(t1(i))) then
           call fail('invalid trapezoid'); exit
        end if
     end do
     call trap_area2(t1, ntrap1, a2, isint)
     if (.not. isint) call fail('area not integral (I1)')
     if (.not. big) call check_fragments()
     if (.not. big) call check_samples()
     if (mode == 0 .and. G <= 200 .and. offset == 0 .and. .not. big) then
        if (st1%crossings_inexact /= 0) call fail('inexact crossing in Manhattan data')
        cnt_cells = manhattan_cells()
        area2_int = int(i128_to_real64(a2), int64)
        if (area2_int /= 2*cnt_cells) call fail('Manhattan area mismatch')
     end if
     tot_traps = tot_traps + ntrap1; tot_inexact = tot_inexact + st1%crossings_inexact
     if (nfail_case > 0) then
        nfail = nfail + 1
        if (nfail <= 5) call dump_case()
     end if
  end do
  write(*, '(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)') 'test_trapmerge: cases=', ncase, ' failed=', nfail, &
       ' traps=', tot_traps, ' inexact_crossings=', tot_inexact, ' samples=', tot_samples, &
       ' snapped_vs_exact_near_boundary=', tot_mismatch_unsnapped
  if (nfail > 0) then
     write(*, '(a)') 'FAIL'; error stop 1
  end if
  write(*, '(a)') 'PASS'

contains

  subroutine init_rng(s)
    integer, intent(in) :: s
    integer :: n
    integer, allocatable :: sd(:)
    call random_seed(size=n)
    allocate(sd(n)); sd = s + 37*[(i, i = 1, n)]
    call random_seed(put=sd)
  end subroutine init_rng

  integer(int64) function irand(lo, hi)
    integer(int64), intent(in) :: lo, hi
    real(real64) :: u
    call random_number(u)
    irand = lo + min(int(u*real(hi - lo + 1, real64), int64), hi - lo)
  end function irand

  subroutine fail(msg)
    character(len=*), intent(in) :: msg
    nfail_case = nfail_case + 1
    if (nfail < 5) write(*, '(a,i0,a,a)') 'case ', icase, ': ', msg
  end subroutine fail

  subroutine add_poly(n, xs, ys, npoly, poff, px, py, nv)
    integer, intent(in) :: n
    integer(int64), intent(in) :: xs(:), ys(:)
    integer(int64), intent(inout) :: npoly, nv
    integer(int64), intent(inout) :: poff(:), px(:), py(:)
    npoly = npoly + 1
    poff(npoly) = nv + 1
    px(nv + 1:nv + n) = xs(1:n); py(nv + 1:nv + n) = ys(1:n)
    nv = nv + n
    poff(npoly + 1) = nv + 1
  end subroutine add_poly

  subroutine make_case(mode, G, npoly, poff, px, py)
    integer, intent(in) :: mode
    integer(int64), intent(in) :: G
    integer(int64), intent(out) :: npoly
    integer(int64), allocatable, intent(inout) :: poff(:), px(:), py(:)
    integer(int64) :: np, p, x0, y0, w, h, xs(16), ys(16), cx, cy, nv
    integer :: kind, n, q
    real(real64) :: ang, rad, th, c, s
    if (allocated(poff)) deallocate(poff, px, py)
    np = irand(1_int64, npmax)
    allocate(poff(np + 2), px(16*np), py(16*np))
    npoly = 0; nv = 0
    do p = 1, np
       kind = int(irand(0_int64, 4_int64))
       if (mode == 0) kind = 0
       select case (kind)
       case (0)                                   ! axis-aligned rectangle (either orientation)
          x0 = irand(0_int64, G - 2); y0 = irand(0_int64, G - 2)
          w = irand(1_int64, max(1_int64, G/2)); h = irand(1_int64, max(1_int64, G/2))
          xs(1:4) = [x0, x0 + w, x0 + w, x0]; ys(1:4) = [y0, y0, y0 + h, y0 + h]
          if (irand(0_int64, 1_int64) == 1) then
             xs(1:4) = xs(4:1:-1); ys(1:4) = ys(4:1:-1)
          end if
          call add_poly(4, xs, ys, npoly, poff, px, py, nv)
       case (1)                                   ! triangle
          do q = 1, 3
             xs(q) = irand(0_int64, G); ys(q) = irand(0_int64, G)
          end do
          call add_poly(3, xs, ys, npoly, poff, px, py, nv)
       case (2)                                   ! rotated rectangle, vertices rounded
          cx = irand(0_int64, G); cy = irand(0_int64, G)
          call random_number(ang); ang = ang*3.14159265358979_real64
          w = irand(1_int64, max(1_int64, G/3)); h = irand(1_int64, max(1_int64, G/3))
          c = cos(ang); s = sin(ang)
          do q = 1, 4
             th = merge(-0.5_real64, 0.5_real64, q == 1 .or. q == 4)*w
             rad = merge(-0.5_real64, 0.5_real64, q <= 2)*h
             xs(q) = cx + nint(th*c - rad*s, int64); ys(q) = cy + nint(th*s + rad*c, int64)
          end do
          call add_poly(4, xs, ys, npoly, poff, px, py, nv)
       case (3)                                   ! star-shaped polygon (simple)
          cx = irand(0_int64, G); cy = irand(0_int64, G)
          n = int(irand(3_int64, 12_int64))
          do q = 1, n
             th = 6.28318530717959_real64*(q - 1 + 0.8_real64*rnd())/n
             rad = (0.1_real64 + rnd())*G/3
             xs(q) = cx + nint(rad*cos(th), int64); ys(q) = cy + nint(rad*sin(th), int64)
          end do
          call add_poly(n, xs, ys, npoly, poff, px, py, nv)
       case default                               ! copy of an earlier polygon, shifted a bit
          if (npoly == 0) then
             x0 = irand(0_int64, G - 2); y0 = irand(0_int64, G - 2)
             xs(1:3) = [x0, x0 + 1 + G/4, x0]; ys(1:3) = [y0, y0, y0 + 1 + G/4]
             call add_poly(3, xs, ys, npoly, poff, px, py, nv)
          else
             q = int(irand(1_int64, npoly))
             n = int(poff(q + 1) - poff(q))
             x0 = irand(-2_int64, 2_int64); y0 = irand(-2_int64, 2_int64)
             xs(1:n) = px(poff(q):poff(q + 1) - 1) + x0; ys(1:n) = py(poff(q):poff(q + 1) - 1) + y0
             call add_poly(n, xs, ys, npoly, poff, px, py, nv)
          end if
       end select
    end do
  end subroutine make_case

  real(real64) function rnd()
    call random_number(rnd)
  end function rnd

  !> Snap-rounding output: no crossings, no overlaps, no endpoint inside another fragment.
  subroutine check_fragments()
    integer(int64) :: a, b
    integer(int32) :: kk
    do a = 1, F%n
       do b = a + 1, F%n
          kk = seg_intersect_kind(F%x1(a), F%y1(a), F%x2(a), F%y2(a), F%x1(b), F%y1(b), F%x2(b), F%y2(b))
          if (kk == K_SEG_NONE) cycle
          if (kk == K_SEG_PROPER) then
             call fail('fragments cross'); return
          end if
          if (kk == K_SEG_OVERLAP) then
             call fail('fragments overlap'); return
          end if
          if (interior_pt(a, F%x1(b), F%y1(b)) .or. interior_pt(a, F%x2(b), F%y2(b)) .or. &
              interior_pt(b, F%x1(a), F%y1(a)) .or. interior_pt(b, F%x2(a), F%y2(a))) then
             call fail('fragment endpoint inside another fragment'); return
          end if
       end do
    end do
  end subroutine check_fragments

  logical function interior_pt(a, x, y)
    integer(int64), intent(in) :: a, x, y
    interior_pt = .false.
    if ((x == F%x1(a) .and. y == F%y1(a)) .or. (x == F%x2(a) .and. y == F%y2(a))) return
    if (orient2d(F%x1(a), F%y1(a), F%x2(a), F%y2(a), x, y) /= 0) return
    interior_pt = on_closed_segment(F%x1(a), F%y1(a), F%x2(a), F%y2(a), x, y)
  end function interior_pt

  !> sign of 21*x_line(Y/21) - X for the line through (xa,ya),(xb,yb) (yb /= ya)
  integer function line_cmp(xa, ya, xb, yb, X, Y)
    integer(int64), intent(in) :: xa, ya, xb, yb, X, Y
    integer(int64) :: dx, dy, v
    dx = xb - xa; dy = yb - ya
    if (dy < 0) then
       dx = -dx; dy = -dy
    end if
    v = 21*xa*dy + (Y - 21*ya)*dx - X*dy
    line_cmp = 0
    if (v > 0) line_cmp = 1
    if (v < 0) line_cmp = -1
  end function line_cmp

  !> winding of segment set S at (X/21, Y/21); on = .true. if the point is on a segment
  subroutine winding(S, X, Y, wsum, on)
    type(FragSet), intent(in) :: S
    integer(int64), intent(in) :: X, Y
    integer(int64), intent(out) :: wsum
    logical, intent(out) :: on
    integer(int64) :: a
    integer :: c
    wsum = 0; on = .false.
    do a = 1, S%n
       if (S%y1(a) == S%y2(a)) cycle
       if (21*S%y1(a) >= Y .or. 21*S%y2(a) <= Y) cycle
       c = line_cmp(S%x1(a), S%y1(a), S%x2(a), S%y2(a), X, Y)
       if (c == 0) then
          on = .true.; return
       end if
       if (c < 0) wsum = wsum + S%w(a)
    end do
  end subroutine winding

  real(real64) function dist_edges(X, Y) result(d)
    integer(int64), intent(in) :: X, Y
    real(real64) :: qx, qy, ax, ay, bx, by, t, l2
    integer(int64) :: a
    qx = X/21.0_real64; qy = Y/21.0_real64
    d = huge(d)
    do a = 1, E%n
       ax = E%x1(a); ay = E%y1(a); bx = E%x2(a); by = E%y2(a)
       l2 = (bx - ax)**2 + (by - ay)**2
       t = max(0.0_real64, min(1.0_real64, ((qx - ax)*(bx - ax) + (qy - ay)*(by - ay))/l2))
       d = min(d, sqrt((ax + t*(bx - ax) - qx)**2 + (ay + t*(by - ay) - qy)**2))
    end do
  end function dist_edges

  subroutine check_samples()
    integer(int64) :: X, Y, ax, ay
    integer :: s, nin, c1, c2
    integer(int64) :: wsnap, worig
    logical :: on1, on2, onb
    integer(int64) :: i
    do s = 1, 400
       X = irand(-21_int64, 21*(G + 2)); Y = irand(-21_int64, 21*(G + 2))
       if (mod(X, 21_int64) == 0 .or. mod(Y, 21_int64) == 0) cycle
       call winding(F, X, Y, wsnap, on1)
       if (on1) cycle
       nin = 0; onb = .false.
       do i = 1, ntrap1
          if (21*int(t1(i)%y1, int64) >= Y .or. 21*int(t1(i)%y2, int64) <= Y) cycle
          ax = t1(i)%l%ax; ay = int(t1(i)%y1, int64) - t1(i)%l%k
          c1 = line_cmp(ax, ay, ax + t1(i)%l%dx, ay + t1(i)%l%dy, X, Y)
          ax = t1(i)%r%ax; ay = int(t1(i)%y1, int64) - t1(i)%r%k
          c2 = line_cmp(ax, ay, ax + t1(i)%r%dx, ay + t1(i)%r%dy, X, Y)
          if (c1 == 0 .or. c2 == 0) onb = .true.
          if (c1 < 0 .and. c2 > 0) nin = nin + 1
       end do
       if (onb) cycle
       tot_samples = tot_samples + 1
       if (nin > 1) then
          call fail('trapezoids overlap'); return
       end if
       if ((nin == 1) .neqv. (wsnap /= 0)) then
          call fail('trapezoids differ from the snapped winding region'); return
       end if
       call winding(E, X, Y, worig, on2)
       if (on2) cycle
       if ((nin == 1) .neqv. (worig /= 0)) then
          tot_mismatch_unsnapped = tot_mismatch_unsnapped + 1
          if (dist_edges(X, Y) > 0.75_real64) then
             call fail('snapped result moved more than 0.75 dbu from the input'); return
          end if
       end if
    end do
  end subroutine check_samples

  !> unit cells (i+1/2, j+1/2) inside the input union (Manhattan cases)
  integer(int64) function manhattan_cells() result(cnt)
    integer(int64) :: x, y
    integer(int64) :: w
    logical :: on
    cnt = 0
    if (G > 200) then
       cnt = -1; return
    end if
    do y = -1, 2*G + 2
       do x = -1, 2*G + 2
          call winding(E, 21*x + 10, 21*y + 10, w, on)       ! (x + 10/21, y + 10/21)
          if (w /= 0) cnt = cnt + 1
       end do
    end do
  end function manhattan_cells

  subroutine dump_case()
    integer(int64) :: p
    write(*, '(a,i0,a,i0)') '  polygons of case ', icase, ', G=', G
    do p = 1, npoly
       write(*, '(a,*(1x,i0))') '   ', (px(j), py(j), j = int(poff(p)), int(poff(p + 1) - 1))
    end do
  end subroutine dump_case

end program test_trapmerge
