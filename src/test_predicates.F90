! File   : test_predicates.F90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Unit test of ExactPredicatesModule against independent brute-force references.
!   * pixel_hits_segment: every segment with endpoints in [-4,4]^2 against every pixel in
!     [-5,5]^2, compared with exact rational sampling (the sample step divides every boundary
!     parameter, so sampling is exact, not approximate);
!   * seg_intersect_kind: all segment pairs from a small grid, compared with a reference built
!     from the rational intersection point and projections;
!   * orient2d / line_x_cmp_at_y / seg_intersection_rational at |c| up to 2**30-1 against
!     integer(16) (gfortran/ifx) or reduced-range int64 (nvfortran);
!   * device run of pixel_hits_segment and line_x_cmp_at_y compared with the host.
program test_predicates
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use WideIntModule
  use ExactPredicatesModule
  implicit none
#if defined(__GFORTRAN__) || defined(__INTEL_COMPILER) || defined(__INTEL_LLVM_COMPILER)
#define HAVE_INT128 1
  integer, parameter :: ik = selected_int_kind(38)
#else
  integer, parameter :: ik = int64
#endif
  integer :: nfail = 0

  call test_pixel_exhaustive()
  call test_seg_kind_exhaustive()
  call test_large_random()
  call test_device()
  if (nfail > 0) then
     print '(A,I0,A)', 'FAIL: ', nfail, ' check(s) failed'
     error stop 1
  end if
  print '(A)', 'PASS test_predicates'

contains

  subroutine expect(ok, what)
    logical, intent(in) :: ok
    character(*), intent(in) :: what
    if (.not. ok) then
       nfail = nfail + 1
       print '(A)', 'FAIL: '//what
    end if
  end subroutine expect

  !> Brute force: sample the segment at t = j/M, M a multiple of 2|dx| and 2|dy|.
  logical function ref_pixel(px, py, ax, ay, bx, by) result(hit)
    integer(int64), intent(in) :: px, py, ax, ay, bx, by
    integer(int64) :: m, j, xs, ys, dx, dy
    dx = bx - ax; dy = by - ay
    m = 8_int64 * max(1_int64, abs(dx)) * max(1_int64, abs(dy))
    hit = .false.
    do j = 0, m
       xs = 2*ax*m + 2*dx*j          ! doubled coordinate times m
       ys = 2*ay*m + 2*dy*j
       if (xs >= (2*px - 1)*m .and. xs < (2*px + 1)*m .and. &
           ys >= (2*py - 1)*m .and. ys < (2*py + 1)*m) then
          hit = .true.; return
       end if
    end do
  end function ref_pixel

  subroutine test_pixel_exhaustive()
    integer(int64) :: ax, ay, bx, by, px, py
    integer :: nbad, ncase, nhit
    nbad = 0; ncase = 0; nhit = 0
    do ax = -4, 4; do ay = -4, 4; do bx = -4, 4; do by = -4, 4
       do px = -5, 5; do py = -5, 5
          ncase = ncase + 1
          if (pixel_hits_segment(px, py, ax, ay, bx, by) .neqv. ref_pixel(px, py, ax, ay, bx, by)) then
             nbad = nbad + 1
             if (nbad <= 5) print '(A,6I4)', '  pixel mismatch px py ax ay bx by:', px, py, ax, ay, bx, by
          end if
          if (pixel_hits_segment(px, py, ax, ay, bx, by)) nhit = nhit + 1
       end do; end do
    end do; end do; end do; end do
    call expect(nbad == 0, 'pixel_hits_segment exhaustive')
    print '(A,I0,A,I0,A,I0)', 'INFO: pixel cases ', ncase, ' hits ', nhit, ' mismatches ', nbad
  end subroutine test_pixel_exhaustive

  !> Reference classification: rational intersection for crossing lines; projections for parallel.
  integer(int32) function ref_kind(p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y) result(k)
    integer(int64), intent(in) :: p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y
    integer(int64) :: rx, ry, sx, sy, den, tn, un, lo, hi
    logical :: pdeg, qdeg
    rx = p2x - p1x; ry = p2y - p1y; sx = q2x - q1x; sy = q2y - q1y
    pdeg = rx == 0 .and. ry == 0; qdeg = sx == 0 .and. sy == 0
    k = K_SEG_NONE
    if (pdeg .and. qdeg) then
       if (p1x == q1x .and. p1y == q1y) k = K_SEG_TOUCH
       return
    end if
    if (pdeg .or. qdeg) then
       if (pdeg) then
          if (on_seg(q1x, q1y, q2x, q2y, p1x, p1y)) k = K_SEG_TOUCH
       else
          if (on_seg(p1x, p1y, p2x, p2y, q1x, q1y)) k = K_SEG_TOUCH
       end if
       return
    end if
    den = rx*sy - ry*sx
    if (den /= 0) then
       ! p1 + t r = q1 + u s, t = tn/den, u = un/den
       tn = (q1x - p1x)*sy - (q1y - p1y)*sx
       un = (q1x - p1x)*ry - (q1y - p1y)*rx
       if (den < 0) then
          den = -den; tn = -tn; un = -un
       end if
       if (tn < 0 .or. tn > den .or. un < 0 .or. un > den) return
       if (tn > 0 .and. tn < den .and. un > 0 .and. un < den) then
          k = K_SEG_PROPER
       else
          k = K_SEG_TOUCH
       end if
       return
    end if
    ! parallel: collinear?
    if ((q1x - p1x)*ry - (q1y - p1y)*rx /= 0) return
    ! project on r: parameters of q1, q2 along p as multiples of r.r
    tn = (q1x - p1x)*rx + (q1y - p1y)*ry
    un = (q2x - p1x)*rx + (q2y - p1y)*ry
    lo = max(0_int64, min(tn, un))
    hi = min(rx*rx + ry*ry, max(tn, un))
    if (lo < hi) then
       k = K_SEG_OVERLAP
    else if (lo == hi) then
       k = K_SEG_TOUCH
    end if
  end function ref_kind

  logical function on_seg(ax, ay, bx, by, cx, cy)
    integer(int64), intent(in) :: ax, ay, bx, by, cx, cy
    integer(int64) :: d
    on_seg = .false.
    if ((bx - ax)*(cy - ay) - (by - ay)*(cx - ax) /= 0) return
    d = (cx - ax)*(bx - ax) + (cy - ay)*(by - ay)
    on_seg = d >= 0 .and. d <= (bx - ax)**2 + (by - ay)**2
  end function on_seg

  subroutine test_seg_kind_exhaustive()
    integer(int64) :: pts(2, 16), p1(2), p2(2), q1(2), q2(2)
    integer :: i, j, k, l, n, nbad, ncase, cnt(0:3), kk
    n = 0
    do i = 0, 3; do j = 0, 3
       n = n + 1; pts(:, n) = [int(i, int64), int(j, int64)]
    end do; end do
    ! also a few off-lattice-direction points
    nbad = 0; ncase = 0; cnt = 0
    do i = 1, 16; do j = 1, 16; do k = 1, 16; do l = 1, 16
       p1 = pts(:, i); p2 = pts(:, j); q1 = pts(:, k); q2 = pts(:, l)
       ncase = ncase + 1
       kk = seg_intersect_kind(p1(1), p1(2), p2(1), p2(2), q1(1), q1(2), q2(1), q2(2))
       cnt(kk) = cnt(kk) + 1
       if (kk /= ref_kind(p1(1), p1(2), p2(1), p2(2), q1(1), q1(2), q2(1), q2(2))) then
          nbad = nbad + 1
          if (nbad <= 5) print '(A,8I3,2I2)', '  kind mismatch:', p1, p2, q1, q2, kk, &
               ref_kind(p1(1), p1(2), p2(1), p2(2), q1(1), q1(2), q2(1), q2(2))
       end if
    end do; end do; end do; end do
    call expect(nbad == 0, 'seg_intersect_kind exhaustive 4x4')
    print '(A,I0,A,4(I0,1X),A,I0)', 'INFO: seg pairs ', ncase, ' none/proper/touch/overlap ', cnt, &
         ' mismatches ', nbad
  end subroutine test_seg_kind_exhaustive

  function rnd(lim) result(v)
    integer(int64), intent(in) :: lim
    integer(int64) :: v
    real(real64) :: u, s
    call random_number(u); call random_number(s)
    v = int((2.0_real64*u - 1.0_real64) * real(lim, real64), int64)
    if (s < 0.05_real64) v = sign(lim, v)
  end function rnd

  subroutine test_large_random()
    integer :: it, nbad
    integer(int64) :: lim, ax, ay, bx, by, cx, cy, adx, ady, bdx, bdy, y, g
    integer(int64) :: p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y, den
    integer(ik) :: ref, na, nb
    integer(int32) :: c
    type(i128) :: xn, yn
    logical :: ok
#ifdef HAVE_INT128
    lim = K_SAFE_COORD
#else
    lim = 1000000_int64           ! int64 reference products must not overflow
#endif
    nbad = 0
    do it = 1, 200000
       ax = rnd(lim); ay = rnd(lim); bx = rnd(lim); by = rnd(lim); cx = rnd(lim); cy = rnd(lim)
       ref = int(bx - ax, ik)*int(cy - ay, ik) - int(by - ay, ik)*int(cx - ax, ik)
       if (orient2d(ax, ay, bx, by, cx, cy) /= int(sign(1_ik, ref), int32) .and. ref /= 0) nbad = nbad + 1
       if (ref == 0 .and. orient2d(ax, ay, bx, by, cx, cy) /= 0) nbad = nbad + 1
       ! nearly collinear triples: c on the line a-b, nudged by one unit
       g = rnd(1000_int64)
       if (abs(ax) < lim/2 .and. abs(ay) < lim/2 .and. abs(bx) < lim/4 .and. abs(by) < lim/4) then
          cx = ax + 2*(bx/2 - ax/2); cy = ay + 2*(by/2 - ay/2) + mod(g, 2_int64)
          ref = int(bx - ax, ik)*int(cy - ay, ik) - int(by - ay, ik)*int(cx - ax, ik)
          c = orient2d(ax, ay, bx, by, cx, cy)
          if (ref == 0 .neqv. c == 0) nbad = nbad + 1
       end if
       ! line_x_cmp_at_y: lines through (ax,ay) dir (adx,ady) and (bx,by) dir (bdx,bdy)
       adx = rnd(lim); ady = abs(rnd(lim)) + 1; bdx = rnd(lim); bdy = abs(rnd(lim)) + 1
       y = rnd(lim)
       if (it <= 1000) then           ! exactly equal x: B = A shifted along its own line
          bx = ax + adx; by = ay + ady; bdx = adx; bdy = ady
       end if
#ifndef HAVE_INT128
       adx = mod(adx, 1000_int64); ady = mod(ady, 1000_int64) + 1; bdx = mod(bdx, 1000_int64)
       bdy = mod(bdy, 1000_int64) + 1
       if (it <= 1000) then
          bdx = adx; bdy = ady; bx = ax + adx; by = ay + ady
       end if
#endif
       na = (int(ax, ik)*ady + int(y - ay, ik)*adx) * bdy
       nb = (int(bx, ik)*bdy + int(y - by, ik)*bdx) * ady
       c = line_x_cmp_at_y(ax, ay, adx, ady, bx, by, bdx, bdy, y)
       if (c /= merge(-1, merge(0, 1, na == nb), na < nb)) nbad = nbad + 1
       ! rational intersection: substitute back exactly
       p1x = rnd(lim); p1y = rnd(lim); p2x = rnd(lim); p2y = rnd(lim)
       q1x = rnd(lim); q1y = rnd(lim); q2x = rnd(lim); q2y = rnd(lim)
#ifndef HAVE_INT128
       p1x = mod(p1x, 3000_int64); p1y = mod(p1y, 3000_int64); p2x = mod(p2x, 3000_int64)
       p2y = mod(p2y, 3000_int64); q1x = mod(q1x, 3000_int64); q1y = mod(q1y, 3000_int64)
       q2x = mod(q2x, 3000_int64); q2y = mod(q2y, 3000_int64)
#endif
       call seg_intersection_rational(p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y, xn, yn, den, ok)
       if (ok) then
          ! (X/den, Y/den) on both lines:  (p2-p1) x ((X,Y) - p1*den) == 0, same for q
          if (cross_on_line(p1x, p1y, p2x, p2y, xn, yn, den) /= 0 .or. &
              cross_on_line(q1x, q1y, q2x, q2y, xn, yn, den) /= 0 .or. den <= 0) nbad = nbad + 1
       end if
    end do
    call expect(nbad == 0, 'large-coordinate random predicates')
    print '(A,I0,A,I0)', 'INFO: random large cases 200000 (|c| <= ', lim, '), mismatches ', nbad
  end subroutine test_large_random

  !> (b - a) x (P - a*den) with P = (X, Y): uses i128 throughout.
  integer(int32) function cross_on_line(ax, ay, bx, by, x, y, den) result(s)
    integer(int64), intent(in) :: ax, ay, bx, by, den
    type(i128), intent(in) :: x, y
    type(i128) :: ux, uy
    ux = i128_sub(x, i128_mul64(ax, den)); uy = i128_sub(y, i128_mul64(ay, den))
    s = i128_cmp(i128_mul_i64(uy, bx - ax), i128_mul_i64(ux, by - ay))
  end function cross_on_line

  subroutine test_device()
    integer, parameter :: n = 20000
    integer(int64) :: s(10, n)
    logical :: hh(n), dh(n)
    integer(int32) :: hc(n), dc(n)
    integer :: i, j
    do i = 1, n
       do j = 1, 6
          s(j, i) = rnd(40_int64)
       end do
       do j = 7, 10
          s(j, i) = rnd(K_SAFE_COORD)
       end do
    end do
    do i = 1, n
       hh(i) = pixel_hits_segment(s(1,i), s(2,i), s(3,i), s(4,i), s(5,i), s(6,i))
       hc(i) = line_x_cmp_at_y(s(7,i), s(8,i), s(3,i), abs(s(4,i)) + 1, s(9,i), s(10,i), &
                               s(5,i), abs(s(6,i)) + 1, s(1,i))
    end do
    !$omp target teams distribute parallel do map(to: s) map(from: dh, dc)
    do i = 1, n
       dh(i) = pixel_hits_segment(s(1,i), s(2,i), s(3,i), s(4,i), s(5,i), s(6,i))
       dc(i) = line_x_cmp_at_y(s(7,i), s(8,i), s(3,i), abs(s(4,i)) + 1, s(9,i), s(10,i), &
                               s(5,i), abs(s(6,i)) + 1, s(1,i))
    end do
    call expect(all(hh .eqv. dh) .and. all(hc == dc), 'target region predicates equal host')
    if (count(hh .neqv. dh) + count(hc /= dc) > 0) then
       print '(A,I0,A,I0)', '  pixel mismatches ', count(hh .neqv. dh), ' xcmp mismatches ', count(hc /= dc)
       do i = 1, n
          if ((hh(i) .neqv. dh(i)) .or. hc(i) /= dc(i)) then
             print '(A,10I12,2L2,2I3)', '  ', s(:, i), hh(i), dh(i), hc(i), dc(i); exit
          end if
       end do
    end if
    print '(A,I0,A)', 'INFO: device predicate check on ', n, ' cases'
  end subroutine test_device

end program test_predicates
