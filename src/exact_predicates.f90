! File   : exact_predicates.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Exact geometric predicates on integer coordinates for the any-angle (trapezoid,
!          snap-rounding) engine. Every result is exact and identical on CPU and GPU: integer
!          arithmetic only, 128-bit intermediates from WideIntModule where int64 does not suffice.
!
! Coordinate range (invariant I0): |x|, |y| <= K_SAFE_COORD = 2**30 - 1. Then every difference
! fits 31 bits plus sign, an orientation determinant fits int64, and intersection numerators
! need at most about 94 bits.
!
! Public predicates (all elemental pure, `declare target`):
!   orient2d                   sign of the turn a -> b -> c (+1 left / CCW, 0 collinear, -1 right)
!   seg_intersect_kind         none / proper crossing / touching / collinear overlap
!   seg_intersection_rational  intersection of two non-parallel lines as (xn/den, yn/den)
!   line_x_cmp_at_y            compare x of two non-horizontal lattice lines at an integer y
!   pixel_hits_segment         does a segment meet the half-open hot pixel of a grid point
!   in_safe_range              coordinate check for I0
module ExactPredicatesModule
  use, intrinsic :: iso_fortran_env, only: int32, int64
  use WideIntModule, only: i128, i128_add, i128_sub, i128_cmp, i128_mul64, i128_mul_i64, &
                           i128_from_int64, i128_sign
  implicit none
  private

  integer(int64), parameter, public :: K_SAFE_COORD = 1073741823_int64   !< 2**30 - 1 (I0)

  ! seg_intersect_kind results
  integer(int32), parameter, public :: K_SEG_NONE = 0     !< disjoint
  integer(int32), parameter, public :: K_SEG_PROPER = 1   !< interiors cross in one point
  integer(int32), parameter, public :: K_SEG_TOUCH = 2    !< one common point, an endpoint of either
  integer(int32), parameter, public :: K_SEG_OVERLAP = 3  !< collinear, common part of positive length

  public :: orient2d, orient2d_det, seg_intersect_kind, seg_intersection_rational
  public :: line_x_cmp_at_y, pixel_hits_segment, in_safe_range, on_closed_segment

contains

  elemental pure logical function in_safe_range(x, y)
    !$omp declare target
    integer(int64), intent(in) :: x, y
    in_safe_range = abs(x) <= K_SAFE_COORD .and. abs(y) <= K_SAFE_COORD
  end function in_safe_range

  !> (b - a) x (c - a), exact in int64 under I0.
  elemental pure function orient2d_det(ax, ay, bx, by, cx, cy) result(d)
    !$omp declare target
    integer(int64), intent(in) :: ax, ay, bx, by, cx, cy
    integer(int64) :: d
    d = (bx - ax)*(cy - ay) - (by - ay)*(cx - ax)
  end function orient2d_det

  elemental pure function orient2d(ax, ay, bx, by, cx, cy) result(s)
    !$omp declare target
    integer(int64), intent(in) :: ax, ay, bx, by, cx, cy
    integer(int32) :: s
    integer(int64) :: d
    d = orient2d_det(ax, ay, bx, by, cx, cy)
    s = int(sign(1_int64, d), int32)
    if (d == 0_int64) s = 0_int32
  end function orient2d

  !> c lies on the closed segment a-b (c known to be collinear with a, b).
  elemental pure logical function on_closed_segment(ax, ay, bx, by, cx, cy)
    !$omp declare target
    integer(int64), intent(in) :: ax, ay, bx, by, cx, cy
    on_closed_segment = cx >= min(ax, bx) .and. cx <= max(ax, bx) .and. &
                        cy >= min(ay, by) .and. cy <= max(ay, by)
  end function on_closed_segment

  !> Relation of closed segments p1-p2 and q1-q2 (K_SEG_*). Zero-length segments are points.
  elemental pure function seg_intersect_kind(p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y) result(k)
    !$omp declare target
    integer(int64), intent(in) :: p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y
    integer(int32) :: k
    integer(int32) :: o1, o2, o3, o4
    integer(int64) :: lo, hi
    o1 = orient2d(p1x, p1y, p2x, p2y, q1x, q1y)
    o2 = orient2d(p1x, p1y, p2x, p2y, q2x, q2y)
    o3 = orient2d(q1x, q1y, q2x, q2y, p1x, p1y)
    o4 = orient2d(q1x, q1y, q2x, q2y, p2x, p2y)
    if (o1 == 0 .and. o2 == 0 .and. o3 == 0 .and. o4 == 0) then
       ! collinear (or degenerate): overlap of the projections on the dominant axis
       if (max(abs(p2x - p1x), abs(q2x - q1x)) >= max(abs(p2y - p1y), abs(q2y - q1y))) then
          lo = max(min(p1x, p2x), min(q1x, q2x)); hi = min(max(p1x, p2x), max(q1x, q2x))
       else
          lo = max(min(p1y, p2y), min(q1y, q2y)); hi = min(max(p1y, p2y), max(q1y, q2y))
       end if
       if (lo < hi) then
          k = K_SEG_OVERLAP
       else if (lo == hi) then
          ! one common projected value: a real common point only if the segments meet there
          k = K_SEG_NONE
          if (on_closed_segment(p1x, p1y, p2x, p2y, q1x, q1y) .or. &
              on_closed_segment(p1x, p1y, p2x, p2y, q2x, q2y) .or. &
              on_closed_segment(q1x, q1y, q2x, q2y, p1x, p1y) .or. &
              on_closed_segment(q1x, q1y, q2x, q2y, p2x, p2y)) k = K_SEG_TOUCH
       else
          k = K_SEG_NONE
       end if
       return
    end if
    if (o1*o2 < 0 .and. o3*o4 < 0) then
       k = K_SEG_PROPER
    else if ((o1 == 0 .and. on_closed_segment(p1x, p1y, p2x, p2y, q1x, q1y)) .or. &
             (o2 == 0 .and. on_closed_segment(p1x, p1y, p2x, p2y, q2x, q2y)) .or. &
             (o3 == 0 .and. on_closed_segment(q1x, q1y, q2x, q2y, p1x, p1y)) .or. &
             (o4 == 0 .and. on_closed_segment(q1x, q1y, q2x, q2y, p2x, p2y))) then
       k = K_SEG_TOUCH
    else
       k = K_SEG_NONE
    end if
  end function seg_intersect_kind

  !> Intersection of the lines through p1-p2 and q1-q2 as (xn/den, yn/den) with den > 0.
  !> ok = .false. for parallel (or degenerate) lines.
  elemental pure subroutine seg_intersection_rational(p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y, &
                                                     xn, yn, den, ok)
    !$omp declare target
    integer(int64), intent(in) :: p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y
    type(i128), intent(out) :: xn, yn
    integer(int64), intent(out) :: den
    logical, intent(out) :: ok
    integer(int64) :: rx, ry, sx, sy, tn
    rx = p2x - p1x; ry = p2y - p1y
    sx = q2x - q1x; sy = q2y - q1y
    den = rx*sy - ry*sx
    tn = (q1x - p1x)*sy - (q1y - p1y)*sx
    ok = den /= 0_int64
    if (.not. ok) then
       xn = i128_from_int64(0_int64); yn = xn; den = 1_int64
       return
    end if
    if (den < 0_int64) then
       den = -den; tn = -tn
    end if
    ! p1 + (tn/den) * r
    xn = i128_add(i128_mul64(p1x, den), i128_mul64(tn, rx))
    yn = i128_add(i128_mul64(p1y, den), i128_mul64(tn, ry))
  end subroutine seg_intersection_rational

  !> Compare x(y) of line A (through (ax,ay) with direction (adx,ady), ady > 0) and line B at
  !> integer y. Returns -1, 0, +1 as xA < xB, xA == xB, xA > xB.
  elemental pure function line_x_cmp_at_y(ax, ay, adx, ady, bx, by, bdx, bdy, y) result(c)
    !$omp declare target
    integer(int64), intent(in) :: ax, ay, adx, ady, bx, by, bdx, bdy, y
    integer(int32) :: c
    type(i128) :: na, nb
    ! xA = (ax*ady + (y-ay)*adx) / ady ; compare na*bdy with nb*ady
    na = i128_add(i128_mul64(ax, ady), i128_mul64(y - ay, adx))
    nb = i128_add(i128_mul64(bx, bdy), i128_mul64(y - by, bdx))
    c = i128_cmp(i128_mul_i64(na, bdy), i128_mul_i64(nb, ady))
  end function line_x_cmp_at_y

  ! ---------------------------------------------------------------------------------------
  ! Hot pixels. The hot pixel of grid point (px, py) is the half-open unit square
  ! [px-1/2, px+1/2) x [py-1/2, py+1/2): the pixels partition the plane. The test runs in
  ! doubled coordinates, where segment endpoints are even and pixel edges odd, so a segment
  ! can never lie along a pixel edge.
  ! ---------------------------------------------------------------------------------------

  !> Does the closed segment a-b meet the half-open hot pixel of (px, py)?
  elemental pure logical function pixel_hits_segment(px, py, ax, ay, bx, by)
    !$omp declare target
    integer(int64), intent(in) :: px, py, ax, ay, bx, by
    integer(int64) :: x0, x1, y0, y1, sx, sy, dx, dy
    integer(int64) :: t0n, t0d, t1n, t1d     ! parameter interval [t0n/t0d, t1n/t1d], d > 0
    integer(int64) :: an, bn, dd
    x0 = 2*px - 1; x1 = 2*px + 1; y0 = 2*py - 1; y1 = 2*py + 1
    sx = 2*ax; sy = 2*ay; dx = 2*(bx - ax); dy = 2*(by - ay)
    pixel_hits_segment = .false.
    ! quick reject on the bounding boxes (closed square)
    if (max(sx, sx + dx) < x0 .or. min(sx, sx + dx) > x1) return
    if (max(sy, sy + dy) < y0 .or. min(sy, sy + dy) > y1) return
    ! Liang-Barsky clip to the closed square, written inline: nvfortran 25.9 miscompiled the
    ! earlier version with an elemental intent(inout) helper inside target regions. A zero
    ! component needs no clip, because the bounding-box test above already holds.
    t0n = 0; t0d = 1; t1n = 1; t1d = 1
    if (dx /= 0_int64) then
       dd = abs(dx)
       an = merge(x0 - sx, sx - x1, dx > 0_int64); bn = merge(x1 - sx, sx - x0, dx > 0_int64)
       if (frac_cmp(an, dd, t0n, t0d) > 0) then
          t0n = an; t0d = dd
       end if
       if (frac_cmp(bn, dd, t1n, t1d) < 0) then
          t1n = bn; t1d = dd
       end if
    end if
    if (dy /= 0_int64) then
       dd = abs(dy)
       an = merge(y0 - sy, sy - y1, dy > 0_int64); bn = merge(y1 - sy, sy - y0, dy > 0_int64)
       if (frac_cmp(an, dd, t0n, t0d) > 0) then
          t0n = an; t0d = dd
       end if
       if (frac_cmp(bn, dd, t1n, t1d) < 0) then
          t1n = bn; t1d = dd
       end if
    end if
    if (frac_cmp(t0n, t0d, t1n, t1d) > 0) return
    if (frac_cmp(t0n, t0d, t1n, t1d) < 0) then
       ! a piece of positive length inside the closed square; it cannot lie on the excluded
       ! edges (x = x1 or y = y1 lines are odd, endpoints even), so it meets the half-open pixel
       pixel_hits_segment = .true.
    else
       ! a single point P = s + t0*d: in the pixel unless on the right or top edge
       pixel_hits_segment = point_lt(sx, dx, t0n, t0d, x1) .and. point_lt(sy, dy, t0n, t0d, y1)
    end if
  end function pixel_hits_segment

  !> Compare an/ad with bn/bd (ad, bd > 0), exactly.
  elemental pure function frac_cmp(an, ad, bn, bd) result(c)
    !$omp declare target
    integer(int64), intent(in) :: an, ad, bn, bd
    integer(int32) :: c
    c = i128_cmp(i128_mul64(an, bd), i128_mul64(bn, ad))
  end function frac_cmp

  !> s + (tn/td)*d < bound, exactly (td > 0).
  elemental pure logical function point_lt(s, d, tn, td, bound)
    !$omp declare target
    integer(int64), intent(in) :: s, d, tn, td, bound
    ! s*td + tn*d < bound*td
    point_lt = i128_sign(i128_sub(i128_add(i128_mul64(s, td), i128_mul64(tn, d)), &
                                  i128_mul64(bound, td))) < 0
  end function point_lt

end module ExactPredicatesModule
