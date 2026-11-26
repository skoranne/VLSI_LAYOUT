! File   : trapezoid.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Exact horizontal trapezoids for any-angle geometry in MAGPARSER.
!
! A layer holds rectilinear pieces as Box (layer_boxes) and everything else as interior-disjoint
! horizontal trapezoids (layer_traps). A trapezoid spans integer rows y1 < y2. Its left and right
! sides are pieces of *lattice lines*: lines through two integer points. A side is stored as the
! line's primitive direction (dx, dy), with dy > 0 and gcd(|dx|, dy) = 1, plus an anchor lattice
! point (ax, ay) with ay = y1 - k, 0 <= k < dy (the highest lattice point of the line at or below
! y1). Then:
!   * x on the side at any integer y is the exact rational  (ax*dy + (y - ay)*dx) / dy;
!   * clipping a trapezoid at an integer y only re-anchors k, so repeated operations never
!     round anything and never drift;
!   * the encoding is canonical (invariant I3): the same line gives bit-identical sides.
! A vertical side is dx = 0, dy = 1, k = 0, ax = x. A trapezoid with two vertical sides is a box.
!
! Ranges (invariant I0): coordinates |c| <= 2**30-1, and every anchor must fit int32. A failure is
! reported through `status`, never wrapped. Exact x numerators can reach 2**63, so they are
! computed as i128 (WideIntModule).
!
! Layer area: twice the area of a set of trapezoids is an exact rational. When the boundary runs
! between lattice points (invariant I1, true after snap rounding), it is an integer; trap_area2
! computes it exactly and reports whether it is an integer.
module TrapezoidModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use WideIntModule
  use ExactPredicatesModule, only: K_SAFE_COORD
  implicit none
  private

  !> One side of a trapezoid: the lattice line through (ax, y1 - k) with direction (dx, dy).
  type, public :: TrapSide
     integer(int32) :: ax = 0, k = 0, dx = 0, dy = 1
  end type TrapSide

  !> Horizontal trapezoid y1 <= y <= y2 between the sides l (left) and r (right).
  type, public :: Trapezoid
     integer(int32) :: y1 = 0, y2 = 0
     type(TrapSide) :: l, r
  end type Trapezoid

  integer, parameter, public :: K_TRAP_OK = 0, K_TRAP_HORIZONTAL = 1, K_TRAP_RANGE = 2, &
                                K_TRAP_EMPTY = 3

  public :: side_vertical, side_from_points, side_reanchor, side_x_num, side_x_floor, side_x_ceil
  public :: side_x_cmp, side_equal
  public :: trap_make, trap_from_box_coords, trap_is_box, trap_mbr, trap_clip_y, trap_valid
  public :: trap_less, trap_equal, trap_hash, trap_area2, trap_sort, trap_overlap_positive
  public :: SlopeTab, stab_init, stab_add, slope_key

  !> Slope -> accumulated numerator table (trap_area2).
  type :: SlopeTab
     integer(int64) :: cap = 0, n = 0
     integer(int64), allocatable :: key(:), dy(:)
     type(i128), allocatable :: acc(:)
     logical, allocatable :: used(:)
  end type SlopeTab

contains

  ! ------------------------------------------------------------------ sides

  elemental pure function side_vertical(x) result(s)
    !$omp declare target
    integer(int32), intent(in) :: x
    type(TrapSide) :: s
    s = TrapSide(x, 0_int32, 0_int32, 1_int32)
  end function side_vertical

  elemental pure function gcd64(a, b) result(g)
    !$omp declare target
    integer(int64), intent(in) :: a, b
    integer(int64) :: g, x, y, t
    x = abs(a); y = abs(b)
    do while (y /= 0_int64)
       t = mod(x, y); x = y; y = t
    end do
    g = x
  end function gcd64

  !> Floor of a / b for b > 0.
  elemental pure function floor_div(a, b) result(q)
    !$omp declare target
    integer(int64), intent(in) :: a, b
    integer(int64) :: q
    q = a / b
    if (mod(a, b) /= 0_int64 .and. a < 0_int64) q = q - 1_int64
  end function floor_div

  !> Side of a trapezoid with lower row y1, on the line through (xa, ya) and (xb, yb).
  !> status: K_TRAP_OK, K_TRAP_HORIZONTAL (ya == yb) or K_TRAP_RANGE (anchor outside int32).
  elemental pure subroutine side_from_points(xa, ya, xb, yb, y1, s, status)
    !$omp declare target
    integer(int64), intent(in) :: xa, ya, xb, yb, y1
    type(TrapSide), intent(out) :: s
    integer, intent(out) :: status
    integer(int64) :: dx, dy, g, t, ax
    s = TrapSide()
    dx = xb - xa; dy = yb - ya
    if (dy == 0_int64) then
       status = K_TRAP_HORIZONTAL; return
    end if
    if (dy < 0_int64) then
       dx = -dx; dy = -dy
    end if
    g = gcd64(dx, dy)
    dx = dx / g; dy = dy / g
    t = floor_div(y1 - ya, dy)                       ! anchor: highest lattice point at or below y1
    ax = xa + t*dx
    if (abs(ax) > int(huge(1_int32), int64) .or. abs(dx) > int(huge(1_int32), int64) .or. &
        dy > int(huge(1_int32), int64)) then
       status = K_TRAP_RANGE; return
    end if
    s = TrapSide(int(ax, int32), int(y1 - (ya + t*dy), int32), int(dx, int32), int(dy, int32))
    status = K_TRAP_OK
  end subroutine side_from_points

  !> The same line re-anchored for a trapezoid whose lower row moves from y1_old to y1_new.
  elemental pure subroutine side_reanchor(s, y1_old, y1_new, r, status)
    !$omp declare target
    type(TrapSide), intent(in) :: s
    integer(int64), intent(in) :: y1_old, y1_new
    type(TrapSide), intent(out) :: r
    integer, intent(out) :: status
    integer(int64) :: ay, t, ax
    ay = y1_old - s%k
    t = floor_div(y1_new - ay, int(s%dy, int64))
    ax = s%ax + t*s%dx
    r = s
    if (abs(ax) > int(huge(1_int32), int64)) then
       status = K_TRAP_RANGE; return
    end if
    r%ax = int(ax, int32)
    r%k = int(y1_new - (ay + t*s%dy), int32)
    status = K_TRAP_OK
  end subroutine side_reanchor

  !> Numerator of x on side s (anchored at row y1) at row y; x = side_x_num / s%dy.
  elemental pure function side_x_num(s, y1, y) result(xn)
    !$omp declare target
    type(TrapSide), intent(in) :: s
    integer(int64), intent(in) :: y1, y
    type(i128) :: xn
    xn = i128_add(i128_mul64(int(s%ax, int64), int(s%dy, int64)), &
                  i128_mul64(y - y1 + s%k, int(s%dx, int64)))
  end function side_x_num

  elemental pure function side_x_floor(s, y1, y) result(x)
    !$omp declare target
    type(TrapSide), intent(in) :: s
    integer(int64), intent(in) :: y1, y
    integer(int64) :: x, r
    call i128_divmod(side_x_num(s, y1, y), int(s%dy, int64), x, r)
  end function side_x_floor

  elemental pure function side_x_ceil(s, y1, y) result(x)
    !$omp declare target
    type(TrapSide), intent(in) :: s
    integer(int64), intent(in) :: y1, y
    integer(int64) :: x, r
    call i128_divmod(side_x_num(s, y1, y), int(s%dy, int64), x, r)
    if (r /= 0_int64) x = x + 1_int64
  end function side_x_ceil

  !> Compare x of side a (anchored at y1a) with x of side b (anchored at y1b) at row y.
  elemental pure function side_x_cmp(a, y1a, b, y1b, y) result(c)
    !$omp declare target
    type(TrapSide), intent(in) :: a, b
    integer(int64), intent(in) :: y1a, y1b, y
    integer(int32) :: c
    c = i128_cmp(i128_mul_i64(side_x_num(a, y1a, y), int(b%dy, int64)), &
                 i128_mul_i64(side_x_num(b, y1b, y), int(a%dy, int64)))
  end function side_x_cmp

  elemental pure logical function side_equal(a, b)
    !$omp declare target
    type(TrapSide), intent(in) :: a, b
    side_equal = a%ax == b%ax .and. a%k == b%k .and. a%dx == b%dx .and. a%dy == b%dy
  end function side_equal

  ! ------------------------------------------------------------------ trapezoids

  !> Trapezoid on rows y1..y2 between the lines (lxa,lya)-(lxb,lyb) and (rxa,rya)-(rxb,ryb).
  elemental pure subroutine trap_make(y1, y2, lxa, lya, lxb, lyb, rxa, rya, rxb, ryb, t, status)
    !$omp declare target
    integer(int64), intent(in) :: y1, y2, lxa, lya, lxb, lyb, rxa, rya, rxb, ryb
    type(Trapezoid), intent(out) :: t
    integer, intent(out) :: status
    integer :: st2
    t = Trapezoid()
    if (y2 <= y1) then
       status = K_TRAP_EMPTY; return
    end if
    if (max(abs(y1), abs(y2), abs(lxa), abs(lya), abs(lxb), abs(lyb), abs(rxa), abs(rya), &
            abs(rxb), abs(ryb)) > K_SAFE_COORD) then
       status = K_TRAP_RANGE; return
    end if
    t%y1 = int(y1, int32); t%y2 = int(y2, int32)
    call side_from_points(lxa, lya, lxb, lyb, y1, t%l, status)
    call side_from_points(rxa, rya, rxb, ryb, y1, t%r, st2)
    if (status == K_TRAP_OK) status = st2
    if (status == K_TRAP_OK .and. .not. trap_valid(t)) status = K_TRAP_EMPTY
  end subroutine trap_make

  elemental pure function trap_from_box_coords(x1, y1, x2, y2) result(t)
    !$omp declare target
    integer(int32), intent(in) :: x1, y1, x2, y2
    type(Trapezoid) :: t
    t%y1 = y1; t%y2 = y2
    t%l = side_vertical(x1); t%r = side_vertical(x2)
  end function trap_from_box_coords

  elemental pure logical function trap_is_box(t)
    !$omp declare target
    type(Trapezoid), intent(in) :: t
    trap_is_box = t%l%dx == 0 .and. t%r%dx == 0
  end function trap_is_box

  !> Integer bounding box (floor of the left side, ceiling of the right side).
  elemental pure subroutine trap_mbr(t, x1, y1, x2, y2)
    !$omp declare target
    type(Trapezoid), intent(in) :: t
    integer(int64), intent(out) :: x1, y1, x2, y2
    integer(int64) :: ya, yb
    ya = t%y1; yb = t%y2
    x1 = min(side_x_floor(t%l, ya, ya), side_x_floor(t%l, ya, yb))
    x2 = max(side_x_ceil(t%r, ya, ya), side_x_ceil(t%r, ya, yb))
    y1 = ya; y2 = yb
  end subroutine trap_mbr

  !> Clip t to rows ylo..yhi; status = K_TRAP_EMPTY when nothing remains. Exact.
  elemental pure subroutine trap_clip_y(t, ylo, yhi, c, status)
    !$omp declare target
    type(Trapezoid), intent(in) :: t
    integer(int64), intent(in) :: ylo, yhi
    type(Trapezoid), intent(out) :: c
    integer, intent(out) :: status
    integer(int64) :: a, b
    integer :: st2
    c = t
    a = max(int(t%y1, int64), ylo); b = min(int(t%y2, int64), yhi)
    if (b <= a) then
       status = K_TRAP_EMPTY; return
    end if
    call side_reanchor(t%l, int(t%y1, int64), a, c%l, status)
    call side_reanchor(t%r, int(t%y1, int64), a, c%r, st2)
    if (status == K_TRAP_OK) status = st2
    c%y1 = int(a, int32); c%y2 = int(b, int32)
  end subroutine trap_clip_y

  !> Well-formed: y1 < y2, canonical sides, left <= right at both rows, positive area.
  elemental pure logical function trap_valid(t)
    !$omp declare target
    type(Trapezoid), intent(in) :: t
    integer(int32) :: c1, c2
    integer(int64) :: y1
    trap_valid = .false.
    if (t%y2 <= t%y1) return
    if (.not. (side_ok(t%l) .and. side_ok(t%r))) return
    y1 = t%y1
    c1 = side_x_cmp(t%l, y1, t%r, y1, y1)
    c2 = side_x_cmp(t%l, y1, t%r, y1, int(t%y2, int64))
    trap_valid = c1 <= 0 .and. c2 <= 0 .and. (c1 < 0 .or. c2 < 0)
  end function trap_valid

  elemental pure logical function side_ok(s)
    !$omp declare target
    type(TrapSide), intent(in) :: s
    side_ok = s%dy > 0 .and. s%k >= 0 .and. s%k < s%dy .and. &
              gcd64(int(s%dx, int64), int(s%dy, int64)) == 1_int64
    if (s%dx == 0) side_ok = s%dy == 1 .and. s%k == 0
  end function side_ok

  !> Total order on the canonical encoding (deterministic; not a geometric order).
  elemental pure logical function trap_less(a, b)
    !$omp declare target
    type(Trapezoid), intent(in) :: a, b
    integer(int32) :: ka(10), kb(10)
    integer :: i
    ka = [a%y1, a%l%ax, a%l%k, a%l%dx, a%l%dy, a%y2, a%r%ax, a%r%k, a%r%dx, a%r%dy]
    kb = [b%y1, b%l%ax, b%l%k, b%l%dx, b%l%dy, b%y2, b%r%ax, b%r%k, b%r%dx, b%r%dy]
    trap_less = .false.
    do i = 1, 10
       if (ka(i) /= kb(i)) then
          trap_less = ka(i) < kb(i); return
       end if
    end do
  end function trap_less

  elemental pure logical function trap_equal(a, b)
    !$omp declare target
    type(Trapezoid), intent(in) :: a, b
    trap_equal = a%y1 == b%y1 .and. a%y2 == b%y2 .and. side_equal(a%l, b%l) .and. side_equal(a%r, b%r)
  end function trap_equal

  !> 32-bit hash of the canonical encoding (order-independent digests sum/xor it).
  elemental pure function trap_hash(t) result(h)
    !$omp declare target
    type(Trapezoid), intent(in) :: t
    integer(int64) :: h
    integer(int64), parameter :: M32 = 4294967295_int64
    integer(int64) :: v(10)
    integer :: i
    v = int([t%y1, t%y2, t%l%ax, t%l%k, t%l%dx, t%l%dy, t%r%ax, t%r%k, t%r%dx, t%r%dy], int64)
    h = 2166136261_int64
    do i = 1, 10
       h = iand(ieor(h, iand(v(i), M32)) * 16777619_int64, M32)
       h = iand(ieor(h, ishft(h, -15)) * 668265263_int64, M32)
    end do
    h = ieor(h, ishft(h, -13))
  end function trap_hash

  !> True iff the interiors of two trapezoids intersect (positive-area overlap; touching edges or
  !> corners do not count). Trapezoids are convex, so by the separating-axis theorem the interiors
  !> are disjoint iff some edge line of either one leaves the other on its outer closed side.
  !> Edge lines: y = y1, y = y2, and the two lattice side lines. Every orientation is exact
  !> (vertices are rationals x = num/dy at integer rows; products stay below 2**95).
  elemental pure logical function trap_overlap_positive(a, b) result(ov)
    !$omp declare target
    type(Trapezoid), intent(in) :: a, b
    ov = .false.
    if (b%y2 <= a%y1 .or. b%y1 >= a%y2) return
    if (outside_side(a%l, a%y1, b, .true.)) return
    if (outside_side(a%r, a%y1, b, .false.)) return
    if (outside_side(b%l, b%y1, a, .true.)) return
    if (outside_side(b%r, b%y1, a, .false.)) return
    ov = .true.
  end function trap_overlap_positive

  !> All four vertices of t on the closed outer side of side s (of a trapezoid starting at row
  !> sy1): left of (or on) a left side, right of (or on) a right side, directed upward.
  pure logical function outside_side(s, sy1, t, is_left) result(out)
    !$omp declare target
    type(TrapSide), intent(in) :: s
    integer(int32), intent(in) :: sy1
    type(Trapezoid), intent(in) :: t
    logical, intent(in) :: is_left
    integer :: c1, c2, c3, c4
    c1 = orient(s, sy1, t%l, t%y1, t%y1)
    c2 = orient(s, sy1, t%r, t%y1, t%y1)
    c3 = orient(s, sy1, t%l, t%y1, t%y2)
    c4 = orient(s, sy1, t%r, t%y1, t%y2)
    if (is_left) then
       out = c1 >= 0 .and. c2 >= 0 .and. c3 >= 0 .and. c4 >= 0
    else
       out = c1 <= 0 .and. c2 <= 0 .and. c3 <= 0 .and. c4 <= 0
    end if
  end function outside_side

  !> Sign of cross(dir(s), p - P) for the point p of side q (trapezoid row ty1) at row y, where
  !> P = (s%ax, sy1 - s%k) is the anchor of s: > 0 left of the upward line of s, < 0 right.
  pure integer function orient(s, sy1, q, ty1, y) result(c)
    !$omp declare target
    type(TrapSide), intent(in) :: s, q
    integer(int32), intent(in) :: sy1, ty1, y
    type(i128) :: xn, t1, t2
    integer(int64) :: py
    xn = side_x_num(q, int(ty1, int64), int(y, int64))            ! x = xn / q%dy
    py = int(sy1, int64) - s%k
    t1 = i128_mul_i64(i128_mul64(int(s%dx, int64), int(y, int64) - py), int(q%dy, int64))
    t2 = i128_mul_i64(i128_sub(xn, i128_mul64(int(s%ax, int64), int(q%dy, int64))), int(s%dy, int64))
    c = i128_cmp(t1, t2)
  end function orient

  ! ------------------------------------------------------------------ area

  !> Twice the total area of traps(1:n) (interior-disjoint), exactly.
  !>   2A(t) = h * [(xr(y1) + xr(y2)) - (xl(y1) + xl(y2))],  x(y1)+x(y2) = 2ax + (2k+h) dx/dy.
  !> The integer parts are summed directly. The fractions are grouped by slope (dx, dy), summed as
  !> i128 numerators and divided per group. is_integer = .false. means the remaining fraction is
  !> not an integer (the boundary is not on lattice points, i.e. invariant I1 does not hold); then
  !> area2 is rounded to the nearest integer.
  subroutine trap_area2(traps, n, area2, is_integer)
    ! Exact 2A of a trapezoid set: integer part (rectangle part of every trapezoid) plus, per
    ! side slope (dx, dy), sum of h (2k + h) dx over the sides with that slope, divided by dy.
    ! Parallel: each thread accumulates the slope sums in its own open-addressing table (passed
    ! explicitly, never through host association); the tables are merged, and the per-slope
    ! divisions are done in ascending key order, so the result is exact and independent of the
    ! thread count.
    type(Trapezoid), intent(in) :: traps(:)
    integer(int64), intent(in) :: n
    type(i128), intent(out) :: area2
    logical, intent(out) :: is_integer
    type(SlopeTab) :: G
    type(SlopeTab), allocatable :: TT(:)     ! one table per chunk (no private derived types:
    type(i128), allocatable :: parts(:)      ! nvfortran 26.1 crashes on allocatable components)
    integer(int64), allocatable :: perm(:), key(:)
    integer(int64) :: i, h, q, r, j, c, nch, lo, hi
    real(real64) :: frac
    area2 = i128_from_int64(0_int64)
    call stab_init(G, 1024_int64)
    nch = max(1_int64, min(256_int64, n / 65536_int64))
    allocate(TT(nch), parts(nch))
    !$omp parallel do schedule(dynamic, 1) private(i, h, lo, hi)
    do c = 1, nch
       call stab_init(TT(c), 256_int64)
       parts(c) = i128_from_int64(0_int64)
       lo = ((c - 1)*n) / nch + 1; hi = (c*n) / nch
       do i = lo, hi
          h = int(traps(i)%y2, int64) - traps(i)%y1
          parts(c) = i128_add(parts(c), i128_mul64(2_int64*h, int(traps(i)%r%ax, int64) - traps(i)%l%ax))
          if (traps(i)%r%dx /= 0) call stab_add(TT(c), slope_key(traps(i)%r), int(traps(i)%r%dy, int64), &
               i128_mul_i64(i128_mul64(h, 2_int64*traps(i)%r%k + h), int(traps(i)%r%dx, int64)))
          if (traps(i)%l%dx /= 0) call stab_add(TT(c), slope_key(traps(i)%l), int(traps(i)%l%dy, int64), &
               i128_neg(i128_mul_i64(i128_mul64(h, 2_int64*traps(i)%l%k + h), int(traps(i)%l%dx, int64))))
       end do
    end do
    !$omp end parallel do
    do c = 1, nch
       area2 = i128_add(area2, parts(c))
       do j = 1, TT(c)%cap
          if (TT(c)%used(j)) call stab_add(G, TT(c)%key(j), TT(c)%dy(j), TT(c)%acc(j))
       end do
    end do
    deallocate(TT, parts)
    is_integer = .true.
    if (G%n == 0) return
    allocate(perm(G%n), key(G%n))
    j = 0
    do i = 1, G%cap
       if (G%used(i)) then
          j = j + 1; perm(j) = i
       end if
    end do
    key(1:G%n) = G%key(perm(1:G%n))
    call sort_by_key(key, perm, G%n)
    frac = 0.0_real64
    do j = 1, G%n
       i = perm(j)
       call i128_divmod(G%acc(i), G%dy(i), q, r)
       area2 = i128_add(area2, i128_from_int64(q))
       frac = frac + real(r, real64) / real(G%dy(i), real64)
    end do
    q = nint(frac, int64)
    is_integer = abs(frac - real(q, real64)) < 1.0e-6_real64
    area2 = i128_add(area2, i128_from_int64(q))
  end subroutine trap_area2

  subroutine stab_init(T, cap)
    type(SlopeTab), intent(inout) :: T
    integer(int64), intent(in) :: cap
    if (allocated(T%key)) deallocate(T%key, T%dy, T%acc, T%used)
    T%cap = cap; T%n = 0
    allocate(T%key(cap), T%dy(cap), T%acc(cap), T%used(cap))
    T%used = .false.
  end subroutine stab_init

  !> T(k) += v (dy is stored with a new key).
  subroutine stab_add(T, k, dy, v)
    type(SlopeTab), intent(inout) :: T
    integer(int64), intent(in) :: k, dy
    type(i128), intent(in) :: v
    integer(int64) :: s, m, oc
    integer(int64), allocatable :: ok(:), od(:)
    type(i128), allocatable :: oa(:)
    logical, allocatable :: ou(:)
    if (2*(T%n + 1) > T%cap) then
       call move_alloc(T%key, ok); call move_alloc(T%dy, od); call move_alloc(T%acc, oa)
       call move_alloc(T%used, ou)
       oc = T%cap; T%cap = 2*T%cap
       allocate(T%key(T%cap), T%dy(T%cap), T%acc(T%cap), T%used(T%cap))
       T%used = .false.
       do m = 1, oc
          if (.not. ou(m)) cycle
          s = iand(mix(ok(m)), T%cap - 1) + 1
          do while (T%used(s))
             s = iand(s, T%cap - 1) + 1
          end do
          T%used(s) = .true.; T%key(s) = ok(m); T%dy(s) = od(m); T%acc(s) = oa(m)
       end do
    end if
    s = iand(mix(k), T%cap - 1) + 1
    do while (T%used(s))
       if (T%key(s) == k) then
          T%acc(s) = i128_add(T%acc(s), v); return
       end if
       s = iand(s, T%cap - 1) + 1
    end do
    T%used(s) = .true.; T%key(s) = k; T%dy(s) = dy; T%acc(s) = v; T%n = T%n + 1
  end subroutine stab_add

  pure integer(int64) function mix(a) result(z)
    integer(int64), intent(in) :: a
    z = a + (-7046029254386353131_int64)
    z = ieor(z, shiftr(z, 30)) * (-4658895280553007687_int64)
    z = ieor(z, shiftr(z, 27)) * (-7723592293110705685_int64)
    z = ieor(z, shiftr(z, 31))
    z = shiftr(z, 1)
  end function mix

  !> Insertion/merge sort of the (small) slope table by key, carrying perm.
  subroutine sort_by_key(key, perm, n)
    integer(int64), intent(inout) :: key(:), perm(:)
    integer(int64), intent(in) :: n
    integer(int64), allocatable :: p2(:)
    integer(int64) :: i
    allocate(p2(n))
    call sort_keys(key(1:n), p2)
    key(1:n) = key(p2); perm(1:n) = perm(p2)
  end subroutine sort_by_key

  elemental pure function slope_key(s) result(k)
    type(TrapSide), intent(in) :: s
    integer(int64) :: k
    k = int(s%dx, int64) * 4294967296_int64 + int(s%dy, int64)
  end function slope_key

  !> perm = stable ascending order of key (bottom-up merge sort).
  subroutine sort_keys(key, perm)
    integer(int64), intent(in) :: key(:)
    integer(int64), intent(out) :: perm(:)
    integer(int64), allocatable :: tmp(:)
    integer(int64) :: n, width, lo, mid, hi, i, j, k
    n = size(key, kind=int64)
    perm = [(i, i = 1, n)]
    allocate(tmp(n))
    width = 1
    do while (width < n)
       lo = 1
       do while (lo <= n)
          mid = min(lo + width - 1, n); hi = min(lo + 2*width - 1, n)
          i = lo; j = mid + 1; k = lo
          do while (i <= mid .and. j <= hi)
             if (key(perm(j)) < key(perm(i))) then
                tmp(k) = perm(j); j = j + 1
             else
                tmp(k) = perm(i); i = i + 1
             end if
             k = k + 1
          end do
          do while (i <= mid)
             tmp(k) = perm(i); i = i + 1; k = k + 1
          end do
          do while (j <= hi)
             tmp(k) = perm(j); j = j + 1; k = k + 1
          end do
          lo = lo + 2*width
       end do
       perm(1:n) = tmp(1:n)
       width = 2*width
    end do
  end subroutine sort_keys

  !> Sort traps(1:n) into canonical order (trap_less), stable.
  subroutine trap_sort(traps, n)
    type(Trapezoid), intent(inout) :: traps(:)
    integer(int64), intent(in) :: n
    type(Trapezoid), allocatable :: tmp(:)
    integer(int64) :: width, lo, mid, hi, i, j, k
    if (n < 2) return
    allocate(tmp(n))
    width = 1
    do while (width < n)
       lo = 1
       do while (lo <= n)
          mid = min(lo + width - 1, n); hi = min(lo + 2*width - 1, n)
          i = lo; j = mid + 1; k = lo
          do while (i <= mid .and. j <= hi)
             if (trap_less(traps(j), traps(i))) then
                tmp(k) = traps(j); j = j + 1
             else
                tmp(k) = traps(i); i = i + 1
             end if
             k = k + 1
          end do
          do while (i <= mid)
             tmp(k) = traps(i); i = i + 1; k = k + 1
          end do
          do while (j <= hi)
             tmp(k) = traps(j); j = j + 1; k = k + 1
          end do
          lo = lo + 2*width
       end do
       traps(1:n) = tmp(1:n)
       width = 2*width
    end do
  end subroutine trap_sort

end module TrapezoidModule
