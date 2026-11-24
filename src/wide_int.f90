! File   : wide_int.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Portable exact 128-bit signed integer arithmetic for the any-angle geometry kernel.
!
! nvfortran has no integer(16), and exact geometric predicates on int32 coordinates need about
! 95 bits (x-at-y comparisons of two lattice lines, intersection numerators). This module
! provides a signed 128-bit type that works the same on every compiler and inside OpenMP target
! regions (every routine is elemental/pure, allocation-free and `declare target`).
!
! Representation: value = hi * 2**62 + lo, with 0 <= lo < 2**62 and hi a signed int64, giving
! a range of about +-2**125. Every limb operation stays inside the signed int64 range, so no
! step relies on wrap-around of signed overflow (which Fortran leaves undefined).
!
! Preconditions (not checked, to stay usable in GPU kernels):
!   * i128_mul64(a, b): a, b /= -2**63 and |a*b| < 2**125;
!   * i128_mul_i64(x, c): |x*c| < 2**125;
!   * i128_divmod(x, d, q, r): d > 0 and the quotient fits int64.
! The geometry kernel stays far inside these bounds: coordinates are limited to |c| <= 2**30-1
! (invariant I0), so its largest products are about 2**94.
module WideIntModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  implicit none
  private

  integer(int64), parameter :: K_R62 = 4611686018427387904_int64      !< 2**62
  integer(int64), parameter :: K_M62 = 4611686018427387903_int64      !< 2**62 - 1
  integer(int64), parameter :: K_M31 = 2147483647_int64               !< 2**31 - 1

  !> Signed 128-bit integer: value = hi * 2**62 + lo, 0 <= lo < 2**62.
  type, public :: i128
     integer(int64) :: hi = 0_int64
     integer(int64) :: lo = 0_int64
  end type i128

  public :: i128_from_int64, i128_add, i128_sub, i128_neg, i128_cmp, i128_sign, i128_is_zero
  public :: i128_mul64, i128_mul_i64, i128_divmod, i128_to_real64, i128_round_div
  public :: i128_cmp_prod

contains

  !> Carry-normalise a (hi, lo) pair with any lo into the canonical form.
  elemental pure function i128_norm(h, l) result(r)
    !$omp declare target
    integer(int64), intent(in) :: h, l
    type(i128) :: r
    integer(int64) :: c
    c = l / K_R62                          ! floor(l / 2**62); shifta is not in nvfortran
    if (l < c*K_R62) c = c - 1_int64
    r%hi = h + c
    r%lo = l - c*K_R62
  end function i128_norm

  elemental pure function i128_from_int64(v) result(r)
    !$omp declare target
    integer(int64), intent(in) :: v
    type(i128) :: r
    r = i128_norm(0_int64, v)
  end function i128_from_int64

  elemental pure function i128_add(a, b) result(r)
    !$omp declare target
    type(i128), intent(in) :: a, b
    type(i128) :: r
    r = i128_norm(a%hi + b%hi, a%lo + b%lo)          ! a%lo + b%lo < 2**63
  end function i128_add

  elemental pure function i128_sub(a, b) result(r)
    !$omp declare target
    type(i128), intent(in) :: a, b
    type(i128) :: r
    r = i128_norm(a%hi - b%hi, a%lo - b%lo)          ! |a%lo - b%lo| < 2**62
  end function i128_sub

  elemental pure function i128_neg(a) result(r)
    !$omp declare target
    type(i128), intent(in) :: a
    type(i128) :: r
    r = i128_norm(-a%hi, -a%lo)
  end function i128_neg

  !> -1, 0 or +1 as a < b, a == b or a > b.
  elemental pure function i128_cmp(a, b) result(c)
    !$omp declare target
    type(i128), intent(in) :: a, b
    integer(int32) :: c
    if (a%hi /= b%hi) then
       c = merge(-1_int32, 1_int32, a%hi < b%hi)
    else if (a%lo /= b%lo) then
       c = merge(-1_int32, 1_int32, a%lo < b%lo)
    else
       c = 0_int32
    end if
  end function i128_cmp

  elemental pure function i128_sign(a) result(s)
    !$omp declare target
    type(i128), intent(in) :: a
    integer(int32) :: s
    if (a%hi > 0_int64) then
       s = 1_int32
    else if (a%hi < 0_int64) then
       s = -1_int32
    else
       s = merge(1_int32, 0_int32, a%lo > 0_int64)
    end if
  end function i128_sign

  elemental pure logical function i128_is_zero(a)
    !$omp declare target
    type(i128), intent(in) :: a
    i128_is_zero = a%hi == 0_int64 .and. a%lo == 0_int64
  end function i128_is_zero

  !> Exact product of two int64 values: three 31-bit limbs per factor, so every partial
  !> product and column sum fits a signed int64.
  elemental pure function i128_mul64(a, b) result(r)
    !$omp declare target
    integer(int64), intent(in) :: a, b
    type(i128) :: r
    integer(int64) :: ua, ub, a0, a1, a2, b0, b1, b2
    integer(int64) :: c0, c1, c2, c3, c4, t, cy, l0, l1, l2, l3, l4
    logical :: negative
    negative = (a < 0_int64) .neqv. (b < 0_int64)
    ua = abs(a); ub = abs(b)
    a0 = iand(ua, K_M31); a1 = iand(shiftr(ua, 31), K_M31); a2 = shiftr(ua, 62)
    b0 = iand(ub, K_M31); b1 = iand(shiftr(ub, 31), K_M31); b2 = shiftr(ub, 62)
    c0 = a0*b0                          ! < 2**62
    c1 = a1*b0 + a0*b1                  ! < 2**63
    c2 = a2*b0 + a1*b1 + a0*b2          ! a2, b2 <= 1
    c3 = a2*b1 + a1*b2
    c4 = a2*b2
    t = c0;      l0 = iand(t, K_M31); cy = shiftr(t, 31)
    t = c1 + cy; l1 = iand(t, K_M31); cy = shiftr(t, 31)
    t = c2 + cy; l2 = iand(t, K_M31); cy = shiftr(t, 31)
    t = c3 + cy; l3 = iand(t, K_M31); cy = shiftr(t, 31)
    l4 = c4 + cy                        ! <= 1 under the precondition
    r%lo = ior(shiftl(l1, 31), l0)
    r%hi = ior(ior(shiftl(l4, 62), shiftl(l3, 31)), l2)
    if (negative) r = i128_neg(r)
  end function i128_mul64

  !> x * c for an i128 x and an int64 c.
  elemental pure function i128_mul_i64(x, c) result(r)
    !$omp declare target
    type(i128), intent(in) :: x
    integer(int64), intent(in) :: c
    type(i128) :: r, plo, phi
    plo = i128_mul64(x%lo, c)           ! x%lo * c
    phi = i128_mul64(x%hi, c)           ! x%hi * c, times 2**62 below (phi%hi must be 0 or -1)
    r = i128_norm(plo%hi + phi%lo + shiftl(phi%hi, 62), plo%lo)
  end function i128_mul_i64

  !> Sign of a*b - c*d for int64 factors (exact).
  elemental pure function i128_cmp_prod(a, b, c, d) result(s)
    !$omp declare target
    integer(int64), intent(in) :: a, b, c, d
    integer(int32) :: s
    s = i128_cmp(i128_mul64(a, b), i128_mul64(c, d))
  end function i128_cmp_prod

  !> Floor division with remainder: x = q*d + r, 0 <= r < d, for d > 0. Binary long division
  !> over the 124 magnitude bits; every intermediate stays below d (overflow-free for any d).
  elemental pure subroutine i128_divmod(x, d, q, r)
    !$omp declare target
    type(i128), intent(in) :: x
    integer(int64), intent(in) :: d
    integer(int64), intent(out) :: q, r
    type(i128) :: m
    integer :: k
    integer(int64) :: bit, gap
    logical :: negative
    negative = x%hi < 0_int64
    m = x
    if (negative) m = i128_neg(x)
    q = 0_int64; r = 0_int64
    do k = 123, 0, -1
       if (k >= 62) then
          bit = merge(1_int64, 0_int64, btest(m%hi, k - 62))
       else
          bit = merge(1_int64, 0_int64, btest(m%lo, k))
       end if
       gap = (d - bit) - r                ! 2r + bit >= d  <=>  r >= gap
       q = shiftl(q, 1)
       if (r >= gap) then
          r = r - gap
          q = q + 1_int64
       else
          r = r + r + bit
       end if
    end do
    if (negative .and. r /= 0_int64) then        ! floor of a negative quotient
       q = -q - 1_int64
       r = d - r
    else if (negative) then
       q = -q
    end if
  end subroutine i128_divmod

  !> Round x / d to the nearest integer, halves upwards (towards +infinity), for d > 0.
  elemental pure function i128_round_div(x, d) result(q)
    !$omp declare target
    type(i128), intent(in) :: x
    integer(int64), intent(in) :: d
    integer(int64) :: q, r
    call i128_divmod(x, d, q, r)
    if (r >= d - r) q = q + 1_int64           ! 2r >= d
  end function i128_round_div

  elemental pure function i128_to_real64(a) result(v)
    !$omp declare target
    type(i128), intent(in) :: a
    real(real64) :: v
    v = real(a%hi, real64) * real(K_R62, real64) + real(a%lo, real64)
  end function i128_to_real64

end module WideIntModule
