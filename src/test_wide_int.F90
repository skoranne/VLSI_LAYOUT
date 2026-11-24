! File   : test_wide_int.F90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Unit test of WideIntModule (portable exact 128-bit integers).
!   * fixed vectors (powers of two, sign boundaries, carries) on every compiler;
!   * random products, sums, compares, floor division and rounding against native integer(16)
!     where the compiler has it (gfortran, ifx);
!   * the same kernels inside an OpenMP target region (GPU with nvfortran -mp=gpu), compared
!     with the host results.
! Usage: test_wide_int.exe            (prints PASS, or FAIL lines and stops with code 1)
program test_wide_int
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use WideIntModule
  implicit none
#if defined(__GFORTRAN__) || defined(__INTEL_COMPILER) || defined(__INTEL_LLVM_COMPILER)
#define HAVE_INT128 1
  integer, parameter :: ik16 = selected_int_kind(38)
#endif
  integer :: nfail = 0
  integer(int64), parameter :: P62 = 4611686018427387904_int64
  type(i128) :: a, b, c
  integer(int64) :: q, r

  ! --- fixed vectors ---
  a = i128_mul64(P62 - 1_int64, P62 - 1_int64)          ! (2^62-1)^2 = 2^124 - 2^63 + 1
  call expect(a%hi == P62 - 2_int64 .and. a%lo == 1_int64, 'mul (2^62-1)^2')
  a = i128_mul64(-(P62 - 1_int64), P62 - 1_int64)
  b = i128_neg(i128_mul64(P62 - 1_int64, P62 - 1_int64))
  call expect(i128_cmp(a, b) == 0 .and. i128_sign(a) == -1, 'mul negative')
  a = i128_from_int64(-huge(1_int64) - 1_int64)          ! -2^63
  call expect(a%hi == -2_int64 .and. a%lo == 0_int64, 'from_int64(-2^63)')
  a = i128_add(i128_from_int64(huge(1_int64)), i128_from_int64(1_int64))   ! 2^63
  call expect(a%hi == 2_int64 .and. a%lo == 0_int64, 'add carry to 2^63')
  a = i128_sub(i128_from_int64(0_int64), i128_from_int64(1_int64))
  call expect(a%hi == -1_int64 .and. a%lo == P62 - 1_int64 .and. i128_sign(a) == -1, 'sub borrow -1')
  call i128_divmod(i128_from_int64(-7_int64), 2_int64, q, r)
  call expect(q == -4_int64 .and. r == 1_int64, 'floor div -7/2')
  call i128_divmod(i128_mul64(3037000499_int64, 3037000499_int64), 3037000499_int64, q, r)
  call expect(q == 3037000499_int64 .and. r == 0_int64, 'exact div of a square')
  call expect(i128_round_div(i128_from_int64(5_int64), 2_int64) == 3_int64, 'round 5/2 -> 3')
  call expect(i128_round_div(i128_from_int64(-5_int64), 2_int64) == -2_int64, 'round -5/2 -> -2')
  call expect(i128_round_div(i128_from_int64(-7_int64), 4_int64) == -2_int64, 'round -7/4 -> -2')
  c = i128_mul_i64(i128_mul64(2147483647_int64, 2147483647_int64), -2147483647_int64)
  call expect(i128_cmp(c, i128_neg(i128_mul_i64(i128_mul64(2147483647_int64, 2147483647_int64), &
              2147483647_int64))) == 0, 'mul_i64 sign')
  call expect(i128_cmp_prod(3_int64, 4_int64, 2_int64, 6_int64) == 0, 'cmp_prod equal')

#ifdef HAVE_INT128
  call random_against_int128()
#else
  print '(A)', 'INFO: no integer(16) on this compiler; random cross-check skipped'
#endif
  call device_check()

  if (nfail > 0) then
     print '(A,I0,A)', 'FAIL: ', nfail, ' check(s) failed'
     error stop 1
  end if
  print '(A)', 'PASS test_wide_int'

contains

  subroutine expect(ok, what)
    logical, intent(in) :: ok
    character(*), intent(in) :: what
    if (.not. ok) then
       nfail = nfail + 1
       print '(A)', 'FAIL: '//what
    end if
  end subroutine expect

  !> Uniform-ish random int64 with |v| < 2**bits.
  function rnd(bits) result(v)
    integer, intent(in) :: bits
    integer(int64) :: v
    real(real64) :: u, s
    call random_number(u); call random_number(s)
    v = int((2.0_real64*u - 1.0_real64) * (2.0_real64**bits - 1.0_real64), int64)
    if (s < 0.05_real64) v = sign(2_int64**bits - 1_int64, v)      ! extremes
  end function rnd

#ifdef HAVE_INT128
  function to16(x) result(v)
    type(i128), intent(in) :: x
    integer(ik16) :: v
    v = int(x%hi, ik16) * 2_ik16**62 + int(x%lo, ik16)
  end function to16

  subroutine random_against_int128()
    integer :: it, bits_a, bits_b, nbad
    integer(int64) :: x, y, z, d, qq, rr
    integer(ik16) :: ref, refq, refr
    type(i128) :: p, s
    nbad = 0
    do it = 1, 200000
       bits_a = 1 + mod(it, 63); bits_b = max(1, min(62, 124 - bits_a))
       x = rnd(bits_a); y = rnd(bits_b)
       p = i128_mul64(x, y)
       ref = int(x, ik16) * int(y, ik16)
       if (to16(p) /= ref .or. p%lo < 0 .or. p%lo >= 2_int64**62) nbad = nbad + 1
       ! sums and compares
       z = rnd(62)
       s = i128_add(p, i128_mul64(z, y))
       if (to16(s) /= ref + int(z, ik16)*int(y, ik16)) nbad = nbad + 1
       if (i128_cmp(p, s) /= int(sign(1_ik16, to16(p) - to16(s)), int32) .and. to16(p) /= to16(s)) nbad = nbad + 1
       ! times a third factor
       if (bits_a + bits_b <= 92) then
          d = rnd(31)
          if (to16(i128_mul_i64(p, d)) /= ref * int(d, ik16)) nbad = nbad + 1
       end if
       ! floor division and rounding
       d = abs(rnd(62)) + 1_int64
       if (abs(ref) / int(d, ik16) < 2_ik16**62) then
          call i128_divmod(p, d, qq, rr)
          refq = ref / int(d, ik16); refr = ref - refq*int(d, ik16)
          if (refr < 0) then
             refq = refq - 1; refr = refr + int(d, ik16)
          end if
          if (int(qq, ik16) /= refq .or. int(rr, ik16) /= refr) nbad = nbad + 1
          if (2_ik16*refr >= int(d, ik16)) refq = refq + 1
          if (int(i128_round_div(p, d), ik16) /= refq) nbad = nbad + 1
       end if
    end do
    call expect(nbad == 0, 'random cross-check against integer(16)')
    print '(A,I0,A)', 'INFO: 200000 random cases against integer(16), ', nbad, ' mismatches'
  end subroutine random_against_int128
#endif

  !> Same kernels in a target region (GPU when offload is available), compared with the host.
  subroutine device_check()
    integer, parameter :: n = 4096
    integer(int64) :: x(n), y(n), d(n), hq(n), dq(n), hr(n), dr(n)
    type(i128) :: hp(n), dp(n)
    integer :: i
    do i = 1, n
       x(i) = rnd(62); y(i) = rnd(60); d(i) = abs(rnd(40)) + 1_int64
    end do
    do i = 1, n
       hp(i) = i128_mul64(x(i), y(i))
       call i128_divmod(hp(i), d(i), hq(i), hr(i))
    end do
    hq = 0; hr = 0
    do i = 1, n
       if (abs(i128_to_real64(hp(i)) / real(d(i), real64)) < 4.0e18_real64) &
            call i128_divmod(hp(i), d(i), hq(i), hr(i))
    end do
    dq = 0; dr = 0
    !$omp target teams distribute parallel do map(to: x, y, d) map(from: dp, dq, dr)
    do i = 1, n
       dp(i) = i128_mul64(x(i), y(i))
       dq(i) = 0; dr(i) = 0
       if (abs(i128_to_real64(dp(i)) / real(d(i), real64)) < 4.0e18_real64) &
            call i128_divmod(dp(i), d(i), dq(i), dr(i))
    end do
    call expect(all(dp%hi == hp%hi) .and. all(dp%lo == hp%lo) .and. all(dq == hq) .and. all(dr == hr), &
                'target region results equal host results')
  end subroutine device_check

end program test_wide_int
