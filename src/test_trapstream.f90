! File   : test_trapstream.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Verify Tier 4 TrapBandStreamModule (M=1, 4, 16 streaming bands) produces exact
!          canonical equivalence (ntrap, exact area2, anomalies=0) vs single-shot Booleans
!          on rotated overlapping polygons across OR, AND, NOT, XOR.
program test_trapstream
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use WideIntModule
  use TrapezoidModule
  use TrapMergeModule
  use TrapBandStreamModule
  implicit none

  real(real64), parameter :: PI = 3.14159265358979323846_real64
  integer, parameter :: NBANDS(3) = [1, 4, 16]
  integer(int32), parameter :: OPS(4) = [TOP_OR, TOP_AND, TOP_NOT, TOP_XOR]
  integer(int64), parameter :: NP = 40
  integer(int64) :: poffA(NP + 1), pxA(4 * NP), pyA(4 * NP)
  integer(int64) :: poffB(NP + 1), pxB(4 * NP), pyB(4 * NP)
  type(TrapList) :: ref(4)
  type(TrapMergeStats) :: mst
  type(Trapezoid), allocatable :: st_traps(:)
  integer(int64) :: st_n, i
  type(i128) :: a2_ref, a2_st
  logical :: ok_ref, ok_st
  type(TrapStreamStats) :: sst
  integer :: status, io, ib, nfail
  real(real64) :: th, c, s, x0, y0, rx(4), ry(4)

  nfail = 0
  th = 23.0_real64 * PI / 180.0_real64
  c = cos(th); s = sin(th)
  poffA(1) = 1; poffB(1) = 1
  do i = 1, NP
     x0 = real(mod(i - 1, 8_int64) * 600, real64)
     y0 = real(((i - 1) / 8_int64) * 600, real64)
     rx = [x0, x0 + 500d0, x0 + 500d0, x0]
     ry = [y0, y0,         y0 + 350d0, y0 + 350d0]
     pxA(4*(i-1)+1:4*i) = nint(c * rx - s * ry, int64)
     pyA(4*(i-1)+1:4*i) = nint(s * rx + c * ry, int64)
     poffA(i + 1) = 4 * i + 1

     rx = [x0 + 200d0, x0 + 700d0, x0 + 700d0, x0 + 200d0]
     ry = [y0 + 150d0, y0 + 150d0, y0 + 500d0, y0 + 500d0]
     pxB(4*(i-1)+1:4*i) = nint(c * rx - s * ry, int64)
     pyB(4*(i-1)+1:4*i) = nint(s * rx + c * ry, int64)
     poffB(i + 1) = 4 * i + 1
  end do

  call TrapBooleanPolygons(NP, poffA, pxA, pyA, NP, poffB, pxB, pyB, 4, OPS, ref, mst, status)
  if (status /= 0 .or. mst%anomalies /= 0) error stop 'reference build failed'

  do io = 1, 4
     call trap_area2(ref(io)%t, ref(io)%n, a2_ref, ok_ref)
     do ib = 1, size(NBANDS)
        call TrapBandedBoolean(NP, poffA, pxA, pyA, NP, poffB, pxB, pyB, OPS(io), NBANDS(ib), &
                               st_traps, st_n, sst, status)
        call trap_area2(st_traps, st_n, a2_st, ok_st)
        if (status /= 0 .or. sst%anomalies /= 0 .or. .not. ok_st .or. &
            st_n /= ref(io)%n .or. i128_cmp(a2_st, a2_ref) /= 0) then
           write(*, '(A,I0,A,I0,A,I0,A,I0,A,I0)') 'FAIL op=', OPS(io), ' M=', NBANDS(ib), &
                ' n=', st_n, ' want_n=', ref(io)%n, ' anom=', sst%anomalies
           nfail = nfail + 1
        end if
     end do
  end do

  if (nfail == 0) then
     write(*, '(A)') 'test_trapstream: PASS'
  else
     write(*, '(A,I0)') 'test_trapstream: FAIL nfail=', nfail
     error stop 1
  end if
end program test_trapstream
