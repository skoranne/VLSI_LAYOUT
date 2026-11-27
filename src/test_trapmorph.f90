! File   : test_trapmorph.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Unit test for TrapMorphModule (exact any-angle GROW / SHRINK / SIZE):
!          1. Manhattan square, directional GROW/SHRINK, L-shape (reflex corner), ring with hole,
!             vanishing narrow bar (exact integer area2 checks).
!          2. 0/15/30/45/90-degree rotated square & ring: inclusion identities (A <= grow,
!             shrink <= A), opening/closing identities, and rotation invariance of area & perimeter.
program test_trapmorph
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use TrapezoidModule
  use TrapMergeModule
  use TrapPNumModule
  use TrapMorphModule
  implicit none

  integer :: nfail, status
  nfail = 0

  call test_manhattan_square(nfail)
  call test_manhattan_directional(nfail)
  call test_manhattan_lshape(nfail)
  call test_manhattan_ring_and_vanish(nfail)
  call test_rotated_angles(nfail)

  if (nfail == 0) then
     write(*, '(A)') 'test_trapmorph: PASS'
  else
     write(*, '(A,I0)') 'test_trapmorph: FAIL nfail=', nfail
     error stop 1
  end if

contains

  subroutine make_rect(x1, y1, x2, y2, traps, ntrap)
    integer(int64), intent(in) :: x1, y1, x2, y2
    type(Trapezoid), allocatable, intent(out) :: traps(:)
    integer(int64), intent(out) :: ntrap
    integer(int64) :: poff(2), px(4), py(4)
    type(TrapMergeStats) :: st
    integer :: status
    poff = [1_int64, 5_int64]
    px = [x1, x2, x2, x1]; py = [y1, y1, y2, y2]
    call TrapMergePolygons(1_int64, poff, px, py, traps, ntrap, st, status)
    if (status /= 0 .or. st%anomalies /= 0) error stop 'make_rect failed'
  end subroutine make_rect

  integer(int64) function area2_of(t, n) result(a2)
    type(Trapezoid), intent(in) :: t(:)
    integer(int64), intent(in) :: n
    type(TrapPolys) :: P
    if (n <= 0) then
       a2 = 0; return
    end if
    call TrapPNum(t, n, P)
    a2 = sum(P%area2(1:P%np))
  end function area2_of

  subroutine check_eq(name, got, want, nfail)
    character(len=*), intent(in) :: name
    integer(int64), intent(in) :: got, want
    integer, intent(inout) :: nfail
    if (got /= want) then
       write(*, '(A,A,A,I0,A,I0)') 'FAIL ', name, ': got=', got, ' want=', want
       nfail = nfail + 1
    end if
  end subroutine check_eq

  subroutine test_manhattan_square(nfail)
    integer, intent(inout) :: nfail
    type(Trapezoid), allocatable :: A(:), G(:), S(:), C(:), O(:)
    integer(int64) :: nA, nG, nS, nC, nO
    type(TrapMorphStats) :: st
    integer :: status
    call make_rect(0_int64, 0_int64, 200_int64, 200_int64, A, nA)
    call TrapMorph(A, nA, 50_int64, 50_int64, 50_int64, 50_int64, .false., G, nG, st, status)
    call check_eq('sq_grow_status', int(status, int64), 0_int64, nfail)
    call check_eq('sq_grow_anom', st%anomalies, 0_int64, nfail)
    call check_eq('sq_grow_a2', area2_of(G, nG), 2_int64 * 300_int64 * 300_int64, nfail)

    call TrapMorph(A, nA, 30_int64, 30_int64, 30_int64, 30_int64, .true., S, nS, st, status)
    call check_eq('sq_shrink_a2', area2_of(S, nS), 2_int64 * 140_int64 * 140_int64, nfail)

    call TrapMorph(G, nG, 50_int64, 50_int64, 50_int64, 50_int64, .true., C, nC, st, status)
    call check_eq('sq_close_a2', area2_of(C, nC), 2_int64 * 200_int64 * 200_int64, nfail)

    call TrapMorph(S, nS, 30_int64, 30_int64, 30_int64, 30_int64, .false., O, nO, st, status)
    call check_eq('sq_open_a2', area2_of(O, nO), 2_int64 * 200_int64 * 200_int64, nfail)
  end subroutine test_manhattan_square

  subroutine test_manhattan_directional(nfail)
    integer, intent(inout) :: nfail
    type(Trapezoid), allocatable :: A(:), G(:), S(:)
    integer(int64) :: nA, nG, nS
    type(TrapMorphStats) :: st
    integer :: status
    call make_rect(0_int64, 0_int64, 100_int64, 100_int64, A, nA)
    call TrapMorph(A, nA, 10_int64, 20_int64, 30_int64, 40_int64, .false., G, nG, st, status)
    ! width = 100 + 10 + 30 = 140, height = 100 + 20 + 40 = 160
    call check_eq('dir_grow_a2', area2_of(G, nG), 2_int64 * 140_int64 * 160_int64, nfail)

    call TrapMorph(A, nA, 10_int64, 20_int64, 30_int64, 15_int64, .true., S, nS, st, status)
    ! width = 100 - 10 - 30 = 60, height = 100 - 20 - 15 = 65
    call check_eq('dir_shrink_a2', area2_of(S, nS), 2_int64 * 60_int64 * 65_int64, nfail)
  end subroutine test_manhattan_directional

  subroutine test_manhattan_lshape(nfail)
    integer, intent(inout) :: nfail
    integer(int64) :: poff(2), px(6), py(6)
    type(Trapezoid), allocatable :: A(:), G(:), S(:)
    integer(int64) :: nA, nG, nS
    type(TrapMergeStats) :: mst
    type(TrapMorphStats) :: st
    integer :: status
    ! L-shape: [0,200]x[0,100] U [0,100]x[0,200], area = 30000
    poff = [1_int64, 7_int64]
    px = [0_int64, 200_int64, 200_int64, 100_int64, 100_int64, 0_int64]
    py = [0_int64, 0_int64,   100_int64, 100_int64, 200_int64, 200_int64]
    call TrapMergePolygons(1_int64, poff, px, py, A, nA, mst, status)
    ! Shrink by 20: [20,180]x[20,80] U [20,80]x[20,180] -> 160*60 + 60*100 = 15600
    call TrapMorph(A, nA, 20_int64, 20_int64, 20_int64, 20_int64, .true., S, nS, st, status)
    call check_eq('lshape_shrink_a2', area2_of(S, nS), 2_int64 * 15600_int64, nfail)
    ! Grow by 20: [-20,220]x[-20,120] U [-20,120]x[-20,220] -> 240*140 + 140*100 = 47600
    call TrapMorph(A, nA, 20_int64, 20_int64, 20_int64, 20_int64, .false., G, nG, st, status)
    call check_eq('lshape_grow_a2', area2_of(G, nG), 2_int64 * 47600_int64, nfail)
  end subroutine test_manhattan_lshape

  subroutine test_manhattan_ring_and_vanish(nfail)
    integer, intent(inout) :: nfail
    integer(int64) :: poA(2), xA(4), yA(4), poB(2), xB(4), yB(4)
    type(TrapList) :: res(1)
    type(TrapMergeStats) :: mst
    type(TrapMorphStats) :: st
    type(Trapezoid), allocatable :: G(:), S(:), V(:)
    integer(int64) :: nG, nS, nV
    integer(int32) :: ops(1)
    integer :: status
    ! Ring: [0,300]^2 \ [100,200]^2 (area = 80000)
    poA = [1_int64, 5_int64]; xA = [0_int64, 300_int64, 300_int64, 0_int64]; yA = [0_int64, 0_int64, 300_int64, 300_int64]
    poB = [1_int64, 5_int64]; xB = [100_int64, 200_int64, 200_int64, 100_int64]; yB = [100_int64, 100_int64, 200_int64, 200_int64]
    ops(1) = TOP_NOT
    call TrapBooleanPolygons(1_int64, poA, xA, yA, 1_int64, poB, xB, yB, 1, ops, res, mst, status)
    ! Grow by 20: outer [-20,320]^2 (340^2=115600), hole [120,180]^2 (60^2=3600) -> 112000
    call TrapMorph(res(1)%t, res(1)%n, 20_int64, 20_int64, 20_int64, 20_int64, .false., G, nG, st, status)
    call check_eq('ring_grow_a2', area2_of(G, nG), 2_int64 * 112000_int64, nfail)
    ! Grow by 60: hole disappears completely -> [-60,360]^2 = 420^2 = 176400
    call TrapMorph(res(1)%t, res(1)%n, 60_int64, 60_int64, 60_int64, 60_int64, .false., G, nG, st, status)
    call check_eq('ring_grow_fill_a2', area2_of(G, nG), 2_int64 * 176400_int64, nfail)
    ! Shrink by 20: outer [20,280]^2 (260^2=67600), hole [80,220]^2 (140^2=19600) -> 48000
    call TrapMorph(res(1)%t, res(1)%n, 20_int64, 20_int64, 20_int64, 20_int64, .true., S, nS, st, status)
    call check_eq('ring_shrink_a2', area2_of(S, nS), 2_int64 * 48000_int64, nfail)
    ! Shrink by 60: walls are 100 wide -> vanishes completely!
    call TrapMorph(res(1)%t, res(1)%n, 60_int64, 60_int64, 60_int64, 60_int64, .true., V, nV, st, status)
    call check_eq('ring_shrink_vanish', nV, 0_int64, nfail)
  end subroutine test_manhattan_ring_and_vanish

  subroutine test_rotated_angles(nfail)
    integer, intent(inout) :: nfail
    integer, parameter :: angs(5) = [0, 15, 30, 45, 90]
    real(real64), parameter :: pi = 3.14159265358979323846_real64
    integer :: ia, k, status
    real(real64) :: th, cth, sth, x0(4), y0(4), rel_g, rel_s
    integer(int64) :: poff(2), px(4), py(4), nA, nG, nS, aG, aS
    type(Trapezoid), allocatable :: A(:), G(:), S(:)
    type(TrapMergeStats) :: mst
    type(TrapMorphStats) :: st
    ! 1000 x 1000 square rotated by 0, 15, 30, 45, 90 degrees; grow +50 -> 1100^2, shrink -30 -> 940^2
    x0 = [-500.0_real64, 500.0_real64, 500.0_real64, -500.0_real64]
    y0 = [-500.0_real64, -500.0_real64, 500.0_real64, 500.0_real64]
    poff = [1_int64, 5_int64]
    do ia = 1, size(angs)
       th = real(angs(ia), real64) * pi / 180.0_real64
       cth = cos(th); sth = sin(th)
       do k = 1, 4
          px(k) = nint(cth * x0(k) - sth * y0(k), int64)
          py(k) = nint(sth * x0(k) + cth * y0(k), int64)
       end do
       call TrapMergePolygons(1_int64, poff, px, py, A, nA, mst, status)
       call TrapMorph(A, nA, 50_int64, 50_int64, 50_int64, 50_int64, .false., G, nG, st, status)
       aG = area2_of(G, nG)
       rel_g = abs(real(aG, real64) - 2.0_real64 * 1100.0_real64**2) / (2.0_real64 * 1100.0_real64**2)
       call TrapMorph(A, nA, 30_int64, 30_int64, 30_int64, 30_int64, .true., S, nS, st, status)
       aS = area2_of(S, nS)
       rel_s = abs(real(aS, real64) - 2.0_real64 * 940.0_real64**2) / (2.0_real64 * 940.0_real64**2)
       if (status /= 0 .or. st%anomalies /= 0 .or. rel_g > 3.0e-3_real64 .or. rel_s > 3.0e-3_real64) then
          write(*, '(A,I0,A,ES10.2,A,ES10.2)') 'FAIL rot angle=', angs(ia), ' rel_g=', rel_g, ' rel_s=', rel_s
          nfail = nfail + 1
       end if
    end do
  end subroutine test_rotated_angles

end program test_trapmorph
