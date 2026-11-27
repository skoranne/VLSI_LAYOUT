! File   : test_trappred.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Unit test for TrapPredModule (rotation-invariant & rectilinear polygon predicates)
!          across 0, 1, 15, 30, 45, 90, 137 degree rotations.
program test_trappred
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use TrapezoidModule
  use TrapMergeModule
  use TrapPNumModule
  use TrapPredModule
  implicit none

  real(real64), parameter :: PI = 3.14159265358979323846_real64
  real(real64), parameter :: PREC = 1000.0_real64
  integer, parameter :: ANGS(7) = [0, 1, 15, 30, 45, 90, 137]
  integer :: nfail, ia

  nfail = 0
  do ia = 1, size(ANGS)
     call test_shapes_at_angle(ANGS(ia), nfail)
  end do
  call test_exact_45_octagon(nfail)

  if (nfail == 0) then
     write(*, '(A)') 'test_trappred: PASS'
  else
     write(*, '(A,I0)') 'test_trappred: FAIL nfail=', nfail
     error stop 1
  end if

contains

  subroutine rot_poly(x0, y0, n, deg, px, py)
    real(real64), intent(in) :: x0(:), y0(:)
    integer, intent(in) :: n, deg
    integer(int64), intent(out) :: px(:), py(:)
    real(real64) :: th, c, s
    integer :: i
    th = real(deg, real64) * PI / 180.0_real64
    c = cos(th); s = sin(th)
    do i = 1, n
       px(i) = nint(c * x0(i) - s * y0(i), int64)
       py(i) = nint(s * x0(i) + c * y0(i), int64)
    end do
  end subroutine rot_poly

  subroutine check_sel(tag, deg, traps, ntrap, pred, cmp, v1, v2, want_sel, nfail)
    character(len=*), intent(in) :: tag
    integer, intent(in) :: deg
    type(Trapezoid), intent(in) :: traps(:)
    integer(int64), intent(in) :: ntrap, want_sel
    integer(int32), intent(in) :: pred, cmp
    real(real64), intent(in) :: v1, v2
    integer, intent(inout) :: nfail
    type(Trapezoid), allocatable :: out(:)
    integer(int64) :: nout
    type(TrapPredStats) :: st
    call TrapPredSelect(traps, ntrap, pred, cmp, v1, v2, PREC, out, nout, st)
    if (st%anomalies /= 0 .or. st%selected /= want_sel) then
       write(*, '(A,A,A,I0,A,I0,A,I0,A,I0)') 'FAIL ', tag, ' deg=', deg, &
            ' got=', st%selected, ' want=', want_sel, ' anom=', st%anomalies
       nfail = nfail + 1
    end if
  end subroutine check_sel

  subroutine test_shapes_at_angle(deg, nfail)
    integer, intent(in) :: deg
    integer, intent(inout) :: nfail
    ! 5 disjoint polygons in one scene (all rotated together by deg):
    !  P1: 2000 x 1000 rectangle (area=2.0 um^2, perim=6.0, W=1.0, L=2.0, AR=2.0, convex, rect)
    !  P2: 1000 x 1000 square    (area=1.0 um^2, perim=4.0, W=1.0, L=1.0, AR=1.0, convex, rect+sq)
    !  P3: L-shape 2000x2000 minus 1000x1000 corner (area=3.0 um^2, 6 verts, 1 reflex, concave)
    !  P4: U-notch 2000x1500 with 200-wide notch (0.2 um gap)
    !  P5: Ring 2000x2000 with 800x800 hole (area=3.36 um^2, 1 hole, wall=0.6 um)
    integer(int64) :: poff(6), px(30), py(30), hpo(2), hx(4), hy(4)
    real(real64) :: r1x(4), r1y(4), r2x(4), r2y(4), l3x(6), l3y(6), u4x(8), u4y(8)
    real(real64) :: o5x(4), o5y(4), i5x(4), i5y(4)
    type(TrapList) :: res(1)
    type(TrapMergeStats) :: mst
    integer(int32) :: ops(1)
    integer :: status
    integer(int64) :: want_ortho, want_45, want_axrect, want_axsq

    ! Place far apart so they never touch
    r1x = [0d0, 2000d0, 2000d0, 0d0];               r1y = [0d0, 0d0, 1000d0, 1000d0]
    r2x = [4000d0, 5000d0, 5000d0, 4000d0];         r2y = [0d0, 0d0, 1000d0, 1000d0]
    l3x = [8000d0, 10000d0, 10000d0, 9000d0, 9000d0, 8000d0]
    l3y = [0d0,    0d0,     1000d0,  1000d0, 2000d0, 2000d0]
    ! U-shape: outer [12000,14000]x[0,1500], notch cutout [12900,13100]x[500,1500] -> 200 dbu = 0.2 um gap
    u4x = [12000d0, 14000d0, 14000d0, 13100d0, 13100d0, 12900d0, 12900d0, 12000d0]
    u4y = [0d0,     0d0,     1500d0,  1500d0,  500d0,   500d0,   1500d0,  1500d0]
    ! Ring outer [16000,18000]x[0,2000], hole [16600,17400]x[600,1400]
    o5x = [16000d0, 18000d0, 18000d0, 16000d0];     o5y = [0d0, 0d0, 2000d0, 2000d0]
    i5x = [16600d0, 17400d0, 17400d0, 16600d0];     i5y = [600d0, 600d0, 1400d0, 1400d0]

    poff = [1_int64, 5_int64, 9_int64, 15_int64, 23_int64, 27_int64]
    call rot_poly(r1x, r1y, 4, deg, px(1:4), py(1:4))
    call rot_poly(r2x, r2y, 4, deg, px(5:8), py(5:8))
    call rot_poly(l3x, l3y, 6, deg, px(9:14), py(9:14))
    call rot_poly(u4x, u4y, 8, deg, px(15:22), py(15:22))
    call rot_poly(o5x, o5y, 4, deg, px(23:26), py(23:26))

    hpo = [1_int64, 5_int64]
    call rot_poly(i5x, i5y, 4, deg, hx(1:4), hy(1:4))

    ops(1) = TOP_NOT
    call TrapBooleanPolygons(5_int64, poff, px(1:26), py(1:26), 1_int64, hpo, hx, hy, 1, ops, res, mst, status)
    if (status /= 0 .or. mst%anomalies /= 0) error stop 'scene build failed'

    ! Rotation-invariant predicates (identical expected counts at ALL 7 angles!)
    call check_sel('AREA>2.5',     deg, res(1)%t, res(1)%n, TPRED_AREA, TCMP_GT, 2.5d0, 0d0, 3_int64, nfail) ! P3(3),P4(2.8),P5(3.36)
    call check_sel('PERIM>7.5',    deg, res(1)%t, res(1)%n, TPRED_PERIMETER, TCMP_GT, 7.5d0, 0d0, 3_int64, nfail) ! P3(8),P4(9),P5(11.2)
    call check_sel('HOLES==1',     deg, res(1)%t, res(1)%n, TPRED_HOLES, TCMP_EQ, 1.0d0, 0d0, 1_int64, nfail) ! P5
    call check_sel('IS_RECT_ROT',  deg, res(1)%t, res(1)%n, TPRED_IS_RECT_ROT, TCMP_NONE, 0d0, 0d0, 2_int64, nfail) ! P1,P2
    call check_sel('IS_SQ_ROT',    deg, res(1)%t, res(1)%n, TPRED_IS_SQ_ROT, TCMP_NONE, 0d0, 0d0, 1_int64, nfail) ! P2
    call check_sel('CONVEX',       deg, res(1)%t, res(1)%n, TPRED_CONVEX, TCMP_NONE, 0d0, 0d0, 2_int64, nfail) ! P1,P2
    call check_sel('CONCAVE',      deg, res(1)%t, res(1)%n, TPRED_CONCAVE, TCMP_NONE, 0d0, 0d0, 3_int64, nfail) ! P3,P4,P5
    call check_sel('WIDTH>1.2',    deg, res(1)%t, res(1)%n, TPRED_WIDTH, TCMP_GT, 1.2d0, 0d0, 3_int64, nfail) ! P3,P4,P5
    call check_sel('LENGTH>1.5',   deg, res(1)%t, res(1)%n, TPRED_LENGTH, TCMP_GT, 1.5d0, 0d0, 4_int64, nfail) ! P1,P3,P4,P5
    call check_sel('ASPECT>1.8',   deg, res(1)%t, res(1)%n, TPRED_ASPECT, TCMP_GT, 1.8d0, 0d0, 1_int64, nfail) ! P1(2.0)
    call check_sel('DIAM>2.6',     deg, res(1)%t, res(1)%n, TPRED_DIAMETER, TCMP_GT, 2.6d0, 0d0, 2_int64, nfail) ! P3(2.83),P5(2.83)
    call check_sel('SOLIDITY<0.9', deg, res(1)%t, res(1)%n, TPRED_SOLIDITY, TCMP_LT, 0.9d0, 0d0, 2_int64, nfail) ! P3(0.857),P5(0.84)
    call check_sel('NOTCH<0.3',    deg, res(1)%t, res(1)%n, TPRED_NOTCH, TCMP_LT, 0.3d0, 0d0, 1_int64, nfail) ! P4(0.2 um)
    ! ENCLOSE_RECTANGLE:
    !   1.9 x 0.9 fits in P1(2x1) and P3(L-arm 2x1), NOT in P2(1x1), P4(0.5 bottom / 0.9x1.5 arms), P5(0.6 wall)
    call check_sel('ENC_1.9x0.9',  deg, res(1)%t, res(1)%n, TPRED_ENCLOSE_RECT, TCMP_NONE, 1.9d0, 0.9d0, 2_int64, nfail)
    !   1.8 x 0.55 fits in P1(2x1), P3(2x1), P5(wall 2x0.6); NOT in P2(1x1) or P4(bottom 2x0.5, arms 0.9x1.5)
    call check_sel('ENC_1.8x0.55', deg, res(1)%t, res(1)%n, TPRED_ENCLOSE_RECT, TCMP_NONE, 1.8d0, 0.55d0, 3_int64, nfail)
    !   0.95 x 0.95 fits in P1, P2, P3; NOT in P4(arms 0.9) or P5(wall 0.6)
    call check_sel('ENC_0.95x0.95',deg, res(1)%t, res(1)%n, TPRED_ENCLOSE_RECT, TCMP_NONE, 0.95d0, 0.95d0, 3_int64, nfail)

    ! Orientation-specific predicates (NOT rotation-invariant)
    want_ortho  = merge(5_int64, 0_int64, mod(deg, 90) == 0)
    want_axrect = merge(2_int64, 0_int64, mod(deg, 90) == 0)
    want_axsq   = merge(1_int64, 0_int64, mod(deg, 90) == 0)
    call check_sel('IS_RECTILIN',  deg, res(1)%t, res(1)%n, TPRED_IS_RECTILIN, TCMP_NONE, 0d0, 0d0, want_ortho, nfail)
    if (mod(deg, 90) == 0) then
       call check_sel('IS_45', deg, res(1)%t, res(1)%n, TPRED_IS_45, TCMP_NONE, 0d0, 0d0, 5_int64, nfail)
    else if (mod(deg, 45) /= 0) then
       call check_sel('IS_45', deg, res(1)%t, res(1)%n, TPRED_IS_45, TCMP_NONE, 0d0, 0d0, 0_int64, nfail)
    end if
    call check_sel('IS_RECT',      deg, res(1)%t, res(1)%n, TPRED_IS_RECT, TCMP_NONE, 0d0, 0d0, want_axrect, nfail)
    call check_sel('IS_SQ',        deg, res(1)%t, res(1)%n, TPRED_IS_SQ, TCMP_NONE, 0d0, 0d0, want_axsq, nfail)
  end subroutine test_shapes_at_angle

  subroutine test_exact_45_octagon(nfail)
    integer, intent(inout) :: nfail
    integer(int64) :: poff(3), px(12), py(12), ntrap
    type(Trapezoid), allocatable :: T(:)
    type(TrapMergeStats) :: mst
    integer :: status
    ! P1: exact 45-deg octagon (dx=0, dy=0, or |dx|=|dy|); P2: non-45 slanted diamond (slope 2:1)
    poff = [1_int64, 9_int64, 13_int64]
    px(1:8) = [200_int64, 800_int64, 1000_int64, 1000_int64, 800_int64, 200_int64, 0_int64, 0_int64]
    py(1:8) = [0_int64,   0_int64,   200_int64,  800_int64,  1000_int64, 1000_int64, 800_int64, 200_int64]
    px(9:12) = [3000_int64, 3400_int64, 3200_int64, 2800_int64]
    py(9:12) = [0_int64,    200_int64,  600_int64,  400_int64]
    call TrapMergePolygons(2_int64, poff, px, py, T, ntrap, mst, status)
    if (status /= 0 .or. mst%anomalies /= 0) error stop 'octagon build failed'
    call check_sel('OCT_IS_45',       45, T, ntrap, TPRED_IS_45, TCMP_NONE, 0d0, 0d0, 1_int64, nfail)
    call check_sel('OCT_IS_RECTILIN', 45, T, ntrap, TPRED_IS_RECTILIN, TCMP_NONE, 0d0, 0d0, 0_int64, nfail)
    call check_sel('OCT_CONVEX',      45, T, ntrap, TPRED_CONVEX, TCMP_NONE, 0d0, 0d0, 2_int64, nfail)
    call check_sel('OCT_IS_RECT_ROT', 45, T, ntrap, TPRED_IS_RECT_ROT, TCMP_NONE, 0d0, 0d0, 1_int64, nfail)
  end subroutine test_exact_45_octagon

end program test_trappred
