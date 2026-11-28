! File   : test_nurbs.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: End-to-end regression for NurbsCurveModule + NurbsBoolModule + CUDA nurbs_gpu.cu:
!          1. Exact 16-pt Gauss-Legendre area/perimeter on circle & curve1.json.
!          2. Bohm knot insertion (nurbs_to_beziers) on mems1.json 9-pt rational hub circle.
!          3. Exact analytical y-extrema, y-monotone split, and bez_x_at_y round-trip.
!          4. GPU (CUDA GB10) vs CPU N1/N2 parity.
!          5. N3 Curved-slab Booleans (OR, AND, NOT, XOR) on overlapping circles (exact lens
!             formula), curve1.json multiply-connected face, and mems1.json hub-ring.
!          6. N4 Native curvilinear offsetting (circle grow/shrink, two-circle comb-gap
!             collision global self-intersection trim, curve1.json d=1.5 outward offset).
!          7. N5 Bounded-memory Y-band streaming (M=1,4,16 exact area equivalence).
program test_nurbs
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use NurbsCurveModule
  use NurbsBoolModule
  implicit none

  real(real64), parameter :: PI = 3.14159265358979323846_real64
  integer(int32) :: nfail
  nfail = 0

  write(*,'(A,I0)') 'nurbs_gpu_available=', nurbs_gpu_available()
  call test_circle_and_curve1_gl(nfail)
  call test_bohm_knot_insertion(nfail)
  call test_y_monotone_and_inverse(nfail)
  call test_curved_booleans_circles(nfail)
  call test_curve1_native_boolean_and_offset(nfail)
  call test_curvilinear_offset_and_collision(nfail)
  call test_banded_streaming_n5(nfail)

  if (nfail == 0) then
     write(*,'(A)') 'test_nurbs: PASS'
  else
     write(*,'(A,I0)') 'test_nurbs: FAIL nfail=', nfail
     error stop 1
  end if

contains

  subroutine check_near(tag, got, want, tol, nfail)
    character(len=*), intent(in) :: tag
    real(real64), intent(in) :: got, want, tol
    integer(int32), intent(inout) :: nfail
    if (abs(got - want) > tol) then
       write(*,'(A,A,A,F18.10,A,F18.10,A,ES10.2)') 'FAIL ', tag, ' got=', got, ' want=', want, ' diff=', abs(got-want)
       nfail = nfail + 1
    end if
  end subroutine check_near

  subroutine test_circle_and_curve1_gl(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: spans(4), outer(5), hole(2)
    real(real64) :: a2, per
    integer(int32) :: i
    call nurbs_circle_bez(0.0_real64, 0.0_real64, 1.0_real64, spans)
    a2 = 0.0_real64; per = 0.0_real64
    do i = 1, 4
       a2 = a2 + bez_area2_gl(spans(i)); per = per + bez_perim_gl(spans(i))
    end do
    call check_near('circle_gl_area', 0.5_real64*a2, PI, 1.0e-12_real64, nfail)
    call check_near('circle_gl_perim', per, 2.0_real64*PI, 1.0e-12_real64, nfail)

    call nurbs_json_curve1_outer(outer); call nurbs_json_curve1_hole(hole)
    a2 = 0.0_real64; per = 0.0_real64
    do i = 1, 5
       a2 = a2 + bez_area2_gl(outer(i)); per = per + bez_perim_gl(outer(i))
    end do
    do i = 1, 2
       a2 = a2 + bez_area2_gl(hole(i));  per = per + bez_perim_gl(hole(i))
    end do
    call check_near('curve1_gl_area',  0.5_real64*a2, 4305.7795645_real64, 1.0e-4_real64, nfail)
    call check_near('curve1_gl_perim', per,            401.3327143_real64, 1.0e-4_real64, nfail)
  end subroutine test_circle_and_curve1_gl

  subroutine test_bohm_knot_insertion(nfail)
    integer(int32), intent(inout) :: nfail
    ! mems1.json C_rotor_hub_circle: degree 2, 9 control points, 12 knots, R=38.0
    real(real64), parameter :: R = 38.0_real64, WQ = 0.70710678118654752440_real64
    real(real64) :: knots(12), cx(9), cy(9), cw(9), a2
    type(BezSpan), allocatable :: spans(:)
    integer(int32) :: ns, i
    knots = [0.0_real64,0.0_real64,0.0_real64, 0.25_real64,0.25_real64, 0.5_real64,0.5_real64, &
             0.75_real64,0.75_real64, 1.0_real64,1.0_real64,1.0_real64]
    cx = [R, R, 0.0_real64, -R, -R, -R, 0.0_real64, R, R]
    cy = [0.0_real64, R, R, R, 0.0_real64, -R, -R, -R, 0.0_real64]
    cw = [1.0_real64, WQ, 1.0_real64, WQ, 1.0_real64, WQ, 1.0_real64, WQ, 1.0_real64]
    call nurbs_to_beziers(2_int32, 9_int32, knots, cx, cy, cw, 1_int64, spans, ns)
    if (ns /= 4) then
       write(*,'(A,I0)') 'FAIL bohm nspans=', ns; nfail = nfail + 1; return
    end if
    a2 = 0.0_real64
    do i = 1, ns
       a2 = a2 + bez_area2_gl(spans(i))
    end do
    call check_near('mems1_hub_bohm_area', 0.5_real64*a2, PI*R*R, 1.0e-9_real64, nfail)
  end subroutine test_bohm_knot_insertion

  subroutine test_y_monotone_and_inverse(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: b, out(8), L, R
    integer(int32) :: nout, i, j
    real(real64) :: t, x, y, t2, x2, py, dir, d
    logical :: ok
    ! Rational quadratic with interior y-extremum
    b%degree=2; b%flags=1; b%cw=[1.0_real64, 0.78_real64, 1.0_real64, 1.0_real64]
    b%cx=[5.0_real64, 21.0_real64, 42.0_real64, 0.0_real64]
    b%cy=[63.0_real64, 31.0_real64, 62.0_real64, 0.0_real64]
    call bez_split(b, 0.37_real64, L, R)
    call bez_eval(b, 0.37_real64, x, y); call bez_eval(L, 1.0_real64, x2, py)
    call check_near('split_left_x', x2, x, 1.0e-12_real64, nfail)
    call bez_make_y_monotone(b, nout, out)
    if (nout /= 2) then
       write(*,'(A,I0)') 'FAIL quad mono nout=', nout; nfail = nfail + 1; return
    end if
    do i = 1, nout
       call bez_eval(out(i), 0.43_real64, x, y)
       call bez_x_at_y(out(i), y, t2, x2, ok)
       if (.not. ok) then
          write(*,'(A)') 'FAIL bez_x_at_y ok=.false.'; nfail = nfail + 1
       else
          call check_near('bez_x_at_y_quad', x2, x, 1.0e-11_real64, nfail)
       end if
    end do
    ! Cubic with 2 y-extrema
    b%degree=3; b%flags=0; b%cw=[1.0_real64,1.0_real64,1.0_real64,1.0_real64]
    b%cx=[0.0_real64,1.0_real64,2.0_real64,3.0_real64]
    b%cy=[0.0_real64,2.0_real64,-1.0_real64,1.0_real64]
    call bez_make_y_monotone(b, nout, out)
    if (nout < 2) then
       write(*,'(A,I0)') 'FAIL cubic mono nout=', nout; nfail = nfail + 1; return
    end if
    do i = 1, nout
       dir = 0.0_real64; py = 0.0_real64
       do j = 0, 64
          t = real(j, real64) / 64.0_real64
          call bez_eval(out(i), t, x, y)
          if (j > 0) then
             d = y - py
             if (abs(d) > 1.0e-10_real64) then
                if (dir == 0.0_real64) dir = sign(1.0_real64, d)
                if (dir * d < -1.0e-9_real64) then
                   write(*,'(A)') 'FAIL cubic not y-monotone'; nfail = nfail + 1; return
                end if
             end if
          end if
          py = y
       end do
    end do
  end subroutine test_y_monotone_and_inverse

  subroutine test_curved_booleans_circles(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: cA(4), cB(4)
    type(CurvedTrap), allocatable :: tr(:)
    integer(int64) :: nt
    type(NurbsBoolStats) :: st
    real(real64) :: lens, want_or, want_and, want_not, want_xor, a_gpu, a_cpu
    ! Two unit circles centered at (0,0) and (1,0): exact lens area = 2*pi/3 - sqrt(3)/2
    call nurbs_circle_bez(0.0_real64, 0.0_real64, 1.0_real64, cA)
    call nurbs_circle_bez(1.0_real64, 0.0_real64, 1.0_real64, cB)
    lens     = 2.0_real64*PI/3.0_real64 - sqrt(3.0_real64)/2.0_real64
    want_and = lens
    want_or  = 2.0_real64*PI - lens
    want_not = PI - lens
    want_xor = want_or - want_and

    call nurbs_curved_boolean(cA, 4_int64, cB, 4_int64, NURBS_AND, tr, nt, st, use_gpu=.true.)
    a_gpu = nurbs_curved_area(tr, nt)
    call check_near('circles_AND_gpu', a_gpu, want_and, 1.0e-7_real64, nfail)

    call nurbs_curved_boolean(cA, 4_int64, cB, 4_int64, NURBS_AND, tr, nt, st, use_gpu=.false.)
    a_cpu = nurbs_curved_area(tr, nt)
    call check_near('circles_AND_gpu_vs_cpu', a_gpu, a_cpu, 1.0e-11_real64, nfail)

    call nurbs_curved_boolean(cA, 4_int64, cB, 4_int64, NURBS_OR, tr, nt, st, use_gpu=.true.)
    call check_near('circles_OR', nurbs_curved_area(tr, nt), want_or, 1.0e-7_real64, nfail)

    call nurbs_curved_boolean(cA, 4_int64, cB, 4_int64, NURBS_NOT, tr, nt, st, use_gpu=.true.)
    call check_near('circles_NOT', nurbs_curved_area(tr, nt), want_not, 1.0e-7_real64, nfail)

    call nurbs_curved_boolean(cA, 4_int64, cB, 4_int64, NURBS_XOR, tr, nt, st, use_gpu=.true.)
    call check_near('circles_XOR', nurbs_curved_area(tr, nt), want_xor, 1.0e-7_real64, nfail)
  end subroutine test_curved_booleans_circles

  subroutine test_curve1_native_boolean_and_offset(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: outer(5), hole(2), all_sp(7), emptyB(0)
    type(CurvedTrap), allocatable :: tr(:)
    integer(int64) :: nt
    type(NurbsBoolStats) :: st
    real(real64) :: area_native, area_off
    call nurbs_json_curve1_outer(outer); call nurbs_json_curve1_hole(hole)
    all_sp(1:5) = outer(1:5); all_sp(6:7) = hole(1:2)
    call nurbs_curved_boolean(all_sp, 7_int64, emptyB, 0_int64, NURBS_NONZERO, tr, nt, st, use_gpu=.true.)
    area_native = nurbs_curved_area(tr, nt)
    call check_near('curve1_native_slab_area', area_native, 4305.7795645_real64, 1.0e-4_real64, nfail)
    ! Native curvilinear outward offset d=1.5 vs Clipper2 reference 4901.543
    call nurbs_offset(all_sp, 7_int64, 1.5_real64, tr, nt, st, use_gpu=.true.)
    area_off = nurbs_curved_area(tr, nt)
    write(*,'(A,F12.4,A,I0,A,L1)') 'curve1_native_offset_d1.5 area=', area_off, ' traps=', nt, ' gpu=', st%used_gpu
    call check_near('curve1_offset_d1.5_vs_clipper2', area_off, 4901.543_real64, 5.0_real64, nfail)
  end subroutine test_curve1_native_boolean_and_offset

  subroutine test_curvilinear_offset_and_collision(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: c1(4), c2(4), pair(8)
    type(CurvedTrap), allocatable :: tr(:)
    integer(int64) :: nt
    type(NurbsBoolStats) :: st
    real(real64) :: d_cc, r_big, lens_big, want_pair
    ! Single unit circle grow +0.5 -> pi * 1.5^2, shrink -0.3 -> pi * 0.7^2
    call nurbs_circle_bez(0.0_real64, 0.0_real64, 1.0_real64, c1)
    call nurbs_offset(c1, 4_int64,  0.5_real64, tr, nt, st, use_gpu=.true.)
    call check_near('circle_grow_0.5', nurbs_curved_area(tr, nt), PI * 1.5_real64**2, 1.0e-5_real64, nfail)
    call nurbs_offset(c1, 4_int64, -0.3_real64, tr, nt, st, use_gpu=.true.)
    call check_near('circle_shrink_0.3', nurbs_curved_area(tr, nt), PI * 0.7_real64**2, 1.0e-5_real64, nfail)

    ! Two circles R=1.0 separated by gap g=0.6 (centers 0 and 2.6), grown by d=0.5 (2d=1.0 > g):
    ! Exact global Minkowski sum = union of two R=1.5 circles separated by 2.6!
    d_cc = 2.6_real64; r_big = 1.5_real64
    call nurbs_circle_bez(0.0_real64, 0.0_real64, 1.0_real64, c1)
    call nurbs_circle_bez(d_cc,       0.0_real64, 1.0_real64, c2)
    pair(1:4) = c1; pair(5:8) = c2
    lens_big = 2.0_real64*r_big*r_big*acos(d_cc/(2.0_real64*r_big)) - &
               0.5_real64*d_cc*sqrt(4.0_real64*r_big*r_big - d_cc*d_cc)
    want_pair = 2.0_real64*PI*r_big*r_big - lens_big
    call nurbs_offset(pair, 8_int64, 0.5_real64, tr, nt, st, use_gpu=.true.)
    call check_near('gap_collision_offset_trim', nurbs_curved_area(tr, nt), want_pair, 1.0e-4_real64, nfail)
  end subroutine test_curvilinear_offset_and_collision

  subroutine test_banded_streaming_n5(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: cA(4), cB(4)
    type(CurvedTrap), allocatable :: tr(:)
    integer(int64) :: nt
    type(NurbsBoolStats) :: st
    integer(int32), parameter :: MB(3) = [1, 4, 16]
    real(real64) :: want_or, a_band
    integer(int32) :: i
    call nurbs_circle_bez(0.0_real64, 0.0_real64, 1.0_real64, cA)
    call nurbs_circle_bez(1.0_real64, 0.0_real64, 1.0_real64, cB)
    want_or = 2.0_real64*PI - (2.0_real64*PI/3.0_real64 - sqrt(3.0_real64)/2.0_real64)
    do i = 1, 3
       call nurbs_banded_boolean(cA, 4_int64, cB, 4_int64, NURBS_OR, MB(i), tr, nt, st, use_gpu=.true.)
       a_band = nurbs_curved_area(tr, nt)
       call check_near('n5_banded_OR', a_band, want_or, 1.0e-6_real64, nfail)
    end do
  end subroutine test_banded_streaming_n5

end program test_nurbs
