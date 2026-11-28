! File   : test_nurbs.f90
! Purpose: Regression for NurbsCurveModule - degree<=3 rational Bezier/NURBS primitives.
program test_nurbs
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use NurbsCurveModule
  implicit none

  integer(int32) :: nfail
  nfail = 0

  call test_circle_area(nfail)
  call test_curve1_area(nfail)
  call test_y_monotone_split(nfail)
  call test_eval_split_consistency(nfail)

  if (nfail == 0) then
     write(*,'(A)') 'test_nurbs: PASS'
  else
     write(*,'(A,I0)') 'test_nurbs: FAIL nfail=', nfail
     error stop 1
  end if

contains

  subroutine test_circle_area(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: spans(4)
    real(real64) :: a2, pi_ref, area
    integer(int32) :: i
    pi_ref = 3.14159265358979323846_real64
    call nurbs_circle_bez(0.0_real64, 0.0_real64, 1.0_real64, spans)
    a2 = 0.0_real64
    do i = 1, 4
       a2 = a2 + bez_area2_green(spans(i), 200000)
    end do
    area = 0.5_real64 * a2
    if (abs(area - pi_ref) > 1.0e-7_real64) then
       write(*,'(A,F16.12,A,F16.12)') 'FAIL circle area=', area, ' want=', pi_ref
       nfail = nfail + 1
    end if
  end subroutine test_circle_area

  subroutine test_curve1_area(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: outer(5), hole(2)
    real(real64) :: a2, area, per, want_a, want_p
    integer(int32) :: i
    ! Reference from nurbs_eval.py nseg=4096: area=4305.779560338 perim=401.332712656
    want_a = 4305.779560338_real64
    want_p = 401.332712656_real64
    call nurbs_json_curve1_outer(outer)
    call nurbs_json_curve1_hole(hole)
    a2 = 0.0_real64; per = 0.0_real64
    do i = 1, 5
       a2 = a2 + bez_area2_green(outer(i), 400000)
       per = per + bez_perimeter(outer(i), 400000)
    end do
    do i = 1, 2
       a2 = a2 + bez_area2_green(hole(i), 400000)
       per = per + bez_perimeter(hole(i), 400000)
    end do
    area = 0.5_real64 * a2
    if (abs(area - want_a) > 5.0e-4_real64) then
       write(*,'(A,F18.9,A,F18.9)') 'FAIL curve1 area=', area, ' want~=', want_a
       nfail = nfail + 1
    end if
    if (abs(per - want_p) > 5.0e-4_real64) then
       write(*,'(A,F18.9,A,F18.9)') 'FAIL curve1 perim=', per, ' want~=', want_p
       nfail = nfail + 1
    end if
  end subroutine test_curve1_area

  subroutine test_y_monotone_split(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: b, out(8)
    integer(int32) :: nout, i, j
    real(real64) :: t, x,y, py, dir, d
    ! cubic with y-extrema: control y 0,1,-1,0
    b%degree=3; b%flags=0; b%cw=[1.0_real64,1.0_real64,1.0_real64,1.0_real64]
    b%cx=[0.0_real64,1.0_real64,2.0_real64,3.0_real64]
    b%cy=[0.0_real64,2.0_real64,-1.0_real64,1.0_real64]
    call bez_make_y_monotone(b, nout, out)
    if (nout < 2) then
       write(*,'(A,I0)') 'FAIL y-monotone split nout=', nout
       nfail = nfail + 1; return
    end if
    do i = 1, nout
       dir = 0.0_real64; py = -huge(1.0_real64)
       do j = 0, 100
          t = real(j,real64)/100.0_real64
          call bez_eval(out(i), t, x, y)
          if (j > 0) then
             d = y - py
             if (abs(d) > 1.0e-10_real64) then
                if (dir == 0.0_real64) dir = sign(1.0_real64,d)
                if (dir*d < -1.0e-9_real64) then
                   write(*,'(A,I0,A,I0)') 'FAIL not y-monotone span=', i, ' at j=', j
                   nfail = nfail + 1; return
                end if
             end if
          end if
          py = y
       end do
    end do
  end subroutine test_y_monotone_split

  subroutine test_eval_split_consistency(nfail)
    integer(int32), intent(inout) :: nfail
    type(BezSpan) :: b, L, R
    real(real64) :: x1,y1,x2,y2
    b%degree=2; b%flags=1; b%cw=[1.0_real64,0.8_real64,1.0_real64,1.0_real64]
    b%cx=[0.0_real64,1.0_real64,2.0_real64,0.0_real64]
    b%cy=[0.0_real64,2.0_real64,0.0_real64,0.0_real64]
    call bez_split(b, 0.37_real64, L, R)
    call bez_eval(b, 0.37_real64, x1, y1)
    call bez_eval(L, 1.0_real64, x2, y2)
    if (hypot(x1-x2,y1-y2) > 1.0e-12_real64) then
       write(*,'(A)') 'FAIL split left end mismatch'; nfail=nfail+1
    end if
    call bez_eval(R, 0.0_real64, x2, y2)
    if (hypot(x1-x2,y1-y2) > 1.0e-12_real64) then
       write(*,'(A)') 'FAIL split right start mismatch'; nfail=nfail+1
    end if
  end subroutine test_eval_split_consistency

end program test_nurbs
