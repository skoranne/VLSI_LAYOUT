! File   : nurbs_curve.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Degree <= 3 rational Bezier/NURBS curve primitives for curvilinear MEMS/ILT geometry.
!          Modern idiomatic Fortran: iso_fortran_env, iso_c_binding, pure/elemental, no class(...).
module NurbsCurveModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding,   only: c_int, c_int64_t, c_double, c_ptr
  implicit none
  private

  public :: BezSpan, NURBS_MAX_DEG
  public :: bez_eval, bez_split, bez_y_extrema, bez_make_y_monotone
  public :: bez_area2_green, bez_perimeter, bez_aabb
  public :: nurbs_circle_bez, nurbs_json_curve1_outer, nurbs_json_curve1_hole

  integer(int32), parameter :: NURBS_MAX_DEG = 3

  !> Rational Bezier span, degree 1..3. POD, no allocatable components.
  type, bind(c) :: BezSpan
     real(real64) :: cx(4) = 0.0_real64
     real(real64) :: cy(4) = 0.0_real64
     real(real64) :: cw(4) = 1.0_real64
     integer(int32) :: degree = 1
     integer(int32) :: flags  = 0   ! bit0=rational, bit1=y-monotone
     integer(int64) :: owner  = 0_int64
     integer(int64) :: band   = 0_int64
  end type BezSpan

  interface
     integer(c_int) function nurbs_gpu_split_y_monotone(in_spans, n, out_spans, max_out) &
          bind(c, name='nurbs_gpu_split_y_monotone')
       import :: c_int, BezSpan
       type(BezSpan), intent(in)  :: in_spans(*)
       integer(c_int), value      :: n, max_out
       type(BezSpan), intent(out) :: out_spans(*)
     end function nurbs_gpu_split_y_monotone
  end interface

contains

  pure elemental subroutine bez_eval(b, t, x, y)
    type(BezSpan), intent(in)  :: b
    real(real64),  intent(in)  :: t
    real(real64),  intent(out) :: x, y
    real(real64) :: bx(4), by(4), bw(4), u
    integer(int32) :: p, r, j
    p = b%degree
    bx = 0.0_real64; by = 0.0_real64; bw = 0.0_real64
    do j = 1, p + 1
       bx(j) = b%cx(j) * b%cw(j)
       by(j) = b%cy(j) * b%cw(j)
       bw(j) = b%cw(j)
    end do
    do r = 1, p
       do j = 1, p - r + 1
          u = 1.0_real64 - t
          bx(j) = u * bx(j) + t * bx(j+1)
          by(j) = u * by(j) + t * by(j+1)
          bw(j) = u * bw(j) + t * bw(j+1)
       end do
    end do
    x = bx(1) / bw(1)
    y = by(1) / bw(1)
  end subroutine bez_eval

  pure subroutine bez_split(b, t, L, R)
    type(BezSpan), intent(in)  :: b
    real(real64),  intent(in)  :: t
    type(BezSpan), intent(out) :: L, R
    real(real64) :: hx(0:3,0:3), hy(0:3,0:3), hw(0:3,0:3), u, w
    integer(int32) :: p, rr, j, i
    p = b%degree
    hx = 0.0_real64; hy = 0.0_real64; hw = 0.0_real64
    do i = 0, p
       w = b%cw(i+1)
       hx(0,i) = b%cx(i+1)*w; hy(0,i) = b%cy(i+1)*w; hw(0,i) = w
    end do
    do rr = 1, p
       do j = 0, p - rr
          u = 1.0_real64 - t
          hx(rr,j) = u*hx(rr-1,j) + t*hx(rr-1,j+1)
          hy(rr,j) = u*hy(rr-1,j) + t*hy(rr-1,j+1)
          hw(rr,j) = u*hw(rr-1,j) + t*hw(rr-1,j+1)
       end do
    end do
    L = b; R = b
    do i = 0, p
       L%cw(i+1) = hw(i,0);   L%cx(i+1) = hx(i,0)/hw(i,0);   L%cy(i+1) = hy(i,0)/hw(i,0)
       R%cw(i+1) = hw(p-i,i); R%cx(i+1) = hx(p-i,i)/hw(p-i,i); R%cy(i+1) = hy(p-i,i)/hw(p-i,i)
    end do
  end subroutine bez_split

  pure subroutine bez_aabb(b, x0, y0, x1, y1)
    type(BezSpan), intent(in)  :: b
    real(real64),  intent(out) :: x0, y0, x1, y1
    integer(int32) :: i
    x0 = b%cx(1); x1 = b%cx(1); y0 = b%cy(1); y1 = b%cy(1)
    do i = 2, b%degree + 1
       x0 = min(x0, b%cx(i)); x1 = max(x1, b%cx(i))
       y0 = min(y0, b%cy(i)); y1 = max(y1, b%cy(i))
    end do
  end subroutine bez_aabb

  !> Count and return y-extrema parameters in (0,1) for degree<=3.
  !> Non-rational: exact quadratic/linear. Rational: robust scan+bisection on dy/dt.
  pure subroutine bez_y_extrema(b, nroots, roots)
    type(BezSpan), intent(in)  :: b
    integer(int32), intent(out) :: nroots
    real(real64),  intent(out) :: roots(4)
    real(real64) :: dc(3), a, bv, c, disc, t1, t2, lo, hi, mid, flo, fhi, fm, prev, cur, r
    integer(int32) :: p, i, it, k
    logical :: dup
    nroots = 0; roots = 0.0_real64
    p = b%degree
    if (p <= 1) return
    if (iand(b%flags, 1_int32) == 0) then
       do i = 1, p
          dc(i) = real(p,real64) * (b%cy(i+1) - b%cy(i))
       end do
       if (p == 2) then
          if (abs(dc(1)-dc(2)) > 1.0e-18_real64) then
             t1 = dc(1) / (dc(1) - dc(2))
             if (t1 > 1.0e-12_real64 .and. t1 < 1.0_real64-1.0e-12_real64) then
                nroots = 1; roots(1) = t1
             end if
          end if
       else if (p == 3) then
          a  = dc(1) - 2.0_real64*dc(2) + dc(3)
          bv = 2.0_real64*(dc(2)-dc(1))
          c  = dc(1)
          if (abs(a) < 1.0e-18_real64) then
             if (abs(bv) > 1.0e-18_real64) then
                t1 = -c/bv
                if (t1 > 1.0e-12_real64 .and. t1 < 1.0_real64-1.0e-12_real64) then
                   nroots=1; roots(1)=t1
                end if
             end if
          else
             disc = bv*bv - 4.0_real64*a*c
             if (disc >= 0.0_real64) then
                disc = sqrt(disc)
                t1 = (-bv-disc)/(2.0_real64*a); t2 = (-bv+disc)/(2.0_real64*a)
                if (t1 > 1.0e-12_real64 .and. t1 < 1.0_real64-1.0e-12_real64) then
                   nroots=nroots+1; roots(nroots)=t1
                end if
                if (t2 > 1.0e-12_real64 .and. t2 < 1.0_real64-1.0e-12_real64 .and. abs(t2-t1)>1.0e-12_real64) then
                   nroots=nroots+1; roots(nroots)=t2
                end if
             end if
          end if
       end if
       return
    end if
    ! Rational: numerical dy/dt scan+bisection
    prev = dy_dt_num(b, 1.0e-12_real64)
    do i = 1, 64
       cur = dy_dt_num(b, real(i,real64)/64.0_real64)
       if (prev == 0.0_real64 .or. prev*cur <= 0.0_real64) then
          lo = real(i-1,real64)/64.0_real64; hi = real(i,real64)/64.0_real64
          flo = prev; fhi = cur
          do it = 1, 80
             mid = 0.5_real64*(lo+hi); fm = dy_dt_num(b, mid)
             if (flo*fm <= 0.0_real64) then
                hi = mid; fhi = fm
             else
                lo = mid; flo = fm
             end if
          end do
          r = 0.5_real64*(lo+hi)
          if (r > 1.0e-10_real64 .and. r < 1.0_real64-1.0e-10_real64) then
             dup = .false.
             do k = 1, nroots
                if (abs(roots(k)-r) < 1.0e-8_real64) dup = .true.
             end do
             if (.not. dup .and. nroots < 4) then
                nroots = nroots + 1; roots(nroots) = r
             end if
          end if
       end if
       prev = cur
    end do
  end subroutine bez_y_extrema

  pure real(real64) function dy_dt_num(b, t) result(dy)
    type(BezSpan), intent(in) :: b
    real(real64), intent(in)  :: t
    real(real64) :: eps, t0, t1, x0,y0,x1,y1
    eps = 1.0e-7_real64
    t0 = max(0.0_real64, t-eps); t1 = min(1.0_real64, t+eps)
    call bez_eval(b, t0, x0, y0); call bez_eval(b, t1, x1, y1)
    dy = (y1-y0) / (t1-t0)
  end function dy_dt_num

  !> Split into y-monotone spans. out_n <= 8.
  pure subroutine bez_make_y_monotone(b, nout, out)
    type(BezSpan), intent(in)  :: b
    integer(int32), intent(out) :: nout
    type(BezSpan), intent(out) :: out(8)
    real(real64) :: roots(4), ts(4), last, local_t
    integer(int32) :: nroots, i, j, nts
    type(BezSpan) :: rem, L, R
    logical :: swapped
    real(real64) :: tmp
    call bez_y_extrema(b, nroots, roots)
    nts = 0; ts = 0.0_real64
    do i = 1, nroots
       ts(nts+1) = roots(i); nts = nts + 1
    end do
    ! simple sort
    do i = 1, nts
       do j = i+1, nts
          if (ts(j) < ts(i)) then
             tmp=ts(i); ts(i)=ts(j); ts(j)=tmp
          end if
       end do
    end do
    nout = 0; rem = b; last = 0.0_real64
    do i = 1, nts
       if (ts(i)-last <= 1.0e-10_real64) cycle
       local_t = (ts(i)-last) / (1.0_real64-last)
       if (local_t <= 1.0e-10_real64 .or. local_t >= 1.0_real64-1.0e-10_real64) cycle
       call bez_split(rem, local_t, L, R)
       L%flags = ior(L%flags, 2_int32)
       nout = nout + 1; out(nout) = L
       rem = R; last = ts(i)
    end do
    rem%flags = ior(rem%flags, 2_int32)
    nout = nout + 1; out(nout) = rem
  end subroutine bez_make_y_monotone

  !> Green's theorem area contribution (signed, 2*area) approximation by sampling.
  pure real(real64) function bez_area2_green(b, ns) result(a2)
    type(BezSpan), intent(in) :: b
    integer(int32), intent(in) :: ns
    real(real64) :: x0,y0,x1,y1,t0,t1
    integer(int32) :: i
    a2 = 0.0_real64
    do i = 0, ns-1
       t0 = real(i,real64)/real(ns,real64)
       t1 = real(i+1,real64)/real(ns,real64)
       call bez_eval(b, t0, x0, y0)
       call bez_eval(b, t1, x1, y1)
       a2 = a2 + (x0*y1 - x1*y0)
    end do
  end function bez_area2_green

  pure real(real64) function bez_perimeter(b, ns) result(p)
    type(BezSpan), intent(in) :: b
    integer(int32), intent(in) :: ns
    real(real64) :: x0,y0,x1,y1,t0,t1
    integer(int32) :: i
    p = 0.0_real64
    do i = 0, ns-1
       t0 = real(i,real64)/real(ns,real64)
       t1 = real(i+1,real64)/real(ns,real64)
       call bez_eval(b, t0, x0, y0)
       call bez_eval(b, t1, x1, y1)
       p = p + hypot(x1-x0, y1-y0)
    end do
  end function bez_perimeter

  !> Build 4 quarter-circle rational quadratic Bezier spans (unit circle, CCW).
  pure subroutine nurbs_circle_bez(cx0, cy0, r, spans)
    real(real64), intent(in) :: cx0, cy0, r
    type(BezSpan), intent(out) :: spans(4)
    real(real64), parameter :: wq = 0.70710678118654752440_real64
    integer(int32) :: k
    do k = 1, 4
       spans(k)%degree = 2; spans(k)%flags = 1; spans(k)%owner = 0_int64; spans(k)%band = 0_int64
       spans(k)%cw = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
       spans(k)%cx = 0.0_real64; spans(k)%cy = 0.0_real64
    end do
    spans(1)%cx(1:3) = [cx0+r, cx0+r, cx0];   spans(1)%cy(1:3) = [cy0,   cy0+r, cy0+r]; spans(1)%cw(2)=wq
    spans(2)%cx(1:3) = [cx0,   cx0-r, cx0-r]; spans(2)%cy(1:3) = [cy0+r, cy0+r, cy0];   spans(2)%cw(2)=wq
    spans(3)%cx(1:3) = [cx0-r, cx0-r, cx0];   spans(3)%cy(1:3) = [cy0,   cy0-r, cy0-r]; spans(3)%cw(2)=wq
    spans(4)%cx(1:3) = [cx0,   cx0+r, cx0+r]; spans(4)%cy(1:3) = [cy0-r, cy0-r, cy0];   spans(4)%cw(2)=wq
  end subroutine nurbs_circle_bez

  !> curve1.json outer wire (5 curves) as Bezier spans.
  subroutine nurbs_json_curve1_outer(spans)
    type(BezSpan), intent(out) :: spans(5)
    integer(int32) :: k
    do k = 1, 5
       spans(k)%degree=2; spans(k)%flags=1; spans(k)%owner=0_int64; spans(k)%band=0_int64
       spans(k)%cw=[1.0_real64,1.0_real64,1.0_real64,1.0_real64]
       spans(k)%cx=0.0_real64; spans(k)%cy=0.0_real64
    end do
    spans(1)%cx(1:3)=[42.0_real64,56.0_real64,34.0_real64]; spans(1)%cy(1:3)=[62.0_real64,32.0_real64,3.0_real64];  spans(1)%cw(2)=0.85_real64
    spans(2)%degree=1; spans(2)%flags=0; spans(2)%cx(1:2)=[34.0_real64,89.0_real64]; spans(2)%cy(1:2)=[3.0_real64,9.0_real64]
    spans(3)%cx(1:3)=[89.0_real64,64.0_real64,96.0_real64]; spans(3)%cy(1:3)=[9.0_real64,56.0_real64,90.0_real64]; spans(3)%cw(2)=0.82_real64
    spans(4)%cx(1:3)=[96.0_real64,48.0_real64,5.0_real64];  spans(4)%cy(1:3)=[90.0_real64,106.0_real64,63.0_real64]; spans(4)%cw(2)=0.88_real64
    spans(5)%cx(1:3)=[5.0_real64,21.0_real64,42.0_real64];  spans(5)%cy(1:3)=[63.0_real64,31.0_real64,62.0_real64]; spans(5)%cw(2)=0.78_real64
  end subroutine nurbs_json_curve1_outer

  subroutine nurbs_json_curve1_hole(spans)
    type(BezSpan), intent(out) :: spans(2)
    integer(int32) :: k
    do k = 1, 2
       spans(k)%degree=1; spans(k)%flags=0; spans(k)%owner=0_int64; spans(k)%band=0_int64
       spans(k)%cw=[1.0_real64,1.0_real64,1.0_real64,1.0_real64]
       spans(k)%cx=0.0_real64; spans(k)%cy=0.0_real64
    end do
    spans(1)%degree=2; spans(1)%flags=1; spans(1)%cx(1:3)=[59.0_real64,72.0_real64,57.0_real64]; spans(1)%cy(1:3)=[87.0_real64,76.0_real64,65.0_real64]; spans(1)%cw(2)=0.80_real64
    spans(2)%cx(1:2)=[57.0_real64,59.0_real64]; spans(2)%cy(1:2)=[65.0_real64,87.0_real64]
  end subroutine nurbs_json_curve1_hole

end module NurbsCurveModule
