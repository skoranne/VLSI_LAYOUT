! File   : nurbs_curve.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Degree <= 3 rational Bezier/NURBS curve primitives for curvilinear MEMS/ILT geometry.
!          Modern idiomatic Fortran: iso_fortran_env, iso_c_binding, pure/elemental, no class(...),
!          zero allocatable components in POD structs (Intel ifx OpenMP-safe).
module NurbsCurveModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding,   only: c_int, c_int64_t, c_double, c_ptr
  implicit none
  private

  public :: BezSpan, CurvedTrap, NURBS_MAX_DEG
  public :: bez_line, bez_eval, bez_eval_deriv, bez_curvature, bez_split, bez_subspan, bez_reverse
  public :: bez_y_extrema, bez_make_y_monotone, bez_x_at_y, bez_aabb
  public :: bez_area2_green, bez_perimeter, bez_area2_gl, bez_perim_gl, curved_trap_area
  public :: bez_intersect, nurbs_to_beziers
  public :: nurbs_circle_bez, nurbs_ellipse_bez, nurbs_arc_bez
  public :: nurbs_json_curve1_outer, nurbs_json_curve1_hole
  public :: nurbs_gpu_available, nurbs_gpu_split_y_monotone, nurbs_gpu_find_overlaps

  integer(int32), parameter :: NURBS_MAX_DEG = 3

  ! 16-point Gauss-Legendre nodes and weights on [0, 1]
  real(real64), parameter :: GL16_X(16) = [ &
       0.005299532504175034_real64, 0.027712488463383712_real64, &
       0.067184398806084126_real64, 0.122297795822498480_real64, &
       0.191061877798678122_real64, 0.270991611171386308_real64, &
       0.359198224610370543_real64, 0.452493745081181279_real64, &
       0.547506254918818721_real64, 0.640801775389629457_real64, &
       0.729008388828613692_real64, 0.808938122201321878_real64, &
       0.877702204177501520_real64, 0.932815601193915874_real64, &
       0.972287511536616288_real64, 0.994700467495824966_real64 ]
  real(real64), parameter :: GL16_W(16) = [ &
       0.013576229705877047_real64, 0.031126761969323946_real64, &
       0.047579255841246392_real64, 0.062314485627766936_real64, &
       0.074797994408288370_real64, 0.084578259697501269_real64, &
       0.091301707522461791_real64, 0.094725305227534250_real64, &
       0.094725305227534250_real64, 0.091301707522461791_real64, &
       0.084578259697501269_real64, 0.074797994408288370_real64, &
       0.062314485627766936_real64, 0.047579255841246392_real64, &
       0.031126761969323946_real64, 0.013576229705877047_real64 ]

  !> Rational Bezier span, degree 1..3. bind(c) POD, matches nurbs_gpu::BezSpan.
  type, bind(c) :: BezSpan
     real(real64) :: cx(4) = 0.0_real64
     real(real64) :: cy(4) = 0.0_real64
     real(real64) :: cw(4) = 1.0_real64
     integer(int32) :: degree = 1
     integer(int32) :: flags  = 0   ! bit0=rational, bit1=y-monotone, bit2=upward
     integer(int64) :: owner  = 0_int64
     integer(int64) :: band   = 0_int64
  end type BezSpan

  !> Curved horizontal slab y1 <= y <= y2 bounded by y-monotone left and right BezSpan curves.
  type, bind(c) :: CurvedTrap
     real(real64) :: y1 = 0.0_real64, y2 = 0.0_real64
     type(BezSpan) :: left, right
     integer(int64) :: winding = 0_int64
  end type CurvedTrap

  interface
     integer(c_int) function nurbs_gpu_available() bind(c, name='nurbs_gpu_available')
       import :: c_int
     end function nurbs_gpu_available

     integer(c_int) function nurbs_gpu_split_y_monotone(in_spans, n, out_spans, max_out) &
          bind(c, name='nurbs_gpu_split_y_monotone')
       import :: c_int, BezSpan
       type(BezSpan), intent(in)  :: in_spans(*)
       integer(c_int), value      :: n, max_out
       type(BezSpan), intent(out) :: out_spans(*)
     end function nurbs_gpu_split_y_monotone

     integer(c_int) function nurbs_gpu_find_overlaps(spans, n, eps, pair_a, pair_b, max_pairs) &
          bind(c, name='nurbs_gpu_find_overlaps')
       import :: c_int, c_double, BezSpan
       type(BezSpan), intent(in)   :: spans(*)
       integer(c_int), value       :: n, max_pairs
       real(c_double), value       :: eps
       integer(c_int), intent(out) :: pair_a(*), pair_b(*)
     end function nurbs_gpu_find_overlaps
  end interface

contains

  pure function bez_line(x0, y0, x1, y1, owner) result(b)
    real(real64), intent(in) :: x0, y0, x1, y1
    integer(int64), intent(in), optional :: owner
    type(BezSpan) :: b
    b%degree = 1; b%flags = 0; b%band = 0_int64
    b%owner = 0_int64
    if (present(owner)) b%owner = owner
    b%cx = [x0, x1, 0.0_real64, 0.0_real64]
    b%cy = [y0, y1, 0.0_real64, 0.0_real64]
    b%cw = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
  end function bez_line

  pure function bez_reverse(b) result(r)
    type(BezSpan), intent(in) :: b
    type(BezSpan) :: r
    integer(int32) :: i, p
    r = b; p = b%degree
    do i = 1, p + 1
       r%cx(i) = b%cx(p + 2 - i)
       r%cy(i) = b%cy(p + 2 - i)
       r%cw(i) = b%cw(p + 2 - i)
    end do
  end function bez_reverse

  pure elemental subroutine bez_eval(b, t, x, y)
    type(BezSpan), intent(in)  :: b
    real(real64),  intent(in)  :: t
    real(real64),  intent(out) :: x, y
    real(real64) :: bx(4), by(4), bw(4), u
    integer(int32) :: p, r, j
    p = b%degree
    do j = 1, p + 1
       bx(j) = b%cx(j) * b%cw(j)
       by(j) = b%cy(j) * b%cw(j)
       bw(j) = b%cw(j)
    end do
    u = 1.0_real64 - t
    do r = 1, p
       do j = 1, p - r + 1
          bx(j) = u * bx(j) + t * bx(j + 1)
          by(j) = u * by(j) + t * by(j + 1)
          bw(j) = u * bw(j) + t * bw(j + 1)
       end do
    end do
    x = bx(1) / bw(1)
    y = by(1) / bw(1)
  end subroutine bez_eval

  !> Exact analytical C(t), C'(t), C''(t) for rational Bezier of degree <= 3.
  pure subroutine bez_eval_deriv(b, t, x, y, dx, dy, ddx, ddy)
    type(BezSpan), intent(in)  :: b
    real(real64),  intent(in)  :: t
    real(real64),  intent(out) :: x, y, dx, dy
    real(real64),  intent(out), optional :: ddx, ddy
    real(real64) :: hx(4), hy(4), hw(4), u
    real(real64) :: xv, yv, wv, hdx, hdy, hdw, hddx, hddy, hddw, invw, invw2
    integer(int32) :: p, j
    p = b%degree; u = 1.0_real64 - t
    do j = 1, p + 1
       hx(j) = b%cx(j) * b%cw(j); hy(j) = b%cy(j) * b%cw(j); hw(j) = b%cw(j)
    end do
    if (p == 1) then
       xv = u*hx(1) + t*hx(2); yv = u*hy(1) + t*hy(2); wv = u*hw(1) + t*hw(2)
       hdx = hx(2) - hx(1); hdy = hy(2) - hy(1); hdw = hw(2) - hw(1)
       hddx = 0.0_real64; hddy = 0.0_real64; hddw = 0.0_real64
    else if (p == 2) then
       xv   = u*u*hx(1) + 2.0_real64*u*t*hx(2) + t*t*hx(3)
       yv   = u*u*hy(1) + 2.0_real64*u*t*hy(2) + t*t*hy(3)
       wv   = u*u*hw(1) + 2.0_real64*u*t*hw(2) + t*t*hw(3)
       hdx  = 2.0_real64*(u*(hx(2)-hx(1)) + t*(hx(3)-hx(2)))
       hdy  = 2.0_real64*(u*(hy(2)-hy(1)) + t*(hy(3)-hy(2)))
       hdw  = 2.0_real64*(u*(hw(2)-hw(1)) + t*(hw(3)-hw(2)))
       hddx = 2.0_real64*(hx(3) - 2.0_real64*hx(2) + hx(1))
       hddy = 2.0_real64*(hy(3) - 2.0_real64*hy(2) + hy(1))
       hddw = 2.0_real64*(hw(3) - 2.0_real64*hw(2) + hw(1))
    else
       xv   = u*u*u*hx(1) + 3.0_real64*u*u*t*hx(2) + 3.0_real64*u*t*t*hx(3) + t*t*t*hx(4)
       yv   = u*u*u*hy(1) + 3.0_real64*u*u*t*hy(2) + 3.0_real64*u*t*t*hy(3) + t*t*t*hy(4)
       wv   = u*u*u*hw(1) + 3.0_real64*u*u*t*hw(2) + 3.0_real64*u*t*t*hw(3) + t*t*t*hw(4)
       hdx  = 3.0_real64*(u*u*(hx(2)-hx(1)) + 2.0_real64*u*t*(hx(3)-hx(2)) + t*t*(hx(4)-hx(3)))
       hdy  = 3.0_real64*(u*u*(hy(2)-hy(1)) + 2.0_real64*u*t*(hy(3)-hy(2)) + t*t*(hy(4)-hy(3)))
       hdw  = 3.0_real64*(u*u*(hw(2)-hw(1)) + 2.0_real64*u*t*(hw(3)-hw(2)) + t*t*(hw(4)-hw(3)))
       hddx = 6.0_real64*(u*(hx(3)-2.0_real64*hx(2)+hx(1)) + t*(hx(4)-2.0_real64*hx(3)+hx(2)))
       hddy = 6.0_real64*(u*(hy(3)-2.0_real64*hy(2)+hy(1)) + t*(hy(4)-2.0_real64*hy(3)+hy(2)))
       hddw = 6.0_real64*(u*(hw(3)-2.0_real64*hw(2)+hw(1)) + t*(hw(4)-2.0_real64*hw(3)+hw(2)))
    end if
    invw = 1.0_real64 / wv; invw2 = invw * invw
    x = xv * invw; y = yv * invw
    dx = (hdx * wv - xv * hdw) * invw2
    dy = (hdy * wv - yv * hdw) * invw2
    if (present(ddx)) ddx = (hddx - 2.0_real64*dx*hdw - x*hddw) * invw
    if (present(ddy)) ddy = (hddy - 2.0_real64*dy*hdw - y*hddw) * invw
  end subroutine bez_eval_deriv

  !> Exact signed curvature kappa(t) and radius of curvature rho(t) = 1/|kappa|.
  pure subroutine bez_curvature(b, t, kappa, rho)
    type(BezSpan), intent(in)  :: b
    real(real64),  intent(in)  :: t
    real(real64),  intent(out) :: kappa, rho
    real(real64) :: x, y, dx, dy, ddx, ddy, sp2, sp3
    call bez_eval_deriv(b, t, x, y, dx, dy, ddx, ddy)
    sp2 = dx*dx + dy*dy
    if (sp2 <= 1.0e-30_real64) then
       kappa = 0.0_real64; rho = huge(1.0_real64); return
    end if
    sp3 = sp2 * sqrt(sp2)
    kappa = (dx*ddy - dy*ddx) / sp3
    if (abs(kappa) > 1.0e-18_real64) then
       rho = 1.0_real64 / abs(kappa)
    else
       rho = huge(1.0_real64)
    end if
  end subroutine bez_curvature

  !> Exact homogeneous de Casteljau split at t in (0, 1).
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
    u = 1.0_real64 - t
    do rr = 1, p
       do j = 0, p - rr
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

  !> Exact sub-span extraction on [t0, t1] subset [0, 1].
  pure subroutine bez_subspan(b, t0, t1, sub)
    type(BezSpan), intent(in)  :: b
    real(real64),  intent(in)  :: t0, t1
    type(BezSpan), intent(out) :: sub
    type(BezSpan) :: tmp, dummy
    real(real64) :: loc
    if (t0 <= 1.0e-14_real64 .and. t1 >= 1.0_real64 - 1.0e-14_real64) then
       sub = b; return
    end if
    if (t0 > 1.0e-14_real64) then
       call bez_split(b, t0, dummy, tmp)
       loc = (t1 - t0) / max(1.0_real64 - t0, 1.0e-30_real64)
    else
       tmp = b
       loc = t1
    end if
    if (loc < 1.0_real64 - 1.0e-14_real64) then
       call bez_split(tmp, loc, sub, dummy)
    else
       sub = tmp
    end if
  end subroutine bez_subspan

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

  !> Exact analytical y-extrema roots in (0, 1) for degree <= 3 rational/non-rational Bezier.
  pure subroutine bez_y_extrema(b, nroots, roots)
    type(BezSpan), intent(in)  :: b
    integer(int32), intent(out) :: nroots
    real(real64),  intent(out) :: roots(4)
    real(real64) :: d01, d02, d12, a, bv, c, disc, s, t1, t2
    real(real64) :: lo, hi, mid, flo, fm, prev, cur, r, x, y, dx, dy
    integer(int32) :: p, i, it, k
    logical :: dup
    nroots = 0; roots = 0.0_real64
    p = b%degree
    if (p <= 1) return
    if (p == 2) then
       ! Exact closed-form rational quadratic: 1/2 (Y'W - YW') = (1-t)^2 d01 + t(1-t) d02 + t^2 d12 = 0
       d01 = b%cw(1) * b%cw(2) * (b%cy(2) - b%cy(1))
       d02 = b%cw(1) * b%cw(3) * (b%cy(3) - b%cy(1))
       d12 = b%cw(2) * b%cw(3) * (b%cy(3) - b%cy(2))
       a  = d01 - d02 + d12
       bv = d02 - 2.0_real64 * d01
       c  = d01
       if (abs(a) <= 1.0e-18_real64 * (abs(bv) + abs(c) + 1.0_real64)) then
          if (abs(bv) > 1.0e-18_real64) then
             t1 = -c / bv
             if (t1 > 1.0e-12_real64 .and. t1 < 1.0_real64 - 1.0e-12_real64) then
                nroots = 1; roots(1) = t1
             end if
          end if
       else
          disc = bv*bv - 4.0_real64*a*c
          if (disc >= 0.0_real64) then
             s = sqrt(disc)
             t1 = (-bv - s) / (2.0_real64*a); t2 = (-bv + s) / (2.0_real64*a)
             if (t1 > 1.0e-12_real64 .and. t1 < 1.0_real64 - 1.0e-12_real64) then
                nroots = nroots + 1; roots(nroots) = t1
             end if
             if (t2 > 1.0e-12_real64 .and. t2 < 1.0_real64 - 1.0e-12_real64 .and. abs(t2 - t1) > 1.0e-11_real64) then
                nroots = nroots + 1; roots(nroots) = t2
             end if
          end if
       end if
       if (nroots == 2 .and. roots(2) < roots(1)) then
          r = roots(1); roots(1) = roots(2); roots(2) = r
       end if
       return
    end if
    ! Degree 3: exact analytical dy/dt scan + bisection to 1e-14
    call bez_eval_deriv(b, 0.0_real64, x, y, dx, prev)
    do i = 1, 32
       call bez_eval_deriv(b, real(i, real64) / 32.0_real64, x, y, dx, cur)
       if (prev == 0.0_real64 .or. prev * cur <= 0.0_real64) then
          lo = real(i - 1, real64) / 32.0_real64; hi = real(i, real64) / 32.0_real64
          flo = prev
          do it = 1, 56
             mid = 0.5_real64 * (lo + hi)
             call bez_eval_deriv(b, mid, x, y, dx, fm)
             if (flo * fm <= 0.0_real64) then
                hi = mid
             else
                lo = mid; flo = fm
             end if
          end do
          r = 0.5_real64 * (lo + hi)
          if (r > 1.0e-11_real64 .and. r < 1.0_real64 - 1.0e-11_real64) then
             dup = .false.
             do k = 1, nroots
                if (abs(roots(k) - r) < 1.0e-9_real64) dup = .true.
             end do
             if (.not. dup .and. nroots < 4) then
                nroots = nroots + 1; roots(nroots) = r
             end if
          end if
       end if
       prev = cur
    end do
  end subroutine bez_y_extrema

  !> Split into y-monotone spans. nout <= 5.
  pure subroutine bez_make_y_monotone(b, nout, out)
    type(BezSpan), intent(in)  :: b
    integer(int32), intent(out) :: nout
    type(BezSpan), intent(out) :: out(8)
    real(real64) :: roots(4), last, local_t, tmp
    integer(int32) :: nroots, i, j
    type(BezSpan) :: rem, L, R
    call bez_y_extrema(b, nroots, roots)
    do i = 1, nroots
       do j = i + 1, nroots
          if (roots(j) < roots(i)) then
             tmp = roots(i); roots(i) = roots(j); roots(j) = tmp
          end if
       end do
    end do
    nout = 0; rem = b; last = 0.0_real64
    do i = 1, nroots
       if (roots(i) - last <= 1.0e-11_real64) cycle
       local_t = (roots(i) - last) / (1.0_real64 - last)
       if (local_t <= 1.0e-11_real64 .or. local_t >= 1.0_real64 - 1.0e-11_real64) cycle
       call bez_split(rem, local_t, L, R)
       L%flags = ior(L%flags, 2_int32)
       nout = nout + 1; out(nout) = L
       rem = R; last = roots(i)
    end do
    rem%flags = ior(rem%flags, 2_int32)
    nout = nout + 1; out(nout) = rem
  end subroutine bez_make_y_monotone

  !> Exact inverse x(y) on a y-monotone BezSpan b. Returns t in [0,1], x, and ok=.true. if y in range.
  pure subroutine bez_x_at_y(b, y, t, x, ok)
    type(BezSpan), intent(in)  :: b
    real(real64),  intent(in)  :: y
    real(real64),  intent(out) :: t, x
    logical,       intent(out) :: ok
    real(real64) :: ya, yb, c0, c1, c2, A, Bv, C, disc, s, r1, r2
    real(real64) :: lo, hi, mid, ym, xm, dx, dy, ylo
    integer(int32) :: p, it
    p = b%degree
    ya = b%cy(1); yb = b%cy(p + 1)
    if (y < min(ya, yb) - 1.0e-12_real64 .or. y > max(ya, yb) + 1.0e-12_real64) then
       t = 0.0_real64; x = 0.0_real64; ok = .false.; return
    end if
    ok = .true.
    if (abs(yb - ya) <= 1.0e-15_real64) then
       t = 0.5_real64; call bez_eval(b, t, x, ym); return
    end if
    if (p == 1) then
       ! Rational/linear: (1-t)*w0*(y0-y) + t*w1*(y1-y) = 0
       c0 = b%cw(1) * (ya - y); c1 = b%cw(2) * (yb - y)
       t = max(0.0_real64, min(1.0_real64, c0 / (c0 - c1)))
       call bez_eval(b, t, x, ym); return
    else if (p == 2) then
       ! Exact closed-form rational quadratic: (1-t)^2 c0 + 2t(1-t) c1 + t^2 c2 = 0
       c0 = b%cw(1) * (b%cy(1) - y)
       c1 = b%cw(2) * (b%cy(2) - y)
       c2 = b%cw(3) * (b%cy(3) - y)
       A  = c0 - 2.0_real64 * c1 + c2
       Bv = 2.0_real64 * (c1 - c0)
       C  = c0
       if (abs(A) <= 1.0e-18_real64 * (abs(Bv) + abs(C) + 1.0_real64)) then
          t = max(0.0_real64, min(1.0_real64, -C / Bv))
       else
          disc = max(0.0_real64, Bv*Bv - 4.0_real64*A*C)
          s = sqrt(disc)
          r1 = (-Bv - s) / (2.0_real64*A); r2 = (-Bv + s) / (2.0_real64*A)
          if (r1 >= -1.0e-9_real64 .and. r1 <= 1.0_real64 + 1.0e-9_real64) then
             t = max(0.0_real64, min(1.0_real64, r1))
          else
             t = max(0.0_real64, min(1.0_real64, r2))
          end if
       end if
       call bez_eval(b, t, x, ym); return
    end if
    ! Degree 3: bracketed Newton-Raphson with bisection safeguard
    lo = 0.0_real64; hi = 1.0_real64; ylo = ya - y
    t = max(0.0_real64, min(1.0_real64, (y - ya) / (yb - ya)))
    do it = 1, 48
       call bez_eval_deriv(b, t, xm, ym, dx, dy)
       if (abs(ym - y) <= 1.0e-13_real64 * (abs(y) + 1.0_real64)) exit
       if (ylo * (ym - y) <= 0.0_real64) then
          hi = t
       else
          lo = t; ylo = ym - y
       end if
       if (abs(dy) > 1.0e-15_real64) then
          mid = t - (ym - y) / dy
          if (mid > lo .and. mid < hi) then
             t = mid
          else
             t = 0.5_real64 * (lo + hi)
          end if
       else
          t = 0.5_real64 * (lo + hi)
       end if
    end do
    call bez_eval(b, t, x, ym)
  end subroutine bez_x_at_y

  !> 16-point Gauss-Legendre Green's theorem 2*area contribution (machine precision ~1e-14).
  pure real(real64) function bez_area2_gl(b) result(a2)
    type(BezSpan), intent(in) :: b
    real(real64) :: x, y, dx, dy
    integer(int32) :: i
    if (b%degree == 1 .and. abs(b%cw(1) - b%cw(2)) < 1.0e-15_real64) then
       a2 = b%cx(1)*b%cy(2) - b%cx(2)*b%cy(1)
       return
    end if
    a2 = 0.0_real64
    do i = 1, 16
       call bez_eval_deriv(b, GL16_X(i), x, y, dx, dy)
       a2 = a2 + GL16_W(i) * (x*dy - y*dx)
    end do
  end function bez_area2_gl

  !> 16-point Gauss-Legendre arc length.
  pure real(real64) function bez_perim_gl(b) result(per)
    type(BezSpan), intent(in) :: b
    real(real64) :: x, y, dx, dy
    integer(int32) :: i
    if (b%degree == 1) then
       per = hypot(b%cx(2) - b%cx(1), b%cy(2) - b%cy(1))
       return
    end if
    per = 0.0_real64
    do i = 1, 16
       call bez_eval_deriv(b, GL16_X(i), x, y, dx, dy)
       per = per + GL16_W(i) * hypot(dx, dy)
    end do
  end function bez_perim_gl

  !> Exact area of one CurvedTrap (left & right oriented upward y1 -> y2).
  pure real(real64) function curved_trap_area(ct) result(area)
    type(CurvedTrap), intent(in) :: ct
    real(real64) :: xL1, yL1, xL2, yL2, xR1, yR1, xR2, yR2, a2
    call bez_eval(ct%left,  0.0_real64, xL1, yL1)
    call bez_eval(ct%left,  1.0_real64, xL2, yL2)
    call bez_eval(ct%right, 0.0_real64, xR1, yR1)
    call bez_eval(ct%right, 1.0_real64, xR2, yR2)
    ! CCW loop: bottom (xL1,y1)->(xR1,y1), right up, top (xR2,y2)->(xL2,y2), left down (-bez_area2_gl)
    a2 = (xL1*ct%y1 - xR1*ct%y1) + bez_area2_gl(ct%right) + (xR2*ct%y2 - xL2*ct%y2) - bez_area2_gl(ct%left)
    area = 0.5_real64 * a2
  end function curved_trap_area

  !> Legacy sampled Green's theorem 2*area (kept for backward compatibility).
  pure real(real64) function bez_area2_green(b, ns) result(a2)
    type(BezSpan), intent(in) :: b
    integer(int32), intent(in) :: ns
    real(real64) :: x0, y0, x1, y1, t0, t1
    integer(int32) :: i
    a2 = 0.0_real64
    do i = 0, ns - 1
       t0 = real(i, real64) / real(ns, real64)
       t1 = real(i + 1, real64) / real(ns, real64)
       call bez_eval(b, t0, x0, y0)
       call bez_eval(b, t1, x1, y1)
       a2 = a2 + (x0*y1 - x1*y0)
    end do
  end function bez_area2_green

  pure real(real64) function bez_perimeter(b, ns) result(p)
    type(BezSpan), intent(in) :: b
    integer(int32), intent(in) :: ns
    real(real64) :: x0, y0, x1, y1, t0, t1
    integer(int32) :: i
    p = 0.0_real64
    do i = 0, ns - 1
       t0 = real(i, real64) / real(ns, real64)
       t1 = real(i + 1, real64) / real(ns, real64)
       call bez_eval(b, t0, x0, y0)
       call bez_eval(b, t1, x1, y1)
       p = p + hypot(x1 - x0, y1 - y0)
    end do
  end function bez_perimeter

  !> Curve-curve intersection for degree <= 3 rational Bezier spans (recursive AABB subdivision +
  !> 2D Newton-Raphson polishing). Returns up to 16 distinct intersections in [0,1]x[0,1].
  subroutine bez_intersect(a, b, nint, ta, tb, ix, iy)
    type(BezSpan), intent(in)  :: a, b
    integer(int32), intent(out) :: nint
    real(real64),  intent(out) :: ta(16), tb(16), ix(16), iy(16)
    real(real64) :: ax0, ay0, ax1, ay1, bx0, by0, bx1, by1, det, s, u
    nint = 0; ta = 0.0_real64; tb = 0.0_real64; ix = 0.0_real64; iy = 0.0_real64
    ! Fast path: line-line exact intersection
    if (a%degree == 1 .and. b%degree == 1) then
       ax0 = a%cx(2) - a%cx(1); ay0 = a%cy(2) - a%cy(1)
       bx0 = b%cx(2) - b%cx(1); by0 = b%cy(2) - b%cy(1)
       det = ax0*by0 - ay0*bx0
       if (abs(det) > 1.0e-18_real64) then
          s = ((b%cx(1) - a%cx(1))*by0 - (b%cy(1) - a%cy(1))*bx0) / det
          u = ((b%cx(1) - a%cx(1))*ay0 - (b%cy(1) - a%cy(1))*ax0) / det
          if (s >= -1.0e-10_real64 .and. s <= 1.0_real64 + 1.0e-10_real64 .and. &
              u >= -1.0e-10_real64 .and. u <= 1.0_real64 + 1.0e-10_real64) then
             nint = 1
             ta(1) = max(0.0_real64, min(1.0_real64, s))
             tb(1) = max(0.0_real64, min(1.0_real64, u))
             call bez_eval(a, ta(1), ix(1), iy(1))
          end if
       end if
       return
    end if
    call bez_aabb(a, ax0, ay0, ax1, ay1)
    call bez_aabb(b, bx0, by0, bx1, by1)
    if (ax1 + 1.0e-11_real64 < bx0 .or. bx1 + 1.0e-11_real64 < ax0) return
    if (ay1 + 1.0e-11_real64 < by0 .or. by1 + 1.0e-11_real64 < ay0) return
    call rec_isect(a, 0.0_real64, 1.0_real64, b, 0.0_real64, 1.0_real64, 0, nint, ta, tb, ix, iy)
  contains
    recursive subroutine rec_isect(sa, a0, a1, sb, b0, b1, depth, ni, ota, otb, oix, oiy)
      type(BezSpan), intent(in) :: sa, sb
      real(real64),  intent(in) :: a0, a1, b0, b1
      integer(int32), intent(in) :: depth
      integer(int32), intent(inout) :: ni
      real(real64),  intent(inout) :: ota(16), otb(16), oix(16), oiy(16)
      real(real64) :: x0a, y0a, x1a, y1a, x0b, y0b, x1b, y1b, da, db, u0, v0
      real(real64) :: xa, ya, dxa, dya, xb, yb, dxb, dyb, fx, fy, det, du, dv
      type(BezSpan) :: la, ra, lb, rb
      integer(int32) :: it, k
      logical :: dup
      if (ni >= 16) return
      call bez_aabb(sa, x0a, y0a, x1a, y1a)
      call bez_aabb(sb, x0b, y0b, x1b, y1b)
      if (x1a + 1.0e-11_real64 < x0b .or. x1b + 1.0e-11_real64 < x0a) return
      if (y1a + 1.0e-11_real64 < y0b .or. y1b + 1.0e-11_real64 < y0a) return
      da = max(x1a - x0a, y1a - y0a)
      db = max(x1b - x0b, y1b - y0b)
      if (max(da, db) < 1.0e-5_real64 .or. depth >= 22) then
         ! 2D Newton-Raphson polish on original curves a, b
         u0 = 0.5_real64 * (a0 + a1); v0 = 0.5_real64 * (b0 + b1)
         do it = 1, 14
            call bez_eval_deriv(a, u0, xa, ya, dxa, dya)
            call bez_eval_deriv(b, v0, xb, yb, dxb, dyb)
            fx = xa - xb; fy = ya - yb
            if (hypot(fx, fy) < 1.0e-12_real64) exit
            det = -dxa*dyb + dya*dxb
            if (abs(det) < 1.0e-22_real64) exit
            du = (-dyb*fx + dxb*fy) / det
            dv = (-dya*fx + dxa*fy) / det
            u0 = u0 - du; v0 = v0 - dv
            if (u0 < -0.05_real64 .or. u0 > 1.05_real64 .or. v0 < -0.05_real64 .or. v0 > 1.05_real64) return
         end do
         u0 = max(0.0_real64, min(1.0_real64, u0))
         v0 = max(0.0_real64, min(1.0_real64, v0))
         call bez_eval(a, u0, xa, ya); call bez_eval(b, v0, xb, yb)
         if (hypot(xa - xb, ya - yb) > 1.0e-7_real64) return
         dup = .false.
         do k = 1, ni
            if (abs(ota(k) - u0) < 1.0e-6_real64 .and. abs(otb(k) - v0) < 1.0e-6_real64) dup = .true.
            if (hypot(oix(k) - xa, oiy(k) - ya) < 1.0e-6_real64) dup = .true.
         end do
         if (.not. dup .and. ni < 16) then
            ni = ni + 1; ota(ni) = u0; otb(ni) = v0; oix(ni) = 0.5_real64*(xa+xb); oiy(ni) = 0.5_real64*(ya+yb)
         end if
         return
      end if
      if (da >= db) then
         call bez_split(sa, 0.5_real64, la, ra)
         call rec_isect(la, a0, 0.5_real64*(a0+a1), sb, b0, b1, depth + 1, ni, ota, otb, oix, oiy)
         call rec_isect(ra, 0.5_real64*(a0+a1), a1, sb, b0, b1, depth + 1, ni, ota, otb, oix, oiy)
      else
         call bez_split(sb, 0.5_real64, lb, rb)
         call rec_isect(sa, a0, a1, lb, b0, 0.5_real64*(b0+b1), depth + 1, ni, ota, otb, oix, oiy)
         call rec_isect(sa, a0, a1, rb, 0.5_real64*(b0+b1), b1, depth + 1, ni, ota, otb, oix, oiy)
      end if
    end subroutine rec_isect
  end subroutine bez_intersect

  !> Exact Bohm knot insertion: decompose a clamped degree-p (p <= 3) rational NURBS curve
  !> into exact rational Bezier spans. Handles multi-span curves (e.g. mems1.json hub circle).
  subroutine nurbs_to_beziers(p, nctrl, knots, cx, cy, cw, owner, spans, nspans)
    integer(int32), intent(in) :: p, nctrl
    real(real64),   intent(in) :: knots(:), cx(:), cy(:), cw(:)
    integer(int64), intent(in) :: owner
    type(BezSpan), allocatable, intent(out) :: spans(:)
    integer(int32), intent(out) :: nspans
    real(real64), allocatable :: U(:), hx(:), hy(:), hw(:), nU(:), nhx(:), nhy(:), nhw(:)
    integer(int32) :: m, nc, i, j, k, mult, r, s, nuniq, ins
    real(real64) :: uval, alpha, denom
    logical :: is_rat

    if (p < 1 .or. p > 3 .or. nctrl < p + 1) then
       allocate(spans(0)); nspans = 0; return
    end if
    m = nctrl + p + 1
    allocate(U(m), hx(nctrl), hy(nctrl), hw(nctrl))
    U(1:m) = knots(1:m)
    do i = 1, nctrl
       hx(i) = cx(i) * cw(i); hy(i) = cy(i) * cw(i); hw(i) = cw(i)
    end do
    nc = nctrl
    is_rat = any(abs(cw(1:nctrl) - 1.0_real64) > 1.0e-12_real64)

    ! Insert each interior knot until its multiplicity equals p
    i = p + 2
    do while (i <= nc)
       uval = U(i)
       mult = 1
       do while (i + mult <= nc .and. abs(U(i + mult) - uval) <= 1.0e-14_real64)
          mult = mult + 1
       end do
       k = i + mult - 1
       ins = p - mult
       do r = 1, ins
          allocate(nU(nc + p + 2), nhx(nc + 1), nhy(nc + 1), nhw(nc + 1))
          nU(1:k) = U(1:k); nU(k + 1) = uval; nU(k + 2:nc + p + 2) = U(k + 1:nc + p + 1)
          do j = 1, k - p
             nhx(j) = hx(j); nhy(j) = hy(j); nhw(j) = hw(j)
          end do
          do j = k - p + 1, k
             denom = U(j + p) - U(j)
             if (abs(denom) > 1.0e-18_real64) then
                alpha = (uval - U(j)) / denom
             else
                alpha = 0.0_real64
             end if
             nhx(j) = alpha * hx(j) + (1.0_real64 - alpha) * hx(j - 1)
             nhy(j) = alpha * hy(j) + (1.0_real64 - alpha) * hy(j - 1)
             nhw(j) = alpha * hw(j) + (1.0_real64 - alpha) * hw(j - 1)
          end do
          do j = k + 1, nc + 1
             nhx(j) = hx(j - 1); nhy(j) = hy(j - 1); nhw(j) = hw(j - 1)
          end do
          call move_alloc(nU, U); call move_alloc(nhx, hx); call move_alloc(nhy, hy); call move_alloc(nhw, hw)
          nc = nc + 1; k = k + 1
       end do
       i = k + 1
    end do

    nspans = (nc - 1) / p
    allocate(spans(nspans))
    do s = 1, nspans
       spans(s)%degree = p
       spans(s)%flags  = merge(1_int32, 0_int32, is_rat)
       spans(s)%owner  = owner
       spans(s)%band   = 0_int64
       spans(s)%cx = 0.0_real64; spans(s)%cy = 0.0_real64; spans(s)%cw = 1.0_real64
       do j = 0, p
          k = (s - 1) * p + j + 1
          spans(s)%cw(j + 1) = hw(k)
          spans(s)%cx(j + 1) = hx(k) / hw(k)
          spans(s)%cy(j + 1) = hy(k) / hw(k)
       end do
    end do
  end subroutine nurbs_to_beziers

  !> Exact degree-2 rational circular arc from angle a0 to a1 (|a1-a0| <= pi/2).
  pure function nurbs_arc_bez(cx0, cy0, r, a0, a1, owner) result(b)
    real(real64), intent(in) :: cx0, cy0, r, a0, a1
    integer(int64), intent(in), optional :: owner
    type(BezSpan) :: b
    real(real64) :: dm, am, rm
    dm = 0.5_real64 * (a1 - a0)
    am = 0.5_real64 * (a0 + a1)
    rm = r / cos(dm)
    b%degree = 2; b%flags = 1; b%band = 0_int64
    b%owner = 0_int64; if (present(owner)) b%owner = owner
    b%cx = [cx0 + r*cos(a0), cx0 + rm*cos(am), cx0 + r*cos(a1), 0.0_real64]
    b%cy = [cy0 + r*sin(a0), cy0 + rm*sin(am), cy0 + r*sin(a1), 0.0_real64]
    b%cw = [1.0_real64, cos(dm), 1.0_real64, 1.0_real64]
  end function nurbs_arc_bez

  !> Build 4 quarter-ellipse rational quadratic Bezier spans (CCW if rx,ry > 0).
  pure subroutine nurbs_ellipse_bez(cx0, cy0, rx, ry, spans, owner)
    real(real64), intent(in) :: cx0, cy0, rx, ry
    type(BezSpan), intent(out) :: spans(4)
    integer(int64), intent(in), optional :: owner
    real(real64), parameter :: wq = 0.70710678118654752440_real64
    integer(int32) :: k
    integer(int64) :: ow
    ow = 0_int64; if (present(owner)) ow = owner
    do k = 1, 4
       spans(k)%degree = 2; spans(k)%flags = 1; spans(k)%owner = ow; spans(k)%band = 0_int64
       spans(k)%cw = [1.0_real64, wq, 1.0_real64, 1.0_real64]
       spans(k)%cx = 0.0_real64; spans(k)%cy = 0.0_real64
    end do
    spans(1)%cx(1:3) = [cx0+rx, cx0+rx, cx0];    spans(1)%cy(1:3) = [cy0,    cy0+ry, cy0+ry]
    spans(2)%cx(1:3) = [cx0,    cx0-rx, cx0-rx]; spans(2)%cy(1:3) = [cy0+ry, cy0+ry, cy0]
    spans(3)%cx(1:3) = [cx0-rx, cx0-rx, cx0];    spans(3)%cy(1:3) = [cy0,    cy0-ry, cy0-ry]
    spans(4)%cx(1:3) = [cx0,    cx0+rx, cx0+rx]; spans(4)%cy(1:3) = [cy0-ry, cy0-ry, cy0]
  end subroutine nurbs_ellipse_bez

  !> Build 4 quarter-circle rational quadratic Bezier spans (CCW).
  pure subroutine nurbs_circle_bez(cx0, cy0, r, spans)
    real(real64), intent(in) :: cx0, cy0, r
    type(BezSpan), intent(out) :: spans(4)
    call nurbs_ellipse_bez(cx0, cy0, r, r, spans)
  end subroutine nurbs_circle_bez

  !> curve1.json outer wire (5 curves) as Bezier spans.
  subroutine nurbs_json_curve1_outer(spans)
    type(BezSpan), intent(out) :: spans(5)
    integer(int32) :: k
    do k = 1, 5
       spans(k)%degree = 2; spans(k)%flags = 1; spans(k)%owner = 0_int64; spans(k)%band = 0_int64
       spans(k)%cw = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
       spans(k)%cx = 0.0_real64; spans(k)%cy = 0.0_real64
    end do
    spans(1)%cx(1:3) = [42.0_real64, 56.0_real64, 34.0_real64]; spans(1)%cy(1:3) = [62.0_real64, 32.0_real64,  3.0_real64]; spans(1)%cw(2) = 0.85_real64
    spans(2)%degree  = 1; spans(2)%flags = 0; spans(2)%cx(1:2) = [34.0_real64, 89.0_real64]; spans(2)%cy(1:2) = [3.0_real64, 9.0_real64]
    spans(3)%cx(1:3) = [89.0_real64, 64.0_real64, 96.0_real64]; spans(3)%cy(1:3) = [ 9.0_real64, 56.0_real64, 90.0_real64]; spans(3)%cw(2) = 0.82_real64
    spans(4)%cx(1:3) = [96.0_real64, 48.0_real64,  5.0_real64]; spans(4)%cy(1:3) = [90.0_real64,106.0_real64, 63.0_real64]; spans(4)%cw(2) = 0.88_real64
    spans(5)%cx(1:3) = [ 5.0_real64, 21.0_real64, 42.0_real64]; spans(5)%cy(1:3) = [63.0_real64, 31.0_real64, 62.0_real64]; spans(5)%cw(2) = 0.78_real64
  end subroutine nurbs_json_curve1_outer

  subroutine nurbs_json_curve1_hole(spans)
    type(BezSpan), intent(out) :: spans(2)
    integer(int32) :: k
    do k = 1, 2
       spans(k)%degree = 1; spans(k)%flags = 0; spans(k)%owner = 0_int64; spans(k)%band = 0_int64
       spans(k)%cw = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
       spans(k)%cx = 0.0_real64; spans(k)%cy = 0.0_real64
    end do
    spans(1)%degree = 2; spans(1)%flags = 1; spans(1)%cx(1:3) = [59.0_real64, 72.0_real64, 57.0_real64]; spans(1)%cy(1:3) = [87.0_real64, 76.0_real64, 65.0_real64]; spans(1)%cw(2) = 0.80_real64
    spans(2)%cx(1:2) = [57.0_real64, 59.0_real64]; spans(2)%cy(1:2) = [65.0_real64, 87.0_real64]
  end subroutine nurbs_json_curve1_hole

end module NurbsCurveModule
