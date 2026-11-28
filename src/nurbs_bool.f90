! File   : nurbs_bool.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Native curvilinear NURBS/Bezier Boolean (N1-N3), Offsetting (N4), and
!          Bounded-Memory Y-Band Streaming (N5) engine in Modern idiomatic Fortran
!          with CUDA GPU acceleration (nurbs_gpu.cu via iso_c_binding).
!          Pure-POD, non-polymorphic (no class(...)), OpenMP/ifx-safe.
module NurbsBoolModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding,   only: c_int, c_double
  use omp_lib
  use NurbsCurveModule
  implicit none
  private

  public :: NURBS_NONZERO, NURBS_OR, NURBS_AND, NURBS_NOT, NURBS_XOR
  public :: NurbsBoolStats
  public :: nurbs_curved_boolean, nurbs_curved_area, nurbs_offset, nurbs_banded_boolean

  integer(int32), parameter :: NURBS_NONZERO = 0
  integer(int32), parameter :: NURBS_OR      = 1
  integer(int32), parameter :: NURBS_AND     = 2
  integer(int32), parameter :: NURBS_NOT     = 3
  integer(int32), parameter :: NURBS_XOR     = 4

  type :: NurbsBoolStats
     integer(int64) :: n_mono = 0, n_overlaps = 0, n_crossings = 0, n_frags = 0, n_slabs = 0, n_traps = 0
     logical        :: used_gpu = .false.
     real(real64)   :: t_n1 = 0.0_real64, t_n2 = 0.0_real64, t_n3 = 0.0_real64, t_total = 0.0_real64
  end type NurbsBoolStats

contains

  pure logical function nurbs_inside(op, wA, wB) result(res)
    integer(int32), intent(in) :: op, wA, wB
    select case (op)
    case (NURBS_NONZERO); res = (wA /= 0)
    case (NURBS_OR);      res = (wA /= 0) .or. (wB /= 0)
    case (NURBS_AND);     res = (wA /= 0) .and. (wB /= 0)
    case (NURBS_NOT);     res = (wA /= 0) .and. (wB == 0)
    case (NURBS_XOR);     res = (wA /= 0) .neqv. (wB /= 0)
    case default;         res = (wA /= 0)
    end select
  end function nurbs_inside

  pure real(real64) function nurbs_curved_area(traps, n) result(area)
    type(CurvedTrap), intent(in) :: traps(:)
    integer(int64),   intent(in) :: n
    integer(int64) :: i
    area = 0.0_real64
    do i = 1, n
       area = area + curved_trap_area(traps(i))
    end do
  end function nurbs_curved_area

  !> N1: Split input spans A and B into upward y-monotone spans with operand winding (wA, wB).
  !> Uses CUDA GPU kernel nurbs_gpu_split_y_monotone when use_gpu=.true. and a GPU is present.
  subroutine prepare_monotone(spA, nA, spB, nB, use_gpu, mono, wA, wB, nmono, used_gpu)
    type(BezSpan), intent(in) :: spA(:), spB(:)
    integer(int64), intent(in) :: nA, nB
    logical, intent(in) :: use_gpu
    type(BezSpan), allocatable, intent(out) :: mono(:)
    integer(int32), allocatable, intent(out) :: wA(:), wB(:)
    integer(int64), intent(out) :: nmono
    logical, intent(out) :: used_gpu

    type(BezSpan), allocatable :: all_in(:), raw(:)
    type(BezSpan) :: tmp(8), s
    integer(int64) :: ntot, i, max_out, k
    integer(int32) :: ksub, j, rc
    real(real64) :: y0, y1

    ntot = nA + nB
    used_gpu = .false.
    if (ntot <= 0) then
       allocate(mono(0), wA(0), wB(0)); nmono = 0; return
    end if
    allocate(all_in(ntot))
    do i = 1, nA
       all_in(i) = spA(i); all_in(i)%owner = 1_int64
    end do
    do i = 1, nB
       all_in(nA + i) = spB(i); all_in(nA + i)%owner = 2_int64
    end do

    max_out = max(ntot * 5_int64, 16_int64)
    allocate(raw(max_out))
    k = 0
    if (use_gpu .and. nurbs_gpu_available() == 1 .and. ntot <= huge(1_c_int) / 6) then
       rc = nurbs_gpu_split_y_monotone(all_in, int(ntot, c_int), raw, int(max_out, c_int))
       if (rc >= 0 .and. rc <= max_out) then
          k = int(rc, int64)
          used_gpu = .true.
       end if
    end if
    if (.not. used_gpu) then
       do i = 1, ntot
          call bez_make_y_monotone(all_in(i), ksub, tmp)
          do j = 1, ksub
             k = k + 1
             raw(k) = tmp(j)
          end do
       end do
    end if

    allocate(mono(max(k, 1_int64)), wA(max(k, 1_int64)), wB(max(k, 1_int64)))
    nmono = 0
    do i = 1, k
       s = raw(i)
       y0 = s%cy(1); y1 = s%cy(s%degree + 1)
       if (abs(y1 - y0) <= 1.0e-11_real64) cycle   ! skip horizontal segments
       nmono = nmono + 1
       if (y1 > y0) then
          ! Going UP: CCW right boundary -> winding delta -1 crossing left-to-right
          mono(nmono) = s
          if (s%owner == 1_int64) then
             wA(nmono) = -1; wB(nmono) = 0
          else
             wA(nmono) = 0;  wB(nmono) = -1
          end if
       else
          ! Going DOWN: reverse to upward y1<y2; CCW left boundary -> winding delta +1
          mono(nmono) = bez_reverse(s)
          if (s%owner == 1_int64) then
             wA(nmono) = 1; wB(nmono) = 0
          else
             wA(nmono) = 0; wB(nmono) = 1
          end if
       end if
       mono(nmono)%flags = ior(mono(nmono)%flags, 6_int32)
    end do
  end subroutine prepare_monotone

  !> N2: Find all curve-curve intersections and split monotone spans into non-crossing fragments.
  subroutine split_intersections(mono, wA, wB, nmono, use_gpu, frags, fwA, fwB, nfrags, nover, ncross)
    type(BezSpan),  intent(in) :: mono(:)
    integer(int32), intent(in) :: wA(:), wB(:)
    integer(int64), intent(in) :: nmono
    logical,        intent(in) :: use_gpu
    type(BezSpan),  allocatable, intent(out) :: frags(:)
    integer(int32), allocatable, intent(out) :: fwA(:), fwB(:)
    integer(int64), intent(out) :: nfrags, nover, ncross

    integer(c_int), allocatable :: pa(:), pb(:)
    integer(int32) :: np_gpu, max_pairs, ni, k, idx_a, idx_b
    integer(int64) :: i, j, cap_frags, p
    real(real64), allocatable :: ax0(:), ay0(:), ax1(:), ay1(:)
    real(real64) :: ta(16), tb(16), ix(16), iy(16), eps
    ! Per-span split parameter lists (flat fixed-stride or dynamic)
    integer(int32), allocatable :: nsp(:)
    real(real64),   allocatable :: tsp(:, :)
    integer(int32), parameter   :: MAX_SPLITS = 64
    real(real64) :: tlist(MAX_SPLITS + 2), tmp
    type(BezSpan) :: sub
    integer(int32) :: ns, m
    logical :: used_gpu_ov

    nover = 0; ncross = 0; nfrags = 0
    if (nmono <= 0) then
       allocate(frags(0), fwA(0), fwB(0)); return
    end if
    eps = 1.0e-9_real64
    allocate(nsp(nmono), tsp(MAX_SPLITS, nmono))
    nsp = 0

    used_gpu_ov = .false.
    if (use_gpu .and. nurbs_gpu_available() == 1 .and. nmono <= 50000_int64) then
       max_pairs = int(min(max(nmono * 32_int64, 1024_int64), 4000000_int64), c_int)
       allocate(pa(max_pairs), pb(max_pairs))
       np_gpu = nurbs_gpu_find_overlaps(mono, int(nmono, c_int), real(eps, c_double), pa, pb, max_pairs)
       if (np_gpu >= 0 .and. np_gpu <= max_pairs) then
          used_gpu_ov = .true.
          nover = int(np_gpu, int64)
          do p = 1, nover
             idx_a = pa(p); idx_b = pb(p)
             call bez_intersect(mono(idx_a), mono(idx_b), ni, ta, tb, ix, iy)
             do k = 1, ni
                ncross = ncross + 1
                call add_split_param(nsp(idx_a), tsp(:, idx_a), ta(k))
                call add_split_param(nsp(idx_b), tsp(:, idx_b), tb(k))
             end do
          end do
       end if
       deallocate(pa, pb)
    end if

    if (.not. used_gpu_ov) then
       allocate(ax0(nmono), ay0(nmono), ax1(nmono), ay1(nmono))
       do i = 1, nmono
          call bez_aabb(mono(i), ax0(i), ay0(i), ax1(i), ay1(i))
       end do
       do i = 1, nmono
          do j = i + 1, nmono
             if (ax1(i) + eps < ax0(j) .or. ax1(j) + eps < ax0(i)) cycle
             if (ay1(i) + eps < ay0(j) .or. ay1(j) + eps < ay0(i)) cycle
             nover = nover + 1
             call bez_intersect(mono(i), mono(j), ni, ta, tb, ix, iy)
             do k = 1, ni
                ncross = ncross + 1
                call add_split_param(nsp(i), tsp(:, i), ta(k))
                call add_split_param(nsp(j), tsp(:, j), tb(k))
             end do
          end do
       end do
    end if

    cap_frags = nmono + sum(int(nsp, int64))
    allocate(frags(max(cap_frags, 1_int64)), fwA(max(cap_frags, 1_int64)), fwB(max(cap_frags, 1_int64)))
    nfrags = 0
    do i = 1, nmono
       ns = nsp(i)
       if (ns == 0) then
          nfrags = nfrags + 1
          frags(nfrags) = mono(i); fwA(nfrags) = wA(i); fwB(nfrags) = wB(i)
       else
          tlist(1) = 0.0_real64
          tlist(2:ns + 1) = tsp(1:ns, i)
          tlist(ns + 2) = 1.0_real64
          ! Sort tlist(2:ns+1)
          do k = 2, ns + 1
             do m = k + 1, ns + 1
                if (tlist(m) < tlist(k)) then
                   tmp = tlist(k); tlist(k) = tlist(m); tlist(m) = tmp
                end if
             end do
          end do
          do k = 1, ns + 1
             if (tlist(k + 1) - tlist(k) <= 1.0e-9_real64) cycle
             call bez_subspan(mono(i), tlist(k), tlist(k + 1), sub)
             if (abs(sub%cy(sub%degree + 1) - sub%cy(1)) <= 1.0e-11_real64) cycle
             nfrags = nfrags + 1
             frags(nfrags) = sub; fwA(nfrags) = wA(i); fwB(nfrags) = wB(i)
          end do
       end if
    end do
  contains
    subroutine add_split_param(cnt, arr, t)
      integer(int32), intent(inout) :: cnt
      real(real64),   intent(inout) :: arr(:)
      real(real64),   intent(in)    :: t
      integer(int32) :: q
      if (t <= 1.0e-8_real64 .or. t >= 1.0_real64 - 1.0e-8_real64) return
      do q = 1, cnt
         if (abs(arr(q) - t) < 1.0e-7_real64) return
      end do
      if (cnt < size(arr)) then
         cnt = cnt + 1; arr(cnt) = t
      end if
    end subroutine add_split_param
  end subroutine split_intersections

  !> Sort real64 array ascending (quicksort with insertion sort cutoff).
  recursive subroutine qsort_r64(a, lo, hi)
    real(real64), intent(inout) :: a(:)
    integer(int64), intent(in)  :: lo, hi
    integer(int64) :: i, j
    real(real64) :: piv, tmp
    if (hi - lo <= 16_int64) then
       do i = lo + 1, hi
          piv = a(i); j = i - 1
          do while (j >= lo)
             if (a(j) <= piv) exit
             a(j + 1) = a(j); j = j - 1
          end do
          a(j + 1) = piv
       end do
       return
    end if
    piv = a((lo + hi) / 2_int64)
    i = lo; j = hi
    do
       do while (a(i) < piv); i = i + 1; end do
       do while (a(j) > piv); j = j - 1; end do
       if (i >= j) exit
       tmp = a(i); a(i) = a(j); a(j) = tmp
       i = i + 1; j = j - 1
    end do
    if (lo < j) call qsort_r64(a, lo, j)
    if (j + 1 < hi) call qsort_r64(a, j + 1, hi)
  end subroutine qsort_r64

  !> N1-N3: Native curvilinear Boolean returning exact CurvedTrap slabs.
  subroutine nurbs_curved_boolean(spA, nA, spB, nB, op, traps, ntraps, st, use_gpu)
    type(BezSpan), intent(in) :: spA(:), spB(:)
    integer(int64), intent(in) :: nA, nB
    integer(int32), intent(in) :: op
    type(CurvedTrap), allocatable, intent(out) :: traps(:)
    integer(int64), intent(out) :: ntraps
    type(NurbsBoolStats), intent(out) :: st
    logical, intent(in), optional :: use_gpu

    type(BezSpan),  allocatable :: mono(:), frags(:)
    integer(int32), allocatable :: wA(:), wB(:), fwA(:), fwB(:)
    real(real64),   allocatable :: ys(:), yuniq(:), fy1(:), fy2(:)
    integer(int64), allocatable :: act(:)
    real(real64),   allocatable :: ax(:)
    type(CurvedTrap), allocatable :: buf(:), tmp_buf(:)
    integer(int64) :: nmono, nfrags, ny, k, i, j, nact, cap, left_id
    integer(int32) :: curA, curB
    real(real64) :: t0, t1, t2, ya, yb, ym, tval, xval, tL0, tL1, tR0, tR1, xd
    type(BezSpan) :: subL, subR
    logical :: gpu_flag, prev_in, now_in, ok
    integer(int64) :: itmp
    real(real64) :: xtmp

    st = NurbsBoolStats()
    ntraps = 0
    t0 = omp_get_wtime()
    gpu_flag = .true.
    if (present(use_gpu)) gpu_flag = use_gpu

    call prepare_monotone(spA, nA, spB, nB, gpu_flag, mono, wA, wB, nmono, st%used_gpu)
    t1 = omp_get_wtime(); st%t_n1 = t1 - t0; st%n_mono = nmono
    if (nmono == 0) then
       allocate(traps(0)); st%t_total = t1 - t0; return
    end if

    call split_intersections(mono, wA, wB, nmono, gpu_flag, frags, fwA, fwB, nfrags, st%n_overlaps, st%n_crossings)
    t2 = omp_get_wtime(); st%t_n2 = t2 - t1; st%n_frags = nfrags
    if (nfrags == 0) then
       allocate(traps(0)); st%t_total = t2 - t0; return
    end if

    ! N3: Collect unique sorted event rows
    allocate(ys(2_int64 * nfrags), fy1(nfrags), fy2(nfrags))
    do i = 1, nfrags
       fy1(i) = frags(i)%cy(1)
       fy2(i) = frags(i)%cy(frags(i)%degree + 1)
       ys(2*i - 1) = fy1(i)
       ys(2*i)     = fy2(i)
    end do
    call qsort_r64(ys, 1_int64, 2_int64 * nfrags)
    allocate(yuniq(2_int64 * nfrags))
    ny = 1; yuniq(1) = ys(1)
    do i = 2, 2_int64 * nfrags
       if (ys(i) - yuniq(ny) > 1.0e-9_real64) then
          ny = ny + 1; yuniq(ny) = ys(i)
       end if
    end do
    st%n_slabs = max(0_int64, ny - 1)

    cap = max(nfrags * 2_int64, 256_int64)
    allocate(buf(cap), act(nfrags), ax(nfrags))
    ntraps = 0

    do k = 1, ny - 1
       ya = yuniq(k); yb = yuniq(k + 1)
       if (yb - ya <= 1.0e-10_real64) cycle
       ym = 0.5_real64 * (ya + yb)
       nact = 0
       do i = 1, nfrags
          if (fy1(i) <= ym .and. fy2(i) >= ym) then
             call bez_x_at_y(frags(i), ym, tval, xval, ok)
             if (ok) then
                nact = nact + 1; act(nact) = i; ax(nact) = xval
             end if
          end if
       end do
       if (nact < 2) cycle
       ! Sort active spans by x(ym) (insertion sort: nact per slab is typically small)
       do i = 2, nact
          xtmp = ax(i); itmp = act(i); j = i - 1
          do while (j >= 1)
             if (ax(j) <= xtmp) exit
             ax(j + 1) = ax(j); act(j + 1) = act(j); j = j - 1
          end do
          ax(j + 1) = xtmp; act(j + 1) = itmp
       end do

       curA = 0; curB = 0; prev_in = .false.; left_id = 0
       do i = 1, nact
          curA = curA + fwA(act(i))
          curB = curB + fwB(act(i))
          now_in = nurbs_inside(op, curA, curB)
          if (.not. prev_in .and. now_in) then
             left_id = act(i)
          else if (prev_in .and. .not. now_in) then
             call bez_x_at_y(frags(left_id), ya, tL0, xd, ok)
             call bez_x_at_y(frags(left_id), yb, tL1, xd, ok)
             call bez_subspan(frags(left_id), tL0, tL1, subL)
             call bez_x_at_y(frags(act(i)), ya, tR0, xd, ok)
             call bez_x_at_y(frags(act(i)), yb, tR1, xd, ok)
             call bez_subspan(frags(act(i)), tR0, tR1, subR)
             if (ntraps >= cap) then
                allocate(tmp_buf(2_int64 * cap))
                tmp_buf(1:ntraps) = buf(1:ntraps)
                call move_alloc(tmp_buf, buf)
                cap = size(buf, kind=int64)
             end if
             ntraps = ntraps + 1
             buf(ntraps) = CurvedTrap(ya, yb, subL, subR, int(curA, int64))
          end if
          prev_in = now_in
       end do
    end do

    allocate(traps(ntraps))
    if (ntraps > 0) traps(1:ntraps) = buf(1:ntraps)
    st%n_traps = ntraps
    st%t_n3 = omp_get_wtime() - t2
    st%t_total = omp_get_wtime() - t0
  end subroutine nurbs_curved_boolean

  !> Build an exact/G1 curvilinear offset curve of b by signed distance d along outward normal
  !> (for CCW boundary: outward normal = (dy, -dx)/hypot(dx,dy)).
  subroutine offset_span_single(b, d, off)
    type(BezSpan), intent(in)  :: b
    real(real64),  intent(in)  :: d
    type(BezSpan), intent(out) :: off
    real(real64) :: x0, y0, dx0, dy0, l0, nx0, ny0
    real(real64) :: x1, y1, dx1, dy1, l1, nx1, ny1
    real(real64) :: p0x, p0y, p2x, p2y, det, s, xm, ym, dxm, dym, lm, nxm, nym, pmx, pmy, tx, ty
    real(real64) :: q0x, q0y, q2x, q2y, w1
    off = b
    call bez_eval_deriv(b, 0.0_real64, x0, y0, dx0, dy0)
    call bez_eval_deriv(b, 1.0_real64, x1, y1, dx1, dy1)
    l0 = max(hypot(dx0, dy0), 1.0e-30_real64)
    l1 = max(hypot(dx1, dy1), 1.0e-30_real64)
    nx0 =  dy0 / l0; ny0 = -dx0 / l0
    nx1 =  dy1 / l1; ny1 = -dx1 / l1
    p0x = x0 + d * nx0; p0y = y0 + d * ny0
    p2x = x1 + d * nx1; p2y = y1 + d * ny1
    if (b%degree == 1) then
       off%cx(1:2) = [p0x, p2x]; off%cy(1:2) = [p0y, p2y]; return
    end if
    ! Intersect endpoint tangents for quadratic control point (exact for all circular arcs!)
    det = dx0 * dy1 - dy0 * dx1
    if (b%degree == 2 .and. abs(det) > 1.0e-12_real64 * l0 * l1) then
       s = ((p2x - p0x) * dy1 - (p2y - p0y) * dx1) / det
       off%cx(1) = p0x; off%cy(1) = p0y
       off%cx(2) = p0x + s * dx0; off%cy(2) = p0y + s * dy0
       off%cx(3) = p2x; off%cy(3) = p2y
       ! Preserve exact rational weight (exact for concentric circular arcs)
       off%cw(1:3) = b%cw(1:3)
    else
       ! Cubic or near-collinear: G1 Hermite midpoint-fitted rational quadratic
       call bez_eval_deriv(b, 0.5_real64, xm, ym, dxm, dym)
       lm = max(hypot(dxm, dym), 1.0e-30_real64)
       nxm = dym / lm; nym = -dxm / lm
       pmx = xm + d * nxm; pmy = ym + d * nym
       off%degree = 2; off%flags = 0; off%cw = 1.0_real64
       off%cx(1:3) = [p0x, 2.0_real64*pmx - 0.5_real64*(p0x + p2x), p2x]
       off%cy(1:3) = [p0y, 2.0_real64*pmy - 0.5_real64*(p0y + p2y), p2y]
    end if
  end subroutine offset_span_single

  !> N4: Native curvilinear offsetting (d > 0 grow, d < 0 shrink) with exact circular fillets
  !> at all vertices and global self-intersection / cusp trimming via N1-N3 Boolean.
  subroutine nurbs_offset(spA, nA, d, traps, ntraps, st, use_gpu)
    type(BezSpan), intent(in) :: spA(:)
    integer(int64), intent(in) :: nA
    real(real64),  intent(in) :: d
    type(CurvedTrap), allocatable, intent(out) :: traps(:)
    integer(int64), intent(out) :: ntraps
    type(NurbsBoolStats), intent(out) :: st
    logical, intent(in), optional :: use_gpu

    type(BezSpan), allocatable :: collar(:), emptyB(:)
    type(BezSpan) :: sub, off, circ(4), c1, c2, rev_off
    integer(int64) :: i, nc, cap, nsub, s
    real(real64) :: ad, x0, y0, x1, y1, ox0, oy0, ox1, oy1, sd, t0, t1, a2
    integer(int32) :: op

    allocate(emptyB(0))
    if (nA <= 0 .or. abs(d) <= 1.0e-12_real64) then
       call nurbs_curved_boolean(spA, nA, emptyB, 0_int64, NURBS_NONZERO, traps, ntraps, st, use_gpu)
       return
    end if
    ad = abs(d)
    sd = merge(ad, -ad, d > 0.0_real64)
    op = merge(NURBS_OR, NURBS_NOT, d > 0.0_real64)

    ! Estimate collar spans: per span, up to 4 sub-spans x 4 collar edges + 4 fillet arcs = 20
    cap = nA * 24_int64 + 16_int64
    allocate(collar(cap))
    nc = 0
    do i = 1, nA
       nsub = merge(1_int64, 4_int64, spA(i)%degree == 1)
       do s = 1, nsub
          t0 = real(s - 1, real64) / real(nsub, real64)
          t1 = real(s,     real64) / real(nsub, real64)
          call bez_subspan(spA(i), t0, t1, sub)
          call offset_span_single(sub, sd, off)
          call bez_eval(sub, 0.0_real64, x0, y0); call bez_eval(sub, 1.0_real64, x1, y1)
          call bez_eval(off, 0.0_real64, ox0, oy0); call bez_eval(off, 1.0_real64, ox1, oy1)
          c1 = bez_line(x1, y1, ox1, oy1)
          c2 = bez_line(ox0, oy0, x0, y0)
          rev_off = bez_reverse(off)
          ! Ensure CCW collar loop (signed area > 0)
          a2 = bez_area2_gl(sub) + bez_area2_gl(c1) + bez_area2_gl(rev_off) + bez_area2_gl(c2)
          if (a2 < 0.0_real64) then
             collar(nc + 1) = bez_reverse(sub)
             collar(nc + 2) = bez_reverse(c2)
             collar(nc + 3) = off
             collar(nc + 4) = bez_reverse(c1)
          else
             collar(nc + 1) = sub
             collar(nc + 2) = c1
             collar(nc + 3) = rev_off
             collar(nc + 4) = c2
          end if
          nc = nc + 4
       end do
       ! Exact circular fillet disk of radius |d| at start vertex of span i
       call bez_eval(spA(i), 0.0_real64, x0, y0)
       call nurbs_circle_bez(x0, y0, ad, circ)
       collar(nc + 1:nc + 4) = circ(1:4)
       nc = nc + 4
    end do

    call nurbs_curved_boolean(spA, nA, collar, nc, op, traps, ntraps, st, use_gpu)
  end subroutine nurbs_offset

  !> N5: Bounded-memory Y-band streaming curvilinear Boolean (O(1/M) working set).
  subroutine nurbs_banded_boolean(spA, nA, spB, nB, op, nbands, traps, ntraps, st, use_gpu)
    type(BezSpan), intent(in) :: spA(:), spB(:)
    integer(int64), intent(in) :: nA, nB
    integer(int32), intent(in) :: op
    integer(int32), intent(in) :: nbands
    type(CurvedTrap), allocatable, intent(out) :: traps(:)
    integer(int64), intent(out) :: ntraps
    type(NurbsBoolStats), intent(out) :: st
    logical, intent(in), optional :: use_gpu

    real(real64), allocatable :: y1A(:), y2A(:), y1B(:), y2B(:)
    type(BezSpan), allocatable :: bA(:), bB(:)
    type(CurvedTrap), allocatable :: btr(:), out_buf(:), tmp(:)
    type(NurbsBoolStats) :: bst
    integer(int64) :: i, k, M, bnA, bnB, bnt, cap
    real(real64) :: x0, y0, x1, y1, ymin, ymax, ylo, yhi, H, tL0, tL1, tR0, tR1, xd, t0
    type(CurvedTrap) :: ct
    type(BezSpan) :: sL, sR
    logical :: ok

    st = NurbsBoolStats()
    ntraps = 0
    t0 = omp_get_wtime()
    M = max(1_int64, int(nbands, int64))
    if (M == 1_int64 .or. (nA == 0 .and. nB == 0)) then
       call nurbs_curved_boolean(spA, nA, spB, nB, op, traps, ntraps, st, use_gpu)
       return
    end if

    allocate(y1A(max(nA, 1_int64)), y2A(max(nA, 1_int64)), y1B(max(nB, 1_int64)), y2B(max(nB, 1_int64)))
    ymin = huge(1.0_real64); ymax = -huge(1.0_real64)
    do i = 1, nA
       call bez_aabb(spA(i), x0, y0, x1, y1)
       y1A(i) = y0; y2A(i) = y1
       ymin = min(ymin, y0); ymax = max(ymax, y1)
    end do
    do i = 1, nB
       call bez_aabb(spB(i), x0, y0, x1, y1)
       y1B(i) = y0; y2B(i) = y1
       ymin = min(ymin, y0); ymax = max(ymax, y1)
    end do
    if (ymax <= ymin) then
       allocate(traps(0)); return
    end if

    H = 1.0e-6_real64 * (ymax - ymin)
    allocate(bA(max(nA, 1_int64)), bB(max(nB, 1_int64)))
    cap = 256_int64; allocate(out_buf(cap))

    do k = 0, M - 1
       ylo = ymin + (ymax - ymin) * real(k, real64) / real(M, real64)
       yhi = ymin + (ymax - ymin) * real(k + 1, real64) / real(M, real64)
       bnA = 0
       do i = 1, nA
          if (y2A(i) >= ylo - H .and. y1A(i) <= yhi + H) then
             bnA = bnA + 1; bA(bnA) = spA(i)
          end if
       end do
       bnB = 0
       do i = 1, nB
          if (y2B(i) >= ylo - H .and. y1B(i) <= yhi + H) then
             bnB = bnB + 1; bB(bnB) = spB(i)
          end if
       end do
       if (bnA == 0 .and. bnB == 0) cycle
       call nurbs_curved_boolean(bA, bnA, bB, bnB, op, btr, bnt, bst, use_gpu)
       st%used_gpu = st%used_gpu .or. bst%used_gpu
       st%n_mono = st%n_mono + bst%n_mono
       st%n_crossings = st%n_crossings + bst%n_crossings
       do i = 1, bnt
          ct = btr(i)
          if (ct%y2 <= ylo + 1.0e-11_real64 .or. ct%y1 >= yhi - 1.0e-11_real64) cycle
          if (ct%y1 < ylo .or. ct%y2 > yhi) then
             y0 = max(ct%y1, ylo); y1 = min(ct%y2, yhi)
             if (y1 - y0 <= 1.0e-11_real64) cycle
             call bez_x_at_y(ct%left,  y0, tL0, xd, ok)
             call bez_x_at_y(ct%left,  y1, tL1, xd, ok)
             call bez_subspan(ct%left, tL0, tL1, sL)
             call bez_x_at_y(ct%right, y0, tR0, xd, ok)
             call bez_x_at_y(ct%right, y1, tR1, xd, ok)
             call bez_subspan(ct%right, tR0, tR1, sR)
             ct = CurvedTrap(y0, y1, sL, sR, ct%winding)
          end if
          if (ntraps >= cap) then
             allocate(tmp(2_int64 * cap))
             tmp(1:ntraps) = out_buf(1:ntraps)
             call move_alloc(tmp, out_buf)
             cap = size(out_buf, kind=int64)
          end if
          ntraps = ntraps + 1
          out_buf(ntraps) = ct
       end do
       if (allocated(btr)) deallocate(btr)
    end do

    allocate(traps(ntraps))
    if (ntraps > 0) traps(1:ntraps) = out_buf(1:ntraps)
    st%n_traps = ntraps
    st%t_total = omp_get_wtime() - t0
  end subroutine nurbs_banded_boolean

end module NurbsBoolModule
