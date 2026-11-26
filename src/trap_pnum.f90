! File   : trap_pnum.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Polygon numbering (PNUM) of a canonical trapezoid set (TrapMergeModule output: interior-
!          disjoint maximal horizontal trapezoids with lattice-line sides, all region vertices on
!          lattice points). The PNUM polygons are the connected components of the closed region:
!          pieces that share an edge or only a corner belong to the same polygon (the box-PNUM rule,
!          and KLayout's merged() with min_coherence off).
!
! Contacts. Two distinct canonical trapezoids never share a side segment (the sweep would have made
! them one interval), and if they meet at a point strictly inside both y-ranges their two sides
! through it would have to be one line, i.e. a shared segment. So every contact lies on a row that
! is the bottom or top row of one of the two pieces. For every piece and each of its two rows y, the
! pieces whose closed x-extent at y meets the closed edge are found in a uniform cell grid and
! tested exactly (rationals with denominators dy < 2**31, compared in i128). Union-find over the
! contacts gives the components.
!
! Canonical numbering. A polygon's key is its lowest, then leftmost, vertex (a lattice point: the
! left end of a bottom edge on its lowest row). Distinct components cannot share it, so ordinals
! 1..np in key order do not depend on how the region was cut into pieces, the band count or threads.
!
! Per polygon: key vertex, exact 2A (integer; exact per-slope accumulation), bounding box (exact:
! extremes are attained at lattice vertices), piece count.
module TrapPNumModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use omp_lib
  use WideIntModule
  use TrapezoidModule
  use CompactLayerModule, only: CompactSortPairs
  implicit none
  private

  integer(int64), parameter :: KB = 1073741824_int64        ! 2**30
  integer(int64), parameter :: K31 = 2147483648_int64       ! 2**31

  !> PNUM result. lab(i) = ordinal (1..np) of piece i.
  type, public :: TrapPolys
     integer(int64) :: np = 0, contacts = 0, anomalies = 0
     integer(int64), allocatable :: lab(:)
     integer(int64), allocatable :: vx(:), vy(:)            !> lowest-leftmost vertex
     integer(int64), allocatable :: area2(:)                !> exact 2A
     integer(int64), allocatable :: x1(:), y1(:), x2(:), y2(:)
     integer(int64), allocatable :: npieces(:)
     real(real64) :: t_grid = 0, t_contacts = 0, t_union = 0, t_props = 0
  end type TrapPolys

  type :: PairBuf
     integer(int64) :: n = 0
     integer(int64), allocatable :: a(:), b(:)
  end type PairBuf

  public :: TrapPNum, traps_touch

contains

  pure logical function traps_touch(a, b, y) result(touch)
    ! closed x-extents of a and b at row y intersect: la <= rb and lb <= ra
    type(Trapezoid), intent(in) :: a, b
    integer(int64), intent(in) :: y
    integer(int64) :: ya, yb
    ya = a%y1; yb = b%y1
    touch = side_x_cmp(a%l, ya, b%r, yb, y) <= 0 .and. side_x_cmp(b%l, yb, a%r, ya, y) <= 0
  end function traps_touch

  subroutine TrapPNum(t, n, P)
    type(Trapezoid), intent(in) :: t(:)
    integer(int64), intent(in) :: n
    type(TrapPolys), intent(inout) :: P
    integer(int64), allocatable :: bx1(:), bx2(:), nreg(:), roff(:), rkey(:), par(:), ckey(:)
    integer(int64), allocatable :: ord_of_root(:), roots(:), cstart(:), ukey(:)
    integer(int32), allocatable :: rval(:), ridx(:)
    integer(int64) :: i, j, s, gx0, gy0, ncx, ncy, cx1, cx2, cy1, cy2, cx, cy, m, tot, nu
    integer(int64) :: xmin, xmax, ymin, ymax, npairs, a, b, r, k
    real(real64) :: sumarea, t0
    integer :: nt, tid
    type(PairBuf), allocatable :: pb(:)

    t0 = omp_get_wtime()
    call free_polys(P)
    allocate(P%lab(max(n, 1_int64)))
    if (n == 0) then
       call alloc_props(P, 0_int64); return
    end if
    ! ---- piece bounding boxes (exact integers: floor / ceil of the four corners)
    allocate(bx1(n), bx2(n))
    xmin = huge(xmin); xmax = -huge(xmax); ymin = huge(ymin); ymax = -huge(ymax); sumarea = 0
    !$omp parallel do reduction(min:xmin,ymin) reduction(max:xmax,ymax) reduction(+:sumarea)
    do i = 1, n
       bx1(i) = min(side_x_floor(t(i)%l, int(t(i)%y1, int64), int(t(i)%y1, int64)), &
                    side_x_floor(t(i)%l, int(t(i)%y1, int64), int(t(i)%y2, int64)))
       bx2(i) = max(side_x_ceil(t(i)%r, int(t(i)%y1, int64), int(t(i)%y1, int64)), &
                    side_x_ceil(t(i)%r, int(t(i)%y1, int64), int(t(i)%y2, int64)))
       xmin = min(xmin, bx1(i)); xmax = max(xmax, bx2(i))
       ymin = min(ymin, int(t(i)%y1, int64)); ymax = max(ymax, int(t(i)%y2, int64))
       sumarea = sumarea + real(bx2(i) - bx1(i) + 1, real64)*real(t(i)%y2 - t(i)%y1 + 1, real64)
    end do
    !$omp end parallel do
    ! cell side: about one piece per cell (pieces are interior-disjoint, so the registrations are
    ! bounded by area / s**2 + perimeter / s + 4 n)
    s = max(1_int64, int(sqrt(sumarea / real(n, real64)), int64))
    gx0 = xmin; gy0 = ymin
    ncx = (xmax - xmin) / s + 1; ncy = (ymax - ymin) / s + 1
    do while (ncx > 0 .and. ncy > huge(1_int64) / max(ncx, 1_int64) / 4)
       s = 2*s; ncx = (xmax - xmin) / s + 1; ncy = (ymax - ymin) / s + 1
    end do
    ! ---- register every piece in the cells of its bounding box
    allocate(nreg(n), roff(n + 1))
    !$omp parallel do
    do i = 1, n
       nreg(i) = ((bx2(i) - gx0) / s - (bx1(i) - gx0) / s + 1) * &
                 ((int(t(i)%y2, int64) - gy0) / s - (int(t(i)%y1, int64) - gy0) / s + 1)
    end do
    !$omp end parallel do
    roff(1) = 0
    do i = 1, n
       roff(i + 1) = roff(i) + nreg(i)
    end do
    tot = roff(n + 1)
    allocate(rkey(tot), rval(tot))
    !$omp parallel do private(cx1, cx2, cy1, cy2, cx, cy, m)
    do i = 1, n
       cx1 = (bx1(i) - gx0) / s; cx2 = (bx2(i) - gx0) / s
       cy1 = (int(t(i)%y1, int64) - gy0) / s; cy2 = (int(t(i)%y2, int64) - gy0) / s
       m = roff(i)
       do cy = cy1, cy2
          do cx = cx1, cx2
             m = m + 1; rkey(m) = cy*ncx + cx; rval(m) = int(i, int32)
          end do
       end do
    end do
    !$omp end parallel do
    deallocate(nreg)
    call CompactSortPairs(rkey, rval, tot)
    call run_starts(rkey, tot, ukey, cstart, nu)
    P%t_grid = omp_get_wtime() - t0; t0 = omp_get_wtime()

    ! ---- contacts: for every piece row (bottom and top), pieces spanning that row whose closed
    !      extent meets the closed edge
    nt = omp_get_max_threads()
    allocate(pb(0:nt - 1))
    !$omp parallel default(shared) private(tid, i, j, k, cy, cx1, cx2, cx, a, b, r)
    tid = omp_get_thread_num()
    allocate(pb(tid)%a(4096), pb(tid)%b(4096)); pb(tid)%n = 0
    !$omp do schedule(dynamic, 4096)
    do i = 1, n
       do k = 1, 2
          if (k == 1) then
             r = t(i)%y1
          else
             r = t(i)%y2
          end if
          cy = (r - gy0) / s
          cx1 = (side_x_floor(t(i)%l, int(t(i)%y1, int64), r) - gx0) / s
          cx2 = (side_x_ceil(t(i)%r, int(t(i)%y1, int64), r) - gx0) / s
          do cx = cx1, cx2
             a = find_cell(ukey, nu, cy*ncx + cx)
             if (a == 0) cycle
             do b = cstart(a), cstart(a + 1) - 1
                j = rval(b)
                if (j == i) cycle
                if (r < t(j)%y1 .or. r > t(j)%y2) cycle
                ! report each contact once: in the first query cell holding j, and from the
                ! lower-indexed piece when the row is a bottom/top row of both
                if (max(cx1, (bx1(j) - gx0) / s) /= cx) cycle
                if ((r == t(j)%y1 .or. r == t(j)%y2) .and. j < i) cycle
                if (traps_touch(t(i), t(j), r)) call pb_push(pb(tid), i, j)
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel
    deallocate(rkey, rval, ukey, cstart, bx1, bx2)
    P%t_contacts = omp_get_wtime() - t0; t0 = omp_get_wtime()

    ! ---- union-find (union by smaller index root, path halving): deterministic roots
    allocate(par(n))
    do i = 1, n
       par(i) = i
    end do
    npairs = 0
    do tid = 0, nt - 1
       do m = 1, pb(tid)%n
          call uf_union(par, pb(tid)%a(m), pb(tid)%b(m))
       end do
       npairs = npairs + pb(tid)%n
    end do
    deallocate(pb)
    P%contacts = npairs
    do i = 1, n
       par(i) = uf_find(par, i)
    end do
    ! ---- canonical ordinals: lowest-leftmost vertex per component
    allocate(ckey(n))
    ckey = huge(1_int64)
    do i = 1, n
       r = par(i)
       ckey(r) = min(ckey(r), (int(t(i)%y1, int64) + KB)*K31 + &
                 (side_x_floor(t(i)%l, int(t(i)%y1, int64), int(t(i)%y1, int64)) + KB))
    end do
    m = 0
    do i = 1, n
       if (par(i) == i) m = m + 1
    end do
    P%np = m
    allocate(roots(m), ridx(m), ord_of_root(n))
    m = 0
    do i = 1, n
       if (par(i) == i) then
          m = m + 1; roots(m) = ckey(i); ridx(m) = int(i, int32)
       end if
    end do
    if (m > 1) call CompactSortPairs(roots, ridx, m)
    do j = 1, m
       ord_of_root(ridx(j)) = j
    end do
    !$omp parallel do
    do i = 1, n
       P%lab(i) = ord_of_root(par(i))
    end do
    !$omp end parallel do
    call alloc_props(P, m)
    do j = 1, m
       P%vy(j) = roots(j) / K31 - KB; P%vx(j) = mod(roots(j), K31) - KB
    end do
    deallocate(par, ckey, roots, ridx, ord_of_root)
    P%t_union = omp_get_wtime() - t0; t0 = omp_get_wtime()
    call poly_props(t, n, P)
    P%t_props = omp_get_wtime() - t0
  end subroutine TrapPNum

  !> Exact 2A, bounding box and piece count per polygon. Pieces are grouped by label; each
  !> polygon is done by one thread with that thread's slope table (exact per-slope sums).
  subroutine poly_props(t, n, P)
    type(Trapezoid), intent(in) :: t(:)
    integer(int64), intent(in) :: n
    type(TrapPolys), intent(inout) :: P
    type(SlopeTab), allocatable :: TT(:)
    integer(int64), allocatable :: key(:), cstart(:)
    integer(int32), allocatable :: pid(:)
    integer(int64) :: i, j, c, h, q, rr, ip, ya
    real(real64) :: frac
    integer :: nt, tid
    type(i128) :: v
    allocate(key(n), pid(n), cstart(P%np + 1))
    !$omp parallel do
    do i = 1, n
       key(i) = P%lab(i); pid(i) = int(i, int32)
    end do
    !$omp end parallel do
    call CompactSortPairs(key, pid, n)
    cstart = 0
    do i = 1, n
       cstart(key(i) + 1) = cstart(key(i) + 1) + 1
    end do
    cstart(1) = 1
    do c = 1, P%np
       cstart(c + 1) = cstart(c) + cstart(c + 1)
    end do
    deallocate(key)
    nt = omp_get_max_threads()
    allocate(TT(0:nt - 1))
    do tid = 0, nt - 1
       call stab_init(TT(tid), 64_int64)
    end do
    !$omp parallel do schedule(dynamic, 256) private(tid, j, i, h, q, rr, ip, ya, frac, v)
    do c = 1, P%np
       tid = omp_get_thread_num()
       if (TT(tid)%n > 0) then
          TT(tid)%used = .false.; TT(tid)%n = 0
       end if
       ip = 0
       P%x1(c) = huge(1_int64); P%y1(c) = huge(1_int64); P%x2(c) = -huge(1_int64); P%y2(c) = -huge(1_int64)
       P%npieces(c) = cstart(c + 1) - cstart(c)
       do j = cstart(c), cstart(c + 1) - 1
          i = pid(j)
          ya = t(i)%y1
          h = int(t(i)%y2, int64) - ya
          ip = ip + 2_int64*h*(int(t(i)%r%ax, int64) - t(i)%l%ax)
          P%y1(c) = min(P%y1(c), ya); P%y2(c) = max(P%y2(c), int(t(i)%y2, int64))
          P%x1(c) = min(P%x1(c), side_x_floor(t(i)%l, ya, ya), side_x_floor(t(i)%l, ya, int(t(i)%y2, int64)))
          P%x2(c) = max(P%x2(c), side_x_ceil(t(i)%r, ya, ya), side_x_ceil(t(i)%r, ya, int(t(i)%y2, int64)))
          if (t(i)%r%dx /= 0) then
             v = i128_mul_i64(i128_mul64(h, 2_int64*t(i)%r%k + h), int(t(i)%r%dx, int64))
             call stab_add(TT(tid), slope_key(t(i)%r), int(t(i)%r%dy, int64), v)
          end if
          if (t(i)%l%dx /= 0) then
             v = i128_neg(i128_mul_i64(i128_mul64(h, 2_int64*t(i)%l%k + h), int(t(i)%l%dx, int64)))
             call stab_add(TT(tid), slope_key(t(i)%l), int(t(i)%l%dy, int64), v)
          end if
       end do
       frac = 0
       if (TT(tid)%n > 0) then
          do j = 1, TT(tid)%cap
             if (.not. TT(tid)%used(j)) cycle
             call i128_divmod(TT(tid)%acc(j), TT(tid)%dy(j), q, rr)
             ip = ip + q
             frac = frac + real(rr, real64) / real(TT(tid)%dy(j), real64)
          end do
       end if
       q = nint(frac, int64)
       if (abs(frac - real(q, real64)) > 1.0e-6_real64) then
          !$omp atomic
          P%anomalies = P%anomalies + 1
       end if
       P%area2(c) = ip + q
    end do
    !$omp end parallel do
    deallocate(TT, pid, cstart)
  end subroutine poly_props

  integer(int64) function stab_find(T, k) result(s)
    type(SlopeTab), intent(in) :: T
    integer(int64), intent(in) :: k
    integer(int64) :: z
    z = k + (-7046029254386353131_int64)
    z = ieor(z, shiftr(z, 30)) * (-4658895280553007687_int64)
    z = ieor(z, shiftr(z, 27)) * (-7723592293110705685_int64)
    z = ieor(z, shiftr(z, 31))
    z = shiftr(z, 1)
    s = iand(z, T%cap - 1) + 1
    do while (T%used(s))
       if (T%key(s) == k) return
       s = iand(s, T%cap - 1) + 1
    end do
    s = 0
  end function stab_find

  subroutine alloc_props(P, m)
    type(TrapPolys), intent(inout) :: P
    integer(int64), intent(in) :: m
    allocate(P%vx(max(m, 1_int64)), P%vy(max(m, 1_int64)), P%area2(max(m, 1_int64)), &
             P%x1(max(m, 1_int64)), P%y1(max(m, 1_int64)), P%x2(max(m, 1_int64)), &
             P%y2(max(m, 1_int64)), P%npieces(max(m, 1_int64)))
  end subroutine alloc_props

  subroutine free_polys(P)
    type(TrapPolys), intent(inout) :: P
    if (allocated(P%lab)) deallocate(P%lab)
    if (allocated(P%vx)) deallocate(P%vx, P%vy, P%area2, P%x1, P%y1, P%x2, P%y2, P%npieces)
    P%np = 0; P%contacts = 0; P%anomalies = 0
  end subroutine free_polys

  ! ---------------------------------------------------------------- helpers

  subroutine pb_push(p, a, b)
    type(PairBuf), intent(inout) :: p
    integer(int64), intent(in) :: a, b
    integer(int64), allocatable :: t(:)
    if (p%n >= size(p%a, kind=int64)) then
       allocate(t(2*p%n)); t(1:p%n) = p%a(1:p%n); call move_alloc(t, p%a)
       allocate(t(2*p%n)); t(1:p%n) = p%b(1:p%n); call move_alloc(t, p%b)
    end if
    p%n = p%n + 1; p%a(p%n) = a; p%b(p%n) = b
  end subroutine pb_push

  integer(int64) function uf_find(par, x0) result(x)
    integer(int64), intent(inout) :: par(:)
    integer(int64), intent(in) :: x0
    x = x0
    do while (par(x) /= x)
       par(x) = par(par(x)); x = par(x)
    end do
  end function uf_find

  subroutine uf_union(par, a, b)
    integer(int64), intent(inout) :: par(:)
    integer(int64), intent(in) :: a, b
    integer(int64) :: ra, rb
    ra = uf_find(par, a); rb = uf_find(par, b)
    if (ra == rb) return
    if (ra < rb) then
       par(rb) = ra
    else
       par(ra) = rb
    end if
  end subroutine uf_union

  !> Start of every run of equal keys in a sorted array: ukey(1:nu), start(1:nu+1).
  subroutine run_starts(key, n, ukey, start, nu)
    integer(int64), intent(in) :: key(:), n
    integer(int64), allocatable, intent(out) :: ukey(:), start(:)
    integer(int64), intent(out) :: nu
    integer(int64) :: i
    nu = 0
    do i = 1, n
       if (i == 1) then
          nu = 1
       else if (key(i) /= key(i - 1)) then
          nu = nu + 1
       end if
    end do
    allocate(ukey(max(nu, 1_int64)), start(nu + 1))
    nu = 0
    do i = 1, n
       if (i == 1) then
          nu = 1; ukey(1) = key(1); start(1) = 1
       else if (key(i) /= key(i - 1)) then
          nu = nu + 1; ukey(nu) = key(i); start(nu) = i
       end if
    end do
    start(nu + 1) = n + 1
  end subroutine run_starts

  !> Index of key k in the sorted unique keys (0 if absent).
  pure integer(int64) function find_cell(ukey, nu, k) result(p)
    integer(int64), intent(in) :: ukey(:), nu, k
    integer(int64) :: lo, hi, mid
    p = 0; lo = 1; hi = nu
    do while (lo <= hi)
       mid = (lo + hi) / 2
       if (ukey(mid) == k) then
          p = mid; return
       else if (ukey(mid) < k) then
          lo = mid + 1
       else
          hi = mid - 1
       end if
    end do
  end function find_cell

end module TrapPNumModule
