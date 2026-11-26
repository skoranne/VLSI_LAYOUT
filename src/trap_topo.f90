! File   : trap_topo.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: polygon-level topological selects on any-angle layers (canonical trapezoid sets).
!
! A polygon is a PNUM component (TrapPNum: closed-set components). For every polygon a of A the
! relation with layer B is decided exactly, then a is selected whole (all its pieces):
!   overlap      some piece of a and some piece of B have intersecting interiors
!   touch        some pair meets along a segment of positive length (interiors disjoint)
!   point touch  some pair meets in a single point
!   inside       a is covered by the region of B (a subset of B, as closed sets)
!   contains     some polygon b of B with interiors meeting a lies inside A (then b is inside a)
!   equal        a inside B and some b inside A that meets a with the same exact area
!   coveredby    a inside B and the boundary of a meets the complement of B along a segment
!   covers       some b inside A, meeting a, whose boundary meets the complement of A
!   overlaps     some b meeting a where neither a inside B nor b inside A
! and the select predicates combine these as in design_topo_impl.f90 (CUT = overlap and not
! inside, TOUCH = touch and not overlap, MEET = (touch or point touch) and not overlap, OUTSIDE /
! DISJOINT = no contact at all).
!
! Coverage of a piece p by a layer B is decided without any rounding: inside each horizontal slab
! between consecutive rows of the B pieces near p, the complement of B is the sequence of gaps
! between consecutive B pieces (sorted by x; they are interior-disjoint), and each gap is itself a
! trapezoid whose sides are B's own lattice lines. p is covered iff no gap overlaps p with positive
! area (trap_overlap_positive); a gap in the slabs one row beyond p that meets p along a segment
! means the boundary of p meets the complement. All predicates are exact (TrapezoidModule).
module TrapTopoModule
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
  use omp_lib
  use TrapezoidModule
  use TrapPNumModule, only: TrapPolys, TrapPNum
  implicit none
  private
  public :: TrapTopoSelect, TrapTopoStats

  integer(int64), parameter :: T_INTERACT = 101, T_NOT_INTERACT = 102, T_CUT = 103, T_NOT_CUT = 104, &
       T_TOUCH = 105, T_NOT_TOUCH = 106, T_INSIDE = 107, T_NOT_INSIDE = 108, T_OUTSIDE = 109, &
       T_NOT_OUTSIDE = 110, T_EQUAL = 119, T_NOT_EQUAL = 120, T_DISJOINT = 121, T_NOT_DISJOINT = 122, &
       T_MEET = 123, T_NOT_MEET = 124, T_OVERLAPS = 125, T_NOT_OVERLAPS = 126, T_COVERS = 127, &
       T_NOT_COVERS = 128, T_COVEREDBY = 129, T_NOT_COVEREDBY = 130, T_CONTAINS = 131, T_NOT_CONTAINS = 132

  type :: TrapTopoStats
     integer(int64) :: polys_a = 0, polys_b = 0, selected = 0, pairs = 0, anomalies = 0
     real(real64) :: t_pnum = 0, t_pairs = 0, t_cover = 0, t_total = 0
  end type TrapTopoStats

  !> pieces of one layer with bounding boxes and a uniform grid over them (CSR)
  type :: PieceGrid
     integer(int64) :: n = 0, nx = 0, ny = 0, cs = 1, gx0 = 0, gy0 = 0
     integer(int64), allocatable :: x1(:), y1(:), x2(:), y2(:), cstart(:), citem(:)
  end type PieceGrid

contains

  !> Select the polygons of A (tA(1:nA)) by their relation with B (tB(1:nB)); sel receives the
  !> pieces of the selected polygons (canonical order of A kept).
  subroutine TrapTopoSelect(tA, nA, tB, nB, op, sel, nsel, st)
    type(Trapezoid), intent(in) :: tA(:), tB(:)
    integer(int64), intent(in) :: nA, nB, op
    type(Trapezoid), allocatable, intent(out) :: sel(:)
    integer(int64), intent(out) :: nsel
    type(TrapTopoStats), intent(out) :: st
    type(TrapPolys) :: PA, PB
    type(PieceGrid) :: GA, GB
    integer(int8), allocatable :: f_ov(:), f_t1(:), f_t2(:), a_out(:), a_bnd(:), b_out(:), b_bnd(:)
    integer(int8), allocatable :: f_contains(:), f_equal(:), f_covers(:), f_prop(:)
    logical, allocatable :: pick(:)
    integer(int64), allocatable :: pa_(:), pb_(:)
    integer(int64) :: i, np, k
    logical :: need_ab, need_ba, s
    real(real64) :: t0, t1
    t0 = omp_get_wtime()
    st = TrapTopoStats()
    call TrapPNum(tA, nA, PA)
    call TrapPNum(tB, nB, PB)
    st%polys_a = PA%np; st%polys_b = PB%np
    st%anomalies = PA%anomalies + PB%anomalies
    t1 = omp_get_wtime(); st%t_pnum = t1 - t0
    allocate(f_ov(max(PA%np, 1_int64)), f_t1(max(PA%np, 1_int64)), f_t2(max(PA%np, 1_int64)))
    f_ov = 0; f_t1 = 0; f_t2 = 0
    call build_grid(tB, nB, GB)
    call build_grid(tA, nA, GA)
    call contact_pairs(tA, nA, PA%lab, GA, tB, PB%lab, GB, f_ov, f_t1, f_t2, pa_, pb_, np)
    st%pairs = np
    st%t_pairs = omp_get_wtime() - t1; t1 = omp_get_wtime()
    need_ab = any(op == [T_INSIDE, T_NOT_INSIDE, T_CUT, T_NOT_CUT, T_EQUAL, T_NOT_EQUAL, T_COVEREDBY, &
         T_NOT_COVEREDBY, T_OVERLAPS, T_NOT_OVERLAPS])
    need_ba = any(op == [T_CONTAINS, T_NOT_CONTAINS, T_EQUAL, T_NOT_EQUAL, T_COVERS, T_NOT_COVERS, &
         T_OVERLAPS, T_NOT_OVERLAPS])
    allocate(a_out(max(PA%np, 1_int64)), a_bnd(max(PA%np, 1_int64)), b_out(max(PB%np, 1_int64)), &
         b_bnd(max(PB%np, 1_int64)))
    a_out = 0; a_bnd = 0; b_out = 0; b_bnd = 0
    if (need_ab) call coverage(tA, nA, PA%lab, GA, tB, GB, a_out, a_bnd)
    if (need_ba) call coverage(tB, nB, PB%lab, GB, tA, GA, b_out, b_bnd)
    st%t_cover = omp_get_wtime() - t1
    ! pair-derived flags (pairs = polygon pairs with intersecting interiors)
    allocate(f_contains(max(PA%np, 1_int64)), f_equal(max(PA%np, 1_int64)), f_covers(max(PA%np, 1_int64)), &
         f_prop(max(PA%np, 1_int64)))
    f_contains = 0; f_equal = 0; f_covers = 0; f_prop = 0
    do k = 1, np
       associate(a => pa_(k), b => pb_(k))
         if (b_out(b) == 0) then
            f_contains(a) = 1
            if (b_bnd(b) /= 0) f_covers(a) = 1
            if (a_out(a) == 0 .and. PA%area2(a) == PB%area2(b)) f_equal(a) = 1
         end if
         if (a_out(a) /= 0 .and. b_out(b) /= 0) f_prop(a) = 1
       end associate
    end do
    allocate(pick(max(PA%np, 1_int64)))
    !$omp parallel do private(s)
    do i = 1, PA%np
       select case (op)
       case (T_INTERACT);      s = f_ov(i) /= 0
       case (T_NOT_INTERACT);  s = f_ov(i) == 0
       case (T_INSIDE);        s = f_ov(i) /= 0 .and. a_out(i) == 0
       case (T_NOT_INSIDE);    s = .not. (f_ov(i) /= 0 .and. a_out(i) == 0)
       case (T_OUTSIDE, T_DISJOINT);         s = f_ov(i) == 0 .and. f_t1(i) == 0 .and. f_t2(i) == 0
       case (T_NOT_OUTSIDE, T_NOT_DISJOINT); s = .not. (f_ov(i) == 0 .and. f_t1(i) == 0 .and. f_t2(i) == 0)
       case (T_TOUCH);         s = f_t2(i) /= 0 .and. f_ov(i) == 0
       case (T_NOT_TOUCH);     s = .not. (f_t2(i) /= 0 .and. f_ov(i) == 0)
       case (T_CUT);           s = f_ov(i) /= 0 .and. a_out(i) /= 0
       case (T_NOT_CUT);       s = .not. (f_ov(i) /= 0 .and. a_out(i) /= 0)
       case (T_MEET);          s = (f_t1(i) /= 0 .or. f_t2(i) /= 0) .and. f_ov(i) == 0
       case (T_NOT_MEET);      s = .not. ((f_t1(i) /= 0 .or. f_t2(i) /= 0) .and. f_ov(i) == 0)
       case (T_EQUAL);         s = f_equal(i) /= 0
       case (T_NOT_EQUAL);     s = f_equal(i) == 0
       case (T_CONTAINS);      s = f_contains(i) /= 0
       case (T_NOT_CONTAINS);  s = f_contains(i) == 0
       case (T_COVERS);        s = f_covers(i) /= 0
       case (T_NOT_COVERS);    s = f_covers(i) == 0
       case (T_COVEREDBY);     s = f_ov(i) /= 0 .and. a_out(i) == 0 .and. a_bnd(i) /= 0
       case (T_NOT_COVEREDBY); s = .not. (f_ov(i) /= 0 .and. a_out(i) == 0 .and. a_bnd(i) /= 0)
       case (T_OVERLAPS);      s = f_prop(i) /= 0
       case (T_NOT_OVERLAPS);  s = f_prop(i) == 0
       case default;           s = .false.
       end select
       pick(i) = s
    end do
    !$omp end parallel do
    st%selected = count(pick(1:PA%np))
    nsel = 0
    do i = 1, nA
       if (pick(PA%lab(i))) nsel = nsel + 1
    end do
    allocate(sel(max(nsel, 1_int64)))
    nsel = 0
    do i = 1, nA
       if (pick(PA%lab(i))) then
          nsel = nsel + 1; sel(nsel) = tA(i)
       end if
    end do
    st%t_total = omp_get_wtime() - t0
  end subroutine TrapTopoSelect

  ! ------------------------------------------------------------------ grid

  subroutine build_grid(t, n, G)
    type(Trapezoid), intent(in) :: t(:)
    integer(int64), intent(in) :: n
    type(PieceGrid), intent(out) :: G
    integer(int64) :: i, k, cx, cy, gx1, gy1, sw, sh, ncell
    integer(int64), allocatable :: cfill(:)
    G%n = n
    allocate(G%x1(max(n, 1_int64)), G%y1(max(n, 1_int64)), G%x2(max(n, 1_int64)), G%y2(max(n, 1_int64)))
    if (n == 0) then
       G%nx = 1; G%ny = 1; allocate(G%cstart(2), G%citem(1)); G%cstart = 1; return
    end if
    !$omp parallel do
    do i = 1, n
       call trap_mbr(t(i), G%x1(i), G%y1(i), G%x2(i), G%y2(i))
    end do
    !$omp end parallel do
    G%gx0 = minval(G%x1(1:n)); G%gy0 = minval(G%y1(1:n))
    gx1 = maxval(G%x2(1:n)); gy1 = maxval(G%y2(1:n))
    sw = 0; sh = 0
    !$omp parallel do reduction(+:sw, sh)
    do i = 1, n
       sw = sw + (G%x2(i) - G%x1(i)); sh = sh + (G%y2(i) - G%y1(i))
    end do
    !$omp end parallel do
    G%cs = max(1_int64, 2 * max(sw, sh) / n)
    do
       G%nx = (gx1 - G%gx0) / G%cs + 1; G%ny = (gy1 - G%gy0) / G%cs + 1
       if (G%nx * G%ny <= 4 * n + 1024) exit
       G%cs = G%cs * 2
    end do
    ncell = G%nx * G%ny
    allocate(G%cstart(ncell + 1), cfill(ncell))
    G%cstart = 0
    do i = 1, n
       do cy = (G%y1(i) - G%gy0) / G%cs, (G%y2(i) - G%gy0) / G%cs
          do cx = (G%x1(i) - G%gx0) / G%cs, (G%x2(i) - G%gx0) / G%cs
             k = cy * G%nx + cx + 2
             G%cstart(k) = G%cstart(k) + 1
          end do
       end do
    end do
    G%cstart(1) = 1
    do k = 2, ncell + 1
       G%cstart(k) = G%cstart(k) + G%cstart(k - 1)
    end do
    allocate(G%citem(max(G%cstart(ncell + 1) - 1, 1_int64)))
    cfill = G%cstart(1:ncell)
    do i = 1, n
       do cy = (G%y1(i) - G%gy0) / G%cs, (G%y2(i) - G%gy0) / G%cs
          do cx = (G%x1(i) - G%gx0) / G%cs, (G%x2(i) - G%gx0) / G%cs
             k = cy * G%nx + cx + 1
             G%citem(cfill(k)) = i
             cfill(k) = cfill(k) + 1
          end do
       end do
    end do
  end subroutine build_grid

  !> Pieces q of G whose closed bounding box meets [x1,x2]x[y1,y2], each once.
  subroutine grid_query(G, x1, y1, x2, y2, out, nout)
    type(PieceGrid), intent(in) :: G
    integer(int64), intent(in) :: x1, y1, x2, y2
    integer(int64), allocatable, intent(inout) :: out(:)
    integer(int64), intent(out) :: nout
    integer(int64) :: cxa, cxb, cya, cyb, cx, cy, k, j, q
    integer(int64), allocatable :: tmp(:)
    nout = 0
    if (G%n == 0) return
    cxa = max(0_int64, (x1 - G%gx0) / G%cs); cxb = min(G%nx - 1, (x2 - G%gx0) / G%cs)
    cya = max(0_int64, (y1 - G%gy0) / G%cs); cyb = min(G%ny - 1, (y2 - G%gy0) / G%cs)
    if (x2 < G%gx0 .or. y2 < G%gy0) return
    do cy = cya, cyb
       do cx = cxa, cxb
          k = cy * G%nx + cx + 1
          do j = G%cstart(k), G%cstart(k + 1) - 1
             q = G%citem(j)
             if (G%x1(q) > x2 .or. G%x2(q) < x1 .or. G%y1(q) > y2 .or. G%y2(q) < y1) cycle
             ! report once: in the cell of the low corner of the bbox intersection (clamped)
             if (max(cxa, (max(x1, G%x1(q)) - G%gx0) / G%cs) /= cx) cycle
             if (max(cya, (max(y1, G%y1(q)) - G%gy0) / G%cs) /= cy) cycle
             if (nout == size(out, kind=int64)) then
                allocate(tmp(2 * nout)); tmp(1:nout) = out(1:nout); call move_alloc(tmp, out)
             end if
             nout = nout + 1
             out(nout) = q
          end do
       end do
    end do
  end subroutine grid_query

  ! ------------------------------------------------------------------ contacts

  !> Per polygon of A: interiors meet B (f_ov), point contact (f_t1), segment contact (f_t2);
  !> pairs (polygon of A, polygon of B) with intersecting interiors, unique.
  subroutine contact_pairs(tA, nA, labA, GA, tB, labB, GB, f_ov, f_t1, f_t2, pa_, pb_, np)
    type(Trapezoid), intent(in) :: tA(:), tB(:)
    integer(int64), intent(in) :: nA, labA(:), labB(:)
    type(PieceGrid), intent(in) :: GA, GB
    integer(int8), intent(inout) :: f_ov(:), f_t1(:), f_t2(:)
    integer(int64), allocatable, intent(out) :: pa_(:), pb_(:)
    integer(int64), intent(out) :: np
    integer(int64), allocatable :: cand(:), tmp(:)
    integer(int64) :: i, j, nc, nb, a, b, nt, tid, off, k
    integer :: kc
    type :: Buf
       integer(int64), allocatable :: a(:), b(:)
       integer(int64) :: n = 0
    end type Buf
    type(Buf), allocatable :: TBF(:)
    nt = omp_get_max_threads()
    allocate(TBF(nt))
    do tid = 1, nt
       allocate(TBF(tid)%a(256), TBF(tid)%b(256)); TBF(tid)%n = 0
    end do
    !$omp parallel private(cand, nc, i, j, a, b, kc, tid, tmp, nb)
    tid = omp_get_thread_num() + 1
    allocate(cand(64))
    !$omp do schedule(dynamic, 512)
    do i = 1, nA
       call grid_query(GB, GA%x1(i), GA%y1(i), GA%x2(i), GA%y2(i), cand, nc)
       a = labA(i)
       do j = 1, nc
          kc = trap_contact(tA(i), tB(cand(j)))
          select case (kc)
          case (1)
             !$omp atomic write
             f_t1(a) = 1_int8
          case (2)
             !$omp atomic write
             f_t2(a) = 1_int8
          case (3)
             !$omp atomic write
             f_ov(a) = 1_int8
             b = labB(cand(j))
             nb = TBF(tid)%n
             if (nb > 0) then
                if (TBF(tid)%a(nb) == a .and. TBF(tid)%b(nb) == b) cycle
             end if
             if (nb == size(TBF(tid)%a, kind=int64)) then
                allocate(tmp(2 * nb)); tmp(1:nb) = TBF(tid)%a(1:nb); call move_alloc(tmp, TBF(tid)%a)
                allocate(tmp(2 * nb)); tmp(1:nb) = TBF(tid)%b(1:nb); call move_alloc(tmp, TBF(tid)%b)
             end if
             TBF(tid)%n = nb + 1
             TBF(tid)%a(nb + 1) = a; TBF(tid)%b(nb + 1) = b
          end select
       end do
    end do
    !$omp end do
    deallocate(cand)
    !$omp end parallel
    ! gather, sort by (a, b) and make unique
    np = 0
    do tid = 1, nt
       np = np + TBF(tid)%n
    end do
    allocate(pa_(max(np, 1_int64)), pb_(max(np, 1_int64)))
    off = 0
    do tid = 1, nt
       pa_(off + 1:off + TBF(tid)%n) = TBF(tid)%a(1:TBF(tid)%n)
       pb_(off + 1:off + TBF(tid)%n) = TBF(tid)%b(1:TBF(tid)%n)
       off = off + TBF(tid)%n
    end do
    call sort_pairs(pa_, pb_, np)
    k = 0
    do j = 1, np
       if (k > 0) then
          if (pa_(k) == pa_(j) .and. pb_(k) == pb_(j)) cycle
       end if
       k = k + 1; pa_(k) = pa_(j); pb_(k) = pb_(j)
    end do
    np = k
  end subroutine contact_pairs

  !> Sort (a, b) pairs lexicographically: keys a * 2**32 + b (a, b < 2**31), bottom-up merge sort
  !> (iterative: no recursion through assumed-shape arrays).
  subroutine sort_pairs(a, b, n)
    integer(int64), intent(inout) :: a(:), b(:)
    integer(int64), intent(in) :: n
    integer(int64), allocatable :: k1(:), k2(:)
    integer(int64) :: i, w, lo, mid, hi, p, q, r
    if (n < 2) return
    allocate(k1(n), k2(n))
    do i = 1, n
       k1(i) = ior(shiftl(a(i), 32), b(i))
    end do
    w = 1
    do while (w < n)
       lo = 1
       do while (lo <= n)
          mid = min(lo + w - 1, n); hi = min(lo + 2 * w - 1, n)
          p = lo; q = mid + 1; r = lo
          do while (p <= mid .and. q <= hi)
             if (k1(q) < k1(p)) then
                k2(r) = k1(q); q = q + 1
             else
                k2(r) = k1(p); p = p + 1
             end if
             r = r + 1
          end do
          do while (p <= mid)
             k2(r) = k1(p); p = p + 1; r = r + 1
          end do
          do while (q <= hi)
             k2(r) = k1(q); q = q + 1; r = r + 1
          end do
          lo = lo + 2 * w
       end do
       k1(1:n) = k2(1:n)
       w = 2 * w
    end do
    do i = 1, n
       a(i) = shiftr(k1(i), 32); b(i) = iand(k1(i), 4294967295_int64)
    end do
  end subroutine sort_pairs

  ! ------------------------------------------------------------------ coverage

  !> For every piece p of P (labels labP): out(polygon) = 1 when some part of p of positive area
  !> lies outside the region of the pieces tQ; bnd(polygon) = 1 when p meets the complement of Q
  !> along a segment (boundary contact; only meaningful when the polygon is inside).
  subroutine coverage(tP, nP, labP, GP, tQ, GQ, out, bnd)
    type(Trapezoid), intent(in) :: tP(:), tQ(:)
    integer(int64), intent(in) :: nP, labP(:)
    type(PieceGrid), intent(in) :: GP, GQ
    integer(int8), intent(inout) :: out(:), bnd(:)
    integer(int64), allocatable :: cand(:)
    integer(int64) :: i, nc
    logical :: o, b
    !$omp parallel private(cand, nc, i, o, b)
    allocate(cand(64))
    !$omp do schedule(dynamic, 256)
    do i = 1, nP
       call grid_query(GQ, GP%x1(i) - 1, GP%y1(i) - 1, GP%x2(i) + 1, GP%y2(i) + 1, cand, nc)
       call piece_cover(tP(i), GP%x1(i), GP%x2(i), tQ, GQ, cand, nc, o, b)
       if (o) then
          !$omp atomic write
          out(labP(i)) = 1_int8
       end if
       if (b) then
          !$omp atomic write
          bnd(labP(i)) = 1_int8
       end if
    end do
    !$omp end do
    deallocate(cand)
    !$omp end parallel
  end subroutine coverage

  !> Coverage of one piece p by the candidate pieces cand(1:nc) of Q (see the module header).
  subroutine piece_cover(p, px1, px2, tQ, GQ, cand, nc, outside, boundary)
    type(Trapezoid), intent(in) :: p, tQ(:)
    integer(int64), intent(in) :: px1, px2, nc, cand(:)
    type(PieceGrid), intent(in) :: GQ
    logical, intent(out) :: outside, boundary
    integer(int64), allocatable :: rows(:)
    type(Trapezoid), allocatable :: cl(:)
    type(Trapezoid) :: pc, g, tmp
    type(TrapSide) :: prev
    integer(int64) :: nr, j, s, r0, r1, m, ylo, yhi, xlo, xhi, k, q
    integer :: status
    logical :: in_p
    outside = .false.; boundary = .false.
    ylo = int(p%y1, int64) - 1; yhi = int(p%y2, int64) + 1
    xlo = px1 - 2; xhi = px2 + 2
    allocate(rows(2 * nc + 4), cl(max(nc, 1_int64)))
    nr = 0
    rows(1) = ylo; rows(2) = p%y1; rows(3) = p%y2; rows(4) = yhi; nr = 4
    do j = 1, nc
       q = cand(j)
       xlo = min(xlo, GQ%x1(q) - 2); xhi = max(xhi, GQ%x2(q) + 2)
       if (tQ(q)%y1 > ylo .and. tQ(q)%y1 < yhi) then
          nr = nr + 1; rows(nr) = tQ(q)%y1
       end if
       if (tQ(q)%y2 > ylo .and. tQ(q)%y2 < yhi) then
          nr = nr + 1; rows(nr) = tQ(q)%y2
       end if
    end do
    call isort(rows, nr)
    m = 0
    do j = 1, nr
       if (m > 0) then
          if (rows(m) == rows(j)) cycle
       end if
       m = m + 1; rows(m) = rows(j)
    end do
    nr = m
    do s = 1, nr - 1
       r0 = rows(s); r1 = rows(s + 1)
       in_p = r0 >= p%y1 .and. r1 <= p%y2
       ! Q pieces across the slab, clipped, sorted by x
       m = 0
       do j = 1, nc
          q = cand(j)
          if (tQ(q)%y1 >= r1 .or. tQ(q)%y2 <= r0) cycle
          call trap_clip_y(tQ(q), r0, r1, tmp, status)
          if (status /= K_TRAP_OK) cycle
          m = m + 1; cl(m) = tmp
          k = m                                    ! insertion by left side at r0, then r1
          do while (k > 1)
             if (.not. left_before(cl(k), cl(k - 1), r0, r1)) exit
             tmp = cl(k); cl(k) = cl(k - 1); cl(k - 1) = tmp
             k = k - 1
          end do
       end do
       if (in_p) then
          call trap_clip_y(p, r0, r1, pc, status)
       end if
       prev = side_vertical(int(xlo, int32))
       do j = 1, m + 1
          g%y1 = int(r0, int32); g%y2 = int(r1, int32)
          g%l = prev
          if (j <= m) then
             g%r = cl(j)%l
          else
             g%r = side_vertical(int(xhi, int32))
          end if
          if (j <= m) prev = cl(j)%r
          if (side_equal(g%l, g%r)) cycle                       ! adjacent pieces: no gap
          if (in_p) then
             if (trap_overlap_positive(pc, g)) then
                outside = .true.; return
             end if
          end if
          if (.not. boundary) then
             if (trap_contact(p, g) == 2) boundary = .true.
          end if
       end do
    end do
  end subroutine piece_cover

  !> a before b in a slab [r0, r1]: left side of a left of b's at r0, or equal there and at r1.
  pure logical function left_before(a, b, r0, r1) result(lt)
    type(Trapezoid), intent(in) :: a, b
    integer(int64), intent(in) :: r0, r1
    integer :: c
    c = side_x_cmp(a%l, int(a%y1, int64), b%l, int(b%y1, int64), r0)
    if (c == 0) c = side_x_cmp(a%l, int(a%y1, int64), b%l, int(b%y1, int64), r1)
    lt = c < 0
  end function left_before

  subroutine isort(v, n)
    integer(int64), intent(inout) :: v(:)
    integer(int64), intent(in) :: n
    integer(int64) :: i, j, x
    do i = 2, n
       x = v(i); j = i - 1
       do while (j >= 1)
          if (v(j) <= x) exit
          v(j + 1) = v(j); j = j - 1
       end do
       v(j + 1) = x
    end do
  end subroutine isort

end module TrapTopoModule
