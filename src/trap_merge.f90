! File   : trap_merge.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Single-layer OR (merge) of any-angle polygons into canonical, interior-disjoint
!          horizontal trapezoids (TrapezoidModule), exactly, on integer coordinates.
!
! Pipeline (TrapMergePolygons):
!   1. TrapBuildEdges  polygons -> directed edges. Every polygon is oriented counter-clockwise
!                      (exact i128 signed area), so it contributes winding +1 inside; the union
!                      is the nonzero-winding region. An edge is stored with y1 < y2 and
!                      w = +1 when the polygon ran downwards (the region is on its right), -1
!                      otherwise; horizontal edges have w = 0.
!   2. TrapSnapRound   Hobby / Guibas-Marimont snap rounding. Hot pixels are the half-open unit
!                      squares around every vertex and around every proper crossing rounded half
!                      up. Every edge is replaced by the polyline through the centres of the hot
!                      pixels it meets, in order. Afterwards no two fragments cross, no fragment
!                      passes through a vertex of another, and fragments that overlap are
!                      identical (they are merged, windings added). Each point moves by at most
!                      sqrt(2)/2 dbu; when every crossing is a lattice point (Manhattan data,
!                      45-degree data on lattice points) nothing moves and the result is exact.
!                      Crossings and pixel hits are found in a uniform cell grid (each edge
!                      registered in the cells along it, each pair/hit handled in exactly one
!                      cell); everything is parallel (OpenMP) and deterministic.
!   3. TrapSweepUnion  band-parallel sweep over integer rows. Fragments do not cross, so the
!                      order of the active fragments changes only at rows with events, and only
!                      inside the x-intervals of the row's event points and horizontal
!                      fragments (clusters). The active fragments are kept in a treap ordered by
!                      exact x (real64 filter, i128 fallback) and augmented with subtree winding
!                      sums and boundary-flag counts. A trapezoid is a maximal inside interval
!                      of a row, bounded by two boundary fragments; it stays open while its two
!                      bounding lines stay the same, so the output is canonical: it depends only
!                      on the region, not on the number of bands or threads.
!   4. TrapStitch      trapezoids cut at band rows are joined (same two lines, vertically
!                      adjacent), then converted to Trapezoid (lattice-line sides).
!
! Coordinates must satisfy |c| <= 2**30 - 1 (invariant I0); status 2 otherwise.
! Every internal consistency check that fails increments stats%anomalies; a correct run has 0.
module TrapMergeModule
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
  use omp_lib
  use WideIntModule
  use ExactPredicatesModule
  use TrapezoidModule
  use TrapArenaModule
  use CompactLayerModule, only: CompactSortPairs
  implicit none
  private

  integer(int64), parameter :: KB = 1073741824_int64        ! 2**30, key bias
  integer(int64), parameter :: K31 = 2147483648_int64       ! 2**31
  !> Region predicates of the sweep on the packed winding pair (WA, WB) of the snapped operands.
  !> TOP_NONZERO is the single-operand merge (W /= 0, any packing).
  integer(int32), parameter, public :: TOP_NONZERO = 0, TOP_OR = 1, TOP_AND = 2, TOP_NOT = 3, &
       TOP_XOR = 4, TOP_BNOTA = 5, TOP_SELF2 = 6, TOP_A = 7, TOP_B = 8
  integer(int64), parameter :: K32 = 4294967296_int64       ! 2**32, operand B weight

  !> Edges or fragments (SoA). Non-horizontal: y1 < y2. Horizontal: y1 == y2, x1 < x2, w = 0.
  type, public :: FragSet
     integer(int64) :: n = 0
     integer(int64), allocatable :: x1(:), y1(:), x2(:), y2(:)
     integer(int64), allocatable :: w(:)          !> packed winding pair wA + 2**32 wB (wpack)
  end type FragSet

  type, public :: TrapMergeStats
     integer(int64) :: polygons = 0, edges = 0, crossings = 0, crossings_inexact = 0
     integer(int64) :: hot_pixels = 0, hits = 0, fragments = 0, fragments_merged = 0
     integer(int64) :: cells = 0, max_cell = 0, registrations = 0, pair_tests = 0
     integer(int64) :: traps_raw = 0, traps = 0, boxes = 0, bands = 0, anomalies = 0
     real(real64) :: t_edges = 0, t_grid = 0, t_cross = 0, t_hot = 0, t_hits = 0, t_frag = 0
     real(real64) :: t_sweep = 0, t_stitch = 0, t_total = 0
  end type TrapMergeStats

  !> One Boolean result (canonical trapezoids).
  type, public :: TrapList
     integer(int32) :: op = 0
     integer(int64) :: n = 0
     type(Trapezoid), allocatable :: t(:)
     real(real64) :: t_sweep = 0, t_stitch = 0
     integer(int64) :: traps_raw = 0, boxes = 0, anomalies = 0
  end type TrapList

  type :: I64Buf
     integer(int64) :: n = 0
     integer(int64), allocatable :: v(:)
  end type I64Buf

  type :: CellGrid
     integer(int64) :: s = 1, gx0 = 0, gy0 = 0, ncx = 1, ncy = 1
  end type CellGrid

  !> Sweep state of one band. Node 0 is the null sentinel (all sums 0).
  type :: Sweep
     integer(int32) :: nn = 0, root = 0
     integer(int64) :: y = 0
     integer(int64) :: anomalies = 0
     integer(int64) :: atype(8) = 0
     integer(int32) :: rng = -1831433054_int32
     integer(int64), allocatable :: x1(:), y1(:), x2(:), y2(:), dx(:), dy(:), rec_y(:)
     integer(int32) :: op = TOP_NONZERO
     integer(int64), allocatable :: w(:), sw(:)
     integer(int32), allocatable :: lc(:), rc(:), par(:), pri(:), slb(:), srb(:)
     integer(int32), allocatable :: rec_r(:)
     integer(int64), allocatable :: gid(:)
     integer(int8), allocatable :: flb(:), frb(:)
     ! work arrays (grown on demand)
     integer(int32), allocatable :: seq(:)
     integer(int32), allocatable :: oL(:), oR(:), nL(:), nR(:)
     logical, allocatable :: oLin(:), oRin(:), nLin(:), nRin(:), used(:)
     integer(int64), allocatable :: oys(:), nys(:)
     integer(int32) :: nseq = 0
     ! persistent per-thread event & cluster scratch (grown on high-water mark, never freed between bands)
     integer(int64), allocatable :: sk(:), sv(:), ek(:), ev(:), hk(:), hv(:), iv(:), stk(:), sti(:)
     integer(int64), allocatable :: ca(:), cb(:), ce0(:), cs0(:), ce1(:), cs1(:)
     integer(int32), allocatable :: starts(:), ends(:)
     ! output: raw trapezoids (y1, y2, left fragment, right fragment), global fragment ids
     integer(int64) :: nout = 0
     integer(int64), allocatable :: oy1(:), oy2(:), outl(:), outr(:)
  end type Sweep

  !> Persistent per-thread sweep arena pool (Tier 1 consing elimination).
  type(Sweep), allocatable, save :: g_sw(:)

  public :: TrapMergePolygons, TrapBuildEdges, TrapSnapRound, TrapSweepUnion
  public :: TrapBooleanPolygons
  public :: TrapMergeStatsLine, TrapConcatEdges, TrapInside, wpack, wunpack, free_set


contains

  ! ======================================================================= driver

  !> Merge (single-layer OR) of npoly polygons. Polygon i has the vertices
  !> (px(j), py(j)), j = poff(i) .. poff(i+1)-1 (implicitly closed, any orientation).
  !> nbands <= 0 picks 8 bands per thread. status: 0 ok, 2 coordinate out of range.
  subroutine TrapMergePolygons(npoly, poff, px, py, traps, ntrap, st, status, nbands)
    integer(int64), intent(in) :: npoly, poff(:), px(:), py(:)
    type(Trapezoid), allocatable, intent(out) :: traps(:)
    integer(int64), intent(out) :: ntrap
    type(TrapMergeStats), intent(inout) :: st
    integer, intent(out) :: status
    integer, intent(in), optional :: nbands
    type(FragSet) :: E, F
    real(real64) :: t0, t1
    integer :: nb
    nb = 0
    if (present(nbands)) nb = nbands
    t0 = omp_get_wtime()
    st%polygons = npoly
    call TrapBuildEdges(npoly, poff, px, py, E, status)
    t1 = omp_get_wtime(); st%t_edges = t1 - t0
    ntrap = 0
    if (status /= 0) then
       allocate(traps(0)); return
    end if
    st%edges = E%n
    call TrapSnapRound(E, F, st)
    call free_set(E)
    call TrapSweepUnion(F, nb, traps, ntrap, st)
    st%t_total = omp_get_wtime() - t0
  end subroutine TrapMergePolygons

  !> Two-operand Booleans on any-angle polygons. A and B are snapped together (one joint snap
  !> rounding: every hot pixel is shared, coincident edges of A and B become identical fragments),
  !> then one predicate sweep per requested op (TOP_*; see TrapInside) on the same fragments, so
  !> all results are exact set operations on the same snapped operands A' and B':
  !> area(OR) + area(AND) = area(A') + area(B'), XOR = OR - AND, NOT = A' - AND, ...
  !> res(k) receives ops(k). st collects the shared stages; per-op sweep data is in res(k).
  subroutine TrapBooleanPolygons(npA, poffA, pxA, pyA, npB, poffB, pxB, pyB, nops, ops, res, &
                                 st, status, nbands)
    integer(int64), intent(in) :: npA, poffA(:), pxA(:), pyA(:)
    integer(int64), intent(in) :: npB, poffB(:), pxB(:), pyB(:)
    integer, intent(in) :: nops
    integer(int32), intent(in) :: ops(:)
    type(TrapList), intent(inout) :: res(:)
    type(TrapMergeStats), intent(inout) :: st
    integer, intent(out) :: status
    integer, intent(in), optional :: nbands
    type(FragSet) :: EA, EB, E, F
    type(TrapMergeStats) :: sk
    real(real64) :: t0, t1
    integer :: nb, k, st2
    nb = 0
    if (present(nbands)) nb = nbands
    t0 = omp_get_wtime()
    st%polygons = npA + npB
    call TrapBuildEdges(npA, poffA, pxA, pyA, EA, status)
    call TrapBuildEdges(npB, poffB, pxB, pyB, EB, st2)
    status = max(status, st2)
    do k = 1, nops
       res(k)%op = ops(k); res(k)%n = 0
       if (allocated(res(k)%t)) deallocate(res(k)%t)
    end do
    if (status /= 0) then
       do k = 1, nops
          allocate(res(k)%t(0))
       end do
       return
    end if
    call TrapConcatEdges(EA, EB, E)
    call free_set(EA); call free_set(EB)
    t1 = omp_get_wtime(); st%t_edges = t1 - t0
    st%edges = E%n
    call TrapSnapRound(E, F, st)
    call free_set(E)
    do k = 1, nops
       sk = TrapMergeStats()
       call TrapSweepUnion(F, nb, res(k)%t, res(k)%n, sk, ops(k))
       res(k)%t_sweep = sk%t_sweep; res(k)%t_stitch = sk%t_stitch
       res(k)%traps_raw = sk%traps_raw; res(k)%boxes = sk%boxes; res(k)%anomalies = sk%anomalies
       st%anomalies = st%anomalies + sk%anomalies
       st%t_sweep = st%t_sweep + sk%t_sweep; st%t_stitch = st%t_stitch + sk%t_stitch
       st%bands = sk%bands
       st%traps = st%traps + res(k)%n
    end do
    call free_set(F)
    st%t_total = omp_get_wtime() - t0
  end subroutine TrapBooleanPolygons

  function TrapMergeStatsLine(st) result(s)
    type(TrapMergeStats), intent(in) :: st
    character(len=1024) :: s
    write(s, '(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)') &
         'polygons=', st%polygons, ' edges=', st%edges, ' crossings=', st%crossings, &
         ' inexact=', st%crossings_inexact, ' hot=', st%hot_pixels, ' hits=', st%hits, &
         ' fragments=', st%fragments, ' merged_frags=', st%fragments_merged, &
         ' cells=', st%cells, ' max_cell=', st%max_cell, ' traps_raw=', st%traps_raw, &
         ' traps=', st%traps, ' bands=', st%bands, ' anomalies=', st%anomalies
  end function TrapMergeStatsLine


  !> Packed winding: wA + 2**32 wB. Exact (and additive) while |wA|, |wB| < 2**31.
  pure integer(int64) function wpack(wa, wb) result(w)
    integer(int64), intent(in) :: wa, wb
    w = wa + K32*wb
  end function wpack

  pure subroutine wunpack(w, wa, wb)
    integer(int64), intent(in) :: w
    integer(int64), intent(out) :: wa, wb
    integer(int64) :: t
    t = w + K31                                     ! wb = floor((w + 2**31) / 2**32)
    if (t >= 0) then
       wb = t / K32
    else
       wb = -((-t + K32 - 1) / K32)
    end if
    wa = w - K32*wb
  end subroutine wunpack

  !> Is the point with packed winding W inside the result of op?
  pure logical function TrapInside(op, W) result(r)
    integer(int32), intent(in) :: op
    integer(int64), intent(in) :: W
    integer(int64) :: wa, wb
    if (op == TOP_NONZERO .or. op == TOP_OR) then
       r = W /= 0; return
    end if
    call wunpack(W, wa, wb)
    select case (op)
    case (TOP_AND);   r = wa /= 0 .and. wb /= 0
    case (TOP_NOT);   r = wa /= 0 .and. wb == 0
    case (TOP_XOR);   r = (wa /= 0) .neqv. (wb /= 0)
    case (TOP_BNOTA); r = wb /= 0 .and. wa == 0
    case (TOP_SELF2); r = abs(wa) >= 2
    case (TOP_A);     r = wa /= 0
    case (TOP_B);     r = wb /= 0
    case default;     r = .false.
    end select
  end function TrapInside

  !> E = A followed by B, B's windings moved to the second operand (wA -> wB).
  subroutine TrapConcatEdges(A, B, E)
    type(FragSet), intent(in) :: A, B
    type(FragSet), intent(inout) :: E
    integer(int64) :: i
    call alloc_set(E, A%n + B%n)
    !$omp parallel do
    do i = 1, A%n
       E%x1(i) = A%x1(i); E%y1(i) = A%y1(i); E%x2(i) = A%x2(i); E%y2(i) = A%y2(i); E%w(i) = A%w(i)
    end do
    !$omp end parallel do
    !$omp parallel do
    do i = 1, B%n
       E%x1(A%n + i) = B%x1(i); E%y1(A%n + i) = B%y1(i); E%x2(A%n + i) = B%x2(i)
       E%y2(A%n + i) = B%y2(i); E%w(A%n + i) = K32*B%w(i)
    end do
    !$omp end parallel do
  end subroutine TrapConcatEdges

  subroutine free_set(S)
    type(FragSet), intent(inout) :: S
    if (allocated(S%x1)) deallocate(S%x1, S%y1, S%x2, S%y2, S%w)
    S%n = 0
  end subroutine free_set

  subroutine alloc_set(S, n)
    type(FragSet), intent(inout) :: S
    integer(int64), intent(in) :: n
    call free_set(S)
    allocate(S%x1(max(n, 1_int64)), S%y1(max(n, 1_int64)), S%x2(max(n, 1_int64)), &
             S%y2(max(n, 1_int64)), S%w(max(n, 1_int64)))
    S%n = n
  end subroutine alloc_set

  subroutine buf_push(b, v)
    type(I64Buf), intent(inout) :: b
    integer(int64), intent(in) :: v
    integer(int64), allocatable :: t(:)
    if (.not. allocated(b%v)) allocate(b%v(4096))
    if (b%n >= size(b%v, kind=int64)) then
       allocate(t(2*size(b%v, kind=int64)))
       t(1:b%n) = b%v(1:b%n)
       call move_alloc(t, b%v)
    end if
    b%n = b%n + 1
    b%v(b%n) = v
  end subroutine buf_push

  ! ======================================================================= 1. edges

  subroutine TrapBuildEdges(npoly, poff, px, py, E, status)
    integer(int64), intent(in) :: npoly, poff(:), px(:), py(:)
    type(FragSet), intent(inout) :: E
    integer, intent(out) :: status
    integer(int64), allocatable :: cnt(:), off(:)
    integer(int32), allocatable :: ori(:)
    integer(int64) :: i, tot
    integer :: st1
    status = 0
    allocate(cnt(max(npoly, 1_int64)), ori(max(npoly, 1_int64)), off(max(npoly, 1_int64) + 1))
    !$omp parallel do schedule(dynamic, 1024) private(st1) reduction(max:status)
    do i = 1, npoly
       call poly_scan(px, py, poff(i), poff(i + 1) - 1, cnt(i), ori(i), st1)
       status = max(status, st1)
    end do
    !$omp end parallel do
    if (status /= 0) then
       call alloc_set(E, 0_int64); return
    end if
    off(1) = 0
    do i = 1, npoly
       off(i + 1) = off(i) + cnt(i)
    end do
    tot = off(npoly + 1)
    call alloc_set(E, tot)
    !$omp parallel do schedule(dynamic, 1024)
    do i = 1, npoly
       if (cnt(i) > 0) call poly_emit(px, py, poff(i), poff(i + 1) - 1, ori(i), off(i), E)
    end do
    !$omp end parallel do
  end subroutine TrapBuildEdges

  !> Count the non-degenerate edges of one polygon and its orientation (+1 ccw, -1 cw, 0 flat).
  subroutine poly_scan(px, py, a, b, cnt, ori, status)
    integer(int64), intent(in) :: px(:), py(:), a, b
    integer(int64), intent(out) :: cnt
    integer(int32), intent(out) :: ori
    integer, intent(out) :: status
    type(i128) :: acc
    integer(int64) :: j, k
    cnt = 0; ori = 0; status = 0
    if (b - a + 1 < 3) return
    acc = i128_from_int64(0_int64)
    do j = a, b
       if (abs(px(j)) > K_SAFE_COORD .or. abs(py(j)) > K_SAFE_COORD) then
          status = 2; return
       end if
       k = j + 1
       if (k > b) k = a
       if (px(j) /= px(k) .or. py(j) /= py(k)) cnt = cnt + 1
       acc = i128_add(acc, i128_from_int64(px(j)*py(k) - px(k)*py(j)))
    end do
    ori = i128_sign(acc)
    if (ori == 0 .or. cnt < 3) then
       cnt = 0; ori = 0
    end if
  end subroutine poly_scan

  subroutine poly_emit(px, py, a, b, ori, off, E)
    integer(int64), intent(in) :: px(:), py(:), a, b, off
    integer(int32), intent(in) :: ori
    type(FragSet), intent(inout) :: E
    integer(int64) :: j, k, m
    m = off
    do j = a, b
       k = j + 1
       if (k > b) k = a
       if (px(j) == px(k) .and. py(j) == py(k)) cycle
       m = m + 1
       if (py(j) == py(k)) then
          E%x1(m) = min(px(j), px(k)); E%x2(m) = max(px(j), px(k))
          E%y1(m) = py(j); E%y2(m) = py(j); E%w(m) = 0
       else if (py(k) < py(j)) then              ! downwards: region on the right (ccw)
          E%x1(m) = px(k); E%y1(m) = py(k); E%x2(m) = px(j); E%y2(m) = py(j)
          E%w(m) = ori
       else
          E%x1(m) = px(j); E%y1(m) = py(j); E%x2(m) = px(k); E%y2(m) = py(k)
          E%w(m) = -ori
       end if
    end do
  end subroutine poly_emit

  ! ======================================================================= 2. snap rounding

  pure integer(int64) function cell_x(g, x)
    type(CellGrid), intent(in) :: g
    integer(int64), intent(in) :: x
    cell_x = min(max((x - g%gx0) / g%s, 0_int64), g%ncx - 1)
  end function cell_x

  pure integer(int64) function cell_y(g, y)
    type(CellGrid), intent(in) :: g
    integer(int64), intent(in) :: y
    cell_y = min(max((y - g%gy0) / g%s, 0_int64), g%ncy - 1)
  end function cell_y

  !> Cells along edge (x1,y1)-(x2,y2) (y1 <= y2), with a margin of 2 dbu, so that the edge is
  !> registered in the cell of every point within 1 dbu of it. Counts them (store = .false.)
  !> or writes the cell ids to cells(base+1:).
  subroutine edge_cells(g, x1, y1, x2, y2, cnt, store, cells, base)
    type(CellGrid), intent(in) :: g
    integer(int64), intent(in) :: x1, y1, x2, y2
    integer(int64), intent(out) :: cnt
    logical, intent(in) :: store
    integer(int64), intent(inout) :: cells(:)
    integer(int64), intent(in) :: base
    integer(int64) :: cy, cy0, cy1, cx, cx0, cx1, yb0, yb1, ya, yb, xa, xb
    real(real64) :: ra, rb, slope
    cnt = 0
    cy0 = cell_y(g, y1 - 2); cy1 = cell_y(g, y2 + 2)
    if (y1 /= y2) slope = real(x2 - x1, real64) / real(y2 - y1, real64)
    do cy = cy0, cy1
       if (y1 == y2) then
          xa = min(x1, x2) - 2; xb = max(x1, x2) + 2
       else
          yb0 = g%gy0 + cy*g%s - 2; yb1 = g%gy0 + (cy + 1)*g%s + 2
          ya = max(yb0, y1); yb = min(yb1, y2)
          if (ya > yb) cycle
          ra = real(x1, real64) + real(ya - y1, real64)*slope
          rb = real(x1, real64) + real(yb - y1, real64)*slope
          xa = floor(min(ra, rb), int64) - 2; xb = ceiling(max(ra, rb), int64) + 2
       end if
       cx0 = cell_x(g, xa); cx1 = cell_x(g, xb)
       do cx = cx0, cx1
          cnt = cnt + 1
          if (store) cells(base + cnt) = cy*g%ncx + cx
       end do
    end do
  end subroutine edge_cells

  !> Unique keys of a sorted key array: ukey(1:nu), ustart(1:nu+1) (positions).
  subroutine run_starts(key, n, ukey, ustart, nu)
    integer(int64), intent(in) :: key(:), n
    integer(int64), allocatable, intent(out) :: ukey(:), ustart(:)
    integer(int64), intent(out) :: nu
    integer(int64) :: i
    nu = 0
    do i = 1, n
       if (i == 1) then
          nu = nu + 1
       else if (key(i) /= key(i - 1)) then
          nu = nu + 1
       end if
    end do
    allocate(ukey(max(nu, 1_int64)), ustart(nu + 1))
    nu = 0
    do i = 1, n
       if (i > 1) then
          if (key(i) == key(i - 1)) cycle
       end if
       nu = nu + 1
       ukey(nu) = key(i); ustart(nu) = i
    end do
    ustart(nu + 1) = n + 1
  end subroutine run_starts

  subroutine TrapSnapRound(E, F, st)
    type(FragSet), intent(in) :: E
    type(FragSet), intent(inout) :: F
    type(TrapMergeStats), intent(inout) :: st
    type(CellGrid) :: g
    integer(int64) :: xmin, xmax, ymin, ymax, i, j, nr, nu, nhu, nhot0, nhot, ncross, ninexact
    integer(int64) :: u, p, q, ea, eb, h, cnt, tot, npairs, maxc, wbox, hbox, target
    integer(int64), allocatable :: nreg(:), roff(:), rkey(:), ucell(:), ustart(:)
    integer(int64), allocatable :: hk(:), hpx(:), hpy(:), hck(:), hcell(:), hstart(:)
    integer(int64), allocatable :: hits(:), hoff(:), foff(:)
    integer(int32), allocatable :: rval(:), hval(:), dummy(:)
    integer :: skind
    type(I64Buf), allocatable :: tb(:)
    integer :: nt, tid
    integer(int64) :: xn1, xn2, fx, fy, rx, ry, den, hx, hy, k1, k2, lo, hi, mid, hkey
    type(i128) :: xn, yn
    logical :: ok
    real(real64) :: t0

    t0 = omp_get_wtime()
    nt = omp_get_max_threads()
    if (E%n == 0) then
       call alloc_set(F, 0_int64); return
    end if
    xmin = huge(xmin); ymin = huge(ymin); xmax = -huge(xmax); ymax = -huge(ymax)
    !$omp parallel do reduction(min:xmin,ymin) reduction(max:xmax,ymax)
    do i = 1, E%n
       xmin = min(xmin, E%x1(i), E%x2(i)); xmax = max(xmax, E%x1(i), E%x2(i))
       ymin = min(ymin, E%y1(i)); ymax = max(ymax, E%y2(i))
    end do
    !$omp end parallel do
    wbox = xmax - xmin + 9; hbox = ymax - ymin + 9
    target = max(1_int64, E%n)
    g%s = max(16_int64, int(ceiling(sqrt(real(wbox, real64)*real(hbox, real64)/real(target, real64))), int64))
    do
       g%ncx = wbox / g%s + 1; g%ncy = hbox / g%s + 1
       if (real(g%ncx, real64)*real(g%ncy, real64) < 2.0e9_real64) exit
       g%s = g%s * 2
    end do
    g%gx0 = xmin - 4; g%gy0 = ymin - 4

    ! ---- registrations, sorted by cell
    allocate(nreg(E%n), roff(E%n + 1))
    allocate(rkey(1))
    !$omp parallel do schedule(dynamic, 4096) private(cnt)
    do i = 1, E%n
       call edge_cells(g, E%x1(i), E%y1(i), E%x2(i), E%y2(i), cnt, .false., rkey, 0_int64)
       nreg(i) = cnt
    end do
    !$omp end parallel do
    roff(1) = 0
    do i = 1, E%n
       roff(i + 1) = roff(i) + nreg(i)
    end do
    nr = roff(E%n + 1)
    deallocate(rkey)
    allocate(rkey(nr), rval(nr))
    !$omp parallel do schedule(dynamic, 4096) private(cnt, j)
    do i = 1, E%n
       call edge_cells(g, E%x1(i), E%y1(i), E%x2(i), E%y2(i), cnt, .true., rkey, roff(i))
       do j = roff(i) + 1, roff(i) + cnt
          rval(j) = int(i, int32)
       end do
    end do
    !$omp end parallel do
    call CompactSortPairs(rkey, rval, nr)
    call run_starts(rkey, nr, ucell, ustart, nu)
    deallocate(rkey, nreg)
    st%registrations = nr; st%cells = nu
    maxc = 0
    do u = 1, nu
       maxc = max(maxc, ustart(u + 1) - ustart(u))
    end do
    st%max_cell = maxc
    st%t_grid = omp_get_wtime() - t0; t0 = omp_get_wtime()

    ! ---- proper crossings: each found in the cell containing the exact crossing point
    allocate(tb(0:nt - 1))
    ncross = 0; ninexact = 0; npairs = 0
    !$omp parallel default(shared) private(tid, u, p, q, ea, eb, skind, xn, yn, den, ok, fx, fy, rx, ry, hx, hy)
    tid = omp_get_thread_num()
    !$omp do schedule(dynamic, 64) reduction(+:ncross, ninexact, npairs)
    do u = 1, nu
       do p = ustart(u), ustart(u + 1) - 2
          ea = rval(p)
          do q = p + 1, ustart(u + 1) - 1
             eb = rval(q)
             if (max(E%x1(ea), E%x2(ea)) < min(E%x1(eb), E%x2(eb))) cycle
             if (min(E%x1(ea), E%x2(ea)) > max(E%x1(eb), E%x2(eb))) cycle
             if (E%y2(ea) < E%y1(eb) .or. E%y1(ea) > E%y2(eb)) cycle
             npairs = npairs + 1
             skind = seg_intersect_kind(E%x1(ea), E%y1(ea), E%x2(ea), E%y2(ea), &
                                       E%x1(eb), E%y1(eb), E%x2(eb), E%y2(eb))
             if (skind /= K_SEG_PROPER) cycle
             call seg_intersection_rational(E%x1(ea), E%y1(ea), E%x2(ea), E%y2(ea), &
                                            E%x1(eb), E%y1(eb), E%x2(eb), E%y2(eb), xn, yn, den, ok)
             if (.not. ok) cycle
             call i128_divmod(xn, den, fx, rx)
             call i128_divmod(yn, den, fy, ry)
             if (cell_y(g, fy)*g%ncx + cell_x(g, fx) /= ucell(u)) cycle
             ncross = ncross + 1
             if (rx /= 0 .or. ry /= 0) ninexact = ninexact + 1
             hx = fx; hy = fy
             if (rx >= den - rx) hx = hx + 1
             if (ry >= den - ry) hy = hy + 1
             call buf_push(tb(tid), (hy + KB)*K31 + (hx + KB))
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel
    st%crossings = ncross; st%crossings_inexact = ninexact; st%pair_tests = npairs
    st%t_cross = omp_get_wtime() - t0; t0 = omp_get_wtime()

    ! ---- hot pixels: all vertices + rounded crossings, unique
    nhot0 = 2*E%n
    do tid = 0, nt - 1
       nhot0 = nhot0 + tb(tid)%n
    end do
    allocate(hk(nhot0), hval(nhot0))
    !$omp parallel do
    do i = 1, E%n
       hk(2*i - 1) = (E%y1(i) + KB)*K31 + (E%x1(i) + KB)
       hk(2*i) = (E%y2(i) + KB)*K31 + (E%x2(i) + KB)
       hval(2*i - 1) = 0; hval(2*i) = 0
    end do
    !$omp end parallel do
    j = 2*E%n
    do tid = 0, nt - 1
       if (tb(tid)%n > 0) then
          hk(j + 1:j + tb(tid)%n) = tb(tid)%v(1:tb(tid)%n)
          hval(j + 1:j + tb(tid)%n) = 0
          j = j + tb(tid)%n
          deallocate(tb(tid)%v)
       end if
       tb(tid)%n = 0
    end do
    call CompactSortPairs(hk, hval, nhot0)
    nhot = 0
    do i = 1, nhot0
       if (i > 1) then
          if (hk(i) == hk(i - 1)) cycle
       end if
       nhot = nhot + 1
       hk(nhot) = hk(i)
    end do
    allocate(hpx(nhot), hpy(nhot), hck(nhot))
    deallocate(hval); allocate(hval(nhot))
    !$omp parallel do
    do i = 1, nhot
       hpy(i) = hk(i) / K31 - KB
       hpx(i) = mod(hk(i), K31) - KB
       hck(i) = cell_y(g, hpy(i))*g%ncx + cell_x(g, hpx(i))
       hval(i) = int(i, int32)
    end do
    !$omp end parallel do
    deallocate(hk)
    call CompactSortPairs(hck, hval, nhot)
    call run_starts(hck, nhot, hcell, hstart, nhu)
    st%hot_pixels = nhot
    st%t_hot = omp_get_wtime() - t0; t0 = omp_get_wtime()

    ! ---- hits: (edge, hot pixel) for pixels met by the edge other than its own endpoints
    !$omp parallel default(shared) private(tid, u, lo, hi, mid, p, q, ea, h, k1, k2)
    tid = omp_get_thread_num()
    !$omp do schedule(dynamic, 64)
    do u = 1, nu
       lo = 1; hi = nhu
       do while (lo < hi)
          mid = (lo + hi) / 2
          if (hcell(mid) < ucell(u)) then
             lo = mid + 1
          else
             hi = mid
          end if
       end do
       if (nhu < 1) cycle
       if (hcell(lo) /= ucell(u)) cycle
       do q = hstart(lo), hstart(lo + 1) - 1
          h = hval(q)
          do p = ustart(u), ustart(u + 1) - 1
             ea = rval(p)
             if (hpx(h) < min(E%x1(ea), E%x2(ea)) - 1 .or. hpx(h) > max(E%x1(ea), E%x2(ea)) + 1) cycle
             if (hpy(h) < E%y1(ea) - 1 .or. hpy(h) > E%y2(ea) + 1) cycle
             if (hpx(h) == E%x1(ea) .and. hpy(h) == E%y1(ea)) cycle
             if (hpx(h) == E%x2(ea) .and. hpy(h) == E%y2(ea)) cycle
             if (.not. pixel_hits_segment(hpx(h), hpy(h), E%x1(ea), E%y1(ea), E%x2(ea), E%y2(ea))) cycle
             call buf_push(tb(tid), ea*K31 + h)
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel
    deallocate(rval, ucell, ustart, hval, hcell, hstart, hck)
    tot = 0
    do tid = 0, nt - 1
       tot = tot + tb(tid)%n
    end do
    allocate(hits(max(tot, 1_int64)), dummy(max(tot, 1_int64)))
    j = 0
    do tid = 0, nt - 1
       if (tb(tid)%n > 0) then
          hits(j + 1:j + tb(tid)%n) = tb(tid)%v(1:tb(tid)%n)
          j = j + tb(tid)%n
          deallocate(tb(tid)%v)
       end if
    end do
    deallocate(tb)
    if (tot > 0) dummy(1:tot) = 0
    if (tot > 1) call CompactSortPairs(hits, dummy, tot)
    st%hits = tot
    st%t_hits = omp_get_wtime() - t0; t0 = omp_get_wtime()

    ! ---- fragments: per edge, through the hit pixel centres in order along the edge
    allocate(hoff(E%n + 1), foff(E%n + 1))
    hoff = 0
    do i = 1, tot
       ea = hits(i) / K31
       hoff(ea + 1) = hoff(ea + 1) + 1
    end do
    hoff(1) = 0; foff(1) = 0
    do i = 1, E%n
       foff(i + 1) = foff(i) + hoff(i + 1) + 1
       hoff(i + 1) = hoff(i) + hoff(i + 1)
    end do
    block
      type(FragSet) :: G
      integer(int64) :: nf, a, b, m, c, r, npt
      integer(int64), allocatable :: ptx(:), pty(:)
      call alloc_set(G, foff(E%n + 1))
      !$omp parallel default(shared) private(i, a, b, m, c, r, npt, ptx, pty, h, j, nf)
      allocate(ptx(16), pty(16))
      !$omp do schedule(dynamic, 4096)
      do i = 1, E%n
         a = hoff(i) + 1; b = hoff(i + 1)
         npt = b - a + 3
         if (npt > size(ptx, kind=int64)) then
            deallocate(ptx, pty); allocate(ptx(2*npt), pty(2*npt))
         end if
         ptx(1) = E%x1(i); pty(1) = E%y1(i)
         m = 1
         ! hot pixels sorted by (y, x) ascending; along the edge y is non-decreasing and x is
         ! monotone in the direction of dx, so reverse runs of equal y when dx < 0
         c = a
         do while (c <= b)
            r = c
            do while (r < b)
               if (hpy(mod(hits(r + 1), K31)) /= hpy(mod(hits(c), K31))) exit
               r = r + 1
            end do
            if (E%x2(i) < E%x1(i)) then
               do j = r, c, -1
                  h = mod(hits(j), K31); m = m + 1; ptx(m) = hpx(h); pty(m) = hpy(h)
               end do
            else
               do j = c, r
                  h = mod(hits(j), K31); m = m + 1; ptx(m) = hpx(h); pty(m) = hpy(h)
               end do
            end if
            c = r + 1
         end do
         m = m + 1; ptx(m) = E%x2(i); pty(m) = E%y2(i)
         do j = 1, m - 1
            nf = foff(i) + j
            if (pty(j) == pty(j + 1)) then
               G%x1(nf) = min(ptx(j), ptx(j + 1)); G%x2(nf) = max(ptx(j), ptx(j + 1))
               G%y1(nf) = pty(j); G%y2(nf) = pty(j); G%w(nf) = 0
            else
               G%x1(nf) = ptx(j); G%y1(nf) = pty(j); G%x2(nf) = ptx(j + 1); G%y2(nf) = pty(j + 1)
               G%w(nf) = E%w(i)
               if (pty(j + 1) < pty(j)) G%w(nf) = 0      ! cannot happen (monotone); dropped
            end if
         end do
      end do
      !$omp end do
      deallocate(ptx, pty)
      !$omp end parallel
      deallocate(hits, dummy, hoff, foff, hpx, hpy)
      st%fragments = G%n
      call merge_identical(G, F, st)
      call free_set(G)
    end block
    st%t_frag = omp_get_wtime() - t0
  end subroutine TrapSnapRound

  pure integer(int64) function mix64(a) result(z)
    integer(int64), intent(in) :: a
    z = a + (-7046029254386353131_int64)
    z = ieor(z, shiftr(z, 30)) * (-4658895280553007687_int64)
    z = ieor(z, shiftr(z, 27)) * (-7723592293110705685_int64)
    z = ieor(z, shiftr(z, 31))
  end function mix64

  !> Identical fragments (after snap rounding overlapping fragments are identical) become one,
  !> with the windings added; non-horizontal fragments with total winding 0 are dropped,
  !> horizontal duplicates are dropped. Survivors keep their original relative order.
  subroutine merge_identical(G, F, st)
    type(FragSet), intent(inout) :: G
    type(FragSet), intent(inout) :: F
    type(TrapMergeStats), intent(inout) :: st
    integer(int64), allocatable :: key(:), ukey(:), ustart(:)
    integer(int32), allocatable :: idx(:)
    logical, allocatable :: keep(:)
    integer(int64) :: i, nu, u, p, q, a, b, m, best
    integer(int64), allocatable :: pos(:)
    allocate(key(G%n), idx(G%n), keep(G%n))
    !$omp parallel do
    do i = 1, G%n
       key(i) = shiftr(mix64(mix64(mix64(mix64(G%x1(i)) + G%y1(i)) + G%x2(i)) + G%y2(i)), 1)
       idx(i) = int(i, int32)
       keep(i) = .true.
    end do
    !$omp end parallel do
    call CompactSortPairs(key, idx, G%n)
    call run_starts(key, G%n, ukey, ustart, nu)
    !$omp parallel do schedule(dynamic, 4096) private(p, q, a, b, best)
    do u = 1, nu
       if (ustart(u + 1) - ustart(u) < 2) cycle
       do p = ustart(u), ustart(u + 1) - 1
          a = idx(p)
          if (.not. keep(a)) cycle
          ! survivor = lowest index among the identical ones
          best = a
          do q = ustart(u), ustart(u + 1) - 1
             b = idx(q)
             if (b == a .or. .not. keep(b)) cycle
             if (G%x1(a) == G%x1(b) .and. G%y1(a) == G%y1(b) .and. G%x2(a) == G%x2(b) &
                 .and. G%y2(a) == G%y2(b)) best = min(best, b)
          end do
          if (best /= a) cycle
          do q = ustart(u), ustart(u + 1) - 1
             b = idx(q)
             if (b == a .or. .not. keep(b)) cycle
             if (G%x1(a) == G%x1(b) .and. G%y1(a) == G%y1(b) .and. G%x2(a) == G%x2(b) &
                 .and. G%y2(a) == G%y2(b)) then
                G%w(a) = G%w(a) + G%w(b)
                keep(b) = .false.
             end if
          end do
       end do
    end do
    !$omp end parallel do
    m = 0
    allocate(pos(G%n))
    do i = 1, G%n
       if (keep(i) .and. G%y1(i) /= G%y2(i) .and. G%w(i) == 0) keep(i) = .false.
       if (keep(i)) m = m + 1
       pos(i) = m
    end do
    call alloc_set(F, m)
    !$omp parallel do
    do i = 1, G%n
       if (keep(i)) then
          F%x1(pos(i)) = G%x1(i); F%y1(pos(i)) = G%y1(i)
          F%x2(pos(i)) = G%x2(i); F%y2(pos(i)) = G%y2(i); F%w(pos(i)) = G%w(i)
       end if
    end do
    !$omp end parallel do
    st%fragments_merged = m
  end subroutine merge_identical

  ! ======================================================================= 3. sweep

  !> Serial LSD radix sort of non-negative keys (< 2**63) carrying idx, reusing caller workspace.
  subroutine radix_sort_pairs_ws(key, idx, n, tk, ti)
    integer(int64), intent(inout) :: key(:)
    integer(int64), intent(inout) :: idx(:)
    integer(int64), intent(in) :: n
    integer(int64), allocatable, intent(inout) :: tk(:), ti(:)
    integer(int64) :: cnt(0:2047), i, j, d, s, kk, ii
    integer :: pass
    if (n < 2) return
    if (n < 48) then
       do i = 2, n
          kk = key(i); ii = idx(i); j = i - 1
          do while (j >= 1)
             if (key(j) <= kk) exit
             key(j + 1) = key(j); idx(j + 1) = idx(j); j = j - 1
          end do
          key(j + 1) = kk; idx(j + 1) = ii
       end do
       return
    end if
    call arena_ensure_i64(tk, n); call arena_ensure_i64(ti, n)
    do pass = 0, 5
       cnt = 0
       do i = 1, n
          d = iand(shiftr(key(i), 11*pass), 2047_int64)
          cnt(d) = cnt(d) + 1
       end do
       if (maxval(cnt) == n) cycle
       s = 0
       do d = 0, 2047
          j = cnt(d); cnt(d) = s; s = s + j
       end do
       do i = 1, n
          d = iand(shiftr(key(i), 11*pass), 2047_int64)
          cnt(d) = cnt(d) + 1
          tk(cnt(d)) = key(i); ti(cnt(d)) = idx(i)
       end do
       key(1:n) = tk(1:n); idx(1:n) = ti(1:n)
    end do
  end subroutine radix_sort_pairs_ws

  subroutine radix_sort_pairs(key, idx, n)
    integer(int64), intent(inout) :: key(:)
    integer(int64), intent(inout) :: idx(:)
    integer(int64), intent(in) :: n
    integer(int64), allocatable :: tk(:), ti(:)
    if (n < 2) return
    call radix_sort_pairs_ws(key, idx, n, tk, ti)
  end subroutine radix_sort_pairs

  !> op (default TOP_NONZERO) selects the region: see TrapInside.
  subroutine TrapSweepUnion(F, nbands_req, traps, ntrap, st, op)
    type(FragSet), intent(in) :: F
    integer, intent(in) :: nbands_req
    type(Trapezoid), allocatable, intent(out) :: traps(:)
    integer(int64), intent(out) :: ntrap
    type(TrapMergeStats), intent(inout) :: st
    integer(int32), intent(in), optional :: op
    integer(int64), parameter :: NBIN = 65536
    integer(int64) :: ymin, ymax, i, nnh, tot, acc, b, nb, b0, b1, j, span
    integer(int64), allocatable :: hist(:), ybnd(:), bcnt(:), boff(:), bids(:), bpos(:)
    integer(int64), allocatable :: ry1(:), ry2(:), rl(:), rr(:)
    integer :: nbw, nt, tid
    integer(int32) :: op_use
    real(real64) :: t0
    integer(int64) :: atot(8)

    t0 = omp_get_wtime()
    ntrap = 0
    nnh = 0
    ymin = huge(ymin); ymax = -huge(ymax)
    do i = 1, F%n
       if (F%y1(i) == F%y2(i)) cycle
       nnh = nnh + 1
       ymin = min(ymin, F%y1(i)); ymax = max(ymax, F%y2(i))
    end do
    if (nnh == 0) then
       allocate(traps(0)); st%t_sweep = omp_get_wtime() - t0; return
    end if
    nbw = nbands_req
    if (nbw <= 0) nbw = 8*omp_get_max_threads()
    span = ymax - ymin
    ! band rows from an event histogram (equal event counts per band)
    allocate(hist(0:NBIN - 1))
    hist = 0
    do i = 1, F%n
       if (F%y1(i) == F%y2(i)) cycle
       j = ((F%y1(i) - ymin)*NBIN) / (span + 1); hist(j) = hist(j) + 1
       j = ((F%y2(i) - ymin)*NBIN) / (span + 1); hist(j) = hist(j) + 1
    end do
    allocate(ybnd(0:nbw))
    nb = 0; ybnd(0) = ymin
    acc = 0; tot = 2*nnh
    do j = 0, NBIN - 1
       acc = acc + hist(j)
       if (acc*nbw >= (nb + 1)*tot .and. nb + 1 < nbw) then
          i = ymin + ((j + 1)*(span + 1) + NBIN - 1) / NBIN        ! first row of the next bin
          if (i > ybnd(nb) .and. i < ymax) then
             nb = nb + 1; ybnd(nb) = i
          end if
       end if
    end do
    nb = nb + 1; ybnd(nb) = ymax
    st%bands = nb
    ! fragments per band
    allocate(bcnt(0:nb - 1), boff(0:nb))
    bcnt = 0
    do i = 1, F%n
       if (F%y1(i) == F%y2(i)) then
          b0 = band_of(ybnd, nb, F%y1(i))
          if (F%y1(i) > ybnd(b0) .and. F%y1(i) < ybnd(b0 + 1)) bcnt(b0) = bcnt(b0) + 1
       else
          b0 = band_of(ybnd, nb, F%y1(i)); b1 = band_of(ybnd, nb, F%y2(i) - 1)
          bcnt(b0:b1) = bcnt(b0:b1) + 1
       end if
    end do
    boff(0) = 0
    do b = 0, nb - 1
       boff(b + 1) = boff(b) + bcnt(b)
    end do
    allocate(bids(max(boff(nb), 1_int64)), bpos(0:nb - 1))
    bpos(0:nb - 1) = boff(0:nb - 1)
    do i = 1, F%n
       if (F%y1(i) == F%y2(i)) then
          b0 = band_of(ybnd, nb, F%y1(i))
          if (F%y1(i) > ybnd(b0) .and. F%y1(i) < ybnd(b0 + 1)) then
             bpos(b0) = bpos(b0) + 1; bids(bpos(b0)) = i
          end if
       else
          b0 = band_of(ybnd, nb, F%y1(i)); b1 = band_of(ybnd, nb, F%y2(i) - 1)
          do b = b0, b1
             bpos(b) = bpos(b) + 1; bids(bpos(b)) = i
          end do
       end if
    end do
    nt = max(1, omp_get_max_threads())
    if (.not. allocated(g_sw)) then
       allocate(g_sw(0:nt - 1))
    else if (size(g_sw) < nt) then
       deallocate(g_sw)
       allocate(g_sw(0:nt - 1))
    end if
    op_use = TOP_NONZERO
    if (present(op)) op_use = op
    do tid = 0, nt - 1
       g_sw(tid)%op = op_use
       g_sw(tid)%nout = 0
       g_sw(tid)%anomalies = 0
       g_sw(tid)%atype = 0
       g_sw(tid)%rng = -1831433054_int32
    end do
    !$omp parallel do schedule(dynamic, 1) private(tid)
    do b = 0, nb - 1
       tid = omp_get_thread_num()
       call sweep_band(F, bids(boff(b) + 1:boff(b + 1)), bcnt(b), ybnd(b), ybnd(b + 1), g_sw(tid))
    end do
    !$omp end parallel do
    tot = 0
    atot = 0
    do tid = 0, nt - 1
       tot = tot + g_sw(tid)%nout
       st%anomalies = st%anomalies + g_sw(tid)%anomalies
       atot = atot + g_sw(tid)%atype
    end do
    allocate(ry1(max(tot, 1_int64)), ry2(max(tot, 1_int64)), rl(max(tot, 1_int64)), rr(max(tot, 1_int64)))
    j = 0
    do tid = 0, nt - 1
       if (g_sw(tid)%nout > 0) then
          ry1(j + 1:j + g_sw(tid)%nout) = g_sw(tid)%oy1(1:g_sw(tid)%nout)
          ry2(j + 1:j + g_sw(tid)%nout) = g_sw(tid)%oy2(1:g_sw(tid)%nout)
          rl(j + 1:j + g_sw(tid)%nout) = g_sw(tid)%outl(1:g_sw(tid)%nout)
          rr(j + 1:j + g_sw(tid)%nout) = g_sw(tid)%outr(1:g_sw(tid)%nout)
          j = j + g_sw(tid)%nout
       end if
    end do
    if (any(atot /= 0)) write(*, '(a,8(1x,i0))') 'TrapSweepUnion anomaly types:', atot
    st%traps_raw = tot
    st%t_sweep = omp_get_wtime() - t0
    t0 = omp_get_wtime()
    call stitch(F, ry1, ry2, rl, rr, tot, traps, ntrap, st)
    st%traps = ntrap
    st%t_stitch = omp_get_wtime() - t0
  end subroutine TrapSweepUnion

  pure integer(int64) function band_of(ybnd, nb, y) result(b)
    integer(int64), intent(in) :: ybnd(0:), nb, y
    integer(int64) :: lo, hi, mid
    lo = 0; hi = nb - 1
    do while (lo < hi)
       mid = (lo + hi + 1) / 2
       if (ybnd(mid) <= y) then
          lo = mid
       else
          hi = mid - 1
       end if
    end do
    b = lo
  end function band_of

  ! ---------------------------------------------------------------- treap

  subroutine tr_pull(S, n)
    type(Sweep), intent(inout) :: S
    integer(int32), intent(in) :: n
    S%sw(n) = S%w(n) + S%sw(S%lc(n)) + S%sw(S%rc(n))
    S%slb(n) = S%flb(n) + S%slb(S%lc(n)) + S%slb(S%rc(n))
    S%srb(n) = S%frb(n) + S%srb(S%lc(n)) + S%srb(S%rc(n))
  end subroutine tr_pull

  subroutine tr_pull_up(S, n0)
    type(Sweep), intent(inout) :: S
    integer(int32), intent(in) :: n0
    integer(int32) :: n
    n = n0
    do while (n /= 0)
       call tr_pull(S, n)
       n = S%par(n)
    end do
  end subroutine tr_pull_up

  !> Rotate x above its parent.
  subroutine tr_rotate_up(S, x)
    type(Sweep), intent(inout) :: S
    integer(int32), intent(in) :: x
    integer(int32) :: p, g, b
    p = S%par(x); g = S%par(p)
    if (S%lc(p) == x) then
       b = S%rc(x); S%lc(p) = b; S%rc(x) = p
    else
       b = S%lc(x); S%rc(p) = b; S%lc(x) = p
    end if
    if (b /= 0) S%par(b) = p
    S%par(p) = x; S%par(x) = g
    if (g == 0) then
       S%root = x
    else if (S%lc(g) == p) then
       S%lc(g) = x
    else
       S%rc(g) = x
    end if
    call tr_pull(S, p); call tr_pull(S, x)
  end subroutine tr_rotate_up

  !> Sign of x_a(y) - x_b(y) at the current row (exact).
  integer(int32) function x_cmp(S, a, b) result(c)
    type(Sweep), intent(in) :: S
    integer(int32), intent(in) :: a, b
    real(real64) :: xa, xb
    type(i128) :: t
    xa = real(S%x1(a), real64) + real(S%y - S%y1(a), real64)*real(S%dx(a), real64)/real(S%dy(a), real64)
    xb = real(S%x1(b), real64) + real(S%y - S%y1(b), real64)*real(S%dx(b), real64)/real(S%dy(b), real64)
    if (xa < xb - 1.0e-3_real64) then
       c = -1; return
    else if (xa > xb + 1.0e-3_real64) then
       c = 1; return
    end if
    t = i128_add(i128_mul64((S%x1(a) - S%x1(b))*S%dy(a), S%dy(b)), &
                 i128_mul64((S%y - S%y1(a))*S%dx(a), S%dy(b)))
    t = i128_sub(t, i128_mul64((S%y - S%y1(b))*S%dx(b), S%dy(a)))
    c = i128_sign(t)
  end function x_cmp

  !> Sign of x_n(y) - X at the current row (exact, int64 under I0).
  pure integer(int32) function x_cmp_int(S, n, X) result(c)
    type(Sweep), intent(in) :: S
    integer(int32), intent(in) :: n
    integer(int64), intent(in) :: X
    integer(int64) :: v
    v = (S%x1(n) - X)*S%dy(n) + (S%y - S%y1(n))*S%dx(n)
    c = 0
    if (v < 0) c = -1
    if (v > 0) c = 1
  end function x_cmp_int

  !> a left of b just above the current row.
  logical function tr_less(S, a, b) result(lt)
    type(Sweep), intent(inout) :: S
    integer(int32), intent(in) :: a, b
    integer(int32) :: c
    integer(int64) :: sa, sb
    c = x_cmp(S, a, b)
    if (c /= 0) then
       lt = c < 0; return
    end if
    sa = S%dx(a)*S%dy(b); sb = S%dx(b)*S%dy(a)
    if (sa /= sb) then
       lt = sa < sb; return
    end if
    S%anomalies = S%anomalies + 1; S%atype(1) = S%atype(1) + 1
    lt = S%gid(a) < S%gid(b)
  end function tr_less

  integer(int32) function next_pri(S) result(p)
    type(Sweep), intent(inout) :: S
    S%rng = ieor(S%rng, shiftl(S%rng, 13))
    S%rng = ieor(S%rng, shiftr(S%rng, 17))
    S%rng = ieor(S%rng, shiftl(S%rng, 5))
    p = S%rng
  end function next_pri

  subroutine tr_insert(S, e)
    type(Sweep), intent(inout) :: S
    integer(int32), intent(in) :: e
    integer(int32) :: cur
    S%lc(e) = 0; S%rc(e) = 0; S%par(e) = 0; S%flb(e) = 0; S%frb(e) = 0
    S%pri(e) = next_pri(S)
    call tr_pull(S, e)
    if (S%root == 0) then
       S%root = e; return
    end if
    cur = S%root
    do
       if (tr_less(S, e, cur)) then
          if (S%lc(cur) == 0) then
             S%lc(cur) = e; exit
          end if
          cur = S%lc(cur)
       else
          if (S%rc(cur) == 0) then
             S%rc(cur) = e; exit
          end if
          cur = S%rc(cur)
       end if
    end do
    S%par(e) = cur
    do while (S%par(e) /= 0)
       if (S%pri(e) <= S%pri(S%par(e))) exit
       call tr_rotate_up(S, e)
    end do
    call tr_pull_up(S, S%par(e))
  end subroutine tr_insert

  subroutine tr_delete(S, e)
    type(Sweep), intent(inout) :: S
    integer(int32), intent(in) :: e
    integer(int32) :: c, p
    do while (S%lc(e) /= 0 .or. S%rc(e) /= 0)
       if (S%lc(e) == 0) then
          c = S%rc(e)
       else if (S%rc(e) == 0) then
          c = S%lc(e)
       else if (S%pri(S%lc(e)) > S%pri(S%rc(e))) then
          c = S%lc(e)
       else
          c = S%rc(e)
       end if
       call tr_rotate_up(S, c)
    end do
    p = S%par(e)
    if (p == 0) then
       S%root = 0
    else if (S%lc(p) == e) then
       S%lc(p) = 0
    else
       S%rc(p) = 0
    end if
    S%par(e) = 0
    call tr_pull_up(S, p)
  end subroutine tr_delete

  integer(int32) function tr_first(S) result(n)
    type(Sweep), intent(in) :: S
    n = S%root
    if (n == 0) return
    do while (S%lc(n) /= 0)
       n = S%lc(n)
    end do
  end function tr_first

  integer(int32) function tr_succ(S, n0) result(n)
    type(Sweep), intent(in) :: S
    integer(int32), intent(in) :: n0
    integer(int32) :: p
    n = n0
    if (S%rc(n) /= 0) then
       n = S%rc(n)
       do while (S%lc(n) /= 0)
          n = S%lc(n)
       end do
       return
    end if
    p = S%par(n)
    do while (p /= 0)
       if (S%lc(p) == n) exit
       n = p; p = S%par(n)
    end do
    n = p
  end function tr_succ

  !> Last node with x(y) < X (0 if none).
  integer(int32) function tr_last_lt(S, X) result(cand)
    type(Sweep), intent(in) :: S
    integer(int64), intent(in) :: X
    integer(int32) :: n
    cand = 0; n = S%root
    do while (n /= 0)
       if (x_cmp_int(S, n, X) < 0) then
          cand = n; n = S%rc(n)
       else
          n = S%lc(n)
       end if
    end do
  end function tr_last_lt

  !> First node with x(y) > X (0 if none).
  integer(int32) function tr_first_gt(S, X) result(cand)
    type(Sweep), intent(in) :: S
    integer(int64), intent(in) :: X
    integer(int32) :: n
    cand = 0; n = S%root
    do while (n /= 0)
       if (x_cmp_int(S, n, X) > 0) then
          cand = n; n = S%lc(n)
       else
          n = S%rc(n)
       end if
    end do
  end function tr_first_gt

  !> Sum of w over all nodes up to and including n.
  integer(int64) function tr_prefix_w(S, n0) result(tot)
    type(Sweep), intent(in) :: S
    integer(int32), intent(in) :: n0
    integer(int32) :: n, p
    n = n0
    tot = S%sw(S%lc(n)) + S%w(n)
    p = S%par(n)
    do while (p /= 0)
       if (S%rc(p) == n) tot = tot + S%sw(S%lc(p)) + S%w(p)
       n = p; p = S%par(n)
    end do
  end function tr_prefix_w

  !> Last node at or before n with the left-boundary flag (0 if none).
  integer(int32) function tr_pred_lb(S, n0) result(r)
    type(Sweep), intent(in) :: S
    integer(int32), intent(in) :: n0
    integer(int32) :: n, p, t
    r = 0
    n = n0
    if (S%flb(n) /= 0) then
       r = n; return
    end if
    t = S%lc(n)
    if (S%slb(t) > 0) then
       r = max_lb(S, t); return
    end if
    p = S%par(n)
    do while (p /= 0)
       if (S%rc(p) == n) then
          if (S%flb(p) /= 0) then
             r = p; return
          end if
          if (S%slb(S%lc(p)) > 0) then
             r = max_lb(S, S%lc(p)); return
          end if
       end if
       n = p; p = S%par(n)
    end do
  end function tr_pred_lb

  integer(int32) function max_lb(S, t0) result(t)
    type(Sweep), intent(in) :: S
    integer(int32), intent(in) :: t0
    t = t0
    do
       if (S%slb(S%rc(t)) > 0) then
          t = S%rc(t)
       else if (S%flb(t) /= 0) then
          return
       else
          t = S%lc(t)
       end if
    end do
  end function max_lb

  !> First node at or after n with the right-boundary flag (0 if none).
  integer(int32) function tr_succ_rb(S, n0) result(r)
    type(Sweep), intent(in) :: S
    integer(int32), intent(in) :: n0
    integer(int32) :: n, p, t
    r = 0
    n = n0
    if (S%frb(n) /= 0) then
       r = n; return
    end if
    t = S%rc(n)
    if (S%srb(t) > 0) then
       r = min_rb(S, t); return
    end if
    p = S%par(n)
    do while (p /= 0)
       if (S%lc(p) == n) then
          if (S%frb(p) /= 0) then
             r = p; return
          end if
          if (S%srb(S%rc(p)) > 0) then
             r = min_rb(S, S%rc(p)); return
          end if
       end if
       n = p; p = S%par(n)
    end do
  end function tr_succ_rb

  integer(int32) function min_rb(S, t0) result(t)
    type(Sweep), intent(in) :: S
    integer(int32), intent(in) :: t0
    t = t0
    do
       if (S%srb(S%lc(t)) > 0) then
          t = S%lc(t)
       else if (S%frb(t) /= 0) then
          return
       else
          t = S%rc(t)
       end if
    end do
  end function min_rb

  subroutine tr_set_flags(S, n, lb, rb)
    type(Sweep), intent(inout) :: S
    integer(int32), intent(in) :: n
    integer, intent(in) :: lb, rb
    if (lb >= 0) S%flb(n) = int(lb, int8)
    if (rb >= 0) S%frb(n) = int(rb, int8)
    call tr_pull_up(S, n)
  end subroutine tr_set_flags

  ! ---------------------------------------------------------------- band sweep

  logical function same_line(S, a, b)
    type(Sweep), intent(in) :: S
    integer(int32), intent(in) :: a, b
    if (a == b) then
       same_line = .true.; return
    end if
    same_line = S%dx(a)*S%dy(b) == S%dx(b)*S%dy(a) .and. &
                (S%x1(b) - S%x1(a))*S%dy(a) == (S%y1(b) - S%y1(a))*S%dx(a)
  end function same_line

  subroutine emit(S, ya, yb, l, r)
    type(Sweep), intent(inout) :: S
    integer(int64), intent(in) :: ya, yb
    integer(int32), intent(in) :: l, r
    integer(int64), allocatable :: t(:)
    integer(int64) :: m
    if (yb <= ya) then
       if (yb < ya) then
          S%anomalies = S%anomalies + 1; S%atype(2) = S%atype(2) + 1
       end if
       return
    end if
    if (S%nout >= size(S%oy1, kind=int64)) then
       m = 2*size(S%oy1, kind=int64)
       allocate(t(m)); t(1:S%nout) = S%oy1(1:S%nout); call move_alloc(t, S%oy1)
       allocate(t(m)); t(1:S%nout) = S%oy2(1:S%nout); call move_alloc(t, S%oy2)
       allocate(t(m)); t(1:S%nout) = S%outl(1:S%nout); call move_alloc(t, S%outl)
       allocate(t(m)); t(1:S%nout) = S%outr(1:S%nout); call move_alloc(t, S%outr)
    end if
    S%nout = S%nout + 1
    S%oy1(S%nout) = ya; S%oy2(S%nout) = yb
    S%outl(S%nout) = S%gid(l); S%outr(S%nout) = S%gid(r)
  end subroutine emit

  !> Capacity n for the interval arrays (contents not kept); S%seq is only created here (it is
  !> grown, keeping its contents, by grow_seq).
  subroutine grow_work(S, n)
    type(Sweep), intent(inout) :: S
    integer, intent(in) :: n
    integer :: m
    if (.not. allocated(S%seq)) allocate(S%seq(max(64, n)))
    if (allocated(S%oL)) then
       if (size(S%oL) >= n .and. size(S%nL) >= n) return
       deallocate(S%oL, S%oR, S%nL, S%nR, S%oLin, S%oRin, S%nLin, S%nRin, S%used, S%oys, S%nys)
    end if
    m = max(64, 2*n)
    allocate(S%oL(m), S%oR(m), S%nL(m), S%nR(m), S%oLin(m), S%oRin(m), S%nLin(m), &
             S%nRin(m), S%used(m), S%oys(m), S%nys(m))
  end subroutine grow_work

  !> Capacity for the new-interval arrays (old-interval arrays are kept).
  subroutine grow_new(S, n)
    type(Sweep), intent(inout) :: S
    integer, intent(in) :: n
    integer :: m
    if (size(S%nL) >= n) return
    m = max(2*n, size(S%oL))
    deallocate(S%nL, S%nR, S%nLin, S%nRin, S%nys, S%used)
    allocate(S%nL(m), S%nR(m), S%nLin(m), S%nRin(m), S%nys(m), S%used(m))
  end subroutine grow_new

  !> Active nodes strictly between L and R (L = 0: from the first) into S%seq.
  subroutine collect_seq(S, L, R)
    type(Sweep), intent(inout) :: S
    integer(int32), intent(in) :: L, R
    integer(int32) :: n
    S%nseq = 0
    if (L == 0) then
       n = tr_first(S)
    else
       n = tr_succ(S, L)
    end if
    do while (n /= 0 .and. n /= R)
       if (S%nseq + 2 > size(S%seq)) call grow_seq(S)
       S%nseq = S%nseq + 1
       S%seq(S%nseq) = n
       n = tr_succ(S, n)
    end do
  end subroutine collect_seq

  subroutine grow_seq(S)
    type(Sweep), intent(inout) :: S
    integer(int32), allocatable :: t(:)
    allocate(t(2*size(S%seq)))
    t(1:S%nseq) = S%seq(1:S%nseq)
    call move_alloc(t, S%seq)
  end subroutine grow_seq

  !> Maximal inside intervals touching the span (L, R): IL/IR are their bounding nodes, ILin /
  !> IRin tell whether the bound lies in S%seq (else it is the left boundary A at or before L,
  !> or the right boundary B at or after R). wend = winding just left of R.
  subroutine build_iv(S, W0, L, R, IL, IR, ILin, IRin, ni, wend)
    type(Sweep), intent(inout) :: S
    integer(int64), intent(in) :: W0
    integer(int32), intent(in) :: L, R
    integer(int32), intent(inout) :: IL(:), IR(:)
    logical, intent(inout) :: ILin(:), IRin(:)
    integer, intent(out) :: ni
    integer(int64), intent(out) :: wend
    integer(int64) :: W
    integer(int32) :: n, curL, A, B
    logical :: prev, now, curLin
    integer :: k
    ni = 0; W = W0; prev = TrapInside(S%op, W0)
    curL = 0; curLin = .false.
    if (prev) then
       A = 0
       if (L /= 0) A = tr_pred_lb(S, L)
       if (A == 0) then
          S%anomalies = S%anomalies + 1; S%atype(3) = S%atype(3) + 1
       end if
       curL = A
    end if
    do k = 1, S%nseq
       n = S%seq(k)
       W = W + S%w(n)
       now = TrapInside(S%op, W)
       if (prev .and. .not. now) then
          ni = ni + 1; IL(ni) = curL; IR(ni) = n; ILin(ni) = curLin; IRin(ni) = .true.
       else if (.not. prev .and. now) then
          curL = n; curLin = .true.
       end if
       prev = now
    end do
    if (prev) then
       B = 0
       if (R /= 0) B = tr_succ_rb(S, R)
       if (B == 0) then
          S%anomalies = S%anomalies + 1; S%atype(4) = S%atype(4) + 1
       end if
       ni = ni + 1; IL(ni) = curL; IR(ni) = B; ILin(ni) = curLin; IRin(ni) = .false.
    end if
    wend = W
  end subroutine build_iv

  subroutine process_cluster(S, xa, xb, ends, e0, e1, starts, s0, s1)
    type(Sweep), intent(inout) :: S
    integer(int64), intent(in) :: xa, xb
    integer(int32), intent(in) :: ends(:), starts(:)
    integer(int64), intent(in) :: e0, e1, s0, s1
    integer(int32) :: L, R, n
    integer(int64) :: W0, wold, wnew
    integer :: nold, nnew, k, j
    integer(int64) :: i
    L = tr_last_lt(S, xa)
    R = tr_first_gt(S, xb)
    W0 = 0
    if (L /= 0) W0 = tr_prefix_w(S, L)
    call collect_seq(S, L, R)
    call grow_work(S, S%nseq + 2)
    call build_iv(S, W0, L, R, S%oL, S%oR, S%oLin, S%oRin, nold, wold)
    do k = 1, nold
       if (S%oL(k) == 0 .or. S%oR(k) == 0) cycle
       S%oys(k) = S%rec_y(S%oL(k))
       if (S%rec_r(S%oL(k)) /= S%oR(k)) then
          S%anomalies = S%anomalies + 1; S%atype(5) = S%atype(5) + 1
       end if
    end do
    do i = e0, e1
       call tr_delete(S, ends(i))
    end do
    do k = 1, S%nseq
       n = S%seq(k)
       if (S%y2(n) /= S%y) call tr_set_flags(S, n, 0, 0)
    end do
    do i = s0, s1
       call tr_insert(S, starts(i))
    end do
    call collect_seq(S, L, R)
    call grow_new(S, S%nseq + 2)
    call build_iv(S, W0, L, R, S%nL, S%nR, S%nLin, S%nRin, nnew, wnew)
    if (wnew /= wold) then
       S%anomalies = S%anomalies + 1; S%atype(6) = S%atype(6) + 1
    end if
    S%used(1:max(nold, 1)) = .false.
    do k = 1, nnew
       S%nys(k) = S%y
       if (S%nL(k) == 0 .or. S%nR(k) == 0) cycle
       do j = 1, nold
          if (S%used(j)) cycle
          if (S%oL(j) == 0 .or. S%oR(j) == 0) cycle
          if (same_line(S, S%oL(j), S%nL(k)) .and. same_line(S, S%oR(j), S%nR(k))) then
             S%used(j) = .true.; S%nys(k) = S%oys(j); exit
          end if
       end do
    end do
    do j = 1, nold
       if (S%used(j) .or. S%oL(j) == 0 .or. S%oR(j) == 0) cycle
       call emit(S, S%oys(j), S%y, S%oL(j), S%oR(j))
    end do
    do k = 1, nnew
       if (S%nL(k) == 0 .or. S%nR(k) == 0) cycle
       S%rec_y(S%nL(k)) = S%nys(k); S%rec_r(S%nL(k)) = S%nR(k)
       if (S%nLin(k)) call tr_set_flags(S, S%nL(k), 1, -1)
       if (S%nRin(k)) call tr_set_flags(S, S%nR(k), -1, 1)
    end do
  end subroutine process_cluster

  !> Sweep one band [ylo, yhi]: fragments ids(1:nid) (global indices into F).
  !> Reuses thread-local persistent arena buffers inside S (zero per-band alloc/free after warmup).
  subroutine sweep_band(F, ids, nid, ylo, yhi, S)
    type(FragSet), intent(in) :: F
    integer(int64), intent(in) :: ids(:), nid, ylo, yhi
    type(Sweep), intent(inout) :: S
    integer(int64) :: i, g, nn, nh, ns, ne, ni, y, ps, pe, ph, ys, ye, yh, pe1, ps1, ph1, xa, xb
    integer(int64) :: nc, c, c2
    integer(int64) :: qe, qs, qh, x0, x1v, which
    integer(int32) :: n, curL, R
    integer(int64) :: W
    logical :: prev, now
    integer(int64), parameter :: BIG = huge(1_int64)

    nn = 0; nh = 0
    do i = 1, nid
       if (F%y1(ids(i)) == F%y2(ids(i))) then
          nh = nh + 1
       else
          nn = nn + 1
       end if
    end do
    S%nn = int(nn, int32)
    call arena_ensure0_i64(S%x1, nn); call arena_ensure0_i64(S%y1, nn)
    call arena_ensure0_i64(S%x2, nn); call arena_ensure0_i64(S%y2, nn)
    call arena_ensure0_i64(S%dx, nn); call arena_ensure0_i64(S%dy, nn)
    call arena_ensure0_i64(S%rec_y, nn); call arena_ensure0_i64(S%w, nn)
    call arena_ensure0_i64(S%sw, nn); call arena_ensure0_i64(S%gid, nn)
    call arena_ensure0_i32(S%lc, nn); call arena_ensure0_i32(S%rc, nn)
    call arena_ensure0_i32(S%par, nn); call arena_ensure0_i32(S%pri, nn)
    call arena_ensure0_i32(S%slb, nn); call arena_ensure0_i32(S%srb, nn)
    call arena_ensure0_i32(S%rec_r, nn)
    call arena_ensure0_i8(S%flb, nn); call arena_ensure0_i8(S%frb, nn)
    S%lc(0:nn) = 0; S%rc(0:nn) = 0; S%par(0:nn) = 0; S%sw(0:nn) = 0
    S%slb(0:nn) = 0; S%srb(0:nn) = 0; S%flb(0:nn) = 0; S%frb(0:nn) = 0; S%w(0:nn) = 0
    S%dy(0) = 1; S%dx(0) = 0; S%x1(0) = 0; S%y1(0) = 0; S%rec_y(0:nn) = 0; S%rec_r(0:nn) = 0
    if (.not. allocated(S%oy1)) then
       allocate(S%oy1(65536), S%oy2(65536), S%outl(65536), S%outr(65536))
    end if
    S%root = 0
    call grow_work(S, 64)
    call arena_ensure_i64(S%sk, max(nn, 1_int64)); call arena_ensure_i64(S%sv, max(nn, 1_int64))
    call arena_ensure_i64(S%ek, max(nn, 1_int64)); call arena_ensure_i64(S%ev, max(nn, 1_int64))
    call arena_ensure_i64(S%hk, max(nh, 1_int64)); call arena_ensure_i64(S%hv, max(nh, 1_int64))
    call arena_ensure_i64(S%iv, max(nn, 1_int64))
    nn = 0; nh = 0; ns = 0; ne = 0; ni = 0
    do i = 1, nid
       g = ids(i)
       if (F%y1(g) == F%y2(g)) then
          nh = nh + 1; S%hk(nh) = (F%y1(g) + KB)*K31 + (F%x1(g) + KB); S%hv(nh) = g
          cycle
       end if
       nn = nn + 1
       S%x1(nn) = F%x1(g); S%y1(nn) = F%y1(g); S%x2(nn) = F%x2(g); S%y2(nn) = F%y2(g)
       S%dx(nn) = F%x2(g) - F%x1(g); S%dy(nn) = F%y2(g) - F%y1(g)
       S%w(nn) = F%w(g); S%gid(nn) = g
       if (F%y1(g) <= ylo) then
          ni = ni + 1; S%iv(ni) = nn
       else
          ns = ns + 1; S%sk(ns) = (F%y1(g) + KB)*K31 + (F%x1(g) + KB); S%sv(ns) = nn
       end if
       if (F%y2(g) < yhi) then
          ne = ne + 1; S%ek(ne) = (F%y2(g) + KB)*K31 + (F%x2(g) + KB); S%ev(ne) = nn
       end if
    end do
    call radix_sort_pairs_ws(S%sk, S%sv, ns, S%stk, S%sti)
    call radix_sort_pairs_ws(S%ek, S%ev, ne, S%stk, S%sti)
    call radix_sort_pairs_ws(S%hk, S%hv, nh, S%stk, S%sti)
    call arena_ensure_i32(S%starts, max(ns, 1_int64)); call arena_ensure_i32(S%ends, max(ne, 1_int64))
    S%starts(1:ns) = int(S%sv(1:ns), int32); S%ends(1:ne) = int(S%ev(1:ne), int32)

    ! ---- initial state at ylo
    S%y = ylo
    do i = 1, ni
       call tr_insert(S, int(S%iv(i), int32))
    end do
    W = 0; prev = .false.; curL = 0
    n = tr_first(S)
    do while (n /= 0)
       W = W + S%w(n); now = TrapInside(S%op, W)
       if (.not. prev .and. now) then
          curL = n
       else if (prev .and. .not. now) then
          call tr_set_flags(S, curL, 1, -1); call tr_set_flags(S, n, -1, 1)
          S%rec_y(curL) = ylo; S%rec_r(curL) = n
       end if
       prev = now
       n = tr_succ(S, n)
    end do
    if (W /= 0) then
       S%anomalies = S%anomalies + 1; S%atype(7) = S%atype(7) + 1
    end if

    ! ---- event rows
    call arena_ensure_i64(S%ca, 64_int64); call arena_ensure_i64(S%cb, 64_int64)
    call arena_ensure_i64(S%ce0, 64_int64); call arena_ensure_i64(S%cs0, 64_int64)
    call arena_ensure_i64(S%ce1, 64_int64); call arena_ensure_i64(S%cs1, 64_int64)
    ps = 1; pe = 1; ph = 1
    do
       ys = BIG; ye = BIG; yh = BIG
       if (ps <= ns) ys = S%sk(ps) / K31 - KB
       if (pe <= ne) ye = S%ek(pe) / K31 - KB
       if (ph <= nh) yh = S%hk(ph) / K31 - KB
       y = min(ys, ye, yh)
       if (y == BIG) exit
       S%y = y
       ps1 = ps; pe1 = pe; ph1 = ph
       do while (ps1 <= ns)
          if (S%sk(ps1) / K31 - KB /= y) exit
          ps1 = ps1 + 1
       end do
       do while (pe1 <= ne)
          if (S%ek(pe1) / K31 - KB /= y) exit
          pe1 = pe1 + 1
       end do
       do while (ph1 <= nh)
          if (S%hk(ph1) / K31 - KB /= y) exit
          ph1 = ph1 + 1
       end do
       ! clusters: x-intervals of end points, start points and horizontal fragments
       nc = 0
       block
         qe = pe; qs = ps; qh = ph
         do while (qe < pe1 .or. qs < ps1 .or. qh < ph1)
            x0 = BIG; which = 0
            if (qe < pe1) then
               if (mod(S%ek(qe), K31) - KB < x0) then
                  x0 = mod(S%ek(qe), K31) - KB; which = 1
               end if
            end if
            if (qs < ps1) then
               if (mod(S%sk(qs), K31) - KB < x0) then
                  x0 = mod(S%sk(qs), K31) - KB; which = 2
               end if
            end if
            if (qh < ph1) then
               if (mod(S%hk(qh), K31) - KB < x0) then
                  x0 = mod(S%hk(qh), K31) - KB; which = 3
               end if
            end if
            x1v = x0
            if (which == 3) x1v = F%x2(S%hv(qh))
            if (nc == 0) then
               call new_cluster()
            else if (x0 > S%cb(nc)) then
               call new_cluster()
            else
               S%cb(nc) = max(S%cb(nc), x1v)
            end if
            select case (which)
            case (1); qe = qe + 1
            case (2); qs = qs + 1
            case default; qh = qh + 1
            end select
         end do
         if (nc > 0) then
            S%ce1(nc) = pe1 - 1; S%cs1(nc) = ps1 - 1
            do c = 1, nc - 1
               S%ce1(c) = S%ce0(c + 1) - 1; S%cs1(c) = S%cs0(c + 1) - 1
            end do
         end if
         c = 1
         do while (c <= nc)
            xa = S%ca(c); xb = S%cb(c); c2 = c
            do
               R = tr_first_gt(S, xb)
               if (R == 0 .or. c2 >= nc) exit
               if (x_cmp_int(S, R, S%ca(c2 + 1)) < 0) exit
               c2 = c2 + 1; xb = S%cb(c2)
            end do
            call process_cluster(S, xa, xb, S%ends, S%ce0(c), S%ce1(c2), S%starts, S%cs0(c), S%cs1(c2))
            c = c2 + 1
         end do
       end block
       ps = ps1; pe = pe1; ph = ph1
    end do

    ! ---- close everything at yhi
    S%y = yhi
    n = tr_first(S)
    do while (n /= 0)
       if (S%flb(n) /= 0) call emit(S, S%rec_y(n), yhi, n, S%rec_r(n))
       n = tr_succ(S, n)
    end do
  contains
    subroutine new_cluster()
      if (nc >= size(S%ca, kind=int64)) then
         call arena_grow_i64(S%ca, nc, 2 * nc)
         call arena_grow_i64(S%cb, nc, 2 * nc)
         call arena_grow_i64(S%ce0, nc, 2 * nc)
         call arena_grow_i64(S%cs0, nc, 2 * nc)
         call arena_grow_i64(S%ce1, nc, 2 * nc)
         call arena_grow_i64(S%cs1, nc, 2 * nc)
      end if
      nc = nc + 1
      S%ca(nc) = x0; S%cb(nc) = x1v; S%ce0(nc) = qe; S%cs0(nc) = qs
    end subroutine new_cluster
  end subroutine sweep_band

  ! ======================================================================= 4. stitch

  !> Join vertically adjacent raw trapezoids with the same two lines, build Trapezoids.
  subroutine stitch(F, ry1, ry2, rl, rr, n, traps, ntrap, st)
    type(FragSet), intent(in) :: F
    integer(int64), intent(inout) :: ry1(:), ry2(:), rl(:), rr(:)
    integer(int64), intent(in) :: n
    type(Trapezoid), allocatable, intent(out) :: traps(:)
    integer(int64), intent(out) :: ntrap
    type(TrapMergeStats), intent(inout) :: st
    integer(int64), allocatable :: lk(:, :), key(:), ukey(:), ustart(:), pos(:)
    integer(int32), allocatable :: idx(:)
    logical, allocatable :: alive(:)
    integer(int64) :: i, nu, u, p, q, a, b, m, j, t, nb
    integer :: status, st2
    allocate(lk(6, max(n, 1_int64)), key(max(n, 1_int64)), idx(max(n, 1_int64)), alive(max(n, 1_int64)))
    !$omp parallel do
    do i = 1, n
       call line_key(F, rl(i), lk(1, i), lk(2, i), lk(3, i))
       call line_key(F, rr(i), lk(4, i), lk(5, i), lk(6, i))
       key(i) = shiftr(mix64(mix64(mix64(mix64(mix64(mix64(lk(1, i)) + lk(2, i)) + lk(3, i)) &
                 + lk(4, i)) + lk(5, i)) + lk(6, i)), 1)
       idx(i) = int(i, int32)
       alive(i) = .true.
    end do
    !$omp end parallel do
    if (n > 1) call CompactSortPairs(key, idx, n)
    call run_starts(key, n, ukey, ustart, nu)
    !$omp parallel do schedule(dynamic, 1024) private(p, q, a, b, j, t, nb)
    do u = 1, nu
       nb = ustart(u + 1) - ustart(u)
       if (nb < 2) cycle
       ! insertion sort of the group by (line keys, y1)
       do p = ustart(u) + 1, ustart(u + 1) - 1
          t = idx(p); q = p - 1
          do while (q >= ustart(u))
             if (.not. raw_less(t, int(idx(q), int64))) exit
             idx(q + 1) = idx(q); q = q - 1
          end do
          idx(q + 1) = int(t, int32)
       end do
       p = ustart(u)
       do while (p < ustart(u + 1))
          a = idx(p); q = p + 1
          do while (q < ustart(u + 1))
             b = idx(q)
             if (any(lk(:, a) /= lk(:, b))) exit
             if (ry1(b) /= ry2(a)) exit
             ry2(a) = ry2(b); alive(b) = .false.
             q = q + 1
          end do
          p = q
       end do
    end do
    !$omp end parallel do
    allocate(pos(max(n, 1_int64)))
    m = 0
    do i = 1, n
       if (alive(i)) m = m + 1
       pos(i) = m
    end do
    allocate(traps(max(m, 1_int64)))
    ntrap = m
    j = 0
    !$omp parallel do private(status, st2) reduction(+:j)
    do i = 1, n
       if (.not. alive(i)) cycle
       call side_from_points(F%x1(rl(i)), F%y1(rl(i)), F%x2(rl(i)), F%y2(rl(i)), ry1(i), traps(pos(i))%l, status)
       call side_from_points(F%x1(rr(i)), F%y1(rr(i)), F%x2(rr(i)), F%y2(rr(i)), ry1(i), traps(pos(i))%r, st2)
       traps(pos(i))%y1 = int(ry1(i), int32); traps(pos(i))%y2 = int(ry2(i), int32)
       if (status /= K_TRAP_OK .or. st2 /= K_TRAP_OK) j = j + 1
       if (.not. trap_valid(traps(pos(i)))) j = j + 1
    end do
    !$omp end parallel do
    st%anomalies = st%anomalies + j
    m = 0
    do i = 1, ntrap
       if (trap_is_box(traps(i))) m = m + 1
    end do
    st%boxes = m
  contains
    logical function raw_less(x, y)
      integer(int64), intent(in) :: x, y
      integer :: k
      do k = 1, 6
         if (lk(k, x) /= lk(k, y)) then
            raw_less = lk(k, x) < lk(k, y); return
         end if
      end do
      raw_less = ry1(x) < ry1(y)
    end function raw_less
  end subroutine stitch

  !> Canonical key of the line through fragment f: primitive (dx, dy), dy > 0, and c = x dy - y dx.
  subroutine line_key(F, fi, dx, dy, c)
    type(FragSet), intent(in) :: F
    integer(int64), intent(in) :: fi
    integer(int64), intent(out) :: dx, dy, c
    integer(int64) :: g, a, b, t
    dx = F%x2(fi) - F%x1(fi); dy = F%y2(fi) - F%y1(fi)
    a = abs(dx); b = dy
    do while (b /= 0)
       t = mod(a, b); a = b; b = t
    end do
    g = max(a, 1_int64)
    dx = dx / g; dy = dy / g
    c = F%x1(fi)*dy - F%y1(fi)*dx
  end subroutine line_key

end module TrapMergeModule
