! File   : trap_pred.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Polygon-component geometric predicates (rotation-invariant & rectilinear-specific)
!          on canonical trapezoid sets (TrapezoidModule / TrapPNumModule).
!
! Rotation-invariant predicates (+ NOT_* complements via TPRED_NOT_OFFSET):
!   AREA, PERIMETER, VERTEX/VERTICES, HOLES, WIDTH (MABR short side), LENGTH (MABR long side),
!   ASPECT_RATIO (LENGTH/WIDTH), DIAMETER (convex hull max distance),
!   COMPACTNESS (4*pi*A/P^2), SOLIDITY (A/A_hull),
!   IS_RECTANGLE_WITH_ROTATION, IS_SQUARE_WITH_ROTATION,
!   CONVEX_CRITERIA / IS_CONVEX, CONCAVE_CRITERIA / IS_CONCAVE,
!   NOTCH (minimum intra-polygon exterior notch gap; excludes holes),
!   ENCLOSE_RECTANGLE (W H in any orientation via rotated slab-core intersection).
!
! Orientation-specific (rectilinear / 45-degree) predicates (+ NOT_* complements):
!   IS_RECTILINEAR, IS_45, IS_RECTANGLE, IS_SQUARE.
!
! Non-polymorphic POD/SoA throughout (no class(...), thread-local OpenMP workspaces).
module TrapPredModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding
  use omp_lib
  use WideIntModule
  use TrapezoidModule
  use TrapPNumModule
  implicit none
  private

  integer(int32), parameter, public :: &
       TPRED_NONE         = 0,  &
       TPRED_AREA         = 1,  &
       TPRED_PERIMETER    = 2,  &
       TPRED_VERTEX       = 3,  &
       TPRED_HOLES        = 4,  &
       TPRED_WIDTH        = 5,  &
       TPRED_LENGTH       = 6,  &
       TPRED_ASPECT       = 7,  &
       TPRED_DIAMETER     = 8,  &
       TPRED_COMPACTNESS  = 9,  &
       TPRED_SOLIDITY     = 10, &
       TPRED_CONVEX       = 11, &
       TPRED_CONCAVE      = 12, &
       TPRED_IS_RECT_ROT  = 13, &
       TPRED_IS_SQ_ROT    = 14, &
       TPRED_IS_RECT      = 15, &
       TPRED_IS_SQ        = 16, &
       TPRED_IS_RECTILIN  = 17, &
       TPRED_IS_45        = 18, &
       TPRED_NOTCH        = 19, &
       TPRED_ENCLOSE_RECT = 20, &
       TPRED_NOT_OFFSET   = 100

  integer(int32), parameter, public :: &
       TCMP_NONE     = 0, &
       TCMP_GT       = 1, &
       TCMP_GE       = 2, &
       TCMP_LT       = 3, &
       TCMP_LE       = 4, &
       TCMP_EQ       = 5, &
       TCMP_NE       = 6, &
       TCMP_RANGE    = 7, &  ! [v1, v2]
       TCMP_RANGE_OO = 8, &  ! (v1, v2)
       TCMP_RANGE_OC = 9, &  ! (v1, v2]
       TCMP_RANGE_CO = 10    ! [v1, v2)

  public :: TrapPredSelect, TrapPredStats

  type :: TrapPredStats
     integer(int64) :: polys = 0, selected = 0, traps_out = 0, anomalies = 0
     real(real64) :: t_pnum = 0.0_real64, t_loops = 0.0_real64, t_eval = 0.0_real64, t_total = 0.0_real64
  end type TrapPredStats

  real(real64), parameter :: PI = 3.14159265358979323846_real64

  interface
     function ldb_trapset_loops(n, rec, lab, np, stats) result(h) bind(c, name='ldb_trapset_loops')
       import :: c_ptr, c_int32_t, c_int64_t
       integer(c_int64_t), value :: n, np
       integer(c_int32_t), intent(in) :: rec(*)
       integer(c_int64_t), intent(in) :: lab(*)
       integer(c_int64_t), intent(out) :: stats(*)
       type(c_ptr) :: h
     end function ldb_trapset_loops
     subroutine ldb_trapset_loops_poly_off(h, poff) bind(c, name='ldb_trapset_loops_poly_off')
       import :: c_ptr, c_int64_t
       type(c_ptr), value :: h
       integer(c_int64_t), intent(out) :: poff(*)
     end subroutine ldb_trapset_loops_poly_off
     function ldb_trapset_polys_count(h, nvert) result(n) bind(c, name='ldb_trapset_polys_count')
       import :: c_ptr, c_int64_t
       type(c_ptr), value :: h
       integer(c_int64_t), intent(out) :: nvert
       integer(c_int64_t) :: n
     end function ldb_trapset_polys_count
     subroutine ldb_trapset_polys_get(h, poff, xs, ys) bind(c, name='ldb_trapset_polys_get')
       import :: c_ptr, c_int64_t
       type(c_ptr), value :: h
       integer(c_int64_t), intent(out) :: poff(*), xs(*), ys(*)
     end subroutine ldb_trapset_polys_get
     subroutine ldb_trapset_polys_free(h) bind(c, name='ldb_trapset_polys_free')
       import :: c_ptr
       type(c_ptr), value :: h
     end subroutine ldb_trapset_polys_free
  end interface

contains

  subroutine TrapPredSelect(traps, ntrap, pred, cmp, v1, v2, prec, out_traps, out_n, st)
    type(Trapezoid), intent(in) :: traps(:)
    integer(int64), intent(in) :: ntrap
    integer(int32), intent(in) :: pred, cmp
    real(real64), intent(in) :: v1, v2, prec
    type(Trapezoid), allocatable, intent(out) :: out_traps(:)
    integer(int64), intent(out) :: out_n
    type(TrapPredStats), intent(out) :: st

    type(TrapPolys) :: tp
    integer(int32), allocatable :: rec(:, :)
    integer(c_int64_t) :: cstat(8)
    type(c_ptr) :: hl
    integer(int64) :: nloop, nvert, pid, i, k, nsel_p
    integer(int64), allocatable :: loff(:), lx(:), ly(:), ploff(:)
    integer(int64), allocatable :: wi1(:), wi2(:), wi3(:), wi4(:), wi5(:), wi6(:), wi7(:), wi8(:)
    real(real64), allocatable :: wr1(:), wr2(:), wr3(:), wr4(:), wr5(:), wr6(:), wr7(:), wr8(:)
    real(real64), allocatable :: wr9(:), wr10(:), wr11(:), wr12(:), wr13(:)
    logical, allocatable :: keep(:)
    logical :: need_loops, need_outline, negate, res
    integer(int32) :: base_pred
    real(real64) :: t0, t1, t2

    st = TrapPredStats()
    out_n = 0
    t0 = omp_get_wtime()
    if (ntrap <= 0) then
       allocate(out_traps(0))
       st%t_total = omp_get_wtime() - t0
       return
    end if

    negate = (pred > TPRED_NOT_OFFSET)
    base_pred = merge(pred - TPRED_NOT_OFFSET, pred, negate)

    call TrapPNum(traps, ntrap, tp)
    st%polys = tp%np
    st%anomalies = st%anomalies + tp%anomalies

    ! Never call both TrapPolyOutline and ldb_trapset_loops: when loops are extracted,
    ! holes and perimeter are computed directly from the loops.
    need_loops = (base_pred /= TPRED_AREA .and. base_pred /= TPRED_PERIMETER .and. &
                  base_pred /= TPRED_VERTEX .and. base_pred /= TPRED_HOLES .and. &
                  base_pred /= TPRED_COMPACTNESS)
    need_outline = (.not. need_loops) .and. (base_pred /= TPRED_AREA)

    if (need_outline) then
       call TrapPolyOutline(traps, ntrap, tp)
       st%anomalies = st%anomalies + tp%outline_anomalies
    end if
    t1 = omp_get_wtime()
    st%t_pnum = t1 - t0

    if (need_loops) then
       call TrapRecords(traps, ntrap, rec)
       cstat = 0
       hl = ldb_trapset_loops(ntrap, rec, tp%lab, tp%np, cstat)
       st%anomalies = st%anomalies + cstat(1)
       nloop = ldb_trapset_polys_count(hl, nvert)
       allocate(loff(nloop + 1), lx(max(nvert, 1_int64)), ly(max(nvert, 1_int64)), ploff(tp%np + 1))
       call ldb_trapset_polys_get(hl, loff, lx, ly)
       call ldb_trapset_loops_poly_off(hl, ploff)
       call ldb_trapset_polys_free(hl)
       deallocate(rec)
    else
       allocate(loff(1), lx(1), ly(1), ploff(1))
    end if
    t2 = omp_get_wtime()
    st%t_loops = t2 - t1

    allocate(keep(tp%np))
    keep = .false.
    nsel_p = 0

    ! Thread-local POD workspaces allocated once per OpenMP thread (grown on demand)
    ! to avoid per-polygon heap contention and remain non-polymorphic / ifx-safe.
    !$omp parallel default(none) shared(tp, loff, lx, ly, ploff, base_pred, negate, cmp, v1, v2, prec, keep) &
    !$omp private(pid, res, wi1, wi2, wi3, wi4, wi5, wi6, wi7, wi8, &
    !$omp         wr1, wr2, wr3, wr4, wr5, wr6, wr7, wr8, wr9, wr10, wr11, wr12, wr13) &
    !$omp reduction(+:nsel_p)
    allocate(wi1(64), wi2(64), wi3(64), wi4(64), wi5(64), wi6(64), wi7(64), wi8(64))
    allocate(wr1(64), wr2(64), wr3(64), wr4(64), wr5(64), wr6(64), wr7(64), wr8(64), &
             wr9(64), wr10(64), wr11(64), wr12(64), wr13(64))
    !$omp do schedule(dynamic, 64)
    do pid = 1, tp%np
       res = eval_poly_pred(pid, tp, loff, lx, ly, ploff, base_pred, cmp, v1, v2, prec, &
                            wi1, wi2, wi3, wi4, wi5, wi6, wi7, wi8, &
                            wr1, wr2, wr3, wr4, wr5, wr6, wr7, wr8, wr9, wr10, wr11, wr12, wr13)
       if (negate) res = .not. res
       keep(pid) = res
       if (res) nsel_p = nsel_p + 1
    end do
    !$omp end do
    deallocate(wi1, wi2, wi3, wi4, wi5, wi6, wi7, wi8)
    deallocate(wr1, wr2, wr3, wr4, wr5, wr6, wr7, wr8, wr9, wr10, wr11, wr12, wr13)
    !$omp end parallel

    st%selected = nsel_p
    deallocate(loff, lx, ly, ploff)

    out_n = 0
    do i = 1, ntrap
       if (keep(tp%lab(i))) out_n = out_n + 1
    end do
    allocate(out_traps(max(out_n, 0_int64)))
    k = 0
    do i = 1, ntrap
       if (keep(tp%lab(i))) then
          k = k + 1
          out_traps(k) = traps(i)
       end if
    end do
    st%traps_out = out_n
    st%t_eval = omp_get_wtime() - t2
    st%t_total = omp_get_wtime() - t0
  end subroutine TrapPredSelect

  subroutine ensure_i64(a, n)
    integer(int64), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    if (size(a, kind=int64) < n) then
       deallocate(a)
       allocate(a(max(n, 2_int64 * size(a, kind=int64))))
    end if
  end subroutine ensure_i64

  subroutine ensure_r64(a, n)
    real(real64), allocatable, intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    if (size(a, kind=int64) < n) then
       deallocate(a)
       allocate(a(max(n, 2_int64 * size(a, kind=int64))))
    end if
  end subroutine ensure_r64

  pure logical function cmp_val(val, cmp, v1, v2) result(ok)
    real(real64), intent(in) :: val, v1, v2
    integer(int32), intent(in) :: cmp
    real(real64), parameter :: eps = 1.0e-9_real64
    select case (cmp)
    case (TCMP_GT);       ok = val > v1
    case (TCMP_GE);       ok = val >= v1 - eps
    case (TCMP_LT);       ok = val < v1
    case (TCMP_LE);       ok = val <= v1 + eps
    case (TCMP_EQ);       ok = abs(val - v1) <= max(eps, 1.0e-6_real64 * max(1.0_real64, abs(v1)))
    case (TCMP_NE);       ok = abs(val - v1) >  max(eps, 1.0e-6_real64 * max(1.0_real64, abs(v1)))
    case (TCMP_RANGE);    ok = val >= v1 - eps .and. val <= v2 + eps
    case (TCMP_RANGE_OO); ok = val > v1 .and. val < v2
    case (TCMP_RANGE_OC); ok = val > v1 .and. val <= v2 + eps
    case (TCMP_RANGE_CO); ok = val >= v1 - eps .and. val < v2
    case default;         ok = val > 0.0_real64
    end select
  end function cmp_val

  logical function eval_poly_pred(pid, tp, loff, lx, ly, ploff, pred, cmp, v1, v2, prec, &
                                  wi1, wi2, wi3, wi4, wi5, wi6, wi7, wi8, &
                                  wr1, wr2, wr3, wr4, wr5, wr6, wr7, wr8, wr9, wr10, wr11, wr12, wr13) result(ok)
    integer(int64), intent(in) :: pid
    type(TrapPolys), intent(in) :: tp
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), ploff(:)
    integer(int32), intent(in) :: pred, cmp
    real(real64), intent(in) :: v1, v2, prec
    integer(int64), allocatable, intent(inout) :: wi1(:), wi2(:), wi3(:), wi4(:), wi5(:), wi6(:), wi7(:), wi8(:)
    real(real64), allocatable, intent(inout) :: wr1(:), wr2(:), wr3(:), wr4(:), wr5(:), wr6(:), wr7(:), wr8(:), wr9(:), wr10(:), wr11(:), wr12(:), wr13(:)

    real(real64) :: area_dbu, perim_dbu, val, ch_area, mabr_w, mabr_l, mabr_area, mabr_ang, diam, dnotch
    integer(int64) :: l0, l1, nl, nconv, nconc, nvert_tot, nholes
    logical :: is_ortho, is_45, is_rect_rot, is_rect

    ok = .false.
    area_dbu = real(tp%area2(pid), real64) * 0.5_real64

    select case (pred)
    case (TPRED_AREA)
       val = area_dbu / (prec * prec)
       ok = cmp_val(val, cmp, v1, v2)
       return
    case (TPRED_PERIMETER)
       val = tp%perim(pid) / prec
       ok = cmp_val(val, cmp, v1, v2)
       return
    case (TPRED_VERTEX)
       val = real(tp%nvert(pid), real64)
       ok = cmp_val(val, cmp, v1, v2)
       return
    case (TPRED_HOLES)
       val = real(tp%holes(pid), real64)
       ok = cmp_val(val, cmp, v1, v2)
       return
    case (TPRED_COMPACTNESS)
       perim_dbu = tp%perim(pid)
       if (perim_dbu > 0.0_real64) then
          val = 4.0_real64 * PI * area_dbu / (perim_dbu * perim_dbu)
       else
          val = 0.0_real64
       end if
       ok = cmp_val(val, cmp, v1, v2)
       return
    end select

    l0 = ploff(pid); l1 = ploff(pid + 1) - 1
    nl = l1 - l0 + 1
    if (nl <= 0) return

    select case (pred)
    case (TPRED_IS_RECTILIN, TPRED_IS_45, TPRED_IS_RECT, TPRED_IS_SQ)
       call loop_edge_classes(loff, lx, ly, l0, l1, is_ortho, is_45, nvert_tot, nholes)
       select case (pred)
       case (TPRED_IS_RECTILIN); ok = is_ortho
       case (TPRED_IS_45);       ok = is_45
       case (TPRED_IS_RECT)
          ok = is_ortho .and. nholes == 0 .and. nl == 1 .and. nvert_tot == 4
       case (TPRED_IS_SQ)
          is_rect = is_ortho .and. nholes == 0 .and. nl == 1 .and. nvert_tot == 4
          ok = is_rect .and. ((tp%x2(pid) - tp%x1(pid)) == (tp%y2(pid) - tp%y1(pid)))
       end select
       if (cmp /= TCMP_NONE) ok = cmp_val(merge(1.0_real64, 0.0_real64, ok), cmp, v1, v2)
       return

    case (TPRED_CONVEX, TPRED_CONCAVE)
       call loop_corner_counts(loff, lx, ly, l0, l1, nconv, nconc, nholes)
       if (pred == TPRED_CONVEX) then
          if (cmp == TCMP_NONE) then
             ok = (nholes == 0 .and. nconc == 0)
          else
             ok = cmp_val(real(nconv, real64), cmp, v1, v2)
          end if
       else
          if (cmp == TCMP_NONE) then
             ok = (nholes > 0 .or. nconc > 0)
          else
             ok = cmp_val(real(nconc, real64), cmp, v1, v2)
          end if
       end if
       return

    case (TPRED_WIDTH, TPRED_LENGTH, TPRED_ASPECT, TPRED_DIAMETER, TPRED_SOLIDITY, &
          TPRED_IS_RECT_ROT, TPRED_IS_SQ_ROT)
       call poly_hull_mabr(loff, lx, ly, l0, l1, ch_area, mabr_w, mabr_l, mabr_area, mabr_ang, diam, &
                           wi1, wi2, wi3, wi4, wr1, wr2)
       select case (pred)
       case (TPRED_WIDTH);    ok = cmp_val(mabr_w / prec, cmp, v1, v2)
       case (TPRED_LENGTH);   ok = cmp_val(mabr_l / prec, cmp, v1, v2)
       case (TPRED_ASPECT)
          val = mabr_l / max(mabr_w, 1.0e-12_real64)
          ok = cmp_val(val, cmp, v1, v2)
       case (TPRED_DIAMETER); ok = cmp_val(diam / prec, cmp, v1, v2)
       case (TPRED_SOLIDITY)
          val = area_dbu / max(ch_area, 1.0e-12_real64)
          ok = cmp_val(val, cmp, v1, v2)
       case (TPRED_IS_RECT_ROT, TPRED_IS_SQ_ROT)
          call loops_perim_holes(loff, lx, ly, l0, l1, perim_dbu, nholes)
          is_rect_rot = (nholes == 0 .and. nl == 1 .and. mabr_area > 0.0_real64 .and. &
                         area_dbu >= 0.85_real64 * mabr_area .and. &
                         abs(area_dbu - mabr_area) <= max(2.0_real64, 0.55_real64 * perim_dbu))
          if (pred == TPRED_IS_RECT_ROT) then
             ok = is_rect_rot
          else
             ok = is_rect_rot .and. abs(mabr_l - mabr_w) <= max(2.0_real64, 0.01_real64 * mabr_l)
          end if
          if (cmp /= TCMP_NONE) ok = cmp_val(merge(1.0_real64, 0.0_real64, ok), cmp, v1, v2)
       end select
       return

    case (TPRED_NOTCH)
       dnotch = poly_min_notch(loff, lx, ly, l0, l1, wi1, wi2, wi3, wi4, wi5, wi6, wi7, wi8)
       if (cmp == TCMP_NONE) then
          ok = (dnotch < 1.0e29_real64)
       else if (dnotch >= 1.0e29_real64) then
          ok = .false.
       else
          ok = cmp_val(dnotch / prec, cmp, v1, v2)
       end if
       return

    case (TPRED_ENCLOSE_RECT)
       call poly_hull_mabr(loff, lx, ly, l0, l1, ch_area, mabr_w, mabr_l, mabr_area, mabr_ang, diam, &
                           wi1, wi2, wi3, wi4, wr1, wr2)
       ok = poly_enclose_rect(loff, lx, ly, l0, l1, area_dbu, mabr_w, mabr_l, mabr_ang, diam, &
                              v1 * prec, v2 * prec, wi1, &
                              wr1, wr2, wr3, wr4, wr5, wr6, wr7, wr8, wr9, wr10, wr11, wr12, wr13)
       return
    end select
  end function eval_poly_pred

  ! ------------------------------------------------------------------ edge, loop & corner helpers

  pure integer(int32) function loop_orient_sign(lx, ly, a, b) result(sgn)
    integer(int64), intent(in) :: lx(:), ly(:), a, b
    integer(int64) :: j, jn, ox, oy
    type(i128) :: a2, term
    if (b - a + 1 < 3) then
       sgn = 0; return
    end if
    ox = lx(a); oy = ly(a)
    a2 = i128_from_int64(0_int64)
    do j = a, b
       jn = j + 1; if (jn > b) jn = a
       term = i128_sub(i128_mul64(lx(j) - ox, ly(jn) - oy), i128_mul64(lx(jn) - ox, ly(j) - oy))
       a2 = i128_add(a2, term)
    end do
    sgn = i128_sign(a2)
  end function loop_orient_sign

  pure subroutine loops_perim_holes(loff, lx, ly, l0, l1, perim, nholes)
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    real(real64), intent(out) :: perim
    integer(int64), intent(out) :: nholes
    integer(int64) :: l, a, b, j, jn
    perim = 0.0_real64; nholes = 0
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       if (loop_orient_sign(lx, ly, a, b) < 0) nholes = nholes + 1
       do j = a, b
          jn = j + 1; if (jn > b) jn = a
          perim = perim + hypot(real(lx(jn) - lx(j), real64), real(ly(jn) - ly(j), real64))
       end do
    end do
  end subroutine loops_perim_holes

  pure subroutine loop_edge_classes(loff, lx, ly, l0, l1, is_ortho, is_45, nvert, nholes)
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    logical, intent(out) :: is_ortho, is_45
    integer(int64), intent(out) :: nvert, nholes
    integer(int64) :: l, a, b, j, jn, dx, dy
    is_ortho = .true.; is_45 = .true.; nvert = 0; nholes = 0
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       if (loop_orient_sign(lx, ly, a, b) < 0) nholes = nholes + 1
       nvert = nvert + (b - a + 1)
       do j = a, b
          jn = j + 1; if (jn > b) jn = a
          dx = abs(lx(jn) - lx(j)); dy = abs(ly(jn) - ly(j))
          if (dx /= 0 .and. dy /= 0) then
             is_ortho = .false.
             if (dx /= dy) is_45 = .false.
          end if
       end do
    end do
  end subroutine loop_edge_classes

  pure subroutine loop_corner_counts(loff, lx, ly, l0, l1, nconv, nconc, nholes)
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    integer(int64), intent(out) :: nconv, nconc, nholes
    integer(int64) :: l, a, b, j, jp, jm
    integer(int32) :: sgn
    nconv = 0; nconc = 0; nholes = 0
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       if (loop_orient_sign(lx, ly, a, b) < 0) nholes = nholes + 1
       do j = a, b
          jm = j - 1; if (jm < a) jm = b
          jp = j + 1; if (jp > b) jp = a
          sgn = i128_cmp_prod(lx(j) - lx(jm), ly(jp) - ly(j), ly(j) - ly(jm), lx(jp) - lx(j))
          if (sgn > 0) then
             nconv = nconv + 1
          else if (sgn < 0) then
             nconc = nconc + 1
          end if
       end do
    end do
  end subroutine loop_corner_counts

  ! ------------------------------------------------------------------ convex hull + O(nh) rotating calipers

  subroutine poly_hull_mabr(loff, lx, ly, l0, l1, ch_area, mabr_w, mabr_l, mabr_area, mabr_ang, diam, &
                            px, py, hx, hy, dx, dy)
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    real(real64), intent(out) :: ch_area, mabr_w, mabr_l, mabr_area, mabr_ang, diam
    integer(int64), allocatable, intent(inout) :: px(:), py(:), hx(:), hy(:)
    real(real64), allocatable, intent(inout) :: dx(:), dy(:)
    integer(int64) :: npts, l, a, b, i, j, k, t, nh, ox, oy
    integer(int64) :: inext, itop, iright, ileft, nxt, steps
    real(real64) :: ux, uy, elen, umin, umax, vmin, vmax, pu, pv, pu_n, pv_n, w, h, ar, d2, dmax2, a2

    ch_area = 0.0_real64; mabr_w = 0.0_real64; mabr_l = 0.0_real64
    mabr_area = 0.0_real64; mabr_ang = 0.0_real64; diam = 0.0_real64
    npts = 0
    do l = l0, l1
       npts = npts + (loff(l + 1) - loff(l))
    end do
    if (npts < 3) return
    call ensure_i64(px, npts); call ensure_i64(py, npts)
    call ensure_i64(hx, 2 * npts + 2); call ensure_i64(hy, 2 * npts + 2)

    k = 0
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       do i = a, b
          k = k + 1; px(k) = lx(i); py(k) = ly(i)
       end do
    end do
    call sort_pts_xy(px, py, npts)

    ! Andrew's monotone chain (strictly convex CCW hull, exact i128 orientation)
    k = 0
    do i = 1, npts
       if (i > 1) then
          if (px(i) == px(i - 1) .and. py(i) == py(i - 1)) cycle
       end if
       do while (k >= 2)
          if (cross_sign_i64(hx(k - 1), hy(k - 1), hx(k), hy(k), px(i), py(i)) <= 0) then
             k = k - 1
          else
             exit
          end if
       end do
       k = k + 1; hx(k) = px(i); hy(k) = py(i)
    end do
    t = k + 1
    do i = npts - 1, 1, -1
       if (px(i) == px(i + 1) .and. py(i) == py(i + 1)) cycle
       do while (k >= t)
          if (cross_sign_i64(hx(k - 1), hy(k - 1), hx(k), hy(k), px(i), py(i)) <= 0) then
             k = k - 1
          else
             exit
          end if
       end do
       k = k + 1; hx(k) = px(i); hy(k) = py(i)
    end do
    nh = k - 1
    if (nh < 3) return

    ! Translate hull to local origin (ox, oy) to avoid real64 cancellation at large coordinates
    call ensure_r64(dx, nh); call ensure_r64(dy, nh)
    ox = hx(1); oy = hy(1)
    do i = 1, nh
       dx(i) = real(hx(i) - ox, real64)
       dy(i) = real(hy(i) - oy, real64)
    end do

    ! Hull area (shifted coordinates)
    a2 = 0.0_real64
    do i = 1, nh
       j = i + 1; if (j > nh) j = 1
       a2 = a2 + dx(i) * dy(j) - dx(j) * dy(i)
    end do
    ch_area = 0.5_real64 * abs(a2)

    ! O(nh) rotating calipers for BOTH diameter and minimum-area bounding rectangle
    mabr_area = huge(1.0_real64)
    dmax2 = 0.0_real64
    itop = 1; iright = 1; ileft = 1
    do i = 1, nh
       inext = i + 1; if (inext > nh) inext = 1
       ux = dx(inext) - dx(i); uy = dy(inext) - dy(i)
       elen = hypot(ux, uy)
       if (elen <= 0.0_real64) cycle
       ux = ux / elen; uy = uy / elen

       if (i == 1) then
          ! Exact O(nh) initialization of the three caliper pointers
          umin = huge(1.0_real64); umax = -huge(1.0_real64); vmax = -huge(1.0_real64)
          do k = 1, nh
             pu = dx(k) * ux + dy(k) * uy
             pv = -dx(k) * uy + dy(k) * ux
             if (pu > umax) then; umax = pu; iright = k; end if
             if (pu < umin) then; umin = pu; ileft  = k; end if
             if (pv > vmax) then; vmax = pv; itop   = k; end if
          end do
       else
          ! Monotonic CCW advance of iright, itop, ileft
          steps = 0
          do while (steps < nh)
             nxt = iright + 1; if (nxt > nh) nxt = 1
             pu   = dx(iright) * ux + dy(iright) * uy
             pu_n = dx(nxt)    * ux + dy(nxt)    * uy
             if (pu_n > pu) then
                iright = nxt; steps = steps + 1
             else
                exit
             end if
          end do
          steps = 0
          do while (steps < nh)
             nxt = itop + 1; if (nxt > nh) nxt = 1
             pv   = -dx(itop) * uy + dy(itop) * ux
             pv_n = -dx(nxt)  * uy + dy(nxt)  * ux
             if (pv_n > pv) then
                itop = nxt; steps = steps + 1
             else
                exit
             end if
          end do
          steps = 0
          do while (steps < nh)
             nxt = ileft + 1; if (nxt > nh) nxt = 1
             pu   = dx(ileft) * ux + dy(ileft) * uy
             pu_n = dx(nxt)   * ux + dy(nxt)   * uy
             if (pu_n < pu) then
                ileft = nxt; steps = steps + 1
             else
                exit
             end if
          end do
       end if

       ! Diameter from antipodal pair (edge i..inext vs itop and next(itop))
       d2 = (dx(itop) - dx(i))**2 + (dy(itop) - dy(i))**2
       if (d2 > dmax2) dmax2 = d2
       d2 = (dx(itop) - dx(inext))**2 + (dy(itop) - dy(inext))**2
       if (d2 > dmax2) dmax2 = d2
       nxt = itop + 1; if (nxt > nh) nxt = 1
       d2 = (dx(nxt) - dx(i))**2 + (dy(nxt) - dy(i))**2
       if (d2 > dmax2) dmax2 = d2

       ! MABR at this edge orientation
       vmin = -dx(i) * uy + dy(i) * ux
       vmax = -dx(itop) * uy + dy(itop) * ux
       umax =  dx(iright) * ux + dy(iright) * uy
       umin =  dx(ileft)  * ux + dy(ileft)  * uy
       w = max(0.0_real64, umax - umin)
       h = max(0.0_real64, vmax - vmin)
       ar = w * h
       if (ar < mabr_area) then
          mabr_area = ar
          mabr_w = min(w, h)
          mabr_l = max(w, h)
          mabr_ang = atan2(uy, ux)
       end if
    end do
    diam = sqrt(dmax2)
    if (mabr_area > 0.5_real64 * huge(1.0_real64)) mabr_area = 0.0_real64
  end subroutine poly_hull_mabr

  pure integer(int32) function cross_sign_i64(ax, ay, bx, by, cx, cy) result(s)
    integer(int64), intent(in) :: ax, ay, bx, by, cx, cy
    s = i128_cmp_prod(bx - ax, cy - ay, by - ay, cx - ax)
  end function cross_sign_i64

  subroutine sort_pts_xy(px, py, n)
    integer(int64), intent(inout) :: px(:), py(:)
    integer(int64), intent(in) :: n
    integer(int64) :: gap, i, j, tx, ty
    gap = n / 2
    do while (gap > 0)
       do i = gap + 1, n
          tx = px(i); ty = py(i); j = i
          do while (j > gap)
             if (px(j - gap) > tx .or. (px(j - gap) == tx .and. py(j - gap) > ty)) then
                px(j) = px(j - gap); py(j) = py(j - gap)
                j = j - gap
             else
                exit
             end if
          end do
          px(j) = tx; py(j) = ty
       end do
       gap = gap / 2
    end do
  end subroutine sort_pts_xy

  ! ------------------------------------------------------------------ NOTCH (exterior gap, excludes holes)

  real(real64) function poly_min_notch(loff, lx, ly, l0, l1, &
                                       sx1, sy1, sx2, sy2, sloop, sidx, slen, sxmin) result(dmin)
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    integer(int64), allocatable, intent(inout) :: sx1(:), sy1(:), sx2(:), sy2(:), sloop(:), sidx(:), slen(:), sxmin(:)
    integer(int64) :: ns, l, a, b, i, j, k, jn, gap, t1, t2, t3, t4, t5, t6, t7, t8
    real(real64) :: d, cx1, cy1, cx2, cy2, mx, my
    logical :: adj

    dmin = 1.0e30_real64
    ! Collect only segments on outer (CCW, signed area > 0) loops so hole widths are excluded.
    ns = 0
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       if (loop_orient_sign(lx, ly, a, b) > 0) ns = ns + (b - a + 1)
    end do
    if (ns < 4) return
    call ensure_i64(sx1, ns); call ensure_i64(sy1, ns)
    call ensure_i64(sx2, ns); call ensure_i64(sy2, ns)
    call ensure_i64(sloop, ns); call ensure_i64(sidx, ns)
    call ensure_i64(slen, ns); call ensure_i64(sxmin, ns)

    k = 0
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       if (loop_orient_sign(lx, ly, a, b) <= 0) cycle
       do j = a, b
          jn = j + 1; if (jn > b) jn = a
          k = k + 1
          sx1(k) = lx(j); sy1(k) = ly(j); sx2(k) = lx(jn); sy2(k) = ly(jn)
          sloop(k) = l; sidx(k) = j - a; slen(k) = b - a + 1
          sxmin(k) = min(lx(j), lx(jn))
       end do
    end do

    ! Sort segments by sxmin for sweep-line early exit
    gap = ns / 2
    do while (gap > 0)
       do i = gap + 1, ns
          t1 = sx1(i); t2 = sy1(i); t3 = sx2(i); t4 = sy2(i)
          t5 = sloop(i); t6 = sidx(i); t7 = slen(i); t8 = sxmin(i)
          j = i
          do while (j > gap)
             if (sxmin(j - gap) > t8) then
                sx1(j) = sx1(j - gap); sy1(j) = sy1(j - gap)
                sx2(j) = sx2(j - gap); sy2(j) = sy2(j - gap)
                sloop(j) = sloop(j - gap); sidx(j) = sidx(j - gap)
                slen(j) = slen(j - gap); sxmin(j) = sxmin(j - gap)
                j = j - gap
             else
                exit
             end if
          end do
          sx1(j) = t1; sy1(j) = t2; sx2(j) = t3; sy2(j) = t4
          sloop(j) = t5; sidx(j) = t6; slen(j) = t7; sxmin(j) = t8
       end do
       gap = gap / 2
    end do

    do i = 1, ns
       do j = i + 1, ns
          ! Sweep-line X bound: all subsequent j have even larger sxmin(j)
          if (real(sxmin(j) - max(sx1(i), sx2(i)), real64) >= dmin) exit
          adj = .false.
          if (sloop(i) == sloop(j)) then
             if (abs(sidx(j) - sidx(i)) == 1 .or. &
                 (sidx(i) == 0 .and. sidx(j) == slen(i) - 1) .or. &
                 (sidx(j) == 0 .and. sidx(i) == slen(i) - 1)) adj = .true.
          end if
          if (adj) cycle
          if (real(max(min(sy1(i), sy2(i)) - max(sy1(j), sy2(j)), &
                       min(sy1(j), sy2(j)) - max(sy1(i), sy2(i))), real64) >= dmin) cycle
          call seg_seg_closest(real(sx1(i), real64), real(sy1(i), real64), &
                               real(sx2(i), real64), real(sy2(i), real64), &
                               real(sx1(j), real64), real(sy1(j), real64), &
                               real(sx2(j), real64), real(sy2(j), real64), &
                               d, cx1, cy1, cx2, cy2)
          if (d <= 1.0e-9_real64 .or. d >= dmin) cycle
          mx = 0.5_real64 * (cx1 + cx2); my = 0.5_real64 * (cy1 + cy2)
          if (.not. point_in_loops(mx, my, loff, lx, ly, l0, l1)) then
             if (.not. point_on_loops_boundary(mx, my, loff, lx, ly, l0, l1)) then
                if (.not. open_seg_crosses_loops(cx1, cy1, cx2, cy2, loff, lx, ly, l0, l1)) then
                   dmin = d
                end if
             end if
          end if
       end do
    end do
  end function poly_min_notch

  pure logical function point_on_loops_boundary(px, py, loff, lx, ly, l0, l1) result(on_b)
    real(real64), intent(in) :: px, py
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    integer(int64) :: l, a, b, j, jn
    real(real64) :: d, cx, cy
    on_b = .false.
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       do j = a, b
          jn = j + 1; if (jn > b) jn = a
          call pt_seg_closest(px, py, real(lx(j), real64), real(ly(j), real64), &
                              real(lx(jn), real64), real(ly(jn), real64), d, cx, cy)
          if (d <= 1.0e-6_real64) then
             on_b = .true.; return
          end if
       end do
    end do
  end function point_on_loops_boundary

  pure logical function open_seg_crosses_loops(ax, ay, bx, by, loff, lx, ly, l0, l1) result(crosses)
    real(real64), intent(in) :: ax, ay, bx, by
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    integer(int64) :: l, a, b, j, jn
    real(real64) :: cx, cy, dx, dy, ux, uy, vx, vy, den, t, u
    real(real64), parameter :: eps = 1.0e-7_real64
    crosses = .false.
    ux = bx - ax; uy = by - ay
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       do j = a, b
          jn = j + 1; if (jn > b) jn = a
          cx = real(lx(j), real64);  cy = real(ly(j), real64)
          dx = real(lx(jn), real64); dy = real(ly(jn), real64)
          if (max(ax, bx) < min(cx, dx) - eps .or. max(cx, dx) < min(ax, bx) - eps) cycle
          if (max(ay, by) < min(cy, dy) - eps .or. max(cy, dy) < min(ay, by) - eps) cycle
          vx = dx - cx; vy = dy - cy
          den = ux * vy - uy * vx
          if (abs(den) <= 1.0e-12_real64) cycle
          t = ((cx - ax) * vy - (cy - ay) * vx) / den
          u = ((cx - ax) * uy - (cy - ay) * ux) / den
          if (t > eps .and. t < 1.0_real64 - eps .and. u >= -eps .and. u <= 1.0_real64 + eps) then
             crosses = .true.; return
          end if
       end do
    end do
  end function open_seg_crosses_loops

  pure subroutine pt_seg_closest(px, py, ax, ay, bx, by, d, cx, cy)
    real(real64), intent(in) :: px, py, ax, ay, bx, by
    real(real64), intent(out) :: d, cx, cy
    real(real64) :: dx, dy, l2, t
    dx = bx - ax; dy = by - ay
    l2 = dx * dx + dy * dy
    if (l2 <= 0.0_real64) then
       cx = ax; cy = ay
    else
       t = max(0.0_real64, min(1.0_real64, ((px - ax) * dx + (py - ay) * dy) / l2))
       cx = ax + t * dx; cy = ay + t * dy
    end if
    d = hypot(px - cx, py - cy)
  end subroutine pt_seg_closest

  pure subroutine seg_seg_closest(ax, ay, bx, by, cx, cy, dx, dy, dmin, p1x, p1y, p2x, p2y)
    real(real64), intent(in) :: ax, ay, bx, by, cx, cy, dx, dy
    real(real64), intent(out) :: dmin, p1x, p1y, p2x, p2y
    real(real64) :: d, qx, qy
    call pt_seg_closest(ax, ay, cx, cy, dx, dy, dmin, p2x, p2y); p1x = ax; p1y = ay
    call pt_seg_closest(bx, by, cx, cy, dx, dy, d, qx, qy)
    if (d < dmin) then; dmin = d; p1x = bx; p1y = by; p2x = qx; p2y = qy; end if
    call pt_seg_closest(cx, cy, ax, ay, bx, by, d, qx, qy)
    if (d < dmin) then; dmin = d; p1x = qx; p1y = qy; p2x = cx; p2y = cy; end if
    call pt_seg_closest(dx, dy, ax, ay, bx, by, d, qx, qy)
    if (d < dmin) then; dmin = d; p1x = qx; p1y = qy; p2x = dx; p2y = dy; end if
  end subroutine seg_seg_closest

  pure logical function point_in_loops(px, py, loff, lx, ly, l0, l1) result(inside)
    real(real64), intent(in) :: px, py
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    integer(int64) :: l, a, b, j, jn
    real(real64) :: y1, y2, x1, x2, xi
    inside = .false.
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       do j = a, b
          jn = j + 1; if (jn > b) jn = a
          x1 = real(lx(j), real64);  y1 = real(ly(j), real64)
          x2 = real(lx(jn), real64); y2 = real(ly(jn), real64)
          if ((y1 > py) .neqv. (y2 > py)) then
             xi = x1 + (py - y1) * (x2 - x1) / (y2 - y1)
             if (px < xi) inside = .not. inside
          end if
       end do
    end do
  end function point_in_loops

  ! ------------------------------------------------------------------ ENCLOSE_RECTANGLE (any orientation)

  logical function poly_enclose_rect(loff, lx, ly, l0, l1, area_dbu, mabr_w, mabr_l, mabr_ang, diam, &
                                     rw, rh, rlo, rx, ry, ycand, yslab, cur_l, cur_r, sl_l, sl_r, &
                                     nxt_l, nxt_r, xl_arr, xh_arr, xm_arr) result(fits)
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    real(real64), intent(in) :: area_dbu, mabr_w, mabr_l, mabr_ang, diam, rw, rh
    integer(int64), allocatable, intent(inout) :: rlo(:)
    real(real64), allocatable, intent(inout) :: rx(:), ry(:), ycand(:), yslab(:)
    real(real64), allocatable, intent(inout) :: cur_l(:), cur_r(:), sl_l(:), sl_r(:), nxt_l(:), nxt_r(:)
    real(real64), allocatable, intent(inout) :: xl_arr(:), xh_arr(:), xm_arr(:)
    real(real64) :: wmin, wmax, angs(512), ang, dth
    integer :: nang, ia, k
    integer(int64) :: l, a, b, j, jn
    logical :: dup

    fits = .false.
    if (rw <= 0.0_real64 .or. rh <= 0.0_real64) then
       fits = .true.; return
    end if
    wmin = min(rw, rh); wmax = max(rw, rh)
    if (area_dbu + 2.0_real64 * (rw + rh) < rw * rh) return
    if (mabr_w + 1.5_real64 < wmin .or. mabr_l + 1.5_real64 < wmax) return
    if (diam + 1.5_real64 < hypot(rw, rh)) return

    ! Candidate angles in [0, pi): MABR angle + polygon edge angles + uniform 5-deg grid
    nang = 0
    call push_ang(angs, nang, mabr_ang)
    do l = l0, l1
       a = loff(l); b = loff(l + 1) - 1
       do j = a, b
          jn = j + 1; if (jn > b) jn = a
          if (lx(jn) == lx(j) .and. ly(jn) == ly(j)) cycle
          ang = atan2(real(ly(jn) - ly(j), real64), real(lx(jn) - lx(j), real64))
          call push_ang(angs, nang, ang)
          if (nang >= 440) exit
       end do
       if (nang >= 440) exit
    end do
    do k = 0, 35
       ang = real(k, real64) * (PI / 36.0_real64)
       dup = .false.
       do ia = 1, nang
          dth = abs(angs(ia) - ang)
          if (min(dth, PI - dth) < 0.008_real64) then; dup = .true.; exit; end if
       end do
       if (.not. dup .and. nang < 512) then
          nang = nang + 1; angs(nang) = ang
       end if
    end do

    do ia = 1, nang
       if (rect_fits_at_angle(loff, lx, ly, l0, l1, angs(ia), wmax, wmin, &
                              rlo, rx, ry, ycand, yslab, cur_l, cur_r, sl_l, sl_r, nxt_l, nxt_r, &
                              xl_arr, xh_arr, xm_arr)) then
          fits = .true.; return
       end if
       if (abs(wmax - wmin) > 1.0e-6_real64) then
          if (rect_fits_at_angle(loff, lx, ly, l0, l1, angs(ia), wmin, wmax, &
                                 rlo, rx, ry, ycand, yslab, cur_l, cur_r, sl_l, sl_r, nxt_l, nxt_r, &
                                 xl_arr, xh_arr, xm_arr)) then
             fits = .true.; return
          end if
       end if
    end do
  end function poly_enclose_rect

  pure subroutine push_ang(angs, nang, raw)
    real(real64), intent(inout) :: angs(:)
    integer, intent(inout) :: nang
    real(real64), intent(in) :: raw
    real(real64) :: a, d
    integer :: i
    a = modulo(raw, PI)
    do i = 1, nang
       d = abs(angs(i) - a)
       if (min(d, PI - d) < 1.0e-4_real64) return
    end do
    if (nang < size(angs)) then
       nang = nang + 1; angs(nang) = a
    end if
  end subroutine push_ang

  ! Fixed-angle slab-core test: does [0, W] x [0, H] fit inside rot(P, -theta)?
  ! Candidate y0 includes every vertex ry, ry - H, exact slanted-edge balance points
  ! x_e1(y0) == x_e2(y0 + H) (for npts <= 64), and uniform slices.
  logical function rect_fits_at_angle(loff, lx, ly, l0, l1, theta, W, H, &
                                      rlo, rx, ry, ycand, yslab, cur_l, cur_r, sl_l, sl_r, nxt_l, nxt_r, &
                                      xl_arr, xh_arr, xm_arr) result(fits)
    integer(int64), intent(in) :: loff(:), lx(:), ly(:), l0, l1
    real(real64), intent(in) :: theta, W, H
    integer(int64), allocatable, intent(inout) :: rlo(:)
    real(real64), allocatable, intent(inout) :: rx(:), ry(:), ycand(:), yslab(:)
    real(real64), allocatable, intent(inout) :: cur_l(:), cur_r(:), sl_l(:), sl_r(:), nxt_l(:), nxt_r(:)
    real(real64), allocatable, intent(inout) :: xl_arr(:), xh_arr(:), xm_arr(:)
    integer(int64) :: npts, nl, l, a, b, j, jn, l2, a2, b2, j2, j2n, k, ny, ns, iy, is, ncur, nnext, ox, oy
    real(real64) :: ct, st, ymin, ymax, y0, y1, step, ylo, yhi, lx0, ly0
    real(real64) :: m1, m2, c1, c2, dya, dyb, ybal

    fits = .false.
    nl = l1 - l0 + 1
    npts = 0
    do l = l0, l1
       npts = npts + (loff(l + 1) - loff(l))
    end do
    if (npts < 3) return
    call ensure_i64(rlo, nl + 1)
    call ensure_r64(rx, npts); call ensure_r64(ry, npts)
    call ensure_r64(yslab, npts + 4)
    call ensure_r64(cur_l, npts + 4); call ensure_r64(cur_r, npts + 4)
    call ensure_r64(sl_l,  npts + 4); call ensure_r64(sl_r,  npts + 4)
    call ensure_r64(nxt_l, npts + 4); call ensure_r64(nxt_r, npts + 4)
    call ensure_r64(xl_arr, npts + 4); call ensure_r64(xh_arr, npts + 4); call ensure_r64(xm_arr, npts + 4)

    ox = lx(loff(l0)); oy = ly(loff(l0))
    ct = cos(theta); st = sin(theta)
    ymin = huge(1.0_real64); ymax = -huge(1.0_real64)
    k = 0
    do l = 1, nl
       rlo(l) = k + 1
       a = loff(l0 + l - 1); b = loff(l0 + l) - 1
       do j = a, b
          k = k + 1
          lx0 = real(lx(j) - ox, real64); ly0 = real(ly(j) - oy, real64)
          rx(k) =  ct * lx0 + st * ly0
          ry(k) = -st * lx0 + ct * ly0
          if (ry(k) < ymin) ymin = ry(k)
          if (ry(k) > ymax) ymax = ry(k)
       end do
    end do
    rlo(nl + 1) = k + 1
    if (ymax - ymin + 1.0e-6_real64 < H) return

    step = max(1.0_real64, 0.25_real64 * min(W, H))
    ns = min(128_int64, max(1_int64, int((ymax - ymin - H) / step, int64) + 1))
    call ensure_r64(ycand, 2 * npts + ns + 4 + merge(npts * npts, 0_int64, npts <= 64))
    ny = 0
    do k = 1, npts
       if (ry(k) <= ymax - H + 1.0e-6_real64) then; ny = ny + 1; ycand(ny) = ry(k); end if
       if (ry(k) - H >= ymin - 1.0e-6_real64 .and. ry(k) - H <= ymax - H + 1.0e-6_real64) then
          ny = ny + 1; ycand(ny) = ry(k) - H
       end if
    end do
    ! Exact slanted-edge balance points: x_e1(y0) == x_e2(y0 + H)
    if (npts <= 64) then
       do l = 1, nl
          a = rlo(l); b = rlo(l + 1) - 1
          do j = a, b
             jn = j + 1; if (jn > b) jn = a
             dya = ry(jn) - ry(j)
             if (abs(dya) <= 1.0e-9_real64) cycle
             m1 = (rx(jn) - rx(j)) / dya
             c1 = rx(j) - m1 * ry(j)
             do l2 = 1, nl
                a2 = rlo(l2); b2 = rlo(l2 + 1) - 1
                do j2 = a2, b2
                   j2n = j2 + 1; if (j2n > b2) j2n = a2
                   dyb = ry(j2n) - ry(j2)
                   if (abs(dyb) <= 1.0e-9_real64) cycle
                   m2 = (rx(j2n) - rx(j2)) / dyb
                   if (abs(m1 - m2) <= 1.0e-9_real64) cycle
                   c2 = rx(j2) - m2 * ry(j2)
                   ybal = (c2 + m2 * H - c1) / (m1 - m2)
                   if (ybal >= max(ymin, min(ry(j), ry(jn)), min(ry(j2), ry(j2n)) - H) - 1.0e-6_real64 .and. &
                       ybal <= min(ymax - H, max(ry(j), ry(jn)), max(ry(j2), ry(j2n)) - H) + 1.0e-6_real64) then
                      ny = ny + 1; ycand(ny) = ybal
                   end if
                end do
             end do
          end do
       end do
    end if
    do k = 0, ns
       y0 = ymin + real(k, real64) * (ymax - ymin - H) / real(max(1_int64, ns), real64)
       ny = ny + 1; ycand(ny) = y0
    end do
    call sort_r64(ycand, ny)

    do iy = 1, ny
       if (iy > 1) then
          if (abs(ycand(iy) - ycand(iy - 1)) < 1.0e-6_real64) cycle
       end if
       y0 = ycand(iy); y1 = y0 + H
       ns = 2; yslab(1) = y0; yslab(2) = y1
       do k = 1, npts
          if (ry(k) > y0 + 1.0e-7_real64 .and. ry(k) < y1 - 1.0e-7_real64) then
             ns = ns + 1; yslab(ns) = ry(k)
          end if
       end do
       call sort_r64(yslab, ns)

       ncur = 0
       do is = 1, ns - 1
          ylo = yslab(is); yhi = yslab(is + 1)
          if (yhi - ylo < 1.0e-7_real64) cycle
          call slab_core_intervals(rlo, rx, ry, nl, ylo, yhi, sl_l, sl_r, nnext, xl_arr, xh_arr, xm_arr)
          if (nnext == 0) then
             ncur = 0; exit
          end if
          if (ncur == 0) then
             ncur = nnext; cur_l(1:ncur) = sl_l(1:ncur); cur_r(1:ncur) = sl_r(1:ncur)
          else
             call intersect_intervals(cur_l, cur_r, ncur, sl_l, sl_r, nnext, nxt_l, nxt_r, k)
             ncur = k
             if (ncur == 0) exit
             cur_l(1:ncur) = nxt_l(1:ncur); cur_r(1:ncur) = nxt_r(1:ncur)
          end if
       end do
       do k = 1, ncur
          if (cur_r(k) - cur_l(k) >= W - 1.0e-6_real64) then
             fits = .true.; return
          end if
       end do
    end do
  end function rect_fits_at_angle

  ! Exact x-intervals where vertical segment {x} x [ylo, yhi] lies completely inside rot(P, -theta).
  ! Uses dynamically sized workspace arrays (no fixed crossing cap).
  subroutine slab_core_intervals(rlo, rx, ry, nl, ylo, yhi, out_l, out_r, nout, xl_arr, xh_arr, xm_arr)
    integer(int64), intent(in) :: rlo(:), nl
    real(real64), intent(in) :: rx(:), ry(:), ylo, yhi
    real(real64), intent(out) :: out_l(:), out_r(:)
    integer(int64), intent(out) :: nout
    real(real64), intent(inout) :: xl_arr(:), xh_arr(:), xm_arr(:)
    real(real64) :: ym, y1, y2, x1, x2, xl, xh, xm, a1, b1
    integer(int64) :: l, a, b, j, jn, nc, i, t, gap

    ym = 0.5_real64 * (ylo + yhi)
    nc = 0
    do l = 1, nl
       a = rlo(l); b = rlo(l + 1) - 1
       do j = a, b
          jn = j + 1; if (jn > b) jn = a
          y1 = ry(j); y2 = ry(jn)
          if ((y1 > ym) .neqv. (y2 > ym)) then
             x1 = rx(j); x2 = rx(jn)
             xm = x1 + (ym - y1) * (x2 - x1) / (y2 - y1)
             xl = x1 + (ylo - y1) * (x2 - x1) / (y2 - y1)
             xh = x1 + (yhi - y1) * (x2 - x1) / (y2 - y1)
             nc = nc + 1
             xm_arr(nc) = xm; xl_arr(nc) = xl; xh_arr(nc) = xh
          end if
       end do
    end do
    ! Shell sort crossings by xm (O(nc log nc) even for dense comb polygons)
    gap = nc / 2
    do while (gap > 0)
       do i = gap + 1, nc
          xm = xm_arr(i); xl = xl_arr(i); xh = xh_arr(i); t = i
          do while (t > gap)
             if (xm_arr(t - gap) > xm) then
                xm_arr(t) = xm_arr(t - gap); xl_arr(t) = xl_arr(t - gap); xh_arr(t) = xh_arr(t - gap)
                t = t - gap
             else
                exit
             end if
          end do
          xm_arr(t) = xm; xl_arr(t) = xl; xh_arr(t) = xh
       end do
       gap = gap / 2
    end do
    nout = 0
    do i = 1, nc - 1, 2
       a1 = max(xl_arr(i), xh_arr(i))
       b1 = min(xl_arr(i + 1), xh_arr(i + 1))
       if (b1 > a1 + 1.0e-9_real64) then
          nout = nout + 1; out_l(nout) = a1; out_r(nout) = b1
       end if
    end do
  end subroutine slab_core_intervals

  pure subroutine intersect_intervals(al, ar, na, bl, br, nb, cl, cr, nc)
    real(real64), intent(in) :: al(:), ar(:), bl(:), br(:)
    integer(int64), intent(in) :: na, nb
    real(real64), intent(out) :: cl(:), cr(:)
    integer(int64), intent(out) :: nc
    integer(int64) :: i, j
    real(real64) :: lo, hi
    i = 1; j = 1; nc = 0
    do while (i <= na .and. j <= nb)
       lo = max(al(i), bl(j)); hi = min(ar(i), br(j))
       if (hi > lo + 1.0e-9_real64) then
          nc = nc + 1; cl(nc) = lo; cr(nc) = hi
       end if
       if (ar(i) < br(j)) then
          i = i + 1
       else
          j = j + 1
       end if
    end do
  end subroutine intersect_intervals

  pure subroutine sort_r64(a, n)
    real(real64), intent(inout) :: a(:)
    integer(int64), intent(in) :: n
    integer(int64) :: gap, i, j
    real(real64) :: tmp
    gap = n / 2
    do while (gap > 0)
       do i = gap + 1, n
          tmp = a(i); j = i
          do while (j > gap)
             if (a(j - gap) > tmp) then
                a(j) = a(j - gap); j = j - gap
             else
                exit
             end if
          end do
          a(j) = tmp
       end do
       gap = gap / 2
    end do
  end subroutine sort_r64

end module TrapPredModule
