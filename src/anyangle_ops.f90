! File   : anyangle_ops.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: MAGPARSER integration of the any-angle engine (TrapMergeModule / TrapPNumModule).
!
!   * LoadAnyAngleLayers   after a design is loaded with `var anyangle 1` (or MAGPARSER_ANYANGLE=1):
!                          every layer with a non-axis-parallel edge is re-read exactly (polygon
!                          iterator, no strip approximation) and merged into its canonical
!                          trapezoid set (Layer%traps). Manhattan layers keep their boxes.
!   * AnyAngleBoolean      two-layer OR / AND / NOT / XOR and single-layer self-overlap AND on
!                          any operands (boxes, trapezoids or both): one joint snap rounding of
!                          A and B, one predicate sweep. A Manhattan result is stored as boxes
!                          (every downstream operation works on it), anything else as trapezoids.
!   * AnyAngleMeasure      `exec measure` of an any-angle layer: PNUM polygons, holes, vertices,
!                          exact area, perimeter (same definitions as measure.f90).
!   * AnyAngleDigest       content hash of an any-angle layer (MAGPARSER_LAYER_DIGEST).
!
! Operands that are any-angle layers are fed back to the engine as their exact lattice polygons
! (db/trap_out.cpp, ldb_trapset_polys). MAGPARSER_FORCE_TRAP=1 (testing) sends Manhattan operands
! through the engine as well.
module AnyAngleOpsModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding
  use omp_lib
  use GeometryModule, only: Box
  use DesignModule
  use LayerMeasureModule, only: LayerMeasures
  use WideIntModule
  use TrapezoidModule
  use TrapMergeModule
  use TrapPNumModule
  use TrapTopoModule
  use TrapMorphModule
  use TrapPredModule
  implicit none
  private

  public :: AnyAngleEnabled, AnyAngleForce, AnyAngleSetEnabled, LoadAnyAngleLayers
  public :: TOP_OR, TOP_AND, TOP_NOT, TOP_XOR, TOP_BNOTA, TOP_SELF2
  public :: AnyAngleTopo, AnyAngleTopoCode, AnyAngleMorph
  public :: AnyAnglePredCode, AnyAngleParsePredArgs, AnyAnglePredicate
  public :: AnyAngleBoolean, AnyAngleMeasure, AnyAngleDigest, AnyAngleOpFromChar, AnyAngleOpFromName

  logical, save :: anyangle_on = .false., anyangle_env_read = .false.

  interface
     function ldb_polys_load(path, nsel, layers, dts, err, errsz) result(h) bind(c, name='ldb_polys_load')
       import :: c_ptr, c_char, c_int, c_size_t
       character(kind=c_char), intent(in) :: path(*)
       integer(c_int), value :: nsel
       integer(c_int), intent(in) :: layers(*), dts(*)
       character(kind=c_char), intent(inout) :: err(*)
       integer(c_size_t), value :: errsz
       type(c_ptr) :: h
     end function ldb_polys_load
     function ldb_polys_count(h, k, nvert) result(n) bind(c, name='ldb_polys_count')
       import :: c_ptr, c_int, c_int64_t
       type(c_ptr), value :: h
       integer(c_int), value :: k
       integer(c_int64_t), intent(out) :: nvert
       integer(c_int64_t) :: n
     end function ldb_polys_count
     subroutine ldb_polys_get(h, k, poff, xs, ys) bind(c, name='ldb_polys_get')
       import :: c_ptr, c_int, c_int64_t
       type(c_ptr), value :: h
       integer(c_int), value :: k
       integer(c_int64_t), intent(out) :: poff(*), xs(*), ys(*)
     end subroutine ldb_polys_get
     subroutine ldb_polys_free(h) bind(c, name='ldb_polys_free')
       import :: c_ptr
       type(c_ptr), value :: h
     end subroutine ldb_polys_free
     function ldb_trapset_polys(n, rec, stats) result(h) bind(c, name='ldb_trapset_polys')
       import :: c_ptr, c_int32_t, c_int64_t
       integer(c_int64_t), value :: n
       integer(c_int32_t), intent(in) :: rec(*)
       integer(c_int64_t), intent(out) :: stats(*)
       type(c_ptr) :: h
     end function ldb_trapset_polys
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

  ! ------------------------------------------------------------------ switches

  logical function AnyAngleEnabled()
    character(len=16) :: v
    integer :: ln, st
    if (.not. anyangle_env_read) then
       anyangle_env_read = .true.
       call get_environment_variable('MAGPARSER_ANYANGLE', v, ln, st)
       if (st == 0 .and. ln > 0) anyangle_on = v(1:1) /= '0'
    end if
    AnyAngleEnabled = anyangle_on
  end function AnyAngleEnabled

  subroutine AnyAngleSetEnabled(on)
    logical, intent(in) :: on
    anyangle_env_read = .true.
    anyangle_on = on
  end subroutine AnyAngleSetEnabled

  !> MAGPARSER_FORCE_TRAP=1: run every supported Boolean through the engine (testing).
  logical function AnyAngleForce()
    character(len=16) :: v
    integer :: ln, st
    call get_environment_variable('MAGPARSER_FORCE_TRAP', v, ln, st)
    AnyAngleForce = st == 0 .and. ln > 0 .and. v(1:1) /= '0'
  end function AnyAngleForce

  !> CTR operator character -> TOP_* (0: not a Boolean the engine handles).
  integer(int32) function AnyAngleOpFromChar(c) result(op)
    character(len=*), intent(in) :: c
    select case (c)
    case ('+');           op = TOP_OR
    case ('*');           op = TOP_AND
    case ('@', '!', '~'); op = TOP_NOT
    case ('^');           op = TOP_XOR
    case default;         op = -1
    end select
  end function AnyAngleOpFromChar

  !> CTR operator word -> TOP_* (-1: not handled). Single-layer AND (B = nothing) is TOP_SELF2.
  integer(int32) function AnyAngleOpFromName(w, single) result(op)
    character(len=*), intent(in) :: w
    logical, intent(in) :: single
    select case (w)
    case ('OR', 'OR2');            op = TOP_OR
    case ('AND2', 'FRAMEAND');     op = TOP_AND
    case ('NOT', 'FRAMENOT');      op = TOP_NOT
    case ('XOR');                  op = TOP_XOR
    case ('AND')                   !> MAGPARSER AND is the single-layer self-overlap (B ignored)
       op = TOP_SELF2
       if (single) op = TOP_SELF2
    case default;                  op = -1
    end select
  end function AnyAngleOpFromName

  ! ------------------------------------------------------------------ loading

  !> Re-read the layers of d that have non-Manhattan shapes exactly, as canonical trapezoids.
  subroutine LoadAnyAngleLayers(d, path)
    type(Design), intent(inout), target :: d
    character(len=*), intent(in) :: path
    integer(c_int), allocatable :: lay(:), dt(:)
    integer, allocatable :: idx(:)
    integer :: i, nsel, ios, p, q
    type(c_ptr) :: h
    character(kind=c_char) :: err(1024)
    character(kind=c_char), allocatable :: cpath(:)
    integer(int64) :: npoly, nvert, ntrap, j, nslant
    integer(int64), allocatable :: poff(:), px(:), py(:)
    type(Trapezoid), allocatable :: traps(:)
    type(TrapMergeStats) :: st
    integer :: status
    real(real64) :: t0
    character(len=1024) :: nm
    if (.not. allocated(d%layers)) return
    allocate(lay(size(d%layers)), dt(size(d%layers)), idx(size(d%layers)))
    nsel = 0
    do i = 1, size(d%layers)
       nm = d%layerNames(i)
       if (nm(1:1) /= 'L') cycle
       p = index(nm, '_D')
       if (p == 0) cycle
       nsel = nsel + 1
       read(nm(2:p - 1), *, iostat=ios) lay(nsel)
       read(nm(p + 2:), *, iostat=ios) dt(nsel)
       idx(nsel) = i
    end do
    if (nsel == 0) return
    t0 = omp_get_wtime()
    allocate(cpath(len_trim(path) + 1))
    do q = 1, len_trim(path)
       cpath(q) = path(q:q)
    end do
    cpath(len_trim(path) + 1) = c_null_char
    err = c_null_char
    h = ldb_polys_load(cpath, int(nsel, c_int), lay, dt, err, int(size(err), c_size_t))
    if (.not. c_associated(h)) then
       write(*, '(A,A)') 'ERROR: anyangle: cannot re-read ', trim(path)
       return
    end if
    do i = 1, nsel
       npoly = ldb_polys_count(h, int(i - 1, c_int), nvert)
       allocate(poff(npoly + 1), px(max(nvert, 1_int64)), py(max(nvert, 1_int64)))
       call ldb_polys_get(h, int(i - 1, c_int), poff, px, py)
       nslant = 0
       !$omp parallel do reduction(+:nslant) schedule(dynamic, 4096)
       do j = 1, npoly
          if (poly_is_slanted(px, py, poff(j), poff(j + 1) - 1)) nslant = nslant + 1
       end do
       !$omp end parallel do
       if (nslant > 0) then
          st = TrapMergeStats()
          call TrapMergePolygons(npoly, poff, px, py, traps, ntrap, st, status)
          if (status /= 0 .or. st%anomalies /= 0) then
             write(*, '(A,A,A,I0,A,I0)') 'ERROR: anyangle: merge of ', trim(d%layerNames(idx(i))), &
                  ' failed: status=', status, ' anomalies=', st%anomalies
          else
             call store_traps(d%layers(idx(i)), traps, ntrap)
             call store_raw(d%layers(idx(i)), npoly, poff, px, py)
             write(*, '(A,A,A,I0,A,I0,A,I0,A,F0.3,A)') 'INFO: anyangle layer ', trim(d%layerNames(idx(i))), &
                  ': polygons=', npoly, ' slanted=', nslant, ' trapezoids=', ntrap, ' (', st%t_total, ' s)'
          end if
          if (allocated(traps)) deallocate(traps)
       end if
       deallocate(poff, px, py)
    end do
    call ldb_polys_free(h)
    write(*, '(A,F0.3,A)') 'INFO: anyangle load done (', omp_get_wtime() - t0, ' s)'
  end subroutine LoadAnyAngleLayers

  pure logical function poly_is_slanted(px, py, a, b) result(sl)
    integer(int64), intent(in) :: px(:), py(:), a, b
    integer(int64) :: j, k
    sl = .false.
    do j = a, b
       k = j + 1
       if (k > b) k = a
       if (px(j) /= px(k) .and. py(j) /= py(k)) then
          sl = .true.; return
       end if
    end do
  end function poly_is_slanted

  ! ------------------------------------------------------------------ Booleans

  !> out = A op B (op: TOP_OR, TOP_AND, TOP_NOT, TOP_XOR, TOP_BNOTA, TOP_SELF2 (B ignored)).
  subroutine AnyAngleBoolean(A, B, out, op, status)
    type(Layer), intent(inout), target :: A, B, out
    integer(int32), intent(in) :: op
    integer, intent(out) :: status
    integer(int64) :: npA, npB
    integer(int64), allocatable :: poA(:), xA(:), yA(:), poB(:), xB(:), yB(:)
    type(TrapList) :: res(1)
    type(TrapMergeStats) :: st
    integer(int32) :: ops(1)
    real(real64) :: t0
    t0 = omp_get_wtime()
    status = 0
    call layer_polys(A, npA, poA, xA, yA)
    if (op == TOP_SELF2) then
       npB = 0
       allocate(poB(1), xB(1), yB(1)); poB(1) = 1
    else
       call layer_polys(B, npB, poB, xB, yB)
    end if
    ops(1) = op
    st = TrapMergeStats()
    call TrapBooleanPolygons(npA, poA, xA, yA, npB, poB, xB, yB, 1, ops, res, st, status)
    deallocate(poA, xA, yA, poB, xB, yB)
    if (status /= 0 .or. st%anomalies /= 0) then
       write(*, '(A,I0,A,I0)') 'ERROR: anyangle Boolean failed: status=', status, ' anomalies=', st%anomalies
       if (status == 0) status = 3
       call ClearLayer(out)
       return
    end if
    call store_traps(out, res(1)%t, res(1)%n)
    write(*, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,F0.3,A)') 'INFO: anyangle op=', op, ' polygons=', st%polygons, &
         ' crossings=', st%crossings, ' inexact=', st%crossings_inexact, ' trapezoids=', res(1)%n, &
         ' (', omp_get_wtime() - t0, ' s)'
  end subroutine AnyAngleBoolean

  !> Store a canonical trapezoid set in L: all boxes -> box layer, else any-angle layer.
  subroutine store_traps(L, t, n)
    type(Layer), intent(inout), target :: L
    type(Trapezoid), intent(in) :: t(:)
    integer(int64), intent(in) :: n
    integer(int64) :: i, nbox
    nbox = 0
    !$omp parallel do reduction(+:nbox)
    do i = 1, n
       if (trap_is_box(t(i))) nbox = nbox + 1
    end do
    !$omp end parallel do
    call ClearLayer(L)
    if (n == 0) return
    if (nbox == n) then
       allocate(L%layer_boxes(n))
       !$omp parallel do
       do i = 1, n
          L%layer_boxes(i)%x1 = int(t(i)%l%ax, kind(L%layer_boxes(i)%x1))
          L%layer_boxes(i)%x2 = int(t(i)%r%ax, kind(L%layer_boxes(i)%x2))
          L%layer_boxes(i)%y1 = int(t(i)%y1, kind(L%layer_boxes(i)%y1))
          L%layer_boxes(i)%y2 = int(t(i)%y2, kind(L%layer_boxes(i)%y2))
       end do
       !$omp end parallel do
       L%n_used = n; L%n_alloc = n
    else
       allocate(L%traps(n))
       L%traps(1:n) = t(1:n)
       L%n_traps = n
    end if
    L%layerState = LAYER_STATE_NONE
  end subroutine store_traps

  subroutine store_raw(L, np, poff, px, py)
    type(Layer), intent(inout) :: L
    integer(int64), intent(in) :: np, poff(:), px(:), py(:)
    integer(int64) :: nv, j
    if (L%n_traps == 0) return                   !> demoted to boxes: nothing slanted left
    nv = poff(np + 1) - 1
    allocate(L%raw_off(np + 1), L%raw_x(max(nv, 1_int64)), L%raw_y(max(nv, 1_int64)))
    L%raw_off(1:np + 1) = poff(1:np + 1)
    !$omp parallel do
    do j = 1, nv
       L%raw_x(j) = int(px(j), int32); L%raw_y(j) = int(py(j), int32)
    end do
    !$omp end parallel do
    L%raw_np = np
  end subroutine store_raw

  !> Exact polygons of a layer: boxes as rectangles, trapezoids as their lattice polygons.
  subroutine layer_polys(L, np, poff, px, py)
    type(Layer), intent(inout), target :: L
    integer(int64), intent(out) :: np
    integer(int64), allocatable, intent(out) :: poff(:), px(:), py(:)
    integer(int64) :: nb, nt, ntv, i, b0, v0
    integer(int64), allocatable :: tpo(:), tx(:), ty(:)
    integer(int32), allocatable :: rec(:, :)
    integer(c_int64_t) :: st(8)
    type(c_ptr) :: h
    if (L%raw_np > 0) then                       !> loaded any-angle layer: its source polygons
       np = L%raw_np
       ntv = L%raw_off(np + 1) - 1
       allocate(poff(np + 1), px(max(ntv, 1_int64)), py(max(ntv, 1_int64)))
       poff(1:np + 1) = L%raw_off(1:np + 1)
       !$omp parallel do
       do i = 1, ntv
          px(i) = L%raw_x(i); py(i) = L%raw_y(i)
       end do
       !$omp end parallel do
       return
    end if
    call LayerExpand(L)
    nb = L%n_used
    nt = 0; ntv = 0
    if (L%n_traps > 0) then
       call TrapRecords(L%traps, L%n_traps, rec)
       st = 0
       h = ldb_trapset_polys(L%n_traps, rec, st)
       deallocate(rec)
       if (st(7) /= 0) write(*, '(A,I0)') 'ERROR: anyangle: polygon export anomalies=', st(7)
       nt = ldb_trapset_polys_count(h, ntv)
       allocate(tpo(nt + 1), tx(max(ntv, 1_int64)), ty(max(ntv, 1_int64)))
       call ldb_trapset_polys_get(h, tpo, tx, ty)
       call ldb_trapset_polys_free(h)
    end if
    np = nb + nt
    allocate(poff(np + 1), px(max(4*nb + ntv, 1_int64)), py(max(4*nb + ntv, 1_int64)))
    !$omp parallel do private(b0)
    do i = 1, nb
       b0 = 4*(i - 1)
       poff(i) = b0 + 1
       px(b0 + 1) = L%layer_boxes(i)%x1; py(b0 + 1) = L%layer_boxes(i)%y1
       px(b0 + 2) = L%layer_boxes(i)%x2; py(b0 + 2) = L%layer_boxes(i)%y1
       px(b0 + 3) = L%layer_boxes(i)%x2; py(b0 + 3) = L%layer_boxes(i)%y2
       px(b0 + 4) = L%layer_boxes(i)%x1; py(b0 + 4) = L%layer_boxes(i)%y2
    end do
    !$omp end parallel do
    v0 = 4*nb
    do i = 1, nt
       poff(nb + i) = v0 + tpo(i)
    end do
    poff(np + 1) = v0 + ntv + 1
    if (nt > 0) then
       px(v0 + 1:v0 + ntv) = tx(1:ntv); py(v0 + 1:v0 + ntv) = ty(1:ntv)
       deallocate(tpo, tx, ty)
    end if
  end subroutine layer_polys

  ! ------------------------------------------------------------------ topologicals

  !> CTR topological operator -> K_TOPO_* code (commonmodule), -1 when not handled on any-angle
  !> layers (the *_EDGE variants).
  integer(int64) function AnyAngleTopoCode(w) result(c)
    character(len=*), intent(in) :: w
    select case (w)
    case ('INTERACT');      c = 101
    case ('NOT_INTERACT');  c = 102
    case ('CUT');           c = 103
    case ('NOT_CUT');       c = 104
    case ('TOUCH');         c = 105
    case ('NOT_TOUCH');     c = 106
    case ('INSIDE');        c = 107
    case ('NOT_INSIDE');    c = 108
    case ('OUTSIDE');       c = 109
    case ('NOT_OUTSIDE');   c = 110
    case ('EQUAL');         c = 119
    case ('NOT_EQUAL');     c = 120
    case ('DISJOINT');      c = 121
    case ('NOT_DISJOINT');  c = 122
    case ('MEET');          c = 123
    case ('NOT_MEET');      c = 124
    case ('OVERLAPS');      c = 125
    case ('NOT_OVERLAPS');  c = 126
    case ('COVERS');        c = 127
    case ('NOT_COVERS');    c = 128
    case ('COVEREDBY');     c = 129
    case ('NOT_COVEREDBY'); c = 130
    case ('CONTAINS');      c = 131
    case ('NOT_CONTAINS');  c = 132
    case default;           c = -1
    end select
  end function AnyAngleTopoCode

  !> out = the PNUM polygons of A selected by their relation with B (TrapTopoModule; exact).
  subroutine AnyAngleTopo(A, B, out, code, status)
    type(Layer), intent(inout), target :: A, B, out
    integer(int64), intent(in) :: code
    integer, intent(out) :: status
    type(Trapezoid), allocatable :: tA(:), tB(:), sel(:)
    integer(int64) :: nA, nB, nsel
    type(TrapTopoStats) :: st
    status = 0
    call layer_traps(A, tA, nA, status); if (status /= 0) return
    call layer_traps(B, tB, nB, status); if (status /= 0) return
    call TrapTopoSelect(tA, nA, tB, nB, code, sel, nsel, st)
    if (st%anomalies /= 0) then
       write(*, '(A,I0)') 'ERROR: anyangle topological select anomalies=', st%anomalies
       status = 3
    end if
    call store_traps(out, sel, nsel)
    write(*, '(A,I0,A,I0,A,I0,A,I0,A,I0,4(A,F0.3))') 'INFO: anyangle topo op=', code, ' polygons A=', st%polys_a, &
         ' B=', st%polys_b, ' selected=', st%selected, ' pairs=', st%pairs, ' t_pnum=', st%t_pnum, &
         ' t_pairs=', st%t_pairs, ' t_cover=', st%t_cover, ' t_total=', st%t_total
  end subroutine AnyAngleTopo

  !> out = GROW/SHRINK/SIZE of A (exact Minkowski collar + TrapBooleanPolygons).
  subroutine AnyAngleMorph(A, out, dE, dN, dW, dS, is_shrink, status)
    type(Layer), intent(inout), target :: A, out
    integer(int64), intent(in) :: dE, dN, dW, dS
    logical, intent(in) :: is_shrink
    integer, intent(out) :: status
    type(Trapezoid), allocatable :: tA(:), res(:)
    integer(int64) :: nA, nres
    type(TrapMorphStats) :: st
    status = 0
    call layer_traps(A, tA, nA, status); if (status /= 0) return
    call TrapMorph(tA, nA, dE, dN, dW, dS, is_shrink, res, nres, st, status)
    if (status /= 0 .or. st%anomalies /= 0) then
       write(*, '(A,I0,A,I0)') 'ERROR: anyangle morph failed: status=', status, ' anomalies=', st%anomalies
       if (status == 0) status = 3
       call ClearLayer(out)
       return
    end if
    call store_traps(out, res, nres)
    write(*, '(A,L1,4(A,I0),3(A,I0),4(A,F0.3))') 'INFO: anyangle morph shrink=', is_shrink, &
         ' E=', dE, ' N=', dN, ' W=', dW, ' S=', dS, ' loops=', st%loops, ' collar=', st%collar_polys, &
         ' traps=', nres, ' t_loops=', st%t_loops, ' t_collar=', st%t_collar, &
         ' t_bool=', st%t_bool, ' t_total=', st%t_total
  end subroutine AnyAngleMorph

  ! ------------------------------------------------------------------ polygon predicates

  integer(int32) function pred_base_code(w) result(c)
    character(len=*), intent(in) :: w
    select case (trim(w))
    case ('AREA');                                                c = TPRED_AREA
    case ('PERIMETER');                                           c = TPRED_PERIMETER
    case ('VERTEX', 'VERTICES');                                  c = TPRED_VERTEX
    case ('HOLES');                                               c = TPRED_HOLES
    case ('WIDTH');                                               c = TPRED_WIDTH
    case ('LENGTH');                                              c = TPRED_LENGTH
    case ('ASPECT_RATIO');                                        c = TPRED_ASPECT
    case ('DIAMETER');                                            c = TPRED_DIAMETER
    case ('COMPACTNESS', 'CIRCULARITY');                          c = TPRED_COMPACTNESS
    case ('SOLIDITY', 'CONVEXITY');                               c = TPRED_SOLIDITY
    case ('CONVEX_CRITERIA', 'IS_CONVEX', 'CONVEX');              c = TPRED_CONVEX
    case ('CONCAVE_CRITERIA', 'IS_CONCAVE', 'CONCAVE');           c = TPRED_CONCAVE
    case ('IS_RECTANGLE_WITH_ROTATION', 'IS_ROTATED_RECTANGLE');  c = TPRED_IS_RECT_ROT
    case ('IS_SQUARE_WITH_ROTATION', 'IS_ROTATED_SQUARE');        c = TPRED_IS_SQ_ROT
    case ('IS_RECTANGLE');                                        c = TPRED_IS_RECT
    case ('IS_SQUARE');                                           c = TPRED_IS_SQ
    case ('IS_RECTILINEAR', 'IS_ORTHOGONAL');                     c = TPRED_IS_RECTILIN
    case ('IS_45', 'IS_OCTAGONAL');                               c = TPRED_IS_45
    case ('NOTCH');                                               c = TPRED_NOTCH
    case ('ENCLOSE_RECTANGLE');                                   c = TPRED_ENCLOSE_RECT
    case default;                                                 c = TPRED_NONE
    end select
  end function pred_base_code

  integer(int32) function AnyAnglePredCode(w) result(c)
    character(len=*), intent(in) :: w
    character(len=:), allocatable :: s
    integer(int32) :: base
    s = trim(adjustl(w))
    if (len(s) > 4) then
       if (s(1:4) == 'NOT_') then
          base = pred_base_code(s(5:))
          if (base > 0) then
             c = TPRED_NOT_OFFSET + base
             return
          end if
       end if
    end if
    c = pred_base_code(s)
  end function AnyAnglePredCode

  subroutine next_ws_tok(s, pos, tok)
    character(len=*), intent(in) :: s
    integer, intent(inout) :: pos
    character(len=*), intent(out) :: tok
    integer :: n, i0
    tok = ''
    n = len_trim(s)
    do while (pos <= n)
       if (s(pos:pos) /= ' ' .and. s(pos:pos) /= char(9)) exit
       pos = pos + 1
    end do
    if (pos > n) return
    i0 = pos
    do while (pos <= n)
       if (s(pos:pos) == ' ' .or. s(pos:pos) == char(9)) exit
       pos = pos + 1
    end do
    tok = s(i0:pos - 1)
  end subroutine next_ws_tok

  !> Parse trailing tokens of `[NOT] OP nothing [<cmp> <val> [<cmp2> <val2>] | <lo> <hi> | <W> <H>]`.
  !> Uses whitespace tokenization (not Fortran list-directed I/O) so `/=` is not truncated by `/`.
  subroutine AnyAngleParsePredArgs(rest, pred, cmp, v1, v2, status)
    character(len=*), intent(in) :: rest
    integer(int32), intent(in) :: pred
    integer(int32), intent(out) :: cmp
    real(real64), intent(out) :: v1, v2
    integer, intent(out), optional :: status
    character(len=128) :: tok, t1, t2, t3, t4
    integer :: pos, ios, st
    integer(int32) :: base_pred, c1, c2
    real(real64) :: x1, x2

    cmp = TCMP_NONE; v1 = 0.0_real64; v2 = 0.0_real64; st = 0
    base_pred = merge(pred - TPRED_NOT_OFFSET, pred, pred > TPRED_NOT_OFFSET)

    pos = 1
    call next_ws_tok(rest, pos, tok)  ! OP or NOT
    if (trim(tok) == 'NOT' .or. trim(tok) == 'not') then
       call next_ws_tok(rest, pos, tok) ! OP
    end if
    call next_ws_tok(rest, pos, tok)  ! rhs2 ('nothing')
    call next_ws_tok(rest, pos, t1)
    call next_ws_tok(rest, pos, t2)
    call next_ws_tok(rest, pos, t3)
    call next_ws_tok(rest, pos, t4)

    if (base_pred == TPRED_ENCLOSE_RECT) then
       if (len_trim(t1) == 0) then
          st = 1
       else
          read(t1, *, iostat=ios) v1
          if (ios /= 0) then
             st = 1
          else if (len_trim(t2) > 0) then
             read(t2, *, iostat=ios) v2
             if (ios /= 0) st = 1
          else
             v2 = v1
          end if
       end if
       if (present(status)) status = st
       return
    end if

    if (len_trim(t1) == 0) then
       if (present(status)) status = st
       return
    end if

    c1 = parse_cmp_tok(t1)
    if (c1 /= TCMP_NONE) then
       if (len_trim(t2) == 0) then
          st = 1
       else
          read(t2, *, iostat=ios) x1
          if (ios /= 0) then
             st = 1
          else if (len_trim(t3) > 0) then
             c2 = parse_cmp_tok(t3)
             if (c2 == TCMP_NONE .or. len_trim(t4) == 0) then
                st = 1
             else
                read(t4, *, iostat=ios) x2
                if (ios /= 0) then
                   st = 1
                else if ((c1 == TCMP_GT .or. c1 == TCMP_GE) .and. (c2 == TCMP_LT .or. c2 == TCMP_LE)) then
                   v1 = x1; v2 = x2
                   if (c1 == TCMP_GT .and. c2 == TCMP_LT) cmp = TCMP_RANGE_OO
                   if (c1 == TCMP_GT .and. c2 == TCMP_LE) cmp = TCMP_RANGE_OC
                   if (c1 == TCMP_GE .and. c2 == TCMP_LT) cmp = TCMP_RANGE_CO
                   if (c1 == TCMP_GE .and. c2 == TCMP_LE) cmp = TCMP_RANGE
                else if ((c2 == TCMP_GT .or. c2 == TCMP_GE) .and. (c1 == TCMP_LT .or. c1 == TCMP_LE)) then
                   v1 = x2; v2 = x1
                   if (c2 == TCMP_GT .and. c1 == TCMP_LT) cmp = TCMP_RANGE_OO
                   if (c2 == TCMP_GT .and. c1 == TCMP_LE) cmp = TCMP_RANGE_OC
                   if (c2 == TCMP_GE .and. c1 == TCMP_LT) cmp = TCMP_RANGE_CO
                   if (c2 == TCMP_GE .and. c1 == TCMP_LE) cmp = TCMP_RANGE
                else
                   st = 1
                end if
             end if
          else
             cmp = c1; v1 = x1
          end if
       end if
    else
       ! Bare numeric values: two values -> closed range [v1, v2]; one value -> == v1
       read(t1, *, iostat=ios) v1
       if (ios /= 0) then
          st = 1
       else if (len_trim(t2) > 0) then
          read(t2, *, iostat=ios) v2
          if (ios /= 0) then
             st = 1
          else
             cmp = TCMP_RANGE
          end if
       else
          cmp = TCMP_EQ
       end if
    end if
    if (present(status)) status = st
  end subroutine AnyAngleParsePredArgs

  pure integer(int32) function parse_cmp_tok(t) result(c)
    character(len=*), intent(in) :: t
    select case (trim(t))
    case ('>');         c = TCMP_GT
    case ('>=');        c = TCMP_GE
    case ('<');         c = TCMP_LT
    case ('<=');        c = TCMP_LE
    case ('==', '=');   c = TCMP_EQ
    case ('!=', '/=');  c = TCMP_NE
    case default;       c = TCMP_NONE
    end select
  end function parse_cmp_tok

  subroutine AnyAnglePredicate(A, out, pred, cmp, v1, v2, prec, status)
    type(Layer), intent(inout), target :: A, out
    integer(int32), intent(in) :: pred, cmp
    real(real64), intent(in) :: v1, v2, prec
    integer, intent(out) :: status
    type(Trapezoid), allocatable :: tA(:), sel(:)
    integer(int64) :: nA, nsel
    type(TrapPredStats) :: st
    status = 0
    call layer_traps(A, tA, nA, status); if (status /= 0) return
    call TrapPredSelect(tA, nA, pred, cmp, v1, v2, prec, sel, nsel, st)
    if (st%anomalies /= 0) then
       write(*, '(A,I0)') 'ERROR: anyangle predicate anomalies=', st%anomalies
       status = 3
    end if
    call store_traps(out, sel, nsel)
    write(*, '(A,I0,A,I0,A,I0,A,I0,A,I0,3(A,F0.3))') 'INFO: poly pred=', pred, ' cmp=', cmp, &
         ' polys=', st%polys, ' selected=', st%selected, ' traps=', nsel, &
         ' t_pnum=', st%t_pnum, ' t_loops=', st%t_loops, ' t_total=', st%t_total
  end subroutine AnyAnglePredicate

  !> Canonical trapezoids of a layer: its own (any-angle) or its boxes merged exactly.
  subroutine layer_traps(L, t, n, status)
    type(Layer), intent(inout), target :: L
    type(Trapezoid), allocatable, intent(out) :: t(:)
    integer(int64), intent(out) :: n
    integer, intent(out) :: status
    integer(int64) :: np
    integer(int64), allocatable :: poff(:), px(:), py(:)
    type(TrapMergeStats) :: st
    status = 0
    if (L%n_traps > 0) then
       n = L%n_traps
       allocate(t(n)); t(1:n) = L%traps(1:n)
       return
    end if
    if (L%n_used == 0 .and. .not. L%is_compact) then
       n = 0; allocate(t(1)); return
    end if
    call LayerExpand(L)
    np = L%n_used
    allocate(poff(np + 1), px(max(4*np, 1_int64)), py(max(4*np, 1_int64)))
    call box_polys(L, np, poff, px, py)
    st = TrapMergeStats()
    call TrapMergePolygons(np, poff, px, py, t, n, st, status)
    if (status /= 0 .or. st%anomalies /= 0) then
       write(*, '(A,I0,A,I0)') 'ERROR: anyangle: box layer merge failed: status=', status, ' anomalies=', st%anomalies
       if (status == 0) status = 3
    end if
  end subroutine layer_traps

  subroutine box_polys(L, nb, poff, px, py)
    type(Layer), intent(in) :: L
    integer(int64), intent(in) :: nb
    integer(int64), intent(inout) :: poff(:), px(:), py(:)
    integer(int64) :: i, b0
    !$omp parallel do private(b0)
    do i = 1, nb
       b0 = 4*(i - 1)
       poff(i) = b0 + 1
       px(b0 + 1) = L%layer_boxes(i)%x1; py(b0 + 1) = L%layer_boxes(i)%y1
       px(b0 + 2) = L%layer_boxes(i)%x2; py(b0 + 2) = L%layer_boxes(i)%y1
       px(b0 + 3) = L%layer_boxes(i)%x2; py(b0 + 3) = L%layer_boxes(i)%y2
       px(b0 + 4) = L%layer_boxes(i)%x1; py(b0 + 4) = L%layer_boxes(i)%y2
    end do
    !$omp end parallel do
    poff(nb + 1) = 4*nb + 1
  end subroutine box_polys

  ! ------------------------------------------------------------------ measure / digest

  subroutine AnyAngleMeasure(L, m)
    type(Layer), intent(inout), target :: L
    type(LayerMeasures), intent(out) :: m
    type(TrapPolys) :: P
    integer(int64) :: a2
    call TrapPNum(L%traps, L%n_traps, P)
    call TrapPolyOutline(L%traps, L%n_traps, P)
    m%boxes = L%n_traps
    m%polygons = P%np
    m%holes = sum(P%holes(1:P%np))
    m%vertices = sum(P%nvert(1:P%np))
    a2 = sum(P%area2(1:P%np))
    m%area = a2 / 2
    m%perimeter = nint(sum(P%perim(1:P%np)), int64)
    m%euler4 = 4*(m%polygons - m%holes)
    if (P%anomalies + P%outline_anomalies /= 0) write(*, '(A,I0)') 'ERROR: anyangle measure anomalies=', &
         P%anomalies + P%outline_anomalies
  end subroutine AnyAngleMeasure

  !> Order-independent content hash of the trapezoid set (canonical, so equal regions hash equal).
  integer(int64) function AnyAngleDigest(L) result(h)
    type(Layer), intent(in) :: L
    integer(int64) :: i
    h = 0
    !$omp parallel do reduction(+:h)
    do i = 1, L%n_traps
       h = h + trap_hash(L%traps(i))
    end do
    !$omp end parallel do
  end function AnyAngleDigest

end module AnyAngleOpsModule
