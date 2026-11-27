! File   : trap_morph.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Exact any-angle morphological operations (GROW / SHRINK / SIZE) on canonical
!          trapezoid sets (TrapezoidModule).
!
! Algorithm (Minkowski collar duality):
!   1. Extract unbridged oriented boundary loops of A (outer CCW, holes CW) via TrapPNum +
!      ldb_trapset_loops (db/trap_out.cpp). Every vertex lies on the integer lattice.
!   2. Build the boundary collar C in parallel over loops:
!      - Every boundary edge e = (p -> q) emits its offset quad (p, q, q+o, p+o) where
!        o = nint(d * n_out) (GROW) or nint(d * n_in) (SHRINK), preserving the exact slope.
!      - Every diverging corner (convex for GROW, reflex for SHRINK) emits a miter quad
!        (v, q1, m, q2) when turning angle <= 90 deg, or an octagon bevel 5-gon cut at
!        sqrt(2)*d along the bisector (matching KLayout Region.sized(d) mode=2).
!      - Anisotropic 4-directional (E, N, W, S) uses the exact support function of [-W,E]x[-S,N].
!   3. Resolve exact topology via TrapBooleanPolygons:
!      - GROW(A, d)   = A OR  C   (guarantees A <= GROW(A, d), zero self-intersection artifacts)
!      - SHRINK(A, d) = A NOT C   (guarantees SHRINK(A, d) <= A, pinches/disappearances exact)
!
! Non-polymorphic POD/SoA throughout (no class(...), no allocatable components in hot loops).
module TrapMorphModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding
  use omp_lib
  use TrapezoidModule
  use TrapMergeModule
  use TrapPNumModule
  implicit none
  private

  public :: TrapMorph, TrapMorphStats

  type :: TrapMorphStats
     integer(int64) :: loops = 0, collar_polys = 0, out_traps = 0, anomalies = 0
     real(real64) :: t_loops = 0.0_real64, t_collar = 0.0_real64, t_bool = 0.0_real64, t_total = 0.0_real64
  end type TrapMorphStats

  interface
     function ldb_trapset_loops(n, rec, lab, np, stats) result(h) bind(c, name='ldb_trapset_loops')
       import :: c_ptr, c_int64_t, Trapezoid
       integer(c_int64_t), value :: n, np
       type(Trapezoid), intent(in) :: rec(*)
       integer(c_int64_t), intent(in) :: lab(*)
       integer(c_int64_t), intent(out) :: stats(*)
       type(c_ptr) :: h
     end function ldb_trapset_loops
     function ldb_trapset_polys(n, rec, stats) result(h) bind(c, name='ldb_trapset_polys')
       import :: c_ptr, c_int64_t, Trapezoid
       integer(c_int64_t), value :: n
       type(Trapezoid), intent(in) :: rec(*)
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

  subroutine TrapMorph(traps, ntrap, dE, dN, dW, dS, is_shrink, out_traps, out_n, st, status)
    type(Trapezoid), intent(in) :: traps(:)
    integer(int64), intent(in) :: ntrap, dE, dN, dW, dS
    logical, intent(in) :: is_shrink
    type(Trapezoid), allocatable, intent(out) :: out_traps(:)
    integer(int64), intent(out) :: out_n
    type(TrapMorphStats), intent(out) :: st
    integer, intent(out) :: status

    type(TrapPolys) :: P
    integer(c_int64_t) :: cstat(8)
    type(c_ptr) :: hloops, hpolys
    integer(int64) :: nloop, nlvert, npA, nvA, npC, nvC, l
    integer(int64), allocatable :: loff(:), lx(:), ly(:), poA(:), xA(:), yA(:)
    integer(int64), allocatable :: cnt_p(:), cnt_v(:), off_p(:), off_v(:)
    integer(int64), allocatable :: poC(:), xC(:), yC(:)
    logical :: iso
    integer(int32) :: ops(1)
    type(TrapList) :: res(1)
    type(TrapMergeStats) :: bst
    real(real64) :: t0, t1, t2

    st = TrapMorphStats()
    status = 0
    out_n = 0
    t0 = omp_get_wtime()

    if (ntrap <= 0) then
       allocate(out_traps(0))
       st%t_total = omp_get_wtime() - t0
       return
    end if
    if (dE == 0 .and. dN == 0 .and. dW == 0 .and. dS == 0) then
       allocate(out_traps(ntrap))
       out_traps(1:ntrap) = traps(1:ntrap)
       out_n = ntrap
       st%out_traps = ntrap
       st%t_total = omp_get_wtime() - t0
       return
    end if
    if (dE < 0 .or. dN < 0 .or. dW < 0 .or. dS < 0) then
       status = 2
       allocate(out_traps(0))
       return
    end if

    ! 1. Canonical PNUM + unbridged oriented boundary loops + exact polygons of A (zero-copy Trapezoid)
    call TrapPNum(traps, ntrap, P)
    st%anomalies = st%anomalies + P%anomalies
    cstat = 0
    hloops = ldb_trapset_loops(ntrap, traps, P%lab, P%np, cstat)
    st%anomalies = st%anomalies + cstat(1)
    nloop = ldb_trapset_polys_count(hloops, nlvert)
    allocate(loff(nloop + 1), lx(max(nlvert, 1_int64)), ly(max(nlvert, 1_int64)))
    call ldb_trapset_polys_get(hloops, loff, lx, ly)
    call ldb_trapset_polys_free(hloops)

    cstat = 0
    hpolys = ldb_trapset_polys(ntrap, traps, cstat)
    st%anomalies = st%anomalies + cstat(7)
    npA = ldb_trapset_polys_count(hpolys, nvA)
    allocate(poA(npA + 1), xA(max(nvA, 1_int64)), yA(max(nvA, 1_int64)))
    call ldb_trapset_polys_get(hpolys, poA, xA, yA)
    call ldb_trapset_polys_free(hpolys)
    t1 = omp_get_wtime()
    st%t_loops = t1 - t0
    st%loops = nloop

    ! 2. Build boundary collar polygons (2-pass count + parallel fill)
    iso = (dE == dN .and. dN == dW .and. dW == dS)
    allocate(cnt_p(max(nloop, 1_int64)), cnt_v(max(nloop, 1_int64)))
    cnt_p = 0; cnt_v = 0
    !$omp parallel do schedule(dynamic, 256)
    do l = 1, nloop
       call loop_collar_count(lx, ly, loff(l), loff(l + 1) - 1, dE, dN, dW, dS, iso, is_shrink, &
            cnt_p(l), cnt_v(l))
    end do
    !$omp end parallel do

    allocate(off_p(nloop + 1), off_v(nloop + 1))
    off_p(1) = 0; off_v(1) = 0
    do l = 1, nloop
       off_p(l + 1) = off_p(l) + cnt_p(l)
       off_v(l + 1) = off_v(l) + cnt_v(l)
    end do
    npC = off_p(nloop + 1)
    nvC = off_v(nloop + 1)
    allocate(poC(npC + 1), xC(max(nvC, 1_int64)), yC(max(nvC, 1_int64)))
    if (npC > 0) then
       !$omp parallel do schedule(dynamic, 256)
       do l = 1, nloop
          if (cnt_p(l) > 0) call loop_collar_fill(lx, ly, loff(l), loff(l + 1) - 1, dE, dN, dW, dS, &
               iso, is_shrink, off_p(l), off_v(l), poC, xC, yC)
       end do
       !$omp end parallel do
    end if
    poC(npC + 1) = nvC + 1
    deallocate(loff, lx, ly, cnt_p, cnt_v, off_p, off_v)
    t2 = omp_get_wtime()
    st%t_collar = t2 - t1
    st%collar_polys = npC

    ! 3. Exact Boolean: GROW = A OR C, SHRINK = A NOT C
    if (is_shrink) then
       ops(1) = TOP_NOT
    else
       ops(1) = TOP_OR
    end if
    bst = TrapMergeStats()
    call TrapBooleanPolygons(npA, poA, xA, yA, npC, poC, xC, yC, 1, ops, res, bst, status)
    deallocate(poA, xA, yA, poC, xC, yC)
    st%anomalies = st%anomalies + bst%anomalies
    st%t_bool = omp_get_wtime() - t2
    st%t_total = omp_get_wtime() - t0

    if (status /= 0) then
       allocate(out_traps(0))
       out_n = 0
       return
    end if
    out_n = res(1)%n
    st%out_traps = out_n
    if (out_n > 0) then
       call move_alloc(res(1)%t, out_traps)
    else
       allocate(out_traps(0))
    end if
  end subroutine TrapMorph

  pure subroutine edge_offset(dx, dy, dE, dN, dW, dS, iso, is_shrink, ox, oy, nx, ny, elen)
    integer(int64), intent(in) :: dx, dy, dE, dN, dW, dS
    logical, intent(in) :: iso, is_shrink
    integer(int64), intent(out) :: ox, oy
    real(real64), intent(out) :: nx, ny, elen
    real(real64) :: sgn, d
    elen = hypot(real(dx, real64), real(dy, real64))
    if (elen <= 0.0_real64) then
       ox = 0; oy = 0; nx = 0.0_real64; ny = 0.0_real64
       return
    end if
    sgn = merge(-1.0_real64, 1.0_real64, is_shrink)
    nx = sgn * real(dy, real64) / elen
    ny = sgn * real(-dx, real64) / elen
    if (iso) then
       d = real(dE, real64)
       ox = nint(d * nx, int64)
       oy = nint(d * ny, int64)
    else if (.not. is_shrink) then
       if (dy > 0) then
          ox = dE
       else if (dy < 0) then
          ox = -dW
       else
          ox = 0
       end if
       if (dx < 0) then
          oy = dN
       else if (dx > 0) then
          oy = -dS
       else
          oy = 0
       end if
    else
       if (dy > 0) then
          ox = -dE
       else if (dy < 0) then
          ox = dW
       else
          ox = 0
       end if
       if (dx < 0) then
          oy = -dN
       else if (dx > 0) then
          oy = dS
       else
          oy = 0
       end if
    end if
  end subroutine edge_offset

  pure subroutine loop_collar_count(lx, ly, a, b, dE, dN, dW, dS, iso, is_shrink, np, nv)
    integer(int64), intent(in) :: lx(:), ly(:), a, b, dE, dN, dW, dS
    logical, intent(in) :: iso, is_shrink
    integer(int64), intent(out) :: np, nv
    integer(int64) :: m, j, jp, jm, dx1, dy1, dx2, dy2, ox1, oy1, ox2, oy2
    real(real64) :: nx1, ny1, l1, nx2, ny2, l2, cross, dot, sgn
    np = 0; nv = 0
    m = b - a + 1
    if (m < 3) return
    sgn = merge(-1.0_real64, 1.0_real64, is_shrink)
    do j = 0, m - 1
       jp = mod(j + 1, m)
       jm = mod(j + m - 1, m)
       dx2 = lx(a + jp) - lx(a + j); dy2 = ly(a + jp) - ly(a + j)
       call edge_offset(dx2, dy2, dE, dN, dW, dS, iso, is_shrink, ox2, oy2, nx2, ny2, l2)
       if (ox2 /= 0 .or. oy2 /= 0) then
          np = np + 1; nv = nv + 4
       end if
       dx1 = lx(a + j) - lx(a + jm); dy1 = ly(a + j) - ly(a + jm)
       cross = real(dx1, real64) * real(dy2, real64) - real(dy1, real64) * real(dx2, real64)
       if (sgn * cross > 0.0_real64) then
          if (iso) then
             call edge_offset(dx1, dy1, dE, dN, dW, dS, iso, is_shrink, ox1, oy1, nx1, ny1, l1)
             dot = nx1 * nx2 + ny1 * ny2
             if (dot >= -1.0e-12_real64) then
                np = np + 1; nv = nv + 4
             else
                np = np + 1; nv = nv + 5
             end if
          else
             if ((dE + dW > 0) .and. (dN + dS > 0)) then
                np = np + 1; nv = nv + 4
             end if
          end if
       else if (iso) then
          call edge_offset(dx1, dy1, dE, dN, dW, dS, iso, is_shrink, ox1, oy1, nx1, ny1, l1)
          if (ox1 /= ox2 .or. oy1 /= oy2) then
             np = np + 1; nv = nv + 3
          end if
       end if
    end do
  end subroutine loop_collar_count

  pure subroutine loop_collar_fill(lx, ly, a, b, dE, dN, dW, dS, iso, is_shrink, &
                                   pbase, vbase, poC, xC, yC)
    integer(int64), intent(in) :: lx(:), ly(:), a, b, dE, dN, dW, dS, pbase, vbase
    logical, intent(in) :: iso, is_shrink
    integer(int64), intent(inout) :: poC(:), xC(:), yC(:)
    integer(int64) :: m, j, jp, jm, ip, iv, vx, vy, qx, qy
    integer(int64) :: dx1, dy1, dx2, dy2, ox1, oy1, ox2, oy2
    integer(int64) :: q1x, q1y, q2x, q2y, mx, my, p1x, p1y, p2x, p2y, bx1, bx2, by1, by2
    real(real64) :: nx1, ny1, l1, nx2, ny2, l2, cross, dot, sgn, det, t
    real(real64) :: bx, by, blen, ubx, uby, dcut, tx1, ty1, tx2, ty2, den1, den2, s1, s2

    m = b - a + 1
    if (m < 3) return
    sgn = merge(-1.0_real64, 1.0_real64, is_shrink)
    ip = pbase
    iv = vbase
    do j = 0, m - 1
       jp = mod(j + 1, m)
       jm = mod(j + m - 1, m)
       vx = lx(a + j);  vy = ly(a + j)
       qx = lx(a + jp); qy = ly(a + jp)
       dx2 = qx - vx; dy2 = qy - vy
       call edge_offset(dx2, dy2, dE, dN, dW, dS, iso, is_shrink, ox2, oy2, nx2, ny2, l2)
       if (ox2 /= 0 .or. oy2 /= 0) then
          ip = ip + 1; poC(ip) = iv + 1
          xC(iv + 1) = vx;       yC(iv + 1) = vy
          xC(iv + 2) = qx;       yC(iv + 2) = qy
          xC(iv + 3) = qx + ox2; yC(iv + 3) = qy + oy2
          xC(iv + 4) = vx + ox2; yC(iv + 4) = vy + oy2
          iv = iv + 4
       end if
       dx1 = vx - lx(a + jm); dy1 = vy - ly(a + jm)
       cross = real(dx1, real64) * real(dy2, real64) - real(dy1, real64) * real(dx2, real64)
       if (sgn * cross > 0.0_real64) then
          if (iso) then
             call edge_offset(dx1, dy1, dE, dN, dW, dS, iso, is_shrink, ox1, oy1, nx1, ny1, l1)
             q1x = vx + ox1; q1y = vy + oy1
             q2x = vx + ox2; q2y = vy + oy2
             dot = nx1 * nx2 + ny1 * ny2
             if (dot >= -1.0e-12_real64) then
                det = cross
                t = (real(q2x - q1x, real64) * real(dy2, real64) - &
                     real(q2y - q1y, real64) * real(dx2, real64)) / det
                mx = nint(real(q1x, real64) + t * real(dx1, real64), int64)
                my = nint(real(q1y, real64) + t * real(dy1, real64), int64)
                ip = ip + 1; poC(ip) = iv + 1
                xC(iv + 1) = vx;  yC(iv + 1) = vy
                xC(iv + 2) = q1x; yC(iv + 2) = q1y
                xC(iv + 3) = mx;  yC(iv + 3) = my
                xC(iv + 4) = q2x; yC(iv + 4) = q2y
                iv = iv + 4
             else
                ! Acute corner (> 90 deg turn): octagon bevel cut at sqrt(2)*d along bisector
                bx = nx1 + nx2; by = ny1 + ny2
                blen = hypot(bx, by)
                tx1 = real(dx1, real64) / l1; ty1 = real(dy1, real64) / l1
                tx2 = real(dx2, real64) / l2; ty2 = real(dy2, real64) / l2
                if (blen > 1.0e-9_real64) then
                   ubx = bx / blen; uby = by / blen
                else
                   bx = tx1 - tx2; by = ty1 - ty2
                   blen = max(hypot(bx, by), 1.0e-12_real64)
                   ubx = bx / blen; uby = by / blen
                end if
                dcut = sqrt(2.0_real64) * real(dE, real64)
                den1 = tx1 * ubx + ty1 * uby
                den2 = -(tx2 * ubx + ty2 * uby)
                if (den1 > 1.0e-9_real64) then
                   s1 = max(0.0_real64, (dcut - (real(q1x - vx, real64) * ubx + real(q1y - vy, real64) * uby)) / den1)
                else
                   s1 = 0.0_real64
                end if
                if (den2 > 1.0e-9_real64) then
                   s2 = max(0.0_real64, (dcut - (real(q2x - vx, real64) * ubx + real(q2y - vy, real64) * uby)) / den2)
                else
                   s2 = 0.0_real64
                end if
                p1x = nint(real(q1x, real64) + s1 * tx1, int64)
                p1y = nint(real(q1y, real64) + s1 * ty1, int64)
                p2x = nint(real(q2x, real64) - s2 * tx2, int64)
                p2y = nint(real(q2y, real64) - s2 * ty2, int64)
                ip = ip + 1; poC(ip) = iv + 1
                xC(iv + 1) = vx;  yC(iv + 1) = vy
                xC(iv + 2) = q1x; yC(iv + 2) = q1y
                xC(iv + 3) = p1x; yC(iv + 3) = p1y
                xC(iv + 4) = p2x; yC(iv + 4) = p2y
                xC(iv + 5) = q2x; yC(iv + 5) = q2y
                iv = iv + 5
             end if
          else if ((dE + dW > 0) .and. (dN + dS > 0)) then
             if (.not. is_shrink) then
                bx1 = vx - dW; bx2 = vx + dE; by1 = vy - dS; by2 = vy + dN
             else
                bx1 = vx - dE; bx2 = vx + dW; by1 = vy - dN; by2 = vy + dS
             end if
             ip = ip + 1; poC(ip) = iv + 1
             xC(iv + 1) = bx1; yC(iv + 1) = by1
             xC(iv + 2) = bx2; yC(iv + 2) = by1
             xC(iv + 3) = bx2; yC(iv + 3) = by2
             xC(iv + 4) = bx1; yC(iv + 4) = by2
             iv = iv + 4
          end if
       else if (iso) then
          call edge_offset(dx1, dy1, dE, dN, dW, dS, iso, is_shrink, ox1, oy1, nx1, ny1, l1)
          if (ox1 /= ox2 .or. oy1 /= oy2) then
             ip = ip + 1; poC(ip) = iv + 1
             xC(iv + 1) = vx;       yC(iv + 1) = vy
             xC(iv + 2) = vx + ox1; yC(iv + 2) = vy + oy1
             xC(iv + 3) = vx + ox2; yC(iv + 3) = vy + oy2
             iv = iv + 3
          end if
       end if
    end do
  end subroutine loop_collar_fill

end module TrapMorphModule
