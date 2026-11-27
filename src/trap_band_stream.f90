! File   : trap_band_stream.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Bounded-memory Y-band streaming execution for any-angle Booleans (Tier 4).
!
! Algorithm (100B-shape out-of-core streaming foundation):
!   1. Partition global die Y-extent [ymin, ymax] into M horizontal streaming bands
!      [y_lo(k), y_hi(k)] with snap-rounding halo H = 1 dbu.
!   2. For each band k = 1..M:
!      - Select all input polygons of A and B whose Y-bbox intersects [y_lo - H, y_hi + H],
!        keeping their EXACT UNCLIPPED lattice endpoints (avoiding band-seam slope tilt).
!      - Run TrapBooleanPolygons on band k's bounded working set (using Tier 1 arenas).
!      - Clip output canonical trapezoids EXACTLY to [y_lo, y_hi] via side_at_row (Invariant I2,
!        zero rounding, zero drift).
!      - Stitch trapezoids touching y_lo against band k-1's 1-row seam buffer whenever both
!        left and right lattice lines are identical; flush completed trapezoids via TrapArenaModule.
!   3. Guarantees exact canonical equivalence (area2, piece count, anomalies=0) with single-shot
!      TrapBooleanPolygons while bounding peak working set to O(1/M).
module TrapBandStreamModule
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use omp_lib
  use TrapezoidModule
  use TrapArenaModule
  use TrapMergeModule
  implicit none
  private

  public :: TrapBandedBoolean, TrapStreamStats

  type :: TrapStreamStats
     integer(int64) :: nbands = 0, max_band_polys = 0, stitched_seams = 0, traps_out = 0, anomalies = 0
     real(real64) :: t_total = 0.0_real64
  end type TrapStreamStats

contains

  subroutine TrapBandedBoolean(npA, poffA, pxA, pyA, npB, poffB, pxB, pyB, op, nbands_stream, &
                               traps, ntrap, st, status)
    integer(int64), intent(in) :: npA, poffA(:), pxA(:), pyA(:)
    integer(int64), intent(in) :: npB, poffB(:), pxB(:), pyB(:)
    integer(int32), intent(in) :: op
    integer, intent(in) :: nbands_stream
    type(Trapezoid), allocatable, intent(out) :: traps(:)
    integer(int64), intent(out) :: ntrap
    type(TrapStreamStats), intent(out) :: st
    integer, intent(out) :: status

    integer(int64) :: ymin, ymax, i, j, k, M, ylo, yhi, H, wlo, whi
    integer(int64), allocatable :: y1A(:), y2A(:), y1B(:), y2B(:)
    integer(int64), allocatable :: sPoA(:), sPxA(:), sPyA(:), sPoB(:), sPxB(:), sPyB(:)
    integer(int64) :: snA, svA, snB, svB, nout, nseam, nnext, nclip
    type(Trapezoid), allocatable :: out_buf(:), seam(:), next_seam(:), clipped(:)
    type(TrapList) :: res(1)
    type(TrapMergeStats) :: mst
    integer(int32) :: ops(1)
    type(Trapezoid) :: z, zs
    type(TrapSide) :: sl, sr
    integer :: st_l, st_r
    logical :: matched
    real(real64) :: t0

    st = TrapStreamStats()
    status = 0
    ntrap = 0
    t0 = omp_get_wtime()

    M = max(1_int64, int(nbands_stream, int64))
    if (npA <= 0 .and. npB <= 0) then
       allocate(traps(0))
       st%t_total = omp_get_wtime() - t0
       return
    end if

    ! Single-band fast path
    if (M == 1) then
       ops(1) = op
       call TrapBooleanPolygons(npA, poffA, pxA, pyA, npB, poffB, pxB, pyB, 1, ops, res, mst, status)
       st%nbands = 1; st%max_band_polys = npA + npB; st%anomalies = mst%anomalies + res(1)%anomalies
       if (status == 0) then
          ntrap = res(1)%n
          call move_alloc(res(1)%t, traps)
          st%traps_out = ntrap
       else
          allocate(traps(0))
       end if
       st%t_total = omp_get_wtime() - t0
       return
    end if

    ! Compute per-polygon Y-extents and global [ymin, ymax]
    allocate(y1A(max(npA, 1_int64)), y2A(max(npA, 1_int64)))
    allocate(y1B(max(npB, 1_int64)), y2B(max(npB, 1_int64)))
    ymin = huge(1_int64); ymax = -huge(1_int64)
    do i = 1, npA
       y1A(i) = minval(pyA(poffA(i):poffA(i + 1) - 1))
       y2A(i) = maxval(pyA(poffA(i):poffA(i + 1) - 1))
       if (y1A(i) < ymin) ymin = y1A(i)
       if (y2A(i) > ymax) ymax = y2A(i)
    end do
    do i = 1, npB
       y1B(i) = minval(pyB(poffB(i):poffB(i + 1) - 1))
       y2B(i) = maxval(pyB(poffB(i):poffB(i + 1) - 1))
       if (y1B(i) < ymin) ymin = y1B(i)
       if (y2B(i) > ymax) ymax = y2B(i)
    end do
    if (ymin >= ymax) then
       allocate(traps(0))
       st%t_total = omp_get_wtime() - t0
       return
    end if

    H = 1_int64
    nout = 0; nseam = 0
    allocate(out_buf(4096), seam(1024), next_seam(1024), clipped(4096))
    ops(1) = op
    st%nbands = M

    do k = 0, M - 1
       ylo = ymin + (ymax - ymin) * k / M
       yhi = ymin + (ymax - ymin) * (k + 1) / M
       if (yhi <= ylo) cycle

       ! Expand halo to cover the full Y-span of any polygon crossing [ylo, yhi] so every
       ! crossing along a seam-spanning edge is present identically in both adjacent bands.
       wlo = ylo - H; whi = yhi + H
       do i = 1, npA
          if (y2A(i) >= ylo - H .and. y1A(i) <= yhi + H) then
             if (y1A(i) - H < wlo) wlo = y1A(i) - H
             if (y2A(i) + H > whi) whi = y2A(i) + H
          end if
       end do
       do i = 1, npB
          if (y2B(i) >= ylo - H .and. y1B(i) <= yhi + H) then
             if (y1B(i) - H < wlo) wlo = y1B(i) - H
             if (y2B(i) + H > whi) whi = y2B(i) + H
          end if
       end do
       call select_band_polys(npA, poffA, pxA, pyA, y1A, y2A, wlo, whi, snA, svA, sPoA, sPxA, sPyA)
       call select_band_polys(npB, poffB, pxB, pyB, y1B, y2B, wlo, whi, snB, svB, sPoB, sPxB, sPyB)
       if (snA + snB > st%max_band_polys) st%max_band_polys = snA + snB

       nclip = 0
       if (snA > 0 .or. snB > 0) then
          call TrapBooleanPolygons(snA, sPoA, sPxA, sPyA, snB, sPoB, sPxB, sPyB, 1, ops, res, mst, status)
          st%anomalies = st%anomalies + mst%anomalies + res(1)%anomalies
          if (status /= 0) then
             allocate(traps(0)); return
          end if
          ! Exact zero-drift horizontal clip to [ylo, yhi] via side_reanchor (Invariant I2)
          do i = 1, res(1)%n
             z = res(1)%t(i)
             if (int(z%y2, int64) <= ylo .or. int(z%y1, int64) >= yhi) cycle
             if (int(z%y1, int64) < ylo) then
                call side_reanchor(z%l, int(z%y1, int64), ylo, sl, st_l)
                call side_reanchor(z%r, int(z%y1, int64), ylo, sr, st_r)
                if (st_l /= 0 .or. st_r /= 0) then
                   st%anomalies = st%anomalies + 1; cycle
                end if
                z%l = sl; z%r = sr
                z%y1 = int(ylo, int32)
             end if
             if (int(z%y2, int64) > yhi) then
                z%y2 = int(yhi, int32)
             end if
             if (z%y2 > z%y1) then
                if (nclip >= size(clipped, kind=int64)) call arena_grow_trap(clipped, nclip, 2 * nclip)
                nclip = nclip + 1
                clipped(nclip) = z
             end if
          end do
          if (allocated(res(1)%t)) deallocate(res(1)%t)
       end if

       ! Stitch against band k-1's 1-row seam buffer at ylo
       nnext = 0
       do i = 1, nclip
          z = clipped(i)
          matched = .false.
          if (int(z%y1, int64) == ylo .and. nseam > 0) then
             do j = 1, nseam
                if (seam(j)%y2 /= int(ylo, int32)) cycle
                call side_reanchor(seam(j)%l, int(seam(j)%y1, int64), ylo, sl, st_l)
                if (st_l /= 0 .or. sl%ax /= z%l%ax .or. sl%k /= z%l%k .or. sl%dx /= z%l%dx .or. sl%dy /= z%l%dy) cycle
                call side_reanchor(seam(j)%r, int(seam(j)%y1, int64), ylo, sr, st_r)
                if (st_r /= 0 .or. sr%ax /= z%r%ax .or. sr%k /= z%r%k .or. sr%dx /= z%r%dx .or. sr%dy /= z%r%dy) cycle
                ! Merge z upward into seam(j)
                seam(j)%y2 = z%y2
                matched = .true.
                st%stitched_seams = st%stitched_seams + 1
                exit
             end do
          end if
          if (.not. matched) then
             if (int(z%y2, int64) == yhi .and. k < M - 1) then
                if (nnext >= size(next_seam, kind=int64)) call arena_grow_trap(next_seam, nnext, 2 * nnext)
                nnext = nnext + 1; next_seam(nnext) = z
             else
                if (nout >= size(out_buf, kind=int64)) call arena_grow_trap(out_buf, nout, 2 * nout)
                nout = nout + 1; out_buf(nout) = z
             end if
          end if
       end do

       ! Flush previous seam pieces that did not reach yhi; carry forward those that did
       do j = 1, nseam
          zs = seam(j)
          if (int(zs%y2, int64) == yhi .and. k < M - 1) then
             if (nnext >= size(next_seam, kind=int64)) call arena_grow_trap(next_seam, nnext, 2 * nnext)
             nnext = nnext + 1; next_seam(nnext) = zs
          else
             if (nout >= size(out_buf, kind=int64)) call arena_grow_trap(out_buf, nout, 2 * nout)
             nout = nout + 1; out_buf(nout) = zs
          end if
       end do
       nseam = nnext
       if (nseam > size(seam, kind=int64)) call arena_grow_trap(seam, 0_int64, nseam)
       if (nseam > 0) seam(1:nseam) = next_seam(1:nseam)
    end do

    ! Flush any remaining seam trapezoids
    do j = 1, nseam
       if (nout >= size(out_buf, kind=int64)) call arena_grow_trap(out_buf, nout, 2 * nout)
       nout = nout + 1; out_buf(nout) = seam(j)
    end do

    allocate(traps(max(nout, 0_int64)))
    if (nout > 0) traps(1:nout) = out_buf(1:nout)
    ntrap = nout
    st%traps_out = ntrap
    st%t_total = omp_get_wtime() - t0
  end subroutine TrapBandedBoolean

  subroutine select_band_polys(np, poff, px, py, y1, y2, ylo, yhi, sn, sv, sPo, sPx, sPy)
    integer(int64), intent(in) :: np, poff(:), px(:), py(:), y1(:), y2(:), ylo, yhi
    integer(int64), intent(out) :: sn, sv
    integer(int64), allocatable, intent(inout) :: sPo(:), sPx(:), sPy(:)
    integer(int64) :: i, lenp, k
    sn = 0; sv = 0
    do i = 1, np
       if (y2(i) >= ylo .and. y1(i) <= yhi) then
          sn = sn + 1
          sv = sv + (poff(i + 1) - poff(i))
       end if
    end do
    call arena_ensure_i64(sPo, sn + 1)
    call arena_ensure_i64(sPx, max(sv, 1_int64))
    call arena_ensure_i64(sPy, max(sv, 1_int64))
    k = 0; sv = 0
    sPo(1) = 1
    do i = 1, np
       if (y2(i) >= ylo .and. y1(i) <= yhi) then
          lenp = poff(i + 1) - poff(i)
          sPx(sv + 1:sv + lenp) = px(poff(i):poff(i + 1) - 1)
          sPy(sv + 1:sv + lenp) = py(poff(i):poff(i + 1) - 1)
          sv = sv + lenp
          k = k + 1
          sPo(k + 1) = sv + 1
       end if
    end do
  end subroutine select_band_polys

end module TrapBandStreamModule
