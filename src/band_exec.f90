! File    : band_exec.f90
! Author  : Sandeep Koranne (C) 2026
! Purpose : Banded execution of local (region) operations over compact layers.
!
! For C = op(A [, B]) where op is a region operation whose value at a point depends only
! on A and B within a distance `halo` of it (AND, OR, NOT, XOR, FRAMENOT; GROW/SIZE with
! halo = grow/size amount), the y-extent is cut into bands holding about
! MAGPARSER_BAND_BOXES input boxes each (default 48M). For each band [y0, y1]:
!   * only the compact blocks whose MBR meets [y0-halo, y1+halo] are decoded (whole input
!     boxes, never clipped), into temporary layers;
!   * the existing operation runs on them unchanged (GPU kernels, Boost, heal, ...);
!   * the result is clipped to [y0, y1] (exact for region semantics) and encoded into a
!     per-band compact part;
! and the parts are merged into the output's compact form (CompactMergeParts) without
! ever materialising the whole input or output as Boxes. The output polygons are split at
! band seams, so the layer is not marked healed; downstream operations heal as usual.
!
! Used only when MAGPARSER_COMPACT_LAYERS=1 and the inputs exceed 2 bands' worth of boxes;
! MAGPARSER_BAND_BOXES=0 disables it.
module BandExecModule
  use iso_fortran_env, only: int32, int64, real64
  use iso_c_binding, only: c_int
  use CommonModule
  use GeometryModule, only: Box
  use DesignModule
  use CompactLayerModule
  use ProfilerModule, only: PhaseBegin, PhaseEnd
  implicit none
  private
  public :: BandedEligible, BandedRun, LayerBoxCount, BandSetTarget
  public :: K_BAND_OR, K_BAND_AND, K_BAND_NOT, K_BAND_FRAMENOT, K_BAND_BOOSTNOT, K_BAND_XOR, &
       K_BAND_GROW, K_BAND_SIZE, K_BAND_INTERACT, LayerIsHealed

  integer, parameter :: K_BAND_OR = 1, K_BAND_AND = 2, K_BAND_NOT = 3, K_BAND_FRAMENOT = 4, &
       K_BAND_BOOSTNOT = 5, K_BAND_XOR = 6, K_BAND_GROW = 7, K_BAND_SIZE = 8, K_BAND_INTERACT = 9
  integer(int64), parameter :: K_HUGE_COORD = int(huge(0_K_COORDINATE_KIND), int64)
  integer(int64), save :: band_override = -1    !> CTR `var band_boxes N` (wins over the env)

contains

  integer(int64) function band_target()
    character(len=32) :: ev
    integer :: evlen, evstat, ios
    band_target = 48000000_int64
    if (band_override >= 0) then
       band_target = band_override
       return
    end if
    call get_environment_variable('MAGPARSER_BAND_BOXES', ev, evlen, evstat)
    if (evstat == 0 .and. evlen > 0) then
       read(ev, *, iostat=ios) band_target
       if (ios /= 0) band_target = 48000000_int64
    end if
  end function band_target

  !> Banded outputs above this many boxes stay multi-part (MAGPARSER_BAND_MERGE_MAX, default 400M).
  integer(int64) function merge_max()
    character(len=32) :: ev
    integer :: evlen, evstat, ios
    merge_max = 400000000_int64
    call get_environment_variable('MAGPARSER_BAND_MERGE_MAX', ev, evlen, evstat)
    if (evstat == 0 .and. evlen > 0) then
       read(ev, *, iostat=ios) merge_max
       if (ios /= 0) merge_max = 400000000_int64
    end if
  end function merge_max

  !> Band size in boxes for the rest of the run (0 disables banding).
  subroutine BandSetTarget(n)
    integer(int64), intent(in) :: n
    band_override = max(0_int64, n)
  end subroutine BandSetTarget

  !> Boxes held by a layer in either form.
  integer(int64) function LayerBoxCount(L)
    type(Layer), intent(in) :: L
    integer :: ip
    LayerBoxCount = L%n_used
    if (L%is_compact) then
       LayerBoxCount = L%compact%n
       if (allocated(L%cextra)) then
          do ip = 1, size(L%cextra)
             LayerBoxCount = LayerBoxCount + L%cextra(ip)%n
          end do
       end if
    end if
  end function LayerBoxCount

  !> Disjoint (healed) in either form.
  logical function LayerIsHealed(L)
    type(Layer), intent(in) :: L
    if (L%is_compact) then
       LayerIsHealed = iand(L%compact_state, LAYER_STATE_HEAL) /= 0
    else
       LayerIsHealed = L%has(LAYER_STATE_HEAL)
    end if
  end function LayerIsHealed

  logical function has_data(L)
    type(Layer), intent(in) :: L
    has_data = L%is_compact .or. L%n_used > 0
  end function has_data

  !> Banding pays off (and is needed for memory) only for large inputs in compact mode.
  logical function BandedEligible(A, B)
    type(Layer), intent(in) :: A
    type(Layer), intent(in), optional :: B
    integer(int64) :: n, t
    BandedEligible = .false.
    if (.not. CompactLayersEnabled()) return
    t = band_target()
    if (t <= 0) return
    if (.not. has_data(A)) return
    n = LayerBoxCount(A)
    if (present(B)) then
       if (.not. has_data(B)) return
       n = n + LayerBoxCount(B)
    end if
    if (n <= 2_int64 * t) return
    BandedEligible = A%is_compact
    if (present(B)) BandedEligible = BandedEligible .or. B%is_compact
  end function BandedEligible

  integer function n_parts(L)
    type(Layer), intent(in) :: L
    n_parts = 1
    if (allocated(L%cextra)) n_parts = 1 + size(L%cextra)
  end function n_parts

  function part_of(L, ip) result(cp)
    type(Layer), intent(in), target :: L
    integer, intent(in) :: ip
    type(CompactLayer), pointer :: cp
    if (ip == 1) then
       cp => L%compact
    else
       cp => L%cextra(ip - 1)
    end if
  end function part_of

  !> y-sample of a layer for band boundaries: block-MBR centres (compact) or every 128th box.
  subroutine add_samples(L, ys, ns)
    type(Layer), intent(in), target :: L
    integer(int64), intent(inout) :: ys(:)
    integer(int64), intent(inout) :: ns
    integer(int64) :: k
    integer :: ip
    type(CompactLayer), pointer :: cp
    if (L%is_compact) then
       do ip = 1, n_parts(L)
          cp => part_of(L, ip)
          do k = 1, cp%nblk
             ns = ns + 1
             ys(ns) = (int(cp%blk_mbr(k)%y1, int64) + int(cp%blk_mbr(k)%y2, int64)) / 2 + K_HUGE_COORD
          end do
       end do
    else
       do k = 1, L%n_used, K_CL_BLOCK
          ns = ns + 1
          ys(ns) = (int(L%layer_boxes(k)%y1, int64) + int(L%layer_boxes(k)%y2, int64)) / 2 + K_HUGE_COORD
       end do
    end if
  end subroutine add_samples

  integer(int64) function n_samples(L)
    type(Layer), intent(in), target :: L
    integer :: ip
    if (L%is_compact) then
       n_samples = 0
       do ip = 1, n_parts(L)
          n_samples = n_samples + part_of(L, ip)%nblk
       end do
    else
       n_samples = (L%n_used + K_CL_BLOCK - 1) / K_CL_BLOCK
    end if
  end function n_samples

  !> Boxes of L meeting y in [wlo, whi] (closed), whole, into T (a fresh temporary layer).
  !> A subset of a healed (disjoint) layer is disjoint, so HEAL carries over.
  subroutine extract_window(L, wlo, whi, T)
    type(Layer), intent(in), target :: L
    integer(int64), intent(in) :: wlo, whi
    type(Layer), intent(inout) :: T
    integer(int64) :: k, nsel, i, j, nout, nkeep
    integer(int64), allocatable :: sel(:), cnt(:), base(:)
    integer, allocatable :: selp(:)
    integer :: ip
    type(CompactLayer), pointer :: cp
    type(Box), allocatable :: tmp(:)
    logical :: healed
    call ClearLayer(T)
    if (L%is_compact) then
       ! pass 1: selected blocks of every part
       nsel = 0
       do ip = 1, n_parts(L)
          cp => part_of(L, ip)
          do k = 1, cp%nblk
             if (int(cp%blk_mbr(k)%y2, int64) < wlo .or. int(cp%blk_mbr(k)%y1, int64) > whi) cycle
             nsel = nsel + 1
          end do
       end do
       allocate(sel(max(1_int64, nsel)), selp(max(1_int64, nsel)))
       nsel = 0
       do ip = 1, n_parts(L)
          cp => part_of(L, ip)
          do k = 1, cp%nblk
             if (int(cp%blk_mbr(k)%y2, int64) < wlo .or. int(cp%blk_mbr(k)%y1, int64) > whi) cycle
             nsel = nsel + 1
             sel(nsel) = k
             selp(nsel) = ip
          end do
       end do
       allocate(tmp(max(1_int64, nsel * K_CL_BLOCK)), cnt(max(1_int64, nsel)), base(max(1_int64, nsel)))
       !$omp parallel do private(k, i, nout, nkeep, cp) schedule(dynamic, 64)
       do k = 1, nsel
          cp => part_of(L, selp(k))
          call CompactDecodeBlocks(cp, sel(k), sel(k), tmp((k - 1) * K_CL_BLOCK + 1:k * K_CL_BLOCK), nout)
          nkeep = 0
          do i = 1, nout
             associate (b => tmp((k - 1) * K_CL_BLOCK + i))
               if (int(b%y2, int64) < wlo .or. int(b%y1, int64) > whi) cycle
               nkeep = nkeep + 1
               tmp((k - 1) * K_CL_BLOCK + nkeep) = b
             end associate
          end do
          cnt(k) = nkeep
       end do
       j = 0
       do k = 1, nsel
          base(k) = j
          j = j + cnt(k)
       end do
       allocate(T%layer_boxes(max(1_int64, j)))
       !$omp parallel do private(k) schedule(static)
       do k = 1, nsel
          if (cnt(k) > 0) T%layer_boxes(base(k) + 1:base(k) + cnt(k)) = &
               tmp((k - 1) * K_CL_BLOCK + 1:(k - 1) * K_CL_BLOCK + cnt(k))
       end do
       T%n_used = j
       healed = iand(L%compact_state, LAYER_STATE_HEAL) /= 0
    else
       j = 0
       do i = 1, L%n_used
          if (int(L%layer_boxes(i)%y2, int64) < wlo .or. int(L%layer_boxes(i)%y1, int64) > whi) cycle
          j = j + 1
       end do
       allocate(T%layer_boxes(max(1_int64, j)))
       j = 0
       do i = 1, L%n_used
          if (int(L%layer_boxes(i)%y2, int64) < wlo .or. int(L%layer_boxes(i)%y1, int64) > whi) cycle
          j = j + 1
          T%layer_boxes(j) = L%layer_boxes(i)
       end do
       T%n_used = j
       healed = L%has(LAYER_STATE_HEAL)
    end if
    T%n_alloc = size(T%layer_boxes, kind=int64)
    T%layerState = LAYER_STATE_NONE
    if (healed .and. T%n_used > 0) T%layerState = LAYER_STATE_HEAL
  end subroutine extract_window

  !> C = op(A [, B]) band by band (see module header). ivar: GROW amounts (E N W S);
  !> cval: SIZE amount. A, B and C may be the same layer object: C is written only at the end.
  subroutine BandedRun(A, B, C, op, halo, ivar, cval)
    type(Layer), intent(inout), target :: A
    type(Layer), intent(inout), target, optional :: B
    type(Layer), intent(inout), target :: C
    integer, intent(in) :: op
    integer(int64), intent(in) :: halo
    integer(K_COORDINATE_KIND), intent(in), optional :: ivar(4)
    integer(int64), intent(in), optional :: cval
    integer(int64), allocatable :: ys(:), edges(:)
    integer(int64) :: ns, ntot, nb, k, lo, hi, i, j, nin_max
    type(Layer) :: tA, tB, tC
    type(CompactLayer), allocatable :: parts(:)
    integer :: st, np
    real(real64) :: tp, tband
    logical :: same_ab, all_healed, clip
    integer(int64) :: nout_tot
    integer :: nparts_out

    tp = PhaseBegin('band.total')
    same_ab = .false.
    if (present(B)) same_ab = associated(ptr_of(A), ptr_of(B))

    ! band boundaries at equal-count quantiles of the inputs' y samples
    ns = n_samples(A)
    if (present(B) .and. .not. same_ab) ns = ns + n_samples(B)
    allocate(ys(max(1_int64, ns)))
    ns = 0
    call add_samples(A, ys, ns)
    if (present(B) .and. .not. same_ab) call add_samples(B, ys, ns)
    call CompactSortU64(ys, ns)
    ntot = LayerBoxCount(A)
    if (present(B) .and. .not. same_ab) ntot = ntot + LayerBoxCount(B)
    nb = max(1_int64, (ntot + band_target() - 1) / band_target())
    allocate(edges(0:nb))
    edges(0) = -K_HUGE_COORD
    do k = 1, nb - 1
       edges(k) = ys(max(1_int64, min(ns, (k * ns) / nb))) - K_HUGE_COORD
    end do
    edges(nb) = K_HUGE_COORD
    deallocate(ys)
    allocate(parts(nb))
    np = 0
    nin_max = 0
    all_healed = .true.
    ! Box-level selections keep whole A boxes (A must be healed: its decomposition is then
    ! the same in every band); an A box selected in several bands is de-duplicated by the merge.
    clip = (op /= K_BAND_INTERACT)

    do k = 1, nb
       lo = edges(k - 1); hi = edges(k)
       if (hi <= lo) cycle                            ! empty band (repeated quantile)
       tband = PhaseBegin('band.extract')
       call extract_window(A, sat(lo - halo), sat(hi + halo), tA)
       if (present(B) .and. .not. same_ab) call extract_window(B, sat(lo - halo), sat(hi + halo), tB)
       call PhaseEnd('band.extract', tband, tA%n_used + tB%n_used)
       nin_max = max(nin_max, tA%n_used + tB%n_used)
       if (band_is_empty(op, tA%n_used, tB%n_used)) then
          call ClearLayer(tA)
          call ClearLayer(tB)
          cycle
       end if
       select case (op)
       case (K_BAND_OR)
          call CalculateBoostOperation(tA, tB, tC, K_BOOST_CONTROL_OR, 0_int64)
       case (K_BAND_AND)
          call CalculateAND(tA, tB, tC)
       case (K_BAND_NOT)
          call CalculateNOT(tA, tB, tC)
       case (K_BAND_FRAMENOT)
          call CalculateFrameNOT(tA, tB, tC)
       case (K_BAND_BOOSTNOT)
          call CalculateBoostOperation(tA, tB, tC, K_BOOST_CONTROL_NOT, 0_int64)
       case (K_BAND_XOR)
          call CalculateBoostOperation(tA, tB, tC, K_BOOST_CONTROL_XOR, 0_int64)
       case (K_BAND_GROW)
          call CalculateGROWLayer(tA, tC, ivar)
       case (K_BAND_INTERACT)
          call CalculateTopologicalSelect(tA, tB, tC, K_TOPO_INTERACT)
       case (K_BAND_SIZE)
          if (same_ab .or. .not. present(B)) then
             call CalculateBoostOperation(tA, tA, tC, K_BOOST_CONTROL_SIZE, cval)
          else
             call CalculateBoostOperation(tA, tB, tC, K_BOOST_CONTROL_SIZE, cval)
          end if
       case default
          error stop 'BandedRun: unsupported operation'
       end select
       call ClearLayer(tA)
       call ClearLayer(tB)
       if (tC%n_used > 0 .and. .not. tC%has(LAYER_STATE_HEAL)) all_healed = .false.
       ! clip to the band (disjoint band results stay disjoint); keep positive-height pieces
       j = 0
       do i = 1, tC%n_used
          associate (b => tC%layer_boxes(i))
            if (clip) then
               if (int(b%y1, int64) < lo) b%y1 = int(lo, K_COORDINATE_KIND)
               if (int(b%y2, int64) > hi) b%y2 = int(hi, K_COORDINATE_KIND)
            end if
            if (b%y2 <= b%y1 .or. b%x2 <= b%x1) cycle
            j = j + 1
            tC%layer_boxes(j) = b
          end associate
       end do
       if (j > 0) then
          np = np + 1
          call CompactEncode(tC%layer_boxes(1:j), j, parts(np), st)
          if (st /= 0) then
             write(*,'(A,I0,A)') 'ERROR: banded output cannot be encoded compactly (status ', st, &
                  '); rerun with MAGPARSER_BAND_BOXES=0'
             error stop 'BandedRun: compact encode failed'
          end if
          call CompactPack(parts(np))
       end if
       call ClearLayer(tC)
    end do

    ! commit: only now is C (possibly the same object as A or B) overwritten
    call ClearLayer(C)
    C%layerState = LAYER_STATE_NONE
    nparts_out = 0
    if (np > 0) then
       nout_tot = 0
       do i = 1, np
          nout_tot = nout_tot + parts(i)%n
       end do
       if (np == 1 .or. nout_tot <= merge_max()) then
          call CompactMergeParts(parts, np, C%compact, st)
          if (st == 2) error stop 'BandedRun: merged output exceeds 31-bit coordinates'
          call CompactPack(C%compact)
          nparts_out = 1
       else
          ! keep the band parts: merging would need a 12 B/box key transient
          call CompactMove(parts(1), C%compact)
          allocate(C%cextra(np - 1))
          do i = 2, np
             call CompactMove(parts(i), C%cextra(i - 1))
          end do
          nparts_out = np
          if (op == K_BAND_INTERACT) all_healed = .false.   ! whole boxes may repeat across parts
       end if
       C%is_compact = .true.
       C%compact_state = LAYER_STATE_NONE
       if (all_healed) C%compact_state = LAYER_STATE_HEAL
    end if
    write(*,'(A,I0,A,I0,A,I0,A,I0,A,L1)') 'INFO: banded op bands=', nb, ' max_band_inputs=', nin_max, &
         ' parts=', nparts_out, ' output_boxes=', LayerBoxCount(C), ' healed=', all_healed .and. np > 0
    call PhaseEnd('band.total', tp, LayerBoxCount(C))
  contains
    pure logical function band_is_empty(opc, na, nbb)
      integer, intent(in) :: opc
      integer(int64), intent(in) :: na, nbb
      select case (opc)
      case (K_BAND_OR, K_BAND_XOR)
         band_is_empty = (na == 0 .and. nbb == 0)
      case (K_BAND_AND)
         band_is_empty = (na == 0 .or. nbb == 0)
      case default
         band_is_empty = (na == 0)
      end select
    end function band_is_empty
    pure integer(int64) function sat(v)
      integer(int64), intent(in) :: v
      sat = max(-K_HUGE_COORD, min(K_HUGE_COORD, v))
    end function sat
    function ptr_of(L) result(pl)
      type(Layer), intent(in), target :: L
      type(Layer), pointer :: pl
      pl => L
    end function ptr_of
  end subroutine BandedRun

end module BandExecModule
