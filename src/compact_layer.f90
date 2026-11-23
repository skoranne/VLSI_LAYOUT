! File   : compact_layer.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: Compact, exact representation of a rectilinear layer as sorted 64-bit keys.
!
!  Layout data is heavily quantized: every layer's box *centre sums* (x1+x2, y1+y2) lie
!  on a coarse grid (5..40 DBU in SKY130 data) and a layer uses few distinct (w,h)
!  shapes (contacts/vias: exactly one; drawing layers: 18..6600).  So a box is fully
!  described by (quantized centre, shape id):
!
!     key = morton( (x1+x2-ox)/g , (y1+y2-oy)/g ) << sbits  |  shape_id
!
!  with a per-layer shape table (w,h).  Decoding is exact: x1 = (sx - w)/2 where
!  sx = x1+x2, because sx - w = 2*x1.  For a single-shape (contact) layer sbits = 0 and
!  the key is just the Morton code of the centre.
!
!  Hot form : keys(1:n), sorted ascending (8 B/box), exact duplicates removed.
!             Sorted Morton order is also a good R-tree packing order, so a decoded
!             layer is marked SORT and goes straight to BuildRTree.
!  Cold form: blocks of K_CL_BLOCK keys, each stored as base key + fixed-width deltas
!             (frame-of-reference bit packing, 2..4.5 B/box on SKY130 data), plus one
!             MBR per block (0.125 B/box) as a coarse spatial index.  Blocks decode
!             independently (OpenMP over blocks; same layout suits a GPU decode).
!
!  2*cbits + sbits must be <= 62 (keys stay non-negative int64); otherwise encoding
!  reports status /= 0 and the caller keeps the plain Box array.
module CompactLayerModule
  use iso_fortran_env, only: int8, int32, int64, real64
  use iso_c_binding, only: c_loc, c_int32_t, c_int64_t, c_ptr
  use CommonModule, only: K_COORDINATE_KIND
  use GeometryModule, only: Box
  use ProfilerModule, only: PhaseBegin, PhaseEnd
  use omp_lib
  implicit none
  private

  public :: CompactLayer, CompactEncode, CompactDecode, CompactPack, CompactUnpack, &
       CompactRelease, CompactBytes, CompactPrintStats, CompactKeyOf, K_CL_BLOCK, &
       CompactDecodeBlocks, CompactMove, CompactDecodeCold, BoxSetHash, CompactMergeParts, CompactSortU64, CompactDecodeColdInto

  integer(kind=int64), parameter :: K_CL_BLOCK = 128_int64

  type :: CompactLayer
     integer(kind=int64) :: n = 0              !< number of (unique) boxes
     integer(kind=int64) :: n_input = 0        !< boxes before duplicate removal
     integer(kind=int64) :: ox = 0, oy = 0     !< origin of centre sums
     integer(kind=int64) :: grid = 1           !< gcd of centre sums (relative to origin)
     integer(kind=int32) :: cbits = 0, sbits = 0
     integer(kind=int32), allocatable :: shape_w(:), shape_h(:)
     !> hot form
     integer(kind=int64), allocatable :: keys(:)
     !> cold form
     integer(kind=int64), allocatable :: blk_base(:)     !< first key of each block
     integer(kind=int64), allocatable :: blk_bitoff(:)   !< bit offset of the block's deltas
     integer(kind=int8),  allocatable :: blk_width(:)    !< delta width in bits (0..62)
     integer(kind=int64), allocatable :: packed(:)       !< delta bit stream, LSB first
     !> coarse spatial index (one MBR per block), valid in both forms
     type(Box), allocatable :: blk_mbr(:)
     integer(kind=int64) :: nblk = 0
     !> wide layers (2*cbits + sbits > 62): keys hold the Morton code only and the shape
     !> ids live in a parallel stream, sid (hot, int32) / sidpack (cold, sbits per box).
     logical :: wide = .false.
     integer(kind=int32), allocatable :: sid(:)
     integer(kind=int64), allocatable :: sidpack(:)
  end type CompactLayer

  interface
     function thrust_sort_u64_host(h_keys, n) result(rc) bind(c, name="thrust_sort_u64_host")
       import :: c_ptr, c_int64_t, c_int32_t
       type(c_ptr), value :: h_keys
       integer(c_int64_t), value :: n
       integer(c_int32_t) :: rc
     end function thrust_sort_u64_host
     function thrust_sort_u64_i32_host(h_keys, h_vals, n) result(rc) bind(c, name="thrust_sort_u64_i32_host")
       import :: c_ptr, c_int64_t, c_int32_t
       type(c_ptr), value :: h_keys, h_vals
       integer(c_int64_t), value :: n
       integer(c_int32_t) :: rc
     end function thrust_sort_u64_i32_host
  end interface

contains

  !------------------------------------------------------------------ bit helpers
  pure elemental integer(kind=int64) function spread_bits(v) result(r)
    integer(kind=int64), intent(in) :: v      !< < 2**31
    r = iand(v, int(z'00000000FFFFFFFF', int64))
    r = iand(ior(r, shiftl(r, 16)), int(z'0000FFFF0000FFFF', int64))
    r = iand(ior(r, shiftl(r, 8)),  int(z'00FF00FF00FF00FF', int64))
    r = iand(ior(r, shiftl(r, 4)),  int(z'0F0F0F0F0F0F0F0F', int64))
    r = iand(ior(r, shiftl(r, 2)),  int(z'3333333333333333', int64))
    r = iand(ior(r, shiftl(r, 1)),  int(z'5555555555555555', int64))
  end function spread_bits

  pure elemental integer(kind=int64) function compact_bits(v) result(r)
    integer(kind=int64), intent(in) :: v
    r = iand(v, int(z'5555555555555555', int64))
    r = iand(ior(r, shiftr(r, 1)),  int(z'3333333333333333', int64))
    r = iand(ior(r, shiftr(r, 2)),  int(z'0F0F0F0F0F0F0F0F', int64))
    r = iand(ior(r, shiftr(r, 4)),  int(z'00FF00FF00FF00FF', int64))
    r = iand(ior(r, shiftr(r, 8)),  int(z'0000FFFF0000FFFF', int64))
    r = iand(ior(r, shiftr(r, 16)), int(z'00000000FFFFFFFF', int64))
  end function compact_bits

  pure integer(kind=int32) function bit_length(v) result(nb)
    integer(kind=int64), intent(in) :: v
    if (v <= 0_int64) then
       nb = 0
    else
       nb = 64 - leadz(v)
    end if
  end function bit_length

  pure integer(kind=int64) function gcd64(a0, b0) result(a)
    integer(kind=int64), intent(in) :: a0, b0
    integer(kind=int64) :: b, t
    a = abs(a0); b = abs(b0)
    do while (b /= 0_int64)
       t = mod(a, b); a = b; b = t
    end do
  end function gcd64

  subroutine sort_u64(a, n)
    integer(kind=int64), intent(inout), target :: a(:)
    integer(kind=int64), intent(in) :: n
    integer(c_int32_t) :: rc
    if (n <= 1) return
    rc = thrust_sort_u64_host(c_loc(a(1)), int(n, c_int64_t))
    if (rc /= 0) error stop "CompactLayer: key sort failed"
  end subroutine sort_u64

  subroutine sort_pairs(a, v, n)
    integer(kind=int64), intent(inout), target :: a(:)
    integer(kind=int32), intent(inout), target :: v(:)
    integer(kind=int64), intent(in) :: n
    integer(c_int32_t) :: rc
    if (n <= 1) return
    rc = thrust_sort_u64_i32_host(c_loc(a(1)), c_loc(v(1)), int(n, c_int64_t))
    if (rc /= 0) error stop "CompactLayer: pair sort failed"
  end subroutine sort_pairs

  !> MAGPARSER_COMPACT_FORCE_WIDE=1 (testing): use the wide layout for every layer.
  logical function force_wide()
    character(len=4) :: ev
    integer :: evlen, evstat
    call get_environment_variable('MAGPARSER_COMPACT_FORCE_WIDE', ev, evlen, evstat)
    force_wide = (evstat == 0 .and. evlen > 0 .and. ev(1:1) == '1')
  end function force_wide

  !> w-bit field number i (1-based) of a fixed-width bit stream.
  pure integer(kind=int64) function get_field(buf, i, w) result(v)
    integer(kind=int64), intent(in) :: buf(:)
    integer(kind=int64), intent(in) :: i
    integer(kind=int32), intent(in) :: w
    integer(kind=int64) :: pos, word, off
    v = 0
    if (w == 0) return
    pos = (i - 1) * w
    word = pos / 64 + 1
    off = mod(pos, 64_int64)
    v = shiftr(buf(word), int(off))
    if (off + w > 64) v = ior(v, shiftl(buf(word + 1), int(64 - off)))
    v = iand(v, shiftl(1_int64, w) - 1_int64)
  end function get_field

  !> Shape-id stream (wide layers) of box i: hot array or cold bit stream.
  pure integer(kind=int64) function sid_of(cl, i) result(sid)
    type(CompactLayer), intent(in) :: cl
    integer(kind=int64), intent(in) :: i
    if (allocated(cl%sid)) then
       sid = cl%sid(i)
    else
       sid = get_field(cl%sidpack, i, cl%sbits)
    end if
  end function sid_of

  !> Box i (1-based, key = its key) of cl, narrow or wide.
  pure subroutine decode_at(cl, key, i, b)
    type(CompactLayer), intent(in) :: cl
    integer(kind=int64), intent(in) :: key, i
    type(Box), intent(out) :: b
    if (cl%wide) then
       call decode_ms(key, sid_of(cl, i), cl%ox, cl%oy, cl%grid, cl%shape_w, cl%shape_h, b)
    else
       call decode_key(key, cl%sbits, shiftl(1_int64, cl%sbits) - 1_int64, cl%ox, cl%oy, cl%grid, &
            cl%shape_w, cl%shape_h, b)
    end if
  end subroutine decode_at

  !> Key of one box under cl's parameters (shape must exist in cl's table).
  integer(kind=int64) function CompactKeyOf(cl, b) result(key)
    type(CompactLayer), intent(in) :: cl
    type(Box), intent(in) :: b
    integer(kind=int64) :: qx, qy, wh
    integer :: lo, hi, mid
    qx = (int(b%x1, int64) + int(b%x2, int64) - cl%ox) / cl%grid
    qy = (int(b%y1, int64) + int(b%y2, int64) - cl%oy) / cl%grid
    wh = ior(shiftl(int(b%x2 - b%x1, int64), 32), int(b%y2 - b%y1, int64))
    lo = 1; hi = size(cl%shape_w)
    do while (lo < hi)
       mid = (lo + hi) / 2
       if (ior(shiftl(int(cl%shape_w(mid), int64), 32), int(cl%shape_h(mid), int64)) < wh) then
          lo = mid + 1
       else
          hi = mid
       end if
    end do
    key = ior(shiftl(ior(spread_bits(qx), shiftl(spread_bits(qy), 1)), cl%sbits), int(lo - 1, int64))
  end function CompactKeyOf

  !------------------------------------------------------------------ encode
  !> Build the hot form (sorted, de-duplicated keys) and the per-block MBR index.
  !> status: 0 ok, 1 empty, 2 coordinate/shape range too large for 62-bit keys,
  !>         3 invalid (inverted) box.
  subroutine CompactEncode(boxes, n, cl, status)
    type(Box), intent(in) :: boxes(:)
    integer(kind=int64), intent(in) :: n
    type(CompactLayer), intent(inout) :: cl
    integer, intent(out) :: status
    integer(kind=int64) :: i, j, ox, oy, g, qmax, wh, nu, nshape
    integer(kind=int64), allocatable :: whk(:)
    integer :: lo, hi, mid
    logical :: bad
    real(kind=real64) :: tp

    call CompactRelease(cl)
    status = 0
    if (n <= 0) then
       status = 1
       return
    end if

    tp = PhaseBegin('cl.scan')
    ! origin of centre sums, validity
    ox = huge(ox); oy = huge(oy); bad = .false.
    !$omp parallel do reduction(min:ox,oy) reduction(.or.:bad) schedule(static)
    do i = 1, n
       ox = min(ox, int(boxes(i)%x1, int64) + int(boxes(i)%x2, int64))
       oy = min(oy, int(boxes(i)%y1, int64) + int(boxes(i)%y2, int64))
       bad = bad .or. boxes(i)%x2 < boxes(i)%x1 .or. boxes(i)%y2 < boxes(i)%y1
    end do
    if (bad) then
       status = 3
       call PhaseEnd('cl.scan', tp, n)
       return
    end if

    ! grid = gcd of all centre-sum offsets (per-thread gcd, then combine)
    g = 0
    !$omp parallel private(i, j, wh)
    j = 0
    !$omp do schedule(static)
    do i = 1, n
       if (j /= 1_int64) then
          ! fast path: once j is the layer grid almost every offset is a multiple of it
          wh = int(boxes(i)%x1, int64) + int(boxes(i)%x2, int64) - ox
          if (j == 0_int64) then
             j = wh
          else if (mod(wh, j) /= 0_int64) then
             j = gcd64(j, wh)
          end if
          wh = int(boxes(i)%y1, int64) + int(boxes(i)%y2, int64) - oy
          if (j == 0_int64) then
             j = wh
          else if (mod(wh, j) /= 0_int64) then
             j = gcd64(j, wh)
          end if
       end if
    end do
    !$omp end do
    !$omp critical (cl_gcd)
    g = gcd64(g, j)
    !$omp end critical (cl_gcd)
    !$omp end parallel
    if (g == 0) g = 1

    qmax = 0
    !$omp parallel do reduction(max:qmax) schedule(static)
    do i = 1, n
       qmax = max(qmax, (int(boxes(i)%x1, int64) + int(boxes(i)%x2, int64) - ox) / g, &
                        (int(boxes(i)%y1, int64) + int(boxes(i)%y2, int64) - oy) / g)
    end do

    call PhaseEnd('cl.scan', tp, n)
    tp = PhaseBegin('cl.shapes')
    ! shape dictionary: sort the (w,h) keys, keep the unique ones
    allocate(whk(n))
    !$omp parallel do schedule(static)
    do i = 1, n
       whk(i) = ior(shiftl(int(boxes(i)%x2 - boxes(i)%x1, int64), 32), int(boxes(i)%y2 - boxes(i)%y1, int64))
    end do
    call sort_u64(whk, n)
    nshape = 1
    do i = 2, n
       if (whk(i) /= whk(nshape)) then
          nshape = nshape + 1
          whk(nshape) = whk(i)
       end if
    end do

    cl%cbits = max(1, bit_length(qmax))
    cl%sbits = bit_length(nshape - 1)
    if (cl%cbits > 31) then
       deallocate(whk)
       status = 2
       call PhaseEnd('cl.shapes', tp, n)
       return
    end if
    cl%wide = (2 * cl%cbits + cl%sbits > 62) .or. force_wide()   !> shape ids go to a separate stream
    cl%ox = ox; cl%oy = oy; cl%grid = g; cl%n_input = n
    allocate(cl%shape_w(nshape), cl%shape_h(nshape))
    do i = 1, nshape
       cl%shape_w(i) = int(shiftr(whk(i), 32), int32)
       cl%shape_h(i) = int(iand(whk(i), int(z'00000000FFFFFFFF', int64)), int32)
    end do

    call PhaseEnd('cl.shapes', tp, n)
    tp = PhaseBegin('cl.keys')
    ! keys
    allocate(cl%keys(n))
    if (cl%wide) allocate(cl%sid(n))
    !$omp parallel do private(wh, lo, hi, mid) schedule(static)
    do i = 1, n
       wh = ior(shiftl(int(boxes(i)%x2 - boxes(i)%x1, int64), 32), int(boxes(i)%y2 - boxes(i)%y1, int64))
       lo = 1; hi = int(nshape)
       do while (lo < hi)
          mid = (lo + hi) / 2
          if (whk(mid) < wh) then
             lo = mid + 1
          else
             hi = mid
          end if
       end do
       wh = ior(spread_bits((int(boxes(i)%x1, int64) + int(boxes(i)%x2, int64) - ox) / g), &
            shiftl(spread_bits((int(boxes(i)%y1, int64) + int(boxes(i)%y2, int64) - oy) / g), 1))
       if (cl%wide) then
          cl%keys(i) = wh
          cl%sid(i) = lo - 1
       else
          cl%keys(i) = ior(shiftl(wh, cl%sbits), int(lo - 1, int64))
       end if
    end do
    deallocate(whk)
    call PhaseEnd('cl.keys', tp, n)
    tp = PhaseBegin('cl.sort')
    if (cl%wide) then
       call sort_pairs(cl%keys, cl%sid, n)
    else
       call sort_u64(cl%keys, n)
    end if
    call PhaseEnd('cl.sort', tp, n)
    tp = PhaseBegin('cl.dedup_mbr')
    ! exact duplicates (identical boxes) carry no information for region operations
    call dedup_sorted(cl, n)
    call build_block_mbrs(cl)
    call PhaseEnd('cl.dedup_mbr', tp, cl%n)
  end subroutine CompactEncode

  !> Drop repeated (key[, sid]) entries of the sorted hot form; sets cl%n.
  subroutine dedup_sorted(cl, n)
    type(CompactLayer), intent(inout) :: cl
    integer(kind=int64), intent(in) :: n
    integer(kind=int64) :: i, nu
    nu = min(n, 1_int64)
    do i = 2, n
       if (cl%wide) then
          if (cl%keys(i) == cl%keys(nu) .and. cl%sid(i) == cl%sid(nu)) cycle
       else
          if (cl%keys(i) == cl%keys(nu)) cycle
       end if
       nu = nu + 1
       cl%keys(nu) = cl%keys(i)
       if (cl%wide) cl%sid(nu) = cl%sid(i)
    end do
    if (nu < n) then
       block
         integer(kind=int64), allocatable :: tmp(:)
         integer(kind=int32), allocatable :: tsid(:)
         allocate(tmp(nu))
         tmp(1:nu) = cl%keys(1:nu)
         call move_alloc(tmp, cl%keys)
         if (cl%wide) then
            allocate(tsid(nu))
            tsid(1:nu) = cl%sid(1:nu)
            call move_alloc(tsid, cl%sid)
         end if
       end block
    end if
    cl%n = nu
  end subroutine dedup_sorted

  !> Decode key -> box (exact).
  pure subroutine decode_key(key, sbits, smask, ox, oy, g, sw, sh, b)
    integer(kind=int64), intent(in) :: key, smask, ox, oy, g
    integer(kind=int32), intent(in) :: sbits
    integer(kind=int32), intent(in) :: sw(:), sh(:)
    type(Box), intent(out) :: b
    call decode_ms(shiftr(key, sbits), iand(key, smask), ox, oy, g, sw, sh, b)
  end subroutine decode_key

  !> Morton code m + 0-based shape id -> box (exact).
  pure subroutine decode_ms(m, sid0, ox, oy, g, sw, sh, b)
    integer(kind=int64), intent(in) :: m, sid0, ox, oy, g
    integer(kind=int32), intent(in) :: sw(:), sh(:)
    type(Box), intent(out) :: b
    integer(kind=int64) :: sx, sy, w, h
    integer :: sid
    sid = int(sid0) + 1
    sx = compact_bits(m) * g + ox
    sy = compact_bits(shiftr(m, 1)) * g + oy
    w = sw(sid); h = sh(sid)
    b%x1 = int((sx - w) / 2, K_COORDINATE_KIND); b%x2 = int((sx - w) / 2 + w, K_COORDINATE_KIND)
    b%y1 = int((sy - h) / 2, K_COORDINATE_KIND); b%y2 = int((sy - h) / 2 + h, K_COORDINATE_KIND)
  end subroutine decode_ms

  subroutine build_block_mbrs(cl)
    type(CompactLayer), intent(inout) :: cl
    integer(kind=int64) :: k, i, i0, i1, smask
    type(Box) :: b, m
    cl%nblk = (cl%n + K_CL_BLOCK - 1) / K_CL_BLOCK
    if (allocated(cl%blk_mbr)) deallocate(cl%blk_mbr)
    allocate(cl%blk_mbr(cl%nblk))
    smask = shiftl(1_int64, cl%sbits) - 1_int64
    !$omp parallel do private(i, i0, i1, b, m) schedule(static)
    do k = 1, cl%nblk
       i0 = (k - 1) * K_CL_BLOCK + 1
       i1 = min(cl%n, k * K_CL_BLOCK)
       call decode_at(cl, cl%keys(i0), i0, m)
       do i = i0 + 1, i1
          call decode_at(cl, cl%keys(i), i, b)
          m%x1 = min(m%x1, b%x1); m%y1 = min(m%y1, b%y1)
          m%x2 = max(m%x2, b%x2); m%y2 = max(m%y2, b%y2)
       end do
       cl%blk_mbr(k) = m
    end do
  end subroutine build_block_mbrs

  !------------------------------------------------------------------ decode
  !> Hot form -> Box array (Morton order = R-tree packing order).
  subroutine CompactDecode(cl, boxes)
    type(CompactLayer), intent(in) :: cl
    type(Box), allocatable, intent(inout) :: boxes(:)
    integer(kind=int64) :: i, smask
    if (.not. allocated(cl%keys)) error stop "CompactDecode: hot form not present (call CompactUnpack)"
    if (allocated(boxes)) deallocate(boxes)
    allocate(boxes(cl%n))
    smask = shiftl(1_int64, cl%sbits) - 1_int64
    !$omp parallel do schedule(static)
    do i = 1, cl%n
       call decode_at(cl, cl%keys(i), i, boxes(i))
    end do
  end subroutine CompactDecode

  !------------------------------------------------------------------ cold form
  !> Hot -> cold: per block, base key + fixed-width deltas. Drops the hot keys.
  subroutine CompactPack(cl)
    type(CompactLayer), intent(inout) :: cl
    integer(kind=int64) :: k, i, i0, i1, d, dmax, nbits, pos, word, off
    integer(kind=int32) :: w
    if (.not. allocated(cl%keys)) return
    if (allocated(cl%blk_base)) deallocate(cl%blk_base)
    if (allocated(cl%blk_bitoff)) deallocate(cl%blk_bitoff)
    if (allocated(cl%blk_width)) deallocate(cl%blk_width)
    if (allocated(cl%packed)) deallocate(cl%packed)
    allocate(cl%blk_base(cl%nblk), cl%blk_bitoff(cl%nblk + 1), cl%blk_width(cl%nblk))
    !$omp parallel do private(i, i0, i1, dmax) schedule(static)
    do k = 1, cl%nblk
       i0 = (k - 1) * K_CL_BLOCK + 1
       i1 = min(cl%n, k * K_CL_BLOCK)
       cl%blk_base(k) = cl%keys(i0)
       dmax = 0
       do i = i0 + 1, i1
          dmax = max(dmax, cl%keys(i) - cl%keys(i - 1))
       end do
       cl%blk_width(k) = int(bit_length(dmax), int8)
    end do
    cl%blk_bitoff(1) = 0
    do k = 1, cl%nblk
       i0 = (k - 1) * K_CL_BLOCK + 1
       i1 = min(cl%n, k * K_CL_BLOCK)
       cl%blk_bitoff(k + 1) = cl%blk_bitoff(k) + int(cl%blk_width(k), int64) * (i1 - i0)
    end do
    nbits = cl%blk_bitoff(cl%nblk + 1)
    allocate(cl%packed(nbits / 64 + 2))
    cl%packed = 0
    ! blocks start at arbitrary bit offsets, so neighbouring blocks may share a word:
    ! write each block's first and last (shared) words atomically, interior words directly.
    !$omp parallel do private(i, i0, i1, d, w, pos, word, off) schedule(static)
    do k = 1, cl%nblk
       i0 = (k - 1) * K_CL_BLOCK + 1
       i1 = min(cl%n, k * K_CL_BLOCK)
       w = int(cl%blk_width(k), int32)
       if (w == 0) cycle
       pos = cl%blk_bitoff(k)
       do i = i0 + 1, i1
          d = cl%keys(i) - cl%keys(i - 1)
          word = pos / 64 + 1
          off = mod(pos, 64_int64)
          call put_bits(cl%packed, word, off, d, w)
          pos = pos + w
       end do
    end do
    deallocate(cl%keys)
    if (cl%wide) then
       if (allocated(cl%sidpack)) deallocate(cl%sidpack)
       allocate(cl%sidpack(max(1_int64, (cl%n * cl%sbits + 63) / 64 + 1)))
       cl%sidpack = 0
       if (cl%sbits > 0) then
          !$omp parallel do private(pos) schedule(static)
          do i = 1, cl%n
             pos = (i - 1) * cl%sbits
             call put_bits(cl%sidpack, pos / 64 + 1, mod(pos, 64_int64), int(cl%sid(i), int64), cl%sbits)
          end do
       end if
       deallocate(cl%sid)
    end if
  end subroutine CompactPack

  subroutine put_bits(buf, word, off, v, w)
    integer(kind=int64), intent(inout) :: buf(:)
    integer(kind=int64), intent(in) :: word, off, v
    integer(kind=int32), intent(in) :: w
    integer(kind=int64) :: lo, hi
    lo = shiftl(v, int(off))
    !$omp atomic update
    buf(word) = ior(buf(word), lo)
    if (off + w > 64) then
       hi = shiftr(v, int(64 - off))
       !$omp atomic update
       buf(word + 1) = ior(buf(word + 1), hi)
    end if
  end subroutine put_bits

  !> Cold -> hot (keeps the cold form unless drop_cold).
  subroutine CompactUnpack(cl, drop_cold)
    type(CompactLayer), intent(inout) :: cl
    logical, intent(in), optional :: drop_cold
    integer(kind=int64) :: k, i, i0, i1, pos, word, off, v, mask
    integer(kind=int32) :: w
    if (allocated(cl%keys)) return
    if (.not. allocated(cl%blk_base)) error stop "CompactUnpack: no cold form"
    allocate(cl%keys(cl%n))
    !$omp parallel do private(i, i0, i1, w, pos, word, off, v, mask) schedule(static)
    do k = 1, cl%nblk
       i0 = (k - 1) * K_CL_BLOCK + 1
       i1 = min(cl%n, k * K_CL_BLOCK)
       w = int(cl%blk_width(k), int32)
       cl%keys(i0) = cl%blk_base(k)
       mask = shiftl(1_int64, w) - 1_int64
       pos = cl%blk_bitoff(k)
       do i = i0 + 1, i1
          if (w == 0) then
             v = 0
          else
             word = pos / 64 + 1
             off = mod(pos, 64_int64)
             v = shiftr(cl%packed(word), int(off))
             if (off + w > 64) v = ior(v, shiftl(cl%packed(word + 1), int(64 - off)))
             v = iand(v, mask)
          end if
          cl%keys(i) = cl%keys(i - 1) + v
          pos = pos + w
       end do
    end do
    if (cl%wide) then
       allocate(cl%sid(cl%n))
       !$omp parallel do schedule(static)
       do i = 1, cl%n
          cl%sid(i) = int(get_field(cl%sidpack, i, cl%sbits), int32)
       end do
    end if
    if (present(drop_cold)) then
       if (drop_cold) then
          deallocate(cl%blk_base, cl%blk_bitoff, cl%blk_width, cl%packed)
          if (allocated(cl%sidpack)) deallocate(cl%sidpack)
       end if
    end if
  end subroutine CompactUnpack

  !> Decode blocks kb0..kb1 (1-based, 128 keys each) into boxes(1:nout), from the hot form
  !> when present, else straight from the cold bit-packed form. Read-only on cl: safe to
  !> call concurrently (MAGVIEWER chunk cache, per-chunk parallel builds).
  subroutine CompactDecodeBlocks(cl, kb0, kb1, boxes, nout)
    type(CompactLayer), intent(in) :: cl
    integer(kind=int64), intent(in) :: kb0, kb1
    type(Box), intent(inout) :: boxes(:)
    integer(kind=int64), intent(out) :: nout
    integer(kind=int64) :: k, i, i0, i1, pos, word, off, v, mask, key, smask
    integer(kind=int32) :: w
    nout = 0
    if (kb0 > kb1 .or. kb0 < 1 .or. kb1 > cl%nblk) return
    smask = shiftl(1_int64, cl%sbits) - 1_int64
    if (allocated(cl%keys)) then
       i0 = (kb0 - 1) * K_CL_BLOCK + 1
       i1 = min(cl%n, kb1 * K_CL_BLOCK)
       do i = i0, i1
          nout = nout + 1
          call decode_at(cl, cl%keys(i), i, boxes(nout))
       end do
       return
    end if
    do k = kb0, kb1
       i0 = (k - 1) * K_CL_BLOCK + 1
       i1 = min(cl%n, k * K_CL_BLOCK)
       w = int(cl%blk_width(k), int32)
       mask = shiftl(1_int64, w) - 1_int64
       pos = cl%blk_bitoff(k)
       key = cl%blk_base(k)
       nout = nout + 1
       call decode_at(cl, key, i0, boxes(nout))
       do i = i0 + 1, i1
          if (w == 0) then
             v = 0
          else
             word = pos / 64 + 1
             off = mod(pos, 64_int64)
             v = shiftr(cl%packed(word), int(off))
             if (off + w > 64) v = ior(v, shiftl(cl%packed(word + 1), int(64 - off)))
             v = iand(v, mask)
          end if
          key = key + v
          pos = pos + w
          nout = nout + 1
          call decode_at(cl, key, i, boxes(nout))
       end do
    end do
  end subroutine CompactDecodeBlocks

  !> Cold form -> Box array, in parallel over 64-block groups, without building the
  !> 8 B/box hot key array. The cold form is left intact.
  subroutine CompactDecodeCold(cl, boxes)
    type(CompactLayer), intent(in) :: cl
    type(Box), allocatable, intent(inout) :: boxes(:)
    integer(kind=int64), parameter :: G = 64
    integer(kind=int64) :: ig, ng, kb0, kb1, nout
    if (allocated(boxes)) deallocate(boxes)
    allocate(boxes(max(cl%n, 1_int64)))
    if (cl%n <= 0) return
    if (allocated(cl%keys)) then
       call CompactDecodeBlocks(cl, 1_int64, cl%nblk, boxes, nout)   ! hot form present: one pass
       return
    end if
    ng = (cl%nblk + G - 1) / G
    !$omp parallel do private(kb0, kb1, nout) schedule(dynamic, 4)
    do ig = 1, ng
       kb0 = (ig - 1) * G + 1
       kb1 = min(ig * G, cl%nblk)
       call CompactDecodeBlocks(cl, kb0, kb1, boxes((kb0 - 1) * K_CL_BLOCK + 1:), nout)
    end do
  end subroutine CompactDecodeCold

  !> Cold (or hot) form -> boxes(1:cl%n) of a caller-provided array (multi-part expand).
  subroutine CompactDecodeColdInto(cl, boxes)
    type(CompactLayer), intent(in) :: cl
    type(Box), intent(inout) :: boxes(:)
    integer(kind=int64), parameter :: KG = 64
    integer(kind=int64) :: ig, ng, kb0, kb1, nout
    if (cl%n <= 0) return
    ng = (cl%nblk + KG - 1) / KG
    !$omp parallel do private(kb0, kb1, nout) schedule(dynamic, 4)
    do ig = 1, ng
       kb0 = (ig - 1) * KG + 1
       kb1 = min(ig * KG, cl%nblk)
       call CompactDecodeBlocks(cl, kb0, kb1, boxes((kb0 - 1) * K_CL_BLOCK + 1:), nout)
    end do
  end subroutine CompactDecodeColdInto

  !> Order-independent 64-bit content hash of a box set (sum of mixed per-box hashes).
  integer(kind=int64) function BoxSetHash(boxes, n) result(h)
    type(Box), intent(in) :: boxes(:)
    integer(kind=int64), intent(in) :: n
    integer(kind=int64) :: i, z
    h = 0
    !$omp parallel do private(z) reduction(+:h) schedule(static)
    do i = 1, n
       z = ieor(shiftl(int(boxes(i)%x1, int64), 32), iand(int(boxes(i)%y1, int64), 4294967295_int64))
       z = ieor(z * (-7046029254386353131_int64), &
            ieor(shiftl(int(boxes(i)%x2, int64), 32), iand(int(boxes(i)%y2, int64), 4294967295_int64)))
       z = ieor(z, shiftr(z, 31)) * (-4658895280553007687_int64)
       z = ieor(z, shiftr(z, 29)) * (-7723592293110705685_int64)
       h = h + ieor(z, shiftr(z, 32))
    end do
  end function BoxSetHash

  !> Public sort of int64 keys (Thrust on the GPU build, std::sort otherwise).
  subroutine CompactSortU64(a, n)
    integer(kind=int64), intent(inout), target :: a(:)
    integer(kind=int64), intent(in) :: n
    call sort_u64(a, n)
  end subroutine CompactSortU64

  !> Merge independently encoded compact layers (e.g. per-band outputs) into one, without
  !> materialising the boxes: global origin = min origin, grid = gcd of the part grids and
  !> origin offsets, shape table = union of the part tables; every part's blocks are decoded
  !> in parallel straight into the global key array, which is then sorted and de-duplicated.
  !> Peak memory is the 8 B/box key array plus the parts. Parts are released.
  !> status: 0 ok, 1 empty, 2 keys exceed 62 bits (parts left intact for the caller).
  subroutine CompactMergeParts(parts, np, out, status)
    type(CompactLayer), intent(inout) :: parts(:)
    integer, intent(in) :: np
    type(CompactLayer), intent(inout) :: out
    integer, intent(out) :: status
    integer(kind=int64), parameter :: KG = 64
    integer(kind=int64) :: ntot, ox, oy, g, nsh, nshape, qmax, it, nit, kb0, kb1, nout, j, wh, qx, qy
    integer(kind=int64), allocatable :: whk(:), off(:), item_p(:), item_kb(:)
    type(Box), allocatable :: buf(:)
    integer :: p, lo, hi, mid
    real(kind=real64) :: tp

    call CompactRelease(out)
    status = 0
    ntot = 0
    do p = 1, np
       ntot = ntot + parts(p)%n
    end do
    if (ntot <= 0) then
       status = 1
       return
    end if
    tp = PhaseBegin('cl.merge')
    ox = huge(ox); oy = huge(oy)
    do p = 1, np
       if (parts(p)%n <= 0) cycle
       ox = min(ox, parts(p)%ox); oy = min(oy, parts(p)%oy)
    end do
    g = 0; nsh = 0
    do p = 1, np
       if (parts(p)%n <= 0) cycle
       g = gcd64(g, parts(p)%grid)
       g = gcd64(g, parts(p)%ox - ox)
       g = gcd64(g, parts(p)%oy - oy)
       nsh = nsh + size(parts(p)%shape_w)
    end do
    if (g == 0) g = 1
    allocate(whk(nsh))
    nsh = 0
    do p = 1, np
       if (parts(p)%n <= 0) cycle
       do j = 1, size(parts(p)%shape_w)
          nsh = nsh + 1
          whk(nsh) = ior(shiftl(int(parts(p)%shape_w(j), int64), 32), int(parts(p)%shape_h(j), int64))
       end do
    end do
    call sort_u64(whk, nsh)
    nshape = 1
    do j = 2, nsh
       if (whk(j) /= whk(nshape)) then
          nshape = nshape + 1
          whk(nshape) = whk(j)
       end if
    end do
    out%sbits = bit_length(nshape - 1)
    allocate(out%shape_w(nshape), out%shape_h(nshape))
    do j = 1, nshape
       out%shape_w(j) = int(shiftr(whk(j), 32), int32)
       out%shape_h(j) = int(iand(whk(j), int(z'00000000FFFFFFFF', int64)), int32)
    end do

    ! work items: (part, 64-block group) with the key offset of the group's first box
    nit = 0
    do p = 1, np
       if (parts(p)%n > 0) nit = nit + (parts(p)%nblk + KG - 1) / KG
    end do
    allocate(item_p(nit), item_kb(nit), off(nit))
    nit = 0; j = 0
    do p = 1, np
       if (parts(p)%n <= 0) cycle
       do kb0 = 1, parts(p)%nblk, KG
          nit = nit + 1
          item_p(nit) = p; item_kb(nit) = kb0; off(nit) = j + (kb0 - 1) * K_CL_BLOCK
       end do
       j = j + parts(p)%n
    end do

    allocate(out%keys(ntot), out%sid(ntot))    !> Morton codes + shape ids; narrowed below if they fit
    qmax = 0
    !$omp parallel private(it, p, kb0, kb1, nout, j, wh, qx, qy, lo, hi, mid, buf) reduction(max:qmax)
    allocate(buf(KG * K_CL_BLOCK))
    !$omp do schedule(dynamic, 4)
    do it = 1, nit
       p = int(item_p(it)); kb0 = item_kb(it); kb1 = min(kb0 + KG - 1, parts(p)%nblk)
       call CompactDecodeBlocks(parts(p), kb0, kb1, buf, nout)
       do j = 1, nout
          wh = ior(shiftl(int(buf(j)%x2 - buf(j)%x1, int64), 32), int(buf(j)%y2 - buf(j)%y1, int64))
          lo = 1; hi = int(nshape)
          do while (lo < hi)
             mid = (lo + hi) / 2
             if (whk(mid) < wh) then
                lo = mid + 1
             else
                hi = mid
             end if
          end do
          qx = (int(buf(j)%x1, int64) + int(buf(j)%x2, int64) - ox) / g
          qy = (int(buf(j)%y1, int64) + int(buf(j)%y2, int64) - oy) / g
          qmax = max(qmax, qx, qy)
          out%keys(off(it) + j) = ior(spread_bits(qx), shiftl(spread_bits(qy), 1))
          out%sid(off(it) + j) = lo - 1
       end do
    end do
    !$omp end do
    deallocate(buf)
    !$omp end parallel
    deallocate(whk, item_p, item_kb, off)

    out%cbits = max(1, bit_length(qmax))
    if (out%cbits > 31) then
       call CompactRelease(out)
       status = 2
       call PhaseEnd('cl.merge', tp, 0_int64)
       return
    end if
    out%ox = ox; out%oy = oy; out%grid = g
    out%n_input = 0
    do p = 1, np
       out%n_input = out%n_input + parts(p)%n_input
       call CompactRelease(parts(p))
    end do
    out%wide = (2 * out%cbits + out%sbits > 62) .or. force_wide()
    if (out%wide) then
       call sort_pairs(out%keys, out%sid, ntot)
    else
       !$omp parallel do schedule(static)
       do j = 1, ntot
          out%keys(j) = ior(shiftl(out%keys(j), out%sbits), int(out%sid(j), int64))
       end do
       deallocate(out%sid)
       call sort_u64(out%keys, ntot)
    end if
    call dedup_sorted(out, ntot)
    call build_block_mbrs(out)
    call PhaseEnd('cl.merge', tp, out%n)
  end subroutine CompactMergeParts

  !> Move src into dst without copying the arrays; src is left empty.
  subroutine CompactMove(src, dst)
    type(CompactLayer), intent(inout) :: src, dst
    call CompactRelease(dst)
    dst%n = src%n; dst%n_input = src%n_input; dst%ox = src%ox; dst%oy = src%oy
    dst%grid = src%grid; dst%cbits = src%cbits; dst%sbits = src%sbits; dst%nblk = src%nblk
    if (allocated(src%shape_w)) call move_alloc(src%shape_w, dst%shape_w)
    if (allocated(src%shape_h)) call move_alloc(src%shape_h, dst%shape_h)
    if (allocated(src%keys)) call move_alloc(src%keys, dst%keys)
    if (allocated(src%blk_base)) call move_alloc(src%blk_base, dst%blk_base)
    if (allocated(src%blk_bitoff)) call move_alloc(src%blk_bitoff, dst%blk_bitoff)
    if (allocated(src%blk_width)) call move_alloc(src%blk_width, dst%blk_width)
    if (allocated(src%packed)) call move_alloc(src%packed, dst%packed)
    if (allocated(src%blk_mbr)) call move_alloc(src%blk_mbr, dst%blk_mbr)
    dst%wide = src%wide
    if (allocated(src%sid)) call move_alloc(src%sid, dst%sid)
    if (allocated(src%sidpack)) call move_alloc(src%sidpack, dst%sidpack)
    call CompactRelease(src)
  end subroutine CompactMove

  subroutine CompactRelease(cl)
    type(CompactLayer), intent(inout) :: cl
    if (allocated(cl%shape_w)) deallocate(cl%shape_w)
    if (allocated(cl%shape_h)) deallocate(cl%shape_h)
    if (allocated(cl%keys)) deallocate(cl%keys)
    if (allocated(cl%blk_base)) deallocate(cl%blk_base)
    if (allocated(cl%blk_bitoff)) deallocate(cl%blk_bitoff)
    if (allocated(cl%blk_width)) deallocate(cl%blk_width)
    if (allocated(cl%packed)) deallocate(cl%packed)
    if (allocated(cl%blk_mbr)) deallocate(cl%blk_mbr)
    if (allocated(cl%sid)) deallocate(cl%sid)
    if (allocated(cl%sidpack)) deallocate(cl%sidpack)
    cl%n = 0; cl%n_input = 0; cl%nblk = 0; cl%grid = 1; cl%cbits = 0; cl%sbits = 0; cl%wide = .false.
  end subroutine CompactRelease

  !> Bytes held (hot + cold + index + shape table).
  pure integer(kind=int64) function CompactBytes(cl) result(nb)
    type(CompactLayer), intent(in) :: cl
    nb = 64
    if (allocated(cl%keys)) nb = nb + 8 * size(cl%keys, kind=int64)
    if (allocated(cl%packed)) nb = nb + 8 * size(cl%packed, kind=int64)
    if (allocated(cl%blk_base)) nb = nb + 17 * size(cl%blk_base, kind=int64)
    if (allocated(cl%blk_mbr)) nb = nb + 16 * size(cl%blk_mbr, kind=int64)
    if (allocated(cl%shape_w)) nb = nb + 8 * size(cl%shape_w, kind=int64)
    if (allocated(cl%sid)) nb = nb + 4 * size(cl%sid, kind=int64)
    if (allocated(cl%sidpack)) nb = nb + 8 * size(cl%sidpack, kind=int64)
  end function CompactBytes

  subroutine CompactPrintStats(cl, label)
    type(CompactLayer), intent(in) :: cl
    character(len=*), intent(in) :: label
    real(kind=real64) :: bpb
    bpb = real(CompactBytes(cl), real64) / real(max(cl%n, 1_int64), real64)
    write(*,'(A,A,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,A,F6.2,A,F6.1,A)') 'COMPACT ', trim(label), ' n=', cl%n, &
         ' (in ', cl%n_input, ') grid=', cl%grid, ' shapes=', size(cl%shape_w), ' cbits=', cl%cbits, &
         ' sbits=', cl%sbits, merge(' (wide)', '       ', cl%wide), ' bytes/box=', bpb, ' (', 16.0_real64 * real(cl%n_input, real64) / &
         real(max(CompactBytes(cl), 1_int64), real64), 'x vs Box)'
  end subroutine CompactPrintStats

end module CompactLayerModule
