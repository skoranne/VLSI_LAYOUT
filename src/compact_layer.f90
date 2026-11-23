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
  use omp_lib
  implicit none
  private

  public :: CompactLayer, CompactEncode, CompactDecode, CompactPack, CompactUnpack, &
       CompactRelease, CompactBytes, CompactPrintStats, CompactKeyOf, K_CL_BLOCK

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
  end type CompactLayer

  interface
     function thrust_sort_u64_host(h_keys, n) result(rc) bind(c, name="thrust_sort_u64_host")
       import :: c_ptr, c_int64_t, c_int32_t
       type(c_ptr), value :: h_keys
       integer(c_int64_t), value :: n
       integer(c_int32_t) :: rc
     end function thrust_sort_u64_host
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

    call CompactRelease(cl)
    status = 0
    if (n <= 0) then
       status = 1
       return
    end if

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
       return
    end if

    ! grid = gcd of all centre-sum offsets (per-thread gcd, then combine)
    g = 0
    !$omp parallel private(i, j)
    j = 0
    !$omp do schedule(static)
    do i = 1, n
       if (j /= 1_int64) then
          j = gcd64(j, int(boxes(i)%x1, int64) + int(boxes(i)%x2, int64) - ox)
          j = gcd64(j, int(boxes(i)%y1, int64) + int(boxes(i)%y2, int64) - oy)
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
    if (2 * cl%cbits + cl%sbits > 62 .or. cl%cbits > 31) then
       deallocate(whk)
       status = 2
       return
    end if
    cl%ox = ox; cl%oy = oy; cl%grid = g; cl%n_input = n
    allocate(cl%shape_w(nshape), cl%shape_h(nshape))
    do i = 1, nshape
       cl%shape_w(i) = int(shiftr(whk(i), 32), int32)
       cl%shape_h(i) = int(iand(whk(i), int(z'00000000FFFFFFFF', int64)), int32)
    end do

    ! keys
    allocate(cl%keys(n))
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
       cl%keys(i) = ior(shiftl(ior(spread_bits((int(boxes(i)%x1, int64) + int(boxes(i)%x2, int64) - ox) / g), &
            shiftl(spread_bits((int(boxes(i)%y1, int64) + int(boxes(i)%y2, int64) - oy) / g), 1)), cl%sbits), &
            int(lo - 1, int64))
    end do
    deallocate(whk)
    call sort_u64(cl%keys, n)

    ! exact duplicates (identical boxes) carry no information for region operations
    nu = 1
    do i = 2, n
       if (cl%keys(i) /= cl%keys(nu)) then
          nu = nu + 1
          cl%keys(nu) = cl%keys(i)
       end if
    end do
    if (nu < n) then
       block
         integer(kind=int64), allocatable :: tmp(:)
         allocate(tmp(nu))
         tmp(1:nu) = cl%keys(1:nu)
         call move_alloc(tmp, cl%keys)
       end block
    end if
    cl%n = nu
    call build_block_mbrs(cl)
  end subroutine CompactEncode

  !> Decode key -> box (exact).
  pure subroutine decode_key(key, sbits, smask, ox, oy, g, sw, sh, b)
    integer(kind=int64), intent(in) :: key, smask, ox, oy, g
    integer(kind=int32), intent(in) :: sbits
    integer(kind=int32), intent(in) :: sw(:), sh(:)
    type(Box), intent(out) :: b
    integer(kind=int64) :: m, sx, sy, w, h
    integer :: sid
    sid = int(iand(key, smask)) + 1
    m = shiftr(key, sbits)
    sx = compact_bits(m) * g + ox
    sy = compact_bits(shiftr(m, 1)) * g + oy
    w = sw(sid); h = sh(sid)
    b%x1 = int((sx - w) / 2, K_COORDINATE_KIND); b%x2 = int((sx - w) / 2 + w, K_COORDINATE_KIND)
    b%y1 = int((sy - h) / 2, K_COORDINATE_KIND); b%y2 = int((sy - h) / 2 + h, K_COORDINATE_KIND)
  end subroutine decode_key

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
       call decode_key(cl%keys(i0), cl%sbits, smask, cl%ox, cl%oy, cl%grid, cl%shape_w, cl%shape_h, m)
       do i = i0 + 1, i1
          call decode_key(cl%keys(i), cl%sbits, smask, cl%ox, cl%oy, cl%grid, cl%shape_w, cl%shape_h, b)
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
       call decode_key(cl%keys(i), cl%sbits, smask, cl%ox, cl%oy, cl%grid, cl%shape_w, cl%shape_h, boxes(i))
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
    if (present(drop_cold)) then
       if (drop_cold) then
          deallocate(cl%blk_base, cl%blk_bitoff, cl%blk_width, cl%packed)
       end if
    end if
  end subroutine CompactUnpack

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
    cl%n = 0; cl%n_input = 0; cl%nblk = 0; cl%grid = 1; cl%cbits = 0; cl%sbits = 0
  end subroutine CompactRelease

  !> Bytes held (hot + cold + index + shape table).
  integer(kind=int64) function CompactBytes(cl) result(nb)
    type(CompactLayer), intent(in) :: cl
    nb = 64
    if (allocated(cl%keys)) nb = nb + 8 * size(cl%keys, kind=int64)
    if (allocated(cl%packed)) nb = nb + 8 * size(cl%packed, kind=int64)
    if (allocated(cl%blk_base)) nb = nb + 17 * size(cl%blk_base, kind=int64)
    if (allocated(cl%blk_mbr)) nb = nb + 16 * size(cl%blk_mbr, kind=int64)
    if (allocated(cl%shape_w)) nb = nb + 8 * size(cl%shape_w, kind=int64)
  end function CompactBytes

  subroutine CompactPrintStats(cl, label)
    type(CompactLayer), intent(in) :: cl
    character(len=*), intent(in) :: label
    real(kind=real64) :: bpb
    bpb = real(CompactBytes(cl), real64) / real(max(cl%n, 1_int64), real64)
    write(*,'(A,A,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,F6.2,A,F6.1,A)') 'COMPACT ', trim(label), ' n=', cl%n, &
         ' (in ', cl%n_input, ') grid=', cl%grid, ' shapes=', size(cl%shape_w), ' cbits=', cl%cbits, &
         ' sbits=', cl%sbits, ' bytes/box=', bpb, ' (', 16.0_real64 * real(cl%n_input, real64) / &
         real(max(CompactBytes(cl), 1_int64), real64), 'x vs Box)'
  end subroutine CompactPrintStats

end module CompactLayerModule
