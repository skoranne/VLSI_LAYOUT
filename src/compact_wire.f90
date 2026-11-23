! File   : compact_wire.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: Network serial form ("CLW1") of a CompactLayer, used by the MAGPARSER RPC
!          server (rpc_server.f90) and by the Julia cluster (cluster/mp/CompactWire.jl).
!
!  All fields little-endian (the RPC server refuses to start on big-endian hosts).
!  Header, 96 bytes:
!     0 'CLW1'            4 int32 version (1)    8 int32 form   12 int32 flags (bit0 = wide)
!    16 int64 n          24 int64 n_input       32 int64 ox    40 int64 oy    48 int64 grid
!    56 int32 cbits      60 int32 sbits         64 int64 nshape               72 int64 nblk
!    80 int64 npacked (cold: words in packed)   88 int64 nsidpack (cold wide: words in sidpack)
!  Body:
!    form 0 (hot) : int32 shape_w(nshape), int32 shape_h(nshape), int64 keys(n)
!                   [, int32 sid(n) if wide]
!    form 1 (cold): int32 shape_w(nshape), int32 shape_h(nshape), int64 blk_base(nblk),
!                   int64 blk_bitoff(nblk+1), int8 blk_width(nblk), zero pad to 8 bytes,
!                   int64 packed(npacked) [, int64 sidpack(nsidpack) if wide]
!    form 2 (raw) : int32 (x1,y1,x2,y2) * n  -- for layers CompactEncode cannot encode
!  An empty layer is form 1 with n = nshape = nblk = 0.
!  The per-block MBR index is not transmitted (it is not needed to expand a layer).
module CompactWireModule
  use iso_fortran_env, only: int8, int32, int64
  use CommonModule, only: K_COORDINATE_KIND
  use GeometryModule, only: Box
  use CompactLayerModule, only: CompactLayer, CompactRelease, K_CL_BLOCK
  implicit none
  private
  public :: CompactToWire, BoxesToWire, CompactFromWire, K_CLW_HEADER, &
       K_CLW_HOT, K_CLW_COLD, K_CLW_RAW

  integer(int64), parameter :: K_CLW_HEADER = 96
  integer(int32), parameter :: K_CLW_HOT = 0, K_CLW_COLD = 1, K_CLW_RAW = 2
  !> 'CLW1' as literal bytes (nvfortran 26.1 mis-evaluates ichar(..., int8) in this constructor)
  integer(int8),  parameter :: MAGIC(4) = [67_int8, 76_int8, 87_int8, 49_int8]

contains

  !--------------------------------------------------------------- little helpers
  subroutine put32(buf, p, v)
    integer(int8), intent(inout) :: buf(:)
    integer(int64), intent(inout) :: p
    integer(int32), intent(in) :: v
    buf(p+1:p+4) = transfer(v, [0_int8], 4); p = p + 4
  end subroutine put32

  subroutine put64(buf, p, v)
    integer(int8), intent(inout) :: buf(:)
    integer(int64), intent(inout) :: p
    integer(int64), intent(in) :: v
    buf(p+1:p+8) = transfer(v, [0_int8], 8); p = p + 8
  end subroutine put64

  subroutine put32a(buf, p, a, m)
    integer(int8), intent(inout) :: buf(:)
    integer(int64), intent(inout) :: p
    integer(int32), intent(in) :: a(:)
    integer(int64), intent(in) :: m
    if (m > 0) buf(p+1:p+4*m) = transfer(a(1:m), [0_int8], 4*m)
    p = p + 4*m
  end subroutine put32a

  subroutine put64a(buf, p, a, m)
    integer(int8), intent(inout) :: buf(:)
    integer(int64), intent(inout) :: p
    integer(int64), intent(in) :: a(:)
    integer(int64), intent(in) :: m
    if (m > 0) buf(p+1:p+8*m) = transfer(a(1:m), [0_int8], 8*m)
    p = p + 8*m
  end subroutine put64a

  integer(int32) function get32(buf, p) result(v)
    integer(int8), intent(in) :: buf(:)
    integer(int64), intent(inout) :: p
    v = transfer(buf(p+1:p+4), 0_int32); p = p + 4
  end function get32

  integer(int64) function get64(buf, p) result(v)
    integer(int8), intent(in) :: buf(:)
    integer(int64), intent(inout) :: p
    v = transfer(buf(p+1:p+8), 0_int64); p = p + 8
  end function get64

  subroutine header(buf, form, wide, cl, nshape, nblk, npacked, nsid)
    integer(int8), intent(inout) :: buf(:)
    integer(int32), intent(in) :: form
    logical, intent(in) :: wide
    type(CompactLayer), intent(in) :: cl
    integer(int64), intent(in) :: nshape, nblk, npacked, nsid
    integer(int64) :: p
    buf(1:4) = MAGIC; p = 4
    call put32(buf, p, 1_int32); call put32(buf, p, form); call put32(buf, p, merge(1_int32, 0_int32, wide))
    call put64(buf, p, cl%n); call put64(buf, p, cl%n_input); call put64(buf, p, cl%ox)
    call put64(buf, p, cl%oy); call put64(buf, p, cl%grid)
    call put32(buf, p, cl%cbits); call put32(buf, p, cl%sbits)
    call put64(buf, p, nshape); call put64(buf, p, nblk); call put64(buf, p, npacked); call put64(buf, p, nsid)
  end subroutine header

  !--------------------------------------------------------------- encode
  !> CompactLayer (cold form if present, else hot form) -> CLW1 bytes.
  subroutine CompactToWire(cl, buf)
    type(CompactLayer), intent(in) :: cl
    integer(int8), allocatable, intent(out) :: buf(:)
    integer(int64) :: p, nshape, npacked, nsid, wpad, total
    logical :: cold
    nshape = 0
    if (allocated(cl%shape_w)) nshape = size(cl%shape_w, kind=int64)
    cold = allocated(cl%blk_base) .or. .not. allocated(cl%keys)
    if (cl%n <= 0) then                                     ! empty layer
       allocate(buf(K_CLW_HEADER)); buf = 0
       block
         type(CompactLayer) :: e
         call header(buf, K_CLW_COLD, .false., e, 0_int64, 0_int64, 0_int64, 0_int64)
       end block
       return
    end if
    if (cold) then
       npacked = size(cl%packed, kind=int64)
       nsid = 0
       if (cl%wide) nsid = size(cl%sidpack, kind=int64)
       wpad = ((cl%nblk + 7) / 8) * 8
       total = K_CLW_HEADER + 8*nshape + 8*cl%nblk + 8*(cl%nblk + 1) + wpad + 8*npacked + 8*nsid
    else
       npacked = 0; nsid = 0; wpad = 0
       total = K_CLW_HEADER + 8*nshape + 8*cl%n
       if (cl%wide) total = total + 4*cl%n
    end if
    allocate(buf(total)); buf = 0
    call header(buf, merge(K_CLW_COLD, K_CLW_HOT, cold), cl%wide, cl, nshape, cl%nblk, npacked, nsid)
    p = K_CLW_HEADER
    call put32a(buf, p, cl%shape_w, nshape)
    call put32a(buf, p, cl%shape_h, nshape)
    if (cold) then
       call put64a(buf, p, cl%blk_base, cl%nblk)
       call put64a(buf, p, cl%blk_bitoff, cl%nblk + 1)
       buf(p+1:p+cl%nblk) = cl%blk_width(1:cl%nblk)
       p = p + wpad
       call put64a(buf, p, cl%packed, npacked)
       if (cl%wide) call put64a(buf, p, cl%sidpack, nsid)
    else
       call put64a(buf, p, cl%keys, cl%n)
       if (cl%wide) call put32a(buf, p, cl%sid, cl%n)
    end if
  end subroutine CompactToWire

  !> Plain boxes -> CLW1 raw form (fallback when CompactEncode reports status 2/3).
  subroutine BoxesToWire(boxes, n, buf)
    type(Box), intent(in) :: boxes(:)
    integer(int64), intent(in) :: n
    integer(int8), allocatable, intent(out) :: buf(:)
    integer(int64) :: p, i
    type(CompactLayer) :: e
    allocate(buf(K_CLW_HEADER + 16*max(n, 0_int64))); buf = 0
    e%n = n; e%n_input = n
    call header(buf, K_CLW_RAW, .false., e, 0_int64, 0_int64, 0_int64, 0_int64)
    p = K_CLW_HEADER
    do i = 1, n
       call put32(buf, p, int(boxes(i)%x1, int32)); call put32(buf, p, int(boxes(i)%y1, int32))
       call put32(buf, p, int(boxes(i)%x2, int32)); call put32(buf, p, int(boxes(i)%y2, int32))
    end do
  end subroutine BoxesToWire

  !--------------------------------------------------------------- decode
  !> CLW1 bytes -> CompactLayer (form 0/1, status 0) or plain boxes (form 2, status 2).
  !> status < 0: malformed buffer.
  subroutine CompactFromWire(buf, cl, boxes, nbox, status)
    integer(int8), intent(in) :: buf(:)
    type(CompactLayer), intent(inout) :: cl
    type(Box), allocatable, intent(inout) :: boxes(:)
    integer(int64), intent(out) :: nbox
    integer, intent(out) :: status
    integer(int64) :: p, nshape, nblk, npacked, nsid, wpad, need, i, nb
    integer(int32) :: ver, form, flags
    call CompactRelease(cl)
    nbox = 0; status = -1
    nb = size(buf, kind=int64)
    if (nb < K_CLW_HEADER) return
    if (any(buf(1:4) /= MAGIC)) return
    p = 4
    ver = get32(buf, p); form = get32(buf, p); flags = get32(buf, p)
    if (ver /= 1) return
    cl%n = get64(buf, p); cl%n_input = get64(buf, p); cl%ox = get64(buf, p); cl%oy = get64(buf, p)
    cl%grid = get64(buf, p); cl%cbits = get32(buf, p); cl%sbits = get32(buf, p)
    nshape = get64(buf, p); nblk = get64(buf, p); npacked = get64(buf, p); nsid = get64(buf, p)
    cl%wide = btest(flags, 0)
    if (cl%n < 0 .or. nshape < 0 .or. nblk < 0 .or. npacked < 0 .or. nsid < 0) return

    if (form == K_CLW_RAW) then
       if (nb < K_CLW_HEADER + 16*cl%n) return
       nbox = cl%n
       if (allocated(boxes)) deallocate(boxes)
       allocate(boxes(max(nbox, 1_int64)))
       p = K_CLW_HEADER
       do i = 1, nbox
          boxes(i)%x1 = int(get32(buf, p), K_COORDINATE_KIND); boxes(i)%y1 = int(get32(buf, p), K_COORDINATE_KIND)
          boxes(i)%x2 = int(get32(buf, p), K_COORDINATE_KIND); boxes(i)%y2 = int(get32(buf, p), K_COORDINATE_KIND)
       end do
       call CompactRelease(cl)
       status = 2
       return
    end if

    if (cl%n == 0) then                       ! empty layer
       call CompactRelease(cl)
       status = 0
       return
    end if
    if (nshape < 1 .or. nblk /= (cl%n + K_CL_BLOCK - 1) / K_CL_BLOCK) return
    cl%nblk = nblk
    p = K_CLW_HEADER
    if (nb < p + 8*nshape) return
    allocate(cl%shape_w(nshape), cl%shape_h(nshape))
    cl%shape_w = transfer(buf(p+1:p+4*nshape), cl%shape_w, nshape); p = p + 4*nshape
    cl%shape_h = transfer(buf(p+1:p+4*nshape), cl%shape_h, nshape); p = p + 4*nshape

    if (form == K_CLW_HOT) then
       need = p + 8*cl%n
       if (cl%wide) need = need + 4*cl%n
       if (nb < need) return
       allocate(cl%keys(cl%n))
       cl%keys = transfer(buf(p+1:p+8*cl%n), cl%keys, cl%n); p = p + 8*cl%n
       if (cl%wide) then
          allocate(cl%sid(cl%n))
          cl%sid = transfer(buf(p+1:p+4*cl%n), cl%sid, cl%n)
       end if
    else if (form == K_CLW_COLD) then
       wpad = ((nblk + 7) / 8) * 8
       if (nb < p + 8*nblk + 8*(nblk + 1) + wpad + 8*npacked + 8*nsid) return
       allocate(cl%blk_base(nblk), cl%blk_bitoff(nblk + 1), cl%blk_width(nblk), cl%packed(max(npacked, 1_int64)))
       cl%blk_base = transfer(buf(p+1:p+8*nblk), cl%blk_base, nblk); p = p + 8*nblk
       cl%blk_bitoff = transfer(buf(p+1:p+8*(nblk+1)), cl%blk_bitoff, nblk + 1); p = p + 8*(nblk + 1)
       cl%blk_width = buf(p+1:p+nblk); p = p + wpad
       cl%packed = 0
       if (npacked > 0) cl%packed(1:npacked) = transfer(buf(p+1:p+8*npacked), cl%packed, npacked)
       p = p + 8*npacked
       if (cl%wide) then
          allocate(cl%sidpack(max(nsid, 1_int64)))
          cl%sidpack = 0
          if (nsid > 0) cl%sidpack(1:nsid) = transfer(buf(p+1:p+8*nsid), cl%sidpack, nsid)
       end if
    else
       return
    end if
    status = 0
  end subroutine CompactFromWire

end module CompactWireModule
