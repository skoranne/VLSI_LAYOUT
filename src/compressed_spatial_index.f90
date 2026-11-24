! File    : compressed_spatial_index.f90
! Author  : Sandeep Koranne (C) 2026
! Purpose : Low-memory compressed spatial index (16-bit quantized SoA R-Tree +
!           BoxCodec/Zstd compressed leaf chunks + bounded LRU chunk cache)
!           for fast interactive pan/zoom rendering of massive VLSI layouts.

module CompressedSpatialIndexModule
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
  use, intrinsic :: iso_c_binding,   only: c_int, c_int8_t, c_size_t, c_loc
  use CommonModule,            only: K_COORDINATE_KIND
  use GeometryModule,          only: Box, mbr_of_array, omt_pack
  use BoxCodecModule,          only: BoxCodec, CompressBoxesUsingCodec, DecompressBoxesUsingCodec
  use SnappyCompressionModule, only: zstd_compress_bound, zstd_compress, zstd_decompress, zstd_is_error
  use CompactLayerModule,      only: CompactLayer, CompactDecodeBlocks, CompactMove, CompactPack, &
                                     CompactRelease, CompactBytes, CompactEncode, K_CL_BLOCK
  implicit none
  private

  integer(int64), parameter, public :: K_CSI_LEAF_CAPACITY  = 32_int64
  integer(int64), parameter, public :: K_CSI_LEAVES_PER_BLK = 512_int64
  integer(int64), parameter, public :: K_CSI_BOXES_PER_BLK  = K_CSI_LEAF_CAPACITY * K_CSI_LEAVES_PER_BLK
  integer,        parameter, public :: K_CSI_DEFAULT_CACHE  = 16
  integer,        parameter         :: K_CSI_DFS_STACK_MAX  = 1024
  integer(c_int), parameter         :: K_CSI_ZSTD_LEVEL     = 1_c_int
  integer(int64), parameter         :: K_CSI_CL_BLOCKS      = K_CSI_BOXES_PER_BLK / K_CL_BLOCK

  type :: CompressedBoxChunk
     integer(int32) :: num_boxes = 0_int32
     integer(int32) :: codec_len = 0_int32
     logical        :: is_zstd   = .false.
     integer(int8), allocatable :: payload(:)
  end type CompressedBoxChunk

  type :: ChunkCacheSlot
     integer(int64) :: chunk_id   = 0_int64
     integer(int64) :: stamp      = 0_int64
     integer(int32) :: num_boxes  = 0_int32
     type(Box), allocatable :: boxes(:)
  end type ChunkCacheSlot

  type, public :: ChunkLRUCache
     integer :: capacity = 0
     integer(int64) :: clock  = 0_int64
     integer(int64) :: hits   = 0_int64
     integer(int64) :: misses = 0_int64
     type(ChunkCacheSlot), allocatable :: slots(:)
     integer(int8), allocatable :: scratch_codec(:)
     type(BoxCodec) :: codec
   contains
     procedure :: init    => cache_init
     procedure :: clear   => cache_clear
     procedure :: destroy => cache_destroy
  end type ChunkLRUCache

  type, public :: CompressedSpatialIndex
     integer(int64) :: n_boxes    = 0_int64
     integer(int64) :: n_leaves   = 0_int64
     integer(int64) :: n_nodes    = 0_int64
     integer(int64) :: root_index = 0_int64
     integer(int64) :: n_chunks   = 0_int64
     type(Box)      :: layer_mbr
     integer(int64) :: span_x     = 1_int64
     integer(int64) :: span_y     = 1_int64

     ! 16-bit conservatively quantized SoA R-Tree node MBRs (8 bytes/node)
     ! Stored shifted by -32768 so unsigned [0..65535] maps cleanly to int16.
     integer(int16), allocatable :: qx1(:)
     integer(int16), allocatable :: qy1(:)
     integer(int16), allocatable :: qx2(:)
     integer(int16), allocatable :: qy2(:)

     ! Topology stored ONLY for internal nodes (index = node_idx - n_leaves)
     integer(int32), allocatable :: int_child_start(:)
     integer(int16), allocatable :: int_child_count(:)

     ! Compressed geometry chunks (each holds up to K_CSI_BOXES_PER_BLK sorted boxes)
     type(CompressedBoxChunk), allocatable :: chunks(:)

     ! Alternative geometry store: a CompactLayer (Morton centre key + shape id, bit-packed
     ! 128-key blocks). Chunk i is compact blocks (i-1)*K_CSI_CL_BLOCKS+1 .. i*K_CSI_CL_BLOCKS,
     ! decoded with plain integer ops (no BoxCodec/zstd). Leaves follow Morton key order.
     logical            :: compact_backed = .false.
     type(CompactLayer) :: cl
     ! Multi-part store (layers compacted while streaming): parts 2.. in clx(:). Chunk ic
     ! decodes blocks chunk_kb0(ic)..chunk_kb1(ic) of part chunk_part(ic) (1 = cl). A partial
     ! chunk at the end of a part keeps its K_CSI_LEAVES_PER_BLK leaf slots; unused slots are
     ! empty (inverted MBR) and decode to nothing.
     type(CompactLayer), allocatable :: clx(:)
     integer(int32), allocatable :: chunk_part(:)
     integer(int64), allocatable :: chunk_kb0(:), chunk_kb1(:)
   contains
     procedure :: build_from_boxes    => csi_build_from_boxes
     procedure :: build_from_compact  => csi_build_from_compact
     procedure :: build_from_compact_parts => csi_build_from_compact_parts
     procedure :: decode_all          => csi_decode_all
     procedure :: build_auto          => csi_build_auto
     procedure :: destroy             => csi_destroy
     procedure :: is_empty            => csi_is_empty
     procedure :: memory_bytes        => csi_memory_bytes
     procedure :: tree_bytes          => csi_tree_bytes
     procedure :: chunk_bytes         => csi_chunk_bytes
     procedure :: uncompressed_bytes  => csi_uncompressed_bytes
     procedure :: compression_ratio   => csi_compression_ratio
     procedure :: query_mbrs_to_quads => csi_query_mbrs_to_quads
     procedure :: query_boxes_to_quads => csi_query_boxes_to_quads
  end type CompressedSpatialIndex

contains

  ! ------------------------------------------------------------------
  ! Conservative 16-bit quantization helpers (guaranteed superset MBR)
  ! ------------------------------------------------------------------
  pure function quantize_lo(val, origin, span) result(q)
    integer(K_COORDINATE_KIND), intent(in) :: val, origin
    integer(int64), intent(in)             :: span
    integer(int16)                         :: q
    integer(int64)                         :: d, u
    d = int(val, int64) - int(origin, int64)
    if (d <= 0_int64) then
       u = 0_int64
    else if (d >= span) then
       u = 65535_int64
    else
       u = (d * 65535_int64) / span
    end if
    q = int(u - 32768_int64, int16)
  end function quantize_lo

  pure function quantize_hi(val, origin, span) result(q)
    integer(K_COORDINATE_KIND), intent(in) :: val, origin
    integer(int64), intent(in)             :: span
    integer(int16)                         :: q
    integer(int64)                         :: d, u
    d = int(val, int64) - int(origin, int64)
    if (d <= 0_int64) then
       u = 0_int64
    else if (d >= span) then
       u = 65535_int64
    else
       u = (d * 65535_int64 + span - 1_int64) / span
       if (u > 65535_int64) u = 65535_int64
    end if
    q = int(u - 32768_int64, int16)
  end function quantize_hi

  pure function dequantize_lo(q, origin, span) result(val)
    integer(int16), intent(in)             :: q
    integer(K_COORDINATE_KIND), intent(in) :: origin
    integer(int64), intent(in)             :: span
    integer(K_COORDINATE_KIND)             :: val
    integer(int64)                         :: u
    u = int(q, int64) + 32768_int64
    val = int(int(origin, int64) + (u * span) / 65535_int64, K_COORDINATE_KIND)
  end function dequantize_lo

  pure function dequantize_hi(q, origin, span) result(val)
    integer(int16), intent(in)             :: q
    integer(K_COORDINATE_KIND), intent(in) :: origin
    integer(int64), intent(in)             :: span
    integer(K_COORDINATE_KIND)             :: val
    integer(int64)                         :: u
    u = int(q, int64) + 32768_int64
    val = int(int(origin, int64) + (u * span + 65534_int64) / 65535_int64, K_COORDINATE_KIND)
  end function dequantize_hi

  pure function compute_total_nodes(n_boxes, cap) result(total)
    integer(int64), intent(in) :: n_boxes, cap
    integer(int64) :: total, lvl
    total = 0_int64
    if (n_boxes <= 0_int64) return
    lvl = (n_boxes + cap - 1_int64) / cap
    total = lvl
    do while (lvl > 1_int64)
       lvl = (lvl + cap - 1_int64) / cap
       total = total + lvl
    end do
  end function compute_total_nodes

  subroutine ensure_quad_capacity(vbuf, needed_ints)
    integer(c_int), allocatable, intent(inout) :: vbuf(:)
    integer, intent(in)                        :: needed_ints
    integer(c_int), allocatable                :: tmp(:)
    integer                                    :: new_cap
    if (.not. allocated(vbuf)) then
       allocate(vbuf(max(needed_ints, 4096)))
    else if (size(vbuf) < needed_ints) then
       new_cap = max(needed_ints, size(vbuf) * 2)
       allocate(tmp(new_cap))
       tmp(1:size(vbuf)) = vbuf
       call move_alloc(from=tmp, to=vbuf)
    end if
  end subroutine ensure_quad_capacity

  ! ------------------------------------------------------------------
  ! ChunkLRUCache methods
  ! ------------------------------------------------------------------
  subroutine cache_init(this, capacity)
    class(ChunkLRUCache), intent(inout) :: this
    integer, intent(in), optional       :: capacity
    integer :: cap
    cap = K_CSI_DEFAULT_CACHE
    if (present(capacity)) cap = max(1, capacity)
    call this%destroy()
    this%capacity = cap
    this%clock    = 0_int64
    this%hits     = 0_int64
    this%misses   = 0_int64
    allocate(this%slots(cap))
  end subroutine cache_init

  subroutine cache_clear(this)
    class(ChunkLRUCache), intent(inout) :: this
    integer :: i
    this%clock  = 0_int64
    this%hits   = 0_int64
    this%misses = 0_int64
    if (allocated(this%slots)) then
       do i = 1, size(this%slots)
          this%slots(i)%chunk_id  = 0_int64
          this%slots(i)%stamp     = 0_int64
          this%slots(i)%num_boxes = 0_int32
       end do
    end if
  end subroutine cache_clear

  subroutine cache_destroy(this)
    class(ChunkLRUCache), intent(inout) :: this
    integer :: i
    if (allocated(this%slots)) then
       do i = 1, size(this%slots)
          if (allocated(this%slots(i)%boxes)) deallocate(this%slots(i)%boxes)
       end do
       deallocate(this%slots)
    end if
    if (allocated(this%scratch_codec)) deallocate(this%scratch_codec)
    this%capacity = 0
    this%clock    = 0_int64
    this%hits     = 0_int64
    this%misses   = 0_int64
  end subroutine cache_destroy

  ! ------------------------------------------------------------------
  ! Decompress a single chunk into a destination Box array
  ! ------------------------------------------------------------------
  subroutine decompress_chunk(chunk, out_boxes, scratch_codec, codec)
    type(CompressedBoxChunk), intent(in), target :: chunk
    type(Box), allocatable, intent(inout)        :: out_boxes(:)
    integer(int8), allocatable, intent(inout), target :: scratch_codec(:)
    type(BoxCodec), intent(inout)                :: codec

    integer(int64)  :: nb, clen
    integer(c_size_t) :: dsz

    nb   = int(chunk%num_boxes, int64)
    clen = int(chunk%codec_len, int64)
    if (nb <= 0_int64) return

    if (.not. allocated(out_boxes)) then
       allocate(out_boxes(max(nb, K_CSI_BOXES_PER_BLK)))
    else if (size(out_boxes, kind=int64) < nb) then
       deallocate(out_boxes)
       allocate(out_boxes(nb))
    end if

    if (.not. allocated(codec%unique_w)) call codec%init()
    ! Decompression only appends to unique_w/unique_h by index; hashing is unused
    ! and leaving stale hash entries across chunks can fill w_hash/h_hash.
    codec%w_hash_valid = .false.
    codec%h_hash_valid = .false.

    if (chunk%is_zstd) then
       if (.not. allocated(scratch_codec)) then
          allocate(scratch_codec(max(clen, 65536_int64)))
       else if (size(scratch_codec, kind=int64) < clen) then
          deallocate(scratch_codec)
          allocate(scratch_codec(clen))
       end if
       dsz = zstd_decompress(c_loc(scratch_codec(1)), int(clen, c_size_t), &
                             c_loc(chunk%payload(1)), int(size(chunk%payload, kind=int64), c_size_t))
       if (zstd_is_error(dsz) /= 0_c_int) then
          error stop 'CompressedSpatialIndex: ZSTD decompression failed'
       end if
       call DecompressBoxesUsingCodec(scratch_codec, clen, nb, out_boxes, codec)
    else
       call DecompressBoxesUsingCodec(chunk%payload, clen, nb, out_boxes, codec)
    end if
  end subroutine decompress_chunk

  subroutine cache_fetch_slot(csi, cache, chunk_id, slot_idx)
    type(CompressedSpatialIndex), intent(in) :: csi
    type(ChunkLRUCache), intent(inout)       :: cache
    integer(int64), intent(in)               :: chunk_id
    integer, intent(out)                     :: slot_idx

    integer :: i, victim
    integer(int64) :: oldest

    if (.not. allocated(cache%slots) .or. cache%capacity <= 0) then
       call cache%init(K_CSI_DEFAULT_CACHE)
    end if

    ! Fast hit scan (small capacity <= 32 fits in L1 cache)
    do i = 1, cache%capacity
       if (cache%slots(i)%chunk_id == chunk_id) then
          cache%clock = cache%clock + 1_int64
          cache%slots(i)%stamp = cache%clock
          cache%hits = cache%hits + 1_int64
          slot_idx = i
          return
       end if
    end do

    ! Miss: find empty or LRU slot
    cache%misses = cache%misses + 1_int64
    victim = 1
    oldest = huge(oldest)
    do i = 1, cache%capacity
       if (cache%slots(i)%chunk_id == 0_int64) then
          victim = i
          exit
       end if
       if (cache%slots(i)%stamp < oldest) then
          oldest = cache%slots(i)%stamp
          victim = i
       end if
    end do

    if (csi%compact_backed) then
       block
         integer(int64) :: nout
         if (.not. allocated(cache%slots(victim)%boxes)) allocate(cache%slots(victim)%boxes(K_CSI_BOXES_PER_BLK))
         call decode_chunk(csi, chunk_id, cache%slots(victim)%boxes, nout)
         cache%slots(victim)%num_boxes = int(nout, int32)
       end block
    else
       call decompress_chunk(csi%chunks(chunk_id), cache%slots(victim)%boxes, &
                             cache%scratch_codec, cache%codec)
       cache%slots(victim)%num_boxes = csi%chunks(chunk_id)%num_boxes
    end if
    cache%clock = cache%clock + 1_int64
    cache%slots(victim)%chunk_id  = chunk_id
    cache%slots(victim)%stamp     = cache%clock
    slot_idx = victim
  end subroutine cache_fetch_slot

  ! ------------------------------------------------------------------
  ! CompressedSpatialIndex lifecycle & metrics
  ! ------------------------------------------------------------------
  subroutine csi_destroy(this)
    class(CompressedSpatialIndex), intent(inout) :: this
    integer(int64) :: i
    if (allocated(this%qx1)) deallocate(this%qx1)
    if (allocated(this%qy1)) deallocate(this%qy1)
    if (allocated(this%qx2)) deallocate(this%qx2)
    if (allocated(this%qy2)) deallocate(this%qy2)
    if (allocated(this%int_child_start)) deallocate(this%int_child_start)
    if (allocated(this%int_child_count)) deallocate(this%int_child_count)
    if (allocated(this%chunks)) then
       do i = 1_int64, size(this%chunks, kind=int64)
          if (allocated(this%chunks(i)%payload)) deallocate(this%chunks(i)%payload)
       end do
       deallocate(this%chunks)
    end if
    if (this%compact_backed) call CompactRelease(this%cl)
    if (allocated(this%clx)) then
       do i = 1_int64, size(this%clx, kind=int64)
          call CompactRelease(this%clx(i))
       end do
       deallocate(this%clx)
    end if
    if (allocated(this%chunk_part)) deallocate(this%chunk_part)
    if (allocated(this%chunk_kb0)) deallocate(this%chunk_kb0)
    if (allocated(this%chunk_kb1)) deallocate(this%chunk_kb1)
    this%compact_backed = .false.
    this%n_boxes    = 0_int64
    this%n_leaves   = 0_int64
    this%n_nodes    = 0_int64
    this%root_index = 0_int64
    this%n_chunks   = 0_int64
    this%span_x     = 1_int64
    this%span_y     = 1_int64
    call this%layer_mbr%reset_to_infinity()
  end subroutine csi_destroy

  pure logical function csi_is_empty(this)
    class(CompressedSpatialIndex), intent(in) :: this
    csi_is_empty = (this%n_boxes <= 0_int64)
  end function csi_is_empty

  pure integer(int64) function csi_tree_bytes(this)
    class(CompressedSpatialIndex), intent(in) :: this
    integer(int64) :: n_int
    n_int = max(0_int64, this%n_nodes - this%n_leaves)
    ! 4 * int16 per node + (int32 + int16) per internal node
    csi_tree_bytes = this%n_nodes * 8_int64 + n_int * 6_int64
  end function csi_tree_bytes

  pure integer(int64) function csi_chunk_bytes(this)
    class(CompressedSpatialIndex), intent(in) :: this
    integer(int64) :: i
    csi_chunk_bytes = 0_int64
    if (this%compact_backed) then
       csi_chunk_bytes = CompactBytes(this%cl)
       if (allocated(this%clx)) then
          do i = 1_int64, size(this%clx, kind=int64)
             csi_chunk_bytes = csi_chunk_bytes + CompactBytes(this%clx(i))
          end do
       end if
       if (allocated(this%chunk_part)) csi_chunk_bytes = csi_chunk_bytes + 20_int64 * size(this%chunk_part, kind=int64)
       return
    end if
    if (.not. allocated(this%chunks)) return
    do i = 1_int64, this%n_chunks
       if (allocated(this%chunks(i)%payload)) then
          csi_chunk_bytes = csi_chunk_bytes + size(this%chunks(i)%payload, kind=int64)
       end if
       csi_chunk_bytes = csi_chunk_bytes + 16_int64
    end do
  end function csi_chunk_bytes

  pure integer(int64) function csi_memory_bytes(this)
    class(CompressedSpatialIndex), intent(in) :: this
    csi_memory_bytes = this%tree_bytes() + this%chunk_bytes()
  end function csi_memory_bytes

  pure integer(int64) function csi_uncompressed_bytes(this)
    class(CompressedSpatialIndex), intent(in) :: this
    ! Raw boxes (16 B/box) + standard RTreeNode (40 B/node with cap=16)
    integer(int64) :: std_nodes
    std_nodes = compute_total_nodes(this%n_boxes, 16_int64)
    csi_uncompressed_bytes = this%n_boxes * 16_int64 + std_nodes * 40_int64
  end function csi_uncompressed_bytes

  pure real(real64) function csi_compression_ratio(this)
    class(CompressedSpatialIndex), intent(in) :: this
    integer(int64) :: comp_b
    comp_b = this%memory_bytes()
    if (comp_b <= 0_int64) then
       csi_compression_ratio = 1.0_real64
    else
       csi_compression_ratio = real(this%uncompressed_bytes(), real64) / real(comp_b, real64)
    end if
  end function csi_compression_ratio

  ! ------------------------------------------------------------------
  ! Build compressed spatial index from a Box array (sorts in-place,
  ! builds quantized SoA R-Tree, compresses chunks in parallel).
  ! ------------------------------------------------------------------
  !> Internal R-tree levels over the quantized leaf MBRs qx1..qy2(1:n_leaves).
  subroutine build_internal_levels(this)
    class(CompressedSpatialIndex), intent(inout) :: this
    integer(int64) :: i, j, c_start, c_end, node_idx, prev_start, prev_count, cur_start, cur_count, int_idx
    integer(int16) :: aq_x1, aq_y1, aq_x2, aq_y2
    prev_start = 1_int64
    prev_count = this%n_leaves
    node_idx   = this%n_leaves + 1_int64
    do while (prev_count > 1_int64)
       cur_start = node_idx
       cur_count = (prev_count + K_CSI_LEAF_CAPACITY - 1_int64) / K_CSI_LEAF_CAPACITY
       do i = 1_int64, cur_count
          c_start = prev_start + (i - 1_int64) * K_CSI_LEAF_CAPACITY
          c_end   = min(prev_start + i * K_CSI_LEAF_CAPACITY - 1_int64, &
                        prev_start + prev_count - 1_int64)
          aq_x1 = this%qx1(c_start); aq_y1 = this%qy1(c_start)
          aq_x2 = this%qx2(c_start); aq_y2 = this%qy2(c_start)
          do j = c_start + 1_int64, c_end
             if (this%qx1(j) < aq_x1) aq_x1 = this%qx1(j)
             if (this%qy1(j) < aq_y1) aq_y1 = this%qy1(j)
             if (this%qx2(j) > aq_x2) aq_x2 = this%qx2(j)
             if (this%qy2(j) > aq_y2) aq_y2 = this%qy2(j)
          end do
          this%qx1(node_idx) = aq_x1; this%qy1(node_idx) = aq_y1
          this%qx2(node_idx) = aq_x2; this%qy2(node_idx) = aq_y2
          int_idx = node_idx - this%n_leaves
          this%int_child_start(int_idx) = int(c_start, int32)
          this%int_child_count(int_idx) = int(c_end - c_start + 1_int64, int16)
          node_idx = node_idx + 1_int64
       end do
       prev_start = cur_start
       prev_count = cur_count
    end do
    this%root_index = node_idx - 1_int64
  end subroutine build_internal_levels

  !> Build the index over a CompactLayer, taking ownership of it (cl is left empty).
  !> Never materialises the layer as a Box array: leaf MBRs come from decoding one
  !> 16K-box chunk at a time per thread; the geometry stays bit-packed (cold form).
  subroutine csi_build_from_compact(this, cl)
    class(CompressedSpatialIndex), intent(inout) :: this
    type(CompactLayer), intent(inout)            :: cl
    integer(int64) :: ic, i, j, l0, nout, leaf, c_start, c_end, n_internal
    integer(K_COORDINATE_KIND) :: lx1, ly1
    type(Box) :: agg
    type(Box), allocatable :: buf(:)

    call this%destroy()
    if (cl%n <= 0_int64) return
    if (allocated(cl%keys)) call CompactPack(cl)          !> keep only the cold form
    call CompactMove(cl, this%cl)
    this%compact_backed = .true.
    this%n_boxes = this%cl%n

    ! exact layer MBR from the per-block MBRs
    agg = this%cl%blk_mbr(1)
    do i = 2_int64, this%cl%nblk
       agg%x1 = min(agg%x1, this%cl%blk_mbr(i)%x1); agg%y1 = min(agg%y1, this%cl%blk_mbr(i)%y1)
       agg%x2 = max(agg%x2, this%cl%blk_mbr(i)%x2); agg%y2 = max(agg%y2, this%cl%blk_mbr(i)%y2)
    end do
    this%layer_mbr = agg
    lx1 = agg%x1; ly1 = agg%y1
    this%span_x = max(1_int64, int(agg%x2, int64) - int(lx1, int64))
    this%span_y = max(1_int64, int(agg%y2, int64) - int(ly1, int64))

    this%n_leaves = (this%n_boxes + K_CSI_LEAF_CAPACITY - 1_int64) / K_CSI_LEAF_CAPACITY
    this%n_nodes  = compute_total_nodes(this%n_boxes, K_CSI_LEAF_CAPACITY)
    n_internal    = max(0_int64, this%n_nodes - this%n_leaves)
    allocate(this%qx1(this%n_nodes), this%qy1(this%n_nodes), this%qx2(this%n_nodes), this%qy2(this%n_nodes))
    if (n_internal > 0_int64) allocate(this%int_child_start(n_internal), this%int_child_count(n_internal))
    this%n_chunks = (this%n_boxes + K_CSI_BOXES_PER_BLK - 1_int64) / K_CSI_BOXES_PER_BLK

    !$omp parallel default(shared) private(ic, i, j, l0, nout, leaf, c_start, c_end, agg, buf)
    allocate(buf(K_CSI_BOXES_PER_BLK))
    !$omp do schedule(dynamic, 4)
    do ic = 1_int64, this%n_chunks
       call CompactDecodeBlocks(this%cl, (ic - 1_int64) * K_CSI_CL_BLOCKS + 1_int64, &
            min(ic * K_CSI_CL_BLOCKS, this%cl%nblk), buf, nout)
       l0 = (ic - 1_int64) * K_CSI_LEAVES_PER_BLK
       do i = 1_int64, (nout + K_CSI_LEAF_CAPACITY - 1_int64) / K_CSI_LEAF_CAPACITY
          c_start = (i - 1_int64) * K_CSI_LEAF_CAPACITY + 1_int64
          c_end   = min(i * K_CSI_LEAF_CAPACITY, nout)
          agg = buf(c_start)
          do j = c_start + 1_int64, c_end
             if (buf(j)%x1 < agg%x1) agg%x1 = buf(j)%x1
             if (buf(j)%y1 < agg%y1) agg%y1 = buf(j)%y1
             if (buf(j)%x2 > agg%x2) agg%x2 = buf(j)%x2
             if (buf(j)%y2 > agg%y2) agg%y2 = buf(j)%y2
          end do
          leaf = l0 + i
          this%qx1(leaf) = quantize_lo(agg%x1, lx1, this%span_x)
          this%qy1(leaf) = quantize_lo(agg%y1, ly1, this%span_y)
          this%qx2(leaf) = quantize_hi(agg%x2, lx1, this%span_x)
          this%qy2(leaf) = quantize_hi(agg%y2, ly1, this%span_y)
       end do
    end do
    !$omp end do
    deallocate(buf)
    !$omp end parallel

    call build_internal_levels(this)
  end subroutine csi_build_from_compact

  !> Decode chunk ic of a compact-backed index into buf(1:nout).
  subroutine decode_chunk(csi, ic, buf, nout)
    class(CompressedSpatialIndex), intent(in) :: csi
    integer(int64), intent(in) :: ic
    type(Box), intent(inout) :: buf(:)
    integer(int64), intent(out) :: nout
    if (allocated(csi%chunk_part)) then
       if (csi%chunk_part(ic) == 1) then
          call CompactDecodeBlocks(csi%cl, csi%chunk_kb0(ic), csi%chunk_kb1(ic), buf, nout)
       else
          call CompactDecodeBlocks(csi%clx(csi%chunk_part(ic) - 1), csi%chunk_kb0(ic), csi%chunk_kb1(ic), buf, nout)
       end if
    else
       call CompactDecodeBlocks(csi%cl, (ic - 1_int64) * K_CSI_CL_BLOCKS + 1_int64, &
            min(ic * K_CSI_CL_BLOCKS, csi%cl%nblk), buf, nout)
    end if
  end subroutine decode_chunk

  !> Build the index over several compact parts of one layer (e.g. a layer compacted in pieces
  !> while it streamed), taking ownership of them. Same tree as build_from_compact; chunks
  !> never span parts, so a part's last chunk may be partial (its unused leaves are empty).
  subroutine csi_build_from_compact_parts(this, parts)
    class(CompressedSpatialIndex), intent(inout), target :: this
    type(CompactLayer), intent(inout)            :: parts(:)
    integer(int64) :: ic, i, j, l0, nout, leaf, c_start, c_end, n_internal, nch, kb
    integer :: p, np, q
    integer(K_COORDINATE_KIND) :: lx1, ly1
    type(Box) :: agg
    logical :: first
    type(CompactLayer), pointer :: c          !> gfortran: no associate on part_ref (defined later)
    type(Box), allocatable :: buf(:)

    call this%destroy()
    np = 0
    do p = 1, size(parts)
       if (parts(p)%n > 0_int64) np = np + 1
    end do
    if (np == 0) return
    ! move non-empty parts in: first -> cl, others -> clx
    if (np > 1) allocate(this%clx(np - 1))
    q = 0
    do p = 1, size(parts)
       if (parts(p)%n <= 0_int64) then
          call CompactRelease(parts(p)); cycle
       end if
       if (allocated(parts(p)%keys)) call CompactPack(parts(p))
       q = q + 1
       if (q == 1) then
          call CompactMove(parts(p), this%cl)
       else
          call CompactMove(parts(p), this%clx(q - 1))
       end if
    end do
    this%compact_backed = .true.

    ! chunk table and totals
    nch = 0_int64
    this%n_boxes = 0_int64
    first = .true.
    do p = 1, np
       c => part_ref(this, p)
         nch = nch + (c%nblk + K_CSI_CL_BLOCKS - 1_int64) / K_CSI_CL_BLOCKS
         this%n_boxes = this%n_boxes + c%n
         do i = 1_int64, c%nblk
            if (first) then
               agg = c%blk_mbr(i); first = .false.
            else
               agg%x1 = min(agg%x1, c%blk_mbr(i)%x1); agg%y1 = min(agg%y1, c%blk_mbr(i)%y1)
               agg%x2 = max(agg%x2, c%blk_mbr(i)%x2); agg%y2 = max(agg%y2, c%blk_mbr(i)%y2)
            end if
         end do
    end do
    allocate(this%chunk_part(nch), this%chunk_kb0(nch), this%chunk_kb1(nch))
    ic = 0_int64
    do p = 1, np
       c => part_ref(this, p)
         do kb = 1_int64, c%nblk, K_CSI_CL_BLOCKS
            ic = ic + 1_int64
            this%chunk_part(ic) = int(p, int32)
            this%chunk_kb0(ic) = kb
            this%chunk_kb1(ic) = min(kb + K_CSI_CL_BLOCKS - 1_int64, c%nblk)
         end do
    end do
    this%n_chunks = nch
    this%layer_mbr = agg
    lx1 = agg%x1; ly1 = agg%y1
    this%span_x = max(1_int64, int(agg%x2, int64) - int(lx1, int64))
    this%span_y = max(1_int64, int(agg%y2, int64) - int(ly1, int64))

    ! leaves: K_CSI_LEAVES_PER_BLK slots per chunk (the last chunk only as many as it needs)
    this%n_leaves = (nch - 1_int64) * K_CSI_LEAVES_PER_BLK + K_CSI_LEAVES_PER_BLK
    this%n_nodes  = compute_total_nodes(this%n_leaves * K_CSI_LEAF_CAPACITY, K_CSI_LEAF_CAPACITY)
    n_internal    = max(0_int64, this%n_nodes - this%n_leaves)
    allocate(this%qx1(this%n_nodes), this%qy1(this%n_nodes), this%qx2(this%n_nodes), this%qy2(this%n_nodes))
    if (n_internal > 0_int64) allocate(this%int_child_start(n_internal), this%int_child_count(n_internal))
    this%qx1(1:this%n_leaves) = huge(this%qx1(1)); this%qy1(1:this%n_leaves) = huge(this%qy1(1))
    this%qx2(1:this%n_leaves) = -huge(this%qx2(1)) - 1_int16; this%qy2(1:this%n_leaves) = -huge(this%qy2(1)) - 1_int16

    !$omp parallel default(shared) private(ic, i, j, l0, nout, leaf, c_start, c_end, agg, buf)
    allocate(buf(K_CSI_BOXES_PER_BLK))
    !$omp do schedule(dynamic, 4)
    do ic = 1_int64, nch
       call decode_chunk(this, ic, buf, nout)
       l0 = (ic - 1_int64) * K_CSI_LEAVES_PER_BLK
       do i = 1_int64, (nout + K_CSI_LEAF_CAPACITY - 1_int64) / K_CSI_LEAF_CAPACITY
          c_start = (i - 1_int64) * K_CSI_LEAF_CAPACITY + 1_int64
          c_end   = min(i * K_CSI_LEAF_CAPACITY, nout)
          agg = buf(c_start)
          do j = c_start + 1_int64, c_end
             if (buf(j)%x1 < agg%x1) agg%x1 = buf(j)%x1
             if (buf(j)%y1 < agg%y1) agg%y1 = buf(j)%y1
             if (buf(j)%x2 > agg%x2) agg%x2 = buf(j)%x2
             if (buf(j)%y2 > agg%y2) agg%y2 = buf(j)%y2
          end do
          leaf = l0 + i
          this%qx1(leaf) = quantize_lo(agg%x1, lx1, this%span_x)
          this%qy1(leaf) = quantize_lo(agg%y1, ly1, this%span_y)
          this%qx2(leaf) = quantize_hi(agg%x2, lx1, this%span_x)
          this%qy2(leaf) = quantize_hi(agg%y2, ly1, this%span_y)
       end do
    end do
    !$omp end do
    deallocate(buf)
    !$omp end parallel

    call build_internal_levels(this)
  end subroutine csi_build_from_compact_parts

  !> All boxes of a compact-backed index (every part), for tests / oracles.
  subroutine csi_decode_all(this, boxes, n)
    class(CompressedSpatialIndex), intent(in) :: this
    type(Box), allocatable, intent(out) :: boxes(:)
    integer(int64), intent(out) :: n
    integer(int64) :: ic, nout
    type(Box), allocatable :: buf(:)
    n = 0_int64
    allocate(boxes(max(1_int64, this%n_boxes)))
    if (.not. this%compact_backed) return
    allocate(buf(K_CSI_BOXES_PER_BLK))
    do ic = 1_int64, this%n_chunks
       call decode_chunk(this, ic, buf, nout)
       boxes(n + 1:n + nout) = buf(1:nout)
       n = n + nout
    end do
  end subroutine csi_decode_all

  function part_ref(this, p) result(c)
    class(CompressedSpatialIndex), intent(in), target :: this
    integer, intent(in) :: p
    type(CompactLayer), pointer :: c
    if (p == 1) then
       c => this%cl
    else
       c => this%clx(p - 1)
    end if
  end function part_ref

  !> MAGVIEWER_COMPACT (default on): use the compact Morton/shape-id store for large layers.
  logical function csi_compact_enabled()
    character(len=8) :: ev
    integer :: evlen, evstat
    call get_environment_variable('MAGVIEWER_COMPACT', ev, evlen, evstat)
    csi_compact_enabled = .not. (evstat == 0 .and. evlen > 0 .and. ev(1:1) == '0')
  end function csi_compact_enabled

  !> Build from boxes, through the compact store when the layer is large and encodes
  !> exactly, else through BoxCodec/zstd chunks. Exact duplicate boxes are dropped
  !> in the compact form (they draw identically).
  subroutine csi_build_auto(this, boxes, n_used)
    class(CompressedSpatialIndex), intent(inout) :: this
    type(Box), intent(inout)                     :: boxes(:)
    integer(int64), intent(in)                   :: n_used
    type(CompactLayer) :: cl
    integer :: status
    if (n_used >= 1_int64 .and. csi_compact_enabled()) then
       call CompactEncode(boxes(1:n_used), n_used, cl, status)
       if (status == 0) then
          call this%build_from_compact(cl)
          return
       end if
       call CompactRelease(cl)
    end if
    call this%build_from_boxes(boxes, n_used, already_sorted=.false.)
  end subroutine csi_build_auto

  recursive subroutine compress_chunks_worker(this, boxes, n_used)
    class(CompressedSpatialIndex), intent(inout) :: this
    type(Box), intent(in)                        :: boxes(:)
    integer(int64), intent(in)                   :: n_used
    type(BoxCodec) :: local_codec
    integer(int8), allocatable, target :: codec_buf(:)
    integer(int8), allocatable, target :: zstd_buf(:)
    integer(int64) :: i, c_start, c_end, codec_bytes, nb
    integer(c_size_t) :: bound, zsz
    call local_codec%init()
    allocate(codec_buf(K_CSI_BOXES_PER_BLK * 24_int64))
    allocate(zstd_buf(K_CSI_BOXES_PER_BLK * 24_int64))

    !$omp do schedule(dynamic, 1)
    do i = 1_int64, this%n_chunks
       c_start = (i - 1_int64) * K_CSI_BOXES_PER_BLK + 1_int64
       c_end   = min(i * K_CSI_BOXES_PER_BLK, n_used)
       nb      = c_end - c_start + 1_int64

       ! Reset codec state per chunk (no reallocation)
       local_codec%num_w = 0_int64
       local_codec%num_h = 0_int64
       local_codec%current_w_index = 0_int64
       local_codec%current_h_index = 0_int64
       local_codec%current_x = 0_K_COORDINATE_KIND
       local_codec%current_y = 0_K_COORDINATE_KIND
       local_codec%w_hash_valid = .true.
       local_codec%h_hash_valid = .true.
       if (allocated(local_codec%w_hash)) local_codec%w_hash = 0_int32
       if (allocated(local_codec%h_hash)) local_codec%h_hash = 0_int32

       call CompressBoxesUsingCodec(boxes(c_start:c_end), local_codec, codec_buf, codec_bytes)

       this%chunks(i)%num_boxes = int(nb, int32)
       this%chunks(i)%codec_len = int(codec_bytes, int32)

       bound = zstd_compress_bound(int(codec_bytes, c_size_t))
       if (int(bound, int64) > size(zstd_buf, kind=int64)) then
          deallocate(zstd_buf)
          allocate(zstd_buf(int(bound, int64)))
       end if

       zsz = zstd_compress(c_loc(zstd_buf(1)), bound, c_loc(codec_buf(1)), &
                           int(codec_bytes, c_size_t), K_CSI_ZSTD_LEVEL)

       if (zstd_is_error(zsz) == 0_c_int .and. int(zsz, int64) < codec_bytes) then
          this%chunks(i)%is_zstd = .true.
          allocate(this%chunks(i)%payload(int(zsz, int64)))
          this%chunks(i)%payload(1:int(zsz, int64)) = zstd_buf(1:int(zsz, int64))
       else
          this%chunks(i)%is_zstd = .false.
          allocate(this%chunks(i)%payload(codec_bytes))
          this%chunks(i)%payload(1:codec_bytes) = codec_buf(1:codec_bytes)
       end if
    end do
    !$omp end do

    if (allocated(codec_buf)) deallocate(codec_buf)
    if (allocated(zstd_buf))  deallocate(zstd_buf)
  end subroutine compress_chunks_worker

  subroutine csi_build_from_boxes(this, boxes, n_used, already_sorted)
    class(CompressedSpatialIndex), intent(inout) :: this
    type(Box), intent(inout)                     :: boxes(:)
    integer(int64), intent(in)                   :: n_used
    logical, intent(in), optional                :: already_sorted

    logical        :: do_sort
    integer(int64) :: i, j, c_start, c_end, node_idx
    integer(int64) :: prev_start, prev_count, cur_start, cur_count, n_internal, int_idx
    integer(K_COORDINATE_KIND) :: lx1, ly1
    type(Box)      :: agg
    integer(int16) :: aq_x1, aq_y1, aq_x2, aq_y2

    call this%destroy()
    if (n_used <= 0_int64) return

    do_sort = .true.
    if (present(already_sorted)) do_sort = .not. already_sorted

    ! 1. In-place OMT spatial packing (zero extra box-array copy, tight R-Tree MBRs)
    if (do_sort .and. n_used > 1_int64) then
       call omt_pack(boxes(1:n_used), K_CSI_LEAF_CAPACITY)
    end if

    ! 2. Compute layer MBR and quantization domain
    this%n_boxes   = n_used
    this%layer_mbr = mbr_of_array(boxes(1:n_used), n_used)
    lx1 = this%layer_mbr%x1
    ly1 = this%layer_mbr%y1
    this%span_x = max(1_int64, int(this%layer_mbr%x2, int64) - int(lx1, int64))
    this%span_y = max(1_int64, int(this%layer_mbr%y2, int64) - int(ly1, int64))

    ! 3. Allocate compact quantized SoA R-Tree
    this%n_leaves = (n_used + K_CSI_LEAF_CAPACITY - 1_int64) / K_CSI_LEAF_CAPACITY
    this%n_nodes  = compute_total_nodes(n_used, K_CSI_LEAF_CAPACITY)
    n_internal    = max(0_int64, this%n_nodes - this%n_leaves)

    allocate(this%qx1(this%n_nodes), this%qy1(this%n_nodes), &
             this%qx2(this%n_nodes), this%qy2(this%n_nodes))
    if (n_internal > 0_int64) then
       allocate(this%int_child_start(n_internal), this%int_child_count(n_internal))
    end if

    ! 4. Build leaf level (indices 1 .. n_leaves) in parallel
    !$omp parallel do default(none) &
    !$omp shared(this, boxes, n_used, lx1, ly1) &
    !$omp private(i, j, c_start, c_end, agg) schedule(static)
    do i = 1_int64, this%n_leaves
       c_start = (i - 1_int64) * K_CSI_LEAF_CAPACITY + 1_int64
       c_end   = min(i * K_CSI_LEAF_CAPACITY, n_used)
       agg = boxes(c_start)
       do j = c_start + 1_int64, c_end
          if (boxes(j)%x1 < agg%x1) agg%x1 = boxes(j)%x1
          if (boxes(j)%y1 < agg%y1) agg%y1 = boxes(j)%y1
          if (boxes(j)%x2 > agg%x2) agg%x2 = boxes(j)%x2
          if (boxes(j)%y2 > agg%y2) agg%y2 = boxes(j)%y2
       end do
       this%qx1(i) = quantize_lo(agg%x1, lx1, this%span_x)
       this%qy1(i) = quantize_lo(agg%y1, ly1, this%span_y)
       this%qx2(i) = quantize_hi(agg%x2, lx1, this%span_x)
       this%qy2(i) = quantize_hi(agg%y2, ly1, this%span_y)
    end do
    !$omp end parallel do

    ! 5. Build internal levels (bottom-up using already-quantized child MBRs)
    call build_internal_levels(this)

    ! 6. Compress leaf geometry into independent BoxCodec + Zstd chunks in parallel
    this%n_chunks = (n_used + K_CSI_BOXES_PER_BLK - 1_int64) / K_CSI_BOXES_PER_BLK
    allocate(this%chunks(this%n_chunks))

    ! Each thread's codec and scratch buffers are locals of the worker (an orphaned
    ! omp do), not PRIVATE copies of routine-scope allocatable derived types.
    !$omp parallel default(shared)
    call compress_chunks_worker(this, boxes, n_used)
    !$omp end parallel
  end subroutine csi_build_from_boxes

  ! ------------------------------------------------------------------
  ! Query LOD MBRs directly from quantized R-Tree (ZERO decompression!)
  ! ------------------------------------------------------------------
  subroutine csi_query_mbrs_to_quads(this, current_view, target_depth, min_span, &
                                     vertex_buffer, num_visible, max_capacity)
    class(CompressedSpatialIndex), intent(in)   :: this
    type(Box), intent(in)                       :: current_view
    integer, intent(in)                         :: target_depth
    integer(K_COORDINATE_KIND), intent(in)      :: min_span
    integer(c_int), allocatable, intent(inout)  :: vertex_buffer(:)
    integer, intent(out)                        :: num_visible
    integer, intent(in)                         :: max_capacity

    integer(int64) :: node_stack(K_CSI_DFS_STACK_MAX)
    integer        :: depth_stack(K_CSI_DFS_STACK_MAX)
    integer        :: sp, cur_d, max_d, vidx
    integer(int64) :: curr_idx, child_idx, c_start, c_end, int_idx
    integer(int16) :: vqx1, vqy1, vqx2, vqy2
    integer(int64) :: q_min_x, q_min_y
    integer(K_COORDINATE_KIND) :: x1, y1, x2, y2

    num_visible = 0
    if (this%n_boxes <= 0_int64 .or. this%root_index <= 0_int64) return

    ! Quick reject against exact layer MBR
    if (this%layer_mbr%x2 < current_view%x1 .or. this%layer_mbr%x1 > current_view%x2 .or. &
        this%layer_mbr%y2 < current_view%y1 .or. this%layer_mbr%y1 > current_view%y2) return

    vqx1 = quantize_lo(current_view%x1, this%layer_mbr%x1, this%span_x)
    vqy1 = quantize_lo(current_view%y1, this%layer_mbr%y1, this%span_y)
    vqx2 = quantize_hi(current_view%x2, this%layer_mbr%x1, this%span_x)
    vqy2 = quantize_hi(current_view%y2, this%layer_mbr%y1, this%span_y)

    if (min_span > 0_K_COORDINATE_KIND) then
       q_min_x = (int(min_span, int64) * 65535_int64) / this%span_x
       q_min_y = (int(min_span, int64) * 65535_int64) / this%span_y
    else
       q_min_x = 0_int64
       q_min_y = 0_int64
    end if

    if (.not. allocated(vertex_buffer)) allocate(vertex_buffer(4096 * 8))
    max_d = max(0, target_depth)

    sp = 1
    node_stack(1)  = this%root_index
    depth_stack(1) = 0
    vidx = 1

    do while (sp > 0 .and. num_visible < max_capacity)
       curr_idx = node_stack(sp)
       cur_d    = depth_stack(sp)
       sp = sp - 1

       if (cur_d >= max_d .or. curr_idx <= this%n_leaves .or. &
           (q_min_x > 0_int64 .and. &
            (int(this%qx2(curr_idx), int64) - int(this%qx1(curr_idx), int64)) <= q_min_x .and. &
            (int(this%qy2(curr_idx), int64) - int(this%qy1(curr_idx), int64)) <= q_min_y)) then
          if (this%qx1(curr_idx) > this%qx2(curr_idx)) cycle      ! empty leaf slot
          x1 = dequantize_lo(this%qx1(curr_idx), this%layer_mbr%x1, this%span_x)
          y1 = dequantize_lo(this%qy1(curr_idx), this%layer_mbr%y1, this%span_y)
          x2 = dequantize_hi(this%qx2(curr_idx), this%layer_mbr%x1, this%span_x)
          y2 = dequantize_hi(this%qy2(curr_idx), this%layer_mbr%y1, this%span_y)
          if (vidx + 7 > size(vertex_buffer)) call ensure_quad_capacity(vertex_buffer, vidx + 7)
          vertex_buffer(vidx)   = x1; vertex_buffer(vidx+1) = y1
          vertex_buffer(vidx+2) = x2; vertex_buffer(vidx+3) = y1
          vertex_buffer(vidx+4) = x2; vertex_buffer(vidx+5) = y2
          vertex_buffer(vidx+6) = x1; vertex_buffer(vidx+7) = y2
          vidx = vidx + 8
          num_visible = num_visible + 1
       else
          int_idx = curr_idx - this%n_leaves
          c_start = int(this%int_child_start(int_idx), int64)
          c_end   = c_start + int(this%int_child_count(int_idx), int64) - 1_int64
          do child_idx = c_start, c_end
             if (this%qx2(child_idx) < vqx1 .or. this%qx1(child_idx) > vqx2 .or. &
                 this%qy2(child_idx) < vqy1 .or. this%qy1(child_idx) > vqy2) cycle
             if (sp < K_CSI_DFS_STACK_MAX) then
                sp = sp + 1
                node_stack(sp)  = child_idx
                depth_stack(sp) = cur_d + 1
             end if
          end do
       end if
    end do
  end subroutine csi_query_mbrs_to_quads

  ! ------------------------------------------------------------------
  ! Query exact boxes using quantized R-Tree + on-demand LRU chunk cache
  ! ------------------------------------------------------------------
  subroutine csi_query_boxes_to_quads(this, current_view, min_span, cache, &
                                      vertex_buffer, num_visible, max_capacity)
    class(CompressedSpatialIndex), intent(in)   :: this
    type(Box), intent(in)                       :: current_view
    integer(K_COORDINATE_KIND), intent(in)      :: min_span
    type(ChunkLRUCache), intent(inout)          :: cache
    integer(c_int), allocatable, intent(inout)  :: vertex_buffer(:)
    integer, intent(out)                        :: num_visible
    integer, intent(in)                         :: max_capacity

    integer(int64) :: stack(K_CSI_DFS_STACK_MAX)
    integer        :: sp, vidx, slot_idx
    integer(int64) :: curr_idx, child_idx, c_start, c_end, int_idx
    integer(int64) :: chunk_id, local_start, local_end, k, last_chunk_id
    integer(int16) :: vqx1, vqy1, vqx2, vqy2
    integer(int64) :: q_min_x, q_min_y
    type(Box)      :: b

    num_visible = 0
    if (this%n_boxes <= 0_int64 .or. this%root_index <= 0_int64) return

    ! Quick reject against exact layer MBR
    if (this%layer_mbr%x2 < current_view%x1 .or. this%layer_mbr%x1 > current_view%x2 .or. &
        this%layer_mbr%y2 < current_view%y1 .or. this%layer_mbr%y1 > current_view%y2) return

    vqx1 = quantize_lo(current_view%x1, this%layer_mbr%x1, this%span_x)
    vqy1 = quantize_lo(current_view%y1, this%layer_mbr%y1, this%span_y)
    vqx2 = quantize_hi(current_view%x2, this%layer_mbr%x1, this%span_x)
    vqy2 = quantize_hi(current_view%y2, this%layer_mbr%y1, this%span_y)

    if (min_span > 0_K_COORDINATE_KIND) then
       q_min_x = (int(min_span, int64) * 65535_int64) / this%span_x
       q_min_y = (int(min_span, int64) * 65535_int64) / this%span_y
    else
       q_min_x = 0_int64
       q_min_y = 0_int64
    end if

    if (.not. allocated(vertex_buffer)) allocate(vertex_buffer(min(int(this%n_boxes), 4096) * 8))

    sp = 1
    stack(1) = this%root_index
    vidx = 1
    last_chunk_id = -1_int64
    slot_idx = 1

    do while (sp > 0 .and. num_visible < max_capacity)
       curr_idx = stack(sp)
       sp = sp - 1

       if (curr_idx <= this%n_leaves) then
          ! Leaf node: locate compressed chunk & slice
          chunk_id = (curr_idx - 1_int64) / K_CSI_LEAVES_PER_BLK + 1_int64
          if (chunk_id /= last_chunk_id) then
             call cache_fetch_slot(this, cache, chunk_id, slot_idx)
             last_chunk_id = chunk_id
          end if
          local_start = mod(curr_idx - 1_int64, K_CSI_LEAVES_PER_BLK) * K_CSI_LEAF_CAPACITY + 1_int64
          local_end   = min(local_start + K_CSI_LEAF_CAPACITY - 1_int64, &
                            int(cache%slots(slot_idx)%num_boxes, int64))
          do k = local_start, local_end
             if (num_visible >= max_capacity) exit
             b = cache%slots(slot_idx)%boxes(k)
             if (b%x2 < current_view%x1 .or. b%x1 > current_view%x2 .or. &
                 b%y2 < current_view%y1 .or. b%y1 > current_view%y2) cycle
             if (min_span > 0_K_COORDINATE_KIND .and. &
                 (b%x2 - b%x1) < min_span .and. (b%y2 - b%y1) < min_span) cycle
             if (vidx + 7 > size(vertex_buffer)) call ensure_quad_capacity(vertex_buffer, vidx + 7)
             vertex_buffer(vidx)   = b%x1; vertex_buffer(vidx+1) = b%y1
             vertex_buffer(vidx+2) = b%x2; vertex_buffer(vidx+3) = b%y1
             vertex_buffer(vidx+4) = b%x2; vertex_buffer(vidx+5) = b%y2
             vertex_buffer(vidx+6) = b%x1; vertex_buffer(vidx+7) = b%y2
             vidx = vidx + 8
             num_visible = num_visible + 1
          end do
       else
          int_idx = curr_idx - this%n_leaves
          c_start = int(this%int_child_start(int_idx), int64)
          c_end   = c_start + int(this%int_child_count(int_idx), int64) - 1_int64
          do child_idx = c_start, c_end
             if (this%qx2(child_idx) < vqx1 .or. this%qx1(child_idx) > vqx2 .or. &
                 this%qy2(child_idx) < vqy1 .or. this%qy1(child_idx) > vqy2) cycle
             ! Sub-pixel subtree culling: if whole subtree MBR is smaller than min_span, skip it
             if (q_min_x > 1_int64 .and. q_min_y > 1_int64) then
                if ((int(this%qx2(child_idx), int64) - int(this%qx1(child_idx), int64)) < (q_min_x - 1_int64) .and. &
                    (int(this%qy2(child_idx), int64) - int(this%qy1(child_idx), int64)) < (q_min_y - 1_int64)) cycle
             end if
             if (sp < K_CSI_DFS_STACK_MAX) then
                sp = sp + 1
                stack(sp) = child_idx
             end if
          end do
       end if
    end do
  end subroutine csi_query_boxes_to_quads

end module CompressedSpatialIndexModule
