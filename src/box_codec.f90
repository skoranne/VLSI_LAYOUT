! File   : box_codec.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: See notes below
! Given billions of type(Box) integer(kind=K_COORDINATE_KIND)
! x1,y2,x2,y2 end type in Modern Fortran we have written order
! statistics which finds unique WIDTH and HEIGHT in this array
! of boxes.

!         w1_count = count_distinct(unique_w)

!         h1_count = count_distinct(unique_h)

! ! =================================================

! !  BOX ORDER STATISTICS

! !  =================================================

! ! Total Boxes (N): 424548608

! ! Unique W/H Pairs (K): 2454

! ! Distinct Widths (W1): 541

! ! Distinct Heights (H1): 251

! !  -------------------------------------------------

! ! Min Width: 170

! ! Max Width: 10120

! ! Median Width (approx): 390

! Therefore we can devise a scheme for compression using the
! fact that the total number of UNIQUE WIDTH and HEIGHT is
! very low compared to the number of boxes. We have already
! integrated SNAPPY compression on the byte stream, but we
! want to integrate a simple ENCODER/DECODER step which uses a
! CODEC type which is something like this

! type CODEC

! integer(kind=K_COORDINATE_KIND) :: UNIQUE_W(:), UNIQUE_H(:)

! integer(kind=int64) :: current_W_index, current_H_index

! end

! Devise a plan and generate a compression subroutine
! CompressBoxesUsingCodec( boxes, codec, output_stream) which
! scans the boxes and writes a sequence of BYTES into
! output_stream which is int(kind=int_8). The logic can be
! something like

! int(kind=int8) :: INFOBYTE = 0x00 in modern fortran
! represents the usage of the CODEC internal state, 0 =
! current W index, 1 implies next byte sequence contains the
! new index, and codec index is updated. Same happens during
! read where we do NOT need to fill in the UNIQUE_W and
! UNIQUE_H. For writing also we dont need to fill these in
! apriori.

! WriteBox(b) : if IndexOf( width(b) ) == current_W_index, let
! the W slot
! Let us write another bit in the INFOBYTE when W==H (square)
! bit and in that we dont have to use and modify both W and H,
! we can update W only. Secondly lets use DELTA encoding in
! the anchors X and Y as its possible that X and Y are also
! same as before. Since INFOBYTE as 5 bits remaining, use
! these bits to incorporate these two optimizations.
! Generate idiomatic modern Fortran for this task.
module CompressionChunkManagerModule
   use CommonModule
   use GeometryModule
   use SnappyCompressionModule
   use iso_c_binding, only: c_char
   use iso_fortran_env, only : int32, int64, real64
   !use snappy_mod, only: Box, compress_box_array, decompress_box_array
   implicit none

   ! ---------------------------------------------------------
   ! Hardware/R-Tree Tuning Constants
   ! ---------------------------------------------------------
   integer(kind=int64), parameter :: LEAVES_PER_CHUNK = 4096_int64
   integer(kind=int64), parameter :: BOXES_PER_CHUNK  = K_LEAF_CAPACITY * LEAVES_PER_CHUNK

   ! ---------------------------------------------------------
   ! 1. The Individual Chunk Data Structure
   ! ---------------------------------------------------------
   type :: Chunk
      integer(kind=int64) :: num_boxes          ! Exact number of boxes in this chunk
      integer(kind=int64) :: compressed_size    ! Size of the byte array
      character(kind=c_char), allocatable :: data(:) ! The Snappy compressed bytes
   end type Chunk

   ! ---------------------------------------------------------
   ! 2. The Master 'CompressedChunks' Structure
   ! ---------------------------------------------------------
   type :: CompressedChunks
      integer(kind=int64) :: total_boxes
      integer(kind=int64) :: num_chunks
      type(Chunk), allocatable :: chunks(:)
   end type CompressedChunks

   ! ---------------------------------------------------------
   ! Interfaces to match your required API
   ! ---------------------------------------------------------
   interface compress
      module procedure compress_to_chunks
   end interface compress

   interface decompress
      module procedure decompress_from_chunks
   end interface decompress

contains

   ! ---------------------------------------------------------
   ! COMPRESS: Takes a massive Box array and populates 'cchunk'
   ! ---------------------------------------------------------
   subroutine compress_to_chunks(boxes, cchunk)
      ! Note: contiguous allows us to safely slice and pass to Snappy
      type(Box), intent(in), target, contiguous :: boxes(:)
      type(CompressedChunks), intent(inout) :: cchunk
      integer(kind=int64) :: current_start, current_end, i
      cchunk%total_boxes = size(boxes, kind=8)
      ! Integer math trick to calculate the exact number of chunks needed
      ! (Equivalent to ceiling(total_boxes / BOXES_PER_CHUNK))
      cchunk%num_chunks = (cchunk%total_boxes + BOXES_PER_CHUNK - 1_int64) / BOXES_PER_CHUNK
      allocate(cchunk%chunks(cchunk%num_chunks))
      current_start = 1_int64
      !$omp parallel do default(none) &
      !$omp shared(cchunk, boxes) &
      !$omp private(i, current_start, current_end)
      do i = 1_int64, cchunk%num_chunks
         ! Ensure the last chunk doesn't overrun the array bounds
         current_start = (i - 1_int64) * BOXES_PER_CHUNK + 1_int64
         current_end = min(current_start + BOXES_PER_CHUNK - 1_int64, cchunk%total_boxes)
         cchunk%chunks(i)%num_boxes = current_end - current_start + 1_int64
         ! Pass just the slice into your snappy_mod
         cchunk%chunks(i)%data = compress_box_array(boxes(current_start:current_end))
         cchunk%chunks(i)%compressed_size = size(cchunk%chunks(i)%data, kind=int64)
      end do
      !$omp end parallel do
   end subroutine compress_to_chunks

   ! ---------------------------------------------------------
   ! DECOMPRESS: Takes 'cchunk' and restores the allocatable Box array
   ! ---------------------------------------------------------
   subroutine decompress_from_chunks(cchunk, boxes)
      type(CompressedChunks), intent(in) :: cchunk
      type(Box), allocatable, intent(out) :: boxes(:)

      integer(kind=int64) :: current_start, current_end, i
      type(Box), allocatable :: temp_boxes(:)

      ! 1. Allocate the master Fortran array to exactly the right size
      allocate(boxes(cchunk%total_boxes))

      ! 2. Open parallel region.
      ! 'temp_boxes' is made private so each thread gets its own unallocated array.
      !$omp parallel default(none) &
      !$omp shared(cchunk, boxes) &
      !$omp private(i, current_start, current_end, temp_boxes)

      ! Allocate the thread-local temporary buffer ONCE per thread
      ! (Assuming BOXES_PER_CHUNK is a module parameter)
      allocate(temp_boxes(BOXES_PER_CHUNK))

      ! 3. Distribute the loop iterations among threads
      !$omp do
      do i = 1_int64, cchunk%num_chunks
         current_start = ((i - 1_int64) * BOXES_PER_CHUNK) + 1_int64
         current_end = current_start + cchunk%chunks(i)%num_boxes - 1_int64

         if ((current_end - current_start + 1_int64) /= cchunk%chunks(i)%num_boxes) then
            print *, "CRITICAL MISMATCH at chunk ", i
         end if

         ! Decompress just this specific chunk into the thread's private array
         call decompress_box_array(cchunk%chunks(i)%data, temp_boxes)

         ! Copy the uncompressed data directly into its place in the master array
         boxes(current_start:current_end) = temp_boxes(1:cchunk%chunks(i)%num_boxes)
      end do
      !$omp end do

      ! Clean up the thread-local buffer before the thread exits
      deallocate(temp_boxes)

      !$omp end parallel
   end subroutine decompress_from_chunks
end module CompressionChunkManagerModule

module BoxCodecModule
  use CommonModule
  use GeometryModule
  use SnappyCompressionModule
  use iso_fortran_env, only: int8, int16, int32, int64
  implicit none
  private
  public ::  BoxCodec, CompressBoxesUsingCodec, DecompressBoxesUsingCodec
  public :: CompressedChunk, CompressedStream
  public :: CompressBoxesToStream, DecompressStreamToBoxes
  public :: COMPRESSION_METHOD_NONE, COMPRESSION_METHOD_SNAPPY, COMPRESSION_METHOD_ZLIB, COMPRESSION_METHOD_ZSTD
  type :: BoxCodec
     integer(K_COORDINATE_KIND), allocatable :: unique_w(:)
     integer(K_COORDINATE_KIND), allocatable :: unique_h(:)
     integer(int64) :: num_w = 0
     integer(int64) :: num_h = 0
     integer(int64) :: current_w_index = 0
     integer(int64) :: current_h_index = 0
     integer(K_COORDINATE_KIND) :: current_x = 0
     integer(K_COORDINATE_KIND) :: current_y = 0
   contains
     procedure :: init => codec_init
     procedure :: get_w_index => codec_get_w_index
     procedure :: get_h_index => codec_get_h_index
     procedure :: add_w => codec_add_w
     procedure :: add_h => codec_add_h
  end type BoxCodec

  ! Adjust this based on your L2/L3 cache sizes.
  ! 65536 boxes * 32 bytes = ~2MB uncompressed per chunk.
  integer(int64), parameter :: BOXES_PER_CHUNK = 65536_int64
  enum, bind(C)
     enumerator :: COMPRESSION_METHOD_NONE   = 0 !> this is simple BIN
     enumerator :: COMPRESSION_METHOD_SNAPPY = 1
     enumerator :: COMPRESSION_METHOD_ZLIB   = 2
     enumerator :: COMPRESSION_METHOD_ZSTD   = 3
  end enum
  type :: CompressedChunk
     integer(int64) :: num_boxes
     integer(int64) :: raw_byte_size
     integer(int64) :: compressed_size
     integer(int8), allocatable :: data(:)
  end type CompressedChunk

  type :: CompressedStream
     integer(int64) :: total_boxes
     integer(int64) :: num_chunks
     type(CompressedChunk), allocatable :: chunks(:)
     integer :: compression_method
  end type CompressedStream

contains

  ! ==================================================================
  ! Codec Type-Bound Procedures
  ! ==================================================================
  subroutine codec_init(this, capacity)
    class(BoxCodec), intent(inout) :: this
    integer, intent(in), optional :: capacity
    integer :: cap
    integer, parameter :: K_WH_CAPACITY = 1024 !> we expand, so this only impacts memory re-alloc
    cap = K_WH_CAPACITY
    if (present(capacity)) cap = capacity

    ! Only allocate the memory if it hasn't been allocated yet.
    ! This makes calling init() on subsequent chunks extremely fast.
    if (.not. allocated(this%unique_w)) allocate(this%unique_w(cap))
    if (.not. allocated(this%unique_h)) allocate(this%unique_h(cap))

    ! Reset the state variables so the new chunk starts fresh
    this%unique_w = 0
    this%unique_h = 0    
    this%num_w = 0
    this%num_h = 0
    this%current_w_index = 0
    this%current_h_index = 0

    ! Don't forget to reset the anchor states we added for the delta optimization!
    this%current_x = 0
    this%current_y = 0
  end subroutine codec_init

  ! Because quantization makes unique W/H sparse, a linear search over a
  ! tiny array is often faster than hashing due to L1 cache proximity.
  function codec_get_w_index(this, w) result(idx)
    class(BoxCodec), intent(in) :: this
    integer(K_COORDINATE_KIND), intent(in) :: w
    integer(int64) :: idx, i
    idx = -1_int64
    do i = 1, this%num_w
       if (this%unique_w(i) == w) then
          idx = i
          return
       end if
    end do
  end function codec_get_w_index

  function codec_get_h_index(this, h) result(idx)
    class(BoxCodec), intent(in) :: this
    integer(K_COORDINATE_KIND), intent(in) :: h
    integer(int64) :: idx, i
    idx = -1_int64
    do i = 1, this%num_h
       if (this%unique_h(i) == h) then
          idx = i
          return
       end if
    end do
  end function codec_get_h_index

  subroutine codec_add_w(this, w, idx)
    class(BoxCodec), intent(inout) :: this
    integer(K_COORDINATE_KIND), intent(in) :: w
    integer(int64), intent(out) :: idx
    integer(K_COORDINATE_KIND), allocatable :: temp(:)

    if (this%num_w >= size(this%unique_w, kind=int64)) then
       ! Reallocate double capacity
       allocate(temp(size(this%unique_w, kind=int64) * 2))
       temp(1:this%num_w) = this%unique_w(1:this%num_w)
       call move_alloc(temp, this%unique_w)
    end if
    this%num_w = this%num_w + 1_int64
    this%unique_w(this%num_w) = w
    idx = this%num_w
  end subroutine codec_add_w

  subroutine codec_add_h(this, h, idx)
    class(BoxCodec), intent(inout) :: this
    integer(K_COORDINATE_KIND), intent(in) :: h
    integer(int64), intent(out) :: idx
    integer(K_COORDINATE_KIND), allocatable :: temp(:)

    if (this%num_h >= size(this%unique_h, kind=int64)) then
       allocate(temp(size(this%unique_h, kind=int64) * 2))
       temp(1:this%num_h) = this%unique_h(1:this%num_h)
       call move_alloc(temp, this%unique_h)
    end if
    this%num_h = this%num_h + 1_int64
    this%unique_h(this%num_h) = h
    idx = this%num_h
  end subroutine codec_add_h

  pure function zigzag_encode(val) result(res)
    integer(K_COORDINATE_KIND), intent(in) :: val
    integer(int64) :: res, temp

    temp = int(val, int64)
    ! If temp is negative, it maps to (temp * -2) - 1
    ! If temp is positive, it maps to (temp * 2)
    if (temp < 0_int64) then
       res = (abs(temp) * 2_int64) - 1_int64
    else
       res = temp * 2_int64
    end if
    ! shiftl: Logical left shift
    ! shifta: Arithmetic right shift (copies the sign bit across all 64 bits)
    !res = ieor(shiftl(temp, 1), shifta(temp, 63))
  end function zigzag_encode

  pure function zigzag_decode(val) result(res)
    integer(int64), intent(in) :: val
    integer(K_COORDINATE_KIND) :: res

    ! shiftr: Logical right shift (fills upper bits with 0)
    res = int(ieor(shiftr(val, 1), -iand(val, 1_int64)), K_COORDINATE_KIND)
  end function zigzag_decode

  subroutine write_varint(stream, pos, val)
    integer(int8), intent(inout) :: stream(:)
    integer(int64), intent(inout) :: pos
    integer(int64), intent(in) :: val
    integer(int64) :: temp
    integer(int8) :: byte_val

    temp = val
    do
       byte_val = int(iand(temp, 127_int64), int8)
       temp = shiftr(temp, 7) ! FORCE logical right shift to ensure termination

       if (temp == 0) then
          stream(pos) = byte_val
          pos = pos + 1_int64
          exit
       else
          stream(pos) = ibset(byte_val, 7) 
          pos = pos + 1_int64
       end if
    end do
  end subroutine write_varint

  subroutine read_varint(stream, pos, val)
    integer(int8), intent(in) :: stream(:)
    integer(int64), intent(inout) :: pos
    integer(int64), intent(out) :: val
    integer(int64) :: shift_amount
    integer(int8) :: byte_val

    val = 0_int64
    shift_amount = 0_int64
    do
       byte_val = stream(pos)
       pos = pos + 1
       val = ior(val, ishft(iand(int(byte_val, int64), 127_int64), shift_amount))
       if (.not. btest(byte_val, 7)) exit
       shift_amount = shift_amount + 7
    end do
  end subroutine read_varint

  subroutine append_int8(stream, pos, val)
    integer(int8), intent(inout) :: stream(:)
    integer(int64), intent(inout) :: pos
    integer(int8), intent(in) :: val
    stream(pos) = val
    pos = pos + 1
  end subroutine append_int8

  subroutine append_int16(stream, pos, val)
    integer(int8), intent(inout) :: stream(:)
    integer(int64), intent(inout) :: pos
    integer(int16), intent(in) :: val
    integer(int8) :: bytes(2)
    bytes = transfer(val, bytes)
    stream(pos : pos+1) = bytes
    pos = pos + 2
  end subroutine append_int16

  subroutine read_int8(stream, pos, val)
    integer(int8), intent(in) :: stream(:)
    integer(int64), intent(inout) :: pos
    integer(int8), intent(out) :: val
    val = stream(pos)
    pos = pos + 1
  end subroutine read_int8

  subroutine read_int16(stream, pos, val)
    integer(int8), intent(in) :: stream(:)
    integer(int64), intent(inout) :: pos
    integer(int16), intent(out) :: val
    integer(int8) :: bytes(2)
    bytes = stream(pos : pos+1)
    val = transfer(bytes, val)
    pos = pos + 2
  end subroutine read_int16

  ! ==================================================================
  ! Main Decompression Engine
  ! ==================================================================
  subroutine DecompressBoxesUsingCodec(in_stream, stream_len, num_boxes, boxes, local_codec)
    integer(int8), intent(in) :: in_stream(:)
    integer(int64), intent(in) :: stream_len, num_boxes
    type(Box), intent(inout) :: boxes(:)
    type(BoxCodec), intent(inout) :: local_codec

    integer(int64) :: i, pos, dummy_idx, val64
    integer(int8) :: infobyte
    integer(K_COORDINATE_KIND) :: dx, dy, w, h
    integer(int16) :: wire_idx
    logical :: box_is_square

    ! FORCE RESET STATE FOR THIS CHUNK (No reallocation)
    local_codec%num_w = 0
    local_codec%num_h = 0
    local_codec%current_w_index = 0
    local_codec%current_h_index = 0
    local_codec%current_x = 0
    local_codec%current_y = 0
    pos = 1_int64

    do i = 1_int64, num_boxes
       ! Read INFOBYTE
       call read_int8(in_stream, pos, infobyte)
       box_is_square = btest(infobyte, 4)

       ! 1. Reconstruct Anchors (ZigZag Decode)
       if (btest(infobyte, 5)) then
          call read_varint(in_stream, pos, val64)
          dx = zigzag_decode(val64)
          local_codec%current_x = local_codec%current_x + dx
       end if

       if (btest(infobyte, 6)) then
          call read_varint(in_stream, pos, val64)
          dy = zigzag_decode(val64)
          local_codec%current_y = local_codec%current_y + dy
       end if

       ! 2. Reconstruct Width
       if (btest(infobyte, 0)) then
          if (btest(infobyte, 1)) then
             call read_varint(in_stream, pos, val64)
             w = int(val64, K_COORDINATE_KIND)
             call local_codec%add_w(w, dummy_idx)
             local_codec%current_w_index = dummy_idx
          else
             call read_int16(in_stream, pos, wire_idx)
             if (wire_idx < 1 .or. wire_idx > local_codec%num_w) then
                print *, "CRITICAL: Stream Desync at W! Invalid wire_idx: ", wire_idx
                stop 1
             end if
             w = local_codec%unique_w(wire_idx)
             local_codec%current_w_index = int(wire_idx, kind=int64)
          end if
       else
          w = local_codec%unique_w(local_codec%current_w_index)
       end if

       ! 3. Reconstruct Height
       if (box_is_square) then
          h = w 
       else
          if (btest(infobyte, 2)) then
             if (btest(infobyte, 3)) then
                call read_varint(in_stream, pos, val64)
                h = int(val64, K_COORDINATE_KIND)
                call local_codec%add_h(h, dummy_idx)
                local_codec%current_h_index = dummy_idx
             else
                call read_int16(in_stream, pos, wire_idx)
                if (wire_idx < 1 .or. wire_idx > local_codec%num_h) then
                   print *, "CRITICAL: Stream Desync at H! Invalid wire_idx: ", wire_idx
                   stop 1
                end if
                h = local_codec%unique_h(wire_idx)
                local_codec%current_h_index = int(wire_idx, kind=int64)
             end if
          else
             h = local_codec%unique_h(local_codec%current_h_index)
          end if
       end if

       ! 4. Finalize Box
       boxes(i)%x1 = local_codec%current_x
       boxes(i)%y1 = local_codec%current_y
       boxes(i)%x2 = local_codec%current_x + w
       boxes(i)%y2 = local_codec%current_y + h
    end do
  end subroutine DecompressBoxesUsingCodec
  ! =========================================================
  ! DYNAMIC STREAM BUFFER MANAGEMENT
  ! =========================================================
  subroutine ensure_capacity(stream, pos, needed, capacity)
    integer(int8), allocatable, intent(inout) :: stream(:)
    integer(int64), intent(in) :: pos, needed
    integer(int64), intent(inout) :: capacity
    integer(int8), allocatable :: temp(:)

    ! If the current position plus what we are about to write exceeds the buffer...
    if (pos + needed - 1_int64 > capacity) then
       ! Double the capacity to amortize allocation costs
       capacity = capacity * 2_int64 + needed
       allocate(temp(capacity))

       ! Copy existing data to the new temporary buffer
       if (pos > 1_int64) then
          temp(1 : pos-1_int64) = stream(1 : pos-1_int64)
       end if

       ! move_alloc safely transfers the allocation from 'temp' to 'stream'
       ! and automatically deallocates the old 'stream' memory.
       call move_alloc(from=temp, to=stream)
    end if
  end subroutine ensure_capacity
  ! ==================================================================
  ! Main Compression Engine
  ! ==================================================================
  subroutine CompressBoxesUsingCodec(boxes, codec_state, out_stream, bytes_written)
    type(Box), intent(in) :: boxes(:)
    type(BoxCodec), intent(inout) :: codec_state
    integer(int8), allocatable, intent(inout) :: out_stream(:)
    integer(int64), intent(out) :: bytes_written

    integer(int64) :: i, stream_cap
    integer(int8) :: infobyte
    integer(K_COORDINATE_KIND) :: w, h, dx, dy
    integer(int64) :: w_idx, h_idx
    integer(int16) :: wire_idx
    logical :: box_is_square

    if (.not. allocated(out_stream)) allocate(out_stream(1024 * 1024))
    stream_cap = size(out_stream, kind=int64)
    bytes_written = 1_int64

    ! This init handles the first-time allocation check securely
    if (codec_state%num_w == 0) call codec_state%init()

    do i = 1, size(boxes, kind=int64)
       w = boxes(i)%x2 - boxes(i)%x1
       h = boxes(i)%y2 - boxes(i)%y1
       infobyte = 0_int8

       ! 1. Evaluate Shape (Square Optimization)
       box_is_square = (w == h)
       if (box_is_square) infobyte = ibset(infobyte, 4) 

       ! 2. Evaluate Anchors (Delta / ZigZag Optimization)
       dx = boxes(i)%x1 - codec_state%current_x
       if (dx /= 0) then
          infobyte = ibset(infobyte, 5) 
          codec_state%current_x = boxes(i)%x1
       end if

       dy = boxes(i)%y1 - codec_state%current_y
       if (dy /= 0) then
          infobyte = ibset(infobyte, 6) 
          codec_state%current_y = boxes(i)%y1
       end if

       ! 3. Evaluate Width
       w_idx = codec_state%get_w_index(w)
       if (w_idx /= codec_state%current_w_index .or. codec_state%num_w == 0) then
          infobyte = ibset(infobyte, 0)
          if (w_idx <= 0) then
             infobyte = ibset(infobyte, 1)
             call codec_state%add_w(w, w_idx)
          end if
          codec_state%current_w_index = w_idx
       end if

       ! 4. Evaluate Height (Skipped entirely if Square)
       if (.not. box_is_square) then
          h_idx = codec_state%get_h_index(h)
          if (h_idx /= codec_state%current_h_index .or. codec_state%num_h == 0) then
             infobyte = ibset(infobyte, 2)
             if (h_idx <= 0) then
                infobyte = ibset(infobyte, 3)
                call codec_state%add_h(h, h_idx)
             end if
             codec_state%current_h_index = h_idx
          end if
       end if

       ! Ensure enough capacity for worst-case box (Infobyte + VarInts + Int16s)
       call ensure_capacity(out_stream, bytes_written, 40_int64, stream_cap)

       ! Write INFOBYTE
       call append_int8(out_stream, bytes_written, infobyte)

       ! Write Anchor Deltas (ZigZag VarInt)
       if (btest(infobyte, 5)) call write_varint(out_stream, bytes_written, zigzag_encode(dx))
       if (btest(infobyte, 6)) call write_varint(out_stream, bytes_written, zigzag_encode(dy))

       ! Write W Payload
       if (btest(infobyte, 0)) then
          if (btest(infobyte, 1)) then
             ! Dimensions are usually positive, standard VarInt is fine
             call write_varint(out_stream, bytes_written, int(w, int64)) 
          else
             wire_idx = int(w_idx, kind=int16)
             call append_int16(out_stream, bytes_written, wire_idx) 
          end if
       end if

       ! Write H Payload
       if (.not. box_is_square) then
          if (btest(infobyte, 2)) then
             if (btest(infobyte, 3)) then
                call write_varint(out_stream, bytes_written, int(h, int64))
             else
                wire_idx = int(h_idx, kind=int16)
                call append_int16(out_stream, bytes_written, wire_idx)
             end if
          end if
       end if
    end do

    bytes_written = bytes_written - 1_int64 
  end subroutine CompressBoxesUsingCodec
  ! ==================================================================
  ! PARALLEL COMPRESSION PIPELINE
  ! ==================================================================
  subroutine CompressBoxesToStream(boxes, stream, method_to_use)
    use iso_c_binding, only: c_size_t
    ! Assumes 'c_max_compressed_length' is accessible via your interface block

    type(Box), intent(in), target, contiguous :: boxes(:)
    type(CompressedStream), intent(inout) :: stream
    integer, intent(in) :: method_to_use
    integer(int64) :: i, current_start, current_end

    ! Buffer Capacity Math
    integer(int64) :: max_raw_bytes, max_comp_bytes
    integer(c_size_t) :: c_max_raw

    ! Thread-local variables
    type(BoxCodec) :: local_codec
    integer(int8), allocatable, target :: thread_raw_buf(:)
    integer(int8), allocatable, target :: thread_comp_buf(:)
    integer(int64) :: actual_raw_len, actual_comp_len

    stream%total_boxes = size(boxes, kind=int64)
    stream%num_chunks = (stream%total_boxes + BOXES_PER_CHUNK - 1_int64) / BOXES_PER_CHUNK
    allocate(stream%chunks(stream%num_chunks))

    ! Calculate maximum possible buffer sizes for a single chunk
    ! 4 integers per box * max 10 bytes per LEB128 VarInt
    max_raw_bytes = BOXES_PER_CHUNK * 40_int64
    c_max_raw = int(max_raw_bytes, c_size_t)
    max_comp_bytes = int(c_max_compressed_length(c_max_raw), int64)
    stream%compression_method = method_to_use
    write(*,*) 'COMPRESSION METHOD TO USE: ', method_to_use
    !$omp parallel default(none) &
    !$omp shared(stream, boxes, max_raw_bytes, max_comp_bytes,method_to_use) &
    !$omp private(i, current_start, current_end, local_codec,&
    !$omp         thread_raw_buf, thread_comp_buf, actual_raw_len, actual_comp_len)

    ! 1. Allocate thread-local buffers ONCE per thread
    allocate(thread_raw_buf(max_raw_bytes))
    allocate(thread_comp_buf(max_comp_bytes))

    !$omp do
    do i = 1_int64, stream%num_chunks
       current_start = (i - 1_int64) * BOXES_PER_CHUNK + 1_int64
       current_end = min(current_start + BOXES_PER_CHUNK - 1_int64, stream%total_boxes)
       call local_codec%init() !> for large N we might want 1024
       stream%chunks(i)%num_boxes = current_end - current_start + 1_int64

       ! Reset codec spatial cursor and dictionary sizes without reallocating
       local_codec%num_w = 0
       local_codec%num_h = 0
       local_codec%current_x = 0
       local_codec%current_y = 0

       ! 2. Compress geometry directly into the pre-allocated raw buffer
       ! (You will need to update CompressBoxesUsingCodec to accept the buffer and max size)
       !subroutine CompressBoxesUsingCodec(boxes, codec_state, out_stream, bytes_written)
       call CompressBoxesUsingCodec(boxes(current_start:current_end), local_codec, &
            thread_raw_buf, actual_raw_len)

       stream%chunks(i)%raw_byte_size = actual_raw_len

       ! 3. Compress raw bytes directly into the pre-allocated snappy buffer
       select case(method_to_use)
          case(COMPRESSION_METHOD_SNAPPY)
             call snappy_compress_buffer(thread_raw_buf, actual_raw_len, &
                  thread_comp_buf, max_comp_bytes, actual_comp_len)
          case(COMPRESSION_METHOD_ZLIB)
             call zlib_compress_buffer(thread_raw_buf, actual_raw_len, &
                  thread_comp_buf, max_comp_bytes, actual_comp_len)
          case(COMPRESSION_METHOD_ZSTD)
             call zstd_compress_buffer(thread_raw_buf, actual_raw_len, &
                  thread_comp_buf, max_comp_bytes, actual_comp_len)
          case default
             error stop "ERROR: Unknown compression method requested."             
          end select
       stream%chunks(i)%compressed_size = actual_comp_len

       ! 4. The ONLY allocation in the loop:
       ! We must allocate exactly enough memory to store the final chunk data permanently
       ! inside the stream structure so it outlives this subroutine.
       allocate(stream%chunks(i)%data(actual_comp_len))

       ! Slice copy from the thread buffer into the permanent stream structure
       stream%chunks(i)%data = thread_comp_buf(1:actual_comp_len)

    end do
    !$omp end do

    ! Cleanup thread-local buffers
    deallocate(thread_raw_buf)
    deallocate(thread_comp_buf)
    if (allocated(local_codec%unique_w)) deallocate(local_codec%unique_w)
    if (allocated(local_codec%unique_h)) deallocate(local_codec%unique_h)

    !$omp end parallel
  end subroutine CompressBoxesToStream

  ! ==================================================================
  ! PARALLEL DECOMPRESSION PIPELINE
  ! ==================================================================
  subroutine DecompressStreamToBoxes(stream, boxes)
    type(CompressedStream), intent(in) :: stream
    type(Box), allocatable, intent(out) :: boxes(:)

    integer(int64) :: i, current_start, current_end, max_raw_size

    ! Thread-local variables
    integer(int8), allocatable :: uncompressed_bytes(:)
    type(Box), allocatable :: temp_boxes(:)
    integer :: status
    integer :: compression_method
    ! ADDED: The thread-local codec state
    type(BoxCodec) :: local_codec 
    compression_method = stream%compression_method
    allocate(boxes(stream%total_boxes))

    ! ---------------------------------------------------------
    ! PRE-COMPUTE MAXIMUM BUFFER SIZE
    ! Find the largest uncompressed chunk size to guarantee our 
    ! thread-local buffers are always large enough.
    ! ---------------------------------------------------------
    max_raw_size = 0
    do i = 1_int64, stream%num_chunks
       if (stream%chunks(i)%raw_byte_size > max_raw_size) then
          max_raw_size = stream%chunks(i)%raw_byte_size
       end if
    end do

    !$omp parallel default(none) &
    !$omp shared(stream, boxes, max_raw_size,compression_method) &
    !$omp private(i, current_start, current_end, uncompressed_bytes, temp_boxes, status, local_codec)
    ! Note: local_codec MUST be in the private() list above ^

    ! ---------------------------------------------------------
    ! THREAD-LOCAL ALLOCATION (Executes exactly once per thread)
    ! ---------------------------------------------------------
    allocate(temp_boxes(BOXES_PER_CHUNK))
    allocate(uncompressed_bytes(max_raw_size)) 

    ! ADDED: Initialize the dictionary exactly ONCE per thread
    call local_codec%init()

    !$omp do
    do i = 1_int64, stream%num_chunks
       current_start = ((i - 1_int64) * BOXES_PER_CHUNK) + 1_int64
       current_end = current_start + stream%chunks(i)%num_boxes - 1_int64

       ! 1. Step 1 Decompression: Snappy to Raw Bytes
       ! We pass a SLICE of the pre-allocated buffer matching the exact expected size
       select case( compression_method )
          case(COMPRESSION_METHOD_SNAPPY)
             call snappy_uncompress_buffer( &
                  stream%chunks(i)%data, &
                  stream%chunks(i)%compressed_size, &
                  uncompressed_bytes(1 : stream%chunks(i)%raw_byte_size), & 
                  stream%chunks(i)%raw_byte_size, &
                  status &
                  )

             if (status /= 0) then
                print *, "CRITICAL ERROR: Snappy decompression failed at chunk ", i
                stop 1
             end if
          case(COMPRESSION_METHOD_ZLIB)
             call zlib_uncompress_buffer( &
                  stream%chunks(i)%data, &
                  stream%chunks(i)%compressed_size, &
                  uncompressed_bytes(1 : stream%chunks(i)%raw_byte_size), & 
                  stream%chunks(i)%raw_byte_size, &
                  status &
                  )

             if (status /= 0) then
                print *, "CRITICAL ERROR: ZLIB decompression failed at chunk ", i
                stop 1
             end if
          case(COMPRESSION_METHOD_ZSTD)
             call zstd_uncompress_buffer( &
                  stream%chunks(i)%data, &
                  stream%chunks(i)%compressed_size, &
                  uncompressed_bytes(1 : stream%chunks(i)%raw_byte_size), & 
                  stream%chunks(i)%raw_byte_size, &
                  status &
                  )

             if (status /= 0) then
                print *, "CRITICAL ERROR: ZSTD decompression failed at chunk ", i
                stop 1
             end if
          case default
             error stop "ERROR: Unknown decompression method requested."             
          end select
          

       ! 2. Step 2 Decompression: Bytes to Geometry
       ! ADDED: Passing local_codec as the 5th argument
       call DecompressBoxesUsingCodec( &
            uncompressed_bytes(1 : stream%chunks(i)%raw_byte_size), &
            stream%chunks(i)%raw_byte_size, &
            stream%chunks(i)%num_boxes, &
            temp_boxes, &
            local_codec &    
            )

       ! 3. Copy out to the globally shared array
       boxes(current_start:current_end) = temp_boxes(1:stream%chunks(i)%num_boxes)

    end do
    !$omp end do

    ! ---------------------------------------------------------
    ! THREAD-LOCAL DEALLOCATION (Executes exactly once per thread)
    ! ---------------------------------------------------------
    deallocate(uncompressed_bytes)
    deallocate(temp_boxes)

    ! ADDED: Safely clean up the thread's dictionary memory
    if (allocated(local_codec%unique_w)) deallocate(local_codec%unique_w)
    if (allocated(local_codec%unique_h)) deallocate(local_codec%unique_h)

    !$omp end parallel

  end subroutine DecompressStreamToBoxes

end module BoxCodecModule

