! File.   : layout_statistics.f90
! Author  : Sandeep Koranne (C) 2026.
! Purpose : Memory reduction using Order Statistics
!We have an array of type(Box), where Box is x1,y1,x2,y2 of type K_COORDINATE_KIND.
!Devise and generate a modern fortran subroutine to calculate ORDER STATISTICS on the WIDTH and HEIGHT of these boxes.
! The number of boxes is 1 billion so efficiency is key. One thought could be to develop an OPEN ADDRESSED HASH TABLE
!on WIDTH HEIGHT. I expect some quantization on WIDTH and HEIGHT so the UNIQUE number of WIDTHS and HEIGHTS
!is very sparse. We want to find UNIQUE boxes which are translation invariant.
!The goal is to first find order statistics, print "There are N boxes and from that there are K
!unique W/H boxes. In total there are W1 widths and H1 heights. Here is a HISTOGRAM in ASCII plot
! (simple frequency on X axis, value on Y).
!Develop idiomatic modern Fortran to achieve this task in a module with an OPEN ADDRESSED
!hash table and subroutines for order statistics.
!Be thoughtful and document your work in detail. We may extend this code to collect the (X,Y)
!translations of a unique box, so keep that in mind.
! =================================================
!  BOX ORDER STATISTICS
!  =================================================
! Total Boxes (N):              424548608
! Unique W/H Pairs (K):              2454
! Distinct Widths (W1):               541
! Distinct Heights (H1):              251
!  -------------------------------------------------
! Min Width:                          170
! Max Width:                        10120
! Median Width (approx):              390

module LayoutStatisticsModule
   use CommonModule
   use GeometryModule
   use iso_fortran_env, only: int64
   implicit none
   private

   ! Prime constants for spatial hashing
   integer(K_COORDINATE_KIND), parameter :: PRIME1 = 73856093_K_COORDINATE_KIND
   integer(K_COORDINATE_KIND), parameter :: PRIME2 = 19349663_K_COORDINATE_KIND

   public :: analyze_boxes

   ! Hash Table Entry
   type :: hash_entry_t
      integer(K_COORDINATE_KIND) :: w = -1_K_COORDINATE_KIND
      integer(K_COORDINATE_KIND) :: h = -1_K_COORDINATE_KIND
      integer(K_COORDINATE_KIND) :: count = 0_K_COORDINATE_KIND
      ! Future extension: index to a CSR array storing actual (X,Y) translations
      integer(K_COORDINATE_KIND) :: head_index = -1_K_COORDINATE_KIND
   end type hash_entry_t

   ! Open Addressed Hash Table
   type :: hash_table_t
      integer(K_COORDINATE_KIND) :: capacity
      integer(K_COORDINATE_KIND) :: size
      type(hash_entry_t), allocatable :: entries(:)
   end type hash_table_t

contains

   ! ------------------------------------------------------------------
   ! Hash Table Management
   ! ------------------------------------------------------------------
   subroutine ht_init(ht, initial_capacity)
      type(hash_table_t), intent(inout) :: ht
      integer(K_COORDINATE_KIND), intent(in) :: initial_capacity
      ht%capacity = initial_capacity
      ht%size = 0
      allocate(ht%entries(ht%capacity))
   end subroutine ht_init

   subroutine ht_free(ht)
      type(hash_table_t), intent(inout) :: ht
      if (allocated(ht%entries)) deallocate(ht%entries)
      ht%size = 0
      ht%capacity = 0
   end subroutine ht_free
   ! ------------------------------------------------------------------
   ! FIXED: Hash Function (Sign Bit Masking)
   ! ------------------------------------------------------------------
   ! ------------------------------------------------------------------
   ! FIXED: Compiler-Agnostic Hash Function
   ! ------------------------------------------------------------------
   pure function hash_func(w, h, capacity) result(idx)
      integer(K_COORDINATE_KIND), intent(in) :: w, h, capacity
      integer(K_COORDINATE_KIND) :: idx, hash_val

      ! Allow the integer multiplication to overflow naturally
      hash_val = ieor(w * PRIME1, h * PRIME2)

      ! Fortran's mod(A, P) preserves the sign of A.
      ! We compute the mod, and if it's negative, wrap it back to positive.
      idx = mod(hash_val, capacity)
      if (idx < 0_K_COORDINATE_KIND) then
         idx = idx + capacity
      end if

      ! Map to Fortran's 1-based indexing
      idx = idx + 1_K_COORDINATE_KIND
   end function hash_func

   ! ------------------------------------------------------------------
   ! FIXED: Rehash (OOM Trapping)
   ! ------------------------------------------------------------------
   subroutine ht_rehash(ht)
      type(hash_table_t), intent(inout) :: ht
      type(hash_table_t) :: new_ht
      integer(K_COORDINATE_KIND) :: i, new_cap
      integer :: alloc_stat

      new_cap = ht%capacity * 2
      new_ht%capacity = new_cap
      new_ht%size = 0

      allocate(new_ht%entries(new_cap), stat=alloc_stat)
      if (alloc_stat /= 0) then
         print *, "CRITICAL ERROR: Out of Memory during Hash Table Rehash!"
         print *, "Attempted to allocate capacity: ", new_cap
         stop 1
      end if

      do i = 1, ht%capacity
         if (ht%entries(i)%count > 0) then
            call ht_insert(new_ht, ht%entries(i)%w, ht%entries(i)%h, ht%entries(i)%head_index)
         end if
      end do

      ! Copy old counts
      do i = 1, ht%capacity
         if (ht%entries(i)%count > 0) then
            new_ht%entries(hash_func_rehash(new_ht, ht%entries(i)%w, ht%entries(i)%h))%count = ht%entries(i)%count
         end if
      end do

      call ht_free(ht)
      ht = new_ht
   end subroutine ht_rehash

   ! ------------------------------------------------------------------
   ! HIGH-PERFORMANCE HYBRID SORT: Quicksort + Insertion Sort Fallback
   ! ------------------------------------------------------------------
   recursive subroutine quicksortWH(a, left, right)
      use iso_fortran_env, only: int64

      integer(K_COORDINATE_KIND), intent(inout) :: a(:)
      integer(int64), intent(in) :: left, right

      ! Threshold for switching to Insertion Sort (typically 16-32)
      integer(int64), parameter :: K_SMALL_THRESHOLD = 16_int64

      integer(K_COORDINATE_KIND) :: pivot, temp
      integer(int64) :: i, j

      ! 1. Base Case / Small Array Fallback (Insertion Sort)
      if (right - left < K_SMALL_THRESHOLD) then
         if (left >= right) return

         ! Inline Insertion Sort for extreme L1 cache efficiency
         do i = left + 1_int64, right
            temp = a(i)
            j = i - 1_int64

            ! Using a loop with explicit exit avoids Fortran's lack of
            ! guaranteed short-circuit evaluation on logical .and.
            do while (j >= left)
               if (a(j) <= temp) exit
               a(j + 1_int64) = a(j)
               j = j - 1_int64
            end do
            a(j + 1_int64) = temp
         end do
         return
      end if

      ! 2. Standard Quicksort Partitioning
      pivot = a((left + right) / 2_int64)
      i = left
      j = right

      do while (i <= j)
         do while (a(i) < pivot)
            i = i + 1_int64
         end do
         do while (a(j) > pivot)
            j = j - 1_int64
         end do
         if (i <= j) then
            temp = a(i)
            a(i) = a(j)
            a(j) = temp
            i = i + 1_int64
            j = j - 1_int64
         end if
      end do

      ! 3. Recursive Calls
      if (left < j) call quicksortWH(a, left, j)
      if (i < right) call quicksortWH(a, i, right)

   end subroutine quicksortWH

   recursive subroutine quicksortWG(a, left, right)
      integer(K_COORDINATE_KIND), intent(inout) :: a(:)
      integer(kind=int64), intent(in) :: left, right
      integer(K_COORDINATE_KIND) :: pivot, temp
      integer(kind=int64) :: i, j

      if (left >= right) return

      pivot = a((left + right) / 2)
      i = left
      j = right

      do while (i <= j)
         do while (a(i) < pivot)
            i = i + 1
         end do
         do while (a(j) > pivot)
            j = j - 1
         end do
         if (i <= j) then
            temp = a(i)
            a(i) = a(j)
            a(j) = temp
            i = i + 1
            j = j - 1
         end if
      end do

      if (left < j) call quicksortWG(a, left, j)
      if (i < right) call quicksortWG(a, i, right)
   end subroutine quicksortWG

   subroutine ht_insert(ht, w, h, box_idx)
      type(hash_table_t), intent(inout) :: ht
      integer(K_COORDINATE_KIND), intent(in) :: w, h
      integer(K_COORDINATE_KIND), intent(in) :: box_idx
      integer(K_COORDINATE_KIND) :: idx, start_idx
      real :: load_factor

      ! Trigger rehash if load factor > 0.6 to maintain O(1) linear probe performance
      load_factor = real(ht%size) / real(ht%capacity)
      if (load_factor > 0.6) then
         call ht_rehash(ht)
      end if

      start_idx = hash_func(w, h, ht%capacity)
      idx = start_idx

      do
         if (ht%entries(idx)%count == 0) then
            ! Found empty slot
            ht%entries(idx)%w = w
            ht%entries(idx)%h = h
            ht%entries(idx)%count = 1
            ht%entries(idx)%head_index = box_idx ! Store first occurrence for translations
            ht%size = ht%size + 1
            return
         else if (ht%entries(idx)%w == w .and. ht%entries(idx)%h == h) then
            ! Found existing W/H signature
            ht%entries(idx)%count = ht%entries(idx)%count + 1
            ! (Future: push box_idx to a linked list/CSR here)
            return
         end if

         ! Linear probe
         idx = mod(idx, ht%capacity) + 1
         if (idx == start_idx) stop "Hash table completely full."
      end do
   end subroutine ht_insert

   ! Helper to find exact index post-rehash
   function hash_func_rehash(ht, w, h) result(idx)
      type(hash_table_t), intent(in) :: ht
      integer(K_COORDINATE_KIND), intent(in) :: w, h
      integer(K_COORDINATE_KIND) :: idx
      idx = hash_func(w, h, ht%capacity)
      do
         if (ht%entries(idx)%w == w .and. ht%entries(idx)%h == h) return
         idx = mod(idx, ht%capacity) + 1
      end do
   end function hash_func_rehash

   ! ------------------------------------------------------------------
   ! Core Analysis Routine
   ! ------------------------------------------------------------------
   subroutine analyze_boxes(boxes)
      type(Box), intent(in) :: boxes(:)
      integer(K_COORDINATE_KIND) :: n_boxes, i, w, h
      type(hash_table_t) :: ht

      ! Stats arrays
      integer(K_COORDINATE_KIND), allocatable :: unique_w(:), unique_h(:)
      integer(kind=int64) :: w1_count, h1_count, k_unique

      n_boxes = size(boxes, kind=K_COORDINATE_KIND)

      ! Assume quantization limits unique signatures.
      ! Start with 1 million capacity to prevent early rehashes.
      call ht_init(ht, 1000000_K_COORDINATE_KIND)

      ! Populate Hash Table
      ! (If migrating to OpenMP target offload later, this loop needs an atomic
      !  update mechanism for the hash slots or thread-local sub-tables)
      do i = 1, n_boxes
         w = abs(boxes(i)%x2 - boxes(i)%x1)
         h = abs(boxes(i)%y2 - boxes(i)%y1)
         call ht_insert(ht, w, h, i)
      end do

      k_unique = ht%size

      ! Extract unique Widths and Heights for Order Statistics
      allocate(unique_w(k_unique), unique_h(k_unique))
      w1_count = 0
      h1_count = 0

      do i = 1, ht%capacity
         if (ht%entries(i)%count > 0) then
            w1_count = w1_count + 1
            h1_count = h1_count + 1
            unique_w(w1_count) = ht%entries(i)%w
            unique_h(h1_count) = ht%entries(i)%h
         end if
      end do

      ! Sort to find distinct W1 and H1 and order stats
      call quicksortWH(unique_w, 1_int64, w1_count)
      call quicksortWH(unique_h, 1_int64, h1_count)

      w1_count = count_distinct(unique_w)
      h1_count = count_distinct(unique_h)

      ! Print Summary
      print *, "================================================="
      print *, "BOX ORDER STATISTICS"
      print *, "================================================="
      print '(A, I15)', "Total Boxes (N):        ", n_boxes
      print '(A, I15)', "Unique W/H Pairs (K):   ", k_unique
      print '(A, I15)', "Distinct Widths (W1):   ", w1_count
      print '(A, I15)', "Distinct Heights (H1):  ", h1_count
      print *, "-------------------------------------------------"
      if (k_unique > 0) then
         print '(A, I15)', "Min Width:              ", unique_w(1)
         print '(A, I15)', "Max Width:              ", unique_w(k_unique)
         print '(A, I15)', "Median Width (approx):  ", unique_w(k_unique/2 + 1)
      end if
      print *, "================================================="

      ! Generate ASCII Histogram
      call print_histogram(ht)

      call ht_free(ht)
   end subroutine analyze_boxes

   ! ------------------------------------------------------------------
   ! Utilities: Sort, Distinct Count, Histogram
   ! ------------------------------------------------------------------
   pure function count_distinct(arr) result(dist_count)
      integer(K_COORDINATE_KIND), intent(in) :: arr(:)
      integer(K_COORDINATE_KIND) :: dist_count, i
      if (size(arr) == 0) then
         dist_count = 0
         return
      end if
      dist_count = 1
      do i = 2, size(arr)
         if (arr(i) /= arr(i-1)) dist_count = dist_count + 1
      end do
   end function count_distinct

   recursive subroutine quicksort(a)
      integer(K_COORDINATE_KIND), intent(inout) :: a(:)
      integer(K_COORDINATE_KIND) :: pivot, temp
      integer :: i, j

      if (size(a) <= 1) return

      pivot = a(size(a)/2)
      i = 1
      j = size(a)

      do while (i <= j)
         do while (a(i) < pivot)
            i = i + 1
         end do
         do while (a(j) > pivot)
            j = j - 1
         end do
         if (i <= j) then
            temp = a(i)
            a(i) = a(j)
            a(j) = temp
            i = i + 1
            j = j - 1
         end if
      end do

      if (1 < j) call quicksort(a(1:j))
      if (i < size(a)) call quicksort(a(i:size(a)))
   end subroutine quicksort

   subroutine print_histogram(ht)
      type(hash_table_t), intent(in) :: ht
      integer(K_COORDINATE_KIND) :: i, max_count, bar_len
      integer, parameter :: MAX_BAR_LEN = 50
      character(len=MAX_BAR_LEN) :: bar
      integer :: printed

      print *, "Top 20 Frequent W/H Signatures (Y-Axis) | Frequency (X-Axis)"
      print *, "------------------------------------------------------------"

      ! Find max frequency for scaling
      max_count = 0
      do i = 1, ht%capacity
         if (ht%entries(i)%count > max_count) max_count = ht%entries(i)%count
      end do

      if (max_count == 0) return

      ! Simple dump of populated bins (In a production system, you'd sort by frequency first)
      ! Here we just print the first 20 we find for brevity
      printed = 0
      do i = 1, ht%capacity
         if (ht%entries(i)%count > 0) then
            bar_len = nint(real(ht%entries(i)%count) / real(max_count) * MAX_BAR_LEN)
            if (bar_len == 0 .and. ht%entries(i)%count > 0) bar_len = 1

            bar = ""
            if (bar_len > 0) bar(1:bar_len) = repeat("*", bar_len)

            print '(A1, I8, A1, I8, A4, I9, A2, A)', &
               "(", ht%entries(i)%w, ",", ht%entries(i)%h, ") | ", &
               ht%entries(i)%count, " | ", trim(bar)

            printed = printed + 1
            if (printed >= 20) exit
         end if
      end do
   end subroutine print_histogram

end module LayoutStatisticsModule

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
module BoxCodecModule
   use CommonModule
   use GeometryModule
   use SnappyCompressionModule
   use iso_fortran_env, only: int8, int32, int64
   implicit none
   private
   public ::  BoxCodec, CompressBoxesUsingCodec

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
   public :: CompressedChunk, CompressedStream
   public :: CompressBoxesToSnappyStream, DecompressStreamToBoxes

   ! Adjust this based on your L2/L3 cache sizes.
   ! 65536 boxes * 32 bytes = ~2MB uncompressed per chunk.
   integer(int64), parameter :: BOXES_PER_CHUNK = 65536_int64

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
   end type CompressedStream

contains

   ! ==================================================================
   ! Codec Type-Bound Procedures
   ! ==================================================================

   subroutine codec_init(this, initial_capacity)
      class(BoxCodec), intent(inout) :: this
      integer(int64), intent(in) :: initial_capacity

      allocate(this%unique_w(initial_capacity))
      allocate(this%unique_h(initial_capacity))
      this%num_w = 0
      this%num_h = 0
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

   ! ==================================================================
   ! Serialization Primitives
   ! ==================================================================

   ! ZigZag encoding maps signed integers to unsigned space
   pure function zigzag_encode(val) result(zval)
      integer(K_COORDINATE_KIND), intent(in) :: val
      integer(int64) :: zval
      ! (val << 1) XOR (val >> 63)
      zval = ieor(shiftl(int(val, int64), 1), shiftr(int(val, int64), 63))
   end function zigzag_encode

   ! LEB128 packs integers into variable bytes (7 data bits + 1 continue bit)
   pure subroutine write_varint(stream, offset, val)
      integer(int8), intent(inout) :: stream(:)
      integer(int64), intent(inout) :: offset
      integer(int64), intent(in) :: val
      integer(int64) :: temp
      integer(int8)  :: byte

      temp = val
      do
         byte = int(iand(temp, 127_int64), int8)
         temp = shiftr(temp, 7)
         if (temp /= 0) then
            ! FIX: Use ibset to set the 7th bit (the MSB) safely
            byte = ibset(byte, 7)
            stream(offset) = byte
            offset = offset + 1_int64
         else
            stream(offset) = byte
            offset = offset + 1_int64
            exit
         end if
      end do
   end subroutine write_varint

   ! ==================================================================
   ! Deserialization Primitives
   ! ==================================================================

   ! Reverse of ZigZag: maps unsigned integers back to signed integers
   pure function zigzag_decode(zval) result(val)
      integer(int64), intent(in) :: zval
      integer(K_COORDINATE_KIND) :: val

      ! If LSB is 0, the original value was positive: val = zval / 2
      if (iand(zval, 1_int64) == 0_int64) then
         val = int(shiftr(zval, 1), K_COORDINATE_KIND)
      else
         ! If LSB is 1, the original value was negative: val = -(zval / 2 + 1)
         val = -int(shiftr(zval, 1) + 1_int64, K_COORDINATE_KIND)
      end if
   end function zigzag_decode

   ! Reads a LEB128 variable-length integer from the byte stream
   pure subroutine read_varint(stream, offset, val)
      integer(int8), intent(in) :: stream(:)
      integer(int64), intent(inout) :: offset
      integer(int64), intent(out) :: val
      integer(int64) :: shift
      integer(int8)  :: byte

      val = 0_int64
      shift = 0_int64
      do
         byte = stream(offset)
         offset = offset + 1_int64

         ! Extract the lower 7 bits and shift them into place
         val = ior(val, shiftl(iand(int(byte, int64), 127_int64), int(shift, 4)))
         shift = shift + 7_int64

         ! If the MSB (bit 7) is 0, this is the last byte
         if (.not. btest(byte, 7)) exit
      end do
   end subroutine read_varint
! ==================================================================
   ! Main Decompression Engine
   ! ==================================================================

   subroutine DecompressBoxesUsingCodec(in_stream, stream_len, num_boxes, boxes)
      integer(int8), intent(in) :: in_stream(:)
      integer(int64), intent(in) :: stream_len, num_boxes
      type(Box), intent(inout) :: boxes(:)

      integer(int64) :: i, offset, val, idx, dummy_idx
      integer(K_COORDINATE_KIND) :: dx, dy, w, h, cur_x, cur_y

      type(BoxCodec) :: local_codec

      ! Initialize isolated dictionary for this specific chunk
      call local_codec%init(1024_int64)
      cur_x = 0_K_COORDINATE_KIND
      cur_y = 0_K_COORDINATE_KIND
      offset = 1_int64

      do i = 1_int64, num_boxes
         ! 1. Delta Encoded Spatial Translation
         call read_varint(in_stream, offset, val)
         dx = zigzag_decode(val)

         call read_varint(in_stream, offset, val)
         dy = zigzag_decode(val)

         cur_x = cur_x + dx
         cur_y = cur_y + dy

         ! 2. Dictionary Encoded Width
         call read_varint(in_stream, offset, idx)
         if (idx == 0_int64) then
            ! Flag 0 detected: The next VarInt is the literal width
            call read_varint(in_stream, offset, val)
            w = int(val, K_COORDINATE_KIND)
            call local_codec%add_w(w, dummy_idx)
         else
            ! Fetch from rebuilt dictionary
            w = local_codec%unique_w(idx)
         end if

         ! 3. Dictionary Encoded Height
         call read_varint(in_stream, offset, idx)
         if (idx == 0_int64) then
            ! Flag 0 detected: The next VarInt is the literal height
            call read_varint(in_stream, offset, val)
            h = int(val, K_COORDINATE_KIND)
            call local_codec%add_h(h, dummy_idx)
         else
            ! Fetch from rebuilt dictionary
            h = local_codec%unique_h(idx)
         end if

         ! 4. Reconstruct Box
         boxes(i)%x1 = cur_x
         boxes(i)%y1 = cur_y
         boxes(i)%x2 = cur_x + w
         boxes(i)%y2 = cur_y + h

         ! Safety check against stream corruption
         if (offset > stream_len + 1_int64) then
            print *, "CRITICAL: Decompression read past end of Snappy uncompressed buffer!"
            stop 1
         end if
      end do

      ! Cleanup thread-local dictionary
      if (allocated(local_codec%unique_w)) deallocate(local_codec%unique_w)
      if (allocated(local_codec%unique_h)) deallocate(local_codec%unique_h)

   end subroutine DecompressBoxesUsingCodec
   ! ==================================================================
   ! Main Compression Engine
   ! ==================================================================
   subroutine CompressBoxesUsingCodec(boxes, codec_state, out_stream, max_bytes, bytes_written)
      type(Box), intent(in), contiguous :: boxes(:)
      type(BoxCodec), intent(inout)     :: codec_state
      ! FIX: out_stream is now a pre-allocated buffer passed from the caller
      integer(int8), intent(inout)      :: out_stream(:)
      integer(int64), intent(in)        :: max_bytes
      integer(int64), intent(out)       :: bytes_written

      integer(int64) :: i, n_boxes, offset
      integer(K_COORDINATE_KIND) :: w, h, dx, dy
      integer(int64) :: w_idx, h_idx

      n_boxes = size(boxes, kind=int64)
      offset = 1_int64

      do i = 1_int64, n_boxes
         ! --- Delta Encoded Spatial Translation ---
         dx = boxes(i)%x1 - codec_state%current_x
         dy = boxes(i)%y1 - codec_state%current_y

         codec_state%current_x = boxes(i)%x1
         codec_state%current_y = boxes(i)%y1

         ! Safety bounds check
         if (offset + 40_int64 > max_bytes) stop "Buffer overflow!"

         call write_varint(out_stream, offset, zigzag_encode(dx))
         call write_varint(out_stream, offset, zigzag_encode(dy))

         ! --- Dictionary Encoded Width ---
         w = boxes(i)%x2 - boxes(i)%x1
         w_idx = codec_state%get_w_index(w)

         if (w_idx == -1_int64) then
            call codec_state%add_w(w, w_idx)
            call write_varint(out_stream, offset, 0_int64) ! FLAG: New Entry
            call write_varint(out_stream, offset, int(w, int64)) ! Literal Value
         else
            call write_varint(out_stream, offset, w_idx)   ! Dictionary Index
         end if

         ! --- Dictionary Encoded Height ---
         h = boxes(i)%y2 - boxes(i)%y1
         h_idx = codec_state%get_h_index(h)

         if (h_idx == -1_int64) then
            call codec_state%add_h(h, h_idx)
            call write_varint(out_stream, offset, 0_int64) ! FLAG: New Entry
            call write_varint(out_stream, offset, int(h, int64)) ! Literal Value
         else
            call write_varint(out_stream, offset, h_idx)   ! Dictionary Index
         end if
      end do
      bytes_written = offset - 1_int64

      ! Note: We no longer slice/reallocate out_stream here.
      ! We just return the bytes_written to the caller.
   end subroutine CompressBoxesUsingCodec
   ! ==================================================================
   ! ZERO-ALLOCATION SNAPPY WRAPPERS
   ! ==================================================================

   subroutine snappy_compress_buffer(input, in_len, output, out_len, actual_len)
      use iso_c_binding, only: c_size_t, c_loc, c_char, c_ptr, c_f_pointer
      implicit none

      integer(int8), intent(in), target    :: input(:)
      integer(int64), intent(in)           :: in_len
      integer(int8), intent(inout), target :: output(:)  ! Pre-allocated thread buffer
      integer(int64), intent(in)           :: out_len    ! Total capacity of output
      integer(int64), intent(out)          :: actual_len ! Bytes actually written

      integer(c_size_t) :: c_in_len, c_out_len
      character(kind=c_char), pointer :: char_output(:)

      c_in_len = int(in_len, c_size_t)
      c_out_len = int(out_len, c_size_t)

      ! Zero-copy type map from int8 to c_char array
      call c_f_pointer(c_loc(output), char_output, [c_out_len])

      ! Call C function directly. c_loc(input) provides the required c_ptr
      call c_compress_bytes(c_loc(input), c_in_len, char_output, c_out_len)

      actual_len = int(c_out_len, int64)
   end subroutine snappy_compress_buffer


   subroutine snappy_uncompress_buffer(input, in_len, output, out_len, status)
      use iso_c_binding, only: c_size_t, c_loc, c_char, c_ptr, c_f_pointer, c_bool
      implicit none

      integer(int8), intent(in), target    :: input(:)
      integer(int64), intent(in)           :: in_len
      integer(int8), intent(inout), target :: output(:)  ! Pre-allocated thread buffer
      integer(int64), intent(in)           :: out_len    ! Expected uncompressed size
      integer :: status

      integer(c_size_t) :: c_in_len
      character(kind=c_char), pointer :: char_input(:)
      logical(c_bool) :: success

      c_in_len = int(in_len, c_size_t)
      ! Zero-copy type map from int8 to c_char array
      call c_f_pointer(c_loc(input), char_input, [c_in_len])

      ! Call C function directly. c_loc(output) provides the required c_ptr
      success = c_uncompress_bytes(char_input, c_in_len, c_loc(output))
      !if (size(output, kind=int64) < expected_output_length) then
      !   print *, "CRITICAL: Allocated output buffer is too small!"
      !   stop 1
      !end if

      if (success) then
         status = 0
      else
         status = 2 ! Corruption
      end if
   end subroutine snappy_uncompress_buffer
   ! ==================================================================
   ! PARALLEL COMPRESSION PIPELINE
   ! ==================================================================
   subroutine CompressBoxesToSnappyStream(boxes, stream)
      use iso_c_binding, only: c_size_t
      ! Assumes 'c_max_compressed_length' is accessible via your interface block

      type(Box), intent(in), target, contiguous :: boxes(:)
      type(CompressedStream), intent(inout) :: stream

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

      !$omp parallel default(none) &
      !$omp shared(stream, boxes, max_raw_bytes, max_comp_bytes) &
      !$omp private(i, current_start, current_end, local_codec, &
      !$omp         thread_raw_buf, thread_comp_buf, actual_raw_len, actual_comp_len)

      ! 1. Allocate thread-local buffers ONCE per thread
      allocate(thread_raw_buf(max_raw_bytes))
      allocate(thread_comp_buf(max_comp_bytes))
      call local_codec%init(1024_int64)

      !$omp do
      do i = 1_int64, stream%num_chunks
         current_start = (i - 1_int64) * BOXES_PER_CHUNK + 1_int64
         current_end = min(current_start + BOXES_PER_CHUNK - 1_int64, stream%total_boxes)

         stream%chunks(i)%num_boxes = current_end - current_start + 1_int64

         ! Reset codec spatial cursor and dictionary sizes without reallocating
         local_codec%num_w = 0
         local_codec%num_h = 0
         local_codec%current_x = 0
         local_codec%current_y = 0

         ! 2. Compress geometry directly into the pre-allocated raw buffer
         ! (You will need to update CompressBoxesUsingCodec to accept the buffer and max size)
         call CompressBoxesUsingCodec(boxes(current_start:current_end), local_codec, &
            thread_raw_buf, max_raw_bytes, actual_raw_len)

         stream%chunks(i)%raw_byte_size = actual_raw_len

         ! 3. Compress raw bytes directly into the pre-allocated snappy buffer
         call snappy_compress_buffer(thread_raw_buf, actual_raw_len, &
            thread_comp_buf, max_comp_bytes, actual_comp_len)

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
   end subroutine CompressBoxesToSnappyStream

   ! ==================================================================
   ! PARALLEL DECOMPRESSION PIPELINE
   ! ==================================================================
   subroutine DecompressStreamToBoxes(stream, boxes)
      type(CompressedStream), intent(in) :: stream
      type(Box), allocatable, intent(out) :: boxes(:)

      integer(int64) :: i, current_start, current_end

      ! Thread-local variables
      integer(int8), allocatable :: uncompressed_bytes(:)
      type(Box), allocatable :: temp_boxes(:)
      integer :: status

      allocate(boxes(stream%total_boxes))

      !$omp parallel default(none) &
      !$omp shared(stream, boxes) &
      !$omp private(i, current_start, current_end, uncompressed_bytes, temp_boxes, status)

      ! Allocate the local geometry buffer once per thread
      allocate(temp_boxes(BOXES_PER_CHUNK))

      !$omp do
      do i = 1_int64, stream%num_chunks
         current_start = ((i - 1_int64) * BOXES_PER_CHUNK) + 1_int64
         current_end = current_start + stream%chunks(i)%num_boxes - 1_int64

         ! 1. Step 1 Decompression: Snappy to Raw ZigZag/LEB128 Bytes
         call snappy_uncompress_buffer( &
            stream%chunks(i)%data, &
            stream%chunks(i)%compressed_size, &
            uncompressed_bytes, &
            stream%chunks(i)%raw_byte_size, &
            status&
            )

         if (status /= 0) then
            print *, "CRITICAL ERROR: Snappy decompression failed at chunk ", i
            ! In a production system, handle this safely rather than stopping
            stop 1
         end if

         ! 2. Step 2 Decompression: Bytes to Geometry
         ! (You will need to implement this inverse function in layout_codec_mod)
         call DecompressBoxesUsingCodec( &
            uncompressed_bytes, &
            stream%chunks(i)%raw_byte_size, &
            stream%chunks(i)%num_boxes, &
            temp_boxes &
            )

         ! 3. Copy out to the globally shared array
         boxes(current_start:current_end) = temp_boxes(1:stream%chunks(i)%num_boxes)

         deallocate(uncompressed_bytes)
      end do
      !$omp end do

      deallocate(temp_boxes)
      !$omp end parallel

   end subroutine DecompressStreamToBoxes


end module BoxCodecModule

