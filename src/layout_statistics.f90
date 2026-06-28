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
  use LayoutStatisticsModule
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
  implicit none
  private

  public :: BoxCodec, CompressBoxesUsingCodec

  type :: BoxCodec
     integer(K_COORDINATE_KIND), allocatable :: unique_w(:)
     integer(K_COORDINATE_KIND), allocatable :: unique_h(:)
     integer(int64) :: num_w = 0
     integer(int64) :: num_h = 0
     integer(int64) :: current_w_index = 0
     integer(int64) :: current_h_index = 0
     integer(kind=K_COORDINATE_KIND) :: current_x = 0
     integer(kind=K_COORDINATE_KIND) :: current_y = 0     
   contains
     procedure :: init => codec_init
     procedure :: get_w_index => codec_get_w_index
     procedure :: get_h_index => codec_get_h_index
     procedure :: add_w => codec_add_w
     procedure :: add_h => codec_add_h
  end type BoxCodec

contains

  ! =========================================================
  ! CODEC METHODS
  ! =========================================================
  subroutine codec_init(this, capacity)
    class(BoxCodec), intent(inout) :: this
    integer, intent(in), optional :: capacity
    integer :: cap

    cap = 10000 ! Default capacity based on your stats (W=541, H=251)
    if (present(capacity)) cap = capacity

    allocate(this%unique_w(cap))
    allocate(this%unique_h(cap))
    this%num_w = 0
    this%num_h = 0
    this%current_w_index = 0
    this%current_h_index = 0
  end subroutine codec_init

  function codec_get_w_index(this, w) result(idx)
    class(BoxCodec), intent(in) :: this
    integer(K_COORDINATE_KIND), intent(in) :: w
    integer(int64) :: idx, i

    idx = 0
    ! Note: For billions of boxes, this linear scan is perfectly fine 
    ! ONLY because num_w is very small (~541). 
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

    idx = 0
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

    this%num_w = this%num_w + 1
    ! Assuming array bounds are large enough (reallocation omitted for brevity)
    this%unique_w(this%num_w) = w
    idx = this%num_w
  end subroutine codec_add_w

  subroutine codec_add_h(this, h, idx)
    class(BoxCodec), intent(inout) :: this
    integer(K_COORDINATE_KIND), intent(in) :: h
    integer(int64), intent(out) :: idx

    this%num_h = this%num_h + 1
    this%unique_h(this%num_h) = h
    idx = this%num_h
  end subroutine codec_add_h

  ! =========================================================
  ! COMPRESSION SUBROUTINE
  ! =========================================================
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

    ! Initialize stream if not allocated
    if (.not. allocated(out_stream)) allocate(out_stream(1024 * 1024))
    stream_cap = size(out_stream, kind=int64)
    bytes_written = 1

    if (codec_state%num_w == 0) call codec_state%init()

    do i = 1, size(boxes, kind=int64)
       w = boxes(i)%x2 - boxes(i)%x1
       h = boxes(i)%y2 - boxes(i)%y1
       infobyte = 0_int8
       
       ! 1. Evaluate Shape (Square Optimization)
       box_is_square = (w == h)
       if (box_is_square) infobyte = ibset(infobyte, 4) ! Bit 4: Square
       
       ! 2. Evaluate Anchors (Delta Encoding Optimization)
       dx = boxes(i)%x1 - codec_state%current_x
       if (dx /= 0) then
          infobyte = ibset(infobyte, 5) ! Bit 5: X Changed
          codec_state%current_x = boxes(i)%x1
       end if
       
       dy = boxes(i)%y1 - codec_state%current_y
       if (dy /= 0) then
          infobyte = ibset(infobyte, 6) ! Bit 6: Y Changed
          codec_state%current_y = boxes(i)%y1
       end if

       ! 3. Evaluate Width (Always evaluated)
       w_idx = codec_state%get_w_index(w)
       if (w_idx /= codec_state%current_w_index .or. codec_state%num_w == 0) then
          infobyte = ibset(infobyte, 0) ! Bit 0: W Changed
          if (w_idx == 0) then
             infobyte = ibset(infobyte, 1) ! Bit 1: W is New
             call codec_state%add_w(w, w_idx)
          end if
          codec_state%current_w_index = w_idx
       end if

       ! 4. Evaluate Height (Skipped entirely if Square)
       if (.not. box_is_square) then
          h_idx = codec_state%get_h_index(h)
          if (h_idx /= codec_state%current_h_index .or. codec_state%num_h == 0) then
             infobyte = ibset(infobyte, 2) ! Bit 2: H Changed
             if (h_idx == 0) then
                infobyte = ibset(infobyte, 3) ! Bit 3: H is New
                call codec_state%add_h(h, h_idx)
             end if
             codec_state%current_h_index = h_idx
          end if
       end if

       ! 5. Write Data to Stream
       ! Check capacity (Bumped to 40 bytes to safely accommodate 64-bit kinds if used)
       call ensure_capacity(out_stream, bytes_written, 40_int64, stream_cap)

       ! Write INFOBYTE
       call append_val(out_stream, bytes_written, infobyte)

       ! Write Anchor Deltas (Only if changed)
       if (btest(infobyte, 5)) call append_val(out_stream, bytes_written, dx)
       if (btest(infobyte, 6)) call append_val(out_stream, bytes_written, dy)

       ! Write W Payload (Only if changed)
       if (btest(infobyte, 0)) then
          if (btest(infobyte, 1)) then
             call append_val(out_stream, bytes_written, w) ! New raw W
          else
             wire_idx = int(w_idx, kind=int16)
             call append_val(out_stream, bytes_written, wire_idx) ! Existing index W
          end if
       end if

       ! Write H Payload (Only if changed AND not a square)
       if (.not. box_is_square) then
          if (btest(infobyte, 2)) then
             if (btest(infobyte, 3)) then
                call append_val(out_stream, bytes_written, h) ! New raw H
             else
                wire_idx = int(h_idx, kind=int16)
                call append_val(out_stream, bytes_written, wire_idx) ! Existing index H
             end if
          end if
       end if
       
    end do

    ! Adjust written count to absolute size (0-based offset correction)
    bytes_written = bytes_written - 1 

  end subroutine CompressBoxesUsingCodec

  ! =========================================================
  ! STREAM UTILITIES
  ! =========================================================
  subroutine ensure_capacity(stream, pos, needed, capacity)
    integer(int8), allocatable, intent(inout) :: stream(:)
    integer(int64), intent(in) :: pos, needed
    integer(int64), intent(inout) :: capacity
    integer(int8), allocatable :: temp(:)

    if (pos + needed - 1 > capacity) then
       capacity = capacity * 2 + needed
       allocate(temp(capacity))
       temp(1:pos-1) = stream(1:pos-1)
       call move_alloc(temp, stream)
    end if
  end subroutine ensure_capacity

  subroutine append_val(stream, pos, val)
    integer(int8), allocatable, intent(inout) :: stream(:)
    integer(int64), intent(inout) :: pos
    class(*), intent(in) :: val

    integer(int8) :: bytes(storage_size(val)/8)
    integer(int64) :: sz

    sz = size(bytes, kind=int64)
    bytes = transfer(val, bytes)
    stream(pos : pos+sz-1) = bytes
    pos = pos + sz
  end subroutine append_val

end module BoxCodecModule
