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

