!=====================================================================
!  compress_mod.f90
!=====================================================================
module compress_mod
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
  use box_mod, only: Box, BoxByteStream, K_COORDINATE_KIND
  implicit none
  private
  public :: compress_boxes, decompress_box_stream

  ! -----------------------------------------------------------------
  !  Private helper: Zig‑Zag encode a signed integer so that small
  !  magnitude values become small *unsigned* values.
  ! -----------------------------------------------------------------
  pure function zz_encode(i) result(u)
    integer(kind=K_COORDINATE_KIND), intent(in) :: i
    integer(kind=int64)                         :: u
    u = shiftl(int(i, int64), 1)
    if (i < 0) u = not(u)
  end function zz_encode

  pure function zz_decode(u) result(i)
    integer(kind=int64), intent(in) :: u
    integer(kind=K_COORDINATE_KIND) :: i
    integer(kind=int64)             :: tmp
    tmp = shiftl(u, -1)
    if (btest(u,0)) tmp = not(tmp)
    i = int(tmp, K_COORDINATE_KIND)
  end function zz_decode

  ! -----------------------------------------------------------------
  !  Write an unsigned 64‑bit integer in VLQ form into a byte buffer.
  !  The buffer is automatically enlarged if needed.
  ! -----------------------------------------------------------------
  subroutine vlq_put(u, buf, pos)
    integer(kind=int64), intent(in)      :: u
    integer(kind=int8),  allocatable, intent(inout) :: buf(:)
    integer(kind=int64), intent(inout)   :: pos          ! 1‑based index

    integer(kind=int64) :: v, more
    integer(kind=int8)  :: byte

    v = u
    do
       more = iand(v, int(z'7F',int64))
       v   = ishft(v, -7)
       if (v /= 0_int64) then
          byte = int(more, int8) .or. int(z'80',int8)   ! set continuation bit
       else
          byte = int(more, int8)                       ! final byte
       end if

       if (pos > size(buf, kind=int64)) call enlarge(buf, pos+1024)
       buf(pos) = byte
       pos = pos + 1
       if (v == 0_int64) exit
    end do
  end subroutine vlq_put

  ! -----------------------------------------------------------------
  !  Read an unsigned 64‑bit integer from a VLQ buffer.
  ! -----------------------------------------------------------------
  subroutine vlq_get(buf, pos, u, ok)
    integer(kind=int8),  intent(in)    :: buf(:)
    integer(kind=int64), intent(inout) :: pos   ! 1‑based index
    integer(kind=int64), intent(out)   :: u
    logical,            intent(out)   :: ok

    integer(kind=int64) :: shift, byte
    u = 0_int64
    shift = 0_int64
    ok = .false.

    do while (pos <= size(buf, kind=int64))
       byte = int(buf(pos), int64)
       pos = pos + 1
       u = ior(u, iand(byte, int(z'7F',int64))  << shift)
       if (iand(byte, int(z'80',int64)) == 0_int64) then
          ok = .true.
          exit
       end if
       shift = shift + 7_int64
    end do
  end subroutine vlq_get

  ! -----------------------------------------------------------------
  !  Helper to grow the byte buffer – doubles the allocation each time.
  ! -----------------------------------------------------------------
  subroutine enlarge(buf, needed)
    integer(kind=int8), allocatable, intent(inout) :: buf(:)
    integer(kind=int64),          intent(in)       :: needed
    integer(kind=int64)                           :: newsize
    integer(kind=int8), allocatable                :: tmp(:)

    newsize = max(needed, max(1024_int64, size(buf, kind=int64)*2))
    allocate(tmp(newsize))
    if (allocated(buf)) then
       tmp(1:size(buf,kind=int64)) = buf
       deallocate(buf)
    end if
    buf = tmp
  end subroutine enlarge

  !=================================================================
  !  PUBLIC INTERFACE
  !=================================================================
  ! -----------------------------------------------------------------
  !  compress_boxes – turn an already scan‑line sorted array of boxes
  !  into a tiny byte stream.
  !
  !  The layout of the stream is:
  !    *VLQ*  number_of_boxes (unsigned)
  !    for each box
  !       *VLQ*  zz_encode(dx1)   (dx = current - previous)
  !       *VLQ*  zz_encode(dy1)
  !       *VLQ*  zz_encode(dx2)
  !       *VLQ*  zz_encode(dy2)
  !
  !  The first box is stored as an absolute value (the previous coordinate
  !  is taken to be zero).
  ! -----------------------------------------------------------------
  function compress_boxes(boxes) result(stream)
    type(Box), intent(in)          :: boxes(:)
    type(BoxByteStream)            :: stream

    integer(kind=int64)            :: pos, i
    integer(kind=K_COORDINATE_KIND):: prev_x1, prev_y1, prev_x2, prev_y2
    integer(kind=int64)            :: u

    ! -----------------------------------------------------------------
    !  Allocate a small initial buffer – it will be enlarged automatically.
    ! -----------------------------------------------------------------
    allocate(stream%data(1024_int64))
    stream%data = 0_int8
    pos = 1_int64

    ! -----------------------------------------------------------------
    !  Number of boxes (unsigned VLQ)
    ! -----------------------------------------------------------------
    call vlq_put(int(size(boxes, kind=int64), int64), stream%data, pos)

    prev_x1 = 0_K_COORDINATE_KIND
    prev_y1 = 0_K_COORDINATE_KIND
    prev_x2 = 0_K_COORDINATE_KIND
    prev_y2 = 0_K_COORDINATE_KIND

    do i = 1_int64, size(boxes, kind=int64)
       ! ---- delta ----------------------------------------------------
       u = zz_encode( int(boxes(i)%x1, int64) - int(prev_x1, int64) )
       call vlq_put(u, stream%data, pos)
       u = zz_encode( int(boxes(i)%y1, int64) - int(prev_y1, int64) )
       call vlq_put(u, stream%data, pos)
       u = zz_encode( int(boxes(i)%x2, int64) - int(prev_x2, int64) )
       call vlq_put(u, stream%data, pos)
       u = zz_encode( int(boxes(i)%y2, int64) - int(prev_y2, int64) )
       call vlq_put(u, stream%data, pos)

       ! ---- store current coordinates as the new “previous” ----------
       prev_x1 = boxes(i)%x1
       prev_y1 = boxes(i)%y1
       prev_x2 = boxes(i)%x2
       prev_y2 = boxes(i)%y2
    end do

    ! -----------------------------------------------------------------
    !  Shrink the allocation to the exact size that was used.
    ! -----------------------------------------------------------------
    if (pos-1_int64 < size(stream%data, kind=int64)) then
       stream%data = stream%data(1_int64:pos-1_int64)
    end if
  end function compress_boxes

  !=================================================================
  !  decompress_box_stream – generator that yields one Box at a time.
  !  The routine is written as a *pure* function returning a derived‑type
  !  iterator; the calling code simply does
  !
  !        call get_next_box(stream, pos, box, ok)
  !
  !  where `pos` is an integer( int64 ) that the caller must keep.
  !=================================================================
  subroutine decompress_box_stream(stream, pos, box, ok)
    type(BoxByteStream), intent(in)   :: stream
    integer(kind=int64), intent(inout):: pos   ! current read pointer (1‑based)
    type(Box),           intent(out)  :: box
    logical,             intent(out)  :: ok

    integer(kind=int64) :: u
    logical            :: read_ok
    integer(kind=K_COORDINATE_KIND) :: dx1, dy1, dx2, dy2
    integer(kind=K_COORDINATE_KIND) :: prev_x1, prev_y1, prev_x2, prev_y2
    integer(kind=int64) :: i

    save :: prev_x1, prev_y1, prev_x2, prev_y2
    ! -----------------------------------------------------------------
    !  The very first call must read the number of boxes; we do it lazily
    !  the first time this routine is invoked.
    ! -----------------------------------------------------------------
    if (.not. associated(prev_x1)) then
       prev_x1 = 0_K_COORDINATE_KIND
       prev_y1 = 0_K_COORDINATE_KIND
       prev_x2 = 0_K_COORDINATE_KIND
       prev_y2 = 0_K_COORDINATE_KIND
    end if

    ! -------------------------------------------------------------
    !  Decode the four signed deltas
    ! -------------------------------------------------------------
    call vlq_get(stream%data, pos, u, read_ok); if (.not.read_ok) then; ok=.false.; return; end if
    dx1 = zz_decode(u)
    call vlq_get(stream%data, pos, u, read_ok); if (.not.read_ok) then; ok=.false.; return; end if
    dy1 = zz_decode(u)
    call vlq_get(stream%data, pos, u, read_ok); if (.not.read_ok) then; ok=.false.; return; end if
    dx2 = zz_decode(u)
    call vlq_get(stream%data, pos, u, read_ok); if (.not.read_ok) then; ok=.false.; return; end if
    dy2 = zz_decode(u)

    box%x1 = prev_x1 + dx1
    box%y1 = prev_y1 + dy1
    box%x2 = prev_x2 + dx2
    box%y2 = prev_y2 + dy2

    prev_x1 = box%x1
    prev_y1 = box%y1
    prev_x2 = box%x2
    prev_y2 = box%y2
    ok = .true.
  end subroutine decompress_box_stream

end module compress_mod

!=====================================================================
!  box_mod.f90
!=====================================================================
module box_mod
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
  implicit none
  private

  ! -----------------------------------------------------------------
  !  Kind for the integer coordinates – you can change this in one place.
  ! -----------------------------------------------------------------
  integer, parameter, public :: K_COORDINATE_KIND = int32

  ! -----------------------------------------------------------------
  !  The simple rectangle type you already use.
  ! -----------------------------------------------------------------
  type, public :: Box
     integer(kind=K_COORDINATE_KIND) :: x1, y1, x2, y2
  end type Box

  ! -----------------------------------------------------------------
  !  Event that the sweep line works with.
  ! -----------------------------------------------------------------
  type, public :: Event
     integer(kind=K_COORDINATE_KIND) :: x, y1, y2
     integer(kind=int64)            :: lap_change   ! +1 or –1
  end type Event

  ! -----------------------------------------------------------------
  !  A *byte stream* that holds the compressed data.  It is an
  !  allocatable array of 8‑bit integers (i.e. raw bytes).
  ! -----------------------------------------------------------------
  type, public :: BoxByteStream
     integer(kind=int8), allocatable :: data(:)   ! the raw byte buffer
   contains
     procedure, public :: size_bytes => stream_size
  end type BoxByteStream

contains

  pure function stream_size(this) result(nbytes)
    class(BoxByteStream), intent(in) :: this
    integer                         :: nbytes
    nbytes = size(this%data, kind=int64)
  end function stream_size

end module box_mod

!=====================================================================
!  scanline_mod.f90
!=====================================================================
module scanline_mod
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
  use box_mod,      only: Box, Event, K_COORDINATE_KIND
  use compress_mod, only: BoxByteStream, decompress_box_stream
  implicit none
  private
  public :: calculate_union_area_stream

  ! -----------------------------------------------------------------
  !  Simple (but reasonably fast) quick‑sort for the event array.
  !  You can replace it with any library sort you like.
  ! -----------------------------------------------------------------
  recursive subroutine sort_events(ev)
    type(Event), intent(inout) :: ev(:)
    integer                    :: n
    n = size(ev)
    if (n <= 1) return
    call quick_sort(ev, 1, n)
  contains
    recursive subroutine quick_sort(a, lo, hi)
      type(Event), intent(inout) :: a(:)
      integer,    intent(in)    :: lo, hi
      integer                    :: i, j
      type(Event)                :: pivot, tmp

      i = lo
      j = hi
      pivot = a( (lo+hi)/2 )
      do
         do while (a(i)%x < pivot%x); i = i+1; end do
            do while (a(j)%x > pivot%x); j = j-1; end do
               if (i <= j) then
                  tmp = a(i); a(i) = a(j); a(j) = tmp
                  i = i+1; j = j-1
               end if
               if (i > j) exit
            end do
            if (lo < j) call quick_sort(a, lo, j)
            if (i < hi) call quick_sort(a, i, hi)
          end subroutine quick_sort
        end subroutine sort_events

        !=================================================================
        !  PUBLIC:  calculate_union_area_stream
        !
        !  INPUT:  a BoxByteStream that contains the compressed boxes.
        !  OUTPUT: the union area as a REAL(REAL64)
        !=================================================================
        function calculate_union_area_stream(stream) result(area)
          type(BoxByteStream), intent(in) :: stream
          real(real64)                    :: area

          integer(kind=int64)          :: nboxes, i, ev_idx, pos
          type(Event), allocatable     :: events(:)
          type(SkipList), target       :: sl                ! defined elsewhere
          integer(kind=int64)          :: current_covered
          integer(kind=K_COORDINATE_KIND) :: current_x, dx

          ! -----------------------------------------------------------------
          !  1️⃣  Decode the *number of boxes* (first VLQ in the stream)
          ! -----------------------------------------------------------------
          pos = 1_int64
          call vlq_get(stream%data, pos, nboxes, i)   ! i is the dummy OK flag
          if (nboxes == 0_int64) then
             area = 0.0_real64
             return
          end if

          ! -----------------------------------------------------------------
          !  2️⃣  Decode every box and build the event list (2 × n boxes)
          ! -----------------------------------------------------------------
          allocate(events(2*nboxes))
          do i = 1_int64, nboxes
             type(Box) :: b
             logical   :: ok

             call decompress_box_stream(stream, pos, b, ok)
             if (.not.ok) stop 'Corrupt compressed stream – unexpected EOF'

             ! Left edge (lap +1)
             events(2*i-1)%x          = min(b%x1, b%x2)
             events(2*i-1)%y1         = min(b%y1, b%y2)
             events(2*i-1)%y2         = max(b%y1, b%y2)
             events(2*i-1)%lap_change = 1_int64

             ! Right edge (lap –1)
             events(2*i)%x            = max(b%x1, b%x2)
             events(2*i)%y1           = min(b%y1, b%y2)
             events(2*i)%y2           = max(b%y1, b%y2)
             events(2*i)%lap_change   = -1_int64
          end do

          ! -----------------------------------------------------------------
          !  3️⃣  Sort events by X coordinate
          ! -----------------------------------------------------------------
          call sort_events(events)

          ! -----------------------------------------------------------------
          !  4️⃣  Initialise the SkipList (your own implementation – the same as
          !      in the original code)
          ! -----------------------------------------------------------------
          call sl_init(sl, 2*nboxes)   ! max distinct Y nodes = 2·N

          ! -----------------------------------------------------------------
          !  5️⃣  Sweep line
          ! -----------------------------------------------------------------
          current_x = events(1)%x
          ev_idx    = 1
          area      = 0.0_real64

          do while (ev_idx <= 2*nboxes)
             dx = events(ev_idx)%x - current_x
             if (dx > 0) then
                call sl_get_covered_y(sl, current_covered)
                area = area + real(dx, real64) * real(current_covered, real64)
                current_x = events(ev_idx)%x
             end if

             ! Process *all* events that share this X coordinate
             do while (ev_idx <= 2*nboxes .and. events(ev_idx)%x == current_x)
                call sl_add_delta(sl, events(ev_idx)%y1, events(ev_idx)%lap_change)
                call sl_add_delta(sl, events(ev_idx)%y2, -events(ev_idx)%lap_change)
                ev_idx = ev_idx + 1
             end do
          end do

          call sl_destroy(sl)
        end function calculate_union_area_stream

      end module scanline_mod

      !=====================================================================
      !  driver.f90
      !=====================================================================
      program demo_union_area
        use, intrinsic :: iso_fortran_env, only: int64, real64
        use box_mod,      only: Box, BoxByteStream
        use compress_mod, only: compress_boxes
        use scanline_mod, only: calculate_union_area_stream
        implicit none

        integer, parameter :: n = 1_000_000      ! try a million boxes first
        type(Box), allocatable :: boxes(:)
        type(BoxByteStream)    :: stream
        real(real64)           :: area

        ! -----------------------------------------------------------------
        !  1️⃣  Create a *synthetic* scan‑line ordered set of boxes.
        !      (In a real program you would read them from a file.)
        ! -----------------------------------------------------------------
        allocate(boxes(n))
        call generate_test_boxes(boxes)

        ! -----------------------------------------------------------------
        !  2️⃣  Compress the array – the byte stream should be a few MB,
        !      not many GB.
        ! -----------------------------------------------------------------
        stream = compress_boxes(boxes)
        print *, 'Original memory  :', real(8_int64*size(boxes,kind=int64))/1e9, 'GB'
        print *, 'Compressed bytes :', stream%size_bytes()/1e6, 'MB'
        print *, 'Compression ratio:', &
             (8.0_real64*size(boxes,kind=int64)) / real(stream%size_bytes(), real64)

        ! -----------------------------------------------------------------
        !  3️⃣  Compute the union area directly from the compressed stream.
        ! -----------------------------------------------------------------
        area = calculate_union_area_stream(stream)
        print *, 'Union area = ', area

      contains

        subroutine generate_test_boxes(b)
          type(Box), intent(out) :: b(:)
          integer                :: i
          integer(kind=K_COORDINATE_KIND) :: x, y, w, h

          do i = 1, size(b)
             x = i*2                 ! monotone increasing in X → already scan‑line order
             y = mod(i, 1000)*5
             w = 10 + mod(i, 7)
             h = 10 + mod(i, 5)
             b(i)%x1 = x
             b(i)%y1 = y
             b(i)%x2 = x + w
             b(i)%y2 = y + h
          end do
        end subroutine generate_test_boxes

      end program demo_union_area

