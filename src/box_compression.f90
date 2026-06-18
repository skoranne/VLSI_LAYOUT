! File   : box_compression.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: In the ScanLine module and heal_boxes function
!        : we dont actually need random access. 
!=====================================================================
!  box_mod.f90
!=====================================================================

!=====================================================================
!  compress_mod.f90
!=====================================================================
module BoxCompressionModule
  use CommonModule
  use GeometryModule
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
  use BoxByteStreamModule
  implicit none
  private
  public :: compress_boxes, decompress_box_stream, get_number_boxes
contains
  ! -----------------------------------------------------------------
  !  Private helper: Zig‑Zag encode a signed integer so that small
  !  magnitude values become small *unsigned* values.
  ! -----------------------------------------------------------------
  function get_number_boxes( stream, pos, ok, are_we_scanning ) result(retval)
    type(BoxByteStream), intent(in) :: stream
    integer(kind=int64), intent(inout) :: pos
    integer(kind=int64) :: retval
    logical, intent(inout) :: ok
    logical, intent(in)    :: are_we_scanning
    pos = 1_int64
    call vlq_get( stream%data, pos, retval, ok )
    if( .not. are_we_scanning ) then
       pos = 1_int64
    end if
  end function get_number_boxes
  
  pure function zz_encode(i) result(u)
    integer(kind=K_COORDINATE_KIND), intent(in) :: i
    integer(kind=int64)                         :: u
    !u = i
    u = shiftl(int(i, int64), 1)
    if (i < 0) u = not(u)
  end function zz_encode

  pure function zz_decode(u) result(i)
    integer(kind=int64), intent(in) :: u
    integer(kind=K_COORDINATE_KIND) :: i
    integer(kind=int64)             :: tmp
    tmp = shiftr(u, 1_int64)
    if( iand(u, 1_int64) == 1_int64) then
       tmp = ieor( tmp, -1_int64)
    end if
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
    integer(kind=int64) :: v, more, incoming_position
    integer(kind=int8)  :: byte
    incoming_position = pos
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
    !write(*,*) 'To write: ', u, ' it took ', (pos-incoming_position), ' bytes.'
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
       u = ior(u, shiftl(iand(byte, int(z'7F',int64)), shift))
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
    integer :: istat
    newsize = max(needed, max(1024_int64, int(size(buf, kind=int64)*1.25), int64))
    write(*,*) 'Enlarging buffer: ', newsize
    allocate(tmp(newsize),stat=istat)
    if( istat /= 0 ) then
       write(*,*) 'Allocation failed: ', newsize, ' istat = ', istat
    end if
    
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
  !  This simple VLQ compression is giving us around 7.5 bytes per box which is
  !  much better than the 16-bytes and if we move to int64 coordinates then
  !  it will be much higher.
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
    allocate(stream%data(6*size(boxes,kind=int64)))
    stream%data = 0_int8
    pos = 1_int64

    ! -----------------------------------------------------------------
    !  Number of boxes (unsigned VLQ)
    ! -----------------------------------------------------------------
    call vlq_put(int(size(boxes, kind=K_COORDINATE_KIND), int64), stream%data, pos)

    prev_x1 = 0_K_COORDINATE_KIND
    prev_y1 = 0_K_COORDINATE_KIND
    prev_x2 = 0_K_COORDINATE_KIND
    prev_y2 = 0_K_COORDINATE_KIND

    do i = 1_int64, size(boxes, kind=int64)
       ! ---- delta ----------------------------------------------------
       u = zz_encode( int(boxes(i)%x1, K_COORDINATE_KIND) - int(prev_x1, K_COORDINATE_KIND) )
       call vlq_put(u, stream%data, pos)
       u = zz_encode( int(boxes(i)%y1, K_COORDINATE_KIND) - int(prev_y1, K_COORDINATE_KIND) )
       call vlq_put(u, stream%data, pos)
       u = zz_encode( int(boxes(i)%x2, K_COORDINATE_KIND) - int(prev_x2, K_COORDINATE_KIND) )
       call vlq_put(u, stream%data, pos)
       u = zz_encode( int(boxes(i)%y2, K_COORDINATE_KIND) - int(prev_y2, K_COORDINATE_KIND) )
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
    !if (pos < size(stream%data, kind=int64)) then
    !   stream%data = stream%data(1_int64:pos-1_int64)
    !end if
  end function compress_boxes

  function compress_boxes_infobyte(boxes) result(stream)
    type(Box), intent(in)          :: boxes(:)
    type(BoxByteStream)            :: stream

    integer(kind=int64)            :: pos, i
    integer(kind=K_COORDINATE_KIND):: prev_x1, prev_y1, prev_x2, prev_y2
    integer(kind=int64)            :: u
    ! -----------------------------------------------------------------
    !  Allocate a small initial buffer – it will be enlarged automatically.
    ! -----------------------------------------------------------------
    allocate(stream%data(6*size(boxes,kind=int64)))
    stream%data = 0_int8
    pos = 1_int64

    ! -----------------------------------------------------------------
    !  Number of boxes (unsigned VLQ)
    ! -----------------------------------------------------------------
    call vlq_put(int(size(boxes, kind=K_COORDINATE_KIND), int64), stream%data, pos)

    prev_x1 = 0_K_COORDINATE_KIND
    prev_y1 = 0_K_COORDINATE_KIND
    prev_x2 = 0_K_COORDINATE_KIND
    prev_y2 = 0_K_COORDINATE_KIND

    do i = 1_int64, size(boxes, kind=int64)
       ! ---- delta ----------------------------------------------------
       u = zz_encode( int(boxes(i)%x1, K_COORDINATE_KIND) - int(prev_x1, K_COORDINATE_KIND) )
       call vlq_put(u, stream%data, pos)
       u = zz_encode( int(boxes(i)%y1, K_COORDINATE_KIND) - int(prev_y1, K_COORDINATE_KIND) )
       call vlq_put(u, stream%data, pos)
       u = zz_encode( int(boxes(i)%x2, K_COORDINATE_KIND) - int(prev_x2, K_COORDINATE_KIND) )
       call vlq_put(u, stream%data, pos)
       u = zz_encode( int(boxes(i)%y2, K_COORDINATE_KIND) - int(prev_y2, K_COORDINATE_KIND) )
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
    !if (pos < size(stream%data, kind=int64)) then
    !   stream%data = stream%data(1_int64:pos-1_int64)
    !end if
  end function compress_boxes_infobyte
  
  !=================================================================
  !  decompress_box_stream – generator that yields one Box at a time.
  !  The routine is written as a *pure* function returning a derived‑type
  !  iterator; the calling code simply does
  !
  !        call get_next_box(stream, pos, box, ok)
  !
  !  where `pos` is an integer( int64 ) that the caller must keep.
  !=================================================================
  subroutine decompress_box_stream(stream, pos, obox, ok)
    type(BoxByteStream), intent(inout)   :: stream
    integer(kind=int64), intent(inout):: pos   ! current read pointer (1‑based)
    type(Box),           intent(out)  :: obox
    logical,             intent(out)  :: ok

    integer(kind=int64) :: u
    logical            :: read_ok
    integer(kind=int64) :: i, num_boxes
    if( pos == 1 ) then
       num_boxes = get_number_boxes( stream, pos, ok, .true. ) !> scanning now
       call InitializeCodec( stream )
    end if

    ! -------------------------------------------------------------
    !  Decode the four signed deltas
    ! -------------------------------------------------------------
    call vlq_get(stream%data, pos, u, read_ok); if (.not.read_ok) then; ok=.false.; return; end if
    stream%codec%dx1 = zz_decode(u)
    call vlq_get(stream%data, pos, u, read_ok); if (.not.read_ok) then; ok=.false.; return; end if
    stream%codec%dy1 = zz_decode(u)
    call vlq_get(stream%data, pos, u, read_ok); if (.not.read_ok) then; ok=.false.; return; end if
    stream%codec%dx2 = zz_decode(u)
    call vlq_get(stream%data, pos, u, read_ok); if (.not.read_ok) then; ok=.false.; return; end if
    stream%codec%dy2 = zz_decode(u)

    obox%x1 = stream%codec%prev_x1 + stream%codec%dx1
    obox%y1 = stream%codec%prev_y1 + stream%codec%dy1
    obox%x2 = stream%codec%prev_x2 + stream%codec%dx2
    obox%y2 = stream%codec%prev_y2 + stream%codec%dy2

    stream%codec%prev_x1 = obox%x1
    stream%codec%prev_y1 = obox%y1
    stream%codec%prev_x2 = obox%x2
    stream%codec%prev_y2 = obox%y2
    ok = .true.
  end subroutine decompress_box_stream

end module BoxCompressionModule
