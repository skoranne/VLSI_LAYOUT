module BoxByteStreamModule
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
  use CommonModule
  implicit none
  private
  public:: InitializeCodec
  ! The generated code uses Fortran save variables which are not re-entrant
  ! and not even callable by 2 different streams, therefore we MUST keep the
  ! state of the encoder/decoder somewhere, and it can be in the stream.
  type, private:: BoxStreamCodecVariables
    integer(kind=K_COORDINATE_KIND) :: dx1, dy1, dx2, dy2
    integer(kind=K_COORDINATE_KIND) :: prev_x1, prev_y1, prev_x2, prev_y2
  end type BoxStreamCodecVariables
  
  ! -----------------------------------------------------------------
  !  A *byte stream* that holds the compressed data.  It is an
  !  allocatable array of 8‑bit integers (i.e. raw bytes).
  ! -----------------------------------------------------------------
  type, public :: BoxByteStream
     integer(kind=int8), allocatable :: data(:)   ! the raw byte buffer
     type(BoxStreamCodecVariables)   :: codec
   contains
     procedure, public :: size_bytes => stream_size
  end type BoxByteStream

contains 
  pure subroutine InitializeCodec(stream)
    class(BoxByteStream), intent(inout) :: stream    
    stream%codec%prev_x1 = 0_K_COORDINATE_KIND
    stream%codec%prev_y1 = 0_K_COORDINATE_KIND
    stream%codec%prev_x2 = 0_K_COORDINATE_KIND
    stream%codec%prev_y2 = 0_K_COORDINATE_KIND
    
  end subroutine InitializeCodec
  
  pure function stream_size(this) result(nbytes)
    class(BoxByteStream), intent(in) :: this
    integer                         :: nbytes
    nbytes = size(this%data, kind=int64)
  end function stream_size

end module BoxByteStreamModule
