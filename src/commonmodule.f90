! File   : commonmodule.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Collect all named constants, literals in one place
module CommonModule
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64, output_unit
  implicit none
  private
  integer(kind=int64), parameter :: K_LEAF_CAPACITY = 16 !> 32 is better than 64
  integer,  parameter            :: K_COORDINATE_KIND = int32
  real(kind=real64), parameter   :: K_SQUARE_DOMINATION_THRESHOLD = 0.8
  real(kind=real64), parameter   :: K_SMALL_EPSILON = 1.0e-12_real64
  integer(kind=int64), parameter :: K_BOOST_CONTROL_XOR = 0, K_BOOST_CONTROL_OR = 1, &
       K_BOOST_CONTROL_AND = 2, K_BOOST_CONTROL_NOT = 3,&
       K_BOOST_CONTROL_MERGE = 4, K_BOOST_CONTROL_SIZE = 5
  integer(kind=int64), parameter :: K_TOPO_INTERACT = 101, K_TOPO_NOT_INTERACT = 102, &
       K_TOPO_CUT = 103, K_TOPO_NOT_CUT = 104, &
       K_TOPO_TOUCH = 105, K_TOPO_NOT_TOUCH = 106, &
       K_TOPO_INSIDE = 107, K_TOPO_NOT_INSIDE = 108, &
       K_TOPO_OUTSIDE = 109, K_TOPO_NOT_OUTSIDE = 110
  integer, parameter             :: K_TEMPORARY_LAYER_MEMORY = 1, K_TEMPORARY_LAYER_DISK = 2
  integer(kind=K_COORDINATE_KIND):: PRECISION = 1
  integer                        :: debug_verbosity = 0
  integer                        :: abort_on_xor = 0
  integer                        :: abort_on_assert_zero = 0
  logical                        :: profiling_enabled = .false.
  integer                        :: temporary_layers = K_TEMPORARY_LAYER_MEMORY
  integer                        :: K_COMPRESSION_METHOD_TO_USE = 1 !> ZLIB
  integer                        :: CODEC_VERSION = 1
  integer(kind=int64), parameter  :: K_FRAME_THRESHOLD = 1024 !> does not make sense to send this to FRAME logic

  !> Structured runtime configuration container for new/refactored callers
  type :: RuntimeConfig
     integer(kind=K_COORDINATE_KIND) :: precision = 1
     integer                         :: debug_verbosity = 0
     integer                         :: abort_on_xor = 0
     integer                         :: abort_on_assert_zero = 0
     integer                         :: temporary_layers = K_TEMPORARY_LAYER_MEMORY
     integer                         :: compression_method = 1
     integer                         :: codec_version = 1
     logical                         :: profiling_enabled = .true.
  end type RuntimeConfig

  public:: InitPrecision, GetPrecision, K_COORDINATE_KIND, K_LEAF_CAPACITY, K_SQUARE_DOMINATION_THRESHOLD, XYTracker,&
       TrackerCell, K_SMALL_EPSILON, K_BOOST_CONTROL_XOR, K_BOOST_CONTROL_OR, K_BOOST_CONTROL_AND,&
       K_BOOST_CONTROL_NOT, K_BOOST_CONTROL_MERGE, K_BOOST_CONTROL_SIZE, debug_verbosity, abort_on_xor,&
       abort_on_assert_zero, temporary_layers, K_COMPRESSION_METHOD_TO_USE, K_TEMPORARY_LAYER_MEMORY, &
       K_TEMPORARY_LAYER_DISK, beep, CODEC_VERSION, K_FRAME_THRESHOLD, profiling_enabled,&
       K_TOPO_INTERACT, K_TOPO_NOT_INTERACT, K_TOPO_CUT, K_TOPO_NOT_CUT, &
       K_TOPO_TOUCH, K_TOPO_NOT_TOUCH, K_TOPO_INSIDE, K_TOPO_NOT_INSIDE, &
       K_TOPO_OUTSIDE, K_TOPO_NOT_OUTSIDE, RuntimeConfig

  type :: XYTracker
     integer(kind=K_COORDINATE_KIND) :: X = 0, Y = 0
     integer(kind=int8) :: polygonNumber = 0_int8
     ! the winding number of the vertex is the sign of polygonNumber
  end type XYTracker
  type :: TrackerCell
     type(XYTracker), allocatable :: trackers(:)
     integer :: count = 0 ! Track active items
  end type TrackerCell
contains
  subroutine InitPrecision(p)
    integer(kind=K_COORDINATE_KIND), intent(in) :: p
    PRECISION = p
  end subroutine InitPrecision
  pure function GetPrecision() result(p)
    integer(kind=K_COORDINATE_KIND) :: p
    p = PRECISION
  end function GetPrecision
  subroutine beep()
    ! The 'advance="no"' option prevents the automatic newline
    write(*, '(A)', advance='no') achar(7)
    ! Optional: Flush the output to ensure the system processes the char immediately
    flush(output_unit)
  end subroutine beep
end module CommonModule

