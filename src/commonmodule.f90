! File   : commonmodule.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Collect all named constants, literals in one place
module CommonModule
   use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
   implicit none
   private
   integer(kind=int64), parameter :: K_LEAF_CAPACITY = 16 !> 32 is better than 64
   integer,  parameter            :: K_COORDINATE_KIND = int32
   real(kind=real64), parameter   :: K_SQUARE_DOMINATION_THRESHOLD = 0.8
   real(kind=real64), parameter   :: K_SMALL_EPSILON = 1.0e-12_real64
   integer(kind=int64), parameter :: K_BOOST_CONTROL_XOR = 0, K_BOOST_CONTROL_OR = 1, &
                                     K_BOOST_CONTROL_AND = 2, K_BOOST_CONTROL_NOT = 3,&
                                     K_BOOST_CONTROL_MERGE = 4, K_BOOST_CONTROL_SIZE = 5
   integer(kind=K_COORDINATE_KIND):: PRECISION
   integer                        :: debug_verbosity = 0
   integer                        :: abort_on_xor = 0
   integer                        :: abort_on_assert_zero = 0
   integer                        :: K_COMPRESSION_METHOD_TO_USE = 1 !> ZLIB
   public:: InitPrecision, GetPrecision, K_COORDINATE_KIND, K_LEAF_CAPACITY, K_SQUARE_DOMINATION_THRESHOLD, XYTracker,&
        TrackerCell, K_SMALL_EPSILON, K_BOOST_CONTROL_XOR, K_BOOST_CONTROL_OR, K_BOOST_CONTROL_AND,&
        K_BOOST_CONTROL_NOT, K_BOOST_CONTROL_MERGE, K_BOOST_CONTROL_SIZE, debug_verbosity, abort_on_xor,&
        abort_on_assert_zero, K_COMPRESSION_METHOD_TO_USE

   type :: XYTracker
      integer(kind=K_COORDINATE_KIND) :: X, Y
      integer(kind=int8) :: polygonNumber
      ! the winding number of the vertex is the sign of polygonNumber
   end type XYTracker
   type :: TrackerCell
      type(XYTracker), allocatable :: trackers(:)
      integer :: count ! Optional, but highly recommended to track active items
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
end module CommonModule

