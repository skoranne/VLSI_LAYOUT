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
   integer(kind=K_COORDINATE_KIND):: PRECISION
   public:: InitPrecision, GetPrecision, K_COORDINATE_KIND, K_LEAF_CAPACITY, K_SQUARE_DOMINATION_THRESHOLD, XYTracker,&
      TrackerCell

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

