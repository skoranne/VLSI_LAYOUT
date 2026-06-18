! File   : commonmodule.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Collect all named constants, literals in one place
module CommonModule
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
  implicit none
  public
  integer(kind=int64), parameter :: K_LEAF_CAPACITY = 16 !> 32 is better than 64  
  integer,  parameter            :: K_COORDINATE_KIND = int32
  real(kind=real64), parameter   :: K_SQUARE_DOMINATION_THRESHOLD = 0.8

  type :: XYTracker
     integer(kind=K_COORDINATE_KIND) :: X, Y
     integer(kind=int8) :: polygonNumber 
     ! the winding number of the vertex is the sign of polygonNumber
  end type XYTracker
  
end module CommonModule

