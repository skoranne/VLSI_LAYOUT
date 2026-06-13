! File   : commonmodule.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Collect all named constants, literals in one place
module CommonModule
  use, intrinsic :: iso_fortran_env, only: int32, int64
  implicit none
  public
  integer, parameter  :: K_LEAF_CAPACITY = 16 !> 32 is better than 64  
  integer,  parameter :: K_COORDINATE_KIND = int32
end module CommonModule

