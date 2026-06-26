! File   : boost_polygon_api.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Use Boost Polygon library in Fortran

module BoostPolygonAPIModule
  use CommonModule
  use GeometryModule
  use iso_fortran_env, only: int32, int64
  use, intrinsic :: iso_c_binding
  implicit none
  public:: MergeBoxesUsingBoostPolygon
  interface
     ! Bind to the C function
     subroutine PerformBoostPolygonMerge(input, N, output, outN) bind(c, name="PerformBoostPolygonMerge")
       import :: c_long, Box
       implicit none

       ! Dimension(*) is used for C-array compatibility (assumed-size)
       type(Box), dimension(*), intent(in) :: input

       ! Pass by value to match 'unsigned long N'
       integer(c_long), value, intent(in) :: N

       ! Pre-allocated output array
       type(Box), dimension(*), intent(out) :: output

       ! Pass by reference (pointer in C) for the output count
       integer(c_long), intent(out) :: outN
     end subroutine PerformBoostPolygonMerge
  end interface

contains

  ! Optional: A convenient Fortran wrapper to handle the memory 
  ! allocation automatically so your main code stays clean.
  subroutine MergeBoxesUsingBoostPolygon(input_boxes, merged_boxes)
    type(Box), dimension(:), intent(in) :: input_boxes
    type(Box), dimension(:), allocatable, intent(out) :: merged_boxes

    integer(c_long) :: N, outN
    type(Box), dimension(:), allocatable :: temp_output

    N = size(input_boxes, kind=c_long)
    if (N == 0) then
       allocate(merged_boxes(0))
       return
    end if

    ! Allocate temp output to max possible size (N)
    allocate(temp_output(2*N)) !> this could be optimistic, use 2*N

    ! Call the C/C++ backend
    call PerformBoostPolygonMerge(input_boxes, N, temp_output, outN)

    ! Allocate final output to exact size and copy
    allocate(merged_boxes(outN))
    merged_boxes(1:outN) = temp_output(1:outN)

  end subroutine MergeBoxesUsingBoostPolygon

end module BoostPolygonAPIModule
