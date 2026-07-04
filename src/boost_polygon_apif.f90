! File   : boost_polygon_api.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Use Boost Polygon library in Fortran

module BoostPolygonAPIModule
  use CommonModule
  use GeometryModule
  use iso_fortran_env, only: int32, int64
  use, intrinsic :: iso_c_binding
  implicit none
  public:: MergeBoxesUsingBoostPolygon, OperateBoxesUsingBoostPolygon
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

     subroutine PerformBoostPolygonOperation(input_A, AN, input_B, BN, output, outN, control_parameter, control_value) bind(c, name="PerformBoostPolygonOperation")
       import :: c_long, Box
       implicit none
       ! Dimension(*) is used for C-array compatibility (assumed-size)
       type(Box), dimension(*), intent(in) :: input_A, input_B
       ! Pass by value to match 'unsigned long AN'
       integer(c_long), value, intent(in) :: AN, BN, control_parameter, control_value
       ! Pre-allocated output array
       type(Box), dimension(*), intent(out) :: output
       ! Pass by reference (pointer in C) for the output count
       integer(c_long), intent(out) :: outN
     end subroutine PerformBoostPolygonOperation
     
  end interface

contains

  subroutine OperateBoxesUsingBoostPolygon(input_boxes_A, input_boxes_B, merged_boxes, control_parameter, control_value)
    type(Box), dimension(:), intent(in) :: input_boxes_A, input_boxes_B
    type(Box), dimension(:), allocatable, intent(out) :: merged_boxes
    integer(c_long), intent(in) :: control_parameter, control_value
    integer(c_long) :: AN, BN, outN, guessed_output_size
    integer :: alloc_status
    type(Box), dimension(:), allocatable :: temp_output
    type(Box) :: bboxA
    AN = size(input_boxes_A, kind=c_long)
    BN = size(input_boxes_B, kind=c_long)
    if ( (AN == 0) .and. (BN  == 0 ) ) then
       allocate(merged_boxes(0))
       return
    end if
    bboxA = mbr_of_array( input_boxes_A, AN )
    !> how to guess the output size, A * B could be O(n^2)
    if( control_parameter == K_BOOST_CONTROL_AND ) then
       write(*,*) 'INFO: this is not supported.'
       guessed_output_size = 10*max(AN,BN) !> this is insufficient
    else
       guessed_output_size = 4*max(AN,BN)
    end if
    !> we have a problem here since due to INTRODUCED fracturing we can create
    !> unbounded (as a function of AN/BN) number of output boxes
    !> K_MAXIMUM_WIDTH is set to 100,000
    guessed_output_size = guessed_output_size*(1+(bboxA%x2-bboxA%x1)/100000)
    !write(*,*) 'Performing BOOST operation: ', control_parameter, ' on ', AN, ' ', BN, ' guessed: ', guessed_output_size    
    allocate( temp_output( guessed_output_size), stat=alloc_status ) !> this could be optimistic, use 2*N
    if( alloc_status /= 0 ) then
       write(*,*) 'ERROR: Requested memory: ', guessed_output_size, ' from ', AN, ' ', BN
       error stop "ERROR: MEMORY ALLOCATION FAILED: "
    end if
    ! Call the C/C++ backend
    call PerformBoostPolygonOperation(input_boxes_A, AN, input_boxes_B, BN, temp_output, outN, control_parameter, control_value)
    ! Allocate final output to exact size and copy
    allocate(merged_boxes(outN))
    merged_boxes(1:outN) = temp_output(1:outN)

  end subroutine OperateBoxesUsingBoostPolygon

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
