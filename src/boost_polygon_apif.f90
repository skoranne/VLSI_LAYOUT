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
     ! Exact-sized allocating C bindings (no caller pre-allocation or buffer overflow)
     function PerformBoostPolygonMergeAlloc(input, N, outN) result(res_ptr) bind(c, name="PerformBoostPolygonMergeAlloc")
       import :: c_long, c_ptr, Box
       implicit none
#ifdef __GFORTRAN__
       !> gfortran: Box has type-bound procedures, so it is not C interoperable (not even as
       !> type(*)); pass the address of the contiguous box array (4 x int32 per box).
       type(c_ptr), value :: input
#else
       type(Box), dimension(*), intent(in) :: input
#endif
       integer(c_long), value, intent(in) :: N
       integer(c_long), intent(out) :: outN
       type(c_ptr) :: res_ptr
     end function PerformBoostPolygonMergeAlloc

     function PerformBoostPolygonOperationAlloc(input_A, AN, input_B, BN, outN, control_parameter, control_value) &
          result(res_ptr) bind(c, name="PerformBoostPolygonOperationAlloc")
       import :: c_long, c_ptr, Box
       implicit none
#ifdef __GFORTRAN__
       type(c_ptr), value :: input_A, input_B
#else
       type(Box), dimension(*), intent(in) :: input_A, input_B
#endif
       integer(c_long), value, intent(in) :: AN, BN, control_parameter, control_value
       integer(c_long), intent(out) :: outN
       type(c_ptr) :: res_ptr
     end function PerformBoostPolygonOperationAlloc

     subroutine FreeBoostPolygonBuffer(ptr) bind(c, name="FreeBoostPolygonBuffer")
       import :: c_ptr
       implicit none
       type(c_ptr), value, intent(in) :: ptr
     end subroutine FreeBoostPolygonBuffer
  end interface

contains

  subroutine OperateBoxesUsingBoostPolygon(input_boxes_A, input_boxes_B, merged_boxes, control_parameter, control_value)
#ifdef __GFORTRAN__
    type(Box), dimension(:), intent(in), contiguous, target :: input_boxes_A, input_boxes_B
    type(Box), target :: dummy_box(1)
#else
    type(Box), dimension(:), intent(in) :: input_boxes_A, input_boxes_B
    type(Box) :: dummy_box(1)
#endif
    type(Box), dimension(:), allocatable, intent(out) :: merged_boxes
    integer(c_long), intent(in) :: control_parameter, control_value
    integer(c_long) :: AN, BN, outN
    integer :: alloc_status
    type(c_ptr) :: c_out_ptr
    type(Box), pointer :: f_out_ptr(:)

    AN = size(input_boxes_A, kind=c_long)
    BN = size(input_boxes_B, kind=c_long)
    if ( (AN == 0) .and. (BN == 0) ) then
       allocate(merged_boxes(0))
       return
    end if

    outN = 0_c_long
#ifdef __GFORTRAN__
    if (AN > 0 .and. BN > 0) then
       c_out_ptr = PerformBoostPolygonOperationAlloc(c_loc(input_boxes_A), AN, c_loc(input_boxes_B), BN, outN, &
            control_parameter, control_value)
    else if (AN > 0) then
       c_out_ptr = PerformBoostPolygonOperationAlloc(c_loc(input_boxes_A), AN, c_loc(dummy_box), 0_c_long, outN, &
            control_parameter, control_value)
    else
       c_out_ptr = PerformBoostPolygonOperationAlloc(c_loc(dummy_box), 0_c_long, c_loc(input_boxes_B), BN, outN, &
            control_parameter, control_value)
    end if
#else
    if (AN > 0 .and. BN > 0) then
       c_out_ptr = PerformBoostPolygonOperationAlloc(input_boxes_A, AN, input_boxes_B, BN, outN, control_parameter, control_value)
    else if (AN > 0) then
       c_out_ptr = PerformBoostPolygonOperationAlloc(input_boxes_A, AN, dummy_box, 0_c_long, outN, control_parameter, control_value)
    else
       c_out_ptr = PerformBoostPolygonOperationAlloc(dummy_box, 0_c_long, input_boxes_B, BN, outN, control_parameter, control_value)
    end if
#endif

    if (outN == 0) then
       if (c_associated(c_out_ptr)) call FreeBoostPolygonBuffer(c_out_ptr)
       allocate(merged_boxes(0))
       return
    end if
    if (.not. c_associated(c_out_ptr)) then
       write(*,*) 'ERROR: BoostPolygon C++ buffer allocation failed for ', outN, ' boxes from ', AN, ' ', BN
       error stop "ERROR: MEMORY ALLOCATION FAILED: "
    end if

    allocate(merged_boxes(outN), stat=alloc_status)
    if (alloc_status /= 0) then
       call FreeBoostPolygonBuffer(c_out_ptr)
       write(*,*) 'ERROR: Requested memory: ', outN, ' from ', AN, ' ', BN
       error stop "ERROR: MEMORY ALLOCATION FAILED: "
    end if
    call c_f_pointer(c_out_ptr, f_out_ptr, [outN])
    merged_boxes(1:outN) = f_out_ptr(1:outN)
    call FreeBoostPolygonBuffer(c_out_ptr)
  end subroutine OperateBoxesUsingBoostPolygon

  subroutine MergeBoxesUsingBoostPolygon(input_boxes, merged_boxes)
#ifdef __GFORTRAN__
    type(Box), dimension(:), intent(in), contiguous, target :: input_boxes
#else
    type(Box), dimension(:), intent(in) :: input_boxes
#endif
    type(Box), dimension(:), allocatable, intent(out) :: merged_boxes
    integer(c_long) :: N, outN
    integer :: alloc_status
    type(c_ptr) :: c_out_ptr
    type(Box), pointer :: f_out_ptr(:)

    N = size(input_boxes, kind=c_long)
    if (N == 0) then
       allocate(merged_boxes(0))
       return
    end if

    outN = 0_c_long
#ifdef __GFORTRAN__
    c_out_ptr = PerformBoostPolygonMergeAlloc(c_loc(input_boxes), N, outN)
#else
    c_out_ptr = PerformBoostPolygonMergeAlloc(input_boxes, N, outN)
#endif
    if (outN == 0) then
       if (c_associated(c_out_ptr)) call FreeBoostPolygonBuffer(c_out_ptr)
       allocate(merged_boxes(0))
       return
    end if
    if (.not. c_associated(c_out_ptr)) then
       write(*,*) 'ERROR: BoostPolygon C++ merge buffer allocation failed for ', outN, ' boxes'
       error stop "ERROR: MEMORY ALLOCATION FAILED"
    end if

    allocate(merged_boxes(outN), stat=alloc_status)
    if (alloc_status /= 0) then
       call FreeBoostPolygonBuffer(c_out_ptr)
       write(*,*) 'ERROR: Requested memory: ', outN
       error stop "ERROR: MEMORY ALLOCATION FAILED"
    end if
    call c_f_pointer(c_out_ptr, f_out_ptr, [outN])
    merged_boxes(1:outN) = f_out_ptr(1:outN)
    call FreeBoostPolygonBuffer(c_out_ptr)
  end subroutine MergeBoxesUsingBoostPolygon
  
end module BoostPolygonAPIModule
