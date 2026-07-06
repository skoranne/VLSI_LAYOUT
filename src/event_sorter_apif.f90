! File   : event_sorter_apif.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: Fortran API wrapper for Event sorter

module EventTrackerSortModule
  use iso_c_binding
  use omp_lib
  use CommonModule
  use GeometryModule
  use iso_fortran_env, only: int32, int64
  implicit none

  ! Ensure these match the C++ definitions
  integer, parameter :: K_SMALL_THRESHOLD = 32

  ! Interface to the C++/Thrust function
  interface
     subroutine device_event_sort(d_arr, n) bind(c, name="device_event_sort")
       import :: c_ptr, c_int64_t
       type(c_ptr), value :: d_arr
       integer(c_int64_t), value :: n
     end subroutine device_event_sort
  end interface

contains

  ! =========================================================================
  ! GPU SORT WRAPPER (OpenMP + Thrust)
  ! =========================================================================
  subroutine sort_event_trackers(arr)
    ! 'target' attribute is required for c_loc to work
    type(XYTracker), intent(inout), target :: arr(:)
    integer(int64) :: n

    n = size(arr, kind=int64)
    if (n <= 1) return

    ! Map the data to the device, then pass the raw device pointer to C++
    !$komp target data map(tofrom: arr) use_device_ptr(arr)
    call device_event_sort(c_loc(arr(1)), n)
    !$komp end target data

  end subroutine sort_event_trackers
end module EventTrackerSortModule
