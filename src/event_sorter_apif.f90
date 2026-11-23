! File   : event_sorter_apif.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: Fortran API wrapper for Event sorter

module EventTrackerSortModule
  use iso_c_binding
  use omp_lib
  use CommonModule
  use GeometryModule
  use DeviceRegistryModule, only: DevicePrepare
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
    call DevicePrepare(c_loc(arr(1)), int(n, c_size_t) * int(storage_size(arr(1))/8, c_size_t))

    ! Map the data to the device and pass the device address to C++/Thrust. When no device is
    ! available (or on CPU-only builds) the if() clause makes use_device_addr yield the host
    ! address; device_event_sort() checks the pointer kind and stages host memory itself.
    !$omp target data map(tofrom: arr) use_device_addr(arr) if(omp_get_num_devices() > 0)
    call device_event_sort(c_loc(arr(1)), n)
    !$omp end target data

  end subroutine sort_event_trackers
end module EventTrackerSortModule
