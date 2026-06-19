submodule(SystemInformationModule) OpenMPDeviceQuery
  use omp_lib
  implicit none

contains

  !> Reliably detects GPU presence and validates actual execution capability
  module subroutine PrintGPUInfo()
    integer :: NumDevices, DefaultDevice, HostDevice
    logical :: ExecutedOnHost
    integer :: NumTeams, NumThreads

    ! 1. Query the OpenMP Runtime
    NumDevices = omp_get_num_devices()
    DefaultDevice = omp_get_default_device()
    HostDevice = omp_get_initial_device()

    write(*, '(A)') '========================================='
    write(*, '(A)') '       OpenMP Target Device Query        '
    write(*, '(A)') '========================================='
    write(*, '(A, I0)') 'Number of available target devices : ', NumDevices
    write(*, '(A, I0)') 'Default target device ID           : ', DefaultDevice
    write(*, '(A, I0)') 'Host device ID                     : ', HostDevice
    write(*, '(A)') '-----------------------------------------'

    if (NumDevices > 0) then
       write(*, '(A)') 'Initiating kernel execution test...'

       ! Assume failure until proven otherwise by the target device
       ExecutedOnHost = .true.
       NumTeams = 0
       NumThreads = 0

       ! 2. The Execution Verification Payload
       !$omp target map(from: ExecutedOnHost, NumTeams, NumThreads)

       ! If this runs on the GPU, omp_is_initial_device() returns .false.
       ExecutedOnHost = omp_is_initial_device()

       ! Query the execution topology provided by the runtime
       NumTeams = omp_get_num_teams()
       NumThreads = omp_get_num_threads()

       !$omp end target

       ! Evaluate execution telemetry
       if (ExecutedOnHost) then
          write(*, '(A)') ' [WARNING] SILENT FALLBACK DETECTED!'
          write(*, '(A)') ' The runtime detected a device, but the kernel executed on the CPU.'
          write(*, '(A)') ' Check compiler flags (e.g., -mp=gpu for nvfortran).'
       else
          write(*, '(A)') ' [SUCCESS] Kernel successfully executed on the target GPU.'
          write(*, '(A, I0)') ' Default Teams dispatched       : ', NumTeams
          write(*, '(A, I0)') ' Default Threads per Team       : ', NumThreads
       end if
    else
       write(*, '(A)') ' [INFO] No target devices detected by the OpenMP runtime.'
    end if

    write(*, '(A)') '========================================='
  end subroutine PrintGPUInfo

end submodule OpenMPDeviceQuery
