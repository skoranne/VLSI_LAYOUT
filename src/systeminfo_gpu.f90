! File   : systeminfo_gpu.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Print detailed information about GPU, if available
!        : device property code from CUDA Fortran Book
!        : url = git@github.com:NVIDIA/CUDA-Fortran-2ed.git

submodule(SystemInformationModule) OpenMPDeviceQuery
  use omp_lib
#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
  use cudafor
#endif
#if defined(__INTEL_LLVM_COMPILER) || defined(__INTEL_COMPILER)

#endif
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
#ifdef _CUDA
    call PrintDetailedInformationCUDA()
#endif
    write(*, '(A)') '========================================='
  end subroutine PrintGPUInfo
  #if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
  subroutine PrintDetailedInformationCUDA()
    use cudafor
    implicit none

    type (cudaDeviceProp) :: prop
    integer :: nDevices=0, i, ierr
    ! Number of CUDA-capable devices
    ierr = cudaGetDeviceCount(nDevices)
    if (nDevices == 0) then
       print "(/,'No CUDA devices found',/)"
       stop
    else if (nDevices == 1) then
       print "(/,'One CUDA device found',/)"
    else 
       print "(/,i0,' CUDA devices found',/)", nDevices
    end if
    ! Loop over devices (N.B. 0-based enumeration)
    do i = 0, nDevices-1
       print "('Device Number: ',i0)", i
       ierr = cudaGetDeviceProperties(prop, i)
       ! General device info
       print "('  Device Name: ', a)", trim(prop%name)
       print "('  Compute Capability: ',i0,'.',i0)", &
            prop%major, prop%minor
       print "('  Number of Multiprocessors: ',i0)", &
            prop%multiProcessorCount
       print "('  Single- to Double-Precision Perf Ratio: &
            &', i0)", &
            prop%singleToDoublePrecisionPerfRatio
       print "('  Max Threads per Multiprocessor: ',i0)", &
            prop%maxThreadsPerMultiprocessor
       if (prop%cooperativeLaunch == 0) then
          print "('  Supports Cooperative Kernels: No',/)"
       else
          print "('  Supports Cooperative Kernels: Yes',/)"
       end if
       print "('  Global Memory (GB): ',f9.3,/)", &
            prop%totalGlobalMem/1024.0**3
       ! Execution Configuration
       print "('  Execution Configuration Limits')"
       print "('    Max Grid Dims: ',2(i0,' x '),i0)", &
            prop%maxGridSize
       print "('    Max Block Dims: ',2(i0,' x '),i0)", &
            prop%maxThreadsDim
       print "('    Max Threads per Block: ',i0,/)", &
            prop%maxThreadsPerBlock
       ! Has managed memory
       print "('  Managed Memory')"
       if (prop%managedMemory == 0) then
          print "('    Can Allocate Managed Memory: No')"
       else
          print "('    Can Allocate Managed Memory: Yes')"
       endif
       if (prop%concurrentManagedAccess == 0) then
          print "('    Device/CPU Concurrent Access &
               &to Managed Memory: No',/)"
       else
          print "('    Device/CPU Concurrent Access &
               &to Managed Memory: Yes',/)"
       endif
    enddo
  end subroutine PrintDetailedInformationCUDA
#endif

end submodule OpenMPDeviceQuery
