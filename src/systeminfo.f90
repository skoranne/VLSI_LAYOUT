! File   : systeminfo.f90
! Author : Sandeep Koranne
! Purpose: In Modern fortran, print the system information such as time/date, CPU, Linux, and process memory used
module SystemInformationModule
  use, intrinsic :: iso_c_binding, only: c_int64_t, c_int32_t
  implicit none
  private
  ! Date and Time variables
  character(len=8)  :: date_str
  character(len=10) :: time_str
  character(len=5)  :: zone_str
  integer           :: dt_values(8)

  ! Memory parsing variables
  integer            :: file_unit, io_stat
  character(len=32)  :: line
  integer(kind=8)    :: vm_size, vm_rss
  logical            :: opened
  integer(kind=8)    :: full_start_tick
  integer(kind=8)    :: start_tick, end_tick, clock_rate
  real               :: full_start_time, t1, t2
  public:: InitSystem, StartMarkTime, StopMarkTime, PrintFullInformation, ReadProcessMemoryUsage

  interface
     module subroutine PrintGPUInfo()
     end subroutine PrintGPUInfo
  end interface

  interface
     subroutine get_macos_memory(rss_bytes, virtual_bytes) bind(C, name="get_macos_memory")
       import :: c_int64_t
       integer(c_int64_t), intent(out) :: rss_bytes, virtual_bytes
     end subroutine get_macos_memory
     function get_macos_pid() bind(C, name="get_macos_pid") result(pid)
       import :: c_int32_t
       integer(c_int32_t) :: pid
     end function get_macos_pid
     function is_macos_traced() bind(C, name="is_macos_traced") result(traced)
       import :: c_int32_t
       integer(c_int32_t) :: traced
     end function is_macos_traced
  end interface

contains
  subroutine InitSystem()
    call system_clock(count_rate=clock_rate)
    call cpu_time(full_start_time)
    call system_clock(count=full_start_tick)
  end subroutine InitSystem

  subroutine StartMarkTime(checkpointName)
    character(len=*), intent(in) :: checkpointName
    call cpu_time(t1)
    call system_clock(count=start_tick)
  end subroutine StartMarkTime

  subroutine StopMarkTime(checkpointName)
    character(len=*), intent(in) :: checkpointName
    real(kind=8)    :: elapsed_time, full_elapsed_time
    integer(kind=8) :: virtual_memory_used, rss_used
    call ReadProcessMemoryUsage( virtual_memory_used, rss_used )
    call cpu_time(t2)
    call system_clock(count=end_tick)
    elapsed_time = real(end_tick - start_tick, kind=8) / real(clock_rate, kind=8)
    full_elapsed_time = real(end_tick - full_start_tick, kind=8) / real(clock_rate, kind=8)
    write(*,'(A20, F10.2, A,F10.2,A,F8.2,A,I12,A,I12)') checkpointName, t2 - t1, &
         ' CPU seconds.', elapsed_time, ' REAL seconds. FULL TIME: ', &
         full_elapsed_time, ' MEM: VM ', virtual_memory_used, ' RSS: ', rss_used
  end subroutine StopMarkTime

  subroutine PrintFullInformation()
    print *, "========================================================="
    print *, "                  SYSTEM INFORMATION                     "
    print *, "========================================================="

    ! -----------------------------------------------------------------
    ! 1. Date and Time (Standard Fortran Intrinsic)
    ! -----------------------------------------------------------------
    call date_and_time(date_str, time_str, zone_str, dt_values)

    write(*, '(A,I4.4,A,I2.2,A,I2.2)') &
         "Date:          ", dt_values(1), "-", dt_values(2), "-", dt_values(3)
    write(*, '(A,I2.2,A,I2.2,A,I2.2,A,I3.3)') &
         "Time:          ", dt_values(5), ":", dt_values(6), ":", dt_values(7), ".", dt_values(8)
    write(*, '(A,A)') &
         "Time Zone:     ", zone_str
    print *, "---------------------------------------------------------"

    ! -----------------------------------------------------------------
    ! 2. OS & CPU Info (best-effort per platform)
    ! -----------------------------------------------------------------
    print *, "OS Kernel Info:"
    call execute_command_line("uname -sr")
    print *, "---------------------------------------------------------"

    write(*,*) 'CPU Metrics:'
    ! Linux: lscpu; macOS: sysctl as fallback (lscpu usually absent)
    call execute_command_line( &
         "lscpu 2>/dev/null | grep -E 'Model name|CPU\\(s\\):|Thread' " // &
         "|| sysctl -n machdep.cpu.brand_string hw.ncpu 2>/dev/null")
    print *, "---------------------------------------------------------"
    vm_size = -1
    vm_rss = -1
    call ReadProcessMemoryUsage( vm_size, vm_rss )
    if (vm_size /= -1) write(*, '(A,I12,A)') "  Virtual Memory (VmSize): ", vm_size, " kB"
    if (vm_rss  /= -1) write(*, '(A,I12,A)') "  Resident Memory (VmRSS):  ", vm_rss,  " kB"
    write(*,*) 'GPU Information:'
    call PrintGPUInfo()
    print *, "========================================================="
  end subroutine PrintFullInformation

  subroutine ReadProcessMemoryUsage( virtual_memory_used, rss_used )
    integer(kind=8), intent(out)    :: virtual_memory_used, rss_used
    integer :: pos
    character(len=10)  :: dummy_unit
    integer(c_int64_t) :: rss_bytes, virt_bytes
    logical :: got
    ! -----------------------------------------------------------------
    ! Process memory: macOS (Mach/task_info) preferred, Linux /proc fallback
    ! Returns values in kB for compatibility with existing timing output.
    ! -----------------------------------------------------------------
    virtual_memory_used = -1
    rss_used = -1
    got = .false.

    ! Try macOS-native path (returns bytes; convert to kB)
    call get_macos_memory(rss_bytes, virt_bytes)
    if (rss_bytes >= 0_c_int64_t .and. virt_bytes >= 0_c_int64_t) then
       rss_used = int(rss_bytes / 1024_c_int64_t, kind=8)
       virtual_memory_used = int(virt_bytes / 1024_c_int64_t, kind=8)
       got = .true.
    end if

    if (.not. got) then
       ! Linux /proc fallback
       open(newunit=file_unit, file="/proc/self/status", status="old", action="read", iostat=io_stat)
       if (io_stat == 0) then
          do
             read(file_unit, '(A)', iostat=io_stat) line
             if (io_stat /= 0) exit  ! End of file reached
             pos = index(line, "VmSize:")
             if (pos > 0) then
                read(line(pos+7:), *, iostat=io_stat) virtual_memory_used, dummy_unit
             else
                pos = index(line, "VmRSS:")
                if ( pos > 0) then
                   read(line(pos+6:), *, iostat=io_stat) rss_used, dummy_unit
                end if
             end if
          end do
          close(file_unit)
          got = .true.
       end if
    end if

    ! Only warn once-ish would be better, but keep quiet to avoid per-op spam;
    ! if neither source worked, leave as -1.
  end subroutine ReadProcessMemoryUsage
end module SystemInformationModule
