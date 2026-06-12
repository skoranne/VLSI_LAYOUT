! File   : systeminfo.f90
! Author : Sandeep Koranne
! Purpose: In Modern fortran, print the system information such as time/date, CPU, Linux, and process memory used
module SystemInformationModule
  implicit none

  ! Date and Time variables
  character(len=8)  :: date_str
  character(len=10) :: time_str
  character(len=5)  :: zone_str
  integer           :: dt_values(8)

  ! Memory parsing variables
  integer            :: file_unit, io_stat
  character(len=32) :: line
  integer(kind=8)    :: vm_size, vm_rss
  logical            :: opened
  integer(kind=8) :: full_start_tick
  integer(kind=8) :: start_tick, end_tick, clock_rate
  real        :: full_start_time, t1, t2
  
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
    write(*,'(A20, F8.2, A,F8.2,A,F8.2,A,I12,A,I12)') checkpointName, t2 - t1, &
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
    ! 2. Linux OS & CPU Info (Using Fortran 2008 execute_command_line)
    ! -----------------------------------------------------------------
    print *, "OS Kernel Info:"
    call execute_command_line("uname -sr")
    print *, "---------------------------------------------------------"

    print *, "CPU Metrics:"
    ! Grabs core counts and model details from the system topology
    call execute_command_line("lscpu | grep -E 'Model name|CPU\(s\):|Thread'")
    print *, "---------------------------------------------------------"
    vm_size = -1
    vm_rss = -1
    call ReadProcessMemoryUsage( vm_size, vm_rss )
    if (vm_size /= -1) write(*, '(A,I12,A)') "  Virtual Memory (VmSize): ", vm_size, " kB"
    if (vm_rss /= -1)  write(*, '(A,I12,A)') "  Resident Memory (VmRSS): ", vm_rss, " kB"
    print *, "========================================================="    
  end subroutine PrintFullInformation
  subroutine ReadProcessMemoryUsage( virtual_memory_used, rss_used )
    integer(kind=8), intent(out)    :: virtual_memory_used, rss_used
    integer :: pos
    character(len=10)  :: dummy_unit
    ! -----------------------------------------------------------------
    ! 3. Process Memory Consumed (Parsing Linux /proc/self/status)
    ! -----------------------------------------------------------------
    virtual_memory_used = -1
    rss_used = -1
    ! Open the Linux pseudo-filesystem status file for the current PID
    open(newunit=file_unit, file="/proc/self/status", status="old", action="read", iostat=io_stat)
    if (io_stat == 0) then
       do
          read(file_unit, '(A)', iostat=io_stat) line
          if (io_stat /= 0) exit  ! End of file reached
          ! Scan the line for Virtual Memory Size
          pos = index(line, "VmSize:")
          if (pos > 0) then
             read(line(pos+7:), *) virtual_memory_used, dummy_unit
             ! Scan the line for Resident Set Size (Actual RAM allocation)
          else
             pos = index(line, "VmRSS:") 
             if ( pos > 0) then
                read(line(pos+6:), *) rss_used, dummy_unit
             end if
          end if
       end do
       close(file_unit)
       ! Print results if successfully parsed
       !if (virtual_memory_used /= -1) write(*, '(A,I12,A)') "  Virtual Memory (VmSize): ", virtual_memory_used, " kB"
       !if (rss_used /= -1)  write(*, '(A,I12,A)') "  Resident Memory (VmRSS): ", rss_used, " kB"
    else
       print *, "  [Error]: Unable to access /proc/self/status. Ensure you are on Linux."
    end if
  end subroutine ReadProcessMemoryUsage
end module SystemInformationModule


