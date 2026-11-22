! File   : profiler.f90
! Author : AI Agent
! Purpose: Provide a simple framework for timing and profiling core functions.

module ProfilerModule
   use, intrinsic :: iso_fortran_env, only: int64, real64
   implicit none
   private

   type :: ProfileEntry
      character(len=64) :: name
      real(kind=real64)  :: total_time
      integer(kind=int64) :: call_count
   end type ProfileEntry

   type(ProfileEntry), allocatable :: profiles(:)
   integer(kind=int64) :: num_profiles
   logical :: profiler_initialized = .false.

   public :: PrintProfileSummary, InitProfiler, RecordTime, get_cpu_time

  contains

    function get_cpu_time() result(t)
       real(kind=real64) :: t
       call cpu_time(t)
    end function get_cpu_time

    subroutine InitProfiler()
       if (profiler_initialized) return
       allocate(profiles(1000))
       num_profiles = 0
       profiler_initialized = .true.
    end subroutine InitProfiler

    subroutine RecordTime(name, duration)
       character(len=*), intent(in) :: name
       real(kind=real64), intent(in) :: duration
       integer :: i
       logical :: found

       ! Initialisation must happen under the lock too: RecordTime is called
       ! from concurrent tasks, and two threads racing through InitProfiler
       ! abort with "allocatable array is already allocated".
       !$omp critical (profiler_table)
       if (.not. profiler_initialized) call InitProfiler()
       found = .false.
       do i = 1, num_profiles
          if (profiles(i)%name == name) then
             profiles(i)%total_time = profiles(i)%total_time + duration
             profiles(i)%call_count = profiles(i)%call_count + 1
             found = .true.
             exit
          end if
       end do

       if (.not. found) then
          ! Add new entry
          num_profiles = num_profiles + 1
          if (num_profiles > size(profiles)) then
             ! Expand array
             block
                type(ProfileEntry), allocatable :: tmp(:)
                allocate(tmp(size(profiles) * 2))
                tmp(1:num_profiles-1) = profiles(1:num_profiles-1)
                call move_alloc(tmp, profiles)
             end block
          end if
          profiles(num_profiles)%name = name
          profiles(num_profiles)%total_time = duration
          profiles(num_profiles)%call_count = 1
       end if
       !$omp end critical (profiler_table)
    end subroutine RecordTime

    subroutine PrintProfileSummary()
       integer :: i
       real(kind=real64) :: total_all
       
       if (.not. profiler_initialized .or. num_profiles == 0) return
       
       write(*,*) ""
       write(*,*) "======================================================================"
       write(*,*) "                       PERFORMANCE PROFILE                           "
       write(*,*) "======================================================================"
       write(*, '(A12, A20, A15, A15)') "Calls", "Function", "Total Time (s)", "Avg Time (s)"
       write(*,*) "----------------------------------------------------------------------"
       
       total_all = 0.0_real64
       do i = 1, num_profiles
          write(*, '(I12, A20, F15.6, F15.6)') &
             profiles(i)%call_count, &
             profiles(i)%name, &
             profiles(i)%total_time, &
             profiles(i)%total_time / real(profiles(i)%call_count, real64)
          total_all = total_all + profiles(i)%total_time
       end do
       
       write(*,*) "----------------------------------------------------------------------"
       write(*, '(A, F15.6)') "Total Profiled Time: ", total_all
       write(*,*) "======================================================================"
       write(*,*) ""
    end subroutine PrintProfileSummary

end module ProfilerModule
