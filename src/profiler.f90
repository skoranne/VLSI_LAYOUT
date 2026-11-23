! File   : profiler.f90
! Author : AI Agent
! Purpose: Provide a simple framework for timing and profiling core functions.

module ProfilerModule
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use, intrinsic :: iso_c_binding, only: c_char, c_null_char
   use, intrinsic :: iso_fortran_env, only: output_unit
   use omp_lib, only: omp_get_wtime
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
   public :: PhaseTimersEnabled, PhaseBegin, PhaseEnd, PrintPhaseSummary

   !> Wall-clock phase timers (MAGPARSER_PHASE_TIMERS=1).  PhaseBegin returns the start
   !> time, so nested and concurrent (OpenMP task) phases need no shared stack; PhaseEnd
   !> accumulates into a name table under a critical section, prints one PHASE line and
   !> closes the matching NVTX range so nsys timelines carry the same names.
   integer, parameter :: K_MAX_PHASES = 128
   type :: PhaseEntry
      character(len=32)   :: name = ''
      real(kind=real64)   :: wall = 0.0_real64
      integer(kind=int64) :: calls = 0
      integer(kind=int64) :: items = 0
   end type PhaseEntry
   type(PhaseEntry), save :: phases(K_MAX_PHASES)
   integer, save :: num_phases = 0
   integer, save :: phase_state = -1     !< -1 unknown, 0 off, 1 on

   interface
      subroutine magparser_nvtx_push(name) bind(C, name="magparser_nvtx_push")
         import :: c_char
         character(kind=c_char), dimension(*), intent(in) :: name
      end subroutine magparser_nvtx_push
      subroutine magparser_nvtx_pop() bind(C, name="magparser_nvtx_pop")
      end subroutine magparser_nvtx_pop
   end interface

  contains

    function get_cpu_time() result(t)
       real(kind=real64) :: t
       call cpu_time(t)
    end function get_cpu_time

    subroutine InitProfiler()
       !$omp critical(profiler_lock)
       if (.not. profiler_initialized) then
          allocate(profiles(1000))
          num_profiles = 0
          profiler_initialized = .true.
       end if
       !$omp end critical(profiler_lock)
    end subroutine InitProfiler

    subroutine RecordTime(name, duration)
       character(len=*), intent(in) :: name
       real(kind=real64), intent(in) :: duration
       integer :: i
       logical :: found
       type(ProfileEntry), allocatable :: tmp(:)

       !$omp critical(profiler_lock)
       if (.not. profiler_initialized) then
          allocate(profiles(1000))
          num_profiles = 0
          profiler_initialized = .true.
       end if

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
             allocate(tmp(size(profiles) * 2))
             tmp(1:num_profiles-1) = profiles(1:num_profiles-1)
             call move_alloc(tmp, profiles)
          end if
          profiles(num_profiles)%name = name
          profiles(num_profiles)%total_time = duration
          profiles(num_profiles)%call_count = 1
       end if
       !$omp end critical(profiler_lock)
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

    logical function PhaseTimersEnabled()
       character(len=8) :: ev
       integer :: evlen, evstat
       if (phase_state < 0) then
          call get_environment_variable('MAGPARSER_PHASE_TIMERS', ev, evlen, evstat)
          !$omp critical(phase_lock)
          if (evstat == 0 .and. evlen > 0 .and. ev(1:1) /= '0') then
             phase_state = 1
          else
             phase_state = 0
          end if
          !$omp end critical(phase_lock)
       end if
       PhaseTimersEnabled = (phase_state == 1)
    end function PhaseTimersEnabled

    !> Returns the start time (0 when disabled); pass it to PhaseEnd.
    function PhaseBegin(name) result(t0)
       character(len=*), intent(in) :: name
       real(kind=real64) :: t0
       t0 = 0.0_real64
       if (.not. PhaseTimersEnabled()) return
       call magparser_nvtx_push(trim(name)//c_null_char)
       !$omp critical(phase_lock)
       write(*,'(A,A)') 'PHASE_BEGIN ', trim(name)
       flush(output_unit)
       !$omp end critical(phase_lock)
       t0 = omp_get_wtime()
    end function PhaseBegin

    !> items: optional work count (boxes processed/produced) reported alongside the time.
    subroutine PhaseEnd(name, t0, items)
       character(len=*), intent(in) :: name
       real(kind=real64), intent(in) :: t0
       integer(kind=int64), intent(in), optional :: items
       real(kind=real64) :: dt
       integer :: i, slot
       integer(kind=int64) :: n
       if (.not. PhaseTimersEnabled()) return
       dt = omp_get_wtime() - t0
       call magparser_nvtx_pop()
       n = 0
       if (present(items)) n = items
       !$omp critical(phase_lock)
       slot = 0
       do i = 1, num_phases
          if (phases(i)%name == name) then
             slot = i
             exit
          end if
       end do
       if (slot == 0 .and. num_phases < K_MAX_PHASES) then
          num_phases = num_phases + 1
          slot = num_phases
          phases(slot)%name = name
       end if
       if (slot > 0) then
          phases(slot)%wall = phases(slot)%wall + dt
          phases(slot)%calls = phases(slot)%calls + 1
          phases(slot)%items = phases(slot)%items + n
       end if
       write(*,'(A,A,F12.4,A,I0)') 'PHASE ', name(1:min(len(name),32)), dt, ' s items=', n
       flush(output_unit)
       !$omp end critical(phase_lock)
    end subroutine PhaseEnd

    subroutine PrintPhaseSummary()
       integer :: i
       if (.not. PhaseTimersEnabled() .or. num_phases == 0) return
       write(*,'(A)') 'PHASE_SUMMARY name calls wall_s items'
       do i = 1, num_phases
          write(*,'(A,A32,I10,F14.4,I16)') 'PHASE_SUMMARY ', phases(i)%name, phases(i)%calls, &
               phases(i)%wall, phases(i)%items
       end do
    end subroutine PrintPhaseSummary

end module ProfilerModule
