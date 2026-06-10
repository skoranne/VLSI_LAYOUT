!=====================================================================
!  gpu_quicksort_fixed.f90  –  works with nvfortran -mp=gpu
!=====================================================================
module gpu_qs_mod
  use iso_fortran_env, only: wp => real64
  use omp_lib
  implicit none
  private
  public :: quicksort_gpu, quicksort_cpu

  integer, parameter :: CUTOFF = 1024   ! base case for insertion sort
contains

  !--------------------------------------------------------------
  !  Partition – device routine (swaps array elements)
  !--------------------------------------------------------------
  subroutine partition(arr, lo, hi, piv)
    real(wp), intent(inout) :: arr(*)
    integer,  intent(in)    :: lo, hi
    integer,  intent(out)   :: piv
  !$omp declare target
    integer :: i, p
    real(wp) :: tmp, pivot

    pivot = arr(hi)
    p = lo - 1
    do i = lo, hi-1
       if (arr(i) <= pivot) then
          p = p + 1
          tmp = arr(p); arr(p) = arr(i); arr(i) = tmp
       end if
    end do
    tmp = arr(p+1); arr(p+1) = arr(hi); arr(hi) = tmp
    piv = p + 1
  end subroutine partition

  !--------------------------------------------------------------
  !  Tiny insertion sort – device routine
  !--------------------------------------------------------------
  !--------------------------------------------------------------
   !  Tiny insertion sort
   !--------------------------------------------------------------
   pure subroutine insertion_sort(a, lo, hi)
      real(wp), intent(inout) :: a(*)
      integer,  intent(in)    :: lo, hi
      !$omp declare target
      integer :: i, j
      real(wp) :: key

      do i = lo+1, hi
         key = a(i)
         j   = i-1
         ! FIX: Manual short-circuit to prevent out-of-bounds GPU access
         do while (j >= lo)
            if (a(j) <= key) exit
            a(j+1) = a(j)
            j = j-1
         end do
         a(j+1) = key
      end do
    end subroutine insertion_sort
    
  pure subroutine broken_insertion_sort(a, lo, hi)
    real(wp), intent(inout) :: a(:)
    integer,  intent(in)    :: lo, hi
  !$omp declare target
    integer :: i, j
    real(wp) :: key

    do i = lo+1, hi
       key = a(i)
       j   = i-1
       do while (j >= lo .and. a(j) > key)
          a(j+1) = a(j)
          j = j-1
       end do
       a(j+1) = key
    end do
  end subroutine broken_insertion_sort

  !--------------------------------------------------------------
  !  Iterative quick-sort – device routine
  !--------------------------------------------------------------
  subroutine quicksort_iterative(arr, n)
    real(wp), intent(inout) :: arr(n)
    integer,  intent(in)    :: n
  !$omp declare target
    ! Replaced dynamic allocation with fixed-size arrays for GPU compatibility
    integer                 :: L(0:100), R(0:100) 
    integer                 :: top, lo, hi, piv

    top = 0
    L(top) = 1
    R(top) = n

    do while (top >= 0)
       lo = L(top); hi = R(top)
       top = top - 1

       if (hi - lo + 1 <= CUTOFF) then
          call insertion_sort(arr, lo, hi)
          cycle
       end if

       call partition(arr, lo, hi, piv)

       if (piv - lo > hi - piv) then
          top = top + 1; L(top) = lo;    R(top) = piv-1
          top = top + 1; L(top) = piv+1; R(top) = hi
       else
          top = top + 1; L(top) = piv+1; R(top) = hi
          top = top + 1; L(top) = lo;    R(top) = piv-1
       end if
    end do
  end subroutine quicksort_iterative

  !--------------------------------------------------------------
  !  GPU wrapper 
  !--------------------------------------------------------------
  subroutine quicksort_gpu(arr)
    real(wp), intent(inout) :: arr(:)
    integer :: n, i ! Added declaration for 'i'

    n = size(arr)

    !$omp target data map(tofrom:arr(1:n))
    !$omp target teams num_teams(1) thread_limit(256)
    !$omp distribute parallel do
    do i = 1, 1 
       call quicksort_iterative(arr, n)
    end do
    !$omp end distribute parallel do
    !$omp end teams
    !$omp end target
    !$omp end target data
  end subroutine quicksort_gpu

  !--------------------------------------------------------------
  !  Serial version – for timing / verification
  !--------------------------------------------------------------
  subroutine quicksort_cpu(arr)
    real(wp), intent(inout) :: arr(:)
    call quicksort_iterative(arr, size(arr))
  end subroutine quicksort_cpu

end module gpu_qs_mod
!=====================================================================
program test_qs
  use iso_fortran_env, only: wp => real64
  use gpu_qs_mod
  implicit none

  integer, parameter :: N = 20000000
  real(wp), allocatable :: a(:), a_ref(:)
  real :: t0, t1, t_cpu, t_gpu

  allocate (a(N), a_ref(N))
  call random_number(a)
  a_ref = a

  !--- CPU reference -------------------------------------------------
  call cpu_time(t0)
  call quicksort_cpu(a_ref)
  call cpu_time(t1)
  t_cpu = t1 - t0
  print *, 'CPU quicksort time   = ', t_cpu, ' s'

  !--- GPU offload ---------------------------------------------------
  call cpu_time(t0)
  call quicksort_gpu(a)
  call cpu_time(t1)
  t_gpu = t1 - t0
  print *, 'GPU quicksort time   = ', t_gpu, ' s'

  !--- Verify ---------------------------------------------------------
  if (all (abs(a - a_ref) < 1.0e-12_wp)) then
     print *, 'Verification : SUCCESS'
  else
     print *, 'Verification : FAILURE'
  end if

  print *, 'Speed-up (CPU / GPU) = ', t_cpu / t_gpu
  deallocate (a, a_ref)
end program test_qs
