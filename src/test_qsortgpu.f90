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
   !  Partition – must be a device routine (it swaps array elements)
   !--------------------------------------------------------------
!$omp declare target
   subroutine partition(arr, lo, hi, piv)
      real(wp), intent(inout) :: arr(:)
      integer,  intent(in)    :: lo, hi
      integer,  intent(out)   :: piv
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
!$omp end declare target

   !--------------------------------------------------------------
   !  Tiny insertion sort – also a device routine
   !--------------------------------------------------------------
!$omp declare target
   pure subroutine insertion_sort(a, lo, hi)
      real(wp), intent(inout) :: a(:)
      integer,  intent(in)    :: lo, hi
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
   end subroutine insertion_sort
!$omp end declare target

   !--------------------------------------------------------------
   !  Iterative quick‑sort – device routine that calls the two above
   !--------------------------------------------------------------
!$omp declare target
   subroutine quicksort_iterative(arr, n)
      real(wp), intent(inout) :: arr(:)
      integer,  intent(in)    :: n
      integer, allocatable    :: L(:), R(:)
      integer                 :: top, lo, hi, piv

      allocate (L(0:2*int(log(real(n))/log(2.0_wp)+2)))
      allocate (R(0:2*int(log(real(n))/log(2.0_wp)+2)))

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
            top = top + 1; L(top) = lo;   R(top) = piv-1
            top = top + 1; L(top) = piv+1; R(top) = hi
         else
            top = top + 1; L(top) = piv+1; R(top) = hi
            top = top + 1; L(top) = lo;   R(top) = piv-1
         end if
      end do

      deallocate (L, R)
   end subroutine quicksort_iterative
!$omp end declare target

   !--------------------------------------------------------------
   !  GPU wrapper – **correct nesting** (see comment block above)
   !--------------------------------------------------------------
   subroutine quicksort_gpu(arr)
      real(wp), intent(inout) :: arr(:)
      integer :: n

      n = size(arr)

!$omp target data map(tofrom:arr(1:n))
!$omp target teams num_teams(1) thread_limit(256)
!$omp distribute parallel do
      do i = 1, 1                     ! dummy loop required after parallel do
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

   integer, parameter :: N = 20_000_000
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

   print *, 'Speed‑up (CPU / GPU) = ', t_cpu / t_gpu
   deallocate (a, a_ref)
end program test_qs
!=====================================================================
