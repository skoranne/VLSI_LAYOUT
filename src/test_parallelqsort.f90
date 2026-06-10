!=====================================================================
!  file: parallel_quicksort.f90
!  purpose: Demonstrate OpenMP task parallelism for a recursive QuickSort
!=====================================================================
module quicksort_mod
   use iso_fortran_env, only: wp => real64
   implicit none
   private
   public :: parallel_quicksort, serial_quicksort

   integer, parameter :: CUTOFF = 128   ! sub‑array size below which we sort sequentially

contains

   !--------------------------------------------------------------
   !  In‑place partition (Lomuto scheme).  Returns the final pivot index.
   !--------------------------------------------------------------
   function partition(A, lo, hi) result(pivot_idx)
      real(wp), intent(inout) :: A(:)
      integer,  intent(in)    :: lo, hi
      integer                 :: i, j
      real(wp)                :: pivot, tmp
      integer                 :: pivot_idx
      pivot = A(hi)                 ! choose last element as pivot
      i = lo - 1
      do j = lo, hi-1
         if (A(j) <= pivot) then
            i = i + 1
            tmp = A(i); A(i) = A(j); A(j) = tmp
         end if
      end do
      ! place pivot in its final position
      tmp = A(i+1); A(i+1) = A(hi); A(hi) = tmp
      pivot_idx = i + 1
   end function partition

   !--------------------------------------------------------------
   !  Simple sequential sort for tiny sub‑arrays (insertion sort)
   !--------------------------------------------------------------
   pure subroutine insertion_sort(A, lo, hi)
      real(wp), intent(inout) :: A(:)
      integer,  intent(in)    :: lo, hi
      integer :: i, j
      real(wp) :: key

      do i = lo + 1, hi
         key = A(i)
         j   = i - 1
         do while (j >= lo .and. A(j) > key)
            A(j+1) = A(j)
            j = j - 1
         end do
         A(j+1) = key
      end do
   end subroutine insertion_sort

   !--------------------------------------------------------------
   !  Recursive QuickSort – **must be called inside an OpenMP parallel region**
   !--------------------------------------------------------------
   recursive subroutine quicksort_task(A, lo, hi)
      real(wp), intent(inout) :: A(:)
      integer,  intent(in)    :: lo, hi
      integer                 :: p

      if (hi - lo + 1 <= CUTOFF) then
         call insertion_sort(A, lo, hi)
         return
      end if

      p = partition(A, lo, hi)

      !--- Left part -------------------------------------------------
!$omp task shared(A) firstprivate(lo, p) default(none)
      call quicksort_task(A, lo, p-1)
!$omp end task

      !--- Right part ------------------------------------------------
!$omp task shared(A) firstprivate(p, hi) default(none)
      call quicksort_task(A, p+1, hi)
!$omp end task

      ! Both children must finish before we return to the caller
!$omp taskwait
   end subroutine quicksort_task

   !--------------------------------------------------------------
   !  Public wrapper – launches the parallel region once.
   !--------------------------------------------------------------
   subroutine parallel_quicksort(A, max_threads)
      real(wp), intent(inout) :: A(:)
      integer,  intent(in), optional :: max_threads
      integer :: nt

      if (present(max_threads)) then
         nt = max_threads
         call omp_set_num_threads(nt)
      end if

!$omp parallel default(none) shared(A) private(nt)
!$omp single nowait
      call quicksort_task(A, 1, size(A))
!$omp end single
!$omp end parallel
   end subroutine parallel_quicksort

   !--------------------------------------------------------------
   !  Serial reference implementation (same algorithm, no tasks)
   !--------------------------------------------------------------
   recursive subroutine quicksort_serial(A, lo, hi)
      real(wp), intent(inout) :: A(:)
      integer,  intent(in)    :: lo, hi
      integer                 :: p

      if (hi - lo + 1 <= CUTOFF) then
         call insertion_sort(A, lo, hi)
         return
      end if

      p = partition(A, lo, hi)
      call quicksort_serial(A, lo, p-1)
      call quicksort_serial(A, p+1, hi)
   end subroutine quicksort_serial

   subroutine serial_quicksort(A)
      real(wp), intent(inout) :: A(:)
      call quicksort_serial(A, 1, size(A))
   end subroutine serial_quicksort

end module quicksort_mod
!=====================================================================

program test_quicksort
   use iso_fortran_env, only: wp => real64
   use quicksort_mod
   implicit none

   integer, parameter :: N = 20000000      ! size of the test array
   real(wp), allocatable :: vec(:), vec_ref(:)
   integer :: i, seed(8), nthreads
   real    :: t0, t1, tserial, tparallel
   logical :: ok

   !--- initialise a reproducible random sequence -----------------
   call random_seed()
   allocate (vec(N), vec_ref(N))

   do i = 1, N
      call random_number(vec(i))
   end do
   vec_ref = vec                         ! keep a copy for the serial run

   !--- Serial quicksort (reference) -------------------------------
   call cpu_time(t0)
   call serial_quicksort(vec_ref)
   call cpu_time(t1)
   tserial = t1 - t0
   write (*,*) 'Serial quicksort time  : ', tserial, ' s'

   !--- Parallel quicksort -----------------------------------------
   nthreads = 2                     ! change to whatever your machine has
   call cpu_time(t0)
   call parallel_quicksort(vec, nthreads)
   call cpu_time(t1)
   tparallel = t1 - t0
   write (*,*) 'Parallel quicksort time: ', tparallel, ' s (', nthreads, ' threads)'
   do i = 1, N-1
      if( vec(i) > vec(i+1) ) error stop "SORT WRONG"
   end do
   !--- Verify correctness -----------------------------------------
   ok = all (abs(vec - vec_ref) < 1.0e-12_wp)
   if (ok) then
      write (*,*) 'Result verification: SUCCESS'
   else
      write (*,*) 'Result verification: FAILURE'
   end if

   write (*,*) 'Speed‑up = ', tserial / tparallel

   deallocate (vec, vec_ref)
end program test_quicksort
!=====================================================================
