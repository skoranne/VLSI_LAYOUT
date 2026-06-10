!=====================================================================
! gpu_bitonic_sort.f90 
!=====================================================================
module box_sort_mod
  use iso_fortran_env, only: int32
  implicit none
  private
  public :: Box, bitonic_sort_indirect_gpu, bitonic_sort_indirect_cpu

  ! Define the custom type
  type :: Box
     integer(int32) :: x1, y1, x2, y2
  end type Box

contains
  !--------------------------------------------------------------
  ! Bitonic Indirect Sort for Multi-Core CPUs
  !--------------------------------------------------------------
  subroutine bitonic_sort_indirect_cpu(boxes, idx, n)
    type(Box), intent(in)    :: boxes(:)
    integer,   intent(inout) :: idx(:)
    integer,   intent(in)    :: n

    integer :: i, j, k, ixj, temp

    k = 2
    do while (k <= n)
       j = k / 2
       do while (j > 0)

          ! FIX: Standard CPU parallel loop. 
          ! Ensuring 'ixj' and 'temp' are private is CRITICAL to avoid race conditions!
          !$omp parallel do private(ixj, temp)
          do i = 0, n - 1
             ixj = ieor(i, j) 

             if (i < ixj) then
                if (iand(i, k) == 0) then
                   ! Ascending sort
                   if (boxes(idx(i+1))%x1 > boxes(idx(ixj+1))%x1) then
                      temp       = idx(i+1)
                      idx(i+1)   = idx(ixj+1)
                      idx(ixj+1) = temp
                   end if
                else
                   ! Descending sort
                   if (boxes(idx(i+1))%x1 < boxes(idx(ixj+1))%x1) then
                      temp       = idx(i+1)
                      idx(i+1)   = idx(ixj+1)
                      idx(ixj+1) = temp
                   end if
                end if
             end if
          end do
          ! End of OpenMP parallel region

          j = j / 2
       end do
       k = k * 2
    end do

  end subroutine bitonic_sort_indirect_cpu
  !--------------------------------------------------------------
  ! Bitonic Indirect Sort for GPUs
  ! Sorts an index array based on the 'x1' property of the Box
  !--------------------------------------------------------------
  subroutine bitonic_sort_indirect_gpu(boxes, idx, n)
    type(Box), intent(in)    :: boxes(:)
    integer,   intent(inout) :: idx(:)
    integer,   intent(in)    :: n

    integer :: i, j, k, ixj, temp

    ! We map the boxes as 'to' (read-only) and idx as 'tofrom'
    ! This keeps the data on the GPU across all kernel launches
    !$omp target data map(to: boxes(1:n)) map(tofrom: idx(1:n))

    k = 2
    do while (k <= n)
       j = k / 2
       do while (j > 0)

          ! Launch a highly parallel kernel for this step
          !$omp target teams distribute parallel do private(ixj, temp)
          do i = 0, n - 1
             ! ieor is Fortran's bitwise XOR intrinsic
             ixj = ieor(i, j) 

             if (i < ixj) then
                ! Check if we are building an ascending or descending sequence
                if (iand(i, k) == 0) then
                   ! Ascending sort (using the x1 coordinate as the key)
                   if (boxes(idx(i+1))%x1 > boxes(idx(ixj+1))%x1) then
                      temp       = idx(i+1)
                      idx(i+1)   = idx(ixj+1)
                      idx(ixj+1) = temp
                   end if
                else
                   ! Descending sort
                   if (boxes(idx(i+1))%x1 < boxes(idx(ixj+1))%x1) then
                      temp       = idx(i+1)
                      idx(i+1)   = idx(ixj+1)
                      idx(ixj+1) = temp
                   end if
                end if
             end if
          end do

          j = j / 2
       end do
       k = k * 2
    end do

    !$omp end target data
  end subroutine bitonic_sort_indirect_gpu

end module box_sort_mod

!=====================================================================
subroutine test_bitonic()
  use box_sort_mod
  use iso_fortran_env, only: int32
  implicit none

  ! N MUST be a power of 2 for this basic Bitonic Sort (e.g., 2^24 = 16,777,216)
  integer, parameter :: N = 16777216 

  type(Box), allocatable :: my_boxes(:)
  integer, allocatable   :: my_idx(:)
  integer :: i
  real    :: t0, t1

  allocate(my_boxes(N))
  allocate(my_idx(N))

  ! Initialize data
  print *, 'Initializing data...'
  do i = 1, N
     ! Give x1 a random value between 1 and 1000
     call random_number(t0)
     my_boxes(i)%x1 = int(t0 * 1000)
     my_idx(i)      = i  ! Initial index is just 1 to N
  end do

  ! Sort on GPU
  print *, 'Sorting on GPU...'
  call cpu_time(t0)
  call bitonic_sort_indirect_gpu(my_boxes, my_idx, N)
  call cpu_time(t1)

  print *, 'GPU Bitonic Sort time = ', t1 - t0, ' s'

  ! Verify
  do i = 1, N - 1
     if (my_boxes(my_idx(i))%x1 > my_boxes(my_idx(i+1))%x1) then
        print *, 'Verification: FAILURE at index ', i
        stop
     end if
  end do
  print *, 'Verification: SUCCESS'

  deallocate(my_boxes, my_idx)
end subroutine test_bitonic

program test_bitonic_padded
  use box_sort_mod
  use iso_fortran_env, only: int32
  implicit none

  ! Define your real size and your padded size (power of 2)
  integer, parameter :: REAL_N = 503489064
  integer, parameter :: PAD_N  = 536870912  

  type(Box), allocatable :: my_boxes(:)
  integer, allocatable   :: my_idx(:)
  integer :: i
  real    :: t0, t1

  ! 1. Allocate arrays to the FULL PADDED SIZE
  allocate(my_boxes(PAD_N))
  allocate(my_idx(PAD_N))

  print *, 'Initializing data...'

  ! 2. Initialize your REAL data
  do i = 1, REAL_N
     call random_number(t0)
     my_boxes(i)%x1 = int(t0 * 1000)
     my_idx(i)      = i
  end do

  ! 3. Initialize the DUMMY data with the maximum possible integer
  do i = REAL_N + 1, PAD_N
     ! huge(1_int32) guarantees these boxes will be sorted to the very end
     my_boxes(i)%x1 = huge(1_int32)
     my_idx(i)      = i 
  end do

  ! 4. Sort on GPU using the PAD_N size
  print *, 'Sorting on GPU...'
  call cpu_time(t0)
  !call bitonic_sort_indirect_gpu(my_boxes, my_idx, PAD_N)
  call bitonic_sort_indirect_cpu(my_boxes, my_idx, PAD_N)
  call cpu_time(t1)

  print *, 'CPU Bitonic Sort time = ', t1 - t0, ' s'

  ! 5. You are done! 
  ! Your perfectly sorted real data is now accessible via my_idx(1) through my_idx(REAL_N).
  ! You can safely ignore everything from my_idx(REAL_N + 1) onwards.

  deallocate(my_boxes, my_idx)
end program test_bitonic_padded
