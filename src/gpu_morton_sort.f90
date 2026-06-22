! File   : gpu_morton_sort.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Simple Morton coded sort using OpenMP target

module MortonSortOMT
  use CommonModule
  use GeometryModule
  use iso_fortran_env, only: int32, int64, real64  
  use omp_lib
  implicit none
  ! Bit-mask constants for 64-bit Morton Code generation
  integer(kind=int64), parameter :: MASK1 = int(Z'0000FFFF0000FFFF', kind=int64)
  integer(kind=int64), parameter :: MASK2 = int(Z'00FF00FF00FF00FF', kind=int64)
  integer(kind=int64), parameter :: MASK3 = int(Z'0F0F0F0F0F0F0F0F', kind=int64)
  integer(kind=int64), parameter :: MASK4 = int(Z'3333333333333333', kind=int64)
  integer(kind=int64), parameter :: MASK5 = int(Z'5555555555555555', kind=int64)
  public:: SortBoxesDirect
contains

  !> Direct Sort: Physically sorts the array of Boxes based on Morton Code
  subroutine SortBoxesDirect(Boxes, N)
    type(Box), intent(inout) :: Boxes(:)
    integer(kind=int64), intent(in) :: N

    integer(kind=int64) :: M, I, J, K
    integer(kind=int64) :: I0, IXJ0
    logical :: Dir

    ! Device padded arrays (Bitonic sort strictly requires power-of-2 size)
    type(Box), allocatable :: BoxesPad(:)
    integer(kind=int64), allocatable :: MortonPad(:)

    integer(kind=int64) :: TempM, CX, CY, MX, MY
    type(Box) :: TempB

    ! 1. Calculate next power of 2 for Bitonic Sort padding
    M = 1
    do while (M < N)
       M = M * 2
    end do

    allocate(BoxesPad(M))
    allocate(MortonPad(M))

    ! Map data to GPU and persist it across all sorting phases
    !$omp target enter data map(to: Boxes(1:N)) &
    !$omp map(alloc: BoxesPad(1:M), MortonPad(1:M))
    ! ==========================================
    ! PHASE 1: Initialize Pads and Inline Morton
    ! ==========================================
    !$omp target teams distribute parallel do private(I, CX, CY, MX, MY)
    do I = 1, M
       if (I <= N) then
          BoxesPad(I) = Boxes(I)

          ! Use center of box for Morton Code (avoiding negative coordinate issues)
          CX = (BoxesPad(I)%X1 + BoxesPad(I)%X2) / 2
          CY = (BoxesPad(I)%Y1 + BoxesPad(I)%Y2) / 2

          ! Inline Morton Code generation (Expand X)
          MX = CX
          MX = iand(ior(MX, ishft(MX, 16)), MASK1)
          MX = iand(ior(MX, ishft(MX,  8)), MASK2)
          MX = iand(ior(MX, ishft(MX,  4)), MASK3)
          MX = iand(ior(MX, ishft(MX,  2)), MASK4)
          MX = iand(ior(MX, ishft(MX,  1)), MASK5)

          ! Inline Morton Code generation (Expand Y)
          MY = CY
          MY = iand(ior(MY, ishft(MY, 16)), MASK1)
          MY = iand(ior(MY, ishft(MY,  8)), MASK2)
          MY = iand(ior(MY, ishft(MY,  4)), MASK3)
          MY = iand(ior(MY, ishft(MY,  2)), MASK4)
          MY = iand(ior(MY, ishft(MY,  1)), MASK5)

          ! Interleave
          MortonPad(I) = ior(ishft(MY, 1), MX)
       else
          ! Pad out-of-bounds with maximum possible values
          BoxesPad(I)%X1 = HUGE(1_K_COORDINATE_KIND)
          BoxesPad(I)%Y1 = HUGE(1_K_COORDINATE_KIND)
          BoxesPad(I)%X2 = HUGE(1_K_COORDINATE_KIND)
          BoxesPad(I)%Y2 = HUGE(1_K_COORDINATE_KIND)
          MortonPad(I)  = HUGE(1_int64)
       end if
    end do

    ! ==========================================
    ! PHASE 2: Parallel Bitonic Sort
    ! ==========================================
    ! Outer loops run on CPU, dispatching lightweight concurrent kernels to GPU.
    ! This guarantees cross-block synchronization without device-side locks.
    K = 2
    do while (K <= M)
       J = K / 2
       do while (J > 0)

          !$omp target teams distribute parallel do private(I, I0, IXJ0, Dir, TempM, TempB)
          do I = 1, M
             I0 = I - 1
             IXJ0 = ieor(I0, J)

             if (I0 < IXJ0) then
                ! Determine if we are in an ascending or descending bitonic block
                Dir = (iand(I0, K) == 0)

                ! Elegant logical equivalence to handle both asc and desc sorts
                if (Dir .eqv. (MortonPad(I) > MortonPad(IXJ0 + 1))) then
                   ! Swap Morton Codes
                   TempM = MortonPad(I)
                   MortonPad(I) = MortonPad(IXJ0 + 1)
                   MortonPad(IXJ0 + 1) = TempM

                   ! Swap Boxes
                   TempB = BoxesPad(I)
                   BoxesPad(I) = BoxesPad(IXJ0 + 1)
                   BoxesPad(IXJ0 + 1) = TempB
                end if
             end if
          end do

          J = J / 2
       end do
       K = K * 2
    end do

    ! ==========================================
    ! PHASE 3: Copy Back Valid Data
    ! ==========================================
    !$omp target teams distribute parallel do private(I)
    do I = 1, N
       Boxes(I) = BoxesPad(I)
    end do
    !$omp target exit data map(release:  Boxes(1:N)) map(delete: BoxesPad(1:M), MortonPad(1:M))    !> leave the boxes on the device
    !$komp target exit data map(delete: Boxes(1:N), BoxesPad(1:M), MortonPad(1:M))

    deallocate(BoxesPad)
    deallocate(MortonPad)

  end subroutine SortBoxesDirect

  !> Indirect Sort: Leaves Boxes untouched, returns an array of sorted indices.
  !> Ideal for keeping massive layout structures in immutable memory.
  subroutine SortBoxesIndirect(Boxes, N, SortedIndices)
    type(Box), intent(in) :: Boxes(:)
    integer(kind=int64), intent(in) :: N
    integer(kind=int64), intent(out) :: SortedIndices(:)

    integer(kind=int64) :: M, I, J, K
    integer(kind=int64) :: I0, IXJ0
    logical :: Dir

    integer(kind=int64), allocatable :: IndicesPad(:)
    integer(kind=int64), allocatable :: MortonPad(:)

    integer(kind=int64) :: TempM, TempIdx, CX, CY, MX, MY

    M = 1
    do while (M < N)
       M = M * 2
    end do

    allocate(IndicesPad(M))
    allocate(MortonPad(M))
    !$omp target enter data map(to: Boxes(1:N)) &
    !$omp map(alloc: SortedIndices(1:N)) map(alloc: IndicesPad(1:M), MortonPad(1:M))

    !$omp target teams distribute parallel do private(I, CX, CY, MX, MY)
    do I = 1, M
       if (I <= N) then
          IndicesPad(I) = I

          CX = (Boxes(I)%X1 + Boxes(I)%X2) / 2
          CY = (Boxes(I)%Y1 + Boxes(I)%Y2) / 2

          MX = CX
          MX = iand(ior(MX, ishft(MX, 16)), MASK1)
          MX = iand(ior(MX, ishft(MX,  8)), MASK2)
          MX = iand(ior(MX, ishft(MX,  4)), MASK3)
          MX = iand(ior(MX, ishft(MX,  2)), MASK4)
          MX = iand(ior(MX, ishft(MX,  1)), MASK5)

          MY = CY
          MY = iand(ior(MY, ishft(MY, 16)), MASK1)
          MY = iand(ior(MY, ishft(MY,  8)), MASK2)
          MY = iand(ior(MY, ishft(MY,  4)), MASK3)
          MY = iand(ior(MY, ishft(MY,  2)), MASK4)
          MY = iand(ior(MY, ishft(MY,  1)), MASK5)

          MortonPad(I) = ior(ishft(MY, 1), MX)
       else
          IndicesPad(I) = HUGE(1_int64)
          MortonPad(I)  = HUGE(1_int64)
       end if
    end do

    K = 2
    do while (K <= M)
       J = K / 2
       do while (J > 0)

          !$omp target teams distribute parallel do private(I, I0, IXJ0, Dir, TempM, TempIdx)
          do I = 1, M
             I0 = I - 1
             IXJ0 = ieor(I0, J)

             if (I0 < IXJ0) then
                Dir = (iand(I0, K) == 0)

                if (Dir .eqv. (MortonPad(I) > MortonPad(IXJ0 + 1))) then
                   ! Swap Morton Codes
                   TempM = MortonPad(I)
                   MortonPad(I) = MortonPad(IXJ0 + 1)
                   MortonPad(IXJ0 + 1) = TempM

                   ! Swap Indices
                   TempIdx = IndicesPad(I)
                   IndicesPad(I) = IndicesPad(IXJ0 + 1)
                   IndicesPad(IXJ0 + 1) = TempIdx
                end if
             end if
          end do

          J = J / 2
       end do
       K = K * 2
    end do

    !$omp target teams distribute parallel do private(I)
    do I = 1, N
       SortedIndices(I) = IndicesPad(I)
    end do

    !$omp target exit data map(from: SortedIndices(1:N)) &
    !$omp map(release: Boxes(1:N)) map(delete: IndicesPad(1:M), MortonPad(1:M))
    !> There is a good opportunity to keep the boxes on the GPU, then
    !> do not delete these here
    deallocate(IndicesPad)
    deallocate(MortonPad)

  end subroutine SortBoxesIndirect
  subroutine merge_sort_events_gpu(arr)
    ! Note: Removed 'pure' prefix as OpenMP target directives perform 
    ! hardware level synchronization and memory allocations.

    ! The 'contiguous' keyword is highly recommended for GPU offload to prevent 
    ! performance hits or mapping failures from strided array slices.
    type(Event), intent(inout), contiguous :: arr(:)
    type(Event), allocatable :: temp(:)

    integer :: n, width, i
    integer :: l, m, r, p1, p2, k

    n = size(arr)
    if (n <= 1) return

    ! Allocate temporary workspace array needed for Merge Sort
    allocate(temp(n))

    ! Map the data to the device ONCE for the entire sorting process
    ! arr is copied to the GPU at the start, and back to the CPU at the end.
    ! temp is allocated purely in device memory.
    !$omp target data map(tofrom: arr) map(alloc: temp)

    width = 1
    do while (width < n)

       ! Farm out the independent merges across GPU teams and threads
       ! Variables updated inside the loop MUST be marked as private
       !$omp target teams distribute parallel do private(l, m, r, p1, p2, k)
       do i = 1, n, 2 * width
          l = i
          m = min(i + width - 1, n)
          r = min(i + 2 * width - 1, n)

          p1 = l
          p2 = m + 1
          k  = l

          ! Merge the two sorted halves into the temp array
          do while (p1 <= m .and. p2 <= r)
             ! <= maintains stable sorting, keeping relative order of equal x's
             if (arr(p1)%x <= arr(p2)%x) then
                temp(k) = arr(p1)
                p1 = p1 + 1
             else
                temp(k) = arr(p2)
                p2 = p2 + 1
             end if
             k = k + 1
          end do

          ! Exhaust left half (if any elements remain)
          do while (p1 <= m)
             temp(k) = arr(p1)
             p1 = p1 + 1
             k = k + 1
          end do

          ! Exhaust right half (if any elements remain)
          do while (p2 <= r)
             temp(k) = arr(p2)
             p2 = p2 + 1
             k = k + 1
          end do
       end do

       ! Synchronize and copy the merged segments back to the main array
       !$omp target teams distribute parallel do
       do i = 1, n
          arr(i) = temp(i)
       end do

       ! Double the merge width for the next pass
       width = width * 2
    end do

    !$omp end target data

    deallocate(temp)
  end subroutine merge_sort_events_gpu

end module MortonSortOMT
