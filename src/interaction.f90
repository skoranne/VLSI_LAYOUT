! File   : interaction.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: The RTree for GPU has to be thought from groudup in NVFORTRAN GPU
!        : as there are several challenges, such as non-allocate, no procs.
module RTreeBuilderGPU
  use iso_fortran_env, only: int8, int32, int64, real64
  use iso_c_binding, only: c_loc, c_int32_t, c_size_t
  use omp_lib
  use CommonModule
  use Utilities
  use GeometryModule
  !use DesignModule
  use DataStructuresModule
  use RTreeBuilder
  use MortonSortOMT, only: thrust_exclusive_scan_i64
  use DeviceRegistryModule, only: DevicePrepare
  implicit none
  private

  public :: prep_boxes, prep_nodes, prep_i64, prep_i8, prep_logical
  public :: gpu_offload_enabled, ComputeInteractionsGPU, CertifyHealingGPU, &
       ComputeANDPairsCountGPU, ComputeANDPairsFillGPU, ComputeANDPairsGPU, &
       ComputeTopoSelectGPU, ComputeMergeEdgesGPU, ComputeNOTGPU

  integer, parameter :: K_MAX_TREE_DEPTH = 1024
  integer(kind=int64), parameter, public :: K_GPU_MIN_BOXES = 4096_int64
  !> Per-thread DFS stack depth for R-tree traversal kernels. BuildRTree produces a balanced
  !> tree with fanout <= K_LEAF_CAPACITY (16); a DFS that pops one node and pushes at most 16
  !> children needs at most 1 + 15*L slots for L internal levels. 160 covers L <= 10, i.e.
  !> 16^10 leaves (> 10^13 boxes). Kernels still guard every push and raise an overflow flag.
  integer(kind=int32), parameter, public :: K_GPU_DFS_STACK = 160_int32
  !> Per-thread fragment capacity of the GPU NOT kernel (ping-pong, 2 x K x 4 int32 coords).
  !> A box of A whose remainder needs more fragments (or a deeper DFS) is flagged and
  !> recomputed on the CPU, so the cap only trades GPU coverage for register/local pressure.
  integer(kind=int32), parameter, public :: K_GPU_NOT_FRAGS = 32_int32


contains

  !> DevicePrepare helpers: call for every host array a wrapper maps, right before mapping it.
  !> Drops any persistent-residency mapping overlapping the array unless it is the exact,
  !> validated-in-this-scope copy (see device_registry.f90). Cheap: O(#registered ranges).
  subroutine prep_boxes(a, n)
    integer(kind=int64), intent(in) :: n
    type(Box), intent(in), target :: a(*)
    if (n > 0_int64) call DevicePrepare(c_loc(a(1)), int(n, c_size_t) * int(storage_size(a(1))/8, c_size_t))
  end subroutine prep_boxes
  subroutine prep_nodes(a, n)
    integer(kind=int64), intent(in) :: n
    type(RTreeNode), intent(in), target :: a(*)
    if (n > 0_int64) call DevicePrepare(c_loc(a(1)), int(n, c_size_t) * int(storage_size(a(1))/8, c_size_t))
  end subroutine prep_nodes
  subroutine prep_i64(a, n)
    integer(kind=int64), intent(in) :: n
    integer(kind=int64), intent(in), target :: a(*)
    if (n > 0_int64) call DevicePrepare(c_loc(a(1)), int(n, c_size_t) * 8_c_size_t)
  end subroutine prep_i64
  subroutine prep_i8(a, n)
    integer(kind=int64), intent(in) :: n
    integer(kind=int8), intent(in), target :: a(*)
    if (n > 0_int64) call DevicePrepare(c_loc(a(1)), int(n, c_size_t))
  end subroutine prep_i8
  subroutine prep_logical(a, n)
    integer(kind=int64), intent(in) :: n
    logical, intent(in), target :: a(*)
    if (n > 0_int64) call DevicePrepare(c_loc(a(1)), int(n, c_size_t) * int(storage_size(a(1))/8, c_size_t))
  end subroutine prep_logical

  !> First-touch helpers. nsys on GB10 showed device-to-host copies into freshly allocated
  !> (never touched) host arrays running at ~0.1 GB/s: the driver faults the destination in
  !> page by page during the DMA. Touching the pages first, in parallel on the CPU, makes the
  !> same copy run at full speed (44 MB: ~250 ms -> ~7 ms). Use before every map(from:) into a
  !> newly allocated array.
  subroutine first_touch_boxes(a, n)
    integer(kind=int64), intent(in) :: n
    type(Box), intent(inout) :: a(*)
    integer(kind=int64) :: i
    !$omp parallel do if(n > 65536_int64) schedule(static)
    do i = 1, n
       a(i)%X1 = 0; a(i)%Y1 = 0; a(i)%X2 = 0; a(i)%Y2 = 0
    end do
  end subroutine first_touch_boxes
  subroutine first_touch_i64(a, n)
    integer(kind=int64), intent(in) :: n
    integer(kind=int64), intent(inout) :: a(*)
    integer(kind=int64) :: i
    !$omp parallel do if(n > 131072_int64) schedule(static)
    do i = 1, n
       a(i) = 0_int64
    end do
  end subroutine first_touch_i64
  subroutine first_touch_i8(a, n)
    integer(kind=int64), intent(in) :: n
    integer(kind=int8), intent(inout) :: a(*)
    integer(kind=int64) :: i
    !$omp parallel do if(n > 1048576_int64) schedule(static)
    do i = 1, n
       a(i) = 0_int8
    end do
  end subroutine first_touch_i8

  !> Runtime GPU offload policy:
  !> - Returns .false. on non-GPU builds, if no OpenMP target device exists, or if OMP_TARGET_OFFLOAD=DISABLED.
  !> - Returns .true. for any n_work > 0 if OMP_TARGET_OFFLOAD=MANDATORY (ensures small regressions test GPU paths).
  !> - Otherwise requires n_work >= threshold to avoid H2D/launch overhead on tiny layers.
  logical function gpu_offload_enabled(n_work, threshold) result(use_gpu)
    integer(kind=int64), intent(in) :: n_work
    integer(kind=int64), intent(in), optional :: threshold
    integer(kind=int64) :: min_work
#if defined(_CUDA) || defined(__NVCOMPILER) || defined(__NVCOMPILER_LLVM__)
    character(len=32) :: offload_env
    integer :: offload_len, offload_stat
#endif

    use_gpu = .false.
    if (n_work <= 0_int64) return
    min_work = K_GPU_MIN_BOXES
    if (present(threshold)) min_work = threshold

#if defined(_CUDA) || defined(__NVCOMPILER) || defined(__NVCOMPILER_LLVM__)
    if (omp_get_num_devices() <= 0) return
    ! Inside a host parallel region (per-tile workers of the frame functions) offload only
    ! tiles big enough to pay for it, whatever OMP_TARGET_OFFLOAD says: thousands of tiny
    ! tiles were each launching kernels from worker threads.
    if (omp_in_parallel()) then
       use_gpu = (n_work >= max(min_work, K_GPU_MIN_BOXES))
       return
    end if
    call get_environment_variable('OMP_TARGET_OFFLOAD', offload_env, length=offload_len, status=offload_stat)
    if (offload_stat == 0 .and. offload_len > 0) then
       if (trim(offload_env) == 'DISABLED' .or. trim(offload_env) == 'disabled') then
          use_gpu = .false.
          return
       else if (trim(offload_env) == 'MANDATORY' .or. trim(offload_env) == 'mandatory') then
          use_gpu = .true.
          return
       end if
    end if
    use_gpu = (n_work >= min_work)
#endif
  end function gpu_offload_enabled
#ifdef WORK_IN_PROGRESS
  subroutine PerformMergeGPU_OMP(sorted_boxes, tree_nodes, root_index, max_expected_edges, &
       overlap_area, overlap_perimeter, area_overlap_roots)
    ! ... (Standard intent(in) declarations) ...
    type(Box), intent(in)           :: sorted_boxes(:)
    type(RTreeNode), intent(in)     :: tree_nodes(:)
    integer(kind=int64), intent(in) :: root_index
    integer(kind=int64), intent(in) :: max_expected_edges

    real(kind=real64), intent(out)  :: overlap_area
    real(kind=real64), intent(out)  :: overlap_perimeter
    integer(kind=int64), allocatable, intent(out) :: area_overlap_roots(:)

    ! --- Kernel-Local Variables ---
    integer(kind=int64) :: num_boxes, tree_size, i, k, idx, unique_count, j

    ! Keep stack small for GPU registers
    integer(kind=int64), parameter :: K_MAX_DEPTH = 64
    integer(kind=int64) :: Stack(K_MAX_DEPTH), StackPtr, curr_index, child_idx

    logical         :: overlapx, overlapy
    type(RTreeNode) :: currNode, childNode
    type(Box)       :: tempBox, boxI, boxK

    ! --- GPU Global Edge Buffers ---
    integer(kind=int64), allocatable :: d_edge_i(:), d_edge_k(:)
    integer(kind=int64) :: edge_count

    ! CPU-side UnionFind
    type(UnionFind) :: uf
    integer(kind=int64), allocatable :: raw_roots(:)

    num_boxes = size(sorted_boxes, kind=int64)
    tree_size = size(tree_nodes, kind=int64)
    overlap_area = 0.0_real64
    overlap_perimeter = 0.0_real64
    edge_count = 0

    if(num_boxes <= 1) then
       allocate(area_overlap_roots(0))
       return
    end if

    ! 1. Pre-allocate the massive flat buffer
    allocate(d_edge_i(max_expected_edges), d_edge_k(max_expected_edges))

    ! 2. OpenMP Target Data Region
    ! We explicitly map the array bounds to ensure the runtime copies the memory correctly to the device.
    !$omp target data map(to: sorted_boxes(1:num_boxes), tree_nodes(1:tree_size)) &
    !$omp map(alloc: d_edge_i(1:max_expected_edges), d_edge_k(1:max_expected_edges)) &
    !$omp map(tofrom: edge_count)

    ! 3. The Kernel: Map to GPU thread blocks/grids
    ! teams distribute parallel do is the OpenMP equivalent of mapping to GPU streaming multiprocessors
    !$omp target teams distribute parallel do &
    !$omp private(Stack, StackPtr, curr_index, child_idx, boxI, boxK, tempBox, overlapx, overlapy, currNode, childNode, idx) &
    !$omp reduction(+:overlap_area, overlap_perimeter) map(tofrom: overlap_area, overlap_perimeter)
    do i = 1, num_boxes
       boxI = sorted_boxes(i)

       overlapx = max(tree_nodes(root_index)%mbr%x1, boxI%x1) <= min(tree_nodes(root_index)%mbr%x2, boxI%x2)
       overlapy = max(tree_nodes(root_index)%mbr%y1, boxI%y1) <= min(tree_nodes(root_index)%mbr%y2, boxI%y2)

       if (overlapx .and. overlapy) then
          StackPtr = 1
          Stack(StackPtr) = root_index

          do while (StackPtr > 0)
             curr_index = Stack(StackPtr)
             StackPtr = StackPtr - 1
             currNode = tree_nodes(curr_index)

             if (currNode%IsLeaf) then
                do k = currNode%ChildStart, currNode%ChildStart + currNode%NumChildren - 1
                   if (k <= i) cycle
                   boxK = sorted_boxes(k)

                   overlapx = max(boxI%x1, boxK%x1) <= min(boxI%x2, boxK%x2)
                   overlapy = max(boxI%y1, boxK%y1) <= min(boxI%y2, boxK%y2)

                   if (overlapx .and. overlapy) then
                      tempBox%x1 = max(boxI%x1, boxK%x1)
                      tempBox%y1 = max(boxI%y1, boxK%y1)
                      tempBox%x2 = min(boxI%x2, boxK%x2)
                      tempBox%y2 = min(boxI%y2, boxK%y2)

                      if (box_area(tempBox) > 0.0) then
                         overlap_area = overlap_area + box_area(tempBox)

                         ! --- THE GPU TRICK: OpenMP Atomic Capture ---
                         ! Safely increments the global counter and captures it for this specific thread.
                         !$omp atomic capture
                         edge_count = edge_count + 1
                         idx = edge_count
                         !$omp end atomic

                         ! Guard against buffer overflow
                         if (idx <= max_expected_edges) then
                            d_edge_i(idx) = i
                            d_edge_k(idx) = k
                         end if
                      else
                         overlap_perimeter = overlap_perimeter + box_perimeter(tempBox)
                      end if
                   end if
                end do
             else
                do child_idx = currNode%ChildStart, currNode%ChildStart + currNode%NumChildren - 1
                   childNode = tree_nodes(child_idx)
                   overlapx = max(childNode%mbr%x1, boxI%x1) <= min(childNode%mbr%x2, boxI%x2)
                   overlapy = max(childNode%mbr%y1, boxI%y1) <= min(childNode%mbr%y2, boxI%y2)

                   if (overlapx .and. overlapy) then
                      if (StackPtr < K_MAX_DEPTH) then 
                         StackPtr = StackPtr + 1
                         Stack(StackPtr) = child_idx
                      end if
                   end if
                end do
             end if
          end do
       end if
    end do

    ! 4. Retrieve edge count, then only the populated portion of edge buffers (minimize D2H)
    !$omp target update from(edge_count)
    edge_count = min(edge_count, max_expected_edges)
    if (edge_count > 0) then
       !$omp target update from(d_edge_i(1:edge_count), d_edge_k(1:edge_count))
    end if

    !$omp end target data

    ! ---------------------------------------------------------
    ! CPU Fallback: Fast Sparse Union-Find & Root Extraction
    ! (Exactly the same logic as before)
    ! ---------------------------------------------------------

    if (abs(overlap_area) > K_SMALL_EPSILON) overlap_perimeter = 0.0

    call uf%init(num_boxes)
    do idx = 1, edge_count
       call uf%merge(d_edge_i(idx), d_edge_k(idx))
    end do

    call uf%fullreduce()

    if (edge_count > 0) then
       allocate(raw_roots(edge_count * 2))

       do idx = 1, edge_count
          raw_roots(idx*2 - 1) = uf%arr(d_edge_i(idx))
          raw_roots(idx*2)     = uf%arr(d_edge_k(idx))
       end do

       call sort_int64(raw_roots)

       unique_count = 1
       do j = 2, size(raw_roots, kind=int64)
          if (raw_roots(j) /= raw_roots(unique_count)) then
             unique_count = unique_count + 1
             raw_roots(unique_count) = raw_roots(j)
          end if
       end do

       allocate(area_overlap_roots(unique_count))
       area_overlap_roots(1:unique_count) = raw_roots(1:unique_count)
    else
       allocate(area_overlap_roots(0))
    end if

  end subroutine PerformMergeGPU_OMP
#endif
#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)    
  !$omp declare target
#endif
  subroutine DO_INTERACTION(I, J)
    integer(kind=int64) :: I, J
    ! Interaction logic goes here
    !$komp atomic
    !count_interactions = count_interactions + 1
  end subroutine DO_INTERACTION

  !> GPU Offloaded All-Query Interaction Generator
  subroutine ComputeInteractionsGPU(TreeNodes, NumNodes, SortedBoxes, NumBoxes, RootIndex, KernelCount, OverflowFlag)
    ! 1. The exact sizes MUST come first
    integer(kind=int64), intent(in) :: NumNodes, NumBoxes

    ! 2. Force Explicit-Shape (No colons allowed!)
    type(RTreeNode), intent(in) :: TreeNodes(NumNodes)
    type(Box), intent(in) :: SortedBoxes(NumBoxes)

    integer(kind=int64), intent(in) :: RootIndex
    integer(kind=int64), intent(out) :: KernelCount
    !> Set to 1 if any traversal overflowed its stack (KernelCount then incomplete).
    !> If absent and an overflow occurs, the routine stops rather than return a wrong count.
    integer(kind=int32), intent(out), optional :: OverflowFlag
    integer(kind=int32) :: StackOverflow

    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode

    ! 3. Stack size of 512 is now massive overkill, which is a good thing.
    integer(kind=int64), parameter :: K_STACK_SIZE = int(K_GPU_DFS_STACK, kind=int64)
    integer(kind=int64) :: Stack(K_STACK_SIZE) 
    integer(kind=int64) :: StackPtr

    type(Box) :: QBox, NodeMbr, TargetBox
    logical :: OverlapX, OverlapY

    KernelCount = 0
    StackOverflow = 0_int32
    if (present(OverflowFlag)) OverflowFlag = 0_int32
    if (NumBoxes <= 0_int64 .or. NumNodes <= 0_int64 .or. RootIndex <= 0_int64) return

    ! Map the explicit bounds (no transfer if the layer is already device-resident)
    call prep_nodes(TreeNodes, NumNodes); call prep_boxes(SortedBoxes, NumBoxes)
    !$omp target enter data map(to: TreeNodes(1:NumNodes), SortedBoxes(1:NumBoxes))

    ! Launch kernel (explicit tofrom so the flag is not firstprivate)
    !$omp target teams distribute parallel do map(tofrom: StackOverflow) &
    !$omp private(I, J, K, ChildIdx, CurrNode, Stack, StackPtr, &
    !$omp         QBox, NodeMbr, TargetBox, OverlapX, OverlapY) &
    !$omp reduction(+:KernelCount)
    do I = 1, NumBoxes
       QBox = SortedBoxes(I)

       ! =======================================================
       ! Pre-Check the Root Node so we don't push it blindly
       ! =======================================================
       NodeMbr = TreeNodes(RootIndex)%Mbr
       OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
       if (OverlapX) then
          OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)
       else
          OverlapY = .false.
       end if

       ! Prune immediately if the root itself does not overlap
       if (.not. (OverlapX .and. OverlapY)) cycle

       ! Initialize static stack
       StackPtr = 1
       Stack(StackPtr) = RootIndex

       ! Iterative Depth-First Search
       do while (StackPtr > 0)
          CurrNode = Stack(StackPtr)
          StackPtr = StackPtr - 1

          ! Because we pre-check, we KNOW CurrNode already overlaps.
          ! We no longer need to check its MBR here.

          if (TreeNodes(CurrNode)%IsLeaf) then
             ! Leaf Node: Check exact geometry intersections
             do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                J = TreeNodes(CurrNode)%ChildStart + K

                if (J <= I) cycle                
                TargetBox = SortedBoxes(J)

                ! Inline Short-circuit Check
                OverlapX = max(TargetBox%X1, QBox%X1) < min(TargetBox%X2, QBox%X2) 
                if (OverlapX) then
                   OverlapY = max(TargetBox%Y1, QBox%Y1) < min(TargetBox%Y2, QBox%Y2) 

                   ! If interaction found, call the procedure
                   if (OverlapY) then
                      call DO_INTERACTION(I, J)
                      KernelCount = KernelCount + 1
                   end if
                end if
             end do
          else
             ! =======================================================
             ! Internal Node: ONLY push children that actually overlap!
             ! =======================================================
             do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                NodeMbr = TreeNodes(ChildIdx)%Mbr

                OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
                if (OverlapX) then
                   OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)

                   if (OverlapY) then
                      ! Bounded push: never index past the private stack. On overflow the
                      ! count is incomplete, so flag it (wrapper reports / stops).
                      if (StackPtr < K_STACK_SIZE) then
                         StackPtr = StackPtr + 1
                         Stack(StackPtr) = ChildIdx
                      else
                         !$omp atomic write
                         StackOverflow = 1_int32
                      end if
                   end if
                end if
             end do
          end if
       end do
    end do
    !$omp target exit data map(release:  TreeNodes(1:NumNodes), SortedBoxes(1:NumBoxes))
    if (StackOverflow /= 0_int32) then
       if (present(OverflowFlag)) then
          OverflowFlag = 1_int32
       else
          error stop "GPU R-tree traversal stack overflow (K_GPU_DFS_STACK): result would be incomplete"
       end if
    end if

  end subroutine ComputeInteractionsGPU

  !> GPU Offloaded Self checker
  !function ComputeSelfCheckGPULayer( input_layer ) result( retval )
  !  type(Layer), intent(inout) :: input_layer
  !  integer(kind=int64)        :: self_check_count
  !  logical                    :: retval
  !  self_check_count = 0
  !  call ComputeSelfCheckGPU( input_layer%tree%tree_nodes,size(input_layer%tree%tree_nodes,kind=int64),&
  !       input_layer%layer_boxes, input_layer%n_used, input_layer%tree%root_index, self_check_count)
  !  
  !  retval = self_check_count == input_layer%n_used
  !end function ComputeSelfCheckGPULayer
  
  subroutine ComputeSelfCheckGPU(TreeNodes, NumNodes, SortedBoxes, NumBoxes, RootIndex, KernelCount, OverflowFlag)
    ! 1. The exact sizes MUST come first
    integer(kind=int64), intent(in) :: NumNodes, NumBoxes

    ! 2. Force Explicit-Shape (No colons allowed!)
    type(RTreeNode), intent(in) :: TreeNodes(NumNodes)
    type(Box), intent(in) :: SortedBoxes(NumBoxes)

    integer(kind=int64), intent(in) :: RootIndex
    integer(kind=int64), intent(out) :: KernelCount
    !> Set to 1 if any traversal overflowed its stack (KernelCount then incomplete).
    !> If absent and an overflow occurs, the routine stops rather than return a wrong count.
    integer(kind=int32), intent(out), optional :: OverflowFlag
    integer(kind=int32) :: StackOverflow

    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode

    ! 3. Stack size of 512 is now massive overkill, which is a good thing.
    integer(kind=int64), parameter :: K_STACK_SIZE = int(K_GPU_DFS_STACK, kind=int64)
    integer(kind=int64) :: Stack(K_STACK_SIZE) 
    integer(kind=int64) :: StackPtr

    type(Box) :: QBox, NodeMbr, TargetBox
    logical :: OverlapX, OverlapY

    KernelCount = 0
    StackOverflow = 0_int32
    if (present(OverflowFlag)) OverflowFlag = 0_int32
    if (NumBoxes <= 0_int64 .or. NumNodes <= 0_int64 .or. RootIndex <= 0_int64) return

    ! Map the explicit bounds (no transfer if the layer is already device-resident)
    call prep_nodes(TreeNodes, NumNodes); call prep_boxes(SortedBoxes, NumBoxes)
    !$omp target enter data map(to: TreeNodes(1:NumNodes), SortedBoxes(1:NumBoxes))

    ! Launch kernel (explicit tofrom so the flag is not firstprivate)
    !$omp target teams distribute parallel do map(tofrom: StackOverflow) &
    !$omp private(I, J, K, ChildIdx, CurrNode, Stack, StackPtr, &
    !$omp         QBox, NodeMbr, TargetBox, OverlapX, OverlapY) &
    !$omp reduction(+:KernelCount)
    do I = 1, NumBoxes
       QBox = SortedBoxes(I)

       ! =======================================================
       ! Pre-Check the Root Node so we don't push it blindly
       ! =======================================================
       NodeMbr = TreeNodes(RootIndex)%Mbr
       OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
       if (OverlapX) then
          OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)
       else
          OverlapY = .false.
       end if

       ! Prune immediately if the root itself does not overlap
       if (.not. (OverlapX .and. OverlapY)) cycle

       ! Initialize static stack
       StackPtr = 1
       Stack(StackPtr) = RootIndex

       ! Iterative Depth-First Search
       do while (StackPtr > 0)
          CurrNode = Stack(StackPtr)
          StackPtr = StackPtr - 1

          ! Because we pre-check, we KNOW CurrNode already overlaps.
          ! We no longer need to check its MBR here.

          if (TreeNodes(CurrNode)%IsLeaf) then
             ! Leaf Node: Check exact geometry intersections
             do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                J = TreeNodes(CurrNode)%ChildStart + K

                if (J == I) then
                   KernelCount = KernelCount + 1
                   cycle
                end if
             end do
          else
             ! =======================================================
             ! Internal Node: ONLY push children that actually overlap!
             ! =======================================================
             do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                NodeMbr = TreeNodes(ChildIdx)%Mbr

                OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
                if (OverlapX) then
                   OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)

                   if (OverlapY) then
                      ! Bounded push: never index past the private stack. On overflow the
                      ! count is incomplete, so flag it (wrapper reports / stops).
                      if (StackPtr < K_STACK_SIZE) then
                         StackPtr = StackPtr + 1
                         Stack(StackPtr) = ChildIdx
                      else
                         !$omp atomic write
                         StackOverflow = 1_int32
                      end if
                   end if
                end if
             end do
          end if
       end do
    end do
    !$omp target exit data map(release:  TreeNodes(1:NumNodes), SortedBoxes(1:NumBoxes))
    if (StackOverflow /= 0_int32) then
       if (present(OverflowFlag)) then
          OverflowFlag = 1_int32
       else
          error stop "GPU R-tree traversal stack overflow (K_GPU_DFS_STACK): result would be incomplete"
       end if
    end if

  end subroutine ComputeSelfCheckGPU

  
  !> GPU Offloaded All-Query Interaction Generator (With Fast Abort)
  logical function CertifyHealingGPU(TreeNodes, NumNodes, SortedBoxes, NumBoxes, RootIndex) result(IsValid)
    ! 1. The exact sizes MUST come first
    integer(kind=int64), intent(in) :: NumNodes, NumBoxes

    ! 2. Force Explicit-Shape (No colons allowed!)
    type(RTreeNode), intent(in) :: TreeNodes(NumNodes)
    type(Box), intent(in) :: SortedBoxes(NumBoxes)

    integer(kind=int64), intent(in) :: RootIndex
    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode

    integer(kind=int32), parameter :: K_STACK_SIZE = 128_int32
    integer(kind=int32) :: Stack(K_STACK_SIZE), StackPtr
    integer(kind=int64) :: Width64, Height64

    type(Box) :: QBox, NodeMbr, TargetBox
    logical :: OverlapX, OverlapY
    integer(kind=K_COORDINATE_KIND) :: InterX1, InterX2, InterY1, InterY2
    ! GPU-safe global flag for short-circuiting across all threads
    integer :: GlobalAbortFlag, LocalAbort

    GlobalAbortFlag = 0
    IsValid = .true.
    if (NumBoxes < 2_int64 .or. NumNodes <= 0_int64) return

    ! Map the explicit bounds and the abort flag
    call prep_nodes(TreeNodes, NumNodes); call prep_boxes(SortedBoxes, NumBoxes)
    !$omp target data map(to: TreeNodes(1:NumNodes), SortedBoxes(1:NumBoxes)) &
    !$omp map(tofrom: GlobalAbortFlag)

    ! Launch kernel (explicit map(tofrom: GlobalAbortFlag) required so scalar is not firstprivate!)
    !$omp target teams distribute parallel do map(tofrom: GlobalAbortFlag) &
    !$omp private(I, J, K, ChildIdx, CurrNode, Stack, StackPtr, &
    !$omp         InterX1, InterX2, InterY1, InterY2, Width64, Height64, &
    !$omp         QBox, NodeMbr, TargetBox, OverlapX, OverlapY, LocalAbort)
    do I = 1, NumBoxes
       !$omp atomic read
       LocalAbort = GlobalAbortFlag
       if (LocalAbort /= 0) cycle

       QBox = SortedBoxes(I)

       ! Pre-Check the Root Node
       NodeMbr = TreeNodes(RootIndex)%Mbr
       OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
       if (OverlapX) then
          OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)
       else
          OverlapY = .false.
       end if

       if (.not. (OverlapX .and. OverlapY)) cycle

       StackPtr = 1_int32
       Stack(1) = int(RootIndex, kind=int32)

       do while (StackPtr > 0_int32)
          CurrNode = int(Stack(StackPtr), kind=int64)
          StackPtr = StackPtr - 1_int32

          if (TreeNodes(CurrNode)%IsLeaf) then
             ! Poll the global abort flag only when doing leaf work (cheaper than every pop).
             !$omp atomic read
             LocalAbort = GlobalAbortFlag
             if (LocalAbort /= 0) exit
             do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                J = TreeNodes(CurrNode)%ChildStart + K
                if (J <= I) cycle
                TargetBox = SortedBoxes(J)

                InterX1 = max(TargetBox%X1, QBox%X1)
                InterX2 = min(TargetBox%X2, QBox%X2)
                if (InterX2 > InterX1) then
                   InterY1 = max(TargetBox%Y1, QBox%Y1)
                   InterY2 = min(TargetBox%Y2, QBox%Y2)
                   Width64  = int(InterX2, kind=int64) - int(InterX1, kind=int64)
                   Height64 = int(InterY2, kind=int64) - int(InterY1, kind=int64)
                   if (Width64 > 0_int64 .and. Height64 > 0_int64) then
                      !$omp atomic write
                      GlobalAbortFlag = 1
                      StackPtr = 0_int32
                      exit
                   end if
                end if
             end do
          else
             do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                NodeMbr = TreeNodes(ChildIdx)%Mbr
                if (NodeMbr%X1 <= QBox%X2 .and. NodeMbr%X2 >= QBox%X1 .and. &
                    NodeMbr%Y1 <= QBox%Y2 .and. NodeMbr%Y2 >= QBox%Y1) then
                   if (StackPtr < K_STACK_SIZE) then
                      StackPtr = StackPtr + 1_int32
                      Stack(StackPtr) = int(ChildIdx, kind=int32)
                   else
                      !$omp atomic write
                      GlobalAbortFlag = 1
                      StackPtr = 0_int32
                      exit
                   end if
                end if
             end do
          end if
       end do
    end do
    !$omp end target data

    ! If overlap or stack overflow was triggered on the GPU, return false (safe conservative)
    if (GlobalAbortFlag /= 0) IsValid = .false.

  end function CertifyHealingGPU

  !> GPU-accelerated AND: count finite-area intersections of A boxes against B RTree.
  !> Produces per-A counts and total count for exact output allocation.
  subroutine ComputeANDPairsCountGPU(TreeNodes, NumNodes, SortedBoxesB, NumBoxesB, RootIndex, &
       BoxesA, NumBoxesA, Counts, TotalCount)
    integer(kind=int64), intent(in) :: NumNodes, NumBoxesB, NumBoxesA
    type(RTreeNode), intent(in)     :: TreeNodes(NumNodes)
    type(Box), intent(in)           :: SortedBoxesB(NumBoxesB)
    integer(kind=int64), intent(in) :: RootIndex
    type(Box), intent(in)           :: BoxesA(NumBoxesA)
    integer(kind=int64), intent(out):: Counts(NumBoxesA)
    integer(kind=int64), intent(out):: TotalCount

    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode, LocalCount
    integer(kind=int64), parameter :: K_STACK_SIZE = int(K_GPU_DFS_STACK, kind=int64)
    integer(kind=int64) :: Stack(K_STACK_SIZE)
    integer(kind=int64) :: StackPtr
    type(Box) :: QBox, NodeMbr, TargetBox
    logical :: OverlapX, OverlapY
    integer(kind=K_COORDINATE_KIND) :: ix1, iy1, ix2, iy2
    integer(kind=int32) :: StackOverflow

    StackOverflow = 0_int32
    Counts = 0_int64
    TotalCount = 0_int64
    if (NumBoxesA <= 0 .or. NumBoxesB <= 0 .or. NumNodes <= 0) return

    call prep_nodes(TreeNodes, NumNodes); call prep_boxes(SortedBoxesB, NumBoxesB)
    call prep_boxes(BoxesA, NumBoxesA); call prep_i64(Counts, NumBoxesA)
    !$omp target enter data map(to: TreeNodes(1:NumNodes), SortedBoxesB(1:NumBoxesB), BoxesA(1:NumBoxesA))
    !$omp target enter data map(alloc: Counts(1:NumBoxesA))

    !$omp target teams distribute parallel do map(tofrom: StackOverflow) &
    !$omp private(I,J,K,ChildIdx,CurrNode,Stack,StackPtr,LocalCount,QBox,NodeMbr,TargetBox,OverlapX,OverlapY,ix1,iy1,ix2,iy2)
    do I = 1, NumBoxesA
       QBox = BoxesA(I)
       LocalCount = 0_int64

       NodeMbr = TreeNodes(RootIndex)%Mbr
       OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
       if (OverlapX) then
          OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)
       else
          OverlapY = .false.
       end if

       if (OverlapX .and. OverlapY) then
          StackPtr = 1
          Stack(StackPtr) = RootIndex
          do while (StackPtr > 0)
             CurrNode = Stack(StackPtr)
             StackPtr = StackPtr - 1
             if (TreeNodes(CurrNode)%IsLeaf) then
                do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                   J = TreeNodes(CurrNode)%ChildStart + K
                   TargetBox = SortedBoxesB(J)
                   OverlapX = max(TargetBox%X1, QBox%X1) < min(TargetBox%X2, QBox%X2)
                   if (OverlapX) then
                      OverlapY = max(TargetBox%Y1, QBox%Y1) < min(TargetBox%Y2, QBox%Y2)
                      if (OverlapY) then
                         ix1 = max(TargetBox%X1, QBox%X1); iy1 = max(TargetBox%Y1, QBox%Y1)
                         ix2 = min(TargetBox%X2, QBox%X2); iy2 = min(TargetBox%Y2, QBox%Y2)
                         if (ix2 > ix1 .and. iy2 > iy1) LocalCount = LocalCount + 1_int64
                      end if
                   end if
                end do
             else
                do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                   ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                   NodeMbr = TreeNodes(ChildIdx)%Mbr
                   OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
                   if (OverlapX) then
                      OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)
                      if (OverlapY) then
                         if (StackPtr < K_STACK_SIZE) then
                            StackPtr = StackPtr + 1
                            Stack(StackPtr) = ChildIdx
                         else
                            !$omp atomic write
                            StackOverflow = 1_int32
                         end if
                      end if
                   end if
                end do
             end if
          end do
       end if
       Counts(I) = LocalCount
    end do

    !$omp target update from(Counts(1:NumBoxesA))
    !$omp target exit data map(release: Counts(1:NumBoxesA))
    !$omp target exit data map(release: TreeNodes(1:NumNodes), SortedBoxesB(1:NumBoxesB), BoxesA(1:NumBoxesA))

    do I = 1, NumBoxesA
       TotalCount = TotalCount + Counts(I)
    end do
    ! Stack overflow => counts incomplete; signal with a negative total (caller must use CPU path).
    if (StackOverflow /= 0_int32) TotalCount = -1_int64
  end subroutine ComputeANDPairsCountGPU

  !> GPU-accelerated AND fill: generate intersection boxes using per-A offsets.
  !> Offsets(I) must contain starting index-1 for A box I results (0-based in result array conceptually).
  subroutine ComputeANDPairsFillGPU(TreeNodes, NumNodes, SortedBoxesB, NumBoxesB, RootIndex, &
       BoxesA, NumBoxesA, Offsets, OutBoxes, OutCount)
    integer(kind=int64), intent(in) :: NumNodes, NumBoxesB, NumBoxesA, OutCount
    type(RTreeNode), intent(in)     :: TreeNodes(NumNodes)
    type(Box), intent(in)           :: SortedBoxesB(NumBoxesB)
    integer(kind=int64), intent(in) :: RootIndex
    type(Box), intent(in)           :: BoxesA(NumBoxesA)
    integer(kind=int64), intent(in) :: Offsets(NumBoxesA)
    type(Box), intent(out)          :: OutBoxes(OutCount)

    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode, WriteIdx, Base
    integer(kind=int64), parameter :: K_STACK_SIZE = int(K_GPU_DFS_STACK, kind=int64)
    integer(kind=int64) :: Stack(K_STACK_SIZE)
    integer(kind=int64) :: StackPtr
    type(Box) :: QBox, NodeMbr, TargetBox
    logical :: OverlapX, OverlapY
    integer(kind=K_COORDINATE_KIND) :: ix1, iy1, ix2, iy2

    if (OutCount <= 0 .or. NumBoxesA <= 0 .or. NumBoxesB <= 0 .or. NumNodes <= 0) return

    call prep_nodes(TreeNodes, NumNodes); call prep_boxes(SortedBoxesB, NumBoxesB)
    call prep_boxes(BoxesA, NumBoxesA); call prep_i64(Offsets, NumBoxesA); call prep_boxes(OutBoxes, OutCount)
    !$omp target enter data map(to: TreeNodes(1:NumNodes), SortedBoxesB(1:NumBoxesB), BoxesA(1:NumBoxesA), Offsets(1:NumBoxesA))
    !$omp target enter data map(alloc: OutBoxes(1:OutCount))

    !$omp target teams distribute parallel do &
    !$omp private(I,J,K,ChildIdx,CurrNode,Stack,StackPtr,Base,WriteIdx,QBox,NodeMbr,TargetBox,OverlapX,OverlapY,ix1,iy1,ix2,iy2)
    do I = 1, NumBoxesA
       QBox = BoxesA(I)
       Base = Offsets(I)
       WriteIdx = Base

       NodeMbr = TreeNodes(RootIndex)%Mbr
       OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
       if (OverlapX) then
          OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)
       else
          OverlapY = .false.
       end if

       if (OverlapX .and. OverlapY) then
          StackPtr = 1
          Stack(StackPtr) = RootIndex
          do while (StackPtr > 0)
             CurrNode = Stack(StackPtr)
             StackPtr = StackPtr - 1
             if (TreeNodes(CurrNode)%IsLeaf) then
                do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                   J = TreeNodes(CurrNode)%ChildStart + K
                   TargetBox = SortedBoxesB(J)
                   OverlapX = max(TargetBox%X1, QBox%X1) < min(TargetBox%X2, QBox%X2)
                   if (OverlapX) then
                      OverlapY = max(TargetBox%Y1, QBox%Y1) < min(TargetBox%Y2, QBox%Y2)
                      if (OverlapY) then
                         ix1 = max(TargetBox%X1, QBox%X1); iy1 = max(TargetBox%Y1, QBox%Y1)
                         ix2 = min(TargetBox%X2, QBox%X2); iy2 = min(TargetBox%Y2, QBox%Y2)
                         if (ix2 > ix1 .and. iy2 > iy1) then
                            WriteIdx = WriteIdx + 1_int64
                            if (WriteIdx <= OutCount) then
                               OutBoxes(WriteIdx)%X1 = ix1; OutBoxes(WriteIdx)%Y1 = iy1
                               OutBoxes(WriteIdx)%X2 = ix2; OutBoxes(WriteIdx)%Y2 = iy2
                            end if
                         end if
                      end if
                   end if
                end do
             else
                do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                   ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                   NodeMbr = TreeNodes(ChildIdx)%Mbr
                   OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
                   if (OverlapX) then
                      OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)
                      if (OverlapY) then
                         if (StackPtr < K_STACK_SIZE) then
                            StackPtr = StackPtr + 1
                            Stack(StackPtr) = ChildIdx
                         end if
                      end if
                   end if
                end do
             end if
          end do
       end if
    end do

    !$omp target update from(OutBoxes(1:OutCount))
    !$omp target exit data map(release: OutBoxes(1:OutCount))
    !$omp target exit data map(release: TreeNodes(1:NumNodes), SortedBoxesB(1:NumBoxesB), BoxesA(1:NumBoxesA), Offsets(1:NumBoxesA))
  end subroutine ComputeANDPairsFillGPU

  !> Fused GPU-accelerated AND (single H2D upload of TreeNodes, SortedBoxesB, BoxesA for both count + fill).
  !> Uses int32(128) local traversal stack, skips zero-count boxes in pass 2, and reports stack overflow via OverflowFlag.
  subroutine ComputeANDPairsGPU(TreeNodes, NumNodes, SortedBoxesB, NumBoxesB, RootIndex, &
       BoxesA, NumBoxesA, OutBoxes, OutCount, OverflowFlag)
    integer(kind=int64), intent(in) :: NumNodes, NumBoxesB, NumBoxesA
    type(RTreeNode), intent(in)     :: TreeNodes(NumNodes)
    type(Box), intent(in)           :: SortedBoxesB(NumBoxesB)
    integer(kind=int64), intent(in) :: RootIndex
    type(Box), intent(in)           :: BoxesA(NumBoxesA)
    type(Box), allocatable, intent(out) :: OutBoxes(:)
    integer(kind=int64), intent(out):: OutCount
    integer(kind=int32), intent(out):: OverflowFlag

    integer(kind=int64), allocatable, target :: Counts(:), Offsets(:)
    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode, LocalCount, WriteIdx
    integer(kind=int32), parameter :: K_STACK_SIZE = 128_int32
    integer(kind=int32) :: Stack(K_STACK_SIZE), StackPtr
    integer :: alloc_status
    integer(c_int32_t) :: scan_rc
    type(Box) :: QBox, NodeMbr, TargetBox

    OutCount = 0_int64
    OverflowFlag = 0_int32
    if (NumBoxesA <= 0 .or. NumBoxesB <= 0 .or. NumNodes <= 0) then
       allocate(OutBoxes(0))
       return
    end if

    allocate(Counts(NumBoxesA), Offsets(NumBoxesA), stat=alloc_status)
    if (alloc_status /= 0) then
       OverflowFlag = 1_int32
       allocate(OutBoxes(0))
       return
    end if

    ! Single persistent device data region: Counts & Offsets stay resident on GPU (zero D2H/H2D roundtrip)
    call prep_nodes(TreeNodes, NumNodes); call prep_boxes(SortedBoxesB, NumBoxesB); call prep_boxes(BoxesA, NumBoxesA)
    call prep_i64(Counts, NumBoxesA); call prep_i64(Offsets, NumBoxesA)
    !$omp target data map(to: TreeNodes(1:NumNodes), SortedBoxesB(1:NumBoxesB), BoxesA(1:NumBoxesA)) &
    !$omp map(alloc: Counts(1:NumBoxesA), Offsets(1:NumBoxesA)) &
    !$omp map(tofrom: OverflowFlag)

    ! --- Pass 1: count intersections per A box ---
    !$omp target teams distribute parallel do map(tofrom: OverflowFlag) &
    !$omp private(I,J,K,ChildIdx,CurrNode,Stack,StackPtr,LocalCount,QBox,NodeMbr,TargetBox)
    do I = 1, NumBoxesA
       QBox = BoxesA(I)
       LocalCount = 0_int64
       NodeMbr = TreeNodes(RootIndex)%Mbr
       if (QBox%X1 < NodeMbr%X2 .and. QBox%X2 > NodeMbr%X1 .and. &
           QBox%Y1 < NodeMbr%Y2 .and. QBox%Y2 > NodeMbr%Y1) then
          StackPtr = 1_int32
          Stack(1) = int(RootIndex, kind=int32)
          do while (StackPtr > 0_int32)
             CurrNode = int(Stack(StackPtr), kind=int64)
             StackPtr = StackPtr - 1_int32
             if (TreeNodes(CurrNode)%IsLeaf) then
                do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                   J = TreeNodes(CurrNode)%ChildStart + K
                   TargetBox = SortedBoxesB(J)
                   if (TargetBox%X1 < QBox%X2 .and. TargetBox%X2 > QBox%X1 .and. &
                       TargetBox%Y1 < QBox%Y2 .and. TargetBox%Y2 > QBox%Y1) then
                      LocalCount = LocalCount + 1_int64
                   end if
                end do
             else
                do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                   ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                   NodeMbr = TreeNodes(ChildIdx)%Mbr
                   if (QBox%X1 < NodeMbr%X2 .and. QBox%X2 > NodeMbr%X1 .and. &
                       QBox%Y1 < NodeMbr%Y2 .and. QBox%Y2 > NodeMbr%Y1) then
                      if (StackPtr < K_STACK_SIZE) then
                         StackPtr = StackPtr + 1_int32
                         Stack(StackPtr) = int(ChildIdx, kind=int32)
                      else
                         !$omp atomic write
                         OverflowFlag = 1_int32
                      end if
                   end if
                end do
             end if
          end do
       end if
       Counts(I) = LocalCount
    end do

    !$omp target update from(OverflowFlag)
    if (OverflowFlag == 0_int32) then
       ! Device-side exclusive prefix sum via Thrust (eliminates D2H Counts + host scan + H2D Offsets)
       !$omp target data use_device_addr(Counts, Offsets)
       scan_rc = thrust_exclusive_scan_i64(c_loc(Counts(1)), c_loc(Offsets(1)), NumBoxesA, OutCount)
       !$omp end target data
       if (scan_rc /= 0_c_int32_t) then
          OverflowFlag = 1_int32
          OutCount = 0_int64
          allocate(OutBoxes(0))
       else if (OutCount > 0_int64) then
          allocate(OutBoxes(OutCount), stat=alloc_status)
          if (alloc_status /= 0) then
             OverflowFlag = 1_int32
             OutCount = 0_int64
             allocate(OutBoxes(0))
          else
             ! --- Pass 2: fill output boxes (Counts & Offsets already resident on GPU!) ---
             call first_touch_boxes(OutBoxes, OutCount)
             call prep_boxes(OutBoxes, OutCount)
             !$omp target data map(from: OutBoxes(1:OutCount))
             !$omp target teams distribute parallel do &
             !$omp private(I,J,K,ChildIdx,CurrNode,Stack,StackPtr,WriteIdx,QBox,NodeMbr,TargetBox)
             do I = 1, NumBoxesA
                if (Counts(I) == 0_int64) cycle
                QBox = BoxesA(I)
                WriteIdx = Offsets(I)
                StackPtr = 1_int32
                Stack(1) = int(RootIndex, kind=int32)
                do while (StackPtr > 0_int32)
                   CurrNode = int(Stack(StackPtr), kind=int64)
                   StackPtr = StackPtr - 1_int32
                   if (TreeNodes(CurrNode)%IsLeaf) then
                      do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                         J = TreeNodes(CurrNode)%ChildStart + K
                         TargetBox = SortedBoxesB(J)
                         if (TargetBox%X1 < QBox%X2 .and. TargetBox%X2 > QBox%X1 .and. &
                             TargetBox%Y1 < QBox%Y2 .and. TargetBox%Y2 > QBox%Y1) then
                            WriteIdx = WriteIdx + 1_int64
                            if (WriteIdx <= OutCount) then
                               OutBoxes(WriteIdx)%X1 = max(TargetBox%X1, QBox%X1)
                               OutBoxes(WriteIdx)%Y1 = max(TargetBox%Y1, QBox%Y1)
                               OutBoxes(WriteIdx)%X2 = min(TargetBox%X2, QBox%X2)
                               OutBoxes(WriteIdx)%Y2 = min(TargetBox%Y2, QBox%Y2)
                            end if
                         end if
                      end do
                   else
                      do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                         ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                         NodeMbr = TreeNodes(ChildIdx)%Mbr
                         if (QBox%X1 < NodeMbr%X2 .and. QBox%X2 > NodeMbr%X1 .and. &
                             QBox%Y1 < NodeMbr%Y2 .and. QBox%Y2 > NodeMbr%Y1) then
                            if (StackPtr < K_STACK_SIZE) then
                               StackPtr = StackPtr + 1_int32
                               Stack(StackPtr) = int(ChildIdx, kind=int32)
                            end if
                         end if
                      end do
                   end if
                end do
             end do
             !$omp end target data
          end if
       else
          allocate(OutBoxes(0))
       end if
    else
       allocate(OutBoxes(0))
    end if

    !$omp end target data
    deallocate(Counts, Offsets)
  end subroutine ComputeANDPairsGPU

  !> GPU NOT (A minus B): one thread per box of A walks B's R-tree in exactly the order of the
  !> CPU path in CalculateNOT (pop top, push children in index order, leaves scanned in index
  !> order) and applies the same 4-way split, so every A box yields the identical fragment list
  !> on both paths.  Two passes over one kernel body (NotKernelPass): count, Thrust exclusive
  !> scan of the counts on the device, fill.  Boxes whose remainder needs more than
  !> K_GPU_NOT_FRAGS fragments or whose DFS exceeds K_GPU_DFS_STACK are not dropped: they are
  !> returned in Fallback(1:NumFallback) for the caller to recompute on the CPU.
  !> Status /= 0 means the GPU path failed as a whole (allocation / scan) -> CPU path.
  subroutine ComputeNOTGPU(TreeNodes, NumNodes, SortedBoxesB, NumBoxesB, RootIndex, &
       BoxesA, NumBoxesA, OutBoxes, OutCount, Fallback, NumFallback, Status)
    integer(kind=int64), intent(in) :: NumNodes, NumBoxesB, NumBoxesA, RootIndex
    type(RTreeNode), intent(in)     :: TreeNodes(NumNodes)
    type(Box), intent(in)           :: SortedBoxesB(NumBoxesB)
    type(Box), intent(in)           :: BoxesA(NumBoxesA)
    type(Box), allocatable, intent(out) :: OutBoxes(:)
    integer(kind=int64), intent(out) :: OutCount
    integer(kind=int64), allocatable, intent(out) :: Fallback(:)
    integer(kind=int64), intent(out) :: NumFallback
    integer(kind=int32), intent(out) :: Status

    integer(kind=int64), allocatable, target :: Counts(:), Offsets(:)
    integer(kind=int8), allocatable :: Flags(:)
    type(Box) :: Dummy(1)
    integer(kind=int64) :: I, K
    integer :: alloc_status
    integer(c_int32_t) :: scan_rc

    OutCount = 0_int64
    NumFallback = 0_int64
    Status = 0_int32
    if (NumBoxesA <= 0 .or. NumBoxesB <= 0 .or. NumNodes <= 0 .or. RootIndex <= 0) then
       Status = 1_int32
       allocate(OutBoxes(0), Fallback(0))
       return
    end if
    allocate(Counts(NumBoxesA), Offsets(NumBoxesA), Flags(NumBoxesA), stat=alloc_status)
    if (alloc_status /= 0) then
       Status = 2_int32
       allocate(OutBoxes(0), Fallback(0))
       return
    end if
    call first_touch_i8(Flags, NumBoxesA)

    call prep_nodes(TreeNodes, NumNodes); call prep_boxes(SortedBoxesB, NumBoxesB); call prep_boxes(BoxesA, NumBoxesA)
    call prep_i64(Counts, NumBoxesA); call prep_i64(Offsets, NumBoxesA); call prep_i8(Flags, NumBoxesA)
    !$omp target data map(to: TreeNodes(1:NumNodes), SortedBoxesB(1:NumBoxesB), BoxesA(1:NumBoxesA)) &
    !$omp map(alloc: Counts(1:NumBoxesA), Offsets(1:NumBoxesA), Flags(1:NumBoxesA))

    call NotKernelPass(1, TreeNodes, NumNodes, SortedBoxesB, NumBoxesB, RootIndex, BoxesA, NumBoxesA, &
         Counts, Offsets, Flags, Dummy, 1_int64)
    !$omp target update from(Flags(1:NumBoxesA))

    !$omp target data use_device_addr(Counts, Offsets)
    scan_rc = thrust_exclusive_scan_i64(c_loc(Counts(1)), c_loc(Offsets(1)), NumBoxesA, OutCount)
    !$omp end target data
    if (scan_rc /= 0_c_int32_t) then
       Status = 3_int32
       OutCount = 0_int64
       allocate(OutBoxes(0))
    else
       allocate(OutBoxes(max(OutCount, 1_int64)), stat=alloc_status)
       if (alloc_status /= 0) then
          Status = 4_int32
          OutCount = 0_int64
          allocate(OutBoxes(0))
       else if (OutCount > 0_int64) then
          call first_touch_boxes(OutBoxes, OutCount)
          call prep_boxes(OutBoxes, OutCount)
          !$omp target data map(from: OutBoxes(1:OutCount))
          call NotKernelPass(2, TreeNodes, NumNodes, SortedBoxesB, NumBoxesB, RootIndex, BoxesA, NumBoxesA, &
               Counts, Offsets, Flags, OutBoxes, OutCount)
          !$omp end target data
       end if
    end if
    !$omp end target data

    if (Status == 0_int32) then
       NumFallback = count(Flags /= 0_int8, kind=int64)
       allocate(Fallback(NumFallback))
       if (NumFallback > 0) then
          K = 0
          do I = 1, NumBoxesA
             if (Flags(I) /= 0_int8) then
                K = K + 1
                Fallback(K) = I
             end if
          end do
       end if
    else
       allocate(Fallback(0))
    end if
    deallocate(Counts, Offsets, Flags)
  end subroutine ComputeNOTGPU

  !> Kernel body shared by both NOT passes.  Mode 1: Counts(I) = #fragments (Flags(I)=1 and
  !> Counts(I)=0 on fragment/stack overflow).  Mode 2: write fragments at Offsets(I)+1...
  !> All arrays are already present on the device (enclosing target data in ComputeNOTGPU).
  subroutine NotKernelPass(Mode, TreeNodes, NumNodes, SortedBoxesB, NumBoxesB, RootIndex, &
       BoxesA, NumBoxesA, Counts, Offsets, Flags, OutBoxes, OutCap)
    integer, intent(in) :: Mode
    integer(kind=int64), intent(in) :: NumNodes, NumBoxesB, NumBoxesA, RootIndex, OutCap
    type(RTreeNode), intent(in)     :: TreeNodes(NumNodes)
    type(Box), intent(in)           :: SortedBoxesB(NumBoxesB)
    type(Box), intent(in)           :: BoxesA(NumBoxesA)
    integer(kind=int64), intent(inout) :: Counts(NumBoxesA), Offsets(NumBoxesA)
    integer(kind=int8), intent(inout)  :: Flags(NumBoxesA)
    type(Box), intent(inout)        :: OutBoxes(OutCap)

    integer(kind=int64) :: I, J, ChildIdx, CurrNode, CEnd, W
    integer(kind=int32) :: Stack(K_GPU_DFS_STACK), StackPtr
    integer(kind=K_COORDINATE_KIND) :: FX1(K_GPU_NOT_FRAGS,2), FY1(K_GPU_NOT_FRAGS,2), &
         FX2(K_GPU_NOT_FRAGS,2), FY2(K_GPU_NOT_FRAGS,2)
    integer(kind=K_COORDINATE_KIND) :: rx1, ry1, rx2, ry2, fx1s, fy1s, fx2s, fy2s, ix1, iy1, ix2, iy2
    integer(kind=int32) :: nf, nn, sb, db, idx
    logical :: ovf, done
    type(Box) :: QBox, NodeMbr, TargetBox

    !$omp target teams distribute parallel do &
    !$omp private(I,J,ChildIdx,CurrNode,CEnd,W,Stack,StackPtr,FX1,FY1,FX2,FY2, &
    !$omp         rx1,ry1,rx2,ry2,fx1s,fy1s,fx2s,fy2s,ix1,iy1,ix2,iy2,nf,nn,sb,db,idx,ovf,done, &
    !$omp         QBox,NodeMbr,TargetBox)
    do I = 1, NumBoxesA
       if (Mode == 2) then
          if (Flags(I) /= 0_int8 .or. Counts(I) == 0_int64) cycle
       end if
       QBox = BoxesA(I)
       ovf = .false.
       nf = 1_int32
       sb = 1_int32
       FX1(1,1) = QBox%X1; FY1(1,1) = QBox%Y1; FX2(1,1) = QBox%X2; FY2(1,1) = QBox%Y2
       NodeMbr = TreeNodes(RootIndex)%Mbr
       !> same quick-accept test as the CPU path (boxes merely touching B's root MBR are kept)
       if (.not. (QBox%X1 >= NodeMbr%X2 .or. QBox%X2 <= NodeMbr%X1 .or. &
                  QBox%Y1 >= NodeMbr%Y2 .or. QBox%Y2 <= NodeMbr%Y1)) then
          rx1 = QBox%X1; ry1 = QBox%Y1; rx2 = QBox%X2; ry2 = QBox%Y2
          StackPtr = 1_int32
          Stack(1) = int(RootIndex, kind=int32)
          done = .false.
          do while (StackPtr > 0_int32 .and. .not. done)
             CurrNode = int(Stack(StackPtr), kind=int64)
             StackPtr = StackPtr - 1_int32
             if (TreeNodes(CurrNode)%IsLeaf) then
                CEnd = min(TreeNodes(CurrNode)%ChildStart + int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64, &
                     NumBoxesB)
                do J = TreeNodes(CurrNode)%ChildStart, CEnd
                   TargetBox = SortedBoxesB(J)
                   if (rx1 < TargetBox%X2 .and. rx2 > TargetBox%X1 .and. &
                       ry1 < TargetBox%Y2 .and. ry2 > TargetBox%Y1) then
                      db = 3_int32 - sb
                      nn = 0_int32
                      do idx = 1_int32, nf
                         fx1s = FX1(idx,sb); fy1s = FY1(idx,sb); fx2s = FX2(idx,sb); fy2s = FY2(idx,sb)
                         if (fx1s >= TargetBox%X2 .or. fx2s <= TargetBox%X1 .or. &
                             fy1s >= TargetBox%Y2 .or. fy2s <= TargetBox%Y1) then
                            if (nn >= K_GPU_NOT_FRAGS) then
                               ovf = .true.; exit
                            end if
                            nn = nn + 1_int32
                            FX1(nn,db) = fx1s; FY1(nn,db) = fy1s; FX2(nn,db) = fx2s; FY2(nn,db) = fy2s
                         else
                            if (nn + 4_int32 > K_GPU_NOT_FRAGS) then
                               ovf = .true.; exit
                            end if
                            ix1 = max(fx1s, TargetBox%X1); iy1 = max(fy1s, TargetBox%Y1)
                            ix2 = min(fx2s, TargetBox%X2); iy2 = min(fy2s, TargetBox%Y2)
                            if (fx1s < ix1) then
                               nn = nn + 1_int32
                               FX1(nn,db) = fx1s; FY1(nn,db) = fy1s; FX2(nn,db) = ix1; FY2(nn,db) = fy2s
                            end if
                            if (fx2s > ix2) then
                               nn = nn + 1_int32
                               FX1(nn,db) = ix2; FY1(nn,db) = fy1s; FX2(nn,db) = fx2s; FY2(nn,db) = fy2s
                            end if
                            if (fy1s < iy1) then
                               nn = nn + 1_int32
                               FX1(nn,db) = ix1; FY1(nn,db) = fy1s; FX2(nn,db) = ix2; FY2(nn,db) = iy1
                            end if
                            if (fy2s > iy2) then
                               nn = nn + 1_int32
                               FX1(nn,db) = ix1; FY1(nn,db) = iy2; FX2(nn,db) = ix2; FY2(nn,db) = fy2s
                            end if
                         end if
                      end do
                      if (ovf) then
                         done = .true.; exit
                      end if
                      nf = nn
                      sb = db
                      if (nf == 0_int32) then
                         done = .true.; exit
                      end if
                      rx1 = FX1(1,sb); ry1 = FY1(1,sb); rx2 = FX2(1,sb); ry2 = FY2(1,sb)
                      do idx = 2_int32, nf
                         if (FX1(idx,sb) < rx1) rx1 = FX1(idx,sb)
                         if (FY1(idx,sb) < ry1) ry1 = FY1(idx,sb)
                         if (FX2(idx,sb) > rx2) rx2 = FX2(idx,sb)
                         if (FY2(idx,sb) > ry2) ry2 = FY2(idx,sb)
                      end do
                   end if
                end do
             else
                CEnd = TreeNodes(CurrNode)%ChildStart + int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                do ChildIdx = TreeNodes(CurrNode)%ChildStart, CEnd
                   NodeMbr = TreeNodes(ChildIdx)%Mbr
                   if (rx1 < NodeMbr%X2 .and. rx2 > NodeMbr%X1 .and. &
                       ry1 < NodeMbr%Y2 .and. ry2 > NodeMbr%Y1) then
                      if (StackPtr >= K_GPU_DFS_STACK) then
                         ovf = .true.; done = .true.; exit
                      end if
                      StackPtr = StackPtr + 1_int32
                      Stack(StackPtr) = int(ChildIdx, kind=int32)
                   end if
                end do
             end if
          end do
       end if
       if (Mode == 1) then
          if (ovf) then
             Flags(I) = 1_int8
             Counts(I) = 0_int64
          else
             Flags(I) = 0_int8
             Counts(I) = int(nf, kind=int64)
          end if
       else
          W = Offsets(I)
          do idx = 1_int32, nf
             if (W + idx <= OutCap) then
                OutBoxes(W + idx)%X1 = FX1(idx,sb); OutBoxes(W + idx)%Y1 = FY1(idx,sb)
                OutBoxes(W + idx)%X2 = FX2(idx,sb); OutBoxes(W + idx)%Y2 = FY2(idx,sb)
             end if
          end do
       end if
    end do
  end subroutine NotKernelPass

  !> Single-pass GPU topological selection for all 10 K_TOPO_* predicates.
  !> Produces int8 KeepMask(1:NumBoxesA) with zero atomics and exact CPU parity.
  subroutine ComputeTopoSelectGPU(TreeNodesB, NumNodesB, SortedBoxesB, NumBoxesB, RootIndexB, &
       BoxesA, NumBoxesA, TopoOp, KeepMask, OverflowFlag)
    integer(kind=int64), intent(in) :: NumNodesB, NumBoxesB, NumBoxesA, RootIndexB, TopoOp
    type(RTreeNode), intent(in)     :: TreeNodesB(NumNodesB)
    type(Box), intent(in)           :: SortedBoxesB(NumBoxesB)
    type(Box), intent(in)           :: BoxesA(NumBoxesA)
    integer(kind=int8), intent(out) :: KeepMask(NumBoxesA)
    integer(kind=int32), intent(out):: OverflowFlag

    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode
    integer(kind=int32), parameter :: K_STACK_SIZE = 128_int32
    integer(kind=int32) :: Stack(K_STACK_SIZE), StackPtr
    type(Box) :: QBox, NodeMbr, TargetBox
    integer(kind=K_COORDINATE_KIND) :: ix1, iy1, ix2, iy2
    logical :: overlap_any, touch_any, inside_any, outside_all, early_done, select_it

    OverflowFlag = 0_int32
    if (NumBoxesA <= 0_int64 .or. NumBoxesB <= 0_int64 .or. NumNodesB <= 0_int64) return

    call prep_nodes(TreeNodesB, NumNodesB); call prep_boxes(SortedBoxesB, NumBoxesB)
    call prep_boxes(BoxesA, NumBoxesA); call first_touch_i8(KeepMask, NumBoxesA); call prep_i8(KeepMask, NumBoxesA)
    !$omp target data map(to: TreeNodesB(1:NumNodesB), SortedBoxesB(1:NumBoxesB), BoxesA(1:NumBoxesA)) &
    !$omp map(from: KeepMask(1:NumBoxesA)) map(tofrom: OverflowFlag)

    !$omp target teams distribute parallel do map(tofrom: OverflowFlag) &
    !$omp private(I,J,K,ChildIdx,CurrNode,Stack,StackPtr,QBox,NodeMbr,TargetBox, &
    !$omp         ix1,iy1,ix2,iy2,overlap_any,touch_any,inside_any,outside_all,early_done,select_it)
    do I = 1, NumBoxesA
       QBox = BoxesA(I)
       overlap_any = .false.
       touch_any   = .false.
       inside_any  = .false.
       outside_all = .true.
       early_done  = .false.

       NodeMbr = TreeNodesB(RootIndexB)%Mbr
       if (QBox%X1 <= NodeMbr%X2 .and. QBox%X2 >= NodeMbr%X1 .and. &
           QBox%Y1 <= NodeMbr%Y2 .and. QBox%Y2 >= NodeMbr%Y1) then
          StackPtr = 1_int32
          Stack(1) = int(RootIndexB, kind=int32)
          do while (StackPtr > 0_int32 .and. .not. early_done)
             CurrNode = int(Stack(StackPtr), kind=int64)
             StackPtr = StackPtr - 1_int32
             if (TreeNodesB(CurrNode)%IsLeaf) then
                do K = 0_int64, int(TreeNodesB(CurrNode)%NumChildren, kind=int64) - 1_int64
                   J = TreeNodesB(CurrNode)%ChildStart + K
                   if (J > NumBoxesB) exit
                   TargetBox = SortedBoxesB(J)
                   if (QBox%X1 > TargetBox%X2 .or. QBox%X2 < TargetBox%X1 .or. &
                       QBox%Y1 > TargetBox%Y2 .or. QBox%Y2 < TargetBox%Y1) cycle

                   if (QBox%X1 >= TargetBox%X1 .and. QBox%Y1 >= TargetBox%Y1 .and. &
                       QBox%X2 <= TargetBox%X2 .and. QBox%Y2 <= TargetBox%Y2) then
                      inside_any = .true.
                   end if

                   ix1 = max(QBox%X1, TargetBox%X1)
                   iy1 = max(QBox%Y1, TargetBox%Y1)
                   ix2 = min(QBox%X2, TargetBox%X2)
                   iy2 = min(QBox%Y2, TargetBox%Y2)
                   if (ix1 < ix2 .and. iy1 < iy2) then
                      overlap_any = .true.
                      outside_all = .false.
                   else if (ix1 <= ix2 .and. iy1 <= iy2 .and. .not. (ix1 == ix2 .and. iy1 == iy2)) then
                      touch_any = .true.
                      outside_all = .false.
                   else if (ix1 == ix2 .and. iy1 == iy2) then
                      ! Corner (point) contact: not an edge TOUCH, but it is contact, so the box
                      ! is not OUTSIDE ("no contact at all", RELATIONS.md; == DISJOINT on CPU).
                      outside_all = .false.
                   end if

                   if (TopoOp == K_TOPO_INTERACT .or. TopoOp == K_TOPO_NOT_INTERACT) then
                      if (overlap_any) then; early_done = .true.; exit; end if
                   else if (TopoOp == K_TOPO_INSIDE .or. TopoOp == K_TOPO_NOT_INSIDE) then
                      if (inside_any) then; early_done = .true.; exit; end if
                   else if (TopoOp == K_TOPO_OUTSIDE .or. TopoOp == K_TOPO_NOT_OUTSIDE) then
                      if (.not. outside_all) then; early_done = .true.; exit; end if
                   else if (TopoOp == K_TOPO_TOUCH .or. TopoOp == K_TOPO_NOT_TOUCH) then
                      if (overlap_any) then; early_done = .true.; exit; end if
                   else if (TopoOp == K_TOPO_CUT .or. TopoOp == K_TOPO_NOT_CUT) then
                      if (inside_any) then; early_done = .true.; exit; end if
                   end if
                end do
             else
                do K = 0_int64, int(TreeNodesB(CurrNode)%NumChildren, kind=int64) - 1_int64
                   ChildIdx = TreeNodesB(CurrNode)%ChildStart + K
                   NodeMbr = TreeNodesB(ChildIdx)%Mbr
                   if (QBox%X1 <= NodeMbr%X2 .and. QBox%X2 >= NodeMbr%X1 .and. &
                       QBox%Y1 <= NodeMbr%Y2 .and. QBox%Y2 >= NodeMbr%Y1) then
                      if (StackPtr < K_STACK_SIZE) then
                         StackPtr = StackPtr + 1_int32
                         Stack(StackPtr) = int(ChildIdx, kind=int32)
                      else
                         !$omp atomic write
                         OverflowFlag = 1_int32
                         early_done = .true.
                         exit
                      end if
                   end if
                end do
             end if
          end do
       end if

       if (TopoOp == K_TOPO_INTERACT) then
          select_it = overlap_any
       else if (TopoOp == K_TOPO_NOT_INTERACT) then
          select_it = .not. overlap_any
       else if (TopoOp == K_TOPO_INSIDE) then
          select_it = inside_any
       else if (TopoOp == K_TOPO_NOT_INSIDE) then
          select_it = .not. inside_any
       else if (TopoOp == K_TOPO_OUTSIDE) then
          select_it = outside_all
       else if (TopoOp == K_TOPO_NOT_OUTSIDE) then
          select_it = .not. outside_all
       else if (TopoOp == K_TOPO_TOUCH) then
          select_it = touch_any .and. .not. overlap_any
       else if (TopoOp == K_TOPO_NOT_TOUCH) then
          select_it = .not. (touch_any .and. .not. overlap_any)
       else if (TopoOp == K_TOPO_CUT) then
          select_it = overlap_any .and. .not. inside_any
       else if (TopoOp == K_TOPO_NOT_CUT) then
          select_it = .not. (overlap_any .and. .not. inside_any)
       else
          select_it = .false.
       end if

       if (select_it) then
          KeepMask(I) = 1_int8
       else
          KeepMask(I) = 0_int8
       end if
    end do

    !$omp end target data
  end subroutine ComputeTopoSelectGPU

  !> Fused 2-pass GPU edge extraction + overlap area/perimeter reduction for PerformMerge.
  !> Exact <= edge-touch + overlap parity with CPU PerformMerge, child-MBR pruning,
  !> int32(128) stack + OverflowFlag, device Thrust exclusive_scan (zero global atomics).
  subroutine ComputeMergeEdgesGPU(TreeNodes, NumNodes, SortedBoxes, NumBoxes, RootIndex, &
       OutEdges, OutCount, OverlapArea, OverlapPerimeter, OverflowFlag)
    integer(kind=int64), intent(in) :: NumNodes, NumBoxes, RootIndex
    type(RTreeNode), intent(in)     :: TreeNodes(NumNodes)
    type(Box), intent(in)           :: SortedBoxes(NumBoxes)
    integer(kind=int64), allocatable, intent(out) :: OutEdges(:,:)
    integer(kind=int64), intent(out):: OutCount
    real(kind=real64), intent(out)  :: OverlapArea, OverlapPerimeter
    integer(kind=int32), intent(out):: OverflowFlag

    integer(kind=int64), allocatable, target :: Counts(:), Offsets(:)
    integer(kind=int64), parameter :: K_MAX_GPU_EDGES = 250000000_int64
    integer(kind=int32), parameter :: K_STACK_SIZE = 128_int32
    integer(kind=int32) :: Stack(K_STACK_SIZE), StackPtr
    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode, LocalCount, WriteIdx
    integer(kind=K_COORDINATE_KIND) :: ix1, iy1, ix2, iy2
    real(kind=real64) :: w, h
    integer :: alloc_status
    integer(c_int32_t) :: scan_rc
    type(Box) :: QBox, NodeMbr, TargetBox

    OutCount = 0_int64
    OverlapArea = 0.0_real64
    OverlapPerimeter = 0.0_real64
    OverflowFlag = 0_int32

    if (NumBoxes <= 1_int64 .or. NumNodes <= 0_int64) then
       allocate(OutEdges(2, 0))
       return
    end if

    allocate(Counts(NumBoxes), Offsets(NumBoxes), stat=alloc_status)
    if (alloc_status /= 0) then
       OverflowFlag = 1_int32
       allocate(OutEdges(2, 0))
       return
    end if

    call prep_nodes(TreeNodes, NumNodes); call prep_boxes(SortedBoxes, NumBoxes)
    call prep_i64(Counts, NumBoxes); call prep_i64(Offsets, NumBoxes)
    !$omp target data map(to: TreeNodes(1:NumNodes), SortedBoxes(1:NumBoxes)) &
    !$omp map(alloc: Counts(1:NumBoxes), Offsets(1:NumBoxes)) &
    !$omp map(tofrom: OverflowFlag)

    ! --- Pass 1: count interacting pairs (J > I) + accumulate area/perimeter ---
    !$omp target teams distribute parallel do map(tofrom: OverflowFlag) &
    !$omp private(I,J,K,ChildIdx,CurrNode,Stack,StackPtr,LocalCount,QBox,NodeMbr,TargetBox,ix1,iy1,ix2,iy2,w,h) &
    !$omp reduction(+:OverlapArea, OverlapPerimeter)
    do I = 1, NumBoxes
       QBox = SortedBoxes(I)
       LocalCount = 0_int64
       NodeMbr = TreeNodes(RootIndex)%Mbr
       if (QBox%X1 <= NodeMbr%X2 .and. QBox%X2 >= NodeMbr%X1 .and. &
           QBox%Y1 <= NodeMbr%Y2 .and. QBox%Y2 >= NodeMbr%Y1) then
          StackPtr = 1_int32
          Stack(1) = int(RootIndex, kind=int32)
          do while (StackPtr > 0_int32)
             CurrNode = int(Stack(StackPtr), kind=int64)
             StackPtr = StackPtr - 1_int32
             if (TreeNodes(CurrNode)%IsLeaf) then
                do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                   J = TreeNodes(CurrNode)%ChildStart + K
                   if (J <= I) cycle
                   TargetBox = SortedBoxes(J)
                   if (QBox%X1 <= TargetBox%X2 .and. QBox%X2 >= TargetBox%X1 .and. &
                       QBox%Y1 <= TargetBox%Y2 .and. QBox%Y2 >= TargetBox%Y1) then
                      LocalCount = LocalCount + 1_int64
                      ix1 = max(QBox%X1, TargetBox%X1)
                      iy1 = max(QBox%Y1, TargetBox%Y1)
                      ix2 = min(QBox%X2, TargetBox%X2)
                      iy2 = min(QBox%Y2, TargetBox%Y2)
                      w = real(int(ix2, kind=int64) - int(ix1, kind=int64), kind=real64)
                      h = real(int(iy2, kind=int64) - int(iy1, kind=int64), kind=real64)
                      if (ix2 > ix1 .and. iy2 > iy1) then
                         OverlapArea = OverlapArea + w * h
                      else
                         OverlapPerimeter = OverlapPerimeter + 2.0_real64 * (w + h)
                      end if
                   end if
                end do
             else
                do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                   ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                   NodeMbr = TreeNodes(ChildIdx)%Mbr
                   if (QBox%X1 <= NodeMbr%X2 .and. QBox%X2 >= NodeMbr%X1 .and. &
                       QBox%Y1 <= NodeMbr%Y2 .and. QBox%Y2 >= NodeMbr%Y1) then
                      if (StackPtr < K_STACK_SIZE) then
                         StackPtr = StackPtr + 1_int32
                         Stack(StackPtr) = int(ChildIdx, kind=int32)
                      else
                         !$omp atomic write
                         OverflowFlag = 1_int32
                      end if
                   end if
                end do
             end if
          end do
       end if
       Counts(I) = LocalCount
    end do

    !$omp target update from(OverflowFlag)
    if (OverflowFlag == 0_int32) then
       !$omp target data use_device_addr(Counts, Offsets)
       scan_rc = thrust_exclusive_scan_i64(c_loc(Counts(1)), c_loc(Offsets(1)), NumBoxes, OutCount)
       !$omp end target data
       if (scan_rc /= 0_c_int32_t .or. OutCount > K_MAX_GPU_EDGES) then
          OverflowFlag = 1_int32
          OutCount = 0_int64
          allocate(OutEdges(2, 0))
       else if (OutCount > 0_int64) then
          allocate(OutEdges(2, OutCount), stat=alloc_status)
          if (alloc_status /= 0) then
             OverflowFlag = 1_int32
             OutCount = 0_int64
             allocate(OutEdges(2, 0))
          else
             ! --- Pass 2: fill exact edge list (skips zero-count boxes, zero atomics) ---
             call first_touch_i64(OutEdges, 2_int64 * OutCount)
             call prep_i64(OutEdges, 2_int64 * OutCount)
             !$omp target data map(from: OutEdges(1:2, 1:OutCount))
             !$omp target teams distribute parallel do &
             !$omp private(I,J,K,ChildIdx,CurrNode,Stack,StackPtr,WriteIdx,QBox,NodeMbr,TargetBox)
             do I = 1, NumBoxes
                if (Counts(I) == 0_int64) cycle
                QBox = SortedBoxes(I)
                WriteIdx = Offsets(I)
                StackPtr = 1_int32
                Stack(1) = int(RootIndex, kind=int32)
                do while (StackPtr > 0_int32)
                   CurrNode = int(Stack(StackPtr), kind=int64)
                   StackPtr = StackPtr - 1_int32
                   if (TreeNodes(CurrNode)%IsLeaf) then
                      do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                         J = TreeNodes(CurrNode)%ChildStart + K
                         if (J <= I) cycle
                         TargetBox = SortedBoxes(J)
                         if (QBox%X1 <= TargetBox%X2 .and. QBox%X2 >= TargetBox%X1 .and. &
                             QBox%Y1 <= TargetBox%Y2 .and. QBox%Y2 >= TargetBox%Y1) then
                            WriteIdx = WriteIdx + 1_int64
                            if (WriteIdx <= OutCount) then
                               OutEdges(1, WriteIdx) = I
                               OutEdges(2, WriteIdx) = J
                            end if
                         end if
                      end do
                   else
                      do K = 0_int64, int(TreeNodes(CurrNode)%NumChildren, kind=int64) - 1_int64
                         ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                         NodeMbr = TreeNodes(ChildIdx)%Mbr
                         if (QBox%X1 <= NodeMbr%X2 .and. QBox%X2 >= NodeMbr%X1 .and. &
                             QBox%Y1 <= NodeMbr%Y2 .and. QBox%Y2 >= NodeMbr%Y1) then
                            if (StackPtr < K_STACK_SIZE) then
                               StackPtr = StackPtr + 1_int32
                               Stack(StackPtr) = int(ChildIdx, kind=int32)
                            end if
                         end if
                      end do
                   end if
                end do
             end do
             !$omp end target data
          end if
       else
          allocate(OutEdges(2, 0))
       end if
    else
       allocate(OutEdges(2, 0))
    end if

    !$omp end target data
    deallocate(Counts, Offsets)
  end subroutine ComputeMergeEdgesGPU

end module RTreeBuilderGPU
