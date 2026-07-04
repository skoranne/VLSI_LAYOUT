! File   : interaction.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: The RTree for GPU has to be thought from groudup in NVFORTRAN GPU
!        : as there are several challenges, such as non-allocate, no procs.
module RTreeBuilderGPU
  use iso_fortran_env, only: int32, int64, real64
  use omp_lib
  use CommonModule
  use Utilities
  use GeometryModule
  use DataStructuresModule
  use RTreeBuilder
  implicit none
  private

  public :: ComputeInteractionsGPU

  integer, parameter :: K_MAX_TREE_DEPTH = 1024


contains
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

    ! 4. Retrieve only the populated portion of the edge buffers back to CPU memory
    !$omp target update from(d_edge_i(1:edge_count), d_edge_k(1:edge_count))

    !$omp end target data

    ! ---------------------------------------------------------
    ! CPU Fallback: Fast Sparse Union-Find & Root Extraction
    ! (Exactly the same logic as before)
    ! ---------------------------------------------------------

    if (abs(overlap_area) > K_SMALL_EPSILON) overlap_perimeter = 0.0

    edge_count = min(edge_count, max_expected_edges)

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
  subroutine ComputeInteractionsGPU(TreeNodes, NumNodes, SortedBoxes, NumBoxes, RootIndex, KernelCount)
    ! 1. The exact sizes MUST come first
    integer(kind=int64), intent(in) :: NumNodes, NumBoxes

    ! 2. Force Explicit-Shape (No colons allowed!)
    type(RTreeNode), intent(in) :: TreeNodes(NumNodes)
    type(Box), intent(in) :: SortedBoxes(NumBoxes)

    integer(kind=int64), intent(in) :: RootIndex
    integer(kind=int64), intent(out) :: KernelCount

    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode

    ! 3. Stack size of 512 is now massive overkill, which is a good thing.
    integer(kind=int64), parameter :: K_STACK_SIZE = 64
    integer(kind=int64) :: Stack(K_STACK_SIZE) 
    integer(kind=int64) :: StackPtr

    type(Box) :: QBox, NodeMbr, TargetBox
    logical :: OverlapX, OverlapY

    KernelCount = 0

    ! Map the explicit bounds
    !$omp target enter data map(to: TreeNodes(1:NumNodes), SortedBoxes(1:NumBoxes))

    ! Launch kernel
    !$omp target teams distribute parallel do &
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
                      StackPtr = StackPtr + 1

                      ! GPU Guard: Prevent silent death if tree is inexplicably deep
                      if (StackPtr <= K_STACK_SIZE) then
                         Stack(StackPtr) = ChildIdx
                      else

                      end if
                   end if
                end if
             end do
          end if
       end do
    end do
    !$omp target exit data map(release:  TreeNodes(1:NumNodes), SortedBoxes(1:NumBoxes))

  end subroutine ComputeInteractionsGPU

end module RTreeBuilderGPU
