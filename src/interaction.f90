! File   : interaction.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: The RTree for GPU has to be thought from groudup in NVFORTRAN GPU
!        : as there are several challenges, such as non-allocate, no procs.
module RTreeBuilderGPU
  use iso_fortran_env, only: int32, int64, real64
  use omp_lib
  use GeometryModule
  use RTreeBuilder
  implicit none
  private

  public :: ComputeInteractionsGPU

  integer, parameter :: K_MAX_TREE_DEPTH = 1024


contains

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
    integer(kind=int64) :: Stack(512) 
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
                      if (StackPtr <= 512) then
                         Stack(StackPtr) = ChildIdx
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
