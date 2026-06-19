! File   : interaction.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: The RTree for GPU has to be thought from groudup in NVFORTRAN GPU
!        : as there are several challenges, such as non-allocate, no procs.
module RTreeBuilderGPU
  use iso_fortran_env, only: int32, int64, real64
  use omp_lib
  implicit none
  private

  public :: ComputeInteractionsGPU

  integer, parameter :: K_MAX_TREE_DEPTH = 64

  type :: Box
     integer(kind=int32) :: X1, Y1, X2, Y2
  end type Box

  type :: RTreeNode
     type(Box) :: Mbr
     integer(kind=int64) :: NumChildren
     integer(kind=int64) :: ChildStart
     logical :: IsLeaf
     ! Note: child_indices array removed for contiguous flat-array mapping
  end type RTreeNode

contains
  !$komp declare target  
  subroutine DO_INTERACTION(I, J)
    integer(kind=int64), value :: I, J
    ! Interaction logic goes here
  end subroutine DO_INTERACTION

  pure function CalculateTotalNodes(NumBoxes, Capacity) result(TotalNodes)
    integer(kind=int64), intent(in) :: NumBoxes, Capacity
    integer(kind=int64) :: TotalNodes, CurrentLevelNodes

    TotalNodes = 0    
    if (NumBoxes == 0) return

    CurrentLevelNodes = NumBoxes
    do while (CurrentLevelNodes > 1)
       CurrentLevelNodes = (CurrentLevelNodes + Capacity - 1) / Capacity
       TotalNodes = TotalNodes + CurrentLevelNodes
    end do
  end function CalculateTotalNodes

  !> Builds the flat array RTree. Safe to run on CPU host prior to offload.
  pure subroutine BuildRTree(SortedBoxes, Capacity, TreeNodes, RootIndex)
    type(Box), intent(in) :: SortedBoxes(:)
    integer(kind=int64), intent(in) :: Capacity
    type(RTreeNode), intent(inout) :: TreeNodes(:)
    integer(kind=int64), intent(out) :: RootIndex

    integer(kind=int64) :: NumBoxes, CurrentLevelNodes, PrevLevelNodes
    integer(kind=int64) :: I, J, CStart, CEnd, NodeIdx
    integer(kind=int64) :: CurrentLevelStart, PrevLevelStart
    type(Box) :: AggMbr

    NumBoxes = size(SortedBoxes, kind=int64)
    if (NumBoxes == 0) return

    ! 1. Build Level 1 (Leaves pointing to SortedBoxes)
    CurrentLevelNodes = (NumBoxes + Capacity - 1) / Capacity
    NodeIdx = 1

    do I = 1, CurrentLevelNodes
       CStart = (I - 1) * Capacity + 1
       CEnd   = min(I * Capacity, NumBoxes)

       AggMbr = SortedBoxes(CStart)
       TreeNodes(NodeIdx)%ChildStart = CStart
       TreeNodes(NodeIdx)%NumChildren = CEnd - CStart + 1
       TreeNodes(NodeIdx)%IsLeaf = .true.

       do J = CStart, CEnd
          AggMbr%X1 = min(AggMbr%X1, SortedBoxes(J)%X1)
          AggMbr%Y1 = min(AggMbr%Y1, SortedBoxes(J)%Y1)
          AggMbr%X2 = max(AggMbr%X2, SortedBoxes(J)%X2)
          AggMbr%Y2 = max(AggMbr%Y2, SortedBoxes(J)%Y2)
       end do

       TreeNodes(NodeIdx)%Mbr = AggMbr
       NodeIdx = NodeIdx + 1
    end do

    ! 2. Build Higher Levels (Internal nodes pointing to TreeNodes)
    PrevLevelStart = 1
    PrevLevelNodes = CurrentLevelNodes

    do while (PrevLevelNodes > 1)
       CurrentLevelStart = NodeIdx
       CurrentLevelNodes = (PrevLevelNodes + Capacity - 1) / Capacity

       do I = 1, CurrentLevelNodes
          CStart = PrevLevelStart + (I - 1) * Capacity
          CEnd   = min(PrevLevelStart + I * Capacity - 1, PrevLevelStart + PrevLevelNodes - 1)

          AggMbr = TreeNodes(CStart)%Mbr
          TreeNodes(NodeIdx)%ChildStart = CStart
          TreeNodes(NodeIdx)%NumChildren = CEnd - CStart + 1
          TreeNodes(NodeIdx)%IsLeaf = .false.

          do J = CStart, CEnd
             AggMbr%X1 = min(AggMbr%X1, TreeNodes(J)%Mbr%X1)
             AggMbr%Y1 = min(AggMbr%Y1, TreeNodes(J)%Mbr%Y1)
             AggMbr%X2 = max(AggMbr%X2, TreeNodes(J)%Mbr%X2)
             AggMbr%Y2 = max(AggMbr%Y2, TreeNodes(J)%Mbr%Y2)
          end do

          TreeNodes(NodeIdx)%Mbr = AggMbr
          NodeIdx = NodeIdx + 1
       end do

       PrevLevelStart = CurrentLevelStart
       PrevLevelNodes = CurrentLevelNodes
    end do

    RootIndex = NodeIdx - 1
  end subroutine BuildRTree


  !> GPU Offloaded All-Query Interaction Generator
  subroutine ComputeInteractionsGPU(TreeNodes, SortedBoxes, RootIndex)
    type(RTreeNode), intent(in) :: TreeNodes(:)
    type(Box), intent(in) :: SortedBoxes(:)
    integer(kind=int64), intent(in) :: RootIndex

    integer(kind=int64) :: NumBoxes, NumNodes
    integer(kind=int64) :: I, J, K, ChildIdx, CurrNode

    ! Thread-local traversal stack
    integer(kind=int64) :: Stack(K_MAX_TREE_DEPTH)
    integer(kind=int64) :: StackPtr

    type(Box) :: QBox, NodeMbr, TargetBox
    logical :: OverlapX, OverlapY

    NumBoxes = size(SortedBoxes, kind=int64)
    NumNodes = size(TreeNodes, kind=int64)

    ! Map immutable tree and geometry to device memory
    !$omp target data map(to: TreeNodes(1:NumNodes), SortedBoxes(1:NumBoxes))

    ! Dispatch parallel kernel to GPU
    !$omp target teams distribute parallel do default(none) &
    !$omp shared(TreeNodes, SortedBoxes, NumBoxes, RootIndex) &
    !$omp private(I, J, K, ChildIdx, CurrNode, Stack, StackPtr, &
    !$omp         QBox, NodeMbr, TargetBox, OverlapX, OverlapY)
    do I = 1, NumBoxes
       QBox = SortedBoxes(I)

       ! Initialize static stack
       StackPtr = 1
       Stack(StackPtr) = RootIndex

       ! Iterative Depth-First Search
       do while (StackPtr > 0)
          CurrNode = Stack(StackPtr)
          StackPtr = StackPtr - 1

          NodeMbr = TreeNodes(CurrNode)%Mbr

          ! Inlined MBR Overlap Check (replaces MBRValid and overloads)
          OverlapX = max(NodeMbr%X1, QBox%X1) <= min(NodeMbr%X2, QBox%X2)
          OverlapY = max(NodeMbr%Y1, QBox%Y1) <= min(NodeMbr%Y2, QBox%Y2)

          ! Prune branch if bounding boxes do not overlap
          if (.not. (OverlapX .and. OverlapY)) cycle

          if (TreeNodes(CurrNode)%IsLeaf) then
             ! Leaf Node: Check exact geometry intersections
             do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                J = TreeNodes(CurrNode)%ChildStart + K
                TargetBox = SortedBoxes(J)

                OverlapX = max(TargetBox%X1, QBox%X1) <= min(TargetBox%X2, QBox%X2)
                OverlapY = max(TargetBox%Y1, QBox%Y1) <= min(TargetBox%Y2, QBox%Y2)

                ! If interaction found, call the procedure
                if (OverlapX .and. OverlapY) then
                   call DO_INTERACTION(I, J)
                end if
             end do
          else
             ! Internal Node: Push children onto the stack
             ! Pushing in reverse order maintains left-to-right DFS, but 
             ! order doesn't mathematically matter for intersection aggregation.
             do K = 0, TreeNodes(CurrNode)%NumChildren - 1
                ChildIdx = TreeNodes(CurrNode)%ChildStart + K
                StackPtr = StackPtr + 1
                Stack(StackPtr) = ChildIdx
             end do
          end if
       end do
    end do
    !$omp end target data

  end subroutine ComputeInteractionsGPU

end module RTreeBuilderGPU
