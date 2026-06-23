! File    : rtree.f90
! Author  : Sandeep Koranne (C) All rights.reserved.
! Purpose : Implementation of MAGIC VLSI format parser and RTree
module RTReeBuilder
  use CommonModule
  use GeometryModule
  use iso_fortran_env, only: int32, int64, real64
  implicit none
  private
  public:: RTReeNode, CalculateTotalNodes, BuildRTree, SelfTestTheTree, SearchTree, K_MAX_SEARCH_LEAVES
  type :: RTreeNode
     type(Box) :: mbr
     integer(kind=int64) :: child_start   ! Index of first child in the flat array
     integer(kind=int64) :: num_children  ! Number of contiguous children
     logical :: is_leaf                   ! Clean boolean flag, no negative number hacks
  end type RTreeNode
  integer, parameter :: K_MAX_SEARCH_LEAVES = 16*4096
contains
  pure function CalculateTotalNodes( n_boxes, capacity ) result(total_nodes)
    integer(kind=int64), intent(in) :: n_boxes
    integer(kind=int64), intent(in) :: capacity    
    integer(kind=int64) :: total_nodes
    integer(kind=int64) :: current_level_nodes
    total_nodes = 0    
    if (n_boxes == 0) return

    ! 1. Calculate the total number of nodes needed for the flat array
    total_nodes = 0
    current_level_nodes = n_boxes
    !> result = (n_boxes + capacity - 1) / capacity
    !do while (current_level_nodes > 1)
    !   current_level_nodes = ceiling(real(current_level_nodes) / real(capacity))
    !   total_nodes = total_nodes + current_level_nodes
    !end do
    do while (current_level_nodes > 1)
       ! Pure integer ceiling division: (A + B - 1) / B
       current_level_nodes = (current_level_nodes + capacity - 1) / capacity
       total_nodes = total_nodes + current_level_nodes
    end do
  end function CalculateTotalNodes
  pure subroutine BuildRTree(sorted_boxes, capacity, tree_nodes, root_index)
    type(Box), intent(in) :: sorted_boxes(:)
    integer(kind=int64), intent(in) :: capacity
    type(RTreeNode), intent(inout) :: tree_nodes(:)
    integer(kind=int64), intent(out) :: root_index

    integer(kind=int64) :: n_boxes, current_level_nodes, prev_level_nodes
    integer(kind=int64) :: i, j, child_start, child_end, node_idx
    integer(kind=int64) :: current_level_start, prev_level_start
    type(Box) :: agg_mbr

    n_boxes = size(sorted_boxes, kind=int64)
    if (n_boxes == 0) return

    ! 1. Build Level 1 (Leaves pointing sequentially to sorted_boxes)
    ! Using integer math instead of ceiling(real(...) / real(...))
    current_level_nodes = (n_boxes + capacity - 1) / capacity
    node_idx = 1

    do i = 1, current_level_nodes
       child_start = (i - 1) * capacity + 1
       child_end   = min(i * capacity, n_boxes)

       ! Initialize the aggregate MBR with the first child
       agg_mbr = sorted_boxes(child_start)
       tree_nodes(node_idx)%child_start = child_start
       tree_nodes(node_idx)%num_children = child_end - child_start + 1
       tree_nodes(node_idx)%is_leaf = .true. 

       ! Compute MBR for this contiguous chunk of boxes
       do j = child_start, child_end
          agg_mbr%x1 = min(agg_mbr%x1, sorted_boxes(j)%x1)
          agg_mbr%y1 = min(agg_mbr%y1, sorted_boxes(j)%y1)
          agg_mbr%x2 = max(agg_mbr%x2, sorted_boxes(j)%x2)
          agg_mbr%y2 = max(agg_mbr%y2, sorted_boxes(j)%y2)
       end do

       tree_nodes(node_idx)%mbr = agg_mbr
       node_idx = node_idx + 1
    end do

    ! 2. Build Higher Levels (Internal Nodes pointing sequentially to tree_nodes)
    prev_level_start = 1
    prev_level_nodes = current_level_nodes

    do while (prev_level_nodes > 1)
       current_level_start = node_idx
       current_level_nodes = (prev_level_nodes + capacity - 1) / capacity

       do i = 1, current_level_nodes
          child_start = prev_level_start + (i - 1) * capacity
          child_end   = min(prev_level_start + i * capacity - 1, prev_level_start + prev_level_nodes - 1)

          ! Initialize aggregate MBR with the first child node
          agg_mbr = tree_nodes(child_start)%mbr

          tree_nodes(node_idx)%child_start = child_start
          tree_nodes(node_idx)%num_children = child_end - child_start + 1
          tree_nodes(node_idx)%is_leaf = .false.

          ! Compute MBR for this contiguous chunk of child nodes
          do j = child_start, child_end
             agg_mbr%x1 = min(agg_mbr%x1, tree_nodes(j)%mbr%x1)
             agg_mbr%y1 = min(agg_mbr%y1, tree_nodes(j)%mbr%y1)
             agg_mbr%x2 = max(agg_mbr%x2, tree_nodes(j)%mbr%x2)
             agg_mbr%y2 = max(agg_mbr%y2, tree_nodes(j)%mbr%y2)
          end do

          tree_nodes(node_idx)%mbr = agg_mbr
          node_idx = node_idx + 1
       end do

       prev_level_start = current_level_start
       prev_level_nodes = current_level_nodes
    end do

    ! The last node created is the root
    root_index = node_idx - 1

  end subroutine BuildRTree
  pure recursive subroutine SearchTreeRecursive(tree_nodes, index, qbox, leafboxes, number_leaves)
    type(RTreeNode), intent(in)        :: tree_nodes(:)
    integer(kind=int64), intent(in)    :: index
    type(Box), intent(in)              :: qbox
    integer(kind=int64), intent(inout) :: leafboxes(K_MAX_SEARCH_LEAVES)
    integer(kind=int64), intent(inout) :: number_leaves

    integer(kind=int64) :: child_idx
    logical             :: overlapx, overlapy
    type(RTreeNode)     :: childNode

    if( size(tree_nodes) == 0 ) then
       leafboxes(1) = 1
       number_leaves = 1
       return
    end if

    ! 1. Initial check on the current node (Catches the root on first call)
    overlapx = max(tree_nodes(index)%mbr%x1, qbox%x1) <= min(tree_nodes(index)%mbr%x2, qbox%x2)
    if (overlapx) then
       overlapy = max(tree_nodes(index)%mbr%y1, qbox%y1) <= min(tree_nodes(index)%mbr%y2, qbox%y2)
    else
       overlapy = .false.
    end if

    if (.not. (overlapx .and. overlapy)) return

    ! 2. Process Leaf or Internal Node
    if( tree_nodes(index)%is_leaf ) then

       !> We have found a leaf
       number_leaves = number_leaves + 1
       if( number_leaves > K_MAX_SEARCH_LEAVES ) then
          error stop "INCREASE NUMBER SEARCH LEAVES"
       end if
       leafboxes( number_leaves ) = tree_nodes(index)%child_start

    else

       !> Internal Node: Iterate over contiguous children
       do child_idx = tree_nodes(index)%child_start, tree_nodes(index)%child_start + tree_nodes(index)%num_children - 1

          childNode = tree_nodes(child_idx)

          ! Inline short-circuit overlap check for the child
          overlapx = max(childNode%mbr%x1, qbox%x1) <= min(childNode%mbr%x2, qbox%x2)
          if (overlapx) then
             overlapy = max(childNode%mbr%y1, qbox%y1) <= min(childNode%mbr%y2, qbox%y2)

             ! Only recurse if the bounding boxes overlap
             if (overlapy) then
                call SearchTreeRecursive( tree_nodes, child_idx, qbox, leafboxes, number_leaves )
             end if
          end if

       end do

    end if

  end subroutine SearchTreeRecursive
  pure subroutine SearchTree(tree_nodes, root_index, qbox, leafboxes, number_leaves)
    type(RTreeNode), intent(in)        :: tree_nodes(:)
    integer(kind=int64), intent(in)    :: root_index
    type(Box), intent(in)              :: qbox
    integer(kind=int64), intent(inout) :: leafboxes(K_MAX_SEARCH_LEAVES)
    integer(kind=int64), intent(inout) :: number_leaves

    integer(kind=int64), parameter     :: K_STACK_SIZE = 256
    ! Explicit Local Stack limits memory footprint per thread
    integer(kind=int64) :: Stack(K_STACK_SIZE) 
    integer(kind=int64) :: StackPtr
    integer(kind=int64) :: curr_index, child_idx
    logical             :: overlapx, overlapy
    type(RTreeNode)     :: currNode, childNode

    number_leaves = 0

    if( size(tree_nodes) == 0 ) then
       leafboxes(1) = 1
       number_leaves = 1
       return
    end if

    ! Initial check on the root node (inlined for performance)
    overlapx = max(tree_nodes(root_index)%mbr%x1, qbox%x1) <= min(tree_nodes(root_index)%mbr%x2, qbox%x2)
    if (overlapx) then
       overlapy = max(tree_nodes(root_index)%mbr%y1, qbox%y1) <= min(tree_nodes(root_index)%mbr%y2, qbox%y2)
    else
       overlapy = .false.
    end if

    if (.not. (overlapx .and. overlapy)) return

    ! Initialize the static stack
    StackPtr = 1
    Stack(StackPtr) = root_index

    ! Iterative Depth-First Search
    do while (StackPtr > 0)
       ! Pop the current node off the stack
       curr_index = Stack(StackPtr)
       StackPtr = StackPtr - 1

       currNode = tree_nodes(curr_index)

       if( currNode%is_leaf ) then
          !> We have found a leaf
          number_leaves = number_leaves + 1
          if( number_leaves > K_MAX_SEARCH_LEAVES ) then
             error stop "INCREASE NUMBER SEARCH LEAVES"
          end if
          leafboxes( number_leaves ) = currNode%child_start
       else
          !> Internal Node: Check contiguous children and push valid ones to stack
          do child_idx = currNode%child_start, currNode%child_start + currNode%num_children - 1
             childNode = tree_nodes(child_idx)

             ! Inline short-circuit overlap check
             overlapx = max( childNode%mbr%x1, qbox%x1 ) <= min( childNode%mbr%x2, qbox%x2 )
             if (overlapx) then
                overlapy = max( childNode%mbr%y1, qbox%y1 ) <= min( childNode%mbr%y2, qbox%y2 )

                ! Only push to stack if the bounding boxes overlap
                if( overlapy ) then
                   StackPtr = StackPtr + 1

                   ! Safety check to prevent memory corruption
                   if (StackPtr > K_STACK_SIZE) then
                      error stop "ERROR: EXPLICIT STACK OVERFLOW"
                   end if

                   Stack(StackPtr) = child_idx
                end if
             end if
          end do
       end if
    end do

  end subroutine SearchTree
  !+----------------------------------------------------------------------------------+                                                  
  !Layer: 1 gL67_D20 has 457108784 rects. |RTREE| = CPU 8137.33 secs. 187.95 REAL secs                            
  !+----------------------------------------------------------------------------------+
  subroutine SelfTestTheTree(sorted_boxes, capacity, tree_nodes, root_index)
    type(Box), intent(in)           :: sorted_boxes(:)
    integer(kind=int64), intent(in) :: capacity
    type(RTreeNode), intent(in)     :: tree_nodes(:)
    integer(kind=int64), intent(in) :: root_index

    integer(kind=int64) :: num_boxes
    logical             :: BIG_FAIL
    integer(kind=int64) :: i, j, k
    integer(kind=int64) :: leafboxes(K_MAX_SEARCH_LEAVES) 
    integer(kind=int64) :: number_leaves
    logical             :: found          

    BIG_FAIL = .false.
    num_boxes = size( sorted_boxes, kind=int64 )

    if( size(tree_nodes) == 0 .and. (num_boxes <= capacity) ) then
       return
    end if

    !$omp parallel do private(leafboxes, number_leaves, j, k, found) reduction(.or.:BIG_FAIL)
    over_all_boxes: do i = 1, num_boxes
       number_leaves = 0
       leafboxes = 0
       found = .false.

       call SearchTree( tree_nodes, root_index, sorted_boxes(i), leafboxes, number_leaves )

       if( number_leaves > 0 ) then
          outer: do j = 1, number_leaves
             ! In the DOD tree, leafboxes(j) contains the child_start index.
             ! The builder guarantees children are contiguous up to capacity.
             over_leaves: do k = leafboxes(j), min(leafboxes(j) + capacity - 1, num_boxes)
                if( i == k ) then
                   if( found ) then
                      ! Prevent thread output from scrambling
                      !$omp critical
                      write(*,*) 'Index ', i, ' found repeated at ', k
                      !$omp end critical
                   end if
                   found = .true.
                end if
             end do over_leaves
          end do outer

          if( .not. found ) then
             ! error stop gracefully halts all OpenMP threads
             error stop "assertion failed: box not found at all"
          end if
       else
          ! Safely flag the failure using the OpenMP reduction
          BIG_FAIL = .true.
       end if
    end do over_all_boxes

    if( BIG_FAIL ) then
       error stop "ERROR: highly unusual - box found no overlapping leaves"
    end if

  end subroutine SelfTestTheTree

end module RTReeBuilder
