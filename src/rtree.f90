! File    : rtree.f90
! Author  : Sandeep Koranne (C) All rights.reserved.
! Purpose : Implementation of MAGIC VLSI format parser and RTree
module RTReeBuilder
  use CommonModule
  use GeometryModule
  implicit none
  private
  public:: RTReeNode, CalculateTotalNodes, BuildRTree, ExplainTheTree, SelfTestTheTree, SearchTree, K_MAX_SEARCH_LEAVES
  type :: RTreeNode
     type(Box) :: mbr
     integer(kind=K_COORDINATE_KIND) :: num_children
     integer(kind=8) :: child_start = -1
     integer(kind=8), allocatable :: child_indices(:)
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
  !> pure
  pure subroutine BuildRTree(sorted_boxes, capacity, tree_nodes, root_index)
    type(Box), intent(in) :: sorted_boxes(:)
    integer(kind=int64), intent(in) :: capacity
    type(RTreeNode), intent(inout) :: tree_nodes(:)
    integer(kind=int64), intent(out) :: root_index
    integer(kind=int64) :: n_boxes, total_nodes, current_level_nodes, prev_level_nodes
    integer(kind=int64) :: i, j, child_start, child_end, node_idx, parent_idx
    integer(kind=int64) :: current_level_start, prev_level_start
    type(Box) :: agg_mbr

    n_boxes = size(sorted_boxes)
    if (n_boxes == 0) return

    ! 1. Calculate the total number of nodes needed for the flat array
    total_nodes = size( tree_nodes )
    current_level_nodes = n_boxes

    !allocate(tree_nodes(total_nodes))

    ! 2. Build Level 1 (Parents of the external sorted boxes)
    !current_level_nodes = ceiling(real(n_boxes) / real(capacity))
    current_level_nodes = (n_boxes + capacity - 1) / capacity
    !write (*,'(A,I12,A,I2,A,I8,A,I12)') 'Flat RTree of ', n_boxes, ' of |C| = ', capacity, ' needs ', total_nodes, ' nodes. ', current_level_nodes    
    node_idx = 1

    do i = 1, current_level_nodes
       child_start = (i - 1) * capacity + 1
       child_end   = min(i * capacity, n_boxes)
       ! Initialize the aggregate MBR with the first child
       agg_mbr = sorted_boxes(child_start)
       tree_nodes(node_idx)%child_start = child_start
       tree_nodes(node_idx)%num_children = child_end - child_start + 1
       !write(*,'(A,I0,A,I0)') 'Creating LEAF NODE with interval: ', child_start, ' ', child_start + tree_nodes(node_idx)%num_children -1
       if( tree_nodes(node_idx)%num_children == capacity ) then
          tree_nodes(node_idx)%num_children = 0 !! HAHA
       else
          tree_nodes(node_idx)%num_children = - tree_nodes(node_idx)%num_children !! Even better
       end if
       ! Compute MBR for this chunk of boxes
       do j = child_start, child_end
          agg_mbr%x1 = min(agg_mbr%x1, sorted_boxes(j)%x1)
          agg_mbr%y1 = min(agg_mbr%y1, sorted_boxes(j)%y1)
          agg_mbr%x2 = max(agg_mbr%x2, sorted_boxes(j)%x2)
          agg_mbr%y2 = max(agg_mbr%y2, sorted_boxes(j)%y2)
       end do
       tree_nodes(node_idx)%mbr = agg_mbr
       node_idx = node_idx + 1
    end do

    ! 3. Build Higher Levels (Internal Nodes)
    prev_level_start = 1
    prev_level_nodes = current_level_nodes

    do while (prev_level_nodes > 1)
       current_level_start = node_idx
       current_level_nodes = ceiling(real(prev_level_nodes) / real(capacity))
       
       do i = 1, current_level_nodes
          child_start = prev_level_start + (i - 1) * capacity
          child_end   = min(prev_level_start + i * capacity-1, prev_level_start + prev_level_nodes - 1)
          !if( prev_level_start == 1 ) then
          !   write (*,*) 'LEVEL1: CS ', child_start, ' CE ', child_end, ' MIN1 ', prev_level_start + i * capacity - 1, ' MIN2 ', prev_level_start + prev_level_nodes - 1
          !end if
          ! Initialize aggregate MBR with the first child node
          agg_mbr = tree_nodes(child_start)%mbr

          allocate(tree_nodes(node_idx)%child_indices(child_end - child_start + 1))
          tree_nodes(node_idx)%num_children = child_end - child_start + 1

          ! Compute MBR for this chunk of child nodes
          do j = child_start, child_end
             agg_mbr%x1 = min(agg_mbr%x1, tree_nodes(j)%mbr%x1)
             agg_mbr%y1 = min(agg_mbr%y1, tree_nodes(j)%mbr%y1)
             agg_mbr%x2 = max(agg_mbr%x2, tree_nodes(j)%mbr%x2)
             agg_mbr%y2 = max(agg_mbr%y2, tree_nodes(j)%mbr%y2)

             ! Store the index of the child node in the flat array
             tree_nodes(node_idx)%child_indices(j - child_start + 1) = j
          end do

          tree_nodes(node_idx)%mbr = agg_mbr
          node_idx = node_idx + 1
       end do

       prev_level_start = current_level_start
       prev_level_nodes = current_level_nodes
    end do

    ! The last node created is the root
    root_index = total_nodes

  end subroutine BuildRTree

  subroutine ExplainTheTree(sorted_boxes, capacity, tree_nodes, root_index)
    type(Box), intent(in) :: sorted_boxes(:)
    integer, intent(in) :: capacity
    type(RTreeNode), intent(in) :: tree_nodes(:)
    integer(kind=8), intent(in) :: root_index

    integer(kind=8) :: n_boxes, total_nodes, current_level_nodes, prev_level_nodes
    integer(kind=8) :: i, j, child_start, child_end, node_idx, parent_idx
    integer(kind=8) :: current_level_start, prev_level_start
    type(Box) :: agg_mbr

    write (*,*) 'Explaining Tree rooted at: ', root_index
    call ExplainTheNode( sorted_boxes, capacity, tree_nodes, root_index)
    write (*,*)
    write (*,*)    
  end subroutine ExplainTheTree
  recursive subroutine ExplainTheNode(sorted_boxes, capacity, tree_nodes, current_node)
    type(Box), intent(in) :: sorted_boxes(:)
    type(Box)             :: b
    integer, intent(in) :: capacity
    type(RTreeNode), intent(in) :: tree_nodes(:)
    type(RTreeNode) :: tempNode, childNode
    integer(kind=8), intent(in) :: current_node
    logical :: is_leaf
    integer(kind=8) :: n_boxes, num_children
    integer(kind=8) :: i, j
    type(Box) :: agg_mbr
    n_boxes = size( sorted_boxes )
    tempNode = tree_nodes( current_node )
    write (*,'(A,I0,A,I0,A,4I)') 'Explaining Node: ', current_node, ' ', &
         FixNumberChildren(tempNode%num_children,capacity), ' children w/MBR ', tempNode%mbr
    is_leaf = .false.
    num_children = tempNode%num_children
    if( num_children == 0 ) then
       num_children = capacity
       is_leaf = .true.
    else if( num_children < 0 ) then
       num_children = -num_children
       is_leaf = .true.
    end if
    if( is_leaf ) then
       write (*,*) 'Leaf node: ', tempNode%child_start, ' : ', tempNode%child_start+num_children-1
       do i = tempNode%child_start, tempNode%child_start+num_children-1
          b = sorted_boxes( i  )
          !write(*,'(A,I,A,4I)') 'Box ', i, ': ', b%x1, b%y1, b%x2, b%y2
       end do
    end if
    if( .not. is_leaf ) then
       do i = 1,tempNode%num_children
          j = tempNode%child_indices(i)
          childNode = tree_nodes( j )
          write (*,'(A,I0,A,I0,A,I0,A,I0,A,4I)') 'Child: ',i, ' node ', j,' of parent ', current_node, ' has ', &
               FixNumberChildren(childNode%num_children,capacity), ' children, with MBR: ', childNode%mbr
          call ExplainTheNode( sorted_boxes, capacity, tree_nodes, j )
       end do
    end if
  end subroutine ExplainTheNode
  pure function FixNumberChildren(n,capacity) result(retval)
    integer, intent(in) :: n, capacity
    integer  retval
    retval = n
    if( n == 0 ) then
       retval = capacity
    elseif(n < 0) then
       retval = -n
    end if
  end function FixNumberChildren
  pure recursive subroutine SearchTree(tree_nodes, index, qbox, leafboxes, number_leaves)
    type(RTreeNode), intent(in) :: tree_nodes(:)
    integer(kind=int64), intent(in) :: index
    type(Box), intent(in)       :: qbox
    integer(kind=int64), intent(inout) :: leafboxes(K_MAX_SEARCH_LEAVES) ! better choose a large number
    integer(kind=int64), intent(inout) :: number_leaves
    ! in case we are not sure we have found everything, we have to return -1
    type(Box) :: cmbr
    type(Box) :: tempBox
    integer(kind=int64) :: i, j
    type(RTreeNode) :: childNode
    if( size(tree_nodes) == 0 ) then
       leafboxes(1) = 1
       number_leaves = 1
       return
    end if
    cmbr = tree_nodes( index )%mbr
    tempBox = cmbr * qbox
    !write (*,'(A,4I,A,4I)') 'QBOX: ', qbox, ' CMBR: ', cmbr
    if( .not. MBRValid( tempBox ) ) then
       !write (*,*) 'QBOX: ', qbox, ' not within CMBR: ', cmbr
       error stop "ERROR: QBOX FAIL"
       return
    end if
    if( tree_nodes( index )%child_start > 0 ) then
       !> we have found a leaf
       number_leaves = number_leaves + 1
       if( number_leaves > K_MAX_SEARCH_LEAVES ) then
          error stop "INCREASE NUMBER SEARCH LEAVES"
       end if
       leafboxes( number_leaves ) = tree_nodes( index )%child_start
       !write (*,*) 'NL = ', number_leaves, ' CS = ', tree_nodes( index )%child_start
    else
       do i = 1,tree_nodes(index)%num_children
          j = tree_nodes(index)%child_indices(i)
          childNode = tree_nodes( j )
          tempBox = childNode%mbr * qbox
          if( MBRValid( tempBox ) ) then
             !write (*,'(A,I0,A,I0,A,I0,A,I0,A,4I)') 'Child: ',i, ' node ', j,' of parent ', index, ' has ', &
             !     childNode%num_children, ' children, with MBR: ', childNode%mbr
             call SearchTree( tree_nodes, j, qbox, leafboxes, number_leaves )
          end if
       end do
    end if
  end subroutine SearchTree
  !+----------------------------------------------------------------------------------+                                                  
  !Layer: 1 gL67_D20 has 457108784 rects. |RTREE| = CPU 8137.33 secs. 187.95 REAL secs                            
  !+----------------------------------------------------------------------------------+        
  subroutine SelfTestTheTree(sorted_boxes, capacity, tree_nodes, root_index)
    type(Box), intent(in) :: sorted_boxes(:)
    integer(kind=int64), intent(in) :: capacity
    integer(kind=int64), intent(in) :: root_index
    type(RTreeNode), intent(in) :: tree_nodes(:)
    !type(RTreeNode) :: tempNode, childNode
    integer(kind=int64) :: num_boxes
    logical         :: BIG_FAIL
    integer(kind=int64) :: i
    integer(kind=int64) :: leafboxes(K_MAX_SEARCH_LEAVES) ! better choose a large number
    integer(kind=int64) :: number_leaves
    integer(kind=int64) :: j, k
    logical         :: found         
    
    BIG_FAIL = .false.
    num_boxes = size( sorted_boxes )
    if( size( tree_nodes ) == 0  .and. ( num_boxes <= capacity ) ) then
       return
    end if
    !write(*,*) 'DBG: ', num_boxes, ' ', size(tree_nodes)
    !> There are cases when the layer has < capacity boxes, so the tree is fake
    !> We are going to assume that self check passes in these cases
    !> we have to use OpenMP has do concurrent (atleast in ifx) did not make local vars
    !$omp parallel do private(leafboxes, number_leaves, j, k, found)
    over_all_boxes: do i=1,num_boxes
       number_leaves = 0
       leafboxes = 0
       found = .false.
       call SearchTree( tree_nodes, root_index, sorted_boxes(i), leafboxes, number_leaves )
       !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
       if( number_leaves > 0 ) then
          !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
          outer: do j=1,number_leaves
             over_leaves: do k=leafboxes(j),min(leafboxes(j)+capacity-1, num_boxes)
                !do k=leafboxes(j),leafboxes(j)+capacity-1
                if( i == k ) then
                   !write(*,*) 'Index ',i, ' found at ', k                   
                   if( found ) then
                      write(*,*) 'Index ',i, ' found repeated at ', k
                      !stop "assertion failed: duplicate index found"
                   end if
                   found = .true.
                   !exit outer
                end if
             end do over_leaves
          end do outer
          if( .not. found ) then
             !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
             error stop "assertion failed: box not found at all"
          end if
       else
          !stop "ERROR: highly unusual"
          BIG_FAIL = .true.
       end if
    end do over_all_boxes

    if( BIG_FAIL ) then
       stop "ERROR: highly unusual"
    end if
  end subroutine SelfTestTheTree
  
end module RTReeBuilder
