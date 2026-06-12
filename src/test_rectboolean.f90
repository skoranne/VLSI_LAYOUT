! --- Mock SearchTree Implementation ---
function SearchTree(root_b_ptr, qbox) result(overlaps)
    use rtree_boolean_mod
    implicit none
    integer(8), intent(in) :: root_b_ptr 
    type(Box), intent(in) :: qbox
    type(Vector_Box) :: overlaps
    
    ! For testing, we mock Tree B as a single hardcoded Box 
    ! that sits directly in the center of our test target.
    type(Box) :: mock_b_box
    mock_b_box = Box(2, 2, 8, 8)
    
    ! Check intersection
    if (.not. (qbox%x2 <= mock_b_box%x1 .or. qbox%x1 >= mock_b_box%x2 .or. &
               qbox%y2 <= mock_b_box%y1 .or. qbox%y1 >= mock_b_box%y2)) then
        call overlaps%push(mock_b_box)
    end if
end function SearchTree


! --- Main Program ---
program test_a_not_b
    use rtree_boolean_mod
    use rtree_parallel_mod
    implicit none

    type(RTreeNode), allocatable :: tree_a(:)
    type(Vector_Box) :: final_result
    integer :: i
    type(Box) :: f
    
    ! Build a simple 2-node Tree A for testing
    allocate(tree_a(2))
    
    ! Node 1: Root Node
    tree_a(1)%mbr = Box(0, 0, 10, 10)
    tree_a(1)%num_children = 1
    tree_a(1)%is_leaf = .false.
    allocate(tree_a(1)%child_indices(1))
    tree_a(1)%child_indices(1) = 2
    
    ! Node 2: Leaf Node containing the actual data box
    tree_a(2)%mbr = Box(0, 0, 10, 10)
    tree_a(2)%num_children = 0
    tree_a(2)%is_leaf = .true.

    print *, "Starting A NOT B Operation..."
    print *, "Target A Box: (0,0) to (10,10)"
    print *, "Mock B Overlap: (2,2) to (8,8) [A hole right in the middle]"
    print *, "--------------------------------------------------------"

    ! Execute (using 0 as a dummy pointer for root_b)
    call execute_a_not_b(tree_a, 1, 0_8, final_result)

    ! Output results
    print *, "Total Fragments created: ", final_result%count
    do i = 1, final_result%count
        f = final_result%elements(i)
        print *, "Fragment ", i, ": (", f%x1, ",", f%y1, ") to (", f%x2, ",", f%y2, ")"
    end do
    
    ! Cleanup
    deallocate(tree_a(1)%child_indices)
    deallocate(tree_a)
    
end program test_a_not_b
