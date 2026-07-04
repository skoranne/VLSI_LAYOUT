! File    : pnummerge.f90
! Author  : Sandeep Koranne (C) All rights reserved.
! Purpose : OpenMP example for complex reduction
!In modern Fortran I am writing a function ST(boxes) which find pairwise intersection of
! box 'i' with 'j' and then uses an UnionFind data structure function called merge(i,j)
!to merge the indices together. The problem is that the pairwise intersection is happening
!inside a OpenMP parallel for do loop; how can I synchronize the merge(i,j) without using
!atomics or copying the whole unionfind array for each thread and merging a reduction later.
!Is there any better idea for this ?

module PNumMergeModule
  use iso_fortran_env, only : int32, int64, real64
  use CommonModule
  use GeometryModule
  use RTreeBuilder
  use DataStructuresModule
  use Utilities
  use omp_lib
  implicit none
  ! Explicitly define what is exposed to the rest of the program
  public :: EdgeBuffer, init_buffer, push_edge, PerformMerge, FindSingletonsCPU, ComputeInteractionsCPU,&
       PerformMergeWithOverlapDetection
  type :: EdgeBuffer
     integer(8), allocatable :: pairs(:,:) ! 2 x Capacity
     integer(8) :: count
     integer(8) :: capacity
  end type EdgeBuffer
  type :: IntBuffer
     integer(kind=int64), allocatable :: data(:)
     integer(kind=int64) :: count
  end type IntBuffer

contains

  subroutine init_int_buffer(buf, initial_capacity)
    type(IntBuffer), intent(out) :: buf
    integer(kind=int64), intent(in) :: initial_capacity
    allocate(buf%data(initial_capacity))
    buf%count = 0
  end subroutine init_int_buffer

  subroutine push_int(buf, val)
    type(IntBuffer), intent(inout) :: buf
    integer(kind=int64), intent(in) :: val
    integer(kind=int64), allocatable :: temp(:)

    if (buf%count == size(buf%data, kind=int64)) then
       ! Double the capacity when full
       allocate(temp(size(buf%data, kind=int64) * 2))
       temp(1:buf%count) = buf%data(1:buf%count)
       call move_alloc(temp, buf%data)
    end if
    buf%count = buf%count + 1
    buf%data(buf%count) = val
  end subroutine push_int

  ! ---------------------------------------------------------
  ! Initializes the thread-local edge buffer
  ! ---------------------------------------------------------
  subroutine init_buffer(buffer, initial_capacity)
    type(EdgeBuffer), intent(out) :: buffer
    integer(8), intent(in) :: initial_capacity

    buffer%capacity = initial_capacity
    buffer%count = 0

    ! Fix the first dimension to 2 here
    allocate(buffer%pairs(2, buffer%capacity))
  end subroutine init_buffer
  subroutine push_edge(buffer, i, j)
    type(EdgeBuffer), intent(inout) :: buffer
    integer(8), intent(in) :: i, j

    integer(8) :: new_capacity
    integer(8), allocatable :: temp_pairs(:,:)

    ! 1. Check if the buffer has reached its capacity limit
    if (buffer%count == buffer%capacity) then
       ! Standard geometric growth to maintain O(1) amortized insertion
       new_capacity = buffer%capacity * 2
       ! Allocate a temporary array with the new capacity
       allocate(temp_pairs(2, new_capacity))
       ! Copy existing data using Fortran's optimized array slicing
       temp_pairs(:, 1:buffer%count) = buffer%pairs(:, 1:buffer%count)
       ! Efficiently transfer the allocation descriptor from temp to buffer.
       ! This automatically deallocates the old buffer%pairs memory and
       ! points buffer%pairs to the new memory block.
       call move_alloc(from=temp_pairs, to=buffer%pairs)
       ! Update the capacity tracker
       buffer%capacity = new_capacity
    end if
    ! 2. Increment the edge count
    buffer%count = buffer%count + 1
    ! 3. Store the intersecting indices
    buffer%pairs(1, buffer%count) = i
    buffer%pairs(2, buffer%count) = j
  end subroutine push_edge

  subroutine process_edges(uf,eb)
    ! We fix the first dimension to 2, and leave the second as assumed-shape
    type(UnionFind), intent(inout) :: uf
    type(EdgeBuffer), intent(in) :: eb
    integer :: k
    do k = 1, eb%count
       call uf%insert( eb%pairs(1,k) )
       call uf%insert( eb%pairs(2, k) )
       !write(*,*) 'Merging: ', eb%pairs(1, k), ' with ', eb%pairs(2, k)
       call uf%merge(eb%pairs(1, k), eb%pairs(2, k))
    end do
  end subroutine process_edges
  subroutine PerformMergeWithOverlapDetection(uf, sorted_boxes, capacity, tree_nodes, root_index, overlap_area, overlap_perimeter, area_overlap_roots)
    type(UnionFind), intent(out) :: uf
    type(Box), intent(in)        :: sorted_boxes(:)
    integer(kind=int64), intent(in) :: capacity
    type(RTreeNode), intent(in)  :: tree_nodes(:)
    integer(kind=int64), intent(in) :: root_index
    real(kind=real64), intent(out)  :: overlap_area
    real(kind=real64), intent(out)  :: overlap_perimeter
    ! --- NEW OUTPUT: Returns unique roots of components with finite area overlap ---
    integer(kind=int64), allocatable, intent(out) :: area_overlap_roots(:) 

    integer(kind=int64) :: num_boxes, i, k, j
    integer(kind=int64), parameter :: K_STACK_SIZE = 256
    integer(kind=int64) :: Stack(K_STACK_SIZE), StackPtr, curr_index, child_idx
    logical             :: overlapx, overlapy
    type(RTreeNode)     :: currNode, childNode
    type(Box)           :: tempBox, boxI, boxK
    type(EdgeBuffer), allocatable :: buffers(:)

    real(kind=real64), allocatable :: overlap_areas(:), overlap_perimeters(:)
    integer :: nthreads, tid

    ! --- NEW VARIABLES FOR SPARSE COLLECTION ---
    type(IntBuffer), allocatable :: area_buffers(:)
    integer(kind=int64) :: total_area_overlaps, unique_count, box_id
    integer(kind=int64), allocatable :: raw_roots(:)

    nthreads = omp_get_max_threads()
    allocate(buffers(nthreads), overlap_areas(nthreads), overlap_perimeters(nthreads), area_buffers(nthreads))

    do i = 1, nthreads
       call init_buffer(buffers(i), initial_capacity=10000_int64)
       call init_int_buffer(area_buffers(i), initial_capacity=1024_int64) ! Initialize sparse buffer
       overlap_areas(i) = 0.0
       overlap_perimeters(i) = 0.0
    end do

    num_boxes = size(sorted_boxes, kind=int64)
    call uf%init(num_boxes)

    if( num_boxes == 1 ) then
       uf%arr(1) = 0
       overlap_area = 0.0_real64
       overlap_perimeter = 0.0_real64
       allocate(area_overlap_roots(0)) ! Return empty
       return
    end if

    !$omp parallel do private(i, k, Stack, StackPtr, curr_index, child_idx, overlapx, overlapy, currNode, childNode, boxI, boxK, tempBox, tid) schedule(dynamic)
    do i = 1, num_boxes
       tid = omp_get_thread_num() + 1
       boxI = sorted_boxes(i)

       ! Inline Root MBR Check
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

                   ! Inlined box_interact
                   overlapx = max(boxI%x1, boxK%x1) <= min(boxI%x2, boxK%x2)
                   overlapy = max(boxI%y1, boxK%y1) <= min(boxI%y2, boxK%y2)

                   if (overlapx .and. overlapy) then
                      call push_edge(buffers(tid), i, k)

                      ! Calculate intersection box manually
                      tempBox%x1 = max(boxI%x1, boxK%x1)
                      tempBox%y1 = max(boxI%y1, boxK%y1)
                      tempBox%x2 = min(boxI%x2, boxK%x2)
                      tempBox%y2 = min(boxI%y2, boxK%y2)

                      ! Inlined area/perimeter logic
                      if (box_area(tempBox) > 0.0) then
                         overlap_areas(tid) = overlap_areas(tid) + box_area(tempBox)

                         ! --- NEW: Record sparse overlap IDs ---
                         call push_int(area_buffers(tid), i)
                         call push_int(area_buffers(tid), k)
                      else
                         overlap_perimeters(tid) = overlap_perimeters(tid) + box_perimeter(tempBox)
                      end if
                   end if
                end do
             else
                do child_idx = currNode%ChildStart, currNode%ChildStart + currNode%NumChildren - 1
                   childNode = tree_nodes(child_idx)
                   overlapx = max(childNode%mbr%x1, boxI%x1) <= min(childNode%mbr%x2, boxI%x2)
                   overlapy = max(childNode%mbr%y1, boxI%y1) <= min(childNode%mbr%y2, boxI%y2)
                   if (overlapx .and. overlapy) then
                      StackPtr = StackPtr + 1
                      Stack(StackPtr) = child_idx
                   end if
                end do
             end if
          end do
       end if
    end do

    ! Sequential reduction
    overlap_area = sum(overlap_areas)
    overlap_perimeter = sum(overlap_perimeters)
    if (abs(overlap_area) > K_SMALL_EPSILON) overlap_perimeter = 0.0

    do tid = 1, nthreads
       call process_edges(uf, buffers(tid))
    end do

    ! Ensure all components point directly to their absolute root
    call uf%fullreduce()

    ! ---------------------------------------------------------
    ! NEW: Harvest, Resolve Roots, and Unique the Sparse Output
    ! ---------------------------------------------------------

    ! 1. Count total elements to allocate flat array
    total_area_overlaps = 0
    do tid = 1, nthreads
       total_area_overlaps = total_area_overlaps + area_buffers(tid)%count
    end do

    allocate(raw_roots(total_area_overlaps))

    ! 2. Flatten buffers and resolve to their ultimate Union-Find roots
    unique_count = 1
    do tid = 1, nthreads
       do j = 1, area_buffers(tid)%count
          box_id = area_buffers(tid)%data(j)
          ! Replace uf%arr(box_id) with your UnionFind root accessor if different
          raw_roots(unique_count) = uf%arr(box_id) 
          unique_count = unique_count + 1
       end do
    end do

    ! 3. Sort the roots array (you will need to provide/call a standard quicksort/radixsort here)
    if (total_area_overlaps > 0) then
       call sort_int64(raw_roots)

       ! 4. Remove duplicates in-place
       unique_count = 1
       do j = 2, total_area_overlaps
          if (raw_roots(j) /= raw_roots(unique_count)) then
             unique_count = unique_count + 1
             raw_roots(unique_count) = raw_roots(j)
          end if
       end do

       ! 5. Allocate and populate the final output array
       allocate(area_overlap_roots(unique_count))
       area_overlap_roots(1:unique_count) = raw_roots(1:unique_count)
    else
       allocate(area_overlap_roots(0))
    end if

  end subroutine PerformMergeWithOverlapDetection

  subroutine PerformMerge(uf, sorted_boxes, capacity, tree_nodes, root_index, overlap_area, overlap_perimeter)
    type(UnionFind), intent(out) :: uf
    type(Box), intent(in)        :: sorted_boxes(:)
    integer(kind=int64), intent(in) :: capacity
    type(RTreeNode), intent(in)  :: tree_nodes(:)
    integer(kind=int64), intent(in) :: root_index
    real(kind=real64), intent(out)  :: overlap_area
    real(kind=real64), intent(out)  :: overlap_perimeter

    integer(kind=int64) :: num_boxes, i, k
    integer(kind=int64), parameter :: K_STACK_SIZE = 256
    integer(kind=int64) :: Stack(K_STACK_SIZE), StackPtr, curr_index, child_idx
    logical             :: overlapx, overlapy
    type(RTreeNode)     :: currNode, childNode
    type(Box)           :: tempBox, boxI, boxK
    type(EdgeBuffer), allocatable :: buffers(:)

    real(kind=real64), allocatable :: overlap_areas(:), overlap_perimeters(:)
    integer :: nthreads, tid

    nthreads = omp_get_max_threads()
    allocate(buffers(nthreads), overlap_areas(nthreads), overlap_perimeters(nthreads))

    do i = 1, nthreads
       call init_buffer(buffers(i), initial_capacity=10000_int64)
       overlap_areas(i) = 0.0
       overlap_perimeters(i) = 0.0
    end do

    num_boxes = size(sorted_boxes, kind=int64)
    call uf%init(num_boxes)
    if( num_boxes == 1 ) then
       uf%arr(1) = 0
       overlap_area = 0.0_real64
       overlap_perimeter = 0.0_real64
       return
    end if
    if( root_index == 0 ) error stop "ERROR: No PNUM proceessing without RTRee"
    !$omp parallel do private(i, k, Stack, StackPtr, curr_index, child_idx, overlapx, overlapy, currNode, childNode, boxI, boxK, tempBox, tid) schedule(dynamic)
    do i = 1, num_boxes
       tid = omp_get_thread_num() + 1
       boxI = sorted_boxes(i)

       ! Inline Root MBR Check
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

                   ! Inlined box_interact
                   overlapx = max(boxI%x1, boxK%x1) <= min(boxI%x2, boxK%x2)
                   overlapy = max(boxI%y1, boxK%y1) <= min(boxI%y2, boxK%y2)

                   if (overlapx .and. overlapy) then
                      call push_edge(buffers(tid), i, k)

                      ! Calculate intersection box manually
                      tempBox%x1 = max(boxI%x1, boxK%x1)
                      tempBox%y1 = max(boxI%y1, boxK%y1)
                      tempBox%x2 = min(boxI%x2, boxK%x2)
                      tempBox%y2 = min(boxI%y2, boxK%y2)

                      ! Inlined area/perimeter logic
                      if (box_area(tempBox) > 0.0) then
                         overlap_areas(tid) = overlap_areas(tid) + box_area(tempBox)
                      else
                         overlap_perimeters(tid) = overlap_perimeters(tid) + box_perimeter(tempBox)
                      end if
                   end if
                end do
             else
                do child_idx = currNode%ChildStart, currNode%ChildStart + currNode%NumChildren - 1
                   childNode = tree_nodes(child_idx)
                   overlapx = max(childNode%mbr%x1, boxI%x1) <= min(childNode%mbr%x2, boxI%x2)
                   overlapy = max(childNode%mbr%y1, boxI%y1) <= min(childNode%mbr%y2, boxI%y2)
                   if (overlapx .and. overlapy) then
                      StackPtr = StackPtr + 1
                      Stack(StackPtr) = child_idx
                   end if
                end do
             end if
          end do
       end if
    end do

    ! Sequential reduction
    overlap_area = sum(overlap_areas)
    overlap_perimeter = sum(overlap_perimeters)
    if (abs(overlap_area) > K_SMALL_EPSILON) overlap_perimeter = 0.0

    do tid = 1, nthreads
       call process_edges(uf, buffers(tid))
    end do
    call uf%fullreduce()
  end subroutine PerformMerge

  !> Check for Single computation vs GPU
  subroutine FindSingletonsCPU(sorted_boxes, tree_nodes, root_index, is_singleton, num_singletons)
    type(Box), intent(in)             :: sorted_boxes(:)
    type(RTreeNode), intent(in)       :: tree_nodes(:)
    integer(kind=int64), intent(in)   :: root_index
    logical, allocatable, intent(out) :: is_singleton(:)
    integer(kind=int64), intent(out)  :: num_singletons

    integer(kind=int64) :: num_boxes
    integer(kind=int64) :: i, j, currnode, childidx

    ! --- Explicit Thread-Local Stack ---
    integer(kind=int64), parameter :: K_STACK_SIZE = 256
    integer(kind=int64) :: stack(K_STACK_SIZE)
    integer(kind=int64) :: stackptr

    ! --- Inlined Math Variables ---
    type(Box) :: qbox, nodembr, targetbox
    logical   :: overlapx, overlapy

    num_boxes = size(sorted_boxes, kind=int64)

    ! Allocate the output boolean mask to mirror the boxes array
    allocate(is_singleton(num_boxes))

    ! Assume all boxes are singletons until proven otherwise
    is_singleton = .true.
    num_singletons = 0

    ! Use dynamic scheduling for the CPU to balance dense vs. sparse bounding box regions
    !$omp parallel do schedule(dynamic) &
    !$omp private(i, j, currnode, childidx, stack, stackptr, qbox, nodembr, targetbox, overlapx, overlapy)
    do i = 1, num_boxes
       qbox = sorted_boxes(i)
       stackptr = 1
       stack(stackptr) = root_index

       ! We name the loop so we can break out of it instantly
       search_tree: do while (stackptr > 0)

          ! Pop current node
          currnode = stack(stackptr)
          stackptr = stackptr - 1
          nodembr = tree_nodes(currnode)%mbr

          ! 1. Internal Node / Root MBR Overlap check (<= includes edge sharing)
          overlapx = max(nodembr%x1, qbox%x1) <= min(nodembr%x2, qbox%x2)
          overlapy = max(nodembr%y1, qbox%y1) <= min(nodembr%y2, qbox%y2)
          if (.not. (overlapx .and. overlapy)) cycle search_tree

          ! 2. Process based on node type
          if (tree_nodes(currnode)%IsLeaf) then

             ! Iterate exactly over the known number of children (No ghost boxes)
             do j = tree_nodes(currnode)%ChildStart, tree_nodes(currnode)%ChildStart + tree_nodes(currnode)%NumChildren - 1

                ! A box cannot intersect itself
                if (j == i) cycle

                targetbox = sorted_boxes(j)

                ! Strict geometry overlap check
                overlapx = max(targetbox%x1, qbox%x1) <= min(targetbox%x2, qbox%x2)
                if (overlapx) then
                   overlapy = max(targetbox%y1, qbox%y1) <= min(targetbox%y2, qbox%y2)

                   if (overlapy) then
                      ! Interaction found! Mark as false and immediately stop searching.
                      is_singleton(i) = .false.
                      exit search_tree
                   end if
                end if

             end do

          else

             ! Internal node: Push exact contiguous children to the stack
             do j = tree_nodes(currnode)%ChildStart, tree_nodes(currnode)%ChildStart + tree_nodes(currnode)%NumChildren - 1
                if (stackptr < K_STACK_SIZE) then
                   stackptr = stackptr + 1
                   stack(stackptr) = j
                else
                   ! Safety catch in case of incredibly deep trees
                   error stop "ERROR: EXPLICIT STACK OVERFLOW"
                end if
             end do

          end if
       end do search_tree
    end do

    ! Quickly tally the singletons using Fortran's highly optimized
    num_singletons = count(is_singleton)

  end subroutine FindSingletonsCPU
  subroutine ComputeInteractionsCPU( tree_nodes, number_nodes, sorted_boxes, num_boxes, root_index, interaction_count )
    integer(kind=int64), intent(in) :: number_nodes, num_boxes
    type(Box), intent(in)           :: sorted_boxes(num_boxes)
    type(RTreeNode), intent(in)     :: tree_nodes(number_nodes)
    integer(kind=int64), intent(in) :: root_index
    integer(kind=int64), intent(out):: interaction_count

    integer(kind=int64) :: i, k

    ! --- Inlined Search Tree Variables ---
    integer(kind=int64), parameter  :: K_STACK_SIZE = 256
    integer(kind=int64) :: Stack(K_STACK_SIZE)
    integer(kind=int64) :: StackPtr
    integer(kind=int64) :: curr_index, child_idx
    logical             :: overlapx, overlapy
    type(RTreeNode)     :: currNode, childNode

    interaction_count = 0

    !$komp target enter data map(to: tree_nodes(1:number_nodes), sorted_boxes(1:num_boxes))
    ! Using dynamic scheduling to handle work-imbalance in dense regions
    !$omp parallel do schedule(dynamic) &
    !$omp private(i, k, Stack, StackPtr, curr_index, child_idx, overlapx, overlapy, currNode, childNode) &
    !$omp reduction(+:interaction_count)
    do i = 1, num_boxes

       if( number_nodes == 0 ) cycle

       ! Inline Root MBR Overlap Check
       overlapx = max(tree_nodes(root_index)%mbr%x1, sorted_boxes(i)%x1) <= min(tree_nodes(root_index)%mbr%x2, sorted_boxes(i)%x2)
       if (overlapx) then
          overlapy = max(tree_nodes(root_index)%mbr%y1, sorted_boxes(i)%y1) <= min(tree_nodes(root_index)%mbr%y2, sorted_boxes(i)%y2)
       else
          overlapy = .false.
       end if

       ! Only traverse if the root overlaps
       if (overlapx .and. overlapy) then
          StackPtr = 1
          Stack(StackPtr) = root_index

          do while (StackPtr > 0)
             curr_index = Stack(StackPtr)
             StackPtr = StackPtr - 1
             currNode = tree_nodes(curr_index)

             if( currNode%IsLeaf ) then
                ! Direct interaction check: No leafboxes array needed!
                do k = currNode%ChildStart, currNode%ChildStart + currNode%NumChildren - 1
                   ! Ensure we don't double count interactions
                   if( k <= i ) cycle

                   ! Inline box_overlap check
                   overlapx = max(sorted_boxes(i)%x1, sorted_boxes(k)%x1) < min(sorted_boxes(i)%x2, sorted_boxes(k)%x2) !> OVERLAP
                   if (overlapx) then
                      overlapy = max(sorted_boxes(i)%y1, sorted_boxes(k)%y1) < min(sorted_boxes(i)%y2, sorted_boxes(k)%y2) !> OVERLAP
                      if (overlapy) then
                         interaction_count = interaction_count + 1
                      end if
                   end if
                end do
             else
                ! Internal node traversal
                do child_idx = currNode%ChildStart, currNode%ChildStart + currNode%NumChildren - 1
                   childNode = tree_nodes(child_idx)

                   overlapx = max( childNode%mbr%x1, sorted_boxes(i)%x1 ) <= min( childNode%mbr%x2, sorted_boxes(i)%x2 )
                   if (overlapx) then
                      overlapy = max( childNode%mbr%y1, sorted_boxes(i)%y1 ) <= min( childNode%mbr%y2, sorted_boxes(i)%y2 )
                      if (overlapy) then
                         StackPtr = StackPtr + 1
                         Stack(StackPtr) = child_idx
                      end if
                   end if
                end do
             end if
          end do
       end if
    end do
  end subroutine ComputeInteractionsCPU

end module PNumMergeModule


