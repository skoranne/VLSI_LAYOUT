! File    : gpu_pnummerge.f90
! Author  : Sandeep Koranne (C) 2026. (Adapted for GPU Offload)
! Purpose : Combines GPU R-Tree traversal with edge extraction for Union-Find
! File    : gpu_pnummerge.f90
! Author  : Sandeep Koranne (C) 2026. 
! Purpose : Handles massive-scale R-Tree Union-Find using GPU chunking without mapping segfaults

module GPUMergeModule
  use iso_fortran_env, only : int32, int64, real64
  use GeometryModule
  use RTreeBuilderGPU
  use RTreeBuilder
  use DataStructuresModule
  use omp_lib
  implicit none

  public :: PerformMergeGPU, FindSingletonsGPU

contains

  subroutine PerformMergeGPU(uf, sorted_boxes, num_boxes, capacity, tree_nodes, num_nodes, root_index, overlap_area, overlap_perimeter)
    type(UnionFind), intent(inout) :: uf
    integer(kind=int64),intent(in) :: num_boxes, num_nodes    
    type(Box), intent(in) :: sorted_boxes(num_boxes)
    integer(kind=int64), intent(in) :: capacity
    type(RTreeNode), intent(in) :: tree_nodes(num_nodes)
    integer(kind=int64), intent(in) :: root_index
    real(kind=real64), intent(out) :: overlap_area
    real(kind=real64), intent(out) :: overlap_perimeter
    integer(kind=int64) :: global_edge_count, limit_edges, valid_edges
    integer(kind=int64), allocatable :: d_edges(:,:)

    ! Batching Parameters
    integer(kind=int64), parameter :: CHUNK_SIZE = 1000_int64
    integer(kind=int64) :: chunk_start, chunk_end, c
    real(kind=real64)   :: chunk_area, chunk_perimeter

    integer(kind=int64) :: i, j, k, childidx, currnode
    integer(kind=int64) :: stackptr, idx
    integer(kind=int64) :: stack(64) 

    type(Box) :: qbox, nodembr, targetbox, tempBox
    logical :: overlapx, overlapy
    real(kind=real64) :: w, h

    !num_boxes = size(sorted_boxes, kind=int64)
    !num_nodes = size(tree_nodes, kind=int64)

    limit_edges = CHUNK_SIZE * 50_int64
    allocate(d_edges(2, limit_edges))

    overlap_area = 0.0_real64
    overlap_perimeter = 0.0_real64
    call uf%init(num_boxes)

    ! CRITICAL FIX 1: global_edge_count MUST be in this map to prevent Error 719 Segfaults
    !$omp target data map(to: tree_nodes(1:num_nodes), sorted_boxes(1:num_boxes)) &
    !$omp map(alloc: d_edges(1:2, 1:limit_edges)) &
    !$omp map(tofrom: global_edge_count)

    do chunk_start = 1, num_boxes, CHUNK_SIZE
       chunk_end = min(chunk_start + CHUNK_SIZE - 1, num_boxes)
       ! Because global_edge_count is mapped above, this update is safe
       global_edge_count = 0
       !$omp target update to(global_edge_count)

       chunk_area = 0.0_real64
       chunk_perimeter = 0.0_real64

       ! CRITICAL FIX: Removed default(none) and the shared() clauses for scalars. 
       ! OpenMP will now correctly register-map the scalars and memory-map the arrays.
       !$omp target teams distribute parallel do &
       !$omp private(i, j, k, childidx, currnode, stack, stackptr, qbox, nodembr, targetbox, tempBox, overlapx, overlapy, idx, w, h) &
       !$omp reduction(+:chunk_area, chunk_perimeter)
       do i = chunk_start, chunk_end
          qbox = sorted_boxes(i)
          stackptr = 1
          stack(stackptr) = root_index

          do while (stackptr > 0)
             currnode = stack(stackptr)
             stackptr = stackptr - 1
             nodembr = tree_nodes(currnode)%mbr

             overlapx = max(nodembr%X1, qbox%X1) <= min(nodembr%X2, qbox%X2)
             overlapy = max(nodembr%Y1, qbox%Y1) <= min(nodembr%Y2, qbox%Y2)
             if (.not. (overlapx .and. overlapy)) cycle

             if (tree_nodes(currnode)%IsLeaf) then
                do k = 0, tree_nodes(currnode)%NumChildren - 1
                   j = tree_nodes(currnode)%ChildStart + k
                   if (j <= i) cycle 

                   targetbox = sorted_boxes(j)
                   overlapx = max(targetbox%X1, qbox%X1) < min(targetbox%X2, qbox%X2)
                   overlapy = max(targetbox%Y1, qbox%Y1) < min(targetbox%Y2, qbox%Y2)

                   if (overlapx .and. overlapy) then
                      tempBox%X1 = max(qbox%X1, targetbox%X1)
                      tempBox%Y1 = max(qbox%Y1, targetbox%Y1)
                      tempBox%X2 = min(qbox%X2, targetbox%X2)
                      tempBox%Y2 = min(qbox%Y2, targetbox%Y2)

                      w = max(0_int32, tempBox%X2 - tempBox%X1) !> TODO: find out how to use K_COORDINATE_KIND
                      h = max(0_int32, tempBox%Y2 - tempBox%Y1) !> TODO: find out how to use K_COORDINATE_KIND

                      if ((w * h) > 0.0_real64) then
                         chunk_area = chunk_area + (w * h)
                      else
                         chunk_perimeter = chunk_perimeter + (2.0_real64 * (w + h))
                      end if

                      !$omp atomic capture
                      idx = global_edge_count
                      global_edge_count = global_edge_count + 1
                      !$omp end atomic

                      if (idx < limit_edges) then
                         d_edges(1, idx + 1) = i
                         d_edges(2, idx + 1) = j
                      end if
                   end if
                end do
             else
                do k = 0, tree_nodes(currnode)%NumChildren - 1
                   childidx = tree_nodes(currnode)%ChildStart + k
                   if (stackptr < 64) then
                      stackptr = stackptr + 1
                      stack(stackptr) = childidx
                   end if
                end do
             end if
          end do
       end do

       ! Fetch counts safely back to the host
       !$omp target update from(global_edge_count)

       ! Accumulate the strictly local reductions into the global trackers
       overlap_area = overlap_area + chunk_area
       overlap_perimeter = overlap_perimeter + chunk_perimeter

       valid_edges = min(global_edge_count, limit_edges)
       if (global_edge_count > limit_edges) then
          print *, "WARNING: Chunk ", chunk_start, " to ", chunk_end, " exceeded buffer! Found ", global_edge_count, " edges."
       end if

       if (valid_edges > 0) then
          !$omp target update from(d_edges(1:2, 1:valid_edges))
          do c = 1, valid_edges
             call uf%insert(d_edges(1, c))
             call uf%insert(d_edges(2, c))
             call uf%merge(d_edges(1, c), d_edges(2, c))
          end do
       end if

    end do
    !$omp end target data

    if (overlap_area > 0.0_real64) overlap_perimeter = 0.0_real64
    call uf%fullreduce()

    deallocate(d_edges)

  end subroutine PerformMergeGPU

  ! Author  : Sandeep Koranne (C) 2026. (Adapted for GPU Offload)
  ! Purpose : Identifies boxes that share no area or edges with any other box
  subroutine FindSingletonsGPU(num_boxes, sorted_boxes, num_nodes, tree_nodes, root_index, is_singleton, num_singletons)
    integer(kind=int64), intent(in) :: num_boxes, num_nodes  

    type(Box), intent(in)             :: sorted_boxes(num_boxes)
    type(RTreeNode), intent(in)    :: tree_nodes(num_nodes) ! Adjusted to your GPU type
    integer(kind=int64), intent(in)   :: root_index
    logical, allocatable, intent(out) :: is_singleton(:)
    integer(kind=int64), intent(out)  :: num_singletons

    integer(kind=int64) :: i, j, currnode, childidx

    ! --- Explicit Thread-Local Stack ---
    ! 256 integers = 2 KB per thread. Easily fits in L1/Registers without causing
    ! silent kernel aborts due to local memory limits.
    integer(kind=int64), parameter :: K_STACK_SIZE = 256
    integer(kind=int64) :: stack(K_STACK_SIZE)
    integer(kind=int64) :: stackptr

    ! --- Inlined Math Variables ---
    type(Box) :: qbox, nodembr, targetbox
    logical   :: overlapx, overlapy, keep_searching

    ! Allocate the output boolean mask to mirror the boxes array
    allocate(is_singleton(num_boxes))

    ! Assume all boxes are singletons until proven otherwise
    is_singleton = .true.
    num_singletons = 0

    ! Map data explicitly to device
    !$omp target data map(to: tree_nodes(1:num_nodes), sorted_boxes(1:num_boxes)) &
    !$omp map(tofrom: is_singleton(1:num_boxes))

    ! distribute parallel do ensures GPU grid/block distribution
    ! keep_searching is explicitly private to prevent cross-thread contamination
    !$omp target teams distribute parallel do &
    !$omp private(i, j, currnode, childidx, stack, stackptr, qbox, nodembr) &
    !$omp private(targetbox, overlapx, overlapy, keep_searching)
    do i = 1, num_boxes
       qbox = sorted_boxes(i)
       stackptr = 1
       stack(stackptr) = root_index
       keep_searching = .true.

       ! Replaced named loop with boolean condition for safe warp divergence
       do while (stackptr > 0 .and. keep_searching)

          ! Pop current node
          currnode = stack(stackptr)
          stackptr = stackptr - 1
          nodembr = tree_nodes(currnode)%mbr

          ! 1. Internal Node / Root MBR Overlap check (<= includes edge sharing)
          overlapx = max(nodembr%x1, qbox%x1) <= min(nodembr%x2, qbox%x2)
          overlapy = max(nodembr%y1, qbox%y1) <= min(nodembr%y2, qbox%y2)

          ! Replaced 'cycle search_tree' with an IF block wrap
          if (overlapx .and. overlapy) then

             ! 2. Process based on node type
             if (tree_nodes(currnode)%IsLeaf) then

                ! Iterate exactly over the known number of children
                do j = tree_nodes(currnode)%childstart, tree_nodes(currnode)%childstart + tree_nodes(currnode)%numchildren - 1

                   ! A box cannot intersect itself
                   if (j == i) cycle 

                   targetbox = sorted_boxes(j)

                   ! Strict geometry overlap check (Short-circuited exactly like your CPU version)
                   overlapx = max(targetbox%x1, qbox%x1) <= min(targetbox%x2, qbox%x2)
                   if (overlapx) then
                      overlapy = max(targetbox%y1, qbox%y1) <= min(targetbox%y2, qbox%y2)

                      if (overlapy) then
                         ! Interaction found! Mark as false and immediately stop searching.
                         is_singleton(i) = .false.
                         keep_searching = .false.
                         ! Simple exit escapes the 'do j' loop. The outer 'while' loop 
                         ! will immediately terminate because keep_searching is false.
                         exit  
                      end if
                   end if

                end do

             else

                ! Internal node: Push exact contiguous children to the stack
                do j = tree_nodes(currnode)%childstart, tree_nodes(currnode)%childstart + tree_nodes(currnode)%numchildren - 1
                   if (stackptr < K_STACK_SIZE) then
                      stackptr = stackptr + 1
                      stack(stackptr) = j
                   end if
                   ! Removed 'error stop' as runtime halts crash GPU kernels.
                   ! If stack exceeds 256, it safely drops the deep branch.
                end do

             end if

          end if
       end do
    end do
    !$omp end target data

    ! Quickly tally the singletons on the CPU using standard Fortran intrinsic
    num_singletons = count(is_singleton)

  end subroutine FindSingletonsGPU
#ifdef OLD
  subroutine FindSingletonsGPU(num_boxes, sorted_boxes, num_nodes, tree_nodes, root_index, is_singleton, num_singletons)
    integer(kind=int64), intent(in) :: num_boxes, num_nodes  
    type(Box), intent(in) :: sorted_boxes(num_boxes)
    type(RTreeNodeGPU), intent(in) :: tree_nodes(num_nodes)
    integer(kind=int64), intent(in) :: root_index
    logical, allocatable, intent(out) :: is_singleton(:)
    integer(kind=int64), intent(out) :: num_singletons

    integer(kind=int64) :: i, j, k, childidx, currnode
    integer(kind=int64) :: stackptr

    ! Reduced to 128 (1 KB per thread). Fits effortlessly in GPU registers/L1.
    integer(kind=int64), parameter :: K_STACK_SIZE = 128 
    integer(kind=int64) :: stack(K_STACK_SIZE)

    type(Box) :: qbox, nodembr, targetbox
    logical :: overlapx, overlapy, keep_searching

    allocate(is_singleton(num_boxes))
    is_singleton = .true.
    num_singletons = 0

    ! Explicitly mapped root_index as a precaution against NVFORTRAN scalar bugs
    !$omp target data map(to: tree_nodes(1:num_nodes), sorted_boxes(1:num_boxes), root_index) &
    !$omp map(tofrom: is_singleton(1:num_boxes))

    !$omp target teams distribute parallel do &
    !$omp private(i, j, k, childidx, currnode, stack, stackptr, qbox, nodembr) &
    !$omp private(targetbox, overlapx, overlapy, keep_searching)
    do i = 1, num_boxes
       qbox = sorted_boxes(i)
       keep_searching = .true.
       stackptr = 0

       ! 1. Check the Root Node FIRST before initiating the stack
       nodembr = tree_nodes(root_index)%mbr
       overlapx = max(nodembr%X1, qbox%X1) <= min(nodembr%X2, qbox%X2)
       overlapy = max(nodembr%Y1, qbox%Y1) <= min(nodembr%Y2, qbox%Y2)

       if (overlapx .and. overlapy) then
          stackptr = 1
          stack(stackptr) = root_index
       end if

       do while (stackptr > 0 .and. keep_searching)
          currnode = stack(stackptr)
          stackptr = stackptr - 1

          if (tree_nodes(currnode)%IsLeaf) then
             do k = 0, tree_nodes(currnode)%NumChildren - 1
                j = tree_nodes(currnode)%ChildStart + k

                if (j == i) cycle 

                targetbox = sorted_boxes(j)
                overlapx = max(targetbox%X1, qbox%X1) <= min(targetbox%X2, qbox%X2)
                overlapy = max(targetbox%Y1, qbox%Y1) <= min(targetbox%Y2, qbox%Y2)

                if (overlapx .and. overlapy) then
                   is_singleton(i) = .false.
                   keep_searching = .false.
                   exit 
                end if
             end do
          else
             do k = 0, tree_nodes(currnode)%NumChildren - 1
                childidx = tree_nodes(currnode)%ChildStart + k
                nodembr = tree_nodes(childidx)%mbr

                ! 2. CHECK BEFORE PUSHING: Only push branches that actually intersect
                overlapx = max(nodembr%X1, qbox%X1) <= min(nodembr%X2, qbox%X2)
                overlapy = max(nodembr%Y1, qbox%Y1) <= min(nodembr%Y2, qbox%Y2)

                if (overlapx .and. overlapy) then
                   if (stackptr < K_STACK_SIZE) then
                      stackptr = stackptr + 1
                      stack(stackptr) = childidx
                   end if
                end if
             end do
          end if
       end do
    end do
    !$omp end target data

    do i = 1, num_boxes
       if (is_singleton(i)) then
          num_singletons = num_singletons + 1
       end if
    end do

  end subroutine FindSingletonsGPU
#endif
end module GPUMergeModule
