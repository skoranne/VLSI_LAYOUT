! File    : gpu_pnummerge.f90
! Author  : Sandeep Koranne (C) 2026. (Adapted for GPU Offload)
! Purpose : Combines GPU R-Tree traversal with edge extraction for Union-Find

module GPUMergeModule
  use iso_fortran_env, only : int32, int64, real64
  use GeometryModule
  use RTreeBuilderGPU
  use DataStructuresModule
  use omp_lib
  implicit none

  public :: PerformMergeGPU

contains

  !> GPU Offloaded R-Tree Traversal and Edge Extraction for Union-Find
  subroutine PerformMergeGPU(uf, sorted_boxes, capacity, tree_nodes, root_index, overlap_area, overlap_perimeter, max_edges)
    type(UnionFind), intent(inout) :: uf        
    type(Box), intent(in) :: sorted_boxes(:)
    integer(kind=int64), intent(in) :: capacity
    type(RTreeNodeGPU), intent(in) :: tree_nodes(:)
    integer(kind=int64), intent(in) :: root_index
    real(kind=real64), intent(out) :: overlap_area
    real(kind=real64), intent(out) :: overlap_perimeter
    integer(kind=int64), intent(in), optional :: max_edges 

    integer(kind=int64) :: num_boxes, num_nodes
    integer(kind=int64) :: limit_edges, global_edge_count
    integer(kind=int64), allocatable :: d_edges(:,:)

    integer(kind=int64) :: i, j, k, childidx, currnode
    integer(kind=int64) :: stackptr, idx
    integer(kind=int64) :: stack(K_MAX_TREE_DEPTH)

    type(Box) :: qbox, nodembr, targetbox, tempBox
    logical :: overlapx, overlapy

    num_boxes = size(sorted_boxes, kind=int64)
    num_nodes = size(tree_nodes, kind=int64)

    ! 1. Pre-allocate a large buffer to avoid in-kernel dynamic allocation
    if (present(max_edges)) then
       limit_edges = max_edges
    else
       limit_edges = num_boxes * 50 ! Adjust heuristic multiplier based on expected graph density
    end if

    allocate(d_edges(2, limit_edges))
    global_edge_count = 0
    overlap_area = 0.0_real64
    overlap_perimeter = 0.0_real64

    ! 2. Map data to the GPU environment
    !$omp target data map(to: tree_nodes(1:num_nodes), sorted_boxes(1:num_boxes)) &
    !$omp map(alloc: d_edges(1:2, 1:limit_edges)) &
    !$omp map(tofrom: global_edge_count, overlap_area, overlap_perimeter)

    !$omp target teams distribute parallel do default(none) &
    !$omp shared(tree_nodes, sorted_boxes, num_boxes, root_index, d_edges, limit_edges, global_edge_count) &
    !$omp private(i, j, k, childidx, currnode, stack, stackptr, qbox, nodembr, targetbox, tempBox, overlapx, overlapy, idx) &
    !$omp reduction(+:overlap_area, overlap_perimeter)
    do i = 1, num_boxes
       qbox = sorted_boxes(i)
       stackptr = 1
       stack(stackptr) = root_index

       ! Iterative Depth-First Search
       do while (stackptr > 0)
          currnode = stack(stackptr)
          stackptr = stackptr - 1
          nodembr = tree_nodes(currnode)%mbr

          ! Bounding box pruning
          overlapx = max(nodembr%X1, qbox%X1) <= min(nodembr%X2, qbox%X2)
          overlapy = max(nodembr%Y1, qbox%Y1) <= min(nodembr%Y2, qbox%Y2)
          if (.not. (overlapx .and. overlapy)) cycle

          if (tree_nodes(currnode)%IsLeaf) then
             do k = 0, tree_nodes(currnode)%NumChildren - 1
                j = tree_nodes(currnode)%ChildStart + k
                if (j <= i) cycle ! Avoid self-interaction and double counting

                targetbox = sorted_boxes(j)
                overlapx = max(targetbox%X1, qbox%X1) < min(targetbox%X2, qbox%X2)
                overlapy = max(targetbox%Y1, qbox%Y1) < min(targetbox%Y2, qbox%Y2)

                if (overlapx .and. overlapy) then

                   ! Calculate the intersected box dimensions
                   tempBox%X1 = max(qbox%X1, targetbox%X1)
                   tempBox%Y1 = max(qbox%Y1, targetbox%Y1)
                   tempBox%X2 = min(qbox%X2, targetbox%X2)
                   tempBox%Y2 = min(qbox%Y2, targetbox%Y2)

                   ! Accumulate Area and Perimeter via OpenMP Reductions
                   if (box_area(tempBox) > 0.0_real64) then
                      overlap_area = overlap_area + box_area(tempBox)
                   else
                      overlap_perimeter = overlap_perimeter + box_perimeter(tempBox)
                   end if

                   ! 3. Hardware-Optimized Atomic Capture
                   ! Safely reserve an index without locking the thread block
                   !$omp atomic capture
                   idx = global_edge_count
                   global_edge_count = global_edge_count + 1
                   !$omp end atomic

                   ! Store edge if within capacity bounds
                   if (idx < limit_edges) then
                      d_edges(1, idx + 1) = i
                      d_edges(2, idx + 1) = j
                   end if

                end if
             end do
          else
             ! Internal Node: Push to stack
             do k = 0, tree_nodes(currnode)%NumChildren - 1
                childidx = tree_nodes(currnode)%ChildStart + k
                stackptr = stackptr + 1
                stack(stackptr) = childidx
             end do
          end if
       end do
    end do
    !$omp end target data

    ! 4. Error Handling: Detect buffer saturation
    if (global_edge_count > limit_edges) then
       print *, "WARNING: Device Edge Buffer Overflow! Found ", global_edge_count, " edges. Limit was ", limit_edges
       global_edge_count = limit_edges
    end if

    ! 5. Sequential Merge on Host
    call uf%init(num_boxes)
    do i = 1, global_edge_count
       call uf%insert(d_edges(1, i))
       call uf%insert(d_edges(2, i))
       call uf%merge(d_edges(1, i), d_edges(2, i))
    end do

    if (overlap_area > 0.0_real64) overlap_perimeter = 0.0_real64
    call uf%fullreduce()

    deallocate(d_edges)

  end subroutine PerformMergeGPU

end module GPUMergeModule
