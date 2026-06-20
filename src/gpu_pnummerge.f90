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
    type(RTreeNodeGPU), intent(in) :: tree_nodes(num_nodes)
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


  subroutine FindSingletonsGPU(sorted_boxes, tree_nodes, root_index, is_singleton, num_singletons)
    type(Box), intent(in) :: sorted_boxes(:)
    type(RTreeNodeGPU), intent(in) :: tree_nodes(:)
    integer(kind=int64), intent(in) :: root_index
    logical, allocatable, intent(out) :: is_singleton(:)
    integer(kind=int64), intent(out) :: num_singletons

    integer(kind=int64) :: num_boxes, num_nodes
    integer(kind=int64) :: i, j, k, childidx, currnode
    integer(kind=int64) :: stackptr
    integer(kind=int64) :: stack(64)

    type(Box) :: qbox, nodembr, targetbox
    logical :: overlapx, overlapy

    num_boxes = size(sorted_boxes, kind=int64)
    num_nodes = size(tree_nodes, kind=int64)

    ! Allocate the output boolean mask to mirror the boxes array
    allocate(is_singleton(num_boxes))

    ! Assume all boxes are singletons until proven otherwise
    is_singleton = .true.
    num_singletons = 0

    ! Map the immutable tree to the GPU, and the is_singleton array back to CPU
    !$omp target data map(to: tree_nodes(1:num_nodes), sorted_boxes(1:num_boxes)) &
    !$omp map(tofrom: is_singleton(1:num_boxes))

    ! No default(none) or shared() needed - let the compiler autoscope safely
    !$omp target teams distribute parallel do &
    !$omp private(i, j, k, childidx, currnode, stack, stackptr, qbox, nodembr, targetbox, overlapx, overlapy)
    do i = 1, num_boxes
       qbox = sorted_boxes(i)
       stackptr = 1
       stack(stackptr) = root_index

       ! We name the loop so we can break out of it instantly
       search_tree: do while (stackptr > 0)
          currnode = stack(stackptr)
          stackptr = stackptr - 1
          nodembr = tree_nodes(currnode)%mbr

          ! MBR Overlap check (<= includes edge sharing)
          overlapx = max(nodembr%X1, qbox%X1) <= min(nodembr%X2, qbox%X2)
          overlapy = max(nodembr%Y1, qbox%Y1) <= min(nodembr%Y2, qbox%Y2)
          if (.not. (overlapx .and. overlapy)) cycle search_tree

          if (tree_nodes(currnode)%IsLeaf) then
             do k = 0, tree_nodes(currnode)%NumChildren - 1
                j = tree_nodes(currnode)%ChildStart + k

                ! A box cannot intersect itself
                if (j == i) cycle 

                targetbox = sorted_boxes(j)

                ! Strict geometry overlap check (<= catches zero-area edge touching)
                overlapx = max(targetbox%X1, qbox%X1) <= min(targetbox%X2, qbox%X2)
                overlapy = max(targetbox%Y1, qbox%Y1) <= min(targetbox%Y2, qbox%Y2)

                if (overlapx .and. overlapy) then
                   ! Interaction found! Mark as false and immediately stop searching.
                   is_singleton(i) = .false.
                   exit search_tree 
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
       end do search_tree
    end do
    !$omp end target data

    ! Quickly tally the singletons on the Host CPU
    do i = 1, num_boxes
       if (is_singleton(i)) then
          num_singletons = num_singletons + 1
       end if
    end do

  end subroutine FindSingletonsGPU

end module GPUMergeModule
