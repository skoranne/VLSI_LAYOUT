! File    : rle_merge_gpu.f90
! Author  : Sandeep Koranne (C) 2026. (Adapted for RLE Compression)
! Purpose : Extracts Union-Find edges using Run-Length Encoding

module RLEMergeModule
  use iso_fortran_env, only : int32, int64, real64
  use GeometryModule
  use RTreeBuilderGPU
  use RTreeBuilder  
  use DataStructuresModule
  use omp_lib
  implicit none

  public :: PerformRLEMergeGPU

contains

  subroutine PerformRLEMergeGPU(uf, sorted_boxes, capacity, tree_nodes, root_index, overlap_area, overlap_perimeter)
    type(UnionFind), intent(inout) :: uf        
    type(Box), intent(in) :: sorted_boxes(:)
    integer(kind=int64), intent(in) :: capacity
    type(RTreeNode), intent(in) :: tree_nodes(:)
    integer(kind=int64), intent(in) :: root_index
    real(kind=real64), intent(out) :: overlap_area
    real(kind=real64), intent(out) :: overlap_perimeter

    integer(kind=int64) :: num_boxes, num_nodes
    integer(kind=int64) :: global_rle_count, limit_rle, valid_runs
    ! Array dimensions: 3 rows (I, J_Start, J_End)
    integer(kind=int64), allocatable :: d_rle(:,:) 
    
    integer(kind=int64), parameter :: CHUNK_SIZE = 1000000_int64
    integer(kind=int64) :: chunk_start, chunk_end, c, run_j
    real(kind=real64)   :: chunk_area, chunk_perimeter
    
    integer(kind=int64) :: i, j, k, childidx, currnode
    integer(kind=int64) :: stackptr, idx
    integer(kind=int64) :: stack(64) 
    
    ! RLE Thread-Local Trackers
    integer(kind=int64) :: j_start, j_end
    
    type(Box) :: qbox, nodembr, targetbox, tempBox
    logical :: overlapx, overlapy
    real(kind=real64) :: w, h

    num_boxes = size(sorted_boxes, kind=int64)
    num_nodes = size(tree_nodes, kind=int64)

    ! With RLE compression, we need significantly fewer allocated edges.
    ! 10 runs per box per chunk is exceptionally safe for sorted data.
    limit_rle = CHUNK_SIZE * 10_int64
    allocate(d_rle(3, limit_rle)) 
    
    overlap_area = 0.0_real64
    overlap_perimeter = 0.0_real64

    ! Map immutable geometry and the RLE buffer
    !$omp target data map(to: tree_nodes(1:num_nodes), sorted_boxes(1:num_boxes)) &
    !$omp map(alloc: d_rle(1:3, 1:limit_rle)) &
    !$omp map(tofrom: global_rle_count)

    do chunk_start = 1, num_boxes, CHUNK_SIZE
       chunk_end = min(chunk_start + CHUNK_SIZE - 1, num_boxes)
       
       global_rle_count = 0
       !$omp target update to(global_rle_count)

       chunk_area = 0.0_real64
       chunk_perimeter = 0.0_real64

       !$omp target teams distribute parallel do &
       !$omp private(i, j, k, childidx, currnode, stack, stackptr, qbox, nodembr, targetbox, tempBox, overlapx, overlapy, idx, w, h, j_start, j_end) &
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
                
                ! Reset RLE tracker at the start of every leaf node
                j_start = -1_int64
                j_end   = -1_int64

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

                      w = max(0_int32, tempBox%X2 - tempBox%X1)
                      h = max(0_int32, tempBox%Y2 - tempBox%Y1)

                      if ((w * h) > 0.0_real64) then
                         chunk_area = chunk_area + (w * h)
                      else
                         chunk_perimeter = chunk_perimeter + (2.0_real64 * (w + h))
                      end if

                      ! --- RLE TRACKING LOGIC ---
                      if (j_start == -1_int64) then
                         ! Start a new run
                         j_start = j
                         j_end   = j
                      else if (j == j_end + 1_int64) then
                         ! Extend the contiguous run
                         j_end = j
                      else
                         ! Run broken. Flush the old run to global memory.
                         !$omp atomic capture
                         idx = global_rle_count
                         global_rle_count = global_rle_count + 1
                         !$omp end atomic
                         
                         if (idx < limit_rle) then
                            d_rle(1, idx + 1) = i
                            d_rle(2, idx + 1) = j_start
                            d_rle(3, idx + 1) = j_end
                         end if
                         
                         ! Start a new run with current j
                         j_start = j
                         j_end   = j
                      end if
                   end if
                end do
                
                ! Flush any remaining run at the end of the leaf
                if (j_start /= -1_int64) then
                   !$omp atomic capture
                   idx = global_rle_count
                   global_rle_count = global_rle_count + 1
                   !$omp end atomic
                   
                   if (idx < limit_rle) then
                      d_rle(1, idx + 1) = i
                      d_rle(2, idx + 1) = j_start
                      d_rle(3, idx + 1) = j_end
                   end if
                end if

             else
                ! Internal Node Push
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
       
       ! Fetch data
       !$omp target update from(global_rle_count)
       overlap_area = overlap_area + chunk_area
       overlap_perimeter = overlap_perimeter + chunk_perimeter
       
       valid_runs = min(global_rle_count, limit_rle)
       if (global_rle_count > limit_rle) then
          print *, "WARNING: Chunk ", chunk_start, " to ", chunk_end, " exceeded RLE buffer!"
       end if

       if (valid_runs > 0) then
          !$omp target update from(d_rle(1:3, 1:valid_runs))
          
          ! --- CPU HOST EXPANSION AND MERGE ---
          do c = 1, valid_runs
             i = d_rle(1, c)
             call uf%insert(i)
             
             ! Expand the RLE run on the CPU
             do run_j = d_rle(2, c), d_rle(3, c)
                call uf%insert(run_j)
                call uf%merge(i, run_j)
             end do
          end do
       end if

    end do 
    !$omp end target data

    if (overlap_area > 0.0_real64) overlap_perimeter = 0.0_real64
    call uf%fullreduce()

    deallocate(d_rle)

  end subroutine PerformRLEMergeGPU

end module RLEMergeModule
