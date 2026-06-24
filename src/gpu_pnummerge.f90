! File    : gpu_pnummerge.f90
! Author  : Sandeep Koranne (C) 2026. (Adapted for GPU Offload)
! Purpose : Combines GPU R-Tree traversal with edge extraction for Union-Find
! File    : gpu_pnummerge.f90
! Author  : Sandeep Koranne (C) 2026. 
! Purpose : Handles massive-scale R-Tree Union-Find using GPU chunking without mapping segfaults

module GPUMergeModule
  use iso_fortran_env, only : int32, int64, real64
  use GeometryModule
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

  ! Proven more difficult than estimated
  ! Author  : Sandeep Koranne (C) 2026. (Adapted for GPU Offload)
  ! Purpose : Identifies boxes that share no area or edges with any other box
  subroutine FindSingletonsGPU(num_boxes, sorted_boxes, num_nodes, tree_nodes, root_index, is_singleton, num_singletons)
    integer(kind=int64), intent(in) :: num_boxes, num_nodes  
    type(Box), intent(in)             :: sorted_boxes(num_boxes)
    type(RTreeNode), intent(in)    :: tree_nodes(num_nodes)
    integer(kind=int64), intent(in)   :: root_index
    logical, allocatable, intent(out) :: is_singleton(:)
    integer(kind=int64), intent(out)  :: num_singletons
    integer(kind=int64) :: i, j, currnode, childidx
    ! Moderate stack perfectly safe for L1/Shared Memory limits
    integer(kind=int64), parameter :: K_STACK_SIZE = 256
    integer(kind=int64) :: stack(K_STACK_SIZE)
    integer(kind=int64) :: stackptr
    type(Box) :: qbox, nodembr, targetbox
    ! Notice: All 'logical' variables have been completely removed.
    allocate(is_singleton(num_boxes))
    is_singleton = .true.
    num_singletons = 0

    !$omp target data map(to: tree_nodes(1:num_nodes), sorted_boxes(1:num_boxes)) &
    !$omp map(tofrom: is_singleton(1:num_boxes))

    !$omp target teams distribute parallel do &
    !$omp private(i, j, currnode, childidx, stack, stackptr, qbox, nodembr, targetbox)
    do i = 1, num_boxes
       qbox = sorted_boxes(i)
       stackptr = 1
       stack(stackptr) = root_index

       do while (stackptr > 0)
          ! Pop current node
          currnode = stack(stackptr)
          stackptr = stackptr - 1
          nodembr = tree_nodes(currnode)%mbr

          ! 1. Evaluate logic DIRECTLY inside the IF to avoid NVFORTRAN scalar logical bugs
          if (max(nodembr%x1, qbox%x1) <= min(nodembr%x2, qbox%x2)) then
             if (max(nodembr%y1, qbox%y1) <= min(nodembr%y2, qbox%y2)) then

                ! 2. Process based on node type
                if (tree_nodes(currnode)%IsLeaf) then
                   do j = tree_nodes(currnode)%ChildStart, tree_nodes(currnode)%ChildStart + tree_nodes(currnode)%NumChildren - 1
                      ! Replace 'cycle' with an active check to maintain strict warp sync
                      if (j /= i) then
                         targetbox = sorted_boxes(j)

                         ! Direct evaluation for final intersection check
                         if (max(targetbox%x1, qbox%x1) <= min(targetbox%x2, qbox%x2)) then
                            if (max(targetbox%y1, qbox%y1) <= min(targetbox%y2, qbox%y2)) then
                               ! Interaction found!
                               is_singleton(i) = .false.

                               ! Safely kill both loops without using named exits
                               stackptr = 0 ! Forces the outer while-loop to terminate
                               exit         ! Escapes the inner do-j loop
                            end if
                         end if
                      end if

                   end do

                else
                   ! Internal node: Push exact contiguous children to the stack
                   do j = tree_nodes(currnode)%ChildStart, tree_nodes(currnode)%ChildStart + tree_nodes(currnode)%NumChildren - 1
                      if (stackptr < K_STACK_SIZE) then
                         stackptr = stackptr + 1
                         stack(stackptr) = j
                      end if
                   end do
                end if

             end if
          end if
       end do
    end do
    
    !$omp end target data

    ! CPU Tally
    num_singletons = count(is_singleton)
  end subroutine FindSingletonsGPU

end module GPUMergeModule

#ifdef DOCUMENTATION_RESUTLS
(base) skoranne@spark-bc08:~/GITHUB/VLSI_LAYOUT/src$ ./test_interaction.exe /scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/MW16_DATA/MW64_L67_D20.bin z.bin 1 2 3 4
 Reading 1st filename:
 /scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/MW16_DATA/MW64_L67_D20.bin                                                          
 TOTAL NODES FOR CPU RT =                  54978018
         CPUSortTree     71.60 CPU seconds.     71.60 REAL seconds. FULL TIME:    71.60 MEM: VM    116204836 RSS:     15333472
 |CPU TOTAL INTERACTIONS| =                 255079360
     CPUInteractions     92.15 CPU seconds.     92.15 REAL seconds. FULL TIME:   163.76 MEM: VM    116361892 RSS:     15336548
 |CPU NUM_SINGLETONS| =                  11319424
        CPUSingleton     66.67 CPU seconds.     66.67 REAL seconds. FULL TIME:   230.43 MEM: VM    116363684 RSS:     18557928
CPU OVLP AREA = ****************** CPU OVLP PERIMETER =             0.0000                                                    
 |Roots| =                 114184192 |Rects| =                         0                                                      
             CPUPNUM    147.84 CPU seconds.    147.84 REAL seconds. FULL TIME:   378.27 MEM: VM    145967140 RSS:     25198616
 ===============================GPU MODE ========================================                                             
Loaded :    824670208 BBOX =         5520       43357    23944100    29341447 |T| =     54978018                              
             GPUSort     67.98 CPU seconds.     67.98 REAL seconds. FULL TIME:   450.58 MEM: VM    145967140 RSS:     28420004
 Tree constructed:                  54978018  |RT| =      54978018
            GPURTree      1.11 CPU seconds.      1.11 REAL seconds. FULL TIME:   451.69 MEM: VM    145967140 RSS:     30567592
 |GPU TOTAL INTERACTIONS| =                 255079360
     GPUInteractions      9.12 CPU seconds.      9.12 REAL seconds. FULL TIME:   460.81 MEM: VM    148588580 RSS:     30567596
 |GPU NUM_SINGLETONS| =                  11319424
        GPUSingleton     37.88 CPU seconds.     37.88 REAL seconds. FULL TIME:   498.69 MEM: VM    152258596 RSS:     30567612
GPU OVLP AREA =             0.0000 GPU OVLP PERIMETER =             0.0000
                PNUM      0.00 CPU seconds.      0.00 REAL seconds. FULL TIME:   498.69 MEM: VM    152258596 RSS:     30567612
RLE GPU OVLP AREA =             0.0000 RLE GPU OVLP PERIMETER =             0.0000
             PNUMRLW      0.00 CPU seconds.      0.00 REAL seconds. FULL TIME:   498.69 MEM: VM    152258596 RSS:     30567612
(base) skoranne@spark-bc08:~/GITHUB/VLSI_LAYOUT/src$ 
#endif
