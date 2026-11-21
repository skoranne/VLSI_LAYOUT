! File   : design_boolean_impl.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Submodule for core boolean operations (OR, AND, NOT)

submodule (DesignModule) DesignBooleanImplModule
  use iso_fortran_env, only: int32, int64, real64
  use CommonModule
  use GeometryModule
  use RTreeBuilder
  use RTreeBuilderGPU
  use PolygonFractureModule
  use ProfilerModule, only: get_cpu_time, RecordTime
  use omp_lib
  implicit none

contains

  module subroutine CalculateOR( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout) :: output_layer
    integer :: alloc_status
    integer(kind=int64) :: nA, nB, nTot

    nA = input_layer_A%n_used
    nB = input_layer_B%n_used
    nTot = nA + nB

    if( allocated( output_layer%layer_boxes ) ) deallocate( output_layer%layer_boxes )
    if( allocated( output_layer%tree%tree_nodes ) ) deallocate( output_layer%tree%tree_nodes )
    if( allocated( output_layer%pnumtable%arr ) ) deallocate( output_layer%pnumtable%arr )
    output_layer%tree%root_index = 0
    output_layer%device_resident = .false.

    if( nTot == 0 ) then
       call ClearLayer( output_layer )
       return
    end if

    allocate( output_layer%layer_boxes( nTot ), stat=alloc_status )
    if( alloc_status /= 0 ) error stop "ALLOCATION FAILED in CalculateOR"
    if( nA > 0 ) output_layer%layer_boxes(1:nA) = input_layer_A%layer_boxes(1:nA)
    if( nB > 0 ) output_layer%layer_boxes(nA+1:nTot) = input_layer_B%layer_boxes(1:nB)
    output_layer%n_used = nTot
    output_layer%n_alloc = nTot
    output_layer%layerState = LAYER_STATE_NONE
    call PreprocessLayer( output_layer )
  end subroutine CalculateOR

  module subroutine CalculateAND( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout) :: output_layer
    real(kind=real64) :: t_start
    integer(kind=int64) :: num_boxesA, num_boxesB, num_boxesC, numNodesB
    integer(kind=int64), parameter :: K_DFS_STACK = 256
    integer(kind=int64) :: dfs_stack(K_DFS_STACK)
    integer(kind=int64) :: sp, curr_idx, child_idx, c_end
    integer(kind=int64) :: i, j, k, root_idx
    type(Layer), allocatable :: buffers(:)
    type(Box) :: boxA, boxB, tempBox
    type(RTreeNode) :: currNode, childNode
    integer :: nthreads, tid, alloc_status
    integer(kind=int32) :: gpu_overflow
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096
    logical :: use_gpu, swap_ab
    integer(kind=int64) :: numNodesA

    if (profiling_enabled) t_start = get_cpu_time()

    !> preprocess A,B (builds RTrees/sorting as needed)
    !$omp parallel default(shared)
    !$omp single
    !$omp task
    call PreprocessLayer( input_layer_A )
    !$omp end task
    !$omp task
    call PreprocessLayer( input_layer_B )
    !$omp end task
    !$omp taskwait
    !$omp end single
    !$omp end parallel

    num_boxesA = input_layer_A%n_used
    num_boxesB = input_layer_B%n_used
    if (num_boxesA == 0 .or. num_boxesB == 0) then
       call ClearLayer(output_layer)
       output_layer%layerState = ior(output_layer%layerState, LAYER_STATE_HEAL)
       if (profiling_enabled) call RecordTime("CalculateAND", get_cpu_time() - t_start)
       return
    end if

    use_gpu = .false.
    swap_ab = .false.
    if (gpu_offload_enabled(min(num_boxesA, num_boxesB))) then
       if (num_boxesA > num_boxesB .and. allocated(input_layer_A%tree%tree_nodes)) then
          numNodesA = size(input_layer_A%tree%tree_nodes, kind=int64)
          if (numNodesA > 0 .and. input_layer_A%tree%root_index > 0) then
             use_gpu = .true.
             swap_ab = .true.
          end if
       end if
       if (.not. use_gpu .and. allocated(input_layer_B%tree%tree_nodes)) then
          numNodesB = size(input_layer_B%tree%tree_nodes, kind=int64)
          if (numNodesB > 0 .and. input_layer_B%tree%root_index > 0) then
             use_gpu = .true.
             swap_ab = .false.
          end if
       end if
    end if

    if (use_gpu) then
       gpu_overflow = 0_int32
       if (swap_ab) then
          call ComputeANDPairsGPU(input_layer_A%tree%tree_nodes, numNodesA, &
               input_layer_A%layer_boxes(1:num_boxesA), num_boxesA, input_layer_A%tree%root_index, &
               input_layer_B%layer_boxes(1:num_boxesB), num_boxesB, output_layer%layer_boxes, num_boxesC, gpu_overflow)
       else
          call ComputeANDPairsGPU(input_layer_B%tree%tree_nodes, numNodesB, &
               input_layer_B%layer_boxes(1:num_boxesB), num_boxesB, input_layer_B%tree%root_index, &
               input_layer_A%layer_boxes(1:num_boxesA), num_boxesA, output_layer%layer_boxes, num_boxesC, gpu_overflow)
       end if
       if (gpu_overflow == 0_int32) then
          output_layer%n_used = num_boxesC
          output_layer%n_alloc = size(output_layer%layer_boxes, kind=int64)
          output_layer%device_resident = .false.
          if (allocated(output_layer%tree%tree_nodes)) deallocate(output_layer%tree%tree_nodes)
          if (allocated(output_layer%pnumtable%arr)) deallocate(output_layer%pnumtable%arr)
          output_layer%tree%root_index = 0
          output_layer%layerState = LAYER_STATE_HEAL
          call PreprocessLayer(output_layer)
          if (profiling_enabled) call RecordTime("CalculateAND", get_cpu_time() - t_start)
          return
       end if
       ! If GPU stack overflowed or allocation failed, cleanly fall through to CPU direct-DFS
       if (allocated(output_layer%layer_boxes)) deallocate(output_layer%layer_boxes)
    end if

    !> CPU fallback (multithreaded direct RTree DFS; avoids 512 KB leafboxes stack array & 65536-leaf cap)
    nthreads = max(1, omp_get_max_threads())
    allocate(buffers(nthreads))
    do i=1,nthreads
       allocate(buffers(i)%layer_boxes(K_INIT_BOX_COUNT))
       buffers(i)%n_alloc = K_INIT_BOX_COUNT
       buffers(i)%n_used = 0
    end do
    root_idx = input_layer_B%tree%root_index
    !$omp parallel do private(dfs_stack, sp, curr_idx, child_idx, c_end, i, k, tid, boxA, boxB, tempBox, currNode, childNode)
    over_all_boxes: do i=1,num_boxesA
       tid = omp_get_thread_num()+1
       boxA = input_layer_A%layer_boxes(i)
       if (size(input_layer_B%tree%tree_nodes) == 0) then
          do k = 1, num_boxesB
             boxB = input_layer_B%layer_boxes(k)
             if( boxA%x1 < boxB%x2 .and. boxA%x2 > boxB%x1 .and. &
                 boxA%y1 < boxB%y2 .and. boxA%y2 > boxB%y1 ) then
                tempBox%x1 = max(boxA%x1, boxB%x1)
                tempBox%y1 = max(boxA%y1, boxB%y1)
                tempBox%x2 = min(boxA%x2, boxB%x2)
                tempBox%y2 = min(boxA%y2, boxB%y2)
                call push_box(tempBox, buffers(tid))
             end if
          end do
          cycle
       end if
       childNode = input_layer_B%tree%tree_nodes(root_idx)
       if (boxA%x1 >= childNode%mbr%x2 .or. boxA%x2 <= childNode%mbr%x1 .or. &
           boxA%y1 >= childNode%mbr%y2 .or. boxA%y2 <= childNode%mbr%y1) cycle
       sp = 1
       dfs_stack(1) = root_idx
       do while (sp > 0)
          curr_idx = dfs_stack(sp)
          sp = sp - 1
          currNode = input_layer_B%tree%tree_nodes(curr_idx)
          if (currNode%IsLeaf) then
             c_end = min(currNode%ChildStart + int(currNode%NumChildren, kind=int64) - 1_int64, num_boxesB)
             do k = currNode%ChildStart, c_end
                boxB = input_layer_B%layer_boxes(k)
                if( boxA%x1 < boxB%x2 .and. boxA%x2 > boxB%x1 .and. &
                    boxA%y1 < boxB%y2 .and. boxA%y2 > boxB%y1 ) then
                   tempBox%x1 = max(boxA%x1, boxB%x1)
                   tempBox%y1 = max(boxA%y1, boxB%y1)
                   tempBox%x2 = min(boxA%x2, boxB%x2)
                   tempBox%y2 = min(boxA%y2, boxB%y2)
                   call push_box(tempBox, buffers(tid))
                end if
             end do
          else
             c_end = currNode%ChildStart + int(currNode%NumChildren, kind=int64) - 1_int64
             do child_idx = currNode%ChildStart, c_end
                childNode = input_layer_B%tree%tree_nodes(child_idx)
                if (boxA%x1 < childNode%mbr%x2 .and. boxA%x2 > childNode%mbr%x1 .and. &
                    boxA%y1 < childNode%mbr%y2 .and. boxA%y2 > childNode%mbr%y1) then
                   if (sp < K_DFS_STACK) then
                      sp = sp + 1
                      dfs_stack(sp) = child_idx
                   end if
                end if
             end do
          end if
       end do
    end do over_all_boxes

    num_boxesC = sum([ (buffers(i)%n_used, i=1,nthreads) ])
    if( allocated( output_layer%layer_boxes ) ) then
       deallocate( output_layer%layer_boxes )
    end if
    allocate( output_layer%layer_boxes( num_boxesC ), stat=alloc_status )
    if( alloc_status /= 0 ) then
       error stop "ALLOCATION FAILED in CalculateAND"
    end if
    i = 1
    do tid = 1, nthreads
       if( buffers(tid)%n_used == 0 ) cycle
       if (i + buffers(tid)%n_used - 1 > size(output_layer%layer_boxes)) then
          print *, "FATAL: Output layer capacity exceeded by thread ", tid
          error stop
       end if
       if (buffers(tid)%n_used > size(buffers(tid)%layer_boxes)) then
          print *, "FATAL: Thread ", tid, " over-reported its used size!"
          error stop
       end if
       do j = 1, buffers(tid)%n_used
          output_layer%layer_boxes(i + j - 1) = buffers(tid)%layer_boxes(j)
       end do
       i = i + buffers(tid)%n_used
       call ClearLayer( buffers(tid) )
    end do
    output_layer%n_used = num_boxesC
    output_layer%n_alloc = num_boxesC
    output_layer%device_resident = .false.
    if (allocated(output_layer%tree%tree_nodes)) deallocate(output_layer%tree%tree_nodes)
    if (allocated(output_layer%pnumtable%arr)) deallocate(output_layer%pnumtable%arr)
    output_layer%tree%root_index = 0
    output_layer%layerState = LAYER_STATE_HEAL
    call PreprocessLayer( output_layer )

    if (profiling_enabled) call RecordTime("CalculateAND", get_cpu_time() - t_start)
  end subroutine CalculateAND

  !> Multiple algorithms for A NOT B
  module subroutine CalculateNOT( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout) :: output_layer
    type(XYTracker), allocatable :: trackers(:)
    type(Box) :: bboxA
    type(Layer) :: temp_layer

    call PreprocessLayer( input_layer_A )
    call PreprocessLayer( input_layer_B )

    if (input_layer_A%n_used == 0) then
       call ClearLayer(output_layer)
       return
    end if
    if (input_layer_B%n_used == 0) then
       call CopyLayer(output_layer, input_layer_A)
       return
    end if

    bboxA = mbr_of_array( input_layer_A%layer_boxes, input_layer_A%n_used )
    call generate_trackers( input_layer_B%layer_boxes(1:input_layer_B%n_used), bboxA, trackers )
    call scanline_fracture(trackers, temp_layer%layer_boxes)
    temp_layer%layerState = ior( temp_layer%layerState, LAYER_STATE_HEAL )
    temp_layer%n_used = size( temp_layer%layer_boxes, kind=int64 )
    temp_layer%n_alloc = temp_layer%n_used
    call CalculateAND( input_layer_A, temp_layer, output_layer )
  end subroutine CalculateNOT

end submodule DesignBooleanImplModule
