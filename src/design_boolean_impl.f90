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
  use ProfilerModule, only: get_cpu_time, RecordTime, PhaseBegin, PhaseEnd
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

    if( nTot == 0 ) then
       call ClearLayer( output_layer )
       return
    end if

    !> Gather into a local array first: the output may be the same layer as A or B
    !> (`X = X + Y`), so it must not be freed before the inputs are read. Explicit
    !> loops: the array-section assignments made ifx build a stack temporary of the
    !> whole layer (the same pattern overflowed the 8 MB stack in the edge selectors).
    block
      type(Box), allocatable :: merged(:)
      integer(kind=int64) :: k
      allocate( merged( nTot ), stat=alloc_status )
      if( alloc_status /= 0 ) error stop "ALLOCATION FAILED in CalculateOR"
      do k = 1, nA
         merged(k) = input_layer_A%layer_boxes(k)
      end do
      do k = 1, nB
         merged(nA + k) = input_layer_B%layer_boxes(k)
      end do
      call LayerDeviceRelease( output_layer )
      if( allocated( output_layer%tree%tree_nodes ) ) deallocate( output_layer%tree%tree_nodes )
      if( allocated( output_layer%pnumtable%arr ) ) deallocate( output_layer%pnumtable%arr )
      output_layer%tree%root_index = 0
      output_layer%device_resident = .false.
      call move_alloc( from=merged, to=output_layer%layer_boxes )
    end block
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
       ! Output buffer is replaced (intent(out) allocatable): drop its device copy first, then
       ! make both operands (boxes + tree) resident / revalidate them for this GPU scope.
       call LayerDeviceRelease(output_layer)
       call LayerDeviceEnsure(input_layer_A, input_layer_B)
       if (swap_ab) then
          call ComputeANDPairsGPU(input_layer_A%tree%tree_nodes, numNodesA, &
               input_layer_A%layer_boxes(1:num_boxesA), num_boxesA, input_layer_A%tree%root_index, &
               input_layer_B%layer_boxes(1:num_boxesB), num_boxesB, output_layer%layer_boxes, num_boxesC, gpu_overflow)
       else
          call ComputeANDPairsGPU(input_layer_B%tree%tree_nodes, numNodesB, &
               input_layer_B%layer_boxes(1:num_boxesB), num_boxesB, input_layer_B%tree%root_index, &
               input_layer_A%layer_boxes(1:num_boxesA), num_boxesA, output_layer%layer_boxes, num_boxesC, gpu_overflow)
       end if
       call LayerDeviceDone(input_layer_A, input_layer_B)
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

  !> Direct multithreaded R-Tree box-subtraction algorithm for A NOT B
#if defined(__APPLE__) && defined(__INTEL_COMPILER)
  !> ifort 2022.2 on macOS: the parallel R-tree subtraction below aborts when it
  !> frees its per-thread buffers ("allocatable array is not allocated" or a
  !> segmentation fault, also with unmodified sources), so this platform keeps
  !> the scanline formulation  A NOT B = A AND complement(B within bbox(A)).
  module subroutine CalculateNOT( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout) :: output_layer
    type(XYTracker), allocatable :: trackers(:)
    type(Box) :: bboxA
    type(Layer) :: temp_layer
    real(kind=real64) :: t_start

    if (profiling_enabled) t_start = get_cpu_time()
    call PreprocessLayer( input_layer_A )
    call PreprocessLayer( input_layer_B )

    if (input_layer_A%n_used == 0) then
       call ClearLayer(output_layer)
    else if (input_layer_B%n_used == 0) then
       call CopyLayer(output_layer, input_layer_A)
    else
       bboxA = mbr_of_array( input_layer_A%layer_boxes, input_layer_A%n_used )
       call generate_trackers( input_layer_B%layer_boxes(1:input_layer_B%n_used), bboxA, trackers )
       call scanline_fracture(trackers, temp_layer%layer_boxes)
       call temp_layer%mark( LAYER_STATE_HEAL )
       temp_layer%n_used = size( temp_layer%layer_boxes, kind=int64 )
       temp_layer%n_alloc = temp_layer%n_used
       call CalculateAND( input_layer_A, temp_layer, output_layer )
    end if
    if (profiling_enabled) call RecordTime("CalculateNOT", get_cpu_time() - t_start)
  end subroutine CalculateNOT
#else
  module subroutine CalculateNOT( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout) :: output_layer
    real(kind=real64) :: t_start
    integer(kind=int64) :: num_boxesA, num_boxesB, num_boxesC
    integer(kind=int64), parameter :: K_DFS_STACK = 1024
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096
    integer(kind=int64), parameter :: K_INIT_FRAG_CAP = 64
    integer(kind=int64) :: dfs_stack(K_DFS_STACK)
    integer(kind=int64) :: sp, curr_idx, child_idx, c_end, root_idx
    integer(kind=int64) :: i, j, k, m, n_frags, n_next, cur_buf, nxt_buf
    integer(kind=int64) :: init_cap, offset
    integer(kind=int64), allocatable :: offsets(:)
    type(Layer), allocatable :: buffers(:)
    type(Box), allocatable :: frag_a(:), frag_b(:)
    type(Box) :: boxA, boxB, root_mbr
    type(RTreeNode) :: currNode, childNode
    integer(kind=K_COORDINATE_KIND) :: rem_x1, rem_y1, rem_x2, rem_y2
    integer :: nthreads, tid, alloc_status
    logical :: has_tree, use_gpu_not
    ! GPU path (ComputeNOTGPU): its output plus the A boxes it hands back for CPU recompute
    type(Box), allocatable :: gpu_out(:)
    integer(kind=int64), allocatable :: fallback(:)
    integer(kind=int64) :: gpu_count, n_fallback, n_work, ii, numNodesB
    integer(kind=int32) :: gpu_status
    real(kind=real64) :: tp

    if (profiling_enabled) t_start = get_cpu_time()

    call PreprocessLayer( input_layer_A )
    call PreprocessLayer( input_layer_B )

    num_boxesA = input_layer_A%n_used
    num_boxesB = input_layer_B%n_used

    if (num_boxesA == 0) then
       call ClearLayer(output_layer)
       call output_layer%mark( LAYER_STATE_HEAL )
       if (profiling_enabled) call RecordTime("CalculateNOT", get_cpu_time() - t_start)
       return
    end if
    if (num_boxesB == 0) then
       call CopyLayer(output_layer, input_layer_A)
       if (profiling_enabled) call RecordTime("CalculateNOT", get_cpu_time() - t_start)
       return
    end if

    root_idx = input_layer_B%tree%root_index
    has_tree = allocated(input_layer_B%tree%tree_nodes)
    if (has_tree) then
       has_tree = (size(input_layer_B%tree%tree_nodes) > 0 .and. root_idx > 0)
    end if
    if (has_tree) root_mbr = input_layer_B%tree%tree_nodes(root_idx)%mbr

    !> GPU path: every A box is processed by ComputeNOTGPU; boxes it cannot finish within its
    !> fixed fragment/stack budget come back in fallback(:) and go through the CPU loop below.
    gpu_count = 0_int64
    n_fallback = 0_int64
    use_gpu_not = .false.
    if (has_tree) use_gpu_not = gpu_offload_enabled(num_boxesA)
    if (use_gpu_not) then
       numNodesB = size(input_layer_B%tree%tree_nodes, kind=int64)
       call LayerDeviceRelease(output_layer)
       call LayerDeviceEnsure(input_layer_A, input_layer_B)
       tp = PhaseBegin('not.gpu')
       call ComputeNOTGPU(input_layer_B%tree%tree_nodes, numNodesB, input_layer_B%layer_boxes(1:num_boxesB), &
            num_boxesB, root_idx, input_layer_A%layer_boxes(1:num_boxesA), num_boxesA, &
            gpu_out, gpu_count, fallback, n_fallback, gpu_status)
       call PhaseEnd('not.gpu', tp, num_boxesA)
       call LayerDeviceDone(input_layer_A, input_layer_B)
       if (gpu_status /= 0_int32) then
          use_gpu_not = .false.
          gpu_count = 0_int64
          n_fallback = 0_int64
          if (allocated(gpu_out)) deallocate(gpu_out)
       end if
    end if
    if (use_gpu_not) then
       n_work = n_fallback
       tp = PhaseBegin('not.cpu_fallback')
    else
       n_work = num_boxesA
       tp = PhaseBegin('not.cpu')
    end if

    nthreads = max(1, omp_get_max_threads())
    allocate(buffers(nthreads))
    init_cap = max(K_INIT_BOX_COUNT, n_work / int(nthreads, kind=int64) + 1024_int64)
    do i = 1, nthreads
       allocate(buffers(i)%layer_boxes(init_cap))
       buffers(i)%n_alloc = init_cap
       buffers(i)%n_used = 0
    end do

    !$omp parallel default(shared) &
    !$omp private(dfs_stack, sp, curr_idx, child_idx, c_end, i, ii, k, m, tid, &
    !$omp         boxA, boxB, currNode, childNode, frag_a, frag_b, &
    !$omp         n_frags, n_next, cur_buf, nxt_buf, rem_x1, rem_y1, rem_x2, rem_y2)
    tid = omp_get_thread_num() + 1
    allocate(frag_a(K_INIT_FRAG_CAP), frag_b(K_INIT_FRAG_CAP))

    !$omp do schedule(dynamic, 256)
    over_all_boxes: do ii = 1, n_work
       if (use_gpu_not) then
          i = fallback(ii)
       else
          i = ii
       end if
       boxA = input_layer_A%layer_boxes(i)

       if (.not. has_tree) then
          n_frags = 1
          cur_buf = 1
          frag_a(1) = boxA
          rem_x1 = boxA%x1; rem_y1 = boxA%y1; rem_x2 = boxA%x2; rem_y2 = boxA%y2
          do k = 1, num_boxesB
             boxB = input_layer_B%layer_boxes(k)
             if (rem_x1 < boxB%x2 .and. rem_x2 > boxB%x1 .and. &
                 rem_y1 < boxB%y2 .and. rem_y2 > boxB%y1) then
                if (cur_buf == 1) then
                   call subtract_box_from_frags(boxB, frag_a, n_frags, frag_b, n_next, rem_x1, rem_y1, rem_x2, rem_y2)
                   n_frags = n_next
                   cur_buf = 2
                else
                   call subtract_box_from_frags(boxB, frag_b, n_frags, frag_a, n_next, rem_x1, rem_y1, rem_x2, rem_y2)
                   n_frags = n_next
                   cur_buf = 1
                end if
                if (n_frags == 0) exit
             end if
          end do
          if (cur_buf == 1) then
             do m = 1, n_frags
                call push_box(frag_a(m), buffers(tid))
             end do
          else
             do m = 1, n_frags
                call push_box(frag_b(m), buffers(tid))
             end do
          end if
          cycle over_all_boxes
       end if

       ! Quick accept if boxA is completely outside B's root MBR
       if (boxA%x1 >= root_mbr%x2 .or. boxA%x2 <= root_mbr%x1 .or. &
           boxA%y1 >= root_mbr%y2 .or. boxA%y2 <= root_mbr%y1) then
          call push_box(boxA, buffers(tid))
          cycle over_all_boxes
       end if

       n_frags = 1
       cur_buf = 1
       frag_a(1) = boxA
       rem_x1 = boxA%x1; rem_y1 = boxA%y1; rem_x2 = boxA%x2; rem_y2 = boxA%y2

       sp = 1
       dfs_stack(1) = root_idx
       dfs_loop: do while (sp > 0)
          curr_idx = dfs_stack(sp)
          sp = sp - 1
          currNode = input_layer_B%tree%tree_nodes(curr_idx)
          if (currNode%IsLeaf) then
             c_end = min(currNode%ChildStart + int(currNode%NumChildren, kind=int64) - 1_int64, num_boxesB)
             do k = currNode%ChildStart, c_end
                boxB = input_layer_B%layer_boxes(k)
                if (rem_x1 < boxB%x2 .and. rem_x2 > boxB%x1 .and. &
                    rem_y1 < boxB%y2 .and. rem_y2 > boxB%y1) then
                   if (cur_buf == 1) then
                      call subtract_box_from_frags(boxB, frag_a, n_frags, frag_b, n_next, rem_x1, rem_y1, rem_x2, rem_y2)
                      n_frags = n_next
                      cur_buf = 2
                   else
                      call subtract_box_from_frags(boxB, frag_b, n_frags, frag_a, n_next, rem_x1, rem_y1, rem_x2, rem_y2)
                      n_frags = n_next
                      cur_buf = 1
                   end if
                   if (n_frags == 0) exit dfs_loop
                end if
             end do
          else
             c_end = currNode%ChildStart + int(currNode%NumChildren, kind=int64) - 1_int64
             do child_idx = currNode%ChildStart, c_end
                childNode = input_layer_B%tree%tree_nodes(child_idx)
                if (rem_x1 < childNode%mbr%x2 .and. rem_x2 > childNode%mbr%x1 .and. &
                    rem_y1 < childNode%mbr%y2 .and. rem_y2 > childNode%mbr%y1) then
                   !> never drop a subtree: a silently skipped B box would leave area in A-B
                   if (sp >= K_DFS_STACK) error stop "CalculateNOT: R-tree DFS stack overflow"
                   sp = sp + 1
                   dfs_stack(sp) = child_idx
                end if
             end do
          end if
       end do dfs_loop

       if (n_frags > 0) then
          if (cur_buf == 1) then
             do m = 1, n_frags
                call push_box(frag_a(m), buffers(tid))
             end do
          else
             do m = 1, n_frags
                call push_box(frag_b(m), buffers(tid))
             end do
          end if
       end if
    end do over_all_boxes
    !$omp end do

    deallocate(frag_a, frag_b)
    !$omp end parallel

    num_boxesC = gpu_count + sum([ (buffers(i)%n_used, i=1,nthreads) ])
    if (use_gpu_not) then
       call PhaseEnd('not.cpu_fallback', tp, n_fallback)
    else
       call PhaseEnd('not.cpu', tp, num_boxesA)
    end if
    if (allocated(output_layer%layer_boxes)) deallocate(output_layer%layer_boxes)
    if (allocated(output_layer%tree%tree_nodes)) deallocate(output_layer%tree%tree_nodes)
    if (allocated(output_layer%pnumtable%arr)) deallocate(output_layer%pnumtable%arr)
    output_layer%tree%root_index = 0
    output_layer%device_resident = .false.

    if (num_boxesC == 0) then
       if (allocated(gpu_out)) deallocate(gpu_out)
       do tid = 1, nthreads
          call ClearLayer(buffers(tid))
       end do
       deallocate(buffers)
       output_layer%n_used = 0
       output_layer%n_alloc = 0
       output_layer%layerState = LAYER_STATE_EVERYTHING
       if (profiling_enabled) call RecordTime("CalculateNOT", get_cpu_time() - t_start)
       return
    end if

    if (gpu_count > 0 .and. gpu_count == num_boxesC .and. size(gpu_out, kind=int64) == gpu_count) then
       call move_alloc(from=gpu_out, to=output_layer%layer_boxes)   !> no CPU fallback output: no copy
    else
       allocate(output_layer%layer_boxes(num_boxesC), stat=alloc_status)
       if (alloc_status /= 0) error stop "ALLOCATION FAILED in CalculateNOT"
       if (gpu_count > 0) output_layer%layer_boxes(1:gpu_count) = gpu_out(1:gpu_count)
    end if
    if (allocated(gpu_out)) deallocate(gpu_out)

    allocate(offsets(nthreads))
    offset = gpu_count + 1
    do tid = 1, nthreads
       offsets(tid) = offset
       offset = offset + buffers(tid)%n_used
    end do

    !$omp parallel do default(shared) private(tid, j, offset) schedule(static, 1)
    do tid = 1, nthreads
       if (buffers(tid)%n_used == 0) cycle
       offset = offsets(tid)
       do j = 1, buffers(tid)%n_used
          output_layer%layer_boxes(offset + j - 1) = buffers(tid)%layer_boxes(j)
       end do
       call ClearLayer(buffers(tid))
    end do
    !$omp end parallel do
    deallocate(offsets, buffers)

    output_layer%n_used = num_boxesC
    output_layer%n_alloc = num_boxesC
    output_layer%layerState = LAYER_STATE_HEAL
    call PreprocessLayer( output_layer )

    if (profiling_enabled) call RecordTime("CalculateNOT", get_cpu_time() - t_start)
  contains

    subroutine subtract_box_from_frags(clip, src, n_src, dst, n_dst, rx1, ry1, rx2, ry2)
      type(Box), intent(in) :: clip
      type(Box), intent(in) :: src(:)
      integer(kind=int64), intent(in) :: n_src
      type(Box), allocatable, intent(inout) :: dst(:)
      integer(kind=int64), intent(out) :: n_dst
      integer(kind=K_COORDINATE_KIND), intent(inout) :: rx1, ry1, rx2, ry2
      integer(kind=int64) :: idx, cap, new_cap
      integer(kind=K_COORDINATE_KIND) :: fx1, fy1, fx2, fy2, ix1, iy1, ix2, iy2
      type(Box), allocatable :: tmp(:)

      ! Fast path: single fragment completely covered by clip
      if (n_src == 1) then
         if (clip%x1 <= src(1)%x1 .and. clip%x2 >= src(1)%x2 .and. &
             clip%y1 <= src(1)%y1 .and. clip%y2 >= src(1)%y2) then
            n_dst = 0
            return
         end if
      end if

      n_dst = 0
      cap = size(dst, kind=int64)
      do idx = 1, n_src
         fx1 = src(idx)%x1; fy1 = src(idx)%y1
         fx2 = src(idx)%x2; fy2 = src(idx)%y2

         if (fx1 >= clip%x2 .or. fx2 <= clip%x1 .or. &
             fy1 >= clip%y2 .or. fy2 <= clip%y1) then
            if (n_dst + 1 > cap) then
               new_cap = max(cap * 2_int64, n_dst + 4_int64)
               allocate(tmp(new_cap))
               if (n_dst > 0) tmp(1:n_dst) = dst(1:n_dst)
               call move_alloc(tmp, dst)
               cap = new_cap
            end if
            n_dst = n_dst + 1
            dst(n_dst) = src(idx)
         else
            if (n_dst + 4 > cap) then
               new_cap = max(cap * 2_int64, n_dst + 4_int64)
               allocate(tmp(new_cap))
               if (n_dst > 0) tmp(1:n_dst) = dst(1:n_dst)
               call move_alloc(tmp, dst)
               cap = new_cap
            end if
            ix1 = max(fx1, clip%x1); iy1 = max(fy1, clip%y1)
            ix2 = min(fx2, clip%x2); iy2 = min(fy2, clip%y2)
            if (fx1 < ix1) then
               n_dst = n_dst + 1
               dst(n_dst)%x1 = fx1; dst(n_dst)%y1 = fy1
               dst(n_dst)%x2 = ix1; dst(n_dst)%y2 = fy2
            end if
            if (fx2 > ix2) then
               n_dst = n_dst + 1
               dst(n_dst)%x1 = ix2; dst(n_dst)%y1 = fy1
               dst(n_dst)%x2 = fx2; dst(n_dst)%y2 = fy2
            end if
            if (fy1 < iy1) then
               n_dst = n_dst + 1
               dst(n_dst)%x1 = ix1; dst(n_dst)%y1 = fy1
               dst(n_dst)%x2 = ix2; dst(n_dst)%y2 = iy1
            end if
            if (fy2 > iy2) then
               n_dst = n_dst + 1
               dst(n_dst)%x1 = ix1; dst(n_dst)%y1 = iy2
               dst(n_dst)%x2 = ix2; dst(n_dst)%y2 = fy2
            end if
         end if
      end do

      if (n_dst > 0) then
         rx1 = dst(1)%x1; ry1 = dst(1)%y1
         rx2 = dst(1)%x2; ry2 = dst(1)%y2
         do idx = 2, n_dst
            if (dst(idx)%x1 < rx1) rx1 = dst(idx)%x1
            if (dst(idx)%y1 < ry1) ry1 = dst(idx)%y1
            if (dst(idx)%x2 > rx2) rx2 = dst(idx)%x2
            if (dst(idx)%y2 > ry2) ry2 = dst(idx)%y2
         end do
      end if
    end subroutine subtract_box_from_frags

  end subroutine CalculateNOT
#endif

end submodule DesignBooleanImplModule
