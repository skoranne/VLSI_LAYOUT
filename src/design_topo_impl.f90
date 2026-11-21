! File   : design_topo_impl.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Submodule for box-level topological selection operations

submodule (DesignModule) DesignTopoImplModule
  use iso_fortran_env, only: int8, int32, int64
  use CommonModule
  use GeometryModule
  use RTreeBuilder
  use RTreeBuilderGPU, only: gpu_offload_enabled, ComputeTopoSelectGPU
  use omp_lib
  implicit none

contains

  !=================================================================
  !  Topological select: select boxes from A by relation to B.
  !  Semantics (box-level, consistent with complement/exclusion chains):
  !    INTERACT      : A box has positive-area overlap with any B box
  !    NOT_INTERACT  : no positive-area overlap with any B box
  !    INSIDE        : A box is fully contained in some B box
  !    NOT_INSIDE    : not fully contained in any B box
  !    OUTSIDE       : A box has no contact with B (no overlap, no edge touch)
  !    NOT_OUTSIDE   : A box has contact with B (overlap or edge touch)
  !    TOUCH         : A box touches B on edge but has no positive-area overlap
  !    NOT_TOUCH     : complement of TOUCH
  !    CUT           : A box has positive-area overlap with B but is not inside any B box
  !    NOT_CUT       : complement of CUT
  !=================================================================
  module subroutine CalculateTopologicalSelect( input_layer_A, input_layer_B, output_layer, topo_op )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout) :: output_layer
    integer(kind=int64), intent(in) :: topo_op
    integer(kind=int64) :: i, j, k, numA, numB, nthreads, tid, root_idx
    integer(kind=int64), parameter :: K_DFS_STACK = 256
    integer(kind=int64) :: dfs_stack(K_DFS_STACK)
    integer(kind=int64) :: sp, curr_idx, child_idx, c_end
    logical :: overlap_any, touch_any, inside_any, outside_all, select_it, early_done
    logical :: has_edge_touch
    type(Layer), allocatable :: buffers(:)
    integer :: alloc_status
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096
    type(Box) :: abox, ibox
    type(RTreeNode) :: currNode, childNode
    integer(kind=int8), allocatable :: keep_mask(:)
    integer(kind=int32) :: gpu_overflow
    integer(kind=int64) :: numNodesB, kept_count, widx

    call PreprocessLayer( input_layer_A )
    call PreprocessLayer( input_layer_B )
    numA = input_layer_A%n_used
    numB = input_layer_B%n_used
    if( allocated( output_layer%layer_boxes ) ) deallocate( output_layer%layer_boxes )
    if( allocated( output_layer%tree%tree_nodes ) ) deallocate( output_layer%tree%tree_nodes )
    if( allocated( output_layer%pnumtable%arr ) ) deallocate( output_layer%pnumtable%arr )
    output_layer%tree%root_index = 0
    if( numA == 0 ) then
       call ClearLayer( output_layer )
       output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_HEAL )
       return
    end if

    if( numB > 0 .and. allocated(input_layer_B%tree%tree_nodes) ) then
       numNodesB = size(input_layer_B%tree%tree_nodes, kind=int64)
       if( numNodesB > 0 .and. input_layer_B%tree%root_index > 0 .and. gpu_offload_enabled(numA) ) then
          allocate(keep_mask(numA), stat=alloc_status)
          if( alloc_status == 0 ) then
             gpu_overflow = 0_int32
             call ComputeTopoSelectGPU(input_layer_B%tree%tree_nodes, numNodesB, &
                  input_layer_B%layer_boxes(1:numB), numB, input_layer_B%tree%root_index, &
                  input_layer_A%layer_boxes(1:numA), numA, topo_op, keep_mask, gpu_overflow)
             if( gpu_overflow == 0_int32 ) then
                kept_count = 0_int64
                do i = 1, numA
                   if( keep_mask(i) /= 0_int8 ) kept_count = kept_count + 1_int64
                end do
                if( kept_count == 0_int64 ) then
                   call ClearLayer( output_layer )
                   output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_HEAL )
                else
                   allocate( output_layer%layer_boxes( kept_count ), stat=alloc_status )
                   if( alloc_status /= 0 ) error stop "ALLOCATION FAILED in CalculateTopologicalSelect (GPU)"
                   widx = 0_int64
                   do i = 1, numA
                      if( keep_mask(i) /= 0_int8 ) then
                         widx = widx + 1_int64
                         output_layer%layer_boxes(widx) = input_layer_A%layer_boxes(i)
                      end if
                   end do
                   output_layer%n_used = kept_count
                   output_layer%n_alloc = kept_count
                   output_layer%device_resident = .false.
                   output_layer%layerState = LAYER_STATE_HEAL
                   call PreprocessLayer( output_layer )
                end if
                deallocate(keep_mask)
                return
             end if
             deallocate(keep_mask)
          end if
       end if
    end if

    nthreads = max(1, omp_get_max_threads())
    allocate(buffers(nthreads))
    do i=1,nthreads
       allocate(buffers(i)%layer_boxes(K_INIT_BOX_COUNT))
       buffers(i)%n_alloc = K_INIT_BOX_COUNT
       buffers(i)%n_used = 0
    end do

    if( numB == 0 .or. .not. allocated(input_layer_B%tree%tree_nodes) ) then
       ! B empty: everything in A is OUTSIDE/NOT_INTERACT/NOT_INSIDE/NOT_TOUCH/NOT_CUT
       !$omp parallel do private(i,tid,select_it)
       do i=1,numA
          tid = omp_get_thread_num()+1
          select case(topo_op)
          case(K_TOPO_OUTSIDE, K_TOPO_NOT_INTERACT, K_TOPO_NOT_INSIDE, K_TOPO_NOT_TOUCH, K_TOPO_NOT_CUT)
             select_it = .true.
          case default
             select_it = .false.
          end select
          if( select_it ) call push_box( input_layer_A%layer_boxes(i), buffers(tid) )
       end do
    else
       root_idx = input_layer_B%tree%root_index
       !$omp parallel do private(i,k,tid,dfs_stack,sp,curr_idx,child_idx,c_end,overlap_any, &
       !$omp   touch_any,inside_any,outside_all,select_it,early_done,abox,ibox,has_edge_touch,currNode,childNode)
       do i=1,numA
          tid = omp_get_thread_num()+1
          abox = input_layer_A%layer_boxes(i)
          overlap_any = .false.
          touch_any = .false.
          inside_any = .false.
          outside_all = .true.
          early_done = .false.

          childNode = input_layer_B%tree%tree_nodes(root_idx)
          if (abox%x1 <= childNode%mbr%x2 .and. abox%x2 >= childNode%mbr%x1 .and. &
              abox%y1 <= childNode%mbr%y2 .and. abox%y2 >= childNode%mbr%y1) then
             sp = 1
             dfs_stack(1) = root_idx
             do while (sp > 0 .and. .not. early_done)
                curr_idx = dfs_stack(sp)
                sp = sp - 1
                currNode = input_layer_B%tree%tree_nodes(curr_idx)
                if (currNode%IsLeaf) then
                   c_end = min(currNode%ChildStart + int(currNode%NumChildren, kind=int64) - 1_int64, numB)
                   do k = currNode%ChildStart, c_end
                      ibox = input_layer_B%layer_boxes(k)
                      if (abox%x1 > ibox%x2 .or. abox%x2 < ibox%x1 .or. &
                          abox%y1 > ibox%y2 .or. abox%y2 < ibox%y1) cycle
                      ! containment: B box contains A box
                      if( abox%x1 >= ibox%x1 .and. abox%y1 >= ibox%y1 .and. &
                          abox%x2 <= ibox%x2 .and. abox%y2 <= ibox%y2 ) then
                         inside_any = .true.
                      end if
                      if( box_overlap(abox, ibox) ) then
                         overlap_any = .true.
                         outside_all = .false.
                      else
                         ibox = abox * ibox
                         has_edge_touch = MBRValid(ibox) .and. .not. (ibox%x1==ibox%x2 .and. ibox%y1==ibox%y2)
                         if( has_edge_touch ) then
                            touch_any = .true.
                            outside_all = .false.
                         end if
                      end if
                      ! Early-exit once predicate outcome is fully determined
                      select case(topo_op)
                      case(K_TOPO_INTERACT, K_TOPO_NOT_INTERACT)
                         if (overlap_any) then; early_done = .true.; exit; end if
                      case(K_TOPO_INSIDE, K_TOPO_NOT_INSIDE)
                         if (inside_any) then; early_done = .true.; exit; end if
                      case(K_TOPO_OUTSIDE, K_TOPO_NOT_OUTSIDE)
                         if (.not. outside_all) then; early_done = .true.; exit; end if
                      case(K_TOPO_TOUCH, K_TOPO_NOT_TOUCH)
                         if (overlap_any) then; early_done = .true.; exit; end if
                      case(K_TOPO_CUT, K_TOPO_NOT_CUT)
                         if (inside_any) then; early_done = .true.; exit; end if
                      end select
                   end do
                else
                   c_end = currNode%ChildStart + int(currNode%NumChildren, kind=int64) - 1_int64
                   do child_idx = currNode%ChildStart, c_end
                      childNode = input_layer_B%tree%tree_nodes(child_idx)
                      if (abox%x1 <= childNode%mbr%x2 .and. abox%x2 >= childNode%mbr%x1 .and. &
                          abox%y1 <= childNode%mbr%y2 .and. abox%y2 >= childNode%mbr%y1) then
                         if (sp < K_DFS_STACK) then
                            sp = sp + 1
                            dfs_stack(sp) = child_idx
                         end if
                      end if
                   end do
                end if
             end do
          end if
          select case(topo_op)
          case(K_TOPO_INTERACT);      select_it = overlap_any
          case(K_TOPO_NOT_INTERACT);  select_it = .not. overlap_any
          case(K_TOPO_INSIDE);        select_it = inside_any
          case(K_TOPO_NOT_INSIDE);    select_it = .not. inside_any
          case(K_TOPO_OUTSIDE);       select_it = outside_all
          case(K_TOPO_NOT_OUTSIDE);   select_it = .not. outside_all
          case(K_TOPO_TOUCH);         select_it = touch_any .and. .not. overlap_any
          case(K_TOPO_NOT_TOUCH);     select_it = .not. (touch_any .and. .not. overlap_any)
          case(K_TOPO_CUT);           select_it = overlap_any .and. .not. inside_any
          case(K_TOPO_NOT_CUT);       select_it = .not. (overlap_any .and. .not. inside_any)
          case default;               select_it = .false.
          end select
          if( select_it ) call push_box( abox, buffers(tid) )
       end do
    end if

    output_layer%n_used = sum([ (buffers(i)%n_used, i=1,nthreads) ])
    if( output_layer%n_used == 0 ) then
       do tid = 1, nthreads
          call ClearLayer( buffers(tid) )
       end do
       call ClearLayer( output_layer )
       output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_HEAL )
    else
       allocate( output_layer%layer_boxes( output_layer%n_used ), stat=alloc_status )
       if( alloc_status /= 0 ) error stop "ALLOCATION FAILED in CalculateTopologicalSelect"
       i = 1
       do tid = 1, nthreads
          if( buffers(tid)%n_used == 0 ) cycle
          do j = 1, buffers(tid)%n_used
             output_layer%layer_boxes(i + j - 1) = buffers(tid)%layer_boxes(j)
          end do
          i = i + buffers(tid)%n_used
          call ClearLayer( buffers(tid) )
       end do
       output_layer%n_alloc = output_layer%n_used
       output_layer%device_resident = .false.
       output_layer%layerState = LAYER_STATE_HEAL
       call PreprocessLayer( output_layer )
    end if
  end subroutine CalculateTopologicalSelect

end submodule DesignTopoImplModule
