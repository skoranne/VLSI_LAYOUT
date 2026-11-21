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

    ! Edge-selection ops: output edge segments (thin boxes) instead of A boxes.
    if( topo_op >= K_TOPO_INSIDE_EDGE .and. topo_op <= K_TOPO_NOT_COINCIDENT_OUTSIDE_EDGE ) then
       call CalculateTopologicalEdgeSelect( input_layer_A, input_layer_B, output_layer, topo_op )
       return
    end if

    if( numB > 0 .and. allocated(input_layer_B%tree%tree_nodes) ) then
       numNodesB = size(input_layer_B%tree%tree_nodes, kind=int64)
       if( numNodesB > 0 .and. input_layer_B%tree%root_index > 0 .and. gpu_offload_enabled(numA) .and. &
            topo_op >= K_TOPO_INTERACT .and. topo_op <= K_TOPO_NOT_OUTSIDE ) then
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
          case(K_TOPO_OUTSIDE, K_TOPO_NOT_INTERACT, K_TOPO_NOT_INSIDE, K_TOPO_NOT_TOUCH, K_TOPO_NOT_CUT, &
               K_TOPO_DISJOINT, K_TOPO_NOT_MEET, K_TOPO_NOT_OVERLAPS, K_TOPO_NOT_COVERS, &
               K_TOPO_NOT_COVEREDBY, K_TOPO_NOT_CONTAINS, K_TOPO_NOT_EQUAL)
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
          block
            logical :: contains_any,covers_any,coveredby_any,equal_any,proper_overlap_any,point_touch_any
            logical :: a_contains_b,b_contains_a,has_1d
            integer(kind=K_COORDINATE_KIND) :: ix1,iy1,ix2,iy2
            contains_any=.false.; covers_any=.false.; coveredby_any=.false.; equal_any=.false.
            proper_overlap_any=.false.; point_touch_any=.false.

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
                      b_contains_a = (abox%x1 >= ibox%x1 .and. abox%y1 >= ibox%y1 .and. abox%x2 <= ibox%x2 .and. abox%y2 <= ibox%y2)
                      a_contains_b = (ibox%x1 >= abox%x1 .and. ibox%y1 >= abox%y1 .and. ibox%x2 <= abox%x2 .and. ibox%y2 <= abox%y2)
                      if( b_contains_a ) inside_any = .true.
                      if( a_contains_b ) contains_any = .true.
                      if( abox%x1==ibox%x1 .and. abox%x2==ibox%x2 .and. abox%y1==ibox%y1 .and. abox%y2==ibox%y2 ) equal_any=.true.
                      ix1=max(abox%x1,ibox%x1); iy1=max(abox%y1,ibox%y1)
                      ix2=min(abox%x2,ibox%x2); iy2=min(abox%y2,ibox%y2)
                      has_1d = (ix1 < ix2 .and. iy1 == iy2) .or. (ix1 == ix2 .and. iy1 < iy2)
                      if( ix1 < ix2 .and. iy1 < iy2 ) then
                         overlap_any = .true.
                         outside_all = .false.
                         if( .not. b_contains_a .and. .not. a_contains_b ) proper_overlap_any = .true.
                      else if( has_1d ) then
                         touch_any = .true.
                         outside_all = .false.
                         if( b_contains_a ) coveredby_any = .true.
                         if( a_contains_b ) covers_any = .true.
                      else if( ix1==ix2 .and. iy1==iy2 ) then
                         point_touch_any = .true.
                         outside_all = .false.
                      end if
                      if( b_contains_a .and. (abox%x1==ibox%x1 .or. abox%x2==ibox%x2 .or. abox%y1==ibox%y1 .or. abox%y2==ibox%y2) ) coveredby_any=.true.
                      if( a_contains_b .and. (abox%x1==ibox%x1 .or. abox%x2==ibox%x2 .or. abox%y1==ibox%y1 .or. abox%y2==ibox%y2) ) covers_any=.true.
                      select case(topo_op)
                      case(K_TOPO_INTERACT, K_TOPO_NOT_INTERACT)
                         if (overlap_any) then; early_done = .true.; exit; end if
                      case(K_TOPO_INSIDE, K_TOPO_NOT_INSIDE)
                         if (inside_any) then; early_done = .true.; exit; end if
                      case(K_TOPO_OUTSIDE, K_TOPO_NOT_OUTSIDE, K_TOPO_DISJOINT, K_TOPO_NOT_DISJOINT)
                         if (.not. outside_all) then; early_done = .true.; exit; end if
                      case(K_TOPO_TOUCH, K_TOPO_NOT_TOUCH, K_TOPO_MEET, K_TOPO_NOT_MEET)
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
          case(K_TOPO_DISJOINT);      select_it = outside_all
          case(K_TOPO_NOT_DISJOINT);  select_it = .not. outside_all
          case(K_TOPO_MEET);          select_it = (touch_any .or. point_touch_any) .and. .not. overlap_any
          case(K_TOPO_NOT_MEET);      select_it = .not. ((touch_any .or. point_touch_any) .and. .not. overlap_any)
          case(K_TOPO_EQUAL);         select_it = equal_any
          case(K_TOPO_NOT_EQUAL);     select_it = .not. equal_any
          case(K_TOPO_CONTAINS);      select_it = contains_any
          case(K_TOPO_NOT_CONTAINS);  select_it = .not. contains_any
          case(K_TOPO_COVERS);        select_it = covers_any
          case(K_TOPO_NOT_COVERS);    select_it = .not. covers_any
          case(K_TOPO_COVEREDBY);     select_it = coveredby_any
          case(K_TOPO_NOT_COVEREDBY); select_it = .not. coveredby_any
          case(K_TOPO_OVERLAPS);      select_it = proper_overlap_any
          case(K_TOPO_NOT_OVERLAPS);  select_it = .not. proper_overlap_any
          case default;               select_it = .false.
          end select
          if( select_it ) call push_box( abox, buffers(tid) )
          end block
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

  !-----------------------------------------------------------------
  ! Edge-selection topological ops (DE-9IM based for rectilinear layouts).
  ! Output: 1-unit-thick Boxes representing selected A edges.
  ! INSIDE_EDGE: edges of A purely inside layer B (B(A)∩I(B)=1, B(A)∩B(B)=F, B(A)∩E(B)=F).
  ! OUTSIDE_EDGE: edges of A purely outside B.
  ! COINCIDENT_INSIDE_EDGE: A,B share edge segment and interior side/direction is same.
  ! COINCIDENT_OUTSIDE_EDGE: A,B share edge segment and interior side/direction is opposite.
  ! NOT_* are complements over all A edges.
  !-----------------------------------------------------------------
  subroutine CalculateTopologicalEdgeSelect( input_layer_A, input_layer_B, output_layer, topo_op )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout) :: output_layer
    integer(kind=int64), intent(in) :: topo_op
    integer(kind=int64) :: i, k, numA, numB, nthreads, tid, root_idx
    integer(kind=int64), parameter :: K_DFS_STACK = 256
    integer(kind=int64) :: dfs_stack(K_DFS_STACK)
    integer(kind=int64) :: sp, curr_idx, child_idx, c_end
    type(Layer), allocatable :: buffers(:)
    integer :: alloc_status
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096
    type(Box) :: abox, ibox, ebox, currNodeMbr, childNodeMbr
    type(RTreeNode) :: currNode, childNode
    integer(kind=K_COORDINATE_KIND) :: ex1,ey1,ex2,ey2
    integer :: e
    logical :: select_it, purely_inside, purely_outside, coinc_in, coinc_out

    call PreprocessLayer( input_layer_A )
    call PreprocessLayer( input_layer_B )
    numA = input_layer_A%n_used
    numB = input_layer_B%n_used
    if( allocated( output_layer%layer_boxes ) ) deallocate( output_layer%layer_boxes )
    if( allocated( output_layer%tree%tree_nodes ) ) deallocate( output_layer%tree%tree_nodes )
    if( allocated( output_layer%pnumtable%arr ) ) deallocate( output_layer%pnumtable%arr )
    output_layer%tree%root_index = 0

    nthreads = max(1, omp_get_max_threads())
    allocate(buffers(nthreads))
    do i=1,nthreads
       allocate(buffers(i)%layer_boxes(K_INIT_BOX_COUNT))
       buffers(i)%n_alloc = K_INIT_BOX_COUNT
       buffers(i)%n_used = 0
    end do

    if( numA == 0 ) then
       call ClearLayer( output_layer ); output_layer%layerState = ior(output_layer%layerState,LAYER_STATE_HEAL); return
    end if

    if( numB == 0 .or. .not. allocated(input_layer_B%tree%tree_nodes) ) then
       !$omp parallel do private(i,e,tid,abox,ex1,ey1,ex2,ey2,ebox,select_it)
       do i=1,numA
          tid = omp_get_thread_num()+1
          abox = input_layer_A%layer_boxes(i)
          do e=1,4
             call edge_from_box(abox,e,ex1,ey1,ex2,ey2)
             call edge_to_box(ex1,ey1,ex2,ey2,e,ebox)
             purely_inside=.false.; purely_outside=.true.; coinc_in=.false.; coinc_out=.false.
             call edge_select_decide(topo_op,purely_inside,purely_outside,coinc_in,coinc_out,select_it)
             if( select_it ) call push_box(ebox,buffers(tid))
          end do
       end do
    else
       root_idx = input_layer_B%tree%root_index
       !$omp parallel do private(i,e,k,tid,dfs_stack,sp,curr_idx,child_idx,c_end,abox,ibox,ebox, &
       !$omp ex1,ey1,ex2,ey2,select_it,purely_inside,purely_outside,coinc_in,coinc_out,currNode,childNode,currNodeMbr,childNodeMbr)
       do i=1,numA
          tid = omp_get_thread_num()+1
          abox = input_layer_A%layer_boxes(i)
          do e=1,4
             call edge_from_box(abox,e,ex1,ey1,ex2,ey2)
             call edge_to_box(ex1,ey1,ex2,ey2,e,ebox)
             call classify_edge_against_B(ex1,ey1,ex2,ey2,e,abox, &
                  input_layer_B, numB, root_idx, dfs_stack, K_DFS_STACK, &
                  purely_inside,purely_outside,coinc_in,coinc_out)
             call edge_select_decide(topo_op,purely_inside,purely_outside,coinc_in,coinc_out,select_it)
             if( select_it ) call push_box(ebox,buffers(tid))
          end do
       end do
    end if

    output_layer%n_used = sum([ (buffers(i)%n_used, i=1,nthreads) ])
    if( output_layer%n_used == 0 ) then
       do tid=1,nthreads; call ClearLayer(buffers(tid)); end do
       call ClearLayer( output_layer )
       output_layer%layerState = ior(output_layer%layerState,LAYER_STATE_HEAL)
    else
       allocate( output_layer%layer_boxes( output_layer%n_used ), stat=alloc_status )
       if( alloc_status /= 0 ) error stop "ALLOCATION FAILED in CalculateTopologicalEdgeSelect"
       i = 1
       do tid=1,nthreads
          if( buffers(tid)%n_used == 0 ) cycle
          output_layer%layer_boxes(i:i+buffers(tid)%n_used-1) = buffers(tid)%layer_boxes(1:buffers(tid)%n_used)
          i = i + buffers(tid)%n_used
          call ClearLayer(buffers(tid))
       end do
       output_layer%n_alloc = output_layer%n_used
       output_layer%device_resident = .false.
       output_layer%layerState = LAYER_STATE_HEAL
       call PreprocessLayer( output_layer )
    end if
  end subroutine CalculateTopologicalEdgeSelect

  pure subroutine edge_from_box(b,e,ex1,ey1,ex2,ey2)
    type(Box), intent(in) :: b
    integer, intent(in) :: e ! 1=bottom,2=top,3=left,4=right
    integer(kind=K_COORDINATE_KIND), intent(out) :: ex1,ey1,ex2,ey2
    select case(e)
    case(1); ex1=b%x1; ey1=b%y1; ex2=b%x2; ey2=b%y1
    case(2); ex1=b%x1; ey1=b%y2; ex2=b%x2; ey2=b%y2
    case(3); ex1=b%x1; ey1=b%y1; ex2=b%x1; ey2=b%y2
    case default; ex1=b%x2; ey1=b%y1; ex2=b%x2; ey2=b%y2
    end select
  end subroutine edge_from_box

  pure subroutine edge_to_box(ex1,ey1,ex2,ey2,edir,ebox)
    integer(kind=K_COORDINATE_KIND), intent(in) :: ex1,ey1,ex2,ey2
    integer, intent(in) :: edir
    type(Box), intent(out) :: ebox
    ! Represent edge as 1-unit-thick box, shrunk by 1 at both ends to avoid corner overlap.
    if( ey1==ey2 ) then ! horizontal
       ebox%x1=ex1+1; ebox%x2=ex2-1; ebox%y1=ey1; ebox%y2=ey1+1
       if( ebox%x2 <= ebox%x1 ) then; ebox%x1=ex1; ebox%x2=ex2; if(ebox%x2<=ebox%x1) ebox%x2=ebox%x1+1; end if
    else ! vertical
       ebox%x1=ex1; ebox%x2=ex1+1; ebox%y1=ey1+1; ebox%y2=ey2-1
       if( ebox%y2 <= ebox%y1 ) then; ebox%y1=ey1; ebox%y2=ey2; if(ebox%y2<=ebox%y1) ebox%y2=ebox%y1+1; end if
    end if
  end subroutine edge_to_box

  pure subroutine edge_select_decide(topo_op,purely_inside,purely_outside,coinc_in,coinc_out,select_it)
    integer(kind=int64), intent(in) :: topo_op
    logical, intent(in) :: purely_inside,purely_outside,coinc_in,coinc_out
    logical, intent(out) :: select_it
    select case(topo_op)
    case(K_TOPO_INSIDE_EDGE);              select_it = purely_inside
    case(K_TOPO_NOT_INSIDE_EDGE);          select_it = .not. purely_inside
    case(K_TOPO_OUTSIDE_EDGE);             select_it = purely_outside
    case(K_TOPO_NOT_OUTSIDE_EDGE);         select_it = .not. purely_outside
    case(K_TOPO_COINCIDENT_INSIDE_EDGE);   select_it = coinc_in
    case(K_TOPO_NOT_COINCIDENT_INSIDE_EDGE); select_it = .not. coinc_in
    case(K_TOPO_COINCIDENT_OUTSIDE_EDGE);  select_it = coinc_out
    case(K_TOPO_NOT_COINCIDENT_OUTSIDE_EDGE); select_it = .not. coinc_out
    case default; select_it = .false.
    end select
  end subroutine edge_select_decide

  ! Classify a single A edge against layer B.
  ! purely_inside: entire edge segment in B interior (no boundary, no exterior).
  ! purely_outside: entire edge segment in B exterior.
  ! coinc_in: coincident with B boundary, A interior on same side as B interior (same direction).
  ! coinc_out: coincident with B boundary, A interior on opposite side (opposite direction).
  subroutine classify_edge_against_B(ex1,ey1,ex2,ey2,edir,abox,B, numB, root_idx, dfs_stack, K_DFS_STACK, &
       purely_inside,purely_outside,coinc_in,coinc_out)
    integer(kind=K_COORDINATE_KIND), intent(in) :: ex1,ey1,ex2,ey2
    integer, intent(in) :: edir
    type(Box), intent(in) :: abox
    type(Layer), intent(in) :: B
    integer(kind=int64), intent(in) :: numB, root_idx, K_DFS_STACK
    integer(kind=int64), intent(inout) :: dfs_stack(K_DFS_STACK)
    logical, intent(out) :: purely_inside,purely_outside,coinc_in,coinc_out
    integer(kind=int64) :: sp, curr_idx, child_idx, c_end, k
    type(RTreeNode) :: currNode, childNode
    type(Box) :: ibox
    integer(kind=K_COORDINATE_KIND) :: ox1,oy1,ox2,oy2
    logical :: any_inside, any_outside, any_boundary
    logical :: on_b_boundary, same_side
    ! For simplicity: test midpoint and endpoints; for rectilinear healed B this is robust for box-aligned edges.
    integer(kind=K_COORDINATE_KIND) :: mx,my

    purely_inside=.false.; purely_outside=.false.; coinc_in=.false.; coinc_out=.false.
    any_inside=.false.; any_outside=.false.; any_boundary=.false.

    mx = (ex1+ex2)/2; my = (ey1+ey2)/2
    block
      logical :: e1_in,e1_b,e1_out,e2_in,e2_b,e2_out,em_in,em_b,em_out
      logical :: l_in,l_b,l_out,r_in,r_b,r_out
      integer(kind=K_COORDINATE_KIND) :: px1,py1,px2,py2
      e1_in=.false.;e1_b=.false.;e1_out=.false.
      e2_in=.false.;e2_b=.false.;e2_out=.false.
      em_in=.false.;em_b=.false.;em_out=.false.
      call classify_point_against_B(ex1,ey1,B,numB,root_idx,dfs_stack,K_DFS_STACK,e1_in,e1_b,e1_out)
      call classify_point_against_B(ex2,ey2,B,numB,root_idx,dfs_stack,K_DFS_STACK,e2_in,e2_b,e2_out)
      call classify_point_against_B(mx,my,B,numB,root_idx,dfs_stack,K_DFS_STACK,em_in,em_b,em_out)
      ! Refine boundary classification: internal healed boundaries count as inside for layer union.
      if( em_b ) then
         if( ey1==ey2 ) then ! horizontal edge: check perpendicular above/below
            px1=mx; py1=my+1; px2=mx; py2=my-1
         else
            px1=mx+1; py1=my; px2=mx-1; py2=my
         end if
         l_in=.false.;l_b=.false.;l_out=.false.;r_in=.false.;r_b=.false.;r_out=.false.
         call classify_point_against_B(px1,py1,B,numB,root_idx,dfs_stack,K_DFS_STACK,l_in,l_b,l_out)
         call classify_point_against_B(px2,py2,B,numB,root_idx,dfs_stack,K_DFS_STACK,r_in,r_b,r_out)
         if( (l_in .or. l_b) .and. (r_in .or. r_b) ) then
            em_in=.true.; em_b=.false.; em_out=.false.
         end if
      end if
      if( e1_b ) then
         if( ey1==ey2 ) then; px1=ex1; py1=ey1+1; px2=ex1; py2=ey1-1
         else; px1=ex1+1; py1=ey1; px2=ex1-1; py2=ey1; end if
         l_in=.false.;l_b=.false.;l_out=.false.;r_in=.false.;r_b=.false.;r_out=.false.
         call classify_point_against_B(px1,py1,B,numB,root_idx,dfs_stack,K_DFS_STACK,l_in,l_b,l_out)
         call classify_point_against_B(px2,py2,B,numB,root_idx,dfs_stack,K_DFS_STACK,r_in,r_b,r_out)
         if( (l_in .or. l_b) .and. (r_in .or. r_b) ) then; e1_in=.true.; e1_b=.false.; e1_out=.false.; end if
      end if
      if( e2_b ) then
         if( ey1==ey2 ) then; px1=ex2; py1=ey2+1; px2=ex2; py2=ey2-1
         else; px1=ex2+1; py1=ey2; px2=ex2-1; py2=ey2; end if
         l_in=.false.;l_b=.false.;l_out=.false.;r_in=.false.;r_b=.false.;r_out=.false.
         call classify_point_against_B(px1,py1,B,numB,root_idx,dfs_stack,K_DFS_STACK,l_in,l_b,l_out)
         call classify_point_against_B(px2,py2,B,numB,root_idx,dfs_stack,K_DFS_STACK,r_in,r_b,r_out)
         if( (l_in .or. l_b) .and. (r_in .or. r_b) ) then; e2_in=.true.; e2_b=.false.; e2_out=.false.; end if
      end if
      purely_inside  = em_in .and. .not. em_b .and. .not. em_out .and. &
                       e1_in .and. .not. e1_out .and. e2_in .and. .not. e2_out
      purely_outside = em_out .and. .not. em_in .and. .not. em_b .and. &
                       e1_out .and. .not. e1_in .and. e2_out .and. .not. e2_in
    end block

    ! Coincident edge detection: find B edge segment overlapping this edge with shared boundary.
    if( .not. purely_outside ) then
       sp = 1; dfs_stack(1) = root_idx
       do while(sp > 0)
          curr_idx = dfs_stack(sp); sp = sp - 1
          currNode = B%tree%tree_nodes(curr_idx)
          if( currNode%IsLeaf ) then
             c_end = min(currNode%ChildStart + int(currNode%NumChildren,kind=int64)-1_int64, numB)
             do k=currNode%ChildStart, c_end
                ibox = B%layer_boxes(k)
                call edge_coincident_with_box(ex1,ey1,ex2,ey2,edir,abox,ibox,coinc_in,coinc_out)
                if( coinc_in .or. coinc_out ) return
             end do
          else
             c_end = currNode%ChildStart + int(currNode%NumChildren,kind=int64)-1_int64
             do child_idx=currNode%ChildStart, c_end
                childNode = B%tree%tree_nodes(child_idx)
                ! edge MBR vs node MBR
                ox1=min(ex1,ex2); ox2=max(ex1,ex2); oy1=min(ey1,ey2); oy2=max(ey1,ey2)
                if( ox1 <= childNode%mbr%x2 .and. ox2 >= childNode%mbr%x1 .and. &
                    oy1 <= childNode%mbr%y2 .and. oy2 >= childNode%mbr%y1 ) then
                   if( sp < K_DFS_STACK ) then; sp=sp+1; dfs_stack(sp)=child_idx; end if
                end if
             end do
          end if
       end do
    end if
  end subroutine classify_edge_against_B

  ! Classify a point against B: inside/boundary/outside.
  subroutine classify_point_against_B(px,py,B,numB,root_idx,dfs_stack,K_DFS_STACK,is_in,is_b,is_out)
    integer(kind=K_COORDINATE_KIND), intent(in) :: px,py
    type(Layer), intent(in) :: B
    integer(kind=int64), intent(in) :: numB, root_idx, K_DFS_STACK
    integer(kind=int64), intent(inout) :: dfs_stack(K_DFS_STACK)
    logical, intent(out) :: is_in,is_b,is_out
    integer(kind=int64) :: sp,curr_idx,child_idx,c_end,k
    type(RTreeNode) :: currNode, childNode
    type(Box) :: ibox
    logical :: found_in, found_b
    found_in=.false.; found_b=.false.
    if( numB == 0 .or. .not. allocated(B%tree%tree_nodes) ) then
       is_in=.false.; is_b=.false.; is_out=.true.; return
    end if
    sp=1; dfs_stack(1)=root_idx
    do while(sp>0 .and. .not. found_in)
       curr_idx=dfs_stack(sp); sp=sp-1
       currNode=B%tree%tree_nodes(curr_idx)
       if( px < currNode%mbr%x1 .or. px > currNode%mbr%x2 .or. py < currNode%mbr%y1 .or. py > currNode%mbr%y2 ) cycle
       if( currNode%IsLeaf ) then
          c_end=min(currNode%ChildStart+int(currNode%NumChildren,kind=int64)-1_int64,numB)
          do k=currNode%ChildStart,c_end
             ibox=B%layer_boxes(k)
             if( px > ibox%x1 .and. px < ibox%x2 .and. py > ibox%y1 .and. py < ibox%y2 ) then
                found_in=.true.; exit
             else if( px >= ibox%x1 .and. px <= ibox%x2 .and. py >= ibox%y1 .and. py <= ibox%y2 ) then
                found_b=.true. ! on boundary or on edge
             end if
          end do
       else
          c_end=currNode%ChildStart+int(currNode%NumChildren,kind=int64)-1_int64
          do child_idx=currNode%ChildStart,c_end
             childNode=B%tree%tree_nodes(child_idx)
             if( px >= childNode%mbr%x1 .and. px <= childNode%mbr%x2 .and. py >= childNode%mbr%y1 .and. py <= childNode%mbr%y2 ) then
                if( sp < K_DFS_STACK ) then; sp=sp+1; dfs_stack(sp)=child_idx; end if
             end if
          end do
       end if
    end do
    is_in = found_in
    is_b  = (.not. found_in) .and. found_b
    is_out= (.not. found_in) .and. (.not. found_b)
  end subroutine classify_point_against_B

  ! Check if A edge is coincident with any edge of B box ibox.
  ! same direction = A interior on same side as B interior relative to shared edge.
  pure subroutine edge_coincident_with_box(ex1,ey1,ex2,ey2,edir,abox,ibox,coinc_in,coinc_out)
    integer(kind=K_COORDINATE_KIND), intent(in) :: ex1,ey1,ex2,ey2
    integer, intent(in) :: edir
    type(Box), intent(in) :: abox, ibox
    logical, intent(inout) :: coinc_in, coinc_out
    integer(kind=K_COORDINATE_KIND) :: ox1,ox2,oy1,oy2
    logical :: overlap1d
    ! horizontal A edge
    if( ey1==ey2 ) then
       ! compare with ibox bottom edge (y=ibox%y1) and top edge (y=ibox%y2)
       if( ey1 == ibox%y1 ) then
          ox1=max(ex1,ibox%x1); ox2=min(ex2,ibox%x2)
          if( ox1 < ox2 ) then
             ! shared segment at ibox bottom. B interior is above (y>ibox%y1). A interior depends on edir.
             if( edir==1 ) then ! A bottom edge, A interior above
                coinc_in=.true.  ! same direction/side
             else if( edir==2 ) then ! A top edge, A interior below
                coinc_out=.true.
             end if
          end if
       end if
       if( ey1 == ibox%y2 ) then
          ox1=max(ex1,ibox%x1); ox2=min(ex2,ibox%x2)
          if( ox1 < ox2 ) then
             ! B interior below
             if( edir==2 ) then ! A top edge, A interior below
                coinc_in=.true.
             else if( edir==1 ) then
                coinc_out=.true.
             end if
          end if
       end if
    else
       ! vertical A edge
       if( ex1 == ibox%x1 ) then
          oy1=max(ey1,ibox%y1); oy2=min(ey2,ibox%y2)
          if( oy1 < oy2 ) then
             ! B interior to right
             if( edir==3 ) then ! A left edge, A interior to right
                coinc_in=.true.
             else if( edir==4 ) then
                coinc_out=.true.
             end if
          end if
       end if
       if( ex1 == ibox%x2 ) then
          oy1=max(ey1,ibox%y1); oy2=min(ey2,ibox%y2)
          if( oy1 < oy2 ) then
             ! B interior to left
             if( edir==4 ) then
                coinc_in=.true.
             else if( edir==3 ) then
                coinc_out=.true.
             end if
          end if
       end if
    end if
  end subroutine edge_coincident_with_box

end submodule DesignTopoImplModule
