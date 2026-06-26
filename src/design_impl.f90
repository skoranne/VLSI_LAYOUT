! File   : design_impl.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Use submodule to move large blocks of code out of "interfaces"

!=====================================================================
!  design_impl.f90   –  submodule containing the heavy code
!=====================================================================
submodule (DesignModule) DesignImplModule
  use iso_fortran_env, only: int64, real64
  use CommonModule
  use GeometryModule
  use MortonSortOMT
  use RTreeBuilderGPU
  use RTreeBuilder
  use GPUMergeModule
  use PNumMergeModule
  use KLDataModule
  implicit none

contains

  module subroutine FinalizeLayer( output_layer )
    type(Layer),intent(inout) :: output_layer
    type(Box), allocatable    :: tempBoxes(:)
    integer(kind=int64)       :: N, i
    N = output_layer%n_used
    if( N == 0 ) then
       call ClearLayer( output_layer )
       return
    end if
    allocate( tempBoxes( N ) )
    do i = 1, N
       tempBoxes(i) = output_layer%layer_boxes(i)
    end do
    call move_alloc( from=tempBoxes, to=output_layer%layer_boxes)
    output_layer%n_alloc = N
    output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_FINAL )
  end subroutine FinalizeLayer


  !=================================================================
  !  AssignFromBox implements the layer = Box
  !  
  !=================================================================

  module subroutine AssignFromBox( output_layer, input_box )
    type(Layer),intent(inout) :: output_layer
    type(Box), intent(in) :: input_box
    if(.not. allocated( output_layer%layer_boxes ) ) allocate( output_layer%layer_boxes(1) )
    if(.not. input_box%is_valid() ) error stop "ERROR: input box not valid"
    output_layer%layer_boxes(1) = input_box
    output_layer%n_used = 1
    !> as a rule we dont want these layers to be written to disk
    output_layer%layerState = ior( LAYER_STATE_SORT, LAYER_STATE_HEAL)
    call PreprocessLayer( output_layer )
  end subroutine AssignFromBox

  !=================================================================
  !  FilterLayer uses RTree to quickly O(log N + k) select 
  !  candidates from input_layer which are interacting with input_box
  !=================================================================

  module subroutine FilterLayer( input_layer, input_box, output_layer )
    type(Layer), intent(in)    :: input_layer
    type(Layer), intent(inout) :: output_layer
    type(Box),   intent(in)    :: input_box
    type(Box)                  :: tempBox
    integer(kind=int64) :: K_INIT_BOX_COUNT = 1024
    integer(kind=int64) :: leafboxes(K_MAX_SEARCH_LEAVES) ! better choose a large number
    integer(kind=int64) :: number_leaves, num_boxes
    integer(kind=int64) :: j, k, interaction_count, N, total_nodes
    num_boxes = input_layer%n_used
    if( num_boxes == 0 ) then
       call ClearLayer( output_layer )
       return
    end if
    allocate(  output_layer%layer_boxes( K_INIT_BOX_COUNT ) )
    output_layer%n_alloc = K_INIT_BOX_COUNT
    if( NeedsRTree( input_layer ) ) error stop "ERROR: Construct RTRee before this step"
    number_leaves = 0
    leafboxes = 0
    call SearchTree( input_layer%tree%tree_nodes, input_layer%tree%root_index, &
         input_box, leafboxes, number_leaves )
    !write(*,'(A,4I8,A,I8)') 'Input Box = ', input_box, ' NL ', number_leaves
    !write(*,'(A,4I8)') 'Input Tree ', input_layer%tree%tree_nodes( input_layer%tree%root_index )%mbr
    if( number_leaves == 0 ) error stop
    if( number_leaves > 0 ) then
       outer: do j=1,number_leaves
          over_leaves: do k=leafboxes(j),min(leafboxes(j)+K_LEAF_CAPACITY-1, num_boxes)
             if( box_interact( input_box, input_layer%layer_boxes(k)) ) then
                tempBox = input_box * input_layer%layer_boxes(k)
                if( .not. tempBox%is_valid() ) cycle
                call push_box( tempBox, output_layer )
             end if
          end do over_leaves
       end do outer
    end if
    !> we can choose not to perform some operations later
    call FinalizeLayer( output_layer )
    output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_HEAL ) !> otherwise this might merge
    call PreprocessLayer( output_layer )
  end subroutine FilterLayer

  !!=================================================================    
  !    Creates GRIDS which are not continuous, for example, one&
  !     & grid will end at (10,10) and the next one will begin&
  !     & at (11,11). Ideally the function should accept an&
  !     & argument called GridOverlap which stitches the grids&
  !     & up and if GridOverlap is 10 then the left and right&
  !     & GRIDs will have an OVERLAP of 10 units. Similarly&
  !     & for TOP and BOTTOM. Devise such a method and&
  !     & regenerate the function. If GridOverlap is 0 then&
  !     & also the GRIDS should touch each other. If one GRID&
  !     & finishes at (10,10) in its upper right top corner,&
  !     & the next GRID bottom left should be (10,10), NOT (11&
  !     &,11).
  !!=================================================================
  module subroutine CreateGrid( input_layer, output_layer, rows, cols, GridOverlap )
    type(Layer), intent(in)    :: input_layer
    type(Layer), intent(inout) :: output_layer
    integer,     intent(in)    :: rows, cols
    integer,     intent(in)    :: GridOverlap

    type(Box)                  :: input_box
    integer(kind=K_COORDINATE_KIND) :: x_lo, x_hi, y_lo, y_hi
    integer(kind=K_COORDINATE_KIND) :: full_w, full_h
    integer(kind=K_COORDINATE_KIND) :: col_width, row_height
    integer(kind=K_COORDINATE_KIND) :: extra_w, extra_h
    integer(kind=int64)             :: i, j, idx, new_alloc
    type(Box)                       :: subbox

    ! Overlap padding variables
    integer(kind=K_COORDINATE_KIND) :: overlap_val, pad_left, pad_right, pad_bottom, pad_top

    input_box = mbr_of_array( input_layer%layer_boxes, input_layer%n_used )
    if( allocated( output_layer%layer_boxes ) ) then
       deallocate( output_layer%layer_boxes )
       output_layer%n_used = 0
       output_layer%n_alloc = 0
    end if

    !-----------------------------------------------------------------
    !  1. Sanity checks
    !-----------------------------------------------------------------
    if (rows <= 2 .or. cols <= 2) then
       error stop "CreateGrid: rows and cols must be positive"
    end if

    if (.not. input_box%is_valid()) then
       error stop "CreateGrid: input_box is not a valid rectangle"
    end if

    !-----------------------------------------------------------------
    !  2. Normalise the limits
    !-----------------------------------------------------------------
    x_lo = min(input_box%x1, input_box%x2)
    x_hi = max(input_box%x1, input_box%x2)
    y_lo = min(input_box%y1, input_box%y2)
    y_hi = max(input_box%y1, input_box%y2)

    !-----------------------------------------------------------------
    !  3. Compute continuous width/height (Removed the + 1)
    !-----------------------------------------------------------------
    full_w = x_hi - x_lo
    full_h = y_hi - y_lo

    !-----------------------------------------------------------------
    !  4. Base size + remainder distribution
    !-----------------------------------------------------------------
    col_width = full_w / cols
    extra_w   = mod(full_w, int(cols, kind=K_COORDINATE_KIND))

    row_height = full_h / rows
    extra_h    = mod(full_h, int(rows, kind=K_COORDINATE_KIND))

    !-----------------------------------------------------------------
    !  5. Calculate Padding from GridOverlap
    !-----------------------------------------------------------------
    overlap_val = int(GridOverlap, kind=K_COORDINATE_KIND)
    pad_left    = overlap_val / 2
    pad_right   = overlap_val - pad_left
    pad_bottom  = overlap_val / 2
    pad_top     = overlap_val - pad_bottom

    !-----------------------------------------------------------------
    !  6. Grow the destination array if necessary
    !-----------------------------------------------------------------
    new_alloc = int(rows, kind=int64) * int(cols, kind=int64)
    allocate( output_layer%layer_boxes( new_alloc ) )
    output_layer%n_alloc = new_alloc
    output_layer%n_used  = new_alloc

    !-----------------------------------------------------------------
    !  7. Fill the sub‑boxes
    !-----------------------------------------------------------------
    idx = 1
    do i = 1, rows
       block
         integer(kind=K_COORDINATE_KIND) :: core_y0, core_y1

         ! Calculate exact touching boundaries (0 overlap)
         core_y0 = y_lo + (i-1)*row_height + min(int(i-1, kind=K_COORDINATE_KIND), extra_h)
         core_y1 = y_lo + i*row_height + min(int(i, kind=K_COORDINATE_KIND), extra_h)

         do j = 1, cols
            block
              integer(kind=K_COORDINATE_KIND) :: core_x0, core_x1

              ! Calculate exact touching boundaries (0 overlap)
              core_x0 = x_lo + (j-1)*col_width + min(int(j-1, kind=K_COORDINATE_KIND), extra_w)
              core_x1 = x_lo + j*col_width + min(int(j, kind=K_COORDINATE_KIND), extra_w)

              ! Apply padding outward
              subbox%x1 = core_x0 - pad_left
              subbox%x2 = core_x1 + pad_right
              subbox%y1 = core_y0 - pad_bottom
              subbox%y2 = core_y1 + pad_top

              ! Clamp to the global layer domain to prevent spilling out of bounds
              subbox%x1 = max(x_lo, subbox%x1)
              subbox%x2 = min(x_hi, subbox%x2)
              subbox%y1 = max(y_lo, subbox%y1)
              subbox%y2 = min(y_hi, subbox%y2)

              output_layer%layer_boxes(idx) = subbox
              idx = idx + 1
            end block
         end do
       end block
    end do
    output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_HEAL ) !> otherwise this might merge
    write(*,*) 'GRID output size = ', output_layer%n_used      
    call PreprocessLayer( output_layer )
    write(*,*) 'GRID output size = ', output_layer%n_used            
  end subroutine CreateGrid

  module subroutine CreateEXTENT( input_layer, output_layer )
    type(Layer),      intent(in)    :: input_layer
    type(Layer),      intent(inout) :: output_layer
    if( allocated( output_layer%layer_boxes ) ) then
       if( size( output_layer%layer_boxes ) /= 0 ) error stop "|LB| /= 0"
       if( output_layer%n_used /= 0  ) error stop "|NU| /= 0"
       if( output_layer%n_alloc /= 0 ) error stop "|NA| /= 0"
       deallocate( output_layer%layer_boxes ) !> we checked n_alloc and size are both 0
    end if
    allocate( output_layer%layer_boxes(1) )
    output_layer%layer_boxes(1) = mbr_of_array( input_layer%layer_boxes, input_layer%n_used )
    if( .not. output_layer%layer_boxes(1)%is_valid() ) error stop
    output_layer%n_used = 1
    output_layer%layerState = LAYER_STATE_HEAL
    output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_SORT )
    call PreprocessLayer( output_layer )
  end subroutine CreateEXTENT

#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
  module function CalculateOverlapCount( input_layer_A ) result( interaction_count )
    type(Layer), intent(inout) :: input_layer_A
    integer(kind=int64) :: interaction_count, N, total_nodes
    type(RTreeNode), allocatable:: TreeNodes(:)
    integer(kind=int64) :: RootIndex
    interaction_count = 0
    if( input_layer_A%n_used == 0 ) then
       return
    end if
    if( .not. NeedsHealing( input_layer_A ) ) then
       write(*,*) 'ERROR: CalculateOverlapCount should be called prior to LAYER HEALING'
       return
    end if
    N = input_layer_A%n_used
    total_nodes = CalculateTotalNodes( N, K_LEAF_CAPACITY ) !> for GPU we might change
    call SortBoxesDirect( input_layer_A%layer_boxes, N )
    allocate( TreeNodes( total_nodes ) )
    call BuildRTree( input_layer_A%layer_boxes, K_LEAF_CAPACITY, TreeNodes, RootIndex)
    call ComputeInteractionsGPU( TreeNodes, total_nodes, input_layer_A%layer_boxes, N, RootIndex, interaction_count)
  end function CalculateOverlapCount
#else
  module function CalculateOverlapCount( input_layer_A ) result( interaction_count )
    type(Layer), intent(inout) :: input_layer_A
    integer(kind=int64) :: interaction_count, N, num_squares
    N = input_layer_A%n_used
    if( NeedsSorting( input_layer_A ) ) then
       if( num_squares*1.0_real64 / (N*1.0_real64) > K_SQUARE_DOMINATION_THRESHOLD ) then
          write(*,*) 'Layer is SQUARE dominated, ', num_squares, ' / ', N
          call MortonSort( input_layer_A%layer_boxes )
       else
          call SortBoxesDirect( input_layer_A%layer_boxes, N )
          !call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
       end if
       input_layer_A%layerState = ior( input_layer_A%layerState, LAYER_STATE_SORT )
       call BuildRTree( input_layer_A%layer_boxes, K_LEAF_CAPACITY, input_layer_A%tree%tree_nodes, input_layer_A%tree%root_index)
       input_layer_A%layerState = ior( input_layer_A%layerState, LAYER_STATE_RTREE )
    end if
    call ComputeInteractionsCPU( input_layer_A%tree%tree_nodes, int(size(input_layer_A%tree%tree_nodes),kind=int64),&
         input_layer_A%layer_boxes, N,&
         input_layer_A%tree%root_index, interaction_count )
  end function CalculateOverlapCount
#endif

#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
  module function CalculateSingletonCount( input_layer, is_singleton ) result( interaction_count )
    type(Layer), intent(inout) :: input_layer
    integer(kind=int64) :: interaction_count, N, total_nodes
    type(RTreeNode), allocatable:: TreeNodes(:)
    integer(kind=int64) :: RootIndex
    logical, allocatable, intent(out) :: is_singleton(:)
    interaction_count = 0
    if( input_layer%n_used == 0 ) then
       return
    end if
    N = input_layer%n_used
    total_nodes = CalculateTotalNodes( N, K_LEAF_CAPACITY ) !> for GPU we might change
    call SortBoxesDirect( input_layer%layer_boxes, N )
    allocate( TreeNodes( total_nodes ) )
    call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, TreeNodes, RootIndex)
    call FindSingletonsGPU( N, input_layer%layer_boxes, total_nodes, TreeNodes, RootIndex, is_singleton, interaction_count)
  end function CalculateSingletonCount
#else
  module function CalculateSingletonCount( input_layer, is_singleton ) result( interaction_count )
    type(Layer), intent(inout) :: input_layer
    logical, allocatable, intent(out) :: is_singleton(:)
    integer(kind=int64) :: interaction_count, N, num_squares
    interaction_count = 0
    N = input_layer%n_used
    if( NeedsSorting( input_layer ) ) then
       if( num_squares*1.0_real64 / (N*1.0_real64) > K_SQUARE_DOMINATION_THRESHOLD ) then
          write(*,*) 'Layer is SQUARE dominated, ', num_squares, ' / ', N
          call MortonSort( input_layer%layer_boxes )
       else
          call SortBoxesDirect( input_layer%layer_boxes, N )
          !call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
       end if
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
       call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
    end if

    call FindSingletonsCPU( input_layer%layer_boxes, input_layer%tree%tree_nodes, input_layer%tree%root_index,&
         is_singleton, interaction_count)

  end function CalculateSingletonCount
#endif




  module subroutine CalculateSingleLayerAND( input_layer_A, output_layer )
    type(Layer), intent(inout) :: input_layer_A
    type(Layer), intent(inout) :: output_layer
    integer(kind=int64) :: num_boxesA, num_boxesC
    integer(kind=int64) :: leafboxes(K_MAX_SEARCH_LEAVES) ! better choose a large number
    integer(kind=int64) :: number_leaves
    integer(kind=int64) :: i, j, k, interaction_count, N, total_nodes
    type(RTreeNode), allocatable:: TreeNodes(:)
    integer(kind=int64) :: RootIndex
    type(Layer), allocatable :: buffers(:)
    type(Box) :: tempBox, bbox
    integer :: nthreads, tid, alloc_status
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096
    !> we cannot call Preprocess and neither should it be called prior
    if( input_layer_A%n_used == 0 ) then
       input_layer_A%layerState = 31 !> we set everything
       return
    end if

    if( .not. NeedsHealing( input_layer_A ) ) then
       write(*,*) 'ERROR: SingleLayerAND should be called prior to LAYER HEALING'
       return
    end if

    N = input_layer_A%n_used
    total_nodes = CalculateTotalNodes( N, K_LEAF_CAPACITY ) !> for GPU we might change
    bbox = mbr_of_array( input_layer_A%layer_boxes, N )
    write(*,*) 'Loaded : ', N, ' BBOX = ', bbox, ' |T| = ', total_nodes
    call SortBoxesDirect( input_layer_A%layer_boxes, N )
    allocate( TreeNodes( total_nodes ) )
    call BuildRTree( input_layer_A%layer_boxes, K_LEAF_CAPACITY, TreeNodes, RootIndex)
    write(*,*) 'Tree constructed: ', RootIndex, ' |RT| = ', size(TreeNodes)
    call ComputeInteractionsGPU( TreeNodes, total_nodes, input_layer_A%layer_boxes, N, RootIndex, interaction_count)
    if( interaction_count == 0 ) then
       output_layer%n_used = 0
       output_layer%layerState = LAYER_STATE_EVERYTHING
       write(*,*) 'In SingleLayerAND no overlap detected: early exit'
       return
    end if
    if( NeedsRTree( input_layer_A ) ) then
       if( allocated( input_layer_A%tree%tree_nodes ) ) deallocate( input_layer_A%tree%tree_nodes )
       allocate( input_layer_A%tree%tree_nodes( CalculateTotalNodes( input_layer_A%n_used, K_LEAF_CAPACITY ) ) )
       call BuildRTree( input_layer_A%layer_boxes, K_LEAF_CAPACITY, input_layer_A%tree%tree_nodes, input_layer_A%tree%root_index)
       input_layer_A%layerState = ior( input_layer_A%layerState, LAYER_STATE_RTREE )
    end if

    nthreads = omp_get_max_threads()
    allocate(buffers(nthreads))
    do i=1,nthreads
       !> we may still need some setup
       allocate(buffers(i)%layer_boxes(K_INIT_BOX_COUNT))
       buffers(i)%n_alloc = K_INIT_BOX_COUNT
    end do
    num_boxesA = input_layer_A%n_used
    !write(*,*) 'DBG: ', num_boxesA, ' ', num_boxesB, ' RTB ', size(input_layer_B%tree%tree_nodes)
    !> we may have to do schedule dynamic:     !$omp do schedule(dynamic)
    !$omp parallel do private(leafboxes, number_leaves, i, j, k, tid, tempBox)
    over_all_boxes: do i=1,num_boxesA
       number_leaves = 0
       leafboxes = 0
       tid = omp_get_thread_num()+1
       call SearchTree( input_layer_A%tree%tree_nodes, input_layer_A%tree%root_index, &
            input_layer_A%layer_boxes(i), leafboxes, number_leaves )
       !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
       if( number_leaves > 0 ) then
          !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
          outer: do j=1,number_leaves
             over_leaves: do k=leafboxes(j),min(leafboxes(j)+K_LEAF_CAPACITY-1, num_boxesA)
                if( k <= i ) cycle
                if( box_interact( input_layer_A%layer_boxes(i), input_layer_A%layer_boxes(k)) ) then
                   tempBox = input_layer_A%layer_boxes(i) * input_layer_A%layer_boxes(k)
                   !$komp critical (console_io)
                   !write(*,*) 'Index ',i, ' ', input_layer_A%layer_boxes(i), ' interacts with ',&
                   !     k, ' ', input_layer_B%layer_boxes(k), &
                   !     ' * ', tempBox, box_area( tempBox )
                   !$komp end critical (console_io)
                   if( box_area( tempBox ) > 0.0 ) then
                      call push_box(tempBox, buffers(tid)) ! Reallocates if capacity exceeded, and here i is very important
                   end if
                end if
             end do over_leaves
          end do outer
       end if
    end do over_all_boxes

    !print *, "DEBUG: Is this loop in a parallel region? ", omp_in_parallel()
    ! The global Union-Find array is updated strictly sequentially
    num_boxesC = sum([ (buffers(i)%n_used, i=1,nthreads) ])
    write(*,*) 'OUTPUT_COUNT =', num_boxesC
    if( allocated( output_layer%layer_boxes ) ) then
       deallocate( output_layer%layer_boxes ) !> since we are now doing inout,
    end if
    allocate( output_layer%layer_boxes( num_boxesC ), stat=alloc_status )
    if( alloc_status /= 0 ) then
       error stop "ALLOCATION FAILED: "
    end if
    i = 1
    do tid = 1, nthreads
       if( buffers(tid)%n_used == 0 ) cycle
       ! 2. Check if we are writing out of bounds on the destination
       if (i + buffers(tid)%n_used - 1 > size(output_layer%layer_boxes)) then
          print *, "FATAL: Output layer capacity exceeded by thread ", tid
          print *, "Current index: ", i, " Trying to add: ", buffers(tid)%n_used
          print *, "Max capacity: ", size(output_layer%layer_boxes)
          error stop
       end if

       ! 3. Check if we are reading out of bounds on the source
       if (buffers(tid)%n_used > size(buffers(tid)%layer_boxes)) then
          print *, "FATAL: Thread ", tid, " over-reported its used size!"
          print *, "Claimed used: ", buffers(tid)%n_used
          print *, "Actual buffer size: ", size(buffers(tid)%layer_boxes)
          error stop
       end if
       ! Explicit loop strictly prevents temporary stack allocations
       do j = 1, buffers(tid)%n_used
          output_layer%layer_boxes(i + j - 1) = buffers(tid)%layer_boxes(j)
       end do
       i = i + buffers(tid)%n_used
    end do
    output_layer%n_used = num_boxesC
    !> we can use the optimization that by construction the output is HEALED
    output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_HEAL )
  end subroutine CalculateSingleLayerAND


  !> Removes exact adjacent duplicates from a sorted array of Boxes in-place.
  subroutine RemoveIdenticalBoxes(boxes)
    ! allocatable dummy arguments allow us to modify the array bounds (F2003)
    type(Box), allocatable, intent(inout) :: boxes(:)
    integer(kind=int64) :: n, read_idx, write_idx

    if (.not. allocated(boxes)) return
    n = size(boxes)
    if (n <= 1) return

    write_idx = 1

    ! Two-pointer traversal
    do read_idx = 2, n
       ! Using the elemental function keeps the logic highly readable
       if (.not. is_identical(boxes(read_idx), boxes(write_idx))) then
          write_idx = write_idx + 1

          if (write_idx /= read_idx) then
             boxes(write_idx) = boxes(read_idx)
          end if
       end if
    end do

    ! Truncate the array if duplicates were removed
    if (write_idx < n) then
       ! block construct limits the scope of temporary variables (F2008)
       block
         type(Box), allocatable :: temp_boxes(:)
         allocate(temp_boxes(write_idx))

         ! Array slicing handles the subset copy implicitly
         temp_boxes = boxes(1:write_idx)

         ! move_alloc transfers the memory pointer without a deep copy (F2003)
         call move_alloc(from=temp_boxes, to=boxes)
       end block
    end if

  end subroutine RemoveIdenticalBoxes

  !> Helper function to check if two boxes are exactly identical.
  !> 'elemental' implies pure, scalar execution, allowing compiler optimizations.
  elemental function is_identical(b1, b2) result(match)
    type(Box), intent(in) :: b1, b2
    logical :: match

    match = (b1%x1 == b2%x1) .and. (b1%y1 == b2%y1) .and. &
         (b1%x2 == b2%x2) .and. (b1%y2 == b2%y2)
  end function is_identical
  ! ==============================================================================

  module subroutine RemoveIdentical(input_layer)
    type(Layer), intent(inout) :: input_layer
    if( input_layer%n_used < 2 ) return
    call RemoveIdenticalBoxes( input_layer%layer_boxes )
    input_layer%n_used = size( input_layer%layer_boxes )
  end subroutine RemoveIdentical

  module subroutine CalculateGROWLayer( input_layer, output_layer, ivar )
    type(Layer),      intent(in)    :: input_layer
    type(Layer),      intent(inout) :: output_layer
    integer(kind=K_COORDINATE_KIND), intent(in)   :: ivar(4)
    call ClearLayer( output_layer )
    output_layer%layerState = LAYER_STATE_NONE
    output_layer%layer_boxes = input_layer%layer_boxes
    output_layer%n_used = input_layer%n_used
    write(*,*) 'GROW : ', ivar
    call box_grow_directional(output_layer%layer_boxes, ivar(1), ivar(2), ivar(3), ivar(4) )
    write(*,*) 'OUTPUT_STATE = ', output_layer%layerState
    call PreprocessLayer( output_layer )
  end subroutine CalculateGROWLayer

  module subroutine CalculateSHRINKLayer( input_layer_A, shrink_value, output_layer )
    type(Layer), intent(inout) :: input_layer_A
    type(Layer), intent(inout)   :: output_layer
    integer(kind=K_COORDINATE_KIND), intent(in) :: shrink_value
    integer(kind=int64) :: num_boxesA, output_box_count
    type(XYTracker), allocatable :: trackers(:)
    type(Box) :: bboxA, bboxB
    type(Layer) :: temp_layer
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096
    call PreprocessLayer( input_layer_A )
    !> Step 1
    bboxA = mbr_of_array( input_layer_A%layer_boxes, input_layer_A%n_used )
    call box_grow( bboxA, shrink_value, shrink_value )
    call generate_trackers( input_layer_A%layer_boxes, bboxA, trackers ) !> this does CW ordering of inner contours
    call scanline_fracture(trackers, temp_layer%layer_boxes)
    temp_layer%n_used = size( temp_layer%layer_boxes)
    !> now grow each box
    call box_grow(temp_layer%layer_boxes, shrink_value, shrink_value )
    call PreprocessLayer( temp_layer )
    call CalculateNOT( input_layer_A, temp_layer, output_layer )
  end subroutine CalculateSHRINKLayer

  !> ConvertLayerToBox
  !>
  module subroutine ConvertLayerToBox( input_layer, output_layer, control_parameter )
    type(Layer), intent(inout) :: input_layer
    type(Layer), intent(inout) :: output_layer
    integer(kind=int64), intent(in) :: control_parameter
  end subroutine ConvertLayerToBox

  !> Trying FRAME based OpenMP optimization for A - B (shown as A ~ B)
  module subroutine CalculateFrameNOT( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout)   :: output_layer
    type(TrackerCell), allocatable :: trackersGrid(:,:)
    integer(kind=int64) :: total_count, i
    integer :: nthreads, tid
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096
    integer(kind=int64), parameter :: K_GRID = 64
    integer :: rows, cols
    type(Layer) :: gridA
    type(Layer) :: grids(K_GRID,K_GRID)
    type(Layer) :: outputGrid(K_GRID, K_GRID)
    type(Layer) :: templayerGrid(K_GRID, K_GRID)
    type(Layer) :: AinGrid(K_GRID, K_GRID)
    type(Layer) :: BinGrid(K_GRID, K_GRID)       !> I dont know if it makes sense to create so many temp layers
    type(Box)   :: bboxA
    integer(kind=int64) :: start_tick(K_GRID,K_GRID), end_tick(K_GRID,K_GRID), clock_rate
    real(kind=real64) :: start_time(K_GRID, K_GRID), end_time(K_GRID, K_GRID), elapsed_time(K_GRID,K_GRID)
    call CreateGrid( input_layer_A, gridA, K_GRID, K_GRID,0) !> create overlapping GRID
    !call WriteKLBin("MY_gridA_L10_D10.bin", gridA%layer_boxes, gridA%n_used)
    allocate( trackersGrid(K_GRID,K_GRID) )
    call PreprocessLayer( input_layer_A )
    call PreprocessLayer( input_layer_B )
    call system_clock(count_rate=clock_rate)

    bboxA = mbr_of_array( input_layer_A%layer_boxes, input_layer_A%n_used )
    !write(*,*) 'BBOX OF A = ', bboxA
    !write(*,*) '|B| = ', input_layer_B%n_used, ' ', input_layer_B%layerState
    write(*,'(A85)') '+----------------------------------------------------------------------------------+'
    write(*,'(A,A3,A,A3,A,A12,A,A12,A,A12,A,A12,A,A12,A,A,A)') '|','RO','|','CO','|','A in Grid','|','B in Grid','|',' Trackers ', '|', ' Comp B', '|', ' A AND COMP B', '|', ' REAL TIME', '|'
    write(*,'(A85)') '+----------------------------------------------------------------------------------+'
    do rows = 1, K_GRID
       do cols = 1, K_GRID
          call system_clock(count=start_tick(rows,cols))           
          call cpu_time(start_time(rows,cols))
          grids(rows,cols) = gridA%layer_boxes( (rows-1)*K_GRID+cols ) !> this does Layer = Box
          call ClearLayer( AinGrid( rows, cols ) )
          call ClearLayer( outputGrid(rows,cols) )
          call FilterLayer( input_layer_A, grids(rows,cols)%layer_boxes(1), AinGrid(rows, cols) )
          !> it is possible that AinGrid or BinGrid is empty, then we have to see what to do          
          if( AinGrid(rows,cols)%n_used == 0 ) then
             cycle
          end if
          call ClearLayer( BinGrid( rows, cols ) )                           
          call FilterLayer( input_layer_B, grids(rows,cols)%layer_boxes(1), BinGrid(rows, cols) )
          !write(*,*) '|G| = ', grids(rows,cols)%n_used, ' ', grids(rows,cols)%layerState, ' ', grids(rows,cols)%layer_boxes(1)
          if( BinGrid(rows,cols)%n_used == 0 ) then
             !> this is not a logical problem
             templayerGrid(rows,cols) = grids(rows,cols)%layer_boxes(1)
          else
             call generate_trackers( BinGrid(rows,cols)%layer_boxes, grids(rows,cols)%layer_boxes(1), &
                  trackersGrid(rows,cols)%trackers ) !> this does CW ordering of inner contours
             call scanline_fracture(trackersGrid(rows,cols)%trackers, templayerGrid(rows,cols)%layer_boxes)
             templayerGrid(rows,cols)%layerState = ior( templayerGrid(rows,cols)%layerState, LAYER_STATE_HEAL)
             templayerGrid(rows,cols)%n_used = size( templayerGrid(rows,cols)%layer_boxes)
             if( templayerGrid(rows,cols)%n_used == 0 ) error stop "GENERATED TASK 2 FRAME tempLayer EMPTY"                  
          end if
          if( templayerGrid(rows,cols)%n_used == 0 ) error stop "CONSUMED TASK 3 FRAME tempLayer EMPTY"
          outputGrid(rows,cols)%layerState = LAYER_STATE_EVERYTHING !> since this is just a collector
          call CalculateAND( AinGrid(rows,cols), templayerGrid(rows,cols), outputGrid(rows,cols))
          call cpu_time(end_time(rows,cols))
          call system_clock(count=end_tick(rows,cols))
          elapsed_time(rows,cols) = real(end_tick(rows,cols) - start_tick(rows,cols), kind=real64) / real(clock_rate, kind=real64)    
          write(*,'(A,I3,A,I3,A,I12,A,I12,A,I12,A,I12,A,I12,A,F10.3,A)') '|',rows,'|',cols,'|',AinGrid(rows,cols)%n_used,'|',&
               BinGrid(rows,cols)%n_used,'|', size(trackersGrid(rows,cols)%trackers),'|',&
               templayerGrid(rows,cols)%n_used,'|', outputGrid(rows,cols)%n_used,'|',elapsed_time(rows,cols), '|'
          if( allocated( BinGrid(rows,cols)%layer_boxes )) call ClearLayer( BinGrid( rows, cols ) )            
          if( allocated( trackersGrid(rows,cols)%trackers)) deallocate( trackersGrid(rows,cols)%trackers )            
          if( allocated( AinGrid(rows,cols)%layer_boxes )) call ClearLayer( AinGrid( rows, cols ) )
          if( allocated( templayerGrid(rows,cols)%layer_boxes )) call ClearLayer( templayerGrid(rows, cols) )
       end do
    end do
    write(*,'(A85)') '+-----------------------------------------------------------------------------------+'
    total_count = 0
    do rows = 1, K_GRID
       do cols = 1, K_GRID
          total_count = total_count + outputGrid(rows,cols)%n_used
       end do
    end do
    write(*,*) 'TOTAL COUNT = ', total_count

    if( allocated( output_layer%layer_boxes )) deallocate( output_layer%layer_boxes )
    allocate( output_layer%layer_boxes( total_count ) )
    total_count = 0
    do rows = 1, K_GRID
       do cols = 1, K_GRID
          do i = 1, outputGrid(rows,cols)%n_used
             output_layer%layer_boxes( total_count + i ) =  outputGrid(rows,cols)%layer_boxes(i)
          end do
          total_count = total_count + outputGrid(rows,cols)%n_used
       end do
    end do
    output_layer%n_used = total_count
    output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_HEAL )    !> correct by construction in each GRID
    call PreprocessLayer( output_layer )
    write(*,*) '|FRAME| = ', output_layer%n_used
  end subroutine CalculateFrameNOT

  ! Convert this function to use OpenMP task with dependencies;
  ! use rows, cols as indices for the dependencies between tiles
  ! all of which are independent. Even within a tile there is a
  ! clear logic and sequence of tasks as shown below. Devise a
  ! method and generate an optimized OpenMP function. Some of
  ! the routines called here also use OpenMP so be aware of
  ! that.
  subroutine CalculateFrameNOTTask( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout)   :: output_layer
    type(TrackerCell), allocatable :: trackersGrid(:,:)
    integer(kind=int64) :: total_count, i
    integer :: nthreads, tid
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096
    integer(kind=int64), parameter :: K_GRID = 16
    integer :: rows, cols
    type(Layer) :: gridA
    type(Layer) :: grids(K_GRID,K_GRID)
    type(Layer) :: outputGrid(K_GRID, K_GRID)
    type(Layer) :: templayerGrid(K_GRID, K_GRID)
    type(Layer) :: AinGrid(K_GRID, K_GRID)
    type(Layer) :: BinGrid(K_GRID, K_GRID)       !> I dont know if it makes sense to create so many temp layers
    type(Box)   :: bboxA
    integer(kind=int64) :: start_tick(K_GRID,K_GRID), end_tick(K_GRID,K_GRID), clock_rate
    real(kind=real64) :: start_time(K_GRID, K_GRID), end_time(K_GRID, K_GRID), elapsed_time(K_GRID,K_GRID)
    call CreateGrid( input_layer_A, gridA, K_GRID, K_GRID,0) !> create overlapping GRID
    !call WriteKLBin("MY_gridA_L10_D10.bin", gridA%layer_boxes, gridA%n_used)
    allocate( trackersGrid(K_GRID,K_GRID) )
    call PreprocessLayer( input_layer_A )
    call PreprocessLayer( input_layer_B )
    call system_clock(count_rate=clock_rate)

    bboxA = mbr_of_array( input_layer_A%layer_boxes, input_layer_A%n_used )
    !write(*,*) 'BBOX OF A = ', bboxA
    !write(*,*) '|B| = ', input_layer_B%n_used, ' ', input_layer_B%layerState
    do rows = 1, K_GRID
       do cols = 1, K_GRID
          !write(*,*) 'Processing ', rows, ' ', cols, ' Flat address ', (rows-1)*K_GRID+cols
          grids(rows,cols) = gridA%layer_boxes( (rows-1)*K_GRID+cols ) !> this does Layer = Box
          !write(*,*) '|G| = ', grids(rows,cols)%n_used, ' ', grids(rows,cols)%layerState, ' ', grids(rows,cols)%layer_boxes(1)
          call ClearLayer( AinGrid( rows, cols ) )
          call ClearLayer( BinGrid( rows, cols ) )            
       end do
    end do
    write(*,'(A85)') '+----------------------------------------------------------------------------------+'
    write(*,'(A,A3,A,A3,A,A12,A,A12,A,A12,A,A12,A,A12,A,A,A)') '|','RO','|','CO','|','A in Grid','|','B in Grid','|',' Trackers ', '|', ' Comp B', '|', ' A AND COMP B', '|', ' REAL TIME', '|'
    write(*,'(A85)') '+----------------------------------------------------------------------------------+'
    !> Initiate OpenMP Parallel region and spawn tasks from a single thread

    !> Initiate OpenMP Parallel Loop over the independent tiles
    !> schedule(dynamic, 1) means threads grab 1 tile, finish it, and grab the next available.
    !$omp parallel do collapse(2) schedule(dynamic, 1) &
    !$omp& private(rows, cols) &
    !$omp& shared(gridA, input_layer_A, input_layer_B, grids, AinGrid, BinGrid, &
    !$omp&        templayerGrid, trackersGrid, outputGrid)
    do rows = 1, K_GRID
       do cols = 1, K_GRID
          call system_clock(count=start_tick(rows,cols))           
          call cpu_time(start_time(rows,cols))
          !> TASK 1: Initialization and Filtering of Layer A
          !$komp task shared(gridA, grids, input_layer_A, AinGrid, outputGrid) &
          !$komp& depend(out: grids(rows,cols), AinGrid(rows,cols)) firstprivate(rows, cols)
          grids(rows,cols) = gridA%layer_boxes( (rows-1)*K_GRID+cols ) !> this does Layer = Box
          !write(*,*) '|G| = ', grids(rows,cols)%n_used, ' ', grids(rows,cols)%layerState, ' ', grids(rows,cols)%layer_boxes(1)
          call ClearLayer( AinGrid( rows, cols ) )
          call ClearLayer( outputGrid(rows,cols) )
          !> find all B inside this GRID
          !write(*,*) 'Now processing B'
          !write(*,*) '|G| = ', grids(rows,cols)%n_used, ' ', grids(rows,cols)%layerState, ' ', grids(rows,cols)%layer_boxes(1)
          call FilterLayer( input_layer_A, grids(rows,cols)%layer_boxes(1), AinGrid(rows, cols) )
          !$komp end task

          !if( AinGrid(rows,cols)%n_used == 0 ) then
          !   cycle
          !end if
          !> TASK 2: Filtering Layer B, Tracking, and Fracturing
          !$komp task shared(input_layer_B, grids, AinGrid, BinGrid, templayerGrid, trackersGrid) &
          !$komp& depend(in: grids(rows,cols), AinGrid(rows,cols)) &
          !$komp& depend(out: BinGrid(rows,cols), templayerGrid(rows,cols), trackersGrid(rows,cols)) firstprivate(rows, cols)

          if( AinGrid(rows,cols)%n_used > 0 ) then
             call ClearLayer( BinGrid( rows, cols ) )                           
             call FilterLayer( input_layer_B, grids(rows,cols)%layer_boxes(1), BinGrid(rows, cols) )
             !write(*,*) '|G| = ', grids(rows,cols)%n_used, ' ', grids(rows,cols)%layerState, ' ', grids(rows,cols)%layer_boxes(1)
             !> it is possible that AinGrid or BinGrid is empty, then we have to see what to do
             if( BinGrid(rows,cols)%n_used == 0 ) then
                !> this is not a logical problem
                templayerGrid(rows,cols) = grids(rows,cols)%layer_boxes(1)
             else
                call generate_trackers( BinGrid(rows,cols)%layer_boxes, grids(rows,cols)%layer_boxes(1), &
                     trackersGrid(rows,cols)%trackers ) !> this does CW ordering of inner contours
                call scanline_fracture(trackersGrid(rows,cols)%trackers, templayerGrid(rows,cols)%layer_boxes)
                templayerGrid(rows,cols)%layerState = ior( templayerGrid(rows,cols)%layerState, LAYER_STATE_HEAL)
                templayerGrid(rows,cols)%n_used = size( templayerGrid(rows,cols)%layer_boxes)
                if( templayerGrid(rows,cols)%n_used == 0 ) error stop "GENERATED TASK 2 FRAME tempLayer EMPTY"                  
             end if
          end if
          !$komp end task

          !> TASK 3: Logic AND, I/O Output, and Cleanup
          !$komp task shared(AinGrid, BinGrid, templayerGrid, outputGrid, trackersGrid) &
          !$komp& depend(in: AinGrid(rows,cols), BinGrid(rows,cols), templayerGrid(rows,cols), trackersGrid(rows,cols)) &
          !$komp& depend(out: outputGrid(rows,cols)) firstprivate(rows, cols)
          if( AinGrid(rows,cols)%n_used > 0 ) then
             if( templayerGrid(rows,cols)%n_used == 0 ) error stop "CONSUMED TASK 3 FRAME tempLayer EMPTY"
             !write(*,*) 'Now processing A'
             !write(*,*) '|G| = ', grids(rows,cols)%n_used, ' ', grids(rows,cols)%layerState, ' ', grids(rows,cols)%layer_boxes(1)
             outputGrid(rows,cols)%layerState = LAYER_STATE_EVERYTHING !> since this is just a collector
             !$omp critical (gpu_lock)
             call CalculateAND( AinGrid(rows,cols), templayerGrid(rows,cols), outputGrid(rows,cols))
             !$omp end critical (gpu_lock)
             call cpu_time(end_time(rows,cols))
             call system_clock(count=end_tick(rows,cols))
             elapsed_time(rows,cols) = real(end_tick(rows,cols) - start_tick(rows,cols), kind=real64) / real(clock_rate, kind=real64)    
             !$omp critical (print_lock)
             write(*,'(A,I3,A,I3,A,I12,A,I12,A,I12,A,I12,A,I12,A,F10.3,A)') '|',rows,'|',cols,'|',AinGrid(rows,cols)%n_used,'|',&
                  BinGrid(rows,cols)%n_used,'|', size(trackersGrid(rows,cols)%trackers),'|',&
                  templayerGrid(rows,cols)%n_used,'|', outputGrid(rows,cols)%n_used,'|',elapsed_time(rows,cols), '|'

             if( allocated( BinGrid(rows,cols)%layer_boxes )) call ClearLayer( BinGrid( rows, cols ) )            
             if( allocated( trackersGrid(rows,cols)%trackers)) deallocate( trackersGrid(rows,cols)%trackers )            
             if( allocated( AinGrid(rows,cols)%layer_boxes )) call ClearLayer( AinGrid( rows, cols ) )
             if( allocated( templayerGrid(rows,cols)%layer_boxes )) call ClearLayer( templayerGrid(rows, cols) )
             !$omp end critical (print_lock)            
          end if
          !$komp end task   
          !write(*,*) 'Processing ', rows, ' ', cols, ' ', outputGrid(rows,cols)%n_used
       end do
    end do
    !$omp end parallel do
    write(*,'(A85)') '+-----------------------------------------------------------------------------------+'
    total_count = 0
    do rows = 1, K_GRID
       do cols = 1, K_GRID
          total_count = total_count + outputGrid(rows,cols)%n_used
       end do
    end do
    write(*,*) 'TOTAL COUNT = ', total_count

    if( allocated( output_layer%layer_boxes )) deallocate( output_layer%layer_boxes )
    allocate( output_layer%layer_boxes( total_count ) )
    total_count = 0
    do rows = 1, K_GRID
       do cols = 1, K_GRID
          do i = 1, outputGrid(rows,cols)%n_used
             output_layer%layer_boxes( total_count + i ) =  outputGrid(rows,cols)%layer_boxes(i)
          end do
          total_count = total_count + outputGrid(rows,cols)%n_used
       end do
    end do
    output_layer%n_used = total_count
    output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_HEAL )    !> correct by construction in each GRID
    call PreprocessLayer( output_layer )
    write(*,*) '|FRAME| = ', output_layer%n_used
  end subroutine CalculateFrameNOTTask


end submodule DesignImplModule
