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
  use GPUMergeModule
  implicit none

contains

  !=================================================================
  !  CreateGrid – implementation (exactly the code from the previous
  !  answer, unchanged except that we are now inside a submodule)
  !=================================================================
  module subroutine CreateGrid( input_layer, output_layer, rows, cols )
    type(Layer), intent(in)    :: input_layer    
    type(Layer), intent(inout) :: output_layer    
    integer,     intent(in)    :: rows, cols
    type(Box)                  :: input_box
    integer(kind=K_COORDINATE_KIND) :: x_lo, x_hi, y_lo, y_hi
    integer(kind=K_COORDINATE_KIND) :: full_w, full_h
    integer(kind=K_COORDINATE_KIND) :: col_width, row_height
    integer(kind=K_COORDINATE_KIND) :: extra_w, extra_h
    integer(kind=int64)             :: i, j, idx, new_alloc
    type(Box)                       :: subbox
    type(Box), allocatable          :: temp(:)
    input_box = mbr_of_array( input_layer%layer_boxes, input_layer%n_used )
    if( allocated( output_layer%layer_boxes ) ) then
       deallocate( output_layer%layer_boxes )
       output_layer%n_used = 0
       output_layer%n_alloc = 0
    end if

    !-----------------------------------------------------------------
    !  1. sanity checks
    !-----------------------------------------------------------------
    if (rows <= 0 .or. cols <= 0) then
       error stop "CreateGrid: rows and cols must be positive"
    end if

    if (.not. input_box%is_valid()) then
       error stop "CreateGrid: input_box is not a valid rectangle"
    end if

    !-----------------------------------------------------------------
    !  2. Normalise the limits – we want x1 <= x2, y1 <= y2
    !-----------------------------------------------------------------
    x_lo = min(input_box%x1, input_box%x2)
    x_hi = max(input_box%x1, input_box%x2)
    y_lo = min(input_box%y1, input_box%y2)
    y_hi = max(input_box%y1, input_box%y2)

    !-----------------------------------------------------------------
    !  3. Compute the total width / height (inclusive)
    !-----------------------------------------------------------------
    full_w = x_hi - x_lo + 1_K_COORDINATE_KIND   ! number of integer columns
    full_h = y_hi - y_lo + 1_K_COORDINATE_KIND   ! number of integer rows

    !-----------------------------------------------------------------
    !  4. Base size + remainder distribution
    !-----------------------------------------------------------------
    col_width = full_w / cols
    extra_w   = mod(full_w, cols)

    row_height = full_h / rows
    extra_h    = mod(full_h, rows)

    !-----------------------------------------------------------------
    !  5. Grow the destination array if necessary
    !-----------------------------------------------------------------
    new_alloc = rows*cols
    allocate( output_layer%layer_boxes( new_alloc ) )
    output_layer%n_alloc = new_alloc
    output_layer%n_used  = rows*cols
    !-----------------------------------------------------------------
    !  6. Fill the sub‑boxes
    !-----------------------------------------------------------------
    idx = 1
    do i = 1, rows
       block
         integer(kind=K_COORDINATE_KIND) :: y0, y1
         y0 = y_lo + i*row_height + min(i, extra_h)
         y1 = y0 + row_height - int(1,kind=K_COORDINATE_KIND)
         if (i < extra_h) y1 = y1 + int(1,kind=K_COORDINATE_KIND)

         do j = 1, cols
            block
              integer(kind=K_COORDINATE_KIND) :: x0, x1
              x0 = x_lo + j*col_width + min(j, extra_w)
              x1 = x0 + col_width - int(1,kind=K_COORDINATE_KIND)
              if (j < extra_w) x1 = x1 + int(1,kind=K_COORDINATE_KIND)
              subbox%x1 = x0
              subbox%y1 = y0
              subbox%x2 = x1
              subbox%y2 = y1
              output_layer%layer_boxes(idx) = subbox
              idx = idx + 1
            end block
         end do
       end block
    end do
    output_layer%n_used = rows*cols
    call PreprocessLayer( output_layer )
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
    type(Layer), intent(in) :: input_layer_A
    integer(kind=int64) :: interaction_count, N, total_nodes
    type(RTreeNodeGPU), allocatable:: TreeNodes(:)
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
    total_nodes = CalculateTotalNodesGPU( N, K_LEAF_CAPACITY ) !> for GPU we might change
    call SortBoxesDirect( input_layer_A%layer_boxes, N )
    allocate( TreeNodes( total_nodes ) )
    call BuildRTreeGPU( input_layer_A%layer_boxes, K_LEAF_CAPACITY, TreeNodes, RootIndex)
    call ComputeInteractionsGPU( TreeNodes, total_nodes, input_layer_A%layer_boxes, N, RootIndex, interaction_count)
  end function CalculateOverlapCount
  #else
  module function CalculateOverlapCount( input_layer_A ) result( interaction_count )
    type(Layer), intent(in) :: input_layer_A
    integer(kind=int64) :: interaction_count
    interaction_count = 1
  end function CalculateOverlapCount
  #endif

  #if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)      
  module function CalculateSingletonCount( input_layer_A, is_singleton ) result( interaction_count )
    type(Layer), intent(in) :: input_layer_A
    integer(kind=int64) :: interaction_count, N, total_nodes
    type(RTreeNodeGPU), allocatable:: TreeNodes(:)
    integer(kind=int64) :: RootIndex    
    logical, allocatable, intent(out) :: is_singleton(:)
    interaction_count = 0
    if( input_layer_A%n_used == 0 ) then
       return
    end if
    N = input_layer_A%n_used
    total_nodes = CalculateTotalNodesGPU( N, K_LEAF_CAPACITY ) !> for GPU we might change
    call SortBoxesDirect( input_layer_A%layer_boxes, N )
    allocate( TreeNodes( total_nodes ) )
    call BuildRTreeGPU( input_layer_A%layer_boxes, K_LEAF_CAPACITY, TreeNodes, RootIndex)
    call FindSingletonsGPU( input_layer_A%layer_boxes, TreeNodes, RootIndex, is_singleton, interaction_count)
  end function CalculateSingletonCount
  #else
  module function CalculateSingletonCount( input_layer_A, is_singleton ) result( interaction_count )
    type(Layer), intent(in) :: input_layer_A
    logical, allocatable, intent(out) :: is_singleton(:)
    integer(kind=int64) :: interaction_count
    interaction_count = 0
  end function CalculateSingletonCount
  #endif

  
  
  
  module subroutine CalculateSingleLayerAND( input_layer_A, output_layer )
    type(Layer), intent(inout) :: input_layer_A
    type(Layer), intent(inout) :: output_layer
    integer(kind=int64) :: num_boxesA, num_boxesC
    integer(kind=int64) :: leafboxes(K_MAX_SEARCH_LEAVES) ! better choose a large number
    integer(kind=int64) :: number_leaves
    integer(kind=int64) :: i, j, k, interaction_count, N, total_nodes
    type(RTreeNodeGPU), allocatable:: TreeNodes(:)
    integer(kind=int64) :: RootIndex    
    type(Layer), allocatable :: buffers(:)
    type(Box) :: tempBox, bbox
    type(Box), pointer :: boxes
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
    total_nodes = CalculateTotalNodesGPU( N, K_LEAF_CAPACITY ) !> for GPU we might change
    bbox = mbr_of_array( input_layer_A%layer_boxes, N )
    write(*,*) 'Loaded : ', N, ' BBOX = ', bbox, ' |T| = ', total_nodes
    call SortBoxesDirect( input_layer_A%layer_boxes, N )
    allocate( TreeNodes( total_nodes ) )
    call BuildRTreeGPU( input_layer_A%layer_boxes, K_LEAF_CAPACITY, TreeNodes, RootIndex)
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
    output_layer%layer_boxes = input_layer%layer_boxes
    output_layer%n_used = input_layer%n_used
    call box_grow_directional(output_layer%layer_boxes, ivar(1), ivar(2), ivar(3), ivar(4) )
    call PreprocessLayer( output_layer )
  end subroutine CalculateGROWLayer
  module subroutine CalculateSHRINKLayer( input_layer_A, shrink_value, output_layer )
    type(Layer), intent(inout) :: input_layer_A
    type(Layer), intent(inout)   :: output_layer
    integer(kind=K_COORDINATE_KIND), intent(in) :: shrink_value
    integer(kind=int64) :: num_boxesA, output_box_count
    type(XYTracker), allocatable :: trackers(:)
    type(Box) :: tempBox, bboxA, bboxB
    type(Layer) :: temp_layer
    integer :: nthreads, tid, alloc_status
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
  

end submodule DesignImplModule
