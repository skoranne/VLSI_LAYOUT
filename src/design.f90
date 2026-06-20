! File   : design.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Keep all layer information together
module DesignModule
  use CommonModule
  use hash_mod  
  use GeometryModule
  use RTreeBuilder
  use DataStructuresModule
  use PNumMergeModule
  use PolygonFractureModule
  use MortonSortModule
  use MortonSortOMT
  use iso_c_binding
  use iso_fortran_env, only : int32, int64, real64
  use omp_lib
  implicit none  
  private
  public :: Design, Layer, LAYER_STATE_NONE, LAYER_STATE_HEAL, &
       LAYER_STATE_SORT, LAYER_STATE_PNUM, LAYER_STATE_RTREE, LAYER_STATE_EVERYTHING, &
       DESIGN_DIRECTION_INPUT, DESIGN_DIRECTION_OUTPUT, DESIGN_DIRECTION_MEMORY,&
       NeedsSorting, NeedsPNum, NeedsHealing, PerformUnion, PerformPolygonUnion, BucketBoundary, &
       get_equal_key_segments, GetSortPermutation, calculate_union_area_by_polygon, &
       PreprocessLayer, PreprocessLayerByPolygon, PreprocessLayerSL, &
       CalculateSingleLayerAND, CalculateAND, CalculateOR, CalculateNOT, &
       CalculateGROWLayer, CalculateSHRINKLayer, CreateGRID, CreateEXTENT,&
       RemoveIdentical, CalculateOverlapCount
  
  type :: LayerTree
     integer(kind=int64) :: root_index
     type(RTreeNode), allocatable :: tree_nodes(:)
  end type LayerTree
  enum, bind(C)                     ! bind(C) makes the values C‑compatible
     enumerator :: LAYER_STATE_NONE  = int(Z'00', kind=c_int)   ! 0b000000
     enumerator :: LAYER_STATE_HEAL  = int(Z'01', kind=c_int)   ! 0b000001
     enumerator :: LAYER_STATE_SORT  = int(Z'02', kind=c_int)   ! 0b000010
     enumerator :: LAYER_STATE_PNUM  = int(Z'04', kind=c_int)   ! 0b000100
     enumerator :: LAYER_STATE_RTREE = int(Z'08', kind=c_int)   ! 0b001000
     enumerator :: LAYER_STATE_SLSORT= int(Z'16', kind=c_int)   ! 0b010000
     enumerator :: LAYER_STATE_EVERYTHING = int(Z'31', kind=c_int)   ! 0b011111
  end enum
  enum, bind(C)
     enumerator :: CONJUGATE_LAYER_PURPOSE_NONE       = int(Z'00', kind=c_int)   ! 0b000000
     enumerator :: CONJUGATE_LAYER_PURPOSE_COMPLEMENT = int(Z'01', kind=c_int)   ! 0b000001
     enumerator :: CONJUGATE_LAYER_PURPOSE_SECTION    = int(Z'02', kind=c_int)   ! 0b000100
     enumerator :: CONJUGATE_LAYER_PURPOSE_GENERAL    = int(Z'04', kind=c_int)   ! 0b001000
     enumerator :: CONJUGATE_LAYER_PURPOSE_EXTENT     = int(Z'08', kind=c_int)   ! 0b001000     
  end enum

  type :: ConjugateLayer
     integer :: conjugate_lid
     integer(kind=c_int) :: purpose = CONJUGATE_LAYER_PURPOSE_NONE
  end type ConjugateLayer
  type :: Repetition
     integer(kind=K_COORDINATE_KIND) :: num_rows, num_cols, dx, dy
  end type Repetition

  type :: Layer
     integer(kind=8)        :: n_used   = 0   ! how many slots are filled
     integer(kind=8)        :: n_alloc  = 0   ! current allocation size
     type(Box), allocatable :: layer_boxes(:)
     integer(kind=c_int)        :: layerState = 0 ! HEAL, SORT, PNUM, RTREE
     type(LayerTree)        :: tree
     type(UnionFind)        :: pnumtable
     real(kind=real64)      :: area, perimeter
     character(len=:), allocatable :: fileName
     type(ConjugateLayer) :: paired_layer
     type(Repetition)     :: in_place_repetition
  end type Layer

  enum, bind(C)                     ! bind(C) makes the values C‑compatible
     enumerator :: DESIGN_DIRECTION_INPUT  =   int(Z'00', kind=c_int)
     enumerator :: DESIGN_DIRECTION_OUTPUT =   int(Z'01', kind=c_int)
     enumerator :: DESIGN_DIRECTION_MEMORY =   int(Z'02', kind=c_int)     
  end enum

  type :: Design
     type(Layer), allocatable :: layers(:)
     type(hash_type) :: ht
     character(len=1024), dimension(:), allocatable :: layerNames(:)
     type(Box)              :: DESIGN_EXTENT
     integer(kind=c_int)    :: design_direction = DESIGN_DIRECTION_INPUT
     character(len=:), allocatable :: fileName     
  end type Design

  ! A clean, modern derived type to hold our bucket boundaries
  type :: BucketBoundary
     integer(int64) :: start_idx
     integer(int64) :: end_idx
  end type BucketBoundary

  !------------------------------------------------------------------
  !  Public interface
  !------------------------------------------------------------------
  interface
     module function CalculateOverlapCount( input_layer_A ) result( interaction_count )
       type(Layer), intent(in) :: input_layer_A
       integer(kind=int64) :: interaction_count
     end function CalculateOverlapCount

     module function CalculateSingletonCount( input_layer_A ) result( interaction_count )
       type(Layer), intent(in) :: input_layer_A
       integer(kind=int64) :: interaction_count
     end function CalculateSingletonCount
     
     module subroutine RemoveIdentical(input_layer)
       type(Layer), intent(inout) :: input_layer
     end subroutine RemoveIdentical
     
     module subroutine CalculateSingleLayerAND( input_layer_A, output_layer )
       type(Layer), intent(inout) :: input_layer_A
       type(Layer), intent(inout) :: output_layer
     end subroutine CalculateSingleLayerAND
     
     module subroutine CreateGrid( input_layer, output_layer, rows, cols )
       type(Layer),      intent(in)    :: input_layer
       type(Layer),      intent(inout) :: output_layer
       integer,          intent(in)    :: rows, cols   ! must be >0
     end subroutine CreateGrid
     
     module subroutine CreateEXTENT( input_layer, output_layer )
       type(Layer),      intent(in)    :: input_layer         
       type(Layer),      intent(inout) :: output_layer
     end subroutine CreateEXTENT

  end interface
 
contains
  pure function NeedsSorting(input_layer) result(retval)
    type(Layer), intent(in) :: input_layer
    logical :: retval
    retval = iand(input_layer%layerState, LAYER_STATE_SORT ) == 0
  end function NeedsSorting
  pure function NeedsPNum(input_layer) result(retval)
    type(Layer), intent(in) :: input_layer
    logical :: retval
    retval = iand(input_layer%layerState, LAYER_STATE_PNUM ) == 0
  end function NeedsPNum
  pure function NeedsRTree(input_layer) result(retval)
    type(Layer), intent(in) :: input_layer
    logical :: retval
    retval = iand(input_layer%layerState, LAYER_STATE_RTREE ) == 0
  end function NeedsRTree
  pure function NeedsHealing(input_layer) result(retval)
    type(Layer), intent(in) :: input_layer
    logical :: retval
    retval = iand(input_layer%layerState, LAYER_STATE_HEAL ) == 0
  end function NeedsHealing
  subroutine PerformUnion( input_layer )
    type(Layer), intent(inout) :: input_layer
    integer(kind=int64) :: i,n, updated_box_count
    type(Box) :: tempBox
    if( input_layer%n_used == 0 ) return
    if( .not. NeedsHealing( input_layer ) ) return !bravo
    call heal_boxes( input_layer%n_used, input_layer%layer_boxes, updated_box_count )
    n = size( input_layer%layer_boxes )
    write (*,*) 'Heal changed: ', input_layer%n_used, ' ', n        
    do i=1,n
       tempBox = input_layer%layer_boxes(i)
       if( .not. tempBox%is_valid() ) error stop "BOX NOT VALID"
       !write(*,'(A,I,A,4I)') 'Box ', i, ': ', tempBox%x1, tempBox%y1, tempBox%x2, tempBox%y2
    end do
    input_layer%n_used = n
    input_layer%n_alloc = n
    deallocate( input_layer%tree%tree_nodes )
    allocate( input_layer%tree%tree_nodes( CalculateTotalNodes( n, K_LEAF_CAPACITY ) ) )
    call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
    call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
  end subroutine PerformUnion
  !> In the case the pnumtable is already established, we should use polygon numbers
  !> and only perform the UNION per polygon
  !> helped function for indirect sorting,
  pure subroutine GetSortPermutation(arr, perm)
    integer(int64), intent(in) :: arr(:)    
    integer(int64), allocatable, intent(out) :: perm(:)
    integer(int64) :: i, n
    n = size(arr, kind=int64)
    allocate(perm(n))

    ! Initialize permutation array with 1, 2, 3... N
    do i = 1, n
       perm(i) = i
    end do

    ! Sort the indices
    if (n > 1) then
       call IndirectQuicksort(arr, perm, 1_int64, n)
    end if
  end subroutine GetSortPermutation

  pure recursive subroutine IndirectQuicksort(arr, perm, left, right)
    integer(int64), intent(in)    :: arr(:)
    integer(int64), intent(inout) :: perm(:)
    integer(int64), intent(in)    :: left, right

    integer(int64) :: i, j, temp
    integer(int64) :: pivot_val, pivot_perm
    integer(int64) :: key_idx, key_val
    integer(int64), parameter :: MIN_THRESHOLD = 16_int64

    if (left >= right) return

    ! ==========================================================
    ! 1. FALLBACK: Indirect Insertion Sort for small subarrays
    ! ==========================================================
    if (right - left + 1 <= MIN_THRESHOLD) then
       do i = left + 1, right
          key_idx = perm(i)
          key_val = arr(key_idx)
          j = i - 1

          ! Shift elements that are strictly greater.
          ! We use '>' instead of '>=' to ensure STABILITY. If values 
          ! are equal, we stop shifting to preserve original order.
          do while (j >= left)
             if (arr(perm(j)) > key_val) then
                perm(j+1) = perm(j)
                j = j - 1
             else
                exit
             end if
          end do
          perm(j+1) = key_idx
       end do

       ! ==========================================================
       ! 2. MAIN: Stable Indirect Quicksort Partitioning
       ! ==========================================================
    else
       ! We must track BOTH the value and the original index of the pivot
       pivot_perm = perm((left + right) / 2)
       pivot_val  = arr(pivot_perm)
       i = left
       j = right

       do while (i <= j)

          ! Left scan: strictly "less" than pivot. 
          ! (Smaller value, OR equal value but smaller original index)
          do while (arr(perm(i)) < pivot_val .or. &
               (arr(perm(i)) == pivot_val .and. perm(i) < pivot_perm))
             i = i + 1
          end do

          ! Right scan: strictly "greater" than pivot.
          ! (Larger value, OR equal value but larger original index)
          do while (arr(perm(j)) > pivot_val .or. &
               (arr(perm(j)) == pivot_val .and. perm(j) > pivot_perm))
             j = j - 1
          end do

          if (i <= j) then
             temp = perm(i)
             perm(i) = perm(j)
             perm(j) = temp
             i = i + 1
             j = j - 1
          end if
       end do

       if (left < j)  call IndirectQuicksort(arr, perm, left, j)
       if (i < right) call IndirectQuicksort(arr, perm, i, right)
    end if

  end subroutine IndirectQuicksort
  pure recursive subroutine SimpleIndirectQuicksort(arr, perm, left, right)
    integer(int64), intent(in)    :: arr(:)
    integer(int64), intent(inout) :: perm(:)
    integer(int64), intent(in) :: left, right
    integer(int64) :: i, j, temp, pivot_val
    if (left < right) then
       ! The pivot VALUE is looked up via the permutation array
       pivot_val = arr(perm((left + right) / 2))
       i = left
       j = right

       do while (i <= j)
          ! Compare values using indices from perm
          do while ((arr(perm(i)) < pivot_val))
             i = i + 1
          end do
          do while (pivot_val < arr(perm(j)))
             j = j - 1
          end do
          if (i <= j) then
             ! Swap the INDICES, not the actual Box data
             temp = perm(i)
             perm(i) = perm(j)
             perm(j) = temp
             i = i + 1
             j = j - 1
          end if
       end do
       if (left < j)  call SimpleIndirectQuicksort(arr, perm, left, j)
       if (i < right) call SimpleIndirectQuicksort(arr, perm, i, right)
    end if
  end subroutine SimpleIndirectQuicksort

  !--------------------------------------------------------------
  ! Fast Two-Pass Segmenter
  ! Returns an array of Segments representing the equal-key buckets
  !--------------------------------------------------------------
  pure subroutine get_equal_key_segments(arr, perm, segments)
    integer(int64), intent(in)  :: arr(:)
    integer(int64), intent(in)  :: perm(:)
    type(BucketBoundary), allocatable, intent(out) :: segments(:)

    integer(int64) :: i, n, num_segments

    n = size(perm, kind=int64)
    if (n == 0) then
       allocate(segments(0))
       return
    end if

    ! ==========================================================
    ! PASS 1: Count the total number of unique buckets.
    ! CPUs execute this rapidly due to tight loops and cache locality.
    ! ==========================================================
    num_segments = 1
    do i = 1, n - 1
       ! Compare the actual keys using our sorted permutation indices
       if (arr(perm(i)) /= arr(perm(i+1))) then
          num_segments = num_segments + 1
       end if
    end do

    ! Allocate exactly the required memory once
    allocate(segments(num_segments))

    ! ==========================================================
    ! PASS 2: Record the precise boundaries of each bucket
    ! ==========================================================
    num_segments = 1
    segments(1)%start_idx = 1

    do i = 1, n - 1
       if (arr(perm(i)) /= arr(perm(i+1))) then
          ! Close the current bucket
          segments(num_segments)%end_idx = i

          ! Open the next bucket
          num_segments = num_segments + 1
          segments(num_segments)%start_idx = i + 1
       end if
    end do

    ! Close the final bucket
    segments(num_segments)%end_idx = n

  end subroutine get_equal_key_segments
  !> One may think why go to the trouble of polygon-wise, but the fact is
  !> that unless one uses some sort of compression, the scanevents leak
  !> and create massive memory pressure, cf
  !> On MW:  Heal changed:   51953736        1568839856
  subroutine PerformPolygonUnion( input_layer )
    type(Layer), intent(inout) :: input_layer
    integer, parameter :: K_POLYGON_INIT_BOX_COUNT = 64
    integer(kind=int64) :: i,j,n, num_roots, num_rects, polygon_number, box_count, num_squares
    integer(kind=int64) :: updated_box_count, final_count, final_capacity
    type(Box) :: tempBox
    type(Box), allocatable :: current_polygon_boxes(:)
    type(Box), allocatable :: final_boxes(:)
    integer(int64), allocatable :: permutation(:)
    type(BucketBoundary), allocatable :: segments(:)
    integer(int64) :: starting_segment
    real(kind=real64) :: overlap_area, overlap_perimeter
    logical :: dominated_by_squares

    dominated_by_squares = .false.
    if( input_layer%n_used == 0 ) return
    if( NeedsPNum( input_layer ) ) then
       error stop "PLEASE RUN PNUM before."
    end if
    !>>> PLEASE UNCOMMENT <<<
    if( .not. NeedsHealing( input_layer ) ) return !bravo
    num_roots = input_layer%pnumtable%count_roots()
    num_rects = count(input_layer%pnumtable%arr == 0)
    num_squares = 0
    if( num_rects == input_layer%n_used ) then
       if( num_roots /= 0 ) error stop "INCONSISTENT ROOT/RECT count."
       return
    end if
    !write(*,*) '|Roots| = ', num_roots, '|Rects| = ', num_rects
    !write(*,*) input_layer%pnumtable%arr
    call GetSortPermutation( input_layer%pnumtable%arr, permutation )
    !write(*,*) 'Permutation = ', permutation
    call get_equal_key_segments( input_layer%pnumtable%arr, permutation, segments )
    !write(*,*) 'Segments = ', segments
    !> Usually the first segment comprises of the rectangles which we dont need to care about
    final_capacity = num_rects + 4*(input_layer%n_used-num_rects)
    !>>> Trying to debug the large memory <<<
    allocate( final_boxes( final_capacity ) )
    final_count = 0
    if( input_layer%pnumtable%arr( permutation( segments(1)%end_idx ) ) == 0 ) then
       !write (*,*) 'Indeed, RECTS ', segments(1)%end_idx
       !TODO: we have had some problem with this type of looping, better to use explicit for-loop
       final_boxes(1:segments(1)%end_idx) = input_layer%layer_boxes( &
            permutation( segments( 1 )%start_idx: segments( 1 )%end_idx ) )
       num_squares = count( is_square( final_boxes(1:segments(1)%end_idx) ) )
       final_count = segments(1)%end_idx
       if( input_layer%pnumtable%arr( permutation( segments(1)%start_idx ) ) /= 0 ) error stop "END_IDX=0, but START_IDX /= 0"
       !write (*,*) 'Input RECTANGLES copied over directly'
       do i=1,final_count
          tempBox = final_boxes(i)
          if( .not. tempBox%is_valid() ) error stop "BOX NOT VALID"
          !write(*,'(A,I,A,4I)') 'Box ', i, ': ', tempBox%x1, tempBox%y1, tempBox%x2, tempBox%y2
       end do
       starting_segment = 2
    else
       starting_segment = 1
    end if
    if( num_squares*1.0_real64 / (input_layer%n_used*1.0_real64) > K_SQUARE_DOMINATION_THRESHOLD ) then
       dominated_by_squares = .true.
    end if

    if( input_layer%pnumtable%arr( permutation( segments( starting_segment )%start_idx ) ) == 0 .or. &
         input_layer%pnumtable%arr( permutation( segments( starting_segment )%end_idx ) ) == 0 ) then
       error stop "INCONSISTENT BUCKET numbering detected"
    end if
    allocate( current_polygon_boxes( K_POLYGON_INIT_BOX_COUNT ) )
    !write (*,*) 'Now processing non-rects: final_count starts at ', final_count
    do i=starting_segment, size(segments)
       !> processing polygon number segments( starting_segment )%start_idx
       polygon_number = input_layer%pnumtable%arr( permutation( segments( i )%start_idx ) )
       box_count = 1+segments( i )%end_idx-segments( i )%start_idx
       if( input_layer%pnumtable%arr( permutation( segments( i )%end_idx ) ) /= polygon_number ) then
          error stop "INCONSISTENT polygon numbering detected"
       end if

       if( box_count > size( current_polygon_boxes ) ) then
          !write(*,'(4(A,I0),A)') 'Processing polygon number: ', polygon_number, ' with ', box_count, ' rects, |cpb| = ', size( current_polygon_boxes ), ' |FC| = ', final_capacity, ' '
          deallocate( current_polygon_boxes )
          allocate( current_polygon_boxes( 2*box_count ) )
       end if
       current_polygon_boxes(1:box_count) = input_layer%layer_boxes( &
            permutation( segments( i )%start_idx:segments( i )%end_idx ) )
       updated_box_count = 0
       call heal_boxes( box_count, current_polygon_boxes, updated_box_count )
       n = size( current_polygon_boxes )
       if( final_count + updated_box_count >= final_capacity ) then
          !write(*,*) 'FC = ', final_capacity, ' EC = ', final_count + updated_box_count
          error stop "FINAL CAPACITY is too low."
       end if
       final_boxes(final_count+1:final_count+updated_box_count) = current_polygon_boxes(1:updated_box_count)
       final_count = final_count + updated_box_count
       !write (*,*) 'Heal changed: ', box_count, ' ', updated_box_count, ' current count = ', final_count       
       do j=1,updated_box_count
          tempBox = current_polygon_boxes(j)
          if( .not. tempBox%is_valid() ) error stop "BOX NOT VALID"
          !write(*,'(A,I,A,4I)') 'Box ', j, ': ', tempBox%x1, tempBox%y1, tempBox%x2, tempBox%y2
       end do
       !write (*,*) current_polygon_boxes(1:box_count)
       !> do we have enough space in final_boxes ?
       !if (box_count > max_boxes) then
       !   allocate(resized_boxes(max_boxes * 2))
       !   resized_boxes(1:max_boxes) = temp_boxes
       !   call move_alloc(resized_boxes, temp_boxes)
       !   max_boxes = max_boxes * 2
       !end if
    end do
    !write(*,'(3(A,I12))') '|Roots| = ', num_roots, ' |Rects| = ', num_rects, ' FINAL COUNT = ', final_count
    deallocate( current_polygon_boxes )
    allocate( current_polygon_boxes( final_count ) )
    current_polygon_boxes(1:final_count) = final_boxes(1:final_count)
    deallocate( final_boxes )
    !deallocate( input_layer%layer_boxes )
    call move_alloc(from=current_polygon_boxes, to=input_layer%layer_boxes )
    do i=1,final_count
       tempBox = input_layer%layer_boxes(i)
       if( .not. tempBox%is_valid() ) error stop "BOX NOT VALID"
       !write(*,'(A,I,A,4I)') 'Box ', i, ': ', tempBox%x1, tempBox%y1, tempBox%x2, tempBox%y2
    end do
    if( size(input_layer%layer_boxes) /= final_count ) error stop "INCONSISTENT layer box size." 
    input_layer%n_used = final_count
    input_layer%n_alloc = final_count
    deallocate( input_layer%tree%tree_nodes )
    allocate( input_layer%tree%tree_nodes( CalculateTotalNodes( final_count, K_LEAF_CAPACITY ) ) )
    !> the layer could be square dominated, lets look at pre-healed rectangles and decide
    if( dominated_by_squares ) then
       call MortonSort( input_layer%layer_boxes )
    else
       call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
    end if
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_HEAL ) !> I hope so    
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
    call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
    call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
         input_layer%tree%root_index, overlap_area, overlap_perimeter)
    !write(*,*) 'Last pnumtable: ', input_layer%pnumtable%arr
    !write(*,*) '|Layer| = ', size(input_layer%layer_boxes), ' n = ', input_layer%n_used
  end subroutine PerformPolygonUnion

  pure function calculate_union_area_by_polygon( input_layer ) result( retval_area )
    type(Layer), intent(in) :: input_layer
    real(kind=real64) :: retval_area, temp_areaA, temp_areaB
    integer, parameter :: K_POLYGON_INIT_BOX_COUNT = 64
    integer(kind=int64) :: i,j, num_roots, num_rects, polygon_number, box_count
    type(Box), allocatable :: current_polygon_boxes(:)
    integer(int64), allocatable :: permutation(:)
    type(BucketBoundary), allocatable :: segments(:)
    integer(int64) :: starting_segment
    retval_area = 0.0
    if( input_layer%n_used == 0 ) return
    if( NeedsPNum( input_layer ) ) then
       error stop "PLEASE RUN PNUM before."
    end if
    do i = 1, input_layer%n_used
       retval_area = retval_area + box_area_vectorized( input_layer%layer_boxes(i) )
    end do
    !>>> PLEASE UNCOMMENT <<<
    if( .not. NeedsHealing( input_layer ) ) return !bravo
    retval_area = 0.0
    num_roots = input_layer%pnumtable%count_roots()
    num_rects = count(input_layer%pnumtable%arr == 0)
    if( num_rects == input_layer%n_used ) then
       if( num_roots /= 0 ) error stop "INCONSISTENT ROOT/RECT count."
       return
    end if
    !write(*,*) '|Roots| = ', num_roots, '|Rects| = ', num_rects
    !write(*,*) input_layer%pnumtable%arr
    call GetSortPermutation( input_layer%pnumtable%arr, permutation )
    !write(*,*) 'Permutation = ', permutation
    call get_equal_key_segments( input_layer%pnumtable%arr, permutation, segments )
    !write(*,*) 'Segments = ', segments
    !> Usually the first segment comprises of the rectangles which we dont need to care about
    if( input_layer%pnumtable%arr( permutation( segments(1)%end_idx ) ) == 0 ) then
       !write (*,*) 'Indeed, RECTS ', segments(1)%end_idx
       do i = 1, segments(1)%end_idx
          retval_area = retval_area + box_area_vectorized( input_layer%layer_boxes( permutation( i ) ) )
       end do
       if( input_layer%pnumtable%arr( permutation( segments(1)%start_idx ) ) /= 0 ) error stop "END_IDX=0, but START_IDX /= 0"
       starting_segment = 2
    else
       starting_segment = 1
    end if
    !write(*,*) 'Rectangle accumulated AREA = ', retval_area
    if( num_roots == 0 ) return !> everything is in rects and we have accounted for all of them
    if( input_layer%pnumtable%arr( permutation( segments( starting_segment )%start_idx ) ) == 0 .or. &
         input_layer%pnumtable%arr( permutation( segments( starting_segment )%end_idx ) ) == 0 ) then
       error stop "INCONSISTENT BUCKET numbering detected"
    end if
    allocate( current_polygon_boxes( K_POLYGON_INIT_BOX_COUNT ) )
    !write (*,*) 'Now processing non-rects: final_count starts at ', starting_segment
    do i=starting_segment, size(segments)
       !> processing polygon number segments( starting_segment )%start_idx
       polygon_number = input_layer%pnumtable%arr( permutation( segments( i )%start_idx ) )
       box_count = 1+segments( i )%end_idx-segments( i )%start_idx
       if( input_layer%pnumtable%arr( permutation( segments( i )%end_idx ) ) /= polygon_number ) then
          error stop "INCONSISTENT polygon numbering detected"
       end if
       !write(*,'(2(A,I0),A)') 'Processing polygon number: ', polygon_number, ' with ', box_count, ' rects.'
       if( box_count > size( current_polygon_boxes ) ) then
          deallocate( current_polygon_boxes )
          allocate( current_polygon_boxes( 2*box_count ) )
       end if
       current_polygon_boxes(1:box_count) = input_layer%layer_boxes( &
            permutation( segments( i )%start_idx: segments( i )%end_idx ) )
       temp_areaA = 0.0_real64
       temp_areaB = 0.0_real64

       temp_areaA = sum( box_area_vectorized( current_polygon_boxes(1:box_count) ) )
       do j=1,box_count
          if(.not. current_polygon_boxes(j)%is_valid() ) error stop "INVALID BOX"
          !write(*,'(A,I,A,4I)') 'Box ', j, ': ', current_polygon_boxes(j)%x1, current_polygon_boxes(j)%y1, &
          !     current_polygon_boxes(j)%x2, current_polygon_boxes(j)%y2
       end do
       !temp_areaB = calculate_union_area( current_polygon_boxes(1:box_count) )
       temp_areaB = calculate_union_area_fast( current_polygon_boxes(1:box_count) )
       if( temp_areaB > temp_areaA ) error stop "INCONSISTENT AREA CALCULATION"
       retval_area = retval_area + temp_areaB
       !retval_area = retval_area + temp_areaA
    end do
    deallocate( current_polygon_boxes )
  end function calculate_union_area_by_polygon

  subroutine PreprocessLayerSL( input_layer )
    type(Layer), intent(inout) :: input_layer
    real(kind=real64) :: overlap_area, overlap_perimeter
    integer(kind=int64) :: output_box_count, num_squares
    logical             :: dominated_by_squares
    integer            :: env_len, env_status
    call get_environment_variable( 'MAGPARSER_CONTROL_PREPROCESS_GPU_SORT', length=env_len, status=env_status )
    
    if( input_layer%n_used == 0 ) then
       input_layer%layerState = 31 !> we set everything
       return
    end if

    if( NeedsHealing( input_layer ) ) then
       !> by the time we come here this should not be needed
       !$komp critical (heal_boxes_lock) !> work around
       call heal_boxes( input_layer%n_used, input_layer%layer_boxes, output_box_count )
       !$komp end critical (heal_boxes_lock)
       write(*,*) 'Healing from: ', input_layer%n_used, ' to ', output_box_count
       input_layer%layerState = LAYER_STATE_HEAL !> we wipe everything else
       input_layer%n_used = output_box_count
    end if

    if( NeedsSorting( input_layer ) ) then
       if( env_status /= 0 ) then !> value is set
          call SortBoxesDirect( input_layer%layer_boxes, int( input_layer%n_used, kind=int64 ) )
       else
          dominated_by_squares = .false.
          num_squares = count( is_square( input_layer%layer_boxes ) )
          if( num_squares*1.0_real64 / (input_layer %n_used*1.0_real64) > K_SQUARE_DOMINATION_THRESHOLD ) then
             write(*,*) 'Using MORTON as layer is dominated by squares.'
             dominated_by_squares = .true.
          end if
          if( dominated_by_squares ) then
             call MortonSort( input_layer%layer_boxes )
          else
             call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
          end if
       end if
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
    end if
    if( NeedsRTree( input_layer ) ) then
       if( allocated( input_layer%tree%tree_nodes ) ) deallocate( input_layer%tree%tree_nodes )
       allocate( input_layer%tree%tree_nodes( CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY ) ) )
       call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
    end if
    if( NeedsPNum( input_layer ) ) then
       call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
            input_layer%tree%root_index, overlap_area, overlap_perimeter)
       if( overlap_area /= 0 ) then
          !error stop "HEALING failed"
       end if
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_PNUM )
    end if
  end subroutine PreprocessLayerSL
  !> we have an alternative method using the RTree which we should compare
  subroutine PreprocessLayerByPolygon( input_layer )
    type(Layer), intent(inout) :: input_layer
    real(kind=real64) :: overlap_area, overlap_perimeter
    integer(kind=int64) :: output_box_count, num_squares
    logical             :: dominated_by_squares
    if( input_layer%n_used == 0 ) then
       input_layer%layerState = LAYER_STATE_EVERYTHING !> we set everything
       return
    end if

    if( NeedsSorting( input_layer ) ) then
       dominated_by_squares = .false.
       num_squares = count( is_square( input_layer%layer_boxes ) )
       if( num_squares*1.0_real64 / (input_layer %n_used*1.0_real64) > K_SQUARE_DOMINATION_THRESHOLD ) then
          write(*,*) 'Using MORTON as layer is dominated by squares.'
          dominated_by_squares = .true.
       end if
       if( dominated_by_squares ) then
          call MortonSort( input_layer%layer_boxes )
       else
          call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
       end if
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
    end if
    if( NeedsRTree( input_layer ) ) then
       allocate( input_layer%tree%tree_nodes( CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY ) ) )
       call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
    end if
    if( NeedsPNum( input_layer ) ) then
       call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
            input_layer%tree%root_index, overlap_area, overlap_perimeter)
       if( overlap_area /= 0 ) then
          !> wipe HEALING state
          input_layer%layerState = iand( input_layer%layerState, NOT(LAYER_STATE_PNUM ) )
       else
          input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_HEAL )
       end if
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_PNUM )
    end if
    call PerformPolygonUnion( input_layer )
  end subroutine PreprocessLayerByPolygon

  subroutine PreprocessLayer( input_layer )
    implicit none
    type(Layer), intent(inout) :: input_layer
    character(len=255) :: val
    integer            :: env_len, env_status
    call get_environment_variable( 'MAGPARSER_CONTROL_PREPROCESS_LAYER_BY_POLYGON', length=env_len, status=env_status )
    if( env_status == 0 ) then !> value is not set
       call PreprocessLayerByPolygon( input_layer )
    elseif( env_status == 1 ) then !> value is not set
       call PreprocessLayerSL( input_layer )
    end if
  end subroutine PreprocessLayer

  subroutine CalculateOR( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout)   :: output_layer
    integer             :: alloc_status
    !> we can create the RTree for A and B if needed
    !call PreprocessLayer( input_layer_A )
    !call PreprocessLayer( input_layer_B )
    if( allocated( output_layer%layer_boxes ) ) then
       deallocate( output_layer%layer_boxes ) !> since we are now doing inout, 
    end if
    allocate( output_layer%layer_boxes( size(input_layer_A%layer_boxes) + size( input_layer_B%layer_boxes) ), stat=alloc_status )
    if( alloc_status /= 0 ) then
       error stop "ALLOCATION FAILED: "
    end if
    output_layer%layer_boxes(1:input_layer_A%n_used) = input_layer_A%layer_boxes
    output_layer%layer_boxes(1+input_layer_A%n_used:) = input_layer_B%layer_boxes
    output_layer%n_used = input_layer_A%n_used + input_layer_B%n_used
    call PreprocessLayer( output_layer )
  end subroutine CalculateOR

  !> utility subroutine, should move to more common place
  subroutine push_box( xbox, th_layer )
    type(Box), intent(in) :: xbox
    type(Layer), intent(inout) :: th_layer
    integer :: alloc_status
    integer(kind=int64), parameter :: K_GROWTH_FACTOR = 2
    if( th_layer%n_used == th_layer%n_alloc ) then
       block
         type(Box), allocatable :: temp(:)
         integer :: i ! Explicitly scoped loop index
         allocate( temp( th_layer%n_alloc*K_GROWTH_FACTOR ), stat=alloc_status )
         if( alloc_status /= 0 ) then
            error stop "Memory allocation failed"
         end if
         th_layer%n_alloc = th_layer%n_alloc*K_GROWTH_FACTOR
         ! Element-by-element copy guarantees no hidden stack allocations
         do i = 1, th_layer%n_used
            temp(i) = th_layer%layer_boxes(i)
         end do
         call move_alloc( from=temp, to=th_layer%layer_boxes)
       end block
    end if
    th_layer%n_used = th_layer%n_used + 1       
    th_layer%layer_boxes( th_layer%n_used ) = xbox
  end subroutine push_box

  subroutine CalculateAND( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout)   :: output_layer
    integer(kind=int64) :: num_boxesA, num_boxesB, num_boxesC
    integer(kind=int64) :: leafboxes(K_MAX_SEARCH_LEAVES) ! better choose a large number
    integer(kind=int64) :: number_leaves
    integer(kind=int64) :: i, j, k
    type(Layer), allocatable :: buffers(:)
    type(Box) :: tempBox
    integer :: nthreads, tid, alloc_status
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096

    !> we can create the RTree for A and B if needed
    !> since the SkipList is not re-entrant, we cannot do parallel here (yet)
    !$komp parallel
    !$komp single
    !$komp task
    call PreprocessLayer( input_layer_A )
    !$komp end task
    !$komp task    
    call PreprocessLayer( input_layer_B )
    !$komp end task
    !$komp taskwait
    !$komp end single
    !$komp end parallel    
    nthreads = omp_get_max_threads()
    allocate(buffers(nthreads))
    do i=1,nthreads
       !> we may still need some setup
       allocate(buffers(i)%layer_boxes(K_INIT_BOX_COUNT))
       buffers(i)%n_alloc = K_INIT_BOX_COUNT
    end do
    num_boxesA = input_layer_A%n_used
    num_boxesB = input_layer_B%n_used    
    !write(*,*) 'DBG: ', num_boxesA, ' ', num_boxesB, ' RTB ', size(input_layer_B%tree%tree_nodes)
    !> we may have to do schedule dynamic:     !$omp do schedule(dynamic)
    !$omp parallel do private(leafboxes, number_leaves, i, j, k, tid, tempBox)
    over_all_boxes: do i=1,num_boxesA
       number_leaves = 0
       leafboxes = 0
       tid = omp_get_thread_num()+1
       call SearchTree( input_layer_B%tree%tree_nodes, input_layer_B%tree%root_index, &
            input_layer_A%layer_boxes(i), leafboxes, number_leaves )
       !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
       if( number_leaves > 0 ) then
          !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
          outer: do j=1,number_leaves
             over_leaves: do k=leafboxes(j),min(leafboxes(j)+K_LEAF_CAPACITY-1, num_boxesB)
                if( box_interact( input_layer_A%layer_boxes(i), input_layer_B%layer_boxes(k)) ) then
                   tempBox = input_layer_A%layer_boxes(i) * input_layer_B%layer_boxes(k)
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
    !write(*,*) 'OUTPUT_COUNT =', num_boxesC
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
    call PreprocessLayer( output_layer )
  end subroutine CalculateAND

  !> Multiple algorithms for A NOT B
  subroutine CalculateNOT( input_layer_A, input_layer_B, output_layer )
    type(Layer), intent(inout) :: input_layer_A, input_layer_B
    type(Layer), intent(inout)   :: output_layer
    integer(kind=int64) :: num_boxesA, num_boxesB, num_boxesC
    type(XYTracker), allocatable :: trackers(:)
    integer(kind=int64) :: number_leaves
    integer(kind=int64) :: i, j, k
    type(Box) :: tempBox, bboxA, bboxB
    type(Layer) :: temp_layer
    integer :: nthreads, tid, alloc_status
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096

    !> we can create the RTree for A and B if needed
    !> since the SkipList is not re-entrant, we cannot do parallel here (yet)
    !$komp parallel
    !$komp single
    !$komp task
    call PreprocessLayer( input_layer_A )
    !$komp end task
    !$komp task    
    call PreprocessLayer( input_layer_B )
    !$komp end task
    !$komp taskwait
    !$komp end single
    !$komp end parallel

    !> Step 1
    write(*,*) 'Executing Scanline Fracturing of COMPLEMENT LAYER with Skip List...'
    bboxA = mbr_of_array( input_layer_A%layer_boxes, input_layer_A%n_used )  
    call generate_trackers( input_layer_B%layer_boxes, bboxA, trackers ) !> this does CW ordering of inner contours
    call scanline_fracture(trackers, temp_layer%layer_boxes)
    temp_layer%layerState = ior( temp_layer%layerState, LAYER_STATE_HEAL )
    temp_layer%n_used = size( temp_layer%layer_boxes)    
    call CalculateAND( input_layer_A, temp_layer, output_layer )
  end subroutine CalculateNOT

  subroutine DeleteLayer( input_layer_A )
    type(Layer), intent(inout) :: input_layer_A
    if( allocated( input_layer_A%layer_boxes ) ) deallocate( input_layer_A%layer_boxes )
    if( allocated( input_layer_A%fileName ) ) deallocate( input_layer_A%fileName )
    input_layer_A%n_used = 0
    input_layer_A%n_alloc = 0
    input_layer_A%layerState = LAYER_STATE_EVERYTHING
  end subroutine DeleteLayer
  
  subroutine CopyLayer( input_layer_A, output_layer )
    type(Layer), intent(inout) :: input_layer_A
    type(Layer), intent(inout) :: output_layer
    integer(kind=int64) :: i
    if( input_layer_A%n_used == 0 ) then
       call DeleteLayer( output_layer )
    end if
    allocate( output_layer%layer_boxes( input_layer_A%n_used ) )
    do i=1,input_layer_A%n_used
       output_layer%layer_boxes(i) = input_layer_A%layer_boxes(i)
    end do
    
  end subroutine CopyLayer
  
  subroutine CalculateGROWLayer( input_layer_A, grow_value, output_layer )
    type(Layer), intent(inout) :: input_layer_A
    type(Layer), intent(inout) :: output_layer
    type(Layer)                :: temp_layer
    integer(kind=K_COORDINATE_KIND), intent(in) :: grow_value
    integer(kind=int64) :: num_boxesA, output_box_count
    type(XYTracker), allocatable :: trackers(:)
    integer(kind=int64) :: i, j, k
    type(Box) :: tempBox, bboxA, bboxB
    integer :: nthreads, tid, alloc_status
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096

    call CopyLayer( input_layer_A, output_layer )
    call box_grow(output_layer%layer_boxes, grow_value, grow_value )
    call PreprocessLayer( output_layer )
  end subroutine CalculateGROWLayer

  subroutine CalculateSHRINKLayer( input_layer_A, shrink_value, output_layer )
    type(Layer), intent(inout) :: input_layer_A
    type(Layer), intent(inout)   :: output_layer
    integer(kind=K_COORDINATE_KIND), intent(in) :: shrink_value
    integer(kind=int64) :: num_boxesA, output_box_count
    type(XYTracker), allocatable :: trackers(:)
    integer(kind=int64) :: i, j, k
    type(Box) :: tempBox, bboxA, bboxB
    type(Layer) :: temp_layer
    integer :: nthreads, tid, alloc_status
    integer(kind=int64), parameter :: K_INIT_BOX_COUNT = 4096

    !> we can create the RTree for A and B if needed
    !> since the SkipList is not re-entrant, we cannot do parallel here (yet)
    !$komp parallel
    !$komp single
    !$komp task
    call PreprocessLayer( input_layer_A )
    !$komp end task
    !$komp taskwait
    !$komp end single
    !$komp end parallel

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

  
end module DesignModule

