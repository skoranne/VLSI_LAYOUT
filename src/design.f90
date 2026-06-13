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
  use iso_c_binding
  use iso_fortran_env, only : int32, int64, real64
  implicit none  
  private
  public :: Design, Layer, LAYER_STATE_NONE, LAYER_STATE_HEAL, &
       LAYER_STATE_SORT, LAYER_STATE_PNUM, LAYER_STATE_RTREE, &
       NeedsSorting, NeedsPNum, NeedsHealing, PerformUnion, PerformPolygonUnion, BucketBoundary, &
       get_equal_key_segments, GetSortPermutation, calculate_union_area_by_polygon
  type :: LayerTree
     integer(kind=8) :: root_index
     type(RTreeNode), allocatable :: tree_nodes(:)
  end type LayerTree
  enum, bind(C)                     ! bind(C) makes the values C‑compatible
     enumerator :: LAYER_STATE_NONE  = int(Z'00', kind=c_int)
     enumerator :: LAYER_STATE_HEAL  = int(Z'01', kind=c_int)   ! 0b0001
     enumerator :: LAYER_STATE_SORT  = int(Z'02', kind=c_int)   ! 0b0010
     enumerator :: LAYER_STATE_PNUM  = int(Z'04', kind=c_int)   ! 0b0100
     enumerator :: LAYER_STATE_RTREE = int(Z'08', kind=c_int)   ! 0b1000
  end enum

  type :: Layer
     integer :: lid
     integer(kind=8)        :: n_used   = 0   ! how many slots are filled
     integer(kind=8)        :: n_alloc  = 0   ! current allocation size
     type(Box), allocatable :: layer_boxes(:)
     integer(kind=8)        :: layerState = 0 ! HEAL, SORT, PNUM, RTREE
     type(LayerTree)        :: tree
     type(UnionFind(int64)) :: pnumtable
     real(kind=real64)      :: area, perimeter
  end type Layer
  type :: Design
     type(Layer), allocatable :: layers(:)
     type(hash_type) :: ht
     character(len=1024), dimension(:), allocatable :: layerNames(:)
     type(Box)              :: DESIGN_EXTENT
  end type Design
  
  ! A clean, modern derived type to hold our bucket boundaries
  type :: BucketBoundary
     integer(int64) :: start_idx
     integer(int64) :: end_idx
  end type BucketBoundary

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
    integer(kind=int64) :: i,j,n, num_roots, num_rects, polygon_number, box_count
    integer(kind=int64) :: updated_box_count, final_count, final_capacity
    type(Box) :: tempBox
    type(Box), allocatable :: current_polygon_boxes(:)
    type(Box), allocatable :: final_boxes(:)
    integer(int64), allocatable :: permutation(:)
    type(BucketBoundary), allocatable :: segments(:)
    integer(int64) :: starting_segment
    real(kind=real64) :: overlap_area, overlap_perimeter
    if( input_layer%n_used == 0 ) return
    if( NeedsPNum( input_layer ) ) then
       error stop "PLEASE RUN PNUM before."
    end if
    !>>> PLEASE UNCOMMENT <<<
    if( .not. NeedsHealing( input_layer ) ) return !bravo
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
    final_capacity = num_rects + 4*(input_layer%n_used-num_rects)
    !>>> Trying to debug the large memory <<<
    allocate( final_boxes( final_capacity ) )
    final_count = 0
    if( input_layer%pnumtable%arr( permutation( segments(1)%end_idx ) ) == 0 ) then
       !write (*,*) 'Indeed, RECTS ', segments(1)%end_idx
       final_boxes(1:segments(1)%end_idx) = input_layer%layer_boxes( &
            permutation( segments( 1 )%start_idx:permutation( segments( 1 )%end_idx ) ) )
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
            permutation( segments( i )%start_idx:permutation( segments( i )%end_idx ) ) )
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
    call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
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
    real(kind=real64) :: retval_area
    integer, parameter :: K_POLYGON_INIT_BOX_COUNT = 64
    integer(kind=int64) :: i,j,n, num_roots, num_rects, polygon_number, box_count
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
       retval_area = retval_area + box_area( input_layer%layer_boxes(i) )
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
          retval_area = retval_area + box_area( input_layer%layer_boxes( permutation( i ) ) )
       end do
       if( input_layer%pnumtable%arr( permutation( segments(1)%start_idx ) ) /= 0 ) error stop "END_IDX=0, but START_IDX /= 0"
       starting_segment = 2
    else
       starting_segment = 1
    end if
    if( num_roots == 0 ) return !> everything is in rects and we have accounted for all of them
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
       !write(*,'(2(A,I0),A)') 'Processing polygon number: ', polygon_number, ' with ', box_count, ' rects.'
       if( box_count > size( current_polygon_boxes ) ) then
          deallocate( current_polygon_boxes )
          allocate( current_polygon_boxes( 2*box_count ) )
       end if
       current_polygon_boxes(1:box_count) = input_layer%layer_boxes( &
            permutation( segments( i )%start_idx:permutation( segments( i )%end_idx ) ) )
       retval_area = retval_area + calculate_union_area( current_polygon_boxes )
    end do
    deallocate( current_polygon_boxes )
  end function calculate_union_area_by_polygon

end module DesignModule

