! File   : test_contour.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: Given Magic VLSI boxes, construct exterior/interior cycles


program testContour
  use iso_fortran_env, only: int32, int64, real64
  use GeometryModule
  use HDFDataModule
  use RTreeBuilder
  use DataStructuresModule
  use PNumMergeModule
  use DesignModule
  use ContourExtractionModule
  implicit none
  integer(kind=int64), parameter :: K_LEAF_CAPACITY = 16
  type(Layer),target :: input_layer
  type(Box), pointer :: boxes(:)
  type(Box) :: tempBox
  real(kind=real64) :: overlap_area, overlap_perimeter
  integer(int64), allocatable :: permutation(:)
  type(BucketBoundary), allocatable :: segments(:)
  integer, parameter :: K_POLYGON_INIT_BOX_COUNT = 64
  integer(kind=int64) :: i,j,n, num_roots, num_rects, polygon_number, box_count, updated_box_count
  type(Box), allocatable :: current_polygon_boxes(:)
  integer(int64) :: starting_segment
  type(Polygon), allocatable :: contours(:)
  integer             :: num_contours
  
  call LoadFromHDF("a.h5", input_layer%layer_boxes )
  input_layer%n_used = size( input_layer%layer_boxes )
  boxes => input_layer%layer_boxes
  write(*,*) 'Loaded ', size( boxes ), ' rects.'
  do i=1,size(boxes)
     !write(*,*) boxes(i)
     if( .not. boxes(i)%is_valid() ) error stop "INVALID BOX in input"
  end do
  allocate( input_layer%tree%tree_nodes( CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY ) ) )
  call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
  input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
  call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
  input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )

  call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
       input_layer%tree%root_index, overlap_area, overlap_perimeter)
  input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_PNUM )

  call input_layer%pnumtable%expand_roots()
  
  num_roots = input_layer%pnumtable%count_roots()
  num_rects = count(input_layer%pnumtable%arr == 0)
  if( num_rects == input_layer%n_used ) then
     if( num_roots /= 0 ) error stop "INCONSISTENT ROOT/RECT count."
  end if
  !write(*,*) '|Roots| = ', num_roots, '|Rects| = ', num_rects
  call GetSortPermutation( input_layer%pnumtable%arr, permutation )
  !write(*,*) 'Permutation = ', permutation
  call get_equal_key_segments( input_layer%pnumtable%arr, permutation,  segments )
  !write(*,*) 'Segments = ', segments

  !> Usually the first segment comprises of the rectangles which we dont need to care about
  if( input_layer%pnumtable%arr( permutation( segments(1)%end_idx ) ) == 0 ) then
     !write (*,*) 'Indeed, RECTS ', segments(1)%end_idx
     do i = 1, segments(1)%end_idx
        tempBox = input_layer%layer_boxes( permutation( i ) )
        write(*,*) 'Polygon ', 1, ' : ', tempBox%x1, ' ', tempBox%y1, ' ', tempBox%x2, ' ', tempBox%y1, &
             ' ', tempBox%x2, ' ', tempBox%y2, ' ', tempBox%x1, ' ', tempBox%y2
     end do
     if( input_layer%pnumtable%arr( permutation( segments(1)%start_idx ) ) /= 0 ) error stop "END_IDX=0, but START_IDX /= 0"
     starting_segment = 2
  else
     starting_segment = 1
  end if
  if( num_roots > 0 ) then !> everything is in rects and we have accounted for all of them
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
        !call heal_boxes( current_polygon_boxes, box_count, updated_box_count )
        !write(*,*) 'Polygon healing: ', box_count, ' to ', updated_box_count
        call extract_contours(current_polygon_boxes, box_count, contours, num_contours)
        !write(*,*) 'Extracted ', num_contours, ' contours'
        do j=1,num_contours
           write(*,*) 'Polygon ',j,' : ', contours(j)%pts
        end do
     end do
     deallocate( current_polygon_boxes )     
  end if
  
end program testContour
