! File   : test_polygonfracture.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: simple test harness for polygon fracturing
program test_fracture
  use CommonModule
  use PolygonFractureModule
  use GeometryModule
  use DesignModule
  use KLDataModule
  use ContourExtractionModule
  !use BoxMergeModule
  use RTreeBuilder
  use DataStructuresModule
  use PNumMergeModule
  use SystemInformationModule
  use ASCIIPlotModule
  use BoostPolygonAPIModule
  use iso_fortran_env
  implicit none

  type(Layer),target :: input_layer
  type(Layer),target :: input_layerB
  type(Layer),target :: output_layer
  type(Layer),target :: temp_layer    
  type(Box), pointer :: boxes(:)
  type(Box) :: tempBox, bbox, bboxB
  real(kind=real64) :: overlap_area, overlap_perimeter
  integer(kind=int64), allocatable :: permutation(:)
  !type(BucketBoundary), allocatable :: segments(:)
  integer(kind=int64) :: i,j,n, num_roots, num_rects, polygon_number, box_count, updated_box_count, n_trackers
  type(Box), allocatable :: current_polygon_boxes(:)
  integer(kind=int64) :: starting_segment
  type(Polygon), allocatable :: contours(:)
  type(BucketBoundary), allocatable :: segments(:)
  integer, parameter :: K_POLYGON_INIT_BOX_COUNT = 64
  integer(kind=int64):: num_contours

  real(kind=real64)   :: layer_area, complement_area, sl_union_area, psl_union_area
  type(XYTracker), allocatable :: trackers(:)
  character(len=256)            :: filenameA, filenameB      
  character(len=256)            :: outFileName   
  integer                       :: control_parameter
  integer                       :: narg          ! # of arguments on the command line
  integer                       :: iostat        ! I/O status for reading the integer argument
  character(len=256)            :: arg_string    ! temporary buffer for the 2nd argument
  integer(kind=K_COORDINATE_KIND), parameter :: K_BBOX_GROW_X = 1000, K_BBOX_GROW_Y = 1000
  !-----------------------------------------------------------------
  !  Get the number of arguments supplied on the command line
  !-----------------------------------------------------------------
  call InitSystem()
  narg = command_argument_count()
  !write(*,*) 'NARG = ', narg
  select case (narg)
  case (0)                     ! No arguments → use defaults
     write(*,*) '0.  Executing Token Tracking Fracture/Scanline Fracturing with Skip List...'
     write(*,*) '1.  BBOX GROW by ', K_BBOX_GROW_X, ' ', K_BBOX_GROW_Y
     write(*,*) '2.  Run HORIZONTAL Merge and save result'
     write(*,*) '3.  Check input and save sorted data'
     write(*,*) '4.  Convert input to HDF5'
     write(*,*) '5.  Print Detailed Information: '     
     write(*,*) '6.  Convert input to COMPLEMENT and run Fracture/Contour/Fracture'
     write(*,*) '7.  Fracture/Contour/Fracture'
     write(*,*) '8.  Run HEAL on whole layer'
     write(*,*) '9.  Run COMPLEMENT and analysis'
     write(*,*) '10. Run op = A OR  B'
     write(*,*) '11. Run op = A AND B'               
     write(*,*) '12. Run op =   AND A'
     write(*,*) '13. Run op = A NOT A'     
     write(*,*) '14. Run op = A XOR B'               
     write(*,*) '15. Run op = COMPLEMENT A'
     write(*,*) '16. Run op = SIZE A BY '
     write(*,*) '17. Run op = BOOST POLYGON MERGE A'
     stop "./CONVERT.exe <input-filenameA> <input-filenameB> <output-file> control"     
  case (2)
     error stop "./CONVERT.exe <input-filename> <output-filename> control"

  case (4)                     ! Two arguments: <fileA> <fileB> <outFile> <control>
     ! ---- first argument: file name ---------------------------------
     call get_command_argument(1, filenameA, status=iostat)   ! allocates automatically
     if (iostat /= 0) then
        write (*,*) "ERROR: 1st argument must be a filename."
        stop 2
     end if
     write (*,*) 'Reading 1st filename: ', trim(filenameA)
     call get_command_argument(2, filenameB, status=iostat)   ! allocates automatically
     if (iostat /= 0) then
        write (*,*) "ERROR: 2st argument must be a filename."
        stop 2
     end if
     write (*,*) 'Reading 2nd filename: ', trim(filenameB)
     call get_command_argument(3, outFileName, status=iostat)   ! allocates automatically
     if (iostat /= 0) then
        write (*,*) "ERROR: 3rd argument must be a filename."
        stop 2
     end if
     ! ---- third argument: integer (max number of layers) ----------
     call get_command_argument(4, arg_string, status=iostat)
     if (iostat /= 0) then
        write (*,*) "ERROR: 4th argument must be an integer."
        stop 2
     end if
     read (arg_string, *, iostat=iostat) control_parameter
     if (iostat /= 0 .or. control_parameter < 0) then
        write (*,*) "ERROR: CONTROL must be a non‑negative integer."
        stop 3
     end if
     ! C equivalent: sprintf(outputName, "%s%s", trim(fileName), "_output");
     ! Write directly into the string variable using string format '(A, A)'
     ! write(outputName, '(A, A)') trim(fileName), "_output"
     ! print *, trim(outputName)
  case (5)
     write(*,*) '5. Print Detailed Information: '

  case default                ! Anything else → print usage and quit
     error stop "./PROG.exe <filename> <MAX_LAYER>"
     stop 1
  end select

  print *, "--- Initializing Polygon Fracturing Test ---"
  call LoadKLBin(filenameA,input_layer%layer_boxes)
  call LoadKLBin(filenameB,input_layerB%layer_boxes)  
  boxes => input_layer%layer_boxes
  input_layer%n_used  = size(boxes)
  input_layerB%n_used = size( input_layerB%layer_boxes )
  bbox = mbr_of_array( boxes, input_layer%n_used )
  bboxB= mbr_of_array( input_layerB%layer_boxes, input_layerB%n_used )  
  select case(control_parameter)
  case (0)
     write(*,*), '0. Executing Token Tracking Fracture/Scanline Fracturing with Skip List...'     
     call box_grow(bbox,K_BBOX_GROW_X,K_BBOX_GROW_Y)
     !> SCANLINE_FRACTURE does not support overlapping boxes, so we need to accomodate that till thats fixed
     call heal_boxes( input_layer%n_used, input_layer%layer_boxes, updated_box_count)
     current_polygon_boxes = input_layer%layer_boxes(1:updated_box_count)
     input_layer%layer_boxes = current_polygon_boxes
     write(*,*) 'Polygon healing: ', input_layer%n_used, ' to ', updated_box_count
     input_layer%n_used = updated_box_count
     call WriteKLBin( "merged.bin", input_layer%layer_boxes, input_layer%n_used )
     boxes => input_layer%layer_boxes
     call generate_trackers( boxes, bbox, trackers ) !> this does CW ordering of inner contours
     !call sort_trackers(trackers) !> this is done inside as well
     n_trackers = size(trackers)

     !print *, "Sorting tokens by X/Y..."
     !do i = 1, n_trackers
     !   write(*,'(4(A,I))') 'Tracker', i, ' -> X:', trackers(i)%X, ' Y:', trackers(i)%Y, ' PolyID:', trackers(i)%polygonNumber
     !end do     
     write(*,*) 'Executing Scanline Fracturing of COMPLEMENT LAYER with Skip List...'

     call scanline_fracture(trackers, output_layer%layer_boxes)

     output_layer%n_used = size( output_layer%layer_boxes)
     print *, "Fracturing algorithm finished without memory leaks."
     n_trackers = size(trackers)     
     !do i = 1, n_trackers
     !   write(*,'(4(A,I))') 'Tracker', i, ' -> X:', trackers(i)%X, ' Y:', trackers(i)%Y, ' PolyID:', trackers(i)%polygonNumber
     !end do
     call WriteKLBin(outFileName, output_layer%layer_boxes, output_layer%n_used)
     layer_area = sum( box_area_vectorized( input_layer%layer_boxes ) )
     !sl_union_area = calculate_union_area_sl(boxes)  !< this is too slow
     sl_union_area = calculate_union_area_fast(boxes) !< uses SegmentTreeModule
     if( layer_area /= sl_union_area ) then
        write(*,*) 'Maybe there is OVLP on input: ', layer_area, ' |SL| = ', sl_union_area
     end if
     complement_area = sum( box_area_vectorized( output_layer%layer_boxes ) )
     if( complement_area /= (box_area(bbox) - layer_area)) then
        layer_area = sum( box_area_vectorized( input_layer%layer_boxes ) )
        write(*,'(A,F25.8,A,F25.8)') 'Expected AREA of complement = ', box_area(bbox) - layer_area, ' while ', complement_area        
        error stop "INCORRECT FRACTURING detected."
     else
        write(*,'(A,F25.8,A,F25.8)') 'Expected AREA of complement = ', box_area(bbox) - layer_area, ' and ', complement_area
     end if
     !call extract_contours(input_layer%layer_boxes, box_count, contours, num_contours)
     !write(*,*) 'Extracted ', num_contours, ' contours'
     stop
  case (1)
     write(*,*), '1. BBOX GROW by ', K_BBOX_GROW_X, ' ', K_BBOX_GROW_Y     
     call box_grow(bbox,K_BBOX_GROW_X,K_BBOX_GROW_Y)
     allocate(output_layer%layer_boxes(1))
     output_layer%layer_boxes(1) = bbox
     output_layer%n_used = 1
     call WriteKLBin(outFileName, output_layer%layer_boxes, output_layer%n_used)
     stop
  case (2)
     write(*,*) 'Running HORIZONTAL based merge: '
     output_layer%layer_boxes = input_layer%layer_boxes
     !call merge_boxes_using_scanline( output_layer%layer_boxes )
     output_layer%n_used = size( output_layer%layer_boxes )
     call WriteKLBin(outFileName, output_layer%layer_boxes, output_layer%n_used)
     !call saveToHDF( outFileName, output_layer%layer_boxes)
     stop
  case (3)
     write(*,*), '3. Check input and save sorted data'
     allocate( input_layer%tree%tree_nodes( CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY ) ) )  
     call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
     !call saveToHDF(outFileName, input_layer%layer_boxes)
     input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
     call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
     input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
     call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
          input_layer%tree%root_index, overlap_area, overlap_perimeter)
     input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_PNUM )     
     psl_union_area = calculate_union_area_by_polygon( input_layer )
     layer_area = sum( box_area_vectorized( input_layer%layer_boxes ) )
     !sl_union_area = calculate_union_area_sl(boxes)  !< this is too slow
     sl_union_area = calculate_union_area_fast(boxes) !< uses SegmentTreeModule     
     write(*,*) 'OVLP AREA by pnum =', overlap_area, ' |AL| = ', layer_area, ' |SL| = ', sl_union_area, ' |PSL| = ', psl_union_area
     num_roots = input_layer%pnumtable%count_roots()
     num_rects = count(input_layer%pnumtable%arr == 0)
     call input_layer%pnumtable%expand_roots() !< ALWAYS REMEMBER after this there are no RECTS
     if( num_rects == input_layer%n_used ) then
        if( num_roots /= 0 ) error stop "INCONSISTENT ROOT/RECT count."
     end if
     write(*,'(A,I12,A,I12)') '|Roots| = ', num_roots, ' |Rects| = ', num_rects
     call GetSortPermutation( input_layer%pnumtable%arr, permutation )
     !write(*,*) 'Permutation = ', permutation
     call get_equal_key_segments( input_layer%pnumtable%arr, permutation,  segments )
     !write(*,*) 'Segments = ', segments
     call WriteKLBin(outFileName, input_layer%layer_boxes, input_layer%n_used )     
     stop
  case (4)
     !call saveToHDF( outFileName, input_layer%layer_boxes )
     stop
  case (5)
     write(*,*) '5. Print Detailed Information: '
     write(*,'(A5,I12,A,F18.8)') 'Num: ', input_layer%n_used, ' area = ', layer_area
     write(*,'(A,4I10,A,F18.8)') 'MBR: ', bbox, ' BOX_AREA: ', box_area(bbox)
     do i = 1, min(input_layer%n_used,10)
        write(*,'(A,I8,A,4I8)') 'Box ', i, ': ', boxes(i)%x1, boxes(i)%y1, boxes(i)%x2, boxes(i)%y2
     end do
     call ascii_plot_boxes(input_layer%layer_boxes)

  case (6,7)
     if( control_parameter == 6 ) then
        write(*,*) '6. Convert input to COMPLEMENT and run Fracture/Contour/Fracture'
        call box_grow(bbox,K_BBOX_GROW_X,K_BBOX_GROW_Y)
        call generate_trackers( boxes, bbox, trackers ) !> this does CW ordering of inner contours
        call sort_trackers(trackers)
        n_trackers = size(trackers)
        write(*,*) 'Executing Scanline Fracturing of COMPLEMENT LAYER with Skip List...'
        call scanline_fracture(trackers, output_layer%layer_boxes)
        output_layer%n_used = size( output_layer%layer_boxes)
        !> swap input and output
        temp_layer = input_layer
        input_layer = output_layer
        output_layer = temp_layer
     else if( control_parameter == 7 ) then
        write(*,*) '7. Fracture/Contour/Fracture'
     else
        error stop "NOT POSSIBLE"
     end if
     if( size(input_layer%layer_boxes) == 0 ) error stop "INPUT_LAYER size has become 0"
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
     write(*,*) '|Roots| = ', num_roots, '|Rects| = ', num_rects
     call GetSortPermutation( input_layer%pnumtable%arr, permutation )
     !write(*,*) 'Permutation = ', permutation
     call get_equal_key_segments( input_layer%pnumtable%arr, permutation,  segments )
     !write(*,*) 'Segments = ', segments

     !> Usually the first segment comprises of the rectangles which we dont need to care about
     if( input_layer%pnumtable%arr( permutation( segments(1)%end_idx ) ) == 0 ) then
        !write (*,*) 'Indeed, RECTS ', segments(1)%end_idx
        do i = 1, segments(1)%end_idx
           tempBox = input_layer%layer_boxes( permutation( i ) )
           !write(*,*) 'Polygon ', 1, ' : ', tempBox%x1, ' ', tempBox%y1, ' ', tempBox%x2, ' ', tempBox%y1, &
           !     ' ', tempBox%x2, ' ', tempBox%y2, ' ', tempBox%x1, ' ', tempBox%y2
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
                permutation( segments( i )%start_idx:segments( i )%end_idx ) )
           !call heal_boxes( box_count, current_polygon_boxes, updated_box_count )
           !write(*,*) 'Polygon healing: ', box_count, ' to ', updated_box_count
           call extract_contours(current_polygon_boxes, box_count, contours, num_contours)
           write(*,*) 'Extracted ', num_contours, ' contours'
           do j=1,num_contours
              write(*,*) 'Polygon ',j,' : ', contours(j)%pts
           end do
        end do
        deallocate( current_polygon_boxes )     
     end if
     stop
  case (8)
     write(*,*)  '8. Run HEAL on whole layer'
     output_layer%layer_boxes = input_layer%layer_boxes
     output_layer%n_used = size( output_layer%layer_boxes )     
     call heal_boxes( output_layer%n_used, output_layer%layer_boxes, output_layer%n_used )
     output_layer%n_used = size( output_layer%layer_boxes )
     call WriteKLBin(outFileName, output_layer%layer_boxes, output_layer%n_used)
     stop
  case (9)
     write(*,*), '9. Executing Token Tracking Fracture/Scanline Fracturing with Skip List...'     
     !> SCANLINE_FRACTURE does not support overlapping boxes, so we need to accomodate that till thats fixed
     !call heal_boxes( input_layer%n_used, input_layer%layer_boxes, updated_box_count)
     bbox = input_layer%layer_boxes(1) !> this is the convention
     current_polygon_boxes = input_layer%layer_boxes(2:size(input_layer%layer_boxes))
     input_layer%layer_boxes = current_polygon_boxes
     input_layer%n_used = input_layer%n_used-1 !> one was the bbox
     boxes => input_layer%layer_boxes
     call generate_trackers( boxes, bbox, trackers ) !> this does CW ordering of inner contours
     call sort_trackers(trackers) !> this is done inside as well
     n_trackers = size(trackers)

     print *, "Sorting tokens by X/Y..."
     do i = 1, n_trackers
        write(*,'(4(A,I))') 'Tracker', i, ' -> X:', trackers(i)%X, ' Y:', trackers(i)%Y, ' PolyID:', trackers(i)%polygonNumber
     end do
     write(*,*) 'Executing Scanline Fracturing of COMPLEMENT LAYER with Skip List...'
     call scanline_fracture(trackers, output_layer%layer_boxes)
     output_layer%n_used = size( output_layer%layer_boxes)
     print *, "Fracturing algorithm finished without memory leaks."
     n_trackers = size(trackers)     
     !do i = 1, n_trackers
     !   write(*,'(4(A,I))') 'Tracker', i, ' -> X:', trackers(i)%X, ' Y:', trackers(i)%Y, ' PolyID:', trackers(i)%polygonNumber
     !end do
     call WriteKLBin(outFileName, output_layer%layer_boxes, output_layer%n_used)
     layer_area = sum( box_area_vectorized( input_layer%layer_boxes ) )
     sl_union_area = calculate_union_area_fast(boxes) !< uses SegmentTreeModule
     if( layer_area /= sl_union_area ) then
        write(*,*) 'Maybe there is OVLP on input: ', layer_area, ' |SL| = ', sl_union_area
     end if
     complement_area = sum( box_area_vectorized( output_layer%layer_boxes ) )
     if( complement_area /= (box_area(bbox) - layer_area)) then
        layer_area = sum( box_area_vectorized( input_layer%layer_boxes ) )
        write(*,'(A,F25.8,A,F25.8)') 'Expected AREA of complement = ', box_area(bbox) - layer_area, ' while ', complement_area        
        !error stop "INCORRECT FRACTURING detected."
     else
        write(*,'(A,F25.8,A,F25.8)') 'Expected AREA of complement = ', box_area(bbox) - layer_area, ' and ', complement_area
     end if
     stop
  case(10)
     call StartMarkTime(" OR ")
     call CalculateOR( input_layer, input_layerB, output_layer )
     call StopMarkTime(" OR ")     
     call WriteKLBin(outFileName, output_layer%layer_boxes, output_layer%n_used)
     stop     
  case(11)
     call StartMarkTime(" AND ")     
     call CalculateAND( input_layer, input_layerB, output_layer )
     call StopMarkTime(" AND ")          
     call WriteKLBin(outFileName, output_layer%layer_boxes, output_layer%n_used)
     stop
  case (17)
     write(*,*)  '17. Run Boost Polygon on whole layer'
     allocate( output_layer%layer_boxes( input_layer%n_used*2 ) )
     !call PerformBoostPolygonMerge( input_layer%layer_boxes, input_layer%n_used, output_layer%layer_boxes, output_layer%n_used )
     call MergeBoxesUsingBoostPolygon( input_layer%layer_boxes, output_layer%layer_boxes )
     output_layer%n_used = size( output_layer%layer_boxes )
     write(*,*) 'Boost Polygon merged: ', input_layer%n_used, ' to ', output_layer%n_used
     call WriteKLBin(outFileName, output_layer%layer_boxes, output_layer%n_used)
     stop
     
  case default
     write(*,*) 'Print Information: '
     write(*,*) 'Num: ', input_layer%n_used, ' area = ', layer_area
     write(*,*) 'MBR: ', bbox, ' BOX_AREA: ', box_area(bbox)
     stop
  end select


end program test_fracture

