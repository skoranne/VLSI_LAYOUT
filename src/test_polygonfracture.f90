! File   : test_polygonfracture.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: simple test harness for polygon fracturing
program test_fracture
  use CommonModule
  use polygon_fracture_mod
  use GeometryModule
  use DesignModule
  use HDFDataModule
  use ContourExtractionModule
  use BoxMergeModule
  use RTreeBuilder
  use DataStructuresModule
  use PNumMergeModule
  
  use iso_fortran_env
  implicit none

  type(Layer),target :: input_layer
  type(Layer),target :: output_layer  
  type(Box), pointer :: boxes(:)
  type(Box) :: tempBox, bbox
  real(kind=real64) :: overlap_area, overlap_perimeter
  integer(kind=int64), allocatable :: permutation(:)
  !type(BucketBoundary), allocatable :: segments(:)
  integer(kind=int64) :: i,j,n, num_roots, num_rects, polygon_number, box_count, updated_box_count, n_trackers
  type(Box), allocatable :: current_polygon_boxes(:)
  integer(kind=int64) :: starting_segment
  type(Polygon), allocatable :: contours(:)
  integer             :: num_contours
  real(kind=real64)   :: layer_area, complement_area
  type(XYTracker), allocatable :: trackers(:)
  print *, "--- Initializing Polygon Fracturing Test ---"
  call LoadKLBin("a.bin",input_layer%layer_boxes)
  boxes => input_layer%layer_boxes
  input_layer%n_used  = size(boxes)
  layer_area = sum( box_area( input_layer%layer_boxes ) )
  bbox = mbr_of_array( boxes, input_layer%n_used )
  call box_grow(bbox,10,10)
  write(*,*) 'Num: ', input_layer%n_used, ' area = ', layer_area
  write(*,*) 'MBR: ', bbox, ' BOX_AREA: ', box_area(bbox)
  write(*,*) 'Expected AREA of complement = ', box_area(bbox) - layer_area
  call generate_trackers( boxes, bbox, trackers )

  !print *, "Sorting tokens by X/Y..."
  call sort_trackers(trackers)
  n_trackers = size(trackers)
  !do i = 1, n_trackers
  !   !write(*,'(4(A,I))') 'Tracker', i, ' -> X:', trackers(i)%X, ' Y:', trackers(i)%Y, ' PolyID:', trackers(i)%polygonNumber
  !end do

  print *, "Executing Scanline Fracturing with Skip List..."
  call scanline_fracture(trackers, output_layer%layer_boxes)
  output_layer%n_used = size( output_layer%layer_boxes)
  print *, "Fracturing algorithm finished without memory leaks."
  !do i = 1, n_trackers
  !   !write(*,'(4(A,I))') 'Tracker', i, ' -> X:', trackers(i)%X, ' Y:', trackers(i)%Y, ' PolyID:', trackers(i)%polygonNumber
  !end do
  call WriteKLBin("a_out.bin", output_layer%layer_boxes)
  call merge_boxes_using_scanline( output_layer%layer_boxes )
  allocate( input_layer%tree%tree_nodes( CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY ) ) )  
  call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
  input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
  call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
  input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )

  call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
       input_layer%tree%root_index, overlap_area, overlap_perimeter)
  write(*,*) 'OVLP AREA by pnum =', overlap_area
  input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_PNUM )

  call WriteKLBin("a_out_merged.bin", output_layer%layer_boxes)
  complement_area = sum( box_area( output_layer%layer_boxes ) )
  if( complement_area /= (box_area(bbox) - layer_area)) then
     write(*,*) 'Expected AREA of complement = ', box_area(bbox) - layer_area, ' while ', complement_area
     error stop "INCORRECT FRACTURING detected."
  end if
    
end program test_fracture

