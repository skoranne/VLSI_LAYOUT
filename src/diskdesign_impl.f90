! File   : diskdesign_impl.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Save/Restore SNAPPY compressed data

submodule (DesignModule) DiskDesignImplModule
  use iso_fortran_env, only: int64, real64
  use CommonModule
  use GeometryModule
  use MortonSortOMT
  use RTreeBuilderGPU
  use RTreeBuilder
  use GPUMergeModule
  use PNumMergeModule
  use KLDataModule
  use BoostPolygonAPIModule
  use SerializationModule
  implicit none

contains
  module subroutine BuildTree( input_layer )
    type(Layer), intent(inout)  :: input_layer
    integer(kind=int64)         :: total_nodes
    if( .not. NeedsRTree( input_layer ) ) return
    total_nodes = CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY )
    call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
    allocate( input_layer%tree%tree_nodes( total_nodes ) )
    call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index )
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
  end subroutine BuildTree
    
  module subroutine SaveLayerToSnap( input_layer, snap_filename )
    type(Layer), intent(inout) :: input_layer
    character(*), intent(in)   :: snap_filename
    type(CompressedStream)     :: snappy_stream
    if( NeedsSorting( input_layer ) ) call omt_pack( input_layer%layer_boxes, K_LEAF_CAPACITY )
    if( NeedsRTree( input_layer ) ) then
       call BuildTree( input_layer )
    end if
    call CompressBoxesToSnappyStream( input_layer%layer_boxes, snappy_stream)    
    call SaveCompressedStreamToDisk( snap_filename, snappy_stream )
  end subroutine SaveLayerToSnap

  module subroutine RestoreSnapToLayer( input_layer, snap_filename )
    type(Layer), intent(inout) :: input_layer
    character(*), intent(in)   :: snap_filename
    type(CompressedStream)     :: snappy_stream
    call RestoreCompressedStreamFromDisk( snap_filename, snappy_stream )
    call DecompressSnappyStreamToBoxes( snappy_stream, input_layer%layer_boxes )
    input_layer%n_used = size( input_layer%layer_boxes )
    if( input_layer%n_used /= snappy_stream%total_boxes) then
       write(*,*) 'ERROR: SNAP restoration failed for file: ', snap_filename
       error stop "ERROR: SNAP RESTORATION FAILED"
    end if
    input_layer%layerState = LAYER_STATE_SORT
    call BuildTree( input_layer )
  end subroutine RestoreSnapToLayer

end submodule DiskDesignImplModule

