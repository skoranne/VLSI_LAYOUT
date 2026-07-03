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
  use KLDataModule
  use BoxCodecModule
  use Utilities
  implicit none

  !> Disk based OOC (out of code) loading system

contains
  module subroutine BuildTree( input_layer )
    type(Layer), intent(inout)  :: input_layer
    integer(kind=int64)         :: total_nodes, num_squares
    logical                     :: dominated_by_squares
    dominated_by_squares = .false.
    if( .not. NeedsRTree( input_layer ) ) return
    if( NeedsSorting( input_layer ) ) then
       num_squares = count( is_square( input_layer%layer_boxes ) )
       if( num_squares*1.0_real64 / (input_layer%n_used*1.0_real64) > K_SQUARE_DOMINATION_THRESHOLD ) then
          dominated_by_squares = .true.
       end if
       if( dominated_by_squares ) then
          call MortonSort( input_layer%layer_boxes )
       else
          call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
       end if
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
    end if
    total_nodes = CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY )
    allocate( input_layer%tree%tree_nodes( total_nodes ) )
    call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index )
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
  end subroutine BuildTree

  module subroutine SaveLayerToSnap( input_layer, snap_filename, method_to_use )
    type(Layer), intent(inout) :: input_layer
    character(*), intent(in)   :: snap_filename
    integer, intent(in)        :: method_to_use
    type(CompressedStream)     :: snappy_stream
    if( NeedsSorting( input_layer ) ) call omt_pack( input_layer%layer_boxes, K_LEAF_CAPACITY )
    if( NeedsRTree( input_layer ) ) then
       call BuildTree( input_layer )
    end if
    if( method_to_use == COMPRESSION_METHOD_SNAPPY .or. method_to_use == COMPRESSION_METHOD_ZLIB &
         .or. method_to_use == COMPRESSION_METHOD_ZSTD ) then
       !> ok
    else
       write(*,*) 'ERROR: unknown compression method: ', method_to_use, ' requested.'
       error stop
    end if
    call CompressBoxesToStream( input_layer%layer_boxes, snappy_stream, method_to_use )
    call SaveCompressedStreamToDisk( snap_filename, snappy_stream )
  end subroutine SaveLayerToSnap

  module subroutine RestoreSnapToLayer( input_layer, snap_filename )
    class(Layer), intent(inout) :: input_layer
    character(*), intent(in)   :: snap_filename
    type(CompressedStream)     :: snappy_stream
    integer                    :: pos, iunit, ios
    !> for memory based layers we need an optimization that NO-FILE => EMPTY LAYER
    if( temporary_layers > 1 ) then
       open(newunit=iunit, file=snap_filename, status='old', access='stream', form='unformatted', iostat=ios)
       if (ios /= 0) then
          close(iunit)
          call ClearLayer( input_layer )
          return
       end if
       close(iunit)
    end if
    pos = index( snap_filename, ".bin" )
    if( pos /= 0 ) then !> we are going to assume this is a non-compressed binary file
       call LoadKLBin( snap_filename, input_layer%layer_boxes)
       input_layer%n_used  = size(input_layer%layer_boxes)
       call BuildTree( input_layer )
       return
    end if
    call RestoreCompressedStreamFromDisk( snap_filename, snappy_stream )
    call DecompressStreamToBoxes( snappy_stream, input_layer%layer_boxes )
    input_layer%n_used = size( input_layer%layer_boxes )
    if( input_layer%n_used /= snappy_stream%total_boxes) then
       write(*,*) 'ERROR: SNAP restoration failed for file: ', snap_filename
       error stop "ERROR: SNAP RESTORATION FAILED"
    end if
    input_layer%layerState = LAYER_STATE_SORT
    call BuildTree( input_layer )
  end subroutine RestoreSnapToLayer

  module subroutine RestoreSnapToDLayer( input_layer, snap_filename )
    type(Layer), intent(inout) :: input_layer
    character(*), intent(in)   :: snap_filename
    type(CompressedStream)     :: snappy_stream
    integer                    :: pos, ios

    open(newunit = input_layer%iunit, file=snap_filename, status='old', access='stream', form='unformatted', iostat=ios)
    if (ios /= 0) then
       write(*,*) 'ERROR: Unable to open file: ', snap_filename, ' for reading.'
       stop "Error opening file for reading."
    end if
    call PrintFileUnitInformation( input_layer%iunit )
  end subroutine RestoreSnapToDLayer
  
end submodule DiskDesignImplModule

