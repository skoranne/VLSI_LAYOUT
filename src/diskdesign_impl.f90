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
  use ProfilerModule, only: get_cpu_time, RecordTime
  implicit none

  !> Disk based OOC (out of code) loading system

contains
  module subroutine BuildTree( input_layer )
    type(Layer), intent(inout)  :: input_layer
    real(kind=real64)           :: t_start
    integer(kind=int64)         :: total_nodes, num_squares, n_used, i
    logical                     :: dominated_by_squares
    dominated_by_squares = .false.

    if (profiling_enabled) t_start = get_cpu_time()

    if( .not. NeedsRTree( input_layer ) ) then
       if (profiling_enabled) call RecordTime("BuildTree", get_cpu_time() - t_start)
       return
    end if
    n_used = input_layer%n_used
    if( n_used <= 0 ) then
       input_layer%layerState = ior( input_layer%layerState, ior( LAYER_STATE_SORT, LAYER_STATE_RTREE ) )
       if (profiling_enabled) call RecordTime("BuildTree", get_cpu_time() - t_start)
       return
    end if
    if( NeedsSorting( input_layer ) ) then
       num_squares = 0_int64
       do i = 1, n_used
          if ( input_layer%layer_boxes(i)%x1 < input_layer%layer_boxes(i)%x2 .and. &
               input_layer%layer_boxes(i)%y1 < input_layer%layer_boxes(i)%y2 .and. &
               (input_layer%layer_boxes(i)%x2 - input_layer%layer_boxes(i)%x1) == &
               (input_layer%layer_boxes(i)%y2 - input_layer%layer_boxes(i)%y1) ) then
             num_squares = num_squares + 1_int64
          end if
       end do
       if( num_squares*1.0_real64 / (n_used*1.0_real64) > K_SQUARE_DOMINATION_THRESHOLD ) then
          dominated_by_squares = .true.
       end if
#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
       call SortBoxesDirect( input_layer%layer_boxes, int( n_used, kind=int64 ) )
#else
       if( dominated_by_squares ) then
          if( size( input_layer%layer_boxes, kind=int64 ) == n_used ) then
             call MortonSort( input_layer%layer_boxes )
          else
             block
               type(Box), allocatable :: sort_boxes(:)
               allocate( sort_boxes(n_used) )
               sort_boxes = input_layer%layer_boxes(1:n_used)
               call MortonSort( sort_boxes )
               call move_alloc( from=sort_boxes, to=input_layer%layer_boxes )
               input_layer%n_alloc = n_used
             end block
          end if
       else
          ! omt_pack sorts the slice in place without copying the full layer
          call omt_pack( input_layer%layer_boxes(1:n_used), K_LEAF_CAPACITY )
       end if
#endif
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
    end if
    total_nodes = CalculateTotalNodes( n_used, K_LEAF_CAPACITY )
    if( allocated( input_layer%tree%tree_nodes ) ) deallocate( input_layer%tree%tree_nodes )
    allocate( input_layer%tree%tree_nodes( total_nodes ) )
    call BuildRTree( input_layer%layer_boxes(1:n_used), K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index )
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )

    if (profiling_enabled) call RecordTime("BuildTree", get_cpu_time() - t_start)
  end subroutine BuildTree

  module subroutine SaveLayerToSnap( input_layer, snap_filename, method_to_use )
    type(Layer), intent(inout), target :: input_layer
    character(*), intent(in)   :: snap_filename
    integer, intent(in)        :: method_to_use
    type(CompressedStream)     :: snappy_stream
    if( NeedsRTree( input_layer ) ) then
       !> make sure its TREE is empty
       if( allocated( input_layer%tree%tree_nodes ) ) error stop "ERROR: INCONSISTENT LAYER STATE during SAVE"
    end if
    call BuildTree( input_layer ) !> does sorting
    snappy_stream%layer_properties = ior( snappy_stream%layer_properties, input_layer%layerState )
    snappy_stream%layer_properties = iand( snappy_stream%layer_properties, not(LAYER_STATE_RTREE) ) !> no point in saving
    if( .not. NeedsPNum( input_layer ) ) snappy_stream%arr => input_layer%pnumtable%arr
    if( method_to_use == COMPRESSION_METHOD_SNAPPY .or. method_to_use == COMPRESSION_METHOD_ZLIB &
         .or. method_to_use == COMPRESSION_METHOD_ZSTD ) then
       !> ok
    else
       write(*,*) 'ERROR: unknown compression method: ', method_to_use, ' requested.'
       error stop
    end if
    call CompressBoxesToStream( input_layer%layer_boxes(1:input_layer%n_used), snappy_stream, method_to_use )
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
       input_layer%n_used  = size(input_layer%layer_boxes, kind=int64)
       input_layer%n_alloc = input_layer%n_used
       call BuildTree( input_layer )
       return
    end if
    call RestoreCompressedStreamFromDisk( snap_filename, snappy_stream )
    call DecompressStreamToBoxes( snappy_stream, input_layer%layer_boxes )
    input_layer%n_used  = size( input_layer%layer_boxes, kind=int64 )
    input_layer%n_alloc = input_layer%n_used
    if( input_layer%n_used /= snappy_stream%total_boxes) then
       write(*,*) 'ERROR: SNAP restoration failed for file: ', snap_filename
       error stop "ERROR: SNAP RESTORATION FAILED"
    end if
    input_layer%layerState = iand( snappy_stream%layer_properties, LAYER_STATE_EVERYTHING )
    call BuildTree( input_layer )
  end subroutine RestoreSnapToLayer

  module subroutine RestoreSnapToDLayer( input_layer, snap_filename )
    type(Layer), intent(inout) :: input_layer
    character(*), intent(in)   :: snap_filename
    integer                    :: ios

    open(newunit = input_layer%iunit, file=snap_filename, status='old', access='stream', form='unformatted', iostat=ios)
    if (ios /= 0) then
       write(*,*) 'ERROR: Unable to open file: ', snap_filename, ' for reading.'
       stop "Error opening file for reading."
    end if
    call PrintFileUnitInformation( input_layer%iunit )
  end subroutine RestoreSnapToDLayer
  
end submodule DiskDesignImplModule

