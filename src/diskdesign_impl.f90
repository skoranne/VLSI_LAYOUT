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
  use ProfilerModule, only: get_cpu_time, RecordTime, PhaseBegin, PhaseEnd
  implicit none

  !> Disk based OOC (out of code) loading system

contains
  module subroutine BuildTree( input_layer )
    type(Layer), intent(inout), target :: input_layer
    real(kind=real64)           :: t_start, t_phase
    integer(kind=int64)         :: total_nodes, num_squares, n_used, i
    logical                     :: dominated_by_squares
    dominated_by_squares = .false.

    if (profiling_enabled) t_start = get_cpu_time()

    ! If state claims R-tree is valid but the tree is missing, rebuild it.
    if( .not. NeedsRTree( input_layer ) ) then
       if( allocated(input_layer%tree%tree_nodes) .and. input_layer%tree%root_index > 0 ) then
          if (profiling_enabled) call RecordTime("BuildTree", get_cpu_time() - t_start)
          return
       end if
       ! Inconsistent state: fall through and rebuild.
    end if
    n_used = input_layer%n_used
    if( n_used <= 0 ) then
       input_layer%layerState = ior( input_layer%layerState, ior( LAYER_STATE_SORT, LAYER_STATE_RTREE ) )
       if (profiling_enabled) call RecordTime("BuildTree", get_cpu_time() - t_start)
       return
    end if
    if( .not. allocated( input_layer%layer_boxes ) ) error stop "BuildTree: n_used > 0 but layer_boxes not allocated"
    if( n_used > size( input_layer%layer_boxes, kind=int64 ) ) error stop "BuildTree: n_used exceeds layer_boxes capacity"
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
       t_phase = PhaseBegin('load.sort_gpu')
       call SortBoxesDirect( input_layer%layer_boxes, int( n_used, kind=int64 ) )
       call PhaseEnd('load.sort_gpu', t_phase, n_used)
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
       call LayerMark( input_layer, LAYER_STATE_SORT )
    end if
    ! Boxes may have been sorted above and the tree is replaced: drop device copies first.
    call LayerDeviceRelease( input_layer )
    if( allocated( input_layer%tree%tree_nodes ) ) deallocate( input_layer%tree%tree_nodes )
    input_layer%tree%root_index = 0
    total_nodes = CalculateTotalNodes( n_used, K_LEAF_CAPACITY )
    allocate( input_layer%tree%tree_nodes( total_nodes ) )
    t_phase = PhaseBegin('load.rtree')
    call BuildRTree( input_layer%layer_boxes(1:n_used), K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index )
    call LayerMark( input_layer, LAYER_STATE_RTREE )
    call LayerTreeBuilt( input_layer )
    call PhaseEnd('load.rtree', t_phase, n_used)

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
    type(Layer), intent(inout) :: input_layer
    character(*), intent(in)   :: snap_filename
    type(CompressedStream)     :: snappy_stream
    integer                    :: pos, iunit, ios
    real(kind=real64)          :: tp
    !> for memory based layers we need an optimization that NO-FILE => EMPTY LAYER
    if( temporary_layers > 1 ) then
       open(newunit=iunit, file=snap_filename, status='old', access='stream', form='unformatted', iostat=ios)
       if (ios /= 0) then
          call ClearLayer( input_layer )
          return
       end if
       close(iunit)
    end if
    pos = index( snap_filename, ".bin" )
    if( pos /= 0 ) then !> we are going to assume this is a non-compressed binary file
       if( allocated(input_layer%layer_boxes) )   deallocate(input_layer%layer_boxes)
       if( allocated(input_layer%pnumtable%arr) )  deallocate(input_layer%pnumtable%arr)
       if( allocated(input_layer%tree%tree_nodes) ) deallocate(input_layer%tree%tree_nodes)
       tp = PhaseBegin('load.klbin')
       call LoadKLBin( snap_filename, input_layer%layer_boxes)
       input_layer%n_used  = size(input_layer%layer_boxes, kind=int64)
       call PhaseEnd('load.klbin', tp, input_layer%n_used)
       input_layer%n_alloc = input_layer%n_used
       input_layer%tree%root_index = 0
       input_layer%layerState = LAYER_STATE_NONE
       call BuildTree( input_layer )
       return
    end if
    tp = PhaseBegin('load.snap')
    call RestoreCompressedStreamFromDisk( snap_filename, snappy_stream )
    call DecompressStreamToBoxes( snappy_stream, input_layer%layer_boxes )
    input_layer%n_used  = size( input_layer%layer_boxes, kind=int64 )
    call PhaseEnd('load.snap', tp, input_layer%n_used)
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

