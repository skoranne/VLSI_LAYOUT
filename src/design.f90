! File   : design.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Keep all layer information together
!
! Layout of this module
!   * Layer      - a growable vector of boxes plus the acceleration
!                  structures derived from it (sort order, R-tree,
!                  polygon numbers). layerState records which of the
!                  derived structures are currently valid.
!   * GridLayer  - a rows x cols partition of a layer into sub-layers.
!   * Design     - a named collection of layers.
!   * CGArgument / LayerKernel - the operand bundle and callback used by
!                  the tiled ("frame") boolean drivers.
! Heavy implementations live in the submodules (design_impl.f90,
! design_boolean_impl.f90, design_topo_impl.f90, diskdesign_impl.f90,
! gridlayer.f90); generic key grouping lives in DataStructuresModule.
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
  use ProfilerModule
  use BoostPolygonAPIModule
  use RTreeBuilderGPU
  use CompactLayerModule
  use PropertyTableModule, only: PropertyTable
  use TrapezoidModule, only: Trapezoid
  use iso_c_binding
  use iso_fortran_env, only : int32, int64, real64
  use omp_lib
  implicit none
  private
  public :: Design, Layer, LayerTree, GridLayer, CGArgument, LayerKernel
  public :: LAYER_STATE_NONE, LAYER_STATE_HEAL, LAYER_STATE_SORT, LAYER_STATE_PNUM, LAYER_STATE_RTREE, &
       LAYER_STATE_SLSORT, LAYER_STATE_FINAL, LAYER_STATE_PREPROCESSED, LAYER_STATE_EVERYTHING
  public :: DESIGN_DIRECTION_INPUT, DESIGN_DIRECTION_OUTPUT, DESIGN_DIRECTION_MEMORY
  public :: NeedsSorting, NeedsRTree, NeedsPNum, NeedsHealing
  public :: LayerHas, LayerMark, LayerUnmark, LayerReserve
  public :: LayerCompact, LayerExpand, LayerDropCompact, LayerExpandedBytes, CompactLayersEnabled
  public :: LayerAppendPart, LayerFinishParts
  public :: push_box, ClearLayer, CopyLayer, AssignFromBox, swap_layer, FinalizeLayer, DeleteDesign
  public :: LayerDeviceEnsure, LayerDeviceRelease, LayerTreeBuilt, LayerDeviceTouch, LayerDeviceDone
  public :: assignment(=)
  public :: PerformUnion, PerformPolygonUnion, calculate_union_area_by_polygon, &
       PreprocessLayer, PreprocessLayerByPolygon, PreprocessLayerSL, &
       BuildTree, MergeHealLayer, CertifyHealing, PerformMergeLayer, RemoveIdentical
  public :: LayerIsAnyAngle
  public :: CalculateSingleLayerAND, CalculateAND, CalculateOR, CalculateNOT, CalculateXOR, &
       CalculateFrameAND, CalculateFrameNOT, CalculateBoostOperation, CalculateTopologicalSelect, &
       CalculateGROWLayer, CalculateSHRINKLayer, CalculateSHRINKDirectional, CalculateOverlapCount, &
       CreateGRID, CreateEXTENT, FilterLayer, PerformOperation
  public :: SaveLayerToSnap, RestoreSnapToLayer, RestoreSnapToDLayer
  ! Re-exported from DataStructuresModule for existing callers.
  public :: BucketBoundary, get_equal_key_segments, GetSortPermutation

  !------------------------------------------------------------------
  !  Layer state: one bit per derived property that is currently valid
  !------------------------------------------------------------------
  enum, bind(C)
     enumerator :: LAYER_STATE_NONE   = 0
     enumerator :: LAYER_STATE_HEAL   = 1     !> boxes are pairwise disjoint (merged)
     enumerator :: LAYER_STATE_SORT   = 2     !> boxes are in R-tree packing order
     enumerator :: LAYER_STATE_PNUM   = 4     !> pnumtable matches the boxes
     enumerator :: LAYER_STATE_RTREE  = 8     !> tree matches the boxes
     enumerator :: LAYER_STATE_SLSORT = 16
     enumerator :: LAYER_STATE_FINAL  = 32
     enumerator :: LAYER_STATE_MAX    = 64
     enumerator :: LAYER_STATE_EVERYTHING = LAYER_STATE_MAX - 1
  end enum
  !> Every preprocessing property, but not FINAL.
  integer(kind=c_int), parameter :: LAYER_STATE_PREPROCESSED = LAYER_STATE_FINAL - 1

  enum, bind(C)
     enumerator :: DESIGN_DIRECTION_INPUT  = 0
     enumerator :: DESIGN_DIRECTION_OUTPUT = 1
     enumerator :: DESIGN_DIRECTION_MEMORY = 2
  end enum

  type :: LayerTree
     integer(kind=int64) :: root_index = 0
     type(RTreeNode), allocatable :: tree_nodes(:)
  end type LayerTree

  type :: Layer
     ! -- geometry: layer_boxes(1:n_used) are live, n_alloc is the capacity
     integer(kind=int64)    :: n_used   = 0
     integer(kind=int64)    :: n_alloc  = 0
     type(Box), allocatable :: layer_boxes(:)
     ! -- structures derived from the geometry; validity is tracked in layerState
     integer(kind=c_int)    :: layerState = LAYER_STATE_NONE
     type(LayerTree)        :: tree
     type(UnionFind)        :: pnumtable
     ! -- cached measurements
     real(kind=real64)      :: area = 0.0_real64
     real(kind=real64)      :: perimeter = 0.0_real64
     ! -- backing store
     character(len=:), allocatable :: fileName
     integer                :: iunit = -1
     logical                :: device_resident = .false.
     ! -- GPU residency bookkeeping (design_device_impl.f90 / device_registry.f90)
     integer(kind=int64)    :: dev_gen = 0       !> generation tag; changes when boxes/tree change
     integer                :: dev_boxes_id = 0  !> registry slot of layer_boxes(1:n_used), 0 = none
     integer                :: dev_tree_id = 0   !> registry slot of tree%tree_nodes, 0 = none
     ! -- compact (Morton key + shape id) storage, see compact_layer.f90.  When is_compact,
     !    the Box array, tree and pnum table are released (n_used = 0) and `compact` is the
     !    only copy of the geometry; LayerExpand restores the Box form on demand.
     type(CompactLayer)     :: compact
     logical                :: is_compact = .false.
     integer(kind=c_int)    :: compact_state = 0  !> layerState bits valid after expand (HEAL)
     integer(kind=int64)    :: last_use = 0       !> op counter of the last use (LRU for the budget)
     !> Shadow: after LayerExpand the cold form is kept (3-5 B/box) with the count and content
     !> hash of the expanded boxes. LayerCompact reuses it when the boxes are unchanged
     !> (same count and hash), so re-compacting an input layer costs nothing.
     logical                :: compact_shadow = .false.
     !> Very large banded outputs stay as several parts (compact = part 1, cextra = the rest)
     !> instead of paying the merge's 12 B/box transient; banded ops and LayerExpand read all parts.
     type(CompactLayer), allocatable :: cextra(:)
     logical                :: parts_failed = .false.   !> streaming compaction gave up (31-bit limit)
     integer(kind=int64)    :: shadow_n = 0, shadow_hash = 0
     !> Per-polygon properties (FASCL, property_table.f90); allocated on first use.
     !> Columns are keyed by canonical polygon ordinal and carry the geometry signature
     !> they were computed on, so a reordered layer keeps them and a rewritten one drops them.
     type(PropertyTable), allocatable :: props
     !> Any-angle geometry (anyangle_ops.f90): when n_traps > 0 the layer's region is the canonical
     !> trapezoid set traps(1:n_traps) (TrapMergeModule: exact, interior-disjoint, lattice-line
     !> sides) and layer_boxes is empty. Only operations that dispatch on LayerIsAnyAngle accept it.
     integer(kind=int64) :: n_traps = 0
     type(Trapezoid), allocatable :: traps(:)
     !> Source polygons of an any-angle layer read from a file (raw_off 1-based, n+1 entries;
     !> int32 lattice coordinates). Booleans on loaded layers snap these, unmerged, jointly
     !> (one rounding of the input, and self-overlap AND sees the input overlaps). Results and
     !> copies of results have raw_np = 0 and are fed back as their exact trapezoid outlines.
     integer(kind=int64) :: raw_np = 0
     integer(kind=int64), allocatable :: raw_off(:)
     integer(kind=int32), allocatable :: raw_x(:), raw_y(:)
  end type Layer

  !> A layer partitioned on a rows x cols grid. This *has* layers, it is
  !> not itself a flat layer, so it does not extend Layer.
  type :: GridLayer
     integer(kind=int64)          :: rows = 0
     integer(kind=int64)          :: cols = 0
     type(Layer), allocatable     :: sub_layers(:,:)
   contains
     procedure :: populate => populate_grid_from_layer
  end type GridLayer

  type :: Design
     type(Layer), allocatable :: layers(:)
     type(hash_type) :: ht
     character(len=1024), allocatable :: layerNames(:)
     type(Box)              :: DESIGN_EXTENT
     integer(kind=c_int)    :: design_direction = DESIGN_DIRECTION_INPUT
     character(len=:), allocatable :: designName
     character(len=:), allocatable :: fileName
  end type Design

  !> Operands of one tile of a frame operation: C = op(A [, B]).
  !> B stays disassociated for single-layer operations.
  type :: CGArgument
     type(Layer), pointer     :: A => null(), B => null(), C => null()
     integer(kind=int64)      :: control_opcode = 0
     integer(kind=int64)      :: control_parameter = 0
  end type CGArgument

  abstract interface
     subroutine LayerKernel(arg)
       import :: CGArgument
       type(CGArgument), intent(inout) :: arg
     end subroutine LayerKernel
  end interface

  interface assignment(=)
     module procedure AssignFromBox
     module procedure CopyLayer
  end interface assignment(=)

  interface

     !> GPU residency (design_device_impl.f90)
     module subroutine LayerDeviceEnsure( A, B )
       type(Layer), intent(inout), target :: A
       type(Layer), intent(inout), target, optional :: B
     end subroutine LayerDeviceEnsure
     module subroutine LayerDeviceRelease( L )
       type(Layer), intent(inout), target :: L
     end subroutine LayerDeviceRelease
     module subroutine LayerTreeBuilt( L )
       type(Layer), intent(inout), target :: L
     end subroutine LayerTreeBuilt
     module subroutine LayerDeviceTouch( L )
       type(Layer), intent(inout) :: L
     end subroutine LayerDeviceTouch
     module subroutine LayerDeviceDone( A, B )
       type(Layer), intent(inout), target :: A
       type(Layer), intent(inout), target, optional :: B
     end subroutine LayerDeviceDone

     module subroutine PerformMergeLayer( input_layer )
       type(Layer), intent( inout )    :: input_layer
     end subroutine PerformMergeLayer
     
     module function CertifyHealing( input_layer ) result(retval)
       type(Layer), intent( inout ), target :: input_layer   !> inout: GPU residency bookkeeping
       logical                      :: retval
     end function CertifyHealing
     
     module subroutine MergeHealLayer( input_layer )
       type(Layer), intent( inout ), target :: input_layer
     end subroutine MergeHealLayer
     
     module subroutine CalculateFrameFunction2( input_layer_A, input_layer_B, output_layer, &
          kernel, control_opcode, control_parameter )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout) :: output_layer
       procedure(LayerKernel)     :: kernel
       integer(kind=int64), intent(in) :: control_opcode, control_parameter
     end subroutine CalculateFrameFunction2

     module subroutine CalculateFrameFunction1( input_layer_A, output_layer, kernel, control_opcode, control_parameter )
       type(Layer), intent(inout) :: input_layer_A
       type(Layer), intent(inout)   :: output_layer
       procedure(LayerKernel)     :: kernel
       integer(kind=int64), intent(in) :: control_opcode, control_parameter
     end subroutine CalculateFrameFunction1

     module subroutine populate_grid_from_layer(this, base_layer, n_rows, n_cols, layout_bounds)
       class(GridLayer), intent(inout) :: this
       type(Layer),      intent(in)    :: base_layer
       integer(kind=int64), intent(in) :: n_rows, n_cols
       type(Box),        intent(in)    :: layout_bounds
     end subroutine populate_grid_from_layer

     module subroutine populate_gridlayer_from_unit(this, base_layer)
       type(Layer), intent(inout) :: this
       type(GridLayer), intent(inout) :: base_layer
     end subroutine populate_gridlayer_from_unit

     module subroutine PerformOperation( gA, gB, gO, opcode )
       type(Layer), intent(inout) :: gA, gB, gO
       integer                        :: opcode
     end subroutine PerformOperation

     module subroutine BuildTree( input_layer )
       type(Layer), intent(inout), target :: input_layer
     end subroutine BuildTree

     module subroutine SaveLayerToSnap( input_layer, snap_filename, method_to_use )
       type(Layer), intent(inout),target  :: input_layer
       character(*), intent(in)           :: snap_filename
       integer, intent(in)                :: method_to_use
     end subroutine SaveLayerToSnap

     module subroutine RestoreSnapToLayer( input_layer, snap_filename )
       type(Layer), intent(inout) :: input_layer
       character(*), intent(in)   :: snap_filename      
     end subroutine RestoreSnapToLayer

     module subroutine RestoreSnapToDLayer( input_layer, snap_filename )
       type(Layer), intent(inout) :: input_layer
       character(*), intent(in)    :: snap_filename      
     end subroutine RestoreSnapToDLayer

     module subroutine FinalizeLayer( output_layer )
       type(Layer),intent(inout) :: output_layer
     end subroutine FinalizeLayer

     module subroutine AssignFromBox( output_layer, input_box )
       type(Layer),intent(inout) :: output_layer
       type(Box), intent(in) :: input_box
     end subroutine AssignFromBox

     module subroutine FilterLayer( input_layer, input_box, output_layer )
       type(Layer), intent(in)    :: input_layer
       type(Layer), intent(inout) :: output_layer
       type(Box),   intent(in)    :: input_box
     end subroutine FilterLayer

     module subroutine ConvertLayerToBox( input_layer, output_layer, control_parameter )
       type(Layer), intent(inout) :: input_layer
       type(Layer), intent(inout) :: output_layer
       integer(kind=int64), intent(in) :: control_parameter
     end subroutine ConvertLayerToBox

     module subroutine CalculateGROWLayer( input_layer, output_layer, ivar )
       type(Layer),      intent(in)    :: input_layer
       type(Layer),      intent(inout) :: output_layer
       integer(kind=K_COORDINATE_KIND), intent(in)   :: ivar(4)
     end subroutine CalculateGROWLayer

     module subroutine CalculateSHRINKLayer( input_layer_A, shrink_value, output_layer )
       type(Layer), intent(inout) :: input_layer_A
       type(Layer), intent(inout)   :: output_layer
       integer(kind=K_COORDINATE_KIND), intent(in) :: shrink_value
     end subroutine CalculateSHRINKLayer

     module subroutine CalculateSHRINKDirectional( input_layer, output_layer, ivar )
       type(Layer), intent(in) :: input_layer
       type(Layer), intent(inout) :: output_layer
       integer(kind=K_COORDINATE_KIND), intent(in) :: ivar(4)
     end subroutine CalculateSHRINKDirectional

     module function CalculateOverlapCount( input_layer_A ) result( interaction_count )
       type(Layer), intent(inout) :: input_layer_A
       integer(kind=int64) :: interaction_count
     end function CalculateOverlapCount

     module function CalculateSingletonCount( input_layer, is_singleton ) result( interaction_count )
       type(Layer), intent(inout) :: input_layer
       logical, allocatable, intent(out) :: is_singleton(:)
       integer(kind=int64) :: interaction_count
     end function CalculateSingletonCount

     module subroutine RemoveIdentical(input_layer)
       type(Layer), intent(inout) :: input_layer
     end subroutine RemoveIdentical

     module subroutine CalculateSingleLayerAND( input_layer_A, output_layer )
       type(Layer), intent(inout) :: input_layer_A
       type(Layer), intent(inout) :: output_layer
     end subroutine CalculateSingleLayerAND

     module subroutine CreateGrid( input_layer, output_layer, rows, cols, GridOverlap )
       type(Layer),      intent(in)    :: input_layer
       type(Layer),      intent(inout) :: output_layer
       integer,          intent(in)    :: rows, cols   ! must be >0
       integer,     intent(in)    :: GridOverlap
     end subroutine CreateGrid

     module subroutine CreateEXTENT( input_layer, output_layer )
       type(Layer),      intent(in)    :: input_layer
       type(Layer),      intent(inout) :: output_layer
     end subroutine CreateEXTENT

     module subroutine CalculateFrameNOT( input_layer_A, input_layer_B, output_layer )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout)   :: output_layer
     end subroutine CalculateFrameNOT

     module subroutine CalculateXOR( input_layer_A, input_layer_B, output_layer )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout) :: output_layer
     end subroutine CalculateXOR

     module subroutine CalculateFrameAND( input_layer_A, input_layer_B, output_layer )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout) :: output_layer
     end subroutine CalculateFrameAND
     
     module subroutine CalculateBoostOperation( input_layer_A, input_layer_B, output_layer, control_parameter, control_value )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout)   :: output_layer
       integer(kind=int64), intent(in) :: control_parameter, control_value
     end subroutine CalculateBoostOperation

     module subroutine MergeBoxes( input_N, boxes, output_N )
       integer(kind=int64), intent(in) :: input_N
       type(Box), allocatable, intent(inout) :: boxes(:)
       integer(kind=int64), intent(out) :: output_N
     end subroutine MergeBoxes

     module subroutine CalculateOR( input_layer_A, input_layer_B, output_layer )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout) :: output_layer
     end subroutine CalculateOR

     module subroutine CalculateAND( input_layer_A, input_layer_B, output_layer )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout) :: output_layer
     end subroutine CalculateAND

     module subroutine CalculateNOT( input_layer_A, input_layer_B, output_layer )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout) :: output_layer
     end subroutine CalculateNOT

     module subroutine CalculateTopologicalSelect( input_layer_A, input_layer_B, output_layer, topo_op )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout) :: output_layer
       integer(kind=int64), intent(in) :: topo_op
     end subroutine CalculateTopologicalSelect

  end interface

contains
  !------------------------------------------------------------------
  !  Layer state (non-polymorphic: avoids ifx static RTTI prologue writes)
  !------------------------------------------------------------------
  pure function LayerHas(this, flags) result(retval)
    type(Layer), intent(in)         :: this
    integer(kind=c_int), intent(in) :: flags
    logical :: retval
    retval = iand(this%layerState, flags) == flags
  end function LayerHas

  pure subroutine LayerMark(this, flags)
    type(Layer), intent(inout)      :: this
    integer(kind=c_int), intent(in) :: flags
    this%layerState = ior(this%layerState, flags)
  end subroutine LayerMark

  pure subroutine LayerUnmark(this, flags)
    type(Layer), intent(inout)      :: this
    integer(kind=c_int), intent(in) :: flags
    this%layerState = iand(this%layerState, not(flags))
  end subroutine LayerUnmark

  pure function NeedsSorting(input_layer) result(retval)
    type(Layer), intent(in) :: input_layer
    logical :: retval
    retval = iand(input_layer%layerState, LAYER_STATE_SORT) == 0
  end function NeedsSorting

  pure function NeedsPNum(input_layer) result(retval)
    type(Layer), intent(in) :: input_layer
    logical :: retval
    retval = iand(input_layer%layerState, LAYER_STATE_PNUM) == 0
  end function NeedsPNum

  pure function NeedsRTree(input_layer) result(retval)
    type(Layer), intent(in) :: input_layer
    logical :: retval
    retval = iand(input_layer%layerState, LAYER_STATE_RTREE) == 0
  end function NeedsRTree

  pure function NeedsHealing(input_layer) result(retval)
    type(Layer), intent(in) :: input_layer
    logical :: retval
    retval = iand(input_layer%layerState, LAYER_STATE_HEAL) == 0
  end function NeedsHealing

  !------------------------------------------------------------------
  !  Layer storage
  !------------------------------------------------------------------
  !> Make room for at least min_capacity boxes, keeping the live ones.
  subroutine LayerReserve(this, min_capacity)
    type(Layer), intent(inout)      :: this
    integer(kind=int64), intent(in) :: min_capacity
    type(Box), allocatable :: grown(:)
    integer(kind=int64)    :: i
    integer                :: alloc_status
    if( allocated( this%layer_boxes ) .and. min_capacity <= this%n_alloc ) return
    allocate( grown( min_capacity ), stat=alloc_status )
    if( alloc_status /= 0 ) error stop "Memory allocation failed"
    ! Explicit loop: no array temporary on the stack for huge layers
    do i = 1, this%n_used
       grown(i) = this%layer_boxes(i)
    end do
    call move_alloc( from=grown, to=this%layer_boxes )
    this%n_alloc = min_capacity
  end subroutine LayerReserve

  subroutine push_box( xbox, th_layer )
    type(Box), intent(in) :: xbox
    type(Layer), intent(inout) :: th_layer
    integer(kind=int64), parameter :: K_GROWTH_FACTOR = 2
    if( th_layer%n_used == th_layer%n_alloc ) then
       call LayerReserve( th_layer, max( 1_int64, th_layer%n_alloc * K_GROWTH_FACTOR ) )
    end if
    th_layer%n_used = th_layer%n_used + 1
    th_layer%layer_boxes( th_layer%n_used ) = xbox
  end subroutine push_box

  !> Release the geometry and everything derived from it. The backing
  !> store (fileName, iunit) is left alone. An empty layer trivially has
  !> every property, so no preprocessing is triggered for it.
  !> The layer holds any-angle geometry (canonical trapezoids instead of boxes).
  pure logical function LayerIsAnyAngle( L )
    type(Layer), intent(in) :: L
    LayerIsAnyAngle = L%n_traps > 0
  end function LayerIsAnyAngle

  subroutine ClearLayer( input_layer_A )
    type(Layer), intent(inout), target :: input_layer_A
    call LayerDeviceRelease( input_layer_A )
    call LayerDropCompact( input_layer_A )
    if( allocated( input_layer_A%layer_boxes ) ) deallocate( input_layer_A%layer_boxes )
    if( allocated( input_layer_A%pnumtable%arr ) ) deallocate( input_layer_A%pnumtable%arr )
    if( allocated( input_layer_A%props ) ) deallocate( input_layer_A%props )
    if( allocated( input_layer_A%traps ) ) deallocate( input_layer_A%traps )
    input_layer_A%n_traps = 0
    if( allocated( input_layer_A%raw_off ) ) deallocate( input_layer_A%raw_off )
    if( allocated( input_layer_A%raw_x ) ) deallocate( input_layer_A%raw_x, input_layer_A%raw_y )
    input_layer_A%raw_np = 0
    call drop_rtree( input_layer_A )
    input_layer_A%n_used = 0
    input_layer_A%n_alloc = 0
    input_layer_A%device_resident = .false.
    input_layer_A%layerState = LAYER_STATE_EVERYTHING
  end subroutine ClearLayer

  !> Deep copy of the geometry and derived structures (not of the
  !> measurements or the backing store).
  subroutine CopyLayer( output_layer, input_layer_A )
    type(Layer), intent(inout), target :: output_layer
    type(Layer), intent(in), target :: input_layer_A
    integer(kind=int64) :: n
    type(c_ptr) :: p_out, p_in
    !> Self-copy (e.g. `X = X COPY nothing`, or `X = X NOT Y` with empty Y via CalculateNOT):
    !> the deallocations below would free the input before it is read.
    !> (two temporaries: gfortran rejects c_loc() results passed straight to c_associated here)
    p_out = c_loc(output_layer); p_in = c_loc(input_layer_A)
    if( c_associated( p_out, p_in ) ) return
    n = input_layer_A%n_used
    if( input_layer_A%n_traps > 0 ) then          !> any-angle layer: the trapezoids are the geometry
       call ClearLayer( output_layer )
       allocate( output_layer%traps, source=input_layer_A%traps(1:input_layer_A%n_traps) )
       output_layer%n_traps = input_layer_A%n_traps
       if( input_layer_A%raw_np > 0 ) then
          allocate( output_layer%raw_off, source=input_layer_A%raw_off )
          allocate( output_layer%raw_x, source=input_layer_A%raw_x )
          allocate( output_layer%raw_y, source=input_layer_A%raw_y )
          output_layer%raw_np = input_layer_A%raw_np
       end if
       output_layer%layerState = LAYER_STATE_NONE
       if( allocated( input_layer_A%props ) ) allocate( output_layer%props, source=input_layer_A%props )
       return
    end if
    if( n == 0 ) then
       call ClearLayer( output_layer )
       return
    end if
    call LayerDeviceRelease( output_layer )
    if( allocated( output_layer%layer_boxes ) ) deallocate( output_layer%layer_boxes )
    allocate( output_layer%layer_boxes, source=input_layer_A%layer_boxes(1:n) )
    output_layer%n_used = n
    output_layer%n_alloc = n
    output_layer%device_resident = .false.
    output_layer%layerState = input_layer_A%layerState

    if( allocated( output_layer%pnumtable%arr ) ) deallocate( output_layer%pnumtable%arr )
    if( allocated( input_layer_A%pnumtable%arr ) ) then
       allocate( output_layer%pnumtable%arr, source=input_layer_A%pnumtable%arr )
    end if

    call drop_rtree( output_layer )
    if( allocated( input_layer_A%tree%tree_nodes ) ) then
       allocate( output_layer%tree%tree_nodes, source=input_layer_A%tree%tree_nodes )
       output_layer%tree%root_index = input_layer_A%tree%root_index
    end if
    !> properties travel with the geometry (e.g. o1:m = d1:m COPY nothing)
    if( allocated( output_layer%props ) ) deallocate( output_layer%props )
    if( allocated( input_layer_A%props ) ) allocate( output_layer%props, source=input_layer_A%props )
  end subroutine CopyLayer

  !> O(1) exchange of two layers; allocatable payloads are moved, never copied.
  subroutine swap_layer(a, b, filename_as_well)
    type(Layer), intent(inout) :: a, b
    logical, intent(in)        :: filename_as_well
    type(Box), allocatable           :: temp_boxes(:)
    type(RTreeNode), allocatable     :: temp_nodes(:)
    integer(kind=int64), allocatable :: temp_pnum(:)
    character(len=:), allocatable    :: temp_fileName
    type(PropertyTable), allocatable :: temp_props

    if( a%is_compact .or. b%is_compact ) error stop "swap_layer: expand compact layers first"
    call LayerDropCompact( a )      !> shadows belong to the old contents
    call LayerDropCompact( b )
    call swap_int64( a%n_used,  b%n_used )
    call swap_int64( a%n_alloc, b%n_alloc )
    call swap_int64( a%tree%root_index, b%tree%root_index )
    call swap_real64( a%area, b%area )
    call swap_real64( a%perimeter, b%perimeter )
    call swap_int( a%layerState, b%layerState )
    call swap_int( a%iunit, b%iunit )
    call swap_logical( a%device_resident, b%device_resident )
    call swap_int64( a%dev_gen, b%dev_gen )
    call swap_int( a%dev_boxes_id, b%dev_boxes_id )
    call swap_int( a%dev_tree_id, b%dev_tree_id )

    call move_alloc(a%layer_boxes, temp_boxes)
    call move_alloc(b%layer_boxes, a%layer_boxes)
    call move_alloc(temp_boxes,    b%layer_boxes)

    call move_alloc(a%tree%tree_nodes, temp_nodes)
    call move_alloc(b%tree%tree_nodes, a%tree%tree_nodes)
    call move_alloc(temp_nodes,        b%tree%tree_nodes)

    call move_alloc(a%pnumtable%arr, temp_pnum)
    call move_alloc(b%pnumtable%arr, a%pnumtable%arr)
    call move_alloc(temp_pnum,       b%pnumtable%arr)

    call move_alloc(a%props, temp_props)
    call move_alloc(b%props, a%props)
    call move_alloc(temp_props, b%props)

    block
      type(Trapezoid), allocatable :: temp_traps(:)
      integer(kind=int64), allocatable :: t_off(:)
      integer(kind=int32), allocatable :: t_c(:)
      call swap_int64( a%n_traps, b%n_traps )
      call move_alloc(a%traps, temp_traps)
      call move_alloc(b%traps, a%traps)
      call move_alloc(temp_traps, b%traps)
      call swap_int64( a%raw_np, b%raw_np )
      call move_alloc(a%raw_off, t_off); call move_alloc(b%raw_off, a%raw_off); call move_alloc(t_off, b%raw_off)
      call move_alloc(a%raw_x, t_c); call move_alloc(b%raw_x, a%raw_x); call move_alloc(t_c, b%raw_x)
      call move_alloc(a%raw_y, t_c); call move_alloc(b%raw_y, a%raw_y); call move_alloc(t_c, b%raw_y)
    end block

    if( filename_as_well ) then
       call move_alloc(a%fileName, temp_fileName)
       call move_alloc(b%fileName, a%fileName)
       call move_alloc(temp_fileName, b%fileName)
    end if
  contains
    pure subroutine swap_int64(x, y)
      integer(kind=int64), intent(inout) :: x, y
      integer(kind=int64) :: t
      t = x; x = y; y = t
    end subroutine swap_int64
    pure subroutine swap_int(x, y)
      integer, intent(inout) :: x, y
      integer :: t
      t = x; x = y; y = t
    end subroutine swap_int
    pure subroutine swap_real64(x, y)
      real(kind=real64), intent(inout) :: x, y
      real(kind=real64) :: t
      t = x; x = y; y = t
    end subroutine swap_real64
    pure subroutine swap_logical(x, y)
      logical, intent(inout) :: x, y
      logical :: t
      t = x; x = y; y = t
    end subroutine swap_logical
  end subroutine swap_layer

  subroutine DeleteDesign( input_design )
    type(Design), intent(inout) :: input_design
    integer :: i
    if( allocated( input_design%layers ) ) then
       do i = 1, size( input_design%layers )
          call ClearLayer( input_design%layers(i) )
       end do
       deallocate( input_design%layers )
    end if
    if( allocated( input_design%layerNames ) ) deallocate( input_design%layerNames )
  end subroutine DeleteDesign

  !------------------------------------------------------------------
  !  Sorting and R-tree maintenance
  !------------------------------------------------------------------
  subroutine drop_rtree( input_layer )
    type(Layer), intent(inout), target :: input_layer
    call LayerDeviceRelease( input_layer )
    if( allocated( input_layer%tree%tree_nodes ) ) deallocate( input_layer%tree%tree_nodes )
    input_layer%tree%root_index = 0
  end subroutine drop_rtree

  !> (Re)build the R-tree over the live boxes, which must already be sorted.
  subroutine rebuild_rtree( input_layer )
    type(Layer), intent(inout) :: input_layer
    integer(kind=int64) :: total_nodes
    total_nodes = CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY )
    if( total_nodes < 1 ) error stop "ERROR: cannot build an RTREE for an empty layer"
    call drop_rtree( input_layer )
    allocate( input_layer%tree%tree_nodes( total_nodes ) )
    call BuildRTree( input_layer%layer_boxes(1:input_layer%n_used), K_LEAF_CAPACITY, &
         input_layer%tree%tree_nodes, input_layer%tree%root_index )
    call LayerMark( input_layer, LAYER_STATE_RTREE )
    call LayerTreeBuilt( input_layer )
  end subroutine rebuild_rtree

  !> Morton order suits layers that are mostly squares (vias, contacts).
  pure function is_square_dominated( boxes ) result(retval)
    type(Box), intent(in) :: boxes(:)
    logical :: retval
    retval = real( count( is_square( boxes ) ), kind=real64 ) > &
         K_SQUARE_DOMINATION_THRESHOLD * real( size( boxes, kind=int64 ), kind=real64 )
  end function is_square_dominated

  !> Put the boxes in packing order. layer_boxes must hold exactly n_used boxes.
  subroutine sort_layer_boxes( input_layer, use_morton )
    type(Layer), intent(inout), target :: input_layer
    logical, intent(in)        :: use_morton
    call LayerDeviceRelease( input_layer )   !> boxes are reordered in place
    if( use_morton ) then
#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
       call SortBoxesDirect( input_layer%layer_boxes, input_layer%n_used )
#else
       call MortonSort( input_layer%layer_boxes )
#endif
    else
#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
       !> Experimental (MAGPARSER_PRESORT_GPU=1): Thrust Morton sort (~7x faster than the CPU
       !> OMT packing on 1e8 boxes) at the cost of a less overlap-minimising R-tree order.
       if( presort_gpu_enabled() .and. input_layer%n_used >= 65536_int64 ) then
          call SortBoxesDirect( input_layer%layer_boxes, input_layer%n_used )
          call LayerMark( input_layer, LAYER_STATE_SORT )
          return
       end if
#endif
       call omt_pack( input_layer%layer_boxes(1:input_layer%n_used), K_LEAF_CAPACITY )
    end if
    call LayerMark( input_layer, LAYER_STATE_SORT )
  end subroutine sort_layer_boxes

  logical function presort_gpu_enabled()
    character(len=8) :: ev
    integer :: evlen, evstat
    call get_environment_variable( 'MAGPARSER_PRESORT_GPU', ev, evlen, evstat )
    presort_gpu_enabled = ( evstat == 0 .and. evlen > 0 .and. ev(1:1) /= '0' )
    if( presort_gpu_enabled ) presort_gpu_enabled = gpu_offload_enabled( 65536_int64 )
  end function presort_gpu_enabled

  pure function all_boxes_valid( boxes ) result(retval)
    type(Box), intent(in) :: boxes(:)
    logical :: retval
    integer(kind=int64) :: i
    retval = .true.
    do i = 1, size( boxes, kind=int64 )
       if( .not. boxes(i)%is_valid() ) then
          retval = .false.
          return
       end if
    end do
  end function all_boxes_valid

  !------------------------------------------------------------------
  !  Healing (UNION)
  !------------------------------------------------------------------
  subroutine PerformUnion( input_layer )
    type(Layer), intent(inout) :: input_layer
    integer(kind=int64) :: n, updated_box_count
    if( input_layer%n_used == 0 ) return
    if( .not. NeedsHealing( input_layer ) ) return
    call heal_boxes( input_layer%n_used, input_layer%layer_boxes, updated_box_count )
    n = size( input_layer%layer_boxes, kind=int64 )
    if( .not. all_boxes_valid( input_layer%layer_boxes ) ) error stop "BOX NOT VALID"
    input_layer%n_used = n
    input_layer%n_alloc = n
    call sort_layer_boxes( input_layer, use_morton=.false. )
    call rebuild_rtree( input_layer )
  end subroutine PerformUnion

  !> Group the boxes of a layer by polygon number.
  !>   permutation(segments(k)%start_idx : segments(k)%end_idx) are the
  !>   boxes of one polygon. Isolated rectangles (polygon number 0), when
  !>   present, are always segments(1); first_polygon is the index of the
  !>   first real polygon segment (1 or 2).
  pure subroutine group_boxes_by_polygon( input_layer, permutation, segments, first_polygon )
    type(Layer), intent(in) :: input_layer
    integer(kind=int64), allocatable, intent(out)  :: permutation(:)
    type(BucketBoundary), allocatable, intent(out) :: segments(:)
    integer(kind=int64), intent(out) :: first_polygon
    integer(kind=int64) :: lowest_pnum
    if( size( input_layer%pnumtable%arr, kind=int64 ) /= input_layer%n_used ) then
       error stop "INCONSISTENT PNUM table size."
    end if
    call group_by_key( input_layer%pnumtable%arr, permutation, segments )
    lowest_pnum = input_layer%pnumtable%arr( permutation(1) )
    if( lowest_pnum < 0 ) error stop "INCONSISTENT BUCKET numbering detected"
    first_polygon = merge( 2_int64, 1_int64, lowest_pnum == 0 )
  end subroutine group_boxes_by_polygon

  !> Heal a layer one polygon at a time. Healing the whole layer in one
  !> sweep leaks scan events and creates massive memory pressure, cf
  !> On MW:  Heal changed:   51953736        1568839856
  !> Only polygons listed in area_overlap_roots (sorted) contain
  !> overlapping boxes; all others are already disjoint.
  subroutine PerformPolygonUnion( input_layer, area_overlap_roots )
    type(Layer), intent(inout) :: input_layer
    integer(kind=int64), intent(in) :: area_overlap_roots(:)
    integer(kind=int64), parameter :: K_POLYGON_INIT_BOX_COUNT = 64
    integer(kind=int64) :: i, j, polygon_number, box_count, healed_count
    integer(kind=int64) :: num_rects, num_squares, final_count, first_polygon
    type(Box), allocatable :: polygon_boxes(:), final_boxes(:)
    integer(kind=int64), allocatable :: permutation(:)
    type(BucketBoundary), allocatable :: segments(:)
    type(Box)         :: mbr_before, mbr_after
    real(kind=real64) :: raw_area, healed_area
    logical           :: has_overlap, dominated_by_squares

    if( input_layer%n_used == 0 ) return
    if( NeedsPNum( input_layer ) ) error stop "PLEASE RUN PNUM before."
    if( .not. NeedsHealing( input_layer ) ) return

    call group_boxes_by_polygon( input_layer, permutation, segments, first_polygon )
    num_rects = 0
    if( first_polygon == 2 ) num_rects = segments(1)%end_idx
    if( num_rects == input_layer%n_used ) then
       !> nothing touches anything else, so the layer is healed as it stands
       call LayerMark( input_layer, LAYER_STATE_HEAL )
       return
    end if

    !> Isolated rectangles are copied over directly
    allocate( final_boxes( input_layer%n_used ) )
    num_squares = 0
    do i = 1, num_rects
       final_boxes(i) = input_layer%layer_boxes( permutation(i) )
       if( .not. final_boxes(i)%is_valid() ) error stop "BOX NOT VALID"
       if( is_square( final_boxes(i) ) ) num_squares = num_squares + 1
    end do
    final_count = num_rects
    dominated_by_squares = real( num_squares, kind=real64 ) > &
         K_SQUARE_DOMINATION_THRESHOLD * real( input_layer%n_used, kind=real64 )

    allocate( polygon_boxes( K_POLYGON_INIT_BOX_COUNT ) )
    do i = first_polygon, size( segments, kind=int64 )
       polygon_number = input_layer%pnumtable%arr( permutation( segments(i)%start_idx ) )
       box_count = 1 + segments(i)%end_idx - segments(i)%start_idx
       if( box_count > size( polygon_boxes, kind=int64 ) ) then
          deallocate( polygon_boxes )
          allocate( polygon_boxes( 2*box_count ) )
       end if
       raw_area = 0.0_real64
       do j = 1, box_count
          polygon_boxes(j) = input_layer%layer_boxes( permutation( segments(i)%start_idx + j - 1 ) )
          if( .not. polygon_boxes(j)%is_valid() ) error stop "BOX NOT VALID"
          raw_area = raw_area + box_area_vectorized( polygon_boxes(j) )
       end do
       mbr_before  = mbr_of_array( polygon_boxes, box_count )
       has_overlap = bsearch_int64( polygon_number, area_overlap_roots ) /= 0

       if( has_overlap ) then
          call heal_boxes( box_count, polygon_boxes, healed_count )
          if( .not. all_boxes_valid( polygon_boxes(1:healed_count) ) ) error stop "ERROR: PUM INVALID BOX"
          healed_area = calculate_union_area_fast( polygon_boxes(1:healed_count) )
          mbr_after   = mbr_of_array( polygon_boxes, healed_count )
          if( healed_area > raw_area ) error stop "ERROR: INCONSISTENT AREA CALCULATION"
          if( mbr_before /= mbr_after ) error stop "ERROR: INCONSISTENT BBOX CALCULATION"
       else
          healed_count = box_count
          healed_area  = raw_area
       end if

       if( abs( healed_area - box_area_vectorized( mbr_before ) ) < K_SMALL_EPSILON ) then
          !> disjoint boxes that exactly tile their bounding box: one rectangle
          call append_boxes( [ mbr_before ], 1_int64 )
       else
          call append_boxes( polygon_boxes, healed_count )
       end if
    end do
    deallocate( polygon_boxes )

    if( final_count == size( final_boxes, kind=int64 ) ) then
       call move_alloc( from=final_boxes, to=input_layer%layer_boxes )
    else
       deallocate( input_layer%layer_boxes )
       allocate( input_layer%layer_boxes, source=final_boxes(1:final_count) )
       deallocate( final_boxes )
    end if
    input_layer%n_used = final_count
    input_layer%n_alloc = final_count

    !> boxes changed: the polygon numbers and the tree are stale
    call LayerUnmark( input_layer, LAYER_STATE_PNUM )
    call LayerMark( input_layer, LAYER_STATE_HEAL )
    call sort_layer_boxes( input_layer, use_morton=dominated_by_squares )
    call rebuild_rtree( input_layer )
  contains
    subroutine append_boxes( src, src_count )
      type(Box), intent(in)           :: src(:)
      integer(kind=int64), intent(in) :: src_count
      type(Box), allocatable :: grown(:)
      integer(kind=int64)    :: k
      if( final_count + src_count > size( final_boxes, kind=int64 ) ) then
         allocate( grown( max( 2*size( final_boxes, kind=int64 ), final_count + src_count ) ) )
         do k = 1, final_count
            grown(k) = final_boxes(k)
         end do
         call move_alloc( from=grown, to=final_boxes )
      end if
      do k = 1, src_count
         final_boxes(final_count + k) = src(k)
      end do
      final_count = final_count + src_count
    end subroutine append_boxes
  end subroutine PerformPolygonUnion

  !> Area covered by the layer. Healed layers are a plain sum; otherwise
  !> the union area is evaluated polygon by polygon.
  pure function calculate_union_area_by_polygon( input_layer ) result( retval_area )
    type(Layer), intent(in) :: input_layer
    real(kind=real64) :: retval_area, raw_area, union_area
    integer(kind=int64), parameter :: K_POLYGON_INIT_BOX_COUNT = 64
    integer(kind=int64) :: i, j, box_count, first_polygon
    type(Box), allocatable :: polygon_boxes(:)
    integer(kind=int64), allocatable :: permutation(:)
    type(BucketBoundary), allocatable :: segments(:)

    retval_area = 0.0_real64
    if( input_layer%n_used == 0 ) return
    if( NeedsPNum( input_layer ) ) error stop "PLEASE RUN PNUM before."
    if( .not. NeedsHealing( input_layer ) ) then
       do i = 1, input_layer%n_used
          retval_area = retval_area + box_area_vectorized( input_layer%layer_boxes(i) )
       end do
       return
    end if

    call group_boxes_by_polygon( input_layer, permutation, segments, first_polygon )
    !> isolated rectangles cannot overlap anything
    if( first_polygon == 2 ) then
       do i = 1, segments(1)%end_idx
          retval_area = retval_area + box_area_vectorized( input_layer%layer_boxes( permutation(i) ) )
       end do
    end if

    allocate( polygon_boxes( K_POLYGON_INIT_BOX_COUNT ) )
    do i = first_polygon, size( segments, kind=int64 )
       box_count = 1 + segments(i)%end_idx - segments(i)%start_idx
       if( box_count > size( polygon_boxes, kind=int64 ) ) then
          deallocate( polygon_boxes )
          allocate( polygon_boxes( 2*box_count ) )
       end if
       raw_area = 0.0_real64
       do j = 1, box_count
          polygon_boxes(j) = input_layer%layer_boxes( permutation( segments(i)%start_idx + j - 1 ) )
          if( .not. polygon_boxes(j)%is_valid() ) error stop "INVALID BOX"
          raw_area = raw_area + box_area_vectorized( polygon_boxes(j) )
       end do
       union_area = calculate_union_area_fast( polygon_boxes(1:box_count) )
       if( union_area > raw_area ) error stop "INCONSISTENT AREA CALCULATION"
       retval_area = retval_area + union_area
    end do
  end function calculate_union_area_by_polygon

  !------------------------------------------------------------------
  !  Preprocessing: bring a layer to HEAL + SORT + RTREE + PNUM
  !------------------------------------------------------------------
  subroutine PreprocessLayerSL( input_layer )
    type(Layer), intent(inout) :: input_layer
    real(kind=real64) :: overlap_area, overlap_perimeter
    integer(kind=int64) :: output_box_count
    real(kind=real64) :: tp

    if( input_layer%n_used == 0 ) then
       input_layer%layerState = LAYER_STATE_PREPROCESSED
       return
    end if
    if( NeedsHealing( input_layer ) ) then
!#define USE_BOOST_POLYGON
#ifdef USE_BOOST_POLYGON
       block
         type(Box), allocatable :: merged_boxes(:)
         call MergeBoxesUsingBoostPolygon( input_layer%layer_boxes, merged_boxes )
         call move_alloc( from=merged_boxes, to=input_layer%layer_boxes )
         output_box_count = size( input_layer%layer_boxes, kind=int64 )
       end block
#else
       if( input_layer%n_used >= K_FRAME_THRESHOLD .and. .not. sl_heal_forced() ) then
          !> Same strategy as the KLBIN load (PNumLoop): certify disjointness on the tree (GPU),
          !> and only heal when needed, with the tiled parallel merge instead of the serial scanline.
          if( NeedsSorting( input_layer ) ) then
             tp = PhaseBegin('pre.sort')
             call sort_layer_boxes( input_layer, use_morton=.false. )
             call PhaseEnd('pre.sort', tp, input_layer%n_used)
          end if
          if( NeedsRTree( input_layer ) ) then
             tp = PhaseBegin('pre.rtree')
             call rebuild_rtree( input_layer )
             call PhaseEnd('pre.rtree', tp, input_layer%n_used)
          end if
          tp = PhaseBegin('pre.certify')
          if( CertifyHealing( input_layer ) ) then
             call PhaseEnd('pre.certify', tp, input_layer%n_used)
             call LayerMark( input_layer, LAYER_STATE_HEAL )      !> boxes, order and tree unchanged
          else
             call PhaseEnd('pre.certify', tp, input_layer%n_used)
             tp = PhaseBegin('pre.mergeheal')
             call MergeHealLayer( input_layer )             !> sets state HEAL only
             call PhaseEnd('pre.mergeheal', tp, input_layer%n_used)
          end if
          output_box_count = input_layer%n_used
       else
          tp = PhaseBegin('pre.heal')
          call heal_boxes( input_layer%n_used, input_layer%layer_boxes, output_box_count )
          call PhaseEnd('pre.heal', tp, input_layer%n_used)
          input_layer%layerState = LAYER_STATE_HEAL !> the boxes changed: everything else is stale
       end if
#endif
       input_layer%n_used = output_box_count
       if( allocated( input_layer%layer_boxes ) ) input_layer%n_alloc = size( input_layer%layer_boxes, kind=int64 )
       if( input_layer%n_used == 0 ) then
          input_layer%layerState = LAYER_STATE_PREPROCESSED
          return
       end if
    end if
    if( NeedsSorting( input_layer ) ) then
       tp = PhaseBegin('pre.sort')
       call sort_layer_boxes( input_layer, use_morton=.false. )
       call PhaseEnd('pre.sort', tp, input_layer%n_used)
    end if
    if( NeedsRTree( input_layer ) ) then
       tp = PhaseBegin('pre.rtree')
       call rebuild_rtree( input_layer )
       call PhaseEnd('pre.rtree', tp, input_layer%n_used)
    end if
    if( NeedsPNum( input_layer ) ) then
       tp = PhaseBegin('pre.pnum')
       call LayerDeviceEnsure( input_layer )   !> no-op unless PerformMerge will take the GPU path
       call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes(1:input_layer%n_used), K_LEAF_CAPACITY, &
            input_layer%tree%tree_nodes, input_layer%tree%root_index, overlap_area, overlap_perimeter)
       call LayerDeviceDone( input_layer )
       call PhaseEnd('pre.pnum', tp, input_layer%n_used)
       call LayerMark( input_layer, LAYER_STATE_PNUM )
    end if
  end subroutine PreprocessLayerSL

  !> Alternative to PreprocessLayerSL: number the polygons first, then
  !> heal only the polygons that actually contain overlaps.
  subroutine PreprocessLayerByPolygon( input_layer )
    type(Layer), intent(inout) :: input_layer
    real(kind=real64) :: overlap_area, overlap_perimeter
    integer(kind=int64), allocatable :: area_overlap_roots(:)

    if( input_layer%n_used == 0 ) then
       input_layer%layerState = LAYER_STATE_EVERYTHING
       return
    end if

    if( NeedsSorting( input_layer ) ) then
       if( size( input_layer%layer_boxes, kind=int64 ) /= input_layer%n_used ) then
          !> MortonSort works on the whole allocation: drop the unused tail
          block
            type(Box), allocatable :: exact(:)
            allocate( exact, source=input_layer%layer_boxes(1:input_layer%n_used) )
            call move_alloc( from=exact, to=input_layer%layer_boxes )
            input_layer%n_alloc = input_layer%n_used
          end block
       end if
       call sort_layer_boxes( input_layer, use_morton=is_square_dominated( input_layer%layer_boxes ) )
    end if
    if( NeedsRTree( input_layer ) ) call rebuild_rtree( input_layer )
    !> an unhealed layer needs the overlap roots even when its polygon numbers are current
    if( NeedsPNum( input_layer ) .or. NeedsHealing( input_layer ) ) then
       call PerformMergeWithOverlapDetection( input_layer%pnumtable, input_layer%layer_boxes(1:input_layer%n_used), &
            K_LEAF_CAPACITY, input_layer%tree%tree_nodes, &
            input_layer%tree%root_index, overlap_area, overlap_perimeter, area_overlap_roots)
       if( overlap_area == 0 ) call LayerMark( input_layer, LAYER_STATE_HEAL )
       call LayerMark( input_layer, LAYER_STATE_PNUM )
    end if
    if( .not. allocated( area_overlap_roots ) ) allocate( area_overlap_roots(0) )
    call PerformPolygonUnion( input_layer, area_overlap_roots )
  end subroutine PreprocessLayerByPolygon

  subroutine PreprocessLayer( input_layer )
    type(Layer), intent(inout) :: input_layer
    integer            :: env_len, env_status
    real(kind=real64)  :: t_start

    if (profiling_enabled) t_start = get_cpu_time()

    call get_environment_variable( 'MAGPARSER_CONTROL_PREPROCESS_LAYER_BY_POLYGON', length=env_len, status=env_status )
    if( env_status == 0 ) then !> value is set
       call PreprocessLayerByPolygon( input_layer )
    elseif( env_status == 1 ) then !> value is not set
       call PreprocessLayerSL( input_layer )
    end if
    if( NeedsRTree( input_layer ) ) then
       if( allocated( input_layer%tree%tree_nodes ) ) error stop "ERROR: INCONSISTENT LAYER STATE during PREPROCESS"
    end if

    if (profiling_enabled) call RecordTime("PreprocessLayer", get_cpu_time() - t_start)
  end subroutine PreprocessLayer


  !------------------------------------------------------------------
  !  Compact layer storage (MAGPARSER_COMPACT_LAYERS=1)
  !------------------------------------------------------------------
  !> MAGPARSER_SL_HEAL=1 restores the serial scanline heal for all layer sizes.
  logical function sl_heal_forced()
    character(len=8) :: ev
    integer :: evlen, evstat
    call get_environment_variable( 'MAGPARSER_SL_HEAL', ev, evlen, evstat )
    sl_heal_forced = ( evstat == 0 .and. evlen > 0 .and. ev(1:1) /= '0' )
  end function sl_heal_forced

  logical function CompactLayersEnabled()
    character(len=8) :: ev
    integer :: evlen, evstat
    call get_environment_variable( 'MAGPARSER_COMPACT_LAYERS', ev, evlen, evstat )
    if( evstat == 0 .and. evlen > 0 ) then
       CompactLayersEnabled = ( ev(1:1) /= '0' )
    else
       CompactLayersEnabled = .true.
    end if
  end function CompactLayersEnabled

  !> Bytes held by the expanded (Box) form of a layer.
  integer(kind=int64) function LayerExpandedBytes( L ) result(nb)
    type(Layer), intent(in) :: L
    nb = 0
    if( allocated( L%layer_boxes ) ) nb = nb + 16_int64 * size( L%layer_boxes, kind=int64 )
    if( allocated( L%tree%tree_nodes ) ) nb = nb + 32_int64 * size( L%tree%tree_nodes, kind=int64 )
    if( allocated( L%pnumtable%arr ) ) nb = nb + 8_int64 * size( L%pnumtable%arr, kind=int64 )
  end function LayerExpandedBytes

  !> Box form -> compact (cold, bit-packed) form; frees boxes, tree and pnum table.
  !> No-op for small layers or when the layer cannot be encoded exactly.
  subroutine LayerCompact( L, min_boxes )
    type(Layer), intent(inout), target :: L
    integer(kind=int64), intent(in), optional :: min_boxes
    integer(kind=int64) :: nmin
    integer :: status
    integer(kind=c_int) :: keep_state
    real(kind=real64) :: tp
    nmin = 65536_int64
    if( present(min_boxes) ) nmin = min_boxes
    if( L%is_compact .or. L%n_used < nmin ) return
    keep_state = iand( L%layerState, LAYER_STATE_HEAL )
    if( L%compact_shadow ) then
       tp = PhaseBegin('compact.reuse')
       if( L%n_used == L%shadow_n ) then
          if( BoxSetHash( L%layer_boxes, L%n_used ) == L%shadow_hash ) then
             call release_expanded( L )
             L%compact_shadow = .false.
             L%is_compact = .true.
             L%compact_state = keep_state
             L%layerState = LAYER_STATE_NONE
             call PhaseEnd('compact.reuse', tp, L%compact%n)
             return
          end if
       end if
       call PhaseEnd('compact.reuse', tp, 0_int64)
       call CompactRelease( L%compact )            !> contents changed: shadow is stale
       L%compact_shadow = .false.
    end if
    tp = PhaseBegin('compact.encode')
    call CompactEncode( L%layer_boxes(1:L%n_used), L%n_used, L%compact, status )
    if( status /= 0 ) then
       call CompactRelease( L%compact )
       call PhaseEnd('compact.encode', tp, 0_int64)
       return
    end if
    call CompactPack( L%compact )
    if( debug_verbosity > 0 .or. PhaseTimersEnabled() ) call CompactPrintStats( L%compact, 'layer' )
    call release_expanded( L )
    L%is_compact = .true.
    L%compact_state = keep_state
    L%layerState = LAYER_STATE_NONE
    call PhaseEnd('compact.encode', tp, L%compact%n)
  end subroutine LayerCompact

  !> Compact -> Box form (Morton order, marked SORT) and R-tree; the compact copy is released.
  subroutine LayerExpand( L, build_tree )
    type(Layer), intent(inout), target :: L
    logical, intent(in), optional :: build_tree
    real(kind=real64) :: tp
    if( .not. L%is_compact ) return
    tp = PhaseBegin('compact.decode')
    if( allocated( L%cextra ) ) then                    !> multi-part: concatenate, no shadow
       block
         integer(kind=int64) :: ntot, off
         integer :: ip
         ntot = L%compact%n
         do ip = 1, size( L%cextra )
            ntot = ntot + L%cextra(ip)%n
         end do
         if( allocated( L%layer_boxes ) ) deallocate( L%layer_boxes )
         allocate( L%layer_boxes( max( 1_int64, ntot ) ) )
         call CompactDecodeColdInto( L%compact, L%layer_boxes )
         off = L%compact%n
         do ip = 1, size( L%cextra )
            call CompactDecodeColdInto( L%cextra(ip), L%layer_boxes( off+1: ) )
            off = off + L%cextra(ip)%n
            call CompactRelease( L%cextra(ip) )
         end do
         deallocate( L%cextra )
         call CompactRelease( L%compact )
         L%n_used = ntot
         L%n_alloc = size( L%layer_boxes, kind=int64 )
         L%is_compact = .false.
         L%compact_shadow = .false.
         L%layerState = L%compact_state                 !> parts are not jointly Morton-sorted
         L%tree%root_index = 0
       end block
       call PhaseEnd('compact.decode', tp, L%n_used)
       if( present(build_tree) ) then
          if( .not. build_tree ) return
       end if
       call BuildTree( L )
       return
    end if
    call CompactDecodeCold( L%compact, L%layer_boxes )   !> cold form kept as the shadow
    L%n_used = L%compact%n
    L%n_alloc = size( L%layer_boxes, kind=int64 )
    L%shadow_n = L%n_used
    L%shadow_hash = BoxSetHash( L%layer_boxes, L%n_used )
    L%compact_shadow = .true.
    L%is_compact = .false.
    L%layerState = ior( L%compact_state, LAYER_STATE_SORT )
    L%tree%root_index = 0
    call PhaseEnd('compact.decode', tp, L%n_used)
    if( present(build_tree) ) then
       if( .not. build_tree ) return
    end if
    call BuildTree( L )
  end subroutine LayerExpand

  !> The layer is about to be overwritten: a compact copy or shadow would be stale.
  subroutine LayerDropCompact( L )
    type(Layer), intent(inout) :: L
    if( allocated( L%cextra ) ) then
       block
         integer :: ip
         do ip = 1, size( L%cextra )
            call CompactRelease( L%cextra(ip) )
         end do
       end block
       deallocate( L%cextra )
    end if
    if( .not. (L%is_compact .or. L%compact_shadow) ) return
    call CompactRelease( L%compact )
    L%is_compact = .false.
    L%compact_shadow = .false.
  end subroutine LayerDropCompact

  !> Free the Box form (boxes, tree, pnum table, device copies) of a layer being compacted.
  !> Streaming compaction: encode the buffered boxes L%layer_boxes(1:n_used) as one more compact
  !> part of L (part 1 is L%compact, later parts go to L%cextra) and empty the buffer while
  !> keeping its allocation for reuse. A layer read this way never exists as one Box array, so
  !> the load peak is one buffer (e.g. 32M boxes, 512 MB) plus the compact parts.
  !> If a part cannot be encoded (coordinates beyond 31 bits) the parts so far are expanded
  !> back to Boxes and the layer continues as a plain Box layer.
  subroutine LayerAppendPart( L )
    type(Layer), intent(inout), target :: L
    type(CompactLayer) :: part
    type(CompactLayer), allocatable :: grown(:)
    type(Box), allocatable :: pend(:)
    integer :: status, ip, np
    integer(kind=int64) :: n
    real(kind=real64) :: tp
    if( L%n_used <= 0 ) return
    if( L%parts_failed ) return
    if( L%compact_shadow ) then                          !> a shadow is not a part
       call CompactRelease( L%compact )
       L%compact_shadow = .false.
    end if
    tp = PhaseBegin('compact.encode')
    call CompactEncode( L%layer_boxes(1:L%n_used), L%n_used, part, status )
    if( status /= 0 ) then
       call CompactRelease( part )
       call PhaseEnd('compact.encode', tp, 0_int64)
       L%parts_failed = .true.
       if( L%is_compact ) then                           !> fold the parts back into Boxes
          n = L%n_used
          allocate( pend(n) )
          pend = L%layer_boxes(1:n)
          call LayerExpand( L, build_tree=.false. )
          call release_tree_only( L )
          block
            type(Box), allocatable :: all(:)
            allocate( all(L%n_used + n) )
            all(1:L%n_used) = L%layer_boxes(1:L%n_used)
            all(L%n_used+1:) = pend
            call move_alloc( all, L%layer_boxes )
          end block
          L%n_used = L%n_used + n
          L%n_alloc = L%n_used
       end if
       return
    end if
    call CompactPack( part )
    if( .not. L%is_compact ) then
       call CompactMove( part, L%compact )
       L%is_compact = .true.
       L%compact_state = LAYER_STATE_NONE
    else
       np = 0
       if( allocated( L%cextra ) ) np = size( L%cextra )
       allocate( grown(np + 1) )
       do ip = 1, np
          call CompactMove( L%cextra(ip), grown(ip) )
       end do
       call CompactMove( part, grown(np + 1) )
       if( allocated( L%cextra ) ) deallocate( L%cextra )
       call move_alloc( grown, L%cextra )
    end if
    L%n_used = 0                                         !> keep the buffer for the next part
    L%layerState = LAYER_STATE_NONE
    call PhaseEnd('compact.encode', tp, 0_int64)
  end subroutine LayerAppendPart

  !> End of a streamed layer: flush the remaining boxes as a part and free the buffer.
  subroutine LayerFinishParts( L )
    type(Layer), intent(inout), target :: L
    call LayerAppendPart( L )
    if( L%is_compact .and. .not. L%parts_failed ) call release_expanded( L )
  end subroutine LayerFinishParts

  subroutine release_tree_only( L )
    type(Layer), intent(inout), target :: L
    call drop_rtree( L )
  end subroutine release_tree_only

  subroutine release_expanded( L )
    type(Layer), intent(inout), target :: L
    call LayerDeviceRelease( L )         !> device copies of the boxes/tree are now stale
    if( allocated( L%layer_boxes ) ) deallocate( L%layer_boxes )
    if( allocated( L%pnumtable%arr ) ) deallocate( L%pnumtable%arr )
    call drop_rtree( L )
    L%n_used = 0
    L%n_alloc = 0
    L%device_resident = .false.
  end subroutine release_expanded

end module DesignModule
