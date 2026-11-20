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
  use ProfilerModule
  use BoostPolygonAPIModule
  use RTreeBuilderGPU
  use iso_c_binding
  use iso_fortran_env, only : int32, int64, real64
  use omp_lib
  implicit none
  private
  public :: Design, LayerWrapper, Layer, GridLayer, LAYER_STATE_NONE, LAYER_STATE_HEAL, &
       LAYER_STATE_SORT, LAYER_STATE_PNUM, LAYER_STATE_RTREE, LAYER_STATE_EVERYTHING, LAYER_STATE_FINAL,&
       DESIGN_DIRECTION_INPUT, DESIGN_DIRECTION_OUTPUT, DESIGN_DIRECTION_MEMORY,&
       NeedsSorting, NeedsRTree, NeedsPNum, NeedsHealing, PerformUnion, PerformPolygonUnion, BucketBoundary, &
       get_equal_key_segments, GetSortPermutation, calculate_union_area_by_polygon, &
       PreprocessLayer, PreprocessLayerByPolygon, PreprocessLayerSL, &
       CalculateSingleLayerAND, CalculateAND, CalculateOR, CalculateNOT, &
       CalculateGROWLayer, CalculateSHRINKLayer, CreateGRID, CreateEXTENT,&
       RemoveIdentical, CalculateOverlapCount, ClearLayer, AssignFromBox, CopyLayer,&
       CalculateFrameNOT, CalculateBoostOperation, FilterLayer, push_box, FinalizeLayer,&
       DeleteDesign, SaveLayerToSnap, RestoreSnapToLayer, RestoreSnapToDLayer, BuildTree, PerformOperation,&
       CalculateXOR, CalculateFrameAND, MergeHealLayer, swap_layer, CertifyHealing, PerformMergeLayer,&
       CalculateTopologicalSelect

  type :: LayerTree
     integer(kind=int64) :: root_index = 0
     type(RTreeNode), allocatable :: tree_nodes(:)
  end type LayerTree
  enum, bind(C)                     ! bind(C) makes the values C‑compatible
     enumerator :: LAYER_STATE_NONE  = int(0, kind=c_int)   ! 0b000000
     enumerator :: LAYER_STATE_HEAL  = int(1, kind=c_int)   ! 0b000001
     enumerator :: LAYER_STATE_SORT  = int(2, kind=c_int)   ! 0b000010
     enumerator :: LAYER_STATE_PNUM  = int(4, kind=c_int)   ! 0b000100
     enumerator :: LAYER_STATE_RTREE = int(8, kind=c_int)   ! 0b001000
     enumerator :: LAYER_STATE_SLSORT= int(16, kind=c_int)   ! 0b010000
     enumerator :: LAYER_STATE_FINAL = int(32, kind=c_int)   ! 0b100000
     enumerator :: LAYER_STATE_MAX   = int(64, kind=c_int)
     !enumerator :: LAYER_STATE_FINAL = int(Z'32', kind=c_int)   !> partial knowledge can be DANGEROUS: Z -> hexadecimal
     enumerator :: LAYER_STATE_EVERYTHING = int(LAYER_STATE_MAX-1, kind=c_int)
  end enum
  enum, bind(C)
     enumerator :: CONJUGATE_LAYER_PURPOSE_NONE       = int(Z'00', kind=c_int)   ! 0b000000
     enumerator :: CONJUGATE_LAYER_PURPOSE_COMPLEMENT = int(Z'01', kind=c_int)   ! 0b000001
     enumerator :: CONJUGATE_LAYER_PURPOSE_SECTION    = int(Z'02', kind=c_int)   ! 0b000100
     enumerator :: CONJUGATE_LAYER_PURPOSE_GENERAL    = int(Z'04', kind=c_int)   ! 0b001000
     enumerator :: CONJUGATE_LAYER_PURPOSE_EXTENT     = int(Z'08', kind=c_int)   ! 0b001000
  end enum

  type :: ConjugateLayer
     integer :: conjugate_lid = 0
     integer(kind=c_int) :: purpose = CONJUGATE_LAYER_PURPOSE_NONE
  end type ConjugateLayer
  type :: Repetition
     integer(kind=K_COORDINATE_KIND) :: num_rows = 0, num_cols = 0, dx = 0, dy = 0
  end type Repetition

  type :: Layer
     integer(kind=int64)    :: n_used   = 0          ! how many slots are filled
     integer(kind=int64)    :: n_alloc  = 0          ! current allocation size
     type(Box), allocatable :: layer_boxes(:)
     integer(kind=c_int)    :: layerState = 0        ! HEAL, SORT, PNUM, RTREE
     type(LayerTree)        :: tree
     type(UnionFind)        :: pnumtable
     real(kind=real64)      :: area = 0.0_real64
     real(kind=real64)      :: perimeter = 0.0_real64
     character(len=:), allocatable :: fileName
     type(ConjugateLayer)   :: paired_layer
     integer                :: iunit = -1
     logical                :: device_resident = .false.
  end type Layer
  type, extends(Layer) :: GridLayer
     integer(kind=int64)          :: rows = 0
     integer(kind=int64)          :: cols = 0
     type(Layer), allocatable     :: sub_layers(:,:)
   contains
     procedure :: populate => populate_grid_from_layer
  end type GridLayer

  enum, bind(C)                     ! bind(C) makes the values C‑compatible
     enumerator :: DESIGN_DIRECTION_INPUT  =   int(Z'00', kind=c_int)
     enumerator :: DESIGN_DIRECTION_OUTPUT =   int(Z'01', kind=c_int)
     enumerator :: DESIGN_DIRECTION_MEMORY =   int(Z'02', kind=c_int)
  end enum

  type :: LayerWrapper
     class(Layer), allocatable :: item
  end type LayerWrapper

  type :: Design
     type(Layer), allocatable :: layers(:)
     type(hash_type) :: ht
     character(len=1024), dimension(:), allocatable :: layerNames(:)
     type(Box)              :: DESIGN_EXTENT
     integer(kind=c_int)    :: design_direction = DESIGN_DIRECTION_INPUT
     character(len=:), allocatable :: designName
     character(len=:), allocatable :: fileName
  end type Design

  ! A clean, modern derived type to hold our bucket boundaries
  type :: BucketBoundary
     integer(int64) :: start_idx
     integer(int64) :: end_idx
  end type BucketBoundary

  abstract interface
     subroutine GenericLambda(arbitrary_data)
       class(*), intent(inout) :: arbitrary_data
     end subroutine GenericLambda
  end interface

  type :: CGFunction
     procedure(GenericLambda), pointer, nopass :: preamble => null()
     procedure(GenericLambda), pointer, nopass :: main_compute => null()
     procedure(GenericLambda), pointer, nopass :: postamble => null()      
  end type CGFunction

  type :: CGArgument1
     type(Layer), pointer     :: A,C
     integer(kind=int64)      :: control_opcode
     integer(kind=int64)      :: control_parameter
  end type CGArgument1

  type :: CGArgument2
     type(Layer), pointer     :: A, B, C
     integer(kind=int64)      :: control_opcode
     integer(kind=int64)      :: control_parameter
  end type CGArgument2
  !------------------------------------------------------------------
  !  Public interface
  !------------------------------------------------------------------
  interface assignment(=)
     module procedure AssignFromBox
     module procedure CopyLayer
  end interface assignment(=)

  abstract interface
     subroutine ComputationalGeometryFunction2(arbitrary_data)
       import :: CGArgument2
       type(CGArgument2), intent(inout) :: arbitrary_data
     end subroutine ComputationalGeometryFunction2
     subroutine ComputationalGeometryFunction1(arbitrary_data)
       import :: CGArgument1        
       type(CGArgument1), intent(inout) :: arbitrary_data        
     end subroutine ComputationalGeometryFunction1

  end interface

  interface

     module subroutine PerformMergeLayer( input_layer )
       type(Layer), intent( inout )    :: input_layer
     end subroutine PerformMergeLayer
     
     module function CertifyHealing( input_layer ) result(retval)
       type(Layer), intent( in )    :: input_layer
       logical                      :: retval
     end function CertifyHealing
     
     module subroutine MergeHealLayer( input_layer )
       type(Layer), intent( inout ) :: input_layer
     end subroutine MergeHealLayer
     
     module subroutine CalculateFrameFunction2( input_layer_A, input_layer_B, output_layer, functype, control_opcode, control_parameter )
       type(Layer), intent(inout) :: input_layer_A, input_layer_B
       type(Layer), intent(inout) :: output_layer
       type(CGFunction) :: functype
       integer(kind=int64), intent(in) :: control_opcode, control_parameter
     end subroutine CalculateFrameFunction2

     module subroutine CalculateFrameFunction1( input_layer_A, output_layer, functype, control_opcode, control_parameter )
       type(Layer), intent(inout) :: input_layer_A
       type(Layer), intent(inout)   :: output_layer
       type(CGFunction) :: functype
       integer(kind=int64), intent(in) :: control_opcode, control_parameter
     end subroutine CalculateFrameFunction1

     module subroutine populate_grid_from_layer(this, base_layer, n_rows, n_cols, layout_bounds)
       class(GridLayer), intent(inout) :: this
       type(Layer),      intent(in)    :: base_layer
       integer(kind=int64), intent(in) :: n_rows, n_cols
       type(Box),        intent(in)    :: layout_bounds
     end subroutine populate_grid_from_layer

     module subroutine populate_gridlayer_from_unit(this, base_layer)
       character(len=*), parameter :: functionName = "PerformOperation"    
       type(Layer), intent(inout) :: this
       type(GridLayer), intent(inout) :: base_layer
     end subroutine populate_gridlayer_from_unit

     module subroutine PerformOperation( gA, gB, gO, opcode )
       class(Layer), intent(inout) :: gA, gB, gO
       integer                        :: opcode
     end subroutine PerformOperation

     module subroutine BuildTree( input_layer )
       type(Layer), intent(inout) :: input_layer
     end subroutine BuildTree

     module subroutine SaveLayerToSnap( input_layer, snap_filename, method_to_use )
       type(Layer), intent(inout),target  :: input_layer
       character(*), intent(in)           :: snap_filename
       integer, intent(in)                :: method_to_use
     end subroutine SaveLayerToSnap

     module subroutine RestoreSnapToLayer( input_layer, snap_filename )
       class(Layer), intent(inout) :: input_layer
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
    !if( .not. retval ) return !> layer is already in HEAL state
    !retval = CertifyHealing( input_layer )
    !if( .not. retval ) return !> GPU concurs
    !> GPU says layer does not need healing
    !input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_HEAL )
  end function NeedsHealing

  subroutine PerformUnion( input_layer )
    type(Layer), intent(inout) :: input_layer
    integer(kind=int64) :: i,n, updated_box_count
    type(Box) :: tempBox
    if( input_layer%n_used == 0 ) return
    if( .not. NeedsHealing( input_layer ) ) return !bravo
    call heal_boxes( input_layer%n_used, input_layer%layer_boxes, updated_box_count )
    n = size( input_layer%layer_boxes )
    !write (*,*) 'Heal changed: ', input_layer%n_used, ' ', n
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
  subroutine PerformPolygonUnion( input_layer, area_overlap_roots )
    type(Layer), intent(inout) :: input_layer
    integer(kind=int64), intent(in) :: area_overlap_roots(:) 
    integer, parameter :: K_POLYGON_INIT_BOX_COUNT = 64
    integer(kind=int64) :: i,j,n, num_roots, num_rects, polygon_number, box_count, num_squares
    integer(kind=int64) :: updated_box_count, final_count, final_capacity
    type(Box) :: tempBox
    type(Box), allocatable :: current_polygon_boxes(:)
    type(Box), allocatable :: final_boxes(:)
    integer(int64), allocatable :: permutation(:)
    type(BucketBoundary), allocatable :: segments(:)
    integer(int64) :: starting_segment
    !real(kind=real64) :: overlap_area, overlap_perimeter
    logical :: dominated_by_squares
    integer(kind=int64), parameter :: K_MINIMUM_SPAWN = 4096 !> we process anything below this on main thread
    real(kind=real64) :: temp_areaA, temp_areaB
    type(Box)         :: localBoxA, localBoxB
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
#define OLD_WORKING_CODE !> OpenMP gave 202.37      135.34 on MW4:poly
#ifdef OLD_WORKING_CODE  !> Old code 200.87      152.82, I think not worth the trouble?
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

       if( box_count > 100000) then
          write(*,*) 'BOX COUNT ', box_count, ' EXCEEDED'
       end if
       temp_areaA = 0.0_real64
       temp_areaB = 0.0_real64

       temp_areaA = sum( box_area_vectorized( current_polygon_boxes(1:box_count) ) )
       localBoxA  = mbr_of_array( current_polygon_boxes(1:box_count), box_count )
       if( abs( temp_areaA-box_area_vectorized( localBoxA ) ) < K_SMALL_EPSILON ) then
          final_boxes(final_count+1:final_count+2) = localBoxA
          final_count = final_count + 1
          cycle
       end if
       if( bsearch_int64( polygon_number, area_overlap_roots ) /= 0 ) then
          call heal_boxes( box_count, current_polygon_boxes, updated_box_count )
          !call MergeBoxes( box_count, current_polygon_boxes, updated_box_count )
       end if
       n = size( current_polygon_boxes )
       if( final_count + updated_box_count >= final_capacity ) then
          !write(*,*) 'FC = ', final_capacity, ' EC = ', final_count + updated_box_count
          error stop "FINAL CAPACITY is too low."
       end if
       do j=1,updated_box_count
          if(.not. current_polygon_boxes(j)%is_valid() ) error stop "ERROR: PUM INVALID BOX"
          !write(*,'(A,I,A,4I)') 'Box ', j, ': ', current_polygon_boxes(j)%x1, current_polygon_boxes(j)%y1, &
          !     current_polygon_boxes(j)%x2, current_polygon_boxes(j)%y2
       end do
       temp_areaB = calculate_union_area_fast( current_polygon_boxes(1:updated_box_count) )
       localBoxB  = mbr_of_array( current_polygon_boxes(1:updated_box_count), updated_box_count )
       if( abs( temp_areaB-box_area_vectorized( localBoxB ) ) < K_SMALL_EPSILON ) then
          final_boxes(final_count+1:final_count+2) = localBoxB
          final_count = final_count + 1
          cycle
       end if
       if( temp_areaB > temp_areaA ) error stop "ERROR: INCONSISTENT AREA CALCULATION"
       if( localBoxA /= localBoxB ) error stop  "ERROR: INCONSISTENT BBOX CALCULATION"
       if( abs( temp_areaB-box_area_vectorized( localBoxA ) ) < K_SMALL_EPSILON ) then
          write(*,*) 'WARN: INSUFFICIENT MERGE'
       end if

       final_boxes(final_count+1:final_count+updated_box_count) = current_polygon_boxes(1:updated_box_count)
       final_count = final_count + updated_box_count
       !write (*,*) 'Heal changed: ', box_count, ' ', updated_box_count, ' current count = ', final_count
       if( debug_verbosity > 1 ) then
          do j=1,updated_box_count
             tempBox = current_polygon_boxes(j)
             if( .not. tempBox%is_valid() ) error stop "BOX NOT VALID"
             !write(*,'(A,I,A,4I)') 'Box ', j, ': ', tempBox%x1, tempBox%y1, tempBox%x2, tempBox%y2
          end do
       end if
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
#else
    ! Declare a local variable to help safely calculate insertion points
    ! Start the parallel region
    !$omp parallel default(shared) &
    !$omp private(i, polygon_number, box_count, updated_box_count, n, j, tempBox, &
    !$omp         current_polygon_boxes, my_start)

    ! IMPORTANT: A private allocatable array starts unallocated in each thread.
    ! We must allocate a baseline size before the loop so the size() check
    ! inside the loop doesn't trigger a segmentation fault.
    if (allocated(current_polygon_boxes)) deallocate(current_polygon_boxes)
    allocate(current_polygon_boxes(K_MINIMUM_SPAWN*2)) ! Provide a sensible initial capacity
#ifdef GUIDED_SCHEDULING
    !$omp do schedule(guided)
    do i = starting_segment, size(segments)
       !> processing polygon number segments( starting_segment )%start_idx
       polygon_number = input_layer%pnumtable%arr( permutation( segments( i )%start_idx ) )
       box_count = 1 + segments( i )%end_idx - segments( i )%start_idx
       if( box_count > K_MINIMUM_SPAWN ) cycle
       if( input_layer%pnumtable%arr( permutation( segments( i )%end_idx ) ) /= polygon_number ) then
          error stop "INCONSISTENT polygon numbering detected"
       end if
       ! Now safe to call size() because we pre-allocated outside the loop
       current_polygon_boxes(1:box_count) = input_layer%layer_boxes( &
            permutation( segments( i )%start_idx:segments( i )%end_idx ) )
       updated_box_count = 0
       call heal_boxes( box_count, current_polygon_boxes, updated_box_count )
       n = size( current_polygon_boxes )
       ! ---------------------------------------------------------
       ! CRITICAL SECTION: Reserve space in the shared output array
       ! ---------------------------------------------------------
       !$omp critical (append_to_final)
       if( final_count + updated_box_count >= final_capacity ) then
          error stop "FINAL CAPACITY is too low."
       end if
       ! Grab the starting index for this specific thread's data
       my_start = final_count + 1
       ! Update the global tracker for the next thread
       final_count = final_count + updated_box_count
       !$omp end critical (append_to_final)
       ! ---------------------------------------------------------
       ! CONCURRENT WRITE: Copy data outside the critical section
       ! ---------------------------------------------------------
       final_boxes(my_start : my_start + updated_box_count - 1) = &
            current_polygon_boxes(1:updated_box_count)
       ! Verification loop
       do j=1, updated_box_count
          tempBox = current_polygon_boxes(j)
          if( .not. tempBox%is_valid() ) error stop "BOX NOT VALID"
       end do
    end do
    !$omp end do
#endif
    !> we are using a technique known as LOOP FISSION or workload separation
    ! Use dynamic scheduling because heal_boxes likely takes variable time
    ! depending on the polygon size.
    !write (*,*) 'Now OMP processing non-rects: final_count starts at ', final_count
    !$omp do schedule(dynamic) !> chunk size of 1
    do i = starting_segment, size(segments)
       !> processing polygon number segments( starting_segment )%start_idx
       polygon_number = input_layer%pnumtable%arr( permutation( segments( i )%start_idx ) )
       box_count = 1 + segments( i )%end_idx - segments( i )%start_idx
       !if( box_count <= K_MINIMUM_SPAWN ) cycle
       if( input_layer%pnumtable%arr( permutation( segments( i )%end_idx ) ) /= polygon_number ) then
          error stop "INCONSISTENT polygon numbering detected"
       end if
       ! Now safe to call size() because we pre-allocated outside the loop
       if( box_count > size( current_polygon_boxes ) ) then
          deallocate( current_polygon_boxes )
          allocate( current_polygon_boxes( 2*box_count ) )
       end if
       current_polygon_boxes(1:box_count) = input_layer%layer_boxes( &
            permutation( segments( i )%start_idx:segments( i )%end_idx ) )

       updated_box_count = 0
       call heal_boxes( box_count, current_polygon_boxes, updated_box_count )
       n = size( current_polygon_boxes )
       ! ---------------------------------------------------------
       ! CRITICAL SECTION: Reserve space in the shared output array
       ! ---------------------------------------------------------
       !$omp critical (append_to_final)
       if( final_count + updated_box_count >= final_capacity ) then
          error stop "FINAL CAPACITY is too low."
       end if
       ! Grab the starting index for this specific thread's data
       my_start = final_count + 1
       ! Update the global tracker for the next thread
       final_count = final_count + updated_box_count
       !$omp end critical (append_to_final)
       ! ---------------------------------------------------------
       ! CONCURRENT WRITE: Copy data outside the critical section
       ! ---------------------------------------------------------
       final_boxes(my_start : my_start + updated_box_count - 1) = &
            current_polygon_boxes(1:updated_box_count)
       ! Verification loop
       do j=1, updated_box_count
          tempBox = current_polygon_boxes(j)
          if( .not. tempBox%is_valid() ) error stop "BOX NOT VALID"
       end do
    end do
    !$omp end do

    ! Clean up thread-local memory before exiting the parallel region
    if (allocated(current_polygon_boxes)) deallocate(current_polygon_boxes)

    !$omp end parallel

    ! ... Proceed with final allocations as in your original code ...
    if (allocated(current_polygon_boxes)) deallocate( current_polygon_boxes )
    allocate( current_polygon_boxes( final_count ) )
    current_polygon_boxes(1:final_count) = final_boxes(1:final_count)
    deallocate( final_boxes )
#endif
    !deallocate( input_layer%layer_boxes )
    call move_alloc(from=current_polygon_boxes, to=input_layer%layer_boxes )
    if( debug_verbosity > 1 ) then
       do i=1,final_count
          tempBox = input_layer%layer_boxes(i)
          if( .not. tempBox%is_valid() ) error stop "BOX NOT VALID"
          !write(*,'(A,I,A,4I)') 'Box ', i, ': ', tempBox%x1, tempBox%y1, tempBox%x2, tempBox%y2
       end do
    end if
    if( size(input_layer%layer_boxes) /= final_count ) error stop "INCONSISTENT layer box size."
    input_layer%n_used = final_count
    input_layer%n_alloc = final_count
    deallocate( input_layer%tree%tree_nodes )
    allocate( input_layer%tree%tree_nodes( CalculateTotalNodes( final_count, K_LEAF_CAPACITY ) ) )
    !> the layer could be square dominated, lets look at pre-healed rectangles and decide
    if( dominated_by_squares ) then
#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
       call SortBoxesDirect( input_layer%layer_boxes, int( input_layer%n_used, kind=int64 ) )
#else
       call MortonSort( input_layer%layer_boxes )
#endif
    else
       call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
    end if
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_HEAL ) !> I hope so
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
    call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
    input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
    input_layer%layerState = iand( input_layer%layerState, .not. LAYER_STATE_PNUM )
    !call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
    !     input_layer%tree%root_index, overlap_area, overlap_perimeter)
    !input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_PNUM )
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
!#define USE_BOOST_POLYGON
#ifdef USE_BOOST_POLYGON
    if( NeedsHealing( input_layer ) ) then
       block
         type(Layer) :: tempLayer
         !> by the time we come here this should not be needed
         !$komp critical (heal_boxes_lock) !> work around
         !call heal_boxes( input_layer%n_used, input_layer%layer_boxes, output_box_count )
         call MergeBoxesUsingBoostPolygon( input_layer%layer_boxes, tempLayer%layer_boxes )
         if( allocated( input_layer%layer_boxes) ) deallocate( input_layer%layer_boxes )
         call move_alloc( from=tempLayer%layer_boxes, to=input_layer%layer_boxes)
         input_layer%n_used = size( input_layer%layer_boxes )
         !$komp end critical (heal_boxes_lock)
         !write(*,*) 'Healing from: ', input_layer%n_used, ' to ', output_box_count
         input_layer%layerState = LAYER_STATE_HEAL !> we wipe everything else
         !input_layer%n_used = output_box_count
       end block
    end if
#else
    if( NeedsHealing( input_layer ) ) then
       block
         !> by the time we come here this should not be needed
         !$komp critical (heal_boxes_lock) !> work around
         call heal_boxes( input_layer%n_used, input_layer%layer_boxes, output_box_count )
         !$komp end critical (heal_boxes_lock)
         !write(*,*) 'Healing from: ', input_layer%n_used, ' to ', output_box_count
         input_layer%layerState = LAYER_STATE_HEAL !> we wipe everything else
         input_layer%n_used = output_box_count
       end block
    end if
#endif
    if( NeedsSorting( input_layer ) ) then
       call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
    end if
    if( .false. ) then
#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
       call SortBoxesDirect( input_layer%layer_boxes, int( input_layer%n_used, kind=int64 ) )
#else
       if( env_status /= 0 .and. env_len > 0 .and. ( .not. omp_in_parallel() ) ) then !> value is set or we can use nested
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
#endif
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
    end if
    if( NeedsRTree( input_layer ) ) then
       if( allocated( input_layer%tree%tree_nodes ) ) deallocate( input_layer%tree%tree_nodes )
       if( CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY ) < 1 ) error stop
       allocate( input_layer%tree%tree_nodes( CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY ) ) )
       call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
    end if
    if( NeedsPNum( input_layer ) ) then
       call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
            input_layer%tree%root_index, overlap_area, overlap_perimeter)
       if( abs(overlap_area) > K_SMALL_EPSILON ) then
          !error stop "HEALING failed"
       end if
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_PNUM )
    end if
  end subroutine PreprocessLayerSL
  !> we have an alternative method using the RTree which we should compare
  subroutine PreprocessLayerByPolygon( input_layer )
    type(Layer), intent(inout) :: input_layer
    real(kind=real64) :: overlap_area, overlap_perimeter
    integer(kind=int64) :: num_squares
    logical             :: dominated_by_squares
    integer(kind=int64), allocatable :: area_overlap_roots(:) 

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
       call PerformMergeWithOverlapDetection( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
            input_layer%tree%root_index, overlap_area, overlap_perimeter, area_overlap_roots)
       if( overlap_area /= 0 ) then
          !> wipe HEALING state
          input_layer%layerState = iand( input_layer%layerState, NOT(LAYER_STATE_PNUM ) )
       else
          input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_HEAL )
       end if
       input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_PNUM )
    end if
    call PerformPolygonUnion( input_layer, area_overlap_roots )
    if( allocated( area_overlap_roots ) ) deallocate( area_overlap_roots )
  end subroutine PreprocessLayerByPolygon

  subroutine PreprocessLayer( input_layer )
    implicit none
    type(Layer), intent(inout) :: input_layer
    integer            :: env_len, env_status
    real(kind=real64)  :: t_start, t_end

    if (profiling_enabled) t_start = get_cpu_time()

    call get_environment_variable( 'MAGPARSER_CONTROL_PREPROCESS_LAYER_BY_POLYGON', length=env_len, status=env_status )
    if( env_status == 0 ) then !> value is set
       call PreprocessLayerByPolygon( input_layer )
    elseif( env_status == 1 ) then !> value is not set
       call PreprocessLayerSL( input_layer )
    end if
    if( NeedsRTree( input_layer ) ) then
       !> make sure its TREE is empty
       if( allocated( input_layer%tree%tree_nodes ) ) error stop "ERROR: INCONSISTENT LAYER STATE during PREPROCESS"
    end if

    if (profiling_enabled) call RecordTime("PreprocessLayer", get_cpu_time() - t_start)
  end subroutine PreprocessLayer

  !> utility subroutine, should move to more common place
  subroutine push_box( xbox, th_layer )
    type(Box), intent(in) :: xbox
    type(Layer), intent(inout) :: th_layer
    integer :: alloc_status
    integer(kind=int64), parameter :: K_GROWTH_FACTOR = 2
    if( th_layer%n_used == th_layer%n_alloc ) then
       block
         type(Box), allocatable :: temp(:)
         integer(kind=int64) :: i, new_alloc
         new_alloc = max(1_int64, th_layer%n_alloc * K_GROWTH_FACTOR)
         allocate( temp( new_alloc ), stat=alloc_status )
         if( alloc_status /= 0 ) then
            error stop "Memory allocation failed"
         end if
         th_layer%n_alloc = new_alloc
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

  subroutine DeleteLayer( input_layer_A )
    type(Layer), intent(inout) :: input_layer_A
    if( allocated( input_layer_A%layer_boxes ) ) deallocate( input_layer_A%layer_boxes )
    if( allocated( input_layer_A%pnumtable%arr ) ) deallocate( input_layer_A%pnumtable%arr )
    if( allocated( input_layer_A%tree%tree_nodes ) ) deallocate( input_layer_A%tree%tree_nodes )
    if( allocated( input_layer_A%fileName ) ) deallocate( input_layer_A%fileName )
    input_layer_A%n_used = 0
    input_layer_A%n_alloc = 0
    input_layer_A%tree%root_index = 0
    input_layer_A%device_resident = .false.
    input_layer_A%layerState = LAYER_STATE_EVERYTHING
  end subroutine DeleteLayer

  subroutine ClearLayer( input_layer_A )
    type(Layer), intent(inout) :: input_layer_A
    if( allocated( input_layer_A%layer_boxes ) ) deallocate( input_layer_A%layer_boxes )
    if( allocated( input_layer_A%pnumtable%arr ) ) deallocate( input_layer_A%pnumtable%arr )
    if( allocated( input_layer_A%tree%tree_nodes ) ) deallocate( input_layer_A%tree%tree_nodes )
    input_layer_A%n_used = 0
    input_layer_A%n_alloc = 0
    input_layer_A%tree%root_index = 0
    input_layer_A%device_resident = .false.
    input_layer_A%layerState = LAYER_STATE_EVERYTHING
  end subroutine ClearLayer

  subroutine CopyLayer( output_layer, input_layer_A )
    type(Layer), intent(inout) :: output_layer
    type(Layer), intent(in) :: input_layer_A
    character(len=*), parameter :: K_COPY_PREFIX = "MTL_COPY_"
    integer(kind=int64) :: n
    if( input_layer_A%n_used == 0 ) then
       call ClearLayer( output_layer )
       return
    end if
    n = input_layer_A%n_used
    if( allocated( output_layer%layer_boxes ) ) deallocate( output_layer%layer_boxes )
    allocate( output_layer%layer_boxes(n) )
    output_layer%layer_boxes(1:n) = input_layer_A%layer_boxes(1:n)
    output_layer%n_used = n
    output_layer%n_alloc = n
    output_layer%device_resident = .false.
    if( allocated( output_layer%pnumtable%arr ) ) deallocate( output_layer%pnumtable%arr )
    if( allocated( input_layer_A%pnumtable%arr ) ) then
       allocate( output_layer%pnumtable%arr(size(input_layer_A%pnumtable%arr, kind=int64)) )
       output_layer%pnumtable%arr = input_layer_A%pnumtable%arr
    end if
    output_layer%layerState = input_layer_A%layerState
    if( allocated( output_layer%tree%tree_nodes ) ) deallocate( output_layer%tree%tree_nodes )
    if( allocated( input_layer_A%tree%tree_nodes ) ) then
       allocate( output_layer%tree%tree_nodes(size(input_layer_A%tree%tree_nodes, kind=int64)) )
       output_layer%tree%tree_nodes = input_layer_A%tree%tree_nodes
       output_layer%tree%root_index = input_layer_A%tree%root_index
    else
       output_layer%tree%root_index = 0
    end if
    !if(allocated(input_layer_A%fileName)) output_layer%fileName = K_COPY_PREFIX//input_layer_A%fileName
  end subroutine CopyLayer

  subroutine DeleteDesign( input_design )
    type(Design), intent(inout) :: input_design
    integer :: i
    do i=1,hash_nitems( input_design%ht )
       associate( resolved_layer => input_design%layers(i) )
         call ClearLayer( resolved_layer )
       end associate
    end do
    if( allocated( input_design%layers ) )     deallocate( input_design%layers )
    if( allocated( input_design%layerNames ) ) deallocate( input_design%layerNames )

  end subroutine DeleteDesign
  subroutine swap_layer(a, b, filename_as_well)
    ! Assuming you have the appropriate USE statements in the host module:
    ! use iso_fortran_env, only: int64, real64
    ! use iso_c_binding, only: c_int
    implicit none

    type(Layer), intent(inout) :: a, b
    logical, intent(in)        :: filename_as_well
    ! 1. Temporaries for Scalars
    integer(kind=int64) :: temp_int64
    integer(kind=c_int) :: temp_c_int
    real(kind=real64)   :: temp_real64
    integer             :: temp_int
    logical             :: temp_logical

    ! 2. Temporaries for Allocatables (O(1) pointer swap via move_alloc)
    type(Box), allocatable        :: temp_boxes(:)
    type(RTreeNode), allocatable  :: temp_nodes(:)
    integer(kind=int64), allocatable :: temp_pnum(:)
    character(len=:), allocatable :: temp_fileName
    type(ConjugateLayer)          :: temp_paired_layer

    ! --- Swap Scalars (Standard swap) ---
    temp_int64 = a%n_used;   a%n_used = b%n_used;   b%n_used = temp_int64
    temp_int64 = a%n_alloc;  a%n_alloc = b%n_alloc; b%n_alloc = temp_int64

    temp_c_int = a%layerState; a%layerState = b%layerState; b%layerState = temp_c_int

    temp_real64 = a%area;           a%area = b%area;           b%area = temp_real64
    temp_real64 = a%perimeter;      a%perimeter = b%perimeter; b%perimeter = temp_real64

    temp_int = a%iunit; a%iunit = b%iunit; b%iunit = temp_int
    temp_logical = a%device_resident; a%device_resident = b%device_resident; b%device_resident = temp_logical

    ! --- Swap Allocatables (O(1) Pointer Swap via move_alloc) ---
    call move_alloc(a%layer_boxes, temp_boxes)
    call move_alloc(b%layer_boxes, a%layer_boxes)
    call move_alloc(temp_boxes,    b%layer_boxes)
    if( filename_as_well ) then
       call move_alloc(a%fileName, temp_fileName)
       call move_alloc(b%fileName, a%fileName)
       call move_alloc(temp_fileName, b%fileName)
    end if
    ! --- Swap Tree & UnionFind in O(1) without deep array copy ---
    temp_int64 = a%tree%root_index; a%tree%root_index = b%tree%root_index; b%tree%root_index = temp_int64
    call move_alloc(a%tree%tree_nodes, temp_nodes)
    call move_alloc(b%tree%tree_nodes, a%tree%tree_nodes)
    call move_alloc(temp_nodes,        b%tree%tree_nodes)

    call move_alloc(a%pnumtable%arr, temp_pnum)
    call move_alloc(b%pnumtable%arr, a%pnumtable%arr)
    call move_alloc(temp_pnum,       b%pnumtable%arr)

    temp_paired_layer = a%paired_layer; a%paired_layer = b%paired_layer; b%paired_layer = temp_paired_layer

  end subroutine swap_layer
end module DesignModule


