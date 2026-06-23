! File   : interaction.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: The RTree for GPU has to be thought from groudup in NVFORTRAN GPU
!        : as there are several challenges, such as non-allocate, no procs.
module Test
  use CommonModule
  use GeometryModule
  use DesignModule
  use RTReeBuilder
  use KLDataModule
  use RTreeBuilderGPU
  use MortonSortModule  
  use MortonSortOMT
  use SystemInformationModule
  use PNumMergeModule
  use GPUMergeModule
  use DatastructuresModule
  !use RLEMergeModule
  use iso_fortran_env, only: int32, int64, real64
  use omp_lib

contains
  subroutine PerformMergeGPUMRE(sorted_boxes, num_boxes, root_index, tree_nodes, num_nodes)
    type(Box), intent(in) :: sorted_boxes(num_boxes)
    integer(kind=int64), intent(in) :: num_boxes, root_index, num_nodes
    type(Box), intent(in) :: tree_nodes(num_nodes)   ! pretend a leaf‑only tree
    integer(kind=int32) :: global_edge_count
    integer(kind=int32), parameter :: CHUNK_SIZE = 256
    integer(kind=int32), parameter :: LIMIT_EDGES = CHUNK_SIZE*50
    integer(kind=int32) :: i, j, idx
    integer(kind=int32), allocatable :: d_edges(:,:)

    allocate(d_edges(2, LIMIT_EDGES))
    global_edge_count = 0

    !$omp target teams distribute parallel do &
    !$omp   map(to: sorted_boxes(1:num_boxes)) &
    !$omp   map(from: d_edges(1:2, 1:LIMIT_EDGES)) &
    !$omp   map(tofrom: global_edge_count) &
    !$omp   private(j, idx)
    do i = 1, num_boxes
       ! The inner loop runs sequentially inside each thread
       do j = i+1, num_boxes
          if (abs(sorted_boxes(i)%X1 - sorted_boxes(j)%X1) < 1.0) then

             ! Safely capture the global counter across all threads
             !$omp atomic capture
             idx = global_edge_count
             global_edge_count = global_edge_count + 1
             !$omp end atomic

             if (idx < LIMIT_EDGES) then
                d_edges(1, idx+1) = i
                d_edges(2, idx+1) = j
             end if
          end if
       end do
    end do

    print *, "edges generated:", global_edge_count
  end subroutine PerformMergeGPUMRE
end module Test

program main
  use Test
  implicit none

  type(Box), pointer     :: boxes(:)
  type(Box)              :: bbox
  type(Layer),target     :: input_layer
  integer(kind=int64)    :: N
  integer(kind=int64)    :: total_nodes_cpu, total_nodes_gpu
  integer(kind=int64)    :: interaction_count_cpu, interaction_count_gpu
  integer(kind=int64), parameter :: K_LEAF_CAPACITY_GPU = K_LEAF_CAPACITY
  type(RTreeNodeGPU), allocatable:: TreeNodes(:)
  integer(kind=int64) :: RootIndex
  character(len=256)            :: filenameA, filenameB      
  character(len=256)            :: outFileName   
  integer                       :: control_parameter(4)
  integer                       :: iostat, file_unit
  integer                :: narg, i, j
  character(len=256)            :: arg_string    ! temporary buffer for the 2nd argument
  integer(kind=int64)    :: num_roots, num_rects
  type(UnionFind) :: uf        
  real(kind=real64) :: overlap_area_cpu, overlap_area_gpu
  real(kind=real64) :: overlap_perimeter_cpu, overlap_perimeter_gpu
  integer(kind=int64)  :: max_edges 
  integer, parameter :: K_MAX_TREE_DEPTH = 1024
  integer(kind=int64) :: num_boxes, num_nodes, num_singletons_gpu, num_singletons_cpu, num_squares
  integer(kind=int64) :: limit_edges, global_edge_count
  logical, allocatable :: is_singleton_gpu(:)
  logical, allocatable :: is_singleton_cpu(:)

  narg = command_argument_count()
  call get_command_argument(1, filenameA, status=iostat)   ! allocates automatically
  if (iostat /= 0) then
     write (*,*) "ERROR: 1st argument must be a filename."
     stop 2
  end if
  write (*,*) 'Reading 1st filename: ', trim(filenameA)
  call get_command_argument(2, outFileName, status=iostat)   ! allocates automatically
  if (iostat /= 0) then
     write (*,*) "ERROR: 3rd argument must be a filename."
     stop 2
  end if
  ! ---- third argument: integer (max number of layers) ----------
  do i=1,4
     call get_command_argument(2+i, arg_string, status=iostat)
     if (iostat /= 0) then
        write (*,*) "ERROR: 4th argument must be an integer."
        stop 2
     end if
     read (arg_string, *, iostat=iostat) control_parameter(i)
     if (iostat /= 0 .or. control_parameter(i) < 0) then
        write (*,*) "ERROR: CONTROL must be a non‑negative integer."
        stop 3
     end if
  end do

  max_edges = 10000
  call LoadKLBin(filenameA, input_layer%layer_boxes)
  boxes => input_layer%layer_boxes
  N = size( boxes )
  input_layer%n_used = N
  call InitSystem()
  !> CPU based runs first
  if( size(input_layer%layer_boxes) == 0 ) error stop "INPUT_LAYER size has become 0"
  call StartMarkTime("CPUSortTree")
  total_nodes_cpu = CalculateTotalNodes( input_layer%n_used, K_LEAF_CAPACITY )
  write(*,*) 'TOTAL NODES FOR CPU RT = ', total_nodes_cpu
  allocate( input_layer%tree%tree_nodes( total_nodes_cpu ) )
  num_squares = count( is_square(boxes) )
  if( .false. .and. num_squares*1.0_real64 / (N*1.0_real64) > K_SQUARE_DOMINATION_THRESHOLD ) then
     write(*,*) 'Layer is SQUARE dominated, ', num_squares, ' / ', size(boxes)
     call MortonSort( input_layer%layer_boxes )
  else
     call SortBoxesDirect( boxes, N )     
     !call omt_pack( input_layer%layer_boxes , K_LEAF_CAPACITY )
  end if
  
  input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_SORT )
  call BuildRTree( input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes, input_layer%tree%root_index)
  input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_RTREE )
  call StopMarkTime("CPUSortTree")
  call StartMarkTime("CPUInteractions")
  !>   ComputeInteractionsCPU( tree_nodes, sorted_boxes, root_index, interaction_count )
  call ComputeInteractionsCPU( input_layer%tree%tree_nodes, total_nodes_cpu, input_layer%layer_boxes,N,&
       input_layer%tree%root_index, interaction_count_cpu )
  write(*,*) '|CPU TOTAL INTERACTIONS| = ', interaction_count_cpu
  call StopMarkTime("CPUInteractions")
  call StartMarkTime("CPUSingletons")  
  allocate( is_singleton_cpu( N ) )  
  !>   FindSingletonsCPU(sorted_boxes, tree_nodes, root_index, is_singleton, num_singletons)
  call FindSingletonsCPU( input_layer%layer_boxes, input_layer%tree%tree_nodes, input_layer%tree%root_index,&
       is_singleton_cpu, num_singletons_cpu)
  write(*,*) '|CPU NUM_SINGLETONS| = ', num_singletons_cpu
  call StopMarkTime("CPUSingleton")
  
  call StartMarkTime("CPUPNUM")
  overlap_area_cpu = 0
  overlap_perimeter_cpu = 0
  call PerformMerge( input_layer%pnumtable, input_layer%layer_boxes, K_LEAF_CAPACITY, input_layer%tree%tree_nodes,&
       input_layer%tree%root_index, overlap_area_cpu, overlap_perimeter_cpu)
  write(*,'(A,F18.4,A,F18.4)') 'CPU OVLP AREA = ', overlap_area_cpu, ' CPU OVLP PERIMETER = ', overlap_perimeter_cpu  
  input_layer%layerState = ior( input_layer%layerState, LAYER_STATE_PNUM )
  
  call input_layer%pnumtable%expand_roots()
  
  num_roots = input_layer%pnumtable%count_roots()
  num_rects = count(input_layer%pnumtable%arr == 0)
  if( num_rects == input_layer%n_used ) then
     if( num_roots /= 0 ) error stop "INCONSISTENT ROOT/RECT count."
  end if
  write(*,*) '|Roots| = ', num_roots, '|Rects| = ', num_rects
  call StopMarkTime("CPUPNUM")
  
  write(*,*) '===============================GPU MODE ========================================'
  allocate( is_singleton_gpu( N ) )
  boxes => input_layer%layer_boxes  
  is_singleton_gpu = .false.
  total_nodes_gpu = CalculateTotalNodesGPU( N, K_LEAF_CAPACITY_GPU ) !> for GPU we might change
  bbox = mbr_of_array( boxes, N )
  write(*,'(A,I12,A,4I12,A,I12)') 'Loaded : ', N, ' BBOX = ', bbox, ' |T| = ', total_nodes_gpu
  call StartMarkTime("GPUSort")  
  call SortBoxesDirect( boxes, N )
  call StopMarkTime("GPUSort")
  call StartMarkTime("GPURTree")    
  allocate( TreeNodes( total_nodes_gpu ) )
  call BuildRTreeGPU( boxes, K_LEAF_CAPACITY_GPU, TreeNodes, RootIndex)
  write(*,*) 'Tree constructed: ', RootIndex, ' |RT| = ', size(TreeNodes)
  call StopMarkTime("GPURTree")  
  call StartMarkTime("GPUInteractions")
  call ComputeInteractionsGPU( TreeNodes, total_nodes_gpu, boxes, N, RootIndex, interaction_count_gpu)
  write(*,*) '|GPU TOTAL INTERACTIONS| = ', interaction_count_gpu
  call StopMarkTime("GPUInteractions")
  call StartMarkTime("GPUSingleton")
  call FindSingletonsGPU( boxes, TreeNodes, RootIndex, is_singleton_gpu, num_singletons_gpu)
  write(*,*) '|GPU NUM_SINGLETONS| = ', num_singletons_gpu
  call StopMarkTime("GPUSingleton")
  call StartMarkTime("PNUM")
  overlap_area_gpu = 0
  overlap_perimeter_gpu = 0 !> if perimeter comes back zero => there was finite overlap, not just touch
  !call PerformMergeGPU(uf, boxes, N, K_LEAF_CAPACITY, TreeNodes, total_nodes, RootIndex, overlap_area, overlap_perimeter)
  !call PerformMergeGPU(boxes, N, RootIndex, boxes, total_nodes)
  write(*,'(A,F18.4,A,F18.4)') 'GPU OVLP AREA = ', overlap_area_gpu, ' GPU OVLP PERIMETER = ', overlap_perimeter_gpu
  call StopMarkTime("PNUM")
  call StartMarkTime("PNUMRLE")
  overlap_area_gpu = 0
  overlap_perimeter_gpu = 0  
  !call PerformRLEMergeGPU(uf, boxes, K_LEAF_CAPACITY, TreeNodes, RootIndex, overlap_area, overlap_perimeter)
  write(*,'(A,F18.4,A,F18.4)') 'RLE GPU OVLP AREA = ', overlap_area_gpu, ' RLE GPU OVLP PERIMETER = ', overlap_perimeter_gpu
  call StopMarkTime("PNUMRLW")
  if( num_singletons_gpu /= num_singletons_cpu ) then
     write(*,*) 'CPU |SINGLETON| /= GPU |SINGLETON|'
     error stop
  end if
  if( interaction_count_gpu /= interaction_count_cpu ) then
     write(*,*) 'CPU |INTERACTION| /= GPU |INTERACTION|'
     error stop
  end if
  
end program main
