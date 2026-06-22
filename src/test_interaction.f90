! File   : interaction.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: The RTree for GPU has to be thought from groudup in NVFORTRAN GPU
!        : as there are several challenges, such as non-allocate, no procs.
module Test
  use CommonModule
  use GeometryModule
  use KLDataModule
  use RTreeBuilderGPU
  use MortonSortOMT
  use SystemInformationModule
  !use GPUMergeModule
  use DatastructuresModule
  !use RLEMergeModule
  use iso_fortran_env, only: int32, int64, real64
  use omp_lib

contains
  subroutine PerformMergeGPU(sorted_boxes, num_boxes, root_index, tree_nodes, num_nodes)
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
end subroutine PerformMergeGPU
end module Test

program main
  use Test
  implicit none

  type(Box), allocatable :: boxes(:)
  type(Box)              :: bbox
  integer(kind=int64)    :: N
  integer(kind=int64)    :: total_nodes
  integer(kind=int64)    :: interaction_count  
  integer(kind=int64), parameter :: K_LEAF_CAPACITY_GPU = K_LEAF_CAPACITY
  type(RTreeNodeGPU), allocatable:: TreeNodes(:)
  integer(kind=int64) :: RootIndex

  type(UnionFind) :: uf        
  real(kind=real64) :: overlap_area
  real(kind=real64) :: overlap_perimeter
  integer(kind=int64)  :: max_edges 
  integer, parameter :: K_MAX_TREE_DEPTH = 1024
  integer(kind=int64) :: num_boxes, num_nodes, num_singletons
  integer(kind=int64) :: limit_edges, global_edge_count
  logical, allocatable :: is_singleton(:)
  max_edges = 10000
  call LoadKLBin("b.bin", boxes)
  N = size( boxes )
  allocate( is_singleton( N ) )
  is_singleton = .false.
  total_nodes = CalculateTotalNodesGPU( N, K_LEAF_CAPACITY_GPU ) !> for GPU we might change
  bbox = mbr_of_array( boxes, N )
  write(*,*) 'Loaded : ', N, ' BBOX = ', bbox, ' |T| = ', total_nodes
  call SortBoxesDirect( boxes, N )
  allocate( TreeNodes( total_nodes ) )
  call BuildRTreeGPU( boxes, K_LEAF_CAPACITY_GPU, TreeNodes, RootIndex)
  write(*,*) 'Tree constructed: ', RootIndex, ' |RT| = ', size(TreeNodes)
  write(*,'(4I8)') boxes(1:10)
  call StartMarkTime("RTree")
  call ComputeInteractionsGPU( TreeNodes, total_nodes, boxes, N, RootIndex, interaction_count)
  write(*,*) '|TOTAL INTERACTIONS| = ', interaction_count
  call StopMarkTime("RTree")
  call StartMarkTime("Singleton")
  !call FindSingletonsGPU( boxes, TreeNodes, RootIndex, is_singleton, num_singletons)
  write(*,*) '|NUM_SINGLETONS| = ', num_singletons
  call StopMarkTime("Singleton")
  call StartMarkTime("PNUM")
  overlap_area = 0
  overlap_perimeter = 0 !> if perimeter comes back zero => there was finite overlap, not just touch
  !call PerformMergeGPU(uf, boxes, N, K_LEAF_CAPACITY, TreeNodes, total_nodes, RootIndex, overlap_area, overlap_perimeter)
  call PerformMergeGPU(boxes, N, RootIndex, boxes, total_nodes)
  write(*,*) 'OVLP AREA = ', overlap_area, ' OVLP PERIMETER = ', overlap_perimeter
  call StopMarkTime("PNUM")
  call StartMarkTime("PNUMRLE")
  overlap_area = 0
  overlap_perimeter = 0  
  !call PerformRLEMergeGPU(uf, boxes, K_LEAF_CAPACITY, TreeNodes, RootIndex, overlap_area, overlap_perimeter)
  write(*,*) 'OVLP AREA = ', overlap_area, ' OVLP PERIMETER = ', overlap_perimeter  
  call StopMarkTime("PNUMRLW")
end program main
