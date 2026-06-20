! File   : interaction.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: The RTree for GPU has to be thought from groudup in NVFORTRAN GPU
!        : as there are several challenges, such as non-allocate, no procs.
program main
  use CommonModule
  use GeometryModule
  use KLDataModule
  use RTreeBuilderGPU
  use MortonSortOMT
  use SystemInformationModule
  use GPUMergeModule
  use DatastructuresModule
  use iso_fortran_env, only: int32, int64, real64
  use omp_lib
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
  call FindSingletonsGPU( boxes, TreeNodes, RootIndex, is_singleton, num_singletons)
  write(*,*) '|NUM_SINGLETONS| = ', num_singletons
  call StopMarkTime("Singleton")
  call StartMarkTime("PNUM")  
  !call PerformMergeGPU(uf, boxes, K_LEAF_CAPACITY, TreeNodes, RootIndex, overlap_area, overlap_perimeter)
  call StopMarkTime("PNUM")    

end program main
