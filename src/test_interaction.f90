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
  call LoadKLBin("b.bin", boxes)
  N = size( boxes )
  total_nodes = CalculateTotalNodesGPU( N, K_LEAF_CAPACITY_GPU ) !> for GPU we might change
  bbox = mbr_of_array( boxes, N )
  write(*,*) 'Loaded : ', N, ' BBOX = ', bbox, ' |T| = ', total_nodes
  call SortBoxesDirect( boxes, N )
  allocate( TreeNodes( total_nodes ) )
  call BuildRTreeGPU( boxes, K_LEAF_CAPACITY_GPU, TreeNodes, RootIndex)
  write(*,*) 'Tree constructed: ', RootIndex, ' |RT| = ', size(TreeNodes)
  write(*,'(4I8)') boxes(1:10)
  call StartMarkTime("RTree")
  interaction_count = ComputeInteractionsGPU( TreeNodes, boxes, RootIndex)
  write(*,*) '|TOTAL INTERACTIONS| = ', interaction_count
  call StopMarkTime("RTree")
  !public :: ComputeInteractionsGPU
end program main
