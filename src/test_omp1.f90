module tiny
  use iso_fortran_env, only: int64, int32, real64
  implicit none

  type :: Box
     real(real64) :: X1, Y1, X2, Y2
  end type Box

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
  !$omp target data map(to: sorted_boxes(1:num_boxes), &
  !$omp                tree_nodes(1:num_nodes)) &
  !$omp               map(tofrom: d_edges) &
  !$omp               map(tofrom: global_edge_count)

  do i = 1, num_boxes
     !$omp target teams distribute parallel do &
     !$omp   private(j, idx) &
     !$omp   reduction(+:global_edge_count)
     do j = i+1, num_boxes
        if (abs(sorted_boxes(i)%X1 - sorted_boxes(j)%X1) < 1.0) then
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
  !$omp end target data

  print *, "edges generated:", global_edge_count
end subroutine PerformMergeGPU

end module tiny
