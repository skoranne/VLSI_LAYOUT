! File    : pnummerge.f90
! Author  : Sandeep Koranne (C) All rights reserved.
! Purpose : OpenMP example for complex reduction
!In modern Fortran I am writing a function ST(boxes) which find pairwise intersection of
! box 'i' with 'j' and then uses an UnionFind data structure function called merge(i,j)
!to merge the indices together. The problem is that the pairwise intersection is happening
!inside a OpenMP parallel for do loop; how can I synchronize the merge(i,j) without using
!atomics or copying the whole unionfind array for each thread and merging a reduction later.
!Is there any better idea for this ?

module PNumMergeModule
  use iso_fortran_env, only : int32, int64, real64
  use GeometryModule
  use RTreeBuilder
  use DataStructuresModule
  use omp_lib
  implicit none  
  ! Explicitly define what is exposed to the rest of the program
  public :: EdgeBuffer, init_buffer, push_edge, PerformMerge
  type :: EdgeBuffer
     integer(8), allocatable :: pairs(:,:) ! 2 x Capacity
     integer(8) :: count
     integer(8) :: capacity
  end type EdgeBuffer
contains
  ! ---------------------------------------------------------
  ! Initializes the thread-local edge buffer
  ! ---------------------------------------------------------
  subroutine init_buffer(buffer, initial_capacity)
    type(EdgeBuffer), intent(out) :: buffer
    integer(8), intent(in) :: initial_capacity

    buffer%capacity = initial_capacity
    buffer%count = 0

    ! Fix the first dimension to 2 here
    allocate(buffer%pairs(2, buffer%capacity))
  end subroutine init_buffer
  subroutine push_edge(buffer, i, j)
    type(EdgeBuffer), intent(inout) :: buffer
    integer(8), intent(in) :: i, j

    integer(8) :: new_capacity
    integer(8), allocatable :: temp_pairs(:,:)

    ! 1. Check if the buffer has reached its capacity limit
    if (buffer%count == buffer%capacity) then
       ! Standard geometric growth to maintain O(1) amortized insertion
       new_capacity = buffer%capacity * 2
       ! Allocate a temporary array with the new capacity
       allocate(temp_pairs(2, new_capacity))
       ! Copy existing data using Fortran's optimized array slicing
       temp_pairs(:, 1:buffer%count) = buffer%pairs(:, 1:buffer%count)
       ! Efficiently transfer the allocation descriptor from temp to buffer.
       ! This automatically deallocates the old buffer%pairs memory and 
       ! points buffer%pairs to the new memory block.
       call move_alloc(from=temp_pairs, to=buffer%pairs)
       ! Update the capacity tracker
       buffer%capacity = new_capacity
    end if
    ! 2. Increment the edge count
    buffer%count = buffer%count + 1
    ! 3. Store the intersecting indices
    buffer%pairs(1, buffer%count) = i
    buffer%pairs(2, buffer%count) = j
  end subroutine push_edge

  subroutine process_edges(uf,eb)
    ! We fix the first dimension to 2, and leave the second as assumed-shape
    type(UnionFind), intent(inout) :: uf
    type(EdgeBuffer), intent(in) :: eb
    integer :: k
    do k = 1, eb%count
       call uf%insert( eb%pairs(1,k) )
       call uf%insert( eb%pairs(2, k) )
       !write(*,*) 'Merging: ', eb%pairs(1, k), ' with ', eb%pairs(2, k)
       call uf%merge(eb%pairs(1, k), eb%pairs(2, k))
    end do
  end subroutine process_edges

  subroutine PerformMerge(uf, sorted_boxes, capacity, tree_nodes, root_index, overlap_area, overlap_perimeter)
    type(UnionFind), intent(out) :: uf        
    type(Box), intent(in) :: sorted_boxes(:)
    integer(kind=int64), intent(in) :: capacity
    type(RTreeNode), intent(in) :: tree_nodes(:)
    integer(kind=int64), intent(in) :: root_index
    real(kind=real64), intent(out)   :: overlap_area
    real(kind=real64), intent(out)   :: overlap_perimeter    
    integer(kind=int64) :: num_boxes
    integer(kind=int64) :: i
    integer(kind=int64) :: leafboxes(K_MAX_SEARCH_LEAVES) ! better choose a large number
    integer(kind=int64) :: number_leaves
    integer(kind=int64) :: j, k
    type(EdgeBuffer), allocatable :: buffers(:)
    real(kind=real64),allocatable :: overlap_areas(:)
    real(kind=real64),allocatable :: overlap_perimeters(:)    
    type(Box) :: tempBox
    integer :: nthreads, tid


    nthreads = omp_get_max_threads()
    allocate(buffers(nthreads))
    allocate(overlap_areas(nthreads))
    allocate(overlap_perimeters(nthreads))    
    do i=1,nthreads
       call init_buffer(buffers(i), initial_capacity=int(10000,kind=int64))
       overlap_areas(i) = 0.0
       overlap_perimeters(i) = 0.0
    end do
    overlap_area = 0.0
    overlap_perimeter = 0.0
    num_boxes = size( sorted_boxes )
    call uf%init( num_boxes )    
    !write(*,*) 'DBG: ', num_boxes, ' ', size(tree_nodes), ' ', uf%arr
    #ifdef TARGET_CODE
    !$komp target loop private(leafboxes, number_leaves, i, j, k, tid, tempBox)
    !$komp target teams distribute parallel do schedule(dynamic) &
    !$komp   private(leafboxes, number_leaves, i, j, k, tid, tempBox) &
    !$komp   map(to: sorted_boxes, tree_nodes, capacity, root_index, num_boxes) &
    !$komp   map(tofrom: buffers, overlap_areas, overlap_perimeter)
    #endif
    !> we may have to do schedule dynamic:     !$omp do schedule(dynamic)
    !$omp parallel do private(leafboxes, number_leaves, i, j, k, tid, tempBox)
    do i=1,num_boxes
       number_leaves = 0
       leafboxes = 0
       tid = omp_get_thread_num()+1
       call SearchTree( tree_nodes, root_index, sorted_boxes(i), leafboxes, number_leaves )
       !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
       if( number_leaves > 0 ) then
          !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
          outer: do j=1,number_leaves
             over_leaves: do k=leafboxes(j),min(leafboxes(j)+capacity-1, num_boxes)
                !do k=leafboxes(j),leafboxes(j)+capacity-1
                if( i < k .and. box_interact( sorted_boxes(i), sorted_boxes(k)) ) then
                   !tempBox = sorted_boxes(i) * sorted_boxes(k)
                   tempBox%x1 = max( sorted_boxes(i)%x1, sorted_boxes(k)%x1)
                   tempBox%y1 = max( sorted_boxes(i)%y1, sorted_boxes(k)%y1)
                   tempBox%x2 = min( sorted_boxes(i)%x2, sorted_boxes(k)%x2)
                   tempBox%y2 = min( sorted_boxes(i)%y2, sorted_boxes(k)%y2)                   
                   !$komp critical (console_io)
                   !write(*,*) 'Index ',i, ' ', sorted_boxes(i), ' interacts with ', k, ' ', sorted_boxes(k), &
                   !     ' * ', tempBox, box_area( tempBox ), box_perimeter( tempBox )
                   !$komp end critical (console_io)
                   call push_edge(buffers(tid), i, k) ! Reallocates if capacity exceeded
                   if( box_area( tempBox ) > 0.0 ) then
                      overlap_areas(tid) = overlap_areas(tid) + box_area( tempBox )
                   else
                      !> good, we stay in overlap free regime, but now since the boxes
                      !> are known to interact, we MUST have non-zero perimeter
                      if( tempBox%x1 == tempBox%x2 .or. tempBox%y1 == tempBox%y2 ) then
                         !> ok
                      else
                         !error stop "OVERLAP DETECTED"
                      end if
                      overlap_perimeters(tid) = overlap_perimeters(tid) + box_perimeter( tempBox )
                   end if
                end if
             end do over_leaves
          end do outer
       end if
    end do
    !$komp end target teams distribute parallel do
    ! The global Union-Find array is updated strictly sequentially
    do tid = 1, nthreads
       call process_edges( uf, buffers(tid) )
       overlap_area = overlap_area + overlap_areas(tid)
       overlap_perimeter = overlap_perimeter + overlap_perimeters(tid)       
    end do
    if( overlap_area > 0.0 ) then
       overlap_perimeter = 0.0
    end if
    call uf%fullreduce()
  end subroutine PerformMerge

end module PNumMergeModule


