! File   : de9im.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Calculate the spatial topological DE-9IM model
!Given two RTrees A and B both containing boxes, say set_A, and set_B, we want
!to calculates the DE9IM Egenhofer matrix or atleast the main topological relations
!between the connected components (called polygon numbers) of set_A with polygon
!numbers of set_B. We can do search(RTreeA, b_i) to get all boxes of set_A which
!potentially interact with box b_i. This is sufficient to calculate the DE9IM matrix.
!Write a modern Fortran function to calculate the DE9IM for each polygon of
!set_A given the RTree A, and B, as well as the set of boxes in A.
! https://en.wikipedia.org/wiki/DE-9IM
module de9im_spatial_mod
  implicit none
  private

  ! Standard precision definitions
  integer, parameter :: dp = kind(0.0d0)

  ! DE-9IM Dimensions
  integer, parameter :: DIM_EMPTY = -1
  integer, parameter :: DIM_POINT = 0
  integer, parameter :: DIM_LINE  = 1
  integer, parameter :: DIM_AREA  = 2

  ! Expose public types and methods
  public :: Box_t, PolygonRelation_t, DE9IM_Matrix_t
  public :: calculate_polygon_de9im, print_de9im, check_relation

  !===================================================================
  ! Derived Types
  !===================================================================

  ! Represents an Axis-Aligned Bounding Box belonging to a polygon
  type :: Box_t
     integer :: id
     integer :: polygon_id
     real(dp) :: xmin, ymin
     real(dp) :: xmax, ymax
  end type Box_t

  ! Represents the 3x3 DE-9IM Matrix
  ! Indices: 1=Interior, 2=Boundary, 3=Exterior
  type :: DE9IM_Matrix_t
     integer :: m(3,3) = DIM_EMPTY
   contains
     procedure :: update => update_de9im_matrix
  end type DE9IM_Matrix_t

  ! Stores the relation between Polygon A and Polygon B
  type :: PolygonRelation_t
     integer :: poly_id_A
     integer :: poly_id_B
     type(DE9IM_Matrix_t) :: matrix
  end type PolygonRelation_t

  ! Abstract interface for your existing RTree search routine.
  ! You will link this to your actual RTree library.
  abstract interface
     subroutine rtree_search_func(tree, query_box, results, n_results)
       import :: Box_t
       ! Opaque type for your RTree
       type(*), intent(in) :: tree
       type(Box_t), intent(in) :: query_box
       type(Box_t), allocatable, intent(out) :: results(:)
       integer, intent(out) :: n_results
     end subroutine rtree_search_func
  end interface

contains

  !===================================================================
  ! Core Subroutine: Computes the relations between Polygons
  !===================================================================
  ! Loops over set_B, queries RTree A, and builds the DE-9IM matrix 
  ! for all intersecting polygons.
  subroutine calculate_polygon_de9im(set_B, rtree_A, search_rtree, relations, n_relations)
    type(Box_t), intent(in)          :: set_B(:)
    type(*), intent(in)              :: rtree_A
    procedure(rtree_search_func)     :: search_rtree
    type(PolygonRelation_t), allocatable, intent(out) :: relations(:)
    integer, intent(out)             :: n_relations

    integer :: i, j, k
    type(Box_t), allocatable :: overlapping_boxes_A(:)
    integer :: n_found
    integer :: pA, pB
    type(DE9IM_Matrix_t) :: local_mat
    logical :: relation_exists

    ! Temporary dynamic array to hold unique polygon relationships
    ! In a production code with millions of polygons, replace this 
    ! linear search list with a Hash Map or sparse matrix format.
    type(PolygonRelation_t), allocatable :: temp_relations(:)
    integer :: max_rels

    max_rels = 1000 
    allocate(temp_relations(max_rels))
    n_relations = 0

    ! "We can do search(RTreeA, b_i) to get all boxes of set_A..."
    do i = 1, size(set_B)

       ! 1. Query RTree A with box from B
       call search_rtree(rtree_A, set_B(i), overlapping_boxes_A, n_found)

       pB = set_B(i)%polygon_id

       ! 2. Compute interactions
       do j = 1, n_found
          pA = overlapping_boxes_A(j)%polygon_id

          ! Calculate bounding box level DE-9IM
          local_mat = compute_box_de9im(overlapping_boxes_A(j), set_B(i))

          ! 3. Aggregate into the Polygon-Polygon Relation
          relation_exists = .false.
          do k = 1, n_relations
             if (temp_relations(k)%poly_id_A == pA .and. &
                  temp_relations(k)%poly_id_B == pB) then

                ! Merge maximum dimensional overlap into the existing polygon relation
                call temp_relations(k)%matrix%update(local_mat)
                relation_exists = .true.
                exit
             end if
          end do

          if (.not. relation_exists) then
             n_relations = n_relations + 1
             ! Reallocate if capacity exceeded (simple dynamic array logic)
             if (n_relations > size(temp_relations)) then
                temp_relations = reallocate_relations(temp_relations, size(temp_relations)*2)
             end if
             temp_relations(n_relations)%poly_id_A = pA
             temp_relations(n_relations)%poly_id_B = pB
             temp_relations(n_relations)%matrix = local_mat
          end if
       end do

       if (allocated(overlapping_boxes_A)) deallocate(overlapping_boxes_A)
    end do

    ! Truncate and output
    allocate(relations(n_relations))
    relations(1:n_relations) = temp_relations(1:n_relations)
    deallocate(temp_relations)

  end subroutine calculate_polygon_de9im

  !===================================================================
  ! Helper: DE-9IM calculation for two intersecting 2D boxes
  !===================================================================
  function compute_box_de9im(A, B) result(mat)
    type(Box_t), intent(in) :: A, B
    type(DE9IM_Matrix_t) :: mat
    real(dp) :: cx1, cx2, cy1, cy2
    integer :: int_dim

    ! Start with disjoint assumption
    mat%m = DIM_EMPTY
    mat%m(3,3) = DIM_AREA ! Exterior always intersects Exterior in 2D

    ! Calculate intersection bounds
    cx1 = max(A%xmin, B%xmin)
    cx2 = min(A%xmax, B%xmax)
    cy1 = max(A%ymin, B%ymin)
    cy2 = min(A%ymax, B%ymax)

    ! Disjoint condition
    if (cx1 > cx2 .or. cy1 > cy2) then
       mat%m(1,3) = DIM_AREA; mat%m(2,3) = DIM_LINE
       mat%m(3,1) = DIM_AREA; mat%m(3,2) = DIM_LINE
       return
    end if

    ! Calculate intersection dimension
    if (cx1 < cx2 .and. cy1 < cy2) then
       int_dim = DIM_AREA
    else if (cx1 == cx2 .and. cy1 == cy2) then
       int_dim = DIM_POINT
    else
       int_dim = DIM_LINE
    end if

    ! Populate standard matrices based on the intersection dimension
    ! (Simplified exact AABB topological matrix evaluation)
    select case (int_dim)
    case (DIM_AREA)
       mat%m(1,1) = DIM_AREA  ! I(A) cap I(B)
       mat%m(1,2) = DIM_LINE  ! I(A) cap B(B)
       mat%m(2,1) = DIM_LINE  ! B(A) cap I(B)
       mat%m(2,2) = DIM_LINE  ! B(A) cap B(B)
       mat%m(1,3) = DIM_AREA  ! I(A) cap E(B)
       mat%m(3,1) = DIM_AREA  ! E(A) cap I(B)
    case (DIM_LINE)
       mat%m(2,2) = DIM_LINE  ! B(A) cap B(B)
       mat%m(1,3) = DIM_AREA
       mat%m(3,1) = DIM_AREA
    case (DIM_POINT)
       mat%m(2,2) = DIM_POINT ! B(A) cap B(B)
       mat%m(1,3) = DIM_AREA
       mat%m(3,1) = DIM_AREA
    end select

  end function compute_box_de9im

  !===================================================================
  ! Aggregator: Updates Matrix for Unions (Polygons)
  !===================================================================
  subroutine update_de9im_matrix(this, other)
    class(DE9IM_Matrix_t), intent(inout) :: this
    type(DE9IM_Matrix_t), intent(in)     :: other
    integer :: r, c

    ! For unions of geometry, the intersection dimension of the 
    ! union is the maximum intersection dimension of its components.
    do r = 1, 3
       do c = 1, 3
          this%m(r,c) = max(this%m(r,c), other%m(r,c))
       end do
    end do
  end subroutine update_de9im_matrix

  !===================================================================
  ! Evaluators: Translates DE-9IM Matrix to standard readable predicates
  !===================================================================
  logical function check_relation(mat, relation_type)
    type(DE9IM_Matrix_t), intent(in) :: mat
    character(len=*), intent(in)     :: relation_type

    check_relation = .false.
    select case(relation_type)
    case ('INTERSECTS')
       ! T******** or *T******* or ***T***** or ****T****
       check_relation = (mat%m(1,1) >= 0 .or. mat%m(1,2) >= 0 .or. &
            mat%m(2,1) >= 0 .or. mat%m(2,2) >= 0)
    case ('TOUCHES')
       ! FT*******, F**T*****, F***T****
       check_relation = (mat%m(1,1) == DIM_EMPTY) .and. &
            (mat%m(1,2) >= 0 .or. mat%m(2,1) >= 0 .or. mat%m(2,2) >= 0)
    case ('CONTAINS')
       ! T*TFF*FF*
       check_relation = (mat%m(1,1) >= 0 .and. mat%m(1,3) >= 0 .and. &
            mat%m(2,2) == DIM_EMPTY .and. mat%m(2,3) == DIM_EMPTY)
    case default
       ! Implement others (WITHIN, OVERLAPS, EQUALS) as needed...
    end select
  end function check_relation

  ! Array reallocation helper
  function reallocate_relations(arr, new_size) result(new_arr)
    type(PolygonRelation_t), allocatable, intent(in) :: arr(:)
    integer, intent(in) :: new_size
    type(PolygonRelation_t), allocatable :: new_arr(:)

    allocate(new_arr(new_size))
    new_arr(1:size(arr)) = arr
  end function reallocate_relations

  ! Debug utility
  subroutine print_de9im(mat)
    type(DE9IM_Matrix_t), intent(in) :: mat
    integer :: i
    print *, "DE-9IM Matrix:"
    do i = 1, 3
       print '(3(I2, 1X))', mat%m(i,:)
    end do
  end subroutine print_de9im

end module de9im_spatial_mod
