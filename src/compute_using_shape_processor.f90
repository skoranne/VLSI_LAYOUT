!> File    : compute_using_shape_processor.f90
!> Author  : Sandeep Koranne (C) 2026.
!> Purpose : Object oriented and functional interface combination for code reuse

submodule (DesignModule) DesignImplShapeModule
  use iso_fortran_env, only: int64, real64
  use CommonModule
  use GeometryModule
  use MortonSortOMT
  use RTreeBuilderGPU
  use GPUMergeModule
  use ShapeProcessorModule

  implicit none

contains


  module subroutine ProcessRectRoots(layer_boxes, permutation, pnumtable, segments, processor)
    use ShapeProcessorModule, only: ShapeProcessor_t
    ! (Import your Segment_t type as well)
    implicit none

    type(Box_t), intent(in)                :: layer_boxes(:)
    integer, intent(in)                    :: permutation(:)
    integer, intent(in)                    :: pnumtable(:)
    type(Segment_t), intent(in)            :: segments(:)
    class(ShapeProcessor_t), intent(inout) :: processor

    integer :: i, starting_segment
    integer :: start_idx, end_idx, poly_id

    if (size(segments) == 0) return

    ! ---------------------------------------------------------
    ! 1. Identify and process singleton RECTANGLES
    ! ---------------------------------------------------------
    start_idx = segments(1)%start_idx
    end_idx   = segments(1)%end_idx

    if (pnumtable(permutation(end_idx)) == 0) then
       ! Sanity check from original code
       if (pnumtable(permutation(start_idx)) /= 0) then
          error stop "END_IDX=0, but START_IDX /= 0"
       end if

       ! Dispatch the vector-subscripted array to the processor
       call processor%process_rectangles( &
            layer_boxes(permutation(start_idx : end_idx)) &
            )
       starting_segment = 2
    else
       starting_segment = 1
    end if

    ! ---------------------------------------------------------
    ! 2. Iterate and process interacting POLYGONS
    ! ---------------------------------------------------------
    do i = starting_segment, size(segments)
       start_idx = segments(i)%start_idx
       end_idx   = segments(i)%end_idx
       poly_id   = pnumtable(permutation(start_idx))

       ! Consistency checks
       if (poly_id == 0 .or. pnumtable(permutation(end_idx)) == 0) then
          error stop "INCONSISTENT BUCKET numbering detected"
       end if
       if (pnumtable(permutation(end_idx)) /= poly_id) then
          error stop "INCONSISTENT polygon numbering detected"
       end if

       ! Dispatch the subset to the processor
       call processor%process_polygon( &
            poly_id, &
            layer_boxes(permutation(start_idx : end_idx)) &
            )
    end do

  end subroutine ProcessRectRoots
end submodule DesignImplShapeModule
