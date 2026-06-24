!> File    : shape_processor_example.f90
!> Author  : Sandeep Koranne (C) 2026.
!> Purpose : Object oriented and functional interface combination for code reuse

module ShapeProcessorExampleModule
  use ShapeProcessorModule
  use CommonModule
  use GeometryModule
  use iso_fortran_env, only: int32, int64, real64

  implicit none

  ! The Concrete Function Object containing your State
  type, extends(ShapeProcessor_t) :: MyHealProcessor_t
     integer :: final_count = 0
     integer :: final_capacity = 10000
     type(Box), allocatable :: final_boxes(:)
     ! Add other state variables like num_squares, dominated_by_squares
   contains
     procedure, pass(this) :: process_rectangles => impl_process_rectangles
     procedure, pass(this) :: process_polygon    => impl_process_polygon
  end type MyHealProcessor_t

contains

  subroutine impl_process_rectangles(this, boxes)
    class(MyHealProcessor_t), intent(inout) :: this
    type(Box), intent(in)                 :: boxes(:)

    integer :: count
    count = size(boxes)

    ! Directly assign boxes to the state
    this%final_boxes(this%final_count + 1 : this%final_count + count) = boxes
    this%final_count = this%final_count + count

    ! (Calculate your num_squares here)
  end subroutine impl_process_rectangles

  subroutine impl_process_polygon(this, poly_id, boxes)
    class(MyHealProcessor_t), intent(inout) :: this
    integer, intent(in)                     :: poly_id
    type(Box), intent(in)                 :: boxes(:)

    integer :: updated_box_count
    type(Box), allocatable :: healed_boxes(:)

    ! Call your heal_boxes routine
    ! call heal_boxes(size(boxes), boxes, updated_box_count, healed_boxes)

    ! Append healed_boxes to this%final_boxes and update this%final_count
  end subroutine impl_process_polygon

end module ShapeProcessorExampleModule
