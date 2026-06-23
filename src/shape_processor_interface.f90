!> File    : shape_processor_interface.f90
!> Author  : Sandeep Koranne (C) 2026.
!> Purpose : Object oriented and functional interface combination for code reuse

module ShapeProcessorModule
  use CommonModule
  use GeometryModule
  use iso_fortran_env, only: int32, int64, real64
  implicit none

  ! Abstract base class: The "Function Object" interface
  type, abstract :: ShapeProcessor_t
   contains
     procedure(process_rects_if), deferred, pass(this) :: process_rectangles
     procedure(process_poly_if), deferred, pass(this)  :: process_polygon
  end type ShapeProcessor_t

  ! Interface definitions for the deferred procedures
  abstract interface
     subroutine process_rects_if(this, boxes)
       import :: ShapeProcessor_t, Box
       class(ShapeProcessor_t), intent(inout) :: this
       type(Box), intent(in)                :: boxes(:)
     end subroutine process_rects_if

     subroutine process_poly_if(this, poly_id, boxes)
       import :: ShapeProcessor_t, Box
       class(ShapeProcessor_t), intent(inout) :: this
       integer, intent(in)                    :: poly_id
       type(Box), intent(in)                :: boxes(:)
     end subroutine process_poly_if
  end interface

end module ShapeProcessorModule
