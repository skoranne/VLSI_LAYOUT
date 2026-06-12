! vector_template.inc
! Expected Macros:
!   KIND_VAL  : The kind parameter (e.g., int32)
!   PROC_NAME : The subroutine name (e.g., push_32)

subroutine PROC_NAME(this, val)
    ! We use type() instead of class() to keep nvfortran happy
    type(Vector(KIND_VAL)), intent(inout) :: this
    integer(kind=KIND_VAL), intent(in)    :: val
    
    integer(kind=KIND_VAL), allocatable   :: temp(:)

    ! Dynamic allocation logic
    if (.not. allocated(this%arr)) then
        allocate(this%arr(10))
    else if (this%count == size(this%arr)) then
        allocate(temp(size(this%arr) * 2))
        temp(1:this%count) = this%arr
        call move_alloc(from=temp, to=this%arr)
    end if

    this%count = this%count + 1
    this%arr(this%count) = val
end subroutine PROC_NAME