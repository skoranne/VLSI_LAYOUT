! File   : geometry_impl.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Use submodule to move large blocks of code out of "interfaces"

!=====================================================================
!  geometry_impl.f90   –  submodule containing the heavy code
!=====================================================================
submodule (GeometryModule) GeometryImplModule
  implicit none
contains

  !=================================================================
  !  CreateGrid – implementation (exactly the code from the previous
  !  answer, unchanged except that we are now inside a submodule)
  !=================================================================
  subroutine CreateGrid( input_box, output_layer, rows, cols )
    type(Box),   intent(in)    :: input_box
    type(Layer), intent(inout) :: output_layer
    integer,     intent(in)    :: rows, cols

    integer(kind=K_COORDINATE_KIND) :: x_lo, x_hi, y_lo, y_hi
    integer(kind=K_COORDINATE_KIND) :: full_w, full_h
    integer(kind=K_COORDINATE_KIND) :: col_width, row_height
    integer(kind=K_COORDINATE_KIND) :: extra_w, extra_h
    integer                         :: i, j, idx
    type(Box)                        :: subbox

    !-----------------------------------------------------------------
    !  1. sanity checks
    !-----------------------------------------------------------------
    if (rows <= 0 .or. cols <= 0) then
       error stop "CreateGrid: rows and cols must be positive"
    end if

    if (.not. input_box%is_valid()) then
       error stop "CreateGrid: input_box is not a valid rectangle"
    end if

    !-----------------------------------------------------------------
    !  2. Normalise the limits – we want x1 <= x2, y1 <= y2
    !-----------------------------------------------------------------
    x_lo = min(input_box%x1, input_box%x2)
    x_hi = max(input_box%x1, input_box%x2)
    y_lo = min(input_box%y1, input_box%y2)
    y_hi = max(input_box%y1, input_box%y2)

    !-----------------------------------------------------------------
    !  3. Compute the total width / height (inclusive)
    !-----------------------------------------------------------------
    full_w = x_hi - x_lo + 1_K_COORDINATE_KIND   ! number of integer columns
    full_h = y_hi - y_lo + 1_K_COORDINATE_KIND   ! number of integer rows

    !-----------------------------------------------------------------
    !  4. Base size + remainder distribution
    !-----------------------------------------------------------------
    col_width = full_w / cols
    extra_w   = mod(full_w, cols)

    row_height = full_h / rows
    extra_h    = mod(full_h, rows)

    !-----------------------------------------------------------------
    !  5. Grow the destination array if necessary
    !-----------------------------------------------------------------
    if (output_layer%n_alloc < output_layer%n_used + rows*cols) then
       integer(kind=8) :: new_alloc
       new_alloc = max( int(2*output_layer%n_alloc, kind=8), &
            output_layer%n_used + rows*cols )
       if (new_alloc < rows*cols) new_alloc = rows*cols   ! first allocation

       if (allocated(output_layer%layer_boxes)) then
          ! Preserve existing data
          call move_alloc( output_layer%layer_boxes, &
               output_layer%layer_boxes )
          allocate( output_layer%layer_boxes(new_alloc) )
       else
          allocate( output_layer%layer_boxes(new_alloc) )
       end if
       output_layer%n_alloc = new_alloc
    end if

    !-----------------------------------------------------------------
    !  6. Fill the sub‑boxes
    !-----------------------------------------------------------------
    idx = output_layer%n_used          ! first free slot (0‑based in our logic)
    do i = 0, rows-1
       integer(kind=K_COORDINATE_KIND) :: y0, y1
       y0 = y_lo + i*row_height + min(i, extra_h)
       y1 = y0 + row_height - 1_K_COORDINATE_KIND
       if (i < extra_h) y1 = y1 + 1_K_COORDINATE_KIND

       do j = 0, cols-1
          integer(kind=K_COORDINATE_KIND) :: x0, x1
          x0 = x_lo + j*col_width + min(j, extra_w)
          x1 = x0 + col_width - 1_K_COORDINATE_KIND
          if (j < extra_w) x1 = x1 + 1_K_COORDINATE_KIND

          subbox%x1 = x0
          subbox%y1 = y0
          subbox%x2 = x1
          subbox%y2 = y1

          output_layer%layer_boxes(idx+1) = subbox
          idx = idx + 1
       end do
    end do

    output_layer%n_used = idx
  end subroutine CreateGrid

end submodule GeometryImplModule
