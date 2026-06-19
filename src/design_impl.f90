! File   : design_impl.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Use submodule to move large blocks of code out of "interfaces"

!=====================================================================
!  design_impl.f90   –  submodule containing the heavy code
!=====================================================================
submodule (DesignModule) DesignImplModule
  use iso_fortran_env, only: int64, real64
  use CommonModule
  use GeometryModule  
  implicit none

contains

  !=================================================================
  !  CreateGrid – implementation (exactly the code from the previous
  !  answer, unchanged except that we are now inside a submodule)
  !=================================================================
  module subroutine CreateGrid( input_layer, output_layer, rows, cols )
    type(Layer), intent(in)    :: input_layer    
    type(Layer), intent(inout) :: output_layer    
    integer,     intent(in)    :: rows, cols
    type(Box)                  :: input_box
    integer(kind=K_COORDINATE_KIND) :: x_lo, x_hi, y_lo, y_hi
    integer(kind=K_COORDINATE_KIND) :: full_w, full_h
    integer(kind=K_COORDINATE_KIND) :: col_width, row_height
    integer(kind=K_COORDINATE_KIND) :: extra_w, extra_h
    integer(kind=int64)             :: i, j, idx, new_alloc
    type(Box)                       :: subbox
    type(Box), allocatable          :: temp(:)
    input_box = mbr_of_array( input_layer%layer_boxes, input_layer%n_used )
    if( allocated( output_layer%layer_boxes ) ) then
       deallocate( output_layer%layer_boxes )
       output_layer%n_used = 0
       output_layer%n_alloc = 0
    end if
    
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
    new_alloc = rows*cols
    allocate( output_layer%layer_boxes( new_alloc ) )
    output_layer%n_alloc = new_alloc
    output_layer%n_used  = rows*cols
    !-----------------------------------------------------------------
    !  6. Fill the sub‑boxes
    !-----------------------------------------------------------------
    idx = 1
    do i = 1, rows
       block
         integer(kind=K_COORDINATE_KIND) :: y0, y1
         y0 = y_lo + i*row_height + min(i, extra_h)
         y1 = y0 + row_height - int(1,kind=K_COORDINATE_KIND)
         if (i < extra_h) y1 = y1 + int(1,kind=K_COORDINATE_KIND)

         do j = 1, cols
            block
              integer(kind=K_COORDINATE_KIND) :: x0, x1
              x0 = x_lo + j*col_width + min(j, extra_w)
              x1 = x0 + col_width - int(1,kind=K_COORDINATE_KIND)
              if (j < extra_w) x1 = x1 + int(1,kind=K_COORDINATE_KIND)
              subbox%x1 = x0
              subbox%y1 = y0
              subbox%x2 = x1
              subbox%y2 = y1
              output_layer%layer_boxes(idx) = subbox
              idx = idx + 1
            end block
         end do
       end block
    end do
    output_layer%n_used = rows*cols
    call PreprocessLayer( output_layer )
  end subroutine CreateGrid
  
  module subroutine CreateEXTENT( input_layer, output_layer )
    type(Layer),      intent(in)    :: input_layer         
    type(Layer),      intent(inout) :: output_layer
    if( allocated( output_layer%layer_boxes ) ) then
       if( size( output_layer%layer_boxes ) /= 0 ) error stop "|LB| /= 0"
       if( output_layer%n_used /= 0  ) error stop "|NU| /= 0"
       if( output_layer%n_alloc /= 0 ) error stop "|NA| /= 0"
       deallocate( output_layer%layer_boxes ) !> we checked n_alloc and size are both 0
    end if
    allocate( output_layer%layer_boxes(1) )
    output_layer%layer_boxes(1) = mbr_of_array( input_layer%layer_boxes, input_layer%n_used )
    if( .not. output_layer%layer_boxes(1)%is_valid() ) error stop
    output_layer%n_used = 1
    output_layer%layerState = LAYER_STATE_HEAL
    output_layer%layerState = ior( output_layer%layerState, LAYER_STATE_SORT )
    call PreprocessLayer( output_layer )
  end subroutine CreateEXTENT

end submodule DesignImplModule
