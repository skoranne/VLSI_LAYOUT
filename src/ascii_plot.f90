module ASCIIPlotModule
  use CommonModule
  use GeometryModule
  use iso_fortran_env, only: int64
contains
  subroutine ascii_plot_boxes(boxes)
    type(Box), intent(in) :: boxes(:)
    type(Box) :: tempBox
    integer(kind=K_COORDINATE_KIND) :: b, x, y
    integer(kind=K_COORDINATE_KIND) :: min_x, max_x, min_y, max_y
    integer(kind=K_COORDINATE_KIND) :: total_cols, total_rows
    ! Grid Canvas Dimensions (Adjust these to fit your terminal window width/height)
    integer(kind=K_COORDINATE_KIND), parameter :: CANVAS_WIDTH  = 80
    integer(kind=K_COORDINATE_KIND), parameter :: CANVAS_HEIGHT = 40

    ! Map indices 
    integer(kind=K_COORDINATE_KIND) :: sx1, sy1, sx2, sy2
    integer(kind=K_COORDINATE_KIND) :: lbl_x, lbl_y
    character(len=1), allocatable :: canvas(:,:)
    character(len=3) :: label_str
    integer, parameter :: K_MAX_PLOT_SIZE = 150
    tempBox = mbr_of_array(boxes, int(size(boxes), kind=int64))
    min_x = tempBox%x1
    max_x = tempBox%x2
    min_y = tempBox%y1
    max_y = tempBox%y2
    if( size(boxes) == 0 ) then
       error stop "BOX set is EMPTY"
    else if( size(boxes) > K_MAX_PLOT_SIZE ) then
       write(*,*) 'Too many boxes for ASCII plot: ', size(boxes)
       return
    end if
    
    
    total_cols = max_x - min_x
    total_rows = max_y - min_y

    ! 2. Allocate and initialize clean canvas with spaces
    allocate(canvas(CANVAS_WIDTH, CANVAS_HEIGHT))
    canvas = ' '

    ! 3. Rasterize each box onto the character array grid
    do b = 1, size(boxes)
       ! Scale coordinates linearly to canvas view limits
       sx1 = nint(real(boxes(b)%x1 - min_x) / real(total_cols) * real(CANVAS_WIDTH - 1)) + 1
       sx2 = nint(real(boxes(b)%x2 - min_x) / real(total_cols) * real(CANVAS_WIDTH - 1)) + 1
       sy1 = nint(real(boxes(b)%y1 - min_y) / real(total_rows) * real(CANVAS_HEIGHT - 1)) + 1
       sy2 = nint(real(boxes(b)%y2 - min_y) / real(total_rows) * real(CANVAS_HEIGHT - 1)) + 1

       ! Draw Horizontal boundaries
       do x = sx1, sx2
          if (canvas(x, sy1) == ' ' .or. canvas(x, sy1) == '|') canvas(x, sy1) = '-'
          if (canvas(x, sy2) == ' ' .or. canvas(x, sy2) == '|') canvas(x, sy2) = '-'
       end do

       ! Draw Vertical boundaries
       do y = sy1, sy2
          if (canvas(sx1, y) == ' ' .or. canvas(sx1, y) == '-') canvas(sx1, y) = '|'
          if (canvas(sx2, y) == ' ' .or. canvas(sx2, y) == '-') canvas(sx2, y) = '|'
       end do

       ! Stamp corner intersections
       canvas(sx1, sy1) = '+'
       canvas(sx2, sy1) = '+'
       canvas(sx1, sy2) = '+'
       canvas(sx2, sy2) = '+'

       ! Insert Box identifier tag inside the box
       write(label_str, '(A,I1,A)') 'B', b, ' '
       lbl_x = (sx1 + sx2) / 2
       lbl_y = (sy1 + sy2) / 2
       if (lbl_x > 0 .and. lbl_x <= CANVAS_WIDTH .and. lbl_y > 0 .and. lbl_y <= CANVAS_HEIGHT) then
          canvas(lbl_x, lbl_y) = label_str(1:1)
          if (lbl_x + 1 <= CANVAS_WIDTH) canvas(lbl_x+1, lbl_y) = label_str(2:2)
       end if
    end do

    ! 4. Output the grid inverted (Top down so Y increases upwards)
    do y = CANVAS_HEIGHT, 1, -1
       ! Print row content safely
       write(*, '(A)', advance='no') '|'
       do x = 1, CANVAS_WIDTH
          write(*, '(A)', advance='no') canvas(x, y)
       end do
       write(*, '(A)') '|'
    end do

  end subroutine ascii_plot_boxes
end module ASCIIPlotModule

