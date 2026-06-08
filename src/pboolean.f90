! File    : pboolean.f90
! Author  : Sandeep Koranne (C)
! Purpose : Booleans based on lapcount

module polygon_boolean_mod
  use iso_fortran_env, only: int64, int32
  implicit none
  integer, parameter :: K = int64
  ! 1. The new ShapeCollection Type
  type, public :: ShapeCollection
     integer(int32), allocatable :: X(:), Y(:)
     integer(int32), allocatable :: poly_start(:), poly_end(:)
  end type ShapeCollection

  ! 2. Event Type (Upgraded with an 'owner' flag)
  type :: Event
     integer(K) :: x
     integer(K) :: y1, y2
     integer(K) :: lap_change
     integer :: owner ! 1 for Shape A, 2 for Shape B
  end type Event

  public :: PolygonBooleanAND

contains

  subroutine PolygonBooleanAND(A, B, C)
    type(ShapeCollection), intent(in)  :: A, B
    type(ShapeCollection), intent(out) :: C
    !integer, parameter :: K = int64
    type(Event), allocatable :: events(:)
    integer(kind=K), allocatable  :: y_vals(:), unique_y(:)
    integer, allocatable     :: lap_A(:), lap_B(:)
    logical, allocatable     :: is_inside(:)
    integer(kind=K), allocatable  :: start_x(:)

    integer :: num_events, num_y, i, j, j1, j2
    integer(kind=K) :: current_x
    logical :: new_inside

    ! Dynamic output buffers (oversized for safety, trimmed at the end)
    integer(int32), allocatable :: out_X(:), out_Y(:)
    integer(int32), allocatable :: out_start(:), out_end(:)
    integer :: out_vert_count, out_poly_count

    ! Allocate maximum possible bounds
    allocate(events(size(A%X) + size(B%X)))
    allocate(y_vals(size(A%X) + size(B%X)))
    num_events = 0
    num_y = 0

    ! ==========================================
    ! 1. Extract Events & Assign Ownership
    ! ==========================================
    call extract_shape_events(A, 1, events, num_events, y_vals, num_y)
    call extract_shape_events(B, 2, events, num_events, y_vals, num_y)

    if (num_events == 0) return

    ! ==========================================
    ! 2. Coordinate Compression
    ! ==========================================
    call sort_int_array(y_vals(1:num_y))

    allocate(unique_y(num_y))
    j = 1
    unique_y(1) = y_vals(1)
    do i = 2, num_y
       if (y_vals(i) /= unique_y(j)) then
          j = j + 1
          unique_y(j) = y_vals(i)
       end if
    end do
    num_y = j

    ! ==========================================
    ! 3. Boolean Sweep-Line Core
    ! ==========================================
    call sort_events(events(1:num_events))

    allocate(lap_A(num_y - 1), lap_B(num_y - 1))
    allocate(is_inside(num_y - 1), start_x(num_y - 1))
    lap_A = 0; lap_B = 0
    is_inside = .false.; start_x = 0

    ! Allocate temporary output buffers
    allocate(out_X(num_events * 5), out_Y(num_events * 5))
    allocate(out_start(num_events), out_end(num_events))
    out_vert_count = 0
    out_poly_count = 0

    i = 1
    do while (i <= num_events)
       current_x = events(i)%x

       ! A. Process ALL events that occur at this exact X coordinate simultaneously
       do while (i <= num_events .and. events(i)%x == current_x)
          j1 = binary_search_y(unique_y, num_y, events(i)%y1)
          j2 = binary_search_y(unique_y, num_y, events(i)%y2)

          if (events(i)%owner == 1) then
             lap_A(j1 : j2-1) = lap_A(j1 : j2-1) + events(i)%lap_change
          else
             lap_B(j1 : j2-1) = lap_B(j1 : j2-1) + events(i)%lap_change
          end if
          i = i + 1
       end do

       ! B. Check for State Changes (The Boolean AND Logic)
       do j = 1, num_y - 1
          ! THE BOOLEAN CONDITION: Change this to implement OR, NOT, XOR
          new_inside = (lap_A(j) > 0 .and. lap_B(j) > 0)

          if (new_inside .and. .not. is_inside(j)) then
             ! Segment just entered the intersection. Mark the start X.
             start_x(j) = current_x
             is_inside(j) = .true.

          else if (.not. new_inside .and. is_inside(j)) then
             ! Segment just exited the intersection. Emit a closed polygon strip.
             ! The strip goes from start_x(j) to current_x, between unique_y(j) and unique_y(j+1)

             out_poly_count = out_poly_count + 1
             out_start(out_poly_count) = out_vert_count + 1

             ! Point 1: Bottom Left
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(start_x(j), int32)
             out_Y(out_vert_count) = int(unique_y(j), int32)

             ! Point 2: Bottom Right
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(current_x, int32)
             out_Y(out_vert_count) = int(unique_y(j), int32)

             ! Point 3: Top Right
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(current_x, int32)
             out_Y(out_vert_count) = int(unique_y(j+1), int32)

             ! Point 4: Top Left
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(start_x(j), int32)
             out_Y(out_vert_count) = int(unique_y(j+1), int32)

             ! Point 5: Close the loop (Bottom Left again)
             out_vert_count = out_vert_count + 1
             out_X(out_vert_count) = int(start_x(j), int32)
             out_Y(out_vert_count) = int(unique_y(j), int32)

             out_end(out_poly_count) = out_vert_count
             is_inside(j) = .false.
          end if
       end do
    end do

    ! ==========================================
    ! 4. Finalize Output ShapeCollection (Trim arrays)
    ! ==========================================
    allocate(C%X(out_vert_count), C%Y(out_vert_count))
    allocate(C%poly_start(out_poly_count), C%poly_end(out_poly_count))

    if (out_vert_count > 0) then
       C%X = out_X(1:out_vert_count)
       C%Y = out_Y(1:out_vert_count)
       C%poly_start = out_start(1:out_poly_count)
       C%poly_end = out_end(1:out_poly_count)
    end if

  end subroutine PolygonBooleanAND

  ! --- Helper to process orientation and populate the shared event queue ---
  subroutine extract_shape_events(shape, owner_id, events, num_events, y_vals, num_y)
    type(ShapeCollection), intent(in) :: shape
    integer, intent(in) :: owner_id
    type(Event), intent(inout) :: events(:)
    integer(kind=K), intent(inout) :: y_vals(:)
    integer, intent(inout) :: num_events, num_y

    integer :: p, s, e, kloop, is_ccw
    integer(kind=K) :: signed_area, dy

    if (.not. allocated(shape%poly_start)) return

    do p = 1, size(shape%poly_start)
       s = shape%poly_start(p)
       e = shape%poly_end(p)

       ! 1. Calculate Signed Area to determine Orientation
       signed_area = 0
       do kloop = s, e - 1
          signed_area = signed_area + &
               (int64(shape%X(kloop)) * int64(shape%Y(kloop+1)) - int64(shape%X(kloop+1)) * int64(shape%Y(kloop)))
       end do

       if (signed_area > 0) then
          is_ccw = 1
       else if (signed_area < 0) then
          is_ccw = -1
       else
          cycle 
       end if

       ! 2. Generate edges tagged with the owner_id
       do kloop = s, e - 1
          if (shape%X(kloop) == shape%X(kloop+1) .and. shape%Y(kloop) /= shape%Y(kloop+1)) then
             num_events = num_events + 1
             events(num_events)%owner = owner_id
             events(num_events)%x = int(shape%X(kloop), K)
             events(num_events)%y1 = min(int(shape%Y(kloop), K), int(shape%Y(kloop+1), K))
             events(num_events)%y2 = max(int(shape%Y(kloop), K), int(shape%Y(kloop+1), K))

             num_y = num_y + 1
             y_vals(num_y) = events(num_events)%y1
             num_y = num_y + 1
             y_vals(num_y) = events(num_events)%y2

             dy = int64(shape%Y(kloop+1)) - int64(shape%Y(kloop))
             if (dy > 0) then
                events(num_events)%lap_change = -1 * is_ccw
             else
                events(num_events)%lap_change =  1 * is_ccw
             end if
          end if
       end do
    end do
  end subroutine extract_shape_events

  ! --- Keep your existing binary_search_y, sort_int_array, and sort_events here ---
  ! ... (Paste from previous code) ...

end module polygon_boolean_mod
