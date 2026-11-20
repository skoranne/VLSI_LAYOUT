! File    : magviewer.f90
! Author  : Sandeep Koranne (C) 2026
! Purpose : Idiomatic OpenGL/GLFW viewer for Magic, GDSII, OASIS, KLBIN, and SNAP layouts

module DrawUsingOpenGL
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding
  use CommonModule
  use GeometryModule
  use DesignModule
  use RTreeBuilder
  use glf90w
  implicit none
  private

  integer, parameter :: K_MAX_OPENGL_CAPACITY = 500000
  integer, parameter :: K_DFS_STACK_SIZE      = 256

  interface
     subroutine glClear(mask) bind(C, name="glClear")
       import :: c_int
       integer(c_int), value :: mask
     end subroutine glClear

     subroutine glClearColor(red, green, blue, alpha) bind(C, name="glClearColor")
       import :: c_float
       real(c_float), value :: red, green, blue, alpha
     end subroutine glClearColor

     subroutine glBegin(mode) bind(C, name="glBegin")
       import :: c_int
       integer(c_int), value :: mode
     end subroutine glBegin

     subroutine glEnd() bind(C, name="glEnd")
     end subroutine glEnd

     subroutine glColor3f(red, green, blue) bind(C, name="glColor3f")
       import :: c_float
       real(c_float), value :: red, green, blue
     end subroutine glColor3f

     subroutine glColor4f(red, green, blue, alpha) bind(C, name="glColor4f")
       import :: c_float
       real(c_float), value :: red, green, blue, alpha
     end subroutine glColor4f

     subroutine glBlendFunc(sfactor, dfactor) bind(C, name="glBlendFunc")
       import :: c_int
       integer(c_int), value :: sfactor, dfactor
     end subroutine glBlendFunc

     subroutine glVertex2f(x, y) bind(C, name="glVertex2f")
       import :: c_float
       real(c_float), value :: x, y
     end subroutine glVertex2f

     subroutine glEnable(cap) bind(C, name="glEnable")
       import :: c_int
       integer(c_int), value :: cap
     end subroutine glEnable

     subroutine glDisable(cap) bind(C, name="glDisable")
       import :: c_int
       integer(c_int), value :: cap
     end subroutine glDisable

     subroutine glViewport(x, y, width, height) bind(C, name="glViewport")
       import :: c_int
       integer(c_int), value :: x, y, width, height
     end subroutine glViewport

     subroutine glMatrixMode(mode) bind(C, name="glMatrixMode")
       import :: c_int
       integer(c_int), value :: mode
     end subroutine glMatrixMode

     subroutine glLoadIdentity() bind(C, name="glLoadIdentity")
     end subroutine glLoadIdentity

     subroutine glOrtho(left, right, bottom, top, zNear, zFar) bind(C, name="glOrtho")
       import :: c_double
       real(c_double), value :: left, right, bottom, top, zNear, zFar
     end subroutine glOrtho

     subroutine glEnableClientState(array_type) bind(C, name="glEnableClientState")
       import :: c_int
       integer(c_int), value :: array_type
     end subroutine glEnableClientState

     subroutine glDisableClientState(array_type) bind(C, name="glDisableClientState")
       import :: c_int
       integer(c_int), value :: array_type
     end subroutine glDisableClientState

     subroutine glVertexPointer(size, array_type, stride, pointer) bind(C, name="glVertexPointer")
       import :: c_int, c_ptr
       integer(c_int), value :: size, array_type, stride
       type(c_ptr), value :: pointer
     end subroutine glVertexPointer

     subroutine glDrawArrays(mode, first, count) bind(C, name="glDrawArrays")
       import :: c_int
       integer(c_int), value :: mode, first, count
     end subroutine glDrawArrays

     subroutine glPolygonMode(face, mode) bind(C, name="glPolygonMode")
       import :: c_int
       integer(c_int), value :: face, mode
     end subroutine glPolygonMode
  end interface

  ! OpenGL constants
  integer(c_int), parameter :: GL_COLOR_BUFFER_BIT    = int(z'00004000', c_int)
  integer(c_int), parameter :: GL_QUADS               = int(z'00000007', c_int)
  integer(c_int), parameter :: GL_PROJECTION          = int(z'1701', c_int)
  integer(c_int), parameter :: GL_MODELVIEW           = int(z'1700', c_int)
  integer(c_int), parameter :: GL_VERTEX_ARRAY        = int(z'8074', c_int)
  integer(c_int), parameter :: GL_BLEND               = int(z'0BE2', c_int)
  integer(c_int), parameter :: GL_SRC_ALPHA           = int(z'0302', c_int)
  integer(c_int), parameter :: GL_ONE_MINUS_SRC_ALPHA = int(z'0303', c_int)
  integer(c_int), parameter :: GL_DEPTH_TEST          = int(z'0B71', c_int)
  integer(c_int), parameter :: GL_INT                 = int(z'1404', c_int)
  integer(c_int), parameter :: GL_FRONT_AND_BACK      = int(z'0408', c_int)
  integer(c_int), parameter :: GL_LINE                = int(z'1B01', c_int)
  integer(c_int), parameter :: GL_FILL                = int(z'1B02', c_int)

  ! GLFW key and mouse constants
  integer(c_int), parameter :: GLFW_PRESS             = 1_c_int
  integer(c_int), parameter :: GLFW_MOUSE_BUTTON_LEFT = 0_c_int
  integer(c_int), parameter :: GLFW_KEY_MINUS         = 45_c_int
  integer(c_int), parameter :: GLFW_KEY_SLASH         = 47_c_int
  integer(c_int), parameter :: GLFW_KEY_EQUAL         = 61_c_int
  integer(c_int), parameter :: GLFW_KEY_A             = 65_c_int
  integer(c_int), parameter :: GLFW_KEY_R             = 82_c_int
  integer(c_int), parameter :: GLFW_KEY_S             = 83_c_int
  integer(c_int), parameter :: GLFW_KEY_T             = 84_c_int
  integer(c_int), parameter :: GLFW_KEY_X             = 88_c_int
  integer(c_int), parameter :: GLFW_KEY_Z             = 90_c_int
  integer(c_int), parameter :: GLFW_KEY_ESCAPE        = 256_c_int
  integer(c_int), parameter :: GLFW_KEY_RIGHT         = 262_c_int
  integer(c_int), parameter :: GLFW_KEY_LEFT          = 263_c_int
  integer(c_int), parameter :: GLFW_KEY_DOWN          = 264_c_int
  integer(c_int), parameter :: GLFW_KEY_UP            = 265_c_int
  integer(c_int), parameter :: GLFW_KEY_LEFT_CONTROL  = 341_c_int
  integer(c_int), parameter :: GLFW_KEY_RIGHT_CONTROL = 345_c_int

  type :: Point2D
     real(c_double) :: x = 0.0_c_double
     real(c_double) :: y = 0.0_c_double
  end type Point2D

  type :: CameraState
     real(c_double) :: win_w  = 1.0_c_double
     real(c_double) :: win_h  = 1.0_c_double
     real(c_double) :: pan_x  = 0.0_c_double
     real(c_double) :: pan_y  = 0.0_c_double
     real(c_double) :: zoom   = 1.0_c_double
     real(c_double) :: left   = 0.0_c_double
     real(c_double) :: right  = 1.0_c_double
     real(c_double) :: bottom = 0.0_c_double
     real(c_double) :: top    = 1.0_c_double
  end type CameraState

  type :: InputState
     logical :: r_was_down     = .false.
     logical :: s_was_down     = .false.
     logical :: t_was_down     = .false.
     logical :: slash_was_down = .false.
     logical :: mouse_was_down = .false.
  end type InputState

  type :: DrawnLayer
     type(Layer), pointer        :: input_layer => null()
     logical                     :: visible     = .true.
     real(c_float)               :: color(3)    = [0.8_c_float, 0.3_c_float, 0.3_c_float]
     real(c_float)               :: alpha       = 0.5_c_float
     integer(c_int), allocatable :: vbo_verts(:)
     integer                     :: vbo_count   = 0
     logical                     :: cache_valid = .false.
  end type DrawnLayer

  public :: DrawnLayer, SimpleWindow, FillLayerBoxesDirect, FillLayerMBRsDirect

contains

  pure subroutine ScreenToWorld(screen, cam, world)
    type(Point2D), intent(in)     :: screen
    type(CameraState), intent(in) :: cam
    type(Point2D), intent(out)    :: world
    world%x = cam%left + (screen%x / max(1.0_c_double, cam%win_w)) * (cam%right - cam%left)
    world%y = cam%top  - (screen%y / max(1.0_c_double, cam%win_h)) * (cam%top - cam%bottom)
  end subroutine ScreenToWorld

  subroutine handle_error(code, desc)
    integer, intent(in)      :: code
    character(*), intent(in) :: desc
    write(*, '(A,I8,A,A)') 'GLFW Error ', code, ' : ', desc
  end subroutine handle_error

  pure subroutine UpdateCameraBounds(cam)
    type(CameraState), intent(inout) :: cam
    real(c_double) :: cx, cy, half_w, half_h
    cx = (cam%win_w * 0.5_c_double) + cam%pan_x
    cy = (cam%win_h * 0.5_c_double) + cam%pan_y
    half_w = (cam%win_w * 0.5_c_double) * cam%zoom
    half_h = (cam%win_h * 0.5_c_double) * cam%zoom
    cam%left   = cx - half_w
    cam%right  = cx + half_w
    cam%bottom = cy - half_h
    cam%top    = cy + half_h
  end subroutine UpdateCameraBounds

  pure subroutine FitCameraToMBR(cam, mbr)
    type(CameraState), intent(inout) :: cam
    type(Box), intent(in)            :: mbr
    real(c_double) :: w, h, cx, cy, zx, zy
    w  = max(1.0_c_double, real(mbr%x2, c_double) - real(mbr%x1, c_double))
    h  = max(1.0_c_double, real(mbr%y2, c_double) - real(mbr%y1, c_double))
    cx = 0.5_c_double * (real(mbr%x1, c_double) + real(mbr%x2, c_double))
    cy = 0.5_c_double * (real(mbr%y1, c_double) + real(mbr%y2, c_double))
    zx = w / max(1.0_c_double, cam%win_w)
    zy = h / max(1.0_c_double, cam%win_h)
    cam%zoom  = max(zx, zy) * 1.1_c_double
    cam%pan_x = cx - (cam%win_w * 0.5_c_double)
    cam%pan_y = cy - (cam%win_h * 0.5_c_double)
    call UpdateCameraBounds(cam)
  end subroutine FitCameraToMBR

  pure function CameraToViewBox(cam) result(v)
    type(CameraState), intent(in) :: cam
    type(Box) :: v
    real(c_double) :: lo, hi
    lo = -real(huge(v%x1) - 1, c_double)
    hi =  real(huge(v%x1) - 1, c_double)
    v%x1 = floor(max(lo, min(hi, cam%left)),   kind=K_COORDINATE_KIND)
    v%y1 = floor(max(lo, min(hi, cam%bottom)), kind=K_COORDINATE_KIND)
    v%x2 = ceiling(max(lo, min(hi, cam%right)), kind=K_COORDINATE_KIND)
    v%y2 = ceiling(max(lo, min(hi, cam%top)),   kind=K_COORDINATE_KIND)
  end function CameraToViewBox

  subroutine EnsureVertexCapacity(vbuf, needed_ints)
    integer(c_int), allocatable, intent(inout) :: vbuf(:)
    integer, intent(in) :: needed_ints
    integer(c_int), allocatable :: tmp(:)
    integer :: new_cap
    if (.not. allocated(vbuf)) then
       allocate(vbuf(max(needed_ints, 1024)))
    else if (size(vbuf) < needed_ints) then
       new_cap = max(needed_ints, size(vbuf) * 2)
       allocate(tmp(new_cap))
       tmp(1:size(vbuf)) = vbuf
       call move_alloc(from=tmp, to=vbuf)
    end if
  end subroutine EnsureVertexCapacity

  !> Single-pass iterative R-Tree traversal writing visible boxes directly into the GL quad buffer.
  !> Eliminates intermediate visible_boxes buffer and recursive call overhead, with optional sub-pixel culling.
  subroutine FillLayerBoxesDirect(input_layer, current_view, min_span, vertex_buffer, num_visible)
    type(Layer), intent(in)                    :: input_layer
    type(Box), intent(in)                      :: current_view
    integer(kind=K_COORDINATE_KIND), intent(in):: min_span
    integer(c_int), allocatable, intent(inout) :: vertex_buffer(:)
    integer, intent(out)                       :: num_visible

    integer(kind=int64) :: stack(K_DFS_STACK_SIZE)
    integer :: sp, vidx
    integer(kind=int64) :: curr_idx, child_idx, c_end, k, n_used, root_idx
    type(RTreeNode) :: currNode, childNode
    type(Box) :: b

    num_visible = 0
    n_used = input_layer%n_used
    if (n_used <= 0_int64 .or. .not. allocated(input_layer%layer_boxes)) return

    if (.not. allocated(vertex_buffer)) allocate(vertex_buffer(min(int(n_used), 4096) * 8))

    if (.not. allocated(input_layer%tree%tree_nodes) .or. input_layer%tree%root_index <= 0_int64) then
       ! Linear fallback if no R-Tree exists
       vidx = 1
       do k = 1_int64, n_used
          if (num_visible >= K_MAX_OPENGL_CAPACITY) exit
          b = input_layer%layer_boxes(k)
          if (b%x2 < current_view%x1 .or. b%x1 > current_view%x2 .or. &
              b%y2 < current_view%y1 .or. b%y1 > current_view%y2) cycle
          if (min_span > 0 .and. (b%x2 - b%x1) < min_span .and. (b%y2 - b%y1) < min_span) cycle
          if (vidx + 7 > size(vertex_buffer)) call EnsureVertexCapacity(vertex_buffer, vidx + 7)
          vertex_buffer(vidx)   = b%x1; vertex_buffer(vidx+1) = b%y1
          vertex_buffer(vidx+2) = b%x2; vertex_buffer(vidx+3) = b%y1
          vertex_buffer(vidx+4) = b%x2; vertex_buffer(vidx+5) = b%y2
          vertex_buffer(vidx+6) = b%x1; vertex_buffer(vidx+7) = b%y2
          vidx = vidx + 8
          num_visible = num_visible + 1
       end do
       return
    end if

    root_idx = input_layer%tree%root_index
    childNode = input_layer%tree%tree_nodes(root_idx)
    if (childNode%mbr%x2 < current_view%x1 .or. childNode%mbr%x1 > current_view%x2 .or. &
        childNode%mbr%y2 < current_view%y1 .or. childNode%mbr%y1 > current_view%y2) return

    sp = 1
    stack(1) = root_idx
    vidx = 1

    do while (sp > 0 .and. num_visible < K_MAX_OPENGL_CAPACITY)
       curr_idx = stack(sp)
       sp = sp - 1
       currNode = input_layer%tree%tree_nodes(curr_idx)

       if (currNode%IsLeaf) then
          c_end = min(currNode%ChildStart + int(currNode%NumChildren, kind=int64) - 1_int64, n_used)
          do k = currNode%ChildStart, c_end
             if (num_visible >= K_MAX_OPENGL_CAPACITY) exit
             b = input_layer%layer_boxes(k)
             if (b%x2 < current_view%x1 .or. b%x1 > current_view%x2 .or. &
                 b%y2 < current_view%y1 .or. b%y1 > current_view%y2) cycle
             if (min_span > 0 .and. (b%x2 - b%x1) < min_span .and. (b%y2 - b%y1) < min_span) cycle
             if (vidx + 7 > size(vertex_buffer)) call EnsureVertexCapacity(vertex_buffer, vidx + 7)
             vertex_buffer(vidx)   = b%x1; vertex_buffer(vidx+1) = b%y1
             vertex_buffer(vidx+2) = b%x2; vertex_buffer(vidx+3) = b%y1
             vertex_buffer(vidx+4) = b%x2; vertex_buffer(vidx+5) = b%y2
             vertex_buffer(vidx+6) = b%x1; vertex_buffer(vidx+7) = b%y2
             vidx = vidx + 8
             num_visible = num_visible + 1
          end do
       else
          c_end = currNode%ChildStart + int(currNode%NumChildren, kind=int64) - 1_int64
          do child_idx = currNode%ChildStart, c_end
             childNode = input_layer%tree%tree_nodes(child_idx)
             if (childNode%mbr%x2 < current_view%x1 .or. childNode%mbr%x1 > current_view%x2 .or. &
                 childNode%mbr%y2 < current_view%y1 .or. childNode%mbr%y1 > current_view%y2) cycle
             if (min_span > 0 .and. (childNode%mbr%x2 - childNode%mbr%x1) < min_span .and. &
                 (childNode%mbr%y2 - childNode%mbr%y1) < min_span) cycle
             if (sp < K_DFS_STACK_SIZE) then
                sp = sp + 1
                stack(sp) = child_idx
             end if
          end do
       end if
    end do
  end subroutine FillLayerBoxesDirect

  !> Single-pass iterative R-Tree MBR traversal at target_depth writing directly into the GL quad buffer.
  subroutine FillLayerMBRsDirect(input_layer, current_view, target_depth, vertex_buffer, num_visible)
    type(Layer), intent(in)                    :: input_layer
    type(Box), intent(in)                      :: current_view
    integer, intent(in)                        :: target_depth
    integer(c_int), allocatable, intent(inout) :: vertex_buffer(:)
    integer, intent(out)                       :: num_visible

    integer(kind=int64) :: node_stack(K_DFS_STACK_SIZE)
    integer             :: depth_stack(K_DFS_STACK_SIZE)
    integer             :: sp, cur_d, max_d, vidx
    integer(kind=int64) :: curr_idx, child_idx, c_end, root_idx
    type(RTreeNode)     :: currNode, childNode
    type(Box)           :: b

    num_visible = 0
    if (input_layer%n_used <= 0_int64) return
    if (.not. allocated(input_layer%tree%tree_nodes) .or. input_layer%tree%root_index <= 0_int64) then
       call FillLayerBoxesDirect(input_layer, current_view, 0_K_COORDINATE_KIND, vertex_buffer, num_visible)
       return
    end if

    if (.not. allocated(vertex_buffer)) allocate(vertex_buffer(4096 * 8))
    max_d = max(0, target_depth)
    root_idx = input_layer%tree%root_index
    childNode = input_layer%tree%tree_nodes(root_idx)
    if (childNode%mbr%x2 < current_view%x1 .or. childNode%mbr%x1 > current_view%x2 .or. &
        childNode%mbr%y2 < current_view%y1 .or. childNode%mbr%y1 > current_view%y2) return

    sp = 1
    node_stack(1)  = root_idx
    depth_stack(1) = 0
    vidx = 1

    do while (sp > 0 .and. num_visible < K_MAX_OPENGL_CAPACITY)
       curr_idx = node_stack(sp)
       cur_d    = depth_stack(sp)
       sp = sp - 1
       currNode = input_layer%tree%tree_nodes(curr_idx)

       if (cur_d >= max_d .or. currNode%IsLeaf) then
          b = currNode%mbr
          if (vidx + 7 > size(vertex_buffer)) call EnsureVertexCapacity(vertex_buffer, vidx + 7)
          vertex_buffer(vidx)   = b%x1; vertex_buffer(vidx+1) = b%y1
          vertex_buffer(vidx+2) = b%x2; vertex_buffer(vidx+3) = b%y1
          vertex_buffer(vidx+4) = b%x2; vertex_buffer(vidx+5) = b%y2
          vertex_buffer(vidx+6) = b%x1; vertex_buffer(vidx+7) = b%y2
          vidx = vidx + 8
          num_visible = num_visible + 1
       else
          c_end = currNode%ChildStart + int(currNode%NumChildren, kind=int64) - 1_int64
          do child_idx = currNode%ChildStart, c_end
             childNode = input_layer%tree%tree_nodes(child_idx)
             if (childNode%mbr%x2 < current_view%x1 .or. childNode%mbr%x1 > current_view%x2 .or. &
                 childNode%mbr%y2 < current_view%y1 .or. childNode%mbr%y1 > current_view%y2) cycle
             if (sp < K_DFS_STACK_SIZE) then
                sp = sp + 1
                node_stack(sp)  = child_idx
                depth_stack(sp) = cur_d + 1
             end if
          end do
       end if
    end do
  end subroutine FillLayerMBRsDirect

  subroutine InvalidateLayerCaches(N, input_layers)
    integer, intent(in)             :: N
    type(DrawnLayer), intent(inout) :: input_layers(:)
    integer :: i
    do i = 1, N
       input_layers(i)%cache_valid = .false.
    end do
  end subroutine InvalidateLayerCaches

  subroutine UpdateVisibleLayerCaches(N, input_layers, current_view, tree_mode, target_depth, min_span)
    integer, intent(in)                        :: N, target_depth
    type(DrawnLayer), intent(inout)            :: input_layers(:)
    type(Box), intent(in)                      :: current_view
    logical, intent(in)                        :: tree_mode
    integer(kind=K_COORDINATE_KIND), intent(in):: min_span
    integer :: i

    do i = 1, N
       if (.not. input_layers(i)%visible) cycle
       if (input_layers(i)%cache_valid) cycle
       if (.not. associated(input_layers(i)%input_layer)) then
          input_layers(i)%vbo_count = 0
          input_layers(i)%cache_valid = .true.
          cycle
       end if
       if (tree_mode) then
          call FillLayerMBRsDirect(input_layers(i)%input_layer, current_view, target_depth, &
               input_layers(i)%vbo_verts, input_layers(i)%vbo_count)
       else
          call FillLayerBoxesDirect(input_layers(i)%input_layer, current_view, min_span, &
               input_layers(i)%vbo_verts, input_layers(i)%vbo_count)
       end if
       input_layers(i)%cache_valid = .true.
    end do
  end subroutine UpdateVisibleLayerCaches

  subroutine RenderMainViewport(N, input_layers, ui_width, main_width, win_h, cam, tree_mode)
    integer, intent(in)                    :: N
    type(DrawnLayer), intent(in), target   :: input_layers(:)
    integer(c_int), intent(in)             :: ui_width, main_width, win_h
    type(CameraState), intent(in)          :: cam
    logical, intent(in)                    :: tree_mode
    integer :: i

    call glViewport(ui_width, 0_c_int, main_width, win_h)
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity()
    call glOrtho(cam%left, cam%right, cam%bottom, cam%top, -1.0_c_double, 1.0_c_double)
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity()

    call glEnableClientState(GL_VERTEX_ARRAY)
    do i = 1, N
       if (.not. input_layers(i)%visible) cycle
       if (input_layers(i)%vbo_count <= 0 .or. .not. allocated(input_layers(i)%vbo_verts)) cycle

       call glVertexPointer(2_c_int, GL_INT, 0_c_int, c_loc(input_layers(i)%vbo_verts(1)))

       ! Filled pass
       call glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
       if (tree_mode) then
          call glColor4f(0.8_c_float, 0.3_c_float, 0.3_c_float, 0.85_c_float)
       else
          call glColor4f(input_layers(i)%color(1), input_layers(i)%color(2), &
               input_layers(i)%color(3), input_layers(i)%alpha)
       end if
       call glDrawArrays(GL_QUADS, 0_c_int, int(input_layers(i)%vbo_count * 4, c_int))

       ! Outline pass
       call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
       call glColor4f(0.0_c_float, 0.0_c_float, 0.0_c_float, 1.0_c_float)
       call glDrawArrays(GL_QUADS, 0_c_int, int(input_layers(i)%vbo_count * 4, c_int))
    end do
    call glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    call glDisableClientState(GL_VERTEX_ARRAY)
  end subroutine RenderMainViewport

  subroutine RenderMinimap(mbr, current_view, ui_width, bot_h)
    type(Box), intent(in)      :: mbr, current_view
    integer(c_int), intent(in) :: ui_width, bot_h
    integer(c_int) :: side
    real(c_double) :: mbr_w, mbr_h, mbr_cx, mbr_cy, half_dim
    real(c_double) :: cur_w, cur_h, min_sz, cx, cy, draw_w, draw_h
    real(c_double) :: ix1, iy1, ix2, iy2

    side = max(1_c_int, min(ui_width / 2_c_int, bot_h / 2_c_int))
    call glViewport(0_c_int, 0_c_int, side, side)
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity()

    mbr_w    = max(1.0_c_double, real(mbr%x2, c_double) - real(mbr%x1, c_double))
    mbr_h    = max(1.0_c_double, real(mbr%y2, c_double) - real(mbr%y1, c_double))
    mbr_cx   = 0.5_c_double * (real(mbr%x1, c_double) + real(mbr%x2, c_double))
    mbr_cy   = 0.5_c_double * (real(mbr%y1, c_double) + real(mbr%y2, c_double))
    half_dim = 0.5_c_double * max(mbr_w, mbr_h) * 1.05_c_double
    call glOrtho(mbr_cx - half_dim, mbr_cx + half_dim, &
         mbr_cy - half_dim, mbr_cy + half_dim, -1.0_c_double, 1.0_c_double)

    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity()

    call glColor3f(0.1_c_float, 0.1_c_float, 0.1_c_float)
    call glBegin(GL_QUADS)
    call glVertex2f(real(mbr%x1, c_float), real(mbr%y1, c_float))
    call glVertex2f(real(mbr%x2, c_float), real(mbr%y1, c_float))
    call glVertex2f(real(mbr%x2, c_float), real(mbr%y2, c_float))
    call glVertex2f(real(mbr%x1, c_float), real(mbr%y2, c_float))
    call glEnd()

    cur_w  = real(current_view%x2, c_double) - real(current_view%x1, c_double)
    cur_h  = real(current_view%y2, c_double) - real(current_view%y1, c_double)
    min_sz = (mbr_w + mbr_h) * 0.025_c_double
    cx     = 0.5_c_double * (real(current_view%x1, c_double) + real(current_view%x2, c_double))
    cy     = 0.5_c_double * (real(current_view%y1, c_double) + real(current_view%y2, c_double))
    draw_w = max(cur_w, min_sz)
    draw_h = max(cur_h, min_sz)
    ix1 = cx - 0.5_c_double * draw_w; ix2 = cx + 0.5_c_double * draw_w
    iy1 = cy - 0.5_c_double * draw_h; iy2 = cy + 0.5_c_double * draw_h

    call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    call glColor3f(1.0_c_float, 1.0_c_float, 0.0_c_float)
    call glBegin(GL_QUADS)
    call glVertex2f(real(ix1, c_float), real(iy1, c_float))
    call glVertex2f(real(ix2, c_float), real(iy1, c_float))
    call glVertex2f(real(ix2, c_float), real(iy2, c_float))
    call glVertex2f(real(ix1, c_float), real(iy2, c_float))
    call glEnd()
    call glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
  end subroutine RenderMinimap

  subroutine RenderLayerPanel(N, input_layers, layer_colors, active_layer, ui_width, bot_h, top_h)
    integer, intent(in)          :: N, active_layer
    type(DrawnLayer), intent(in) :: input_layers(:)
    real(c_float), intent(in)    :: layer_colors(:,:)
    integer(c_int), intent(in)   :: ui_width, bot_h, top_h
    integer :: r, col, idx, total_btns, num_rows
    real(c_double) :: bw, bh, bx1, by1, bx2, by2

    call glViewport(0_c_int, bot_h, ui_width, top_h)
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity()
    call glOrtho(0.0_c_double, real(ui_width, c_double), 0.0_c_double, &
         real(top_h, c_double), -1.0_c_double, 1.0_c_double)
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity()

    total_btns = N + 2
    num_rows   = max(12, (total_btns + 4) / 5)
    bw = real(ui_width, c_double) / 5.0_c_double
    bh = real(top_h, c_double) / real(num_rows, c_double)

    idx = 1
    do r = 0, num_rows - 1
       do col = 0, 4
          if (idx > total_btns) exit
          bx1 = real(col, c_double) * bw + 2.0_c_double
          bx2 = bx1 + bw - 4.0_c_double
          by2 = real(top_h, c_double) - real(r, c_double) * bh - 2.0_c_double
          by1 = by2 - bh + 4.0_c_double

          if (idx <= N) then
             if (input_layers(idx)%visible) then
                call glColor3f(layer_colors(1, idx), layer_colors(2, idx), layer_colors(3, idx))
             else
                call glColor3f(0.25_c_float * layer_colors(1, idx), &
                     0.25_c_float * layer_colors(2, idx), 0.25_c_float * layer_colors(3, idx))
             end if
          else if (idx == N + 1) then
             call glColor3f(0.35_c_float, 0.35_c_float, 0.35_c_float) ! All OFF button
          else
             call glColor3f(0.75_c_float, 0.75_c_float, 0.75_c_float) ! All ON button
          end if

          call glBegin(GL_QUADS)
          call glVertex2f(real(bx1, c_float), real(by1, c_float))
          call glVertex2f(real(bx2, c_float), real(by1, c_float))
          call glVertex2f(real(bx2, c_float), real(by2, c_float))
          call glVertex2f(real(bx1, c_float), real(by2, c_float))
          call glEnd()

          if (idx == active_layer .or. idx == N + 1 .or. idx == N + 2) then
             call glColor4f(1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float)
             call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
             call glBegin(GL_QUADS)
             if (idx > N) then
                call glVertex2f(real(bx1 + 4.0_c_double, c_float), real(by1 + 4.0_c_double, c_float))
                call glVertex2f(real(bx2 - 4.0_c_double, c_float), real(by1 + 4.0_c_double, c_float))
                call glVertex2f(real(bx2 - 4.0_c_double, c_float), real(by2 - 4.0_c_double, c_float))
                call glVertex2f(real(bx1 + 4.0_c_double, c_float), real(by2 - 4.0_c_double, c_float))
             else
                call glVertex2f(real(bx1, c_float), real(by1, c_float))
                call glVertex2f(real(bx2, c_float), real(by1, c_float))
                call glVertex2f(real(bx2, c_float), real(by2, c_float))
                call glVertex2f(real(bx1, c_float), real(by2, c_float))
             end if
             call glEnd()
             call glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
          end if
          idx = idx + 1
       end do
    end do
  end subroutine RenderLayerPanel

  subroutine SimpleWindow(N, input_layers, win_x_in, win_y_in)
    integer, intent(in)             :: N
    type(DrawnLayer), intent(inout) :: input_layers(:)
    real, intent(in)                :: win_x_in, win_y_in

    type(Box)         :: mbr, current_view
    type(GLFWwindow)  :: window
    type(CameraState) :: cam
    type(InputState)  :: inp
    type(Point2D)     :: mouse_screen, mouse_world

    integer :: ierr, idx, active_layer, target_depth, num_rows, col, r
    integer(c_int) :: live_win_x, live_win_y, prev_win_x, prev_win_y
    integer(c_int) :: ui_width, main_width, top_h, bot_h
    real(c_double) :: mbr_span, max_zoom, bw, bh
    real(c_float), allocatable :: layer_colors(:,:)
    logical :: tree_mode, dirty, should_quit, is_down, any_input
    integer(kind=K_COORDINATE_KIND) :: min_span

    if (N <= 0) then
       write(*,*) 'INFO: MAGVIEWER has 0 layers to display.'
       return
    end if

    ui_width     = 200_c_int
    active_layer = 1
    target_depth = 1
    tree_mode    = .true.
    prev_win_x   = -1_c_int
    prev_win_y   = -1_c_int

    ! Compute global design MBR across all non-empty layers
    call mbr%reset_to_infinity()
    do idx = 1, N
       if (associated(input_layers(idx)%input_layer)) then
          if (input_layers(idx)%input_layer%n_used > 0_int64) then
             mbr = mbr + mbr_of_array(input_layers(idx)%input_layer%layer_boxes, &
                  input_layers(idx)%input_layer%n_used)
          end if
       end if
    end do
    if (.not. mbr%is_valid()) then
       mbr%x1 = 0; mbr%y1 = 0; mbr%x2 = 1000; mbr%y2 = 1000
    else
       mbr_span = max(real(mbr%x2 - mbr%x1, c_double), real(mbr%y2 - mbr%y1, c_double)) * 0.1_c_double
       call box_grow(mbr, int(max(1.0_c_double, mbr_span), kind=K_COORDINATE_KIND), &
            int(max(1.0_c_double, mbr_span), kind=K_COORDINATE_KIND))
    end if

    ! Dynamic palette supporting any number of layers
    allocate(layer_colors(3, max(60, N + 2)))
    do idx = 1, size(layer_colors, 2)
       layer_colors(1, idx) = 0.5_c_float + 0.5_c_float * sin(real(idx, c_float) * 0.3_c_float)
       layer_colors(2, idx) = 0.5_c_float + 0.5_c_float * sin(real(idx, c_float) * 0.3_c_float + 2.09_c_float)
       layer_colors(3, idx) = 0.5_c_float + 0.5_c_float * sin(real(idx, c_float) * 0.3_c_float + 4.18_c_float)
    end do
    do idx = 1, N
       input_layers(idx)%color       = layer_colors(:, idx)
       input_layers(idx)%visible     = .true.
       input_layers(idx)%cache_valid = .false.
    end do

    call glfwSetErrorCallback(handle_error)
    call glfwInit(ierr)
    if (ierr /= 0) error stop 'Error whilst initialising GLFW'

    window = glfwCreateWindow(int(win_x_in, c_int), int(win_y_in, c_int), 'MAGVIEWER')
    if (.not. associated(window)) then
       call glfwTerminate()
       error stop 'Error whilst creating GLFW window'
    end if

    call glfwMakeContextCurrent(window)
    call glDisable(GL_DEPTH_TEST)
    call glEnable(GL_BLEND)
    call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    cam%win_w = max(1.0_c_double, real(win_x_in, c_double) - real(ui_width, c_double))
    cam%win_h = max(1.0_c_double, real(win_y_in, c_double))
    call FitCameraToMBR(cam, mbr)
    dirty = .true.

    do while (.not. glfwWindowShouldClose(window))
       call glfwGetWindowSize(window, live_win_x, live_win_y)
       main_width = max(1_c_int, live_win_x - ui_width)
       bot_h = live_win_y / 2_c_int
       top_h = live_win_y - bot_h
       any_input = .false.
       should_quit = .false.

       if (live_win_x /= prev_win_x .or. live_win_y /= prev_win_y) then
          cam%win_w = real(main_width, c_double)
          cam%win_h = real(max(1_c_int, live_win_y), c_double)
          prev_win_x = live_win_x
          prev_win_y = live_win_y
          dirty = .true.
       end if

       ! --- Keyboard navigation ---
       if (glfwGetKey(window, GLFW_KEY_DOWN)  == GLFW_PRESS) then
          cam%pan_y = cam%pan_y + 5.0_c_double * cam%zoom; dirty = .true.; any_input = .true.
       end if
       if (glfwGetKey(window, GLFW_KEY_UP)    == GLFW_PRESS) then
          cam%pan_y = cam%pan_y - 5.0_c_double * cam%zoom; dirty = .true.; any_input = .true.
       end if
       if (glfwGetKey(window, GLFW_KEY_RIGHT) == GLFW_PRESS) then
          cam%pan_x = cam%pan_x - 5.0_c_double * cam%zoom; dirty = .true.; any_input = .true.
       end if
       if (glfwGetKey(window, GLFW_KEY_LEFT)  == GLFW_PRESS) then
          cam%pan_x = cam%pan_x + 5.0_c_double * cam%zoom; dirty = .true.; any_input = .true.
       end if
       if (glfwGetKey(window, GLFW_KEY_MINUS) == GLFW_PRESS .or. &
           glfwGetKey(window, GLFW_KEY_X)     == GLFW_PRESS) then
          cam%zoom = cam%zoom * 1.02_c_double; dirty = .true.; any_input = .true.
       end if
       if (glfwGetKey(window, GLFW_KEY_Z)     == GLFW_PRESS .or. &
           glfwGetKey(window, GLFW_KEY_EQUAL) == GLFW_PRESS) then
          cam%zoom = max(1.0e-4_c_double, cam%zoom / 1.02_c_double); dirty = .true.; any_input = .true.
       end if

       ! Edge-triggered toggles (independent per key)
       is_down = (glfwGetKey(window, GLFW_KEY_R) == GLFW_PRESS)
       if (is_down .and. .not. inp%r_was_down) then
          tree_mode = .not. tree_mode
          dirty = .true.
          write(*,'(A,L1)') 'MAGVIEWER: tree_mode = ', tree_mode
       end if
       inp%r_was_down = is_down

       is_down = (glfwGetKey(window, GLFW_KEY_S) == GLFW_PRESS)
       if (is_down .and. .not. inp%s_was_down) then
          target_depth = max(0, target_depth - 1)
          dirty = .true.
          write(*,'(A,I0)') 'MAGVIEWER: target_depth = ', target_depth
       end if
       inp%s_was_down = is_down

       is_down = (glfwGetKey(window, GLFW_KEY_T) == GLFW_PRESS)
       if (is_down .and. .not. inp%t_was_down) then
          target_depth = target_depth + 1
          dirty = .true.
          write(*,'(A,I0)') 'MAGVIEWER: target_depth = ', target_depth
       end if
       inp%t_was_down = is_down

       if (glfwGetKey(window, GLFW_KEY_A) == GLFW_PRESS .and. &
           (glfwGetKey(window, GLFW_KEY_LEFT_CONTROL)  == GLFW_PRESS .or. &
            glfwGetKey(window, GLFW_KEY_RIGHT_CONTROL) == GLFW_PRESS)) then
          call FitCameraToMBR(cam, mbr)
          dirty = .true.
       end if

       if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS) should_quit = .true.
       if (should_quit) exit

       ! Clamp zoom and update camera bounds
       max_zoom = max(1.0_c_double, &
            max(1.2_c_double * real(mbr%x2 - mbr%x1, c_double) / max(1.0_c_double, cam%win_w), &
                1.2_c_double * real(mbr%y2 - mbr%y1, c_double) / max(1.0_c_double, cam%win_h)))
       if (cam%zoom > max_zoom) cam%zoom = max_zoom
       call UpdateCameraBounds(cam)

       ! Mouse tracking (single query per frame)
       call glfwGetCursorPos(window, mouse_screen%x, mouse_screen%y)
       is_down = (glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS)
       if (is_down .and. .not. inp%mouse_was_down) then
          if (mouse_screen%x < real(ui_width, c_double) .and. &
              mouse_screen%y < real(top_h, c_double)) then
             num_rows = max(12, (N + 6) / 5)
             bw = real(ui_width, c_double) / 5.0_c_double
             bh = real(top_h, c_double) / real(num_rows, c_double)
             col = int(mouse_screen%x / bw)
             r   = int(mouse_screen%y / bh)
             idx = r * 5 + col + 1
             if (idx >= 1 .and. idx <= N) then
                active_layer = idx
                input_layers(idx)%visible = .not. input_layers(idx)%visible
             else if (idx == N + 1) then
                input_layers(1:N)%visible = .false.
             else if (idx == N + 2) then
                input_layers(1:N)%visible = .true.
             end if
          end if
       end if
       inp%mouse_was_down = is_down

       is_down = (glfwGetKey(window, GLFW_KEY_SLASH) == GLFW_PRESS)
       if (is_down .and. .not. inp%slash_was_down) then
          if (mouse_screen%x >= real(ui_width, c_double)) then
             mouse_screen%x = mouse_screen%x - real(ui_width, c_double)
             call ScreenToWorld(mouse_screen, cam, mouse_world)
             write(*,'(A,F12.2,A,F12.2)') 'World X: ', mouse_world%x, ' | World Y: ', mouse_world%y
          end if
       end if
       inp%slash_was_down = is_down

       ! Only rebuild per-layer vertex buffers when camera or LOD mode changes
       current_view = CameraToViewBox(cam)
       if (dirty) then
          call InvalidateLayerCaches(N, input_layers)
          dirty = .false.
       end if
       min_span = int(max(0.0_c_double, cam%zoom * 0.5_c_double), kind=K_COORDINATE_KIND)
       call UpdateVisibleLayerCaches(N, input_layers, current_view, tree_mode, target_depth, min_span)

       ! Single global clear + 3 clean render passes
       call glViewport(0_c_int, 0_c_int, live_win_x, live_win_y)
       call glClearColor(0.2_c_float, 0.2_c_float, 0.2_c_float, 1.0_c_float)
       call glClear(GL_COLOR_BUFFER_BIT)

       call RenderMainViewport(N, input_layers, ui_width, main_width, live_win_y, cam, tree_mode)
       call RenderMinimap(mbr, current_view, ui_width, bot_h)
       call RenderLayerPanel(N, input_layers, layer_colors, active_layer, ui_width, bot_h, top_h)

       call glfwSwapBuffers(window)
       if (any_input) then
          call glfwPollEvents()
       else
          call glfwWaitEventsTimeout(0.016_c_double)
       end if
    end do

    do idx = 1, N
       if (allocated(input_layers(idx)%vbo_verts)) deallocate(input_layers(idx)%vbo_verts)
    end do
    if (allocated(layer_colors)) deallocate(layer_colors)
    call glfwDestroyWindow(window)
    call glfwTerminate()
  end subroutine SimpleWindow

end module DrawUsingOpenGL

program magviewer
  use, intrinsic :: iso_fortran_env, only: int64
  use, intrinsic :: iso_c_binding, only: c_int
  use DrawUsingOpenGL
  use LayoutDBLoaderModule, only: LoadLayoutDBFile, LoadLayoutDBFileHier
  use DesignModule, only: Design, Layer, DESIGN_DIRECTION_INPUT, RestoreSnapToLayer, BuildTree
  use GeometryModule, only: Box, mbr_of_array
  use CommonModule, only: temporary_layers, K_COORDINATE_KIND
  use hash_mod, only: hash_nitems
  use MagicVLSILayoutParser, only: parseMagicLayoutFile
  implicit none

  integer :: narg, iarg, ios, extpos, j, ch, max_layers, num_layers, i, test_vis
  integer(kind=int64) :: total_boxes
  character(len=512) :: arg, filename
  character(len=16)  :: fext
  logical :: is_layoutdb, is_single_layer, use_hier, headless_info
  type(Design), target :: load_design
  type(Layer), target  :: single_layer
  type(DrawnLayer), allocatable :: input_layers(:)
  type(Box) :: global_mbr
  integer(c_int), allocatable :: test_vbuf(:)

  narg = command_argument_count()
  if (narg < 1) then
     write(*,'(A)') 'Usage: MAGVIEWER.exe <layout.mag|layout.gds|layout.oas|layer.bin|layer.snap> ' // &
          '[max_layers] [--hier] [--info|--headless]'
     stop 1
  end if

  filename      = ''
  max_layers    = 256
  use_hier      = .false.
  headless_info = .false.

  do iarg = 1, narg
     call get_command_argument(iarg, arg, status=ios)
     if (ios /= 0) cycle
     select case (trim(arg))
     case ('--hier', '-H')
        use_hier = .true.
     case ('--info', '--headless')
        headless_info = .true.
     case default
        if (len_trim(filename) == 0) then
           filename = trim(arg)
        else
           read(arg, *, iostat=ios) max_layers
           if (ios /= 0 .or. max_layers <= 0) then
              write(*,'(A,A)') 'ERROR: invalid max_layers argument: ', trim(arg)
              stop 2
           end if
        end if
     end select
  end do

  if (len_trim(filename) == 0) then
     write(*,'(A)') 'ERROR: missing input layout filename.'
     stop 2
  end if

  ! Determine file format from lowercase extension
  fext = ''
  extpos = index(trim(filename), '.', back=.true.)
  if (extpos > 0) fext = filename(extpos + 1:)
  do j = 1, len_trim(fext)
     ch = iachar(fext(j:j))
     if (ch >= iachar('A') .and. ch <= iachar('Z')) fext(j:j) = achar(ch + 32)
  end do

  is_layoutdb = (trim(fext) == 'oas' .or. trim(fext) == 'oasis' .or. &
                 trim(fext) == 'gds' .or. trim(fext) == 'gds2'  .or. trim(fext) == 'gdsii')
  is_single_layer = (trim(fext) == 'bin' .or. trim(fext) == 'snap' .or. trim(fext) == 'snaz') &
                    .or. ((.not. is_layoutdb) .and. max_layers == 1)

  if (is_single_layer) then
     call RestoreSnapToLayer(single_layer, trim(filename))
     call BuildTree(single_layer)
     num_layers = 1
     allocate(input_layers(1))
     input_layers(1)%input_layer => single_layer
  else
     if (allocated(load_design%fileName)) deallocate(load_design%fileName)
     allocate(character(len=len_trim(filename)) :: load_design%fileName)
     load_design%fileName = trim(filename)
     load_design%design_direction = DESIGN_DIRECTION_INPUT
     temporary_layers = 2
     if (is_layoutdb) then
        if (use_hier) then
           write(*,'(A,A)') 'INFO: MAGVIEWER loading GDS/OASIS (hierarchical flatten): ', trim(filename)
           call LoadLayoutDBFileHier(load_design, max_layers)
        else
           write(*,'(A,A)') 'INFO: MAGVIEWER loading GDS/OASIS (native layoutdb): ', trim(filename)
           call LoadLayoutDBFile(load_design, max_layers)
        end if
        num_layers = hash_nitems(load_design%ht)
        ! Build R-Trees for all loaded GDS/OASIS layers so viewport culling and LOD work
        do i = 1, num_layers
           if (load_design%layers(i)%n_used > 0_int64) then
              call BuildTree(load_design%layers(i))
           end if
        end do
     else
        call parseMagicLayoutFile(load_design, max_layers)
        num_layers = hash_nitems(load_design%ht)
     end if
     allocate(input_layers(max(1, num_layers)))
     do i = 1, num_layers
        input_layers(i)%input_layer => load_design%layers(i)
     end do
  end if

  total_boxes = 0_int64
  call global_mbr%reset_to_infinity()
  do i = 1, num_layers
     if (associated(input_layers(i)%input_layer)) then
        total_boxes = total_boxes + input_layers(i)%input_layer%n_used
        if (input_layers(i)%input_layer%n_used > 0_int64) then
           global_mbr = global_mbr + mbr_of_array(input_layers(i)%input_layer%layer_boxes, &
                input_layers(i)%input_layer%n_used)
        end if
     end if
  end do
  write(*,'(A,I0,A,I0,A,4I12)') 'INFO: MAGVIEWER ready: layers=', num_layers, &
       ' total_boxes=', total_boxes, ' extent=', &
       global_mbr%x1, global_mbr%y1, global_mbr%x2, global_mbr%y2

  if (headless_info) then
     ! Exercise the direct R-Tree vertex-buffer fill path in headless verification mode
     test_vis = 0
     do i = 1, num_layers
        if (associated(input_layers(i)%input_layer)) then
           if (input_layers(i)%input_layer%n_used > 0_int64) then
              call FillLayerBoxesDirect(input_layers(i)%input_layer, global_mbr, &
                   0_K_COORDINATE_KIND, test_vbuf, j)
              test_vis = test_vis + j
           end if
        end if
     end do
     write(*,'(A,I0)') 'INFO: MAGVIEWER headless render-fill boxes=', test_vis
     stop 0
  end if

  call SimpleWindow(num_layers, input_layers, 1600.0, 900.0)
end program magviewer
