! File    : magviewer.f90
! Author  : Sandeep Koranne (C) 2026
! Purpose : Idiomatic Fortran 2018 OpenGL/GLFW viewer for Magic, GDSII, OASIS,
!           KLBIN, and SNAP layouts backed by CompressedSpatialIndex
!           (16-bit quantized SoA R-Tree + BoxCodec/Zstd chunks + LRU cache).
!           Every layer uses the compact store instead of BoxCodec/Zstd
!           (compact_layer.f90: Morton centre key + shape id, 128-key bit-packed blocks;
!           contacts are centre-only). Flat OASIS streams and compacts layer by layer, so
!           the peak is one layer as Boxes. MAGVIEWER_COMPACT=0 restores BoxCodec chunks.
!           Headless load/query check: test_viewer_index.exe.

module DrawUsingOpenGL
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding
  use CommonModule,                 only: K_COORDINATE_KIND
  use GeometryModule,               only: Box, box_grow
  use CompressedSpatialIndexModule, only: CompressedSpatialIndex, ChunkLRUCache, K_CSI_DEFAULT_CACHE
  use glf90w
  implicit none
  private

  integer, parameter, public :: K_MAX_OPENGL_CAPACITY = 500000
  real(c_double), parameter  :: K_AUTO_LOD_ZOOM_RATIO = 0.015_c_double

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
       type(c_ptr), value    :: pointer
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
   contains
     procedure :: update_bounds   => cam_update_bounds
     procedure :: fit_to_mbr      => cam_fit_to_mbr
     procedure :: to_view_box     => cam_to_view_box
     procedure :: screen_to_world => cam_screen_to_world
  end type CameraState

  type :: InputState
     logical :: r_was_down     = .false.
     logical :: s_was_down     = .false.
     logical :: t_was_down     = .false.
     logical :: slash_was_down = .false.
     logical :: mouse_was_down = .false.
  end type InputState

  type, public :: DrawnLayer
     character(len=64)                      :: name        = ''
     type(CompressedSpatialIndex), pointer  :: index       => null()
     type(ChunkLRUCache)                    :: chunk_cache
     logical                                :: visible     = .true.
     real(c_float)                          :: color(3)    = [0.8_c_float, 0.3_c_float, 0.3_c_float]
     real(c_float)                          :: alpha       = 0.5_c_float
     integer(c_int), allocatable            :: vbo_verts(:)
     integer                                :: vbo_count   = 0
     logical                                :: cache_valid = .false.
   contains
     procedure :: destroy => drawn_layer_destroy
  end type DrawnLayer

  public :: SimpleWindow, UpdateVisibleLayerCaches

contains

  ! ------------------------------------------------------------------
  ! CameraState methods
  ! ------------------------------------------------------------------
  pure subroutine cam_screen_to_world(this, screen, world)
    class(CameraState), intent(in) :: this
    type(Point2D), intent(in)      :: screen
    type(Point2D), intent(out)     :: world
    world%x = this%left + (screen%x / max(1.0_c_double, this%win_w)) * (this%right - this%left)
    world%y = this%top  - (screen%y / max(1.0_c_double, this%win_h)) * (this%top - this%bottom)
  end subroutine cam_screen_to_world

  pure subroutine cam_update_bounds(this)
    class(CameraState), intent(inout) :: this
    real(c_double) :: cx, cy, half_w, half_h
    cx = (this%win_w * 0.5_c_double) + this%pan_x
    cy = (this%win_h * 0.5_c_double) + this%pan_y
    half_w = (this%win_w * 0.5_c_double) * this%zoom
    half_h = (this%win_h * 0.5_c_double) * this%zoom
    this%left   = cx - half_w
    this%right  = cx + half_w
    this%bottom = cy - half_h
    this%top    = cy + half_h
  end subroutine cam_update_bounds

  pure subroutine cam_fit_to_mbr(this, mbr)
    class(CameraState), intent(inout) :: this
    type(Box), intent(in)             :: mbr
    real(c_double) :: w, h, cx, cy, zx, zy
    w  = max(1.0_c_double, real(mbr%x2, c_double) - real(mbr%x1, c_double))
    h  = max(1.0_c_double, real(mbr%y2, c_double) - real(mbr%y1, c_double))
    cx = 0.5_c_double * (real(mbr%x1, c_double) + real(mbr%x2, c_double))
    cy = 0.5_c_double * (real(mbr%y1, c_double) + real(mbr%y2, c_double))
    zx = w / max(1.0_c_double, this%win_w)
    zy = h / max(1.0_c_double, this%win_h)
    this%zoom  = max(zx, zy) * 1.1_c_double
    this%pan_x = cx - (this%win_w * 0.5_c_double)
    this%pan_y = cy - (this%win_h * 0.5_c_double)
    call this%update_bounds()
  end subroutine cam_fit_to_mbr

  pure function cam_to_view_box(this) result(v)
    class(CameraState), intent(in) :: this
    type(Box) :: v
    real(c_double) :: lo, hi
    lo = -real(huge(v%x1) - 1, c_double)
    hi =  real(huge(v%x1) - 1, c_double)
    v%x1 = floor(max(lo, min(hi, this%left)),   kind=K_COORDINATE_KIND)
    v%y1 = floor(max(lo, min(hi, this%bottom)), kind=K_COORDINATE_KIND)
    v%x2 = ceiling(max(lo, min(hi, this%right)), kind=K_COORDINATE_KIND)
    v%y2 = ceiling(max(lo, min(hi, this%top)),   kind=K_COORDINATE_KIND)
  end function cam_to_view_box

  ! ------------------------------------------------------------------
  ! DrawnLayer cleanup & cache updates
  ! ------------------------------------------------------------------
  subroutine drawn_layer_destroy(this)
    class(DrawnLayer), intent(inout) :: this
    call this%chunk_cache%destroy()
    if (allocated(this%vbo_verts)) deallocate(this%vbo_verts)
    this%vbo_count   = 0
    this%cache_valid = .false.
    nullify(this%index)
  end subroutine drawn_layer_destroy

  subroutine handle_error(code, desc)
    integer, intent(in)      :: code
    character(*), intent(in) :: desc
    write(*, '(A,I8,A,A)') 'GLFW Error ', code, ' : ', desc
  end subroutine handle_error

  subroutine InvalidateLayerCaches(n_layers, layers)
    integer, intent(in)             :: n_layers
    type(DrawnLayer), intent(inout) :: layers(:)
    integer :: i
    do i = 1, n_layers
       layers(i)%cache_valid = .false.
    end do
  end subroutine InvalidateLayerCaches

  !> Parallel per-layer vertex buffer rebuild from CompressedSpatialIndex.
  !> Each layer owns its own vertex buffer and LRU chunk cache (lock-free).
  subroutine UpdateVisibleLayerCaches(n_layers, layers, current_view, tree_mode, target_depth, min_span)
    integer, intent(in)                    :: n_layers, target_depth
    type(DrawnLayer), intent(inout)        :: layers(:)
    type(Box), intent(in)                  :: current_view
    logical, intent(in)                    :: tree_mode
    integer(K_COORDINATE_KIND), intent(in) :: min_span
    integer :: i

    !$omp parallel do default(none) &
    !$omp shared(n_layers, layers, current_view, tree_mode, target_depth, min_span) &
    !$omp private(i) schedule(dynamic, 1)
    do i = 1, n_layers
       if (.not. layers(i)%visible) cycle
       if (layers(i)%cache_valid) cycle
       if (.not. associated(layers(i)%index)) then
          layers(i)%vbo_count   = 0
          layers(i)%cache_valid = .true.
          cycle
       end if
       if (layers(i)%index%is_empty()) then
          layers(i)%vbo_count   = 0
          layers(i)%cache_valid = .true.
          cycle
       end if
       if (tree_mode) then
          call layers(i)%index%query_mbrs_to_quads(current_view, target_depth, min_span, &
               layers(i)%vbo_verts, layers(i)%vbo_count, K_MAX_OPENGL_CAPACITY)
       else
          call layers(i)%index%query_boxes_to_quads(current_view, min_span, &
               layers(i)%chunk_cache, layers(i)%vbo_verts, layers(i)%vbo_count, K_MAX_OPENGL_CAPACITY)
       end if
       layers(i)%cache_valid = .true.
    end do
    !$omp end parallel do
  end subroutine UpdateVisibleLayerCaches

  ! ------------------------------------------------------------------
  ! OpenGL Render Passes
  ! ------------------------------------------------------------------
  subroutine RenderMainViewport(n_layers, layers, ui_width, main_width, win_h, cam, tree_mode)
    integer, intent(in)                  :: n_layers
    type(DrawnLayer), intent(in), target :: layers(:)
    integer(c_int), intent(in)           :: ui_width, main_width, win_h
    type(CameraState), intent(in)        :: cam
    logical, intent(in)                  :: tree_mode
    integer :: i

    call glViewport(ui_width, 0_c_int, main_width, win_h)
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity()
    call glOrtho(cam%left, cam%right, cam%bottom, cam%top, -1.0_c_double, 1.0_c_double)
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity()

    call glEnableClientState(GL_VERTEX_ARRAY)
    do i = 1, n_layers
       if (.not. layers(i)%visible) cycle
       if (layers(i)%vbo_count <= 0 .or. .not. allocated(layers(i)%vbo_verts)) cycle

       call glVertexPointer(2_c_int, GL_INT, 0_c_int, c_loc(layers(i)%vbo_verts(1)))

       ! Filled pass
       call glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
       if (tree_mode) then
          call glColor4f(layers(i)%color(1), layers(i)%color(2), layers(i)%color(3), 0.75_c_float)
       else
          call glColor4f(layers(i)%color(1), layers(i)%color(2), layers(i)%color(3), layers(i)%alpha)
       end if
       call glDrawArrays(GL_QUADS, 0_c_int, int(layers(i)%vbo_count * 4, c_int))

       ! Outline pass
       call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
       call glColor4f(0.0_c_float, 0.0_c_float, 0.0_c_float, 1.0_c_float)
       call glDrawArrays(GL_QUADS, 0_c_int, int(layers(i)%vbo_count * 4, c_int))
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

  subroutine RenderLayerPanel(n_layers, layers, layer_colors, active_layer, ui_width, bot_h, top_h)
    integer, intent(in)          :: n_layers, active_layer
    type(DrawnLayer), intent(in) :: layers(:)
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

    total_btns = n_layers + 2
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

          if (idx <= n_layers) then
             if (layers(idx)%visible) then
                call glColor3f(layer_colors(1, idx), layer_colors(2, idx), layer_colors(3, idx))
             else
                call glColor3f(0.25_c_float * layer_colors(1, idx), &
                     0.25_c_float * layer_colors(2, idx), 0.25_c_float * layer_colors(3, idx))
             end if
          else if (idx == n_layers + 1) then
             call glColor3f(0.35_c_float, 0.35_c_float, 0.35_c_float) ! All OFF
          else
             call glColor3f(0.75_c_float, 0.75_c_float, 0.75_c_float) ! All ON
          end if

          call glBegin(GL_QUADS)
          call glVertex2f(real(bx1, c_float), real(by1, c_float))
          call glVertex2f(real(bx2, c_float), real(by1, c_float))
          call glVertex2f(real(bx2, c_float), real(by2, c_float))
          call glVertex2f(real(bx1, c_float), real(by2, c_float))
          call glEnd()

          if (idx == active_layer .or. idx == n_layers + 1 .or. idx == n_layers + 2) then
             call glColor4f(1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float)
             call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
             call glBegin(GL_QUADS)
             if (idx > n_layers) then
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

  subroutine SimpleWindow(n_layers, layers, win_x_in, win_y_in)
    integer, intent(in)             :: n_layers
    type(DrawnLayer), intent(inout) :: layers(:)
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
    integer(K_COORDINATE_KIND) :: min_span

    if (n_layers <= 0) then
       write(*,'(A)') 'INFO: MAGVIEWER has 0 layers to display.'
       return
    end if

    ui_width     = 200_c_int
    active_layer = 1
    target_depth = 2
    tree_mode    = .true.
    prev_win_x   = -1_c_int
    prev_win_y   = -1_c_int

    ! O(1) global MBR aggregation from compressed index roots
    call mbr%reset_to_infinity()
    do idx = 1, n_layers
       if (associated(layers(idx)%index)) then
          if (.not. layers(idx)%index%is_empty()) then
             mbr = mbr + layers(idx)%index%layer_mbr
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
    allocate(layer_colors(3, max(60, n_layers + 2)))
    do idx = 1, size(layer_colors, 2)
       layer_colors(1, idx) = 0.5_c_float + 0.5_c_float * sin(real(idx, c_float) * 0.3_c_float)
       layer_colors(2, idx) = 0.5_c_float + 0.5_c_float * sin(real(idx, c_float) * 0.3_c_float + 2.09_c_float)
       layer_colors(3, idx) = 0.5_c_float + 0.5_c_float * sin(real(idx, c_float) * 0.3_c_float + 4.18_c_float)
    end do
    do idx = 1, n_layers
       layers(idx)%color       = layer_colors(:, idx)
       layers(idx)%visible     = .true.
       layers(idx)%cache_valid = .false.
       call layers(idx)%chunk_cache%init(K_CSI_DEFAULT_CACHE)
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
    call cam%fit_to_mbr(mbr)
    dirty = .true.

    do while (.not. glfwWindowShouldClose(window))
       call glfwGetWindowSize(window, live_win_x, live_win_y)
       main_width  = max(1_c_int, live_win_x - ui_width)
       bot_h       = live_win_y / 2_c_int
       top_h       = live_win_y - bot_h
       any_input   = .false.
       should_quit = .false.

       if (live_win_x /= prev_win_x .or. live_win_y /= prev_win_y) then
          cam%win_w  = real(main_width, c_double)
          cam%win_h  = real(max(1_c_int, live_win_y), c_double)
          prev_win_x = live_win_x
          prev_win_y = live_win_y
          dirty      = .true.
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

       ! Edge-triggered toggles
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
          call cam%fit_to_mbr(mbr)
          dirty = .true.
       end if

       if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS) should_quit = .true.
       if (should_quit) exit

       max_zoom = max(1.0_c_double, &
            max(1.2_c_double * real(mbr%x2 - mbr%x1, c_double) / max(1.0_c_double, cam%win_w), &
                1.2_c_double * real(mbr%y2 - mbr%y1, c_double) / max(1.0_c_double, cam%win_h)))
       if (cam%zoom > max_zoom) cam%zoom = max_zoom
       call cam%update_bounds()

       ! Mouse tracking
       call glfwGetCursorPos(window, mouse_screen%x, mouse_screen%y)
       is_down = (glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS)
       if (is_down .and. .not. inp%mouse_was_down) then
          if (mouse_screen%x < real(ui_width, c_double) .and. &
              mouse_screen%y < real(top_h, c_double)) then
             num_rows = max(12, (n_layers + 6) / 5)
             bw = real(ui_width, c_double) / 5.0_c_double
             bh = real(top_h, c_double) / real(num_rows, c_double)
             col = int(mouse_screen%x / bw)
             r   = int(mouse_screen%y / bh)
             idx = r * 5 + col + 1
             if (idx >= 1 .and. idx <= n_layers) then
                active_layer = idx
                layers(idx)%visible = .not. layers(idx)%visible
             else if (idx == n_layers + 1) then
                layers(1:n_layers)%visible = .false.
             else if (idx == n_layers + 2) then
                layers(1:n_layers)%visible = .true.
             end if
          end if
       end if
       inp%mouse_was_down = is_down

       is_down = (glfwGetKey(window, GLFW_KEY_SLASH) == GLFW_PRESS)
       if (is_down .and. .not. inp%slash_was_down) then
          if (mouse_screen%x >= real(ui_width, c_double)) then
             mouse_screen%x = mouse_screen%x - real(ui_width, c_double)
             call cam%screen_to_world(mouse_screen, mouse_world)
             write(*,'(A,F12.2,A,F12.2)') 'World X: ', mouse_world%x, ' | World Y: ', mouse_world%y
          end if
       end if
       inp%slash_was_down = is_down

       current_view = cam%to_view_box()
       if (dirty) then
          call InvalidateLayerCaches(n_layers, layers)
          dirty = .false.
       end if
       min_span = int(max(0.0_c_double, cam%zoom * 0.5_c_double), kind=K_COORDINATE_KIND)
       call UpdateVisibleLayerCaches(n_layers, layers, current_view, tree_mode, target_depth, min_span)

       call glViewport(0_c_int, 0_c_int, live_win_x, live_win_y)
       call glClearColor(0.2_c_float, 0.2_c_float, 0.2_c_float, 1.0_c_float)
       call glClear(GL_COLOR_BUFFER_BIT)

       call RenderMainViewport(n_layers, layers, ui_width, main_width, live_win_y, cam, tree_mode)
       call RenderMinimap(mbr, current_view, ui_width, bot_h)
       call RenderLayerPanel(n_layers, layers, layer_colors, active_layer, ui_width, bot_h, top_h)

       call glfwSwapBuffers(window)
       if (any_input) then
          call glfwPollEvents()
       else
          call glfwWaitEventsTimeout(0.016_c_double)
       end if
    end do

    do idx = 1, n_layers
       call layers(idx)%destroy()
    end do
    if (allocated(layer_colors)) deallocate(layer_colors)
    call glfwDestroyWindow(window)
    call glfwTerminate()
  end subroutine SimpleWindow

end module DrawUsingOpenGL

program magviewer
  use, intrinsic :: iso_fortran_env, only: int64, real64
  use, intrinsic :: iso_c_binding,   only: c_int
  use DrawUsingOpenGL,              only: DrawnLayer, SimpleWindow, UpdateVisibleLayerCaches, K_MAX_OPENGL_CAPACITY
  use CompressedSpatialIndexModule, only: CompressedSpatialIndex, K_CSI_DEFAULT_CACHE
  use LayoutDBLoaderModule,         only: LoadLayoutDBToCompressedIndex
  use DesignModule,                 only: Design, Layer, DESIGN_DIRECTION_INPUT, RestoreSnapToLayer, &
                                          ClearLayer, DeleteDesign
  use GeometryModule,               only: Box
  use CommonModule,                 only: temporary_layers, K_COORDINATE_KIND
  use hash_mod,                     only: hash_nitems
  use MagicVLSILayoutParser,        only: parseMagicLayoutFile
  use SystemInformationModule,      only: ReadProcessMemoryUsage
  implicit none

  integer :: narg, iarg, ios, extpos, j, ch, max_layers, num_layers, i
  integer(int64) :: total_boxes, total_tree_b, total_chunk_b, total_comp_b, total_uncomp_b
  integer(int64) :: vm_kb, rss_kb, t_start, t_end, clock_rate
  integer(int64) :: vis_lod, vis_full, vis_zoom1, vis_zoom2, total_hits, total_misses
  real(real64)   :: t_load_s, t_lod_ms, t_full_ms, t_zoom1_ms, t_zoom2_ms, ratio
  character(len=512) :: arg, filename
  character(len=16)  :: fext
  logical :: is_layoutdb, is_single_layer, use_hier, headless_info
  type(Design), target :: load_design
  type(Layer),  target :: single_layer
  type(CompressedSpatialIndex), allocatable, target :: indices(:)
  character(len=64), allocatable :: layer_names(:)
  type(DrawnLayer), allocatable  :: drawn_layers(:)
  type(Box) :: global_mbr, zoom_view
  integer(K_COORDINATE_KIND) :: cx, cy, qw, qh, min_span

  call system_clock(count_rate=clock_rate)

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

  call system_clock(count=t_start)
  call global_mbr%reset_to_infinity()

  if (is_single_layer) then
     call RestoreSnapToLayer(single_layer, trim(filename))
     num_layers = 1
     allocate(indices(1), layer_names(1))
     layer_names(1) = trim(filename)
     if (single_layer%n_used > 0_int64 .and. allocated(single_layer%layer_boxes)) then
        call indices(1)%build_auto(single_layer%layer_boxes, single_layer%n_used)
        global_mbr = indices(1)%layer_mbr
     end if
     call ClearLayer(single_layer)
  else if (is_layoutdb) then
     if (use_hier) then
        write(*,'(A,A)') 'INFO: MAGVIEWER loading GDS/OASIS (hierarchical compressed index): ', trim(filename)
     else
        write(*,'(A,A)') 'INFO: MAGVIEWER loading GDS/OASIS (native compressed index): ', trim(filename)
     end if
     call LoadLayoutDBToCompressedIndex(trim(filename), max_layers, use_hier, &
          indices, layer_names, num_layers, global_mbr)
  else
     if (allocated(load_design%fileName)) deallocate(load_design%fileName)
     allocate(character(len=len_trim(filename)) :: load_design%fileName)
     load_design%fileName = trim(filename)
     load_design%design_direction = DESIGN_DIRECTION_INPUT
     temporary_layers = 2
     call parseMagicLayoutFile(load_design, max_layers)
     num_layers = hash_nitems(load_design%ht)
     allocate(indices(max(1, num_layers)), layer_names(max(1, num_layers)))
     do i = 1, num_layers
        layer_names(i) = trim(load_design%layerNames(i))
        if (load_design%layers(i)%n_used > 0_int64 .and. &
            allocated(load_design%layers(i)%layer_boxes)) then
           call indices(i)%build_auto(load_design%layers(i)%layer_boxes, load_design%layers(i)%n_used)
           global_mbr = global_mbr + indices(i)%layer_mbr
        end if
        call ClearLayer(load_design%layers(i))
     end do
     call DeleteDesign(load_design)
  end if

  call system_clock(count=t_end)
  t_load_s = real(t_end - t_start, real64) / real(max(1_int64, clock_rate), real64)

  ! Aggregate compressed index statistics
  total_boxes    = 0_int64
  total_tree_b   = 0_int64
  total_chunk_b  = 0_int64
  total_comp_b   = 0_int64
  total_uncomp_b = 0_int64
  call global_mbr%reset_to_infinity()

  allocate(drawn_layers(max(1, num_layers)))
  do i = 1, num_layers
     drawn_layers(i)%name    = trim(layer_names(i))
     drawn_layers(i)%index   => indices(i)
     drawn_layers(i)%visible = .true.
     if (.not. indices(i)%is_empty()) then
        total_boxes    = total_boxes    + indices(i)%n_boxes
        total_tree_b   = total_tree_b   + indices(i)%tree_bytes()
        total_chunk_b  = total_chunk_b  + indices(i)%chunk_bytes()
        total_comp_b   = total_comp_b   + indices(i)%memory_bytes()
        total_uncomp_b = total_uncomp_b + indices(i)%uncompressed_bytes()
        global_mbr     = global_mbr     + indices(i)%layer_mbr
     end if
  end do

  if (total_comp_b > 0_int64) then
     ratio = real(total_uncomp_b, real64) / real(total_comp_b, real64)
  else
     ratio = 1.0_real64
  end if
  call ReadProcessMemoryUsage(vm_kb, rss_kb)

  write(*,'(A,I0,A,I0,A,F8.3,A)') 'INFO: MAGVIEWER ready: layers=', num_layers, &
       ' total_boxes=', total_boxes, ' load+index=', t_load_s, ' s'
  write(*,'(A,4I12)') 'INFO: Global MBR: ', global_mbr%x1, global_mbr%y1, global_mbr%x2, global_mbr%y2
  write(*,'(A,F8.2,A,F8.2,A,F8.2,A,F8.2,A,F6.2,A)') &
       'INFO: SpatialIndex RAM: uncompressed=', real(total_uncomp_b, real64) / 1048576.0_real64, &
       ' MB | compressed=', real(total_comp_b, real64) / 1048576.0_real64, &
       ' MB (qRTree=', real(total_tree_b, real64) / 1048576.0_real64, &
       ' MB, chunks=', real(total_chunk_b, real64) / 1048576.0_real64, &
       ' MB) | ratio=', ratio, 'x'
  write(*,'(A,F8.2,A,F8.2,A)') 'INFO: Process Memory: VmSize=', real(vm_kb, real64) / 1024.0_real64, &
       ' MB | VmRSS=', real(rss_kb, real64) / 1024.0_real64, ' MB'

  if (headless_info) then
     do i = 1, num_layers
        call drawn_layers(i)%chunk_cache%init(K_CSI_DEFAULT_CACHE)
     end do

     ! 1. Full-chip LOD query (quantized R-Tree MBRs only, 0 chunk decompressions)
     call system_clock(count=t_start)
     drawn_layers(1:num_layers)%cache_valid = .false.
     min_span = int(max(1_int64, (int(global_mbr%x2, int64) - int(global_mbr%x1, int64)) / 1400_int64), &
                    kind=K_COORDINATE_KIND)
     call UpdateVisibleLayerCaches(num_layers, drawn_layers, global_mbr, .true., 2, min_span)
     call system_clock(count=t_end)
     t_lod_ms = 1000.0_real64 * real(t_end - t_start, real64) / real(max(1_int64, clock_rate), real64)
     vis_lod = sum([(int(drawn_layers(i)%vbo_count, int64), i = 1, num_layers)])

     ! 2. Full-chip detail query with sub-pixel culling
     call system_clock(count=t_start)
     drawn_layers(1:num_layers)%cache_valid = .false.
     call UpdateVisibleLayerCaches(num_layers, drawn_layers, global_mbr, .false., 2, min_span)
     call system_clock(count=t_end)
     t_full_ms = 1000.0_real64 * real(t_end - t_start, real64) / real(max(1_int64, clock_rate), real64)
     vis_full = sum([(int(drawn_layers(i)%vbo_count, int64), i = 1, num_layers)])

     ! 3. Zoomed-in 1/16th-chip detail query (cold vs warm LRU cache)
     cx = int((int(global_mbr%x1, int64) + int(global_mbr%x2, int64)) / 2_int64, K_COORDINATE_KIND)
     cy = int((int(global_mbr%y1, int64) + int(global_mbr%y2, int64)) / 2_int64, K_COORDINATE_KIND)
     qw = int(max(1_int64, (int(global_mbr%x2, int64) - int(global_mbr%x1, int64)) / 8_int64), K_COORDINATE_KIND)
     qh = int(max(1_int64, (int(global_mbr%y2, int64) - int(global_mbr%y1, int64)) / 8_int64), K_COORDINATE_KIND)
     zoom_view%x1 = cx - qw; zoom_view%x2 = cx + qw
     zoom_view%y1 = cy - qh; zoom_view%y2 = cy + qh

     do i = 1, num_layers
        call drawn_layers(i)%chunk_cache%clear()
     end do
     call system_clock(count=t_start)
     drawn_layers(1:num_layers)%cache_valid = .false.
     call UpdateVisibleLayerCaches(num_layers, drawn_layers, zoom_view, .false., 2, 0_K_COORDINATE_KIND)
     call system_clock(count=t_end)
     t_zoom1_ms = 1000.0_real64 * real(t_end - t_start, real64) / real(max(1_int64, clock_rate), real64)
     vis_zoom1 = sum([(int(drawn_layers(i)%vbo_count, int64), i = 1, num_layers)])

     call system_clock(count=t_start)
     drawn_layers(1:num_layers)%cache_valid = .false.
     call UpdateVisibleLayerCaches(num_layers, drawn_layers, zoom_view, .false., 2, 0_K_COORDINATE_KIND)
     call system_clock(count=t_end)
     t_zoom2_ms = 1000.0_real64 * real(t_end - t_start, real64) / real(max(1_int64, clock_rate), real64)
     vis_zoom2 = sum([(int(drawn_layers(i)%vbo_count, int64), i = 1, num_layers)])

     total_hits   = sum([(drawn_layers(i)%chunk_cache%hits,   i = 1, num_layers)])
     total_misses = sum([(drawn_layers(i)%chunk_cache%misses, i = 1, num_layers)])
     call ReadProcessMemoryUsage(vm_kb, rss_kb)

     write(*,'(A,I0,A,F8.2,A)') 'INFO: Headless LOD full-chip:     quads=', vis_lod,   ' time=', t_lod_ms,   ' ms'
     write(*,'(A,I0,A,F8.2,A)') 'INFO: Headless Detail full-chip:  quads=', vis_full,  ' time=', t_full_ms,  ' ms'
     write(*,'(A,I0,A,F8.2,A)') 'INFO: Headless Zoom 1/16 (cold):  quads=', vis_zoom1, ' time=', t_zoom1_ms, ' ms'
     write(*,'(A,I0,A,F8.2,A)') 'INFO: Headless Zoom 1/16 (warm):  quads=', vis_zoom2, ' time=', t_zoom2_ms, ' ms'
     write(*,'(A,I0,A,I0,A,F8.2,A)') 'INFO: Chunk LRU cache: hits=', total_hits, ' misses=', total_misses, &
          ' | Post-render VmRSS=', real(rss_kb, real64) / 1024.0_real64, ' MB'

     do i = 1, num_layers
        call drawn_layers(i)%destroy()
        call indices(i)%destroy()
     end do
     stop 0
  end if

  call SimpleWindow(num_layers, drawn_layers, 1600.0, 900.0)

  do i = 1, num_layers
     call indices(i)%destroy()
  end do
end program magviewer
