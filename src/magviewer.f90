! File    : magviewer.f90
! Author  : Sandeep Koranne (C) 2026
! Purpose : Fortran 2018 OpenGL/GLFW viewer for Magic, GDSII, OASIS, KLBIN and SNAP layouts
!           backed by CompressedSpatialIndex (16-bit quantized R-tree over compact layers:
!           Morton centre key + shape id, 128-key bit-packed blocks, LRU chunk cache).
!           Flat OASIS streams and compacts layer by layer. MAGVIEWER_COMPACT=0 restores the
!           legacy BoxCodec chunks. Plan and roadmap: MAGVIEWER_TODO.md.
!
!  UI:  layer list with names (OASIS LAYERNAME, else layer/datatype such as 67/20), minimap,
!       status bar (cursor in um, um/pixel, boxes drawn, frame ms), performance HUD, key map
!       on '?'. Small C helpers in viewer_gl.c (input queue, bitmap text, screenshots).
!  Env: MAGVIEWER_SIZE=WxH            window size (default 1600x900)
!       MAGVIEWER_SCREENSHOT=out.ppm  write the first complete frame and exit (or the last
!                                     benchmark frame with MAGVIEWER_BENCH)
!       MAGVIEWER_BENCH=1             scripted zoom/pan path; prints per-step query/draw ms
!       MAGVIEWER_EGL=1               create the GL context through EGL (X servers without GLX)
!       MAGVIEWER_HUD=1, MAGVIEWER_HELP=1  start with the HUD / key-map overlay shown
!       MAGVIEWER_VIEW=x1,y1,x2,y2    start view in um (tests, benchmarks)
!       MAGVIEWER_KEYS='g 3 ?'        key presses injected after the first frame (tests);
!                                     tokens: a letter/digit/symbol, PgUp, PgDn, Esc, S-<x> (Shift)
!       MAGVIEWER_SIDECAR=0           flat OASIS: load everything (the Phase 1 path) instead of
!                                     opening through the sidecar index (file.oas.mvi)
!       MAGVIEWER_FRAME_BOXES=N       sidecar mode: draw geometry when at most N boxes are in
!                                     view, else the density pyramid (default 3000000)
!       MAGVIEWER_CACHE_GB=G          sidecar mode: decoded tile cache budget (default 4)
!  Flat OASIS opens through the sidecar (built on first open, about 3 s per billion boxes):
!  nothing is loaded up front; zoomed out the view is drawn from the density pyramid, zoomed
!  in from CBLOCKs decoded on demand (viewer_tiles.f90).
!  Headless load/query check without a display: test_viewer_index.exe, or --headless.

module DrawUsingOpenGL
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
  use, intrinsic :: iso_c_binding
  use CommonModule,                 only: K_COORDINATE_KIND
  use GeometryModule,               only: Box, box_grow
  use CompressedSpatialIndexModule, only: CompressedSpatialIndex, ChunkLRUCache, K_CSI_DEFAULT_CACHE
  use SystemInformationModule,      only: ReadProcessMemoryUsage
  use ViewerFontModule,             only: FONT_BITS, FONT_W, FONT_H
  use ViewerTileSourceModule,       only: TiledLayout
  implicit none
  private

  !> Sidecar-backed geometry source (flat OASIS); null when layers come from indices.
  type(TiledLayout), pointer, public :: tiled => null()
  !> Density images (sidecar mode): main view rebuilt when stale, minimap when its size or
  !> the visible layers change.
  integer(int8), allocatable :: density_img(:), minimap_img(:)
  logical :: density_stale = .true.
  integer :: minimap_side = -1
  logical, allocatable :: minimap_vis(:)

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


  ! GLFW 3 (direct bindings; the window is an opaque C pointer)
  interface
     integer(c_int) function glfwInit() bind(C, name="glfwInit")
       import :: c_int
     end function glfwInit
     subroutine glfwTerminate() bind(C, name="glfwTerminate")
     end subroutine glfwTerminate
     type(c_ptr) function glfwCreateWindow(w, h, title, mon, share) bind(C, name="glfwCreateWindow")
       import :: c_ptr, c_int, c_char
       integer(c_int), value :: w, h
       character(kind=c_char), intent(in) :: title(*)
       type(c_ptr), value :: mon, share
     end function glfwCreateWindow
     subroutine glfwDestroyWindow(win) bind(C, name="glfwDestroyWindow")
       import :: c_ptr
       type(c_ptr), value :: win
     end subroutine glfwDestroyWindow
     subroutine glfwMakeContextCurrent(win) bind(C, name="glfwMakeContextCurrent")
       import :: c_ptr
       type(c_ptr), value :: win
     end subroutine glfwMakeContextCurrent
     integer(c_int) function glfwWindowShouldClose(win) bind(C, name="glfwWindowShouldClose")
       import :: c_ptr, c_int
       type(c_ptr), value :: win
     end function glfwWindowShouldClose
     subroutine glfwGetWindowSize(win, w, h) bind(C, name="glfwGetWindowSize")
       import :: c_ptr, c_int
       type(c_ptr), value :: win
       integer(c_int), intent(out) :: w, h
     end subroutine glfwGetWindowSize
     integer(c_int) function glfwGetKey(win, key) bind(C, name="glfwGetKey")
       import :: c_ptr, c_int
       type(c_ptr), value :: win
       integer(c_int), value :: key
     end function glfwGetKey
     subroutine glfwGetCursorPos(win, x, y) bind(C, name="glfwGetCursorPos")
       import :: c_ptr, c_double
       type(c_ptr), value :: win
       real(c_double), intent(out) :: x, y
     end subroutine glfwGetCursorPos
     subroutine glfwSwapBuffers(win) bind(C, name="glfwSwapBuffers")
       import :: c_ptr
       type(c_ptr), value :: win
     end subroutine glfwSwapBuffers
     subroutine glfwPollEvents() bind(C, name="glfwPollEvents")
     end subroutine glfwPollEvents
     subroutine glfwWaitEventsTimeout(t) bind(C, name="glfwWaitEventsTimeout")
       import :: c_double
       real(c_double), value :: t
     end subroutine glfwWaitEventsTimeout
     subroutine viewer_glfw_errors() bind(C, name="viewer_glfw_errors")
     end subroutine viewer_glfw_errors
     subroutine viewer_context_hints() bind(C, name="viewer_context_hints")
     end subroutine viewer_context_hints
     integer(c_int) function viewer_input_script(s, n) bind(C, name="viewer_input_script")
       import :: c_char, c_int
       character(kind=c_char), intent(in) :: s(*)
       integer(c_int), value :: n
     end function viewer_input_script
  end interface

  ! C helpers (viewer_gl.c): input queue, bitmap text, screenshot
  interface
     subroutine viewer_input_install(win) bind(C, name="viewer_input_install")
       import :: c_ptr
       type(c_ptr), value :: win
     end subroutine viewer_input_install
     integer(c_int) function viewer_input_next(etype, code, mods, action, x, y) bind(C, name="viewer_input_next")
       import :: c_int, c_double
       integer(c_int), intent(out) :: etype, code, mods, action
       real(c_double), intent(out) :: x, y
     end function viewer_input_next
     subroutine viewer_text_set_font(bits) bind(C, name="viewer_text_set_font")
       import :: c_int8_t
       integer(c_int8_t), intent(in) :: bits(*)
     end subroutine viewer_text_set_font
     subroutine viewer_draw_text(x, y, s, n) bind(C, name="viewer_draw_text")
       import :: c_float, c_char, c_int
       real(c_float), value :: x, y
       character(kind=c_char), intent(in) :: s(*)
       integer(c_int), value :: n
     end subroutine viewer_draw_text
     integer(c_int) function viewer_screenshot_ppm(path, w, h) bind(C, name="viewer_screenshot_ppm")
       import :: c_char, c_int
       character(kind=c_char), intent(in) :: path(*)
       integer(c_int), value :: w, h
     end function viewer_screenshot_ppm
     subroutine viewer_draw_rgb(x, y, w, h, rgb) bind(C, name="viewer_draw_rgb")
       import :: c_int, c_int8_t
       integer(c_int), value :: x, y, w, h
       integer(c_int8_t), intent(in) :: rgb(*)
     end subroutine viewer_draw_rgb
     subroutine viewer_finish() bind(C, name="viewer_finish")
     end subroutine viewer_finish
     subroutine viewer_swap_interval(n) bind(C, name="viewer_swap_interval")
       import :: c_int
       integer(c_int), value :: n
     end subroutine viewer_swap_interval
     subroutine glLineWidth(w) bind(C, name="glLineWidth")
       import :: c_float
       real(c_float), value :: w
     end subroutine glLineWidth
  end interface


  ! OpenGL constants
  integer(c_int), parameter :: GL_COLOR_BUFFER_BIT    = int(z'00004000', c_int)
  integer(c_int), parameter :: GL_QUADS               = int(z'00000007', c_int)
  integer(c_int), parameter :: GL_LINES               = int(z'00000001', c_int)
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
  integer(c_int), parameter :: GLFW_MOD_SHIFT         = 1_c_int
  integer(c_int), parameter :: GLFW_MOD_CONTROL       = 2_c_int
  integer(c_int), parameter :: GLFW_KEY_0             = 48_c_int
  integer(c_int), parameter :: GLFW_KEY_9             = 57_c_int
  integer(c_int), parameter :: GLFW_KEY_MINUS         = 45_c_int
  integer(c_int), parameter :: GLFW_KEY_EQUAL         = 61_c_int
  integer(c_int), parameter :: GLFW_KEY_A             = 65_c_int
  integer(c_int), parameter :: GLFW_KEY_F             = 70_c_int
  integer(c_int), parameter :: GLFW_KEY_G             = 71_c_int
  integer(c_int), parameter :: GLFW_KEY_H             = 72_c_int
  integer(c_int), parameter :: GLFW_KEY_O             = 79_c_int
  integer(c_int), parameter :: GLFW_KEY_P             = 80_c_int
  integer(c_int), parameter :: GLFW_KEY_Q             = 81_c_int
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
  integer(c_int), parameter :: GLFW_KEY_PAGE_UP       = 266_c_int
  integer(c_int), parameter :: GLFW_KEY_PAGE_DOWN     = 267_c_int

  ! Layout of the left column (pixels)
  integer(c_int), parameter :: K_UI_WIDTH  = 280_c_int
  integer(c_int), parameter :: K_ROW_H     = 17_c_int
  integer(c_int), parameter :: K_HEADER_H  = 22_c_int
  integer(c_int), parameter :: K_STATUS_H  = 18_c_int

  integer, parameter :: K_NHELP = 27
  character(len=80), parameter :: HELP_LINES(K_NHELP) = [character(len=80) :: &
       'MAGVIEWER keys', &
       '  arrows                 pan (hold)', &
       '  = z / - x              zoom in / out (hold)', &
       '  mouse wheel            zoom about the cursor (over the layer list: scroll)', &
       '  f  or Ctrl-A           fit the whole layout', &
       '  1 .. 9, 0              toggle layer 1 .. 10 (Shift: show only that layer)', &
       '  a                      all layers on', &
       '  click layer row        toggle layer (Shift-click: solo)', &
       '  click ALL / NONE       all layers on / off', &
       '  PageUp / PageDown      scroll the layer list', &
       '  r                      tree mode (R-tree MBRs) / geometry', &
       '  s / t                  tree depth - / +', &
       '  o                      outlines on / off', &
       '  g                      performance HUD on / off', &
       '  p                      print performance report to the terminal', &
       '  /                      print cursor position (um and database units)', &
       '  h                      key map overlay on / off', &
       '  ?                      print this key map to the terminal', &
       '  q or Esc               quit', &
       '', &
       'Status bar: cursor x y (um) | um per pixel | boxes drawn | frame ms', &
       'HUD: fps, frame / query / draw ms (last, avg, p95), boxes, chunk cache,', &
       '     resident memory. Load phases are in the p report.', &
       'Env: MAGVIEWER_SIZE=WxH  MAGVIEWER_SCREENSHOT=f.ppm  MAGVIEWER_BENCH=1', &
       '     MAGVIEWER_COMPACT=0 (legacy chunks)', &
       'Layer names: OASIS LAYERNAME table when present, else layer/datatype.', &
       '']

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
     procedure :: zoom_about      => cam_zoom_about
  end type CameraState

  type, public :: DrawnLayer
     character(len=64)                      :: name        = ''   ! loader name, e.g. L67_D20
     character(len=64)                      :: label       = ''   ! shown, e.g. "li1  67/20"
     type(CompressedSpatialIndex), pointer  :: index       => null()
     type(ChunkLRUCache)                    :: chunk_cache
     logical                                :: visible     = .true.
     real(c_float)                          :: color(3)    = [0.8_c_float, 0.3_c_float, 0.3_c_float]
     real(c_float)                          :: alpha       = 0.5_c_float
     integer(c_int), allocatable            :: vbo_verts(:)
     integer                                :: vbo_count   = 0
     logical                                :: cache_valid = .false.
     logical                                :: density     = .false.  ! drawn from the density image
   contains
     procedure :: destroy => drawn_layer_destroy
  end type DrawnLayer

  !> Load-phase and frame statistics (filled by the program and the render loop).
  integer, parameter :: K_RING = 512
  type, public :: ViewerStats
     character(len=512) :: file = ''
     character(len=32)  :: format = ''
     integer            :: strict = -1          ! OASIS strict tables: 1 yes, 0 no, -1 n/a
     real(real64)       :: tables_ms = -1.0_real64
     real(real64)       :: read_s = 0.0_real64, index_s = 0.0_real64, load_s = 0.0_real64
     real(real64)       :: unit_per_um = 1000.0_real64   ! database steps per micron
     integer(int64)     :: t0_ticks = 0         ! program start (system_clock)
     integer(int64)     :: total_boxes = 0
     real(real64)       :: index_mb = 0.0_real64
     real(real64)       :: first_frame_s = -1.0_real64
     integer(int64)     :: frames = 0
     real(real64)       :: frame_ms(K_RING) = 0.0_real64, query_ms(K_RING) = 0.0_real64, &
                           draw_ms(K_RING) = 0.0_real64
     integer(int64)     :: boxes_drawn = 0
  end type ViewerStats
  type(ViewerStats), public :: vstats

  public :: SimpleWindow, UpdateVisibleLayerCaches, PrintKeyMap, PrintPerfReport, wall_seconds

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


  !> Zoom by factor (new zoom = zoom * factor) keeping the world point under screen (sx, sy) fixed.
  pure subroutine cam_zoom_about(this, factor, sx, sy)
    class(CameraState), intent(inout) :: this
    real(c_double), intent(in) :: factor, sx, sy
    real(c_double) :: wx, wy, z2, cx, cy
    wx = this%left + sx * this%zoom
    wy = this%top  - sy * this%zoom
    z2 = max(1.0e-4_c_double, this%zoom * factor)
    cx = wx + 0.5_c_double * this%win_w * z2 - sx * z2
    cy = wy - 0.5_c_double * this%win_h * z2 + sy * z2
    this%zoom  = z2
    this%pan_x = cx - 0.5_c_double * this%win_w
    this%pan_y = cy - 0.5_c_double * this%win_h
    call this%update_bounds()
  end subroutine cam_zoom_about

  real(real64) function wall_seconds()
    integer(int64) :: t, r
    call system_clock(t, r)
    wall_seconds = real(t, real64) / real(max(1_int64, r), real64)
  end function wall_seconds

  real(real64) function since_ms(t0)
    real(real64), intent(in) :: t0
    since_ms = 1000.0_real64 * (wall_seconds() - t0)
  end function since_ms

  !> Text at pixel (x, y) (lower-left of the first glyph) in the current pixel projection.
  subroutine draw_text(x, y, s)
    real, intent(in) :: x, y
    character(len=*), intent(in) :: s
    if (len_trim(s) == 0) return
    call viewer_draw_text(real(x, c_float), real(y, c_float), s, int(len_trim(s), c_int))
  end subroutine draw_text

  subroutine fill_rect(x1, y1, x2, y2)
    real, intent(in) :: x1, y1, x2, y2
    call glBegin(GL_QUADS)
    call glVertex2f(x1, y1); call glVertex2f(x2, y1)
    call glVertex2f(x2, y2); call glVertex2f(x1, y2)
    call glEnd()
  end subroutine fill_rect

  !> Pixel projection over the viewport (x0, y0, w, h); y grows upward.
  subroutine pixel_projection(x0, y0, w, h)
    integer(c_int), intent(in) :: x0, y0, w, h
    call glViewport(x0, y0, w, h)
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity()
    call glOrtho(0.0_c_double, real(w, c_double), 0.0_c_double, real(h, c_double), &
         -1.0_c_double, 1.0_c_double)
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity()
  end subroutine pixel_projection

  !> Mean and 95th percentile of the last min(n, K_RING) samples.
  subroutine ring_stats(v, n, avg, p95)
    real(real64), intent(in) :: v(K_RING)
    integer(int64), intent(in) :: n
    real(real64), intent(out) :: avg, p95
    real(real64) :: tmp(K_RING), t
    integer :: m, i, j
    m = int(min(n, int(K_RING, int64)))
    avg = 0.0_real64; p95 = 0.0_real64
    if (m <= 0) return
    tmp(1:m) = v(1:m)
    avg = sum(tmp(1:m)) / real(m, real64)
    do i = 2, m                                    ! insertion sort, m <= 512
       t = tmp(i); j = i - 1
       do while (j >= 1)
          if (tmp(j) <= t) exit
          tmp(j + 1) = tmp(j); j = j - 1
       end do
       tmp(j + 1) = t
    end do
    p95 = tmp(max(1, min(m, ceiling(0.95_real64 * real(m, real64)))))
  end subroutine ring_stats

  subroutine record_frame(frame_ms, query_ms, draw_ms)
    real(real64), intent(in) :: frame_ms, query_ms, draw_ms
    integer :: k
    vstats%frames = vstats%frames + 1
    k = int(mod(vstats%frames - 1, int(K_RING, int64))) + 1
    vstats%frame_ms(k) = frame_ms
    vstats%query_ms(k) = query_ms
    vstats%draw_ms(k)  = draw_ms
  end subroutine record_frame

  subroutine PrintKeyMap()
    integer :: i
    do i = 1, K_NHELP
       write(*,'(A)') trim(HELP_LINES(i))
    end do
  end subroutine PrintKeyMap

  subroutine PrintPerfReport(n_layers, layers)
    integer, intent(in) :: n_layers
    type(DrawnLayer), intent(in) :: layers(:)
    real(real64) :: fa, fp, qa, qp, da, dp
    integer(int64) :: vm_kb, rss_kb, hits, misses
    integer :: i
    call ring_stats(vstats%frame_ms, vstats%frames, fa, fp)
    call ring_stats(vstats%query_ms, vstats%frames, qa, qp)
    call ring_stats(vstats%draw_ms,  vstats%frames, da, dp)
    hits = 0; misses = 0
    do i = 1, n_layers
       hits = hits + layers(i)%chunk_cache%hits
       misses = misses + layers(i)%chunk_cache%misses
    end do
    if (associated(tiled)) then
       hits = hits + tiled%hits; misses = misses + tiled%misses
    end if
    call ReadProcessMemoryUsage(vm_kb, rss_kb)
    write(*,'(A)') '---------------- MAGVIEWER performance ----------------'
    write(*,'(A,A)') '  file            ', trim(vstats%file)
    if (vstats%strict >= 0) then
       write(*,'(A,A,A,I0,A,F9.3,A)') '  format          ', trim(vstats%format), '  strict tables=', &
            vstats%strict, '  (tables read in ', vstats%tables_ms, ' ms)'
    else
       write(*,'(A,A)') '  format          ', trim(vstats%format)
    end if
    write(*,'(A,F9.2,A,F9.2,A,F9.2,A)') '  load            read ', vstats%read_s, ' s  index ', &
         vstats%index_s, ' s  total ', vstats%load_s, ' s'
    write(*,'(A,I0,A,I0,A,F10.1,A)') '  data            layers ', n_layers, '  boxes ', vstats%total_boxes, &
         '  index ', vstats%index_mb, ' MB'
    write(*,'(A,F9.2,A)') '  first frame     ', vstats%first_frame_s, ' s after start'
    write(*,'(A,I0)') '  frames          ', vstats%frames
    write(*,'(A,3F10.2)') '  frame ms        last/avg/p95 ', &
         vstats%frame_ms(int(mod(max(0_int64, vstats%frames - 1), int(K_RING, int64))) + 1), fa, fp
    write(*,'(A,3F10.2)') '  query ms        last/avg/p95 ', &
         vstats%query_ms(int(mod(max(0_int64, vstats%frames - 1), int(K_RING, int64))) + 1), qa, qp
    write(*,'(A,3F10.2)') '  draw ms         last/avg/p95 ', &
         vstats%draw_ms(int(mod(max(0_int64, vstats%frames - 1), int(K_RING, int64))) + 1), da, dp
    write(*,'(A,I0)') '  boxes drawn     ', vstats%boxes_drawn
    write(*,'(A,I0,A,I0)') '  chunk cache     hits ', hits, '  misses ', misses
    if (associated(tiled)) then
       write(*,'(A,A)') '  sidecar         ', tiled%side%path
       write(*,'(A,I0,A,I0,A,F8.2,A,F8.3,A)') '                  units ', tiled%side%n_units, '  dependent ', &
            tiled%side%n_dependent, '  built in ', tiled%side%build_s, ' s  opened in ', tiled%side%open_s, ' s'
       write(*,'(A,I0,A,F9.1,A,F9.1,A,I0)') '  tile cache      tiles ', tiled%n_tiles, '  ', &
            real(tiled%bytes, real64) / 1048576.0_real64, ' MB of ', real(tiled%budget, real64) / 1048576.0_real64, &
            ' MB  frame budget ', tiled%frame_boxes
       write(*,'(A,A,A,I0,A,F9.2,A)') '  last frame      ', merge('geometry', 'density ', tiled%geometry), &
            '  boxes in view ', tiled%estimate, '  decode ', tiled%decode_ms, ' ms'
    end if
    write(*,'(A,F10.1,A,F10.1,A)') '  memory          VmRSS ', real(rss_kb, real64) / 1024.0_real64, &
         ' MB  VmSize ', real(vm_kb, real64) / 1024.0_real64, ' MB'
    write(*,'(A)') '--------------------------------------------------------'
  end subroutine PrintPerfReport

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

    if (associated(tiled)) then
       call UpdateTiledLayerCaches(n_layers, layers, current_view, min_span)
       return
    end if
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

  !> Sidecar mode: density pyramid when the view holds more boxes than the frame budget,
  !> otherwise geometry from the decoded tiles. All layers are refreshed together (one mode).
  subroutine UpdateTiledLayerCaches(n_layers, layers, current_view, min_span)
    integer, intent(in)                    :: n_layers
    type(DrawnLayer), intent(inout)        :: layers(:)
    type(Box), intent(in)                  :: current_view
    integer(K_COORDINATE_KIND), intent(in) :: min_span
    logical :: vis(n_layers), geometry
    integer :: i
    if (all(layers(1:n_layers)%cache_valid .or. .not. layers(1:n_layers)%visible)) return
    vis = layers(1:n_layers)%visible
    geometry = tiled%choose_mode(vis, current_view, 2.0_real64 * real(max(1, min_span), real64))
    if (geometry) call tiled%prepare(vis, current_view)
    density_stale = .true.
    !$omp parallel do default(shared) private(i) schedule(dynamic, 1)
    do i = 1, n_layers
       if (.not. vis(i)) cycle
       layers(i)%density = .not. geometry
       layers(i)%vbo_count = 0
       if (geometry) call tiled%geometry_quads(i, current_view, min_span, layers(i)%vbo_verts, &
            layers(i)%vbo_count, K_MAX_OPENGL_CAPACITY * 8)
       layers(i)%cache_valid = .true.
    end do
    !$omp end parallel do
  end subroutine UpdateTiledLayerCaches


  ! ------------------------------------------------------------------
  ! OpenGL Render Passes
  ! ------------------------------------------------------------------
  subroutine RenderMainViewport(n_layers, layers, ui_width, main_width, win_h, cam, tree_mode, outlines)
    integer, intent(in)                  :: n_layers
    type(DrawnLayer), intent(in), target :: layers(:)
    integer(c_int), intent(in)           :: ui_width, main_width, win_h
    type(CameraState), intent(in)        :: cam
    logical, intent(in)                  :: tree_mode, outlines
    integer :: i, k, ns
    real(c_double) :: wsum

    call glViewport(ui_width, 0_c_int, main_width, win_h)
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity()
    call glOrtho(cam%left, cam%right, cam%bottom, cam%top, -1.0_c_double, 1.0_c_double)
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity()

    if (associated(tiled)) then
       if (any(layers(1:n_layers)%density .and. layers(1:n_layers)%visible)) then
          if (density_stale) then
             call tiled%density_image(layers(1:n_layers)%visible, layer_colors(n_layers, layers), &
                  cam%left, cam%bottom, cam%zoom, int(main_width), int(win_h), density_img)
             density_stale = .false.
          end if
          call viewer_draw_rgb(ui_width, 0_c_int, main_width, win_h, density_img)
          return
       end if
    end if
    call glEnableClientState(GL_VERTEX_ARRAY)
    do i = 1, n_layers
       if (.not. layers(i)%visible) cycle
       if (layers(i)%vbo_count <= 0 .or. .not. allocated(layers(i)%vbo_verts)) cycle

       call glVertexPointer(2_c_int, GL_INT, 0_c_int, c_loc(layers(i)%vbo_verts(1)))

       call glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
       if (tree_mode) then
          call glColor4f(layers(i)%color(1), layers(i)%color(2), layers(i)%color(3), 0.75_c_float)
       else
          call glColor4f(layers(i)%color(1), layers(i)%color(2), layers(i)%color(3), layers(i)%alpha)
       end if
       call glDrawArrays(GL_QUADS, 0_c_int, int(layers(i)%vbo_count * 4, c_int))

       if (outlines) then
          ! Outline only when boxes are at least ~4 pixels wide on average (sampled), otherwise
          ! the edges of millions of tiny boxes hide the fill.
          ns = min(64, layers(i)%vbo_count)
          wsum = 0.0_c_double
          do k = 0, ns - 1
             wsum = wsum + abs(real(layers(i)%vbo_verts(8*k + 3) - layers(i)%vbo_verts(8*k + 1), c_double))
          end do
          if (ns > 0 .and. wsum / real(max(1, ns), c_double) >= 4.0_c_double * cam%zoom) then
             call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
             call glColor4f(0.6_c_float * layers(i)%color(1), 0.6_c_float * layers(i)%color(2), &
                  0.6_c_float * layers(i)%color(3), 1.0_c_float)
             call glDrawArrays(GL_QUADS, 0_c_int, int(layers(i)%vbo_count * 4, c_int))
          end if
       end if
    end do
    call glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    call glDisableClientState(GL_VERTEX_ARRAY)
  end subroutine RenderMainViewport

  !> 'D' density, 'G' geometry (sidecar mode), 'I' index.
  character(len=1) function bench_mode()
    bench_mode = 'I'
    if (associated(tiled)) bench_mode = merge('G', 'D', tiled%geometry)
  end function bench_mode

  !> Layer colours as a (3, n) array for the density compositor.
  function layer_colors(n_layers, layers) result(c)
    integer, intent(in) :: n_layers
    type(DrawnLayer), intent(in) :: layers(:)
    real(c_float) :: c(3, n_layers)
    integer :: i
    do i = 1, n_layers
       c(:, i) = layers(i)%color
    end do
  end function layer_colors

  subroutine RenderMinimap(mbr, current_view, ui_width, bot_h, n_layers, layers)
    type(Box), intent(in)      :: mbr, current_view
    integer(c_int), intent(in) :: ui_width, bot_h
    integer, intent(in)        :: n_layers
    type(DrawnLayer), intent(in) :: layers(:)
    integer(c_int) :: side
    real(c_double) :: mbr_w, mbr_h, mbr_cx, mbr_cy, half_dim
    real(c_double) :: cur_w, cur_h, min_sz, cx, cy, draw_w, draw_h
    real(c_double) :: ix1, iy1, ix2, iy2

    side = max(1_c_int, min(ui_width, bot_h))
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

    if (associated(tiled)) then           ! whole design from the density pyramid
       if (minimap_side /= side .or. .not. allocated(minimap_vis)) then
          minimap_side = -1
       else if (size(minimap_vis) /= n_layers) then
          minimap_side = -1
       else if (any(minimap_vis .neqv. layers(1:n_layers)%visible)) then
          minimap_side = -1
       end if
       if (minimap_side < 0) then
          call tiled%density_image(layers(1:n_layers)%visible, layer_colors(n_layers, layers), &
               mbr_cx - half_dim, mbr_cy - half_dim, 2.0_c_double * half_dim / real(side, c_double), &
               int(side), int(side), minimap_img)
          minimap_side = side
          minimap_vis = layers(1:n_layers)%visible
       end if
       call viewer_draw_rgb(0_c_int, 0_c_int, side, side, minimap_img)
    else
       call glColor3f(0.1_c_float, 0.1_c_float, 0.1_c_float)
       call glBegin(GL_QUADS)
       call glVertex2f(real(mbr%x1, c_float), real(mbr%y1, c_float))
       call glVertex2f(real(mbr%x2, c_float), real(mbr%y1, c_float))
       call glVertex2f(real(mbr%x2, c_float), real(mbr%y2, c_float))
       call glVertex2f(real(mbr%x1, c_float), real(mbr%y2, c_float))
       call glEnd()
    end if

    cur_w  = real(current_view%x2, c_double) - real(current_view%x1, c_double)
    cur_h  = real(current_view%y2, c_double) - real(current_view%y1, c_double)
    min_sz = (mbr_w + mbr_h) * 0.025_c_double
    cx     = 0.5_c_double * (real(current_view%x1, c_double) + real(current_view%x2, c_double))
    cy     = 0.5_c_double * (real(current_view%y1, c_double) + real(current_view%y2, c_double))
    draw_w = max(cur_w, min_sz)
    draw_h = max(cur_h, min_sz)
    ix1 = max(real(mbr%x1, c_double), cx - 0.5_c_double * draw_w); ix2 = min(real(mbr%x2, c_double), cx + 0.5_c_double * draw_w)
    iy1 = max(real(mbr%y1, c_double), cy - 0.5_c_double * draw_h); iy2 = min(real(mbr%y2, c_double), cy + 0.5_c_double * draw_h)

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


  !> Layer list: header with ALL / NONE buttons, then one row per layer (swatch + name).
  !> list_top is the window height of the list area (pixels from the bottom of the window).
  subroutine RenderLayerPanel(n_layers, layers, active_layer, scroll, ui_width, y0, list_h)
    integer, intent(in)          :: n_layers, active_layer, scroll
    type(DrawnLayer), intent(in) :: layers(:)
    integer(c_int), intent(in)   :: ui_width, y0, list_h
    integer :: i, row, nrows
    real :: yt, w
    character(len=40) :: hdr

    call pixel_projection(0_c_int, y0, ui_width, list_h)
    w = real(ui_width)
    call glColor3f(0.12_c_float, 0.12_c_float, 0.14_c_float)
    call fill_rect(0.0, 0.0, w, real(list_h))

    ! header
    yt = real(list_h) - real(K_HEADER_H)
    call glColor3f(0.30_c_float, 0.30_c_float, 0.34_c_float)
    call fill_rect(2.0, yt + 2.0, w * 0.5 - 1.0, real(list_h) - 2.0)
    call fill_rect(w * 0.5 + 1.0, yt + 2.0, w - 2.0, real(list_h) - 2.0)
    call glColor3f(1.0_c_float, 1.0_c_float, 1.0_c_float)
    write(hdr, '(A,I0,A)') 'ALL (', n_layers, ')'
    call draw_text(8.0, yt + 6.0, trim(hdr))
    call draw_text(w * 0.5 + 8.0, yt + 6.0, 'NONE')

    nrows = int((list_h - K_HEADER_H) / K_ROW_H)
    do row = 1, nrows
       i = scroll + row
       if (i > n_layers) exit
       yt = real(list_h) - real(K_HEADER_H) - real(row * K_ROW_H)
       if (i == active_layer) then
          call glColor3f(0.22_c_float, 0.22_c_float, 0.28_c_float)
          call fill_rect(0.0, yt, w, yt + real(K_ROW_H))
       end if
       if (layers(i)%visible) then
          call glColor3f(layers(i)%color(1), layers(i)%color(2), layers(i)%color(3))
       else
          call glColor3f(0.25_c_float * layers(i)%color(1), 0.25_c_float * layers(i)%color(2), &
               0.25_c_float * layers(i)%color(3))
       end if
       call fill_rect(6.0, yt + 3.0, 26.0, yt + real(K_ROW_H) - 3.0)
       if (layers(i)%visible) then
          call glColor3f(0.95_c_float, 0.95_c_float, 0.95_c_float)
       else
          call glColor3f(0.45_c_float, 0.45_c_float, 0.45_c_float)
       end if
       write(hdr, '(I3)') i
       call draw_text(30.0, yt + 3.0, hdr(1:3))
       call draw_text(62.0, yt + 3.0, layers(i)%label(1:min(len_trim(layers(i)%label), &
            max(1, (ui_width - 66_c_int) / FONT_W))))
    end do
    if (scroll > 0 .or. scroll + nrows < n_layers) then
       call glColor3f(0.6_c_float, 0.6_c_float, 0.6_c_float)
       write(hdr, '(I0,A,I0,A,I0)') scroll + 1, '-', min(n_layers, scroll + nrows), ' of ', n_layers
       call draw_text(w - 8.0 * real(len_trim(hdr)) - 6.0, 2.0, trim(hdr))
    end if
  end subroutine RenderLayerPanel

  !> Status bar along the bottom of the main view and the optional HUD / key-map overlays.
  subroutine RenderOverlays(n_layers, layers, ui_width, main_width, win_h, cam, mouse_world, in_view, &
                            show_hud, show_help, tree_mode)
    integer, intent(in)           :: n_layers
    type(DrawnLayer), intent(in)  :: layers(:)
    integer(c_int), intent(in)    :: ui_width, main_width, win_h
    type(CameraState), intent(in) :: cam
    type(Point2D), intent(in)     :: mouse_world
    logical, intent(in)           :: in_view, show_hud, show_help, tree_mode
    character(len=160) :: line
    real(real64) :: u, fa, fp, qa, qp, da, dp, last
    integer(int64) :: vm_kb, rss_kb, hits, misses
    integer :: i, k

    call pixel_projection(ui_width, 0_c_int, main_width, win_h)
    u = vstats%unit_per_um
    call glColor4f(0.0_c_float, 0.0_c_float, 0.0_c_float, 0.70_c_float)
    call fill_rect(0.0, 0.0, real(main_width), real(K_STATUS_H))
    k = int(mod(max(0_int64, vstats%frames - 1), int(K_RING, int64))) + 1
    last = vstats%frame_ms(k)
    if (in_view) then
       write(line, '(A,F12.3,A,F12.3,A,ES9.2,A,I0,A,F7.1,A,A)') 'x ', mouse_world%x / u, '  y ', &
            mouse_world%y / u, ' um | ', cam%zoom / u, ' um/px | boxes ', vstats%boxes_drawn, &
            ' | ', last, ' ms', merge(' | TREE', '       ', tree_mode)
    else
       write(line, '(A,ES9.2,A,I0,A,F7.1,A)') 'um/px ', cam%zoom / u, ' | boxes ', vstats%boxes_drawn, &
            ' | ', last, ' ms   (? keys)'
    end if
    if (associated(tiled)) then
       if (.not. tiled%geometry) line = trim(line) // ' | density'
    end if
    call glColor3f(0.9_c_float, 0.9_c_float, 0.6_c_float)
    call draw_text(6.0, 3.0, trim(adjustl(line)))

    if (show_hud) then
       call ring_stats(vstats%frame_ms, vstats%frames, fa, fp)
       call ring_stats(vstats%query_ms, vstats%frames, qa, qp)
       call ring_stats(vstats%draw_ms,  vstats%frames, da, dp)
       hits = 0; misses = 0
       do i = 1, n_layers
          hits = hits + layers(i)%chunk_cache%hits
          misses = misses + layers(i)%chunk_cache%misses
       end do
       if (associated(tiled)) then
          hits = hits + tiled%hits; misses = misses + tiled%misses
       end if
       call ReadProcessMemoryUsage(vm_kb, rss_kb)
       call glColor4f(0.0_c_float, 0.0_c_float, 0.0_c_float, 0.70_c_float)
       call fill_rect(real(main_width) - 330.0, real(win_h) - 156.0, real(main_width) - 4.0, real(win_h) - 4.0)
       call glColor3f(0.6_c_float, 1.0_c_float, 0.6_c_float)
       write(line, '(A,F6.1,A,F7.2,A,F7.2)') 'fps', 1000.0_real64 / max(0.01_real64, fa), &
            '  load s', vstats%load_s, '  1st s', vstats%first_frame_s
       call draw_text(real(main_width) - 324.0, real(win_h) - 20.0, trim(line))
       write(line, '(A,3F8.1)') 'frame ms l/a/95', last, fa, fp
       call draw_text(real(main_width) - 324.0, real(win_h) - 36.0, trim(line))
       write(line, '(A,3F8.1)') 'query ms l/a/95', vstats%query_ms(k), qa, qp
       call draw_text(real(main_width) - 324.0, real(win_h) - 52.0, trim(line))
       write(line, '(A,3F8.1)') 'draw  ms l/a/95', vstats%draw_ms(k), da, dp
       call draw_text(real(main_width) - 324.0, real(win_h) - 68.0, trim(line))
       write(line, '(A,I0)') 'boxes drawn ', vstats%boxes_drawn
       call draw_text(real(main_width) - 324.0, real(win_h) - 84.0, trim(line))
       write(line, '(A,I0,A,I0)') 'cache hit ', hits, ' miss ', misses
       call draw_text(real(main_width) - 324.0, real(win_h) - 100.0, trim(line))
       write(line, '(A,F9.1,A,F8.1,A)') 'RSS ', real(rss_kb, real64) / 1024.0_real64, ' MB  index ', &
            vstats%index_mb, ' MB'
       call draw_text(real(main_width) - 324.0, real(win_h) - 116.0, trim(line))
       write(line, '(A,I0)') 'frames ', vstats%frames
       call draw_text(real(main_width) - 324.0, real(win_h) - 132.0, trim(line))
       if (associated(tiled)) then
          if (tiled%geometry) then
             write(line, '(A,I0,A,F7.1,A,F7.0,A)') 'tiles ', tiled%n_tiles, '  decode ', tiled%decode_ms, &
                  ' ms  ', real(tiled%bytes, real64) / 1048576.0_real64, ' MB'
          else
             write(line, '(A,I0,A,ES9.2)') 'density level ', tiled%level, '  in view ', real(tiled%estimate, real64)
          end if
          call draw_text(real(main_width) - 324.0, real(win_h) - 148.0, trim(line))
       end if
    end if

    if (show_help) then
       call glColor4f(0.0_c_float, 0.0_c_float, 0.05_c_float, 0.85_c_float)
       call fill_rect(20.0, real(win_h) - 30.0 - real(K_NHELP * 15), 680.0, real(win_h) - 20.0)
       call glColor3f(1.0_c_float, 1.0_c_float, 1.0_c_float)
       do i = 1, K_NHELP
          call draw_text(28.0, real(win_h) - 22.0 - real(i * 15), trim(HELP_LINES(i)))
       end do
    end if
  end subroutine RenderOverlays

  !> Visible-box count of the last query (quads in the vertex buffers).
  integer(int64) function drawn_boxes(n_layers, layers)
    integer, intent(in) :: n_layers
    type(DrawnLayer), intent(in) :: layers(:)
    integer :: i
    drawn_boxes = 0
    do i = 1, n_layers
       if (layers(i)%visible) drawn_boxes = drawn_boxes + int(layers(i)%vbo_count, int64)
    end do
  end function drawn_boxes

  subroutine set_all(layers, n, v)
    type(DrawnLayer), intent(inout) :: layers(:)
    integer, intent(in) :: n
    logical, intent(in) :: v
    integer :: i
    do i = 1, n
       layers(i)%visible = v
    end do
  end subroutine set_all

  subroutine toggle_or_solo(layers, n, idx, solo)
    type(DrawnLayer), intent(inout) :: layers(:)
    integer, intent(in) :: n, idx
    logical, intent(in) :: solo
    if (idx < 1 .or. idx > n) return
    if (solo) then
       call set_all(layers, n, .false.)
       layers(idx)%visible = .true.
    else
       layers(idx)%visible = .not. layers(idx)%visible
    end if
  end subroutine toggle_or_solo

  logical function env_set(name, val)
    character(len=*), intent(in) :: name
    character(len=*), intent(out) :: val
    integer :: l, st
    val = ''
    call get_environment_variable(name, val, l, st)
    env_set = (st == 0 .and. l > 0)
  end function env_set

  subroutine SimpleWindow(n_layers, layers, win_x_in, win_y_in)
    integer, intent(in)             :: n_layers
    type(DrawnLayer), intent(inout) :: layers(:)
    real, intent(in)                :: win_x_in, win_y_in

    type(Box)         :: mbr, current_view
    type(c_ptr)       :: window
    type(CameraState) :: cam
    type(Point2D)     :: mouse_screen, mouse_world, msv

    integer :: ierr, idx, active_layer, target_depth, scroll, nrows_vis, bench_step, bench_n
    integer(c_int) :: live_win_x, live_win_y, prev_win_x, prev_win_y
    integer(c_int) :: ui_width, main_width, list_h, side, etype, code, mods, action
    integer(c_int) :: win_w0, win_h0
    real(c_double) :: mbr_span, max_zoom, ex, ey
    real(real64)   :: tf0, tq0, tq, td0, td
    logical :: tree_mode, dirty, should_quit, any_input, outlines, show_hud, show_help, in_view
    logical :: bench, shot_pending, shift
    character(len=512) :: shot_path, envv, keys_script
    logical :: keys_pending
    character(len=24)  :: bench_kind
    integer(K_COORDINATE_KIND) :: min_span

    if (n_layers <= 0) then
       write(*,'(A)') 'INFO: MAGVIEWER has 0 layers to display.'
       return
    end if

    ui_width     = K_UI_WIDTH
    active_layer = 1
    target_depth = 2
    tree_mode    = .false.
    outlines     = .true.
    show_hud     = .false.
    show_help    = .false.
    scroll       = 0
    prev_win_x   = -1_c_int
    prev_win_y   = -1_c_int
    win_w0 = int(win_x_in, c_int); win_h0 = int(win_y_in, c_int)
    if (env_set('MAGVIEWER_SIZE', envv)) then
       idx = index(envv, 'x')
       if (idx > 1) then
          read(envv(1:idx-1), *, iostat=ierr) win_w0
          read(envv(idx+1:), *, iostat=ierr) win_h0
       end if
    end if
    bench = env_set('MAGVIEWER_BENCH', envv)
    if (bench) bench = (envv(1:1) /= '0')
    shot_pending = env_set('MAGVIEWER_SCREENSHOT', shot_path)
    if (env_set('MAGVIEWER_HUD', envv)) show_hud = (envv(1:1) /= '0')
    keys_pending = env_set('MAGVIEWER_KEYS', keys_script)
    if (env_set('MAGVIEWER_HELP', envv)) show_help = (envv(1:1) /= '0')
    bench_step = 0
    bench_n = 57

    ! Global MBR from the compressed index roots
    call mbr%reset_to_infinity()
    if (associated(tiled)) mbr = tiled%mbr()
    do idx = 1, n_layers
       if (associated(layers(idx)%index)) then
          if (.not. layers(idx)%index%is_empty()) mbr = mbr + layers(idx)%index%layer_mbr
       end if
    end do
    if (.not. mbr%is_valid()) then
       mbr%x1 = 0; mbr%y1 = 0; mbr%x2 = 1000; mbr%y2 = 1000
    else
       mbr_span = max(real(mbr%x2 - mbr%x1, c_double), real(mbr%y2 - mbr%y1, c_double)) * 0.02_c_double
       call box_grow(mbr, int(max(1.0_c_double, mbr_span), kind=K_COORDINATE_KIND), &
            int(max(1.0_c_double, mbr_span), kind=K_COORDINATE_KIND))
    end if

    ! Palette (golden-angle hues, saturated, readable on the dark background)
    do idx = 1, n_layers
       layers(idx)%color(1) = 0.55_c_float + 0.45_c_float * sin(real(idx, c_float) * 2.39996_c_float)
       layers(idx)%color(2) = 0.55_c_float + 0.45_c_float * sin(real(idx, c_float) * 2.39996_c_float + 2.09_c_float)
       layers(idx)%color(3) = 0.55_c_float + 0.45_c_float * sin(real(idx, c_float) * 2.39996_c_float + 4.18_c_float)
       layers(idx)%visible     = .true.
       layers(idx)%cache_valid = .false.
       call layers(idx)%chunk_cache%init(K_CSI_DEFAULT_CACHE)
    end do

    call viewer_glfw_errors()
    if (glfwInit() == 0_c_int) error stop 'Error whilst initialising GLFW (is DISPLAY set?)'
    call viewer_context_hints()

    window = glfwCreateWindow(win_w0, win_h0, 'MAGVIEWER - ' // trim(vstats%file) // c_null_char, &
         c_null_ptr, c_null_ptr)
    if (.not. c_associated(window)) then
       call glfwTerminate()
       error stop 'Error whilst creating GLFW window'
    end if

    call glfwMakeContextCurrent(window)
    call viewer_input_install(window)
    call viewer_text_set_font(FONT_BITS)
    if (bench) call viewer_swap_interval(0_c_int)
    call glDisable(GL_DEPTH_TEST)
    call glEnable(GL_BLEND)
    call glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    cam%win_w = max(1.0_c_double, real(win_w0, c_double) - real(ui_width, c_double))
    cam%win_h = max(1.0_c_double, real(win_h0, c_double))
    call cam%fit_to_mbr(mbr)
    if (env_set('MAGVIEWER_VIEW', envv)) then        ! start view x1,y1,x2,y2 in um
       block
         real(c_double) :: v(4)
         type(Box) :: vb
         read(envv, *, iostat=ierr) v
         if (ierr == 0) then
            v = v * vstats%unit_per_um
            vb = Box(int(v(1), K_COORDINATE_KIND), int(v(2), K_COORDINATE_KIND), &
                     int(v(3), K_COORDINATE_KIND), int(v(4), K_COORDINATE_KIND))
            call cam%fit_to_mbr(vb)
         end if
       end block
    end if
    dirty = .true.
    write(*,'(A)') 'INFO: MAGVIEWER window open; press ? in the window for the key map.'

    do while (glfwWindowShouldClose(window) == 0_c_int)
       tf0 = wall_seconds()
       call glfwGetWindowSize(window, live_win_x, live_win_y)
       main_width  = max(1_c_int, live_win_x - ui_width)
       side        = max(1_c_int, min(ui_width, live_win_y / 3_c_int))
       list_h      = max(K_HEADER_H + K_ROW_H, live_win_y - side)
       nrows_vis   = int((list_h - K_HEADER_H) / K_ROW_H)
       any_input   = .false.
       should_quit = .false.

       if (live_win_x /= prev_win_x .or. live_win_y /= prev_win_y) then
          cam%win_w  = real(main_width, c_double)
          cam%win_h  = real(max(1_c_int, live_win_y), c_double)
          prev_win_x = live_win_x
          prev_win_y = live_win_y
          dirty      = .true.
       end if

       call glfwGetCursorPos(window, mouse_screen%x, mouse_screen%y)
       in_view = mouse_screen%x >= real(ui_width, c_double) .and. mouse_screen%x < real(live_win_x, c_double) &
                 .and. mouse_screen%y >= 0.0_c_double .and. mouse_screen%y < real(live_win_y, c_double)
       msv%x = mouse_screen%x - real(ui_width, c_double)
       msv%y = mouse_screen%y

       ! --- held keys: continuous pan / zoom ---
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

       ! --- scripted key presses (MAGVIEWER_KEYS) after the first frame, for tests ---
       if (keys_pending .and. vstats%frames >= 1) then
          ierr = viewer_input_script(keys_script, int(len_trim(keys_script), c_int))
          keys_pending = .false.
       end if

       ! --- discrete events from the C queue ---
       do while (viewer_input_next(etype, code, mods, action, ex, ey) /= 0)
          shift = iand(mods, GLFW_MOD_SHIFT) /= 0
          select case (etype)
          case (1)                                   ! key press / repeat
             if (action /= 1 .and. .not. (code == GLFW_KEY_PAGE_UP .or. code == GLFW_KEY_PAGE_DOWN)) cycle
             select case (code)
             case (GLFW_KEY_ESCAPE, GLFW_KEY_Q)
                should_quit = .true.
             case (GLFW_KEY_F)
                call cam%fit_to_mbr(mbr); dirty = .true.
             case (GLFW_KEY_A)
                if (iand(mods, GLFW_MOD_CONTROL) /= 0) then
                   call cam%fit_to_mbr(mbr)
                else
                   call set_all(layers, n_layers, .true.)
                end if
                dirty = .true.
             case (GLFW_KEY_R)
                tree_mode = .not. tree_mode; dirty = .true.
                write(*,'(A,L1)') 'MAGVIEWER: tree_mode = ', tree_mode
             case (GLFW_KEY_S)
                target_depth = max(0, target_depth - 1); dirty = .true.
                write(*,'(A,I0)') 'MAGVIEWER: target_depth = ', target_depth
             case (GLFW_KEY_T)
                target_depth = target_depth + 1; dirty = .true.
                write(*,'(A,I0)') 'MAGVIEWER: target_depth = ', target_depth
             case (GLFW_KEY_O)
                outlines = .not. outlines
             case (GLFW_KEY_G)
                show_hud = .not. show_hud
             case (GLFW_KEY_H)
                show_help = .not. show_help
             case (GLFW_KEY_P)
                call PrintPerfReport(n_layers, layers)
             case (GLFW_KEY_PAGE_UP)
                scroll = max(0, scroll - max(1, nrows_vis - 1))
             case (GLFW_KEY_PAGE_DOWN)
                scroll = max(0, min(n_layers - nrows_vis, scroll + max(1, nrows_vis - 1)))
             case (GLFW_KEY_0:GLFW_KEY_9)
                idx = code - GLFW_KEY_0
                if (idx == 0) idx = 10
                call toggle_or_solo(layers, n_layers, idx, shift)
                active_layer = idx; dirty = .true.
             end select
          case (2)                                   ! character
             if (code == iachar('?')) then
                call PrintKeyMap()
             else if (code == iachar('/')) then
                if (in_view) then
                   call cam%screen_to_world(msv, mouse_world)
                   write(*,'(A,F14.4,A,F14.4,A,F16.1,A,F16.1,A)') 'MAGVIEWER: cursor x ', &
                        mouse_world%x / vstats%unit_per_um, ' um  y ', mouse_world%y / vstats%unit_per_um, &
                        ' um  (', mouse_world%x, ', ', mouse_world%y, ' dbu)'
                end if
             end if
          case (3)                                   ! scroll wheel
             if (ex >= real(ui_width, c_double)) then
                call cam%zoom_about(merge(0.8_c_double, 1.25_c_double, code > 0), &
                     ex - real(ui_width, c_double), ey)
                dirty = .true.; any_input = .true.
             else
                scroll = max(0, min(max(0, n_layers - nrows_vis), scroll - 3 * code))
             end if
          case (4)                                   ! mouse button
             if (code == GLFW_MOUSE_BUTTON_LEFT .and. action == 1 .and. ex < real(ui_width, c_double) &
                 .and. ey < real(list_h, c_double)) then
                if (ey < real(K_HEADER_H, c_double)) then
                   call set_all(layers, n_layers, ex < 0.5_c_double * real(ui_width, c_double))
                else
                   idx = scroll + int((ey - real(K_HEADER_H, c_double)) / real(K_ROW_H, c_double)) + 1
                   call toggle_or_solo(layers, n_layers, idx, shift)
                   if (idx <= n_layers) active_layer = idx
                end if
                dirty = .true.
             end if
          end select
       end do
       if (should_quit) exit

       ! --- scripted benchmark path ---
       if (bench) then
          bench_step = bench_step + 1
          ! fit, 8 zoom steps (x0.6) about a point inside a tile, 30 half-screen pans east,
          ! 10 north, 8 zoom steps out
          if (bench_step == 1) then
             call cam%fit_to_mbr(mbr); bench_kind = 'fit'
          else if (bench_step <= 9) then
             call cam%zoom_about(0.6_c_double, 0.37_c_double * cam%win_w, 0.40_c_double * cam%win_h)
             bench_kind = 'zoom_in'
          else if (bench_step <= 39) then
             cam%pan_x = cam%pan_x + 0.5_c_double * cam%win_w * cam%zoom; bench_kind = 'pan_x'
          else if (bench_step <= 49) then
             cam%pan_y = cam%pan_y + 0.5_c_double * cam%win_h * cam%zoom; bench_kind = 'pan_y'
          else if (bench_step <= bench_n) then
             call cam%zoom_about(1.0_c_double / 0.6_c_double, 0.5_c_double * cam%win_w, &
                  0.5_c_double * cam%win_h)
             bench_kind = 'zoom_out'
          end if
          dirty = .true.
       end if

       max_zoom = max(1.0_c_double, &
            max(1.2_c_double * real(mbr%x2 - mbr%x1, c_double) / max(1.0_c_double, cam%win_w), &
                1.2_c_double * real(mbr%y2 - mbr%y1, c_double) / max(1.0_c_double, cam%win_h)))
       if (cam%zoom > max_zoom) cam%zoom = max_zoom
       call cam%update_bounds()
       call cam%screen_to_world(msv, mouse_world)

       ! --- query (index -> vertex buffers) ---
       tq0 = wall_seconds()
       current_view = cam%to_view_box()
       if (dirty) then
          call InvalidateLayerCaches(n_layers, layers)
          dirty = .false.
       end if
       min_span = int(max(0.0_c_double, cam%zoom * 0.5_c_double), kind=K_COORDINATE_KIND)
       call UpdateVisibleLayerCaches(n_layers, layers, current_view, tree_mode, target_depth, min_span)
       tq = since_ms(tq0)
       vstats%boxes_drawn = drawn_boxes(n_layers, layers)

       ! --- draw ---
       td0 = wall_seconds()
       call glViewport(0_c_int, 0_c_int, live_win_x, live_win_y)
       call glClearColor(0.08_c_float, 0.08_c_float, 0.10_c_float, 1.0_c_float)
       call glClear(GL_COLOR_BUFFER_BIT)
       call RenderMainViewport(n_layers, layers, ui_width, main_width, live_win_y, cam, tree_mode, outlines)
       call RenderMinimap(mbr, current_view, ui_width, side, n_layers, layers)
       call RenderLayerPanel(n_layers, layers, active_layer, scroll, ui_width, side, list_h)
       call RenderOverlays(n_layers, layers, ui_width, main_width, live_win_y, cam, mouse_world, in_view, &
            show_hud, show_help, tree_mode)
       call viewer_finish()                         ! include GPU time in the draw figure
       td = since_ms(td0)

       call record_frame(since_ms(tf0), tq, td)
       if (vstats%first_frame_s < 0.0_real64) then
          vstats%first_frame_s = wall_seconds() - real(vstats%t0_ticks, real64) / clock_rate_r()
          write(*,'(A,F9.3,A,F9.2,A,F9.2,A,I0)') 'PERF: first frame ', vstats%first_frame_s, &
               ' s after start (query ', tq, ' ms, draw ', td, ' ms, boxes ', vstats%boxes_drawn
       end if
       if (bench .and. bench_step <= bench_n) then
          write(*,'(A,I3,1X,A10,A,F10.3,A,F10.3,A,I10,A,ES10.3,A,A)') 'BENCH step ', bench_step, bench_kind, &
               ' query_ms ', tq, ' draw_ms ', td, ' boxes ', vstats%boxes_drawn, ' um_per_px ', &
               cam%zoom / vstats%unit_per_um, ' mode ', bench_mode()
       end if

       if (shot_pending .and. .not. keys_pending .and. ((.not. bench .and. vstats%frames >= 3) .or. (bench .and. bench_step >= bench_n))) then
          if (viewer_screenshot_ppm(trim(shot_path) // c_null_char, live_win_x, live_win_y) == 0) then
             write(*,'(A,A)') 'INFO: screenshot written to ', trim(shot_path)
          else
             write(*,'(A,A)') 'ERROR: screenshot failed: ', trim(shot_path)
          end if
          shot_pending = .false.
          should_quit = .true.
       end if
       call glfwSwapBuffers(window)
       if (bench .and. bench_step >= bench_n) then
          call PrintPerfReport(n_layers, layers)
          should_quit = .true.
       end if
       if (should_quit) exit

       if (any_input .or. bench) then
          call glfwPollEvents()
       else
          call glfwWaitEventsTimeout(0.05_c_double)
       end if
    end do

    do idx = 1, n_layers
       call layers(idx)%destroy()
    end do
    call glfwDestroyWindow(window)
    call glfwTerminate()
  end subroutine SimpleWindow

  real(real64) function clock_rate_r()
    integer(int64) :: r
    call system_clock(count_rate=r)
    clock_rate_r = real(max(1_int64, r), real64)
  end function clock_rate_r

end module DrawUsingOpenGL

program magviewer
  use, intrinsic :: iso_fortran_env, only: int64, real64
  use, intrinsic :: iso_c_binding,   only: c_int
  use DrawUsingOpenGL,              only: DrawnLayer, SimpleWindow, UpdateVisibleLayerCaches, K_MAX_OPENGL_CAPACITY, &
                                          vstats, PrintKeyMap, wall_seconds, tiled
  use LayoutFilesModule,            only: OasisTables, SidecarLayer
  use ViewerTileSourceModule,       only: TiledLayout
  use CompressedSpatialIndexModule, only: CompressedSpatialIndex, K_CSI_DEFAULT_CACHE
  use LayoutDBLoaderModule,         only: LoadLayoutDBToCompressedIndex, viewer_last_read_s, viewer_last_index_s
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
  type(OasisTables) :: tables
  type(TiledLayout), allocatable, target :: tiled_src
  type(SidecarLayer) :: sl
  character(len=256) :: lname
  character(len=32)  :: envv
  integer :: ll, dd, k, envl, envs
  logical :: use_sidecar
  type(Design), target :: load_design
  type(Layer),  target :: single_layer
  type(CompressedSpatialIndex), allocatable, target :: indices(:)
  character(len=64), allocatable :: layer_names(:)
  type(DrawnLayer), allocatable  :: drawn_layers(:)
  type(Box) :: global_mbr, zoom_view
  integer(K_COORDINATE_KIND) :: cx, cy, qw, qh, min_span

  call system_clock(count_rate=clock_rate)
  call system_clock(count=vstats%t0_ticks)

  narg = command_argument_count()
  if (narg < 1) then
     write(*,'(A)') 'Usage: MAGVIEWER.exe <layout.mag|layout.gds|layout.oas|layer.bin|layer.snap> ' // &
          '[max_layers] [--hier] [--info|--headless] [--keys]'
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
     case ('--keys', '-?', '--help')
        call PrintKeyMap()
        stop 0
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

  vstats%file = trim(filename)
  vstats%format = trim(fext)
  if (trim(fext) == 'oas' .or. trim(fext) == 'oasis') then
     vstats%format = 'OASIS'
     if (tables%open(trim(filename))) then
        vstats%strict    = merge(1, 0, tables%strict())
        vstats%tables_ms = tables%open_ms()
        if (tables%unit() > 0.0_real64) vstats%unit_per_um = tables%unit()
        write(*,'(A,I0,A,A,A,I0,A,F10.3,A,F8.1)') 'INFO: OASIS tables: strict=', vstats%strict, ' top=', &
             tables%top(), ' layernames=', tables%n_layernames(), ' read in ', vstats%tables_ms, &
             ' ms, unit/um=', vstats%unit_per_um
     end if
  else if (is_layoutdb) then
     vstats%format = 'GDSII'
  else if (.not. is_single_layer) then
     vstats%format = 'Magic'
  end if

  call system_clock(count=t_start)
  call global_mbr%reset_to_infinity()

  ! Flat OASIS: open through the sidecar index (density pyramid + on-demand CBLOCK decode).
  use_sidecar = vstats%format == 'OASIS' .and. .not. use_hier
  call get_environment_variable('MAGVIEWER_SIDECAR', envv, envl, envs)
  if (envs == 0 .and. envl > 0) use_sidecar = use_sidecar .and. envv(1:1) /= '0'
  if (use_sidecar) then
     allocate(tiled_src)
     if (tiled_src%open(trim(filename))) then
        write(*,'(A,A,A,F8.3,A)') 'INFO: MAGVIEWER sidecar ', tiled_src%side%path, &
             merge(' built ', ' opened', tiled_src%side%built), tiled_src%side%open_s, ' s'
        if (tiled_src%side%built) write(*,'(A,A)') 'INFO: ', tiled_src%side%summary
        write(*,'(A,I0,A,I0,A,I0,A,I0,A,I0)') 'INFO: sidecar units=', tiled_src%side%n_units, ' dependent=', &
             tiled_src%side%n_dependent, ' layers=', tiled_src%side%n_layers, ' boxes=', &
             tiled_src%side%total_boxes, ' density levels=', tiled_src%side%n_levels
        tiled => tiled_src
        if (tiled_src%side%unit > 0.0_real64) vstats%unit_per_um = tiled_src%side%unit
     else
        write(*,'(A,A,A)') 'INFO: no sidecar (', tiled_src%side%error, '); loading the whole file'
        deallocate(tiled_src)
     end if
  end if

  if (associated(tiled)) then
     num_layers = tiled%n_layers()
     allocate(indices(max(1, num_layers)), layer_names(max(1, num_layers)))
     do i = 1, num_layers
        sl = tiled%layer(i)
        write(layer_names(i), '(A,I0,A,I0)') 'L', sl%layer, '_D', sl%datatype
     end do
     global_mbr = tiled%mbr()
     vstats%read_s = tiled%side%open_s
     vstats%index_s = 0.0_real64
  else if (is_single_layer) then
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
     vstats%read_s  = viewer_last_read_s
     vstats%index_s = viewer_last_index_s
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
     drawn_layers(i)%label   = trim(layer_names(i))
     ! Layer label: OASIS LAYERNAME when present, else layer/datatype (from names like L67_D20).
     if (parse_ld(layer_names(i), ll, dd)) then
        lname = tables%layer_name(int(ll, int64), int(dd, int64))
        write(drawn_layers(i)%label, '(I0,A,I0)') ll, '/', dd
        if (len_trim(lname) > 0) drawn_layers(i)%label = trim(lname) // '  ' // trim(drawn_layers(i)%label)
     end if
     drawn_layers(i)%index   => indices(i)
     drawn_layers(i)%visible = .true.
     if (associated(tiled)) then
        sl = tiled%layer(i)
        total_boxes = total_boxes + sl%count
     else if (.not. indices(i)%is_empty()) then
        total_boxes    = total_boxes    + indices(i)%n_boxes
        total_tree_b   = total_tree_b   + indices(i)%tree_bytes()
        total_chunk_b  = total_chunk_b  + indices(i)%chunk_bytes()
        total_comp_b   = total_comp_b   + indices(i)%memory_bytes()
        total_uncomp_b = total_uncomp_b + indices(i)%uncompressed_bytes()
        global_mbr     = global_mbr     + indices(i)%layer_mbr
     end if
  end do

  if (associated(tiled)) global_mbr = tiled%mbr()
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

  call tables%close()
  vstats%load_s = t_load_s
  if (vstats%read_s <= 0.0_real64) vstats%read_s = t_load_s
  vstats%total_boxes = total_boxes
  vstats%index_mb = real(total_comp_b, real64) / 1048576.0_real64
  write(*,'(A)') 'INFO: layers:'
  do i = 1, num_layers
     write(*,'(A,I4,2X,A)') '  ', i, trim(drawn_layers(i)%label)
  end do

  call SimpleWindow(num_layers, drawn_layers, 1600.0, 900.0)

  do i = 1, num_layers
     call indices(i)%destroy()
  end do

contains

  !> Layer and datatype from a loader layer name such as "L67_D20" (or "67/20", "67:20").
  logical function parse_ld(nm, l, d)
    character(len=*), intent(in) :: nm
    integer, intent(out) :: l, d
    integer :: p, q, ios
    character(len=64) :: t
    parse_ld = .false.; l = -1; d = -1
    t = adjustl(nm)
    if (t(1:1) == 'L') then
       p = index(t, '_D')
       if (p <= 2) return
       read(t(2:p-1), *, iostat=ios) l; if (ios /= 0) return
       read(t(p+2:), *, iostat=ios) d; if (ios /= 0) return
    else
       q = scan(t, '/:')
       if (q <= 1) return
       read(t(1:q-1), *, iostat=ios) l; if (ios /= 0) return
       read(t(q+1:), *, iostat=ios) d; if (ios /= 0) return
    end if
    parse_ld = (l >= 0 .and. d >= 0)
  end function parse_ld


end program magviewer
