! File    : magparser_display.f90
! Author  : Sandeep Koranne (C) 2026
! Purpose : OpenGL display of MAGIC VLSI rectangles
module DrawUsingOpenGL
  use CommonModule
  use GeometryModule
  use DesignModule
  use RTreeBuilder
  use MortonSortModule
  use KLDataModule
  use glf90w
  use iso_fortran_env, only: int32, int64, real64
  use iso_c_binding
  implicit none
  interface
     ! --- OpenGL Core Functions ---
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

     subroutine glVertex2f(x, y) bind(C, name="glVertex2f")
       import :: c_float
       real(c_float), value :: x, y
     end subroutine glVertex2f

     ! --- glShadeModel(GLenum mode) ---
     subroutine glShadeModel(mode) bind(C, name="glShadeModel")
       import :: c_int
       integer(c_int), value :: mode
     end subroutine glShadeModel

     ! --- glNormal3f(GLfloat nx, GLfloat ny, GLfloat nz) ---
     subroutine glNormal3f(nx, ny, nz) bind(C, name="glNormal3f")
       import :: c_float
       real(c_float), value :: nx, ny, nz
     end subroutine glNormal3f

     ! --- glEnable(GLenum cap) ---
     subroutine glEnable(cap) bind(C, name="glEnable")
       import :: c_int
       integer(c_int), value :: cap
     end subroutine glEnable

     ! --- glDisable(GLenum cap) ---
     subroutine glDisable(cap) bind(C, name="glDisable")
       import :: c_int
       integer(c_int), value :: cap
     end subroutine glDisable

     ! --- glLightfv(GLenum light, GLenum pname, const GLfloat *params) ---
     ! Note: params is passed by reference (no VALUE attribute) because it is a C pointer/array
     subroutine glLightfv(light, pname, params) bind(C, name="glLightfv")
       import :: c_int, c_float
       integer(c_int), value :: light, pname
       real(c_float), intent(in) :: params(*)
     end subroutine glLightfv

     ! --- glMaterialfv(GLenum face, GLenum pname, const GLfloat *params) ---
     subroutine glMaterialfv(face, pname, params) bind(C, name="glMaterialfv")
       import :: c_int, c_float
       integer(c_int), value :: face, pname
       real(c_float), intent(in) :: params(*)
     end subroutine glMaterialfv

     ! --- glMaterialf(GLenum face, GLenum pname, GLfloat param) ---
     subroutine glMaterialf(face, pname, param) bind(C, name="glMaterialf")
       import :: c_int, c_float
       integer(c_int), value :: face, pname
       real(c_float), value :: param
     end subroutine glMaterialf

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

     ! Overloaded variation for multi-element array drawing
     subroutine glDrawArrays(mode, first, count) bind(C, name="glDrawArrays")
       import :: c_int
       integer(c_int), value :: mode, first, count
     end subroutine glDrawArrays
     subroutine glPolygonMode(face, mode) bind(C, name="glPolygonMode")
       import :: c_int
       integer(c_int), value :: face, mode
     end subroutine glPolygonMode

     ! Optional: If you want to make the lines thicker
     subroutine glLineWidth(width) bind(C, name="glLineWidth")
       import :: c_float
       real(c_float), value :: width
     end subroutine glLineWidth
  end interface
  ! OpenGL Constants
  integer(c_int), parameter :: GL_COLOR_BUFFER_BIT = z'00004000'
  integer(c_int), parameter :: GL_QUADS            = z'00000007'
  integer(c_int), parameter :: GL_FLAT = z'1D00'
  integer(c_int), parameter :: GL_LIGHTING   = z'0B50'
  integer(c_int), parameter :: GL_LIGHT0     = z'4000'
  integer(c_int), parameter :: GL_POSITION   = z'1203'
  integer(c_int), parameter :: GL_DIFFUSE    = z'1201'
  integer(c_int), parameter :: GL_COLOR_MATERIAL   = z'0B57'
  integer(c_int), parameter :: GL_FRONT          = z'0404'
  integer(c_int), parameter :: GL_AMBIENT        = z'1200'
  integer(c_int), parameter :: GL_SPECULAR       = z'1202'
  integer(c_int), parameter :: GL_SHININESS      = z'1204'
  integer(c_int), parameter :: GL_AMBIENT_AND_DIFFUSE = z'1602'
  integer(c_int), parameter :: GL_PROJECTION = z'1701'
  integer(c_int), parameter :: GL_MODELVIEW  = z'1700'
  integer(c_int), parameter :: GL_VERTEX_ARRAY = z'8074'
  ! Replace your GL_INT line with these:
  integer(c_int), parameter :: GL_SHORT = z'1402'
  integer(c_int), parameter :: GL_INT   = z'1404'
  integer(c_int), parameter :: GL_FLOAT = z'1406'
  integer(c_int), parameter :: GL_FRONT_AND_BACK = z'0408'
  integer(c_int), parameter :: GL_LINE           = z'1B01'
  integer(c_int), parameter :: GL_FILL           = z'1B02'
  public:: SimpleWindow
  ! A generic 2D coordinate (can be used for Screen pixels OR World coordinates)
  type :: Point2D
     real(c_double) :: x
     real(c_double) :: y
  end type Point2D

  ! Holds all the data about our current view and screen dimensions
  type :: CameraState
     real(c_double) :: win_w, win_h
     real(c_double) :: left, right, bottom, top
  end type CameraState
contains
  ! ---------------------------------------------------------
  ! Convert Screen Pixels (Viewport) to Application (World) Space
  ! ---------------------------------------------------------
  pure subroutine ScreenToWorld(screen, cam, world)
    implicit none
    type(Point2D), intent(in)     :: screen
    type(CameraState), intent(in) :: cam
    type(Point2D), intent(out)    :: world

    world%x = cam%left + (screen%x / cam%win_w) * (cam%right - cam%left)
    world%y = cam%top - (screen%y / cam%win_h) * (cam%top - cam%bottom)
  end subroutine ScreenToWorld

  ! ---------------------------------------------------------
  ! Convert Application (World) Space to Screen Pixels (Viewport)
  ! ---------------------------------------------------------
  pure subroutine WorldToScreen(world, cam, screen)
    implicit none
    type(Point2D), intent(in)     :: world
    type(CameraState), intent(in) :: cam
    type(Point2D), intent(out)    :: screen

    screen%x = ((world%x - cam%left) / (cam%right - cam%left)) * cam%win_w
    screen%y = cam%win_h - (((world%y - cam%bottom) / (cam%top - cam%bottom)) * cam%win_h)
  end subroutine WorldToScreen
  subroutine handle_error(code, desc)
    implicit none
    integer, intent(in) :: code
    character(*), intent(in) :: desc

    print '(''Error '', I8,'' : '',A)', code, desc
  end subroutine handle_error
  ! ---------------------------------------------------------
  ! Calculates the World-Space Bounding Box of the Current View
  ! ---------------------------------------------------------
  pure function GetCurrentViewportWorld(win_x, win_y, pan_x, pan_y, zoom) result(view_box)
    implicit none
    real(c_double), intent(in) :: win_x, win_y   ! Live window dimensions
    real(c_double), intent(in) :: pan_x, pan_y   ! Current pan offsets
    real(c_double), intent(in) :: zoom           ! Current zoom level
    type(Box)                  :: view_box       ! The resulting world boundary

    real(c_double) :: center_x, center_y, half_w, half_h

    ! 1. Calculate the true world center of the camera
    center_x = (win_x / 2.0_c_double) + pan_x
    center_y = (win_y / 2.0_c_double) + pan_y

    ! 2. Calculate the scaled half-dimensions of the view
    half_w = (win_x / 2.0_c_double) * zoom
    half_h = (win_y / 2.0_c_double) * zoom

    ! 3. Map the boundaries to your Box structure
    ! Note: Using int() because standard VLSI Box coordinates are typically integers.
    ! If your GeometryModule Box uses real/float, simply remove the int() casts.
    view_box%x1 = int(center_x - half_w)   ! Left boundary
    view_box%y1 = int(center_y - half_h)   ! Bottom boundary
    view_box%x2 = int(center_x + half_w)   ! Right boundary
    view_box%y2 = int(center_y + half_h)   ! Top boundary

  end function GetCurrentViewportWorld
  pure recursive subroutine SearchTreeMBRsRecursive(tree_nodes, index, qbox, &
       max_depth, current_depth, out_mbrs, num_mbrs, max_capacity)

    type(RTreeNode), intent(in)        :: tree_nodes(:)
    integer(kind=int64), intent(in)    :: index
    type(Box), intent(in)              :: qbox
    integer, intent(in)                :: max_depth
    integer, intent(in)                :: current_depth
    type(Box), intent(inout)           :: out_mbrs(:)
    integer, intent(inout)             :: num_mbrs
    integer, intent(in)                :: max_capacity

    integer(kind=int64) :: child_idx
    logical             :: overlapx, overlapy
    type(RTreeNode)     :: childNode

    ! Safeguard: If the tree is empty or buffer is full, exit immediately
    if (size(tree_nodes) == 0 .or. num_mbrs >= max_capacity) return

    ! 1. Initial check on the current node
    overlapx = max(tree_nodes(index)%mbr%x1, qbox%x1) <= min(tree_nodes(index)%mbr%x2, qbox%x2)
    if (overlapx) then
       overlapy = max(tree_nodes(index)%mbr%y1, qbox%y1) <= min(tree_nodes(index)%mbr%y2, qbox%y2)
    else
       overlapy = .false.
    end if

    if (.not. (overlapx .and. overlapy)) return

    ! 2. Check if we should stop recursing (either reached max depth or hit a leaf)
    if (current_depth == max_depth .or. tree_nodes(index)%IsLeaf) then
       num_mbrs = num_mbrs + 1
       out_mbrs(num_mbrs) = tree_nodes(index)%mbr
       return
    end if

    ! 3. Internal Node: Iterate over contiguous children
    do child_idx = tree_nodes(index)%ChildStart, tree_nodes(index)%ChildStart + tree_nodes(index)%NumChildren - 1

       ! Stop processing siblings if capacity was hit in a previous recursive call
       if (num_mbrs >= max_capacity) return 

       childNode = tree_nodes(child_idx)

       ! Inline short-circuit overlap check for the child
       overlapx = max(childNode%mbr%x1, qbox%x1) <= min(childNode%mbr%x2, qbox%x2)
       if (overlapx) then
          overlapy = max(childNode%mbr%y1, qbox%y1) <= min(childNode%mbr%y2, qbox%y2)

          ! Recurse deeper if bounding boxes overlap
          if (overlapy) then
             call SearchTreeMBRsRecursive(tree_nodes, child_idx, qbox, &
                  max_depth, current_depth + 1, out_mbrs, num_mbrs, max_capacity)
          end if
       end if
    end do

  end subroutine SearchTreeMBRsRecursive
  pure recursive subroutine SearchTreeBoxesRecursive(tree_nodes, index, qbox, &
       layer_boxes, n_used, out_boxes, num_boxes, max_capacity)

    type(RTreeNode), intent(in)        :: tree_nodes(:)
    integer(kind=int64), intent(in)    :: index
    type(Box), intent(in)              :: qbox
    type(Box), intent(in)              :: layer_boxes(:)  ! The global array of actual boxes
    integer(kind=int64), intent(in)    :: n_used          ! Total number of valid boxes
    type(Box), intent(inout)           :: out_boxes(:)    ! GPU Rendering buffer
    integer, intent(inout)             :: num_boxes
    integer, intent(in)                :: max_capacity

    integer(kind=int64) :: child_idx, k, leaf_end
    logical             :: overlapx, overlapy
    type(RTreeNode)     :: childNode

    ! Safeguard: If the tree is empty or rendering buffer is full, exit immediately
    if (size(tree_nodes) == 0 .or. num_boxes >= max_capacity) return

    ! 1. Initial check on the current node's MBR
    overlapx = max(tree_nodes(index)%mbr%x1, qbox%x1) <= min(tree_nodes(index)%mbr%x2, qbox%x2)
    if (overlapx) then
       overlapy = max(tree_nodes(index)%mbr%y1, qbox%y1) <= min(tree_nodes(index)%mbr%y2, qbox%y2)
    else
       overlapy = .false.
    end if

    if (.not. (overlapx .and. overlapy)) return

    ! 2. Check if we hit a leaf node
    if (tree_nodes(index)%IsLeaf) then

       ! We reached a leaf. Iterate through the actual boxes stored in layer_boxes
       leaf_end = min(tree_nodes(index)%ChildStart + K_LEAF_CAPACITY - 1, n_used)

       do k = tree_nodes(index)%ChildStart, leaf_end

          ! Strict capacity check before adding a new box
          if (num_boxes >= max_capacity) return

          ! Fine-grained inline check: Does the actual box overlap with the query view?
          overlapx = max(layer_boxes(k)%x1, qbox%x1) <= min(layer_boxes(k)%x2, qbox%x2)
          if (overlapx) then
             overlapy = max(layer_boxes(k)%y1, qbox%y1) <= min(layer_boxes(k)%y2, qbox%y2)

             if (overlapy) then
                num_boxes = num_boxes + 1
                out_boxes(num_boxes) = layer_boxes(k)
             end if
          end if
       end do

       return
    end if

    ! 3. Internal Node: Iterate over contiguous children
    do child_idx = tree_nodes(index)%ChildStart, tree_nodes(index)%ChildStart + tree_nodes(index)%NumChildren - 1

       ! Stop processing siblings if capacity was hit in a previous recursive call
       if (num_boxes >= max_capacity) return 

       childNode = tree_nodes(child_idx)

       ! Inline short-circuit overlap check for the child's MBR
       overlapx = max(childNode%mbr%x1, qbox%x1) <= min(childNode%mbr%x2, qbox%x2)
       if (overlapx) then
          overlapy = max(childNode%mbr%y1, qbox%y1) <= min(childNode%mbr%y2, qbox%y2)

          ! Recurse deeper if bounding boxes overlap
          if (overlapy) then
             call SearchTreeBoxesRecursive(tree_nodes, child_idx, qbox, &
                  layer_boxes, n_used, out_boxes, num_boxes, max_capacity)
          end if
       end if
    end do

  end subroutine SearchTreeBoxesRecursive
  subroutine PerformBoxFilling(input_layer, current_view, visible_mbrs, vertex_buffer, num_visible)
    type(Layer), intent(in)               :: input_layer
    type(Box), intent(in)                 :: current_view
    type(Box), allocatable, intent(inout) :: visible_mbrs(:)
    integer, intent(out)                  :: num_visible
    integer(c_int), allocatable, intent(inout) :: vertex_buffer(:)
    integer :: max_render_capacity, i, idx

    ! Define a safe upper limit for OpenGL rendering to prevent memory blowouts
    max_render_capacity = 500000 

    if (.not. allocated(visible_mbrs)) then
       allocate(visible_mbrs(max_render_capacity))
    else if (size(visible_mbrs) < max_render_capacity) then
       deallocate(visible_mbrs)
       allocate(visible_mbrs(max_render_capacity))
    end if

    num_visible = 0

    ! Start recursive search from the root (assuming root_index is valid, usually 1)
    call SearchTreeBoxesRecursive( &
         tree_nodes    = input_layer%tree%tree_nodes, &
         index         = input_layer%tree%root_index, &
         qbox          = current_view, &
         layer_boxes   = input_layer%layer_boxes,&
         n_used        = input_layer%n_used,&
         out_boxes      = visible_mbrs, &
         num_boxes      = num_visible, &
         max_capacity  = max_render_capacity &
         )

    ! write(*,*) 'Found ', num_visible, ' MBRs for OpenGL rendering at depth ', target_depth
    if( allocated( vertex_buffer ) ) then
       if( size( vertex_buffer ) < ( num_visible*8) ) then
          deallocate( vertex_buffer )
          allocate( vertex_buffer( num_visible*8) )
       end if
    else
       allocate( vertex_buffer( num_visible*8) )
    end if
    idx = 1
    do i = 1, num_visible
       ! Vertex 1: Top-Left
       vertex_buffer(idx)   = visible_mbrs(i)%x1
       vertex_buffer(idx+1) = visible_mbrs(i)%y1
       ! Vertex 2: Top-Right
       vertex_buffer(idx+2) = visible_mbrs(i)%x2
       vertex_buffer(idx+3) = visible_mbrs(i)%y1
       ! Vertex 3: Bottom-Right
       vertex_buffer(idx+4) = visible_mbrs(i)%x2
       vertex_buffer(idx+5) = visible_mbrs(i)%y2
       ! Vertex 4: Bottom-Left
       vertex_buffer(idx+6) = visible_mbrs(i)%x1
       vertex_buffer(idx+7) = visible_mbrs(i)%y2
       idx = idx + 8
    end do

  end subroutine PerformBoxFilling
  subroutine PerformMBRFilling(input_layer, current_view, target_depth, visible_mbrs, vertex_buffer, num_visible)
    type(Layer), intent(in)               :: input_layer
    type(Box), intent(in)                 :: current_view
    integer, intent(in)                   :: target_depth
    type(Box), allocatable, intent(inout) :: visible_mbrs(:)
    integer, intent(out)                  :: num_visible
    integer(c_int), allocatable, intent(inout) :: vertex_buffer(:)
    integer :: max_render_capacity, i, idx

    ! Define a safe upper limit for OpenGL rendering to prevent memory blowouts
    max_render_capacity = 500000 

    if (.not. allocated(visible_mbrs)) then
       allocate(visible_mbrs(max_render_capacity))
    else if (size(visible_mbrs) < max_render_capacity) then
       deallocate(visible_mbrs)
       allocate(visible_mbrs(max_render_capacity))
    end if

    num_visible = 0

    ! Start recursive search from the root (assuming root_index is valid, usually 1)
    call SearchTreeMBRsRecursive( &
         tree_nodes    = input_layer%tree%tree_nodes, &
         index         = input_layer%tree%root_index, &
         qbox          = current_view, &
         max_depth     = target_depth, &
         current_depth = 0, &
         out_mbrs      = visible_mbrs, &
         num_mbrs      = num_visible, &
         max_capacity  = max_render_capacity &
         )

    ! write(*,*) 'Found ', num_visible, ' MBRs for OpenGL rendering at depth ', target_depth
    if( allocated( vertex_buffer ) ) then
       if( size( vertex_buffer ) < ( num_visible*8) ) then
          deallocate( vertex_buffer )
          allocate( vertex_buffer( num_visible*8) )
       end if
    else
       allocate( vertex_buffer( num_visible*8) )
    end if
    idx = 1
    do i = 1, num_visible
       ! Vertex 1: Top-Left
       vertex_buffer(idx)   = visible_mbrs(i)%x1
       vertex_buffer(idx+1) = visible_mbrs(i)%y1
       ! Vertex 2: Top-Right
       vertex_buffer(idx+2) = visible_mbrs(i)%x2
       vertex_buffer(idx+3) = visible_mbrs(i)%y1
       ! Vertex 3: Bottom-Right
       vertex_buffer(idx+4) = visible_mbrs(i)%x2
       vertex_buffer(idx+5) = visible_mbrs(i)%y2
       ! Vertex 4: Bottom-Left
       vertex_buffer(idx+6) = visible_mbrs(i)%x1
       vertex_buffer(idx+7) = visible_mbrs(i)%y2
       idx = idx + 8
    end do

  end subroutine PerformMBRFilling

  subroutine PerformCoordinateFilling( input_layer, current_view, visible_boxes, vertex_buffer, num_visible)
    type(Layer), intent(in) :: input_layer
    type(Box), intent(in)   :: current_view
    type(Box), allocatable, intent(inout) :: visible_boxes(:)
    integer(c_int), allocatable, intent(inout) :: vertex_buffer(:)
    integer, intent(out) :: num_visible
    integer(kind=int64) :: leafboxes(K_MAX_SEARCH_LEAVES) ! better choose a large number
    integer(kind=int64) :: number_leaves
    integer(kind=int64) :: i, idx, j, k
    !write(*,*) 'Searching for current_view = ', current_view
    !if( NeedsRTree( input_layer ) ) error stop "ERROR: Layer needs to have an RTree established."
    call SearchTree( input_layer%tree%tree_nodes, input_layer%tree%root_index, &
         current_view, leafboxes, number_leaves )
    if( allocated( visible_boxes ) ) then
       if( size( visible_boxes ) < number_leaves*K_LEAF_CAPACITY ) then
          deallocate( visible_boxes )
          allocate( visible_boxes( number_leaves*K_LEAF_CAPACITY ) )
       end if
    else
       allocate( visible_boxes( number_leaves*K_LEAF_CAPACITY ) )
    end if

    !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
    num_visible = 0
    if( number_leaves > 0 ) then
       !write(*,*) 'i=',i,' |Q| = ', number_leaves, ' ', leafboxes(1:number_leaves)
       outer: do j=1,number_leaves
          over_leaves: do k=leafboxes(j),min(leafboxes(j)+K_LEAF_CAPACITY-1, input_layer%n_used)
             if( box_interact( current_view, input_layer%layer_boxes(k)) ) then
                num_visible = num_visible + 1
                visible_boxes(num_visible) = input_layer%layer_boxes(k)
             end if
          end do over_leaves
       end do outer
    end if
    !write(*,*) 'Found ', num_visible, ' boxes in FRUSTUM'
    if( allocated( vertex_buffer ) ) then
       if( size( vertex_buffer ) < ( num_visible*8) ) then
          deallocate( vertex_buffer )
          allocate( vertex_buffer( num_visible*8) )
       end if
    else
       allocate( vertex_buffer( num_visible*8) )
    end if
    idx = 1
    do i = 1, num_visible
       ! Vertex 1: Top-Left
       vertex_buffer(idx)   = visible_boxes(i)%x1
       vertex_buffer(idx+1) = visible_boxes(i)%y1
       ! Vertex 2: Top-Right
       vertex_buffer(idx+2) = visible_boxes(i)%x2
       vertex_buffer(idx+3) = visible_boxes(i)%y1
       ! Vertex 3: Bottom-Right
       vertex_buffer(idx+4) = visible_boxes(i)%x2
       vertex_buffer(idx+5) = visible_boxes(i)%y2
       ! Vertex 4: Bottom-Left
       vertex_buffer(idx+6) = visible_boxes(i)%x1
       vertex_buffer(idx+7) = visible_boxes(i)%y2
       idx = idx + 8
    end do
  end subroutine PerformCoordinateFilling

  ! We have a snappy compressed file on disk which contains the
  ! boxes in sorted order (same order as RTree).
  ! Thererfore it seems we dont have to load the whole file in
  ! memory to display the MBR of the RTree. We can load the
  ! chunks, decompress them, and construct the MBR filling. Then
  ! when an appropriate LOD is needed we will expand the
  ! requested chunks which lie within our view port. Devise a
  ! careful and well thought out plan to perform this task and
  ! generate idiomatic modern Fortran for this task. The goal is
  ! to write a series of functions and subroutines which take
  ! the SNAPPY compressed file as input and the current view
  ! port as we did in
  ! Step 1: build the RTree MBR to file level chunk index map
  ! Step 2: in Tree Mode this is sufficient to render the tree
  ! bbox MBR
  ! Step 3: after a LOD is requested, load the chunks from file
  ! in memory, decompress and return VERTEX BUFFER using
  ! visible_boxes using the code inside this
  ! Which is simply a RTree traversal


  !> The main rendering window stuff follows


  subroutine SimpleWindow(input_layer, WIN_X, WIN_Y)
    type(Layer), intent(in) :: input_layer
    real, intent(in) :: WIN_X, WIN_Y
    type(Box) :: MBR
    type(GLFWwindow) :: window
    integer :: ierr
    integer(c_int), allocatable, target :: vertex_buffer(:)
    integer :: num_visible, i, idx
    type(Box),allocatable :: visible_boxes(:)
    integer, parameter :: GLFW_KEY_KP_ADD      = 334
    integer, parameter :: GLFW_KEY_KP_SUBTRACT = 333

    ! --- ADD THESE MISSING GLFW CONSTANTS ---
    integer(c_int), parameter :: GLFW_KEY_MINUS = 45
    integer(c_int), parameter :: GLFW_KEY_EQUAL = 61
    integer(c_int), parameter :: GLFW_KEY_RIGHT = 262
    integer(c_int), parameter :: GLFW_KEY_LEFT  = 263
    integer(c_int), parameter :: GLFW_KEY_DOWN  = 264
    integer(c_int), parameter :: GLFW_KEY_UP    = 265
    integer(c_int), parameter :: GLFW_PRESS     = 1
    integer(c_int), parameter :: GLFW_KEY_A             = 65
    integer(c_int), parameter :: GLFW_KEY_B             = 66
    integer(c_int), parameter :: GLFW_KEY_R             = 82
    integer(c_int), parameter :: GLFW_KEY_S             = 83
    integer(c_int), parameter :: GLFW_KEY_T             = 84    
    integer(c_int), parameter :: GLFW_KEY_Z             = 90
    integer(c_int), parameter :: GLFW_KEY_LEFT_CONTROL  = 341
    integer(c_int), parameter :: GLFW_KEY_RIGHT_CONTROL = 345
    integer(c_int), parameter :: GLFW_KEY_ESCAPE = 256
    ! ----------------------------------------       
    ! Navigation Variables
    real(c_double) :: zoom  = 1.0_c_double
    real(c_double) :: pan_x = 0.0_c_double
    real(c_double) :: pan_y = 0.0_c_double
    real(c_double) :: current_left, current_right, current_bottom, current_top
    real(c_double) :: center_x, center_y, half_w, half_h
    real(c_double) :: mbr_min_x, mbr_max_x, mbr_min_y, mbr_max_y
    real(c_double) :: mbr_center_x, mbr_center_y, zoom_x, zoom_y
    real(c_double) :: mouse_x_screen, mouse_y_screen
    real(c_double) :: mouse_x_world,  mouse_y_world
    integer(c_int) :: live_win_x, live_win_y
    real(c_double) :: real_win_x, real_win_y
    ! Replace your old coordinate and boundary variables with these:
    type(CameraState) :: cam
    type(Point2D)     :: mouse_screen
    type(Point2D)     :: mouse_world
    type(Box)         :: current_view
    integer           :: target_depth 
    logical           :: tree_mode
    logical :: key_was_down = .false.
    real(c_double) :: last_toggle_time = 0.0
    real(c_double) :: current_time
    logical        :: needs_buffer_update = .true.
    num_visible = 0
    target_depth = 1
    tree_mode = .true.
    call cpu_time( last_toggle_time )
    MBR = mbr_of_array( input_layer%layer_boxes, input_layer%n_used )
    call glfwSetErrorCallback(handle_error)

    call glfwInit(ierr)
    if (ierr /= 0) then
       stop 'Error whilst initialising GLFW'
    end if

    window = glfwCreateWindow(int(WIN_X), int(WIN_Y), 'MAGPARSER DISPLAY')
    if (.not. associated(window)) then
       call glfwTerminate()
       stop 'Error whilst creating window'
    end if

    call glfwMakeContextCurrent(window)
    call glClearColor(0.2_c_float, 0.3_c_float, 0.3_c_float, 1.0_c_float)

    call glViewport(0, 0, int(WIN_X), int(WIN_Y))

    ! 2. Configure projection matrix (World space to Screen space)
    call glMatrixMode(GL_PROJECTION)
    call glLoadIdentity()

    ! 3. Map pixel boundaries: Left=0, Right=WIN_X, Bottom=WIN_Y, Top=0, Near=-1, Far=1
    ! Note: Switching Bottom and Top changes whether (0,0) is Top-Left or Bottom-Left.
    call glOrtho(0.0_c_double, real(WIN_X, c_double), 0.0_c_double, real(WIN_Y, c_double), -1.0_c_double, 1.0_c_double)

    ! 4. Switch back to Modelview matrix for actual object drawing
    call glMatrixMode(GL_MODELVIEW)
    call glLoadIdentity()

    ! 1. Define your MBR limits (Replace with your actual collection MBR)
    mbr_min_x = real(MBR%x1)
    mbr_max_x = real(MBR%x2)
    mbr_min_y = real(MBR%y1)
    mbr_max_y = real(MBR%y2)

    ! 2. Find the exact center of the MBR
    mbr_center_x = (mbr_min_x + mbr_max_x) / 2.0_c_double
    mbr_center_y = (mbr_min_y + mbr_max_y) / 2.0_c_double

    ! 3. Shift the pan so the camera's center matches the MBR's center.
    ! (Because our camera center is WIN / 2 + pan, we subtract WIN / 2 from the target)
    pan_x = mbr_center_x - (real(WIN_X, c_double) / 2.0_c_double)
    pan_y = mbr_center_y - (real(WIN_Y, c_double) / 2.0_c_double)

    ! 4. Calculate required scale for both axes
    zoom_x = (mbr_max_x - mbr_min_x) / real(WIN_X, c_double)
    zoom_y = (mbr_max_y - mbr_min_y) / real(WIN_Y, c_double)

    ! 5. Use the LARGEST zoom ratio so nothing gets cut off, 
    ! and multiply by 1.1 to add a nice 10% padding margin around the edges.
    zoom = max(zoom_x, zoom_y) * 1.1_c_double

    do while (glfwWindowShouldClose(window) == 0)
       ! 1. Get the ACTUAL live window size from the OS
       call glfwGetWindowSize(window, live_win_x, live_win_y)

       ! Convert to real(c_double) once to keep the math clean
       real_win_x = real(live_win_x, c_double)
       real_win_y = real(live_win_y, c_double)

       ! 2. Update Viewport dynamically (prevents stretching if window is resized)
       call glViewport(0, 0, live_win_x, live_win_y)
       ! --- 1. Update Camera State ---
       cam%win_w = real_win_x
       cam%win_h = real_win_y

       cam%left   = center_x - half_w
       cam%right  = center_x + half_w
       cam%bottom = center_y - half_h
       cam%top    = center_y + half_h
       ! 1. Handle Key Inputs for Real-Time Pan and Zoom
       if (glfwGetKey(window, GLFW_KEY_DOWN) == GLFW_PRESS)    pan_y = pan_y + (5.0_c_double * zoom)
       if (glfwGetKey(window, GLFW_KEY_UP) == GLFW_PRESS)  pan_y = pan_y - (5.0_c_double * zoom)
       if (glfwGetKey(window, GLFW_KEY_RIGHT) == GLFW_PRESS)  pan_x = pan_x - (5.0_c_double * zoom) 
       if (glfwGetKey(window, GLFW_KEY_LEFT) == GLFW_PRESS) pan_x = pan_x + (5.0_c_double * zoom)
       if ( (glfwGetKey(window, GLFW_KEY_MINUS) == GLFW_PRESS) .or. &
            (glfwGetKey(window, GLFW_KEY_Z) == GLFW_PRESS) ) then
          zoom  = zoom * 1.02_c_double ! '+' key (Zoom In)
          !write(*,*) 'Current ZOOM level = ', zoom          
       end if
       if (glfwGetKey(window, GLFW_KEY_EQUAL) == GLFW_PRESS) zoom  = zoom / 1.02_c_double ! '=' key (Zoom In)
       call cpu_time( current_time )
       if (glfwGetKey(window, GLFW_KEY_R) == GLFW_PRESS) then
          if (.not. key_was_down .and. (current_time - last_toggle_time > 0.02)) then
             ! State is safe to toggle
             TREE_MODE = .not. TREE_MODE
             last_toggle_time = current_time
             ! Force a re-evaluation of the buffers on the NEXT frame
             needs_buffer_update = .true. 
          end if
          key_was_down = .true.
       else
          key_was_down = .false.
       end if
       if (glfwGetKey(window, GLFW_KEY_S) == GLFW_PRESS) then
          if (.not. key_was_down .and. (current_time - last_toggle_time > 0.02)) then
             ! State is safe to toggle
             target_depth = target_depth - 1
             write(*,*) 'TARGET DEPTH = ', target_depth
             last_toggle_time = current_time
             ! Force a re-evaluation of the buffers on the NEXT frame
             needs_buffer_update = .true. 
          end if
          key_was_down = .true.
       else
          key_was_down = .false.
       end if
       if (glfwGetKey(window, GLFW_KEY_T) == GLFW_PRESS) then
          if (.not. key_was_down .and. (current_time - last_toggle_time > 0.02)) then
             ! State is safe to toggle
             target_depth = target_depth + 1
             last_toggle_time = current_time
             ! Force a re-evaluation of the buffers on the NEXT frame
             needs_buffer_update = .true. 
          end if
          key_was_down = .true.
       else
          key_was_down = .false.
       end if

       ! --- QUIT ON ESCAPE ---
       if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS) then
          exit
          !call glfwSetWindowShouldClose(window, 1)
       end if

       ! --- ZOOM TO FIT (CTRL + A) ---
       if (glfwGetKey(window, GLFW_KEY_A) == GLFW_PRESS .and. &
            (glfwGetKey(window, GLFW_KEY_LEFT_CONTROL) == GLFW_PRESS .or. &
            glfwGetKey(window, GLFW_KEY_RIGHT_CONTROL) == GLFW_PRESS)) then

          ! 1. Define your MBR limits (Replace with your actual collection MBR)
          mbr_min_x = real(MBR%x1)
          mbr_max_x = real(MBR%x2)
          mbr_min_y = real(MBR%y1)
          mbr_max_y = real(MBR%y2)

          ! 2. Find the exact center of the MBR
          mbr_center_x = (mbr_min_x + mbr_max_x) / 2.0_c_double
          mbr_center_y = (mbr_min_y + mbr_max_y) / 2.0_c_double

          ! 3. Shift the pan so the camera's center matches the MBR's center.
          ! (Because our camera center is WIN / 2 + pan, we subtract WIN / 2 from the target)
          pan_x = mbr_center_x - (real(WIN_X, c_double) / 2.0_c_double)
          pan_y = mbr_center_y - (real(WIN_Y, c_double) / 2.0_c_double)

          ! 4. Calculate required scale for both axes
          zoom_x = (mbr_max_x - mbr_min_x) / real(WIN_X, c_double)
          zoom_y = (mbr_max_y - mbr_min_y) / real(WIN_Y, c_double)

          ! 5. Use the LARGEST zoom ratio so nothing gets cut off, 
          ! and multiply by 1.1 to add a nice 10% padding margin around the edges.
          zoom = max(zoom_x, zoom_y) * 1.1_c_double
          write(*,*) 'Current ZOOM level = ', zoom
       end if

       call glMatrixMode(GL_PROJECTION)
       call glLoadIdentity()
       center_x = (real_win_x / 2.0_c_double) + pan_x
       center_y = (real_win_y / 2.0_c_double) + pan_y

       half_w = (real_win_x / 2.0_c_double) * zoom
       half_h = (real_win_y / 2.0_c_double) * zoom

       current_left   = center_x - half_w
       current_right  = center_x + half_w
       current_bottom = center_y - half_h
       current_top    = center_y + half_h

       call glOrtho(current_left, current_right, current_bottom, current_top, -1.0_c_double, 1.0_c_double)

       call glMatrixMode(GL_MODELVIEW)
       call glLoadIdentity()

       ! --- TRACK AND CONVERT MOUSE CURSOR ---

       ! 1. Get the raw screen pixels from GLFW
       call glfwGetCursorPos(window, mouse_x_screen, mouse_y_screen)
       call glfwGetCursorPos(window, mouse_screen%x, mouse_screen%y)       
       mouse_x_world = current_left + &
            (mouse_x_screen / real_win_x) * (current_right - current_left)

       mouse_y_world = current_top - &
            (mouse_y_screen / real_win_y) * (current_top - current_bottom)          

       ! Optional: Print to console to verify it works!
       call ScreenToWorld(mouse_screen, cam, mouse_world)
       !print *, "World X: ", mouse_world%x, " | World Y: ", mouse_y_world          
       call glOrtho(current_left, current_right, current_bottom, current_top, -1.0_c_double, 1.0_c_double)


       !> perform RTree lookup
       !> fill num_visible and allocate vertex_buffer, and do coordinate filling
       if( .not. tree_mode ) then
          current_view = GetCurrentViewportWorld(real_win_x, real_win_y, pan_x, pan_y, zoom)
          call PerformBoxFilling( input_layer, current_view, visible_boxes, vertex_buffer, num_visible)
          !call PerformCoordinateFilling( input_layer, current_view, visible_boxes, vertex_buffer, num_visible)
       else
          current_view = GetCurrentViewportWorld(real_win_x, real_win_y, pan_x, pan_y, zoom)          
          call PerformMBRFilling( input_layer, current_view, target_depth, visible_boxes, vertex_buffer, num_visible)          
       end if
       !write(*,*) 'Current Frustum = ', current_view
       call glMatrixMode(GL_MODELVIEW)
       call glLoadIdentity()

       ! 1. Clear the screen to the background color
       call glClear(GL_COLOR_BUFFER_BIT)
       call glColor3f(0.8_c_float, 0.3_c_float, 0.3_c_float) ! Coral Red
       if( num_visible > 0 ) then
          call glEnableClientState(GL_VERTEX_ARRAY)          
          call glVertexPointer(2, GL_INT, 0, c_loc(vertex_buffer(1)))
          ! 4. Draw all quads in a single hardware call
          ! Total vertices = num_visible * 4
          call glDrawArrays(GL_QUADS, 0, num_visible * 4)

          ! --- PASS 2: DRAW THE OUTLINES ---
          ! Tell OpenGL to only draw the edges of the polygons
          call glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
          call glColor3f(0.0_c_float, 0.0_c_float, 0.0_c_float) ! Black borders

          ! Optional: Make the borders slightly thicker so they stand out
          ! call glLineWidth(1.5_c_float) 

          call glDrawArrays(GL_QUADS, 0, num_visible * 4)

          ! --- CLEANUP ---
          ! Always reset the polygon mode back to FILL when you are done, 
          ! so you don't accidentally draw your other UI elements as wireframes!
          call glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)

          call glDisableClientState(GL_VERTEX_ARRAY)
       end if

       ! 3. Swap buffers and check for input events
       call glfwSwapBuffers(window)
       call glfwPollEvents()
    end do

    deallocate(vertex_buffer)
    !do
    !   call glfwPollEvents()
    !   call glfwSwapBuffers(window)
    !   if (glfwWindowShouldClose(window)) exit
    !end do

    call glfwDestroyWindow(window)
    call glfwTerminate()
  end subroutine SimpleWindow
end module DrawUsingOpenGL

program main
  use DrawUsingOpenGL
  implicit none
  integer :: narg, iostat
  integer(kind=int64) :: total_nodes
  type(Layer),target     :: input_layer
  character(len=256)     :: filename
  character(len=256)     :: buf
  integer                :: control_parameter
  narg = command_argument_count()
  if( narg /= 2 ) error stop "./MAGVIEW KLBIN CONTROL"
  call get_command_argument(1, filename, status=iostat)   ! allocates automatically
  if (iostat /= 0) then
     write (*,*) "ERROR: 1st argument must be a filename."
     stop 2
  end if
  call get_command_argument(2, buf, status=iostat)   ! allocates automatically
  read( buf, *, iostat=iostat ) control_parameter
  if (iostat /= 0) then
     write (*,*) "ERROR: 2nd argument must be an integer."
     stop 2
  end if
  write (*,*) 'Reading filename: ', trim(filename)
  call RestoreSnapToLayer( input_layer, filename )
  call BuildTree( input_layer )
  write(*,*) 'Built RTree for ', input_layer%n_used, ' boxes.'
  call SimpleWindow(input_layer, 800.0, 800.0)
end program main

