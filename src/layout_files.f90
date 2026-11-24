!> Fortran face of the C++ layout library in src/db.
!>
!> Three types, each owning one C handle (released by close() or by the finalizer), with
!> Fortran strings, arrays and logical results; no c_ptr, c_null_char or error buffers
!> reach the caller:
!>
!>   LayoutFile     any GDSII or OASIS file: format, strict flag, extent, top cells,
!>                  properties (e.g. net ids and names), boxes of one layer, write as GDSII / OASIS
!>   OasisTables    the strict-mode tables of an OASIS file, read in well under a millisecond:
!>                  unit, top cell, LAYERNAMEs, each cell's S_CELL_OFFSET and S_BOUNDING_BOX
!>   LayoutSidecar  the MAGVIEWER sidecar index (file.oas.mvi): per-layer counts and bboxes,
!>                  the density pyramid (zero-copy views into the mapped file), the units
!>                  (CBLOCKs) meeting a window, and their parallel decode into Box arrays
!>
!> Example:
!>     type(OasisTables)   :: t
!>     type(LayoutSidecar) :: s
!>     type(Box), allocatable :: b(:)
!>     if (t%open('chip.oas')) print *, t%top(), t%layer_name(67, 20)      ! "li1"
!>     if (s%open('chip.oas')) then                                     ! builds the sidecar once
!>        ok = s%decode(s%units_in(1, view), 1, b, n)                   ! layer 1 boxes near view
!>     end if
!>
!> Layer indices are 1-based (sidecar layer i = C index i-1); unit ids are the sidecar's own
!> 0-based unit numbers, passed back unchanged.
module LayoutFilesModule
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use GeometryModule, only: Box
  implicit none
  private

  public :: LayoutFile, LayoutProperty, OasisTables, OasisCell, LayoutSidecar, SidecarLayer, DensityLevel
  public :: to_c, from_c

  integer, parameter :: K_ERR = 1024

  ! ------------------------------------------------------------------ public types
  type :: LayoutProperty
     integer :: target_kind = 0                 !< 0 file, 1 cell, 2 element, 3 instance
     character(len=:), allocatable :: cell, name, value
  end type LayoutProperty

  type :: LayoutFile
     private
     type(c_ptr) :: h = c_null_ptr
     character(len=:), allocatable, public :: path, error
   contains
     procedure :: open        => lf_open
     procedure :: close       => lf_close
     procedure :: is_open     => lf_is_open
     procedure :: format      => lf_format
     procedure :: is_strict   => lf_is_strict
     procedure :: extent      => lf_extent
     procedure :: top_cells   => lf_top_cells
     procedure :: properties  => lf_properties
     procedure :: layer_boxes => lf_layer_boxes
     procedure :: write       => lf_write
     final     :: lf_final
  end type LayoutFile

  type :: OasisCell
     character(len=:), allocatable :: name
     integer(int64) :: offset = -1              !< S_CELL_OFFSET, -1 when absent
     integer        :: bbox_flags = -1          !< S_BOUNDING_BOX flags, -1 when absent
     integer(int64) :: bbox(4) = 0              !< x1, y1, x2, y2
   contains
     procedure :: has_bbox => oc_has_bbox
  end type OasisCell

  type :: OasisTables
     private
     type(c_ptr) :: h = c_null_ptr
     character(len=:), allocatable, public :: error
   contains
     procedure :: open         => ot_open
     procedure :: close        => ot_close
     procedure :: is_open      => ot_is_open
     procedure :: strict       => ot_strict
     procedure :: offsets_ok   => ot_offsets_ok
     procedure :: unit         => ot_unit
     procedure :: open_ms      => ot_open_ms
     procedure :: top          => ot_top
     procedure :: n_layernames => ot_n_layernames
     procedure :: layer_name   => ot_layer_name
     procedure :: n_cells      => ot_n_cells
     procedure :: cell         => ot_cell
     final     :: ot_final
  end type OasisTables

  type :: SidecarLayer
     integer(int64) :: layer = -1, datatype = -1, count = 0
     integer(int64) :: bbox(4) = 0
  end type SidecarLayer

  !> One level of a layer's density pyramid: pointers into the mapped sidecar (no copy),
  !> valid while the sidecar is open. Tile (i, j) covers
  !> [x0 + (i-1)*tile, x0 + i*tile) x [y0 + (j-1)*tile, y0 + j*tile).
  type :: DensityLevel
     integer :: nx = 0, ny = 0
     integer(int64) :: x0 = 0, y0 = 0, tile = 1
     integer(int32), pointer, contiguous :: count(:, :) => null()   !< boxes centred in the tile
     real(c_float),  pointer, contiguous :: coverage(:, :) => null() !< covered area / tile area
  end type DensityLevel

  type :: LayoutSidecar
     private
     type(c_ptr) :: h = c_null_ptr
     character(len=:), allocatable, public :: path, error, summary
     real(real64),   public :: unit = 0.0_real64        !< database steps per micron
     integer(int64), public :: bbox(4) = 0, tile = 1, n_units = 0, total_boxes = 0, n_dependent = 0
     integer,        public :: nx = 0, ny = 0, n_levels = 0, n_layers = 0
     real(real64),   public :: build_s = 0.0_real64      !< when the sidecar was built
     real(real64),   public :: open_s = 0.0_real64       !< this open (including a build)
     logical,        public :: built = .false.           !< built by this open
   contains
     procedure :: open       => sc_open
     procedure :: close      => sc_close
     procedure :: is_open    => sc_is_open
     procedure :: layer      => sc_layer
     procedure :: density    => sc_density
     procedure :: level_for  => sc_level_for
     procedure :: units_in   => sc_units_in
     procedure :: unit_boxes => sc_unit_boxes
     procedure :: decode     => sc_decode
     final     :: sc_final
  end type LayoutSidecar

  ! ------------------------------------------------------------------ C interfaces
  type, bind(C) :: c_extent
     integer(c_int64_t) :: xmin, ymin, xmax, ymax
     integer(c_int)     :: valid
  end type c_extent
  type, bind(C) :: c_open_opts
     integer(c_int32_t) :: flags
     integer(c_int64_t) :: large_cell_box_threshold
  end type c_open_opts

  interface
     type(c_ptr) function c_ldb_open_opts(path, opts, err, n) bind(C, name='ldb_open_opts')
       import :: c_ptr, c_char, c_size_t, c_open_opts
       character(kind=c_char), intent(in) :: path(*)
       type(c_open_opts), intent(in) :: opts
       character(kind=c_char), intent(inout) :: err(*)
       integer(c_size_t), value :: n
     end function
     subroutine c_ldb_close(f) bind(C, name='ldb_close')
       import :: c_ptr
       type(c_ptr), value :: f
     end subroutine
     integer(c_int) function c_ldb_format(f) bind(C, name='ldb_format')
       import :: c_ptr, c_int
       type(c_ptr), value :: f
     end function
     integer(c_int) function c_ldb_is_strict(f) bind(C, name='ldb_is_strict')
       import :: c_ptr, c_int
       type(c_ptr), value :: f
     end function
     integer(c_int) function c_ldb_extent_whole_hier(f, top, ext) bind(C, name='ldb_extent_whole_hier')
       import :: c_ptr, c_int, c_extent
       type(c_ptr), value :: f, top
       type(c_extent), intent(out) :: ext
     end function
     integer(c_int) function c_ldb_top_cell_count(f) bind(C, name='ldb_top_cell_count')
       import :: c_ptr, c_int
       type(c_ptr), value :: f
     end function
     type(c_ptr) function c_ldb_top_cell_name(f, i) bind(C, name='ldb_top_cell_name')
       import :: c_ptr, c_int
       type(c_ptr), value :: f
       integer(c_int), value :: i
     end function
     integer(c_int) function c_ldb_iterate_properties(f, cb, ud) bind(C, name='ldb_iterate_properties')
       import :: c_ptr, c_funptr, c_int
       type(c_ptr), value :: f, ud
       type(c_funptr), value :: cb
     end function
     integer(c_int) function c_ldb_iterate_boxes_flat(f, cb, ud) bind(C, name='ldb_iterate_boxes_flat')
       import :: c_ptr, c_funptr, c_int
       type(c_ptr), value :: f, ud
       type(c_funptr), value :: cb
     end function
     integer(c_int) function c_ldb_write_gdsii(f, path, err, n) bind(C, name='ldb_write_gdsii')
       import :: c_ptr, c_char, c_size_t, c_int
       type(c_ptr), value :: f
       character(kind=c_char), intent(in) :: path(*)
       character(kind=c_char), intent(inout) :: err(*)
       integer(c_size_t), value :: n
     end function
     integer(c_int) function c_ldb_write_oasis(f, path, err, n) bind(C, name='ldb_write_oasis')
       import :: c_ptr, c_char, c_size_t, c_int
       type(c_ptr), value :: f
       character(kind=c_char), intent(in) :: path(*)
       character(kind=c_char), intent(inout) :: err(*)
       integer(c_size_t), value :: n
     end function

     ! --- strict tables
     type(c_ptr) function c_oas_info_open(path, err, n) bind(C, name='ldb_oas_info_open')
       import :: c_ptr, c_char, c_size_t
       character(kind=c_char), intent(in) :: path(*)
       character(kind=c_char), intent(inout) :: err(*)
       integer(c_size_t), value :: n
     end function
     subroutine c_oas_info_close(t) bind(C, name='ldb_oas_info_close')
       import :: c_ptr
       type(c_ptr), value :: t
     end subroutine
     integer(c_int) function c_oas_info_strict(t) bind(C, name='ldb_oas_info_strict')
       import :: c_ptr, c_int
       type(c_ptr), value :: t
     end function
     integer(c_int) function c_oas_info_offsets_ok(t) bind(C, name='ldb_oas_info_offsets_ok')
       import :: c_ptr, c_int
       type(c_ptr), value :: t
     end function
     real(c_double) function c_oas_info_unit(t) bind(C, name='ldb_oas_info_unit')
       import :: c_ptr, c_double
       type(c_ptr), value :: t
     end function
     real(c_double) function c_oas_info_open_ms(t) bind(C, name='ldb_oas_info_open_ms')
       import :: c_ptr, c_double
       type(c_ptr), value :: t
     end function
     integer(c_int) function c_oas_info_top(t, name, n) bind(C, name='ldb_oas_info_top')
       import :: c_ptr, c_char, c_int
       type(c_ptr), value :: t
       character(kind=c_char), intent(inout) :: name(*)
       integer(c_int), value :: n
     end function
     integer(c_int) function c_oas_info_n_layernames(t) bind(C, name='ldb_oas_info_n_layernames')
       import :: c_ptr, c_int
       type(c_ptr), value :: t
     end function
     integer(c_int) function c_oas_info_layer_name(t, l, d, name, n) bind(C, name='ldb_oas_info_layer_name')
       import :: c_ptr, c_char, c_int
       type(c_ptr), value :: t
       integer(c_int), value :: l, d, n
       character(kind=c_char), intent(inout) :: name(*)
     end function
     integer(c_int) function c_oas_info_n_cells(t) bind(C, name='ldb_oas_info_n_cells')
       import :: c_ptr, c_int
       type(c_ptr), value :: t
     end function
     integer(c_int) function c_oas_info_cell(t, i, name, n, offset, flags, bbox) bind(C, name='ldb_oas_info_cell')
       import :: c_ptr, c_char, c_int, c_int64_t
       type(c_ptr), value :: t
       integer(c_int), value :: i, n
       character(kind=c_char), intent(inout) :: name(*)
       integer(c_int64_t), intent(out) :: offset, bbox(4)
       integer(c_int), intent(out) :: flags
     end function

     ! --- sidecar
     type(c_ptr) function c_mvi_open_or_build(oas, mvi, grid, built, err, n) bind(C, name='ldb_mvi_open_or_build')
       import :: c_ptr, c_char, c_int, c_size_t
       character(kind=c_char), intent(in) :: oas(*), mvi(*)
       integer(c_int), value :: grid
       integer(c_int), intent(out) :: built
       character(kind=c_char), intent(inout) :: err(*)
       integer(c_size_t), value :: n
     end function
     subroutine c_mvi_close(m) bind(C, name='ldb_mvi_close')
       import :: c_ptr
       type(c_ptr), value :: m
     end subroutine
     type(c_ptr) function c_mvi_path(m) bind(C, name='ldb_mvi_path')
       import :: c_ptr
       type(c_ptr), value :: m
     end function
     integer(c_int) function c_mvi_info(m, unit, bbox, tile, nx, ny, nlev, nl, nu, tot, ndep, bs) bind(C, name='ldb_mvi_info')
       import :: c_ptr, c_int, c_int64_t, c_double
       type(c_ptr), value :: m
       real(c_double), intent(out) :: unit, bs
       integer(c_int64_t), intent(out) :: bbox(4), tile, nu, tot, ndep
       integer(c_int), intent(out) :: nx, ny, nlev, nl
     end function
     integer(c_int) function c_mvi_layer(m, i, l, d, c, bbox) bind(C, name='ldb_mvi_layer')
       import :: c_ptr, c_int, c_int64_t
       type(c_ptr), value :: m
       integer(c_int), value :: i
       integer(c_int64_t), intent(out) :: l, d, c, bbox(4)
     end function
     integer(c_int) function c_mvi_density(m, i, level, ax, ay, counts, cov) bind(C, name='ldb_mvi_density')
       import :: c_ptr, c_int
       type(c_ptr), value :: m
       integer(c_int), value :: i, level
       integer(c_int), intent(out) :: ax, ay
       type(c_ptr), intent(out) :: counts, cov
     end function
     integer(c_int64_t) function c_mvi_units_in(m, i, x1, y1, x2, y2, ids, cap) bind(C, name='ldb_mvi_units_in')
       import :: c_ptr, c_int, c_int64_t
       type(c_ptr), value :: m, ids
       integer(c_int), value :: i
       integer(c_int64_t), value :: x1, y1, x2, y2, cap
     end function
     integer(c_int64_t) function c_mvi_unit_boxes(m, u, i, bbox) bind(C, name='ldb_mvi_unit_boxes')
       import :: c_ptr, c_int, c_int64_t
       type(c_ptr), value :: m, bbox
       integer(c_int64_t), value :: u
       integer(c_int), value :: i
     end function
     integer(c_int64_t) function c_mvi_decode(m, units, n, i, xyxy, cap, unit_off, err, en) bind(C, name='ldb_mvi_decode')
       import :: c_ptr, c_int, c_int64_t, c_char, c_size_t
       type(c_ptr), value :: m, xyxy, unit_off
       integer(c_int64_t), intent(in) :: units(*)
       integer(c_int64_t), value :: n, cap
       integer(c_int), value :: i
       character(kind=c_char), intent(inout) :: err(*)
       integer(c_size_t), value :: en
     end function
     integer(c_size_t) function c_strlen(s) bind(C, name='strlen')
       import :: c_ptr, c_size_t
       type(c_ptr), value :: s
     end function
  end interface

  !> Property collector handed to the C callback.
  type :: PropBag
     type(LayoutProperty), allocatable :: p(:)
     integer :: n = 0
  end type PropBag
  !> Box collector for one layer/datatype.
  type :: BoxBag
     integer :: layer = -1, datatype = -1
     type(Box), allocatable :: b(:)
     integer(int64) :: n = 0
  end type BoxBag

contains

  ! ================================================================== strings
  !> Fortran string -> NUL-terminated C string.
  pure function to_c(s) result(c)
    character(len=*), intent(in) :: s
    character(kind=c_char, len=len_trim(s) + 1) :: c
    c = trim(s) // c_null_char
  end function to_c

  !> NUL-terminated C buffer (character array or string) -> trimmed Fortran string.
  function from_c(buf) result(s)
    character(kind=c_char, len=*), intent(in) :: buf
    character(len=:), allocatable :: s
    integer :: k
    k = index(buf, c_null_char)
    if (k == 0) then
       s = trim(buf)
    else
       s = buf(1:k - 1)
    end if
  end function from_c

  function from_cptr(p) result(s)
    type(c_ptr), intent(in) :: p
    character(len=:), allocatable :: s
    character(kind=c_char), pointer :: a(:)
    integer :: n, k
    s = ''
    if (.not. c_associated(p)) return
    n = int(c_strlen(p))
    call c_f_pointer(p, a, [n])
    s = repeat(' ', n)
    do k = 1, n
       s(k:k) = a(k)
    end do
  end function from_cptr

  ! ================================================================== LayoutFile
  !> Open a GDSII or OASIS file. properties = keep PROPERTY records (net ids / names);
  !> strict = OASIS strict-mode validation.
  logical function lf_open(this, path, properties, strict) result(ok)
    class(LayoutFile), intent(inout) :: this
    character(len=*), intent(in) :: path
    logical, intent(in), optional :: properties, strict
    type(c_open_opts) :: o
    character(kind=c_char, len=K_ERR) :: err
    call this%close()
    o%flags = 0_c_int32_t
    o%large_cell_box_threshold = 0_c_int64_t
    if (present(properties)) then
       if (properties) o%flags = ior(o%flags, 2_c_int32_t)
    end if
    if (present(strict)) then
       if (strict) o%flags = ior(o%flags, 1_c_int32_t)
    end if
    err = c_null_char
    this%h = c_ldb_open_opts(to_c(path), o, err, int(K_ERR, c_size_t))
    this%path = trim(path)
    this%error = from_c(err)
    ok = c_associated(this%h)
  end function lf_open

  subroutine lf_close(this)
    class(LayoutFile), intent(inout) :: this
    if (c_associated(this%h)) call c_ldb_close(this%h)
    this%h = c_null_ptr
  end subroutine lf_close

  subroutine lf_final(this)
    type(LayoutFile), intent(inout) :: this
    call this%close()
  end subroutine lf_final

  logical function lf_is_open(this)
    class(LayoutFile), intent(in) :: this
    lf_is_open = c_associated(this%h)
  end function lf_is_open

  !> 'GDSII', 'OASIS' or '' when not open / unknown.
  function lf_format(this) result(s)
    class(LayoutFile), intent(in) :: this
    character(len=:), allocatable :: s
    s = ''
    if (.not. c_associated(this%h)) return
    select case (c_ldb_format(this%h))
    case (1); s = 'GDSII'
    case (2); s = 'OASIS'
    end select
  end function lf_format

  logical function lf_is_strict(this)
    class(LayoutFile), intent(in) :: this
    lf_is_strict = .false.
    if (c_associated(this%h)) lf_is_strict = c_ldb_is_strict(this%h) /= 0
  end function lf_is_strict

  !> Flattened extent x1, y1, x2, y2 of the top cell(s); valid = .false. for an empty file.
  function lf_extent(this, valid) result(e)
    class(LayoutFile), intent(in) :: this
    logical, intent(out), optional :: valid
    integer(int64) :: e(4)
    type(c_extent) :: x
    integer(c_int) :: rc
    e = 0
    if (present(valid)) valid = .false.
    if (.not. c_associated(this%h)) return
    rc = c_ldb_extent_whole_hier(this%h, c_null_ptr, x)
    if (rc /= 0 .or. x%valid == 0) return
    e = [x%xmin, x%ymin, x%xmax, x%ymax]
    if (present(valid)) valid = .true.
  end function lf_extent

  function lf_top_cells(this) result(names)
    class(LayoutFile), intent(in) :: this
    character(len=256), allocatable :: names(:)
    integer :: i, n
    n = 0
    if (c_associated(this%h)) n = max(0, int(c_ldb_top_cell_count(this%h)))
    allocate(names(n))
    do i = 1, n
       names(i) = from_cptr(c_ldb_top_cell_name(this%h, int(i - 1, c_int)))
    end do
  end function lf_top_cells

  subroutine prop_cb(ud, kind, cell, name, value) bind(C)
    type(c_ptr), value :: ud, cell, name, value
    integer(c_int), value :: kind
    type(PropBag), pointer :: bag
    type(LayoutProperty), allocatable :: grown(:)
    call c_f_pointer(ud, bag)
    if (bag%n >= size(bag%p)) then
       allocate(grown(max(16, 2 * size(bag%p))))
       grown(1:bag%n) = bag%p(1:bag%n)
       call move_alloc(grown, bag%p)
    end if
    bag%n = bag%n + 1
    bag%p(bag%n)%target_kind = int(kind)
    bag%p(bag%n)%cell  = from_cptr(cell)
    bag%p(bag%n)%name  = from_cptr(name)
    bag%p(bag%n)%value = from_cptr(value)
  end subroutine prop_cb

  !> All properties (file, cell, element, instance). Open with properties=.true. to keep them.
  function lf_properties(this) result(props)
    class(LayoutFile), intent(in) :: this
    type(LayoutProperty), allocatable :: props(:)
    type(PropBag), target :: bag
    integer(c_int) :: rc
    allocate(bag%p(16))
    if (c_associated(this%h)) rc = c_ldb_iterate_properties(this%h, c_funloc(prop_cb), c_loc(bag))
    props = bag%p(1:bag%n)
  end function lf_properties

  subroutine box_cb(ud, cell, layer, datatype, kind, x1, y1, x2, y2) bind(C)
    type(c_ptr), value :: ud, cell
    integer(c_int), value :: layer, datatype, kind
    integer(c_int64_t), value :: x1, y1, x2, y2
    type(BoxBag), pointer :: bag
    type(Box), allocatable :: grown(:)
    call c_f_pointer(ud, bag)
    if (layer /= bag%layer .or. datatype /= bag%datatype) return
    if (bag%n >= size(bag%b, kind=int64)) then
       allocate(grown(max(1024_int64, 2_int64 * size(bag%b, kind=int64))))
       grown(1:bag%n) = bag%b(1:bag%n)
       call move_alloc(grown, bag%b)
    end if
    bag%n = bag%n + 1
    bag%b(bag%n) = Box(int(x1, int32), int(y1, int32), int(x2, int32), int(y2, int32))
  end subroutine box_cb

  !> Flattened boxes of one layer/datatype (polygons and paths as their rectangle cover).
  function lf_layer_boxes(this, layer, datatype) result(boxes)
    class(LayoutFile), intent(in) :: this
    integer, intent(in) :: layer, datatype
    type(Box), allocatable :: boxes(:)
    type(BoxBag), target :: bag
    integer(c_int) :: rc
    bag%layer = layer; bag%datatype = datatype
    allocate(bag%b(1024))
    if (c_associated(this%h)) rc = c_ldb_iterate_boxes_flat(this%h, c_funloc(box_cb), c_loc(bag))
    boxes = bag%b(1:bag%n)
  end function lf_layer_boxes

  !> Write as GDSII (.gds / .gdsii) or OASIS (.oas / .oasis), chosen by the extension.
  logical function lf_write(this, path) result(ok)
    class(LayoutFile), intent(inout) :: this
    character(len=*), intent(in) :: path
    character(kind=c_char, len=K_ERR) :: err
    integer :: k
    ok = .false.
    if (.not. c_associated(this%h)) return
    err = c_null_char
    k = index(path, '.', back=.true.)
    if (k > 0 .and. (path(k:) == '.oas' .or. path(k:) == '.oasis')) then
       ok = c_ldb_write_oasis(this%h, to_c(path), err, int(K_ERR, c_size_t)) == 0
    else
       ok = c_ldb_write_gdsii(this%h, to_c(path), err, int(K_ERR, c_size_t)) == 0
    end if
    this%error = from_c(err)
  end function lf_write

  ! ================================================================== OasisTables
  logical function ot_open(this, path) result(ok)
    class(OasisTables), intent(inout) :: this
    character(len=*), intent(in) :: path
    character(kind=c_char, len=K_ERR) :: err
    call this%close()
    err = c_null_char
    this%h = c_oas_info_open(to_c(path), err, int(K_ERR, c_size_t))
    this%error = from_c(err)
    ok = c_associated(this%h)
  end function ot_open

  subroutine ot_close(this)
    class(OasisTables), intent(inout) :: this
    if (c_associated(this%h)) call c_oas_info_close(this%h)
    this%h = c_null_ptr
  end subroutine ot_close

  subroutine ot_final(this)
    type(OasisTables), intent(inout) :: this
    call this%close()
  end subroutine ot_final

  logical function ot_is_open(this)
    class(OasisTables), intent(in) :: this
    ot_is_open = c_associated(this%h)
  end function ot_is_open

  logical function ot_strict(this)
    class(OasisTables), intent(in) :: this
    ot_strict = .false.
    if (c_associated(this%h)) ot_strict = c_oas_info_strict(this%h) /= 0
  end function ot_strict

  logical function ot_offsets_ok(this)
    class(OasisTables), intent(in) :: this
    ot_offsets_ok = .false.
    if (c_associated(this%h)) ot_offsets_ok = c_oas_info_offsets_ok(this%h) /= 0
  end function ot_offsets_ok

  real(real64) function ot_unit(this)
    class(OasisTables), intent(in) :: this
    ot_unit = 0.0_real64
    if (c_associated(this%h)) ot_unit = c_oas_info_unit(this%h)
  end function ot_unit

  real(real64) function ot_open_ms(this)
    class(OasisTables), intent(in) :: this
    ot_open_ms = -1.0_real64
    if (c_associated(this%h)) ot_open_ms = c_oas_info_open_ms(this%h)
  end function ot_open_ms

  !> S_TOP_CELL, or '' when the file does not name one.
  function ot_top(this) result(s)
    class(OasisTables), intent(in) :: this
    character(len=:), allocatable :: s
    character(kind=c_char, len=512) :: buf
    s = ''
    if (.not. c_associated(this%h)) return
    buf = c_null_char
    if (c_oas_info_top(this%h, buf, 512_c_int) /= 0) s = from_c(buf)
  end function ot_top

  integer function ot_n_layernames(this)
    class(OasisTables), intent(in) :: this
    ot_n_layernames = 0
    if (c_associated(this%h)) ot_n_layernames = int(c_oas_info_n_layernames(this%h))
  end function ot_n_layernames

  !> LAYERNAME of layer/datatype, or '' when the table has none for it.
  function ot_layer_name(this, layer, datatype) result(s)
    class(OasisTables), intent(in) :: this
    integer(int64), intent(in) :: layer, datatype
    character(len=:), allocatable :: s
    character(kind=c_char, len=256) :: buf
    s = ''
    if (.not. c_associated(this%h)) return
    buf = c_null_char
    if (c_oas_info_layer_name(this%h, int(layer, c_int), int(datatype, c_int), buf, 256_c_int) /= 0) s = from_c(buf)
  end function ot_layer_name

  integer function ot_n_cells(this)
    class(OasisTables), intent(in) :: this
    ot_n_cells = 0
    if (c_associated(this%h)) ot_n_cells = int(c_oas_info_n_cells(this%h))
  end function ot_n_cells

  !> Cell i (1-based) of the CELLNAME table.
  type(OasisCell) function ot_cell(this, i) result(c)
    class(OasisTables), intent(in) :: this
    integer, intent(in) :: i
    character(kind=c_char, len=1024) :: buf
    integer(c_int64_t) :: off, bb(4)
    integer(c_int) :: fl
    c%name = ''
    if (.not. c_associated(this%h)) return
    buf = c_null_char
    if (c_oas_info_cell(this%h, int(i - 1, c_int), buf, 1024_c_int, off, fl, bb) == 0) return
    c%name = from_c(buf)
    c%offset = off; c%bbox_flags = int(fl); c%bbox = bb
  end function ot_cell

  logical function oc_has_bbox(this)
    class(OasisCell), intent(in) :: this
    oc_has_bbox = this%bbox_flags >= 0 .and. iand(this%bbox_flags, 6) == 0
  end function oc_has_bbox

  ! ================================================================== LayoutSidecar
  !> Open the sidecar of an OASIS file, building it first when it is missing or stale.
  !> mvi = '' : file.oas.mvi, or $MAGVIEWER_CACHE_DIR / $TMPDIR when that directory is read-only.
  !> grid = base density grid (tiles along the longer side, default 1024).
  logical function sc_open(this, oas, mvi, grid) result(ok)
    class(LayoutSidecar), intent(inout) :: this
    character(len=*), intent(in) :: oas
    character(len=*), intent(in), optional :: mvi
    integer, intent(in), optional :: grid
    character(kind=c_char, len=K_ERR) :: err
    integer(c_int) :: g, built
    integer(int64) :: t0, t1, rate
    character(len=:), allocatable :: mp
    call this%close()
    mp = ''
    if (present(mvi)) mp = trim(mvi)
    g = 0_c_int
    if (present(grid)) g = int(grid, c_int)
    err = c_null_char
    call system_clock(t0, rate)
    this%h = c_mvi_open_or_build(to_c(oas), to_c(mp), g, built, err, int(K_ERR, c_size_t))
    call system_clock(t1)
    this%open_s = real(t1 - t0, real64) / real(max(1_int64, rate), real64)
    this%built = built /= 0
    ok = c_associated(this%h)
    if (.not. ok) then
       this%error = from_c(err)
       this%summary = ''
       return
    end if
    this%error = ''
    this%summary = from_c(err)
    this%path = from_cptr(c_mvi_path(this%h))
    block
      real(c_double) :: u, bs
      integer(c_int64_t) :: bb(4), tile, nu, tot, ndep
      integer(c_int) :: nx, ny, nlev, nl
      if (c_mvi_info(this%h, u, bb, tile, nx, ny, nlev, nl, nu, tot, ndep, bs) /= 0) return
      this%unit = u; this%bbox = bb; this%tile = tile; this%nx = nx; this%ny = ny
      this%n_levels = nlev; this%n_layers = nl; this%n_units = nu; this%total_boxes = tot
      this%n_dependent = ndep; this%build_s = bs
    end block
  end function sc_open

  subroutine sc_close(this)
    class(LayoutSidecar), intent(inout) :: this
    if (c_associated(this%h)) call c_mvi_close(this%h)
    this%h = c_null_ptr
  end subroutine sc_close

  subroutine sc_final(this)
    type(LayoutSidecar), intent(inout) :: this
    call this%close()
  end subroutine sc_final

  logical function sc_is_open(this)
    class(LayoutSidecar), intent(in) :: this
    sc_is_open = c_associated(this%h)
  end function sc_is_open

  type(SidecarLayer) function sc_layer(this, i) result(L)
    class(LayoutSidecar), intent(in) :: this
    integer, intent(in) :: i
    integer(c_int64_t) :: ll, dd, cc, bb(4)
    if (.not. c_associated(this%h)) return
    if (c_mvi_layer(this%h, int(i - 1, c_int), ll, dd, cc, bb) /= 0) return
    L%layer = ll; L%datatype = dd; L%count = cc; L%bbox = bb
  end function sc_layer

  !> Density of layer i at pyramid level (0 = finest).
  type(DensityLevel) function sc_density(this, i, level) result(D)
    class(LayoutSidecar), intent(in) :: this
    integer, intent(in) :: i, level
    integer(c_int) :: ax, ay
    type(c_ptr) :: pc, pv
    if (.not. c_associated(this%h)) return
    if (c_mvi_density(this%h, int(i - 1, c_int), int(level, c_int), ax, ay, pc, pv) /= 0) return
    D%nx = ax; D%ny = ay
    D%x0 = this%bbox(1); D%y0 = this%bbox(2)
    D%tile = this%tile * 2_int64**level
    call c_f_pointer(pc, D%count, [D%nx, D%ny])
    call c_f_pointer(pv, D%coverage, [D%nx, D%ny])
  end function sc_density

  !> Finest pyramid level whose tiles are at least min_tile database units wide.
  integer function sc_level_for(this, min_tile) result(level)
    class(LayoutSidecar), intent(in) :: this
    real(real64), intent(in) :: min_tile
    level = 0
    do while (level < this%n_levels - 1 .and. real(this%tile, real64) * 2.0_real64**level < min_tile)
       level = level + 1
    end do
  end function sc_level_for

  !> Units holding boxes of layer i (0: any layer) that meet the window.
  function sc_units_in(this, i, view) result(ids)
    class(LayoutSidecar), intent(in) :: this
    integer, intent(in) :: i
    type(Box), intent(in) :: view
    integer(int64), allocatable, target :: ids(:)
    integer(int64) :: n
    allocate(ids(0))
    if (.not. c_associated(this%h)) return
    n = c_mvi_units_in(this%h, int(i - 1, c_int), int(view%x1, c_int64_t), int(view%y1, c_int64_t), &
         int(view%x2, c_int64_t), int(view%y2, c_int64_t), c_null_ptr, 0_c_int64_t)
    deallocate(ids)
    allocate(ids(max(1_int64, n)))
    if (n > 0) n = c_mvi_units_in(this%h, int(i - 1, c_int), int(view%x1, c_int64_t), int(view%y1, c_int64_t), &
         int(view%x2, c_int64_t), int(view%y2, c_int64_t), c_loc(ids(1)), n)
    ids = ids(1:n)
  end function sc_units_in

  !> Boxes of layer i (0: all layers) in unit u.
  integer(int64) function sc_unit_boxes(this, u, i) result(n)
    class(LayoutSidecar), intent(in) :: this
    integer(int64), intent(in) :: u
    integer, intent(in) :: i
    n = 0
    if (c_associated(this%h)) n = max(0_int64, c_mvi_unit_boxes(this%h, u, int(i - 1, c_int), c_null_ptr))
  end function sc_unit_boxes

  !> Decode the boxes of layer i (0: all layers) in the given units, in parallel, into
  !> boxes(1:n) (grown when too small). unit_first(k) (optional, size(units)+1) is the
  !> index of unit k's first box. Returns .false. with this%error on failure.
  logical function sc_decode(this, units, i, boxes, n, unit_first) result(ok)
    class(LayoutSidecar), intent(inout) :: this
    integer(int64), intent(in) :: units(:)
    integer, intent(in) :: i
    type(Box), allocatable, target, intent(inout) :: boxes(:)
    integer(int64), intent(out) :: n
    integer(int64), allocatable, target, intent(out), optional :: unit_first(:)
    integer(int64) :: need, k
    integer(int64), allocatable, target :: offs(:)
    character(kind=c_char, len=K_ERR) :: err
    ok = .false.; n = 0
    if (.not. c_associated(this%h)) return
    need = 0
    do k = 1, size(units, kind=int64)
       need = need + this%unit_boxes(units(k), i)
    end do
    if (.not. allocated(boxes)) allocate(boxes(max(1_int64, need)))
    if (size(boxes, kind=int64) < need) then
       deallocate(boxes)
       allocate(boxes(need))
    end if
    allocate(offs(size(units) + 1))
    if (size(units) == 0) then
       ok = .true.
       if (present(unit_first)) unit_first = [1_int64]
       return
    end if
    err = c_null_char
    n = c_mvi_decode(this%h, units, size(units, kind=c_int64_t), int(i - 1, c_int), c_loc(boxes(1)), &
         size(boxes, kind=c_int64_t), c_loc(offs(1)), err, int(K_ERR, c_size_t))
    this%error = from_c(err)
    ok = n >= 0
    if (.not. ok) n = 0
    if (present(unit_first)) unit_first = offs + 1
  end function sc_decode

end module LayoutFilesModule
