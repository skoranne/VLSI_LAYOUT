! Load GDSII/OASIS into MAGPARSER Design or CompressedSpatialIndex structures via src/db C library
module LayoutDBLoaderModule
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
  use, intrinsic :: iso_c_binding
  use CommonModule
  use hash_mod
  use DesignModule
  use GeometryModule, only: Box
  use layoutdb_f_api
  use CompressedSpatialIndexModule, only: CompressedSpatialIndex
  use CompactLayerModule, only: CompactLayer, CompactMove
  implicit none
  private
  public :: LoadLayoutDBFile, LoadLayoutDBFileHier, CountLoadedLayers, LoadLayoutDBToCompressedIndex
  public :: PrintLayoutViewInfo, PlacementsBuildGrid, PlacementAt, OrientName

  !> How a GDSII/OASIS file was turned into layers (filled when the caller asks for it).
  !>   flat file                 : hierarchical = .false.
  !>   hierarchical, cell view   : the cell's own shapes + one outline box per placement on the
  !>                               INSTANCES layer (nothing flattened)
  !>   hierarchical, flattened   : flattened = .true.
  !> The per-layer summary covers every layer below the viewed cell, including layers that
  !> occur only inside placed cells (own = 0), so a viewer can list them.
  type, public :: LayoutViewInfo
     logical            :: hierarchical = .false.
     logical            :: flattened    = .false.
     character(len=256) :: cell         = ''
     integer(int64)     :: placements   = 0_int64
     integer            :: n_tops       = 0
     integer            :: n            = 0
     integer,        allocatable :: layer(:), dtype(:)
     integer(int64), allocatable :: own(:), flat(:), cells(:)
  end type LayoutViewInfo

  !> Placements of the viewed cell (cell view of a hierarchical file), arrays expanded.
  !> orient: 0..7 = R0 R90 R180 R270 MX MX+R90 MX+R180 MX+R270 (DEF N W S E FS FW FN FE),
  !> -1 for a non-orthogonal or magnified placement. tri: leg length of the orientation
  !> marker in the placed cell's coordinates. bbox: placed extent in the viewed cell.
  type, public :: LayoutPlacements
     integer(int64) :: n = 0_int64
     integer(int64), allocatable :: ox(:), oy(:), bx1(:), by1(:), bx2(:), by2(:)
     integer(int32), allocatable :: tri(:), cell(:)
     integer(int8),  allocatable :: orient(:)
     character(len=64),  allocatable :: inst(:)
     character(len=128), allocatable :: cell_names(:)
     integer :: n_cells = 0
     integer(int64) :: n_named = 0_int64
     ! uniform grid over the bboxes (CSR): bucket b holds gitems(gstart(b):gstart(b+1)-1)
     integer(int64) :: gx0 = 0, gy0 = 0, gs = 1
     integer :: gnx = 0, gny = 0
     integer(int64), allocatable :: gstart(:)
     integer(int32), allocatable :: gitems(:)
  end type LayoutPlacements
  !> Phase times of the last LoadLayoutDBToCompressedIndex (seconds), for the viewer's report.
  real(real64), public :: viewer_last_read_s = 0.0_real64, viewer_last_index_s = 0.0_real64

  integer(int32), parameter :: K_LD_CACHE_SIZE = 1024_int32
  integer(int32), parameter :: K_LD_CACHE_MASK = 1023_int32

  type :: ldb_load_ctx
     type(Design), pointer :: design => null()
     integer :: max_layers = 0
     integer :: num_unique_layers = 0
     ! Fast single-entry last-hit cache (covers >95% of callbacks in O(1))
     integer(c_int) :: last_layer_num = -2147483647_c_int
     integer(c_int) :: last_datatype  = -2147483647_c_int
     integer        :: last_layer_idx = 0
     logical        :: compact = .false.   !> compact each layer when the stream moves on
     logical        :: interleaved = .false. !> a layer came back: compact all at the end instead
     !> > 0: streaming compaction; each layer is compacted in parts of this many boxes while it
     !> streams, so it is never held whole as Boxes (viewer default 32M boxes = 512 MB buffer).
     integer(int64) :: part_boxes = 0_int64
     ! Open-addressed integer hash table: (layer_num, datatype) -> layer_idx
     integer(c_int) :: tab_layer(0:K_LD_CACHE_SIZE-1) = 0_c_int
     integer(c_int) :: tab_dtype(0:K_LD_CACHE_SIZE-1) = 0_c_int
     integer        :: tab_idx(0:K_LD_CACHE_SIZE-1)   = 0     !> -1: layer skipped (max_layers)
     ! Layers dropped because the design already holds max_layers layers
     integer        :: n_skipped = 0
     integer(int64) :: skipped_boxes = 0_int64
     integer(c_int) :: skip_layer(32) = 0_c_int
     integer(c_int) :: skip_dtype(32) = 0_c_int
  end type ldb_load_ctx

contains

  pure integer(int32) function ld_hash_slot(layer_num, datatype)
    integer(c_int), intent(in) :: layer_num, datatype
    integer(int64) :: h
    h = int(layer_num, int64) * 2654435761_int64 + int(datatype, int64) * 2246822519_int64
    ld_hash_slot = int(iand(ieor(h, shiftr(h, 16)), int(K_LD_CACHE_MASK, int64)), int32)
  end function ld_hash_slot

  pure integer function CountLoadedLayers(d) result(cnt)
    type(Design), intent(in) :: d
    integer :: i
    cnt = 0
    if (.not. allocated(d%layerNames)) return
    do i = 1, size(d%layerNames)
       if (len_trim(d%layerNames(i)) > 0) cnt = i
    end do
  end function CountLoadedLayers

  subroutine box_callback(ud, cell, layer_num, datatype, element_kind, x1, y1, x2, y2) bind(C)
    type(c_ptr), value        :: ud
    type(c_ptr), value        :: cell
    integer(c_int), value     :: layer_num, datatype, element_kind
    integer(c_int64_t), value :: x1, y1, x2, y2
    type(ldb_load_ctx), pointer :: ctx
    integer :: layer_idx
    integer(int32) :: slot, probes
    integer(K_COORDINATE_KIND) :: cx1, cy1, cx2, cy2, bx1, by1, bx2, by2

    call c_f_pointer(ud, ctx)
    if (.not. associated(ctx)) return
    if (.not. associated(ctx%design)) return
    ! Zero-area shapes contribute nothing and are rejected downstream as INVALID BOX.
    if (x1 == x2 .or. y1 == y2) return

    ! 1. Ultra-fast O(1) last-hit check (no string formatting, no hash lookup)
    if (layer_num == ctx%last_layer_num .and. datatype == ctx%last_datatype) then
       layer_idx = ctx%last_layer_idx
    else
       ! 2. Fast O(1) open-addressed integer table lookup
       layer_idx = 0
       slot = ld_hash_slot(layer_num, datatype)
       do probes = 0_int32, K_LD_CACHE_MASK
          if (ctx%tab_idx(slot) == 0) exit
          if (ctx%tab_layer(slot) == layer_num .and. ctx%tab_dtype(slot) == datatype) then
             layer_idx = ctx%tab_idx(slot)
             exit
          end if
          slot = iand(slot + 1_int32, K_LD_CACHE_MASK)
       end do

       ! 3. Cold path: first time seeing this (layer_num, datatype)
       if (layer_idx == 0) then
          if (ctx%num_unique_layers >= ctx%max_layers) then
             ! Remember the skipped layer so it is reported and later boxes hit the table.
             ctx%n_skipped = ctx%n_skipped + 1
             if (ctx%n_skipped <= size(ctx%skip_layer)) then
                ctx%skip_layer(ctx%n_skipped) = layer_num
                ctx%skip_dtype(ctx%n_skipped) = datatype
             end if
             if (ctx%tab_idx(slot) == 0) then
                ctx%tab_layer(slot) = layer_num
                ctx%tab_dtype(slot) = datatype
                ctx%tab_idx(slot)   = -1
             end if
             ctx%skipped_boxes = ctx%skipped_boxes + 1_int64
             return
          end if
          ctx%num_unique_layers = ctx%num_unique_layers + 1
          layer_idx = ctx%num_unique_layers

          if (ctx%tab_idx(slot) == 0) then
             ctx%tab_layer(slot) = layer_num
             ctx%tab_dtype(slot) = datatype
             ctx%tab_idx(slot)   = layer_idx
          end if

          block
            character(len=64) :: a1, a2, a3, a4
            logical :: ins2
            write(a1, '("L",I0,"_D",I0)') layer_num, datatype
            if (layer_num < 0_c_int) a1 = 'INSTANCES'   ! cell view: placement outlines
            write(a2, '("L",I0,"_",I0)')  layer_num, datatype
            write(a3, '(I0,":",I0)')      layer_num, datatype
            write(a4, '(I0,"_",I0)')      layer_num, datatype
            if (layer_idx <= size(ctx%design%layerNames)) then
               ctx%design%layerNames(layer_idx) = trim(a1)
            end if
            ctx%design%layers(layer_idx)%n_used  = 0_int64
            ctx%design%layers(layer_idx)%n_alloc = 0_int64
            ctx%design%layers(layer_idx)%layerState = LAYER_STATE_NONE
            call hash_put(ctx%design%ht, trim(a1), layer_idx, ins2)
            call hash_put(ctx%design%ht, trim(a2), layer_idx, ins2)
            call hash_put(ctx%design%ht, trim(a3), layer_idx, ins2)
            call hash_put(ctx%design%ht, trim(a4), layer_idx, ins2)
          end block
       end if

       if (layer_idx < 0) then
          ctx%skipped_boxes = ctx%skipped_boxes + 1_int64
          return
       end if
       if (ctx%compact .and. ctx%part_boxes > 0_int64) then
          if (ctx%last_layer_idx > 0 .and. ctx%last_layer_idx /= layer_idx) &
               call LayerFinishParts(ctx%design%layers(ctx%last_layer_idx))
       else if (ctx%compact) then
          !> Compact a layer when the stream leaves it; once a compacted layer comes back
          !> (layers interleaved, e.g. shapes written per net) stop doing that: expanding and
          !> re-compacting on every switch is quadratic. The rest is compacted at the end.
          if (.not. ctx%interleaved .and. ctx%last_layer_idx > 0 .and. ctx%last_layer_idx /= layer_idx) &
               call LayerCompact(ctx%design%layers(ctx%last_layer_idx))
          if (ctx%design%layers(layer_idx)%is_compact) then      !> interleaved file: append
               call LayerExpand(ctx%design%layers(layer_idx), build_tree=.false.)
               ctx%interleaved = .true.
          end if
       end if
       ctx%last_layer_num = layer_num
       ctx%last_datatype  = datatype
       ctx%last_layer_idx = layer_idx
    end if

    if (layer_idx <= 0 .or. layer_idx > ctx%max_layers) return

    cx1 = int(x1, kind=K_COORDINATE_KIND)
    cy1 = int(y1, kind=K_COORDINATE_KIND)
    cx2 = int(x2, kind=K_COORDINATE_KIND)
    cy2 = int(y2, kind=K_COORDINATE_KIND)
    bx1 = min(cx1, cx2)
    by1 = min(cy1, cy2)
    bx2 = max(cx1, cx2)
    by2 = max(cy1, cy2)

    call push_box_local(ctx%design, layer_idx, bx1, by1, bx2, by2)
    if (ctx%part_boxes > 0_int64) then
       if (ctx%design%layers(layer_idx)%n_used >= ctx%part_boxes) &
            call LayerAppendPart(ctx%design%layers(layer_idx))
    end if

    if (bx1 < ctx%design%DESIGN_EXTENT%x1) ctx%design%DESIGN_EXTENT%x1 = bx1
    if (by1 < ctx%design%DESIGN_EXTENT%y1) ctx%design%DESIGN_EXTENT%y1 = by1
    if (bx2 > ctx%design%DESIGN_EXTENT%x2) ctx%design%DESIGN_EXTENT%x2 = bx2
    if (by2 > ctx%design%DESIGN_EXTENT%y2) ctx%design%DESIGN_EXTENT%y2 = by2
  end subroutine box_callback

  subroutine push_box_local(d, layer_idx, x1, y1, x2, y2)
    type(Design), intent(inout)            :: d
    integer, intent(in)                    :: layer_idx
    integer(K_COORDINATE_KIND), intent(in) :: x1, y1, x2, y2
    type(Box), allocatable :: tmp(:)
    integer(int64) :: n, na, new_na

    associate(L => d%layers(layer_idx))
      n  = L%n_used
      na = L%n_alloc
      if (.not. allocated(L%layer_boxes)) then
         allocate(L%layer_boxes(1024))
         L%n_alloc = 1024_int64
         na        = 1024_int64
         L%n_used  = 0_int64
         n         = 0_int64
      end if
      if (n >= na) then
         ! 1.5x growth reduces peak memory vs 2x while keeping amortized copies low
         new_na = max(na + na / 2_int64, na + 1024_int64)
         allocate(tmp(new_na))
         tmp(1:n) = L%layer_boxes(1:n)
         call move_alloc(from=tmp, to=L%layer_boxes)
         L%n_alloc = new_na
      end if
      n = n + 1_int64
      L%n_used = n
      L%layer_boxes(n)%x1 = x1
      L%layer_boxes(n)%y1 = y1
      L%layer_boxes(n)%x2 = x2
      L%layer_boxes(n)%y2 = y2
    end associate
  end subroutine push_box_local

  subroutine load_ldb_common(load_design, MAX_LAYERS, fh_out, ctx)
    type(Design), intent(inout), target     :: load_design
    integer, intent(in)                     :: MAX_LAYERS
    type(c_ptr), intent(out)                :: fh_out
    type(ldb_load_ctx), intent(out), target :: ctx
    character(kind=c_char), allocatable :: cpath(:)
    character(kind=c_char)              :: errbuf(512)
    integer(c_size_t)                   :: esz
    integer                             :: i, n
    character(len=:), allocatable       :: fname

    fh_out = c_null_ptr
    if (.not. allocated(load_design%fileName)) then
       write(*,*) 'ERROR: ldb_open failed: load_design%fileName not allocated'
       return
    end if

    n = len_trim(load_design%fileName)
    allocate(character(len=n) :: fname)
    fname = load_design%fileName(1:n)
    allocate(cpath(n + 1))
    do i = 1, n
       cpath(i) = fname(i:i)
    end do
    cpath(n + 1) = c_null_char
    errbuf = c_null_char
    esz = 512_c_size_t

    block
      type(ldb_open_opts_c_t) :: opts
      opts%flags = LDB_OPEN_PROPERTY_F + LDB_OPEN_LOW_MEMORY_F
      opts%large_cell_box_threshold = 100000_c_int64_t
      fh_out = ldb_open_opts_c(cpath, opts, errbuf, esz)
    end block

    if (.not. c_associated(fh_out)) then
       block
         integer :: elen
         character(len=512) :: emsg
         emsg = ''
         elen = 0
         do i = 1, 512
            if (errbuf(i) == c_null_char) exit
            elen = i
            emsg(i:i) = errbuf(i)
         end do
         if (elen > 0) then
            write(*,*) 'ERROR: ldb_open failed for ', trim(fname), ': ', emsg(1:elen)
         else
            write(*,*) 'ERROR: ldb_open failed for ', trim(fname)
         end if
       end block
       return
    end if

    if (ldb_is_strict_c(fh_out) /= 0) write(*,*) 'INFO: OASIS STRICT mode detected/enforced'
    write(*,'(A,A,A,I0)') 'INFO: Loading layout DB file (GDSII/OASIS): ', trim(fname), &
         ' format=', ldb_format_c(fh_out)

    call init_design_ctx(load_design, MAX_LAYERS, ctx)
  end subroutine load_ldb_common

  subroutine init_design_ctx(load_design, MAX_LAYERS, ctx)
    type(Design), intent(inout), target     :: load_design
    integer, intent(in)                     :: MAX_LAYERS
    type(ldb_load_ctx), intent(inout), target :: ctx
    integer :: i
    if (allocated(load_design%layers))     deallocate(load_design%layers)
    if (allocated(load_design%layerNames)) deallocate(load_design%layerNames)
    allocate(load_design%layers(MAX_LAYERS))
    allocate(load_design%layerNames(MAX_LAYERS))
    load_design%layerNames = ''
    call hash_create(load_design%ht, MAX_LAYERS * 4)
    call load_design%DESIGN_EXTENT%reset_to_infinity()
    do i = 1, MAX_LAYERS
       load_design%layers(i)%n_used  = 0_int64
       load_design%layers(i)%n_alloc = 0_int64
       load_design%layers(i)%layerState = LAYER_STATE_NONE
    end do

    ctx%design            => load_design
    ctx%max_layers        = MAX_LAYERS
    ctx%num_unique_layers = 0
    ctx%last_layer_num    = -2147483647_c_int
    ctx%last_datatype     = -2147483647_c_int
    ctx%last_layer_idx    = 0
    ctx%tab_idx           = 0
    ctx%compact           = CompactLayersEnabled()
    ctx%part_boxes        = env_int64('MAGPARSER_STREAM_PART_BOXES', 0_int64)
  end subroutine init_design_ctx

  logical function stream_enabled()
    character(len=8) :: ev
    integer :: evlen, evstat
    call get_environment_variable('MAGPARSER_OASIS_STREAM', ev, evlen, evstat)
    stream_enabled = .not. (evstat == 0 .and. evlen > 0 .and. ev(1:1) == '0')
  end function stream_enabled

  !> Warn about layers dropped because the design already held max_layers layers.
  subroutine report_skipped_layers(ctx)
    type(ldb_load_ctx), intent(in) :: ctx
    integer :: k
    character(len=:), allocatable :: lst
    character(len=32) :: one
    if (ctx%n_skipped == 0) return
    lst = ''
    do k = 1, min(ctx%n_skipped, size(ctx%skip_layer))
       write(one, '(I0,":",I0)') ctx%skip_layer(k), ctx%skip_dtype(k)
       lst = lst // ' ' // trim(one)
    end do
    if (ctx%n_skipped > size(ctx%skip_layer)) lst = lst // ' ...'
    write(*,'(A,I0,A,I0,A,I0,A)') 'WARNING: ', ctx%n_skipped, ' layer(s) not loaded (max_layers=', &
         ctx%max_layers, ', ', ctx%skipped_boxes, ' boxes dropped; raise the max_layers argument):'
    write(*,'(A,A)') 'WARNING:  ', lst
  end subroutine report_skipped_layers

  subroutine trim_design_layers(d, n_layers)
    type(Design), intent(inout) :: d
    integer, intent(in)         :: n_layers
    integer :: i
    integer(int64) :: n
    type(Box), allocatable :: tmp(:)

    if (.not. allocated(d%layers)) return
    do i = 1, min(n_layers, size(d%layers))
       associate(L => d%layers(i))
         n = L%n_used
         if (allocated(L%layer_boxes)) then
            if (n <= 0_int64) then
               deallocate(L%layer_boxes)
               L%n_alloc = 0_int64
            else if (L%n_alloc /= n) then
               allocate(tmp(n))
               tmp(1:n) = L%layer_boxes(1:n)
               call move_alloc(from=tmp, to=L%layer_boxes)
               L%n_alloc = n
            end if
         end if
       end associate
    end do
  end subroutine trim_design_layers

  !> DEF / OASIS name of an orientation code.
  function OrientName(o) result(nm)
    integer, intent(in) :: o
    character(len=16) :: nm
    character(len=16), parameter :: names(0:7) = [character(len=16) :: 'R0 (N)', 'R90 (W)', 'R180 (S)', &
         'R270 (E)', 'MX (FS)', 'MX R90 (FW)', 'MX R180 (FN)', 'MX R270 (FE)']
    if (o >= 0 .and. o <= 7) then
       nm = names(o)
    else
       nm = 'general'
    end if
  end function OrientName

  subroutine placement_callback(ud, ref_cell, inst_name, ox, oy, angle, mirror, mag, lx1, ly1, lx2, ly2, &
                                wx1, wy1, wx2, wy2) bind(C)
    type(c_ptr), value        :: ud, ref_cell, inst_name
    integer(c_int64_t), value :: ox, oy, lx1, ly1, lx2, ly2, wx1, wy1, wx2, wy2
    real(c_double), value     :: angle, mag
    integer(c_int), value     :: mirror
    type(LayoutPlacements), pointer :: pl
    character(len=128) :: cn
    integer(int64) :: n, cap
    integer :: q, k
    real(c_double) :: qa
    call c_f_pointer(ud, pl)
    n = pl%n + 1_int64
    if (.not. allocated(pl%ox)) then
       cap = 1024_int64
       allocate(pl%ox(cap), pl%oy(cap), pl%bx1(cap), pl%by1(cap), pl%bx2(cap), pl%by2(cap), &
                pl%tri(cap), pl%cell(cap), pl%orient(cap), pl%inst(cap), pl%cell_names(64))
    end if
    if (n > size(pl%ox, kind=int64)) call grow_placements(pl, 2_int64 * size(pl%ox, kind=int64))
    pl%ox(n) = ox; pl%oy(n) = oy
    pl%bx1(n) = wx1; pl%by1(n) = wy1; pl%bx2(n) = wx2; pl%by2(n) = wy2
    qa = angle / 90.0_c_double
    if (abs(qa - anint(qa)) < 1.0e-9_c_double .and. abs(mag - 1.0_c_double) < 1.0e-12_c_double) then
       q = modulo(nint(qa), 4)
       pl%orient(n) = int(q + merge(4, 0, mirror /= 0_c_int), int8)
    else
       pl%orient(n) = -1_int8
    end if
    pl%tri(n) = int(max(1_int64, (min(lx2 - lx1, ly2 - ly1) * 2_int64) / 5_int64), int32)
    call cstr(ref_cell, cn)
    k = 0
    do q = pl%n_cells, 1, -1
       if (pl%cell_names(q) == cn) then
          k = q; exit
       end if
    end do
    if (k == 0) then
       if (pl%n_cells >= size(pl%cell_names)) pl%cell_names = [pl%cell_names, pl%cell_names]
       pl%n_cells = pl%n_cells + 1
       pl%cell_names(pl%n_cells) = cn
       k = pl%n_cells
    end if
    pl%cell(n) = int(k, int32)
    call cstr(inst_name, pl%inst(n))
    if (len_trim(pl%inst(n)) > 0) pl%n_named = pl%n_named + 1_int64
    pl%n = n
  contains
    subroutine cstr(p, out)
      type(c_ptr), intent(in) :: p
      character(len=*), intent(out) :: out
      character(kind=c_char), pointer :: cs(:)
      integer :: i
      out = ''
      if (.not. c_associated(p)) return
      call c_f_pointer(p, cs, [len(out) + 1])
      do i = 1, len(out)
         if (cs(i) == c_null_char) exit
         out(i:i) = cs(i)
      end do
    end subroutine cstr
  end subroutine placement_callback

  subroutine grow_placements(pl, cap)
    type(LayoutPlacements), intent(inout) :: pl
    integer(int64), intent(in) :: cap
    integer(int64), allocatable :: t8(:)
    integer(int32), allocatable :: t4(:)
    integer(int8),  allocatable :: t1(:)
    character(len=64), allocatable :: ts(:)
    integer(int64) :: m
    m = pl%n
    allocate(t8(cap)); t8(1:m) = pl%ox(1:m);  call move_alloc(t8, pl%ox)
    allocate(t8(cap)); t8(1:m) = pl%oy(1:m);  call move_alloc(t8, pl%oy)
    allocate(t8(cap)); t8(1:m) = pl%bx1(1:m); call move_alloc(t8, pl%bx1)
    allocate(t8(cap)); t8(1:m) = pl%by1(1:m); call move_alloc(t8, pl%by1)
    allocate(t8(cap)); t8(1:m) = pl%bx2(1:m); call move_alloc(t8, pl%bx2)
    allocate(t8(cap)); t8(1:m) = pl%by2(1:m); call move_alloc(t8, pl%by2)
    allocate(t4(cap)); t4(1:m) = pl%tri(1:m); call move_alloc(t4, pl%tri)
    allocate(t4(cap)); t4(1:m) = pl%cell(1:m); call move_alloc(t4, pl%cell)
    allocate(t1(cap)); t1(1:m) = pl%orient(1:m); call move_alloc(t1, pl%orient)
    allocate(ts(cap)); ts(1:m) = pl%inst(1:m); call move_alloc(ts, pl%inst)
  end subroutine grow_placements

  !> Grid over the placement bboxes, about 4 placements per bucket.
  subroutine PlacementsBuildGrid(pl)
    type(LayoutPlacements), intent(inout) :: pl
    integer(int64) :: i, xa, ya, xb, yb, gx, gy, b, nb
    integer(int64), allocatable :: fill(:)
    real(real64) :: area
    if (pl%n <= 0_int64) return
    xa = minval(pl%bx1(1:pl%n)); ya = minval(pl%by1(1:pl%n))
    xb = maxval(pl%bx2(1:pl%n)); yb = maxval(pl%by2(1:pl%n))
    area = real(xb - xa + 1, real64) * real(yb - ya + 1, real64)
    pl%gs = max(256_int64, int(sqrt(area / max(1.0_real64, real(pl%n, real64) / 4.0_real64)), int64))
    do
       pl%gnx = int((xb - xa) / pl%gs) + 1; pl%gny = int((yb - ya) / pl%gs) + 1
       if (int(pl%gnx, int64) * int(pl%gny, int64) <= 4000000_int64) exit
       pl%gs = pl%gs * 2_int64
    end do
    pl%gx0 = xa; pl%gy0 = ya
    nb = int(pl%gnx, int64) * int(pl%gny, int64)
    if (allocated(pl%gstart)) deallocate(pl%gstart)
    if (allocated(pl%gitems)) deallocate(pl%gitems)
    allocate(pl%gstart(nb + 1)); pl%gstart = 0_int64
    do i = 1, pl%n
       do gy = (pl%by1(i) - ya) / pl%gs, (pl%by2(i) - ya) / pl%gs
          do gx = (pl%bx1(i) - xa) / pl%gs, (pl%bx2(i) - xa) / pl%gs
             b = gy * pl%gnx + gx + 1
             pl%gstart(b + 1) = pl%gstart(b + 1) + 1_int64
          end do
       end do
    end do
    pl%gstart(1) = 1_int64
    do b = 2, nb + 1
       pl%gstart(b) = pl%gstart(b) + pl%gstart(b - 1)
    end do
    allocate(pl%gitems(max(1_int64, pl%gstart(nb + 1) - 1_int64)), fill(nb))
    fill = pl%gstart(1:nb)
    do i = 1, pl%n
       do gy = (pl%by1(i) - ya) / pl%gs, (pl%by2(i) - ya) / pl%gs
          do gx = (pl%bx1(i) - xa) / pl%gs, (pl%bx2(i) - xa) / pl%gs
             b = gy * pl%gnx + gx + 1
             pl%gitems(fill(b)) = int(i, int32)
             fill(b) = fill(b) + 1_int64
          end do
       end do
    end do
  end subroutine PlacementsBuildGrid

  !> Smallest placement whose bbox holds (x, y); 0 when none.
  integer(int64) function PlacementAt(pl, x, y) result(best)
    type(LayoutPlacements), intent(in) :: pl
    integer(int64), intent(in) :: x, y
    integer(int64) :: gx, gy, b, k, i
    real(real64) :: a, abest
    best = 0_int64
    if (pl%n <= 0_int64 .or. .not. allocated(pl%gstart)) return
    if (x < pl%gx0 .or. y < pl%gy0) return
    gx = (x - pl%gx0) / pl%gs; gy = (y - pl%gy0) / pl%gs
    if (gx >= pl%gnx .or. gy >= pl%gny) return
    b = gy * pl%gnx + gx + 1
    abest = huge(1.0_real64)
    do k = pl%gstart(b), pl%gstart(b + 1) - 1
       i = pl%gitems(k)
       if (x < pl%bx1(i) .or. x > pl%bx2(i) .or. y < pl%by1(i) .or. y > pl%by2(i)) cycle
       a = real(pl%bx2(i) - pl%bx1(i), real64) * real(pl%by2(i) - pl%by1(i), real64)
       if (a < abest) then
          abest = a; best = i
       end if
    end do
  end function PlacementAt

  subroutine summary_callback(ud, layer, datatype, own, flat, cells) bind(C)
    type(c_ptr), value        :: ud
    integer(c_int), value     :: layer, datatype
    integer(c_int64_t), value :: own, flat, cells
    type(LayoutViewInfo), pointer :: info
    integer :: n
    call c_f_pointer(ud, info)
    if (.not. allocated(info%layer)) then
       allocate(info%layer(16), info%dtype(16), info%own(16), info%flat(16), info%cells(16))
    end if
    n = info%n + 1
    if (n > size(info%layer)) then
       info%layer = [info%layer, info%layer]; info%dtype = [info%dtype, info%dtype]
       info%own = [info%own, info%own]; info%flat = [info%flat, info%flat]; info%cells = [info%cells, info%cells]
    end if
    info%layer(n) = int(layer); info%dtype(n) = int(datatype)
    info%own(n) = int(own, int64); info%flat(n) = int(flat, int64); info%cells(n) = int(cells, int64)
    info%n = n
  end subroutine summary_callback

  !> Cell name, layer summary and placement count of a hierarchical file (fh open).
  subroutine fill_view_info(fh, cell, info)
    type(c_ptr), intent(in)                    :: fh
    character(len=*), intent(in)               :: cell
    type(LayoutViewInfo), intent(inout), target :: info
    character(kind=c_char), allocatable :: cc(:)
    type(c_ptr) :: p
    character(kind=c_char), pointer :: cs(:)
    integer :: k, n
    integer(c_int) :: rc
    info%hierarchical = .true.
    info%n_tops = int(ldb_top_cell_count_c(fh))
    if (len_trim(cell) > 0) then
       info%cell = trim(cell)
    else
       info%cell = ''
       p = ldb_top_cell_name_c(fh, 0_c_int)
       if (c_associated(p)) then
          call c_f_pointer(p, cs, [256])
          do k = 1, 256
             if (cs(k) == c_null_char) exit
             info%cell(k:k) = cs(k)
          end do
       end if
    end if
    n = len_trim(info%cell)
    allocate(cc(n + 1))
    do k = 1, n
       cc(k) = info%cell(k:k)
    end do
    cc(n + 1) = c_null_char
    info%n = 0
    rc = ldb_layer_summary_hier_c(fh, cc, c_funloc(summary_callback), c_loc(info))
    info%placements = int(ldb_cell_placements_c(fh, cc), int64)
  end subroutine fill_view_info

  !> Prints how the file was opened and, for hierarchical files, the per-layer summary.
  subroutine PrintLayoutViewInfo(info)
    type(LayoutViewInfo), intent(in) :: info
    integer :: i
    if (.not. info%hierarchical) return
    if (info%flattened) then
       write(*,'(A,A,A)') 'INFO: hierarchical layout FLATTENED from cell ', trim(info%cell), &
            ' (every placement expanded)'
    else
       write(*,'(A,A,A,I0,A)') 'INFO: hierarchical layout, CELL VIEW of ', trim(info%cell), ': its own shapes and ', &
            info%placements, ' placements drawn as outlines (layer INSTANCES); nothing flattened.'
       write(*,'(A)') 'INFO:   --flatten expands the hierarchy, --cell NAME views another cell.'
    end if
    write(*,'(A)') 'INFO: layer summary below the cell, in boxes (own = in the cell itself, flat = after flattening):'
    write(*,'(A)') '    layer/dt          own            flat   cells'
    do i = 1, info%n
       if (info%own(i) == 0_int64 .and. .not. info%flattened) then
          write(*,'(2X,I5,A,I0,T16,I12,2X,I14,2X,I6,A)') info%layer(i), '/', info%dtype(i), info%own(i), &
               info%flat(i), info%cells(i), '  (only in placed cells)'
       else
          write(*,'(2X,I5,A,I0,T16,I12,2X,I14,2X,I6)') info%layer(i), '/', info%dtype(i), info%own(i), &
               info%flat(i), info%cells(i)
       end if
    end do
  end subroutine PrintLayoutViewInfo

  subroutine LoadLayoutDBFile(load_design, MAX_LAYERS, compact, part_boxes, view_cell, info, placements)
    type(Design), intent(inout), target :: load_design
    integer, intent(in)                 :: MAX_LAYERS
    logical, intent(in), optional       :: compact   !> override MAGPARSER_COMPACT_LAYERS
    integer(int64), intent(in), optional :: part_boxes !> streaming compaction part size (0 = off)
    !> With `info` present a hierarchical file is opened as a cell view (`view_cell`, default the
    !> top cell) instead of every cell's own shapes piled up at the origin.
    character(len=*), intent(in), optional :: view_cell
    type(LayoutViewInfo), intent(inout), optional, target :: info
    type(LayoutPlacements), intent(inout), optional, target :: placements   !> cell view only
    type(c_ptr)               :: fh
    type(ldb_load_ctx), target :: ctx
    type(c_funptr)            :: cb
    integer(c_int)            :: rc
    integer                   :: i
    logical                   :: view_hier

    cb = c_funloc(box_callback)
    ! Flat OASIS: stream boxes straight from the parser (no 48 B/box C++ copy of the geometry).
    if (stream_enabled() .and. allocated(load_design%fileName)) then
       block
         character(kind=c_char), allocatable :: cpath(:)
         character(kind=c_char) :: errbuf(512)
         integer :: k, n
         n = len_trim(load_design%fileName)
         allocate(cpath(n + 1))
         do k = 1, n
            cpath(k) = load_design%fileName(k:k)
         end do
         cpath(n + 1) = c_null_char
         errbuf = c_null_char
         call init_design_ctx(load_design, MAX_LAYERS, ctx)
         if (present(compact)) ctx%compact = compact
         if (present(part_boxes)) ctx%part_boxes = part_boxes
         rc = ldb_stream_boxes_c(cpath, cb, c_loc(ctx), errbuf, 512_c_size_t)
         if (rc == 0_c_int) then
            write(*,'(A,A)') 'INFO: Streamed OASIS: ', trim(load_design%fileName)
            fh = c_null_ptr
         else
            write(*,'(A,I0,A)') 'INFO: OASIS stream not possible (rc=', rc, '), using full reader'
            do k = 1, size(load_design%layers)
               call ClearLayer(load_design%layers(k))
            end do
         end if
       end block
    else
       rc = -1_c_int
    end if
    if (rc /= 0_c_int) then
       call load_ldb_common(load_design, MAX_LAYERS, fh, ctx)
       if (.not. c_associated(fh)) return
       if (present(compact)) ctx%compact = compact
       if (present(part_boxes)) ctx%part_boxes = part_boxes
       if (present(info)) info%hierarchical = .false.
       if (present(info)) then
          if (ldb_is_hierarchical_c(fh) /= 0_c_int) then
             if (present(view_cell)) then
                call fill_view_info(fh, view_cell, info)
             else
                call fill_view_info(fh, '', info)
             end if
          end if
       end if
       if (present(info)) then
          view_hier = info%hierarchical
       else
          view_hier = .false.
       end if
       if (view_hier) then
          block
            character(kind=c_char), allocatable :: cc(:)
            integer :: k, n
            n = len_trim(info%cell)
            allocate(cc(n + 1))
            do k = 1, n
               cc(k) = info%cell(k:k)
            end do
            cc(n + 1) = c_null_char
            rc = ldb_iterate_cell_view_c(fh, cc, cb, c_loc(ctx))
            if (rc == -2_c_int) write(*,'(A,A)') 'ERROR: no cell named ', trim(info%cell)
            if (present(placements)) then
               placements%n = 0_int64
               rc = ldb_iterate_placements_c(fh, cc, c_funloc(placement_callback), c_loc(placements))
               call PlacementsBuildGrid(placements)
               write(*,'(A,I0,A,I0,A,I0,A)') 'INFO: placements ', placements%n, ' (', placements%n_named, &
                    ' named) of ', placements%n_cells, ' cells'
            end if
          end block
       else
          ! Flatten from the top cell(s): every placement (SREF/AREF, OASIS PLACEMENT with
          ! repetitions, rotation, mirroring) is expanded. ldb_iterate_boxes would drop all
          ! instantiated geometry; for a flat file both give the same boxes.
          block
            character(kind=c_char) :: topc(1)
            topc(1) = c_null_char
            rc = ldb_iterate_boxes_hier_c(fh, topc, -1_c_int, cb, c_loc(ctx))
          end block
       end if
       call ldb_close_c(fh)
    end if
    if (ctx%compact .and. ctx%part_boxes > 0_int64) then
       do i = 1, ctx%num_unique_layers
          call LayerFinishParts(load_design%layers(i))
       end do
    end if
    call trim_design_layers(load_design, ctx%num_unique_layers)
    if (ctx%compact .and. ctx%interleaved) then
       do i = 1, ctx%num_unique_layers
          call LayerCompact(load_design%layers(i))
       end do
    else if (ctx%compact .and. ctx%last_layer_idx > 0) then
       call LayerCompact(load_design%layers(ctx%last_layer_idx))
    end if
    write(*,'(A,I0,A,A)') 'INFO: Loaded ', ctx%num_unique_layers, ' unique layers from ', trim(load_design%fileName)
    call report_skipped_layers(ctx)
    do i = 1, ctx%num_unique_layers
       write(*,'(A,I0,A,A,A,I0,A)') '  Layer ', i, ': ', trim(load_design%layerNames(i)), &
            ' (', load_design%layers(i)%n_used + load_design%layers(i)%compact%n, ' boxes)'
    end do
  end subroutine LoadLayoutDBFile

  subroutine LoadLayoutDBFileHier(load_design, MAX_LAYERS, compact, view_cell, info)
    type(Design), intent(inout), target :: load_design
    integer, intent(in)                 :: MAX_LAYERS
    logical, intent(in), optional       :: compact
    character(len=*), intent(in), optional :: view_cell   !> flatten from this cell (default: all tops)
    type(LayoutViewInfo), intent(inout), optional, target :: info
    type(c_ptr)               :: fh
    type(ldb_load_ctx), target :: ctx
    type(c_funptr)            :: cb
    integer(c_int)            :: rc
    character(kind=c_char)    :: topc(1)
    integer                   :: i

    call load_ldb_common(load_design, MAX_LAYERS, fh, ctx)
    if (.not. c_associated(fh)) return
    if (present(compact)) ctx%compact = compact
    cb = c_funloc(box_callback)
    topc(1) = c_null_char
    if (present(info)) then
       info%hierarchical = ldb_is_hierarchical_c(fh) /= 0_c_int
       if (info%hierarchical) then
          if (present(view_cell)) then
             call fill_view_info(fh, view_cell, info)
          else
             call fill_view_info(fh, '', info)
          end if
          info%flattened = .true.
       end if
    end if
    if (present(view_cell)) then
       if (len_trim(view_cell) > 0) then
          block
            character(kind=c_char), allocatable :: cc(:)
            integer :: k, n
            n = len_trim(view_cell)
            allocate(cc(n + 1))
            do k = 1, n
               cc(k) = view_cell(k:k)
            end do
            cc(n + 1) = c_null_char
            rc = ldb_iterate_boxes_hier_c(fh, cc, -1_c_int, cb, c_loc(ctx))
          end block
       else
          rc = ldb_iterate_boxes_hier_c(fh, topc, -1_c_int, cb, c_loc(ctx))
       end if
    else
       rc = ldb_iterate_boxes_hier_c(fh, topc, -1_c_int, cb, c_loc(ctx))
    end if
    call ldb_close_c(fh)
    call trim_design_layers(load_design, ctx%num_unique_layers)
    if (ctx%compact .and. ctx%interleaved) then
       do i = 1, ctx%num_unique_layers
          call LayerCompact(load_design%layers(i))
       end do
    else if (ctx%compact .and. ctx%last_layer_idx > 0) then
       call LayerCompact(load_design%layers(ctx%last_layer_idx))
    end if
    write(*,'(A,I0,A,A)') 'INFO: Hierarchically loaded ', ctx%num_unique_layers, &
         ' unique layers from ', trim(load_design%fileName)
    call report_skipped_layers(ctx)
    do i = 1, ctx%num_unique_layers
       write(*,'(A,I0,A,A,A,I0,A)') '  Layer ', i, ': ', trim(load_design%layerNames(i)), &
            ' (', load_design%layers(i)%n_used + load_design%layers(i)%compact%n, ' boxes)'
    end do
  end subroutine LoadLayoutDBFileHier

  ! ------------------------------------------------------------------
  ! Low-memory ingestion for MAGVIEWER: loads GDSII/OASIS, immediately
  ! builds each layer's CompressedSpatialIndex, and frees raw boxes
  ! per layer to minimize peak RSS.
  ! ------------------------------------------------------------------
  subroutine LoadLayoutDBToCompressedIndex(filename, max_layers, use_hier, &
                                           indices, layer_names, num_layers, global_mbr, view_cell, info, &
                                           placements)
    character(len=*), intent(in)                            :: filename
    integer, intent(in)                                     :: max_layers
    logical, intent(in)                                     :: use_hier
    type(CompressedSpatialIndex), allocatable, intent(out)  :: indices(:)
    character(len=64), allocatable, intent(out)             :: layer_names(:)
    integer, intent(out)                                    :: num_layers
    type(Box), intent(out)                                  :: global_mbr
    character(len=*), intent(in), optional                  :: view_cell
    type(LayoutViewInfo), intent(inout), optional           :: info
    type(LayoutPlacements), intent(inout), optional         :: placements

    type(Design), target :: tmp_design
    integer              :: i
    logical              :: use_compact
    integer(int64)       :: t0, t1, t2, rate

    num_layers = 0
    call global_mbr%reset_to_infinity()

    allocate(character(len=len_trim(filename)) :: tmp_design%fileName)
    tmp_design%fileName = trim(filename)
    tmp_design%design_direction = DESIGN_DIRECTION_INPUT

    ! Flat OASIS streams box by box; with the compact store each layer is encoded as the
    ! stream moves past it, so the peak is one layer as Boxes plus the compact layers.
    use_compact = viewer_compact_enabled()
    call system_clock(t0, rate)
    if (use_hier) then
       call LoadLayoutDBFileHier(tmp_design, max_layers, compact=use_compact, view_cell=view_cell, info=info)
    else
       call LoadLayoutDBFile(tmp_design, max_layers, compact=use_compact, &
            part_boxes=merge(env_int64('MAGVIEWER_PART_BOXES', 33554432_int64), 0_int64, use_compact), &
            view_cell=view_cell, info=info, placements=placements)
    end if
    num_layers = CountLoadedLayers(tmp_design)
    if (num_layers <= 0) then
       call DeleteDesign(tmp_design)
       return
    end if

    call system_clock(t1)
    allocate(indices(num_layers))
    allocate(layer_names(num_layers))
    do i = 1, num_layers
       layer_names(i) = trim(tmp_design%layerNames(i))
       if (tmp_design%layers(i)%is_compact .and. allocated(tmp_design%layers(i)%cextra)) then
          ! Streamed in parts: the index takes ownership of all parts.
          block
            type(CompactLayer), allocatable :: parts(:)
            integer :: ip, np
            np = 1 + size(tmp_design%layers(i)%cextra)
            allocate(parts(np))
            call CompactMove(tmp_design%layers(i)%compact, parts(1))
            do ip = 2, np
               call CompactMove(tmp_design%layers(i)%cextra(ip - 1), parts(ip))
            end do
            deallocate(tmp_design%layers(i)%cextra)
            call indices(i)%build_from_compact_parts(parts)
            tmp_design%layers(i)%is_compact = .false.
          end block
       else if (tmp_design%layers(i)%is_compact) then
          ! The index takes ownership of the packed keys: geometry never re-expands.
          call indices(i)%build_from_compact(tmp_design%layers(i)%compact)
          tmp_design%layers(i)%is_compact = .false.
       else if (tmp_design%layers(i)%n_used > 0_int64 .and. &
                allocated(tmp_design%layers(i)%layer_boxes)) then
          call indices(i)%build_auto(tmp_design%layers(i)%layer_boxes, tmp_design%layers(i)%n_used)
       end if
       if (.not. indices(i)%is_empty()) global_mbr = global_mbr + indices(i)%layer_mbr
       call ClearLayer(tmp_design%layers(i))
    end do
    call system_clock(t2)
    viewer_last_read_s  = real(t1 - t0, real64) / real(rate, real64)
    viewer_last_index_s = real(t2 - t1, real64) / real(rate, real64)
    write(*,'(A,F8.2,A,F8.2,A,L1,A)') 'INFO: MAGVIEWER read ', real(t1 - t0) / real(rate), &
         ' s, index build ', real(t2 - t1) / real(rate), ' s (compact=', use_compact, ')'

    call DeleteDesign(tmp_design)
  end subroutine LoadLayoutDBToCompressedIndex

  integer(int64) function env_int64(name, dflt)
    character(len=*), intent(in) :: name
    integer(int64), intent(in) :: dflt
    character(len=32) :: ev
    integer :: evlen, evstat, ios
    env_int64 = dflt
    call get_environment_variable(name, ev, evlen, evstat)
    if (evstat == 0 .and. evlen > 0) then
       read(ev, *, iostat=ios) env_int64
       if (ios /= 0) env_int64 = dflt
    end if
  end function env_int64

  logical function viewer_compact_enabled()
    character(len=8) :: ev
    integer :: evlen, evstat
    call get_environment_variable('MAGVIEWER_COMPACT', ev, evlen, evstat)
    viewer_compact_enabled = .not. (evstat == 0 .and. evlen > 0 .and. ev(1:1) == '0')
  end function viewer_compact_enabled

end module LayoutDBLoaderModule
