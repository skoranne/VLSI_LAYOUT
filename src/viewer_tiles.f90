!> MAGVIEWER geometry source backed by the sidecar index (LayoutSidecar), for flat OASIS files.
!>
!> Nothing is loaded up front. Every frame is drawn in one of two modes:
!>   density   the per-layer density pyramid at the level whose tiles are about a pixel,
!>             composited into one RGB image (shade = covered area). Costs the same for 1M
!>             or 10T shapes and shows layers whose shapes are far below a pixel.
!>   geometry  when the boxes in view (estimated from the pyramid) fit the frame budget:
!>             the units (CBLOCKs) meeting the view are decoded in parallel and kept in an LRU
!>             tile cache; each tile keeps the MBR of every run of K_RUN boxes, so culling
!>             skips runs outside the view (files written with spatial bins have tight runs).
!>
!> Tuning: MAGVIEWER_FRAME_BOXES (geometry budget, default 3M boxes per frame),
!>         MAGVIEWER_CACHE_GB (tile cache, default 4 GB).
module ViewerTileSourceModule
  use, intrinsic :: iso_c_binding, only: c_int, c_float
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
  use CommonModule, only: K_COORDINATE_KIND
  use GeometryModule, only: Box
  use LayoutFilesModule, only: LayoutSidecar, SidecarLayer, DensityLevel
  implicit none
  private
  public :: TiledLayout

  integer, parameter :: K_RUN = 1024            !< boxes per culling run inside a tile

  type :: Tile
     integer(int64) :: unit = -1
     integer        :: layer = 0
     integer(int64) :: n = 0
     type(Box), allocatable :: b(:)
     type(Box), allocatable :: run_mbr(:)
     integer(int64) :: last_used = 0
  end type Tile

  type :: SlotList
     integer, allocatable :: s(:)
     integer :: n = 0
  end type SlotList

  type :: TiledLayout
     type(LayoutSidecar) :: side
     type(Tile), allocatable :: tiles(:)
     integer :: n_tiles = 0
     type(SlotList), allocatable :: frame(:)     !< per layer: tiles of the current view
     integer(int64) :: bytes = 0, budget = 4_int64 * 1024_int64**3
     integer(int64) :: frame_boxes = 3000000_int64
     integer(int64) :: clock = 0, hits = 0, misses = 0
     real(real64)   :: decode_ms = 0.0_real64    !< last prepare()
     integer(int64) :: estimate = 0              !< boxes in view (pyramid) at the last choice
     logical        :: geometry = .false.        !< mode of the last choice
     integer        :: level = 0                 !< pyramid level of the last density frame
   contains
     procedure :: open           => tl_open
     procedure :: close          => tl_close
     procedure :: n_layers       => tl_n_layers
     procedure :: layer          => tl_layer
     procedure :: mbr            => tl_mbr
     procedure :: count_in_view  => tl_count_in_view
     procedure :: choose_mode    => tl_choose_mode
     procedure :: prepare        => tl_prepare
     procedure :: geometry_quads => tl_geometry_quads
     procedure :: density_image  => tl_density_image
  end type TiledLayout

contains

  logical function tl_open(this, oas) result(ok)
    class(TiledLayout), intent(inout) :: this
    character(len=*), intent(in) :: oas
    character(len=32) :: ev
    integer :: l, st
    real(real64) :: gb
    ok = this%side%open(oas)
    if (.not. ok) return
    allocate(this%tiles(64), this%frame(max(1, this%side%n_layers)))
    call get_environment_variable('MAGVIEWER_FRAME_BOXES', ev, l, st)
    if (st == 0 .and. l > 0) read(ev, *, iostat=st) this%frame_boxes
    call get_environment_variable('MAGVIEWER_CACHE_GB', ev, l, st)
    if (st == 0 .and. l > 0) then
       read(ev, *, iostat=st) gb
       if (st == 0) this%budget = int(gb * 1024.0_real64**3, int64)
    end if
  end function tl_open

  subroutine tl_close(this)
    class(TiledLayout), intent(inout) :: this
    if (allocated(this%tiles)) deallocate(this%tiles)
    if (allocated(this%frame)) deallocate(this%frame)
    this%n_tiles = 0; this%bytes = 0
    call this%side%close()
  end subroutine tl_close

  integer function tl_n_layers(this)
    class(TiledLayout), intent(in) :: this
    tl_n_layers = this%side%n_layers
  end function tl_n_layers

  type(SidecarLayer) function tl_layer(this, i)
    class(TiledLayout), intent(in) :: this
    integer, intent(in) :: i
    tl_layer = this%side%layer(i)
  end function tl_layer

  type(Box) function tl_mbr(this)
    class(TiledLayout), intent(in) :: this
    tl_mbr = Box(int(this%side%bbox(1), int32), int(this%side%bbox(2), int32), &
                 int(this%side%bbox(3), int32), int(this%side%bbox(4), int32))
  end function tl_mbr

  !> Tile index range of a density level covering the view (clamped; empty when outside).
  pure subroutine tile_range(D, view, i1, i2, j1, j2)
    type(DensityLevel), intent(in) :: D
    type(Box), intent(in) :: view
    integer, intent(out) :: i1, i2, j1, j2
    i1 = int(max(0_int64, (int(view%x1, int64) - D%x0) / D%tile)) + 1
    i2 = int(min(int(D%nx - 1, int64), (int(view%x2, int64) - D%x0) / D%tile)) + 1
    j1 = int(max(0_int64, (int(view%y1, int64) - D%y0) / D%tile)) + 1
    j2 = int(min(int(D%ny - 1, int64), (int(view%y2, int64) - D%y0) / D%tile)) + 1
  end subroutine tile_range

  !> Boxes of layer i centred in tiles that meet the view, at pyramid level `level`.
  integer(int64) function tl_count_in_view(this, i, view, level) result(n)
    class(TiledLayout), intent(in) :: this
    integer, intent(in) :: i, level
    type(Box), intent(in) :: view
    type(DensityLevel) :: D
    integer :: i1, i2, j1, j2
    n = 0
    D = this%side%density(i, level)
    if (D%nx == 0) return
    call tile_range(D, view, i1, i2, j1, j2)
    if (i1 > i2 .or. j1 > j2) return
    n = sum(int(D%count(i1:i2, j1:j2), int64))
  end function tl_count_in_view

  !> Geometry when the visible layers have at most frame_boxes boxes in view, else density
  !> (with hysteresis, so panning near the threshold does not flip modes). px = database
  !> units per pixel. The estimate sums the pyramid over tiles about 1/256 of the view, so
  !> it errs slightly on the large side at the view edges.
  logical function tl_choose_mode(this, visible, view, px) result(geometry)
    class(TiledLayout), intent(inout) :: this
    logical, intent(in) :: visible(:)
    type(Box), intent(in) :: view
    real(real64), intent(in) :: px
    integer :: i, lev
    integer(int64) :: est
    real(real64) :: span
    span = max(real(int(view%x2, int64) - int(view%x1, int64), real64), &
               real(int(view%y2, int64) - int(view%y1, int64), real64))
    lev = this%side%level_for(span / 256.0_real64)
    est = 0
    do i = 1, this%side%n_layers
       if (visible(i)) est = est + this%count_in_view(i, view, lev)
    end do
    this%estimate = est
    ! hysteresis: leave geometry only when the view holds twice the budget
    if (this%geometry) then
       geometry = est <= 2_int64 * this%frame_boxes
    else
       geometry = est <= this%frame_boxes
    end if
    this%geometry = geometry
  end function tl_choose_mode

  integer function find_tile(this, unit, layer) result(k)
    class(TiledLayout), intent(in) :: this
    integer(int64), intent(in) :: unit
    integer, intent(in) :: layer
    do k = 1, this%n_tiles
       if (this%tiles(k)%unit == unit .and. this%tiles(k)%layer == layer) return
    end do
    k = 0
  end function find_tile

  integer function new_slot(this) result(k)
    class(TiledLayout), intent(inout) :: this
    type(Tile), allocatable :: grown(:)
    if (this%n_tiles >= size(this%tiles)) then
       allocate(grown(2 * size(this%tiles)))
       grown(1:this%n_tiles) = this%tiles(1:this%n_tiles)
       call move_alloc(grown, this%tiles)
    end if
    this%n_tiles = this%n_tiles + 1
    k = this%n_tiles
  end function new_slot

  !> Make the tiles of every visible layer that meet the view resident: decode the missing
  !> ones (all units of a layer in one parallel call), then evict least recently used tiles
  !> of earlier frames while the cache is over budget.
  subroutine tl_prepare(this, visible, view)
    class(TiledLayout), intent(inout) :: this
    logical, intent(in) :: visible(:)
    type(Box), intent(in) :: view
    integer(int64), allocatable :: ids(:), miss(:), first(:)
    type(Box), allocatable :: buf(:)
    integer(int64) :: n, k, nm, t0, t1, rate
    integer :: i, s
    logical :: ok
    this%clock = this%clock + 1
    call system_clock(t0, rate)
    do i = 1, this%side%n_layers
       this%frame(i)%n = 0
       if (.not. visible(i)) cycle
       ids = this%side%units_in(i, view)
       if (allocated(this%frame(i)%s)) deallocate(this%frame(i)%s)
       allocate(this%frame(i)%s(size(ids)), miss(size(ids)))
       nm = 0
       do k = 1, size(ids, kind=int64)
          s = find_tile(this, ids(k), i)
          if (s > 0) then
             this%hits = this%hits + 1
             this%tiles(s)%last_used = this%clock
             this%frame(i)%n = this%frame(i)%n + 1
             this%frame(i)%s(this%frame(i)%n) = s
          else
             nm = nm + 1
             miss(nm) = ids(k)
          end if
       end do
       if (nm > 0) then
          this%misses = this%misses + nm
          ok = this%side%decode(miss(1:nm), i, buf, n, first)
          if (.not. ok) then
             write(*,'(A,A)') 'WARNING: MAGVIEWER tile decode failed: ', this%side%error
          else
             s = new_slot(this)                       ! reserve nm consecutive slots
             do k = 2, nm
                s = new_slot(this)
             end do
             s = this%n_tiles - int(nm) + 1
             !$omp parallel do default(shared) private(k) schedule(dynamic, 1)
             do k = 1, nm
                call fill_tile(this%tiles(s + int(k) - 1), miss(k), i, buf(first(k):first(k + 1) - 1), this%clock)
             end do
             !$omp end parallel do
             do k = 1, nm
                this%bytes = this%bytes + tile_bytes(this%tiles(s + int(k) - 1))
                this%frame(i)%n = this%frame(i)%n + 1
                this%frame(i)%s(this%frame(i)%n) = s + int(k) - 1
             end do
          end if
       end if
       deallocate(miss)
    end do
    call evict(this)
    call system_clock(t1)
    this%decode_ms = 1000.0_real64 * real(t1 - t0, real64) / real(max(1_int64, rate), real64)
  end subroutine tl_prepare

  subroutine fill_tile(t, unit, layer, b, stamp)
    type(Tile), intent(inout) :: t
    integer(int64), intent(in) :: unit, stamp
    integer, intent(in) :: layer
    type(Box), intent(in) :: b(:)
    integer(int64) :: r, nr, a, z, k
    t%unit = unit; t%layer = layer; t%last_used = stamp
    t%n = size(b, kind=int64)
    t%b = b
    nr = (t%n + K_RUN - 1) / K_RUN
    if (allocated(t%run_mbr)) deallocate(t%run_mbr)
    allocate(t%run_mbr(max(1_int64, nr)))
    do r = 1, nr
       a = (r - 1) * K_RUN + 1; z = min(r * K_RUN, t%n)
       t%run_mbr(r) = t%b(a)
       do k = a + 1, z
          t%run_mbr(r)%x1 = min(t%run_mbr(r)%x1, t%b(k)%x1); t%run_mbr(r)%y1 = min(t%run_mbr(r)%y1, t%b(k)%y1)
          t%run_mbr(r)%x2 = max(t%run_mbr(r)%x2, t%b(k)%x2); t%run_mbr(r)%y2 = max(t%run_mbr(r)%y2, t%b(k)%y2)
       end do
    end do
  end subroutine fill_tile

  pure integer(int64) function tile_bytes(t)
    type(Tile), intent(in) :: t
    tile_bytes = 16_int64 * (t%n + (t%n + K_RUN - 1) / K_RUN)
  end function tile_bytes

  !> Drop least recently used tiles (never those of the current frame) while over budget;
  !> the frame lists are renumbered after compaction.
  subroutine evict(this)
    class(TiledLayout), intent(inout) :: this
    integer :: k, w, i, j
    integer, allocatable :: newpos(:)
    integer(int64) :: oldest
    integer :: victim
    do while (this%bytes > this%budget)
       oldest = huge(oldest); victim = 0
       do k = 1, this%n_tiles
          if (this%tiles(k)%unit >= 0 .and. this%tiles(k)%last_used < this%clock .and. &
              this%tiles(k)%last_used < oldest) then
             oldest = this%tiles(k)%last_used; victim = k
          end if
       end do
       if (victim == 0) exit                       ! everything left is in view
       this%bytes = this%bytes - tile_bytes(this%tiles(victim))
       if (allocated(this%tiles(victim)%b)) deallocate(this%tiles(victim)%b)
       if (allocated(this%tiles(victim)%run_mbr)) deallocate(this%tiles(victim)%run_mbr)
       this%tiles(victim)%unit = -1; this%tiles(victim)%n = 0
    end do
    ! compact the slot array
    allocate(newpos(this%n_tiles))
    w = 0
    do k = 1, this%n_tiles
       if (this%tiles(k)%unit < 0) then
          newpos(k) = 0
          cycle
       end if
       w = w + 1
       newpos(k) = w
       if (w /= k) then
          this%tiles(w)%unit = this%tiles(k)%unit; this%tiles(w)%layer = this%tiles(k)%layer
          this%tiles(w)%n = this%tiles(k)%n; this%tiles(w)%last_used = this%tiles(k)%last_used
          call move_alloc(this%tiles(k)%b, this%tiles(w)%b)
          call move_alloc(this%tiles(k)%run_mbr, this%tiles(w)%run_mbr)
          this%tiles(k)%unit = -1
       end if
    end do
    this%n_tiles = w
    do i = 1, size(this%frame)
       do j = 1, this%frame(i)%n
          this%frame(i)%s(j) = newpos(this%frame(i)%s(j))
       end do
    end do
  end subroutine evict

  !> Quads (x1,y1, x2,y1, x2,y2, x1,y2) of layer i's resident boxes meeting the view. A box
  !> narrower than a pixel (2*min_span) is drawn one pixel wide about its centre, so contacts
  !> stay visible as dots instead of vanishing. Read-only on the cache, so layers can run in
  !> parallel.
  subroutine tl_geometry_quads(this, i, view, min_span, verts, n, cap)
    class(TiledLayout), intent(in) :: this
    integer, intent(in) :: i
    type(Box), intent(in) :: view
    integer(K_COORDINATE_KIND), intent(in) :: min_span
    integer(c_int), allocatable, intent(inout) :: verts(:)
    integer, intent(out) :: n
    integer, intent(in) :: cap
    integer :: j, s
    integer(int64) :: r, a, z, k, nr
    type(Box) :: b
    n = 0
    if (.not. allocated(verts)) allocate(verts(8 * 4096))
    do j = 1, this%frame(i)%n
       s = this%frame(i)%s(j)
       if (s <= 0) cycle
       associate (t => this%tiles(s))
         nr = (t%n + K_RUN - 1) / K_RUN
         do r = 1, nr
            if (t%run_mbr(r)%x2 < view%x1 .or. t%run_mbr(r)%x1 > view%x2 .or. &
                t%run_mbr(r)%y2 < view%y1 .or. t%run_mbr(r)%y1 > view%y2) cycle
            a = (r - 1) * K_RUN + 1; z = min(r * K_RUN, t%n)
            do k = a, z
               b = t%b(k)
               if (b%x2 < view%x1 .or. b%x1 > view%x2 .or. b%y2 < view%y1 .or. b%y1 > view%y2) cycle
               if (b%x2 - b%x1 < 2 * min_span) then
                  b%x1 = (b%x1 + b%x2) / 2 - min_span; b%x2 = b%x1 + 2 * min_span
               end if
               if (b%y2 - b%y1 < 2 * min_span) then
                  b%y1 = (b%y1 + b%y2) / 2 - min_span; b%y2 = b%y1 + 2 * min_span
               end if
               if (n >= cap) return
               if (8 * (n + 1) > size(verts)) call grow(verts, 8 * (n + 1))
               verts(8*n + 1) = b%x1; verts(8*n + 2) = b%y1
               verts(8*n + 3) = b%x2; verts(8*n + 4) = b%y1
               verts(8*n + 5) = b%x2; verts(8*n + 6) = b%y2
               verts(8*n + 7) = b%x1; verts(8*n + 8) = b%y2
               n = n + 1
            end do
         end do
       end associate
    end do
  end subroutine tl_geometry_quads

  !> Composite the density of the visible layers into an RGB image of w x h pixels (rows
  !> bottom to top, as glDrawPixels wants). Pixel (1,1) has its lower-left corner at world
  !> (x0, y0); px = world units per pixel. Each layer is blended over the background in list
  !> order with alpha 0.12 + 0.8*sqrt(coverage) where it has boxes. The pyramid level is the
  !> finest whose tiles are at least a pixel, so the cost is w*h*layers whatever the file size.
  subroutine tl_density_image(this, visible, colors, x0, y0, px, w, h, img)
    class(TiledLayout), intent(inout) :: this
    logical, intent(in) :: visible(:)
    real(c_float), intent(in) :: colors(:, :)          !< (3, n_layers), 0..1
    real(real64), intent(in) :: x0, y0, px
    integer, intent(in) :: w, h
    integer(int8), allocatable, intent(inout) :: img(:)
    type(DensityLevel), allocatable :: D(:)
    integer, allocatable :: col(:, :)                  !< tile column of each pixel, per layer level
    integer :: L, nl, x, y, j, ii, lev
    real(c_float) :: c(3), a, bg(3)
    nl = this%side%n_layers
    if (.not. allocated(img)) allocate(img(3 * w * h))
    if (size(img) < 3 * w * h) then
       deallocate(img); allocate(img(3 * w * h))
    end if
    lev = this%side%level_for(px)
    this%level = lev
    allocate(D(nl), col(w, nl))
    do L = 1, nl
       if (.not. visible(L)) cycle
       D(L) = this%side%density(L, lev)
       do x = 1, w
          col(x, L) = tile_of(x0 + (real(x, real64) - 0.5_real64) * px, D(L)%x0, D(L)%tile, D(L)%nx)
       end do
    end do
    bg = [0.08_c_float, 0.08_c_float, 0.10_c_float]
    !$omp parallel do default(shared) private(y, x, L, j, ii, c, a) schedule(static)
    do y = 1, h
       do x = 1, w
          c = bg
          do L = 1, nl
             if (.not. visible(L)) cycle
             if (D(L)%nx == 0) cycle
             ii = col(x, L)
             j = tile_of(y0 + (real(y, real64) - 0.5_real64) * px, D(L)%y0, D(L)%tile, D(L)%ny)
             if (ii == 0 .or. j == 0) cycle
             if (D(L)%count(ii, j) == 0 .and. D(L)%coverage(ii, j) <= 0.0_c_float) cycle
             a = 0.12_c_float + 0.80_c_float * sqrt(min(1.0_c_float, max(0.0_c_float, D(L)%coverage(ii, j))))
             c = c * (1.0_c_float - a) + colors(:, L) * a
          end do
          img(3 * ((y - 1) * w + x - 1) + 1:3 * ((y - 1) * w + x)) = to_byte(c)
       end do
    end do
    !$omp end parallel do
  contains
    !> 0..1 -> unsigned byte stored in an int8
    elemental integer(int8) function to_byte(v)
      real(c_float), intent(in) :: v
      integer :: k
      k = nint(255.0_c_float * min(1.0_c_float, max(0.0_c_float, v)))
      to_byte = int(merge(k - 256, k, k > 127), int8)
    end function to_byte
    pure integer function tile_of(v, o, t, n)
      real(real64), intent(in) :: v
      integer(int64), intent(in) :: o, t
      integer, intent(in) :: n
      real(real64) :: f
      f = (v - real(o, real64)) / real(t, real64)
      if (f < 0.0_real64 .or. f >= real(n, real64)) then
         tile_of = 0
      else
         tile_of = int(f) + 1
      end if
    end function tile_of
  end subroutine tl_density_image

  subroutine grow(v, need)
    integer(c_int), allocatable, intent(inout) :: v(:)
    integer, intent(in) :: need
    integer(c_int), allocatable :: g(:)
    allocate(g(max(need, 2 * size(v))))
    g(1:size(v)) = v
    call move_alloc(g, v)
  end subroutine grow

end module ViewerTileSourceModule
