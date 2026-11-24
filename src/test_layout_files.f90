!> test_layout_files: exercises LayoutFilesModule (OasisTables, LayoutFile, LayoutSidecar) and
!> the viewer tile source on one flat OASIS file, without a display.
!>
!>   test_layout_files.exe file.oas [windows] [frac]
!>
!> 1. strict tables: unit, top cell, layer names, cell offsets and bboxes
!> 2. LayoutFile: format, extent, top cells, properties (files up to ~200 MB only)
!> 3. sidecar: layer counts add up to the total; every unit of every layer decodes to the
!>    count the sidecar records
!> 4. tiled geometry: for random windows, the boxes TiledLayout%geometry_quads returns
!>    (min_span 0) equal a brute-force overlap count over all decoded boxes
!> Prints PASS / FAIL per check and exits 1 on any failure.
program test_layout_files
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding, only: c_int
  use GeometryModule, only: Box
  use LayoutFilesModule
  use ViewerTileSourceModule, only: TiledLayout
  implicit none

  character(len=1024) :: fname, arg
  integer :: nwin, i, k, nfail, ios, w, nq
  real(real64) :: frac
  type(OasisTables) :: tables
  type(LayoutFile) :: lf
  type(TiledLayout) :: tl
  type(OasisCell) :: cell
  type(SidecarLayer) :: L
  type(LayoutProperty), allocatable :: props(:)
  character(len=256), allocatable :: tops(:)
  type(Box), allocatable :: all_boxes(:)
  type(Box) :: view
  integer(int64), allocatable :: ids(:)
  integer(int64) :: n, total, expect, got, x0, y0, wx, wy, seed
  integer(c_int), allocatable :: verts(:)
  logical :: ok, valid
  logical, allocatable :: vis(:)
  integer(int64) :: ext(4)
  integer(int64) :: fsize

  nfail = 0
  call get_command_argument(1, fname)
  if (len_trim(fname) == 0) then
     write(*,'(A)') 'usage: test_layout_files.exe file.oas [windows] [frac]'
     stop 2
  end if
  nwin = 50; frac = 0.02_real64
  call get_command_argument(2, arg); read(arg, *, iostat=ios) nwin
  if (ios /= 0 .or. nwin <= 0) nwin = 50
  call get_command_argument(3, arg); read(arg, *, iostat=ios) frac
  if (ios /= 0 .or. frac <= 0.0_real64) frac = 0.02_real64

  ! 1. strict tables
  call check(tables%open(trim(fname)), 'tables open ' // tables%error)
  write(*,'(A,L1,A,F8.1,A,A,A,I0,A,F8.3,A)') 'TABLES strict=', tables%strict(), ' unit=', tables%unit(), &
       ' top=', tables%top(), ' cells=', tables%n_cells(), ' (', tables%open_ms(), ' ms)'
  do i = 1, min(tables%n_cells(), 3)
     cell = tables%cell(i)
     write(*,'(A,A,A,I0,A,L1,A,4I12)') '  cell ', cell%name, ' offset ', cell%offset, ' bbox ', &
          cell%has_bbox(), ' ', cell%bbox
  end do

  ! 2. LayoutFile (whole-file model: only for small files)
  inquire(file=trim(fname), size=fsize)
  if (fsize < 200000000_int64) then
     call check(lf%open(trim(fname), properties=.true.), 'LayoutFile open ' // lf%error)
     ext = lf%extent(valid)
     tops = lf%top_cells()
     props = lf%properties()
     write(*,'(A,A,A,L1,A,4I12,A,I0,A,I0)') 'FILE format=', lf%format(), ' strict=', lf%is_strict(), &
          ' extent', ext, ' tops=', size(tops), ' properties=', size(props)
     if (size(tops) > 0) write(*,'(A,A)') '  top ', trim(tops(1))
     do i = 1, min(size(props), 3)
        write(*,'(A,I0,1X,A,1X,A,A,A)') '  property ', props(i)%target_kind, props(i)%cell, props(i)%name, &
             ' = ', props(i)%value
     end do
     call check(valid, 'extent valid')
     call lf%close()
  else
     write(*,'(A)') 'FILE skipped (large file: LayoutFile loads the whole model)'
  end if

  ! 3. sidecar
  call check(tl%open(trim(fname)), 'sidecar open ' // tl%side%error)
  write(*,'(A,A,A,L1,A,F8.3,A,I0,A,I0,A,I0)') 'SIDECAR ', tl%side%path, ' built=', tl%side%built, ' open s=', &
       tl%side%open_s, ' units=', tl%side%n_units, ' layers=', tl%side%n_layers, ' boxes=', tl%side%total_boxes
  total = 0
  do i = 1, tl%n_layers()
     L = tl%layer(i)
     total = total + L%count
  end do
  call check(total == tl%side%total_boxes, 'layer counts add up')

  ! 4. tiled geometry vs brute force, layer by layer (one layer's boxes in memory at a time)
  allocate(vis(tl%n_layers()))
  seed = 12345
  do i = 1, tl%n_layers()
     L = tl%layer(i)
     if (L%count > 300000000_int64) then
        write(*,'(A,I0,A)') 'LAYER ', i, ' skipped in the oracle (over 300M boxes)'
        cycle
     end if
     view = Box(-huge(1_int32), -huge(1_int32), huge(1_int32), huge(1_int32))
     ids = tl%side%units_in(i, view)
     ok = tl%side%decode(ids, i, all_boxes, n)
     call check(ok .and. n == L%count, 'layer decode count')
     vis = .false.; vis(i) = .true.
     wx = max(1_int64, int(real(L%bbox(3) - L%bbox(1), real64) * frac, int64))
     wy = max(1_int64, int(real(L%bbox(4) - L%bbox(2), real64) * frac, int64))
     nq = 0
     do w = 1, nwin
        x0 = L%bbox(1) + mod(lcg(seed), max(1_int64, L%bbox(3) - L%bbox(1) - wx))
        y0 = L%bbox(2) + mod(lcg(seed), max(1_int64, L%bbox(4) - L%bbox(2) - wy))
        view = Box(int(x0, int32), int(y0, int32), int(x0 + wx, int32), int(y0 + wy, int32))
        expect = count(all_boxes(1:n)%x2 >= view%x1 .and. all_boxes(1:n)%x1 <= view%x2 .and. &
                       all_boxes(1:n)%y2 >= view%y1 .and. all_boxes(1:n)%y1 <= view%y2)
        call tl%prepare(vis, view)
        call tl%geometry_quads(i, view, 0_int32, verts, k, huge(1))
        got = k
        if (got /= expect) then
           nq = nq + 1
           if (nq <= 3) write(*,'(A,I0,A,I0,A,I0,A,I0)') '  window ', w, ' layer ', i, ' tiled ', got, ' oracle ', expect
        end if
     end do
     write(*,'(A,I3,A,I0,A,I0,A,I0,A,I0,A,I0)') 'LAYER ', i, ' ', L%layer, '/', L%datatype, ' boxes ', n, &
          ' windows ', nwin, ' mismatches ', nq
     call check(nq == 0, 'tiled windows match the oracle')
  end do
  write(*,'(A,I0,A,I0,A,I0)') 'CACHE tiles ', tl%n_tiles, ' hits ', tl%hits, ' misses ', tl%misses
  call tl%close()
  call tables%close()

  if (nfail == 0) then
     write(*,'(A)') 'RESULT PASS'
  else
     write(*,'(A,I0,A)') 'RESULT FAIL (', nfail, ' checks)'
     stop 1
  end if

contains

  subroutine check(cond, what)
    logical, intent(in) :: cond
    character(len=*), intent(in) :: what
    if (.not. cond) then
       nfail = nfail + 1
       write(*,'(A,A)') 'FAIL ', what
    end if
  end subroutine check

  integer(int64) function lcg(s)
    integer(int64), intent(inout) :: s
    s = iand(s * 6364136223846793005_int64 + 1442695040888963407_int64, huge(s))
    lcg = ishft(s, -17)
  end function lcg

end program test_layout_files
