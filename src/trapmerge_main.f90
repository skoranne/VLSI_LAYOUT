! File   : trapmerge_main.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: TRAPMERGE.exe - single-layer OR (merge) of any-angle layers of an OASIS/GDSII file
!          with TrapMergeModule, timing breakdown, and OASIS output of the merged trapezoids.
!
!   TRAPMERGE.exe IN OUT.oas L/D [L/D ...] [-b NBANDS] [-round]
!
! Output (OUT.oas, cell TOP, same layer numbers), exact (db/trap_out.cpp): a trapezoid whose four
! corners are lattice points is written as RECTANGLE, CTRAPEZOID (45-degree shapes) or TRAPEZOID;
! trapezoids with corners between lattice points are joined across those corners into regions
! with lattice vertices and written as POLYGONs (holes bridged exactly).
! -round: write every trapezoid as a record, corners rounded half up (lossy, `rounded=` count).
! One line per layer:  TRAPMERGE layer=L/D polygons=... area2=... traps=... t_*=...
program trapmerge_main
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use, intrinsic :: iso_c_binding
  use omp_lib
  use WideIntModule
  use TrapezoidModule
  use TrapMergeModule
  implicit none

  interface
     function ldb_polys_load(path, nsel, layers, dts, err, errsz) result(h) bind(c, name='ldb_polys_load')
       import :: c_ptr, c_char, c_int, c_size_t
       character(kind=c_char), intent(in) :: path(*)
       integer(c_int), value :: nsel
       integer(c_int), intent(in) :: layers(*), dts(*)
       character(kind=c_char), intent(inout) :: err(*)
       integer(c_size_t), value :: errsz
       type(c_ptr) :: h
     end function ldb_polys_load
     function ldb_polys_count(h, k, nvert) result(n) bind(c, name='ldb_polys_count')
       import :: c_ptr, c_int, c_int64_t
       type(c_ptr), value :: h
       integer(c_int), value :: k
       integer(c_int64_t), intent(out) :: nvert
       integer(c_int64_t) :: n
     end function ldb_polys_count
     subroutine ldb_polys_get(h, k, poff, xs, ys) bind(c, name='ldb_polys_get')
       import :: c_ptr, c_int, c_int64_t
       type(c_ptr), value :: h
       integer(c_int), value :: k
       integer(c_int64_t), intent(out) :: poff(*), xs(*), ys(*)
     end subroutine ldb_polys_get
     function ldb_polys_unit(h) result(u) bind(c, name='ldb_polys_unit')
       import :: c_ptr, c_double
       type(c_ptr), value :: h
       real(c_double) :: u
     end function ldb_polys_unit
     subroutine ldb_polys_free(h) bind(c, name='ldb_polys_free')
       import :: c_ptr
       type(c_ptr), value :: h
     end subroutine ldb_polys_free
     function ldb_ow_open(path, unit) result(h) bind(c, name='ldb_ow_open')
       import :: c_ptr, c_char, c_double
       character(kind=c_char), intent(in) :: path(*)
       real(c_double), value :: unit
       type(c_ptr) :: h
     end function ldb_ow_open
     subroutine ldb_ow_begin_cell(h, name) bind(c, name='ldb_ow_begin_cell')
       import :: c_ptr, c_char
       type(c_ptr), value :: h
       character(kind=c_char), intent(in) :: name(*)
     end subroutine ldb_ow_begin_cell
     subroutine ldb_ow_end_cell(h) bind(c, name='ldb_ow_end_cell')
       import :: c_ptr
       type(c_ptr), value :: h
     end subroutine ldb_ow_end_cell
     subroutine ldb_ow_trap(h, l, d, y1, y2, xbl, xbr, xtl, xtr) bind(c, name='ldb_ow_trap')
       import :: c_ptr, c_int, c_int64_t
       type(c_ptr), value :: h
       integer(c_int), value :: l, d
       integer(c_int64_t), value :: y1, y2, xbl, xbr, xtl, xtr
     end subroutine ldb_ow_trap
     function ldb_ow_trapset(h, l, d, n, rec, stats) result(rc) bind(c, name='ldb_ow_trapset')
       import :: c_ptr, c_int, c_int64_t, c_int32_t
       type(c_ptr), value :: h
       integer(c_int), value :: l, d
       integer(c_int64_t), value :: n
       integer(c_int32_t), intent(in) :: rec(*)
       integer(c_int64_t), intent(out) :: stats(7)
       integer(c_int) :: rc
     end function ldb_ow_trapset
     function ldb_ow_close(h, stats) result(rc) bind(c, name='ldb_ow_close')
       import :: c_ptr, c_int, c_int64_t
       type(c_ptr), value :: h
       integer(c_int64_t), intent(out) :: stats(6)
       integer(c_int) :: rc
     end function ldb_ow_close
  end interface

  character(len=4096) :: fin, fout, arg
  character(kind=c_char) :: err(1024)
  integer :: nargs, i, k, nsel, nbands, ios, slash
  integer(c_int), allocatable :: lay(:), dt(:)
  type(c_ptr) :: hp, hw
  integer(int64) :: npoly, nvert, ntrap, j, nrounded, nexact
  integer(int64), allocatable :: poff(:), px(:), py(:)
  type(Trapezoid), allocatable :: traps(:)
  type(TrapMergeStats) :: st
  type(i128) :: a2
  logical :: isint
  integer :: status
  real(real64) :: t0, tload, twrite, unit
  integer(c_int64_t) :: wst(6), ost(7)
  integer(int32), allocatable :: rec(:, :)
  logical :: rounded_mode
  character(len=32) :: a2s

  nargs = command_argument_count()
  if (nargs < 3) then
     write(*, '(a)') 'usage: TRAPMERGE.exe IN OUT.oas L/D [L/D ...] [-b NBANDS]'
     stop 1
  end if
  call get_command_argument(1, fin); call get_command_argument(2, fout)
  allocate(lay(nargs), dt(nargs))
  nsel = 0; nbands = 0; rounded_mode = .false.
  i = 3
  do while (i <= nargs)
     call get_command_argument(i, arg)
     if (trim(arg) == '-round') then
        rounded_mode = .true.
     else if (trim(arg) == '-b') then
        i = i + 1; call get_command_argument(i, arg); read(arg, *) nbands
     else
        slash = index(arg, '/')
        if (slash == 0) slash = index(arg, ':')
        nsel = nsel + 1
        read(arg(1:slash - 1), *, iostat=ios) lay(nsel)
        read(arg(slash + 1:), *, iostat=ios) dt(nsel)
     end if
     i = i + 1
  end do
  t0 = omp_get_wtime()
  err = c_null_char
  hp = ldb_polys_load(trim(fin)//c_null_char, int(nsel, c_int), lay, dt, err, int(size(err), c_size_t))
  if (.not. c_associated(hp)) then
     write(*, '(a)') 'TRAPMERGE: cannot open '//trim(fin)
     stop 1
  end if
  tload = omp_get_wtime() - t0
  unit = ldb_polys_unit(hp)
  write(*, '(a,a,a,f0.3,a,i0,a,f0.1)') 'TRAPMERGE file=', trim(fin), ' load_s=', tload, &
       ' threads=', omp_get_max_threads(), ' unit=', unit
  hw = ldb_ow_open(trim(fout)//c_null_char, real(unit, c_double))
  if (.not. c_associated(hw)) stop 1
  call ldb_ow_begin_cell(hw, 'TOP'//c_null_char)
  do k = 1, nsel
     npoly = ldb_polys_count(hp, int(k - 1, c_int), nvert)
     allocate(poff(npoly + 1), px(max(nvert, 1_int64)), py(max(nvert, 1_int64)))
     call ldb_polys_get(hp, int(k - 1, c_int), poff, px, py)
     st = TrapMergeStats()
     call TrapMergePolygons(npoly, poff, px, py, traps, ntrap, st, status, nbands)
     if (status /= 0) then
        write(*, '(a,i0,a,i0,a,i0)') 'TRAPMERGE layer=', lay(k), '/', dt(k), ' status=', status
        deallocate(poff, px, py); cycle
     end if
     call trap_area2(traps, ntrap, a2, isint)
     t0 = omp_get_wtime()
     nrounded = 0; nexact = 0; ost = 0
     if (rounded_mode) then
        do j = 1, ntrap
           call write_trap(traps(j), lay(k), dt(k))
        end do
     else
        allocate(rec(10, max(ntrap, 1_int64)))
        do j = 1, ntrap
           rec(:, j) = [traps(j)%y1, traps(j)%y2, traps(j)%l%ax, traps(j)%l%k, traps(j)%l%dx, traps(j)%l%dy, &
                        traps(j)%r%ax, traps(j)%r%k, traps(j)%r%dx, traps(j)%r%dy]
        end do
        if (ntrap > 0) i = ldb_ow_trapset(hw, lay(k), dt(k), ntrap, rec, ost)
        deallocate(rec)
     end if
     twrite = omp_get_wtime() - t0
     write(a2s, '(es22.15)') i128_to_real64(a2)
     write(*, '(a,i0,a,i0,1x,a)') 'TRAPMERGE layer=', lay(k), '/', dt(k), trim(TrapMergeStatsLine(st))
     write(*, '(a,i0,a,i0,a,a,a,l1,a,i0,a,i0,a,i0)') 'TRAPMERGE layer=', lay(k), '/', dt(k), &
          ' area2=', trim(i128_str(a2)), ' area_integral=', isint, ' boxes=', st%boxes, &
          ' exact_traps=', nexact, &
          ' rounded=', nrounded
     if (.not. rounded_mode) write(*, '(a,i0,a,i0,7(a,i0))') 'TRAPMERGE layer=', lay(k), '/', dt(k), &
          ' out_trap_records=', ost(1), ' out_polygons=', ost(2), ' out_poly_vertices=', ost(3), &
          ' groups=', ost(4), ' holes=', ost(5), ' max_group=', ost(6), ' out_anomalies=', ost(7)
     write(*, '(a,i0,a,i0,10(a,f0.3))') 'TRAPMERGE layer=', lay(k), '/', dt(k), &
          ' t_edges=', st%t_edges, ' t_grid=', st%t_grid, ' t_cross=', st%t_cross, &
          ' t_hot=', st%t_hot, ' t_hits=', st%t_hits, ' t_frag=', st%t_frag, &
          ' t_sweep=', st%t_sweep, ' t_stitch=', st%t_stitch, ' t_total=', st%t_total, &
          ' t_write=', twrite
     deallocate(poff, px, py, traps)
  end do
  call ldb_polys_free(hp)
  call ldb_ow_end_cell(hw)
  if (ldb_ow_close(hw, wst) /= 0) stop 1
  write(*, '(6(a,i0))') 'TRAPMERGE out: rectangles=', wst(1), ' trapezoids=', wst(2), &
       ' ctrapezoids=', wst(3), ' polygons=', wst(4), ' degenerate=', wst(5), ' bytes=', wst(6)

contains

  !> x of side s (anchored at row y1) at row y: exact floor and remainder.
  subroutine side_x(s, y1, y, x, exact)
    type(TrapSide), intent(in) :: s
    integer(int64), intent(in) :: y1, y
    integer(int64), intent(out) :: x
    logical, intent(out) :: exact
    integer(int64) :: q, r
    call i128_divmod(side_x_num(s, y1, y), int(s%dy, int64), q, r)
    exact = r == 0
    x = q
    if (2*r >= s%dy) x = q + 1                   ! round half up
  end subroutine side_x

  subroutine write_trap(t, l, d)
    type(Trapezoid), intent(in) :: t
    integer(c_int), intent(in) :: l, d
    integer(int64) :: ya, yb, xbl, xbr, xtl, xtr
    logical :: e1, e2, e3, e4
    ya = t%y1; yb = t%y2
    call side_x(t%l, ya, ya, xbl, e1); call side_x(t%l, ya, yb, xtl, e2)
    call side_x(t%r, ya, ya, xbr, e3); call side_x(t%r, ya, yb, xtr, e4)
    if (e1 .and. e2 .and. e3 .and. e4) then
       nexact = nexact + 1
    else
       nrounded = nrounded + 1
    end if
    call ldb_ow_trap(hw, l, d, ya, yb, xbl, xbr, xtl, xtr)
  end subroutine write_trap

  function i128_str(a) result(s)
    type(i128), intent(in) :: a
    character(len=48) :: s
    integer(int64) :: q, r
    type(i128) :: x
    character(len=48) :: buf
    integer :: p
    logical :: neg
    x = a; neg = i128_sign(a) < 0
    if (neg) x = i128_neg(a)
    buf = ''; p = 48
    do
       call i128_divmod(x, 10_int64, q, r)
       buf(p:p) = achar(48 + int(r)); p = p - 1
       x = i128_from_int64(q)
       if (q == 0) exit
    end do
    if (neg) then
       buf(p:p) = '-'; p = p - 1
    end if
    s = adjustl(buf(p + 1:48))
  end function i128_str

end program trapmerge_main
