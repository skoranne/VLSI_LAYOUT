! File   : trapbool_main.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: TRAPBOOL.exe - two-layer Booleans (OR, AND, NOT, XOR, B-A) of any-angle layers of an
!          OASIS/GDSII file with TrapMergeModule (one joint snap rounding of A and B, one predicate
!          sweep per op), timing breakdown, exact OASIS output (db/trap_out.cpp).
!
!   TRAPBOOL.exe IN OUT.oas LA/DA LB/DB [-ops OR,AND,NOT,XOR,BNOTA,A,B] [-b NBANDS] [-out L]
!
! Result of op k goes to layer L+k-1 (default L = 1000), datatype 0, in the order of -ops
! (default OR,AND,NOT,XOR). Lines:
!   TRAPBOOL shared polygons=... edges=... crossings=... t_edges=... t_grid=... ... t_frag=...
!   TRAPBOOL op=AND layer=1001/0 area2=... traps=... boxes=... t_sweep=... t_stitch=... t_write=...
!   TRAPBOOL identities ...    (exact checks, on the requested ops that allow them)
program trapbool_main
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

  character(len=4096) :: fin, fout, arg, opsarg
  character(kind=c_char) :: err(1024)
  integer :: nargs, i, k, nbands, ios, slash, nops, outbase, p0, p1, rc
  integer(c_int) :: lay(2), dt(2)
  type(c_ptr) :: hp, hw
  integer(int64) :: npA, npB, nvA, nvB, j
  integer(int64), allocatable :: poffA(:), pxA(:), pyA(:), poffB(:), pxB(:), pyB(:)
  type(TrapList), allocatable :: res(:)
  type(TrapMergeStats) :: st
  integer(int32) :: ops(16)
  character(len=8) :: opn(16), tok
  type(i128) :: a2(16)
  logical :: isint
  integer :: status, nin
  real(real64) :: t0, tload, twrite(16), unit
  integer(c_int64_t) :: wst(6), ost(7)
  integer(int32), allocatable :: rec(:, :)
  integer :: iOR, iAND, iNOT, iXOR, iBN, iA, iB

  nargs = command_argument_count()
  if (nargs < 4) then
     write(*, '(a)') 'usage: TRAPBOOL.exe IN OUT.oas LA/DA LB/DB [-ops OR,AND,NOT,XOR,BNOTA,A,B] [-b N] [-out L]'
     stop 1
  end if
  call get_command_argument(1, fin); call get_command_argument(2, fout)
  nbands = 0; outbase = 1000; opsarg = 'OR,AND,NOT,XOR'; nin = 0
  i = 3
  do while (i <= nargs)
     call get_command_argument(i, arg)
     if (trim(arg) == '-b') then
        i = i + 1; call get_command_argument(i, arg); read(arg, *) nbands
     else if (trim(arg) == '-out') then
        i = i + 1; call get_command_argument(i, arg); read(arg, *) outbase
     else if (trim(arg) == '-ops') then
        i = i + 1; call get_command_argument(i, opsarg)
     else
        slash = index(arg, '/')
        if (slash == 0) slash = index(arg, ':')
        nin = nin + 1
        if (nin > 2) stop 'TRAPBOOL: two layers expected'
        read(arg(1:slash - 1), *, iostat=ios) lay(nin)
        read(arg(slash + 1:), *, iostat=ios) dt(nin)
     end if
     i = i + 1
  end do
  if (nin /= 2) stop 'TRAPBOOL: two layers expected'
  nops = 0; p0 = 1
  do
     p1 = index(opsarg(p0:), ',')
     if (p1 == 0) then
        tok = opsarg(p0:len_trim(opsarg))
     else
        tok = opsarg(p0:p0 + p1 - 2)
     end if
     nops = nops + 1
     opn(nops) = tok
     select case (trim(tok))
     case ('OR');    ops(nops) = TOP_OR
     case ('AND');   ops(nops) = TOP_AND
     case ('NOT');   ops(nops) = TOP_NOT
     case ('XOR');   ops(nops) = TOP_XOR
     case ('BNOTA'); ops(nops) = TOP_BNOTA
     case ('A');     ops(nops) = TOP_A
     case ('B');     ops(nops) = TOP_B
     case default
        write(*, '(a)') 'TRAPBOOL: unknown op '//trim(tok); stop 1
     end select
     if (p1 == 0) exit
     p0 = p0 + p1
  end do
  t0 = omp_get_wtime()
  err = c_null_char
  hp = ldb_polys_load(trim(fin)//c_null_char, 2_c_int, lay, dt, err, int(size(err), c_size_t))
  if (.not. c_associated(hp)) then
     write(*, '(a)') 'TRAPBOOL: cannot open '//trim(fin)
     stop 1
  end if
  npA = ldb_polys_count(hp, 0_c_int, nvA)
  allocate(poffA(npA + 1), pxA(max(nvA, 1_int64)), pyA(max(nvA, 1_int64)))
  call ldb_polys_get(hp, 0_c_int, poffA, pxA, pyA)
  npB = ldb_polys_count(hp, 1_c_int, nvB)
  allocate(poffB(npB + 1), pxB(max(nvB, 1_int64)), pyB(max(nvB, 1_int64)))
  call ldb_polys_get(hp, 1_c_int, poffB, pxB, pyB)
  unit = ldb_polys_unit(hp)
  call ldb_polys_free(hp)
  tload = omp_get_wtime() - t0
  write(*, '(a,a,a,f0.3,a,i0,a,f0.1,4(a,i0))') 'TRAPBOOL file=', trim(fin), ' load_s=', tload, &
       ' threads=', omp_get_max_threads(), ' unit=', unit, ' A=', lay(1), '/', dt(1), ' B=', lay(2), '/', dt(2)
  allocate(res(nops))
  st = TrapMergeStats()
  call TrapBooleanPolygons(npA, poffA, pxA, pyA, npB, poffB, pxB, pyB, nops, ops, res, st, status, nbands)
  if (status /= 0) then
     write(*, '(a,i0)') 'TRAPBOOL status=', status; stop 1
  end if
  write(*, '(a,1x,a)') 'TRAPBOOL shared', trim(TrapMergeStatsLine(st))
  write(*, '(a,i0,a,i0,7(a,f0.3))') 'TRAPBOOL shared polygonsA=', npA, ' polygonsB=', npB, &
       ' t_edges=', st%t_edges, ' t_grid=', st%t_grid, ' t_cross=', st%t_cross, ' t_hot=', st%t_hot, &
       ' t_hits=', st%t_hits, ' t_frag=', st%t_frag, ' t_total=', st%t_total
  hw = ldb_ow_open(trim(fout)//c_null_char, real(unit, c_double))
  if (.not. c_associated(hw)) stop 1
  call ldb_ow_begin_cell(hw, 'TOP'//c_null_char)
  do k = 1, nops
     call trap_area2(res(k)%t, res(k)%n, a2(k), isint)
     t0 = omp_get_wtime(); ost = 0
     if (res(k)%n > 0) then
        allocate(rec(10, res(k)%n))
        !$omp parallel do
        do j = 1, res(k)%n
           rec(1, j) = res(k)%t(j)%y1;   rec(2, j) = res(k)%t(j)%y2
           rec(3, j) = res(k)%t(j)%l%ax; rec(4, j) = res(k)%t(j)%l%k
           rec(5, j) = res(k)%t(j)%l%dx; rec(6, j) = res(k)%t(j)%l%dy
           rec(7, j) = res(k)%t(j)%r%ax; rec(8, j) = res(k)%t(j)%r%k
           rec(9, j) = res(k)%t(j)%r%dx; rec(10, j) = res(k)%t(j)%r%dy
        end do
        !$omp end parallel do
        rc = ldb_ow_trapset(hw, int(outbase + k - 1, c_int), 0_c_int, res(k)%n, rec, ost)
        deallocate(rec)
     end if
     twrite(k) = omp_get_wtime() - t0
     write(*, '(a,a,a,i0,a,a,a,l1,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,3(a,f0.3))') 'TRAPBOOL op=', trim(opn(k)), &
          ' layer=', outbase + k - 1, '/0 area2=', trim(i128_str(a2(k))), ' area_integral=', isint, &
          ' traps=', res(k)%n, ' boxes=', res(k)%boxes, ' anomalies=', res(k)%anomalies, &
          ' out_trap_records=', ost(1), ' out_polygons=', ost(2), ' out_anomalies=', ost(7), &
          ' t_sweep=', res(k)%t_sweep, ' t_stitch=', res(k)%t_stitch, ' t_write=', twrite(k)
  end do
  call ldb_ow_end_cell(hw)
  if (ldb_ow_close(hw, wst) /= 0) stop 1
  write(*, '(6(a,i0))') 'TRAPBOOL out: rectangles=', wst(1), ' trapezoids=', wst(2), &
       ' ctrapezoids=', wst(3), ' polygons=', wst(4), ' degenerate=', wst(5), ' bytes=', wst(6)
  ! exact identities among the requested ops
  iOR = find(TOP_OR); iAND = find(TOP_AND); iNOT = find(TOP_NOT); iXOR = find(TOP_XOR)
  iBN = find(TOP_BNOTA); iA = find(TOP_A); iB = find(TOP_B)
  if (iOR > 0 .and. iAND > 0 .and. iXOR > 0) call ident('XOR=OR-AND', &
       i128_cmp(a2(iXOR), i128_sub(a2(iOR), a2(iAND))) == 0)
  if (iOR > 0 .and. iAND > 0 .and. iA > 0 .and. iB > 0) call ident('OR+AND=A+B', &
       i128_cmp(i128_add(a2(iOR), a2(iAND)), i128_add(a2(iA), a2(iB))) == 0)
  if (iNOT > 0 .and. iAND > 0 .and. iA > 0) call ident('NOT+AND=A', &
       i128_cmp(i128_add(a2(iNOT), a2(iAND)), a2(iA)) == 0)
  if (iBN > 0 .and. iAND > 0 .and. iB > 0) call ident('BNOTA+AND=B', &
       i128_cmp(i128_add(a2(iBN), a2(iAND)), a2(iB)) == 0)
  if (iOR > 0 .and. iNOT > 0 .and. iBN > 0 .and. iAND > 0) call ident('OR=NOT+AND+BNOTA', &
       i128_cmp(a2(iOR), i128_add(i128_add(a2(iNOT), a2(iAND)), a2(iBN))) == 0)
  if (st%anomalies /= 0) write(*, '(a,i0)') 'TRAPBOOL FAIL anomalies=', st%anomalies

contains

  integer function find(op)
    integer(int32), intent(in) :: op
    integer :: q
    find = 0
    do q = 1, nops
       if (ops(q) == op) then
          find = q; return
       end if
    end do
  end function find

  subroutine ident(name, ok)
    character(len=*), intent(in) :: name
    logical, intent(in) :: ok
    if (ok) then
       write(*, '(a,a,a)') 'TRAPBOOL identity ', name, ' OK'
    else
       write(*, '(a,a,a)') 'TRAPBOOL identity ', name, ' FAIL'
    end if
  end subroutine ident

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

end program trapbool_main
