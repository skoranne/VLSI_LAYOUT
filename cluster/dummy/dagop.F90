! dagop.F90 -- dummy MAGPARSER-like DAG operation worker for the cluster prototype.
!
! Usage:  dagop <op> <outfile> [inputs...] [params...]
!   gen    out seed n extent maxsize      seeded random boxes (Park-Miller LCG)
!   grow   out in d                       grow every box by d
!   shrink out in d                       shrink by d, drop empty boxes
!   and    out in x1 y1 x2 y2             clip to window, drop empty boxes
!   or     out in1 in2                    concatenate
!   area   out in                         int64(3): area, count, checksum mod P
!
! Box files: stream of int32 x1,y1,x2,y2 (little-endian hosts assumed).
! With MPI (default) every rank processes a slice of the boxes and rank 0
! gathers / reduces; build with -DNO_MPI for a serial binary.
! DAGOP_SLEEP_MS=<ms> burns CPU for ms/nprocs to simulate an expensive op.
! Must stay bit-identical to ../dagref.jl.
program dagop
#ifndef NO_MPI
  use mpi
#endif
  use, intrinsic :: iso_fortran_env, only: int32, int64, error_unit
  implicit none
  integer(int64), parameter :: P = 2147483647_int64
  integer :: rank, nprocs, ierr, nargs, i, m, lo, hi
  character(len=:), allocatable :: op, outf
  integer(int32), allocatable :: a(:,:), b(:,:), r(:,:)
  integer(int64) :: d, w(4), st(3), tot(3), x1, y1, x2, y2

  rank = 0; nprocs = 1; ierr = 0
#ifndef NO_MPI
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
#endif
  nargs = command_argument_count()
  if (nargs < 2) call die('usage: dagop op out [inputs] [params]')
  op = arg_s(1); outf = arg_s(2)

  select case (op)
  case ('gen')
    if (nargs /= 6) call die('gen: out seed n extent maxsize')
    do i = 1, 4
      w(i) = arg_i(2 + i)
    end do
    call burn()
    if (rank == 0) then
      call gen(w, r)
      call write_boxes(outf, r)
    end if

  case ('grow', 'shrink')
    if (nargs /= 4) call die(op // ': out in d')
    call read_boxes(arg_s(3), a)
    d = arg_i(4)
    if (op == 'shrink') d = -d
    call burn()
    call slice(size(a, 2), lo, hi)
    allocate(r(4, max(0, hi - lo + 1)))
    m = 0
    do i = lo, hi
      x1 = int(a(1,i), int64) - d; y1 = int(a(2,i), int64) - d
      x2 = int(a(3,i), int64) + d; y2 = int(a(4,i), int64) + d
      call keep(x1, y1, x2, y2)
    end do
    call gather_write(r, m)

  case ('and')
    if (nargs /= 7) call die('and: out in x1 y1 x2 y2')
    call read_boxes(arg_s(3), a)
    do i = 1, 4
      w(i) = arg_i(3 + i)
    end do
    call burn()
    call slice(size(a, 2), lo, hi)
    allocate(r(4, max(0, hi - lo + 1)))
    m = 0
    do i = lo, hi
      x1 = max(int(a(1,i), int64), w(1)); y1 = max(int(a(2,i), int64), w(2))
      x2 = min(int(a(3,i), int64), w(3)); y2 = min(int(a(4,i), int64), w(4))
      call keep(x1, y1, x2, y2)
    end do
    call gather_write(r, m)

  case ('or')
    if (nargs /= 4) call die('or: out in1 in2')
    call burn()
    if (rank == 0) then
      call read_boxes(arg_s(3), a)
      call read_boxes(arg_s(4), b)
      allocate(r(4, size(a, 2) + size(b, 2)))
      r(:, 1:size(a, 2)) = a
      r(:, size(a, 2) + 1:) = b
      call write_boxes(outf, r)
    end if

  case ('area')
    if (nargs /= 3) call die('area: out in')
    call read_boxes(arg_s(3), a)
    call burn()
    call slice(size(a, 2), lo, hi)
    st = 0
    do i = lo, hi
      x1 = a(1,i); y1 = a(2,i); x2 = a(3,i); y2 = a(4,i)
      st(1) = st(1) + (x2 - x1) * (y2 - y1)
      st(2) = st(2) + 1
      st(3) = modulo(st(3) + modulo(31*x1 + 17*y1 + 13*x2 + 7*y2, P), P)
    end do
#ifndef NO_MPI
    call MPI_Reduce(st, tot, 3, MPI_INTEGER8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
#else
    tot = st
#endif
    if (rank == 0) then
      tot(3) = modulo(tot(3), P)
      call write_i64(outf, tot)
    end if

  case default
    call die('unknown op ' // op)
  end select

#ifndef NO_MPI
  call MPI_Finalize(ierr)
#endif

contains

  subroutine keep(x1, y1, x2, y2)
    integer(int64), intent(in) :: x1, y1, x2, y2
    if (x1 < x2 .and. y1 < y2) then
      m = m + 1
      r(:, m) = int([x1, y1, x2, y2], int32)
    end if
  end subroutine keep

  subroutine die(msg)
    character(len=*), intent(in) :: msg
    write(error_unit, '(a)') 'dagop: ' // msg
#ifndef NO_MPI
    call MPI_Abort(MPI_COMM_WORLD, 2, ierr)
#endif
    error stop 2
  end subroutine die

  function arg_s(k) result(s)
    integer, intent(in) :: k
    character(len=:), allocatable :: s
    integer :: l
    call get_command_argument(k, length=l)
    allocate(character(len=l) :: s)
    call get_command_argument(k, s)
  end function arg_s

  function arg_i(k) result(v)
    integer, intent(in) :: k
    integer(int64) :: v
    integer :: ios
    character(len=:), allocatable :: s
    s = arg_s(k)
    read(s, *, iostat=ios) v
    if (ios /= 0) call die('bad integer argument ' // arg_s(k))
  end function arg_i

  subroutine slice(n, lo, hi)
    integer, intent(in) :: n
    integer, intent(out) :: lo, hi
    lo = int(int(rank, int64) * n / nprocs) + 1
    hi = int(int(rank + 1, int64) * n / nprocs)
  end subroutine slice

  subroutine burn()
    character(len=32) :: v
    integer :: l, s, j
    integer(int64) :: ms, t0, t1, rate
    real(8) :: acc
    call get_environment_variable('DAGOP_SLEEP_MS', v, l, s)
    if (s /= 0 .or. l == 0) return
    read(v, *, iostat=s) ms
    if (s /= 0 .or. ms <= 0) return
    ms = ms / nprocs
    call system_clock(t0, rate)
    acc = 0
    do
      do j = 1, 20000
        acc = acc + sqrt(real(j, 8))
      end do
      call system_clock(t1)
      if ((t1 - t0) * 1000_int64 >= ms * rate) exit
    end do
    if (acc < 0) print *, acc
  end subroutine burn

  subroutine gen(prm, out)
    integer(int64), intent(in) :: prm(4)
    integer(int32), allocatable, intent(out) :: out(:,:)
    integer(int64) :: s, x, y, wd, ht
    integer :: k
    if (prm(2) < 0 .or. prm(3) <= 0 .or. prm(4) <= 0) call die('bad gen params')
    s = modulo(prm(1), P)
    if (s == 0) s = 1
    allocate(out(4, prm(2)))
    do k = 1, int(prm(2))
      s = modulo(s * 48271_int64, P); x  = modulo(s, prm(3))
      s = modulo(s * 48271_int64, P); y  = modulo(s, prm(3))
      s = modulo(s * 48271_int64, P); wd = 1 + modulo(s, prm(4))
      s = modulo(s * 48271_int64, P); ht = 1 + modulo(s, prm(4))
      out(:, k) = int([x, y, x + wd, y + ht], int32)
    end do
  end subroutine gen

  subroutine read_boxes(fn, x)
    character(len=*), intent(in) :: fn
    integer(int32), allocatable, intent(out) :: x(:,:)
    integer :: u, ios
    integer(int64) :: sz
    inquire(file=fn, size=sz)
    if (sz < 0) call die('cannot stat ' // fn)
    if (modulo(sz, 16_int64) /= 0) call die('bad box file size ' // fn)
    allocate(x(4, sz / 16))
    open(newunit=u, file=fn, access='stream', form='unformatted', status='old', action='read', iostat=ios)
    if (ios /= 0) call die('cannot open ' // fn)
    if (sz > 0) read(u) x
    close(u)
  end subroutine read_boxes

  subroutine write_boxes(fn, x)
    character(len=*), intent(in) :: fn
    integer(int32), intent(in) :: x(:,:)
    integer :: u
    open(newunit=u, file=fn, access='stream', form='unformatted', status='replace', action='write')
    if (size(x) > 0) write(u) x
    close(u)
  end subroutine write_boxes

  subroutine write_i64(fn, x)
    character(len=*), intent(in) :: fn
    integer(int64), intent(in) :: x(:)
    integer :: u
    open(newunit=u, file=fn, access='stream', form='unformatted', status='replace', action='write')
    write(u) x
    close(u)
  end subroutine write_i64

  subroutine gather_write(loc, cnt)
    integer(int32), intent(in) :: loc(:,:)
    integer, intent(in) :: cnt
#ifndef NO_MPI
    integer, allocatable :: counts(:), displs(:)
    integer(int32), allocatable :: g(:,:)
    integer :: j, ntot
    allocate(counts(nprocs), displs(nprocs))
    counts = 0; displs = 0; ntot = 0
    call MPI_Gather(4 * cnt, 1, MPI_INTEGER, counts, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (rank == 0) then
      do j = 2, nprocs
        displs(j) = displs(j - 1) + counts(j - 1)
      end do
      ntot = sum(counts) / 4
    end if
    allocate(g(4, max(1, ntot)))
    call MPI_Gatherv(loc, 4 * cnt, MPI_INTEGER, g, counts, displs, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    if (rank == 0) call write_boxes(outf, g(:, 1:ntot))
#else
    call write_boxes(outf, loc(:, 1:cnt))
#endif
  end subroutine gather_write

end program dagop
