! File   : test_trapbool.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Randomised brute-force checks of the two-operand any-angle Booleans in TrapMergeModule
!   (joint snap rounding of A and B, packed winding pair, predicate sweep). Per case:
!   * operands A, B random (rectangles, triangles, rotated rectangles, stars); B partly made of
!     exact or slightly shifted copies of A polygons, so the operands share edges and vertices
!   * for OR, AND, NOT (A-B), XOR, B-A, A, B: no anomalies, valid trapezoids, integral area,
!     1 band == k bands (canonical)
!   * at rational sample points (x/21, y/21): inside the result <=> op(WA /= 0, WB /= 0) of the
!     snapped fragments (exact), and any disagreement with the unsnapped operands lies within
!     0.75 dbu of an input edge
!   * identities on the snapped operands (exact, integer 2A): OR + AND = A + B, XOR = OR - AND,
!     NOT + AND = A, (B-A) + AND = B
!   * Manhattan cases: 2A of every op == 2 * (unit cells where the op holds on the input)
!   usage: test_trapbool.exe [ncases] [seed] [offset] [npmax]
program test_trapbool
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use WideIntModule
  use ExactPredicatesModule
  use TrapezoidModule
  use TrapMergeModule
  use TrapPNumModule
  implicit none
  integer, parameter :: NOP = 7
  integer(int32), parameter :: OPS(NOP) = [TOP_OR, TOP_AND, TOP_NOT, TOP_XOR, TOP_BNOTA, TOP_A, TOP_B]
  character(len=5), parameter :: OPN(NOP) = ['OR   ', 'AND  ', 'NOT  ', 'XOR  ', 'BNOTA', 'A    ', 'B    ']
  integer :: ncase, seed, icase, nfail, nb, mode, i, j, k, nfail_case, iop
  integer(int64) :: npA, npB, ntrap1, ntrapk, G, area2_int
  integer(int64), allocatable :: poffA(:), pxA(:), pyA(:), poffB(:), pxB(:), pyB(:)
  integer(int64), allocatable :: poff(:), px(:), py(:)
  integer(int64) :: npoly
  type(FragSet) :: EA, EB, E, F
  type(Trapezoid), allocatable :: t1(:), tk(:)
  type(TrapMergeStats) :: st1, stk
  type(i128) :: a2
  logical :: isint
  character(len=32) :: arg
  integer :: istat
  real(real64) :: r
  integer(int64) :: offset, npmax, ar(NOP)
  logical :: big
  integer(int64), parameter :: gsizes(5) = [8_int64, 16_int64, 40_int64, 200_int64, 3000_int64]
  integer(int64) :: tot_traps(NOP), tot_inexact, tot_samples, tot_mismatch_unsnapped
  integer(int64) :: tot_polys = 0, tot_contacts = 0, tot_brute = 0

  ncase = 2000; seed = 4242
  if (command_argument_count() >= 1) then
     call get_command_argument(1, arg); read(arg, *) ncase
  end if
  if (command_argument_count() >= 2) then
     call get_command_argument(2, arg); read(arg, *) seed
  end if
  offset = 0; npmax = 10
  if (command_argument_count() >= 3) then
     call get_command_argument(3, arg); read(arg, *) offset
  end if
  if (command_argument_count() >= 4) then
     call get_command_argument(4, arg); read(arg, *) npmax
  end if
  big = npmax > 40
  call init_rng(seed)
  nfail = 0; tot_traps = 0; tot_inexact = 0; tot_samples = 0; tot_mismatch_unsnapped = 0
  do icase = 1, ncase
     mode = mod(icase, 5)            ! 0: Manhattan only
     call random_number(r)
     G = gsizes(1 + int(r*5))
     nfail_case = 0
     call make_case(mode, G, npoly, poff, px, py)
     call take(npoly, poff, px, py, npA, poffA, pxA, pyA)
     call make_case(mode, G, npoly, poff, px, py)
     call mix_copies(npoly, poff, px, py)
     call take(npoly, poff, px, py, npB, poffB, pxB, pyB)
     pxA = pxA + offset; pyA = pyA - offset; pxB = pxB + offset; pyB = pyB - offset
     call TrapBuildEdges(npA, poffA, pxA, pyA, EA, istat)
     if (istat /= 0) call fail('build status A')
     call TrapBuildEdges(npB, poffB, pxB, pyB, EB, istat)
     if (istat /= 0) call fail('build status B')
     call TrapConcatEdges(EA, EB, E)
     st1 = TrapMergeStats()
     call TrapSnapRound(E, F, st1)
     tot_inexact = tot_inexact + st1%crossings_inexact
     if (.not. big) call check_fragments()
     do iop = 1, NOP
        st1%anomalies = 0; stk = TrapMergeStats()
        call TrapSweepUnion(F, 1, t1, ntrap1, st1, OPS(iop))
        call random_number(r); nb = 2 + int(r*9)
        call TrapSweepUnion(F, nb, tk, ntrapk, stk, OPS(iop))
        if (st1%anomalies /= 0) call fail('anomalies nb=1 '//OPN(iop))
        if (stk%anomalies /= 0) call fail('anomalies nb=k '//OPN(iop))
        call trap_sort(t1, ntrap1); call trap_sort(tk, ntrapk)
        if (ntrap1 /= ntrapk) then
           call fail('band count changes the result '//OPN(iop))
        else
           do i = 1, int(ntrap1)
              if (.not. trap_equal(t1(i), tk(i))) then
                 call fail('band count changes the result '//OPN(iop)); exit
              end if
           end do
        end if
        do i = 1, int(ntrap1)
           if (.not. trap_valid(t1(i))) then
              call fail('invalid trapezoid '//OPN(iop)); exit
           end if
        end do
        call trap_area2(t1, ntrap1, a2, isint)
        if (.not. isint) call fail('area not integral (I1) '//OPN(iop))
        ar(iop) = nint(i128_to_real64(a2), int64)
        tot_traps(iop) = tot_traps(iop) + ntrap1
        if (.not. big) call check_samples(OPS(iop), OPN(iop))
        call check_pnum(OPN(iop))
        if (mode == 0 .and. G <= 200 .and. offset == 0 .and. .not. big) then
           area2_int = ar(iop)
           if (area2_int /= 2*manhattan_cells_op(OPS(iop))) call fail('Manhattan area mismatch '//OPN(iop))
        end if
     end do
     ! exact identities on the snapped operands (ar = 2A, integers)
     if (ar(1) + ar(2) /= ar(6) + ar(7)) call fail('identity OR + AND = A + B')
     if (ar(4) /= ar(1) - ar(2)) call fail('identity XOR = OR - AND')
     if (ar(3) + ar(2) /= ar(6)) call fail('identity NOT + AND = A')
     if (ar(5) + ar(2) /= ar(7)) call fail('identity (B-A) + AND = B')
     if (mode == 0 .and. st1%crossings_inexact /= 0) call fail('inexact crossing in Manhattan data')
     if (nfail_case > 0) then
        nfail = nfail + 1
        if (nfail <= 3) call dump_case()
     end if
  end do
  write(*, '(a,i0,a,i0,a,i0,a,i0,a,i0)') 'test_trapbool: cases=', ncase, ' failed=', nfail, &
       ' inexact_crossings=', tot_inexact, ' samples=', tot_samples, &
       ' snapped_vs_exact_near_boundary=', tot_mismatch_unsnapped
  write(*, '(a,7(1x,a,"=",i0))') ' traps:', (trim(OPN(k)), tot_traps(k), k = 1, NOP)
  write(*, '(a,i0,a,i0,a,i0)') ' pnum: polygons=', tot_polys, ' contacts=', tot_contacts, &
       ' brute_force_checked=', tot_brute
  if (nfail > 0) then
     write(*, '(a)') 'FAIL'; error stop 1
  end if
  write(*, '(a)') 'PASS'

contains

  !> Explicit copy (nvfortran 26.1 corrupted descriptors with move_alloc between main-program
  !> allocatables here).
  subroutine take(n, po, x, y, nd, pod, xd, yd)
    integer(int64), intent(in) :: n
    integer(int64), intent(in) :: po(:), x(:), y(:)
    integer(int64), intent(out) :: nd
    integer(int64), allocatable, intent(inout) :: pod(:), xd(:), yd(:)
    integer(int64) :: nv
    nd = n
    nv = po(n + 1) - 1
    if (allocated(pod)) deallocate(pod)
    if (allocated(xd)) deallocate(xd)
    if (allocated(yd)) deallocate(yd)
    allocate(pod(n + 1), xd(max(nv, 1_int64)), yd(max(nv, 1_int64)))
    pod(1:n + 1) = po(1:n + 1); xd(1:nv) = x(1:nv); yd(1:nv) = y(1:nv)
  end subroutine take

  !> Replace some of B's polygons by exact or slightly shifted copies of A's (shared edges).
  subroutine mix_copies(npoly, poff, px, py)     ! arguments, not host association (nvfortran)
    integer(int64), intent(inout) :: npoly
    integer(int64), allocatable, intent(inout) :: poff(:), px(:), py(:)
    integer(int64), allocatable :: qo(:), qx(:), qy(:)
    integer(int64) :: p, q, n, nv, dx, dy, np2
    np2 = npoly + npA
    allocate(qo(np2 + 2), qx(size(px) + size(pxA)), qy(size(px) + size(pxA)))
    nv = 0; n = 0
    do p = 1, npoly
       n = n + 1; qo(n) = nv + 1
       qx(nv + 1:nv + poff(p + 1) - poff(p)) = px(poff(p):poff(p + 1) - 1)
       qy(nv + 1:nv + poff(p + 1) - poff(p)) = py(poff(p):poff(p + 1) - 1)
       nv = nv + poff(p + 1) - poff(p)
    end do
    do p = 1, npA
       if (irand(0_int64, 2_int64) /= 0) cycle
       dx = 0; dy = 0
       if (irand(0_int64, 1_int64) == 1) then
          dx = irand(-3_int64, 3_int64); dy = irand(-3_int64, 3_int64)
       end if
       n = n + 1; qo(n) = nv + 1
       q = poffA(p + 1) - poffA(p)
       qx(nv + 1:nv + q) = pxA(poffA(p):poffA(p + 1) - 1) + dx
       qy(nv + 1:nv + q) = pyA(poffA(p):poffA(p + 1) - 1) + dy
       nv = nv + q
    end do
    qo(n + 1) = nv + 1
    npoly = n
    call move_alloc(qo, poff); call move_alloc(qx, px); call move_alloc(qy, py)
  end subroutine mix_copies

  real(real64) function dist_ab(X, Y) result(d)
    integer(int64), intent(in) :: X, Y
    d = dist_edges(X, Y)          ! E = A ++ B: all input edges
  end function dist_ab

  subroutine check_samples(op, nm)
    integer(int32), intent(in) :: op
    character(len=*), intent(in) :: nm
    integer(int64) :: X, Y, ax, ay, wsnap, wa, wb
    integer :: s, nin, c1, c2
    logical :: on1, on2, on3, onb, want
    integer(int64) :: i
    do s = 1, 250
       X = irand(-21_int64, 21*(G + 5)); Y = irand(-21_int64, 21*(G + 5))
       X = X + 21*offset; Y = Y - 21*offset
       if (mod(X, 21_int64) == 0 .or. mod(Y, 21_int64) == 0) cycle
       call winding(F, X, Y, wsnap, on1)
       if (on1) cycle
       nin = 0; onb = .false.
       do i = 1, ntrap1
          if (21*int(t1(i)%y1, int64) >= Y .or. 21*int(t1(i)%y2, int64) <= Y) cycle
          ax = t1(i)%l%ax; ay = int(t1(i)%y1, int64) - t1(i)%l%k
          c1 = line_cmp(ax, ay, ax + t1(i)%l%dx, ay + t1(i)%l%dy, X, Y)
          ax = t1(i)%r%ax; ay = int(t1(i)%y1, int64) - t1(i)%r%k
          c2 = line_cmp(ax, ay, ax + t1(i)%r%dx, ay + t1(i)%r%dy, X, Y)
          if (c1 == 0 .or. c2 == 0) onb = .true.
          if (c1 < 0 .and. c2 > 0) nin = nin + 1
       end do
       if (onb) cycle
       tot_samples = tot_samples + 1
       if (nin > 1) then
          call fail('trapezoids overlap '//nm); return
       end if
       if ((nin == 1) .neqv. TrapInside(op, wsnap)) then
          call fail('result differs from op on the snapped windings '//nm); return
       end if
       call winding(EA, X, Y, wa, on2)
       call winding(EB, X, Y, wb, on3)
       if (on2 .or. on3) cycle
       want = TrapInside(op, wpack(wa, wb))
       if ((nin == 1) .neqv. want) then
          tot_mismatch_unsnapped = tot_mismatch_unsnapped + 1
          if (dist_ab(X, Y) > 0.75_real64) then
             call fail('snapped result moved more than 0.75 dbu from the input '//nm); return
          end if
       end if
    end do
  end subroutine check_samples

  !> PNUM of t1 vs the k-band result (canonical: identical), vs brute-force contacts over all
  !> pairs, and the area/piece sums.
  subroutine check_pnum(nm)
    character(len=*), intent(in) :: nm
    type(TrapPolys) :: P1, Pk
    integer(int64), allocatable :: par(:), ck(:), bl(:)
    integer(int64) :: i, j, r, s2, m, tot2
    integer :: kk
    call TrapPNum(t1, ntrap1, P1)
    call TrapPNum(tk, ntrapk, Pk)
    if (P1%anomalies /= 0) call fail('pnum anomalies '//nm)
    tot_polys = tot_polys + P1%np; tot_contacts = tot_contacts + P1%contacts
    if (P1%np /= Pk%np) then
       call fail('pnum: band count changes the polygon count '//nm); return
    end if
    do i = 1, P1%np
       if (P1%vx(i) /= Pk%vx(i) .or. P1%vy(i) /= Pk%vy(i) .or. P1%area2(i) /= Pk%area2(i) .or. &
           P1%x1(i) /= Pk%x1(i) .or. P1%x2(i) /= Pk%x2(i) .or. P1%y1(i) /= Pk%y1(i) .or. &
           P1%y2(i) /= Pk%y2(i)) then
          call fail('pnum: band count changes polygon data '//nm); return
       end if
    end do
    tot2 = 0
    do i = 1, P1%np
       tot2 = tot2 + P1%area2(i)
    end do
    if (tot2 /= ar(iop)) call fail('pnum: polygon areas do not sum to the layer area '//nm)
    if (sum(P1%npieces(1:P1%np)) /= ntrap1 .and. ntrap1 > 0) call fail('pnum: piece count '//nm)
    if (big .or. ntrap1 > 3000) return
    ! brute force: every pair, both rows of either piece
    tot_brute = tot_brute + 1
    allocate(par(max(ntrap1, 1_int64)))
    do i = 1, ntrap1
       par(i) = i
    end do
    do i = 1, ntrap1
       do j = i + 1, ntrap1
          do kk = 1, 4
             select case (kk)
             case (1); r = t1(i)%y1
             case (2); r = t1(i)%y2
             case (3); r = t1(j)%y1
             case default; r = t1(j)%y2
             end select
             if (r < max(t1(i)%y1, t1(j)%y1) .or. r > min(t1(i)%y2, t1(j)%y2)) cycle
             if (traps_touch(t1(i), t1(j), r)) then
                call bunion(par, i, j); exit
             end if
          end do
       end do
    end do
    ! same partition: pieces i, j share a brute root <=> same label
    allocate(bl(max(P1%np, 1_int64)), ck(max(ntrap1, 1_int64)))
    bl = 0
    do i = 1, ntrap1
       r = bfind(par, i)
       ! all pieces with the same brute root must have the same label, and labels map 1-1 to roots
       if (bl(P1%lab(i)) == 0) then
          bl(P1%lab(i)) = r
       else if (bl(P1%lab(i)) /= r) then
          call fail('pnum: grid contacts differ from brute force (split) '//nm); return
       end if
    end do
    m = 0
    do i = 1, ntrap1
       if (bfind(par, i) == i) m = m + 1
    end do
    if (m /= P1%np) call fail('pnum: grid contacts differ from brute force (count) '//nm)
  end subroutine check_pnum

  integer(int64) function bfind(par, x0) result(x)
    integer(int64), intent(inout) :: par(:)
    integer(int64), intent(in) :: x0
    x = x0
    do while (par(x) /= x)
       x = par(x)
    end do
  end function bfind

  subroutine bunion(par, a, b)
    integer(int64), intent(inout) :: par(:)
    integer(int64), intent(in) :: a, b
    integer(int64) :: ra, rb
    ra = bfind(par, a); rb = bfind(par, b)
    if (ra /= rb) par(max(ra, rb)) = min(ra, rb)
  end subroutine bunion

  integer(int64) function manhattan_cells_op(op) result(cnt)
    integer(int32), intent(in) :: op
    integer(int64) :: x, y, wa, wb
    logical :: on
    cnt = 0
    do y = -4, 2*G + 4
       do x = -4, 2*G + 4
          call winding(EA, 21*x + 10, 21*y + 10, wa, on)
          call winding(EB, 21*x + 10, 21*y + 10, wb, on)
          if (TrapInside(op, wpack(wa, wb))) cnt = cnt + 1
       end do
    end do
  end function manhattan_cells_op

  subroutine dump_case()
    integer(int64) :: p
    write(*, '(a,i0,a,i0)') '  case ', icase, ', G=', G
    do p = 1, npA
       write(*, '(a,*(1x,i0))') '   A', (pxA(j), pyA(j), j = int(poffA(p)), int(poffA(p + 1) - 1))
    end do
    do p = 1, npB
       write(*, '(a,*(1x,i0))') '   B', (pxB(j), pyB(j), j = int(poffB(p)), int(poffB(p + 1) - 1))
    end do
  end subroutine dump_case

  subroutine init_rng(s)
    integer, intent(in) :: s
    integer :: n
    integer, allocatable :: sd(:)
    call random_seed(size=n)
    allocate(sd(n)); sd = s + 37*[(i, i = 1, n)]
    call random_seed(put=sd)
  end subroutine init_rng

  integer(int64) function irand(lo, hi)
    integer(int64), intent(in) :: lo, hi
    real(real64) :: u
    call random_number(u)
    irand = lo + min(int(u*real(hi - lo + 1, real64), int64), hi - lo)
  end function irand

  subroutine fail(msg)
    character(len=*), intent(in) :: msg
    nfail_case = nfail_case + 1
    if (nfail < 5) write(*, '(a,i0,a,a)') 'case ', icase, ': ', msg
  end subroutine fail

  subroutine add_poly(n, xs, ys, npoly, poff, px, py, nv)
    integer, intent(in) :: n
    integer(int64), intent(in) :: xs(:), ys(:)
    integer(int64), intent(inout) :: npoly, nv
    integer(int64), intent(inout) :: poff(:), px(:), py(:)
    npoly = npoly + 1
    poff(npoly) = nv + 1
    px(nv + 1:nv + n) = xs(1:n); py(nv + 1:nv + n) = ys(1:n)
    nv = nv + n
    poff(npoly + 1) = nv + 1
  end subroutine add_poly

  subroutine make_case(mode, G, npoly, poff, px, py)
    integer, intent(in) :: mode
    integer(int64), intent(in) :: G
    integer(int64), intent(out) :: npoly
    integer(int64), allocatable, intent(inout) :: poff(:), px(:), py(:)
    integer(int64) :: np, p, x0, y0, w, h, xs(16), ys(16), cx, cy, nv
    integer :: kind, n, q
    real(real64) :: ang, rad, th, c, s
    if (allocated(poff)) deallocate(poff, px, py)
    np = irand(1_int64, npmax)
    allocate(poff(np + 2), px(16*np), py(16*np))
    npoly = 0; nv = 0
    do p = 1, np
       kind = int(irand(0_int64, 4_int64))
       if (mode == 0) kind = 0
       select case (kind)
       case (0)                                   ! axis-aligned rectangle (either orientation)
          x0 = irand(0_int64, G - 2); y0 = irand(0_int64, G - 2)
          w = irand(1_int64, max(1_int64, G/2)); h = irand(1_int64, max(1_int64, G/2))
          xs(1:4) = [x0, x0 + w, x0 + w, x0]; ys(1:4) = [y0, y0, y0 + h, y0 + h]
          if (irand(0_int64, 1_int64) == 1) then
             xs(1:4) = xs(4:1:-1); ys(1:4) = ys(4:1:-1)
          end if
          call add_poly(4, xs, ys, npoly, poff, px, py, nv)
       case (1)                                   ! triangle
          do q = 1, 3
             xs(q) = irand(0_int64, G); ys(q) = irand(0_int64, G)
          end do
          call add_poly(3, xs, ys, npoly, poff, px, py, nv)
       case (2)                                   ! rotated rectangle, vertices rounded
          cx = irand(0_int64, G); cy = irand(0_int64, G)
          call random_number(ang); ang = ang*3.14159265358979_real64
          w = irand(1_int64, max(1_int64, G/3)); h = irand(1_int64, max(1_int64, G/3))
          c = cos(ang); s = sin(ang)
          do q = 1, 4
             th = merge(-0.5_real64, 0.5_real64, q == 1 .or. q == 4)*w
             rad = merge(-0.5_real64, 0.5_real64, q <= 2)*h
             xs(q) = cx + nint(th*c - rad*s, int64); ys(q) = cy + nint(th*s + rad*c, int64)
          end do
          call add_poly(4, xs, ys, npoly, poff, px, py, nv)
       case (3)                                   ! star-shaped polygon (simple)
          cx = irand(0_int64, G); cy = irand(0_int64, G)
          n = int(irand(3_int64, 12_int64))
          do q = 1, n
             th = 6.28318530717959_real64*(q - 1 + 0.8_real64*rnd())/n
             rad = (0.1_real64 + rnd())*G/3
             xs(q) = cx + nint(rad*cos(th), int64); ys(q) = cy + nint(rad*sin(th), int64)
          end do
          call add_poly(n, xs, ys, npoly, poff, px, py, nv)
       case default                               ! copy of an earlier polygon, shifted a bit
          if (npoly == 0) then
             x0 = irand(0_int64, G - 2); y0 = irand(0_int64, G - 2)
             xs(1:3) = [x0, x0 + 1 + G/4, x0]; ys(1:3) = [y0, y0, y0 + 1 + G/4]
             call add_poly(3, xs, ys, npoly, poff, px, py, nv)
          else
             q = int(irand(1_int64, npoly))
             n = int(poff(q + 1) - poff(q))
             x0 = irand(-2_int64, 2_int64); y0 = irand(-2_int64, 2_int64)
             xs(1:n) = px(poff(q):poff(q + 1) - 1) + x0; ys(1:n) = py(poff(q):poff(q + 1) - 1) + y0
             call add_poly(n, xs, ys, npoly, poff, px, py, nv)
          end if
       end select
    end do
  end subroutine make_case

  real(real64) function rnd()
    call random_number(rnd)
  end function rnd

  !> Snap-rounding output: no crossings, no overlaps, no endpoint inside another fragment.
  subroutine check_fragments()
    integer(int64) :: a, b
    integer(int32) :: kk
    do a = 1, F%n
       do b = a + 1, F%n
          kk = seg_intersect_kind(F%x1(a), F%y1(a), F%x2(a), F%y2(a), F%x1(b), F%y1(b), F%x2(b), F%y2(b))
          if (kk == K_SEG_NONE) cycle
          if (kk == K_SEG_PROPER) then
             call fail('fragments cross'); return
          end if
          if (kk == K_SEG_OVERLAP) then
             call fail('fragments overlap'); return
          end if
          if (interior_pt(a, F%x1(b), F%y1(b)) .or. interior_pt(a, F%x2(b), F%y2(b)) .or. &
              interior_pt(b, F%x1(a), F%y1(a)) .or. interior_pt(b, F%x2(a), F%y2(a))) then
             call fail('fragment endpoint inside another fragment'); return
          end if
       end do
    end do
  end subroutine check_fragments

  logical function interior_pt(a, x, y)
    integer(int64), intent(in) :: a, x, y
    interior_pt = .false.
    if ((x == F%x1(a) .and. y == F%y1(a)) .or. (x == F%x2(a) .and. y == F%y2(a))) return
    if (orient2d(F%x1(a), F%y1(a), F%x2(a), F%y2(a), x, y) /= 0) return
    interior_pt = on_closed_segment(F%x1(a), F%y1(a), F%x2(a), F%y2(a), x, y)
  end function interior_pt

  !> sign of 21*x_line(Y/21) - X for the line through (xa,ya),(xb,yb) (yb /= ya)
  integer function line_cmp(xa, ya, xb, yb, X, Y)
    integer(int64), intent(in) :: xa, ya, xb, yb, X, Y
    integer(int64) :: dx, dy, v
    dx = xb - xa; dy = yb - ya
    if (dy < 0) then
       dx = -dx; dy = -dy
    end if
    v = 21*xa*dy + (Y - 21*ya)*dx - X*dy
    line_cmp = 0
    if (v > 0) line_cmp = 1
    if (v < 0) line_cmp = -1
  end function line_cmp

  !> winding of segment set S at (X/21, Y/21); on = .true. if the point is on a segment
  subroutine winding(S, X, Y, wsum, on)
    type(FragSet), intent(in) :: S
    integer(int64), intent(in) :: X, Y
    integer(int64), intent(out) :: wsum
    logical, intent(out) :: on
    integer(int64) :: a
    integer :: c
    wsum = 0; on = .false.
    do a = 1, S%n
       if (S%y1(a) == S%y2(a)) cycle
       if (21*S%y1(a) >= Y .or. 21*S%y2(a) <= Y) cycle
       c = line_cmp(S%x1(a), S%y1(a), S%x2(a), S%y2(a), X, Y)
       if (c == 0) then
          on = .true.; return
       end if
       if (c < 0) wsum = wsum + S%w(a)
    end do
  end subroutine winding

  real(real64) function dist_edges(X, Y) result(d)
    integer(int64), intent(in) :: X, Y
    real(real64) :: qx, qy, ax, ay, bx, by, t, l2
    integer(int64) :: a
    qx = X/21.0_real64; qy = Y/21.0_real64
    d = huge(d)
    do a = 1, E%n
       ax = E%x1(a); ay = E%y1(a); bx = E%x2(a); by = E%y2(a)
       l2 = (bx - ax)**2 + (by - ay)**2
       t = max(0.0_real64, min(1.0_real64, ((qx - ax)*(bx - ax) + (qy - ay)*(by - ay))/l2))
       d = min(d, sqrt((ax + t*(bx - ax) - qx)**2 + (ay + t*(by - ay) - qy)**2))
    end do
  end function dist_edges

end program test_trapbool
