! File   : test_trapezoid.F90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Unit test of TrapezoidModule.
!   * canonical sides: the same lattice line from any two of its points encodes bit-identically;
!   * exact clipping: x on a re-anchored side equals x on the original at every row;
!   * exact area: random convex lattice polygons are cut into trapezoids at their vertex rows
!     (and again at random rows, which gives rational corners); twice the trapezoid area must
!     equal the shoelace 2A exactly and be flagged integral;
!   * trap_mbr contains the exact corners; trap_valid rejects crossed sides;
!   * device run of trap_clip_y / side_x_cmp compared with the host.
program test_trapezoid
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use WideIntModule
  use TrapezoidModule
  implicit none
  integer :: nfail = 0

  call test_canonical()
  call test_clip_exact()
  call test_area_polygons()
  call test_validity()
  call test_device()
  if (nfail > 0) then
     print '(A,I0,A)', 'FAIL: ', nfail, ' check(s) failed'
     error stop 1
  end if
  print '(A)', 'PASS test_trapezoid'

contains

  subroutine expect(ok, what)
    logical, intent(in) :: ok
    character(*), intent(in) :: what
    if (.not. ok) then
       nfail = nfail + 1
       print '(A)', 'FAIL: '//what
    end if
  end subroutine expect

  integer(int64) function irand(lo, hi)
    integer(int64), intent(in) :: lo, hi
    real(real64) :: u
    call random_number(u)
    irand = lo + min(hi - lo, int(u * real(hi - lo + 1, real64), int64))
  end function irand

  subroutine test_canonical()
    integer :: it, nbad, st, st2
    integer(int64) :: x0, y0, dx, dy, a, b, y1
    type(TrapSide) :: s1, s2
    nbad = 0
    do it = 1, 20000
       x0 = irand(-1000000_int64, 1000000_int64); y0 = irand(-1000000_int64, 1000000_int64)
       dx = irand(-50_int64, 50_int64); dy = irand(1_int64, 40_int64)
       a = irand(-30_int64, 30_int64); b = irand(-30_int64, 30_int64)
       if (a == b) b = a + 1
       y1 = irand(-1000000_int64, 1000000_int64)
       call side_from_points(x0 + a*dx, y0 + a*dy, x0 + b*dx, y0 + b*dy, y1, s1, st)
       call side_from_points(x0 + 3*dx, y0 + 3*dy, x0 - 7*dx, y0 - 7*dy, y1, s2, st2)  ! reversed
       if (st /= K_TRAP_OK .or. st2 /= K_TRAP_OK .or. .not. side_equal(s1, s2)) nbad = nbad + 1
       if (s1%k < 0 .or. s1%k >= s1%dy) nbad = nbad + 1
    end do
    call expect(nbad == 0, 'canonical side encoding')
    call side_from_points(0_int64, 5_int64, 9_int64, 5_int64, 0_int64, s1, st)
    call expect(st == K_TRAP_HORIZONTAL, 'horizontal side rejected')
  end subroutine test_canonical

  subroutine test_clip_exact()
    integer :: it, nbad, st
    integer(int64) :: y, ylo, yhi
    type(Trapezoid) :: t, c
    nbad = 0
    do it = 1, 20000
       call random_trap(t)
       ylo = irand(int(t%y1, int64) - 5, int(t%y2, int64)); yhi = irand(ylo + 1, int(t%y2, int64) + 5)
       call trap_clip_y(t, ylo, yhi, c, st)
       if (st == K_TRAP_EMPTY) cycle
       if (st /= K_TRAP_OK .or. .not. trap_valid(c)) then
          nbad = nbad + 1; cycle
       end if
       do y = c%y1, c%y2
          if (side_x_cmp(c%l, int(c%y1, int64), t%l, int(t%y1, int64), y) /= 0 .or. &
              side_x_cmp(c%r, int(c%y1, int64), t%r, int(t%y1, int64), y) /= 0) nbad = nbad + 1
       end do
    end do
    call expect(nbad == 0, 'clip is exact (x unchanged on every row)')
  end subroutine test_clip_exact

  subroutine random_trap(t)
    type(Trapezoid), intent(out) :: t
    integer(int64) :: y1, y2, xl1, xl2, xr1, xr2
    integer :: st
    do
       y1 = irand(-100000_int64, 100000_int64); y2 = y1 + irand(1_int64, 60_int64)
       xl1 = irand(-1000_int64, 1000_int64); xl2 = irand(-1000_int64, 1000_int64)
       xr1 = xl1 + irand(0_int64, 500_int64); xr2 = xl2 + irand(0_int64, 500_int64)
       ! sides through lattice points below/above the rows, so corners can be rational
       call trap_make(y1, y2, xl1, y1 - irand(0_int64, 7_int64), xl2, y2 + irand(0_int64, 7_int64), &
                      xr1, y1 - irand(0_int64, 7_int64), xr2, y2 + irand(0_int64, 7_int64), t, st)
       if (st == K_TRAP_OK) exit
    end do
  end subroutine random_trap

  !> Convex hull (monotone chain) of random lattice points; returns CCW vertices.
  subroutine random_convex(px, py, nv)
    integer(int64), allocatable, intent(out) :: px(:), py(:)
    integer, intent(out) :: nv
    integer(int64) :: ax(40), ay(40), hx(80), hy(80), tx, ty
    integer :: n, i, j, k
    n = int(irand(3_int64, 40_int64))
    do i = 1, n
       ax(i) = irand(-200_int64, 200_int64); ay(i) = irand(-200_int64, 200_int64)
    end do
    do i = 2, n                                          ! sort by (x, y)
       tx = ax(i); ty = ay(i); j = i - 1
       do while (j >= 1)
          if (ax(j) < tx .or. (ax(j) == tx .and. ay(j) <= ty)) exit
          ax(j+1) = ax(j); ay(j+1) = ay(j); j = j - 1
       end do
       ax(j+1) = tx; ay(j+1) = ty
    end do
    k = 0
    do i = 1, n
       do while (k >= 2)
          if (cross(hx(k-1), hy(k-1), hx(k), hy(k), ax(i), ay(i)) > 0) exit
          k = k - 1
       end do
       k = k + 1; hx(k) = ax(i); hy(k) = ay(i)
    end do
    j = k + 1
    do i = n - 1, 1, -1
       do while (k >= j)
          if (cross(hx(k-1), hy(k-1), hx(k), hy(k), ax(i), ay(i)) > 0) exit
          k = k - 1
       end do
       k = k + 1; hx(k) = ax(i); hy(k) = ay(i)
    end do
    nv = k - 1
    px = hx(1:nv); py = hy(1:nv)
  end subroutine random_convex

  integer(int64) function cross(ax, ay, bx, by, cx, cy)
    integer(int64), intent(in) :: ax, ay, bx, by, cx, cy
    cross = (bx - ax)*(cy - ay) - (by - ay)*(cx - ax)
  end function cross

  subroutine test_area_polygons()
    integer :: it, nv, i, j, e, nbad, ntest, st, ns, nedge
    integer(int64), allocatable :: px(:), py(:)
    integer(int64) :: ys(200), a2ref, ya, yb, ymid2, cut
    integer :: eid(2), jl, jr
    type(Trapezoid) :: traps(400), c
    integer(int64) :: nt
    type(i128) :: a2
    logical :: isint
    nbad = 0; ntest = 0
    do it = 1, 3000
       call random_convex(px, py, nv)
       if (nv < 3) cycle
       a2ref = 0
       do i = 1, nv
          j = mod(i, nv) + 1
          a2ref = a2ref + px(i)*py(j) - px(j)*py(i)
       end do
       if (a2ref <= 0) cycle
       ! slab boundaries: vertex rows plus (half the time) extra random rows
       ns = 0
       do i = 1, nv
          ns = ns + 1; ys(ns) = py(i)
       end do
       if (mod(it, 2) == 0) then
          do i = 1, 5
             ns = ns + 1; ys(ns) = irand(minval(py), maxval(py))
          end do
       end if
       call sort_unique(ys, ns)
       nt = 0
       do i = 1, ns - 1
          ya = ys(i); yb = ys(i+1); ymid2 = ya + yb          ! doubled mid row
          nedge = 0
          do e = 1, nv
             j = mod(e, nv) + 1
             if (min(py(e), py(j)) <= ya .and. max(py(e), py(j)) >= yb .and. py(e) /= py(j)) then
                nedge = nedge + 1
                if (nedge <= 2) eid(nedge) = e
             end if
          end do
          if (nedge /= 2) then
             nbad = nbad + 1; exit
          end if
          if (.not. edge_left(px, py, nv, eid(1), eid(2), ymid2)) eid = [eid(2), eid(1)]
          jl = mod(eid(1), nv) + 1; jr = mod(eid(2), nv) + 1
          nt = nt + 1
          call trap_make(ya, yb, px(eid(1)), py(eid(1)), px(jl), py(jl), &
                         px(eid(2)), py(eid(2)), px(jr), py(jr), traps(nt), st)
          if (st /= K_TRAP_OK) then
             nbad = nbad + 1; nt = nt - 1
          end if
       end do
       call trap_area2(traps, nt, a2, isint)
       ntest = ntest + 1
       if (.not. isint .or. i128_cmp(a2, i128_from_int64(a2ref)) /= 0) then
          nbad = nbad + 1
          if (nbad <= 3) print '(A,I0,A,F20.1,L2)', '  area mismatch: ref 2A=', a2ref, ' got ', &
               i128_to_real64(a2), isint
       end if
       ! split one trapezoid at an interior row: area unchanged
       if (nt >= 1) then
          if (traps(1)%y2 - traps(1)%y1 >= 2) then
             cut = traps(1)%y1 + 1
             call trap_clip_y(traps(1), int(traps(1)%y1, int64), cut, c, st)
             nt = nt + 1
             call trap_clip_y(traps(1), cut, int(traps(1)%y2, int64), traps(nt), st)
             traps(1) = c
             call trap_area2(traps, nt, a2, isint)
             if (.not. isint .or. i128_cmp(a2, i128_from_int64(a2ref)) /= 0) nbad = nbad + 1
          end if
       end if
    end do
    call expect(nbad == 0, 'exact area of trapezoid decompositions')
    print '(A,I0,A,I0)', 'INFO: polygons decomposed ', ntest, ' area mismatches ', nbad
  end subroutine test_area_polygons

  !> Is edge a left of edge b at the doubled row y2m? Compare x*2*dy exactly in int64.
  logical function edge_left(px, py, nv, a, b, y2m)
    integer(int64), intent(in) :: px(:), py(:), y2m
    integer, intent(in) :: nv, a, b
    integer :: ja, jb
    integer(int64) :: na, da, nb, db
    ja = mod(a, nv) + 1; jb = mod(b, nv) + 1
    ! x(y) = x0 + (y - y0) * (x1 - x0)/(y1 - y0); doubled: 2x = 2x0 + (2y - 2y0)(x1-x0)/(y1-y0)
    da = py(ja) - py(a); na = 2*px(a)*da + (y2m - 2*py(a))*(px(ja) - px(a))
    db = py(jb) - py(b); nb = 2*px(b)*db + (y2m - 2*py(b))*(px(jb) - px(b))
    if (da < 0) then
       na = -na; da = -da
    end if
    if (db < 0) then
       nb = -nb; db = -db
    end if
    edge_left = na*db < nb*da
  end function edge_left

  subroutine sort_unique(v, n)
    integer(int64), intent(inout) :: v(:)
    integer, intent(inout) :: n
    integer :: i, j, m
    integer(int64) :: t
    do i = 2, n
       t = v(i); j = i - 1
       do while (j >= 1)
          if (v(j) <= t) exit
          v(j+1) = v(j); j = j - 1
       end do
       v(j+1) = t
    end do
    m = 1
    do i = 2, n
       if (v(i) /= v(m)) then
          m = m + 1; v(m) = v(i)
       end if
    end do
    n = m
  end subroutine sort_unique

  subroutine test_validity()
    type(Trapezoid) :: t
    integer :: st, it, nbad
    integer(int64) :: x1, y1, x2, y2
    ! crossed sides: left goes from 0 to 10, right from 10 to 0 over rows 0..10
    call trap_make(0_int64, 10_int64, 0_int64, 0_int64, 10_int64, 10_int64, &
                   10_int64, 0_int64, 0_int64, 10_int64, t, st)
    call expect(st == K_TRAP_EMPTY, 'crossed sides rejected')
    ! a triangle (sides meet at the top row) is valid
    call trap_make(0_int64, 10_int64, 0_int64, 0_int64, 5_int64, 10_int64, &
                   10_int64, 0_int64, 5_int64, 10_int64, t, st)
    call expect(st == K_TRAP_OK .and. trap_valid(t), 'triangle valid')
    t = trap_from_box_coords(1_int32, 2_int32, 7_int32, 9_int32)
    call expect(trap_is_box(t) .and. trap_valid(t), 'box as trapezoid')
    nbad = 0
    do it = 1, 5000
       call random_trap(t)
       call trap_mbr(t, x1, y1, x2, y2)
       ! the exact corners lie inside the MBR: x1*dy <= xnum and xnum <= x2*dy
       if (i128_cmp(i128_mul64(x1, int(t%l%dy, int64)), side_x_num(t%l, y1, y1)) > 0 .or. &
           i128_cmp(i128_mul64(x1, int(t%l%dy, int64)), side_x_num(t%l, y1, y2)) > 0 .or. &
           i128_cmp(i128_mul64(x2, int(t%r%dy, int64)), side_x_num(t%r, y1, y1)) < 0 .or. &
           i128_cmp(i128_mul64(x2, int(t%r%dy, int64)), side_x_num(t%r, y1, y2)) < 0) nbad = nbad + 1
    end do
    call expect(nbad == 0, 'trap_mbr contains the exact corners')
  end subroutine test_validity

  subroutine test_device()
    integer, parameter :: n = 8192
    type(Trapezoid) :: t(n), hc(n), dc(n)
    integer(int64) :: lo(n), hi(n)
    integer :: hs(n), ds(n), i
    integer(int32) :: hx(n), dx(n)
    do i = 1, n
       call random_trap(t(i))
       lo(i) = irand(int(t(i)%y1, int64) - 3, int(t(i)%y2, int64)); hi(i) = lo(i) + irand(1_int64, 40_int64)
    end do
    do i = 1, n
       call trap_clip_y(t(i), lo(i), hi(i), hc(i), hs(i))
       hx(i) = side_x_cmp(t(i)%l, int(t(i)%y1, int64), t(i)%r, int(t(i)%y1, int64), lo(i))
    end do
    !$omp target teams distribute parallel do map(to: t, lo, hi) map(from: dc, ds, dx)
    do i = 1, n
       call trap_clip_y(t(i), lo(i), hi(i), dc(i), ds(i))
       dx(i) = side_x_cmp(t(i)%l, int(t(i)%y1, int64), t(i)%r, int(t(i)%y1, int64), lo(i))
    end do
    call expect(all(hs == ds) .and. all(trap_equal(hc, dc) .or. hs == K_TRAP_EMPTY) .and. all(hx == dx), &
                'target region trapezoid ops equal host')
  end subroutine test_device

end program test_trapezoid
