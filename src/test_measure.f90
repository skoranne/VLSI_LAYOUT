! test_measure: LayerMeasureModule against a pixel-grid reference.
! Random pixel sets on a small grid are cut into disjoint boxes three ways (single pixels,
! row runs, column runs: many T and 4-way junctions). The reference counts on the pixels:
!   area       filled pixels
!   perimeter  pixel edges between a filled and an empty pixel
!   polygons   8-connected components of the filled pixels (closed set: corner contact joins)
!   holes      4-connected components of the empty pixels not reaching the border
!   vertices   lattice points with 1 or 3 filled pixels around them, 2 for a diagonal pair
program test_measure
  use iso_fortran_env, only: int32, int64
  use GeometryModule, only: Box
  use LayerMeasureModule
  implicit none
  integer, parameter :: G = 14
  integer :: trial, mode, nfail, ntrial
  logical :: px(0:G+1, 0:G+1)
  type(Box), allocatable :: bx(:)
  integer(int64) :: nb
  type(LayerMeasures) :: m
  integer(int64) :: r_area, r_per, r_poly, r_holes, r_vert
  real :: u, dens

  nfail = 0; ntrial = 0
  call random_seed()
  do trial = 1, 3000
     call random_number(dens); dens = 0.2 + 0.6*dens
     call fill(dens)
     call reference()
     do mode = 1, 3
        call decompose(mode)
        call MeasureDisjointBoxes(bx, nb, m)
        ntrial = ntrial + 1
        if (m%area /= r_area .or. m%perimeter /= r_per .or. m%polygons /= r_poly .or. &
            m%holes /= r_holes .or. m%vertices /= r_vert) then
           nfail = nfail + 1
           if (nfail <= 5) then
              write(*,'(A,I0,A,I0)') 'MISMATCH trial ', trial, ' mode ', mode
              write(*,'(A,5I8)') '  ref  area per poly holes vert', r_area, r_per, r_poly, r_holes, r_vert
              write(*,'(A,5I8)') '  got  area per poly holes vert', m%area, m%perimeter, m%polygons, m%holes, m%vertices
           end if
        end if
     end do
  end do
  ! a ring closed only through corner contacts: one polygon with one hole
  px = .false.
  px(2,1) = .true.; px(1,2) = .true.; px(3,2) = .true.; px(2,3) = .true.
  call reference(); call decompose(1); call MeasureDisjointBoxes(bx, nb, m)
  ntrial = ntrial + 1
  if (m%polygons /= 1 .or. m%holes /= 1 .or. r_holes /= 1 .or. m%vertices /= r_vert) then
     nfail = nfail + 1
     write(*,'(A,3I6)') 'MISMATCH corner ring: polygons holes vertices ', m%polygons, m%holes, m%vertices
  end if
  write(*,'(A,I0,A,I0)') 'INFO: cases ', ntrial, ' mismatches ', nfail
  if (nfail /= 0) error stop 'FAIL test_measure'
  write(*,'(A)') 'PASS test_measure'

contains

  subroutine fill(d)
    real, intent(in) :: d
    integer :: i, j
    px = .false.
    do j = 1, G
       do i = 1, G
          call random_number(u)
          px(i,j) = u < d
       end do
    end do
  end subroutine fill

  subroutine add(x1, y1, x2, y2)
    integer, intent(in) :: x1, y1, x2, y2
    type(Box), allocatable :: t(:)
    if (nb >= size(bx, kind=int64)) then
       allocate(t(2*size(bx))); t(1:nb) = bx(1:nb); call move_alloc(t, bx)
    end if
    nb = nb + 1
    bx(nb)%x1 = x1; bx(nb)%y1 = y1; bx(nb)%x2 = x2; bx(nb)%y2 = y2
  end subroutine add

  subroutine decompose(md)
    integer, intent(in) :: md
    integer :: i, j, k
    if (allocated(bx)) deallocate(bx)
    allocate(bx(64)); nb = 0
    select case (md)
    case (1)
       do j = 1, G
          do i = 1, G
             if (px(i,j)) call add(i, j, i+1, j+1)
          end do
       end do
    case (2)
       do j = 1, G
          i = 1
          do while (i <= G)
             if (px(i,j)) then
                k = i
                do while (k + 1 <= G)
                   if (.not. px(k+1,j)) exit
                   k = k + 1
                end do
                call add(i, j, k+1, j+1); i = k + 1
             else
                i = i + 1
             end if
          end do
       end do
    case (3)
       do i = 1, G
          j = 1
          do while (j <= G)
             if (px(i,j)) then
                k = j
                do while (k + 1 <= G)
                   if (.not. px(i,k+1)) exit
                   k = k + 1
                end do
                call add(i, j, i+1, k+1); j = k + 1
             else
                j = j + 1
             end if
          end do
       end do
    end select
  end subroutine decompose

  subroutine reference()
    integer :: lab(0:G+1, 0:G+1), i, j, n, q
    logical :: border
    r_area = count(px)
    r_per = 0
    do j = 0, G
       do i = 0, G
          if (px(i,j) .neqv. px(i+1,j)) r_per = r_per + 1
          if (px(i,j) .neqv. px(i,j+1)) r_per = r_per + 1
       end do
    end do
    ! filled components, 8-connected
    lab = 0; r_poly = 0
    do j = 1, G
       do i = 1, G
          if (px(i,j) .and. lab(i,j) == 0) then
             r_poly = r_poly + 1
             call flood(i, j, .true., 8, int(r_poly), lab, border)
          end if
       end do
    end do
    ! empty components, 4-connected, not touching the border ring
    lab = 0; r_holes = 0; n = 0
    do j = 0, G+1
       do i = 0, G+1
          if (.not. px(i,j) .and. lab(i,j) == 0) then
             n = n + 1
             call flood(i, j, .false., 4, n, lab, border)
             if (.not. border) r_holes = r_holes + 1
          end if
       end do
    end do
    ! vertices at lattice points (i,j): pixels (i-1,j-1) SW, (i,j-1) SE, (i-1,j) NW, (i,j) NE
    r_vert = 0
    do j = 1, G+1
       do i = 1, G+1
          q = count([px(i,j), px(i-1,j), px(i-1,j-1), px(i,j-1)])
          if (q == 1 .or. q == 3) r_vert = r_vert + 1
          if (q == 2 .and. (px(i,j) .eqv. px(i-1,j-1))) r_vert = r_vert + 2
       end do
    end do
  end subroutine reference

  subroutine flood(i0, j0, val, conn, id, lab, border)
    integer, intent(in) :: i0, j0, conn, id
    logical, intent(in) :: val
    integer, intent(inout) :: lab(0:G+1, 0:G+1)
    logical, intent(out) :: border
    integer :: st(2, (G+2)*(G+2)*8), sp, i, j, di, dj
    border = .false.
    sp = 1; st(:,1) = [i0, j0]; lab(i0,j0) = id
    do while (sp > 0)
       i = st(1,sp); j = st(2,sp); sp = sp - 1
       if (i == 0 .or. j == 0 .or. i == G+1 .or. j == G+1) border = .true.
       do dj = -1, 1
          do di = -1, 1
             if (di == 0 .and. dj == 0) cycle
             if (conn == 4 .and. di /= 0 .and. dj /= 0) cycle
             if (i+di < 0 .or. j+dj < 0 .or. i+di > G+1 .or. j+dj > G+1) cycle
             if ((px(i+di,j+dj) .eqv. val) .and. lab(i+di,j+dj) == 0) then
                lab(i+di,j+dj) = id
                sp = sp + 1; st(:,sp) = [i+di, j+dj]
             end if
          end do
       end do
    end do
  end subroutine flood
end program test_measure
