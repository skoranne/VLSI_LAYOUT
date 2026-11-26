! File   : measure.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: rotation-invariant measurements of a merged (healed) layer: polygon count, holes,
!          vertices, exact area and perimeter. CTR: `exec measure d1:LAYER [d1:LAYER ...]`.
!
! Input: pairwise interior-disjoint boxes (a healed layer). The region R is their union taken
! as a closed set, so shapes touching at a corner belong to one polygon, the KLayout merge
! convention (min_coherence = false).
!
!   area       sum of box areas (exact, int64)
!   perimeter  sum of box perimeters minus twice the length shared by two boxes; shared length
!              is found per vertical (horizontal) line by overlapping the edges of boxes on
!              either side of it (exact, int64 dbu)
!   polygons   connected components: union-find over boxes whose closed edges overlap on a
!              common line (edge or corner contact)
!   vertices   every boundary vertex is a box corner. At a corner point p the four quadrants
!              NE NW SW SE are covered by the boxes having p as a corner or holding p in the
!              open interior of one of their edges (T junctions; found on the same line sweeps).
!              One or three covered quadrants: one vertex (convex/reflex). Two opposite
!              quadrants: the boundary passes p twice, two vertices.
!   holes      polygons - chi, with the Euler characteristic chi = (convex - reflex - 2*diagonal)/4
!              of the closed region (a diagonal point joins its two quadrants).
!
! All work is sorts of 64-bit keys (CompactSortPairs: Thrust or std::sort) and linear sweeps:
! O(n log n) time, about 60 B per box of scratch.
module LayerMeasureModule
  use iso_fortran_env, only: int32, int64
  use GeometryModule, only: Box
  use CompactLayerModule, only: CompactSortPairs
  implicit none
  private
  public :: LayerMeasures, MeasureDisjointBoxes, MeasuresToString

  type :: LayerMeasures
     integer(int64) :: boxes = 0, polygons = 0, holes = 0, vertices = 0
     integer(int64) :: area = 0, perimeter = 0
     integer(int64) :: euler4 = 0     !> 4 * Euler characteristic (sanity: divisible by 4)
  end type LayerMeasures

  !> scratch of one measurement: union-find over boxes, point records (key, quadrant mask)
  type :: MeasureState
     integer(int32), allocatable :: parent(:), pmask(:)
     integer(int64), allocatable :: pk(:)
     integer(int64) :: np = 0, shared = 0
  end type MeasureState

  integer(int32), parameter :: Q_NE = 1, Q_NW = 2, Q_SW = 4, Q_SE = 8
  integer(int64), parameter :: BIAS = 2147483648_int64          !> 2^31: signed int32 -> unsigned

contains

  pure integer(int64) function pkey( a, b )
    integer(int64), intent(in) :: a, b
    pkey = ior( shiftl( a + BIAS, 32 ), b + BIAS )
  end function pkey

  pure function MeasuresToString( m ) result(s)
    type(LayerMeasures), intent(in) :: m
    character(len=:), allocatable :: s
    character(len=256) :: buf
    write(buf,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)') 'boxes=', m%boxes, ' polygons=', m%polygons, &
         ' holes=', m%holes, ' vertices=', m%vertices, ' area=', m%area, ' perimeter=', m%perimeter
    s = trim(buf)
  end function MeasuresToString

  !> Measurements of the union of n pairwise interior-disjoint boxes.
  subroutine MeasureDisjointBoxes( b, n, m )
    type(Box), intent(in)            :: b(:)
    integer(int64), intent(in)       :: n
    type(LayerMeasures), intent(out) :: m
    type(MeasureState) :: st
    integer(int64) :: i, j, c4, vert, ncomp
    integer(int32) :: mk

    m%boxes = n
    if( n <= 0 ) return
    if( n >= int(huge(0_int32), int64) ) error stop "MeasureDisjointBoxes: more than 2^31 boxes"
    allocate( st%parent(n) )
    do i = 1, n
       st%parent(i) = int(i, int32)
    end do

    !> corner records: the box covers one quadrant of each of its corners
    allocate( st%pk(4*n), st%pmask(4*n) )
    st%np = 0
    do i = 1, n
       associate( q => b(i) )
         if( q%x2 <= q%x1 .or. q%y2 <= q%y1 ) error stop "MeasureDisjointBoxes: invalid box"
         m%area = m%area + int(q%x2 - q%x1, int64) * int(q%y2 - q%y1, int64)
         m%perimeter = m%perimeter + 2_int64 * ( int(q%x2 - q%x1, int64) + int(q%y2 - q%y1, int64) )
         call addp( st, int(q%x1,int64), int(q%y1,int64), Q_NE )
         call addp( st, int(q%x2,int64), int(q%y1,int64), Q_NW )
         call addp( st, int(q%x2,int64), int(q%y2,int64), Q_SW )
         call addp( st, int(q%x1,int64), int(q%y2,int64), Q_SE )
       end associate
    end do

    st%shared = 0
    call line_pass( st, b, n, .true. )      !> vertical lines x = const (left/right edges)
    call line_pass( st, b, n, .false. )     !> horizontal lines y = const (bottom/top edges)
    m%perimeter = m%perimeter - 2_int64 * st%shared

    !> group the point records by location and classify
    call CompactSortPairs( st%pk, st%pmask, st%np )
    c4 = 0; vert = 0
    i = 1
    do while( i <= st%np )
       mk = st%pmask(i)
       j = i + 1
       do while( j <= st%np )
          if( st%pk(j) /= st%pk(i) ) exit
          mk = ior( mk, st%pmask(j) )
          j = j + 1
       end do
       select case( popcnt(mk) )
       case( 1 )
          vert = vert + 1; c4 = c4 + 1
       case( 3 )
          vert = vert + 1; c4 = c4 - 1
       case( 2 )
          if( mk == ior(Q_NE, Q_SW) .or. mk == ior(Q_NW, Q_SE) ) then
             vert = vert + 2; c4 = c4 - 2
          end if
       end select
       i = j
    end do
    ncomp = 0
    do i = 1, n
       if( find(st, int(i,int32)) == int(i,int32) ) ncomp = ncomp + 1
    end do
    m%vertices = vert
    m%polygons = ncomp
    m%euler4   = c4
    if( modulo(c4, 4_int64) /= 0 ) write(*,'(A,I0)') 'WARNING: measure: 4*chi not divisible by 4: ', c4
    m%holes = ncomp - c4 / 4
  end subroutine MeasureDisjointBoxes

  subroutine addp( st, x, y, q )
    type(MeasureState), intent(inout) :: st
    integer(int64), intent(in) :: x, y
    integer(int32), intent(in) :: q
    integer(int64), allocatable :: tk(:)
    integer(int32), allocatable :: tm(:)
    if( st%np >= size(st%pk, kind=int64) ) then
       allocate( tk(2*size(st%pk, kind=int64)), tm(2*size(st%pk, kind=int64)) )
       tk(1:st%np) = st%pk(1:st%np); tm(1:st%np) = st%pmask(1:st%np)
       call move_alloc( tk, st%pk ); call move_alloc( tm, st%pmask )
    end if
    st%np = st%np + 1
    st%pk(st%np) = pkey( x, y ); st%pmask(st%np) = q
  end subroutine addp

  integer(int32) function find( st, x ) result(r)
    type(MeasureState), intent(inout) :: st
    integer(int32), intent(in) :: x
    integer(int32) :: y, t
    r = x
    do while( st%parent(r) /= r )
       r = st%parent(r)
    end do
    y = x
    do while( st%parent(y) /= r )               !> path compression
       t = st%parent(y); st%parent(y) = r; y = t
    end do
  end function find

  subroutine unite( st, a, c )
    type(MeasureState), intent(inout) :: st
    integer(int32), intent(in) :: a, c
    integer(int32) :: ra, rc
    ra = find(st, a); rc = find(st, c)
    if( ra /= rc ) st%parent( max(ra, rc) ) = min(ra, rc)
  end subroutine unite

  subroutine push( alo, ahi, aid, cnt, lo_, hi_, id_ )
    integer(int64), allocatable, intent(inout) :: alo(:), ahi(:)
    integer(int32), allocatable, intent(inout) :: aid(:)
    integer(int64), intent(inout) :: cnt
    integer(int64), intent(in) :: lo_, hi_
    integer(int32), intent(in) :: id_
    integer(int64), allocatable :: t1(:), t2(:)
    integer(int32), allocatable :: t3(:)
    if( cnt >= size(alo, kind=int64) ) then
       allocate( t1(2*cnt), t2(2*cnt), t3(2*cnt) )
       t1(1:cnt) = alo(1:cnt); t2(1:cnt) = ahi(1:cnt); t3(1:cnt) = aid(1:cnt)
       call move_alloc( t1, alo ); call move_alloc( t2, ahi ); call move_alloc( t3, aid )
    end if
    cnt = cnt + 1
    alo(cnt) = lo_; ahi(cnt) = hi_; aid(cnt) = id_
  end subroutine push

  !> One family of parallel lines. Edge e of box i: e = 2i-1 is the low edge (x1 or y1, the
  !> box lies on the + side of the line), e = 2i the high edge (box on the - side).
  !> Keys (line, lo) sort the edges by line and along it; per line the + and - edges are
  !> each interior-disjoint and ordered, so two-pointer walks give the shared length, the
  !> contacts (union-find) and the T junctions (an endpoint strictly inside an edge of the
  !> other side: the edge's box covers the two quadrants on its side of the line).
  subroutine line_pass( st, b, n, vertical )
    type(MeasureState), intent(inout) :: st
    type(Box), intent(in) :: b(:)
    integer(int64), intent(in) :: n
    logical, intent(in) :: vertical
    integer(int64), allocatable :: ek(:), plo(:), phi(:), mlo(:), mhi(:)
    integer(int32), allocatable :: ev(:), pid(:), mid(:)
    integer(int64) :: i, e, g, h, line, np_, nm_, k, l, lo, hi, nkey
    integer(int32) :: bi, qplus, qminus
    allocate( ek(2*n), ev(2*n) )
    do i = 1, n
       associate( q => b(i) )
         if( vertical ) then
            ek(2*i-1) = pkey( int(q%x1,int64), int(q%y1,int64) )
            ek(2*i)   = pkey( int(q%x2,int64), int(q%y1,int64) )
         else
            ek(2*i-1) = pkey( int(q%y1,int64), int(q%x1,int64) )
            ek(2*i)   = pkey( int(q%y2,int64), int(q%x1,int64) )
         end if
       end associate
       ev(2*i-1) = int(2*i-1, int32); ev(2*i) = int(2*i, int32)
    end do
    nkey = 2*n
    call CompactSortPairs( ek, ev, nkey )
    if( vertical ) then
       qplus = ior(Q_NE, Q_SE); qminus = ior(Q_NW, Q_SW)
    else
       qplus = ior(Q_NE, Q_NW); qminus = ior(Q_SW, Q_SE)
    end if
    allocate( plo(16), phi(16), pid(16), mlo(16), mhi(16), mid(16) )
    g = 1
    do while( g <= nkey )
       line = shiftr( ek(g), 32 )
       h = g
       do while( h + 1 <= nkey )
          if( shiftr( ek(h+1), 32 ) /= line ) exit
          h = h + 1
       end do
       np_ = 0; nm_ = 0
       do e = g, h
          bi = (ev(e) + 1) / 2
          if( vertical ) then
             lo = b(bi)%y1; hi = b(bi)%y2
          else
             lo = b(bi)%x1; hi = b(bi)%x2
          end if
          if( mod(ev(e), 2) == 1 ) then
             call push( plo, phi, pid, np_, lo, hi, bi )
          else
             call push( mlo, mhi, mid, nm_, lo, hi, bi )
          end if
       end do
       line = line - BIAS
       if( np_ > 0 .and. nm_ > 0 ) then
          !> shared length and contacts
          k = 1; l = 1
          do while( k <= np_ .and. l <= nm_ )
             if( max(plo(k), mlo(l)) <= min(phi(k), mhi(l)) ) then
                st%shared = st%shared + max( 0_int64, min(phi(k), mhi(l)) - max(plo(k), mlo(l)) )
                call unite( st, pid(k), mid(l) )
             end if
             if( phi(k) < mhi(l) ) then
                k = k + 1
             else if( mhi(l) < phi(k) ) then
                l = l + 1
             else
                k = k + 1; l = l + 1
             end if
          end do
          call tjunctions( st, vertical, line, mlo, mhi, nm_, plo, phi, np_, qplus )  !> - ends inside + edges
          call tjunctions( st, vertical, line, plo, phi, np_, mlo, mhi, nm_, qminus ) !> + ends inside - edges
       end if
       g = h + 1
    end do
  end subroutine line_pass

  !> Endpoints of the edges (slo,shi) on `line` lying strictly inside an edge of (tlo,thi):
  !> the point gets the quadrants qbits of the target edge's box. Both lists sorted, disjoint.
  subroutine tjunctions( st, vertical, line, slo, shi, ns, tlo, thi, nt, qbits )
    type(MeasureState), intent(inout) :: st
    logical, intent(in) :: vertical
    integer(int64), intent(in) :: line, slo(:), shi(:), ns, tlo(:), thi(:), nt
    integer(int32), intent(in) :: qbits
    integer(int64) :: s, t, side, y
    t = 1
    do s = 1, ns
       do side = 0, 1
          y = merge( slo(s), shi(s), side == 0 )
          do while( t <= nt )
             if( thi(t) > y ) exit
             t = t + 1
          end do
          if( t > nt ) return
          if( tlo(t) < y ) then
             if( vertical ) then
                call addp( st, line, y, qbits )
             else
                call addp( st, y, line, qbits )
             end if
          end if
       end do
    end do
  end subroutine tjunctions

end module LayerMeasureModule
