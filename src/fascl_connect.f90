! File   : fascl_connect.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: CTR CONNECT statement: nets across a routing/cut/routing layer stack, stored per
!          polygon (FASCL NET_ID).
!
!   CONNECT d1:li1 d1:mcon d1:met1 d1:via d1:met2 ...
!
! Odd positions are routing layers, even positions the cut layers between them. A cut polygon
! connects the routing polygons below and above it that it overlaps with positive area
! (touching edges or corners do not connect); within a layer, a polygon is a PNum component.
! Nets are global over the whole stack and numbered canonically 1..N in order of first
! appearance over (stack position, polygon ordinal): net 1 holds polygon #1 of the first layer.
! The result lives in each member layer's property table (props%net, props%net_rec); a layer
! belongs to at most one live CONNECT, a later CONNECT that lists it supersedes the earlier one.
module FASCLConnectModule
  use iso_fortran_env, only: int32, int64, real64
  use DesignModule
  use GeometryModule, only: Box
  use PropertyTableModule
  use FASCLModule, only: LayerEnsureProperties
  use TechStackModule, only: TechDefNames, current_tech
  use CommonModule, only: GetPrecision
  use iso_c_binding, only: c_char, c_int32_t, c_int64_t, c_null_char
  use TrapezoidModule, only: Trapezoid, trap_from_box_coords, trap_mbr, trap_overlap_positive
  implicit none
  private
  !> Pieces of one CONNECT member for the any-angle path: trapezoids (boxes converted) with
  !> their polygon ordinals and integer bounding boxes.
  type :: PieceSet
     integer(int64) :: n = 0
     type(Trapezoid), allocatable :: t(:)
     integer(int64), allocatable :: ord(:), x1(:), y1(:), x2(:), y2(:)
  end type PieceSet

  type, public :: LayerPtr
     type(Layer), pointer :: p => null()
  end type LayerPtr

  type, public :: ConnectRecord
     type(PropString), allocatable :: specs(:)
     logical :: live = .false.
     integer(int64) :: n_nets = 0, n_contacts = 0, n_polygons = 0
     character(len=16) :: tech = ''                    !> built-in stack (CONNECT STACK / var tech)
     type(PropString), allocatable :: defn(:)          !> DEF layer name per member
  end type ConnectRecord

  type(ConnectRecord), allocatable, public, save :: connect_records(:)
  integer, public, save :: n_connect_records = 0

  integer, parameter :: K_STACK = 512

  public :: ConnectNew, ConnectCompute, NetInfoPrint, ConnectValidate, DefCheck

  !> db/def_probe.cpp
  interface
     integer(c_int32_t) function fdef_open( path, layers, lefs, nprobe, nnet, nskip, units ) bind(C, name='fdef_open')
       import :: c_char, c_int32_t, c_int64_t
       character(kind=c_char), intent(in) :: path(*), layers(*), lefs(*)
       integer(c_int64_t), intent(out) :: nprobe, nnet, nskip
       integer(c_int32_t), intent(out) :: units
     end function fdef_open
     integer(c_int32_t) function fdef_probes( h, layer, x, y, net, kind ) bind(C, name='fdef_probes')
       import :: c_int32_t, c_int64_t
       integer(c_int32_t), value :: h
       integer(c_int32_t), intent(out) :: layer(*), kind(*)
       integer(c_int64_t), intent(out) :: x(*), y(*), net(*)
     end function fdef_probes
     integer(c_int32_t) function fdef_check( h, nid, n_ext, prefix, layer_names, stats ) bind(C, name='fdef_check')
       import :: c_char, c_int32_t, c_int64_t
       integer(c_int32_t), value :: h
       integer(c_int64_t), intent(in) :: nid(*)
       integer(c_int64_t), value :: n_ext
       character(kind=c_char), intent(in) :: prefix(*), layer_names(*)
       integer(c_int64_t), intent(out) :: stats(16)
     end function fdef_check
     integer(c_int32_t) function fdef_label( h, e, buf, buflen ) bind(C, name='fdef_label')
       import :: c_char, c_int32_t, c_int64_t
       integer(c_int32_t), value :: h, buflen
       integer(c_int64_t), value :: e
       character(kind=c_char), intent(out) :: buf(*)
     end function fdef_label
     integer(c_int32_t) function fdef_report( h, buf, buflen ) bind(C, name='fdef_report')
       import :: c_char, c_int32_t
       integer(c_int32_t), value :: h, buflen
       character(kind=c_char), intent(out) :: buf(*)
     end function fdef_report
     integer(c_int32_t) function fdef_error( buf, buflen ) bind(C, name='fdef_error')
       import :: c_char, c_int32_t
       integer(c_int32_t), value :: buflen
       character(kind=c_char), intent(out) :: buf(*)
     end function fdef_error
     subroutine fdef_close( h ) bind(C, name='fdef_close')
       import :: c_int32_t
       integer(c_int32_t), value :: h
     end subroutine fdef_close
  end interface

contains

  subroutine copy_record( a, b )
    type(ConnectRecord), intent(in) :: a
    type(ConnectRecord), intent(inout) :: b
    b%live = a%live; b%n_nets = a%n_nets; b%n_contacts = a%n_contacts
    b%n_polygons = a%n_polygons; b%tech = a%tech
    if( allocated( a%specs ) ) call PropStringsAssign( b%specs, a%specs )
    if( allocated( a%defn ) )  call PropStringsAssign( b%defn, a%defn )
  end subroutine copy_record

  !> Check the operand list: routing cut routing [cut routing ...], distinct layers.
  subroutine ConnectValidate( specs, Ls, status, msg )
    type(PropString), intent(in) :: specs(:)
    type(LayerPtr), intent(in) :: Ls(:)
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    integer :: i, j, n
    status = 0; msg = ''
    n = size(specs)
    if( n < 3 .or. mod(n, 2) == 0 ) then
       status = 1
       msg = 'CONNECT: expected routing cut routing [cut routing ...] (an odd number >= 3 of db:layer)'
       return
    end if
    do i = 1, n
       if( .not. associated( Ls(i)%p ) ) then
          status = 1; msg = 'CONNECT: '//specs(i)%s//' is nothing; every operand must be a layer'; return
       end if
       do j = 1, i - 1
          if( associated( Ls(i)%p, Ls(j)%p ) ) then
             status = 1; msg = 'CONNECT: layer '//specs(i)%s//' listed twice'; return
          end if
       end do
    end do
  end subroutine ConnectValidate

  !> New CONNECT record over the given layers; computes its nets.
  subroutine ConnectNew( specs, Ls, rec, status, msg )
    type(PropString), intent(in) :: specs(:)
    type(LayerPtr), intent(inout) :: Ls(:)
    integer, intent(out) :: rec, status
    character(len=:), allocatable, intent(out) :: msg
    type(ConnectRecord), allocatable :: tmp(:)
    integer :: i, old
    rec = 0
    call ConnectValidate( specs, Ls, status, msg )
    if( status /= 0 ) return
    if( .not. allocated( connect_records ) ) allocate( connect_records(4) )
    if( n_connect_records == size( connect_records ) ) then
       allocate( tmp( 2*n_connect_records ) )
       do i = 1, n_connect_records                 ! element-wise (PropString arrays inside)
          call copy_record( connect_records(i), tmp(i) )
       end do
       call move_alloc( tmp, connect_records )
    end if
    n_connect_records = n_connect_records + 1
    rec = n_connect_records
    call PropStringsAssign( connect_records(rec)%specs, specs )
    connect_records(rec)%live = .true.
    do i = 1, size(Ls)
       if( .not. allocated( Ls(i)%p%props ) ) cycle
       old = Ls(i)%p%props%net_rec
       if( old > 0 .and. old /= rec ) then
          if( connect_records(old)%live ) then
             connect_records(old)%live = .false.
             write(*,'(A,I0,A,A,A,I0,A)') 'WARNING: CONNECT #', old, ' superseded (layer ', specs(i)%s, &
                  ' re-connected in CONNECT #', rec, ')'
          end if
       end if
    end do
    call ConnectCompute( rec, Ls, status, msg )
  end subroutine ConnectNew

  !> (Re)compute the nets of record `rec` over its layers Ls (same order as its specs).
  subroutine ConnectCompute( rec, Ls, status, msg )
    integer, intent(in) :: rec
    type(LayerPtr), intent(inout) :: Ls(:)
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    integer :: n, k, c, r, iter
    integer(int64), allocatable :: off(:), par(:), lab(:)
    integer(int64) :: total, ord, root, next, ncontacts
    real(real64) :: t0, t1
    logical :: aa
    type(PieceSet), allocatable :: P(:)

    status = 0; msg = ''
    call cpu_time( t0 )
    n = size(Ls)
    call PropNetNamesDrop( rec )            !> net numbers change: DEF names are stale
    !> Every member: healed, PNum'd, indexed, with an R-tree that matches the box order.
    !> Any member any-angle: every member goes through the exact trapezoid path (boxes become
    !> trapezoids); an all-Manhattan stack keeps the box / R-tree path below.
    aa = .false.
    do k = 1, n
       aa = aa .or. LayerIsAnyAngle( Ls(k)%p )
    end do
    block                                   !> MAGPARSER_FORCE_TRAP=1: trapezoid path (testing)
      character(len=8) :: ev
      integer :: ln, st
      call get_environment_variable( 'MAGPARSER_FORCE_TRAP', ev, ln, st )
      if( st == 0 .and. ln > 0 ) aa = aa .or. ev(1:1) /= '0'
    end block
    do k = 1, n
       if( aa ) then
          call LayerEnsureProperties( Ls(k)%p, connect_records(rec)%specs(k)%s )
          cycle
       end if
       associate( L => Ls(k)%p )
         call LayerEnsureProperties( L, connect_records(rec)%specs(k)%s )   !> expands, heals, PNums
         !> BuildTree may re-sort the boxes: polygon numbers and the index are rebuilt after it.
         do iter = 1, 3
            if( L%n_used == 0 .or. .not. NeedsRTree( L ) ) exit
            call L%unmark( LAYER_STATE_PNUM )
            call BuildTree( L )
            call LayerEnsureProperties( L, connect_records(rec)%specs(k)%s )
         end do
         if( L%n_used > 0 ) then
            if( NeedsRTree( L ) .or. .not. allocated( L%tree%tree_nodes ) ) then
               status = 1; msg = 'CONNECT: no spatial index for '//connect_records(rec)%specs(k)%s; return
            end if
         end if
         if( size( L%props%box_ord, kind=int64 ) /= L%n_used ) then
            status = 1; msg = 'CONNECT: polygon index out of date for '//connect_records(rec)%specs(k)%s; return
         end if
       end associate
    end do

    allocate( off(n+1) )
    off(1) = 0
    do k = 1, n
       off(k+1) = off(k) + Ls(k)%p%props%n_poly
    end do
    total = off(n+1)
    allocate( par(total) )
    do ord = 1, total
       par(ord) = ord
    end do

    ncontacts = 0
    if( aa ) then
       allocate( P(n) )
       do k = 1, n
          call layer_pieces( Ls(k)%p, P(k) )
       end do
       do c = 2, n - 1, 2
          do r = c - 1, c + 1, 2
             call collect_overlaps_traps( P(c), P(r), off(c), off(r), par, ncontacts )
          end do
       end do
       deallocate( P )
    else
       do c = 2, n - 1, 2
          do r = c - 1, c + 1, 2
             call collect_overlaps( Ls(c)%p, Ls(r)%p, off(c), off(r), par, ncontacts )
          end do
       end do
    end if

    !> canonical numbering: first appearance over (stack position, ordinal)
    allocate( lab(total) )
    lab = 0
    next = 0
    do k = 1, n
       associate( t => Ls(k)%p%props )
         call PropDropNets( t )
         allocate( t%net( t%n_poly ) )
         do ord = 1, t%n_poly
            root = find( par, off(k) + ord )
            if( lab(root) == 0 ) then
               next = next + 1
               lab(root) = next
            end if
            t%net(ord) = lab(root)
         end do
         t%net_rec = rec
       end associate
    end do
    do k = 1, n
       Ls(k)%p%props%n_nets = next
    end do
    connect_records(rec)%n_nets = next
    connect_records(rec)%n_contacts = ncontacts
    connect_records(rec)%n_polygons = total
    call cpu_time( t1 )
    write(*,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,F10.3,A)') 'CONNECT #', rec, ': layers=', n, ' polygons=', total, &
         ' contacts=', ncontacts, ' nets=', next, ' cpu=', t1 - t0, ' s'
  end subroutine ConnectCompute

  !> Union every polygon of cut layer C with every polygon of routing layer R that it overlaps
  !> with positive area. Pairs are found in parallel (R-tree of R per box of C) and unioned per
  !> thread inside a critical section; the resulting partition does not depend on the order.
  subroutine collect_overlaps( C, R, offc, offr, par, ncontacts )
    type(Layer), intent(in) :: C, R
    integer(int64), intent(in) :: offc, offr
    integer(int64), intent(inout) :: par(:)
    integer(int64), intent(inout) :: ncontacts
    integer(int64), allocatable :: pc(:), pr(:), tmp(:)
    integer(int64) :: i, k, np, c_end, root_idx, curr, child, numR, oc, orr
    integer(int64) :: stack(K_STACK)
    integer :: sp
    type(Box) :: a, b

    if( C%n_used == 0 .or. R%n_used == 0 ) return
    numR = R%n_used
    root_idx = R%tree%root_index
    !$omp parallel private(pc, pr, tmp, np, i, k, c_end, curr, child, stack, sp, a, b, oc, orr)
    allocate( pc(256), pr(256) )
    np = 0
    !$omp do schedule(dynamic, 1024)
    do i = 1, C%n_used
       a = C%layer_boxes(i)
       oc = offc + C%props%box_ord(i)
       if( a%x1 > R%tree%tree_nodes(root_idx)%mbr%x2 .or. a%x2 < R%tree%tree_nodes(root_idx)%mbr%x1 .or. &
           a%y1 > R%tree%tree_nodes(root_idx)%mbr%y2 .or. a%y2 < R%tree%tree_nodes(root_idx)%mbr%y1 ) cycle
       sp = 1
       stack(1) = root_idx
       do while( sp > 0 )
          curr = stack(sp)
          sp = sp - 1
          associate( nd => R%tree%tree_nodes(curr) )
            if( nd%IsLeaf ) then
               c_end = min( nd%ChildStart + int( nd%NumChildren, int64 ) - 1_int64, numR )
               do k = nd%ChildStart, c_end
                  b = R%layer_boxes(k)
                  !> positive-area overlap only
                  if( max( a%x1, b%x1 ) >= min( a%x2, b%x2 ) ) cycle
                  if( max( a%y1, b%y1 ) >= min( a%y2, b%y2 ) ) cycle
                  orr = offr + R%props%box_ord(k)
                  if( np > 0 ) then
                     if( pc(np) == oc .and. pr(np) == orr ) cycle
                  end if
                  if( np == size(pc, kind=int64) ) then
                     allocate( tmp(2*np) ); tmp(1:np) = pc(1:np); call move_alloc( tmp, pc )
                     allocate( tmp(2*np) ); tmp(1:np) = pr(1:np); call move_alloc( tmp, pr )
                  end if
                  np = np + 1
                  pc(np) = oc
                  pr(np) = orr
               end do
            else
               c_end = nd%ChildStart + int( nd%NumChildren, int64 ) - 1_int64
               do child = nd%ChildStart, c_end
                  associate( cn => R%tree%tree_nodes(child) )
                    if( a%x1 <= cn%mbr%x2 .and. a%x2 >= cn%mbr%x1 .and. &
                        a%y1 <= cn%mbr%y2 .and. a%y2 >= cn%mbr%y1 ) then
                       if( sp >= K_STACK ) error stop 'CONNECT: R-tree DFS stack overflow'
                       sp = sp + 1
                       stack(sp) = child
                    end if
                  end associate
               end do
            end if
          end associate
       end do
    end do
    !$omp end do
    !$omp critical (connect_union)
    do k = 1, np
       call unite( par, pc(k), pr(k) )
    end do
    ncontacts = ncontacts + np
    !$omp end critical (connect_union)
    deallocate( pc, pr )
    !$omp end parallel
  end subroutine collect_overlaps

  !> Pieces of a member (after LayerEnsureProperties): the canonical trapezoids of an any-angle
  !> layer or the healed boxes of a Manhattan one, each with its polygon ordinal (props%box_ord).
  subroutine layer_pieces( L, S )
    type(Layer), intent(in) :: L
    type(PieceSet), intent(out) :: S
    integer(int64) :: i
    if( LayerIsAnyAngle( L ) ) then
       S%n = L%n_traps
    else
       S%n = L%n_used
    end if
    allocate( S%t(max(S%n,1_int64)), S%ord(max(S%n,1_int64)), S%x1(max(S%n,1_int64)), &
         S%y1(max(S%n,1_int64)), S%x2(max(S%n,1_int64)), S%y2(max(S%n,1_int64)) )
    !$omp parallel do
    do i = 1, S%n
       if( LayerIsAnyAngle( L ) ) then
          S%t(i) = L%traps(i)
       else
          S%t(i) = trap_from_box_coords( int( L%layer_boxes(i)%x1, int32 ), int( L%layer_boxes(i)%y1, int32 ), &
               int( L%layer_boxes(i)%x2, int32 ), int( L%layer_boxes(i)%y2, int32 ) )
       end if
       call trap_mbr( S%t(i), S%x1(i), S%y1(i), S%x2(i), S%y2(i) )
       S%ord(i) = L%props%box_ord(i)
    end do
    !$omp end parallel do
  end subroutine layer_pieces

  !> Any-angle collect_overlaps: union the polygons of pieces of C and R whose interiors meet
  !> (exact trap_overlap_positive). Candidates from a uniform grid over R's bounding boxes; a pair
  !> is tested only in the lowest grid cell of the intersection of the two bounding boxes.
  subroutine collect_overlaps_traps( C, R, offc, offr, par, ncontacts )
    type(PieceSet), intent(in) :: C, R
    integer(int64), intent(in) :: offc, offr
    integer(int64), intent(inout) :: par(:)
    integer(int64), intent(inout) :: ncontacts
    integer(int64) :: gx0, gy0, gx1, gy1, cs, nx, ny, ncell, i, j, k, cx, cy, cxa, cxb, cya, cyb
    integer(int64) :: oc, orr, np, sw, sh
    integer(int64), allocatable :: cstart(:), cfill(:), citem(:), pc(:), pr(:), tmp(:)
    if( C%n == 0 .or. R%n == 0 ) return
    gx0 = minval( R%x1(1:R%n) ); gy0 = minval( R%y1(1:R%n) )
    gx1 = maxval( R%x2(1:R%n) ); gy1 = maxval( R%y2(1:R%n) )
    sw = 0; sh = 0
    !$omp parallel do reduction(+:sw, sh)
    do i = 1, R%n
       sw = sw + ( R%x2(i) - R%x1(i) ); sh = sh + ( R%y2(i) - R%y1(i) )
    end do
    !$omp end parallel do
    cs = max( 1_int64, 2 * max( sw, sh ) / R%n )
    do
       nx = ( gx1 - gx0 ) / cs + 1; ny = ( gy1 - gy0 ) / cs + 1
       if( nx * ny <= 4 * R%n + 1024 ) exit
       cs = cs * 2
    end do
    ncell = nx * ny
    allocate( cstart(ncell + 1), cfill(ncell) )
    cstart = 0
    do i = 1, R%n
       do cy = ( R%y1(i) - gy0 ) / cs, ( R%y2(i) - gy0 ) / cs
          do cx = ( R%x1(i) - gx0 ) / cs, ( R%x2(i) - gx0 ) / cs
             k = cy * nx + cx + 2
             cstart(k) = cstart(k) + 1
          end do
       end do
    end do
    cstart(1) = 1
    do k = 2, ncell + 1
       cstart(k) = cstart(k) + cstart(k - 1)
    end do
    allocate( citem(max(cstart(ncell + 1) - 1, 1_int64)) )
    cfill = cstart(1:ncell)
    do i = 1, R%n
       do cy = ( R%y1(i) - gy0 ) / cs, ( R%y2(i) - gy0 ) / cs
          do cx = ( R%x1(i) - gx0 ) / cs, ( R%x2(i) - gx0 ) / cs
             k = cy * nx + cx + 1
             citem( cfill(k) ) = i
             cfill(k) = cfill(k) + 1
          end do
       end do
    end do
    !$omp parallel private(pc, pr, tmp, np, i, j, k, cx, cy, cxa, cxb, cya, cyb, oc, orr)
    allocate( pc(256), pr(256) )
    np = 0
    !$omp do schedule(dynamic, 1024)
    do i = 1, C%n
       if( C%x2(i) < gx0 .or. C%x1(i) > gx1 .or. C%y2(i) < gy0 .or. C%y1(i) > gy1 ) cycle
       cxa = max( 0_int64, ( C%x1(i) - gx0 ) / cs ); cxb = min( nx - 1, ( C%x2(i) - gx0 ) / cs )
       cya = max( 0_int64, ( C%y1(i) - gy0 ) / cs ); cyb = min( ny - 1, ( C%y2(i) - gy0 ) / cs )
       oc = offc + C%ord(i)
       do cy = cya, cyb
          do cx = cxa, cxb
             k = cy * nx + cx + 1
             do j = cstart(k), cstart(k + 1) - 1
                associate( q => citem(j) )
                  if( max( C%x1(i), R%x1(q) ) >= min( C%x2(i), R%x2(q) ) ) cycle
                  if( max( C%y1(i), R%y1(q) ) >= min( C%y2(i), R%y2(q) ) ) cycle
                  !> test the pair once: in the cell holding the low corner of the bbox intersection
                  if( ( max( C%x1(i), R%x1(q) ) - gx0 ) / cs /= cx ) cycle
                  if( ( max( C%y1(i), R%y1(q) ) - gy0 ) / cs /= cy ) cycle
                  if( .not. trap_overlap_positive( C%t(i), R%t(q) ) ) cycle
                  orr = offr + R%ord(q)
                end associate
                if( np > 0 ) then
                   if( pc(np) == oc .and. pr(np) == orr ) cycle
                end if
                if( np == size(pc, kind=int64) ) then
                   allocate( tmp(2*np) ); tmp(1:np) = pc(1:np); call move_alloc( tmp, pc )
                   allocate( tmp(2*np) ); tmp(1:np) = pr(1:np); call move_alloc( tmp, pr )
                end if
                np = np + 1
                pc(np) = oc
                pr(np) = orr
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp critical (connect_union)
    do k = 1, np
       call unite( par, pc(k), pr(k) )
    end do
    ncontacts = ncontacts + np
    !$omp end critical (connect_union)
    deallocate( pc, pr )
    !$omp end parallel
  end subroutine collect_overlaps_traps

  integer(int64) function find( par, x0 ) result(x)
    integer(int64), intent(inout) :: par(:)
    integer(int64), intent(in) :: x0
    x = x0
    do while( par(x) /= x )
       par(x) = par( par(x) )          !> path halving
       x = par(x)
    end do
  end function find

  subroutine unite( par, a, b )
    integer(int64), intent(inout) :: par(:)
    integer(int64), intent(in) :: a, b
    integer(int64) :: ra, rb
    ra = find( par, a )
    rb = find( par, b )
    if( ra == rb ) return
    if( ra < rb ) then
       par(rb) = ra
    else
       par(ra) = rb
    end if
  end subroutine unite

  !> exec netinfo: nets of one member layer.
  subroutine NetInfoPrint( L, lname )
    type(Layer), intent(in) :: L
    character(len=*), intent(in) :: lname
    integer(int64), allocatable :: cnt(:)
    integer(int64) :: ord, big, bign, distinct
    integer :: rec, k
    character(len=:), allocatable :: stack
    rec = L%props%net_rec
    stack = ''
    do k = 1, size( connect_records(rec)%specs )
       stack = stack//' '//connect_records(rec)%specs(k)%s
    end do
    allocate( cnt( max( 1_int64, L%props%n_nets ) ) )
    cnt = 0
    do ord = 1, L%props%n_poly
       cnt( L%props%net(ord) ) = cnt( L%props%net(ord) ) + 1
    end do
    distinct = count( cnt > 0 )
    big = 0; bign = 0
    if( L%props%n_poly > 0 ) then
       bign = maxloc( cnt, 1 )
       big = cnt(bign)
    end if
    write(*,'(A,A,A,I0,A,A)') 'NETS ', lname, ': CONNECT #', rec, ' :', stack
    write(*,'(A,I0,A,I0,A,I0,A,I0)') '  stack polygons=', connect_records(rec)%n_polygons, &
         ' contacts=', connect_records(rec)%n_contacts, ' nets=', connect_records(rec)%n_nets, &
         '  layer polygons=', L%props%n_poly
    write(*,'(A,I0,A,I0,A,I0,A)') '  nets on this layer=', distinct, '  largest: net ', bign, &
         ' (', big, ' polygons on this layer)'
  end subroutine NetInfoPrint

  !> Extracted net (props%net) of the polygon of L that contains (x,y), 0 if none. Polygons of
  !> one layer never touch (touching boxes are one polygon), so any containing box decides.
  integer(int64) function point_net( L, x, y ) result(nid)
    type(Layer), intent(in) :: L
    integer(int64), intent(in) :: x, y
    integer(int64) :: stack(K_STACK), curr, child, k, c_end
    integer :: sp
    nid = 0
    if( L%n_used == 0 ) return
    sp = 1
    stack(1) = L%tree%root_index
    do while( sp > 0 )
       curr = stack(sp)
       sp = sp - 1
       associate( nd => L%tree%tree_nodes(curr) )
         if( x < nd%mbr%x1 .or. x > nd%mbr%x2 .or. y < nd%mbr%y1 .or. y > nd%mbr%y2 ) cycle
         if( nd%IsLeaf ) then
            c_end = min( nd%ChildStart + int( nd%NumChildren, int64 ) - 1_int64, L%n_used )
            do k = nd%ChildStart, c_end
               associate( b => L%layer_boxes(k) )
                 if( x >= b%x1 .and. x <= b%x2 .and. y >= b%y1 .and. y <= b%y2 ) then
                    nid = L%props%net( L%props%box_ord(k) )
                    return
                 end if
               end associate
            end do
         else
            c_end = nd%ChildStart + int( nd%NumChildren, int64 ) - 1_int64
            do child = nd%ChildStart, c_end
               if( sp >= K_STACK ) error stop 'defcheck: R-tree DFS stack overflow'
               sp = sp + 1
               stack(sp) = child
            end do
         end if
       end associate
    end do
  end function point_net

  !> exec defcheck: probe the routing of every DEF net (db/def_probe.cpp) on the routing members
  !> of CONNECT record `rec`, look up the extracted net under each probe and report opens,
  !> shorts and misses; the DEF names become the NET_NAME of the extracted nets.
  !> stats(1..16) = fdef_check stats 0..15 (see def_probe.cpp).
  subroutine DefCheck( rec, Ls, def_path, lefs, prefix, layout_dbu, stats, status, msg )
    integer, intent(in) :: rec
    character(len=*), intent(in) :: lefs             !> comma-separated LEF files ('' = none)
    real(real64), intent(in) :: layout_dbu       !> layout database units per micron (<= 0: CTR precision)
    type(LayerPtr), intent(inout) :: Ls(:)
    character(len=*), intent(in) :: def_path, prefix
    integer(int64), intent(out) :: stats(16)
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    character(len=64), allocatable :: sp(:), defn(:)
    character(len=16) :: tech_used
    character(len=:), allocatable :: names
    integer(c_int32_t) :: h, units, rc
    integer(c_int64_t) :: np, nn, nskip
    integer(c_int32_t), allocatable :: lay(:), pkind(:)
    integer(c_int64_t), allocatable :: px(:), py(:), pnet(:), nid(:)
    integer(c_int64_t) :: cstats(16)
    type(PropString), allocatable :: lab(:)
    character(len=8192) :: buf
    character(len=:), allocatable :: rep
    integer(int64) :: i, e, mult, dv, nr, m
    real(real64) :: ref, ratio
    integer :: k, n, ln, p0, p1
    real(real64) :: t0, t1
    integer(int64) :: c0, c1, cr

    status = 0; msg = ''; stats = 0
    do i = 1, size(Ls)
       if( LayerIsAnyAngle( Ls(i)%p ) ) then
          status = 1; msg = 'defcheck: any-angle layer '//connect_records(rec)%specs(i)%s//' is not supported yet'
          return
       end if
    end do
    call cpu_time( t0 ); call system_clock( c0, cr )
    n = size(Ls)
    nr = ( n + 1 ) / 2
    !> DEF layer names of the members
    if( .not. allocated( connect_records(rec)%defn ) ) then
       allocate( sp(n) )
       do k = 1, n
          sp(k) = connect_records(rec)%specs(k)%s
       end do
       call TechDefNames( current_tech, sp, defn, tech_used, status, msg )
       if( status /= 0 ) then
          msg = 'defcheck: '//msg; return
       end if
       allocate( connect_records(rec)%defn(n) )
       do k = 1, n
          connect_records(rec)%defn(k)%s = trim(defn(k))
       end do
       connect_records(rec)%tech = tech_used
    end if
    names = ''
    do k = 1, n, 2
       names = names//' '//connect_records(rec)%defn(k)%s
    end do
    names = trim(adjustl(names))
    do k = 1, n, 2
       associate( L => Ls(k)%p )
         if( L%n_used > 0 ) then
            if( .not. allocated( L%tree%tree_nodes ) .or. .not. allocated( L%props%net ) ) then
               status = 1; msg = 'defcheck: the nets of '//connect_records(rec)%specs(k)%s//' are out of date'
               return
            end if
            if( size( L%props%box_ord, kind=int64 ) /= L%n_used ) then
               status = 1; msg = 'defcheck: the nets of '//connect_records(rec)%specs(k)%s//' are out of date'
               return
            end if
         end if
       end associate
    end do

    h = fdef_open( trim(def_path)//c_null_char, names//c_null_char, trim(lefs)//c_null_char, np, nn, nskip, units )
    if( h == 0 ) then
       ln = fdef_error( buf, len(buf, kind=c_int32_t) )
       msg = 'defcheck: '//buf(1:min(ln, len(buf)))
       status = 1; return
    end if
    m = max( np, 1_int64 )
    allocate( lay(m), pkind(m), px(m), py(m), pnet(m), nid(m) )
    rc = fdef_probes( h, lay, px, py, pnet, pkind )
    !> DEF units vs layout grid
    !> DEF units -> layout grid: an integer factor either way (ASAP7 DEFs use 4000/um)
    mult = 1; dv = 1
    ref = layout_dbu
    if( ref <= 0 ) ref = real( GetPrecision(), real64 )
    if( units > 0 .and. ref > 0 ) then
       ratio = ref / real( units, real64 )
       if( ratio >= 1 .and. abs( ratio - nint(ratio) ) < 1d-9 ) then
          mult = nint( ratio, int64 )
       else if( ratio < 1 .and. abs( 1/ratio - nint(1/ratio) ) < 1d-9 ) then
          dv = nint( 1/ratio, int64 )
       else
          write(*,'(A,I0,A,F0.3,A)') 'WARNING: defcheck: DEF units ', units, ' and layout grid ', ref, &
               ' per micron differ by a non-integer factor; coordinates used as they are'
       end if
    end if
    if( mult /= 1 .or. dv /= 1 ) then
       do i = 1, np
          px(i) = px(i) * mult / dv; py(i) = py(i) * mult / dv
       end do
    end if
    !$omp parallel do schedule(dynamic, 4096)
    do i = 1, np
       nid(i) = point_net( Ls(2*lay(i)-1)%p, px(i), py(i) )
    end do
    !$omp end parallel do
    rc = fdef_check( h, nid, int(connect_records(rec)%n_nets, c_int64_t), trim(prefix)//c_null_char, &
         names//c_null_char, cstats )
    stats = cstats
    allocate( lab( connect_records(rec)%n_nets ) )
    do e = 1, connect_records(rec)%n_nets
       ln = fdef_label( h, int(e, c_int64_t), buf, len(buf, kind=c_int32_t) )
       lab(e)%s = buf(1:min(ln, len(buf)))
    end do
    call PropNetNamesSet( rec, def_path, lab )
    ln = fdef_report( h, buf, len(buf, kind=c_int32_t) )
    rep = buf(1:min(ln, len(buf)))
    call fdef_close( h )
    call cpu_time( t1 ); call system_clock( c1 )

    write(*,'(A,I0,A,A,A,A,A)') 'DEFCHECK CONNECT #', rec, ' (', trim(connect_records(rec)%tech), ') vs ', &
         trim(def_path), ':'
    write(*,'(A,A,A,I0,A,F0.1)') '  routing layers: ', names, '   DEF units/um: ', units, '  layout units/um: ', ref
    write(*,'(A,I0,A,I0,A,I0,A,I0,A)') '  DEF nets=', stats(4), ' (special ', stats(13), ')  probes=', stats(1), &
         ' (+', stats(14), ' on layers outside the stack)'
    write(*,'(A,I0,A,I0,A,I0,A)') '  probes on metal=', stats(2), '  missed=', stats(3), ' (in ', stats(8), ' DEF nets)'
    write(*,'(A,I0,A,I0,A,I0,A,I0,A,I0)') '  DEF nets: probed=', stats(5), ' clean=', stats(6), ' opens=', stats(7), &
         ' (special ', stats(15), ')  without probes=', stats(12)
    write(*,'(A,I0,A,I0,A,I0,A,I0,A,I0)') '  extracted nets=', connect_records(rec)%n_nets, ': named=', stats(9), &
         ' shorts=', stats(10), ' (special ', stats(16), ')  unnamed=', stats(11)
    p0 = 1
    do while( p0 <= len(rep) )
       p1 = index( rep(p0:), achar(10) )
       if( p1 == 0 ) then
          write(*,'(A)') rep(p0:); exit
       end if
       write(*,'(A)') rep(p0:p0+p1-2)
       p0 = p0 + p1
    end do
    write(*,'(A,I0,A,I0,A,I0,A,F9.3,A,F9.3,A)') 'DEFCHECK RESULT: opens=', stats(7), ' shorts=', stats(10), &
         ' misses=', stats(3), '  cpu=', t1 - t0, ' s wall=', real(c1-c0, real64)/real(cr, real64), ' s'
  end subroutine DefCheck

end module FASCLConnectModule
