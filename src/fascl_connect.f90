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
  use iso_fortran_env, only: int64, real64
  use DesignModule
  use GeometryModule, only: Box
  use PropertyTableModule
  use FASCLModule, only: LayerEnsureProperties
  implicit none
  private

  type, public :: LayerPtr
     type(Layer), pointer :: p => null()
  end type LayerPtr

  type, public :: ConnectRecord
     type(PropString), allocatable :: specs(:)
     logical :: live = .false.
     integer(int64) :: n_nets = 0, n_contacts = 0, n_polygons = 0
  end type ConnectRecord

  type(ConnectRecord), allocatable, public, save :: connect_records(:)
  integer, public, save :: n_connect_records = 0

  integer, parameter :: K_STACK = 512

  public :: ConnectNew, ConnectCompute, NetInfoPrint, ConnectValidate

contains

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
       tmp(1:n_connect_records) = connect_records(1:n_connect_records)
       call move_alloc( tmp, connect_records )
    end if
    n_connect_records = n_connect_records + 1
    rec = n_connect_records
    connect_records(rec)%specs = specs
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

    status = 0; msg = ''
    call cpu_time( t0 )
    n = size(Ls)
    !> Every member: healed, PNum'd, indexed, with an R-tree that matches the box order.
    do k = 1, n
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
    do c = 2, n - 1, 2
       do r = c - 1, c + 1, 2
          call collect_overlaps( Ls(c)%p, Ls(r)%p, off(c), off(r), par, ncontacts )
       end do
    end do

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

end module FASCLConnectModule
