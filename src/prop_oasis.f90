! File   : prop_oasis.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Write MAGPARSER layers (with their per-polygon PropertyTable) as OASIS.
!
! The geometry is written per polygon (outline traced from the healed boxes), each
! element followed by its PROPERTY records, by db/prop_oasis_writer.cpp.  Options
! (CTR `var`): oasis_cblock, oasis_repetition, oasis_strict (all default 1), oasis_cblock_bytes
! (uncompressed bytes per CBLOCK, default 4 MiB; blocks end on record boundaries).
module PropOasisModule
  use iso_c_binding
  use iso_fortran_env, only: int8, int64, real64
  use DesignModule
  use PropertyTableModule
  use FASCLModule, only: LayerEnsureProperties
  use CommonModule, only: GetPrecision
  implicit none
  private
  public :: OasisPropOpen, OasisPropAddLayer, OasisPropClose, ParseLayerDatatype, OasisPropSetOption
  public :: OasisWriter

  integer, parameter :: FPO_CBLOCK = 1, FPO_REP = 2, FPO_STRICT = 4
  integer, save :: oasis_flags = FPO_CBLOCK + FPO_REP + FPO_STRICT
  integer(int64), save :: oasis_cblock_bytes = 0     !> 0 = writer default (4 MiB)

  type :: OasisWriter
     type(c_ptr) :: h = c_null_ptr
     character(len=:), allocatable :: path
  end type OasisWriter

  interface
     function fpo_create( path, cell, grid, flags ) bind(C, name='fpo_create') result(h)
       import :: c_ptr, c_char, c_int64_t, c_int32_t
       character(kind=c_char), intent(in) :: path(*), cell(*)
       integer(c_int64_t), value :: grid
       integer(c_int32_t), value :: flags
       type(c_ptr) :: h
     end function fpo_create
     subroutine fpo_set_cblock_bytes( h, n ) bind(C, name='fpo_set_cblock_bytes')
       import :: c_ptr, c_int64_t
       type(c_ptr), value :: h
       integer(c_int64_t), value :: n
     end subroutine fpo_set_cblock_bytes
     function fpo_begin_layer( h, lname, layer, datatype, npoly, poly_start, nbox, boxes ) &
          bind(C, name='fpo_begin_layer') result(st)
       import :: c_ptr, c_char, c_int64_t, c_int32_t
       type(c_ptr), value :: h
       character(kind=c_char), intent(in) :: lname(*)
       integer(c_int64_t), value :: layer, datatype, npoly, nbox
       integer(c_int64_t), intent(in) :: poly_start(*), boxes(*)
       integer(c_int32_t) :: st
     end function fpo_begin_layer
     function fpo_column( h, name, field, kind, present, ival, rval, soff, schars ) &
          bind(C, name='fpo_column') result(st)
       import :: c_ptr, c_char, c_int32_t
       type(c_ptr), value :: h
       character(kind=c_char), intent(in) :: name(*), field(*)
       integer(c_int32_t), value :: kind
       type(c_ptr), value :: present, ival, rval, soff, schars
       integer(c_int32_t) :: st
     end function fpo_column
     function fpo_end_layer( h, lname, errbuf, errlen ) bind(C, name='fpo_end_layer') result(st)
       import :: c_ptr, c_char, c_int32_t
       type(c_ptr), value :: h
       character(kind=c_char), intent(in) :: lname(*)
       character(kind=c_char), intent(inout) :: errbuf(*)
       integer(c_int32_t), value :: errlen
       integer(c_int32_t) :: st
     end function fpo_end_layer
     function fpo_close( h, errbuf, errlen ) bind(C, name='fpo_close') result(st)
       import :: c_ptr, c_char, c_int32_t
       type(c_ptr), value :: h
       character(kind=c_char), intent(inout) :: errbuf(*)
       integer(c_int32_t), value :: errlen
       integer(c_int32_t) :: st
     end function fpo_close
  end interface

contains

  !> `var oasis_cblock|oasis_repetition|oasis_strict 0|1`, `var oasis_cblock_bytes N`;
  !> returns .false. if not an OASIS option.
  logical function OasisPropSetOption( name, value ) result(known)
    character(len=*), intent(in) :: name
    integer, intent(in) :: value
    integer :: bit
    known = .true.
    select case( name )
    case( 'oasis_cblock_bytes' )
       oasis_cblock_bytes = value
       return
    case( 'oasis_cblock' );     bit = FPO_CBLOCK
    case( 'oasis_repetition' ); bit = FPO_REP
    case( 'oasis_strict' );     bit = FPO_STRICT
    case default
       known = .false.; return
    end select
    if( value /= 0 ) then
       oasis_flags = ior( oasis_flags, bit )
    else
       oasis_flags = iand( oasis_flags, not( bit ) )
    end if
  end function OasisPropSetOption

  pure function cstr( s ) result(c)
    character(len=*), intent(in) :: s
    character(kind=c_char, len=len_trim(s)+1) :: c
    c = trim(s) // c_null_char
  end function cstr

  subroutine OasisPropOpen( w, path, cell )
    type(OasisWriter), intent(out) :: w
    character(len=*), intent(in) :: path, cell
    w%path = trim(path)
    w%h = fpo_create( cstr(path), cstr(cell), int( GetPrecision(), c_int64_t ), int( oasis_flags, c_int32_t ) )
    if( oasis_cblock_bytes > 0 ) call fpo_set_cblock_bytes( w%h, int( oasis_cblock_bytes, c_int64_t ) )
  end subroutine OasisPropOpen

  subroutine OasisPropAddLayer( w, L, lname, layer_no, datatype, status, msg )
    type(OasisWriter), intent(inout) :: w
    type(Layer), intent(inout), target :: L
    character(len=*), intent(in) :: lname
    integer(int64), intent(in) :: layer_no, datatype
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    integer(c_int64_t), allocatable, target :: bx(:), ps(:)
    integer(int64) :: p, i, k, nb, np
    integer :: c, j
    character(kind=c_char, len=512) :: errbuf

    msg = ''
    status = 0
    if( LayerIsAnyAngle( L ) ) then
       status = 1; msg = 'oasout: any-angle layer '//lname//' is not supported yet'; return
    end if
    call LayerEnsureProperties( L, lname )
    np = L%props%n_poly
    nb = L%n_used
    allocate( bx( 4*max(nb,1_int64) ), ps( np+1 ) )
    k = 0
    do i = 1, nb
       associate( b => L%layer_boxes( L%props%poly_boxes(i) ) )
         bx(k+1) = b%x1; bx(k+2) = b%y1; bx(k+3) = b%x2; bx(k+4) = b%y2
       end associate
       k = k + 4
    end do
    ps(:) = L%props%poly_start(1:np+1)
    status = fpo_begin_layer( w%h, cstr(lname), int(layer_no, c_int64_t), int(datatype, c_int64_t), &
         int(np, c_int64_t), ps, int(nb, c_int64_t), bx )
    deallocate( bx )
    do c = 1, L%props%n_cols
       associate( col => L%props%cols(c) )
         if( col%kind == PV_STRUCT ) then
            call send( col%name, '', col%d )
            do j = 1, size( col%fields )
               call send( col%name, col%fields(j)%name, col%fields(j) )
               if( status /= 0 ) exit
            end do
         else
            call send( col%name, '', col%d )
         end if
       end associate
       if( status /= 0 ) exit
    end do
    if( status == 0 ) then
       errbuf = ''
       status = fpo_end_layer( w%h, cstr(lname), errbuf, int( len(errbuf), c_int32_t ) )
       if( status /= 0 ) msg = 'OASIS writer: ' // c_trim( errbuf )
    else
       msg = 'OASIS writer: bad property column on layer ' // lname
    end if
    if( status == 0 ) write(*,'(A,A,A,I0,A,I0,A,I0,A,I0,A)') 'INFO: OASIS layer ', trim(lname), ' -> ', layer_no, '/', &
         datatype, ' : ', np, ' polygons, ', L%props%n_cols, ' properties'
  contains
    subroutine send( name, field, d )
      character(len=*), intent(in) :: name, field
      type(PropData), intent(in) :: d
      integer(int8), allocatable, target :: pr(:)
      integer(c_int64_t), allocatable, target :: iv(:), soff(:)
      real(c_double), allocatable, target :: rv(:)
      character(kind=c_char), allocatable, target :: chars(:)
      type(c_ptr) :: pi, pr_, po, pc
      integer(int64) :: q, tot, n
      n = np
      allocate( pr( max(n,1_int64) ) )
      pr = 0_int8
      do q = 1, n
         if( d%present(q) ) pr(q) = 1_int8
      end do
      pi = c_null_ptr; pr_ = c_null_ptr; po = c_null_ptr; pc = c_null_ptr
      select case( d%kind )
      case( PV_INT )
         allocate( iv( max(n,1_int64) ) ); if( n > 0 ) iv(1:n) = d%iv(1:n); pi = c_loc( iv )
      case( PV_FLOAT )
         allocate( rv( max(n,1_int64) ) ); if( n > 0 ) rv(1:n) = d%rv(1:n); pr_ = c_loc( rv )
      case( PV_STRING )
         allocate( soff( n+1 ) )
         tot = 0
         soff(1) = 0
         do q = 1, n
            tot = tot + len( d%sv(q)%s )
            soff(q+1) = tot
         end do
         allocate( chars( max(tot,1_int64) ) )
         do q = 1, n
            do i = 1, len( d%sv(q)%s )
               chars( soff(q) + i ) = d%sv(q)%s(i:i)
            end do
         end do
         po = c_loc( soff ); pc = c_loc( chars )
      end select
      status = fpo_column( w%h, cstr(name), cstr(field), int( d%kind, c_int32_t ), c_loc( pr ), pi, pr_, po, pc )
    end subroutine send
  end subroutine OasisPropAddLayer

  subroutine OasisPropClose( w, status, msg )
    type(OasisWriter), intent(inout) :: w
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    character(kind=c_char, len=512) :: errbuf
    errbuf = ''
    msg = ''
    status = fpo_close( w%h, errbuf, int( len(errbuf), c_int32_t ) )
    w%h = c_null_ptr
    if( status /= 0 ) then
       msg = 'OASIS writer: ' // c_trim( errbuf )
    else
       write(*,'(A,A)') 'INFO: wrote OASIS ', w%path
    end if
  end subroutine OasisPropClose

  function c_trim( s ) result(r)
    character(kind=c_char, len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: k
    k = index( s, c_null_char )
    if( k == 0 ) then
       r = trim(s)
    else
       r = s(1:k-1)
    end if
  end function c_trim

  !> Layer/datatype from a layer or file name: "L67_20", "67:20", "..._L67_D20..." or "L67D20".
  subroutine ParseLayerDatatype( name, layer_no, datatype, ok )
    character(len=*), intent(in) :: name
    integer(int64), intent(out) :: layer_no, datatype
    logical, intent(out) :: ok
    integer :: i, n, p, q
    character(len=:), allocatable :: s
    ok = .false.
    layer_no = 0; datatype = 0
    s = trim(name)
    p = index( s, '/', back=.true. )
    if( p > 0 ) s = s(p+1:)
    n = len(s)
    ! "<int>:<int>"
    p = index( s, ':' )
    if( p > 1 ) then
       if( all_digits( s(1:p-1) ) .and. all_digits( s(p+1:) ) ) then
          read( s(1:p-1), * ) layer_no; read( s(p+1:), * ) datatype; ok = .true.; return
       end if
    end if
    ! "...L<int>_D<int>..." / "...L<int>D<int>..." / "L<int>_<int>"
    do i = 1, n
       if( s(i:i) /= 'L' ) cycle
       if( i > 1 ) then
          if( s(i-1:i-1) /= '_' .and. s(i-1:i-1) /= '-' .and. s(i-1:i-1) /= '.' ) cycle
       end if
       q = i + 1
       do while( q <= n )
          if( s(q:q) < '0' .or. s(q:q) > '9' ) exit
          q = q + 1
       end do
       if( q == i + 1 .or. q > n ) cycle
       p = q
       if( s(p:p) == '_' ) p = p + 1
       if( p <= n ) then
          if( s(p:p) == 'D' ) p = p + 1
       end if
       if( p == q ) cycle
       q = p
       do while( q <= n )
          if( s(q:q) < '0' .or. s(q:q) > '9' ) exit
          q = q + 1
       end do
       if( q == p ) cycle
       block
         integer :: e
         e = i + 1
         do while( s(e:e) >= '0' .and. s(e:e) <= '9' )
            e = e + 1
         end do
         read( s(i+1:e-1), * ) layer_no
       end block
       read( s(p:q-1), * ) datatype
       ok = .true.
       return
    end do
  contains
    pure logical function all_digits( t )
      character(len=*), intent(in) :: t
      integer :: k
      all_digits = len(t) > 0
      do k = 1, len(t)
         if( t(k:k) < '0' .or. t(k:k) > '9' ) all_digits = .false.
      end do
    end function all_digits
  end subroutine ParseLayerDatatype

end module PropOasisModule
