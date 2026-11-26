! File   : property_table.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Per-polygon property tables for geometric layers (OASIS-style NAME -> VALUE).
!
! A layer's polygons are the connected components of its boxes, as numbered by the
! PNum pass (PerformMerge / DisjointUnion + fullreduce): multi-box polygons carry labels
! 1..R, single-rect polygons are numbered R+1..R+S in box order (uf%expand_roots, the
! NSPT counts of the PNumLoop table); that number is the polygon id seen by FASCL
! lambdas.  The table indexes polygons by an
! *ordinal* 1..n_poly in canonical order (polygons sorted by the lexicographically
! smallest (x1,y1,x2,y2) of their boxes), so ordinals and property columns do not
! depend on box storage order, thread count or CPU/GPU numbering.  The id -> ordinal
! map (box_ord) is a cache rebuilt from the geometry whenever the table is used.
!
! Columns are typed like OASIS property values: INT (signed 64 bit), FLOAT (real64),
! STRING, or STRUCT (an ordered list of named scalar fields; one level, no nesting).
! A column is NAME -> value for every polygon, with a presence mask (a lambda may skip
! a polygon with WHEN).  The geometry signature (box count + order-independent box
! hash) detects when the layer was overwritten so stale properties are dropped.
module PropertyTableModule
  use iso_fortran_env, only: int8, int32, int64, real64
  use GeometryModule, only: Box
  use DataStructuresModule, only: UnionFind
  use CompactLayerModule, only: BoxSetHash
  implicit none
  private

  integer, parameter, public :: PV_NONE = 0, PV_INT = 1, PV_FLOAT = 2, PV_STRING = 3, &
       PV_STRUCT = 4, PV_ERROR = 5

  public :: PropStringsGrow, PropStringsAssign

  type, public :: PropString
     character(len=:), allocatable :: s
  end type PropString

  !> One scalar field of a STRUCT value.
  type, public :: PropField
     character(len=:), allocatable :: name
     integer                       :: kind = PV_NONE
     integer(int64)                :: i = 0
     real(real64)                  :: r = 0.0_real64
     character(len=:), allocatable :: s
  end type PropField

  !> A value produced by a lambda (or read back from a column).
  type, public :: PropValue
     integer                       :: kind = PV_NONE
     integer(int64)                :: i = 0
     real(real64)                  :: r = 0.0_real64
     character(len=:), allocatable :: s          !> STRING payload, or ERROR message
     type(PropField), allocatable  :: f(:)       !> STRUCT fields
  end type PropValue

  !> One typed column of scalars, indexed by polygon ordinal.
  type, public :: PropData
     character(len=:), allocatable :: name
     integer                       :: kind = PV_NONE
     logical, allocatable          :: present(:)
     integer(int64), allocatable   :: iv(:)
     real(real64), allocatable     :: rv(:)
     type(PropString), allocatable :: sv(:)
  end type PropData

  !> A named property: scalar (d) or STRUCT (d%present + one PropData per field).
  type, public :: PropColumn
     character(len=:), allocatable :: name
     integer                       :: kind = PV_NONE
     type(PropData)                :: d
     type(PropData), allocatable   :: fields(:)
  end type PropColumn

  type, public :: PropertyTable
     integer(int64) :: n_poly = 0
     integer(int64) :: n_rect = 0              !> polygons made of a single box
     integer(int64) :: geom_n = -1             !> geometry signature the columns belong to
     integer(int64) :: geom_hash = 0
     integer(int64), allocatable :: box_ord(:)     !> box index -> polygon ordinal
     integer(int64), allocatable :: poly_id(:)     !> ordinal -> PNum polygon id (FASCL `id`)
     integer(int64), allocatable :: id_ord(:)      !> PNum polygon id -> ordinal
     integer(int64), allocatable :: poly_start(:)  !> CSR (n_poly+1) into poly_boxes
     integer(int64), allocatable :: poly_boxes(:)  !> box indices grouped by polygon
     type(Box), allocatable      :: bbox(:)
     integer(int64), allocatable :: area(:)        !> sum of (disjoint) box areas
     integer                     :: n_cols = 0
     type(PropColumn), allocatable :: cols(:)
     !> Outline measures (FASCL PERIMETER/VERTICES/LINE_END_*), computed on first use and
     !> kept while the geometry signature is unchanged (like the columns).
     logical :: have_outline = .false.
     integer(int64) :: n_outline_fail = 0
     integer(int64), allocatable :: perim(:), nvert(:), le_w(:), le_len(:)
     integer(int8),  allocatable :: le_dir(:)      !> -1 none, 0 E, 1 N, 2 W, 3 S
     !> Nets from a CONNECT statement (FASCL NET_ID): record id, global net count, ordinal -> net.
     integer        :: net_rec = 0
     integer(int64) :: n_nets = 0
     integer(int64), allocatable :: net(:)
  end type PropertyTable

  public :: PropTableIndex, PropTableClear, PropColumnIndex, PropCommitColumn, PropGetValue, &
       PropGetField, PropOrdinalOfId, PropKindName, PropTablePrint, PropTableDumpCSV, PropValueToString, &
       PropDropOutline, PropDropNets

  !> DEF net names of the extracted nets of a CONNECT record (exec defcheck), read by the FASCL
  !> NET_NAME builtin: lab(net id) = DEF net name, "a|b|..." for a short, "" when unnamed.
  type :: NetNameSet
     logical :: have = .false.
     character(len=:), allocatable :: def_file
     type(PropString), allocatable :: lab(:)
  end type NetNameSet
  type(NetNameSet), allocatable, save :: net_names(:)
  public :: PropNetNamesSet, PropNetNamesDrop, PropNetName, PropNetNamesHave

contains

  !> Resize v to newsize keeping v(1:n). Arrays of PropString are copied element by element:
  !> whole-array assignment / array constructors of a type with a deferred-length character
  !> component lose the strings with nvfortran 26.1 (and some gfortran versions).
  subroutine PropStringsGrow( v, newsize, n )
    type(PropString), allocatable, intent(inout) :: v(:)
    integer, intent(in) :: newsize, n
    type(PropString), allocatable :: tmp(:)
    integer :: j
    allocate( tmp(newsize) )
    if( allocated(v) ) then
       do j = 1, min( n, size(v), newsize )
          if( allocated( v(j)%s ) ) tmp(j)%s = v(j)%s
       end do
    end if
    call move_alloc( tmp, v )
  end subroutine PropStringsGrow

  !> dst = src (fresh allocation, element-wise copy; see PropStringsGrow).
  subroutine PropStringsAssign( dst, src )
    type(PropString), allocatable, intent(inout) :: dst(:)
    type(PropString), intent(in) :: src(:)
    integer :: j
    if( allocated(dst) ) deallocate(dst)
    allocate( dst( size(src) ) )
    do j = 1, size(src)
       if( allocated( src(j)%s ) ) dst(j)%s = src(j)%s
    end do
  end subroutine PropStringsAssign

  !=================================================================
  ! Index: polygons of the current geometry, canonical ordinals, stats.
  ! Existing columns survive only if the geometry signature is unchanged.
  !=================================================================
  subroutine PropTableIndex( t, boxes, n, uf, dropped )
    type(PropertyTable), intent(inout) :: t
    type(Box), intent(in)              :: boxes(:)
    integer(int64), intent(in)         :: n
    type(UnionFind), intent(in)        :: uf
    logical, intent(out), optional     :: dropped
    integer(int64), allocatable :: root(:), amin(:), perm(:), fill(:)
    integer(int64), allocatable :: kx1(:), ky1(:), kx2(:), ky2(:)
    integer(int64) :: i, r, p, np, h, nr, nid, bi
    logical :: have_uf

    if( present(dropped) ) dropped = .false.
    h = 0
    if( n > 0 ) h = BoxSetHash( boxes, n )
    if( t%geom_n /= n .or. t%geom_hash /= h ) then
       !> geometry changed: values computed for the old polygons are meaningless now
       if( present(dropped) ) dropped = ( t%n_cols > 0 )
       call drop_columns( t )
       call PropDropOutline( t )
       call PropDropNets( t )
    end if
    t%geom_n = n
    t%geom_hash = h
    call drop_index( t )
    t%n_poly = 0
    t%n_rect = 0
    allocate( t%box_ord(n), t%poly_start(1) )
    t%poly_start(1) = 1
    if( n == 0 ) then
       allocate( t%poly_id(0), t%id_ord(0), t%poly_boxes(0), t%bbox(0), t%area(0) )
       return
    end if

    !> PNum polygon id, as uf%expand_roots would number it: after PerformMerge (fullreduce)
    !> arr(i) is the label 1..R of a multi-box polygon, 0 for a single-rect polygon; the
    !> single rects then take ids R+1..R+S in box order (NSPT: R polygons + S rects).
    have_uf = .false.
    if( allocated( uf%arr ) ) have_uf = ( size( uf%arr, kind=int64 ) >= n )
    allocate( root(n) )
    nr = 0
    if( have_uf ) nr = max( 0_int64, maxval( uf%arr(1:n) ) )
    nid = nr
    do i = 1, n
       if( have_uf ) then
          if( uf%arr(i) > 0 ) then
             root(i) = uf%arr(i)
             cycle
          end if
       end if
       nid = nid + 1
       root(i) = nid
    end do

    !> anchor = smallest box of the polygon in (x1,y1,x2,y2) order
    allocate( amin(nid), source=0_int64 )
    do i = 1, n
       r = root(i)
       if( amin(r) == 0 ) then
          amin(r) = i
       else if( box_less( boxes(i), boxes(amin(r)) ) ) then
          amin(r) = i
       end if
    end do
    np = count( amin > 0 )
    allocate( kx1(np), ky1(np), kx2(np), ky2(np), t%poly_id(np) )
    p = 0
    do r = 1, nid
       if( amin(r) == 0 ) cycle
       p = p + 1
       t%poly_id(p) = r
       kx1(p) = boxes(amin(r))%x1; ky1(p) = boxes(amin(r))%y1
       kx2(p) = boxes(amin(r))%x2; ky2(p) = boxes(amin(r))%y2
    end do
    call sort_perm4( kx1, ky1, kx2, ky2, perm )
    !> permute through kx1 (no longer needed): `poly_id = poly_id(perm)` aliases and makes ifx
    !> build an n-element stack temporary (SIGSEGV with an 8 MB stack above ~1M polygons)
    do p = 1, np
       kx1(p) = t%poly_id( perm(p) )
    end do
    do p = 1, np
       t%poly_id(p) = kx1(p)
    end do
    deallocate( kx1, ky1, kx2, ky2, perm )

    allocate( t%id_ord(nid), source=0_int64 )
    do p = 1, np
       t%id_ord( t%poly_id(p) ) = p
    end do
    t%n_poly = np
    deallocate( t%poly_start )
    allocate( t%poly_start(np+1), source=0_int64 )
    do i = 1, n
       t%box_ord(i) = t%id_ord( root(i) )
       t%poly_start( t%box_ord(i) + 1 ) = t%poly_start( t%box_ord(i) + 1 ) + 1
    end do
    t%poly_start(1) = 1
    do p = 1, np
       t%poly_start(p+1) = t%poly_start(p+1) + t%poly_start(p)
    end do
    allocate( t%poly_boxes(n), fill(np) )
    fill = t%poly_start(1:np)
    do i = 1, n
       p = t%box_ord(i)
       t%poly_boxes( fill(p) ) = i
       fill(p) = fill(p) + 1
    end do
    deallocate( fill, root, amin )

    allocate( t%bbox(np), t%area(np) )
    ! no associate inside the parallel loop: nvfortran 26.1 outlines it with a bad descriptor (SIGSEGV)
    !$omp parallel do private(i, bi) schedule(dynamic,256)
    do p = 1, np
       t%bbox(p) = boxes( t%poly_boxes( t%poly_start(p) ) )
       t%area(p) = 0
       do i = t%poly_start(p), t%poly_start(p+1) - 1
          bi = t%poly_boxes(i)
          t%bbox(p)%x1 = min( t%bbox(p)%x1, boxes(bi)%x1 ); t%bbox(p)%y1 = min( t%bbox(p)%y1, boxes(bi)%y1 )
          t%bbox(p)%x2 = max( t%bbox(p)%x2, boxes(bi)%x2 ); t%bbox(p)%y2 = max( t%bbox(p)%y2, boxes(bi)%y2 )
          t%area(p) = t%area(p) + int( boxes(bi)%x2 - boxes(bi)%x1, int64 ) * int( boxes(bi)%y2 - boxes(bi)%y1, int64 )
       end do
    end do
    !$omp end parallel do
    t%n_rect = count( t%poly_start(2:np+1) - t%poly_start(1:np) == 1 )
  end subroutine PropTableIndex

  pure logical function box_less( a, b )
    type(Box), intent(in) :: a, b
    if( a%x1 /= b%x1 ) then
       box_less = a%x1 < b%x1
    else if( a%y1 /= b%y1 ) then
       box_less = a%y1 < b%y1
    else if( a%x2 /= b%x2 ) then
       box_less = a%x2 < b%x2
    else
       box_less = a%y2 < b%y2
    end if
  end function box_less

  !> Stable bottom-up merge sort of 1..n by the key tuple (a,b,c,d).
  subroutine sort_perm4( a, b, c, d, perm )
    integer(int64), intent(in) :: a(:), b(:), c(:), d(:)
    integer(int64), allocatable, intent(out) :: perm(:)
    integer(int64), allocatable :: tmp(:)
    integer(int64) :: n, width, lo, mid, hi, i, j, k
    n = size(a, kind=int64)
    allocate( perm(n), tmp(n) )
    do i = 1, n                      !> explicit loop: an array constructor is a stack temporary
       perm(i) = i
    end do
    width = 1
    do while( width < n )
       lo = 1
       do while( lo <= n )
          mid = min( lo + width - 1, n )
          hi  = min( lo + 2*width - 1, n )
          i = lo; j = mid + 1; k = lo
          do while( i <= mid .and. j <= hi )
             if( key_less( perm(j), perm(i) ) ) then
                tmp(k) = perm(j); j = j + 1
             else
                tmp(k) = perm(i); i = i + 1
             end if
             k = k + 1
          end do
          do while( i <= mid )
             tmp(k) = perm(i); i = i + 1; k = k + 1
          end do
          do while( j <= hi )
             tmp(k) = perm(j); j = j + 1; k = k + 1
          end do
          lo = lo + 2*width
       end do
       perm = tmp
       width = 2*width
    end do
  contains
    pure logical function key_less( x, y )
      integer(int64), intent(in) :: x, y
      if( a(x) /= a(y) ) then
         key_less = a(x) < a(y)
      else if( b(x) /= b(y) ) then
         key_less = b(x) < b(y)
      else if( c(x) /= c(y) ) then
         key_less = c(x) < c(y)
      else
         key_less = d(x) < d(y)
      end if
    end function key_less
  end subroutine sort_perm4

  subroutine drop_index( t )
    type(PropertyTable), intent(inout) :: t
    if( allocated(t%box_ord) )    deallocate( t%box_ord )
    if( allocated(t%poly_id) )    deallocate( t%poly_id )
    if( allocated(t%id_ord) )     deallocate( t%id_ord )
    if( allocated(t%poly_start) ) deallocate( t%poly_start )
    if( allocated(t%poly_boxes) ) deallocate( t%poly_boxes )
    if( allocated(t%bbox) )       deallocate( t%bbox )
    if( allocated(t%area) )       deallocate( t%area )
  end subroutine drop_index

  subroutine drop_columns( t )
    type(PropertyTable), intent(inout) :: t
    if( allocated(t%cols) ) deallocate( t%cols )
    t%n_cols = 0
  end subroutine drop_columns

  subroutine PropDropOutline( t )
    type(PropertyTable), intent(inout) :: t
    if( allocated(t%perim) )  deallocate( t%perim )
    if( allocated(t%nvert) )  deallocate( t%nvert )
    if( allocated(t%le_w) )   deallocate( t%le_w )
    if( allocated(t%le_len) ) deallocate( t%le_len )
    if( allocated(t%le_dir) ) deallocate( t%le_dir )
    t%have_outline = .false.
    t%n_outline_fail = 0
  end subroutine PropDropOutline

  subroutine PropNetNamesSet( rec, def_file, lab )
    integer, intent(in) :: rec
    character(len=*), intent(in) :: def_file
    type(PropString), allocatable, intent(inout) :: lab(:)
    type(NetNameSet), allocatable :: tmp(:)
    integer :: k
    if( .not. allocated( net_names ) ) allocate( net_names(max(8, rec)) )
    if( size(net_names) < rec ) then
       allocate( tmp(max(rec, 2*size(net_names))) )
       do k = 1, size(net_names)
          call move_set( net_names(k), tmp(k) )
       end do
       call move_alloc( tmp, net_names )
    end if
    net_names(rec)%have = .true.
    net_names(rec)%def_file = def_file
    call move_alloc( lab, net_names(rec)%lab )
  contains
    subroutine move_set( a, b )
      type(NetNameSet), intent(inout) :: a, b
      b%have = a%have
      if( allocated( a%def_file ) ) call move_alloc( a%def_file, b%def_file )
      if( allocated( a%lab ) ) call move_alloc( a%lab, b%lab )
    end subroutine move_set
  end subroutine PropNetNamesSet

  subroutine PropNetNamesDrop( rec )
    integer, intent(in) :: rec
    if( .not. allocated( net_names ) ) return
    if( rec < 1 .or. rec > size(net_names) ) return
    net_names(rec)%have = .false.
    if( allocated( net_names(rec)%lab ) ) deallocate( net_names(rec)%lab )
  end subroutine PropNetNamesDrop

  logical function PropNetNamesHave( rec ) result(h)
    integer, intent(in) :: rec
    h = .false.
    if( .not. allocated( net_names ) ) return
    if( rec < 1 .or. rec > size(net_names) ) return
    h = net_names(rec)%have
  end function PropNetNamesHave

  function PropNetName( rec, net ) result(s)
    integer, intent(in) :: rec
    integer(int64), intent(in) :: net
    character(len=:), allocatable :: s
    s = ''
    if( .not. PropNetNamesHave( rec ) ) return
    if( net < 1 .or. net > size( net_names(rec)%lab, kind=int64 ) ) return
    if( allocated( net_names(rec)%lab(net)%s ) ) s = net_names(rec)%lab(net)%s
  end function PropNetName

  subroutine PropDropNets( t )
    type(PropertyTable), intent(inout) :: t
    if( allocated(t%net) ) deallocate( t%net )
    t%net_rec = 0
    t%n_nets = 0
  end subroutine PropDropNets

  subroutine PropTableClear( t )
    type(PropertyTable), intent(inout) :: t
    call drop_index( t )
    call drop_columns( t )
    call PropDropOutline( t )
    call PropDropNets( t )
    t%n_poly = 0
    t%n_rect = 0
    t%geom_n = -1
    t%geom_hash = 0
  end subroutine PropTableClear

  !> Polygon id (PNum root) -> ordinal; 0 when id is not a box of this layer.
  pure integer(int64) function PropOrdinalOfId( t, id ) result(ord)
    type(PropertyTable), intent(in) :: t
    integer(int64), intent(in)      :: id
    ord = 0
    if( .not. allocated(t%id_ord) ) return
    if( id < 1 .or. id > size(t%id_ord, kind=int64) ) return
    ord = t%id_ord(id)
  end function PropOrdinalOfId

  pure integer function PropColumnIndex( t, name ) result(idx)
    type(PropertyTable), intent(in) :: t
    character(len=*), intent(in)    :: name
    integer :: k
    idx = 0
    do k = 1, t%n_cols
       if( t%cols(k)%name == name ) then
          idx = k
          return
       end if
    end do
  end function PropColumnIndex

  pure function PropKindName( kind ) result(s)
    integer, intent(in) :: kind
    character(len=:), allocatable :: s
    select case( kind )
    case( PV_INT );    s = 'INT'
    case( PV_FLOAT );  s = 'FLOAT'
    case( PV_STRING ); s = 'STRING'
    case( PV_STRUCT ); s = 'STRUCT'
    case( PV_ERROR );  s = 'ERROR'
    case default;      s = 'NONE'
    end select
  end function PropKindName

  !=================================================================
  ! Commit vals(1:n_poly) as column `name` (replacing any column of that name).
  ! Column type: STRING if any value is a string, else FLOAT if any float, else INT;
  ! STRUCT columns take the union of field names (first-seen order), typed per field.
  ! Mixing STRUCT and scalar values, or strings and numbers, is an error (status 1).
  !=================================================================
  subroutine PropCommitColumn( t, name, vals, status, msg )
    type(PropertyTable), intent(inout)        :: t
    character(len=*), intent(in)              :: name
    type(PropValue), intent(in)               :: vals(:)
    integer, intent(out)                      :: status
    character(len=:), allocatable, intent(out) :: msg
    type(PropColumn) :: col
    type(PropString), allocatable :: fnames(:), tmpn(:)
    integer :: nf, k, j, idx
    integer(int64) :: p, np, nstruct, nscalar
    type(PropColumn), allocatable :: grown(:)

    status = 0
    msg = ''
    np = t%n_poly
    if( size(vals, kind=int64) /= np ) then
       status = 1; msg = 'internal: value count /= polygon count'; return
    end if
    nstruct = count( vals(:)%kind == PV_STRUCT )
    nscalar = count( vals(:)%kind == PV_INT .or. vals(:)%kind == PV_FLOAT .or. vals(:)%kind == PV_STRING )
    if( nstruct > 0 .and. nscalar > 0 ) then
       status = 1; msg = 'property '//name//' mixes STRUCT and scalar values'; return
    end if
    col%name = name
    if( nstruct > 0 ) then
       col%kind = PV_STRUCT
       allocate( col%d%present(np) )
       col%d%name = name
       col%d%kind = PV_STRUCT
       col%d%present = ( vals(:)%kind == PV_STRUCT )
       nf = 0
       allocate( fnames(8) )
       do p = 1, np
          if( vals(p)%kind /= PV_STRUCT ) cycle
          if( .not. allocated( vals(p)%f ) ) cycle
          do k = 1, size( vals(p)%f )
             idx = 0
             do j = 1, nf
                if( fnames(j)%s == vals(p)%f(k)%name ) then
                   idx = j; exit
                end if
             end do
             if( idx == 0 ) then
                nf = nf + 1
                if( nf > size(fnames) ) then
                   call PropStringsGrow( fnames, 2*size(fnames), nf-1 )
                end if
                fnames(nf)%s = vals(p)%f(k)%name
             end if
          end do
       end do
       allocate( col%fields(nf) )
       do j = 1, nf
          call build_field_data( fnames(j)%s, col%fields(j), status, msg )
          if( status /= 0 ) return
       end do
    else
       call build_scalar_data( col%d, status, msg )
       if( status /= 0 ) return
       col%kind = col%d%kind
    end if

    idx = PropColumnIndex( t, name )
    if( idx == 0 ) then
       if( .not. allocated( t%cols ) ) allocate( t%cols(4) )
       if( t%n_cols == size( t%cols ) ) then
          allocate( grown( 2*size(t%cols) ) )
          do k = 1, t%n_cols
             call move_column( t%cols(k), grown(k) )
          end do
          call move_alloc( grown, t%cols )
       end if
       t%n_cols = t%n_cols + 1
       idx = t%n_cols
    end if
    call move_column( col, t%cols(idx) )
  contains
    subroutine build_scalar_data( d, st, m )
      type(PropData), intent(inout) :: d
      integer, intent(out) :: st
      character(len=:), allocatable, intent(out) :: m
      integer(int64) :: q, nstr, nflt, nint
      st = 0; m = ''
      d%name = name
      nstr = count( vals(:)%kind == PV_STRING )
      nflt = count( vals(:)%kind == PV_FLOAT )
      nint = count( vals(:)%kind == PV_INT )
      allocate( d%present(np) )
      d%present = ( vals(:)%kind == PV_INT .or. vals(:)%kind == PV_FLOAT .or. vals(:)%kind == PV_STRING )
      if( nstr > 0 .and. nstr /= nflt + nint + nstr ) then
         st = 1; m = 'property '//name//' mixes STRING and numeric values'; return
      end if
      if( nstr > 0 ) then
         d%kind = PV_STRING
         allocate( d%sv(np) )
         do q = 1, np
            if( d%present(q) ) then
               d%sv(q)%s = vals(q)%s
            else
               d%sv(q)%s = ''
            end if
         end do
      else if( nflt > 0 ) then
         d%kind = PV_FLOAT
         allocate( d%rv(np), source=0.0_real64 )
         do q = 1, np
            if( vals(q)%kind == PV_FLOAT ) d%rv(q) = vals(q)%r
            if( vals(q)%kind == PV_INT )   d%rv(q) = real( vals(q)%i, real64 )
         end do
      else
         d%kind = PV_INT
         allocate( d%iv(np), source=0_int64 )
         do q = 1, np
            if( vals(q)%kind == PV_INT ) d%iv(q) = vals(q)%i
         end do
      end if
    end subroutine build_scalar_data

    subroutine build_field_data( fname, d, st, m )
      character(len=*), intent(in) :: fname
      type(PropData), intent(inout) :: d
      integer, intent(out) :: st
      character(len=:), allocatable, intent(out) :: m
      integer(int64) :: q
      integer :: kk, nstr, nnum, nflt
      integer, allocatable :: fk(:)
      st = 0; m = ''
      d%name = fname
      allocate( d%present(np), source=.false. )
      allocate( fk(np), source=0 )
      do q = 1, np
         if( vals(q)%kind /= PV_STRUCT ) cycle
         if( .not. allocated( vals(q)%f ) ) cycle
         do kk = 1, size( vals(q)%f )
            if( vals(q)%f(kk)%name == fname ) then
               fk(q) = kk
               d%present(q) = vals(q)%f(kk)%kind /= PV_NONE
               exit
            end if
         end do
      end do
      nstr = 0; nnum = 0; nflt = 0
      do q = 1, np
         if( fk(q) == 0 ) cycle
         select case( vals(q)%f(fk(q))%kind )
         case( PV_STRING ); nstr = nstr + 1
         case( PV_FLOAT );  nflt = nflt + 1; nnum = nnum + 1
         case( PV_INT );    nnum = nnum + 1
         end select
      end do
      if( nstr > 0 .and. nnum > 0 ) then
         st = 1; m = 'struct field '//name//'.'//fname//' mixes STRING and numeric values'; return
      end if
      if( nstr > 0 ) then
         d%kind = PV_STRING
         allocate( d%sv(np) )
         do q = 1, np
            d%sv(q)%s = ''
            if( fk(q) > 0 ) then
               if( allocated( vals(q)%f(fk(q))%s ) ) d%sv(q)%s = vals(q)%f(fk(q))%s
            end if
         end do
      else if( nflt > 0 ) then
         d%kind = PV_FLOAT
         allocate( d%rv(np), source=0.0_real64 )
         do q = 1, np
            if( fk(q) == 0 ) cycle
            if( vals(q)%f(fk(q))%kind == PV_FLOAT ) d%rv(q) = vals(q)%f(fk(q))%r
            if( vals(q)%f(fk(q))%kind == PV_INT )   d%rv(q) = real( vals(q)%f(fk(q))%i, real64 )
         end do
      else
         d%kind = PV_INT
         allocate( d%iv(np), source=0_int64 )
         do q = 1, np
            if( fk(q) == 0 ) cycle
            d%iv(q) = vals(q)%f(fk(q))%i
         end do
      end if
    end subroutine build_field_data
  end subroutine PropCommitColumn

  subroutine move_data( a, b )
    type(PropData), intent(inout) :: a, b
    call move_alloc( a%name, b%name )
    b%kind = a%kind
    call move_alloc( a%present, b%present )
    call move_alloc( a%iv, b%iv )
    call move_alloc( a%rv, b%rv )
    call move_alloc( a%sv, b%sv )
  end subroutine move_data

  subroutine move_column( a, b )
    type(PropColumn), intent(inout) :: a, b
    call move_alloc( a%name, b%name )
    b%kind = a%kind
    call move_data( a%d, b%d )
    if( allocated( b%fields ) ) deallocate( b%fields )
    call move_alloc( a%fields, b%fields )
  end subroutine move_column

  !> Scalar read of column c at ordinal ord (STRUCT columns give the whole struct).
  function PropGetValue( t, c, ord ) result(v)
    type(PropertyTable), intent(in) :: t
    integer, intent(in)             :: c
    integer(int64), intent(in)      :: ord
    type(PropValue) :: v
    integer :: j, nf
    if( c < 1 .or. c > t%n_cols .or. ord < 1 .or. ord > t%n_poly ) return
    associate( col => t%cols(c) )
      if( .not. col%d%present(ord) ) return
      if( col%kind == PV_STRUCT ) then
         v%kind = PV_STRUCT
         nf = 0
         do j = 1, size( col%fields )
            if( col%fields(j)%present(ord) ) nf = nf + 1
         end do
         allocate( v%f(nf) )
         nf = 0
         do j = 1, size( col%fields )
            if( .not. col%fields(j)%present(ord) ) cycle
            nf = nf + 1
            v%f(nf)%name = col%fields(j)%name
            v%f(nf)%kind = col%fields(j)%kind
            select case( col%fields(j)%kind )
            case( PV_INT );    v%f(nf)%i = col%fields(j)%iv(ord)
            case( PV_FLOAT );  v%f(nf)%r = col%fields(j)%rv(ord)
            case( PV_STRING ); v%f(nf)%s = col%fields(j)%sv(ord)%s
            end select
         end do
      else
         call data_value( col%d, ord, v )
      end if
    end associate
  end function PropGetValue

  !> Read field `fname` of STRUCT column c (kind PV_NONE when absent).
  function PropGetField( t, c, fname, ord ) result(v)
    type(PropertyTable), intent(in) :: t
    integer, intent(in)             :: c
    character(len=*), intent(in)    :: fname
    integer(int64), intent(in)      :: ord
    type(PropValue) :: v
    integer :: j
    if( c < 1 .or. c > t%n_cols .or. ord < 1 .or. ord > t%n_poly ) return
    if( t%cols(c)%kind /= PV_STRUCT ) return
    do j = 1, size( t%cols(c)%fields )
       if( t%cols(c)%fields(j)%name == fname ) then
          call data_value( t%cols(c)%fields(j), ord, v )
          return
       end if
    end do
  end function PropGetField

  subroutine data_value( d, ord, v )
    type(PropData), intent(in)     :: d
    integer(int64), intent(in)     :: ord
    type(PropValue), intent(inout) :: v
    if( .not. d%present(ord) ) return
    v%kind = d%kind
    select case( d%kind )
    case( PV_INT );    v%i = d%iv(ord)
    case( PV_FLOAT );  v%r = d%rv(ord)
    case( PV_STRING ); v%s = d%sv(ord)%s
    end select
  end subroutine data_value

  !> Text form of a value: INT as integer, FLOAT with 17 significant digits,
  !> STRING raw, STRUCT as {a=1;b=x}.
  recursive function PropValueToString( v ) result(s)
    type(PropValue), intent(in) :: v
    character(len=:), allocatable :: s
    character(len=40) :: buf
    integer :: k
    select case( v%kind )
    case( PV_INT )
       write(buf,'(I0)') v%i; s = trim(buf)
    case( PV_FLOAT )
       write(buf,'(ES24.16E3)') v%r; s = trim(adjustl(buf))
    case( PV_STRING, PV_ERROR )
       s = v%s
    case( PV_STRUCT )
       s = '{'
       if( allocated( v%f ) ) then
          do k = 1, size( v%f )
             if( k > 1 ) s = s // ';'
             s = s // v%f(k)%name // '=' // PropValueToString( field_as_value( v%f(k) ) )
          end do
       end if
       s = s // '}'
    case default
       s = ''
    end select
  end function PropValueToString

  function field_as_value( f ) result(v)
    type(PropField), intent(in) :: f
    type(PropValue) :: v
    v%kind = f%kind; v%i = f%i; v%r = f%r
    if( allocated( f%s ) ) v%s = f%s
  end function field_as_value

  subroutine PropTablePrint( t, lname )
    type(PropertyTable), intent(in) :: t
    character(len=*), intent(in)    :: lname
    integer :: k, j
    write(*,'(A,A,A,I0,A,I0,A,I0,A,I0)') 'PROPERTY TABLE ', lname, ': polygons=', t%n_poly, &
         ' (single-rect=', t%n_rect, ', multi-box=', t%n_poly - t%n_rect, ') columns=', t%n_cols
    if( t%have_outline ) write(*,'(A,I0,A)') '  outline measures cached (', t%n_outline_fail, ' trace failures)'
    if( t%net_rec > 0 .and. allocated(t%net) ) write(*,'(A,I0,A,I0,A)') '  nets: CONNECT #', t%net_rec, &
         ', ', t%n_nets, ' nets in the connected stack'
    do k = 1, t%n_cols
       associate( c => t%cols(k) )
         write(*,'(A,A,A,A,A,I0)') '  ', c%name, ' : ', PropKindName(c%kind), ' set on ', count( c%d%present )
         select case( c%kind )
         case( PV_INT )
            if( any( c%d%present ) ) write(*,'(A,I0,A,I0)') '    min=', minval( c%d%iv, mask=c%d%present ), &
                 ' max=', maxval( c%d%iv, mask=c%d%present )
         case( PV_FLOAT )
            if( any( c%d%present ) ) write(*,'(A,ES14.6,A,ES14.6)') '    min=', minval( c%d%rv, mask=c%d%present ), &
                 ' max=', maxval( c%d%rv, mask=c%d%present )
         case( PV_STRUCT )
            do j = 1, size( c%fields )
               write(*,'(A,A,A,A,A,I0)') '    .', c%fields(j)%name, ' : ', PropKindName( c%fields(j)%kind ), &
                    ' set on ', count( c%fields(j)%present )
            end do
         end select
       end associate
    end do
  end subroutine PropTablePrint

  !> CSV: ordinal,id,nboxes,area,x1,y1,x2,y2,<columns...>; STRUCT columns expand to
  !> NAME.field; absent values are empty cells; strings are double-quoted.
  subroutine PropTableDumpCSV( t, fname, status )
    type(PropertyTable), intent(in) :: t
    character(len=*), intent(in)    :: fname
    integer, intent(out)            :: status
    integer :: u, k, j
    integer(int64) :: p
    character(len=:), allocatable :: line
    character(len=200) :: buf
    open( newunit=u, file=fname, status='replace', action='write', iostat=status )
    if( status /= 0 ) return
    line = 'ordinal,id,nboxes,area,x1,y1,x2,y2'
    do k = 1, t%n_cols
       if( t%cols(k)%kind == PV_STRUCT ) then
          do j = 1, size( t%cols(k)%fields )
             line = line // ',' // t%cols(k)%name // '.' // t%cols(k)%fields(j)%name
          end do
       else
          line = line // ',' // t%cols(k)%name
       end if
    end do
    write(u,'(A)') line
    do p = 1, t%n_poly
       write(buf,'(I0,",",I0,",",I0,",",I0,",",I0,",",I0,",",I0,",",I0)') p, t%poly_id(p), &
            t%poly_start(p+1) - t%poly_start(p), t%area(p), t%bbox(p)%x1, t%bbox(p)%y1, t%bbox(p)%x2, t%bbox(p)%y2
       line = trim(buf)
       do k = 1, t%n_cols
          if( t%cols(k)%kind == PV_STRUCT ) then
             do j = 1, size( t%cols(k)%fields )
                line = line // ',' // cell( t%cols(k)%fields(j), p )
             end do
          else
             line = line // ',' // cell( t%cols(k)%d, p )
          end if
       end do
       write(u,'(A)') line
    end do
    close(u)
  contains
    function cell( d, q ) result(s)
      type(PropData), intent(in) :: d
      integer(int64), intent(in) :: q
      character(len=:), allocatable :: s
      type(PropValue) :: v
      integer :: i
      s = ''
      if( .not. d%present(q) ) return
      call data_value( d, q, v )
      if( d%kind == PV_STRING ) then
         s = '"'
         do i = 1, len( v%s )
            if( v%s(i:i) == '"' ) then
               s = s // '""'
            else
               s = s // v%s(i:i)
            end if
         end do
         s = s // '"'
      else
         s = PropValueToString( v )
      end if
    end function cell
  end subroutine PropTableDumpCSV

end module PropertyTableModule
