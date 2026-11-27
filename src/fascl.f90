! File   : fascl.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: FASCL (FASt Calculation of Lambdas on Coupling Length) for the CTR language.
!
! FASCL was first written in Lisp (2000-2001): find externally facing edges that meet a
! spacing criterion, then call a user lambda on each candidate (condition function +
! user function, results accumulated per net).  This module brings the lambda engine
! into MAGPARSER: a CTR statement attaches per-polygon properties computed by a lambda.
!
!   PROPERTY:PNUM = d1:met1 FASCL [ (id) PNUM = POLYGON_NUMBER(id) ] d1:nothing
!
! (also accepted after `exec run`).  The lambda runs once for every polygon of d1:met1;
! `id` is the polygon id from the PNum pass.  Names on the left of '=' (comma separated)
! are the properties written to the layer's PropertyTable; other assigned names are
! locals.  The second operand is the coupling partner layer of the (future) spacing
! stage; unary lambdas take `nothing` there.
!
! Lambda language (case-sensitive names, keywords upper case):
!   lambda := '[' '(' param {',' param} ')' stmt {';' stmt} ']'
!   stmt   := NAME '=' expr | NAME '.' FIELD '=' expr | 'WHEN' expr | expr
!             (a lone expr is allowed when exactly one property is defined; it is the value)
!   expr   := || && == != < <= > >= + - * / % unary - ! with usual precedence, ( ), literals
!             123  1.5  2.0e-3  "text"  , names, NAME.field, calls, STRUCT(f=expr, ...)
!   geometry of a polygon id:  POLYGON_NUMBER POLYGON_ID AREA NUM_BOXES BBOX_X1 BBOX_Y1
!                              BBOX_X2 BBOX_Y2 BBOX_WIDTH BBOX_HEIGHT
!                              PERIMETER VERTICES LINE_END_DIR LINE_END_WIDTH LINE_END_LENGTH
!                              NET_ID (net of the polygon from a CONNECT statement)
!                              NET_NAME (DEF net name of that net, after exec defcheck)
!   values: INT FLOAT STRING ABS SQRT POW MIN MAX IF(c,a,b)
!   existing property of the layer:  NAME(id)  and  NAME.field(id)
!   '/' always gives FLOAT, '%' is integer modulo, '+' concatenates when a STRING is involved.
!   WHEN c : the polygon gets no value for any defined property unless c is true.
module FASCLModule
  use iso_fortran_env, only: int8, int32, int64, real64
  use, intrinsic :: iso_c_binding, only: c_int64_t, c_int32_t
  use DesignModule
  use PNumMergeModule
  use PropertyTableModule
  use CommonModule, only: K_LEAF_CAPACITY
  implicit none
  private
  public :: FASCLExecute, FASCLParseStatement, LayerEnsureProperties, FASCLRunLambda, PropEnsureOutline

  interface
     !> db/prop_oasis_writer.cpp: perimeter, vertices, narrowest line end of one polygon
     function fpo_poly_geom( nbox, boxes, out, le_dir ) bind(C, name='fpo_poly_geom') result(st)
       import :: c_int64_t, c_int32_t
       integer(c_int64_t), value :: nbox
       integer(c_int64_t), intent(in) :: boxes(*)
       integer(c_int64_t), intent(out) :: out(4)
       integer(c_int32_t), intent(out) :: le_dir
       integer(c_int32_t) :: st
     end function fpo_poly_geom
  end interface

  ! ---- tokens
  integer, parameter :: T_EOF = 0, T_ID = 1, T_INT = 2, T_FLT = 3, T_STR = 4, T_OP = 5
  type :: Token
     integer :: kind = T_EOF
     character(len=:), allocatable :: s
     integer(int64) :: iv = 0
     real(real64)   :: rv = 0.0_real64
  end type Token

  ! ---- AST
  integer, parameter :: N_INT = 1, N_FLT = 2, N_STR = 3, N_PARAM = 4, N_LOCAL = 5, N_BIN = 6, &
       N_NEG = 7, N_NOT = 8, N_CALL = 9, N_PROP = 10, N_PROPF = 11, N_FIELD = 12, N_STRUCT = 13
  integer, parameter :: B_ADD = 1, B_SUB = 2, B_MUL = 3, B_DIV = 4, B_MOD = 5, B_EQ = 6, B_NE = 7, &
       B_LT = 8, B_LE = 9, B_GT = 10, B_GE = 11, B_AND = 12, B_OR = 13
  integer, parameter :: F_PNUM = 1, F_PID = 2, F_AREA = 3, F_NBOX = 4, F_BX1 = 5, F_BY1 = 6, F_BX2 = 7, &
       F_BY2 = 8, F_BW = 9, F_BH = 10, F_INT = 11, F_FLOAT = 12, F_STRING = 13, F_ABS = 14, F_SQRT = 15, &
       F_POW = 16, F_MIN = 17, F_MAX = 18, F_IF = 19, F_PERIM = 20, F_NVERT = 21, F_LEDIR = 22, &
       F_LELEN = 23, F_LEW = 24, F_NET = 25, F_NETNAME = 26
  type :: Node
     integer :: op = 0, fn = 0, slot = 0, col = 0
     integer(int64) :: iv = 0
     real(real64)   :: rv = 0.0_real64
     character(len=:), allocatable :: sv
     integer :: a = 0, b = 0
     integer, allocatable :: args(:)
     type(PropString), allocatable :: names(:)
  end type Node

  integer, parameter :: S_ASSIGN = 1, S_FASSIGN = 2, S_WHEN = 3
  type :: Stmt
     integer :: kind = 0, slot = 0, expr = 0
     character(len=:), allocatable :: field
  end type Stmt

  type, public :: Lambda
     integer :: nparams = 0
     type(PropString), allocatable :: params(:)
     integer :: nslots = 0
     type(PropString), allocatable :: slots(:)       !> local / property names
     integer :: nnodes = 0
     type(Node), allocatable :: nodes(:)
     integer :: nstmts = 0
     type(Stmt), allocatable :: stmts(:)
  end type Lambda

  !> Parser state
  type :: Parser
     type(Token), allocatable :: toks(:)
     integer :: ntok = 0, pos = 1
     integer :: status = 0
     character(len=:), allocatable :: msg
  end type Parser

  type :: ValueColumn
     type(PropValue), allocatable :: v(:)
  end type ValueColumn

  !> Plain POD runtime value (no allocatable components) so ifx does not emit
  !> static global RTTI/finalizer descriptor writes on every AST node inside
  !> the OpenMP polygon loop. STRING, STRUCT and ERROR payloads live in a
  !> thread-local ExtPool slot indexed by `ext` (> 0).
  type :: FastVal
     integer        :: kind = PV_NONE
     integer(int64) :: i = 0
     real(real64)   :: r = 0.0_real64
     integer        :: ext = 0
  end type FastVal

  type :: ExtPool
     type(PropValue), allocatable :: items(:)
     integer :: n = 0
  end type ExtPool

contains

  !=================================================================
  ! Statement splitter: "PROPERTY:A[,B] = db:layer FASCL [ ... ] db2:layer2"
  !=================================================================
  subroutine FASCLParseStatement( text, names, opA, lam_text, opB, status, msg )
    character(len=*), intent(in) :: text
    type(PropString), allocatable, intent(out) :: names(:)
    character(len=:), allocatable, intent(out) :: opA, lam_text, opB, msg
    integer, intent(out) :: status
    character(len=:), allocatable :: s, lhs, rhs
    integer :: p, q, depth, i, n
    logical :: in_str
    status = 1
    opA = ''; lam_text = ''; opB = ''; msg = ''
    s = trim(adjustl(text))
    if( len(s) >= 9 ) then
       if( s(1:9) == 'PROPERTY:' ) s = s(10:)
    end if
    p = index( s, '=' )
    if( p == 0 ) then
       msg = 'FASCL: expected PROPERTY:NAME = layer FASCL [ lambda ] layer'; allocate(names(0)); return
    end if
    lhs = trim(adjustl(s(1:p-1)))
    rhs = trim(adjustl(s(p+1:)))
    ! property names
    n = 1
    do i = 1, len(lhs)
       if( lhs(i:i) == ',' ) n = n + 1
    end do
    allocate( names(n) )
    q = 1
    do i = 1, n
       p = index( lhs(q:), ',' )
       if( p == 0 ) then
          names(i)%s = trim(adjustl(lhs(q:)))
       else
          names(i)%s = trim(adjustl(lhs(q:q+p-2)))
          q = q + p
       end if
       if( .not. valid_name( names(i)%s ) ) then
          msg = 'FASCL: invalid property name "'//names(i)%s//'"'; return
       end if
    end do
    ! operand A
    p = index( rhs, ' ' )
    if( p == 0 ) then
       msg = 'FASCL: missing FASCL keyword'; return
    end if
    opA = rhs(1:p-1)
    rhs = trim(adjustl(rhs(p+1:)))
    if( len(rhs) < 5 ) then
       msg = 'FASCL: missing FASCL keyword'; return
    end if
    if( rhs(1:5) /= 'FASCL' ) then
       msg = 'FASCL: expected FASCL after '//opA; return
    end if
    rhs = trim(adjustl(rhs(6:)))
    if( len(rhs) == 0 ) then
       msg = 'FASCL: missing lambda'; return
    end if
    if( rhs(1:1) /= '[' ) then
       msg = 'FASCL: lambda must start with ['; return
    end if
    depth = 0; in_str = .false.; q = 0
    i = 1
    do while( i <= len(rhs) )
       if( in_str ) then
          if( rhs(i:i) == achar(92) ) then
             i = i + 1
          else if( rhs(i:i) == '"' ) then
             in_str = .false.
          end if
       else
          select case( rhs(i:i) )
          case( '"' ); in_str = .true.
          case( '[' ); depth = depth + 1
          case( ']' )
             depth = depth - 1
             if( depth == 0 ) then
                q = i; exit
             end if
          end select
       end if
       i = i + 1
    end do
    if( q == 0 ) then
       msg = 'FASCL: unbalanced [ ] in lambda'; return
    end if
    lam_text = rhs(1:q)
    opB = trim(adjustl(rhs(q+1:)))
    if( len(opB) == 0 ) opB = 'nothing'
    p = index( opB, ' ' )
    if( p > 0 ) opB = opB(1:p-1)
    status = 0
  end subroutine FASCLParseStatement

  pure logical function valid_name( s )
    character(len=*), intent(in) :: s
    integer :: i
    valid_name = len(s) > 0
    if( .not. valid_name ) return
    valid_name = is_alpha( s(1:1) ) .or. s(1:1) == '_'
    do i = 2, len(s)
       if( .not. ( is_alpha(s(i:i)) .or. is_digit(s(i:i)) .or. s(i:i) == '_' ) ) valid_name = .false.
    end do
  end function valid_name

  pure logical function is_alpha( c )
    character, intent(in) :: c
    is_alpha = ( c >= 'a' .and. c <= 'z' ) .or. ( c >= 'A' .and. c <= 'Z' )
  end function is_alpha
  pure logical function is_digit( c )
    character, intent(in) :: c
    is_digit = c >= '0' .and. c <= '9'
  end function is_digit

  !=================================================================
  ! Lexer
  !=================================================================
  subroutine lex( src, p )
    character(len=*), intent(in) :: src
    type(Parser), intent(inout) :: p
    integer :: i, j, n
    character(len=:), allocatable :: buf
    logical :: is_flt
    integer :: ios
    allocate( p%toks(16) )
    p%ntok = 0
    n = len(src)
    i = 1
    do while( i <= n )
       if( src(i:i) == ' ' .or. src(i:i) == achar(9) ) then
          i = i + 1; cycle
       end if
       if( is_alpha(src(i:i)) .or. src(i:i) == '_' ) then
          j = i
          do while( j < n )
             if( .not. ( is_alpha(src(j+1:j+1)) .or. is_digit(src(j+1:j+1)) .or. src(j+1:j+1) == '_' ) ) exit
             j = j + 1
          end do
          call push( T_ID, src(i:j) )
          i = j + 1
       else if( is_digit(src(i:i)) .or. ( src(i:i) == '.' .and. i < n .and. is_digit(src(min(i+1,n):min(i+1,n))) ) ) then
          j = i; is_flt = .false.
          do while( j < n )
             if( is_digit(src(j+1:j+1)) ) then
                j = j + 1
             else if( src(j+1:j+1) == '.' .and. .not. is_flt ) then
                is_flt = .true.; j = j + 1
             else if( ( src(j+1:j+1) == 'e' .or. src(j+1:j+1) == 'E' ) .and. j+1 < n ) then
                if( is_digit(src(j+2:j+2)) ) then
                   is_flt = .true.; j = j + 1
                else if( ( src(j+2:j+2) == '-' .or. src(j+2:j+2) == '+' ) .and. j+2 < n ) then
                   if( .not. is_digit(src(j+3:j+3)) ) exit
                   is_flt = .true.; j = j + 2
                else
                   exit
                end if
             else
                exit
             end if
          end do
          if( is_flt ) then
             call push( T_FLT, src(i:j) )
             read( src(i:j), *, iostat=ios ) p%toks(p%ntok)%rv
          else
             call push( T_INT, src(i:j) )
             read( src(i:j), *, iostat=ios ) p%toks(p%ntok)%iv
          end if
          if( ios /= 0 ) then
             p%status = 1; p%msg = 'bad number '//src(i:j); return
          end if
          i = j + 1
       else if( src(i:i) == '"' ) then
          buf = ''
          j = i + 1
          do
             if( j > n ) then
                p%status = 1; p%msg = 'unterminated string'; return
             end if
             if( src(j:j) == achar(92) .and. j < n ) then
                buf = buf // src(j+1:j+1); j = j + 2; cycle
             end if
             if( src(j:j) == '"' ) exit
             buf = buf // src(j:j)
             j = j + 1
          end do
          call push( T_STR, buf )
          i = j + 1
       else
          if( i < n ) then
             select case( src(i:i+1) )
             case( '==', '!=', '<=', '>=', '&&', '||' )
                call push( T_OP, src(i:i+1) ); i = i + 2; cycle
             end select
          end if
          if( index( '()[],;=.+-*/%<>!', src(i:i) ) == 0 ) then
             p%status = 1; p%msg = 'unexpected character "'//src(i:i)//'"'; return
          end if
          call push( T_OP, src(i:i) )
          i = i + 1
       end if
    end do
    call push( T_EOF, '' )
  contains
    subroutine push( k, s )
      integer, intent(in) :: k
      character(len=*), intent(in) :: s
      type(Token), allocatable :: tmp(:)
      if( p%ntok == size(p%toks) ) then
         allocate( tmp( 2*size(p%toks) ) )
         tmp(1:p%ntok) = p%toks(1:p%ntok)
         call move_alloc( tmp, p%toks )
      end if
      p%ntok = p%ntok + 1
      p%toks(p%ntok)%kind = k
      p%toks(p%ntok)%s = s
    end subroutine push
  end subroutine lex

  logical function at( p, s )
    type(Parser), intent(in) :: p
    character(len=*), intent(in) :: s
    at = .false.
    if( p%toks(p%pos)%kind == T_OP .or. p%toks(p%pos)%kind == T_ID ) at = ( p%toks(p%pos)%s == s )
  end function at

  subroutine expect( p, s )
    type(Parser), intent(inout) :: p
    character(len=*), intent(in) :: s
    if( p%status /= 0 ) return
    if( at( p, s ) ) then
       p%pos = p%pos + 1
    else
       p%status = 1
       p%msg = 'expected "'//s//'" near "'//p%toks(p%pos)%s//'"'
    end if
  end subroutine expect

  !=================================================================
  ! Compiler: text -> Lambda
  !=================================================================
  subroutine FASCLCompile( src, defined, t, lam, status, msg )
    character(len=*), intent(in) :: src
    type(PropString), intent(in) :: defined(:)       !> properties this statement defines
    type(PropertyTable), intent(in) :: t             !> for NAME(id) reads of existing columns
    type(Lambda), intent(out) :: lam
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    type(Parser) :: p
    integer :: e, slot, k, save_pos
    character(len=:), allocatable :: nm, fld

    msg = ''
    p%msg = ''
    call lex( src, p )
    allocate( lam%params(4), lam%slots(8), lam%nodes(64), lam%stmts(8) )
    if( p%status == 0 ) call expect( p, '[' )
    if( p%status == 0 ) call expect( p, '(' )
    do while( p%status == 0 )
       if( p%toks(p%pos)%kind /= T_ID ) then
          p%status = 1; p%msg = 'lambda parameter name expected'; exit
       end if
       call add_param( p%toks(p%pos)%s )
       p%pos = p%pos + 1
       if( at( p, ',' ) ) then
          p%pos = p%pos + 1; cycle
       end if
       call expect( p, ')' ); exit
    end do
    do while( p%status == 0 )
       if( at( p, ']' ) ) exit
       if( at( p, 'WHEN' ) ) then
          p%pos = p%pos + 1
          call parse_expr( p, lam, t, e )
          if( p%status == 0 ) call add_stmt( S_WHEN, 0, e, '' )
       else if( p%toks(p%pos)%kind == T_ID .and. p%toks(p%pos+1)%kind == T_OP .and. &
            p%toks(min(p%pos+1,p%ntok))%s == '=' ) then
          nm = p%toks(p%pos)%s
          p%pos = p%pos + 2
          call parse_expr( p, lam, t, e )
          if( p%status == 0 ) then
             slot = slot_of( nm, .true. )
             call add_stmt( S_ASSIGN, slot, e, '' )
          end if
       else if( p%toks(p%pos)%kind == T_ID .and. at_offset( p, 1, '.' ) .and. &
            p%toks(min(p%pos+2,p%ntok))%kind == T_ID .and. at_offset( p, 3, '=' ) ) then
          nm = p%toks(p%pos)%s
          fld = p%toks(p%pos+2)%s
          p%pos = p%pos + 4
          call parse_expr( p, lam, t, e )
          if( p%status == 0 ) then
             slot = slot_of( nm, .true. )
             call add_stmt( S_FASSIGN, slot, e, fld )
          end if
       else
          save_pos = p%pos
          call parse_expr( p, lam, t, e )
          if( p%status == 0 ) then
             if( size(defined) /= 1 ) then
                p%status = 1
                p%msg = 'a bare expression is only allowed when exactly one property is defined'
             else
                slot = slot_of( defined(1)%s, .true. )
                call add_stmt( S_ASSIGN, slot, e, '' )
             end if
          end if
       end if
       if( p%status /= 0 ) exit
       if( at( p, ';' ) ) then
          p%pos = p%pos + 1
       else if( .not. at( p, ']' ) ) then
          p%status = 1; p%msg = 'expected ; or ] near "'//p%toks(p%pos)%s//'"'
       end if
    end do
    if( p%status == 0 ) call expect( p, ']' )
    if( p%status == 0 .and. p%toks(p%pos)%kind /= T_EOF ) then
       p%status = 1; p%msg = 'text after the closing ] of the lambda'
    end if
    if( p%status == 0 ) then
       if( lam%nparams /= 1 ) then
          p%status = 1
          p%msg = 'only unary lambdas (id) are supported; coupling lambdas (id1,id2) need the spacing stage'
       end if
    end if
    if( p%status == 0 ) then
       do k = 1, size(defined)
          if( slot_of( defined(k)%s, .false. ) == 0 ) then
             p%status = 1; p%msg = 'property '//defined(k)%s//' is never assigned in the lambda'; exit
          end if
       end do
    end if
    status = p%status
    if( status /= 0 ) msg = 'FASCL lambda: '//p%msg
  contains
    logical function at_offset( pp, off, s )
      type(Parser), intent(in) :: pp
      integer, intent(in) :: off
      character(len=*), intent(in) :: s
      at_offset = .false.
      if( pp%pos + off > pp%ntok ) return
      if( pp%toks(pp%pos+off)%kind /= T_OP ) return
      at_offset = pp%toks(pp%pos+off)%s == s
    end function at_offset
    subroutine add_param( s )
      character(len=*), intent(in) :: s
      type(PropString), allocatable :: tmp(:)
      if( lam%nparams == size(lam%params) ) then
         allocate( tmp( 2*size(lam%params) ) ); tmp(1:lam%nparams) = lam%params(1:lam%nparams)
         call move_alloc( tmp, lam%params )
      end if
      lam%nparams = lam%nparams + 1
      lam%params(lam%nparams)%s = s
    end subroutine add_param
    integer function slot_of( s, create )
      character(len=*), intent(in) :: s
      logical, intent(in) :: create
      type(PropString), allocatable :: tmp(:)
      integer :: k2
      do k2 = 1, lam%nslots
         if( lam%slots(k2)%s == s ) then
            slot_of = k2; return
         end if
      end do
      slot_of = 0
      if( .not. create ) return
      if( lam%nslots == size(lam%slots) ) then
         allocate( tmp( 2*size(lam%slots) ) ); tmp(1:lam%nslots) = lam%slots(1:lam%nslots)
         call move_alloc( tmp, lam%slots )
      end if
      lam%nslots = lam%nslots + 1
      lam%slots(lam%nslots)%s = s
      slot_of = lam%nslots
    end function slot_of
    subroutine add_stmt( kind, slot_, expr_, field_ )
      integer, intent(in) :: kind, slot_, expr_
      character(len=*), intent(in) :: field_
      type(Stmt), allocatable :: tmp(:)
      if( lam%nstmts == size(lam%stmts) ) then
         allocate( tmp( 2*size(lam%stmts) ) ); tmp(1:lam%nstmts) = lam%stmts(1:lam%nstmts)
         call move_alloc( tmp, lam%stmts )
      end if
      lam%nstmts = lam%nstmts + 1
      lam%stmts(lam%nstmts)%kind = kind
      lam%stmts(lam%nstmts)%slot = slot_
      lam%stmts(lam%nstmts)%expr = expr_
      lam%stmts(lam%nstmts)%field = field_
    end subroutine add_stmt
  end subroutine FASCLCompile

  integer function new_node( lam, op )
    type(Lambda), intent(inout) :: lam
    integer, intent(in) :: op
    type(Node), allocatable :: tmp(:)
    if( lam%nnodes == size(lam%nodes) ) then
       allocate( tmp( 2*size(lam%nodes) ) )
       tmp(1:lam%nnodes) = lam%nodes(1:lam%nnodes)
       call move_alloc( tmp, lam%nodes )
    end if
    lam%nnodes = lam%nnodes + 1
    lam%nodes(lam%nnodes)%op = op
    new_node = lam%nnodes
  end function new_node

  integer function bin_node( lam, code, a, b ) result(k)
    type(Lambda), intent(inout) :: lam
    integer, intent(in) :: code, a, b
    k = new_node( lam, N_BIN )
    lam%nodes(k)%fn = code
    lam%nodes(k)%a = a
    lam%nodes(k)%b = b
  end function bin_node

  recursive subroutine parse_expr( p, lam, t, e )
    type(Parser), intent(inout) :: p
    type(Lambda), intent(inout) :: lam
    type(PropertyTable), intent(in) :: t
    integer, intent(out) :: e
    integer :: r
    call parse_and( p, lam, t, e )
    do while( p%status == 0 .and. at( p, '||' ) )
       p%pos = p%pos + 1
       call parse_and( p, lam, t, r )
       e = bin_node( lam, B_OR, e, r )
    end do
  end subroutine parse_expr

  recursive subroutine parse_and( p, lam, t, e )
    type(Parser), intent(inout) :: p
    type(Lambda), intent(inout) :: lam
    type(PropertyTable), intent(in) :: t
    integer, intent(out) :: e
    integer :: r
    call parse_cmp( p, lam, t, e )
    do while( p%status == 0 .and. at( p, '&&' ) )
       p%pos = p%pos + 1
       call parse_cmp( p, lam, t, r )
       e = bin_node( lam, B_AND, e, r )
    end do
  end subroutine parse_and

  recursive subroutine parse_cmp( p, lam, t, e )
    type(Parser), intent(inout) :: p
    type(Lambda), intent(inout) :: lam
    type(PropertyTable), intent(in) :: t
    integer, intent(out) :: e
    integer :: r, code
    call parse_add( p, lam, t, e )
    do while( p%status == 0 )
       code = 0
       if( at( p, '==' ) ) code = B_EQ
       if( at( p, '!=' ) ) code = B_NE
       if( at( p, '<' ) )  code = B_LT
       if( at( p, '<=' ) ) code = B_LE
       if( at( p, '>' ) )  code = B_GT
       if( at( p, '>=' ) ) code = B_GE
       if( code == 0 ) exit
       p%pos = p%pos + 1
       call parse_add( p, lam, t, r )
       e = bin_node( lam, code, e, r )
    end do
  end subroutine parse_cmp

  recursive subroutine parse_add( p, lam, t, e )
    type(Parser), intent(inout) :: p
    type(Lambda), intent(inout) :: lam
    type(PropertyTable), intent(in) :: t
    integer, intent(out) :: e
    integer :: r, code
    call parse_mul( p, lam, t, e )
    do while( p%status == 0 )
       code = 0
       if( at( p, '+' ) ) code = B_ADD
       if( at( p, '-' ) ) code = B_SUB
       if( code == 0 ) exit
       p%pos = p%pos + 1
       call parse_mul( p, lam, t, r )
       e = bin_node( lam, code, e, r )
    end do
  end subroutine parse_add

  recursive subroutine parse_mul( p, lam, t, e )
    type(Parser), intent(inout) :: p
    type(Lambda), intent(inout) :: lam
    type(PropertyTable), intent(in) :: t
    integer, intent(out) :: e
    integer :: r, code
    call parse_unary( p, lam, t, e )
    do while( p%status == 0 )
       code = 0
       if( at( p, '*' ) ) code = B_MUL
       if( at( p, '/' ) ) code = B_DIV
       if( at( p, '%' ) ) code = B_MOD
       if( code == 0 ) exit
       p%pos = p%pos + 1
       call parse_unary( p, lam, t, r )
       e = bin_node( lam, code, e, r )
    end do
  end subroutine parse_mul

  recursive subroutine parse_unary( p, lam, t, e )
    type(Parser), intent(inout) :: p
    type(Lambda), intent(inout) :: lam
    type(PropertyTable), intent(in) :: t
    integer, intent(out) :: e
    integer :: a
    if( at( p, '-' ) ) then
       p%pos = p%pos + 1
       call parse_unary( p, lam, t, a )
       e = new_node( lam, N_NEG ); lam%nodes(e)%a = a
    else if( at( p, '!' ) ) then
       p%pos = p%pos + 1
       call parse_unary( p, lam, t, a )
       e = new_node( lam, N_NOT ); lam%nodes(e)%a = a
    else
       call parse_postfix( p, lam, t, e )
    end if
  end subroutine parse_unary

  recursive subroutine parse_postfix( p, lam, t, e )
    type(Parser), intent(inout) :: p
    type(Lambda), intent(inout) :: lam
    type(PropertyTable), intent(in) :: t
    integer, intent(out) :: e
    integer :: a
    call parse_primary( p, lam, t, e )
    do while( p%status == 0 .and. at( p, '.' ) )
       p%pos = p%pos + 1
       if( p%toks(p%pos)%kind /= T_ID ) then
          p%status = 1; p%msg = 'field name expected after .'; return
       end if
       a = e
       e = new_node( lam, N_FIELD )
       lam%nodes(e)%a = a
       lam%nodes(e)%sv = p%toks(p%pos)%s
       p%pos = p%pos + 1
    end do
  end subroutine parse_postfix

  recursive subroutine parse_primary( p, lam, t, e )
    type(Parser), intent(inout) :: p
    type(Lambda), intent(inout) :: lam
    type(PropertyTable), intent(in) :: t
    integer, intent(out) :: e
    character(len=:), allocatable :: nm, fld
    integer :: k, fn, nargs, a
    integer, allocatable :: args(:)
    type(PropString), allocatable :: fnames(:)
    e = 0
    if( p%status /= 0 ) return
    select case( p%toks(p%pos)%kind )
    case( T_INT )
       e = new_node( lam, N_INT ); lam%nodes(e)%iv = p%toks(p%pos)%iv; p%pos = p%pos + 1
    case( T_FLT )
       e = new_node( lam, N_FLT ); lam%nodes(e)%rv = p%toks(p%pos)%rv; p%pos = p%pos + 1
    case( T_STR )
       e = new_node( lam, N_STR ); lam%nodes(e)%sv = p%toks(p%pos)%s; p%pos = p%pos + 1
    case( T_OP )
       if( at( p, '(' ) ) then
          p%pos = p%pos + 1
          call parse_expr( p, lam, t, e )
          call expect( p, ')' )
       else
          p%status = 1; p%msg = 'unexpected "'//p%toks(p%pos)%s//'"'
       end if
    case( T_ID )
       nm = p%toks(p%pos)%s
       p%pos = p%pos + 1
       if( nm == 'STRUCT' ) then
          call expect( p, '(' )
          allocate( args(0), fnames(0) )
          do while( p%status == 0 .and. .not. at( p, ')' ) )
             if( p%toks(p%pos)%kind /= T_ID ) then
                p%status = 1; p%msg = 'STRUCT field name expected'; return
             end if
             fnames = [ fnames, PropString( p%toks(p%pos)%s ) ]
             p%pos = p%pos + 1
             call expect( p, '=' )
             call parse_expr( p, lam, t, a )
             args = [ args, a ]
             if( at( p, ',' ) ) p%pos = p%pos + 1
          end do
          call expect( p, ')' )
          e = new_node( lam, N_STRUCT )
          lam%nodes(e)%args = args
          lam%nodes(e)%names = fnames
          return
       end if
       ! NAME.field(id) : field of an existing STRUCT property
       fld = ''
       if( at( p, '.' ) .and. p%toks(p%pos+1)%kind == T_ID .and. p%pos + 2 <= p%ntok ) then
          if( p%toks(p%pos+2)%kind == T_OP .and. p%toks(p%pos+2)%s == '(' .and. &
               PropColumnIndex( t, nm ) > 0 ) then
             fld = p%toks(p%pos+1)%s
             p%pos = p%pos + 2
          end if
       end if
       if( at( p, '(' ) ) then
          p%pos = p%pos + 1
          allocate( args(0) )
          do while( p%status == 0 .and. .not. at( p, ')' ) )
             call parse_expr( p, lam, t, a )
             args = [ args, a ]
             if( at( p, ',' ) ) then
                p%pos = p%pos + 1
             else if( .not. at( p, ')' ) ) then
                p%status = 1; p%msg = 'expected , or ) in call to '//nm
             end if
          end do
          call expect( p, ')' )
          if( p%status /= 0 ) return
          nargs = size(args)
          fn = builtin( nm )
          if( fn > 0 .and. len(fld) == 0 ) then
             if( nargs /= arity( fn ) ) then
                p%status = 1; p%msg = nm//' takes '//itoa(arity(fn))//' argument(s)'; return
             end if
             e = new_node( lam, N_CALL )
             lam%nodes(e)%fn = fn
             lam%nodes(e)%args = args
          else
             k = PropColumnIndex( t, nm )
             if( k == 0 ) then
                p%status = 1; p%msg = 'unknown function or property "'//nm//'"'; return
             end if
             if( nargs /= 1 ) then
                p%status = 1; p%msg = 'property read '//nm//'(id) takes one argument'; return
             end if
             if( len(fld) > 0 ) then
                e = new_node( lam, N_PROPF ); lam%nodes(e)%sv = fld
             else
                e = new_node( lam, N_PROP )
             end if
             lam%nodes(e)%col = k
             lam%nodes(e)%a = args(1)
          end if
       else
          do k = 1, lam%nparams
             if( lam%params(k)%s == nm ) then
                e = new_node( lam, N_PARAM ); lam%nodes(e)%slot = k
                return
             end if
          end do
          do k = 1, lam%nslots
             if( lam%slots(k)%s == nm ) then
                e = new_node( lam, N_LOCAL ); lam%nodes(e)%slot = k
                return
             end if
          end do
          p%status = 1; p%msg = 'unknown name "'//nm//'" (not a parameter or an earlier assignment)'
       end if
    case default
       p%status = 1; p%msg = 'unexpected end of lambda'
    end select
  end subroutine parse_primary

  pure function itoa( i ) result(s)
    integer, intent(in) :: i
    character(len=:), allocatable :: s
    character(len=16) :: b
    write(b,'(I0)') i
    s = trim(b)
  end function itoa

  pure integer function builtin( nm )
    character(len=*), intent(in) :: nm
    select case( nm )
    case( 'POLYGON_NUMBER' ); builtin = F_PNUM
    case( 'POLYGON_ID' );     builtin = F_PID
    case( 'AREA' );           builtin = F_AREA
    case( 'NUM_BOXES', 'NUM_RECTS' ); builtin = F_NBOX
    case( 'BBOX_X1' );        builtin = F_BX1
    case( 'BBOX_Y1' );        builtin = F_BY1
    case( 'BBOX_X2' );        builtin = F_BX2
    case( 'BBOX_Y2' );        builtin = F_BY2
    case( 'BBOX_WIDTH' );     builtin = F_BW
    case( 'BBOX_HEIGHT' );    builtin = F_BH
    case( 'INT' );            builtin = F_INT
    case( 'FLOAT' );          builtin = F_FLOAT
    case( 'STRING' );         builtin = F_STRING
    case( 'ABS' );            builtin = F_ABS
    case( 'SQRT' );           builtin = F_SQRT
    case( 'POW' );            builtin = F_POW
    case( 'MIN' );            builtin = F_MIN
    case( 'MAX' );            builtin = F_MAX
    case( 'IF' );             builtin = F_IF
    case( 'PERIMETER' );      builtin = F_PERIM
    case( 'VERTICES', 'NUM_VERTICES' ); builtin = F_NVERT
    case( 'LINE_END_DIR' );   builtin = F_LEDIR
    case( 'LINE_END_LENGTH' ); builtin = F_LELEN
    case( 'LINE_END_WIDTH' ); builtin = F_LEW
    case( 'NET_ID' );         builtin = F_NET
    case( 'NET_NAME' );       builtin = F_NETNAME
    case default;             builtin = 0
    end select
  end function builtin

  pure integer function arity( fn )
    integer, intent(in) :: fn
    select case( fn )
    case( F_POW, F_MIN, F_MAX ); arity = 2
    case( F_IF );                arity = 3
    case default;                arity = 1
    end select
  end function arity

  !=================================================================
  ! ExtPool helpers (cold path: STRING / STRUCT / ERROR only)
  !=================================================================
  function ext_alloc( pool ) result(idx)
    type(ExtPool), intent(inout) :: pool
    integer :: idx, new_cap, j
    type(PropValue), allocatable :: tmp(:)
    if( .not. allocated( pool%items ) ) then
       allocate( pool%items(16) )
    else if( pool%n >= size( pool%items ) ) then
       new_cap = size( pool%items ) * 2
       allocate( tmp(new_cap) )
       do j = 1, pool%n
          tmp(j) = pool%items(j)
       end do
       call move_alloc( tmp, pool%items )
    end if
    pool%n = pool%n + 1
    idx = pool%n
    pool%items(idx)%kind = PV_NONE
    pool%items(idx)%i = 0
    pool%items(idx)%r = 0.0_real64
    if( allocated( pool%items(idx)%s ) ) deallocate( pool%items(idx)%s )
    if( allocated( pool%items(idx)%f ) ) deallocate( pool%items(idx)%f )
  end function ext_alloc

  function ext_err( pool, m ) result(v)
    type(ExtPool), intent(inout) :: pool
    character(len=*), intent(in) :: m
    type(FastVal) :: v
    integer :: idx
    idx = ext_alloc( pool )
    pool%items(idx)%kind = PV_ERROR
    pool%items(idx)%s = m
    v = FastVal( PV_ERROR, 0_int64, 0.0_real64, idx )
  end function ext_err

  function ext_str( pool, s ) result(v)
    type(ExtPool), intent(inout) :: pool
    character(len=*), intent(in) :: s
    type(FastVal) :: v
    integer :: idx
    idx = ext_alloc( pool )
    pool%items(idx)%kind = PV_STRING
    pool%items(idx)%s = s
    v = FastVal( PV_STRING, 0_int64, 0.0_real64, idx )
  end function ext_str

  function ext_from_prop( pool, pv ) result(v)
    type(ExtPool), intent(inout) :: pool
    type(PropValue), intent(in) :: pv
    type(FastVal) :: v
    integer :: idx
    select case( pv%kind )
    case( PV_NONE );  v = FastVal( PV_NONE,  0_int64, 0.0_real64, 0 )
    case( PV_INT );   v = FastVal( PV_INT,   pv%i,    0.0_real64, 0 )
    case( PV_FLOAT ); v = FastVal( PV_FLOAT, 0_int64, pv%r,       0 )
    case default
       idx = ext_alloc( pool )
       pool%items(idx) = pv
       v = FastVal( pv%kind, pv%i, pv%r, idx )
    end select
  end function ext_from_prop

  subroutine ext_to_prop( pool, fv, pv )
    type(ExtPool), intent(in) :: pool
    type(FastVal), intent(in) :: fv
    type(PropValue), intent(inout) :: pv
    if( fv%ext > 0 ) then
       pv = pool%items( fv%ext )
    else
       pv%kind = fv%kind
       pv%i    = fv%i
       pv%r    = fv%r
    end if
  end subroutine ext_to_prop

  function ext_to_string_val( pool, a ) result(s)
    type(ExtPool), intent(in) :: pool
    type(FastVal), intent(in) :: a
    character(len=:), allocatable :: s
    type(PropValue) :: pv
    if( a%ext > 0 ) then
       s = PropValueToString( pool%items( a%ext ) )
    else
       pv%kind = a%kind; pv%i = a%i; pv%r = a%r
       s = PropValueToString( pv )
    end if
  end function ext_to_string_val

  function ext_binop_str( code, a, b, pool ) result(v)
    integer, intent(in) :: code
    type(FastVal), intent(in) :: a, b
    type(ExtPool), intent(inout) :: pool
    type(FastVal) :: v
    logical :: r
    if( code == B_ADD ) then
       v = ext_str( pool, ext_to_string_val( pool, a ) // ext_to_string_val( pool, b ) )
       return
    end if
    if( a%kind == PV_STRING .and. b%kind == PV_STRING ) then
       select case( code )
       case( B_EQ ); r = pool%items(a%ext)%s == pool%items(b%ext)%s
       case( B_NE ); r = pool%items(a%ext)%s /= pool%items(b%ext)%s
       case( B_LT ); r = llt( pool%items(a%ext)%s, pool%items(b%ext)%s )
       case( B_LE ); r = lle( pool%items(a%ext)%s, pool%items(b%ext)%s )
       case( B_GT ); r = lgt( pool%items(a%ext)%s, pool%items(b%ext)%s )
       case( B_GE ); r = lge( pool%items(a%ext)%s, pool%items(b%ext)%s )
       case default
          v = ext_err( pool, 'operator not defined on STRING' ); return
       end select
       v = ival( merge( 1_int64, 0_int64, r ) ); return
    end if
    v = ext_err( pool, 'operator mixes STRING and number' )
  end function ext_binop_str

  function ext_field_get( pool, a_ext, fname ) result(v)
    type(ExtPool), intent(inout) :: pool
    integer, intent(in) :: a_ext
    character(len=*), intent(in) :: fname
    type(FastVal) :: v
    integer :: j, idx
    do j = 1, size( pool%items(a_ext)%f )
       if( pool%items(a_ext)%f(j)%name == fname ) then
          select case( pool%items(a_ext)%f(j)%kind )
          case( PV_INT )
             v = ival( pool%items(a_ext)%f(j)%i )
          case( PV_FLOAT )
             v = rval( pool%items(a_ext)%f(j)%r )
          case( PV_STRING )
             if( allocated( pool%items(a_ext)%f(j)%s ) ) then
                v = ext_str( pool, pool%items(a_ext)%f(j)%s )
             else
                v = ext_str( pool, '' )
             end if
          case default
             idx = ext_alloc( pool )
             pool%items(idx)%kind = pool%items(a_ext)%f(j)%kind
             pool%items(idx)%i    = pool%items(a_ext)%f(j)%i
             pool%items(idx)%r    = pool%items(a_ext)%f(j)%r
             if( allocated( pool%items(a_ext)%f(j)%s ) ) pool%items(idx)%s = pool%items(a_ext)%f(j)%s
             v = FastVal( pool%items(idx)%kind, pool%items(idx)%i, pool%items(idx)%r, idx )
          end select
          return
       end if
    end do
    v = ext_err( pool, 'STRUCT has no field '//fname )
  end function ext_field_get

  function ext_eval_prop( t, col, fname, is_field, ord, pool ) result(v)
    type(PropertyTable), intent(in) :: t
    integer, intent(in) :: col
    character(len=*), intent(in) :: fname
    logical, intent(in) :: is_field
    integer(int64), intent(in) :: ord
    type(ExtPool), intent(inout) :: pool
    type(FastVal) :: v
    type(PropValue) :: pv
    if( is_field ) then
       pv = PropGetField( t, col, fname, ord )
    else
       pv = PropGetValue( t, col, ord )
    end if
    if( pv%kind == PV_NONE ) then
       v = ext_err( pool, 'property '//t%cols(col)%name//' has no value on this polygon' )
    else
       v = ext_from_prop( pool, pv )
    end if
  end function ext_eval_prop

  function ext_parse_int( pool, a ) result(v)
    type(ExtPool), intent(inout) :: pool
    type(FastVal), intent(in) :: a
    type(FastVal) :: v
    integer(int64) :: iv
    integer :: ios
    iv = 0_int64
    read( pool%items(a%ext)%s, *, iostat=ios ) iv
    if( ios /= 0 ) then
       v = ext_err( pool, 'INT: cannot convert "'//pool%items(a%ext)%s//'"' )
    else
       v = ival( iv )
    end if
  end function ext_parse_int

  function ext_parse_float( pool, a ) result(v)
    type(ExtPool), intent(inout) :: pool
    type(FastVal), intent(in) :: a
    type(FastVal) :: v
    real(real64) :: rv
    integer :: ios
    rv = 0.0_real64
    read( pool%items(a%ext)%s, *, iostat=ios ) rv
    if( ios /= 0 ) then
       v = ext_err( pool, 'FLOAT: cannot convert "'//pool%items(a%ext)%s//'"' )
    else
       v = rval( rv )
    end if
  end function ext_parse_float

  subroutine ext_fassign( pool, slot_val, slot_name, field_name, v, errv )
    type(ExtPool), intent(inout) :: pool
    type(FastVal), intent(inout) :: slot_val
    character(len=*), intent(in) :: slot_name, field_name
    type(FastVal), intent(in) :: v
    type(FastVal), intent(inout) :: errv
    type(PropField), allocatable :: tmp(:)
    integer :: idx, j, n
    if( v%kind == PV_STRUCT ) then
       errv = ext_err( pool, 'STRUCT fields must be INT, FLOAT or STRING (no nesting)' ); return
    end if
    if( slot_val%kind /= PV_STRUCT ) then
       if( slot_val%kind /= PV_NONE ) then
          errv = ext_err( pool, slot_name//'.'//field_name//' = ... on a non-STRUCT value' ); return
       end if
       idx = ext_alloc( pool )
       pool%items(idx)%kind = PV_STRUCT
       allocate( pool%items(idx)%f(0) )
       slot_val = FastVal( PV_STRUCT, 0_int64, 0.0_real64, idx )
    else
       idx = slot_val%ext
    end if
    associate( L => pool%items(idx) )
      n = 0
      do j = 1, size( L%f )
         if( L%f(j)%name == field_name ) then
            n = j; exit
         end if
      end do
      if( n == 0 ) then
         allocate( tmp( size(L%f) + 1 ) )
         tmp(1:size(L%f)) = L%f
         call move_alloc( tmp, L%f )
         n = size( L%f )
         L%f(n)%name = field_name
      end if
      L%f(n)%kind = v%kind; L%f(n)%i = v%i; L%f(n)%r = v%r
      if( v%ext > 0 ) then
         if( allocated( pool%items(v%ext)%s ) ) then
            L%f(n)%s = pool%items(v%ext)%s
         else if( allocated( L%f(n)%s ) ) then
            deallocate( L%f(n)%s )
         end if
      else if( allocated( L%f(n)%s ) ) then
         deallocate( L%f(n)%s )
      end if
    end associate
  end subroutine ext_fassign

  !=================================================================
  ! Evaluator (hot path: strictly POD FastVal, zero allocatable locals)
  !=================================================================
  pure function ival( i ) result(v)
    integer(int64), intent(in) :: i
    type(FastVal) :: v
    v = FastVal( PV_INT, i, 0.0_real64, 0 )
  end function ival

  pure function rval( r ) result(v)
    real(real64), intent(in) :: r
    type(FastVal) :: v
    v = FastVal( PV_FLOAT, 0_int64, r, 0 )
  end function rval

  pure logical function is_num( v )
    type(FastVal), intent(in) :: v
    is_num = v%kind == PV_INT .or. v%kind == PV_FLOAT
  end function is_num

  pure real(real64) function as_real( v )
    type(FastVal), intent(in) :: v
    if( v%kind == PV_INT ) then
       as_real = real( v%i, real64 )
    else
       as_real = v%r
    end if
  end function as_real

  pure logical function truthy( v, pool )
    type(FastVal), intent(in) :: v
    type(ExtPool), intent(in) :: pool
    select case( v%kind )
    case( PV_INT );    truthy = v%i /= 0
    case( PV_FLOAT );  truthy = v%r /= 0.0_real64
    case( PV_STRING ); truthy = len( pool%items(v%ext)%s ) > 0
    case( PV_STRUCT ); truthy = .true.
    case default;      truthy = .false.
    end select
  end function truthy

  !> Polygon ordinal of an id-valued argument (0 + error value when invalid).
  function ord_of( t, v, e, pool ) result(ord)
    type(PropertyTable), intent(in) :: t
    type(FastVal), intent(in) :: v
    type(FastVal), intent(inout) :: e
    type(ExtPool), intent(inout) :: pool
    integer(int64) :: ord
    ord = 0
    if( v%kind /= PV_INT ) then
       e = ext_err( pool, 'polygon id must be an INT' ); return
    end if
    ord = PropOrdinalOfId( t, v%i )
    if( ord == 0 ) e = ext_err( pool, 'not a polygon id of this layer' )
  end function ord_of

  recursive function eval( lam, k, ids, locals, t, pool ) result(v)
    type(Lambda), intent(in) :: lam
    integer, intent(in) :: k
    integer(int64), intent(in) :: ids(:)
    type(FastVal), intent(in) :: locals(:)
    type(PropertyTable), intent(in) :: t
    type(ExtPool), intent(inout) :: pool
    type(FastVal) :: v, a, b, c
    integer(int64) :: ord
    associate( nd => lam%nodes(k) )
      select case( nd%op )
      case( N_INT );   v = ival( nd%iv )
      case( N_FLT );   v = rval( nd%rv )
      case( N_STR );   v = ext_str( pool, nd%sv )
      case( N_PARAM ); v = ival( ids(nd%slot) )
      case( N_LOCAL )
         v = locals( nd%slot )
         if( v%kind == PV_NONE ) v = ext_err( pool, lam%slots(nd%slot)%s//' used before it is assigned' )
      case( N_NEG )
         a = eval( lam, nd%a, ids, locals, t, pool )
         select case( a%kind )
         case( PV_INT );   v = ival( -a%i )
         case( PV_FLOAT ); v = rval( -a%r )
         case( PV_ERROR ); v = a
         case default;     v = ext_err( pool, 'unary - needs a number' )
         end select
      case( N_NOT )
         a = eval( lam, nd%a, ids, locals, t, pool )
         if( a%kind == PV_ERROR ) then
            v = a
         else
            v = ival( merge( 0_int64, 1_int64, truthy( a, pool ) ) )
         end if
      case( N_BIN )
         if( nd%fn == B_AND .or. nd%fn == B_OR ) then
            a = eval( lam, nd%a, ids, locals, t, pool )
            if( a%kind == PV_ERROR ) then
               v = a; return
            end if
            if( nd%fn == B_AND .and. .not. truthy( a, pool ) ) then
               v = ival( 0_int64 ); return
            end if
            if( nd%fn == B_OR .and. truthy( a, pool ) ) then
               v = ival( 1_int64 ); return
            end if
            b = eval( lam, nd%b, ids, locals, t, pool )
            if( b%kind == PV_ERROR ) then
               v = b
            else
               v = ival( merge( 1_int64, 0_int64, truthy( b, pool ) ) )
            end if
            return
         end if
         a = eval( lam, nd%a, ids, locals, t, pool )
         if( a%kind == PV_ERROR ) then
            v = a; return
         end if
         b = eval( lam, nd%b, ids, locals, t, pool )
         if( b%kind == PV_ERROR ) then
            v = b; return
         end if
         v = binop( nd%fn, a, b, pool )
      case( N_STRUCT )
         v = eval_struct( lam, k, ids, locals, t, pool )
      case( N_FIELD )
         a = eval( lam, nd%a, ids, locals, t, pool )
         if( a%kind == PV_ERROR ) then
            v = a; return
         end if
         if( a%kind /= PV_STRUCT ) then
            v = ext_err( pool, '.'//nd%sv//' applied to a non-STRUCT value' ); return
         end if
         v = ext_field_get( pool, a%ext, nd%sv )
      case( N_PROP, N_PROPF )
         a = eval( lam, nd%a, ids, locals, t, pool )
         if( a%kind == PV_ERROR ) then
            v = a; return
         end if
         c = FastVal()
         ord = ord_of( t, a, c, pool )
         if( c%kind == PV_ERROR ) then
            v = c; return
         end if
         v = ext_eval_prop( t, nd%col, nd%sv, nd%op == N_PROPF, ord, pool )
      case( N_CALL )
         v = call_builtin( lam, nd, ids, locals, t, pool )
      case default
         v = ext_err( pool, 'internal: bad node' )
      end select
    end associate
  end function eval

  ! STRUCT constructor, outside eval's associate block (nvfortran 26.1 ICE: "args$sd1 is an
  ! inconsistent array descriptor" on an allocatable component through the associate name).
  recursive function eval_struct( lam, k, ids, locals, t, pool ) result(v)
    type(Lambda), intent(in) :: lam
    integer, intent(in) :: k
    integer(int64), intent(in) :: ids(:)
    type(FastVal), intent(in) :: locals(:)
    type(PropertyTable), intent(in) :: t
    type(ExtPool), intent(inout) :: pool
    type(FastVal) :: v, a
    integer, allocatable :: args(:)
    integer :: j, idx
    args = lam%nodes(k)%args
    idx = ext_alloc( pool )
    pool%items(idx)%kind = PV_STRUCT
    allocate( pool%items(idx)%f( size(args) ) )
    do j = 1, size( args )
       a = eval( lam, args(j), ids, locals, t, pool )
       if( a%kind == PV_ERROR ) then
          v = a; return
       end if
       if( a%kind == PV_STRUCT ) then
          v = ext_err( pool, 'STRUCT fields must be INT, FLOAT or STRING (no nesting)' ); return
       end if
       pool%items(idx)%f(j)%name = lam%nodes(k)%names(j)%s
       pool%items(idx)%f(j)%kind = a%kind
       pool%items(idx)%f(j)%i    = a%i
       pool%items(idx)%f(j)%r    = a%r
       if( a%ext > 0 ) then
          if( allocated( pool%items(a%ext)%s ) ) pool%items(idx)%f(j)%s = pool%items(a%ext)%s
       end if
    end do
    v = FastVal( PV_STRUCT, 0_int64, 0.0_real64, idx )
  end function eval_struct

  function binop( code, a, b, pool ) result(v)
    integer, intent(in) :: code
    type(FastVal), intent(in) :: a, b
    type(ExtPool), intent(inout) :: pool
    type(FastVal) :: v
    logical :: both_int, r
    if( a%kind == PV_STRUCT .or. b%kind == PV_STRUCT ) then
       v = ext_err( pool, 'operators do not apply to STRUCT values' ); return
    end if
    if( a%kind == PV_STRING .or. b%kind == PV_STRING ) then
       v = ext_binop_str( code, a, b, pool ); return
    end if
    if( .not. ( is_num( a ) .and. is_num( b ) ) ) then
       v = ext_err( pool, 'operator mixes STRING and number' ); return
    end if
    both_int = a%kind == PV_INT .and. b%kind == PV_INT
    select case( code )
    case( B_ADD )
       if( both_int ) then
          v = ival( a%i + b%i )
       else
          v = rval( as_real(a) + as_real(b) )
       end if
    case( B_SUB )
       if( both_int ) then
          v = ival( a%i - b%i )
       else
          v = rval( as_real(a) - as_real(b) )
       end if
    case( B_MUL )
       if( both_int ) then
          v = ival( a%i * b%i )
       else
          v = rval( as_real(a) * as_real(b) )
       end if
    case( B_DIV )
       if( as_real(b) == 0.0_real64 ) then
          v = ext_err( pool, 'division by zero' )
       else
          v = rval( as_real(a) / as_real(b) )
       end if
    case( B_MOD )
       if( .not. both_int ) then
          v = ext_err( pool, '% needs INT operands' )
       else if( b%i == 0 ) then
          v = ext_err( pool, 'modulo by zero' )
       else
          v = ival( modulo( a%i, b%i ) )
       end if
    case default
       if( both_int ) then
          select case( code )
          case( B_EQ ); r = a%i == b%i
          case( B_NE ); r = a%i /= b%i
          case( B_LT ); r = a%i <  b%i
          case( B_LE ); r = a%i <= b%i
          case( B_GT ); r = a%i >  b%i
          case( B_GE ); r = a%i >= b%i
          end select
       else
          select case( code )
          case( B_EQ ); r = as_real(a) == as_real(b)
          case( B_NE ); r = as_real(a) /= as_real(b)
          case( B_LT ); r = as_real(a) <  as_real(b)
          case( B_LE ); r = as_real(a) <= as_real(b)
          case( B_GT ); r = as_real(a) >  as_real(b)
          case( B_GE ); r = as_real(a) >= as_real(b)
          end select
       end if
       v = ival( merge( 1_int64, 0_int64, r ) )
    end select
  end function binop

  recursive function call_builtin( lam, nd, ids, locals, t, pool ) result(v)
    type(Lambda), intent(in) :: lam
    type(Node), intent(in) :: nd
    integer(int64), intent(in) :: ids(:)
    type(FastVal), intent(in) :: locals(:)
    type(PropertyTable), intent(in) :: t
    type(ExtPool), intent(inout) :: pool
    type(FastVal) :: v, a, b, e
    integer(int64) :: ord
    if( nd%fn == F_IF ) then
       a = eval( lam, nd%args(1), ids, locals, t, pool )
       if( a%kind == PV_ERROR ) then
          v = a
       else if( truthy( a, pool ) ) then
          v = eval( lam, nd%args(2), ids, locals, t, pool )
       else
          v = eval( lam, nd%args(3), ids, locals, t, pool )
       end if
       return
    end if
    a = eval( lam, nd%args(1), ids, locals, t, pool )
    if( a%kind == PV_ERROR ) then
       v = a; return
    end if
    if( size( nd%args ) > 1 ) then
       b = eval( lam, nd%args(2), ids, locals, t, pool )
       if( b%kind == PV_ERROR ) then
          v = b; return
       end if
    end if
    select case( nd%fn )
    case( F_PNUM, F_PID, F_AREA, F_NBOX, F_BX1, F_BY1, F_BX2, F_BY2, F_BW, F_BH, &
          F_PERIM, F_NVERT, F_LEDIR, F_LELEN, F_LEW, F_NET, F_NETNAME )
       e = FastVal()
       ord = ord_of( t, a, e, pool )
       if( e%kind == PV_ERROR ) then
          v = e; return
       end if
       select case( nd%fn )
       case( F_PNUM ); v = ival( ord )
       case( F_PID );  v = ival( a%i )
       case( F_AREA ); v = ival( t%area(ord) )
       case( F_NBOX ); v = ival( t%poly_start(ord+1) - t%poly_start(ord) )
       case( F_BX1 );  v = ival( int( t%bbox(ord)%x1, int64 ) )
       case( F_BY1 );  v = ival( int( t%bbox(ord)%y1, int64 ) )
       case( F_BX2 );  v = ival( int( t%bbox(ord)%x2, int64 ) )
       case( F_BY2 );  v = ival( int( t%bbox(ord)%y2, int64 ) )
       case( F_BW );   v = ival( int( t%bbox(ord)%x2, int64 ) - int( t%bbox(ord)%x1, int64 ) )
       case( F_BH );   v = ival( int( t%bbox(ord)%y2, int64 ) - int( t%bbox(ord)%y1, int64 ) )
       case( F_PERIM, F_NVERT, F_LEDIR, F_LELEN, F_LEW )
          if( .not. allocated( t%perim ) ) then
             v = ext_err( pool, 'outline measures not computed' ); return
          end if
          select case( nd%fn )
          case( F_PERIM ); v = ival( t%perim(ord) )
          case( F_NVERT )
             if( t%nvert(ord) < 0 ) then
                v = ext_err( pool, 'VERTICES: outline could not be traced' )
             else
                v = ival( t%nvert(ord) )
             end if
          case( F_LEW );   v = ival( t%le_w(ord) )
          case( F_LELEN ); v = ival( t%le_len(ord) )
          case( F_LEDIR )
             select case( int( t%le_dir(ord) ) )
             case( 0 ); v = ext_str( pool, 'E' )
             case( 1 ); v = ext_str( pool, 'N' )
             case( 2 ); v = ext_str( pool, 'W' )
             case( 3 ); v = ext_str( pool, 'S' )
             case default; v = ext_str( pool, '' )
             end select
          end select
       case( F_NET )
          if( .not. allocated( t%net ) ) then
             v = ext_err( pool, 'NET_ID: layer is not part of a CONNECT' ); return
          end if
          v = ival( t%net(ord) )
       case( F_NETNAME )
          if( .not. allocated( t%net ) ) then
             v = ext_err( pool, 'NET_NAME: layer is not part of a CONNECT' ); return
          end if
          v = ext_str( pool, PropNetName( t%net_rec, t%net(ord) ) )
       end select
    case( F_INT )
       select case( a%kind )
       case( PV_INT );    v = a
       case( PV_FLOAT );  v = ival( int( a%r, int64 ) )
       case( PV_STRING ); v = ext_parse_int( pool, a )
       case default;      v = ext_err( pool, 'INT: bad argument' )
       end select
    case( F_FLOAT )
       select case( a%kind )
       case( PV_INT, PV_FLOAT ); v = rval( as_real( a ) )
       case( PV_STRING );        v = ext_parse_float( pool, a )
       case default;             v = ext_err( pool, 'FLOAT: bad argument' )
       end select
    case( F_STRING )
       v = ext_str( pool, ext_to_string_val( pool, a ) )
    case( F_ABS )
       if( a%kind == PV_INT ) then
          v = ival( abs( a%i ) )
       else if( a%kind == PV_FLOAT ) then
          v = rval( abs( a%r ) )
       else
          v = ext_err( pool, 'ABS needs a number' )
       end if
    case( F_SQRT )
       if( .not. is_num( a ) ) then
          v = ext_err( pool, 'SQRT needs a number' )
       else if( as_real( a ) < 0 ) then
          v = ext_err( pool, 'SQRT of a negative number' )
       else
          v = rval( sqrt( as_real( a ) ) )
       end if
    case( F_POW )
       if( .not. ( is_num( a ) .and. is_num( b ) ) ) then
          v = ext_err( pool, 'POW needs numbers' )
       else
          v = rval( as_real( a ) ** as_real( b ) )
       end if
    case( F_MIN, F_MAX )
       if( .not. ( is_num( a ) .and. is_num( b ) ) ) then
          v = ext_err( pool, 'MIN/MAX need numbers' )
       else if( a%kind == PV_INT .and. b%kind == PV_INT ) then
          v = ival( merge( min( a%i, b%i ), max( a%i, b%i ), nd%fn == F_MIN ) )
       else
          v = rval( merge( min( as_real(a), as_real(b) ), max( as_real(a), as_real(b) ), nd%fn == F_MIN ) )
       end if
    case default
       v = ext_err( pool, 'internal: unknown builtin' )
    end select
  end function call_builtin

  !> Run the lambda statements for one polygon; locals(slot) hold the results.
  !> skipped = a WHEN guard was false; errv%kind == PV_ERROR on a runtime error.
  subroutine run_one( lam, id, locals, t, pool, skipped, errv )
    type(Lambda), intent(in) :: lam
    integer(int64), intent(in) :: id
    type(FastVal), intent(inout) :: locals(:)
    type(PropertyTable), intent(in) :: t
    type(ExtPool), intent(inout) :: pool
    logical, intent(out) :: skipped
    type(FastVal), intent(inout) :: errv
    type(FastVal) :: v
    integer :: s
    integer(int64) :: ids(1)
    ids(1) = id
    skipped = .false.
    do s = 1, lam%nstmts
       associate( st => lam%stmts(s) )
         v = eval( lam, st%expr, ids, locals, t, pool )
         if( v%kind == PV_ERROR ) then
            errv = v; return
         end if
         select case( st%kind )
         case( S_WHEN )
            if( .not. truthy( v, pool ) ) then
               skipped = .true.; return
            end if
         case( S_ASSIGN )
            locals( st%slot ) = v
         case( S_FASSIGN )
            call ext_fassign( pool, locals( st%slot ), lam%slots(st%slot)%s, st%field, v, errv )
            if( errv%kind == PV_ERROR ) return
         end select
       end associate
    end do
  end subroutine run_one

  !=================================================================
  ! Bring the layer to Box form with polygon numbers and (re)index its table.
  !=================================================================
  subroutine LayerEnsureProperties( L, lname )
    type(Layer), intent(inout), target :: L
    character(len=*), intent(in) :: lname
    logical :: dropped
    real(kind=real64) :: oa, op
    call LayerExpand( L )
    if( L%n_used > 0 ) then
       if( NeedsHealing( L ) .or. NeedsPNum( L ) ) call PreprocessLayer( L )
       if( NeedsPNum( L ) ) then
          if( NeedsRTree( L ) ) call BuildTree( L )
          call PerformMerge( L%pnumtable, L%layer_boxes(1:L%n_used), K_LEAF_CAPACITY, &
               L%tree%tree_nodes, L%tree%root_index, oa, op )
          call LayerMark( L, LAYER_STATE_PNUM )
       end if
    end if
    if( .not. allocated( L%props ) ) allocate( L%props )
    if( L%n_used > 0 ) then
       call PropTableIndex( L%props, L%layer_boxes, L%n_used, L%pnumtable, dropped )
    else
       call PropTableIndex( L%props, L%layer_boxes, 0_int64, L%pnumtable, dropped )
    end if
    if( dropped ) write(*,'(A,A,A)') 'WARNING: layer ', lname, &
         ' changed since its properties were computed; stale properties dropped'
  end subroutine LayerEnsureProperties

  !> Compile and run `lam_text` over every polygon of L, defining properties `names`.
  !> True if the compiled lambda calls any of the given builtins.
  pure logical function lambda_calls( lam, fns )
    type(Lambda), intent(in) :: lam
    integer, intent(in) :: fns(:)
    integer :: k
    lambda_calls = .false.
    do k = 1, lam%nnodes
       if( lam%nodes(k)%op == N_CALL ) then
          if( any( fns == lam%nodes(k)%fn ) ) then
             lambda_calls = .true.; return
          end if
       end if
    end do
  end function lambda_calls

  !> Perimeter, vertex count and narrowest line end of every polygon (lazy, cached in the
  !> table until the geometry changes). Polygons are the healed, disjoint boxes of the layer.
  subroutine PropEnsureOutline( t, boxes )
    type(PropertyTable), intent(inout) :: t
    type(Box), intent(in) :: boxes(:)
    integer(int64) :: p, j, nb, k, nfail
    integer(c_int64_t), allocatable :: buf(:)
    integer(c_int64_t) :: o(4)
    integer(c_int32_t) :: d, st
    if( t%have_outline ) return
    call PropDropOutline( t )
    allocate( t%perim(t%n_poly), t%nvert(t%n_poly), t%le_w(t%n_poly), t%le_len(t%n_poly), &
         t%le_dir(t%n_poly) )
    nfail = 0
    !$omp parallel private(buf, p, j, nb, k, o, d, st)
    allocate( buf(64) )
    !$omp do schedule(dynamic, 256) reduction(+:nfail)
    do p = 1, t%n_poly
       nb = t%poly_start(p+1) - t%poly_start(p)
       if( size(buf, kind=int64) < 4*nb ) then
          deallocate( buf ); allocate( buf( 4*nb + 64 ) )
       end if
       do j = 1, nb
          k = t%poly_boxes( t%poly_start(p) + j - 1 )
          buf(4*j-3) = boxes(k)%x1; buf(4*j-2) = boxes(k)%y1
          buf(4*j-1) = boxes(k)%x2; buf(4*j)   = boxes(k)%y2
       end do
       st = fpo_poly_geom( nb, buf, o, d )
       if( st /= 0 ) nfail = nfail + 1
       t%perim(p) = o(1); t%nvert(p) = o(2); t%le_w(p) = o(3); t%le_len(p) = o(4)
       t%le_dir(p) = int( d, int8 )
    end do
    !$omp end do
    deallocate( buf )
    !$omp end parallel
    t%n_outline_fail = nfail
    t%have_outline = .true.
    if( nfail > 0 ) write(*,'(A,I0,A)') 'WARNING: FASCL outline could not be traced for ', nfail, ' polygon(s)'
  end subroutine PropEnsureOutline

  subroutine FASCLRunLambda( L, lname, names, lam_text, status, msg )
    type(Layer), intent(inout), target :: L
    character(len=*), intent(in) :: lname
    type(PropString), intent(in) :: names(:)
    character(len=*), intent(in) :: lam_text
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    type(Lambda) :: lam
    type(ValueColumn), allocatable :: outs(:)
    integer, allocatable :: out_slot(:)
    type(PropValue) :: first_err
    integer(int64) :: np, err_ord, nskip
    integer :: k, j
    real(real64) :: t0, t1

    call LayerEnsureProperties( L, lname )
    call FASCLCompile( lam_text, names, L%props, lam, status, msg )
    if( status /= 0 ) return
    if( lambda_calls( lam, [ F_NET, F_NETNAME ] ) ) then
       if( L%props%net_rec == 0 .or. .not. allocated( L%props%net ) ) then
          status = 1
          msg = 'NET_ID: layer '//lname//' is not part of a CONNECT (CONNECT db:metal db:via db:metal ... must come first)'
          return
       end if
    end if
    if( lambda_calls( lam, [ F_NETNAME ] ) ) then
       if( .not. PropNetNamesHave( L%props%net_rec ) ) then
          status = 1
          msg = 'NET_NAME: no DEF net names for the CONNECT of '//lname//' (exec defcheck '//lname//' file.def must come first)'
          return
       end if
    end if
    if( lambda_calls( lam, [ F_PERIM, F_NVERT, F_LEDIR, F_LELEN, F_LEW ] ) ) &
         call PropEnsureOutline( L%props, L%layer_boxes )
    np = L%props%n_poly
    allocate( outs( size(names) ), out_slot( size(names) ) )
    do k = 1, size(names)
       allocate( outs(k)%v(np) )
       out_slot(k) = 0
       do j = 1, lam%nslots
          if( lam%slots(j)%s == names(k)%s ) out_slot(k) = j
       end do
    end do
    err_ord = huge(err_ord)
    nskip = 0
    call cpu_time( t0 )
    !$omp parallel
    call run_lambda_worker( lam, L, np, outs, out_slot, err_ord, first_err, nskip )
    !$omp end parallel
    call cpu_time( t1 )
    if( err_ord /= huge(err_ord) ) then
       status = 1
       write(msg,'(A)') ''
       msg = 'FASCL runtime error on polygon #'//i8toa(err_ord)//' (id '// &
            i8toa(L%props%poly_id(err_ord))//'): '//first_err%s
       return
    end if
    do k = 1, size(names)
       call PropCommitColumn( L%props, names(k)%s, outs(k)%v, status, msg )
       if( status /= 0 ) return
    end do
    write(*,'(A,A,A,I0,A,I0,A,F10.3,A)') 'FASCL: ', lname, ' polygons=', np, ' skipped(WHEN)=', nskip, &
         ' cpu=', t1 - t0, ' s'
    status = 0
    msg = ''
  end subroutine FASCLRunLambda

  ! Body of FASCLRunLambda's parallel region, called by every thread (outlined: nvfortran 26.1
  ! ICE "mk_mem_ptr_shape: lwb not sdsc" on a private allocatable array of PropValue).
  subroutine run_lambda_worker( lam, L, np, outs, out_slot, err_ord, first_err, nskip )
    type(Lambda), intent(in) :: lam
    type(Layer), intent(inout), target :: L
    integer(int64), intent(in) :: np
    type(ValueColumn), intent(inout) :: outs(:)
    integer, intent(in) :: out_slot(:)
    integer(int64), intent(inout) :: err_ord, nskip
    type(PropValue), intent(inout) :: first_err
    type(FastVal), allocatable :: locals(:)
    type(FastVal) :: errv, lv
    type(ExtPool) :: pool
    integer(int64) :: ord
    integer :: k
    logical :: skipped
    allocate( locals( lam%nslots ) )
    !$omp do schedule(dynamic, 512) reduction(+:nskip)
    do ord = 1, np
       do k = 1, lam%nslots
          locals(k)%kind = PV_NONE
          locals(k)%ext  = 0
       end do
       errv%kind = PV_NONE
       errv%ext  = 0
       pool%n    = 0
       call run_one( lam, L%props%poly_id(ord), locals, L%props, pool, skipped, errv )
       if( errv%kind == PV_ERROR ) then
          !$omp critical (fascl_err)
          if( ord < err_ord ) then
             err_ord = ord
             call ext_to_prop( pool, errv, first_err )
          end if
          !$omp end critical (fascl_err)
          cycle
       end if
       if( skipped ) then
          nskip = nskip + 1
          cycle
       end if
       do k = 1, size(out_slot)
          lv = locals( out_slot(k) )
          if( lv%ext == 0 ) then
             outs(k)%v(ord)%kind = lv%kind
             outs(k)%v(ord)%i    = lv%i
             outs(k)%v(ord)%r    = lv%r
          else
             call ext_to_prop( pool, lv, outs(k)%v(ord) )
          end if
       end do
    end do
    !$omp end do
    deallocate( locals )
  end subroutine run_lambda_worker

  pure function i8toa( i ) result(s)
    integer(int64), intent(in) :: i
    character(len=:), allocatable :: s
    character(len=24) :: b
    write(b,'(I0)') i
    s = trim(b)
  end function i8toa

  !> Execute a parsed FASCL statement on resolved layers (B may be null for `nothing`).
  subroutine FASCLExecute( names, A, lnameA, B, lam_text, status, msg )
    type(PropString), intent(in) :: names(:)
    type(Layer), intent(inout), target :: A
    character(len=*), intent(in) :: lnameA
    type(Layer), pointer, intent(in) :: B
    character(len=*), intent(in) :: lam_text
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    if( associated( B ) ) then
       if( B%n_used > 0 .or. B%is_compact ) then
          status = 1
          msg = 'FASCL: a non-empty second layer needs coupling (spacing) lambdas, which are not implemented yet; use nothing'
          return
       end if
    end if
    call FASCLRunLambda( A, lnameA, names, lam_text, status, msg )
    if( status == 0 ) call PropTablePrint( A%props, lnameA )
  end subroutine FASCLExecute

end module FASCLModule
