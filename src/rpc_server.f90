! File   : rpc_server.f90
! Author : Sandeep Koranne (C) 2026
! Purpose: MAGPARSER RPC server mode for the Julia compute cluster (cluster/mp/).
!
!    MAGPARSER.exe --rpc-serve 127.0.0.1:PORT        (token in env MAGPARSER_RPC_TOKEN)
!
!  MAGPARSER connects to the Julia worker that launched it, then serves requests until
!  the socket closes or QUIT arrives.  Layers travel as CompactLayer network form
!  (compact_wire.f90, "CLW1"); no CTR/BIN files are read or written.  Cross-machine
!  transport is Julia's Distributed.jl RPC; this socket is always local to the node.
!
!  Local protocol, little-endian:
!    hello    : 'MPHI' int32 version int32 toklen token(toklen)
!    request  : 'MPRQ' int32 version char(32) op int32 nparam int64 params(8)
!               int32 nlayers { int64 len, bytes(len) } * nlayers
!    response : 'MPRS' int32 status int64 elapsed_us int64 n_out int32 msglen msg(msglen)
!               int64 len bytes(len)                 (len = 0: no layer returned)
!  status: 0 ok, 1 unknown op, 2 bad input layer, 3 wrong number of layers.
!  Ops: PING QUIT | OR OR2 AND NOT XOR BOOSTNOT <topological ops> (2 layers) |
!       GROW e n w s, SHRINK e [n w s], EXTENT, COPY, MERGE (1 layer).  Sizes in DBU.
module RPCServerModule
  use iso_c_binding
  use iso_fortran_env, only: int8, int32, int64, real64, compiler_version
  use CommonModule
  use GeometryModule, only: Box
  use CompactLayerModule, only: CompactLayer, CompactEncode, CompactPack, CompactRelease
  use CompactWireModule
  use DesignModule
  use version_mod, only: commit_hash
  implicit none
  private
  public :: RPCServe

  interface
     function rpc_connect(host, port) bind(c, name="rpc_connect") result(fd)
       import :: c_char, c_int
       character(kind=c_char), intent(in) :: host(*)
       integer(c_int), value :: port
       integer(c_int) :: fd
     end function rpc_connect
     function rpc_send_all(fd, buf, n) bind(c, name="rpc_send_all") result(rc)
       import :: c_int, c_ptr, c_int64_t
       integer(c_int), value :: fd
       type(c_ptr), value :: buf
       integer(c_int64_t), value :: n
       integer(c_int) :: rc
     end function rpc_send_all
     function rpc_recv_all(fd, buf, n) bind(c, name="rpc_recv_all") result(rc)
       import :: c_int, c_ptr, c_int64_t
       integer(c_int), value :: fd
       type(c_ptr), value :: buf
       integer(c_int64_t), value :: n
       integer(c_int) :: rc
     end function rpc_recv_all
     subroutine rpc_close(fd) bind(c, name="rpc_close")
       import :: c_int
       integer(c_int), value :: fd
     end subroutine rpc_close
     function rpc_little_endian() bind(c, name="rpc_little_endian") result(le)
       import :: c_int
       integer(c_int) :: le
     end function rpc_little_endian
  end interface

  integer(c_int), parameter :: RPC_VERSION = 1
  integer(int64), parameter :: MAX_LAYER_BYTES = 8_int64 * 1024_int64**3

  type :: ByteBuf
     integer(int8), allocatable :: b(:)
  end type ByteBuf

  integer(c_int) :: sock = -1

contains

  !--------------------------------------------------------------- socket I/O
  subroutine send_raw(b, n)
    integer(int8), intent(in), target :: b(:)
    integer(int64), intent(in) :: n
    if (n <= 0) return
    if (rpc_send_all(sock, c_loc(b(1)), int(n, c_int64_t)) /= 0) then
       write(*,'(A)') 'RPC: send failed, peer gone'
       call rpc_close(sock)
       stop 0
    end if
  end subroutine send_raw

  subroutine send_i32(v)
    integer(int32), intent(in) :: v
    call send_raw(transfer(v, [0_int8], 4), 4_int64)
  end subroutine send_i32

  subroutine send_i64(v)
    integer(int64), intent(in) :: v
    call send_raw(transfer(v, [0_int8], 8), 8_int64)
  end subroutine send_i64

  subroutine send_str(s)
    character(len=*), intent(in) :: s
    integer(int8), allocatable :: b(:)
    integer :: i
    allocate(b(max(len(s), 1)))
    do i = 1, len(s)
       b(i) = int(ichar(s(i:i)), int8)
    end do
    call send_raw(b, int(len(s), int64))
  end subroutine send_str

  !> Receive n bytes; .false. on EOF/error.
  logical function recv_raw(b, n) result(ok)
    integer(int8), intent(inout), target :: b(:)
    integer(int64), intent(in) :: n
    ok = .true.
    if (n <= 0) return
    ok = rpc_recv_all(sock, c_loc(b(1)), int(n, c_int64_t)) == 0
  end function recv_raw

  logical function recv_i32(v) result(ok)
    integer(int32), intent(out) :: v
    integer(int8) :: b(4)
    ok = recv_raw(b, 4_int64)
    v = transfer(b, 0_int32)
  end function recv_i32

  logical function recv_i64(v) result(ok)
    integer(int64), intent(out) :: v
    integer(int8) :: b(8)
    ok = recv_raw(b, 8_int64)
    v = transfer(b, 0_int64)
  end function recv_i64

  !--------------------------------------------------------------- server loop
  subroutine RPCServe(endpoint)
    character(len=*), intent(in) :: endpoint
    character(len=256) :: host, token
    character(len=32) :: op
    integer :: colon, port, ios, toklen, tokstat, i
    integer(int8) :: magic(4), opb(32)
    integer(int32) :: ver, nparam, nl
    integer(int64) :: params(8), ln
    type(ByteBuf), allocatable :: inputs(:)
    logical :: quit

    if (rpc_little_endian() /= 1) error stop "RPC: big-endian hosts are not supported"
    colon = index(endpoint, ':', back=.true.)
    if (colon <= 1) error stop "RPC: endpoint must be HOST:PORT"
    host = endpoint(1:colon-1)
    read(endpoint(colon+1:), *, iostat=ios) port
    if (ios /= 0) error stop "RPC: bad port"
    call get_environment_variable('MAGPARSER_RPC_TOKEN', token, toklen, tokstat)
    if (tokstat /= 0) toklen = 0

    sock = rpc_connect(trim(host)//c_null_char, int(port, c_int))
    if (sock < 0) error stop "RPC: cannot connect to the cluster worker"
    write(*,'(A,A,A,I0)') 'RPC: MAGPARSER ', commit_hash, ' serving ', port

    call send_str('MPHI'); call send_i32(RPC_VERSION); call send_i32(int(toklen, int32))
    if (toklen > 0) call send_str(token(1:toklen))

    quit = .false.
    do while (.not. quit)
       if (.not. recv_raw(magic, 4_int64)) exit
       if (any(magic /= [77_int8, 80_int8, 82_int8, 81_int8])) then   ! 'MPRQ'
          write(*,'(A)') 'RPC: bad request magic, closing'
          exit
       end if
       if (.not. recv_i32(ver)) exit
       if (.not. recv_raw(opb, 32_int64)) exit
       op = ''
       do i = 1, 32
          if (opb(i) == 0) exit
          op(i:i) = achar(opb(i))
       end do
       if (.not. recv_i32(nparam)) exit
       do i = 1, 8
          if (.not. recv_i64(params(i))) exit
       end do
       if (.not. recv_i32(nl)) exit
       if (nl < 0 .or. nl > 8) exit
       if (allocated(inputs)) deallocate(inputs)
       allocate(inputs(nl))
       do i = 1, nl
          if (.not. recv_i64(ln)) exit
          if (ln < 0 .or. ln > MAX_LAYER_BYTES) error stop "RPC: layer too large"
          allocate(inputs(i)%b(max(ln, 1_int64)))
          if (.not. recv_raw(inputs(i)%b, ln)) exit
          if (ln < size(inputs(i)%b, kind=int64)) inputs(i)%b = inputs(i)%b(1:0)   ! zero-length layer
       end do
       call handle(trim(adjustl(op)), nparam, params, inputs, quit)
    end do
    call rpc_close(sock)
  end subroutine RPCServe

  !--------------------------------------------------------------- one request
  subroutine handle(op, nparam, params, inputs, quit)
    character(len=*), intent(in) :: op
    integer(int32), intent(in) :: nparam
    integer(int64), intent(in) :: params(8)
    type(ByteBuf), intent(inout) :: inputs(:)
    logical, intent(inout) :: quit
    type(Layer), target :: A, B, C
    integer(int8), allocatable :: out(:)
    integer(int32) :: status
    integer(int64) :: t0, t1, rate, n_out, topo
    integer :: need, ok
    character(len=:), allocatable :: msg
    integer(K_COORDINATE_KIND) :: iv(4)

    call system_clock(t0, rate)
    status = 0; n_out = 0; msg = ''
    allocate(out(0))
    need = layers_needed(op)
    if (op == 'PING') then
       msg = 'MAGPARSER-RPC 1 commit=' // trim(commit_hash) // ' compiler=' // trim(compiler_version())
    else if (op == 'QUIT') then
       msg = 'bye'; quit = .true.
    else if (need == 0) then
       status = 1; msg = 'unknown op ' // op
    else if (size(inputs) /= need) then
       status = 3; msg = 'wrong number of layers for ' // op
    else
       ok = load_layer(inputs(1)%b, A)
       if (ok == 0 .and. need == 2) ok = load_layer(inputs(2)%b, B)
       if (ok /= 0) then
          status = 2; msg = 'malformed CompactLayer input'
       else
          topo = topo_code(op)
          select case (op)
          case ('OR');       call CalculateBoostOperation(A, B, C, K_BOOST_CONTROL_OR, 0_int64)
          case ('OR2');      call CalculateOR(A, B, C)
          case ('AND');      call CalculateAND(A, B, C)
          case ('NOT');      call CalculateNOT(A, B, C)
          case ('XOR');      call CalculateBoostOperation(A, B, C, K_BOOST_CONTROL_XOR, 0_int64)
          case ('BOOSTNOT'); call CalculateBoostOperation(A, B, C, K_BOOST_CONTROL_NOT, 0_int64)
          case ('GROW')
             iv = int(params(1:4), K_COORDINATE_KIND)
             call CalculateGROWLayer(A, C, iv)
          case ('SHRINK')
             iv = int(params(1:4), K_COORDINATE_KIND)
             if (nparam <= 1) iv(2:4) = iv(1)
             if (all(iv == iv(1))) then
                call CalculateSHRINKLayer(A, iv(1), C)
             else
                call CalculateSHRINKDirectional(A, C, iv)
             end if
          case ('EXTENT');   call CreateEXTENT(A, C)
          case ('COPY');     call CopyLayer(C, A)
          case ('MERGE');    call CalculateSingleLayerAND(A, C)
          case default
             call CalculateTopologicalSelect(A, B, C, topo)
          end select
          call encode_layer(C, out, n_out)
       end if
    end if
    call system_clock(t1)
    call send_str('MPRS'); call send_i32(status)
    call send_i64(int(real(t1 - t0, real64) * 1.0e6_real64 / real(rate, real64), int64))
    call send_i64(n_out); call send_i32(int(len(msg), int32)); call send_str(msg)
    call send_i64(size(out, kind=int64)); call send_raw(out, size(out, kind=int64))
    call LayerDeviceRelease(A); call LayerDeviceRelease(B); call LayerDeviceRelease(C)
  end subroutine handle

  integer function layers_needed(op) result(n)
    character(len=*), intent(in) :: op
    select case (op)
    case ('OR', 'OR2', 'AND', 'NOT', 'XOR', 'BOOSTNOT'); n = 2
    case ('GROW', 'SHRINK', 'EXTENT', 'COPY', 'MERGE');  n = 1
    case default
       n = merge(2, 0, topo_code(op) > 0)
    end select
  end function layers_needed

  integer(int64) function topo_code(op) result(code)
    character(len=*), intent(in) :: op
    character(len=28), parameter :: names(32) = [character(len=28) :: &
         'INTERACT', 'NOT_INTERACT', 'CUT', 'NOT_CUT', 'TOUCH', 'NOT_TOUCH', 'INSIDE', 'NOT_INSIDE', &
         'OUTSIDE', 'NOT_OUTSIDE', 'INSIDE_EDGE', 'NOT_INSIDE_EDGE', 'OUTSIDE_EDGE', 'NOT_OUTSIDE_EDGE', &
         'COINCIDENT_INSIDE_EDGE', 'NOT_COINCIDENT_INSIDE_EDGE', 'COINCIDENT_OUTSIDE_EDGE', &
         'NOT_COINCIDENT_OUTSIDE_EDGE', 'EQUAL', 'NOT_EQUAL', 'DISJOINT', 'NOT_DISJOINT', 'MEET', 'NOT_MEET', &
         'OVERLAPS', 'NOT_OVERLAPS', 'COVERS', 'NOT_COVERS', 'COVEREDBY', 'NOT_COVEREDBY', 'CONTAINS', 'NOT_CONTAINS']
    integer :: i
    code = -1
    do i = 1, size(names)
       if (op == trim(names(i))) then
          code = K_TOPO_INTERACT + i - 1     ! K_TOPO_* are consecutive from 101
          return
       end if
    end do
  end function topo_code

  !> CLW1 bytes -> Layer (Box form, R-tree built). 0 ok, else malformed.
  integer function load_layer(buf, L) result(rc)
    integer(int8), intent(in) :: buf(:)
    type(Layer), intent(inout), target :: L
    integer(int64) :: nbox
    integer :: st
    rc = 1
    if (size(buf) == 0) return
    call CompactFromWire(buf, L%compact, L%layer_boxes, nbox, st)
    if (st < 0) return
    if (st == 2) then                              ! raw boxes
       L%n_used = nbox
       L%n_alloc = size(L%layer_boxes, kind=int64)
       L%layerState = LAYER_STATE_NONE
    else if (L%compact%n == 0) then                ! empty layer
       call CompactRelease(L%compact)
       if (allocated(L%layer_boxes)) deallocate(L%layer_boxes)
       allocate(L%layer_boxes(1))
       L%n_used = 0; L%n_alloc = 1
    else
       L%is_compact = .true.
       L%compact_state = LAYER_STATE_NONE          ! healing state unknown: ops re-derive it
       call LayerExpand(L)
    end if
    rc = 0
  end function load_layer

  !> Layer result -> CLW1 (cold CompactLayer; raw boxes if the layer cannot be encoded).
  subroutine encode_layer(L, out, n_out)
    type(Layer), intent(inout) :: L
    integer(int8), allocatable, intent(inout) :: out(:)
    integer(int64), intent(out) :: n_out
    type(CompactLayer) :: cl
    integer :: st
    if (allocated(out)) deallocate(out)
    !> An op may leave its result compact, and very large banded outputs can be multi-part
    !> (L%compact + L%cextra(:)). LayerExpand concatenates every part into layer_boxes.
    if (L%is_compact) call LayerExpand(L, build_tree=.false.)
    n_out = L%n_used
    if (L%n_used <= 0) then
       call CompactToWire(cl, out)                 ! empty
       return
    end if
    call CompactEncode(L%layer_boxes(1:L%n_used), L%n_used, cl, st)
    if (st == 0) then
       call CompactPack(cl)
       call CompactToWire(cl, out)
       n_out = cl%n                                ! after exact-duplicate removal
    else
       call BoxesToWire(L%layer_boxes, L%n_used, out)
    end if
    call CompactRelease(cl)
  end subroutine encode_layer

end module RPCServerModule
