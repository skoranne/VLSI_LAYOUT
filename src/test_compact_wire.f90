! File   : test_compact_wire.f90
! Purpose: Decode a CLW1 (CompactLayer network form) file, re-encode it, and print stats.
!   test_compact_wire.exe layer.clw [out.clw]
program test_compact_wire
  use iso_fortran_env, only: int8, int64
  use GeometryModule, only: Box
  use CompactLayerModule
  use CompactWireModule
  implicit none
  character(len=1024) :: fn, fo
  integer(int8), allocatable :: buf(:), out(:)
  integer(int64) :: sz, nbox
  integer :: u, st
  type(CompactLayer) :: cl
  type(Box), allocatable :: boxes(:)
  call get_command_argument(1, fn)
  inquire(file=trim(fn), size=sz)
  allocate(buf(sz))
  open(newunit=u, file=trim(fn), access='stream', form='unformatted', status='old', action='read')
  read(u) buf
  close(u)
  call CompactFromWire(buf, cl, boxes, nbox, st)
  write(*,'(A,I0,A,I0,A,I0,A,L1,A,I0,A,I0)') 'status=', st, ' bytes=', sz, ' n=', cl%n, ' wide=', cl%wide, &
       ' nshape=', merge(size(cl%shape_w), 0, allocated(cl%shape_w)), ' nblk=', cl%nblk
  if (st /= 0) stop 1
  if (cl%n > 0) then
     call CompactDecodeCold(cl, boxes)
     write(*,'(A,4I12)') 'first box: ', boxes(1)%x1, boxes(1)%y1, boxes(1)%x2, boxes(1)%y2
     write(*,'(A,Z17)') 'BoxSetHash: ', BoxSetHash(boxes, cl%n)
  end if
  if (command_argument_count() >= 2) then
     call get_command_argument(2, fo)
     call CompactToWire(cl, out)
     open(newunit=u, file=trim(fo), access='stream', form='unformatted', status='replace', action='write')
     write(u) out
     close(u)
     write(*,'(A,I0,A)') 're-encoded ', size(out), ' bytes'
  end if
end program test_compact_wire
