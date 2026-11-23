! File   : test_compact_layer.f90
! Purpose: Round-trip and size test for CompactLayerModule on KLBIN layer files.
!   test_compact_layer.exe file1.bin [file2.bin ...]
! For each file: encode -> every original box re-derived from its key must match exactly
! (bijection check) -> decode -> pack -> unpack -> keys identical; prints sizes and timings.
program test_compact_layer
  use iso_fortran_env, only: int64, real64
  use GeometryModule, only: Box
  use KLDataModule, only: LoadKLBin
  use CompactLayerModule
  use omp_lib
  implicit none
  character(len=1024) :: fname
  type(Box), allocatable :: boxes(:), dec(:)
  type(CompactLayer) :: cl
  integer(kind=int64), allocatable :: keys_hot(:)
  integer(kind=int64) :: n, i, nbad, nmissing
  integer :: ia, status, nfail
  real(kind=real64) :: t0, t_enc, t_dec, t_pack, t_unpack
  nfail = 0
  do ia = 1, command_argument_count()
     call get_command_argument(ia, fname)
     call LoadKLBin(trim(fname), boxes)
     n = size(boxes, kind=int64)
     t0 = omp_get_wtime()
     call CompactEncode(boxes, n, cl, status)
     t_enc = omp_get_wtime() - t0
     if (status /= 0) then
        write(*,*) 'ENCODE status ', status, ' for ', trim(fname)
        nfail = nfail + 1
        cycle
     end if
     ! every original box must map to a key present in the set and decode back to itself
     nbad = 0; nmissing = 0
     !$omp parallel do reduction(+:nbad,nmissing) schedule(static)
     do i = 1, n
        if (.not. key_present(cl%keys, CompactKeyOf(cl, boxes(i)))) nmissing = nmissing + 1
     end do
     t0 = omp_get_wtime()
     call CompactDecode(cl, dec)
     t_dec = omp_get_wtime() - t0
     !$omp parallel do reduction(+:nbad) schedule(static)
     do i = 1, cl%n
        if (CompactKeyOf(cl, dec(i)) /= cl%keys(i)) nbad = nbad + 1
     end do
     allocate(keys_hot, source=cl%keys)
     t0 = omp_get_wtime()
     call CompactPack(cl)
     t_pack = omp_get_wtime() - t0
     call CompactPrintStats(cl, trim(fname))
     t0 = omp_get_wtime()
     call CompactUnpack(cl)
     t_unpack = omp_get_wtime() - t0
     if (any(cl%keys /= keys_hot)) then
        write(*,*) 'FAIL: unpack keys differ'
        nfail = nfail + 1
     end if
     if (nbad /= 0 .or. nmissing /= 0) then
        write(*,*) 'FAIL: nbad=', nbad, ' nmissing=', nmissing
        nfail = nfail + 1
     end if
     write(*,'(A,I0,A,F7.3,A,F7.3,A,F7.3,A,F7.3,A)') '  dup_removed=', cl%n_input - cl%n, '  encode ', t_enc, &
          ' s  decode ', t_dec, ' s  pack ', t_pack, ' s  unpack ', t_unpack, ' s'
     deallocate(keys_hot, dec)
  end do
  if (nfail > 0) then
     write(*,*) 'TEST FAILED: ', nfail
     error stop 1
  end if
  write(*,*) 'ALL ROUND-TRIPS EXACT'
contains
  logical function key_present(keys, k)
    integer(kind=int64), intent(in) :: keys(:), k
    integer(kind=int64) :: lo, hi, mid
    lo = 1; hi = size(keys, kind=int64)
    do while (lo < hi)
       mid = (lo + hi) / 2
       if (keys(mid) < k) then
          lo = mid + 1
       else
          hi = mid
       end if
    end do
    key_present = (keys(lo) == k)
  end function key_present
end program test_compact_layer
