! File    : test_viewer_index.f90
! Author  : Sandeep Koranne (C) 2026
! Purpose : Headless MAGVIEWER load + pan/zoom query benchmark. Loads a GDS/OASIS file
!           exactly as MAGVIEWER does (LoadLayoutDBToCompressedIndex), reports load time
!           and index memory, then runs exact box queries over a fixed sequence of
!           windows and prints, per window, the number of distinct boxes and an
!           order-independent hash. Run once with MAGVIEWER_COMPACT=0 and once with the
!           default compact store: the per-window lines must be identical.
!
! Usage   : test_viewer_index.exe file.oas [max_layers] [n_windows] [window_frac]
program test_viewer_index
  use, intrinsic :: iso_fortran_env, only: int64, real64
  use, intrinsic :: iso_c_binding, only: c_int
  use CommonModule, only: K_COORDINATE_KIND
  use GeometryModule, only: Box
  use CompressedSpatialIndexModule, only: CompressedSpatialIndex, ChunkLRUCache, K_CSI_DEFAULT_CACHE
  use CompactLayerModule, only: CompactDecodeBlocks
  use LayoutDBLoaderModule, only: LoadLayoutDBToCompressedIndex
  implicit none

  type(CompressedSpatialIndex), allocatable, target :: indices(:)
  character(len=64), allocatable :: names(:)
  type(ChunkLRUCache), allocatable :: caches(:)
  type(Box) :: gmbr, view
  integer :: nl, i, w, nwin, maxl, nvis, k
  integer(int64) :: t0, t1, rate, nbox, tree_b, chunk_b, nuniq, qtot
  integer(int64) :: h, seed
  real(real64) :: frac, tload, tq, cx, cy, hw, hh
  character(len=1024) :: fname, arg
  integer(c_int), allocatable :: vb(:)
  integer(int64), allocatable :: keys(:,:)
  integer, parameter :: K_MAXCAP = 4000000
  type :: BoxArr
     type(Box), allocatable :: b(:)
     integer(int64) :: n = 0
  end type BoxArr
  type(BoxArr), allocatable :: oracle(:)
  logical :: verify
  integer(int64) :: hq, hb, nq, nbf, nbad, j
  character(len=8) :: ev
  integer :: evl, evs

  call get_command_argument(1, fname)
  if (len_trim(fname) == 0) stop 'usage: test_viewer_index.exe file.oas [max_layers] [n_windows] [window_frac]'
  maxl = 40; nwin = 24; frac = 0.002_real64
  if (command_argument_count() >= 2) then; call get_command_argument(2, arg); read(arg,*) maxl; end if
  if (command_argument_count() >= 3) then; call get_command_argument(3, arg); read(arg,*) nwin; end if
  if (command_argument_count() >= 4) then; call get_command_argument(4, arg); read(arg,*) frac; end if

  call system_clock(t0, rate)
  call LoadLayoutDBToCompressedIndex(trim(fname), maxl, .false., indices, names, nl, gmbr)
  call system_clock(t1)
  tload = real(t1 - t0, real64) / real(rate, real64)

  nbox = 0; tree_b = 0; chunk_b = 0
  do i = 1, nl
     if (indices(i)%is_empty()) cycle
     nbox = nbox + indices(i)%n_boxes
     tree_b = tree_b + indices(i)%tree_bytes()
     chunk_b = chunk_b + indices(i)%chunk_bytes()
     write(*,'(A,A20,A,I12,A,F7.3,A,L1)') 'LAYER ', trim(names(i)), ' boxes ', indices(i)%n_boxes, &
          ' B/box ', real(indices(i)%memory_bytes(), real64) / real(max(1_int64, indices(i)%n_boxes), real64), &
          ' compact ', indices(i)%compact_backed
  end do
  write(*,'(A,F9.2,A,I0,A,F9.3,A,F9.3,A)') 'LOAD ', tload, ' s  boxes ', nbox, '  tree ', &
       real(tree_b, real64) / 1d9, ' GB  geometry ', real(chunk_b, real64) / 1d9, ' GB'

  call get_environment_variable('TVI_VERIFY', ev, evl, evs)
  verify = (evs == 0 .and. evl > 0 .and. ev(1:1) /= '0')
  nbad = 0
  if (verify) then        ! brute-force oracle from the full compact geometry
     allocate(oracle(max(1, nl)))
     do i = 1, nl
        if (.not. indices(i)%compact_backed) cycle
        call indices(i)%decode_all(oracle(i)%b, oracle(i)%n)
     end do
  end if

  ! Deterministic window sequence (LCG), sized as frac of the design extent: a zoomed-in pan.
  allocate(caches(max(1,nl)))
  do i = 1, nl
     call caches(i)%init(K_CSI_DEFAULT_CACHE)
  end do
  allocate(keys(2, K_MAXCAP))
  seed = 12345_int64
  qtot = 0
  call system_clock(t0)
  do w = 1, nwin
     seed = mod(seed * 6364136223846793005_int64 + 1442695040888963407_int64, huge(seed))
     cx = gmbr%x1 + (gmbr%x2 - gmbr%x1) * real(iand(abs(seed), 1048575_int64), real64) / 1048576d0
     seed = mod(seed * 6364136223846793005_int64 + 1442695040888963407_int64, huge(seed))
     cy = gmbr%y1 + (gmbr%y2 - gmbr%y1) * real(iand(abs(seed), 1048575_int64), real64) / 1048576d0
     hw = 0.5d0 * frac * (gmbr%x2 - gmbr%x1); hh = 0.5d0 * frac * (gmbr%y2 - gmbr%y1)
     if (w == 1) then          ! one full-design overview query (LOD-capped like the viewer)
        view = gmbr
     else
        view%x1 = int(cx - hw, K_COORDINATE_KIND); view%x2 = int(cx + hw, K_COORDINATE_KIND)
        view%y1 = int(cy - hh, K_COORDINATE_KIND); view%y2 = int(cy + hh, K_COORDINATE_KIND)
     end if
     h = 0; nuniq = 0
     do i = 1, nl
        if (indices(i)%is_empty()) cycle
        call indices(i)%query_boxes_to_quads(view, 0_K_COORDINATE_KIND, caches(i), vb, nvis, &
             merge(1000, K_MAXCAP, w == 1))
        qtot = qtot + nvis
        if (w == 1) cycle      ! capped overview: traversal order differs, not compared
        do k = 1, nvis
           keys(1, k) = ior(shiftl(int(vb(8*k-7), int64), 32), iand(int(vb(8*k-6), int64), 4294967295_int64))
           keys(2, k) = ior(shiftl(int(vb(8*k-3), int64), 32), iand(int(vb(8*k-2), int64), 4294967295_int64))
        end do
        hq = 0; nq = 0
        call hash_unique(keys, nvis, i, hq, nq)
        h = h + hq; nuniq = nuniq + nq
        if (verify .and. indices(i)%compact_backed) then
           k = 0
           do j = 1, oracle(i)%n
              associate (bb => oracle(i)%b(j))
                if (bb%x2 < view%x1 .or. bb%x1 > view%x2 .or. bb%y2 < view%y1 .or. bb%y1 > view%y2) cycle
                k = k + 1
                keys(1, k) = ior(shiftl(int(bb%x1, int64), 32), iand(int(bb%y1, int64), 4294967295_int64))
                keys(2, k) = ior(shiftl(int(bb%x2, int64), 32), iand(int(bb%y2, int64), 4294967295_int64))
              end associate
           end do
           hb = 0; nbf = 0
           call hash_unique(keys, k, i, hb, nbf)
           if (hb /= hq .or. nbf /= nq) then
              nbad = nbad + 1
              write(*,'(A,I3,A,A,A,I0,A,I0)') 'MISMATCH win ', w, ' layer ', trim(names(i)), ' query ', nq, ' brute ', nbf
           end if
        end if
     end do
     if (w > 1) write(*,'(A,I3,A,I10,A,Z16.16)') 'WIN ', w, ' unique ', nuniq, ' hash ', h
  end do
  call system_clock(t1)
  tq = real(t1 - t0, real64) / real(rate, real64)
  write(*,'(A,F9.3,A,I0,A,I0,A,I0)') 'QUERY ', tq, ' s  boxes ', qtot, '  cache hits ', &
       sum(caches(1:nl)%hits), ' misses ', sum(caches(1:nl)%misses)
  if (verify) write(*,'(A,I0)') 'VERIFY mismatches ', nbad

contains

  !> Sort (x1y1, x2y2) pairs, drop duplicates, fold into an order-independent hash.
  subroutine hash_unique(kk, n, layer, hacc, nu)
    integer(int64), intent(inout) :: kk(:,:)
    integer, intent(in) :: n, layer
    integer(int64), intent(inout) :: hacc, nu
    integer(int64), allocatable :: a(:), b(:)
    integer(int64) :: z
    integer :: j
    if (n == 0) return
    a = kk(1, 1:n); b = kk(2, 1:n)
    call sort_pairs(a, b, n)
    do j = 1, n
       if (j > 1) then
          if (a(j) == a(j-1) .and. b(j) == b(j-1)) cycle
       end if
       z = a(j) * 31_int64 + b(j) * 1000003_int64 + int(layer, int64) * 7919_int64
       z = ieor(z, shiftr(z, 29)) * (-4658895280553007687_int64)
       z = ieor(z, shiftr(z, 32))
       hacc = hacc + z
       nu = nu + 1
    end do
  end subroutine hash_unique

  !> Simple in-place heap sort on (a,b) lexicographic.
  subroutine sort_pairs(a, b, n)
    integer(int64), intent(inout) :: a(:), b(:)
    integer, intent(in) :: n
    integer :: j, e
    do j = n / 2, 1, -1
       call sift(a, b, j, n)
    end do
    do e = n, 2, -1
       call swp(a, b, 1, e)
       call sift(a, b, 1, e - 1)
    end do
  end subroutine sort_pairs

  subroutine sift(a, b, s0, n)
    integer(int64), intent(inout) :: a(:), b(:)
    integer, intent(in) :: s0, n
    integer :: r, c
    r = s0
    do while (2 * r <= n)
       c = 2 * r
       if (c < n) then
          if (lt(a(c), b(c), a(c+1), b(c+1))) c = c + 1
       end if
       if (.not. lt(a(r), b(r), a(c), b(c))) return
       call swp(a, b, r, c)
       r = c
    end do
  end subroutine sift

  logical function lt(a1, b1, a2, b2)
    integer(int64), intent(in) :: a1, b1, a2, b2
    lt = (a1 < a2) .or. (a1 == a2 .and. b1 < b2)
  end function lt

  subroutine swp(a, b, i1, i2)
    integer(int64), intent(inout) :: a(:), b(:)
    integer, intent(in) :: i1, i2
    integer(int64) :: t
    t = a(i1); a(i1) = a(i2); a(i2) = t
    t = b(i1); b(i1) = b(i2); b(i2) = t
  end subroutine swp

end program test_viewer_index
