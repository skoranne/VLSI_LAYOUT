! File: test_nurbs_offset.f90
! Purpose: OVER_UNDER/UNDER_OVER 1dbu offset regression for curvilinear MEMS testcases.
! Reads tessellated .magpoly files, runs TrapMerge+TrapMorph, writes mag.json.
program test_nurbs_offset
  use, intrinsic :: iso_fortran_env, only: int32, int64, real64
  use TrapezoidModule
  use TrapMergeModule
  use TrapPNumModule
  use TrapMorphModule
  implicit none

  character(len=1024) :: workdir, fname, line, cmd
  character(len=256) :: name
  integer :: nargs, i, j, k, ncases, ios, u
  integer(int64) :: npoly, nv, d
  integer(int64), allocatable :: poff(:), px(:), py(:)
  type(Trapezoid), allocatable :: A(:), G(:), OU(:), S(:), UO(:)
  integer(int64) :: nA, nG, nOU, nS, nUO
  type(TrapMergeStats) :: mst
  type(TrapMorphStats) :: ost
  integer :: status
  real(real64) :: a_orig, a_ou, a_uo, scale
  character(len=256), allocatable :: names(:)
  real(real64), allocatable :: orig_a(:), ou_a(:), uo_a(:)
  integer :: nfound

  nargs = command_argument_count()
  if (nargs < 1) then
     write(*,'(A)') 'usage: test_nurbs_offset <workdir>'; error stop 2
  end if
  call get_command_argument(1, workdir)
  d = 1000_int64  ! 1um in nm scale
  scale = 1000.0_real64

  ! known testcase names
  ncases = 6
  allocate(names(ncases), orig_a(ncases), ou_a(ncases), uo_a(ncases))
  names = [character(len=256) :: 'accel_differential', 'actuator_comb_linear', 'gear_involute', &
           'gyro_tuning_fork', 'mag_flux_concentrator', 'mirror_gimbal_2axis']
  nfound = 0
  do k = 1, ncases
     write(fname,'(A)') trim(workdir)//'/'//trim(names(k))//'.magpoly'
     open(newunit=u, file=trim(fname), status='old', action='read', iostat=ios)
     if (ios /= 0) then
        write(*,'(A,A)') 'SKIP ', trim(fname); cycle
     end if
     read(u,*) npoly
     allocate(poff(npoly+1))
     poff(1) = 1
     ! first pass to count vertices
     do i = 1, npoly
        read(u,*) nv
        poff(i+1) = poff(i) + nv
        do j = 1, nv
           read(u,*) ! skip
        end do
     end do
     allocate(px(poff(npoly+1)-1), py(poff(npoly+1)-1))
     rewind(u)
     read(u,*) npoly
     do i = 1, npoly
        read(u,*) nv
        do j = 0, nv-1
           read(u,*) px(poff(i)+j), py(poff(i)+j)
        end do
     end do
     close(u)

     call build_net(npoly, poff, px, py, A, nA, status)
     if (status /= 0) then
        write(*,'(A,A)') 'FAIL build_net ', trim(names(k)); cycle
     end if
     a_orig = real(area2_of(A,nA), real64) / (2.0_real64 * scale * scale)

     ! OVER_UNDER: +d then -d
     call TrapMorph(A, nA, d,d,d,d, .false., G, nG, ost, status)
     if (status /= 0) then
        write(*,'(A,A)') 'FAIL grow ', trim(names(k)); cycle
     end if
     call TrapMorph(G, nG, d,d,d,d, .true., OU, nOU, ost, status)
     if (status /= 0) then
        write(*,'(A,A)') 'FAIL OU shrink ', trim(names(k)); cycle
     end if
     a_ou = real(area2_of(OU,nOU), real64) / (2.0_real64 * scale * scale)

     ! UNDER_OVER: -d then +d
     call TrapMorph(A, nA, d,d,d,d, .true., S, nS, ost, status)
     if (status /= 0) then
        write(*,'(A,A)') 'FAIL shrink ', trim(names(k)); cycle
     end if
     call TrapMorph(S, nS, d,d,d,d, .false., UO, nUO, ost, status)
     if (status /= 0) then
        write(*,'(A,A)') 'FAIL UO grow ', trim(names(k)); cycle
     end if
     a_uo = real(area2_of(UO,nUO), real64) / (2.0_real64 * scale * scale)

     orig_a(k)=a_orig; ou_a(k)=a_ou; uo_a(k)=a_uo; nfound=nfound+1
     write(*,'(A,A,A,F14.3,A,F14.3,A,F14.3)') trim(names(k)), ' orig=', '', a_orig, ' OU=', a_ou, ' UO=', a_uo

     deallocate(poff,px,py)
     if (allocated(A)) deallocate(A)
     if (allocated(G)) deallocate(G)
     if (allocated(OU)) deallocate(OU)
     if (allocated(S)) deallocate(S)
     if (allocated(UO)) deallocate(UO)
  end do

  ! write mag.json
  write(fname,'(A)') trim(workdir)//'/mag.json'
  open(newunit=u, file=trim(fname), status='replace', action='write')
  write(u,'(A)') '{'
  do k = 1, ncases
     write(u,'(A,A,A)',advance='no') '  "', trim(names(k)), '": {"orig":{"area":'
     write(u,'(F18.6)',advance='no') orig_a(k)
     write(u,'(A)',advance='no') '},"over_under":{"area":'
     write(u,'(F18.6)',advance='no') ou_a(k)
     write(u,'(A)',advance='no') '},"under_over":{"area":'
     write(u,'(F18.6)',advance='no') uo_a(k)
     write(u,'(A)',advance='no') '}}'
     if (k < ncases) write(u,'(A)') ','; if (k==ncases) write(u,'(A)') ''
  end do
  write(u,'(A)') '}'
  close(u)
  write(*,'(A,A)') 'wrote ', trim(fname)

contains

  integer(int64) function poly_signed_area2(nv, xs, ys) result(a2)
    integer(int64), intent(in) :: nv, xs(nv), ys(nv)
    integer(int64) :: i, j
    a2 = 0
    j = nv
    do i = 1, nv
       a2 = a2 + xs(j)*ys(i) - xs(i)*ys(j)
       j = i
    end do
  end function poly_signed_area2

  subroutine build_net(npoly, poff, px, py, out, nout, status)
    integer(int64), intent(in) :: npoly, poff(:), px(:), py(:)
    type(Trapezoid), allocatable, intent(out) :: out(:)
    integer(int64), intent(out) :: nout
    integer, intent(out) :: status
    integer(int64) :: i, nv, npos, nhol, vpos, vhol, ip, ih, j
    integer(int64), allocatable :: poA(:), xA(:), yA(:), poB(:), xB(:), yB(:)
    type(Trapezoid), allocatable :: tA(:), tB(:)
    integer(int64) :: nA, nB
    type(TrapMergeStats) :: mst
    type(TrapList) :: res(1)
    integer(int32) :: ops(1)
    npos=0; nhol=0; vpos=0; vhol=0
    do i=1,npoly
       nv = poff(i+1)-poff(i)
       if (poly_signed_area2(nv, px(poff(i):poff(i+1)-1), py(poff(i):poff(i+1)-1)) >= 0) then
          npos=npos+1; vpos=vpos+nv
       else
          nhol=nhol+1; vhol=vhol+nv
       end if
    end do
    allocate(poA(npos+1), xA(max(vpos,1_int64)), yA(max(vpos,1_int64)))
    allocate(poB(nhol+1), xB(max(vhol,1_int64)), yB(max(vhol,1_int64)))
    poA(1)=1; poB(1)=1; ip=0; ih=0; vpos=0; vhol=0
    do i=1,npoly
       nv = poff(i+1)-poff(i)
       if (poly_signed_area2(nv, px(poff(i):poff(i+1)-1), py(poff(i):poff(i+1)-1)) >= 0) then
          ip=ip+1
          xA(vpos+1:vpos+nv)=px(poff(i):poff(i+1)-1)
          yA(vpos+1:vpos+nv)=py(poff(i):poff(i+1)-1)
          vpos=vpos+nv; poA(ip+1)=vpos+1
       else
          ih=ih+1
          xB(vhol+1:vhol+nv)=px(poff(i):poff(i+1)-1)
          yB(vhol+1:vhol+nv)=py(poff(i):poff(i+1)-1)
          vhol=vhol+nv; poB(ih+1)=vhol+1
       end if
    end do
    if (npos==0) then
       allocate(out(0)); nout=0; status=0; return
    end if
    call TrapMergePolygons(npos, poA, xA, yA, tA, nA, mst, status)
    if (status/=0) return
    if (nhol==0) then
       call move_alloc(tA,out); nout=nA; status=0; return
    end if
    call TrapMergePolygons(nhol, poB, xB, yB, tB, nB, mst, status)
    if (status/=0) return
    ops(1)=TOP_NOT
    call TrapBooleanPolygons(npos, poA, xA, yA, nhol, poB, xB, yB, 1, ops, res, mst, status)
    if (status/=0) return
    nout=res(1)%n
    call move_alloc(res(1)%t, out)
  end subroutine build_net

  integer(int64) function area2_of(t, n) result(a2)
    type(Trapezoid), intent(in) :: t(:)
    integer(int64), intent(in) :: n
    type(TrapPolys) :: P
    integer(int64) :: i
    if (n <= 0) then; a2=0; return; end if
    call TrapPNum(t, n, P)
    a2 = 0
    do i = 1, P%np
       a2 = a2 + P%area2(i)
    end do
  end function area2_of

end program test_nurbs_offset
