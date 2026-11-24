! File   : tech_stacks.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: Built-in interconnect stacks (GDS layer/datatype, DEF/LEF layer name, routing or cut)
!          for SKY130, ASAP7 and GT2N, used by
!
!   var tech SKY130                      layer names (met1, li1, M3, BV0 ...) as aliases of
!                                        the L<layer>_<datatype> layers of designs loaded later
!   exec tech d1 SKY130                  the same for an already loaded design
!   CONNECT STACK SKY130 d1 [lo hi]      CONNECT over the stack (or its lo..hi slice), using the
!                                        layers that are present in d1
!   exec defcheck d1:met1 file.def       DEF layer names of the CONNECT members
!
! Sources: SKY130 - sky130 KLayout tech / Magic CALMA table (drawing datatype 20, cuts 44);
! ASAP7 - OpenROAD-flow-scripts platforms/asap7/KLayout/asap7.lyt; GT2N - GT2N
! techlib/gt2_techfile.layermap and the platform's gt2n.lyt (front side M0..RDL, backside
! BPR..BRDL, VBPR joining BPR to M0; datatype 0). Stacks: SKY130 (default li1..met5), ASAP7,
! GT2N (front), GT2N_BS (backside), GT2N_FULL (backside + VBPR + front).
module TechStackModule
  use hash_mod
  implicit none
  private

  type :: TechLayer
     character(len=8) :: name = ''      !> PDK / DEF layer name
     character(len=8) :: alt  = ''      !> extra alias ('' = none)
     integer :: gl = 0, gd = 0          !> GDS layer / datatype
     character :: kind = 'R'            !> 'R' routing, 'C' cut
  end type TechLayer

  type :: TechStack
     character(len=16) :: name = '', family = ''
     character(len=200) :: about = ''
     type(TechLayer), allocatable :: L(:)
     integer :: lo = 1, hi = 0          !> default CONNECT STACK slice
  end type TechStack

  type(TechStack), allocatable, save :: stacks(:)
  character(len=16), public, save :: current_tech = ''

  public :: TechCanon, TechStackIndex, TechAliasLayers, TechStackSpecs, TechDefNames, TechList

contains

  subroutine init_stacks()
    integer :: k
    if( allocated( stacks ) ) return
    allocate( stacks(5) )
    !> SKY130: poly/licon1 below li1 are available as an explicit slice (CONNECT STACK SKY130 d1
    !> poly met5); diff is not, it runs under the gates and would short source and drain.
    stacks(1)%name = 'SKY130'; stacks(1)%family = 'SKY130'
    stacks(1)%about = 'SkyWater 130nm: poly licon1 li1 mcon met1 via met2 via2 met3 via3 met4 via4 met5'
    stacks(1)%L = [ tl('poly',66,20,'R'), tl('licon1',66,44,'C','licon'), tl('li1',67,20,'R'), &
         tl('mcon',67,44,'C'), tl('met1',68,20,'R'), tl('via',68,44,'C','via1'), tl('met2',69,20,'R'), &
         tl('via2',69,44,'C'), tl('met3',70,20,'R'), tl('via3',70,44,'C'), tl('met4',71,20,'R'), &
         tl('via4',71,44,'C'), tl('met5',72,20,'R') ]
    stacks(1)%lo = 3; stacks(1)%hi = 13
    !> ASAP7: cells pin on M1 (V0 joins M1 to both LIG and LISD, which is not a chain)
    stacks(2)%name = 'ASAP7'; stacks(2)%family = 'ASAP7'
    stacks(2)%about = 'ASAP7 7nm predictive: M1 V1 M2 ... V8 M9'
    stacks(2)%L = [ tl('M1',19,0,'R'), tl('V1',21,0,'C'), tl('M2',20,0,'R'), tl('V2',25,0,'C'), &
         tl('M3',30,0,'R'), tl('V3',35,0,'C'), tl('M4',40,0,'R'), tl('V4',45,0,'C'), tl('M5',50,0,'R'), &
         tl('V5',55,0,'C'), tl('M6',60,0,'R'), tl('V6',65,0,'C'), tl('M7',70,0,'R'), tl('V7',75,0,'C'), &
         tl('M8',80,0,'R'), tl('V8',85,0,'C'), tl('M9',90,0,'R') ]
    !> GT2N front side
    stacks(3)%name = 'GT2N'; stacks(3)%family = 'GT2N'
    stacks(3)%about = 'GT2N 2nm front side: M0 V0 M1 ... M13 V13 RDL'
    allocate( stacks(3)%L(29) )
    stacks(3)%L(1) = tl('M0',20,0,'R')
    do k = 0, 12
       stacks(3)%L(2+2*k) = tl('V'//itoa(k), 22+5*k, 0, 'C')
       stacks(3)%L(3+2*k) = tl('M'//itoa(k+1), 25+5*k, 0, 'R')
    end do
    stacks(3)%L(28) = tl('V13',87,0,'C')
    stacks(3)%L(29) = tl('RDL',90,0,'R')
    !> GT2N backside power: buried power rail up through the backside metals
    stacks(4)%name = 'GT2N_BS'; stacks(4)%family = 'GT2N'
    stacks(4)%about = 'GT2N 2nm backside: BPR BV0 BM1 BV1 BM2 BV2 BM3 BV3 BM4 BV4 BRDL'
    stacks(4)%L = [ tl('BPR',8,0,'R'), tl('BV0',910,0,'C'), tl('BM1',920,0,'R'), tl('BV1',930,0,'C'), &
         tl('BM2',940,0,'R'), tl('BV2',950,0,'C'), tl('BM3',960,0,'R'), tl('BV3',970,0,'C'), &
         tl('BM4',980,0,'R'), tl('BV4',990,0,'C'), tl('BRDL',1000,0,'R') ]
    !> GT2N backside + front side through the buried power rail, as the platform's KLayout
    !> net tracer connects them (gt2n.lyt: BPR,VBPR,M0): BRDL BV4 BM4 ... BM1 BV0 BPR VBPR M0 V0 ... RDL
    stacks(5)%name = 'GT2N_FULL'; stacks(5)%family = 'GT2N'
    stacks(5)%about = 'GT2N 2nm backside + front side: BRDL BV4 ... BM1 BV0 BPR VBPR M0 V0 M1 ... V13 RDL'
    allocate( stacks(5)%L( size(stacks(4)%L) + 1 + size(stacks(3)%L) ) )
    do k = 1, size(stacks(4)%L)
       stacks(5)%L(k) = stacks(4)%L( size(stacks(4)%L) + 1 - k )
    end do
    stacks(5)%L( size(stacks(4)%L) + 1 ) = tl('VBPR',9,0,'C')
    stacks(5)%L( size(stacks(4)%L) + 2 : ) = stacks(3)%L
    do k = 2, 5
       stacks(k)%lo = 1
    end do
    do k = 1, 5
       if( stacks(k)%hi == 0 ) stacks(k)%hi = size( stacks(k)%L )
    end do
  end subroutine init_stacks

  type(TechLayer) function tl( name, gl, gd, kind, alt ) result(t)
    character(len=*), intent(in) :: name
    integer, intent(in) :: gl, gd
    character, intent(in) :: kind
    character(len=*), intent(in), optional :: alt
    t%name = name; t%gl = gl; t%gd = gd; t%kind = kind
    if( present(alt) ) t%alt = alt
  end function tl

  pure function itoa( i ) result(s)
    integer, intent(in) :: i
    character(len=:), allocatable :: s
    character(len=16) :: b
    write(b,'(I0)') i
    s = trim(b)
  end function itoa

  pure function upper( s ) result(u)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: u
    integer :: i
    u = s
    do i = 1, len(s)
       if( u(i:i) >= 'a' .and. u(i:i) <= 'z' ) u(i:i) = achar( iachar(u(i:i)) - 32 )
    end do
  end function upper

  pure function lower( s ) result(u)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: u
    integer :: i
    u = s
    do i = 1, len(s)
       if( u(i:i) >= 'A' .and. u(i:i) <= 'Z' ) u(i:i) = achar( iachar(u(i:i)) + 32 )
    end do
  end function lower

  !> Canonical stack name for a user spelling ('' if unknown).
  function TechCanon( name ) result(c)
    character(len=*), intent(in) :: name
    character(len=16) :: c
    character(len=len(name)) :: u
    u = upper( adjustl(name) )
    select case( trim(u) )
    case( 'SKY130', 'SKYWATER130', 'SKY130A', 'SKY130B', 'SKY130HD', 'SKYWATER' )
       c = 'SKY130'
    case( 'ASAP7' )
       c = 'ASAP7'
    case( 'GT2N', 'GT2' )
       c = 'GT2N'
    case( 'GT2N_BS', 'GT2N_BACKSIDE', 'GT2_BS' )
       c = 'GT2N_BS'
    case( 'GT2N_FULL', 'GT2_FULL' )
       c = 'GT2N_FULL'
    case default
       c = ''
    end select
  end function TechCanon

  integer function TechStackIndex( name ) result(k)
    character(len=*), intent(in) :: name
    character(len=16) :: c
    call init_stacks()
    c = TechCanon( name )
    do k = 1, size(stacks)
       if( stacks(k)%name == c .and. len_trim(c) > 0 ) return
    end do
    k = 0
  end function TechStackIndex

  subroutine TechList()
    integer :: k, i
    call init_stacks()
    do k = 1, size(stacks)
       write(*,'(A,A,A,A)') '  ', stacks(k)%name, ' ', trim(stacks(k)%about)
       write(*,'(A)', advance='no') '     '
       do i = 1, size(stacks(k)%L)
          write(*,'(A,A,I0,A,I0,A)', advance='no') trim(stacks(k)%L(i)%name), '=', stacks(k)%L(i)%gl, &
               '/', stacks(k)%L(i)%gd, ' '
       end do
       write(*,*)
    end do
  end subroutine TechList

  !> Index of the design layer that holds GDS gl/gd (0 if absent).
  integer function layer_of( ht, gl, gd ) result(il)
    type(hash_type), intent(in) :: ht
    integer, intent(in) :: gl, gd
    logical :: found
    call hash_get( ht, 'L'//itoa(gl)//'_'//itoa(gd), il, found )
    if( .not. found ) il = 0
  end function layer_of

  !> Add the stack layer names of every stack of the family of `tech` as aliases of the present
  !> L<layer>_<datatype> layers (as spelled, lower and upper case). Existing names are kept.
  subroutine TechAliasLayers( ht, tech, nadded, npresent )
    type(hash_type), intent(inout) :: ht
    character(len=*), intent(in) :: tech
    integer, intent(out) :: nadded, npresent
    integer :: k, i, il, j, v
    character(len=16) :: fam
    character(len=8) :: nm(6)
    logical :: found
    call init_stacks()
    nadded = 0; npresent = 0
    k = TechStackIndex( tech )
    if( k == 0 ) return
    fam = stacks(k)%family
    do k = 1, size(stacks)
       if( stacks(k)%family /= fam ) cycle
       do i = 1, size(stacks(k)%L)
          associate( t => stacks(k)%L(i) )
            il = layer_of( ht, t%gl, t%gd )
            if( il == 0 ) cycle
            npresent = npresent + 1
            nm = [ t%name, lower(t%name), upper(t%name), t%alt, lower(t%alt), upper(t%alt) ]
            do j = 1, 6
               if( len_trim(nm(j)) == 0 ) cycle
               call hash_get( ht, trim(nm(j)), v, found )
               if( found ) cycle
               call hash_put( ht, trim(nm(j)), il )
               nadded = nadded + 1
            end do
          end associate
       end do
    end do
  end subroutine TechAliasLayers

  !> Find a stack position by layer name (case-insensitive, name or alias).
  integer function find_name( k, nm ) result(i)
    integer, intent(in) :: k
    character(len=*), intent(in) :: nm
    do i = 1, size(stacks(k)%L)
       if( upper(trim(stacks(k)%L(i)%name)) == upper(trim(nm)) ) return
       if( len_trim(stacks(k)%L(i)%alt) > 0 ) then
          if( upper(trim(stacks(k)%L(i)%alt)) == upper(trim(nm)) ) return
       end if
    end do
    i = 0
  end function find_name

  !> CONNECT STACK: db:L<l>_<d> specs for stack `tech` (slice lo..hi by layer name, '' = default)
  !> restricted to the layers present in the design: leading/trailing absent layers are dropped
  !> (and a cut at either end), an absent layer inside the slice is an error.
  subroutine TechStackSpecs( tech, db, ht, lo_name, hi_name, specs, defn, status, msg )
    character(len=*), intent(in) :: tech, db, lo_name, hi_name
    type(hash_type), intent(in) :: ht
    character(len=64), allocatable, intent(out) :: specs(:), defn(:)
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    integer :: k, lo, hi, i, a, b
    logical, allocatable :: have(:)
    call init_stacks()
    status = 0; msg = ''
    k = TechStackIndex( tech )
    if( k == 0 ) then
       status = 1; msg = 'CONNECT STACK: unknown technology "'//trim(tech)//'" (SKY130, ASAP7, GT2N, GT2N_BS, GT2N_FULL)'
       return
    end if
    lo = stacks(k)%lo; hi = stacks(k)%hi
    if( len_trim(lo_name) > 0 ) lo = find_name( k, lo_name )
    if( len_trim(hi_name) > 0 ) hi = find_name( k, hi_name )
    if( lo == 0 .or. hi == 0 ) then
       if( lo == 0 ) then
          msg = 'CONNECT STACK: layer "'//trim(lo_name)//'" is not in the '//trim(stacks(k)%name)//' stack'
       else
          msg = 'CONNECT STACK: layer "'//trim(hi_name)//'" is not in the '//trim(stacks(k)%name)//' stack'
       end if
       status = 1
       return
    end if
    if( lo > hi ) then
       i = lo; lo = hi; hi = i
    end if
    if( stacks(k)%L(lo)%kind /= 'R' .or. stacks(k)%L(hi)%kind /= 'R' ) then
       status = 1; msg = 'CONNECT STACK: a slice must start and end on routing layers'
       return
    end if
    allocate( have(lo:hi) )
    do i = lo, hi
       have(i) = layer_of( ht, stacks(k)%L(i)%gl, stacks(k)%L(i)%gd ) > 0
    end do
    a = lo
    do while( a <= hi )
       if( have(a) .and. stacks(k)%L(a)%kind == 'R' ) exit
       a = a + 1
    end do
    b = hi
    do while( b >= a )
       if( have(b) .and. stacks(k)%L(b)%kind == 'R' ) exit
       b = b - 1
    end do
    if( b - a < 2 ) then
       status = 1
       msg = 'CONNECT STACK '//trim(stacks(k)%name)//': fewer than two connected routing layers of the stack are present in '//trim(db)
       return
    end if
    do i = a, b
       if( .not. have(i) ) then
          status = 1
          msg = 'CONNECT STACK '//trim(stacks(k)%name)//': layer '//trim(stacks(k)%L(i)%name)//' (L'// &
               itoa(stacks(k)%L(i)%gl)//'_'//itoa(stacks(k)%L(i)%gd)//') is missing in '//trim(db)// &
               ' inside the stack (not in the file, or skipped at load: raise max_layers); give a slice that avoids it'
          return
       end if
    end do
    allocate( specs(b-a+1), defn(b-a+1) )
    do i = a, b
       specs(i-a+1) = trim(db)//':L'//itoa(stacks(k)%L(i)%gl)//'_'//itoa(stacks(k)%L(i)%gd)
       defn(i-a+1)  = stacks(k)%L(i)%name
    end do
  end subroutine TechStackSpecs

  !> DEF layer names of CONNECT members given as db:<layer> specs: matched against the stacks
  !> of `tech` (or of every technology if tech is ''), by name/alias or by L<l>_<d>, L<l>_D<d>,
  !> <l>:<d>, <l>_<d>. All members must resolve in one technology; '' entries otherwise.
  subroutine TechDefNames( tech, specs, defn, tech_used, status, msg )
    character(len=*), intent(in) :: tech
    character(len=*), intent(in) :: specs(:)
    character(len=64), allocatable, intent(out) :: defn(:)
    character(len=16), intent(out) :: tech_used
    integer, intent(out) :: status
    character(len=:), allocatable, intent(out) :: msg
    integer :: k, i, j, p, nmatch
    character(len=64), allocatable :: try(:)
    character(len=64) :: ln
    logical :: ok
    call init_stacks()
    status = 0; msg = ''; tech_used = ''
    allocate( defn(size(specs)), try(size(specs)) )
    defn = ''
    nmatch = 0
    do k = 1, size(stacks)
       if( len_trim(tech) > 0 ) then
          if( stacks(k)%family /= stacks(max(1,TechStackIndex(tech)))%family ) cycle
       end if
       ok = .true.
       do i = 1, size(specs)
          p = index( specs(i), ':' )
          ln = specs(i)(p+1:)
          try(i) = ''
          j = find_name( k, ln )
          if( j == 0 ) j = find_gds( k, ln )
          if( j == 0 ) then
             ok = .false.; exit
          end if
          try(i) = stacks(k)%L(j)%name
       end do
       if( ok ) then
          nmatch = nmatch + 1
          if( nmatch == 1 ) then
             defn = try; tech_used = stacks(k)%name
          end if
       end if
    end do
    if( nmatch == 0 ) then
       status = 1
       msg = 'the CONNECT layers do not all belong to one built-in stack (use CONNECT STACK or var tech)'
    else if( nmatch > 1 .and. len_trim(tech) == 0 ) then
       status = 1
       msg = 'the CONNECT layers match more than one technology; set var tech SKY130|ASAP7|GT2N'
    end if
  end subroutine TechDefNames

  integer function find_gds( k, ln ) result(j)
    integer, intent(in) :: k
    character(len=*), intent(in) :: ln
    character(len=32) :: f(4)
    integer :: m
    do j = 1, size(stacks(k)%L)
       associate( t => stacks(k)%L(j) )
         f(1) = 'L'//itoa(t%gl)//'_'//itoa(t%gd)
         f(2) = 'L'//itoa(t%gl)//'_D'//itoa(t%gd)
         f(3) = itoa(t%gl)//':'//itoa(t%gd)
         f(4) = itoa(t%gl)//'_'//itoa(t%gd)
         do m = 1, 4
            if( trim(f(m)) == trim(ln) ) return
         end do
       end associate
    end do
    j = 0
  end function find_gds

end module TechStackModule
