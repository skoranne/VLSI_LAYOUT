! File   : controlparser.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: VLSI Layout Magic file parsing and operation
!        : we want to compare RTree with other methods
! program my_control
! decl design d1 = FPU.mag
! decl design d2 = STD.mag
! decl output o1 = FOO.mag
! decl output o2 = FOO.gds
! info
! decl int ctr = 2
! decl real dmin = 3.0
! decl layer diff = d1:65:20
! decl layer poly = d1:66:20
! var layer gate = ( diff * poly )
! var layer source_drain = ( diff - poly )
! decl group g1 = [ gate, source_drain ]
! exec run g1
! exec push g1 o1:gate
! exec push g1 o2:10:0
! end my_control

module ControlModule
  use hash_mod
  use DesignModule
  use SystemInformationModule
  use MagicVLSILayoutParser  
  implicit none
  public :: ParseControlFile
contains
  subroutine ParseControlFile( fileName, MAX_LAYERS )
    character(len=*), intent(in) :: fileName
    integer, intent(in) :: MAX_LAYERS
    integer, parameter  :: K_MAX_DBS = 32 !< will this work
    character(len=256) :: line, keyword, rest
    integer :: unit, ios, pos, line_number, db_count
    type(Design), allocatable :: design_dbs(:)
    type(hash_type) :: ht
    logical :: ins
    db_count = 1
    call hash_create(ht,K_MAX_DBS)
    allocate( design_dbs( K_MAX_DBS ) )
    ! Open the file
    open(newunit=unit, file=fileName, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       print *, "Error: Could not open file."
       stop
    end if
    line_number = 1
    ! Read line by line
    read_loop: do
       read(unit, '(A)', end=100,iostat=ios) line
       write(*,'(A,I4,A,A80)') 'Line number: ', line_number, ' ', line       
       if (ios < 0) exit read_loop ! End of file
       line_number = line_number+1
       line = adjustl(line)       ! Remove leading whitespace
       if (len(line) == 0) cycle  ! Skip empty lines

       ! Extract the first keyword (everything before the first space)
       pos = index(line, ' ')
       if (pos == 0) then
          keyword = line
          rest = ""
       else
          keyword = line(1:pos-1)
          rest = adjustl(line(pos+1:))
       end if
       if( keyword == "#" ) cycle read_loop
       ! Process based on keyword
       select case (keyword)
       case ('program')
          print *, "--- Starting Program:", trim(rest)
          call InitSystem()
          call StartMarkTime("program")
          call StopMarkTime("program")
       case ('decl')
          rest = adjustl( rest ) !> remove leading whitespace
          write(*,*) 'Declaration found: ', trim(rest)
          ! Further logic to parse types (int, real, design, etc.)
          pos = index( rest, ' ' )
          write(*,*) 'KW (design/output) = ', trim(rest(1:pos-1))
          if( trim(rest(1:pos-1)) == 'output' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             write(*,*) 'Output DB handle = ', trim(rest(1:pos-1)) !> o1
             call hash_put( ht, trim(rest(1:pos-1)) , db_count, ins )
             if( .not. ins ) write (*,*) 'Duplicate db handle seen: ', trim(rest(1:pos-1))
             pos = index( rest, ' ' )          
             keyword = adjustl(rest(pos+2:))
             pos = index( keyword, ' ' )
             write(*,*) 'Generating output in MAGIC file: ', trim(keyword(1:pos)) !> file
             db_count = db_count+1
          else if( trim(rest(1:pos-1)) == 'design' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             write(*,*) 'KW = ', trim(rest(1:pos-1)) !> d1
             call hash_put( ht, trim(rest(1:pos-1)) , db_count, ins )
             if( .not. ins ) write (*,*) 'Duplicate db handle seen: ', trim(rest(1:pos-1))
             pos = index( rest, ' ' )          
             keyword = adjustl(rest(pos+2:))
             pos = index( keyword, ' ' )
             write(*,*) 'DB handle = ', trim(keyword(1:pos)) !> file
             call parseMagicLayoutFile(design_dbs(db_count), trim(keyword(1:pos)), MAX_LAYERS)
             db_count = db_count + 1
          else
             write(*,*) 'Syntax ERROR: line: ', line_number
             error stop "Syntax ERROR."
          end if
       case ('systeminfo') !> print information about the state of the system
          call PrintFullInformation()
       case ('info') !> print information about RSS of current pid
       case ('var')
          print *, "Variable definition:", rest

       case ('exec')
          print *, "Execution command:", rest
          rest = adjustl( rest ) !> remove leading whitespace
          write(*,*) 'Declaration found: ', trim(rest)
          ! Further logic to parse types (int, real, design, etc.)
          pos = index( rest, ' ' )
          write(*,*) 'KW (design/output) = ', trim(rest(1:pos-1))
          if( trim(rest(1:pos-1)) == 'output' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             write(*,*) 'Output DB handle = ', trim(rest(1:pos-1)) !> o1
             call hash_put( ht, trim(rest(1:pos-1)) , db_count, ins )
             if( .not. ins ) write (*,*) 'Duplicate db handle seen: ', trim(rest(1:pos-1))
             pos = index( rest, ' ' )          
             keyword = adjustl(rest(pos+2:))
             pos = index( keyword, ' ' )
             write(*,*) 'Generating output in MAGIC file: ', trim(keyword(1:pos)) !> file
             db_count = db_count+1
          else if( trim(rest(1:pos-1)) == 'design' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             write(*,*) 'KW = ', trim(rest(1:pos-1)) !> d1
             call hash_put( ht, trim(rest(1:pos-1)) , db_count, ins )
             if( .not. ins ) write (*,*) 'Duplicate db handle seen: ', trim(rest(1:pos-1))
             pos = index( rest, ' ' )          
             keyword = adjustl(rest(pos+2:))
             pos = index( keyword, ' ' )
             write(*,*) 'DB handle = ', trim(keyword(1:pos)) !> file
             call parseMagicLayoutFile(design_dbs(db_count), trim(keyword(1:pos)), MAX_LAYERS)
             db_count = db_count + 1
          else
             write(*,*) 'Syntax ERROR: line: ', line_number
             error stop "Syntax ERROR."
          end if
  
       case ('end')
          print *, "--- End of file"
          call StopMarkTime("END ")
          exit read_loop

       case default
          print *, "Unknown command:", keyword
       end select
    end do read_loop
100 continue !end of file
    close(unit)
  end subroutine ParseControlFile
end module ControlModule

