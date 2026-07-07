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
! sync
! flush !< writes data to disk
! end my_control

module ControlModule
  use CommonModule
  use hash_mod
  use DesignModule
  use SystemInformationModule
  use MagicVLSILayoutParser
  use iso_fortran_env, only: int32, int64, real64
  implicit none
  public :: ParseControlFile, GetLayerFromDBName
  integer, parameter :: PARSE_OK = 0
  integer, parameter :: PARSE_ERR_SYNTAX = 1
  integer, parameter :: PARSE_ERR_UNKNOWN = 2
contains

  !=================================================================
  ! 1) GetLayerFromDBName – the routine you asked for.
  !=================================================================
  subroutine GetLayerFromDBName( full_name, design_hash_table, design_dbs,db_index, input_layer )
    character(len=*),          intent(in)    :: full_name
    type(hash_type),           intent(in)    :: design_hash_table
    type(Design), allocatable, intent(in)    :: design_dbs(:)
    integer,                   intent(out)   :: db_index
    type(Layer),               intent(out)   :: input_layer

    character(len=:), allocatable :: design_name, layer_name
    integer                        :: pos, layer_index
    logical                        :: present

    ! ----------------------------------------------------------------
    ! Split the string on the first ':' – everything left of it is the
    ! design name, everything right of it is the layer name.
    ! ----------------------------------------------------------------
    pos = index(full_name, ':')
    if ( pos == 0 ) then
       error stop "GetLayerFromDBName: missing ':' in '"//trim(full_name)//"'"
    end if

    design_name = trim(full_name(:pos-1))
    layer_name  = trim(full_name(pos+1:))

    ! ----------------------------------------------------------------
    ! Look up the design index.
    ! ----------------------------------------------------------------
    call hash_get( design_hash_table, design_name, db_index, present )
    if ( .not. present ) then
       error stop "GetLayerFromDBName: unknown design '"//design_name//"'"
    end if

    ! ----------------------------------------------------------------
    ! Look up the layer index inside the chosen design.
    ! ----------------------------------------------------------------
    call hash_get( design_dbs(db_index)%ht, layer_name, &
         layer_index, present )   ! we temporarily reuse dummy
    if ( .not. present ) then
       error stop "GetLayerFromDBName: unknown layer '"//layer_name//"'"//" in design '"//design_name//"'"
    end if

    ! ----------------------------------------------------------------
    ! Finally fetch the Layer object.
    ! ----------------------------------------------------------------
    input_layer = design_dbs(db_index)%layers(layer_index)

  end subroutine GetLayerFromDBName
  !=================================================================
  ! EvaluateExpression – parses a whole expression and returns a Layer.
  !=================================================================
  function EvaluateExpression( expr, design_hash_table, design_dbs, status ) result(res_layer)
    character(len=*), intent(in) :: expr
    type(hash_type),   intent(in) :: design_hash_table
    type(Design), allocatable, intent(in) :: design_dbs(:)
    integer,            intent(out) :: status
    type(Layer) :: res_layer
    integer :: pos, err

    pos = 1
    err = PARSE_OK

    call skip_spaces( expr, pos )
    call parse_expr( expr, pos, design_hash_table, design_dbs, &
         res_layer, err )
    call skip_spaces( expr, pos )
    if ( pos <= len_trim(expr) .and. err == PARSE_OK ) err = PARSE_ERR_SYNTAX

    status = err
    if ( err /= PARSE_OK ) then
       write(*,*) "Parse error (code=", err, ") near: '", &
            expr(pos:), "'"
       error stop "Expression parsing failed"
    end if
  end function EvaluateExpression

  subroutine ParseControlFile( fileName, MAX_LAYERS )
    character(len=*), intent(in) :: fileName
    integer, intent(in) :: MAX_LAYERS
    integer, parameter  :: K_MAX_DBS = 32 !< will this work
    character(len=256) :: line, keyword, rest
    integer :: unit, ios, pos, line_number, db_count, i
    integer :: lhs_db_index, rhs1_db_index, rhs2_db_index
    integer :: lhs_layer_index, rhs1_layer_index, rhs2_layer_index    
    type(Design), allocatable, target :: design_dbs(:)
    type(Layer), pointer :: lhs_layer, rhs1_layer, rhs2_layer !> either dont use
    type(hash_type) :: ht
    logical :: ins
    character :: operator_char !> single letter only
    character(len=256) :: TEMPORARY_FOLDER
    character(len=256) :: TEMPORARY_LAYER_PREFIX
    character(len=256) :: strvar
    real(kind=real64)  :: rvar(16)
    integer(kind=K_COORDINATE_KIND):: ivar(16)
    integer(kind=K_COORDINATE_KIND) :: used_precision
    db_count = 1
    !TEMPORARY_LAYER_PREFIX = "MAG_TEMP_LAYER_"
    TEMPORARY_LAYER_PREFIX = "MTL_"
    call hash_create(ht,K_MAX_DBS)
    allocate( design_dbs( K_MAX_DBS ) )
    ! Open the file
    open(newunit=unit, file=fileName, status='old', action='read', iostat=ios)
    if (ios /= 0) then
       print *, "Error: Could not open file."
       stop
    end if
    call InitPrecision(1000) !> read it from CTR file to override
    used_precision = GetPrecision()
    line_number = 1
    ! Read line by line
    read_loop: do
       read(unit, '(A)', end=100,iostat=ios) line
       !write(*,'(A,I4,A,A80)') 'Line number: ', line_number, ' ', line       
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
          !call StopMarkTime("program")
       case ('var')
          rest = adjustl( rest ) !> remove leading whitespace
          write(*,*) 'Variable found: ', trim(rest)
          pos = index( rest, ' ' )
          write(*,*) 'VAR x = ', trim(rest(1:pos-1))
          if( trim(rest(1:pos-1)) == 'temp_folder' ) then
             rest = adjustl(rest)
             pos = index( rest, ' ' )
             rest = trim( adjustl( rest(pos+1:)))             
             write(*,*) 'USING TEMP_FOLDER = ', trim(rest)
             TEMPORARY_FOLDER = trim(rest)
          else if( trim(rest(1:pos-1)) == 'abort_on_xor' ) then
             rest = adjustl(rest)
             pos = index( rest, ' ' )
             rest = trim( adjustl( rest(pos+1:)))
             read(rest, *, iostat=ios) abort_on_xor
             abort_on_xor = 1
             if( ios == 0 ) then
                write(*,*) 'INFO: VAR changing abort_on_xor = ', abort_on_xor
             else
                write(*,*) 'WARNING: VAR not parsed correctly on line: ', line_number
             end if
          else if( trim(rest(1:pos-1)) == 'abort_on_assert_zero' ) then
             rest = adjustl(rest)
             pos = index( rest, ' ' )
             rest = trim( adjustl( rest(pos+1:)))
             read(rest, *, iostat=ios) abort_on_xor
             abort_on_xor = 1
             if( ios == 0 ) then
                write(*,*) 'INFO: VAR changing abort_on_assert_zero = ', abort_on_assert_zero
             else
                write(*,*) 'WARNING: VAR not parsed correctly on line: ', line_number
             end if             
          else if( trim(rest(1:pos-1)) == 'debug_verbosity' ) then             
             rest = adjustl(rest)
             pos = index( rest, ' ' )
             rest = trim( adjustl( rest(pos+1:)))
             read( rest, *, iostat=ios) debug_verbosity
             if( ios == 0 ) then
                write(*,*) 'INFO: VAR changing debug_verbosity = ', debug_verbosity
             else
                write(*,*) 'WARNING: VAR not parsed correctly on line: ', line_number
             end if             
          else if( trim(rest(1:pos-1)) == 'temporary_layers' ) then             
             rest = adjustl(rest)
             pos = index( rest, ' ' )
             rest = trim( adjustl( rest(pos+1:)))
             read( rest, *, iostat=ios) temporary_layers
             if( ios == 0 ) then
                write(*,*) 'INFO: VAR changing temporary_layers = ', temporary_layers
             else
                write(*,*) 'WARNING: VAR not parsed correctly on line: ', line_number
             end if             
          end if          
          cycle read_loop
       case ('decl')
          rest = adjustl( rest ) !> remove leading whitespace
          !write(*,*) 'Declaration found: ', trim(rest)
          ! Further logic to parse types (int, real, design, etc.)
          pos = index( rest, ' ' )
          !write(*,*) 'KW (design/output/memory) = ', trim(rest(1:pos-1))
          if( trim(rest(1:pos-1)) == 'output' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             !write(*,*) 'Output DB handle = ', trim(rest(1:pos-1)), ' db_index = ', db_count !> o1
             call hash_put( ht, trim(rest(1:pos-1)) , db_count, ins )
             if( .not. ins ) write (*,*) 'Duplicate db handle seen: ', trim(rest(1:pos-1))
             allocate( design_dbs( db_count )%designName, source = trim(adjustl(rest(1:pos-1))) )
             pos = index( rest, ' ' )          
             keyword = adjustl(rest(pos+2:))
             pos = index( keyword, ' ' )
             write(*,*) 'Generating output in MAGIC file: ', trim(keyword(1:pos)) !> file
             design_dbs(db_count)%design_direction = DESIGN_DIRECTION_OUTPUT
             !write(*,*) 'Setting direction of ', db_count, ' to ', design_dbs(db_count)%design_direction
             !call hash_create(design_dbs(db_count)%ht, MAX_LAYERS )
             !allocate( design_dbs(db_count)%layers( MAX_LAYERS ) )
             !> we also want output association of layer data with disk
             design_dbs(db_count)%fileName = trim(keyword(1:pos))             
             call parseMagicLayoutFile(design_dbs(db_count), MAX_LAYERS)             
             db_count = db_count+1
          else if( trim(rest(1:pos-1)) == 'design' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             !write(*,*) 'KW = ', trim(rest(1:pos-1)) !> d1
             call hash_put( ht, trim(rest(1:pos-1)) , db_count, ins )
             if( .not. ins ) write (*,*) 'Duplicate db handle seen: ', trim(rest(1:pos-1))
             allocate( design_dbs( db_count )%designName, source = trim(adjustl(rest(1:pos-1))))
             !write(*,*) '=======> Assigning ', db_count, ' to ', design_dbs( db_count )%designName
             pos = index( rest, ' ' )          
             keyword = adjustl(rest(pos+2:))
             pos = index( keyword, ' ' )
             !write(*,*) 'DB file handle = ', trim(keyword(1:pos)) !> file
             design_dbs(db_count)%fileName = trim(keyword(1:pos))
             call parseMagicLayoutFile(design_dbs(db_count), MAX_LAYERS)
             db_count = db_count + 1
          else if( trim(rest(1:pos-1)) == 'memory' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             !write(*,*) 'KW = ', trim(rest(1:pos-1)) !> d1
             call hash_put( ht, trim(rest(1:pos-1)) , db_count, ins )
             if( .not. ins ) write (*,*) 'Duplicate db handle seen: ', trim(rest(1:pos-1))
             allocate( design_dbs( db_count )%designName, source = trim(adjustl(rest(1:pos-1))))
             pos = index( rest, ' ' )          
             keyword = adjustl(rest(pos+2:))
             pos = index( keyword, ' ' )
             if( trim(keyword(1:pos)) /= 'nothing' ) error stop "MEMORY DB must have backing store nothing"
             write(*,*) 'Creating MEMORY db: ', trim( design_dbs( db_count )%designName )
             !write(*,*) 'DB file handle = ', trim(keyword(1:pos)), ' assigned DB_COUNT: ', db_count !> file
             !call parseMagicLayoutFile(design_dbs(db_count), trim(keyword(1:pos)), MAX_LAYERS)
             design_dbs(db_count)%design_direction = DESIGN_DIRECTION_MEMORY
             allocate( design_dbs(db_count)%layerNames( MAX_LAYERS ) )
             call hash_create(design_dbs(db_count)%ht, MAX_LAYERS )
             allocate( design_dbs(db_count)%layers( MAX_LAYERS ) )
             db_count = db_count + 1
             cycle read_loop
          else
             write(*,*) 'Syntax ERROR: line: ', line_number
             error stop "Syntax ERROR."
          end if
       case ('systeminfo') !> print information about the state of the system
          call PrintFullInformation()
       case ('info') !> print information about RSS of current pid
          call StopMarkTime("info")
       case ('exec')
          !print *, "Execution command:", rest
          !lhs_layer = EvaluateExpression( rest, ht, design_dbs, parse_status )
          rest = adjustl( rest ) !> remove leading whitespace
          !write(*,*) 'Declaration found: ', trim(rest)
          ! Further logic to parse types (int, real, design, etc.)
          pos = index( rest, ' ' )
          !write(*,*) 'KW? (run/push/group) = ', trim(rest(1:pos-1))
          !> RUN section immediate mode for now, but we can create a graph
          !> the assumption is that this microcode is generated by a compiler
          if( trim(rest(1:pos-1)) == 'init' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             !write(*,*) 'LHS = ', trim(rest(1:pos-1)) !> f1:gate
             pos = index( trim(rest(1:pos-1)),':')
             call hash_get( ht, trim(rest(1:pos-1)), lhs_db_index, ins )
             if( .not. ins ) then
                write(*,*) 'DB: ', trim(rest(1:pos-1)), ' not found.'
                write(*,*) 'Syntax ERROR: line: ', line_number                
                error stop "Syntax ERROR"
             else
                !write(*,*) 'Using LHS DB Index: ', lhs_db_index
             end if
             rest = adjustl(rest(pos+1:))
             pos  = index( rest, ' ')
             lhs_layer_index = hash_nitems( design_dbs( lhs_db_index )%ht ) + 1
             call hash_put( design_dbs( lhs_db_index )%ht, trim(rest(1:pos-1)), lhs_layer_index, ins )
             if( .not. ins ) then
                write(*,*) 'ERROR: Unable to INIT layer: ', trim(rest(1:pos-1)), ' at index: ', lhs_layer_index
             end if
             allocate( design_dbs( lhs_db_index )%layers( lhs_layer_index )%fileName, source = 'MTL_NOTHING')
             call ClearLayer( design_dbs( lhs_db_index )%layers( lhs_layer_index ) )
             cycle
          end if
          if( trim(rest(1:pos-1)) == 'run' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             !write(*,*) 'LHS = ', trim(rest(1:pos-1)) !> f1:gate
             pos = index( trim(rest(1:pos-1)),':')
             call hash_get( ht, trim(rest(1:pos-1)), lhs_db_index, ins )
             if( .not. ins ) then
                write(*,*) 'DB: ', trim(rest(1:pos-1)), ' not found.'
                write(*,*) 'Syntax ERROR: line: ', line_number                
                error stop "Syntax ERROR"
             else
                !write(*,*) 'Using LHS DB Index: ', lhs_db_index
             end if
             rest = adjustl(rest(pos+1:))
             pos  = index( rest, ' ')
             lhs_layer_index = -1
             call hash_get( design_dbs( lhs_db_index )%ht, trim(rest(1:pos-1)), lhs_layer_index, ins )
             if( ins .and. design_dbs( lhs_db_index )%design_direction == DESIGN_DIRECTION_MEMORY ) then
                if( trim(rest(1:pos-1)) /= 'nothing' ) then
                   write(*,*) 'MEMORY LEAK: Reusing LHS Layer name: ', trim(rest(1:pos-1)), ' index: ', lhs_layer_index
                end if
             end if
             if( ins .and. design_dbs( lhs_db_index )%design_direction == DESIGN_DIRECTION_OUTPUT ) then
                !> normal situation, we have a layer handle and output file
             elseif(lhs_layer_index < 0 ) then
                lhs_layer_index = hash_nitems(design_dbs( lhs_db_index )%ht)+1
                call hash_put( design_dbs( lhs_db_index )%ht, trim(rest(1:pos-1)), lhs_layer_index, ins )
                if( .not. ins ) error stop "DB HASH TABLE CORRUPTED"
                !> we have to decide temporary layer format
                associate( resolved_layer => design_dbs( lhs_db_index )%layers(lhs_layer_index) )
                  lhs_layer => design_dbs( lhs_db_index )%layers(lhs_layer_index)
                  if( .not. allocated( design_dbs( lhs_db_index )%layerNames ) ) error stop "DB Layernames not populated"
                  design_dbs( lhs_db_index )%layerNames(lhs_layer_index) = trim(adjustl(TEMPORARY_LAYER_PREFIX))//trim(adjustl(rest(1:pos-1)))
                  lhs_layer%fileName = trim(TEMPORARY_FOLDER)//trim(design_dbs( lhs_db_index )%designName)//'_'//trim(design_dbs( lhs_db_index )%layerNames(lhs_layer_index))
                  !write(*,*) 'NEW LHS layer index = ', lhs_layer_index, ' for ', trim(adjustl(rest(1:pos-1))), ' ', &
                  !     trim(adjustl(design_dbs( lhs_db_index )%layerNames(lhs_layer_index))), ' FILENAME: ',&
                  !     resolved_layer%fileName
                end associate
             end if
             if( lhs_layer_index < 0 ) error stop "DB INDEX layer corruption"
             !write(*,*) 'LHS index = ', lhs_layer_index
             lhs_layer => design_dbs( lhs_db_index )%layers(lhs_layer_index)
             if(.not. allocated( lhs_layer%fileName ) ) then
                error stop "ERROR: Each layer must have a backing-store/name by now"
             end if
             rest = adjustl(rest(pos+1:))
             pos  = index( rest, '= ')
             rest = adjustl(rest(pos+1:))
             pos = index( rest, ':' )
             call hash_get( ht, trim(rest(1:pos-1)), rhs1_db_index, ins )
             if( .not. ins ) then
                write(*,*) 'ERROR: RHS1 DB Index not found: ', trim(rest(1:pos-1)), ' line: ', line_number-1
                error stop
             else
                if(.not. allocated( design_dbs(rhs1_db_index)%designName ) ) error stop
                !write(*,*) 'RHS1 DB index: ', rhs1_db_index, ' for ', trim(rest(1:pos-1)), ' ~ ', trim(design_dbs(rhs1_db_index)%designName)
             end if
             rest = adjustl(rest(pos+1:))
             pos  = index( rest, ' ')
             
             call hash_get( design_dbs( rhs1_db_index )%ht, trim(rest(1:pos-1)), rhs1_layer_index, ins )
             if( .not. ins ) then
                write(*,*) 'ERROR: RHS1 layer not found: ',trim(rest(1:pos-1)), ' check spelling or existence of layer in db. Line: ', line_number-1
                error stop "ERROR: layer not found"
             end if
             rhs1_layer => design_dbs( rhs1_db_index )%layers( rhs1_layer_index )
             rest = adjustl(rest(pos+1:))
             !write(*,*) 'RESTD BEFORE CHAR OPERATOR SCAN = ', trim(rest)
             pos  = scan( rest, '+@*%^~!') !> second time we tripped on this, -0.5 is valid
             if( pos /= 0 ) then !> single char operators for brevity
                !> valid operator found
                operator_char = rest(pos:pos)
                rest = adjustl(rest(pos+1:))
                pos = index( rest, ':' )
                call hash_get( ht, trim(rest(1:pos-1)), rhs2_db_index, ins )
                if( .not. ins ) then
                   write(*,*) 'ERROR RHS2 DB Indes for db: ', trim(rest(1:pos-1)), ' not located on line ', line_number-1
                   error stop "RHS2 DB INDEX not located"
                end if
                rest = adjustl(rest(pos+1:))
                pos  = index( rest, ' ')
                call hash_get( design_dbs( rhs2_db_index )%ht, trim(rest(1:pos-1)), rhs2_layer_index, ins )
                if( .not. ins ) then
                   write(*,*) 'ERROR: RHS2 layer not found: ',trim(rest(1:pos-1)) ,' check spelling or existence of layer in db: line: ', line_number-1
                   error stop "ERROR: layer not found"
                end if
                rhs2_layer => design_dbs( rhs2_db_index )%layers( rhs2_layer_index )
                pos  = index( rest, ' ')
                if( debug_verbosity > 1 ) then
                   write(*,'(A,I3,A,I3,A,I3,A,I3,A4,I3,A,I3)') 'Getting ready to execute: ', &
                        lhs_db_index,':',lhs_layer_index, ' = ', &
                        rhs1_db_index,':',rhs1_layer_index, operator_char, rhs2_db_index,':',rhs2_layer_index
                end if
                write(*,'(A,A3,A,A10,A,A4,A,A20,A4,A3,A,A20)') 'Getting ready to execute: ', &
                     design_dbs(lhs_db_index)%designName,':',&
                     trim(design_dbs(lhs_db_index)%layerNames(lhs_layer_index)), ' = ', &
                     design_dbs(rhs1_db_index)%designName,':',trim(design_dbs(rhs1_db_index)%layerNames(rhs1_layer_index)),&
                     operator_char, &
                     design_dbs(rhs2_db_index)%designName,':',trim(design_dbs(rhs2_db_index)%layerNames(rhs2_layer_index))
                if( rhs1_layer%n_used == 0 ) call RestoreSnapToLayer( rhs1_layer, rhs1_layer%fileName )
                if( rhs2_layer%n_used == 0 ) call RestoreSnapToLayer( rhs2_layer, rhs2_layer%fileName )                
                select case (operator_char)
                case('+')
                   call StartMarkTime("OR")
                   call CalculateBoostOperation( rhs1_layer, rhs2_layer, lhs_layer, K_BOOST_CONTROL_OR, 0_int64 )
                   call StopMarkTime("OR")
                   !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |R2| = ', rhs2_layer%n_used, ' |O1| = ', lhs_layer%n_used            
                case('*')
                   call StartMarkTime("AND")
                   call CalculateAND( rhs1_layer, rhs2_layer, lhs_layer )
                   !> since we really cannot predict the output size
                   !call CalculateBoostOperation( rhs1_layer, rhs2_layer, lhs_layer, K_BOOST_CONTROL_AND, int( ivar(1), kind=int64) )            
                   call StopMarkTime("AND")
                   !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |R2| = ', rhs2_layer%n_used, ' |O1| = ', lhs_layer%n_used      
                case('@')
                   call StartMarkTime("NOT")
                   call CalculateNOT( rhs1_layer, rhs2_layer, lhs_layer )                   
                   call StopMarkTime("NOT")
                case('~')
                   call StartMarkTime("FRAMENOT")
                   call CalculateFrameNOT( rhs1_layer, rhs2_layer, lhs_layer )
                   call StopMarkTime("FRAMENOT")
                case('!')
                   call StartMarkTime("BOOSTNOT")
                   call CalculateBoostOperation( rhs1_layer, rhs2_layer, lhs_layer, K_BOOST_CONTROL_NOT, 0_int64 )
                   call StopMarkTime("BOOSTNOT")                                   
                case('^')
                   call StartMarkTime("BOOSTXOR")
                   call CalculateBoostOperation( rhs1_layer, rhs2_layer, lhs_layer, K_BOOST_CONTROL_XOR, 0_int64 )
                   call StopMarkTime("BOOSTXOR")                                                      
                   !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |R2| = ', rhs2_layer%n_used, ' |O1| = ', lhs_layer%n_used
                   if( abort_on_xor > 0 .and. lhs_layer%n_used > 0 ) then
                      write(*,*) 'ERROR: Aborting RUN since abort_on_xor set > 0, and ^ layer has non-zero shapes.'
                      error stop
                   end if
                case('%')
                end select
             else
                !> more verbose style eg f1:bbox_poly = d1:poly EXTENT nothing                
                !> try "list directed read"
                block
                  character(len=256)  :: buf1, buf2
                  character(len=:), allocatable  :: primary_operator, rhs2_source_name
                  integer(kind=int64) :: ios, ivar1, ivar2, ivar3, ivar4, ivar5 !> add more if needed
                  integer(kind=real64):: rvar1, rvar2, rvar3, rvar4, rvar5 !> add more if needed
                  !write(*,*) 'REST HERE: = ', trim(rest), ' ', adjustl(rest)
                  rest = adjustl(rest)
                  rest = trim(rest)
                  read(rest, *, iostat=ios) buf1, buf2 !> now rest is at the COMMAND options
                  if( ios == 0 ) then
                     !> we got z = x OP y
                     primary_operator = trim( adjustl( buf1 ) )
                     rhs2_source_name = trim( adjustl( buf2 ) )
                     if( .not. associated( rhs1_layer ) ) error stop "RHS1 layer not associated"
                     if( .not. associated( lhs_layer ) )  error stop "LHS  layer not associated"                     
                     if( rhs2_source_name /= 'nothing' ) then
                        pos = index( rhs2_source_name, ':' )
                        !write(*,*) 'RESTG = ', trim(rhs2_source_name(1:pos-1))
                        call hash_get( ht, trim(rhs2_source_name(1:pos-1)), rhs2_db_index, ins )
                        if( .not. ins ) error stop "TEXT OPERATOR RHS2 DB INDEX not located"
                        buf2 = adjustl(trim(rhs2_source_name(1+pos:)))
                        pos  = index( buf2, ' ')
                        call hash_get( design_dbs( rhs2_db_index )%ht, trim(buf2(1:pos-1)), rhs2_layer_index, ins )
                        if( .not. ins ) then
                           write(*,*) 'ERROR: TEXT RHS2 ', rhs2_source_name, ' layer not found.', ' ', trim(buf2(1:pos-1))
                           error stop "ERROR"
                        end if
                        rhs2_layer => design_dbs( rhs2_db_index )%layers( rhs2_layer_index )
                     else
                        nullify( rhs2_layer )
                     end if
                     call StartMarkTime(primary_operator)
                     if( associated( rhs1_layer ) ) then
                        if( rhs1_layer%n_used == 0 ) call RestoreSnapToLayer( rhs1_layer, rhs1_layer%fileName )
                     end if
                     if( associated( rhs2_layer ) ) then
                        if( rhs2_layer%n_used == 0 ) call RestoreSnapToLayer( rhs2_layer, rhs2_layer%fileName )
                     end if

                     select case (primary_operator)
                     case('OR2')
                        call CalculateOR( rhs1_layer, rhs2_layer, lhs_layer )
                        !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |R2| = ', rhs2_layer%n_used, ' |O1| = ', lhs_layer%n_used
                     case('ASSERT_ZERO')
                        !> for now we have to do t1:nothing = ASSERT_ZERO t1:check_and nothing
                        if( rhs2_source_name /= 'nothing' .and. rhs2_source_name /= 't1:nothing' ) error stop "ASSERT_ZERO must use nothing as second layer"
                        if( rhs1_layer%n_used > 0 ) then
                           write(*,'(A,I5,A)') '************ ASSERT_ZERO fail on layer. Line: ', line_number-1, ' **********************'
                           if( abort_on_assert_zero > 1 ) error stop "ERROR: ASSERT_ZERO variable > 1"
                        end if
                     case('AND2')
                        call CalculateFrameAND( rhs1_layer, rhs2_layer, lhs_layer )
                        !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |R2| = ', rhs2_layer%n_used, ' |O1| = ', lhs_layer%n_used
                     case('NOT')
                        call CalculateNOT( rhs1_layer, rhs2_layer, lhs_layer )
                        !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |R2| = ', rhs2_layer%n_used, ' |O1| = ', lhs_layer%n_used
                     case('FRAMENOT')
                        call CalculateFrameNOT( rhs1_layer, rhs2_layer, lhs_layer )
                        !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |R2| = ', rhs2_layer%n_used, ' |O1| = ', lhs_layer%n_used
                     case ('EXTENT')
                        !> d1:poly EXTENT nothing
                        if( rhs2_source_name /= 'nothing' .and. rhs2_source_name /= 't1:nothing') error stop "EXTENT must use nothing as second layer"
                        !write(*,*) 'Found PRIMARY_OPERATOR = EXTENT'
                        call CreateEXTENT( rhs1_layer, lhs_layer )
                     case ('AND')
                        !> d1:poly AND nothing
                        !if( rhs2_source_name /= 'nothing' ) error stop "AND must use nothing as second layer"
                        !write(*,*) 'Found PRIMARY_OPERATOR = AND'
                        call CalculateSingleLayerAND( rhs1_layer, lhs_layer )
                     case ('XOR')
                        !> d1:poly XOR d2:poly
                        !write(*,*) 'Found PRIMARY_OPERATOR = XOR'
                        call CalculateXOR(rhs1_layer, rhs2_layer, lhs_layer )                        
                     case ('COPY')
                        !> d1:poly COPY nothing
                        !if( rhs2_source_name /= 'nothing' ) error stop "COPY must use nothing as second layer"
                        !write(*,*) 'Found PRIMARY_OPERATOR = COPY'
                        call CopyLayer( lhs_layer, rhs1_layer )
                     case ('GRID')
                        !> d1:poly GRID nothing 10 10
                        !write(*,*) 'Found PRIMARY_OPERATOR = GRID'
                        call CreateGRID( rhs1_layer, lhs_layer, 10, 10, 10 ) !> the last argument is OVERLAP
                     case ('GROW') !> lhs = rhs GROW nothing EAST NORTH WEST SOUTH (all positive)
                        if( rhs2_source_name /= 'nothing' .and. rhs2_source_name /= 't1:nothing' ) error stop "GROW must use nothing as second layer"
                        !write(*,*) 'RESTJUST before: ', rest
                        read(rest, *, iostat=ios) buf1, buf2, rvar(1), rvar(2), rvar(3), rvar(4)
                        !write(*,'(A,I8,4(A,F8.2))') 'Found PRIMARY_OPERATOR = GROW at PRECISION ', used_precision, ' ', rvar(1), ' ', rvar(2), ' ', rvar(3), ' ', rvar(4)
                        do i=1,4
                           ivar(i) = int( used_precision*rvar(i), kind = K_COORDINATE_KIND )
                        end do
                        call CalculateGROWLayer( rhs1_layer, lhs_layer, ivar(1:4) )
                     case ('SIZE')
                        if( rhs2_source_name /= 'nothing' .and. rhs2_source_name /= 't1:nothing' ) error stop "SIZE must use nothing as second layer"
                        read(rest, *, iostat=ios) buf1, buf2, rvar(1)
                        !write(*,*) 'Found PRIMARY_OPERATOR = SIZE ', rvar(1)
                        ivar(1) = int( rvar(1)*used_precision, kind=int64)
                        call CalculateBoostOperation( rhs1_layer, rhs2_layer, lhs_layer, K_BOOST_CONTROL_SIZE, int( ivar(1), kind=int64) )
                        !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |O1| = ', lhs_layer%n_used
                     case ('WORMHOLE')
                        write(*,*) 'Found PRIMARY_OPERATOR = WORMHOLE'
                        !> d1:met1 WORMHOLE f1:met1_marker 5 100
                     case default
                        write(*,*) 'UNKNOWN primary_operator: ', primary_operator, ' on line: ', line_number-1
                        error stop "SYNTAX ERROR on TEXT based operator"
                     end select
                     call StopMarkTime(primary_operator)
                  else
                     write(*,*) 'SYNTAX ERROR on line: ', line_number-1
                     error stop "SYNTAX ERROR on TEXT based operator"
                  end if
                end block
             end if
             cycle read_loop
          else if( trim(rest(1:pos-1)) == 'modedesign' ) then
             rest = adjustl(rest(pos+1:))             
             pos = index( rest, ' ' )
             write(*,*) 'KW = ', trim(rest(1:pos-1)) !> d1
             call hash_put( ht, trim(rest(1:pos-1)) , db_count, ins )
             if( .not. ins ) write (*,*) 'Duplicate db handle seen: ', trim(rest(1:pos-1))
             pos = index( rest, ' ' )          
             keyword = adjustl(rest(pos+2:))
             pos = index( keyword, ' ' )
             write(*,*) 'DB handle = ', trim(keyword(1:pos)) !> file
             design_dbs(db_count)%fileName = trim(keyword(1:pos))
             call parseMagicLayoutFile(design_dbs(db_count), MAX_LAYERS)
             design_dbs(db_count)%design_direction = DESIGN_DIRECTION_INPUT
             db_count = db_count + 1
          else
             write(*,*) 'Syntax ERROR: line: ', line_number-1
             error stop "Syntax ERROR."
          end if
          cycle read_loop
       case ('flush')
          call StartMarkTime("dbFlush")
          do i=1,size(design_dbs)
             if( design_dbs(i)%design_direction == DESIGN_DIRECTION_OUTPUT ) then
                call writeMagicLayoutFile( design_dbs(i) )
             end if
             if( temporary_layers == K_TEMPORARY_LAYER_DISK .and. design_dbs(i)%design_direction == DESIGN_DIRECTION_MEMORY ) then
                !> we have to flush memory layers to disk as well
                call writeMagicLayoutFile( design_dbs(i) )
             end if
          end do
          call StopMarkTime("dbFlush")          
          cycle read_loop
       case ('delete')
          call StartMarkTime("delete")
          rest = adjustl( rest ) !> remove leading whitespace
          pos = index( rest, ' ' )
          call hash_get( ht, trim(rest(1:pos-1)), lhs_db_index, ins )
          if( .not. ins ) then
             write(*,*) 'ERROR: DELETE db-index not found: ', line_number-1
             error stop "ERROR: DELETE db-index not found."
          end if
          call DeleteDesign( design_dbs( lhs_db_index ) )
          !> any use of this db after this point in the file is an ERROR
          call hash_remove( ht, trim(rest(1:pos-1)), ins )
          if( .not. ins ) then
             write(*,*) 'INFO: DELETE db-index not removed: ', line_number-1
          end if
          call StopMarkTime("delete")          
          cycle read_loop
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

  !> helper function, hopefully we do not have to debug them often
  !---------------------------------------------------------------
  ! Skip blanks – tiny utility used everywhere.
  !---------------------------------------------------------------
  pure subroutine skip_spaces( str, i )
    character(len=*), intent(in) :: str
    integer,          intent(inout) :: i
    do while ( i <= len_trim(str) .and. str(i:i) == ' ' )
       i = i + 1
    end do
  end subroutine skip_spaces

  !---------------------------------------------------------------
  ! parse_expr  →  term { (+|-) term }
  !---------------------------------------------------------------
  subroutine parse_expr( str, i, dhash, dbs, result, err )
    character(len=*), intent(in)    :: str
    integer,          intent(inout) :: i
    type(hash_type),  intent(in)    :: dhash
    type(Design), allocatable, intent(in) :: dbs(:)
    type(Layer),      intent(out)   :: result
    integer,          intent(inout) :: err

    type(Layer) :: left, right
    character(len=1) :: op

    call parse_term( str, i, dhash, dbs, left, err )
    if ( err /= PARSE_OK ) return

    call skip_spaces( str, i )
    do while ( i <= len_trim(str) .and. ( str(i:i) == '+' .or. &
         str(i:i) == '-' ) )
       op = str(i:i)
       i = i + 1
       call skip_spaces( str, i )
       call parse_term( str, i, dhash, dbs, right, err )
       if ( err /= PARSE_OK ) return

       select case (op)
       case ('+')
          call CalculateOR ( left, right, left )
       case ('-')
          call CalculateOR( left, right, left )
       end select
       call skip_spaces( str, i )
    end do
    result = left
  end subroutine parse_expr

  !---------------------------------------------------------------
  ! parse_term  →  factor { (*|%) factor }
  !---------------------------------------------------------------
  subroutine parse_term( str, i, dhash, dbs, result, err )
    character(len=*), intent(in)    :: str
    integer,          intent(inout) :: i
    type(hash_type),  intent(in)    :: dhash
    type(Design), allocatable, intent(in) :: dbs(:)
    type(Layer),      intent(out)   :: result
    integer,          intent(inout) :: err

    type(Layer) :: left, right
    character(len=1) :: op

    call parse_factor( str, i, dhash, dbs, left, err )
    if ( err /= PARSE_OK ) return

    call skip_spaces( str, i )
    do while ( i <= len_trim(str) .and. ( str(i:i) == '*' .or. &
         str(i:i) == '%' ) )
       op = str(i:i)
       i = i + 1
       call skip_spaces( str, i )
       call parse_factor( str, i, dhash, dbs, right, err )
       if ( err /= PARSE_OK ) return

       select case (op)
       case ('*')
          call CalculateAND ( left, right, left )
       case ('%')
          call CalculateAND  ( left, right, left )
       end select
       call skip_spaces( str, i )
    end do
    result = left
  end subroutine parse_term

  !---------------------------------------------------------------
  ! parse_factor → primary { ^ primary }
  !---------------------------------------------------------------
  subroutine parse_factor( str, i, dhash, dbs, result, err )
    character(len=*), intent(in)    :: str
    integer,          intent(inout) :: i
    type(hash_type),  intent(in)    :: dhash
    type(Design), allocatable, intent(in) :: dbs(:)
    type(Layer),      intent(out)   :: result
    integer,          intent(inout) :: err

    type(Layer) :: left, right

    call parse_primary( str, i, dhash, dbs, left, err )
    if ( err /= PARSE_OK ) return

    call skip_spaces( str, i )
    do while ( i <= len_trim(str) .and. str(i:i) == '^' )
       i = i + 1
       call skip_spaces( str, i )
       call parse_primary( str, i, dhash, dbs, right, err )
       if ( err /= PARSE_OK ) return
       call CalculateAND( left, right, left )
       call skip_spaces( str, i )
    end do
    result = left
  end subroutine parse_factor

  !---------------------------------------------------------------
  ! parse_primary → layer_ref | number | '(' expr ')'
  !---------------------------------------------------------------
  subroutine parse_primary( str, i, dhash, dbs, result, err )
    character(len=*), intent(in)    :: str
    integer,          intent(inout) :: i
    type(hash_type),  intent(in)    :: dhash
    type(Design), allocatable, intent(in) :: dbs(:)
    type(Layer),      intent(out)   :: result
    integer,          intent(inout) :: err

    character(len=:), allocatable :: token
    integer :: db_index, layer_index
    logical :: present

    call skip_spaces( str, i )
    if ( i > len_trim(str) ) then
       err = PARSE_ERR_SYNTAX
       return
    end if

    select case ( str(i:i) )
    case ( '(' )
       i = i + 1
       call parse_expr( str, i, dhash, dbs, result, err )
       if ( err /= PARSE_OK ) return
       call skip_spaces( str, i )
       if ( i > len_trim(str) .or. str(i:i) /= ')' ) then
          err = PARSE_ERR_SYNTAX
          return
       end if
       i = i + 1

    case default
       ! Try to see if we have a design:layer reference.
       call extract_until( str, i, ':', token )
       if ( token /= '' .and. i <= len_trim(str) .and. str(i:i) == ':' ) then
          ! We really have a layer reference.
          i = i + 1          ! skip the ':'
          call extract_word( str, i, token )   ! token now holds the layer name

          ! ---- look up the design ----
          call hash_get( dhash, token, db_index, present )
          if ( .not. present ) then
             err = PARSE_ERR_UNKNOWN
             return
          end if

          ! ---- look up the layer inside that design ----
          call hash_get( dbs(db_index)%ht, token, layer_index, present )
          if ( .not. present ) then
             err = PARSE_ERR_UNKNOWN
             return
          end if

          result = dbs(db_index)%layers(layer_index)
       else
          ! No ':' → must be a numeric constant.
          call extract_number( str, i, token )
          if ( token == '' ) then
             err = PARSE_ERR_SYNTAX
             return
          end if
          ! For the moment we treat a constant as a *dummy* layer.
          ! You can replace this by a proper conversion routine that
          ! creates a Layer from a real value.
          !result%dummy = int(token)   ! <-- placeholder
       end if
    end select
  end subroutine parse_primary
  !---------------------------------------------------------------
  ! extract_until – returns characters from i up to (but not
  !                 including) the delimiter.  i is left pointing
  !                 at the delimiter (or at the end of the string).
  !---------------------------------------------------------------
  pure subroutine extract_until( str, i, delim, token )
    character(len=*), intent(in)  :: str
    integer,          intent(inout):: i
    character(len=1), intent(in)   :: delim
    character(len=:), allocatable, intent(out) :: token

    integer :: start, finish

    start = i
    do while ( i <= len_trim(str) .and. str(i:i) /= delim )
       i = i + 1
    end do
    finish = i - 1
    if ( finish < start ) then
       token = ''
    else
       token = str(start:finish)
    end if
  end subroutine extract_until

  !---------------------------------------------------------------
  ! extract_word – reads a sequence of non‑blank characters.
  !---------------------------------------------------------------
  pure subroutine extract_word( str, i, token )
    character(len=*), intent(in)  :: str
    integer,          intent(inout):: i
    character(len=:), allocatable, intent(out) :: token

    integer :: start

    call skip_spaces( str, i )
    start = i
    do while ( i <= len_trim(str) .and. str(i:i) > ' ' )
       i = i + 1
    end do
    token = str(start:i-1)
  end subroutine extract_word

  !---------------------------------------------------------------
  ! extract_number – reads a Fortran‑compatible real literal.
  !---------------------------------------------------------------
  pure subroutine extract_number( str, i, token )
    character(len=*), intent(in)  :: str
    integer,          intent(inout):: i
    character(len=:), allocatable, intent(out) :: token

    integer :: start
    logical :: dot_seen, exp_seen, sign_seen

    call skip_spaces( str, i )
    start = i
    dot_seen = .false.
    exp_seen = .false.
    sign_seen = .false.

    do while ( i <= len_trim(str) )
       select case ( str(i:i) )
       case ( '0':'9' )
          i = i + 1
       case ( '.' )
          if ( dot_seen ) exit
          dot_seen = .true.
          i = i + 1
       case ( 'e','E','d','D' )
          if ( exp_seen ) exit
          exp_seen = .true.
          i = i + 1
          if ( i <= len_trim(str) .and. &
               ( str(i:i) == '+' .or. str(i:i) == '-' ) ) i = i + 1
       case default
          exit
       end select
    end do
    token = str(start:i-1)
  end subroutine extract_number
  subroutine shrink_to_fit(str)
    character(len=:), allocatable, intent(inout) :: str
    character(len=:), allocatable               :: tmp
    integer                                     :: n

    n   = len_trim(str)                 ! number of non‑blank characters
    allocate(character(len=n) :: tmp)   ! temporary of the right size
    tmp = str(:n)                       ! copy the trimmed part
    deallocate(str)                     ! free the old 256‑char block
    allocate(character(len=n) :: str)   ! allocate the right‑sized block
    str = tmp
  end subroutine shrink_to_fit
end module ControlModule

