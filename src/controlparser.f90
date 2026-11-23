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
  use BandExecModule
  use SystemInformationModule
  use MagicVLSILayoutParser
  use LayoutDBLoaderModule
  use TRSOptimizerModule
  use iso_fortran_env, only: int32, int64, real64
  implicit none
  public :: ParseControlFile, GetLayerFromDBName
  integer, parameter :: PARSE_OK = 0
  integer, parameter :: PARSE_ERR_SYNTAX = 1
  integer, parameter :: PARSE_ERR_UNKNOWN = 2
contains

  subroutine LoadDesignFileByExt(load_design, MAX_LAYERS)
    type(Design), intent(inout) :: load_design
    integer, intent(in) :: MAX_LAYERS
    character(len=:), allocatable :: fn, ext, lower_ext
    integer :: dot, i
    fn = trim(load_design%fileName)
    dot = index(fn, '.', back=.true.)
    ext = ''
    if(dot > 0) ext = fn(dot+1:)
    lower_ext = ext
    do i=1,len_trim(lower_ext)
       if(lower_ext(i:i)>='A' .and. lower_ext(i:i)<='Z') lower_ext(i:i)=achar(iachar(lower_ext(i:i))+32)
    end do
    if(lower_ext == 'gds' .or. lower_ext == 'gdsii' .or. lower_ext == 'oas' .or. lower_ext == 'oasis') then
       write(*,*) 'INFO: CTR loading GDSII/OASIS design: ', trim(fn)
       call LoadLayoutDBFile(load_design, MAX_LAYERS)
    else
       call parseMagicLayoutFile(load_design, MAX_LAYERS)
    end if
  end subroutine LoadDesignFileByExt

  subroutine LoadDesignFileByExtHier(load_design, MAX_LAYERS)
    type(Design), intent(inout) :: load_design
    integer, intent(in) :: MAX_LAYERS
    character(len=:), allocatable :: fn, ext, lower_ext
    integer :: dot, i
    fn = trim(load_design%fileName)
    dot = index(fn, '.', back=.true.)
    ext = ''
    if(dot > 0) ext = fn(dot+1:)
    lower_ext = ext
    do i=1,len_trim(lower_ext)
       if(lower_ext(i:i)>='A' .and. lower_ext(i:i)<='Z') lower_ext(i:i)=achar(iachar(lower_ext(i:i))+32)
    end do
    if(lower_ext == 'gds' .or. lower_ext == 'gdsii' .or. lower_ext == 'oas' .or. lower_ext == 'oasis') then
       write(*,*) 'INFO: CTR loading GDSII/OASIS design (hierarchical flatten): ', trim(fn)
       call LoadLayoutDBFileHier(load_design, MAX_LAYERS)
    else
       call parseMagicLayoutFile(load_design, MAX_LAYERS)
    end if
  end subroutine LoadDesignFileByExtHier

  subroutine ensure_layer_capacity(d, idx)
    type(Design), intent(inout) :: d
    integer, intent(in) :: idx
    type(Layer), allocatable :: tmp_layers(:)
    character(len=1024), allocatable :: tmp_names(:)
    integer :: oldn, oldn_names, newn

    if (allocated(d%layers)) then
       oldn = size(d%layers)
    else
       oldn = 0
    end if
    if (allocated(d%layerNames)) then
       oldn_names = size(d%layerNames)
    else
       oldn_names = 0
    end if
    if (idx <= oldn .and. idx <= oldn_names) return

    newn = max(1, max(oldn, oldn_names))
    if (newn < idx) newn = idx
    newn = max(newn, max(oldn, oldn_names) + max(oldn, oldn_names)/2 + 8)

    if (.not. allocated(d%layers) .or. oldn < newn) then
       allocate(tmp_layers(newn))
       if (allocated(d%layers) .and. oldn > 0) tmp_layers(1:oldn) = d%layers(1:oldn)
       call move_alloc(tmp_layers, d%layers)
    end if

    if (.not. allocated(d%layerNames) .or. oldn_names < newn) then
       allocate(tmp_names(newn))
       tmp_names = ''
       if (allocated(d%layerNames) .and. oldn_names > 0) tmp_names(1:oldn_names) = d%layerNames(1:oldn_names)
       call move_alloc(tmp_names, d%layerNames)
    end if
  end subroutine ensure_layer_capacity

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

  !> Regression aid: when MAGPARSER_LAYER_DIGEST is set (to anything but 0), print an
  !> order-independent digest of an operation's output layer so that runs (CPU vs GPU,
  !> baseline vs modified build) can be compared exactly. No effect when unset.
  subroutine PrintLayerDigest( lyr, lname, line_no )
    type(Layer), intent(in) :: lyr
    character(len=*), intent(in) :: lname
    integer, intent(in) :: line_no
    logical, save :: initialized = .false., enabled = .false.
    character(len=16) :: ev
    integer :: evlen, evstat
    integer(kind=int64) :: i, h, hsum, hxor, area, n
    integer(kind=int64), parameter :: M32 = int(z'FFFFFFFF', int64)
    if( .not. initialized ) then
       call get_environment_variable( 'MAGPARSER_LAYER_DIGEST', ev, evlen, evstat )
       enabled = ( evstat == 0 .and. evlen > 0 .and. ev(1:1) /= '0' )
       initialized = .true.
    end if
    if( .not. enabled ) return
    n = lyr%n_used
    hsum = 0; hxor = 0; area = 0
    if( n > 0 .and. allocated( lyr%layer_boxes ) ) then
       do i = 1, n
          associate( b => lyr%layer_boxes(i) )
            area = area + int(b%x2 - b%x1, int64) * int(b%y2 - b%y1, int64)
            h = iand( int(b%x1, int64) * 73856093_int64, M32 )
            h = iand( ieor( h, int(b%y1, int64) * 19349663_int64 ) * 83492791_int64, M32 )
            h = iand( ieor( h, int(b%x2, int64) * 2654435761_int64 ) * 40503_int64, M32 )
            h = iand( ieor( h, int(b%y2, int64) * 2246822519_int64 ), M32 )
            h = iand( ieor( h, ishft( h, -15 ) ) * 668265263_int64, M32 )
            h = ieor( h, ishft( h, -13 ) )
          end associate
          hsum = iand( hsum + h, int(z'FFFFFFFFFFFF', int64) )
          hxor = ieor( hxor, h )
       end do
    end if
    write(*,'(A,I0,A,A,A,I0,A,I0,A,I0,A,I0)') 'LAYER_DIGEST line=', line_no, ' layer=', lname, &
         ' n=', n, ' area=', area, ' hsum=', hsum, ' hxor=', hxor
  end subroutine PrintLayerDigest

  subroutine ParseControlFile( fileName, MAX_LAYERS )
    character(len=*), intent(in) :: fileName
    integer, intent(in) :: MAX_LAYERS
    integer, parameter  :: K_MAX_DBS = 32 !< will this work
    character(len=256) :: line, keyword, rest
    integer :: unit, ios, pos, line_number, db_count, i
    logical :: compact_on
    integer(kind=int64) :: compact_budget
    integer :: lhs_db_index, rhs1_db_index, rhs2_db_index
    integer :: lhs_layer_index, rhs1_layer_index, rhs2_layer_index    
    type(Design), allocatable, target :: design_dbs(:)
    type(Layer), pointer :: lhs_layer, rhs1_layer, rhs2_layer !> either dont use
    type(hash_type) :: ht
    logical :: ins
    character :: operator_char !> single letter only
    character(len=256) :: TEMPORARY_FOLDER
    character(len=256) :: TEMPORARY_LAYER_PREFIX
    character(len=4096) :: filename_work
    real(kind=real64)  :: rvar(16)
    integer(kind=K_COORDINATE_KIND):: ivar(16)
    integer(kind=K_COORDINATE_KIND) :: used_precision
    logical :: banded_done
    character(len=1024) :: m4_cmd, expanded_file, echo_line, helper_name
    integer :: m4_stat, echo_unit, echo_ios, pid_self, pid_unit, helper_unit, helper_ios
    character(len=1024) :: trs_rules_file, trs_file
    integer :: trs_status
    interface
       function c_getpid() bind(C, name="getpid") result(pid)
         use, intrinsic :: iso_c_binding, only: c_int
         integer(c_int) :: pid
       end function c_getpid
    end interface
    compact_on = CompactLayersEnabled()
    compact_budget = compact_budget_bytes()
    if( compact_on ) write(*,'(A,F8.2,A)') 'INFO: compact layers ON, expanded budget ', &
         real(compact_budget,8)/1073741824.0d0, ' GB'
    db_count = 1
    TEMPORARY_FOLDER = ''
    TEMPORARY_LAYER_PREFIX = "MTL_"
    call hash_create(ht,K_MAX_DBS)
    allocate( design_dbs( K_MAX_DBS ) )

    ! Preprocess control file through m4, echo expanded content to stdout, then parse it.
    ! Use mktemp under /tmp/MAGPARSER/ to avoid conflicts between concurrent users/runs.
    pid_self = int( c_getpid() )   ! POSIX; /proc/self/stat exists only on Linux

    call execute_command_line('mkdir -p /tmp/MAGPARSER')
    write(helper_name, '(A,I0,A)') '/tmp/MAGPARSER/.m4name_', pid_self, '.tmp'
    expanded_file = ''
    m4_cmd = 'sh -c ''f=$(mktemp /tmp/MAGPARSER/magparser_m4_ctr_XXXXXX) && m4 "$1" > "$f" && printf "%s\n" "$f" > "$2"'' sh "' &
         // trim(fileName) // '" "' // trim(helper_name) // '"'
    call execute_command_line(trim(m4_cmd), exitstat=m4_stat)
    if (m4_stat /= 0) then
       write(*,*) 'ERROR: m4 preprocessing failed for control file: ', trim(fileName)
       write(*,*) '       m4 exit status: ', m4_stat
       call execute_command_line('rm -f "' // trim(helper_name) // '"')
       stop
    end if

    open(newunit=helper_unit, file=trim(helper_name), status='old', action='read', iostat=helper_ios)
    if (helper_ios /= 0) then
       write(*,*) 'ERROR: Could not read temporary m4 filename for control file: ', trim(fileName)
       call execute_command_line('rm -f "' // trim(helper_name) // '"')
       stop
    end if
    read(helper_unit, '(A)', iostat=helper_ios) expanded_file
    close(helper_unit)
    call execute_command_line('rm -f "' // trim(helper_name) // '"')
    if (helper_ios /= 0 .or. len_trim(expanded_file) == 0) then
       write(*,*) 'ERROR: Could not determine expanded control file name for: ', trim(fileName)
       stop
    end if

    write(*,'(A)') '--- BEGIN M4-EXPANDED CONTROL FILE: ' // trim(fileName) // ' ---'
    open(newunit=echo_unit, file=trim(expanded_file), status='old', action='read', iostat=echo_ios)
    if (echo_ios /= 0) then
       write(*,*) 'ERROR: Could not open m4-expanded control file: ', trim(expanded_file)
       call execute_command_line('rm -f "' // trim(expanded_file) // '"')
       stop
    end if
    do
       read(echo_unit, '(A)', iostat=echo_ios) echo_line
       if (echo_ios /= 0) exit
       write(*,'(A)') trim(echo_line)
    end do
    close(echo_unit)
    write(*,'(A)') '--- END M4-EXPANDED CONTROL FILE ---'

    ! Compile-time DAG analysis + term rewriting (trs_optimizer.f90), enabled by
    ! `var trs_rules <file>` in the CTR or MAGPARSER_TRS_RULES in the environment.
    call TRSFindRulesFile(trim(expanded_file), trs_rules_file)
    if (len_trim(trs_rules_file) > 0) then
       trs_file = trim(expanded_file) // '.trs.ctr'
       write(*,'(A)') '--- BEGIN TRS OPTIMIZATION, rules: ' // trim(trs_rules_file) // ' ---'
       call TRSOptimizeFile(trim(trs_rules_file), trim(expanded_file), trim(trs_file), .true., trs_status)
       if (trs_status == 0) then
          write(*,'(A)') 'INFO: TRS optimized control file: ' // trim(trs_file)
          open(newunit=echo_unit, file=trim(trs_file), status='old', action='read', iostat=echo_ios)
          if (echo_ios == 0) then
             do
                read(echo_unit, '(A)', iostat=echo_ios) echo_line
                if (echo_ios /= 0) exit
                write(*,'(A)') trim(echo_line)
             end do
             close(echo_unit)
          end if
          expanded_file = trs_file
       else
          write(*,'(A)') 'WARNING: TRS optimization failed; running the control file unoptimized.'
       end if
       write(*,'(A)') '--- END TRS OPTIMIZATION ---'
    end if

    ! Open the expanded file for parsing
    open(newunit=unit, file=trim(expanded_file), status='old', action='read', iostat=ios)
    if (ios /= 0) then
       print *, "Error: Could not open expanded control file."
       call execute_command_line('rm -f "' // trim(expanded_file) // '"')
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
       ! Check if line is empty or contains only spaces
       if (len_trim(line) == 0) cycle
       if( compact_on ) call enforce_compact_budget( design_dbs, db_count, compact_budget )
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
          else if( trim(rest(1:pos-1)) == 'band_boxes' ) then
             rest = adjustl(rest)
             pos = index( rest, ' ' )
             rest = trim( adjustl( rest(pos+1:)))
             block
               integer(kind=int64) :: nb_band
               read(rest, *, iostat=ios) nb_band
               if( ios == 0 ) then
                  call BandSetTarget( nb_band )
                  write(*,*) 'INFO: VAR changing band_boxes = ', nb_band
               else
                  write(*,*) 'WARNING: VAR not parsed correctly on line: ', line_number
               end if
             end block
          else if( trim(rest(1:pos-1)) == 'abort_on_xor' ) then
             rest = adjustl(rest)
             pos = index( rest, ' ' )
             rest = trim( adjustl( rest(pos+1:)))
             read(rest, *, iostat=ios) abort_on_xor
             if( ios == 0 ) then
                write(*,*) 'INFO: VAR changing abort_on_xor = ', abort_on_xor
             else
                write(*,*) 'WARNING: VAR not parsed correctly on line: ', line_number
             end if
          else if( trim(rest(1:pos-1)) == 'abort_on_assert_zero' ) then
             rest = adjustl(rest)
             pos = index( rest, ' ' )
             rest = trim( adjustl( rest(pos+1:)))
             read(rest, *, iostat=ios) abort_on_assert_zero
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
          else if( trim(rest(1:pos-1)) == 'trs_rules' .or. trim(rest(1:pos-1)) == 'trs_enable' ) then
             !> consumed at compile time by TRSFindRulesFile / TRSOptimizeFile
             write(*,*) 'INFO: TRS directive (handled before execution): ', trim(rest)
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
             !> An output file is normally written by this run: read it only if it
             !> already exists, otherwise start with an empty design (like a memory db).
             block
               logical :: out_exists
               inquire( file=trim(keyword(1:pos)), exist=out_exists )
               if( out_exists ) then
                  call LoadDesignFileByExt(design_dbs(db_count), MAX_LAYERS)
               else
                  write(*,*) 'INFO: output file does not exist yet, starting empty: ', trim(keyword(1:pos))
                  allocate( design_dbs(db_count)%layerNames( MAX_LAYERS ) )
                  design_dbs(db_count)%layerNames = ''
                  call hash_create(design_dbs(db_count)%ht, MAX_LAYERS )
                  allocate( design_dbs(db_count)%layers( MAX_LAYERS ) )
               end if
             end block
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
             call LoadDesignFileByExt(design_dbs(db_count), MAX_LAYERS)
             db_count = db_count + 1
          else if( trim(rest(1:pos-1)) == 'hierdesign' ) then
             rest = adjustl(rest(pos+1:))
             pos = index( rest, ' ' )
             call hash_put( ht, trim(rest(1:pos-1)) , db_count, ins )
             if( .not. ins ) write (*,*) 'Duplicate db handle seen: ', trim(rest(1:pos-1))
             allocate( design_dbs( db_count )%designName, source = trim(adjustl(rest(1:pos-1))))
             pos = index( rest, ' ' )
             keyword = adjustl(rest(pos+2:))
             pos = index( keyword, ' ' )
             design_dbs(db_count)%fileName = trim(keyword(1:pos))
             call LoadDesignFileByExtHier(design_dbs(db_count), MAX_LAYERS)
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
          if( trim(rest(1:pos-1)) == 'hierextent' ) then
             rest = adjustl(rest(pos+1:))
             if (len_trim(rest) > 0) then
                pos = index(rest, ' ')
                if (pos == 0) pos = len_trim(rest)+1
                call hash_get(ht, trim(rest(1:pos-1)), lhs_db_index, ins)
                if (ins) then
                   write(*,'(A,A,A,4I12)') 'INFO: HIER_EXTENT ', trim(design_dbs(lhs_db_index)%designName), ' = ', &
                        design_dbs(lhs_db_index)%DESIGN_EXTENT%x1, design_dbs(lhs_db_index)%DESIGN_EXTENT%y1, &
                        design_dbs(lhs_db_index)%DESIGN_EXTENT%x2, design_dbs(lhs_db_index)%DESIGN_EXTENT%y2
                else
                   write(*,*) 'ERROR: hierextent unknown db: ', trim(rest)
                end if
             end if
             cycle read_loop
          else if( trim(rest(1:pos-1)) == 'hierinfo' ) then
             rest = adjustl(rest(pos+1:))
             if (len_trim(rest) > 0) then
                pos = index(rest, ' ')
                if (pos == 0) pos = len_trim(rest)+1
                call hash_get(ht, trim(rest(1:pos-1)), lhs_db_index, ins)
                if (ins) then
                   write(*,*) 'INFO: HIER_INFO design=', trim(design_dbs(lhs_db_index)%designName), &
                        ' layers=', hash_nitems(design_dbs(lhs_db_index)%ht), &
                        ' extent=', design_dbs(lhs_db_index)%DESIGN_EXTENT%x1, design_dbs(lhs_db_index)%DESIGN_EXTENT%y1, &
                        design_dbs(lhs_db_index)%DESIGN_EXTENT%x2, design_dbs(lhs_db_index)%DESIGN_EXTENT%y2
                else
                   write(*,*) 'ERROR: hierinfo unknown db: ', trim(rest)
                end if
             end if
             cycle read_loop
          else if( trim(rest(1:pos-1)) == 'init' ) then
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
             call ensure_layer_capacity( design_dbs(lhs_db_index), lhs_layer_index )
             if( .not. ins ) then
                write(*,*) 'ERROR: Unable to INIT layer: ', trim(rest(1:pos-1)), ' at index: ', lhs_layer_index
             else
                design_dbs( lhs_db_index )%layerNames( lhs_layer_index ) = trim(TEMPORARY_LAYER_PREFIX)//trim(rest(1:pos-1))
             end if
             if( trim(rest(1:pos-1)) == 'nothing') then
                allocate( design_dbs( lhs_db_index )%layers( lhs_layer_index )%fileName, source = 'MTL_NOTHING')
             else
                filename_work = ' '
                write(filename_work,'(A,A,A,A)') trim(TEMPORARY_FOLDER), trim(design_dbs( lhs_db_index )%designName), '_', &
                     trim(design_dbs( lhs_db_index )%layerNames(lhs_layer_index))
                design_dbs( lhs_db_index )%layers( lhs_layer_index )%fileName = filename_work(1:len_trim(filename_work))
                write(*,*) 'INFO: Reusing FILE: ', design_dbs( lhs_db_index )%layers( lhs_layer_index )%fileName
             end if
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
                call ensure_layer_capacity( design_dbs(lhs_db_index), lhs_layer_index )
                !> we have to decide temporary layer format
                associate( resolved_layer => design_dbs( lhs_db_index )%layers(lhs_layer_index) )
                  lhs_layer => design_dbs( lhs_db_index )%layers(lhs_layer_index)
                  if( .not. allocated( design_dbs( lhs_db_index )%layerNames ) ) error stop "DB Layernames not populated"
                  design_dbs( lhs_db_index )%layerNames(lhs_layer_index) = &
                       trim(adjustl(TEMPORARY_LAYER_PREFIX))//trim(adjustl(rest(1:pos-1)))
                  filename_work = ' '
                  write(filename_work,'(A,A,A,A)') trim(TEMPORARY_FOLDER), trim(design_dbs( lhs_db_index )%designName), '_', &
                       trim(design_dbs( lhs_db_index )%layerNames(lhs_layer_index))
                  lhs_layer%fileName = filename_work(1:len_trim(filename_work))
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
                write(*,*) 'ERROR: RHS1 layer not found: ', trim(rest(1:pos-1)), &
                     ' check spelling or existence of layer in db. Line: ', line_number-1
                error stop "ERROR: layer not found"
             end if
             if( rhs1_layer_index > size( design_dbs( rhs1_db_index )%layers ) ) then
                write(*,*) 'ERROR: RHS1 layer index ', rhs1_layer_index, ' was skipped at load (raise max_layers). Line: ', line_number-1
                error stop "ERROR: layer beyond max_layers"
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
                   write(*,*) 'ERROR: RHS2 layer not found: ', trim(rest(1:pos-1)), &
                        ' check spelling or existence of layer in db: line: ', line_number-1
                   error stop "ERROR: layer not found"
                end if
                if( rhs2_layer_index > size( design_dbs( rhs2_db_index )%layers ) ) then
                   write(*,*) 'ERROR: RHS2 layer index ', rhs2_layer_index, ' was skipped at load (raise max_layers). Line: ', line_number-1
                   error stop "ERROR: layer beyond max_layers"
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
                banded_done = .false.
                if( index('+*@~!^', operator_char) > 0 ) then
                   if( BandedEligible( rhs1_layer, rhs2_layer ) ) then
                      !> large compact inputs: run band by band, never expanding whole layers
                      call touch_layers( lhs_layer, rhs1_layer, rhs2_layer )
                      select case (operator_char)
                      case('+')
                         call StartMarkTime("OR")
                         call BandedRun( rhs1_layer, rhs2_layer, lhs_layer, K_BAND_OR, 0_int64 )
                         call StopMarkTime("OR")
                      case('*')
                         call StartMarkTime("AND")
                         call BandedRun( rhs1_layer, rhs2_layer, lhs_layer, K_BAND_AND, 0_int64 )
                         call StopMarkTime("AND")
                      case('@')
                         call StartMarkTime("NOT")
                         call BandedRun( rhs1_layer, rhs2_layer, lhs_layer, K_BAND_NOT, 0_int64 )
                         call StopMarkTime("NOT")
                      case('~')
                         call StartMarkTime("FRAMENOT")
                         call BandedRun( rhs1_layer, rhs2_layer, lhs_layer, K_BAND_FRAMENOT, 0_int64 )
                         call StopMarkTime("FRAMENOT")
                      case('!')
                         call StartMarkTime("BOOSTNOT")
                         call BandedRun( rhs1_layer, rhs2_layer, lhs_layer, K_BAND_BOOSTNOT, 0_int64 )
                         call StopMarkTime("BOOSTNOT")
                      case('^')
                         call StartMarkTime("BOOSTXOR")
                         call BandedRun( rhs1_layer, rhs2_layer, lhs_layer, K_BAND_XOR, 0_int64 )
                         call StopMarkTime("BOOSTXOR")
                         if( abort_on_xor > 0 .and. LayerBoxCount( lhs_layer ) > 0 ) then
                            write(*,*) 'ERROR: Aborting RUN since abort_on_xor set > 0, and ^ layer has non-zero shapes.'
                            error stop
                         end if
                      end select
                      banded_done = .true.
                   end if
                end if
                if( .not. banded_done ) then
                call LayerExpand( rhs1_layer )
                call LayerExpand( rhs2_layer )
                if( rhs1_layer%n_used == 0 ) call RestoreSnapToLayer( rhs1_layer, rhs1_layer%fileName )
                if( rhs2_layer%n_used == 0 ) call RestoreSnapToLayer( rhs2_layer, rhs2_layer%fileName )
                call LayerDropCompact( lhs_layer )   !> about to be overwritten
                call touch_layers( lhs_layer, rhs1_layer, rhs2_layer )
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
                end if   !> .not. banded_done
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
                     primary_operator = to_upper_f(trim( adjustl( buf1 ) ))
                     rhs2_source_name = trim( adjustl( buf2 ) )
                     !> Support multi-word topological operators, e.g. INSIDE EDGE, NOT INSIDE EDGE,
                     !> COINCIDENT INSIDE EDGE, NOT COINCIDENT OUTSIDE EDGE. Normalize to underscore form.
                     block
                       character(len=:), allocatable :: rest_u, op_cand
                       character(len=64) :: u1,u2,u3,u4,u5,o1,o2,o3,o4,o5
                       integer :: ios3
                       rest_u = to_upper_f( trim(adjustl(rest)) )
                       u1='';u2='';u3='';u4='';u5='';o1='';o2='';o3='';o4='';o5=''
                       read(rest_u, *, iostat=ios3) u1,u2,u3,u4,u5
                       read(rest,   *, iostat=ios3) o1,o2,o3,o4,o5
                       op_cand = ''
                       if( trim(u1)=='NOT' .and. trim(u2)=='COINCIDENT' .and. trim(u3)=='INSIDE' .and. trim(u4)=='EDGE' ) then
                          op_cand='NOT_COINCIDENT_INSIDE_EDGE'; rhs2_source_name=trim(adjustl(o5))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='COINCIDENT' .and. trim(u3)=='OUTSIDE' .and. trim(u4)=='EDGE' ) then
                          op_cand='NOT_COINCIDENT_OUTSIDE_EDGE'; rhs2_source_name=trim(adjustl(o5))
                       else if( trim(u1)=='COINCIDENT' .and. trim(u2)=='INSIDE' .and. trim(u3)=='EDGE' ) then
                          op_cand='COINCIDENT_INSIDE_EDGE'; rhs2_source_name=trim(adjustl(o4))
                       else if( trim(u1)=='COINCIDENT' .and. trim(u2)=='OUTSIDE' .and. trim(u3)=='EDGE' ) then
                          op_cand='COINCIDENT_OUTSIDE_EDGE'; rhs2_source_name=trim(adjustl(o4))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='INSIDE' .and. trim(u3)=='EDGE' ) then
                          op_cand='NOT_INSIDE_EDGE'; rhs2_source_name=trim(adjustl(o4))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='OUTSIDE' .and. trim(u3)=='EDGE' ) then
                          op_cand='NOT_OUTSIDE_EDGE'; rhs2_source_name=trim(adjustl(o4))
                       else if( trim(u1)=='INSIDE' .and. trim(u2)=='EDGE' ) then
                          op_cand='INSIDE_EDGE'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='OUTSIDE' .and. trim(u2)=='EDGE' ) then
                          op_cand='OUTSIDE_EDGE'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='INTERACT' ) then
                          op_cand='NOT_INTERACT'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='CUT' ) then
                          op_cand='NOT_CUT'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='TOUCH' ) then
                          op_cand='NOT_TOUCH'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='INSIDE' ) then
                          op_cand='NOT_INSIDE'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='OUTSIDE' ) then
                          op_cand='NOT_OUTSIDE'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='EQUAL' ) then
                          op_cand='NOT_EQUAL'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='DISJOINT' ) then
                          op_cand='NOT_DISJOINT'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='MEET' ) then
                          op_cand='NOT_MEET'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='OVERLAPS' ) then
                          op_cand='NOT_OVERLAPS'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='COVERS' ) then
                          op_cand='NOT_COVERS'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='COVEREDBY' ) then
                          op_cand='NOT_COVEREDBY'; rhs2_source_name=trim(adjustl(o3))
                       else if( trim(u1)=='NOT' .and. trim(u2)=='CONTAINS' ) then
                          op_cand='NOT_CONTAINS'; rhs2_source_name=trim(adjustl(o3))
                       end if
                       if( len_trim(op_cand) > 0 ) primary_operator = op_cand
                     end block
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
                        if( rhs2_layer_index > size( design_dbs( rhs2_db_index )%layers ) ) then
                           write(*,*) 'ERROR: RHS2 layer index ', rhs2_layer_index, ' was skipped at load (raise max_layers). Line: ', line_number-1
                           error stop "ERROR: layer beyond max_layers"
                        end if
                        rhs2_layer => design_dbs( rhs2_db_index )%layers( rhs2_layer_index )
                     else
                        nullify( rhs2_layer )
                     end if
                     call StartMarkTime(primary_operator)
                     banded_done = .false.
                     if( associated( rhs1_layer ) .and. associated( rhs2_layer ) .and. &
                         associated( lhs_layer ) .and. primary_operator == 'INTERACT' ) then
                        if( BandedEligible( rhs1_layer, rhs2_layer ) ) then
                           if( LayerIsHealed( rhs1_layer ) ) then
                              call touch_layers( lhs_layer, rhs1_layer, rhs2_layer )
                              call BandedRun( rhs1_layer, rhs2_layer, lhs_layer, K_BAND_INTERACT, 0_int64 )
                              banded_done = .true.
                           end if
                        end if
                     end if
                     if( associated( rhs1_layer ) .and. associated( lhs_layer ) .and. &
                         ( primary_operator == 'GROW' .or. primary_operator == 'SIZE' ) ) then
                        if( BandedEligible( rhs1_layer ) ) then
                           call touch_layers( lhs_layer, rhs1_layer, rhs1_layer )
                           if( primary_operator == 'GROW' ) then
                              read(rest, *, iostat=ios) buf1, buf2, rvar(1), rvar(2), rvar(3), rvar(4)
                              do i=1,4
                                 ivar(i) = int( used_precision*rvar(i), kind = K_COORDINATE_KIND )
                              end do
                              call BandedRun( rhs1_layer, C=lhs_layer, op=K_BAND_GROW, &
                                   halo=int( maxval( abs( ivar(1:4) ) ), int64 ), ivar=ivar(1:4) )
                              banded_done = .true.
                           else if( .not. associated( rhs2_layer ) ) then
                              read(rest, *, iostat=ios) buf1, buf2, rvar(1)
                              ivar(1) = int( rvar(1)*used_precision, kind=K_COORDINATE_KIND )
                              call BandedRun( rhs1_layer, C=lhs_layer, op=K_BAND_SIZE, &
                                   halo=abs( int( ivar(1), int64 ) ), cval=int( ivar(1), int64 ) )
                              banded_done = .true.
                           end if
                        end if
                     end if
                     if( .not. banded_done ) then
                     if( associated( rhs1_layer ) ) then
                        call LayerExpand( rhs1_layer )
                        if( rhs1_layer%n_used == 0 ) call RestoreSnapToLayer( rhs1_layer, rhs1_layer%fileName )
                     end if
                     if( associated( rhs2_layer ) ) then
                        call LayerExpand( rhs2_layer )
                        if( rhs2_layer%n_used == 0 ) call RestoreSnapToLayer( rhs2_layer, rhs2_layer%fileName )
                     end if
                     if( associated( lhs_layer ) ) then
                        call LayerDropCompact( lhs_layer )
                        call touch_layers( lhs_layer, rhs1_layer, rhs2_layer )
                     end if

                     select case (primary_operator)
                     case('OR2')
                        call CalculateOR( rhs1_layer, rhs2_layer, lhs_layer )
                        !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |R2| = ', rhs2_layer%n_used, ' |O1| = ', lhs_layer%n_used
                     case('ASSERT_ZERO')
                        !> for now we have to do t1:nothing = ASSERT_ZERO t1:check_and nothing
                        if( rhs2_source_name /= 'nothing' .and. rhs2_source_name /= 't1:nothing' ) &
                             error stop "ASSERT_ZERO must use nothing as second layer"
                        if( rhs1_layer%n_used > 0 ) then
                           write(*,'(A,I5,A)') &
                                '************ ASSERT_ZERO fail on layer. Line: ', line_number-1, &
                                ' **********************'
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
                        if( rhs2_source_name /= 'nothing' .and. rhs2_source_name /= 't1:nothing') &
                             error stop "EXTENT must use nothing as second layer"
                        !write(*,*) 'Found PRIMARY_OPERATOR = EXTENT'
                        call CreateEXTENT( rhs1_layer, lhs_layer )
                     case ('AND')
                        !> d1:poly AND nothing
                        !if( rhs2_source_name /= 'nothing' ) error stop "AND must use nothing as second layer"
                        !write(*,*) 'Found PRIMARY_OPERATOR = AND'
                        call CalculateSingleLayerAND( rhs1_layer, lhs_layer )
                     case ('XOR')
                        !> d1:poly XOR d2:poly
                        !> Use direct Boost polygon XOR (same as ^) for robustness/performance.
                        !> The grid/frame path (CalculateXOR/CalculateFrameFunction2) can be
                        !> expensive or unstable on large/unhealed inputs.
                        call CalculateBoostOperation( rhs1_layer, rhs2_layer, lhs_layer, K_BOOST_CONTROL_XOR, 0_int64 )
                     case ('INTERACT')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_INTERACT)
                     case ('NOT_INTERACT')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_INTERACT)
                     case ('CUT')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_CUT)
                     case ('NOT_CUT')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_CUT)
                     case ('TOUCH')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_TOUCH)
                     case ('NOT_TOUCH')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_TOUCH)
                     case ('INSIDE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_INSIDE)
                     case ('NOT_INSIDE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_INSIDE)
                     case ('OUTSIDE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_OUTSIDE)
                     case ('NOT_OUTSIDE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_OUTSIDE)
                     case ('INSIDE_EDGE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_INSIDE_EDGE)
                     case ('NOT_INSIDE_EDGE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_INSIDE_EDGE)
                     case ('OUTSIDE_EDGE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_OUTSIDE_EDGE)
                     case ('NOT_OUTSIDE_EDGE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_OUTSIDE_EDGE)
                     case ('COINCIDENT_INSIDE_EDGE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_COINCIDENT_INSIDE_EDGE)
                     case ('NOT_COINCIDENT_INSIDE_EDGE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_COINCIDENT_INSIDE_EDGE)
                     case ('COINCIDENT_OUTSIDE_EDGE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_COINCIDENT_OUTSIDE_EDGE)
                     case ('NOT_COINCIDENT_OUTSIDE_EDGE')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_COINCIDENT_OUTSIDE_EDGE)
                     case ('EQUAL')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_EQUAL)
                     case ('NOT_EQUAL')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_EQUAL)
                     case ('DISJOINT')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_DISJOINT)
                     case ('NOT_DISJOINT')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_DISJOINT)
                     case ('MEET')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_MEET)
                     case ('NOT_MEET')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_MEET)
                     case ('OVERLAPS')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_OVERLAPS)
                     case ('NOT_OVERLAPS')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_OVERLAPS)
                     case ('COVERS')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_COVERS)
                     case ('NOT_COVERS')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_COVERS)
                     case ('COVEREDBY')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_COVEREDBY)
                     case ('NOT_COVEREDBY')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_COVEREDBY)
                     case ('CONTAINS')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_CONTAINS)
                     case ('NOT_CONTAINS')
                        call CalculateTopologicalSelect(rhs1_layer, rhs2_layer, lhs_layer, K_TOPO_NOT_CONTAINS)
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
                        if( rhs2_source_name /= 'nothing' .and. rhs2_source_name /= 't1:nothing' ) &
                             error stop "GROW must use nothing as second layer"
                        !write(*,*) 'RESTJUST before: ', rest
                        read(rest, *, iostat=ios) buf1, buf2, rvar(1), rvar(2), rvar(3), rvar(4)
                        !write(*,'(A,I8,4(A,F8.2))') 'GROW prec ', used_precision, ' ', rvar(1:4)
                        do i=1,4
                           ivar(i) = int( used_precision*rvar(i), kind = K_COORDINATE_KIND )
                        end do
                        call CalculateGROWLayer( rhs1_layer, lhs_layer, ivar(1:4) )
                     case ('SHRINK') !> SHRINK: morphological erosion/inset. Supports 4-directional args (E N W S) or single value.
                        if( rhs2_source_name /= 'nothing' .and. rhs2_source_name /= 't1:nothing' ) &
                             error stop "SHRINK must use nothing as second layer"
                        rvar = 0.0_real64
                        read(rest, *, iostat=ios) buf1, buf2, rvar(1), rvar(2), rvar(3), rvar(4)
                        if( ios /= 0 ) then
                           read(rest, *, iostat=ios) buf1, buf2, rvar(1)
                           rvar(2)=rvar(1); rvar(3)=rvar(1); rvar(4)=rvar(1)
                        end if
                        do i=1,4
                           ivar(i) = int( used_precision*rvar(i), kind = K_COORDINATE_KIND )
                        end do
                        if( ivar(1)==ivar(2) .and. ivar(2)==ivar(3) .and. ivar(3)==ivar(4) ) then
                           call CalculateSHRINKLayer( rhs1_layer, ivar(1), lhs_layer )
                        else
                           call CalculateSHRINKDirectional( rhs1_layer, lhs_layer, ivar(1:4) )
                        end if
                     case ('SIZE')
                        if( rhs2_source_name /= 'nothing' .and. rhs2_source_name /= 't1:nothing' ) &
                             error stop "SIZE must use nothing as second layer"
                        read(rest, *, iostat=ios) buf1, buf2, rvar(1)
                        !write(*,*) 'Found PRIMARY_OPERATOR = SIZE ', rvar(1)
                        ivar(1) = int( rvar(1)*used_precision, kind=K_COORDINATE_KIND )
                        if( associated(rhs2_layer) ) then
                           call CalculateBoostOperation( rhs1_layer, rhs2_layer, lhs_layer, &
                                K_BOOST_CONTROL_SIZE, int( ivar(1), kind=int64 ) )
                        else
                           call CalculateBoostOperation( rhs1_layer, rhs1_layer, lhs_layer, &
                                K_BOOST_CONTROL_SIZE, int( ivar(1), kind=int64 ) )
                        end if
                        !write(*,'(A,I12,A,I12,A,I12)') '|R1| = ', rhs1_layer%n_used, ' |O1| = ', lhs_layer%n_used
                     case ('WORMHOLE')
                        write(*,*) 'Found PRIMARY_OPERATOR = WORMHOLE'
                        !> d1:met1 WORMHOLE f1:met1_marker 5 100
                     case default
                        write(*,*) 'UNKNOWN primary_operator: ', primary_operator, ' on line: ', line_number-1
                        error stop "SYNTAX ERROR on TEXT based operator"
                     end select
                     end if   !> .not. banded_done
                     call StopMarkTime(primary_operator)
                  else
                     write(*,*) 'SYNTAX ERROR on line: ', line_number-1
                     error stop "SYNTAX ERROR on TEXT based operator"
                  end if
                end block
             end if
             if( associated( lhs_layer ) ) call PrintLayerDigest( lhs_layer, &
                  trim(design_dbs(lhs_db_index)%designName)//':'// &
                  trim(design_dbs(lhs_db_index)%layerNames(lhs_layer_index)), line_number-1 )
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
             if( design_dbs(i)%design_direction == DESIGN_DIRECTION_OUTPUT .or. &
                  ( temporary_layers == K_TEMPORARY_LAYER_DISK .and. &
                  design_dbs(i)%design_direction == DESIGN_DIRECTION_MEMORY ) ) then
                if( allocated( design_dbs(i)%layers ) ) then
                   block
                     integer :: jl
                     do jl = 1, size( design_dbs(i)%layers )
                        call LayerExpand( design_dbs(i)%layers(jl) )
                     end do
                   end block
                end if
             end if
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
    close(unit, status='delete')
  end subroutine ParseControlFile

  !> Mark the layers used by the current op as most recently used (LRU clock).
  subroutine touch_layers( a, b, c )
    type(Layer), pointer, intent(in) :: a, b, c
    integer(kind=int64), save :: clock = 0
    clock = clock + 1
    if( associated(a) ) a%last_use = clock
    if( associated(b) ) b%last_use = clock
    if( associated(c) ) c%last_use = clock
  end subroutine touch_layers

  integer(kind=int64) function compact_budget_bytes() result(nb)
    character(len=32) :: ev
    integer :: evlen, evstat, ios
    real(kind=8) :: gb
    gb = 16.0d0
    call get_environment_variable( 'MAGPARSER_COMPACT_BUDGET_GB', ev, evlen, evstat )
    if( evstat == 0 .and. evlen > 0 ) then
       read(ev, *, iostat=ios) gb
       if( ios /= 0 ) gb = 16.0d0
    end if
    nb = int( gb * 1073741824.0d0, int64 )
  end function compact_budget_bytes

  !> Compact least-recently-used expanded layers until the expanded working set fits
  !> the budget. Layers used by the op on the previous line are compacted last.
  subroutine enforce_compact_budget( dbs, ndb, budget )
    type(Design), intent(inout), target :: dbs(:)
    integer, intent(in) :: ndb
    integer(kind=int64), intent(in) :: budget
    integer(kind=int64) :: total, oldest
    integer :: d, l, bd, bl
    do
       total = 0; oldest = huge(oldest); bd = 0; bl = 0
       do d = 1, min( ndb, size(dbs) )
          if( .not. allocated( dbs(d)%layers ) ) cycle
          do l = 1, size( dbs(d)%layers )
             associate( L_ => dbs(d)%layers(l) )
               if( L_%is_compact .or. L_%n_used < 65536_int64 ) cycle
               total = total + LayerExpandedBytes( L_ )
               if( L_%last_use < oldest ) then
                  oldest = L_%last_use; bd = d; bl = l
               end if
             end associate
          end do
       end do
       if( total <= budget .or. bd == 0 ) exit
       call LayerCompact( dbs(bd)%layers(bl) )
       if( .not. dbs(bd)%layers(bl)%is_compact ) dbs(bd)%layers(bl)%last_use = huge(oldest)  !> not encodable: skip
    end do
  end subroutine enforce_compact_budget

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

  pure function to_upper_f(s) result(r)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: r
    integer :: i, c
    allocate(character(len=len(s)) :: r)
    r = s
    do i=1,len(s)
      c = iachar(s(i:i))
      if(c>=iachar('a') .and. c<=iachar('z')) r(i:i)=achar(c-32)
    end do
  end function to_upper_f
end module ControlModule

