! File    : magparser.f90
! Author  : Sandeep Koranne (C) All rights.reserved.
! Purpose : Implementation of MAGIC VLSI format parser
!
! Modern Fortran parser for INV.mag file format
! This program reads and parses Magic VLSI layout files containing geometric
! definitions and component information. The parser extracts rectangle
! coordinates and label information from various sections (nwell, nmos, pmos,
! ndiff, pdiff, poly, metal1, labels) and stores them in a structured Box type.
!
! File format structure:
! - Magic file identifier
! - Technology timestamp
! - Section headers enclosed in << >> brackets
! - Rectangle definitions: "rect x1 y1 x2 y2"
! - Label definitions: "rlabel layer x1 y1 x2 y2 z label_name"
!
! The parser handles:
! - Section boundary detection
! - Coordinate parsing for rectangles
! - Label extraction with layer information
! - Error handling for malformed input
! - Dynamic memory allocation for box storage
!
! Sample input file structure:
! magic
! tech sky130A
! timestamp 1733796373
! << nwell >>
! rect -25 65 90 185 << labels >>
! rlabel metal1 -30 -45 102 -30 1 VGND
! << end >>

! magic
! tech sky130A
! timestamp 1733796373
! << nwell >>
! rect -25 65 90 185
! << nmos >>
! rect 20 -50 40 25
! << pmos >>
! rect 20 90 40 165
! << ndiff >>
! rect -10 -50 20 25
! rect 40 -50 70 25
! << pdiff >>
! rect -5 90 20 165
! rect 40 90 70 165
! << poly >>
! rect 20 165 40 180
! rect 20 25 40 90
! rect 20 -65 40 -50
! << metal1 >>
! rect -25 145 90 160
! rect -30 -45 102 -30
! << labels >>
! rlabel metal1 -30 -45 102 -30 1 VGND
! rlabel metal1 -25 145 90 160 1 VPWR
! << end >>
module MagicVLSILayoutParser
  use CommonModule
  use hash_mod
  use DesignModule
  use GeometryModule
  use DesignModule
  use RTreeBuilder
  use HDFDataModule
  use KLDataModule
  use PNumMergeModule
  use SystemInformationModule
  use MortonSortModule
  use MortonSortOMT
  use PolygonFractureModule
  use SerializationModule
  use iso_c_binding
  use iso_fortran_env, only : int32, int64
  implicit none
  private
  public:: parseMagicLayoutFile, writeMagicLayoutFile
  interface
     function gzopen(path, mode) bind(C, name="gzopen")
       import :: c_ptr, c_char
       type(c_ptr) :: gzopen
       character(kind=c_char), intent(in) :: path(*)
       character(kind=c_char), intent(in) :: mode(*)
     end function gzopen

     function gzgets(file, buf, len) bind(C, name="gzgets")
       import :: c_ptr, c_char, c_int
       type(c_ptr) :: gzgets
       type(c_ptr), value :: file
       character(kind=c_char), intent(out) :: buf(*)
       integer(c_int), value :: len
     end function gzgets

     function gzclose(file) bind(C, name="gzclose")
       import :: c_ptr, c_int
       integer(c_int) :: gzclose
       type(c_ptr), value :: file
     end function gzclose
  end interface

  integer :: MAX_N = 10
  integer, parameter :: M = 16                ! max entries per node
  integer, parameter :: MAX_CHILD = M         ! internal nodes have ≤ M children
contains

  subroutine parseMagicLayoutFile(load_design, MAX_LAYERS)
    implicit none
    ! Parses Magic VLSI layout files with component sections and rectangle definitions
    type(Design), intent(inout), target :: load_design
    integer, intent(in) :: MAX_LAYERS
    type(hash_type), pointer :: ht
    character(len=1024), dimension(:), pointer :: layerNames(:)
    type(Layer), pointer :: layers(:)
    type(Box), pointer :: boxes(:)
    type(Box)          :: tempBox
    integer :: box_count = 0
    character(len=1024) :: line
    character(len=:), allocatable :: fileName
    character(len=200) :: section_name
    character(len=200) :: dummy
    integer :: i, j
    integer :: x1, y1, x2, y2
    integer :: line_number
    integer :: layer_count, current_layer_id
    logical :: found_section
    integer :: layer_id
    logical :: ins,ok
    integer :: ASCALE = 1
    integer :: BSCALE = 1
    integer, parameter :: INIT_ALLOC = 4
    ! to support compressed files
    real        :: t1, t2
    !> fileName processing logic for HDF5
    integer :: dot_pos
    ! Deferred-length allocatable strings (Modern Fortran feature)
    ! These automatically resize to fit the data assigned to them.
    character(len=:), allocatable :: prefix
    integer(kind=8) :: start_tick, end_tick, clock_rate
    real(kind=8)    :: elapsed_time, elapsed_time2
    integer(kind=8) :: num_roots, num_rects
    real(kind=real64),allocatable :: overlap_areas(:)
    real(kind=real64),allocatable :: overlap_perimeter(:)
    real(kind=real64)             :: area_by_union
    type(Box), allocatable :: extents(:)
    type(Box), pointer            :: DESIGN_EXTENT
    integer            :: env_len, env_status
    integer            :: NUM_LAYERS
    ! 1. Get the number of ticks per second
    fileName = load_design%fileName
    line_number = 0
    layer_count = 1
    current_layer_id = 1
    NUM_LAYERS = 0
    call system_clock(count_rate=clock_rate)

    ! Initialize the layer structure by allocating memory for box arrays
    ! This loop iterates through all possible layers in the hash table
    ! For each layer (from 1 to MAX_LAYERS), array of Box objects
    ! The allocation occurs at initialization time, making subsequent box insertions
    ! into layers very fast as they simply require array indexing rather than memory allocation
    call StartMarkTime("ReadDB")
    write (*,*) '+-----------------------------------------------------------------+'
    allocate(load_design%layers(MAX_LAYERS))
    allocate(extents(MAX_LAYERS))
    allocate(load_design%layerNames(MAX_LAYERS))
    allocate(overlap_areas(MAX_LAYERS))
    allocate(overlap_perimeter(MAX_LAYERS))
    ht => load_design%ht
    layerNames => load_design%layerNames
    layers => load_design%layers
    DESIGN_EXTENT => load_design%DESIGN_EXTENT
    do i = 1, MAX_LAYERS
       allocate(layers(i)%layer_boxes(INIT_ALLOC))
       layers(i)%n_used  = 0
       layers(i)%n_alloc = 4
    end do
    do i = 1, MAX_LAYERS
       call extents(i)%reset_to_infinity()
    end do
    call DESIGN_EXTENT%reset_to_infinity()
    call hash_create(ht,MAX_LAYERS)
    !write(*,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    !write(*,*)
    ! Open and parse the file
    i = len_trim(fileName)
    open(unit=10, file=fileName, status='old', action='read')
    dot_pos = index( trim( fileName ), '.', back = .true. )
    if( dot_pos < 0 ) then
       stop 'Use proper file name'
    end if
    prefix = fileName( 1 : dot_pos - 1 )
    print *, "The file is NOT a gzipped file, w/prefix: ", prefix
    mag_file_processing_loop:do
       read(10, '(A)', end=100) line
       line_number = line_number + 1

       ! Skip empty lines and comments
       if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
       if (line(1:8) == 'magscale') then
          read(line, *, iostat=i) dummy, ASCALE, BSCALE
          write(*,*) 'MAGIC is using scaling parameters: ', ASCALE, ' ', BSCALE
          cycle
       end if

       ! Check for section headers
       if (line(1:9) == '<< end >>') then
          exit mag_file_processing_loop
       end if
       if (line(1:2) == '<<') then
          section_name = trim(line(4:len_trim(line)-2))
          section_name = trim(section_name)
          if( section_name == "labels" ) cycle
          if( section_name == "end" ) cycle
          write (*,'(3A10,I5)') 'Layer = ', section_name, ' = id: ', layer_count
          call hash_put( ht, section_name, layer_count, ins )
          if( .not. ins ) write (*,*) 'Duplicate layer seen: ', section_name
          layerNames(layer_count) = trim(adjustl(section_name))
          current_layer_id = layer_count
          layer_count = layer_count + 1
          found_section = .true.
          cycle
       end if

       ! Parse rectangle definitions
       if (line(1:4) == 'rect' .and. found_section) then
          !write (*,*) line
          ! Parse rectangle coordinates
          read(line, *, iostat=i) dummy, x1, y1, x2, y2
          associate( resolved_layer => layers(current_layer_id) )
            tempBox%x1 = x1
            tempBox%y1 = y1
            tempBox%x2 = x2
            tempBox%y2 = y2
            call addBoxToLayer( resolved_layer, tempBox )
          end associate
       end if
       !> INSPECT, ALLOCATE, RESOLVE, LOAD
       if (line(1:5) == 'KLBIN' .and. found_section) then
          !write (*,*) line
          call hash_get( ht, trim(section_name), layer_id, ok )            
          !if( allocated( layers(layer_id)%item ) ) deallocate( layers(layer_id)%item )
          !allocate( Layer :: layers(layer_id) )
          associate( resolved_layer => layers(layer_id) )
            ! Parse rectangle coordinates
            section_name = trim(line(7:len_trim(line)))
            if( load_design%design_direction == DESIGN_DIRECTION_INPUT ) then
               if( allocated( resolved_layer%layer_boxes ) ) deallocate( resolved_layer%layer_boxes )
               !call LoadKLBin( section_name, resolved_layer%layer_boxes )
               call RestoreSnapToLayer( resolved_layer, section_name )
               !resolved_layer%n_used = size( resolved_layer%layer_boxes )
               !resolved_layer%n_alloc = resolved_layer%n_used
               !write(*,'(A,A8,A30)') 'Input  Request KLBIN: ', layerNames(layer_id), ' => ', section_name
            else if( load_design%design_direction == DESIGN_DIRECTION_OUTPUT ) then
               !write(*,'(A,A10,A30)') 'Output Request KLBIN: ', trim(layerNames(layer_id)), ' => ', trim(adjustl(section_name))
            end if
            allocate( resolved_layer%fileName, source= trim(adjustl(section_name)))
          end associate
       end if
       if ( line(1:4) == 'SNAP' .and. found_section) then
          !write (*,*) line
          call hash_get( ht, trim(section_name), layer_id, ok )
          associate( resolved_layer => layers(layer_id) )
            section_name = trim(line(6:len_trim(line))) !>>> ROOKIE <<<
            if( load_design%design_direction == DESIGN_DIRECTION_INPUT ) then
               call RestoreSnapToLayer( resolved_layer, section_name )
               resolved_layer%n_alloc = resolved_layer%n_used
               !write(*,'(A,A8,A30)') 'Input  Request SNAP: ', layerNames(layer_id), ' => ', section_name
            else if( load_design%design_direction == DESIGN_DIRECTION_OUTPUT ) then
               !write(*,'(A,A10,A30)') 'Output Request SNAP: ', trim(layerNames(layer_id)), ' => ', trim(adjustl(section_name))
            end if
            allocate( resolved_layer%fileName, source= trim(adjustl(section_name)))
          end associate
       end if
       if (line(1:5) == 'DSNAP' .and. found_section) then
          !write (*,*) line
          call hash_get( ht, trim(section_name), layer_id, ok )
          ! Parse rectangle coordinates
          section_name = trim(line(7:len_trim(line)))
          if( load_design%design_direction == DESIGN_DIRECTION_INPUT ) then
             call RestoreSnapToDLayer( load_design%layers(layer_id), section_name )
             layers(layer_id)%n_alloc = layers(layer_id)%n_used
             !write(*,'(A,A8,A30)') 'Input  Request SNAP: ', layerNames(layer_id), ' => ', section_name
          else if( load_design%design_direction == DESIGN_DIRECTION_OUTPUT ) then
             !write(*,'(A,A10,A30)') 'Output Request SNAP: ', trim(layerNames(layer_id)), ' => ', trim(adjustl(section_name))
          end if
          allocate( layers(layer_id)%fileName, source= trim(adjustl(section_name)))
       end if

       ! Parse label definitions
       if (line(1:5) == 'rlabel' .and. found_section) then
          ! Parse label information
          ! rlabel metal1 -25 145 90 160 1 VPWR
          read(line, *, iostat=i) dummy, dummy, x1, y1, x2, y2, dummy, dummy
          box_count = box_count + 1
          !boxes(box_count)%x1 = x1
          !boxes(box_count)%y1 = y1
          !boxes(box_count)%x2 = x2
          !boxes(box_count)%y2 = y2
       end if
    end do mag_file_processing_loop
100 continue ! this is end of file
    close(10)
    nullify(boxes)
    NUM_LAYERS = hash_nitems( load_design%ht )
    ! Print parsed results
    write(*,*) 'Parsed ', hash_nitems(ht), ' layer(s).'
    call StopMarkTime("ReadDB")
    if( load_design%design_direction /= DESIGN_DIRECTION_INPUT ) then
       return
    end if
    call StartMarkTime("SortBuildRTree")
    call cpu_time(t1)
    call system_clock(count=start_tick)
    do i = 1, NUM_LAYERS
       associate( resolved_layer => layers(i) )
         call BuildTree( resolved_layer )
       end associate
    end do
    call cpu_time(t2)
    call system_clock(count=end_tick)
    elapsed_time = real(end_tick - start_tick, kind=8) / real(clock_rate, kind=8)
    !print '(A, F12.2, A,F12.2,A)', 'Sorting/OMT completed in ', t2 - t1, ' CPU seconds.', elapsed_time, ' REAL seconds.'
    call StopMarkTime("SortBuildRTree")
    !print *, "=== number of boxes stored per layer ==="
    !> everything else after this is mostly informatics, except the UNION/OVLP check
    write (*,*) '+--------------------------------------------------+'
    call StartMarkTime("TreeCheckLoop")
    write (*,*) '+--------------------------------------------------+'
    write(*,'(A8,A12,A30)') 'Layer','Total','RTree CPU and REAL time'
    write (*,*) '+--------------------------------------------------+'
    tree_check_loop: do i = 1, NUM_LAYERS
       associate( resolved_layer => layers(i) )
         if( resolved_layer%n_used == 0 ) cycle
         call system_clock(count=start_tick)
         call cpu_time(t1)
         env_status = 1
         call get_environment_variable( 'MAGPARSER_CONTROL_SELFTEST_TREE', length=env_len, status=env_status )
         if( env_status == 0 ) then
            write(*,*) 'ENV MAGPARSER_CONTROL_SELFTEST_TREE is ON'
            call SelfTestTheTree( resolved_layer%layer_boxes, K_LEAF_CAPACITY, resolved_layer%tree%tree_nodes, resolved_layer%tree%root_index )
         end if
         call cpu_time(t2)
         call system_clock(count=end_tick)
         elapsed_time = real(end_tick - start_tick, kind=8) / real(clock_rate, kind=8)
         write(*,'(A8,I12,F12.2,F12.2)') layerNames(i), resolved_layer%n_used, &
              (t2-t1), elapsed_time
       end associate
    end do tree_check_loop
    write (*,*) '+--------------------------------------------------+'
    call StopMarkTime("TreeCheckLoop")
    call StartMarkTime("PNumLoop")
    write (*,*) '+-----------------------------------------------------------------------------+'
    write(*,'(A8,3(A12),A5,A30)') 'Layer','Polygons','Rects','Total','ST','RTree CPU and REAL time'
    write (*,*) '+-----------------------------------------------------------------------------+'
    !> Polygon Number loop, this loop is parallelized inside over each polygon/box
    pnum_loop: do i = 1, NUM_LAYERS
       block
         integer(kind=int64), allocatable :: area_overlap_roots(:) 
         associate( resolved_layer => layers(i) )
           if( resolved_layer%n_used == 0 ) cycle
           ! 2. Record the start tick
           call system_clock(count=start_tick)
           call cpu_time(t1)
           call MergeHealLayer( resolved_layer )
           call BuildTree( resolved_layer )
           call PerformMergeWithOverlapDetection( resolved_layer%pnumtable, resolved_layer%layer_boxes, K_LEAF_CAPACITY, resolved_layer%tree%tree_nodes,&
                resolved_layer%tree%root_index, overlap_areas(i), overlap_perimeter(i), area_overlap_roots)
           call cpu_time(t2)
           call system_clock(count=end_tick)
           elapsed_time = real(end_tick - start_tick, kind=8) / real(clock_rate, kind=8)
           num_roots = resolved_layer%pnumtable%count_roots()
           num_rects = count(resolved_layer%pnumtable%arr == 0)
           resolved_layer%layerState = ior( resolved_layer%layerState, LAYER_STATE_PNUM )
           if( overlap_areas(i) > 0.0 ) then
              !> this layer needs HEALING as we have detected overlap
              !write(*,*) 'PerformUnion as overlap detected on ', size(area_overlap_roots), ' roots ', elapsed_time
              !write(*,'(A,A12,A,F20.2)') 'OVERLAP AREAS for layer: ', layerNames(i), ' = ', overlap_areas(i)
              !call PerformUnion( layers(i) )
              call PerformPolygonUnion( resolved_layer, area_overlap_roots ) !> this performs the Merge so no need to duplicate
              if( allocated( area_overlap_roots ) ) deallocate( area_overlap_roots )
              if( NeedsPNum( resolved_layer ) ) then
                 call PerformMerge( resolved_layer%pnumtable, resolved_layer%layer_boxes, K_LEAF_CAPACITY, resolved_layer%tree%tree_nodes,&
                      resolved_layer%tree%root_index, overlap_areas(i), overlap_perimeter(i))
                 resolved_layer%layerState = ior( resolved_layer%layerState, LAYER_STATE_PNUM )
              end if
              num_roots = resolved_layer%pnumtable%count_roots()
              num_rects = count(resolved_layer%pnumtable%arr == 0)
              if( abs(overlap_areas(i)) > K_SMALL_EPSILON ) then
                 write(*,'(A,I3,A8,A8,A,I12,A,I12,A,I2,A,F12.2,A,F12.2,A)') 'Layer: ', i, ' ', layerNames(i), ' has ', num_roots, &
                      ' non-rects ',num_rects , ' rects. STAT ',resolved_layer%layerState, &
                      ' |RTREE| = CPU ', (t2-t1), ' secs.', elapsed_time, ' REAL secs'
                 write(*,*) 'OVLP AREA = ', overlap_areas(i)
                 resolved_layer%layerState = iand( resolved_layer%layerState, NOT(LAYER_STATE_HEAL ) )
                 error stop 'UNION failed.'
              else
                 resolved_layer%layerState = ior( resolved_layer%layerState, LAYER_STATE_HEAL )
              end if
           else
              resolved_layer%layerState = ior( resolved_layer%layerState, LAYER_STATE_HEAL )
           end if
           call system_clock(count=end_tick)
           elapsed_time2 = real(end_tick - start_tick, kind=8) / real(clock_rate, kind=8)
           write(*,'(A8,3(I12),I5,F12.2,F12.2)') layerNames(i), &
                num_roots, num_rects , resolved_layer%n_used, resolved_layer%layerState, &
                (t2-t1), elapsed_time2
         end associate
       end block
    end do pnum_loop
    write (*,*) '+-----------------------------------------------------------------------------+'
    call StopMarkTime("PNumLoop")
    
    call StartMarkTime("ExtentLoop")
    do i = 1,NUM_LAYERS
       associate( resolved_layer => layers(i) )
         if( resolved_layer%n_used == 0 ) then
            write(*,*) 'Layer: ', layerNames(i), ' is EMPTY'
            cycle
         end if
         extents(i) = mbr_of_array( resolved_layer%layer_boxes, resolved_layer%n_used )
         if( .not. extents(i)%is_valid() ) error stop "ERROR: Layer EXTENT not valid, but layer not empty"
         DESIGN_EXTENT = DESIGN_EXTENT + extents(i)
         resolved_layer%area = 0.0
         if( .not. NeedsHealing( resolved_layer ) ) then
            do j = 1, resolved_layer%n_used
               resolved_layer%area = resolved_layer%area + box_area( resolved_layer%layer_boxes(j) )
               resolved_layer%perimeter = resolved_layer%perimeter + box_perimeter( resolved_layer%layer_boxes(j) )
            end do
            area_by_union = calculate_union_area_by_polygon( resolved_layer )
            if( resolved_layer%area /= area_by_union ) then
               write(*,*) 'Layer ',i, 'Layer State ', resolved_layer%layerState
               write(*,*) 'Union Area by SCANLINE: ', area_by_union, ' OVLP: ', resolved_layer%area
               error stop
            end if
            resolved_layer%perimeter = resolved_layer%perimeter - overlap_perimeter(i)
            if( resolved_layer%perimeter < 0.0 ) error stop "INCONSISTENT PERIMETER detected."
         else
            resolved_layer%area = calculate_union_area_by_polygon( resolved_layer )
         end if
       end associate
    end do
    if( .not. DESIGN_EXTENT%is_valid() ) then
       error stop 'Design EXTENT is not valid'
    end if
    call StopMarkTime("ExtentLoop")

    write (*,*) '+-----------------------------------------------------------------+'
    write (*,*) '+                Layer Areas and Perimeters                       +'
    write (*,*) '+-----------------------------------------------------------------+'
    do i = 1, NUM_LAYERS
       associate( resolved_layer => layers(i) )
         if( extents(i)%is_valid() ) then
            write(*,'(A1,A12,F18.8,A1,F18.8)') ' ',layerNames(i), resolved_layer%area*1e-6, ' ', resolved_layer%perimeter*1e-6
         else if( resolved_layer%n_used > 0 ) then
            write(*,*) 'ERROR: This is not expected: ', resolved_layer%n_used, ' ', extents(i)
            error stop
         else
            write(*,*) 'Layer: ', layerNames(i), ' is EMPTY'
         end if
       end associate
    end do
    write (*,*) '+-----------------------------------------------------------------+'
    write(*,*) ''

    write (*,*) '+-------------------------- Design Extent ------------------------+'
    call DESIGN_EXTENT%print_box()
    write (*,*) '+---------------------------Layers Extent ------------------------+'
    do i = 1, MAX_LAYERS
       if( extents(i)%is_valid() ) call extents(i)%print_box()
    end do
    write (*,*) '+-----------------------------------------------------------------+'
    write(*,*) ''
  end subroutine parseMagicLayoutFile

  subroutine ResizeLayer(boxes, newSize)
    type(Box), allocatable, intent(inout) :: boxes(:)
    integer(kind=int64), intent(in)  :: newSize
    type(Box), allocatable :: tmp(:)
    allocate(tmp(newSize))
    if (size(boxes)> 0) tmp(1:size(boxes)) = boxes(1:size(boxes))   ! copy old data
    call move_alloc(from=tmp, to=boxes)   ! replace the old array
  end subroutine ResizeLayer

  subroutine addBoxToLayer( input_layer, tempBox )
    type(Layer), intent(inout) :: input_layer
    type(Box), intent(in) :: tempBox
    integer(kind=8) :: newSize
    if( CheckBox( tempBox ) ) then
       stop "ERROR: box in valid"
    end if
    if (input_layer%n_used == input_layer%n_alloc) then                ! buffer full → grow
       newSize = max(1_int64, input_layer%n_alloc*2)                ! double the size
       call ResizeLayer(input_layer%layer_boxes, newSize)
       input_layer%n_alloc = newSize
    end if
    input_layer%n_used = input_layer%n_used + 1
    input_layer%layer_boxes(input_layer%n_used) = tempBox
  end subroutine addBoxToLayer

  !> support for writing VLSI Magic format with binary payload of geometry data
  subroutine writeMagicLayoutFile(load_design)
    ! Write Magic VLSI layout files with component sections and rectangle definitions
    type(Design), intent(inout), target :: load_design
    integer :: i, pos, deleted_layer_count
    if( load_design%design_direction /= DESIGN_DIRECTION_OUTPUT ) then
       if( temporary_layers /= K_TEMPORARY_LAYER_DISK ) then
          error stop "Non-output DB being written out" !> I may relax this, but not good idea
       end if
    end if
    if( debug_verbosity > 1 ) then
       write(*,*) 'Writing DB: ', load_design%fileName
    end if
    deleted_layer_count = 0
    if( .not. allocated( load_design%layers ) ) return !> we have already flushed this db
    do i=1,hash_nitems( load_design%ht)
       associate( resolved_layer => load_design%layers(i) )
         if( resolved_layer%n_used == 0 ) cycle
         !> Lets just assume we are writing in KLBIN
         if(.not. allocated( resolved_layer%fileName ) ) then
            error stop "ERROR: layer backing store name not allocated"
         end if
         if( load_design%design_direction == DESIGN_DIRECTION_MEMORY ) then
            !> we are not going to check syntax
            call SaveLayerToSnap( resolved_layer, resolved_layer%fileName, K_COMPRESSION_METHOD_TO_USE )
            call ClearLayer( resolved_layer )
            deleted_layer_count = deleted_layer_count + 1
            cycle
         end if
         pos = index( resolved_layer%fileName, ".snap" )
         !write(*,*) 'IS_SNAP ',pos,' ',load_design%layers(i)%fileName
         if( pos == len_trim(resolved_layer%fileName)-4 ) then
            call SaveLayerToSnap( resolved_layer, resolved_layer%fileName, K_COMPRESSION_METHOD_TO_USE )
            call ClearLayer( resolved_layer )
            deleted_layer_count = deleted_layer_count + 1
            cycle
         end if
         pos = index( resolved_layer%fileName, ".bin" )
         !write(*,*) 'pos = ', pos, ' len = ', len_trim(load_design%layers(i)%fileName)
         if( pos /= len_trim(resolved_layer%fileName)-3 ) then
            write(*,*) 'ERROR: File format not supported: ', resolved_layer%fileName
            error stop "DBOUT only supports KLBIN, edit the output MAG file to fix"
         end if
         call WriteKLBin( resolved_layer%fileName, resolved_layer%layer_boxes, resolved_layer%n_used )
         !> as an memory optimization we are going to delete the layer after a flush
         !> if this layer is reused later, that will be a problem, so let us deliberately let it crash for now
         call ClearLayer( resolved_layer )
         deleted_layer_count = deleted_layer_count + 1
       end associate
    end do
    if( debug_verbosity > 1 ) then
       write(*,*) 'Written and delete : ', deleted_layer_count, ' layers.'
    end if
  end subroutine writeMagicLayoutFile
end module MagicVLSILayoutParser
