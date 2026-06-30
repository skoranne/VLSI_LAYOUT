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
      integer :: layer_count
      logical :: found_section
      integer :: layer_id
      logical :: ins,ok
      integer :: ASCALE = 1
      integer :: BSCALE = 1
      integer, parameter :: INIT_ALLOC = 4
      ! to support compressed files
      type(c_ptr) :: gz_file
      integer(c_int) :: status
      type(c_ptr) :: res_ptr
      real        :: t1, t2
      !> fileName processing logic for HDF5
      integer :: dot_pos
      ! Deferred-length allocatable strings (Modern Fortran feature)
      ! These automatically resize to fit the data assigned to them.
      character(len=:), allocatable :: prefix
      character(len=:), allocatable :: layerFileName
      character(10)                 :: num_str
      integer(kind=8) :: start_tick, end_tick, clock_rate
      real(kind=8)    :: elapsed_time, elapsed_time2
      integer(kind=8) :: num_roots, num_rects
      real(kind=real64),allocatable :: overlap_areas(:)
      real(kind=real64),allocatable :: overlap_perimeter(:)
      real(kind=real64)             :: area_by_union
      type(Box), allocatable :: extents(:)
      type(Box), pointer            :: DESIGN_EXTENT
      integer(kind=int64)    :: num_squares
      integer            :: env_len, env_status
      integer(kind=int64):: number_expected_interactions

      ! 1. Get the number of ticks per second
      fileName = load_design%fileName
      line_number = 0
      layer_count = 1
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
         layers(i)%n_alloc = INIT_ALLOC
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
      if (i >= 3 .and. fileName(i-2:i) == ".gz") then
         dot_pos = index( trim( fileName ), '.' )
         if( dot_pos < 0 ) then
            stop 'Use proper file name'
         end if
         prefix = fileName( 1 : dot_pos - 1 )
         print *, "The file is a gzipped file!, w/prefix: ", prefix
         gz_file = gzopen(fileName // c_null_char, "r" // c_null_char)

         if (.not. c_associated(gz_file)) then
            print *, "Error: Could not open the gzipped file."
            stop
         end if
         gz_file_processing_loop:do
            block
               integer :: null_pos

               ! 1. Locate the C null character position
               line = repeat(c_null_char, len(line))
               res_ptr = gzgets(gz_file, line, int(len(line), c_int))
               ! If gzgets returns a null pointer, we hit EOF or an error
               if (.not. c_associated(res_ptr)) exit
               line_number = line_number + 1
               null_pos = index(line, c_null_char)
               if (null_pos > 1) then
                  ! 2. Check if the character right before '\0' is a newline (\n = ASCII 10)
                  !    Or a Windows carriage return (\r = ASCII 13)
                  do while (null_pos > 1)
                     if (ichar(line(null_pos-1:null_pos-1)) == 10 .or. &
                        ichar(line(null_pos-1:null_pos-1)) == 13) then
                        null_pos = null_pos - 1
                     else
                        exit
                     end if
                  end do

                  ! 3. Clear everything from the data end to the end of the line variable
                  line(null_pos:) = ' '
               else if (null_pos == 1) then
                  line = ' '
               end if
               if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
               if (line(1:8) == 'magscale') then
                  read(line, *, iostat=i) dummy, ASCALE, BSCALE
                  write(*,*) 'MAGIC is using scaling parameters: ', ASCALE, ' ', BSCALE
                  cycle
               end if

               ! Check for section headers
               if (line(1:2) == '<<') then
                  section_name = trim(line(4:len_trim(line)-2))
                  section_name = trim(section_name)
                  if( section_name == "labels" ) cycle
                  if( section_name == "end" ) cycle
                  write (*,'(3A10,I5)') 'Layer = ', section_name, ' = id: ', layer_count
                  call hash_put( ht, section_name, layer_count, ins )
                  if( .not. ins ) write (*,*) 'Duplicate layer seen: ', section_name
                  layerNames(layer_count) = section_name
                  layer_count = layer_count + 1
                  found_section = .true.
                  cycle
               end if

               ! Parse rectangle definitions
               if (line(1:4) == 'rect' ) then
                  exit gz_file_processing_loop
               end if
               if (line(1:4) == 'rect' .and. found_section) then
                  !write (*,*) line
                  ! Parse rectangle coordinates
                  read(line, *, iostat=i) dummy, x1, y1, x2, y2
                  call hash_get( ht, trim(section_name), layer_id, ok )
                  tempBox%x1 = x1
                  tempBox%y1 = y1
                  tempBox%x2 = x2
                  tempBox%y2 = y2
                  !call box_scale( tempBox, ASCALE, BSCALE )
                  call addBoxToLayer( layers, layer_id, tempBox )
               end if
               if (line(1:5) == 'HDF5' .and. found_section) then
                  !write (*,*) line
                  call hash_get( ht, trim(section_name), layer_id, ok )
                  ! Parse rectangle coordinates
                  section_name = trim(line(6:len_trim(line)))
                  call loadFromHDF( section_name, layers(layer_id)%layer_boxes )
                  layers(layer_id)%n_used = size( layers(layer_id)%layer_boxes )
                  layers(layer_id)%layerState = ior( layers(layer_id)%layerState, LAYER_STATE_SORT )
                  !write (*,'(A,I0,3A15,I0)') 'RL: ', layer_id, ' from HDF5: ', section_name, ' ', layers(layer_id)%n_used
                  !boxes => layers(i)%layer_boxes
                  !do j = 1, layers(i)%n_used
                  !   write(*,'(A,I,A,4I)') 'Box ', j, ': ', boxes(j)%x1, boxes(j)%y1, boxes(j)%x2, boxes(j)%y2
                  !end do
               end if
               if (line(1:4) == 'JHDF5' .and. found_section) then
                  !write (*,*) line
                  call hash_get( ht, trim(section_name), layer_id, ok )
                  ! Parse rectangle coordinates
                  section_name = trim(line(7:len_trim(line)))
                  call LoadJuliaHDF5( section_name, layers(layer_id)%layer_boxes, 5 ) ! scaling_factor set to 5
                  layers(layer_id)%n_used = size( layers(layer_id)%layer_boxes )
                  !write (*,'(A,I0,3A15,I0)') 'RL: ', layer_id, ' from JHDF5: ', section_name, ' ', layers(layer_id)%n_used
                  !boxes => layers(i)%layer_boxes
                  !do j = 1, layers(i)%n_used
                  !   write(*,'(A,I,A,4I)') 'Box ', j, ': ', boxes(j)%x1, boxes(j)%y1, boxes(j)%x2, boxes(j)%y2
                  !end do
               end if
               if (line(1:4) == 'KLBIN' .and. found_section) then
                  !write (*,*) line
                  call hash_get( ht, trim(section_name), layer_id, ok )
                  ! Parse rectangle coordinates
                  section_name = trim(line(7:len_trim(line)))
                  call LoadKLBin( section_name, layers(layer_id)%layer_boxes )
                  layers(layer_id)%n_used = size( layers(layer_id)%layer_boxes )
                  !write (*,'(A,I0,3A15,I0)') 'RL: ', layer_id, ' from KLBIN: ', section_name, ' ', layers(layer_id)%n_used
                  !boxes => layers(i)%layer_boxes
                  !do j = 1, layers(i)%n_used
                  !   write(*,'(A,I,A,4I)') 'Box ', j, ': ', boxes(j)%x1, boxes(j)%y1, boxes(j)%x2, boxes(j)%y2
                  !end do
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
            end block
         end do gz_file_processing_loop
         ! 3. Close the file handle safely
         status = gzclose(gz_file)
      else
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
               if( layer_count > 1 ) then
                  call ResizeLayer( layers, layer_count-1, layers(layer_count-1)%n_used ) !in-time compaction
               end if
               layer_count = layer_count + 1
               found_section = .true.
               cycle
            end if

            ! Parse rectangle definitions
            if (line(1:4) == 'rect' .and. found_section) then
               !write (*,*) line
               ! Parse rectangle coordinates
               read(line, *, iostat=i) dummy, x1, y1, x2, y2
               call hash_get( ht, trim(section_name), layer_id, ok )
               tempBox%x1 = x1
               tempBox%y1 = y1
               tempBox%x2 = x2
               tempBox%y2 = y2
               !call box_scale( tempBox, ASCALE, BSCALE )
               call addBoxToLayer( layers, layer_id, tempBox )
            end if

            if (line(1:4) == 'HDF5' .and. found_section) then
               !write (*,*) line
               call hash_get( ht, trim(section_name), layer_id, ok )
               ! Parse rectangle coordinates
               section_name = trim(line(6:len_trim(line)))
               call loadFromHDF( section_name, layers(layer_id)%layer_boxes )
               layers(layer_id)%n_used = size( layers(layer_id)%layer_boxes )
               layers(layer_id)%layerState = ior( layers(layer_id)%layerState, LAYER_STATE_SORT )
               layers(layer_id)%n_alloc = layers(layer_id)%n_used
               layers(layer_id)%fileName = section_name
               !write (*,'(A,I0,3A15,I0)') 'RL: ', layer_id, ' from HDF5: ', section_name, ' ', layers(layer_id)%n_used
               !boxes => layers(i)%layer_boxes
               !do j = 1, layers(i)%n_used
               !   write(*,'(A,I,A,4I)') 'Box ', j, ': ', boxes(j)%x1, boxes(j)%y1, boxes(j)%x2, boxes(j)%y2
               !end do
            end if
            if (line(1:5) == 'JHDF5' .and. found_section) then
               !write (*,*) line
               call hash_get( ht, trim(section_name), layer_id, ok )
               ! Parse rectangle coordinates
               section_name = trim(line(7:len_trim(line)))
               call LoadJuliaHDF5( section_name, layers(layer_id)%layer_boxes, 5 ) ! scaling_factor set to 5
               layers(layer_id)%n_used = size( layers(layer_id)%layer_boxes )
               layers(layer_id)%n_alloc = layers(layer_id)%n_used
               layers(layer_id)%fileName = section_name
               !write (*,'(A,I0,3A15,I0)') 'RL: ', layer_id, ' from JHDF5: ', section_name, ' ', layers(layer_id)%n_used
               !boxes => layers(i)%layer_boxes
               !do j = 1, layers(i)%n_used
               !   write(*,'(A,I,A,4I)') 'Box ', j, ': ', boxes(j)%x1, boxes(j)%y1, boxes(j)%x2, boxes(j)%y2
               !end do
            end if
            if (line(1:5) == 'KLBIN' .and. found_section) then
               !write (*,*) line
               call hash_get( ht, trim(section_name), layer_id, ok )
               ! Parse rectangle coordinates
               section_name = trim(line(7:len_trim(line)))
               if( load_design%design_direction == DESIGN_DIRECTION_INPUT ) then
                  call LoadKLBin( section_name, layers(layer_id)%layer_boxes )
                  layers(layer_id)%n_used = size( layers(layer_id)%layer_boxes )
                  layers(layer_id)%n_alloc = layers(layer_id)%n_used
                  !write(*,'(A,A8,A30)') 'Input  Request KLBIN: ', layerNames(layer_id), ' => ', section_name
               else if( load_design%design_direction == DESIGN_DIRECTION_OUTPUT ) then
                  !write(*,'(A,A10,A30)') 'Output Request KLBIN: ', trim(layerNames(layer_id)), ' => ', trim(adjustl(section_name))
               end if
               allocate( layers(layer_id)%fileName, source= trim(adjustl(section_name)))
               !write(*,*) layers(layer_id)%fileName
               !layers(layer_id)%fileName =section_name
               !write (*,'(A,I0,3A15,I0)') 'RL: ', layer_id, ' from KLBIN: ', section_name, ' ', layers(layer_id)%n_used
               !boxes => layers(i)%layer_boxes
               !do j = 1, layers(i)%n_used
               !   write(*,'(A,I,A,4I)') 'Box ', j, ': ', boxes(j)%x1, boxes(j)%y1, boxes(j)%x2, boxes(j)%y2
               !end do
            end if
            if (line(1:4) == 'SNAP' .and. found_section) then
               !write (*,*) line
               call hash_get( ht, trim(section_name), layer_id, ok )
               ! Parse rectangle coordinates
               section_name = trim(line(6:len_trim(line))) !>>> ROOKIE <<<
               if( load_design%design_direction == DESIGN_DIRECTION_INPUT ) then
                  call RestoreSnapToLayer( layers(layer_id), section_name )
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
100      continue ! this is end of file
         close(10)
         nullify(boxes)
      end if ! for .gz vs simple files

      ! Print parsed results
      write(*,*) 'Parsed ', hash_nitems(ht), ' layer(s).'
      call StopMarkTime("ReadDB")
      if( load_design%design_direction /= DESIGN_DIRECTION_INPUT ) then
         return
      end if
      call StartMarkTime("SortBuildRTree")
      call cpu_time(t1)
      call system_clock(count=start_tick)
      do i = 1, size(layers)
         !Contrary to logic this increases the peak RSS; if we want to do compaction
         !we have to do it right after a layer is "completed"
         if( layers(i)%n_used > 0 .and. layers(i)%n_used .ne. size( layers(i)%layer_boxes ) ) then
            write(*,'(A,I3,A8,A,I12)') 'Performing compaction on layer: ', i, layerNames(i), ' ', layers(i)%n_used
            call ResizeLayer( layers, i, layers(i)%n_used ) ! performs compaction
         end if
      end do
      do i = 1, size(layers)
         allocate( layers(i)%tree%tree_nodes( CalculateTotalNodes( layers(i)%n_used, K_LEAF_CAPACITY ) ) )
      end do
      !> The older method is much/much faster, so I think we should use this
#define OLD_CODE
      !#define NEW_CODE
#ifdef NEW_CODE
      !do concurrent (i = 1:MAX_LAYERS)
      !$omp parallel do
      do i = 1,size(layers)
         !$omp critical (console_io)
         write(*,'(A,A8,A,I12)') 'START Preprocess Layer ', layerNames(i), ' |N| = ', layers(i)%n_used
         !$omp end critical (console_io)
         !call PreprocessLayer( layers(i) )
         !$omp critical (console_io)
         write(*,'(A,A8,A,I12)') 'END   Preprocess Layer ', layerNames(i), ' |N| = ', layers(i)%n_used
         !$omp end critical (console_io)
      end do
#endif

#ifdef OLD_CODE
      do i = 1,size(layers)
         if( layers(i)%n_used == 0 ) cycle
         !for SDT6x6 it went from 16.8 to ~21
#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
         !call SortBoxesDirect( layers(i)%layer_boxes, layers(i)%n_used )
#else
         !call StartMarkTime("RI_SORT")
         !call SortBoxesDirect( layers(i)%layer_boxes, layers(i)%n_used ) !> 20s for SDT16_6x6_MCON (67/44)
         !call MortonSort( layers(i)%layer_boxes ) !> 7s for SDT16_6x6_MCON (67/44)
         !call StopMarkTime("RI_SORT")
#endif
         boxes => layers(i)%layer_boxes
         if( NeedsSorting( layers(i) ) ) then
            num_squares = count( is_square(boxes) )
            !write(*,*) 'Layer ', layerNames(i), ' is SQUARE dominated. ', num_squares
            if( num_squares*1.0_real64 / (layers(i)%n_used*1.0_real64) > K_SQUARE_DOMINATION_THRESHOLD ) then
               !write(*,*) 'Layer ', trim(layerNames(i)), ' is SQUARE dominated, ', num_squares, ' / ', size(boxes)
               !number_expected_interactions = CalculateOverlapCount( layers(i) ) !> this will sort on the GPU
               number_expected_interactions = 0
               if( number_expected_interactions > 10000 ) then
                  block
                     integer(kind=int64) :: original_count, thinned_count
                     original_count = layers(i)%n_used
                     write(*,'(A,A12,A,I12)') ' Layer ', trim(layerNames(i)), ' may have OVERLAP ', number_expected_interactions
                     call RemoveIdentical( layers(i) )
                     thinned_count = layers(i)%n_used
                     if( thinned_count > original_count ) then
                        error stop "ERROR: RemoveIdentical increased count"
                     else
                        write(*,*) 'RemoveIdentical decreased from: ', original_count, ' ', thinned_count
                        layers(i)%n_used = thinned_count
                     end if
                  end block
               end if
#if defined(_CUDA) || defined(__NVCOMPILER_LLVM__)
               !call MortonSort( layers(i)%layer_boxes )
               call SortBoxesDirect( layers(i)%layer_boxes, layers(i)%n_used )
#else
               call SortBoxesDirect( layers(i)%layer_boxes, layers(i)%n_used )
               !call MortonSort( layers(i)%layer_boxes )
#endif
            else
               call omt_pack( layers(i)%layer_boxes , K_LEAF_CAPACITY )
            end if
            layers(i)%layerState = ior( layers(i)%layerState, LAYER_STATE_SORT )
         end if
         call BuildRTree( layers(i)%layer_boxes, K_LEAF_CAPACITY, layers(i)%tree%tree_nodes, layers(i)%tree%root_index)
         layers(i)%layerState = ior( layers(i)%layerState, LAYER_STATE_RTREE )
         !call str_pack( boxes, K_LEAF_CAPACITY )
         !call quicksort_boxes( boxes, 1, layers(i)%n_used )
         !ok = CheckSortOrder( boxes, 1,  layers(i)%n_used )
         if( .not. ok ) then
            write(*,*) 'Sorting failed for layer: ', i, layerNames(i)
         end if
      end do
#endif
      call cpu_time(t2)
      call system_clock(count=end_tick)
      elapsed_time = real(end_tick - start_tick, kind=8) / real(clock_rate, kind=8)
      !print '(A, F12.2, A,F12.2,A)', 'Sorting/OMT completed in ', t2 - t1, ' CPU seconds.', elapsed_time, ' REAL seconds.'
      call StopMarkTime("SortBuildRTree")
      !print *, "=== number of boxes stored per layer ==="
      if( .false. ) then
         do i = 1, size(layers)
            if( layers(i)%n_used == 0 ) then
               cycle
            end if
            write(num_str,'(I0)') i
            layerFileName = prefix // '_L' // trim(num_str) // '.h5'
            call saveToHDF( layerFileName, layers(i)%layer_boxes )
         end do
      end if
      !> everything else after this is mostly informatics, except the UNION/OVLP check
      write (*,*) '+--------------------------------------------------+'
      call StartMarkTime("TreeCheckLoop")
      write (*,*) '+--------------------------------------------------+'
      write(*,'(A8,A12,A30)') 'Layer','Total','RTree CPU and REAL time'
      write (*,*) '+--------------------------------------------------+'
      tree_check_loop: do i = 1, size(layers)
         if( layers(i)%n_used == 0 ) then
            cycle
         end if
         ! 2. Record the start tick
         call system_clock(count=start_tick)
         call cpu_time(t1)
         !write(*,*) 'RT = ', size(layers(i)%tree%tree_nodes), ' ', layers(i)%tree%root_index
         !do j=1,size(layers(i)%tree%tree_nodes)
         !   write(*,*) j,' ',layers(i)%tree%tree_nodes(j)%mbr
         !end do
         !>>> UNCOMMENT <<<
         env_status = 1
         call get_environment_variable( 'MAGPARSER_CONTROL_SELFTEST_TREE', length=env_len, status=env_status )
         if( env_status == 0 ) then
            write(*,*) 'ENV MAGPARSER_CONTROL_SELFTEST_TREE is ON'
            call SelfTestTheTree( layers(i)%layer_boxes, K_LEAF_CAPACITY, layers(i)%tree%tree_nodes, layers(i)%tree%root_index )
         end if
         call cpu_time(t2)
         call system_clock(count=end_tick)
         elapsed_time = real(end_tick - start_tick, kind=8) / real(clock_rate, kind=8)
         write(*,'(A8,I12,F12.2,F12.2)') layerNames(i), layers(i)%n_used, &
            (t2-t1), elapsed_time
         boxes => layers(i)%layer_boxes
         !call ExplainTheTree( layers(i)%layer_boxes, K_LEAF_CAPACITY, layers(i)%tree%tree_nodes, layers(i)%tree%root_index )
         !awk 'NR>76262119 && NR<76401348' log_FPU.txt > m3_FPU_OMT64.txt
         !do j = 1, layers(i)%n_used
         !   write(*,'(A,I,A,4I)') 'Box ', j, ': ', boxes(j)%x1, boxes(j)%y1, boxes(j)%x2, boxes(j)%y2
         !end do
      end do tree_check_loop
      write (*,*) '+--------------------------------------------------+'
      call StopMarkTime("TreeCheckLoop")
      call StartMarkTime("PNumLoop")
      write (*,*) '+-----------------------------------------------------------------------------+'
      write(*,'(A8,3(A12),A5,A30)') 'Layer','Polygons','Rects','Total','ST','RTree CPU and REAL time'
      write (*,*) '+-----------------------------------------------------------------------------+'
      !> Polygon Number loop, this loop is parallelized inside over each polygon/box
      pnum_loop: do i = 1, size(layers)
         if( layers(i)%n_used == 0 ) then
            cycle
         end if
         ! 2. Record the start tick
         call system_clock(count=start_tick)
         call cpu_time(t1)
         call PerformMerge( layers(i)%pnumtable, layers(i)%layer_boxes, K_LEAF_CAPACITY, layers(i)%tree%tree_nodes,&
            layers(i)%tree%root_index, overlap_areas(i), overlap_perimeter(i))
         call cpu_time(t2)
         call system_clock(count=end_tick)
         elapsed_time = real(end_tick - start_tick, kind=8) / real(clock_rate, kind=8)
         num_roots = layers(i)%pnumtable%count_roots()
         num_rects = count(layers(i)%pnumtable%arr == 0)
         layers(i)%layerState = ior( layers(i)%layerState, LAYER_STATE_PNUM )
         if( overlap_areas(i) > 0.0 ) then
            !> this layer needs HEALING as we have detected overlap
            !write(*,*) 'PerformUnion as overlap detected: ', elapsed_time
            !write(*,'(A,A12,A,F20.2)') 'OVERLAP AREAS for layer: ', layerNames(i), ' = ', overlap_areas(i)
            !call PerformUnion( layers(i) )
            call PerformPolygonUnion( layers(i) ) !> this performs the Merge so no need to duplicate
            if( NeedsPNum( layers(i) ) ) then
               call PerformMerge( layers(i)%pnumtable, layers(i)%layer_boxes, K_LEAF_CAPACITY, layers(i)%tree%tree_nodes,&
                  layers(i)%tree%root_index, overlap_areas(i), overlap_perimeter(i))
               layers(i)%layerState = ior( layers(i)%layerState, LAYER_STATE_PNUM )
            end if
            num_roots = layers(i)%pnumtable%count_roots()
            num_rects = count(layers(i)%pnumtable%arr == 0)
            if( abs(overlap_areas(i)) > K_SMALL_EPSILON ) then
               write(*,'(A,I3,A8,A8,A,I12,A,I12,A,I2,A,F12.2,A,F12.2,A)') 'Layer: ', i, ' ', layerNames(i), ' has ', num_roots, &
                  ' non-rects ',num_rects , ' rects. STAT ',layers(i)%layerState, &
                  ' |RTREE| = CPU ', (t2-t1), ' secs.', elapsed_time, ' REAL secs'
               write(*,*) 'OVLP AREA = ', overlap_areas(i)
               layers(i)%layerState = iand( layers(i)%layerState, NOT(LAYER_STATE_HEAL ) )
               error stop 'UNION failed.'
            else
               layers(i)%layerState = ior( layers(i)%layerState, LAYER_STATE_HEAL )
            end if
         else
            layers(i)%layerState = ior( layers(i)%layerState, LAYER_STATE_HEAL )
         end if
         !write(*,'(A,A8,3(I12),A,I2,A,F12.2,A,F12.2,A)')
         call system_clock(count=end_tick)
         elapsed_time2 = real(end_tick - start_tick, kind=8) / real(clock_rate, kind=8)
         write(*,'(A8,3(I12),I5,F12.2,F12.2)') layerNames(i), &
            num_roots, num_rects , layers(i)%n_used, layers(i)%layerState, &
            (t2-t1), elapsed_time2
         !boxes => layers(i)%layer_boxes
         !call ExplainTheTree( layers(i)%layer_boxes, K_LEAF_CAPACITY, layers(i)%tree%tree_nodes, layers(i)%tree%root_index )
         !awk 'NR>76262119 && NR<76401348' log_FPU.txt > m3_FPU_OMT64.txt
         !do j = 1, layers(i)%n_used
         !   write(*,'(A,I,A,4I)') 'Box ', j, ': ', boxes(j)%x1, boxes(j)%y1, boxes(j)%x2, boxes(j)%y2
         !end do
      end do pnum_loop
      write (*,*) '+-----------------------------------------------------------------------------+'
      call StopMarkTime("PNumLoop")
      call StartMarkTime("ExtentLoop")
      do concurrent (i = 1:MAX_LAYERS)
         !do i = 1,size(layers)
         if( layers(i)%n_used == 0 ) then
            cycle
         end if
         extents(i) = mbr_of_array( layers(i)%layer_boxes, layers(i)%n_used )
         DESIGN_EXTENT = DESIGN_EXTENT + extents(i)
         layers(i)%area = 0.0
         if( .not. NeedsHealing( layers(i) ) ) then
            do j = 1, layers(i)%n_used
               layers(i)%area = layers(i)%area + box_area( layers(i)%layer_boxes(j) )
               layers(i)%perimeter = layers(i)%perimeter + box_perimeter( layers(i)%layer_boxes(j) )
            end do
            area_by_union = calculate_union_area_by_polygon( layers(i) )
            if( layers(i)%area /= area_by_union ) then
               write(*,*) 'Layer ',i, 'Layer State ', layers(i)%layerState
               write(*,*) 'Union Area by SCANLINE: ', area_by_union, ' OVLP: ', layers(i)%area
               error stop
            end if
            layers(i)%perimeter = layers(i)%perimeter - overlap_perimeter(i)
            if( layers(i)%perimeter < 0.0 ) error stop "INCONSISTENT PERIMETER detected."
            !write(*,*) 'AREA = ', layers(i)%area
         else
            layers(i)%area = calculate_union_area_by_polygon( layers(i) )
            !write(*,*) 'AREA NOT COMPUTABLE = ', layers(i)%area
         end if
         !end do
         !write(*,'(A,4I)') 'Box: ',extents(i)%x1, extents(i)%y1, extents(i)%x2, extents(i)%y2
         !extents(i) = mbr_of_array( boxes, layers(i)%n_used )
         !write(*,'(A,4I)') 'Box: ', extents(i)%x1, extents(i)%y1, extents(i)%x2, extents(i)%y2
         !DESIGN_EXTENT = DESIGN_EXTENT + extents(i)
      end do
      if( .not. DESIGN_EXTENT%is_valid() ) then
         error stop 'Design EXTENT is not valid'
      end if
      call StopMarkTime("ExtentLoop")
      write (*,*) '+-----------------------------------------------------------------+'
      write (*,*) '+                Layer Areas and Perimeters                       +'
      write (*,*) '+-----------------------------------------------------------------+'
      do i = 1, size(layers)
         if( extents(i)%is_valid() ) then
            write(*,'(A1,A12,F18.8,A1,F18.8)') ' ',layerNames(i), layers(i)%area*1e-6, ' ', layers(i)%perimeter*1e-6
         end if
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

   subroutine ResizeLayer(layers,layer_id, newSize)
      type(Layer), intent(inout) :: layers(:)
      integer, intent(in) :: layer_id
      integer(kind=8), intent(in)  :: newSize
      type(Box), allocatable :: tmp(:)
      allocate(tmp(newSize))
      if (layers(layer_id)%n_used > 0) tmp(1:layers(layer_id)%n_used) = layers(layer_id)%layer_boxes(1:layers(layer_id)%n_used)   ! copy old data
      call move_alloc(from=tmp, to=layers(layer_id)%layer_boxes)   ! replace the old array
      layers(layer_id)%n_alloc = newSize
   end subroutine ResizeLayer

   subroutine addBoxToLayer( layers, layer_id, tempBox )
      type(Layer), intent(inout), target :: layers(:)
      integer, intent(in) :: layer_id
      type(Box), intent(in) :: tempBox
      type(Layer), pointer :: l
      integer(kind=8) :: newSize
      if( CheckBox( tempBox ) ) then
         stop "ERROR: box in valid"
      end if
      l => layers( layer_id )
      if (l%n_used == l%n_alloc) then                ! buffer full → grow
         newSize = max(1_8, l%n_alloc*2)                ! double the size
         call ResizeLayer(layers, layer_id, newSize)
      end if
      l%n_used = l%n_used + 1
      l%layer_boxes(l%n_used) = tempBox
      !write (*,*) 'Reading box into lid: ', layer_id, ' |x| = ', l%n_used
   end subroutine addBoxToLayer

   !> support for writing VLSI Magic format with binary payload of geometry data
   subroutine writeMagicLayoutFile(load_design)
      ! Write Magic VLSI layout files with component sections and rectangle definitions
      type(Design), intent(inout), target :: load_design
      integer :: i, pos, deleted_layer_count
      if( load_design%design_direction /= DESIGN_DIRECTION_OUTPUT ) then
         error stop "Non-output DB being written out" !> I may relax this, but not good idea
      end if
      if( debug_verbosity > 1 ) then
         write(*,*) 'Writing DB: ', load_design%fileName
      end if
      deleted_layer_count = 0
      if( .not. allocated( load_design%layers ) ) return !> we have already flushed this db
      do i=1,size(load_design%layers)
         if( load_design%layers(i)%n_used == 0 ) cycle
         !> Lets just assume we are writing in KLBIN
         if(.not. allocated( load_design%layers(i)%fileName ) ) then
            error stop "ERROR: layer backing store name not allocated"
         end if
         pos = index( load_design%layers(i)%fileName, ".snap" )
         !write(*,*) 'IS_SNAP ',pos,' ',load_design%layers(i)%fileName
         if( pos == len_trim(load_design%layers(i)%fileName)-4 ) then
            call SaveLayerToSnap( load_design%layers(i), load_design%layers(i)%fileName )
            call ClearLayer( load_design%layers(i) )
            deleted_layer_count = deleted_layer_count + 1
            cycle
         end if
         pos = index( load_design%layers(i)%fileName, ".bin" )
         !write(*,*) 'pos = ', pos, ' len = ', len_trim(load_design%layers(i)%fileName)
         if( pos /= len_trim(load_design%layers(i)%fileName)-3 ) then
            write(*,*) 'ERROR: File format not supported: ', load_design%layers(i)%fileName
            error stop "DBOUT only supports KLBIN, edit the output MAG file to fix"
         end if
         call WriteKLBin( load_design%layers(i)%fileName, load_design%layers(i)%layer_boxes, load_design%layers(i)%n_used )
         !> as an memory optimization we are going to delete the layer after a flush
         !> if this layer is reused later, that will be a problem, so let us deliberately let it crash for now
         call ClearLayer( load_design%layers(i) )
         deleted_layer_count = deleted_layer_count + 1
      end do
      if( debug_verbosity > 1 ) then
         write(*,*) 'Written and delete : ', deleted_layer_count, ' layers.'
      end if
   end subroutine writeMagicLayoutFile
end module MagicVLSILayoutParser
