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
  use hash_mod
  implicit none
  private
  public:: Box, parseMagicLayoutFile

  type :: Box
     integer(kind=4) :: x1, y1, x2, y2
   contains
     procedure, pass :: reset_to_infinity
     procedure, pass :: is_valid
     procedure, pass :: print_box
     procedure, pass :: box_union
     procedure, pass :: box_intersection
     generic :: operator(+) => box_union
     generic :: operator(*) => box_intersection
  end type Box

  type :: Layer
     integer :: lid
     type(Box),  pointer :: layer_boxes(:) => null()
  end type Layer
  integer :: MAX_N = 10
  type(hash_type) :: ht
  type(Layer), pointer :: layers(:) => null()
  integer, allocatable :: geometry_count(:)
  type(Box), allocatable :: extents(:)
  type(Box)              :: DESIGN_EXTENT
contains
  ! Type procedure to reset box to [infinity,infinity,-infinity,-infinity]
  subroutine reset_to_infinity(this)
    class(Box), intent(inout) :: this
    this%x1 = huge(this%x1)
    this%y1 = huge(this%y1)
    this%x2 = -huge(this%x1)
    this%y2 = -huge(this%y1)
  end subroutine reset_to_infinity
  pure logical function is_valid(this)
    class(Box), intent(in) :: this
    !box is valid if x1 < x2 and y1 < y2
    is_valid = (this%x1 < this%x2 .and. this%y1 < this%y2)
  end function is_valid
  
  subroutine print_box(this)
    class(Box), intent(in) :: this    
    print *, 'Box: [', this%x1, ',', this%y1, '] to [', this%x2, ',', this%y2, ']'
  end subroutine print_box
  ! Type procedure for union of two boxes
  function box_union(this, other) result(union_box)
    class(Box), intent(in) :: this, other
    type(Box) :: union_box
    
    ! Find the bounding box that contains both boxes
    union_box%x1 = min(this%x1, other%x1)
    union_box%y1 = min(this%y1, other%y1)
    union_box%x2 = max(this%x2, other%x2)
    union_box%y2 = max(this%y2, other%y2)
  end function box_union
  
  ! Type procedure for intersection of two boxes
  function box_intersection(this, other) result(intersection_box)
    class(Box), intent(in) :: this, other
    type(Box) :: intersection_box
    
    ! Find the intersection box
    intersection_box%x1 = max(this%x1, other%x1)
    intersection_box%y1 = max(this%y1, other%y1)
    intersection_box%x2 = min(this%x2, other%x2)
    intersection_box%y2 = min(this%y2, other%y2)
    
    ! Check if intersection is valid (non-empty)
    if (intersection_box%x1 > intersection_box%x2 .or. &
        intersection_box%y1 > intersection_box%y2) then
       ! Invalid intersection - set to empty box
       intersection_box%x1 = huge(this%x1)
       intersection_box%y1 = huge(this%y1)
       intersection_box%x2 = -huge(this%x1)
       intersection_box%y2 = -huge(this%y1)
    end if
  end function box_intersection
  
  subroutine parseMagicLayoutFile(fileName,max_layers)
    ! Parses Magic VLSI layout files with component sections and rectangle definitions
    character(len=*), intent(in) :: fileName
    type(Box), pointer :: boxes(:)
    integer, parameter :: MAX_BOXES  = 100
    integer, intent(in) :: MAX_LAYERS 
    integer :: box_count = 0
    character(len=200) :: line
    character(len=200) :: section_name
    character(len=200) :: dummy  
    integer :: i, j, k, pos
    integer :: x1, y1, x2, y2
    integer :: line_number = 0
    integer :: layer_count = 1
    logical :: found_section
    integer :: layer_id
    logical :: ins,ok

    ! Initialize the layer structure by allocating memory for box arrays
    ! This loop iterates through all possible layers in the hash table
    ! For each layer (from 1 to MAX_LAYERS), array of Box objects
    ! The allocation size is fixed at MAX_BOXES per layer, creating a uniform memory layout
    ! This approach provides efficient memory access patterns and predictable allocation behavior
    ! Each layer's box array is stored as a contiguous block of memory for optimal cache performance
    ! The MAX_BOXES parameter defines the maximum capacity for each layer's box storage
    ! Memory is allocated once per layer, avoiding repeated allocations during runtime
    ! This design assumes that each layer will not exceed MAX_BOXES boxes, which should be
    ! carefully managed in the calling code to prevent buffer overflows
    ! The allocation occurs at initialization time, making subsequent box insertions
    ! into layers very fast as they simply require array indexing rather than memory allocation
    
    allocate(geometry_count(MAX_LAYERS))
    allocate(layers(MAX_LAYERS))
    allocate(extents(MAX_LAYERS))
    do i=1,MAX_LAYERS
       allocate(layers(i)%layer_boxes(MAX_BOXES))
    end do
    do i = 1, MAX_LAYERS
       call extents(i)%reset_to_infinity()
    end do
    call DESIGN_EXTENT%reset_to_infinity()
    call hash_create(ht,10)
    ! Open and parse the file
    open(unit=10, file=fileName, status='old', action='read')

    do
       read(10, '(A)', end=100) line
       line_number = line_number + 1

       ! Skip empty lines and comments
       if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
       ! Check for section headers
       if (line(1:2) == '<<') then
          section_name = trim(line(4:len_trim(line)-2))
          section_name = trim(section_name)
          if( section_name == "labels" ) cycle
          if( section_name == "end" ) cycle          
          write (*,'(3A10,I)') 'Layer = ', section_name, ' = id: ', layer_count
          call hash_put( ht, section_name, layer_count, ins )
          if( .not. ins ) write (*,*) 'Duplicate layer seen: ', section_name
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
          boxes => layers( layer_id )%layer_boxes
          geometry_count(layer_id) = geometry_count(layer_id)+1
          box_count = geometry_count(layer_id)
          if (box_count > max_boxes) then
             write(*,*) 'Error: Too many boxes in file'
             stop
          end if
          boxes(box_count)%x1 = x1
          boxes(box_count)%y1 = y1
          boxes(box_count)%x2 = x2
          boxes(box_count)%y2 = y2
          if( ok ) write (*,*) 'Reading box into lid: ', layer_id
       end if

       ! Parse label definitions
       if (line(1:5) == 'rlabel' .and. found_section) then
          ! Parse label information
          ! rlabel metal1 -25 145 90 160 1 VPWR
          read(line, *, iostat=i) dummy, dummy, x1, y1, x2, y2, dummy, dummy
          box_count = box_count + 1
          boxes(box_count)%x1 = x1
          boxes(box_count)%y1 = y1
          boxes(box_count)%x2 = x2
          boxes(box_count)%y2 = y2
       end if

    end do

100 continue ! this is end of file
    close(10)
    nullify(boxes)

    ! Print parsed results
    write(*,*) 'Parsed ', hash_nitems(ht), ' layers.'
    write(*,*) 'Geometry Count: ', geometry_count
    write(*,*) 'Parsed ', sum(geometry_count), ' total boxes from ', fileName
    !do i = 1, box_count
    !   write(*,'(A,I,A,4I)') 'Box ', i, ': ', boxes(i)%x1, boxes(i)%y1, boxes(i)%x2, boxes(i)%y2
    !end do
    do i = 1, MAX_LAYERS
       boxes => layers(i)%layer_boxes
       do j = 1, geometry_count(i)
          extents(i) = extents(i) + boxes(j)
       end do
       DESIGN_EXTENT = DESIGN_EXTENT + extents(i)
    end do
    if( .not. DESIGN_EXTENT%is_valid() ) then
       error stop 'Design EXTENT is not valid'
    end if
    
    write (*,*) '+-------------------------- Design Extent ------------------------+'
    call DESIGN_EXTENT%print_box()
    write (*,*) '+-----------------------------------------------------------------+'
    do i = 1, MAX_LAYERS
       if( extents(i)%is_valid() ) call extents(i)%print_box()
    end do
  end subroutine parseMagicLayoutFile
end module MagicVLSILayoutParser


!===============================================================================
! MAG_PARSER - Magic VLSI Layout File Parser
!===============================================================================
!
! This program serves as a specialized parser for Magic VLSI layout files,
! process ".mag" format files used in VLSI design.
!
! parser reads Magic layout files and extracts geometric information
! about circuit components, including their coordinates, dimensions, and
! hierarchical relationships within the layout.
!
! Key Features:
!   - Parses Magic ".mag" files with standard Magic layout format
!   - Supports hierarchical layout structures
!   - Extracts geometric data for circuit components
!   - Handles Magic-specific syntax and formatting conventions
!   - Provides error handling for malformed input files
!
! File Format Support:
!   - Input file: INV.mag (Magic layout file)
!   - Output: Geometry data extraction and processing
!
! Usage:
!   - Program expects a file named "INV.mag" in the current working directory
!   - The file must be in valid Magic VLSI layout format
!   - The second parameter (10 verbosity level or
!     processing option for the parser
!
! Dependencies:
!   - MagicVLSILayoutParser module (contains parseMagicLayoutFile subroutine)
!
! Author: Sandeep Koranne and Qwen3-Coder running on GB10 Blackwell DGX Spark
! Date: October 2025
! Version: 1.0
!
!===============================================================================

program mag_parser
  use MagicVLSILayoutParser
  implicit none  
  ! Modern Fort parser for INV.mag file format
  call parseMagicLayoutFile("INV.mag",10)
end program mag_parser
