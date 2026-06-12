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

! program mag_parser
!   use MagicVLSILayoutParser
!   implicit none  
!   ! Modern Fort parser for INV.mag file format
!   call parseMagicLayoutFile("INV.mag",10)
! end program mag_parser

!=====================================================================
!  mag_parser.f90
!  Simple driver that calls the MagicVLSILayoutParser module.
!  It accepts the input file name and the maximum number of layers
!  as command‑line arguments.
!=====================================================================
program mag_parser
  use MagicVLSILayoutParser               ! <-- your parser module
  use DesignModule
  use ControlModule
  use version_mod, only: commit_hash
  implicit none

  !-----------------------------------------------------------------
  !  Local variables
  !-----------------------------------------------------------------
  character(len=256)            :: filename      ! name of the .mag file
  integer                       :: maxLayers     ! maximum layer count
  integer                       :: narg          ! # of arguments on the command line
  integer                       :: iostat        ! I/O status for reading the integer argument
  character(len=256)            :: arg_string    ! temporary buffer for the 2nd argument
  character(len=*), parameter   :: default_file = "INV.mag"
  integer,          parameter   :: default_max  = 10
  type(Design)                  :: load_design

  write(*,*) 'MAGPARSER.exe by Sandeep Koranne (C) All rights reserved.'
  write(*,*) 'Released under MIT License. See the README and LICENSE '
  write(*,*) 'https://github.com/skoranne/VLSI_LAYOUT/'
  write(*,*) 'Release: ', commit_hash
  !-----------------------------------------------------------------
  !  Get the number of arguments supplied on the command line
  !-----------------------------------------------------------------
  narg = command_argument_count()

  select case (narg)
  case (0)                     ! No arguments → use defaults
     filename = default_file
     maxLayers = default_max

  case (2)                     ! Two arguments: <file> <maxLayers>
     ! ---- first argument: file name ---------------------------------
     call get_command_argument(1, filename, status=iostat)   ! allocates automatically
     if (iostat /= 0) then
        write (*,*) "ERROR: 1st argument must be a filename."
        stop 2
     end if
     write (*,*) 'Reading filename: ', trim(filename)
     ! ---- second argument: integer (max number of layers) ----------
     call get_command_argument(2, arg_string, status=iostat)
     if (iostat /= 0) then
        write (*,*) "ERROR: 2nd argument must be an integer."
        stop 2
     end if
     read (arg_string, *, iostat=iostat) maxLayers
     if (iostat /= 0 .or. maxLayers < 0) then
        write (*,*) "ERROR: maxLayers must be a non‑negative integer."
        stop 3
     end if

  case default                ! Anything else → print usage and quit
     write (*,*) "Usage: mag_parser [file_name max_layers]"
     write (*,*) "  file_name   : name of the Magic .mag file (default = '"// &
          trim(default_file)//"')"
     write (*,*) "  max_layers  : maximum number of layers to read (default = ", &
          default_max, ")"
     stop 1
  end select

  !-----------------------------------------------------------------
  !  Call the actual parser (the routine you already have)
  !-----------------------------------------------------------------
  !call parseMagicLayoutFile(load_design, trim(filename), maxLayers)
  call ParseControlFile( trim(filename), maxLayers )

  !-----------------------------------------------------------------
  !  Normal program termination
  !-----------------------------------------------------------------
  stop
end program mag_parser
