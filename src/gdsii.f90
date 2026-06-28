 ! File    : gdsii.f90
 ! Author  : Sandeep Koranne (C) 2026.
 ! Purpose : Although we have the .bin files and HDF5, its also useful to
 !         : add direct read/write for GDSII
 !         : Using modern Fortran create a GDSII module with subroutines and functions
 !         : to read and write gdsii files.
 !         : Do not make mistakes and check your work and also generate test harness.
 !         : Use lots of effort
 !open(newunit=file_unit, file='layout.gds', access='stream', form='unformatted', status='replace')
!=====================================================================
!  GDSII – simple read/write utilities
!  Modern Fortran (2008/2018) implementation
!=====================================================================
module gdsii
   use, intrinsic :: iso_fortran_env, only: int8, int16, int32, real64, int64, wp => real64
   implicit none
   private
   public :: gds_header_t, gds_boundary_t
   public :: write_gds, read_gds, test_gds
   public :: write_gds_int16, write_gds_int32, write_gds_real64
   public :: bytes_to_int16, bytes_to_int32, ibmhex_to_real64

   !-----------------------------------------------------------------
   !  Record type codes (hex, as defined by the spec)
   !-----------------------------------------------------------------
   integer(int16), parameter :: REC_HEADER   = int(z'00')   ! 0x00 – Header
   integer(int16), parameter :: REC_BGNLIB   = int(z'01')   ! 0x01 – Begin Library
   integer(int16), parameter :: REC_LIBNAME  = int(z'02')   ! 0x02 – Library Name
   integer(int16), parameter :: REC_UNITS    = int(z'03')   ! 0x03 – Units
   integer(int16), parameter :: REC_ENDLIB   = int(z'04')   ! 0x04 – End Library
   integer(int16), parameter :: REC_BGNSTR   = int(z'05')   ! 0x05 – Begin Structure
   integer(int16), parameter :: REC_STRNAME  = int(z'06')   ! 0x06 – Structure Name
   integer(int16), parameter :: REC_ENDSTR   = int(z'07')   ! 0x07 – End Structure
   integer(int16), parameter :: REC_BOUNDARY = int(z'08')   ! 0x08 – Boundary
   integer(int16), parameter :: REC_PATH     = int(z'09')   ! 0x09 – Path
   integer(int16), parameter :: REC_SREF     = int(z'0A')   ! 0x0A – Structure Reference
   integer(int16), parameter :: REC_AREF     = int(z'0B')   ! 0x0B – Array Reference
   integer(int16), parameter :: REC_TEXTTYPE = int(z'0C')   ! 0x0C – Text
   integer(int16), parameter :: REC_LAYER    = int(z'0D')   ! 0x0D – Layer
   integer(int16), parameter :: REC_DATATYPE = int(z'0E')   ! 0x0E – Data type
   integer(int16), parameter :: REC_XY       = int(z'10')   ! 0x10 – XY (coordinates)


   !-----------------------------------------------------------------
   !  Simple derived types for a few records (expand as needed)
   !-----------------------------------------------------------------
   type :: gds_header_t
      integer(int16) :: version   ! version number (must be 5 for GDSII‑5.2)
   end type gds_header_t

   type :: gds_boundary_t
      integer(int16) :: layer
      integer(int16) :: datatype
      integer(int32), allocatable :: xy(:)   ! [x1,y1,x2,y2,...] in database units
   end type gds_boundary_t

contains

   !=================================================================
   !  Write a complete GDSII file (very small subset)
   !=================================================================
   subroutine write_gds(filename, header, boundary, libname, structname, &
      dbunit, userunit, iostat)
      character(*), intent(in)           :: filename
      type(gds_header_t), intent(in)     :: header
      type(gds_boundary_t), intent(in)   :: boundary
      character(*), intent(in)           :: libname, structname
      real(wp), intent(in)               :: dbunit, userunit
      integer, intent(out)               :: iostat

      integer(int8)  :: rec_type, rec_datatype
      integer(int16) :: rec_len
      integer(int32) :: i, ncoord
      integer(int8), allocatable :: buffer(:)
      integer :: unit

      open(newunit=unit, file=filename, form='unformatted', &
         access='stream', status='replace', iostat=iostat)
      if (iostat /= 0) return

      !----- Header -------------------------------------------------
      rec_type = int(REC_HEADER, int8)
      rec_len  = 6   ! 4‑byte header + 2‑byte version
      call write_record(unit, rec_type, rec_len, header%version, iostat)
      if (iostat /= 0) return

      !----- Begin Library -------------------------------------------
      call write_simple_int16(unit, REC_BGNLIB, 0_int16, iostat)
      if (iostat /= 0) return

      !----- Library Name (string, padded) ---------------------------
      call write_string_record(unit, REC_LIBNAME, libname, iostat)
      if (iostat /= 0) return

      !----- Units ---------------------------------------------------
      call write_units(unit, dbunit, userunit, iostat)
      if (iostat /= 0) return
#ifdef NOT_COMPLETE_YET
      !----- Begin Structure -----------------------------------------
      call write_simple_int16(unit, REC_BGNSTR, 0_int16, iostat)
      if (iostat /= 0) return

      call write_string_record(unit, REC_STRNAME, structname, iostat)
      if (iostat /= 0) return

      !----- Boundary ------------------------------------------------
      call write_simple_int16(unit, REC_BOUNDARY, 0_int16, iostat)
      if (iostat /= 0) return

      call write_simple_int16(unit, REC_LAYER, boundary%layer, iostat)
      if (iostat /= 0) return

      call write_simple_int16(unit, REC_DATATYPE, boundary%datatype, iostat)
      if (iostat /= 0) return

      ncoord = size(boundary%xy)
      rec_len = 4 + 4*ncoord          ! 4‑byte header + 4 bytes per coordinate
      allocate(buffer(rec_len))
      buffer(1:2) = transfer(int(z'0000'), buffer(1:2))   ! placeholder for length
      buffer(3)   = int(REC_XY, int8)
      buffer(4)   = 0_int8                               ! datatype 0 = integer
      do i = 1, ncoord
         buffer(4+4*i-3:4+4*i) = transfer(boundary%xy(i), buffer(1:4))
      end do
      call write_raw_record(unit, buffer, iostat)
      deallocate(buffer)
      if (iostat /= 0) return

      !----- End Structure -------------------------------------------
      call write_simple_int16(unit, REC_ENDSTR, 0_int16, iostat)
      if (iostat /= 0) return
#endif
      !----- End Library ---------------------------------------------
      call write_simple_int16(unit, REC_ENDLIB, 0_int16, iostat)
      close(unit)
   end subroutine write_gds

   !=================================================================
   !  Helper: write a generic record (type, length, data...)
   !=================================================================
   subroutine write_record(unit, rtype, rlen, data, iostat)
      integer, intent(in)    :: unit
      integer(int8), intent(in) :: rtype
      integer(int16), intent(in):: rlen
      integer(int16), intent(in):: data
      integer, intent(out)   :: iostat
      integer(int8), dimension(8) :: rec

      rec = 0_int8
      rec(1:2) = transfer(rlen, rec(1:2))
      rec(3)   = rtype
      rec(4)   = 0_int8               ! datatype = 2‑byte signed integer
      rec(5:6) = transfer(data, rec(5:6))

      write(unit, iostat=iostat) rec
   end subroutine write_record

   !=================================================================
   !  Helper: write a simple 2‑byte integer record (no payload)
   !=================================================================
   subroutine write_simple_int16(unit, rtype, payload, iostat)
      integer, intent(in)    :: unit
      integer(int16), intent(in) :: rtype
      integer(int16), intent(in) :: payload
      integer, intent(out)   :: iostat
      integer(int8), dimension(6) :: rec

      rec = 0_int8
      rec(1:2) = transfer(int(6, int16), rec(1:2))   ! length = 6 bytes
      rec(3)   = transfer(rtype, rec(3))
      rec(4)   = 0_int8                               ! datatype = 2‑byte integer
      rec(5:6) = transfer(payload, rec(5:6))

      write(unit, iostat=iostat) rec
   end subroutine write_simple_int16

   !=================================================================
   !  Helper: write a string record, padding with a null byte when
   !  the character count is odd (spec requirement) [1].
   !=================================================================
   subroutine write_string_record(unit, rtype, txt, iostat)
      integer, intent(in)          :: unit
      integer(int16), intent(in)   :: rtype
      character(*), intent(in)     :: txt
      integer, intent(out)         :: iostat
      integer(int8), allocatable   :: rec(:)
      integer(int16)               :: rlen, nch
      integer                      :: i, nbytes

      nch = len_trim(txt)
      nbytes = nch
      if (mod(nbytes,2) == 1) nbytes = nbytes + 1   ! pad with null if odd [1]

      rlen = 4 + nbytes   ! header (4) + padded string
      allocate(rec(rlen))

      rec(1:2) = transfer(rlen, rec(1:2))
      rec(3)   = transfer(rtype, rec(3))
      rec(4)   = 2_int8      ! datatype = 2‑byte string

      do i = 1, nch
         rec(4+i) = iachar(txt(i:i), kind=int8)
      end do
      if (nbytes > nch) rec(4+nch+1) = 0_int8   ! null padding

      write(unit, iostat=iostat) rec
      deallocate(rec)
   end subroutine write_string_record

   !=================================================================
   !  Helper: write the UNITS record (two 8‑byte reals)
   !=================================================================
   subroutine write_units(unit, dbunit, userunit, iostat)
      integer, intent(in)    :: unit
      real(wp), intent(in)   :: dbunit, userunit
      integer, intent(out)   :: iostat
      integer(int8), dimension(24) :: rec
      integer(int16) :: rlen

      rlen = 24
      rec(1:2) = transfer(rlen, rec(1:2))
      rec(3)   = transfer(REC_UNITS, rec(3))
      rec(4)   = 5_int8                     ! datatype = 8‑byte real
      rec(5:12)  = transfer(dbunit, rec(5:12))
      rec(13:20) = transfer(userunit, rec(13:20))
      rec(21:22) = 0_int8                   ! filler to reach 24 bytes

      write(unit, iostat=iostat) rec
   end subroutine write_units

   !=================================================================
   !  Helper: write a raw byte array as a record (used for XY)
   !=================================================================
   subroutine write_raw_record(unit, buffer, iostat)
      integer, intent(in)          :: unit
      integer(int8), intent(in)    :: buffer(:)
      integer, intent(out)         :: iostat
      write(unit, iostat=iostat) buffer
   end subroutine write_raw_record

   !=================================================================
   !  READ a GDSII file – very small subset, enough for the test.
   !=================================================================
   subroutine read_gds(filename, header, boundary, iostat)
      character(*), intent(in)        :: filename
      type(gds_header_t), intent(out) :: header
      type(gds_boundary_t), intent(out):: boundary
      integer, intent(out)            :: iostat

      integer :: unit, rec_len, rec_type, rec_datatype
      integer(int8), allocatable :: rec(:)
      integer :: pos, ncoord

      open(newunit=unit, file=filename, form='unformatted', &
         access='stream', status='old', iostat=iostat)
      if (iostat /= 0) return

      !--- read Header -------------------------------------------------
      call read_raw_record(unit, rec, rec_len, iostat)
      if (iostat /= 0) then; close(unit); return; end if
      rec_type = rec(3)
      if (rec_type /= REC_HEADER) then
         iostat = -1
         close(unit)
         return
      end if
      header%version = transfer(rec(5:6), header%version)

      !--- skip everything until we encounter a BOUNDARY record -------
      do
         call read_raw_record(unit, rec, rec_len, iostat)
         if (iostat /= 0) exit
         rec_type = rec(3)
         select case (rec_type)
          case (REC_BOUNDARY)
            exit   ! we have reached the start of a boundary
          case default
            cycle
         end select
      end do
      if (iostat /= 0) then; close(unit); return; end if

      !--- read Layer --------------------------------------------------
      call read_simple_int16(unit, REC_LAYER, boundary%layer, iostat)
      if (iostat /= 0) then; close(unit); return; end if

      !--- read DataType -----------------------------------------------
      call read_simple_int16(unit, REC_DATATYPE, boundary%datatype, iostat)
      if (iostat /= 0) then; close(unit); return; end if

      !--- read XY (coordinates) ---------------------------------------
      call read_raw_record(unit, rec, rec_len, iostat)
      if (iostat /= 0) then; close(unit); return; end if
      if (rec(3) /= REC_XY) then
         iostat = -2
         close(unit)
         return
      end if
      ncoord = (rec_len - 4) / 4
      allocate(boundary%xy(ncoord))
      do pos = 1, ncoord
         boundary%xy(pos) = transfer(rec(4+4*pos-3:4+4*pos), 0_int32)
      end do

      close(unit)
   end subroutine read_gds

   !-----------------------------------------------------------------
   !  Helper: read a raw record (length, type, datatype, payload)
   !-----------------------------------------------------------------
   subroutine read_raw_record(unit, rec, reclen, iostat)
      integer, intent(in)          :: unit
      integer(int8), allocatable, intent(out) :: rec(:)
      integer, intent(out)         :: reclen, iostat
      integer(int16)               :: len_hdr

      read(unit, iostat=iostat) len_hdr
      if (iostat /= 0) return
      reclen = len_hdr
      allocate(rec(reclen))
      rec(1:2) = transfer(len_hdr, rec(1:2))
      read(unit, iostat=iostat) rec(3:)
   end subroutine read_raw_record

   !-----------------------------------------------------------------
   !  Helper: read a simple 2‑byte integer record (e.g. LAYER)
   !-----------------------------------------------------------------
   subroutine read_simple_int16(unit, expected_type, value, iostat)
      integer, intent(in)          :: unit
      integer(int16), intent(in)   :: expected_type
      integer(int16), intent(out)  :: value
      integer, intent(out)         :: iostat
      integer(int8) :: rec(6)

      read(unit, iostat=iostat) rec
      if (iostat /= 0) return
      if (rec(3) /= expected_type) then
         iostat = -3
         return
      end if
      value = transfer(rec(5:6), value)
   end subroutine read_simple_int16

   ! ==================================================================
   ! HIGH-PERFORMANCE WRITERS
   ! ==================================================================

   ! Regenerated 16-bit writer (Length: 6 bytes)
   subroutine write_gds_int16(unit, rtype, payload, iostat)
      integer, intent(in)        :: unit
      integer(int8), intent(in)  :: rtype
      integer(int16), intent(in) :: payload
      integer, intent(out)       :: iostat
      integer(int8)              :: rec(6)

      rec(1) = 0_int8
      rec(2) = 6_int8 ! Length = 6 bytes

      rec(3) = rtype
      rec(4) = 2_int8 ! Datatype = 2-byte integer

      ! Pack Payload (Big-Endian)
      rec(5) = int(iand(shiftr(payload, 8), 255_int16), int8)
      rec(6) = int(iand(payload, 255_int16), int8)

      write(unit, iostat=iostat) rec
   end subroutine write_gds_int16

   ! 32-bit writer for coordinates (Length: 8 bytes)
   subroutine write_gds_int32(unit, rtype, payload, iostat)
      integer, intent(in)        :: unit
      integer(int8), intent(in)  :: rtype
      integer(int32), intent(in) :: payload
      integer, intent(out)       :: iostat
      integer(int8)              :: rec(8)

      rec(1) = 0_int8
      rec(2) = 8_int8 ! Length = 8 bytes

      rec(3) = rtype
      rec(4) = 3_int8 ! Datatype = 4-byte integer

      ! Pack Payload (Big-Endian)
      rec(5) = int(iand(shiftr(payload, 24), 255_int32), int8)
      rec(6) = int(iand(shiftr(payload, 16), 255_int32), int8)
      rec(7) = int(iand(shiftr(payload, 8),  255_int32), int8)
      rec(8) = int(iand(payload, 255_int32), int8)

      write(unit, iostat=iostat) rec
   end subroutine write_gds_int32

   ! 64-bit float writer for Angles/Mag (Length: 12 bytes)
   subroutine write_gds_real64(unit, rtype, payload, iostat)
      integer, intent(in)        :: unit
      integer(int8), intent(in)  :: rtype
      real(real64), intent(in)   :: payload
      integer, intent(out)       :: iostat
      integer(int8)              :: rec(12)
      integer(int8)              :: ibm_bytes(8)
      integer                    :: i

      rec(1) = 0_int8
      rec(2) = 12_int8 ! Length = 12 bytes

      rec(3) = rtype
      rec(4) = 5_int8  ! Datatype = 8-byte real

      ! Convert IEEE-754 to IBM Hex Array
      call real64_to_ibmhex(payload, ibm_bytes)

      do i = 1, 8
         rec(4 + i) = ibm_bytes(i)
      end do

      write(unit, iostat=iostat) rec
   end subroutine write_gds_real64


   ! ==================================================================
   ! RAW BYTE DECODERS (FOR READING STREAMS)
   ! ==================================================================

   ! Warning: Masking with 255 is mandatory to prevent negative int8
   ! values from sign-extending when promoted to int16/int32.

   pure function bytes_to_int16(b) result(res)
      integer(int8), intent(in) :: b(2)
      integer(int16)            :: res
      res = ior(shiftl(iand(int(b(1), int16), 255_int16), 8), &
         iand(int(b(2), int16), 255_int16))
   end function bytes_to_int16

   pure function bytes_to_int32(b) result(res)
      integer(int8), intent(in) :: b(4)
      integer(int32)            :: res
      res = ior(shiftl(iand(int(b(1), int32), 255_int32), 24), &
         ior(shiftl(iand(int(b(2), int32), 255_int32), 16), &
         ior(shiftl(iand(int(b(3), int32), 255_int32),  8), &
         iand(int(b(4), int32), 255_int32))))
   end function bytes_to_int32


   ! ==================================================================
   ! IEEE 754 <--> IBM HEX FLOAT CONVERSIONS
   ! ==================================================================

   pure subroutine real64_to_ibmhex(val, b)
      real(real64), intent(in)   :: val
      integer(int8), intent(out) :: b(8)
      real(real64)               :: a
      integer(int32)             :: exp
      integer(int64)             :: fraction
      integer                    :: i

      if (val == 0.0_real64) then
         b = 0_int8
         return
      end if

      a = abs(val)
      exp = 64 ! IBM Excess-64 offset

      ! Normalize for Base 16
      do while (a >= 1.0_real64)
         a = a / 16.0_real64
         exp = exp + 1
      end do
      do while (a < 0.0625_real64 .and. exp > 0)
         a = a * 16.0_real64
         exp = exp - 1
      end do

      ! Shift fraction into 56-bit integer space (16^14)
      fraction = int(a * (16.0_real64**14), int64)

      ! Byte 1: Sign (Bit 7) and Exponent (Bits 0-6)
      b(1) = int(exp, int8)
      if (val < 0.0_real64) b(1) = ibset(b(1), 7)

      ! Bytes 2-8: 56-bit Fraction
      do i = 2, 8
         b(i) = int(iand(shiftr(fraction, (8-i)*8), 255_int64), int8)
      end do
   end subroutine real64_to_ibmhex

   pure function ibmhex_to_real64(b) result(val)
      integer(int8), intent(in) :: b(8)
      real(real64)              :: val
      integer(int32)            :: exp
      integer(int64)            :: fraction
      integer                   :: i
      real(real64)              :: sign_mult

      sign_mult = 1.0_real64
      if (btest(b(1), 7)) sign_mult = -1.0_real64

      ! Extract 7-bit exponent and remove excess-64
      exp = iand(int(b(1), int32), 127_int32) - 64

      ! Extract 56-bit fraction
      fraction = 0_int64
      do i = 2, 8
         fraction = ior(shiftl(fraction, 8), iand(int(b(i), int64), 255_int64))
      end do

      if (fraction == 0) then
         val = 0.0_real64
      else
         val = sign_mult * real(fraction, real64) / (16.0_real64**14) * (16.0_real64**exp)
      end if
   end function ibmhex_to_real64



   !=================================================================
   !  Test harness – writes a file, reads it back and prints results
   !=================================================================
   subroutine test_gds()
      implicit none
      type(gds_header_t)   :: hdr
      type(gds_boundary_t) :: bnd
      integer :: ios

      hdr%version = 5_int16                     ! GDSII‑5.2 version (spec) [1]

      bnd%layer = 1_int16
      bnd%datatype = 0_int16
      allocate(bnd%xy(8))
      bnd%xy = [ 0_int32, 0_int32, 1000_int32, 0_int32, &
         1000_int32, 1000_int32, 0_int32, 1000_int32 ]

      call write_gds('test.gds', hdr, bnd, &
         'demo_lib', 'demo_cell', 0.001_wp, 1.0_wp, ios)
      if (ios /= 0) then
         print *, 'WRITE error, iostat =', ios
         stop
      end if

      call read_gds('test.gds', hdr, bnd, ios)
      if (ios /= 0) then
         print *, 'READ error, iostat =', ios
         stop
      end if

      print *, '--- GDSII Test Result -----------------------------------'
      print *, 'Version  :', hdr%version
      print *, 'Layer    :', bnd%layer
      print *, 'Datatype :', bnd%datatype
      print *, 'Coordinates (db units):', bnd%xy
   end subroutine test_gds

end module gdsii

program main
   use gdsii
   implicit none
   call test_gds()
end program
