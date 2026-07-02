! File    : utilities.f90
! Author  : Sandeep Koranne (C) 2026.
! Purpose : General purpose utilities for file, system etc
module Utilities
  implicit none
  public:: PrintFileUnitInformation
contains
  subroutine PrintFileUnitInformation( iunit )
    integer :: iunit

    ! Variables to store INQUIRE results
    logical           :: file_exists, is_opened
    integer           :: file_size
    character(len=30) :: file_access, file_form, file_action
    character(len=10) :: read_perm, write_perm
    character(len=1024)  :: filename
    ! 1. Create a dummy file to inspect
    open(unit=10, file=filename, status='replace', action='write')
    write(10, *) 'Fortran INQUIRE test data.'
    close(10)

    ! 2. Inquire about the file by its name
    inquire(unit=iunit,&
         name=filename, &
         exist=file_exists, &
         opened=is_opened, &
         size=file_size, &       ! Added in F2003: returns size in bytes
         access=file_access, &
         form=file_form, &
         action=file_action, &
         read=read_perm, &
         write=write_perm)

    ! 3. Report the properties
    print *, "--- File Properties ---"
    print *, "File Name:   ", filename
    print *, "Exists:      ", file_exists
    print *, "Is Opened:   ", is_opened
    print *, "Size (bytes):", file_size
    print *, "Access Mode: ", trim(file_access)  ! e.g., SEQUENTIAL, STREAM, DIRECT
    print *, "Format:      ", trim(file_form)    ! e.g., FORMATTED, UNFORMATTED
    print *, "Action:      ", trim(file_action)  ! e.g., READ, WRITE, READWRITE
    print *, "Can Read:    ", trim(read_perm)    ! e.g., YES, NO, UNKNOWN
    print *, "Can Write:   ", trim(write_perm)

  end subroutine PrintFileUnitInformation
end module Utilities

