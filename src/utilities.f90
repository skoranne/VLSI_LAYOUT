! File    : utilities.f90
! Author  : Sandeep Koranne (C) 2026.
! Purpose : General purpose utilities for file, system etc
module Utilities
  use iso_fortran_env, only: int64
  implicit none  
  public:: PrintFileUnitInformation, sort_int64, bsearch_int64
contains
  subroutine PrintFileUnitInformation( iunit )
    integer :: iunit

    ! Variables to store INQUIRE results
    logical           :: file_exists, is_opened
    integer           :: file_size
    character(len=30) :: file_access, file_form, file_action
    character(len=10) :: read_perm, write_perm
    character(len=1024)  :: filename
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

  pure recursive subroutine sort_int64(arr)
    integer(kind=int64), intent(inout) :: arr(:)
    integer(kind=int64) :: pivot, temp
    integer(kind=int64) :: i, j, left, right

    if (size(arr) <= 1) return
    left = 1
    right = size(arr)
    pivot = arr((left + right) / 2)
    i = left
    j = right

    do while (i <= j)
       do while (arr(i) < pivot)
          i = i + 1
       end do
       do while (arr(j) > pivot)
          j = j - 1
       end do
       if (i <= j) then
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
          i = i + 1
          j = j - 1
       end if
    end do

    if (left < j) call sort_int64(arr(left:j))
    if (i < right) call sort_int64(arr(i:right))
  end subroutine sort_int64
  function bsearch_int64(key, arr) result(idx)
    ! Assuming iso_fortran_env or your specific kind module is used in the host scope
    ! use iso_fortran_env, only: int64 
    implicit none

    integer(kind=int64), intent(in) :: key
    integer(kind=int64), intent(in) :: arr(:)
    integer(kind=int64)             :: idx

    integer(kind=int64) :: low, high, mid

    low = 1_int64
    high = size(arr, kind=int64)
    idx = 0_int64 ! Default to 0 (not found)

    do while (low <= high)
       ! Use (high - low) / 2 to prevent potential integer overflow 
       ! for massively large arrays instead of (low + high) / 2
       mid = low + (high - low) / 2_int64

       if (arr(mid) == key) then
          idx = mid
          return
       else if (arr(mid) < key) then
          low = mid + 1_int64
       else
          high = mid - 1_int64
       end if
    end do

  end function bsearch_int64
end module Utilities

