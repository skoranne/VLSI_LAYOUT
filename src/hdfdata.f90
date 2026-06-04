!=====================================================================
! File   : hdfdata.f90
! Author : Sandeep Koranne (C) All rights reserved
!  Simple demo: store an array of Box objects in an HDF5 file and read it
!  back again.  Uses the HDF5‑Fortran API (module hdf5).
!  HDF5_FC=ifx /scratch1/skoranne/INTELCOMPILERS/bin/h5fc -O3 disk.f90 -o disk.exe
!  h5ls and h5stat show file info, and also use Julia HDF5 package
!=====================================================================
module HDFDataModule
  use iso_fortran_env,only: int32, int64
  use GeometryModule
  implicit none
  private

  public :: saveToHDF, loadFromHDF
contains
  !=================================================================
  !  Helper: abort the program if an HDF5 call returned a non‑zero error.
  !=================================================================
  subroutine h5check(errcode, msg)
    integer, intent(in) :: errcode
    character(*), intent(in) :: msg
    if( errcode < 0) then
       write(*,*) 'HDF5 error in ', trim(msg), ': err = ', errcode
       stop 1
    end if
  end subroutine h5check

  !=================================================================
  !  Build a *compound* HDF5 datatype that matches the memory layout
  !  of the Fortran type Box.
  !=================================================================
  subroutine make_box_type(h5type)
    use hdf5
    use iso_c_binding
    implicit none
    type(Box) :: dummy
    integer(HID_T), intent(out) :: h5type
    integer :: ierr
    integer(HSIZE_T) :: offset
    integer(size_t) :: sz

    sz = storage_size(dummy) / 8          ! bytes occupied by a Box

    call h5tcreate_f(H5T_COMPOUND_F,sz, h5type, ierr)
    call h5check(ierr, 'h5tcreate_f (compound)')

    offset = 0
    call h5tinsert_f(h5type, 'x1', offset, H5T_NATIVE_INTEGER, ierr)
    call h5check(ierr, 'h5tinsert_f x1')
    offset = offset + storage_size(dummy%x1) / 8

    call h5tinsert_f(h5type, 'y1', offset, H5T_NATIVE_INTEGER, ierr)
    call h5check(ierr, 'h5tinsert_f y1')
    offset = offset + storage_size(dummy%y1) / 8

    call h5tinsert_f(h5type, 'x2', offset, H5T_NATIVE_INTEGER, ierr)
    call h5check(ierr, 'h5tinsert_f x2')
    offset = offset + storage_size(dummy%x2) / 8

    call h5tinsert_f(h5type, 'y2', offset, H5T_NATIVE_INTEGER, ierr)
    call h5check(ierr, 'h5tinsert_f y2')
  end subroutine make_box_type

  !=================================================================
  !  saveToHDF – write an array of Box to a file
  !=================================================================
  subroutine saveToHDF(filename, boxes)
    use hdf5
    use iso_c_binding
    implicit none
    character(*),               intent(in) :: filename
    type(Box),   intent(in),target :: boxes(:)

    integer(HID_T) :: file_id, dset_id, space_id, type_id
    integer(HSIZE_T), dimension(1) :: dims
    integer :: ierr
    type(c_ptr) :: ptr
    ptr = c_loc(boxes(1))
    call h5open_f(ierr)
    call h5check(ierr, 'h5fopen_f')

    !--- 1) open/create the file ------------------------------------
    call h5fcreate_f(trim(filename), H5F_ACC_TRUNC_F, file_id, ierr)
    call h5check(ierr, 'h5fcreate_f')

    !--- 2) describe the dataspace (1‑D array of length = size(boxes)) ---
    dims(1) = size(boxes)
    call h5screate_simple_f(1, dims, space_id, ierr)
    call h5check(ierr, 'h5screate_simple_f')

    !--- 3) create the compound datatype that mirrors Box -------------
    call make_box_type(type_id)

    !--- 4) create the dataset ---------------------------------------
    call h5dcreate_f(file_id, "boxes", type_id, space_id, dset_id, ierr)
    call h5check(ierr, 'h5dcreate_f')

    !--- 5) write the data -------------------------------------------
    call h5dwrite_f(dset_id, type_id, ptr, ierr)
    call h5check(ierr, 'h5dwrite_f')

    !--- 6) clean up -------------------------------------------------
    call h5tclose_f(type_id, ierr)
    call h5sclose_f(space_id, ierr)
    call h5dclose_f(dset_id, ierr)
    call h5fclose_f(file_id, ierr)
  end subroutine saveToHDF

  !=================================================================
  !  loadFromHDF – read the array back from a file
  !=================================================================
  subroutine loadFromHDF(filename, boxes)
    use hdf5
    use iso_c_binding
    character(*),               intent(in)  :: filename
    type(Box), allocatable, target, intent(out) :: boxes(:)

    integer(HID_T) :: file_id, dset_id, space_id, type_id
    integer(HSIZE_T), dimension(1) :: dims, maxdims
    type(c_ptr) :: ptr
    integer :: ierr, alloc_err

    !--- 1) open the file --------------------------------------------
    call h5open_f(ierr)
    call h5check(ierr, 'h5fopen_f')

    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, ierr)
    call h5check(ierr, 'h5fopen_f')

    !--- 2) open the dataset ------------------------------------------
    call h5dopen_f(file_id, "boxes", dset_id, ierr)
    call h5check(ierr, 'h5dopen_f')

    !--- 3) query its size --------------------------------------------
    call h5dget_space_f(dset_id, space_id, ierr)
    call h5check(ierr, 'h5dget_space_f')
    call h5sget_simple_extent_dims_f(space_id, dims, maxdims,ierr)
    call h5check(ierr, 'h5sget_simple_extent_dims_f')

    !--- 4) allocate the Fortran array -------------------------------
    write(*,*) 'NUM BOXES in DATA FILE: ', dims(1)
    if( allocated(boxes) ) then
       deallocate(boxes)
    end if
    allocate(boxes(dims(1)),stat=alloc_err)
    if( alloc_err /= 0 ) then
       stop "ERROR: allocating box from HDF read"
    end if
    ptr = c_loc(boxes(1))
    !--- 5) create the compound datatype (must match the writer) -----
    call make_box_type(type_id)

    !--- 6) read the data ---------------------------------------------
    call h5dread_f(dset_id, type_id, ptr, ierr)
    call h5check(ierr, 'h5dread_f')

    !--- 7) clean up -------------------------------------------------
    call h5tclose_f(type_id, ierr)
    call h5sclose_f(space_id, ierr)
    call h5dclose_f(dset_id, ierr)
    call h5fclose_f(file_id, ierr)
  end subroutine loadFromHDF
end module HDFDataModule
