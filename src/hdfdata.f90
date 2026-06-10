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
  use hdf5
  implicit none
  private
  public :: h5check, saveToHDF, loadFromHDF, LoadJuliaHDF5, LoadPolygonOffsetsHDF5, &
       LoadKLBin
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
    !write(*,*) 'NUM BOXES in DATA FILE: ', dims(1)
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
  subroutine LoadJuliaHDF5(filename, boxes, scaling_factor)
    character(len=*), intent(in)  :: filename
    type(Box), allocatable, intent(out) :: boxes(:)
    integer, intent(in)           :: scaling_factor
    integer(HID_T) :: file_id, dset_id, space_id
    integer(HID_T) :: mem_type_id
    ! Add this variable for the size setter
    integer(SIZE_T) :: type_size
    integer(HSIZE_T), allocatable :: dims(:), maxdims(:)
    integer :: rank, num_boxes, i, alloc_err
    integer          :: ierr
    ! 1. Change raw data buffer to 32-bit
    integer(int64), allocatable, target :: raw_data(:,:)
    type(c_ptr) :: ptr

    call h5open_f(ierr)
    call h5check(ierr, 'h5open_f')

    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, ierr)
    call h5check(ierr, 'h5fopen_f')
    call h5dopen_f(file_id, "rectangles", dset_id, ierr)
    call h5check(ierr, 'h5dopen_f')

    call h5dget_space_f(dset_id, space_id, ierr)
    call h5check(ierr, 'h5dget_space_f')
    call h5sget_simple_extent_ndims_f(space_id, rank, ierr)
    call h5check(ierr, 'h5sget_simple_extent_ndims_f')

    allocate(dims(rank), maxdims(rank))
    call h5sget_simple_extent_dims_f(space_id, dims, maxdims, ierr)
    call h5check(ierr, 'h5sget_simple_extent_dims_f')
    !write(*,*) 'RANK = ', rank, ' DIMS = ', dims, ' MAXDIMS = ', maxdims    
    ! Allocate the 32-bit 2D buffer
    allocate(raw_data(dims(1), dims(2)), stat=alloc_err)
    if (alloc_err /= 0) stop "ERROR: allocating raw HDF5 read buffer"

    ptr = c_loc(raw_data(1, 1))

    ! 2. Tell HDF5 the memory buffer is 32-bit
    ! HDF5 will automatically downcast the 64-bit disk data to 32-bit memory data
    !mem_type_id = h5kind_to_type(kind(1_int64), H5_INTEGER_KIND) ! choose 32-bit or 64-bit
    call h5tcopy_f(H5T_NATIVE_INTEGER, mem_type_id, ierr)
    ! Explicitly set its size to 4 bytes (32-bit)
    type_size = 8 ! this was 4
    call h5tset_size_f(mem_type_id, type_size, ierr)

    ! Read the data using this explicit memory type
    call h5dread_f(dset_id, mem_type_id, ptr, ierr)
    call h5check(ierr, 'h5dread_f') ! Make sure this is uncommented to catch issues early

    ! Map 2D raw data into the 1D array of Box types
    if (dims(1) == 4) then
       num_boxes = dims(2)
       allocate(boxes(num_boxes))
       do i = 1, num_boxes
          boxes(i)%x1 = raw_data(1, i) / scaling_factor
          boxes(i)%y1 = raw_data(2, i) / scaling_factor
          boxes(i)%x2 = raw_data(3, i) / scaling_factor
          boxes(i)%y2 = raw_data(4, i) / scaling_factor
          if( .not. boxes(i)%is_valid() ) then
             error stop "INVALID BOX 1"
          end if
       end do
    else if (dims(2) == 4) then
       num_boxes = dims(1)
       allocate(boxes(num_boxes))
       do i = 1, num_boxes
          boxes(i)%x1 = raw_data(i, 1) / scaling_factor
          boxes(i)%y1 = raw_data(i, 2) / scaling_factor
          boxes(i)%x2 = raw_data(i, 3) / scaling_factor
          boxes(i)%y2 = raw_data(i, 4) / scaling_factor
          if( .not. boxes(i)%is_valid() ) then
             error stop "INVALID BOX 2"
          end if
       end do
    else
       stop "ERROR: Neither HDF5 dimension equals 4. Cannot map coordinates."
    end if

    write(*,*) 'NUM BOXES LOADED FROM JULIA: ', num_boxes

    ! Clean up
    call h5tclose_f(mem_type_id, ierr)
    call h5sclose_f(space_id, ierr)
    call h5dclose_f(dset_id, ierr)
    call h5fclose_f(file_id, ierr)

    deallocate(raw_data)
    deallocate(dims)
    deallocate(maxdims)

  end subroutine LoadJuliaHDF5
  subroutine NewLoadJuliaHDF5(filename, boxes, scaling_factor)
    character(len=*), intent(in)  :: filename
    type(Box), allocatable, intent(out) :: boxes(:)
    integer, intent(in)           :: scaling_factor
    integer(HID_T) :: file_id, dset_id, space_id
    integer(HSIZE_T), allocatable :: dims(:), maxdims(:)
    integer :: rank, num_boxes, i, alloc_err
    integer :: ierr

    ! 1. Match type exactly with H5T_STD_I64LE on disk
    integer(int64), allocatable, target :: raw_data(:,:)
    type(c_ptr) :: ptr

    call h5open_f(ierr)
    call h5check(ierr, 'h5open_f')

    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, ierr)
    call h5check(ierr, 'h5fopen_f')
    call h5dopen_f(file_id, "rectangles", dset_id, ierr)
    call h5check(ierr, 'h5dopen_f')

    call h5dget_space_f(dset_id, space_id, ierr)
    call h5check(ierr, 'h5dget_space_f')
    call h5sget_simple_extent_ndims_f(space_id, rank, ierr)
    call h5check(ierr, 'h5sget_simple_extent_ndims_f')

    allocate(dims(rank), maxdims(rank))
    call h5sget_simple_extent_dims_f(space_id, dims, maxdims, ierr)
    call h5check(ierr, 'h5sget_simple_extent_dims_f')

    ! Allocate 64-bit matching array layout exactly as described by HDF5
    allocate(raw_data(dims(1), dims(2)), stat=alloc_err)
    if (alloc_err /= 0) stop "ERROR: allocating raw HDF5 read buffer"

    ptr = c_loc(raw_data(1, 1))

    ! 2. CLEANUP: Use native 64-bit integer descriptor directly. 
    ! No manual type copying or size modifications required.
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, ptr, ierr)
    call h5check(ierr, 'h5dread_f')

    ! 3. Map 2D raw data into 1D array of Box types
    ! Since h5dump says (4, 224), dims(1) is exactly 4.
    if (dims(1) == 4) then
       num_boxes = dims(2)
       allocate(boxes(num_boxes))
       do i = 1, num_boxes
          ! Downcast safely during calculation assignment
          boxes(i)%x1 = int(raw_data(1, i) / scaling_factor, kind=4)
          boxes(i)%y1 = int(raw_data(2, i) / scaling_factor, kind=4)
          boxes(i)%x2 = int(raw_data(3, i) / scaling_factor, kind=4)
          boxes(i)%y2 = int(raw_data(4, i) / scaling_factor, kind=4)
          if( .not. boxes(i)%is_valid() ) then
             write(*,*) "CRITICAL: Invalid box found at index: ", i
             write(*,*) "Coords: ", raw_data(1,i), raw_data(2,i), raw_data(3,i), raw_data(4,i)
             error stop "INVALID BOX 1"
          end if
       end do
    else if (dims(2) == 4) then
       num_boxes = dims(1)
       allocate(boxes(num_boxes))
       do i = 1, num_boxes
          boxes(i)%x1 = int(raw_data(i, 1) / scaling_factor, kind=4)
          boxes(i)%y1 = int(raw_data(i, 2) / scaling_factor, kind=4)
          boxes(i)%x2 = int(raw_data(i, 3) / scaling_factor, kind=4)
          boxes(i)%y2 = int(raw_data(i, 4) / scaling_factor, kind=4)
          if( .not. boxes(i)%is_valid() ) then
             write(*,*) "CRITICAL: Invalid box found at index: ", i
             write(*,*) "Coords: ", raw_data(1,i), raw_data(2,i), raw_data(3,i), raw_data(4,i)             
             error stop "INVALID BOX 2"
          end if
       end do
    else
       stop "ERROR: Neither HDF5 dimension equals 4. Cannot map coordinates."
    end if

    write(*,*) 'NUM BOXES LOADED FROM JULIA: ', num_boxes

    ! Clean up
    call h5sclose_f(space_id, ierr)
    call h5dclose_f(dset_id, ierr)
    call h5fclose_f(file_id, ierr)

    deallocate(raw_data)
    deallocate(dims)
    deallocate(maxdims)

  end subroutine NewLoadJuliaHDF5

  !Write another Julia function to directly store this collection of polygon boundaries into HDF5
  !boundary 66 20 {10205 105} {10205 1035} {10525 1035} {10525 705} {10355 705} {10355 105} {10205 105}
  !boundary 66 20 {10520 1605} {10520 1875} {10525 1875} {10525 2615} {10675 2615} {10675 1875} {10850 1875} {10850 1605} {10520 1605}
  !boundary 66 20 {10735 105} {10735 1245} {10040 1245} {10040 1575} {10105 1575} {10105 2615} {10255 2615} {10255 1575} {10310 1575} {10310 1395} {10885 1395} {10885 105} {10735 105}
  !boundary 66 20 {11210 105} {11210 1035} {11095 1035} {11095 2615} {11245 2615} {11245 1220} {11580 1220} {11580 2615} {11730 2615} {11730 1220} {12535 1220} {12535 890} {12265 890} {12265 1035} {11845 1035} {11845 105} {11695 105} {11695 1035} {11360 1035} {11360 105} {11210 105}
  !I think storing these as 3 columns (X,Y,pnum-offset) might work.
  !Can you check this and convert the data I shared into a worked example and then generate
  !Julia code to generate such an HDF5 (use dataset polygons, instead of rectangles) and then generate
  !modern Fortran code to parse it into 3 arrays of (X,Y,pnum-offset).
  subroutine LoadPolygonOffsetsHDF5(filename, X, Y, poly_start, poly_end)
    character(len=*), intent(in)  :: filename
    integer(int32), allocatable, intent(out) :: X(:), Y(:)
    integer(int32), allocatable, intent(out) :: poly_start(:), poly_end(:)
    integer          :: ierr

    integer(HID_T) :: file_id, dset_id, space_id, mem_type_id
    integer(HSIZE_T), allocatable :: dims(:), maxdims(:)
    integer :: rank, num_vertices, num_polygons, i, alloc_err
    integer(SIZE_T) :: type_size

    integer(int32), allocatable, target :: raw_data(:,:)
    type(c_ptr) :: ptr

    ! --- 0) Initialize HDF5 & Memory Type ---
    call h5open_f(ierr)

    call h5tcopy_f(H5T_NATIVE_INTEGER, mem_type_id, ierr)
    type_size = 4
    call h5tset_size_f(mem_type_id, type_size, ierr)

    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, ierr)

    ! ==========================================
    ! READ DATASET 1: VERTICES
    ! ==========================================
    call h5dopen_f(file_id, "vertices", dset_id, ierr)
    call h5dget_space_f(dset_id, space_id, ierr)
    call h5sget_simple_extent_ndims_f(space_id, rank, ierr)

    allocate(dims(rank), maxdims(rank))
    call h5sget_simple_extent_dims_f(space_id, dims, maxdims, ierr)

    allocate(raw_data(dims(1), dims(2)), stat=alloc_err)
    ptr = c_loc(raw_data(1, 1))

    call h5dread_f(dset_id, mem_type_id, ptr, ierr)

    if (dims(1) == 2) then
       num_vertices = dims(2)
       allocate(X(num_vertices), Y(num_vertices))
       do i = 1, num_vertices
          X(i) = raw_data(1, i) / 5
          Y(i) = raw_data(2, i) / 5
       end do
    else if (dims(2) == 2) then
       num_vertices = dims(1)
       allocate(X(num_vertices), Y(num_vertices))
       do i = 1, num_vertices
          X(i) = raw_data(i, 1) / 5
          Y(i) = raw_data(i, 2) / 5
       end do
    else
       stop "ERROR: Invalid vertex dimensions."
    end if

    call h5sclose_f(space_id, ierr)
    call h5dclose_f(dset_id, ierr)
    deallocate(dims, maxdims, raw_data)

    ! ==========================================
    ! READ DATASET 2: OFFSETS
    ! ==========================================
    call h5dopen_f(file_id, "offsets", dset_id, ierr)
    call h5dget_space_f(dset_id, space_id, ierr)
    call h5sget_simple_extent_ndims_f(space_id, rank, ierr)

    allocate(dims(rank), maxdims(rank))
    call h5sget_simple_extent_dims_f(space_id, dims, maxdims, ierr)

    allocate(raw_data(dims(1), dims(2)), stat=alloc_err)
    ptr = c_loc(raw_data(1, 1))

    call h5dread_f(dset_id, mem_type_id, ptr, ierr)

    ! Map offset values (no division by 5 here, these are indices)
    if (dims(1) == 2) then
       num_polygons = dims(2)
       allocate(poly_start(num_polygons), poly_end(num_polygons))
       do i = 1, num_polygons
          poly_start(i) = raw_data(1, i)
          poly_end(i)   = raw_data(2, i)
       end do
    else if (dims(2) == 2) then
       num_polygons = dims(1)
       allocate(poly_start(num_polygons), poly_end(num_polygons))
       do i = 1, num_polygons
          poly_start(i) = raw_data(i, 1)
          poly_end(i)   = raw_data(i, 2)
       end do
    else
       stop "ERROR: Invalid offset dimensions."
    end if

    call h5sclose_f(space_id, ierr)
    call h5dclose_f(dset_id, ierr)
    deallocate(dims, maxdims, raw_data)

    ! --- Clean up ---
    call h5tclose_f(mem_type_id, ierr)
    call h5fclose_f(file_id, ierr)

    write(*,*) 'Loaded Polygons: ', num_polygons
    write(*,*) 'Loaded Vertices: ', num_vertices

  end subroutine LoadPolygonOffsetsHDF5
  subroutine LoadKLBin(fileName,boxes)
    character(len=*), intent(in)  :: filename
    type(Box), allocatable,intent(out) :: boxes(:)
    integer(kind=int64) :: file_bytes, total_boxes, i, dot_pos
    integer, parameter  :: BOX_SIZE_BYTES = 16 ! 4 coordinates * 4 bytes
    integer :: file_unit, io_status
    ! 1. Query the filesystem for the total file size in bytes
    inquire(file=trim(filename), size=file_bytes)
    if (file_bytes <= 0) then
       print *, "Error: File is empty or does not exist."
       stop
    end if
    ! 2. Calculate the exact number of box structs in the array
    total_boxes = file_bytes / BOX_SIZE_BYTES
    write(*,'(A,I12,A)') 'INFO: Total file size =    ', file_bytes, ' bytes'
    write(*,'(A,I12,A)') 'INFO: Allocating array for ', total_boxes, ' boxes.'
    ! 3. Allocate the dynamic storage
    allocate(boxes(total_boxes))
    ! 4. Open the file in raw binary stream mode
    open(newunit=file_unit, &
         file=trim(filename), &
         access='stream', &         ! Eliminates complex record headers
         form='unformatted', &      ! Tells Fortran it is binary, not text
         status='old', &
         action='read', &
         iostat=io_status)
    if (io_status /= 0) then
       print *, "Error: Could not open the binary file."
       stop
    end if
    ! 5. Read the entire file cleanly into your allocated array in one shot
    read(file_unit, iostat=io_status) boxes
    if (io_status == 0) then
       write(*,'(A,I12,A)') 'INFO: Read successful for  ', total_boxes, ' boxes.'       
       ! Example: print the first box if it exists
       !if (total_boxes > 0) then
       !   print '(A,4I12)', "First Box: ", boxes(1)%x1, &
       !        boxes(1)%y1, &
       !        boxes(1)%x2, &
       !        boxes(1)%y2
       !end if
       do i=1,total_boxes
          if( .not. boxes(i)%is_valid() ) error stop "INVALID BOX detected on input"
       end do
    else
       print *, "Error occurred while reading the data array."
    end if

    close(file_unit)

  end subroutine LoadKLBin

end module HDFDataModule
