  ! File     : generate_bin.f90
  ! Author   : Sandeep Koranne (C) 2026. All rights reserved.
! Puropose : Generate 3-4 boxes in bin as 4-points in int32
module SimpleTest
  use GeometryModule
  use BoxByteStreamModule
  use BoxCompressionModule
  use KLDataModule
  use iso_fortran_env, only : int32, int64
contains  
subroutine Test1()
  !use HDFDataModule
  implicit none
  type(Box),allocatable :: arr(:)
  type(BoxByteStream) :: stream
  integer(kind=int64) :: num_boxes, pos,i
  logical :: ok
  pos = 1
  arr(1) = Box(0,0,2,2)
  arr(2) = Box(2,0,4,2)
  arr(3) = Box(4,0,6,2)
  stream = compress_boxes(arr)
  write(*,*) stream%data
  num_boxes = get_number_boxes( stream, pos, ok, .false. ) !> not scanning yet
  if( .not. ok .or. num_boxes == 0) then
     error stop "Decoding failed."
  end if
  write(*,*) '+++++ Number Boxes in Stream : ', num_boxes, '++++++'
  num_boxes = get_number_boxes( stream, pos, ok, .true. ) !> scanning now
  do i=1,num_boxes
     call decompress_box_stream( stream, pos, arr(i), ok )
     write(*,*) i, ' ', arr(i)
     if( .not. ok .or. .not. arr(i)%is_valid()) then
        error stop "BOX reading failed."
     end if
  end do
  write(*,*) '+++++ Number Boxes in Stream : ', num_boxes, '++++++'
  write(*,*) arr
  write(*,*) '+++++'  
  call WriteKLBin( "x.bin", arr )
end subroutine Test1
end module SimpleTest


program main
  use GeometryModule
  use BoxByteStreamModule
  use BoxCompressionModule
  use KLDataModule
  use iso_fortran_env, only : int32, int64
  !use HDFDataModule
  implicit none
  type(Box),allocatable :: arr(:), readback(:)
  type(Box) :: bbox
  type(BoxByteStream) :: stream
  integer(kind=int64) :: num_boxes, pos,i
  logical :: ok
  pos = 1_int64
  call LoadKLBin("b.bin", arr)
  bbox = mbr_of_array( arr, size(arr) )
  write(*,*) 'Loaded : ', size(arr), ' BBOX = ', bbox
  write(*,'(4I8)') arr(1:10)  
  call quicksort_boxes( arr, 1, size(arr) )
  write(*,*) 'Sorting complete: '
  write(*,'(4I8)') arr(1:10)
  call WriteKLBin( "x.bin", arr(1:1000) )
  stop
  stream = compress_boxes(arr)
  write(*,*) 'Stream size = ', size( stream%data )
  num_boxes = get_number_boxes( stream, pos, ok, .false. ) !> not scanning yet
  if( .not. ok .or. num_boxes == 0) then
     error stop "Decoding failed."
  end if
  write(*,*) '+++++ Number Boxes in Stream : ', num_boxes, '++++++'
  num_boxes = get_number_boxes( stream, pos, ok, .true. ) !> scanning now
  allocate(readback(num_boxes))
  do i=1,num_boxes
     call decompress_box_stream( stream, pos, readback(i), ok )
     if( .not. ok .or. .not. readback(i)%is_valid()) then
        error stop "BOX reading failed."
     end if
  end do
  write(*,*) '+++++ Number Boxes in Stream : ', num_boxes, '++++++'
  !write(*,*) arr
  write(*,*) '+++++'
  if( size(arr) /= size(readback) ) then
     error stop "SIZES do NOT match"
  end if
  do i=1,num_boxes
     if( arr(i) == readback(i) ) then
     else
        
        error stop "CONTENTS do NOT match"
     end if
  end do
  ! calculate the compression ratio
  write(*,*) 'Number of boxes: ', size(arr), ' took ', size(stream%data), ' ratio = ', &
       size(stream%data)*1.0/size(arr), ' bytes per box.'
end program main

