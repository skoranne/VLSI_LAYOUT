! =========================================================================
! TEST PROGRAM
! =========================================================================
program test_ScanBox_merge
  use GeometryModule
  use BoxMergeModule
  use, intrinsic :: iso_fortran_env, only: int32
  implicit none

  type(ScanBox), allocatable :: my_ScanBoxes(:)

  ! Setup 5 test cases
  allocate(my_ScanBoxes(6))

  ! Case 1: Two horizontally touching ScanBoxes forming a rectangle
  my_ScanBoxes(1) = ScanBox(Box(0, 0, 2, 2), .true.)
  my_ScanBoxes(2) = ScanBox(Box(2, 0, 4, 2), .true.)

  ! Case 2: A ScanBox entirely inside another ScanBox (perfect overlap, preserves outer rectangle)
  my_ScanBoxes(3) = ScanBox(Box(10, 10, 20, 20), .true.)
  my_ScanBoxes(4) = ScanBox(Box(12, 12, 15, 15), .true.)

  ! Case 3: Two overlapping ScanBoxes forming an 'L-Shape' (Should NOT merge)
  my_ScanBoxes(5) = ScanBox(Box(30, 30, 40, 40), .true.)
  my_ScanBoxes(6) = ScanBox(Box(35, 35, 45, 45), .true.)

  call print_ScanBoxes(my_ScanBoxes, "Before Merge")

  ! Execute Scanline Merge
  call merge_all_boxes(my_ScanBoxes)

  call print_ScanBoxes(my_ScanBoxes, "After Merge")

  deallocate(my_ScanBoxes)
end program test_ScanBox_merge
