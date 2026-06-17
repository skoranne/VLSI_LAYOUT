  ! File     : generate_bin.f90
  ! Author   : Sandeep Koranne (C) 2026. All rights reserved.
  ! Puropose : Generate 3-4 boxes in bin as 4-points in int32
program main
  use GeometryModule
  use HDFDataModule
  use KLDataModule
  implicit none
  type(Box) :: arr(3)
  arr(1) = Box(0,0,10,10)
  arr(2) = Box(8,2,12,4)
  arr(3) = Box(6,6,10,8)
  call WriteKLBin( "bdry.bin", arr )
end program main

