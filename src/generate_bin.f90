  ! File     : generate_bin.f90
  ! Author   : Sandeep Koranne (C) 2026. All rights reserved.
  ! Puropose : Generate 3-4 boxes in bin as 4-points in int32
program main
  use GeometryModule
  use HDFDataModule
  implicit none
  type(Box) :: arr(3)
  arr(1) = Box(0,0,2,2)
  arr(2) = Box(2,0,4,2)
  arr(3) = Box(4,0,6,2)
  call WriteKLBin( "x.bin", arr )
end program main

