! File   : trs_main.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: stand-alone driver for the CTR TRS optimizer (same code MAGPARSER runs at compile time)
!   TRS_OPT.exe relations_trs.rules input.ctr output.ctr
program trs_opt
  use TRSOptimizerModule
  implicit none
  character(len=1024) :: rules, fin, fout
  integer :: status
  if (command_argument_count() /= 3) then
     write(*,'(A)') 'Usage: TRS_OPT.exe relations_trs.rules input.ctr output.ctr'
     stop 1
  end if
  call get_command_argument(1, rules)
  call get_command_argument(2, fin)
  call get_command_argument(3, fout)
  call TRSOptimizeFile(trim(rules), trim(fin), trim(fout), .true., status)
  if (status /= 0) error stop 2
end program trs_opt
