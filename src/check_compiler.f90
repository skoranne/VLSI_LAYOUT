#ifdef __INTEL_COMPILER
program main
  write(*,*) 'Intel compiler'
end program main
#else
program main
  write(*,*) 'Non-Intel compiler'
end program main
#endif
