program main
  integer, allocatable :: x(:)
  allocate (x(10))
  write(*,*) 'Size of x = ', size(x)
  deallocate(x)
  allocate(x(20))
  write(*,*) 'Size of x = ', size(x)  
end program main
