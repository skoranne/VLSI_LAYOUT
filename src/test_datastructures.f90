! File    : test_datastructures.f90
! Author  : Sandeep Koranne (C) All rights reserved.
! Purpose : Test of RingBuffer
program demo_ringbuffer
  use DataStructuresModule
  implicit none

  !-----------------------------------------------------------------
  !  Example 1 : integer buffer with capacity 5
  !-----------------------------------------------------------------
  type(RingBuffer(5)) :: int_buf
  integer :: i, val, stat
  type(UnionFind) :: uf
  integer :: root_i

  call int_buf%init()                     ! allocate storage

  print *, '--- pushing 1..5 ---'
  do i = 1,5
     call int_buf%push(i,stat)
     if (stat/=0) stop 'unexpected full condition'
  end do

  print *, 'Is buffer full? ', int_buf%full()
  print *, 'Current size   : ', int_buf%size()

  print *, '--- trying to push 99 (should fail) ---'
  call int_buf%push(99, stat)
  print *, 'stat = ', stat, ' (1 means full)'

  print *, '--- popping all elements ---'
  do while (.not. int_buf%empty())
     call int_buf%pop(val, stat)
     if (stat==0) print *, 'popped = ', val
  end do


  write(*,*) '----------------------UNION FIND TEST CASE----------------------------------'
  !--------------------------------------------------------------
  !  Create a Union‑Find that can hold up to 20 elements.
  !--------------------------------------------------------------
  call uf%init(8)

  !  Merge a few pairs (forming two separate sets):
  !     {1,2,3,4}   and   {5,6,7}
  do i=1,7
     call uf%insert(i)
  end do
  write(*,'(8I2)') uf%arr
  print *, 'root(1) =', uf%root(1)   ! → 1 (still the original root)
  print *, 'root(5) =', uf%root(5)   ! → 5
  
  call uf%merge(1,2)
  write(*,'(8I2)') uf%arr  
  call uf%merge(2,3)
  write(*,'(8I2)') uf%arr    
  call uf%merge(3,4)
  write(*,'(8I2)') uf%arr  
  call uf%merge(5,6)
  write(*,'(8I2)') uf%arr    
  call uf%merge(6,7)
  write(*,'(8I2)') uf%arr  
  !--------------------------------------------------------------
  !  Check the root of a few elements (pure function – no side‑effects)
  !--------------------------------------------------------------
  print *, 'root(1) =', uf%root(1)   ! → 1 (still the original root)
  print *, 'root(5) =', uf%root(5)   ! → 5

  !--------------------------------------------------------------
  !  Apply path compression to element 4
  !--------------------------------------------------------------
  call uf%reduce(4)                ! now arr(4) points directly to the root
  print *, 'After reduce(4), arr(4) =', uf%arr(4)

  !--------------------------------------------------------------
  !  Merge the two big sets together:
  !--------------------------------------------------------------
  call uf%merge(4,6)               ! joins {1‑4} with {5‑7}
  print *, 'Now root(1) =', uf%root(1)
  print *, 'Now root(5) =', uf%root(5)
  write(*,'(8I2)') uf%arr
  !--------------------------------------------------------------
  !  Demonstrate that path compression works for any element:
  !--------------------------------------------------------------
  call uf%reduce(2)                ! path of element 2
  print *, 'After reduce(2), arr(2) =', uf%arr(2)
  call uf%reduce(8)
  print *, 'After reduce(8), arr(8) =', uf%arr(8)
  write(*,'(8I2)') uf%arr
  do i=1,8
     call uf%reduce(i)
  end do
  print *, 'After FULL reduce'
  write(*,'(8I2)') uf%arr
  

end program demo_ringbuffer
