program demo
  use DataStructuresModule
  use iso_fortran_env, only : int64
   implicit none
   type(UnionFind) :: uf
   integer(int64)  :: n = 100_int64

   call uf%init( n )      ! <‑‑ resolved entirely by the compiler
   call uf%insert( 5_int64 )
   print *, uf%root(5_int64)
end program demo
