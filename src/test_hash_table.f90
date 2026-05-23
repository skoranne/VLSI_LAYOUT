
!=====================================================================
! test driver – shows the public api in action
!=====================================================================
program demo_hash
  use hash_mod
  implicit none

  type(hash_type) :: ht
  integer         :: i, v
  logical         :: ok, ins, rem

  !-------------------------------------------------------------
  ! 1) create a table with roughly 10 buckets (the routine will
  !    round this up to a power of two → 16 buckets)
  !-------------------------------------------------------------
  call hash_destroy (ht)
  call hash_destroy (ht)  
  call hash_create (ht, 10)
  print *, 'created hash table with', ht%nbuckets, 'buckets.'

  !-------------------------------------------------------------
  ! 2) insert a few keys
  !-------------------------------------------------------------
  call hash_put (ht, 'apple',   1, ins); print *, 'insert apple  =>', ins
  call hash_put (ht, 'banana',  2,  ins); print *, 'insert banana =>', ins
  call hash_put (ht, 'citrus',  3,  ins); print *, 'insert citrus =>', ins
  call hash_put (ht, 'date',    4,  ins); print *, 'insert date   =>', ins

  ! updating an existing key:
  call hash_put (ht, 'banana', 77, ins); print *, 'update banana =>', ins

  print *, 'number of items stored =', hash_nitems(ht)

  !-------------------------------------------------------------
  ! 3) look up a few keys
  !-------------------------------------------------------------
  do i = 1, 5
     select case (i)
     case (1); call query('apple')
     case (2); call query('banana')
     case (3); call query('citrus')
     case (4); call query('date')
     case (5); call query('eggplant')
     end select
  end do

  contains
    subroutine query (k)
      character(*), intent(in) :: k
      call hash_get (ht, k, v, ok)
      if (ok) then
         print *, '  key = "', trim(k), '"  →  value =', v
      else
         print *, '  key = "', trim(k), '"  not present.'
      end if
    end subroutine query
end program demo_hash
