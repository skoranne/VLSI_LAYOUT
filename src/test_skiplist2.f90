!=====================================================================
!  test_skiplist.f90
!  -----------------
!  Driver program that exercises the generic skip‑list interface.
!  It builds a list where each integer 1 … MAXKEY occurs exactly K times,
!  then checks:
!     * find_candidate works for every inserted key,
!     * next_key returns the same key K‑1 times and then the next larger key,
!     * prev_key returns the same key K‑1 times and then the previous key.
!=====================================================================
program TestSkipList
   use iso_fortran_env, only: int32, real64
   use SkipListModule2
   implicit none

   integer, parameter :: K        = 5          ! repetitions per key
   integer, parameter :: MAXKEY   = 10         ! highest key value
   integer, parameter :: MAXLEVEL = 12
   real(real64), parameter :: PROB = 0.25_real64
   integer, parameter :: ARENASIZE = (MAXKEY+1)*K + 100   ! generous arena

   type(SkipList(int32)) :: sl
   integer(int32)        :: i, key, value
   logical               :: found
   integer               :: ierr
   integer(int32)        :: nextKey, prevKey
   type(SkipNode(int32)), pointer :: firstNode, curNode

   print *, "=== Initialise 32‑bit skip‑list ==="
   call init_skiplist(sl, MAXLEVEL, real(PROB, kind=real64), ARENASIZE)

   !-----------------------------------------------------------------
   !  Insert each key K times
   !-----------------------------------------------------------------
   print *, "Inserting keys 1 ..", MAXKEY, " each", K, "times ..."
   do key = 1_int32, MAXKEY
      do i = 1, K
         value = key*10_int32 + i_int32   ! payload is just for illustration
         call insert_node(sl, key, value, ierr)
         if (ierr /= 0) then
            print *, "  *** insertion error for key ", key
         end if
      end do
   end do

   !-----------------------------------------------------------------
   !  Exhaustive verification
   !-----------------------------------------------------------------
   print *, "=== Running exhaustive verification ==="
   do key = 1_int32, MAXKEY
      !--- find_candidate must succeed for every key
      call find_candidate(sl, key, value, found)
      if (.not. found) then
         print *, "ERROR: key", key, "not found by find_candidate"
         stop 1
      end if

      !--- locate the *first* node with this key
      call find_first_node(sl, key, firstNode, found)
      if (.not. found) then
         print *, "ERROR: first node for key", key, "missing"
         stop 1
      end if

      !--- Walk forward K‑1 times – we should stay on the same key
      curNode => firstNode
      do i = 1, K-1
         if (.not. associated(curNode%forward(1))) then
            print *, "ERROR: premature end of list while walking forward for key", key
            stop 1
         end if
         curNode => curNode%forward(1)
         if (curNode%key /= key) then
            print *, "ERROR: next key after", key, "should still be", key, &
                     "but got", curNode%key
            stop 1
         end if
      end do

      !--- The K‑th forward step must give the *next* distinct key
      if (key < MAXKEY) then
         if (.not. associated(curNode%forward(1))) then
            print *, "ERROR: missing next distinct key after", key
            stop 1
         end if
         if (curNode%forward(1)%key /= key+1_int32) then
            print *, "ERROR: expected next key", key+1_int32, &
                     "but got", curNode%forward(1)%key
            stop 1
         end if
      else
         ! for the maximal key there should be no forward node
         if (associated(curNode%forward(1))) then
            print *, "ERROR: expected end of list after max key", key
            stop 1
         end if
      end if

      !--- Test the generic next_key routine
      call next_key(sl, key, nextKey, found)
      if (key < MAXKEY) then
         if (.not. found .or. nextKey /= key+1_int32) then
            print *, "ERROR: next_key returned", nextKey, "for key", key
            stop 1
         end if
      else
         if (found) then
            print *, "ERROR: next_key should not find a key after the maximum"
            stop 1
         end if
      end if

      !--- Test prev_key (previous distinct key = key‑1)
      call prev_key(sl, key, prevKey, found)
      if (key > 1_int32) then
         if (.not. found .or. prevKey /= key-1_int32) then
            print *, "ERROR: prev_key returned", prevKey, "for key", key
            stop 1
         end if
      else
         if (found) then
            print *, "ERROR: prev_key should not find a key before the minimum"
            stop 1
         end if
      end if
   end do

   print *, "All checks passed."

   !-----------------------------------------------------------------
   !  Clean‑up
   !-----------------------------------------------------------------
   print *, "=== Destroying the list ==="
   call destroy_skiplist(sl)

   print *, "Test program finished."
end program TestSkipList
