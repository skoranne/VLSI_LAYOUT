! File   : test_skiplist.f90
! Author : Sandeep Koranne (C) 2026. All rights reserved.
! Purpose: We are going to test our skiplist implementation by
!        : generating N*k values in an array, (1,1,1,2,2,2,3,3,3,...,N,N,N)
!        : then we are going to shuffle it, and then do lookups.
! File   : test_skiplist.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Test harness for the CPP Templatized Arena SkipList.

module TestUtilities
  use iso_fortran_env, only: int32, real32
  implicit none

contains
  subroutine GenerateRepeatArray(N, K, Ans)
    integer, intent(in)               :: N, K
    integer(int32), allocatable, intent(out) :: Ans(:)
    integer :: i, StartIdx, EndIdx

    if (N <= 0 .or. K <= 0) then
       allocate(Ans(0)); return
    end if

    allocate(Ans(N * K))
    do i = 1, N
       StartIdx = (i - 1) * K + 1
       EndIdx   = i * K
       Ans(StartIdx : EndIdx) = i
    end do
  end subroutine GenerateRepeatArray

  subroutine ShuffleArray(Array)
    integer(int32), allocatable, intent(inout) :: Array(:)
    integer :: n, i, j, temp, SeedSize, ClockTime
    real(kind=real32) :: u
    integer, allocatable :: SeedVals(:)

    n = size(Array)
    if (n <= 1) return

    call random_seed(size = SeedSize)
    allocate(SeedVals(SeedSize))

    call system_clock(count=ClockTime)
    do i = 1, SeedSize
       SeedVals(i) = ClockTime + (i * 37) 
    end do
    call random_seed(put = SeedVals)
    deallocate(SeedVals)

    ! Fisher-Yates
    do i = n, 2, -1
       call random_number(u)
       j = floor(u * i) + 1
       temp = Array(i)
       Array(i) = Array(j)
       Array(j) = temp
    end do
  end subroutine ShuffleArray

end module TestUtilities

! File   : test_skiplist_exhaustive.f90
! Author : Sandeep Koranne (C) 2026. (Adapted for SkipListModule)
! Purpose: Exhaustive verification of the templatized, LIFO arena-allocated SkipList.

program TestSkipList
  use iso_fortran_env, only: int32, int64
  use SkipListModule
  use TestUtilities
  implicit none

  integer, parameter :: K         = 5           ! repetitions per key
  integer, parameter :: MAXKEY    = 100000      ! highest key value
  integer, parameter :: ARENASIZE = (MAXKEY+1)*K + 100   ! generous arena

  type(SkipListInt32)              :: sl
  integer(int32)                   :: i, key
  logical                          :: found
  integer(int32)                   :: nextKey, prevKey
  type(SkipListNodeInt32), pointer :: firstNode, curNode
  integer, allocatable             :: arr(:)
  call GenerateRepeatArray( MAXKEY, K, arr )
  call ShuffleArray( arr )
  print *, "=== Initialise 32-bit skip-list ==="
  ! Initialize the LIFO Arena with the requested capacity
  call InitSkipList(sl, int(ARENASIZE, int64))

  !-----------------------------------------------------------------
  !  Insert each key K times
  !-----------------------------------------------------------------
  print *, "Inserting keys 1 ..", MAXKEY, " each", K, "times ..."
  !do key = 1_int32, MAXKEY
  !   do i = 1, K
  !      ! Our engine uses a unified Value for sorting and payload
  !      call InsertNode(sl, key)
  !   end do
  !end do
  do key = 1,size(arr)
     call InsertNode( sl, arr(key) )
  end do

  !-----------------------------------------------------------------
  !  Exhaustive verification
  !-----------------------------------------------------------------
  print *, "=== Running exhaustive verification ==="
  do key = 1_int32, MAXKEY

     !--- FindNode (Generic Interface) must succeed for every key
     found = FindNode(sl, key)
     if (.not. found) then
        print *, "ERROR: key", key, "not found by FindNode"
        stop 1
     end if

     !--- locate the *first* node with this key
     call find_first_node(sl, key, firstNode, found)
     if (.not. found) then
        print *, "ERROR: first node for key", key, "missing"
        stop 1
     end if

     !--- Walk forward K-1 times – we should stay on the same key
     curNode => firstNode
     do i = 1, K-1
        if (.not. associated(curNode%Forward(1)%Ptr)) then
           print *, "ERROR: premature end of list while walking forward for key", key
           stop 1
        end if

        curNode => curNode%Forward(1)%Ptr

        if (curNode%Value /= key) then
           print *, "ERROR: next key after", key, "should still be", key, &
                "but got", curNode%Value
           stop 1
        end if
     end do

     !--- The K-th forward step must give the *next* distinct key
     if (key < MAXKEY) then
        if (.not. associated(curNode%Forward(1)%Ptr)) then
           print *, "ERROR: missing next distinct key after", key
           stop 1
        end if
        if (curNode%Forward(1)%Ptr%Value /= key+1_int32) then
           print *, "ERROR: expected next key", key+1_int32, &
                "but got", curNode%Forward(1)%Ptr%Value
           stop 1
        end if
     else
        ! for the maximal key there should be no forward node
        if (associated(curNode%Forward(1)%Ptr)) then
           print *, "ERROR: expected end of list after max key", key
           stop 1
        end if
     end if

     !--- Test the generic next_key routine
     call get_next_key(sl, key, nextKey, found)
     if (key < MAXKEY) then
        if (.not. found .or. nextKey /= key+1_int32) then
           print *, "ERROR: get_next_key returned", nextKey, "for key", key
           stop 1
        end if
     else
        if (found) then
           print *, "ERROR: get_next_key should not find a key after the maximum"
           stop 1
        end if
     end if

     !--- Test prev_key (previous distinct key = key-1)
     call get_prev_key(sl, key, prevKey, found)
     if (key > 1_int32) then
        if (.not. found .or. prevKey /= key-1_int32) then
           print *, "ERROR: get_prev_key returned", prevKey, "for key", key
           stop 1
        end if
     else
        if (found) then
           print *, "ERROR: get_prev_key should not find a key before the minimum"
           stop 1
        end if
     end if
  end do

  print *, "All checks passed successfully."

  !-----------------------------------------------------------------
  !  Clean-up
  !-----------------------------------------------------------------
  print *, "=== Destroying the list ==="
  call DestroySkipList(sl)

  print *, "Test program finished."


contains

  ! =======================================================================
  ! HELPER ROUTINES (Implemented directly using the public Node structure)
  ! =======================================================================

  subroutine find_first_node1(sl, target_val, node, is_found)
    type(SkipListInt32), intent(in) :: sl
    integer(int32), intent(in) :: target_val
    type(SkipListNodeInt32), pointer :: node, curr
    logical, intent(out) :: is_found
    integer :: j

    is_found = .false.
    curr => sl%Header
    ! Fast skip down to the correct neighborhood
    do j = sl%MaxLevel, 1, -1
       do while (associated(curr%Forward(j)%Ptr))
          if (curr%Forward(j)%Ptr%Value < target_val) then
             curr => curr%Forward(j)%Ptr
          else
             exit
          end if
       end do
    end do

    ! Step into level 1. Because we only advanced while strictly < target_val,
    ! the very next node is guaranteed to be the FIRST instance of the target.
    curr => curr%Forward(1)%Ptr
    if (associated(curr)) then
       if (curr%Value == target_val) then
          node => curr
          is_found = .true.
       end if
    end if
  end subroutine find_first_node1


  subroutine get_next_key1(sl, target_val, nkey, is_found)
    type(SkipListInt32), intent(in) :: sl
    integer(int32), intent(in) :: target_val
    integer(int32), intent(out) :: nkey
    logical, intent(out) :: is_found
    type(SkipListNodeInt32), pointer :: curr

    is_found = .false.
    call find_first_node(sl, target_val, curr, is_found)
    if (.not. is_found) return

    is_found = .false.
    ! Walk horizontally until we hit a strictly greater value
    do while (associated(curr))
       if (curr%Value > target_val) then
          nkey = curr%Value
          is_found = .true.
          return
       end if
       curr => curr%Forward(1)%Ptr
    end do
  end subroutine get_next_key1


  subroutine get_prev_key1(sl, target_val, pkey, is_found)
    type(SkipListInt32), intent(in) :: sl
    integer(int32), intent(in) :: target_val
    integer(int32), intent(out) :: pkey
    logical, intent(out) :: is_found
    type(SkipListNodeInt32), pointer :: curr

    is_found = .false.
    curr => sl%Header%Forward(1)%Ptr
    if (.not. associated(curr)) return

    ! If the very first element in the list is >= target, there is no previous
    if (curr%Value >= target_val) return

    ! Walk level 1 until the *next* node's value is >= target
    do while (associated(curr%Forward(1)%Ptr))
       if (curr%Forward(1)%Ptr%Value >= target_val) then
          pkey = curr%Value
          is_found = .true.
          return
       end if
       curr => curr%Forward(1)%Ptr
    end do
  end subroutine get_prev_key1

end program TestSkipList

! ==============================================================================
! MAIN PROGRAM
! ==============================================================================
subroutine Main()
  use iso_fortran_env, only: int32, int64
  use SkipListModule
  use TestUtilities
  implicit none

  type(SkipListInt32) :: MyList
  integer(kind=int32), allocatable :: Arr(:)
  integer :: i
  integer(kind=int64) :: TotalElements

  print *, "=== SkipList Engine Test (Int32) ==="

  ! 1. Generate 50 unique numbers, repeated 4 times (200 items total)
  call GenerateRepeatArray(50, 4, Arr)
  TotalElements = size(Arr)

  ! 2. Shuffle to simulate real-world un-ordered geometry inputs
  call ShuffleArray(Arr)
  print *, "Data Shuffled successfully."

  ! 3. Initialize Arena (Capacity = TotalElements + buffer)
  call InitSkipList(MyList, TotalElements + 10_int64)
  print *, "SkipList Arena Initialized."

  ! 4. Benchmark Insertion
  do i = 1, TotalElements
     call InsertNode(MyList, Arr(i))
  end do
  print *, "Inserted ", TotalElements, " elements into the SkipList."

  ! 5. Verify Structure Data Integrity
  print *, "Testing valid lookups..."
  if (FindNode(MyList, 25_int32)) then
     print *, " -> SUCCESS: Found element '25' in $O(log N)$ time."
  else
     print *, " -> FAILED: Could not find element '25'."
  end if

  print *, "Testing invalid lookups..."
  if (.not. FindNode(MyList, 999_int32)) then
     print *, " -> SUCCESS: Correctly rejected '999' (Not in set)."
  else
     print *, " -> FAILED: False positive on '999'."
  end if

  ! 6. Clean Memory
  call DestroySkipList(MyList)
  print *, "SkipList memory Arena cleanly destroyed."

end subroutine Main

