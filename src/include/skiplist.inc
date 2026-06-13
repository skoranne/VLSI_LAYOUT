! File   : skiplist_module.F90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Notice the F90 extension, the compilers will pass this thru
!        : cpp and we will get all the different skiplist types we want
! Purpose: CPP Template for a High-Performance, Arena-Allocated SkipList.
!          Uses #ifdef blocks to separate Type generation from Procedure generation.

! ==============================================================================
! SECTION 1: TYPE DEFINITIONS
! ==============================================================================
#ifdef SKIPLIST_GENERATE_TYPES
!=====================================================================
!  skiplist_template.inc
!  --------------------
!  This file is included twice (once for int32, once for int64) by
!  skiplist_module.F90.  Two macros must be defined before the include:
!
!     KIND_VAL   – the integer kind (int32 or int64)
!     PROC_NAME  – a unique prefix for the generated procedures
!
!  The derived type SkipNode(KIND_VAL) is already defined in the
!  module, so this file contains **only the procedures** that operate
!  on it.
!=====================================================================

!-----------------------------------------------------------------
!  Random level generator (pure function – same for both kinds)
!-----------------------------------------------------------------
pure integer function PROC_NAME_random_level(maxLevel, prob) result(lvl)
   integer, intent(in) :: maxLevel
   real,    intent(in) :: prob
   real                :: r
   lvl = 1
   call random_number(r)
   do while (r < prob .and. lvl < maxLevel)
      lvl = lvl + 1
      call random_number(r)
   end do
end function PROC_NAME_random_level


!-----------------------------------------------------------------
!  init_skiplist – allocate arena, set up head sentinel and free list.
!-----------------------------------------------------------------
subroutine PROC_NAME_init_skiplist(list, maxLevel, prob, arenaSize)
   type(SkipList(KIND_VAL)), intent(inout) :: list
   integer,                 intent(in)    :: maxLevel
   real,                    intent(in)    :: prob
   integer,                 intent(in)    :: arenaSize
   integer :: i

   list%maxLevel = maxLevel
   list%prob     = prob

   allocate(list%arena(arenaSize))
   do i = 1, arenaSize
      list%arena(i)%forward => null()
   end do

   ! initialise free‑list (LIFO)
   list%freePtr => list%arena(1)
   do i = 1, arenaSize-1
      list%arena(i)%forward(1) => list%arena(i+1)   ! store next free node in forward(1)
   end do
   list%arena(arenaSize)%forward(1) => null()

   ! head sentinel (level = maxLevel)
   allocate(list%head)
   list%head%key   = -huge(0_KIND_VAL)
   list%head%value = 0_KIND_VAL
   list%head%level = maxLevel
   allocate(list%head%forward(maxLevel))
   list%head%forward = null()
end subroutine PROC_NAME_init_skiplist


!-----------------------------------------------------------------
!  get_free_node – pop a node from the LIFO free list (grow arena if needed).
!-----------------------------------------------------------------
function PROC_NAME_get_free_node(list) result(node)
   type(SkipList(KIND_VAL)), intent(inout) :: list
   type(SkipNode(KIND_VAL)), pointer       :: node

   if (associated(list%freePtr)) then
      node => list%freePtr
      list%freePtr => list%freePtr%forward(1)
   else
      ! grow arena by 50 %
      integer :: oldSize, newSize, i
      oldSize = size(list%arena)
      newSize = max(1, oldSize + (oldSize+1)/2)
      allocate(list%arena(oldSize+1:newSize))
      do i = oldSize+1, newSize
         list%arena(i)%forward => null()
      end do
      do i = oldSize+1, newSize-1
         list%arena(i)%forward(1) => list%arena(i+1)
      end do
      list%arena(newSize)%forward(1) => null()
      list%freePtr => list%arena(oldSize+1)
      node => PROC_NAME_get_free_node(list)   ! now we have a node
   end if
end function PROC_NAME_get_free_node


!-----------------------------------------------------------------
!  release_node – push a node back onto the free list.
!-----------------------------------------------------------------
subroutine PROC_NAME_release_node(list, node)
   type(SkipList(KIND_VAL)), intent(inout) :: list
   type(SkipNode(KIND_VAL)), pointer       :: node

   node%forward(1) => list%freePtr
   list%freePtr => node
end subroutine PROC_NAME_release_node


!-----------------------------------------------------------------
!  insert_node – standard skip‑list insertion (duplicates are allowed).
!-----------------------------------------------------------------
subroutine PROC_NAME_insert_node(list, key, value, ierr)
   type(SkipList(KIND_VAL)), intent(inout) :: list
   integer(KIND_VAL),        intent(in)    :: key
   integer(KIND_VAL),        intent(in)    :: value
   integer,                  intent(out)   :: ierr

   type(SkipNode(KIND_VAL)), pointer :: update(:)
   type(SkipNode(KIND_VAL)), pointer :: x
   integer                           :: i, lvl

   allocate(update(list%maxLevel))
   x => list%head

   ! locate insertion points at each level
   do i = list%maxLevel, 1, -1
      do while (associated(x%forward(i)) .and. x%forward(i)%key < key)
         x => x%forward(i)
      end do
      update(i) => x
   end do

   x => x%forward(1)

   ! (duplicates are allowed – we simply insert another node)
   ierr = 0

   ! generate a random level for the new node
   lvl = PROC_NAME_random_level(list%maxLevel, list%prob)

   ! obtain a node from the arena
   x => PROC_NAME_get_free_node(list)
   x%key   = key
   x%value = value
   x%level = lvl
   allocate(x%forward(lvl))
   x%forward = null()

   ! splice into the list
   do i = 1, lvl
      x%forward(i) => update(i)%forward(i)
      update(i)%forward(i) => x
   end do

   deallocate(update)
end subroutine PROC_NAME_insert_node


!-----------------------------------------------------------------
!  find_candidate – locate *any* node with the given key.
!-----------------------------------------------------------------
subroutine PROC_NAME_find_candidate(list, key, value, found)
   type(SkipList(KIND_VAL)), intent(in)  :: list
   integer(KIND_VAL),        intent(in)  :: key
   integer(KIND_VAL),        intent(out) :: value
   logical,                  intent(out) :: found

   type(SkipNode(KIND_VAL)), pointer :: x
   integer                           :: i

   x => list%head
   do i = list%maxLevel, 1, -1
      do while (associated(x%forward(i)) .and. x%forward(i)%key < key)
         x => x%forward(i)
      end do
   end do

   x => x%forward(1)
   if (associated(x) .and. x%key == key) then
      value = x%value
      found = .true.
   else
      value = 0_KIND_VAL
      found = .false.
   end if
end subroutine PROC_NAME_find_candidate


!-----------------------------------------------------------------
!  find_first_node – return a pointer to the *first* node that holds KEY.
!-----------------------------------------------------------------
subroutine PROC_NAME_find_first_node(list, key, node, found)
   type(SkipList(KIND_VAL)), intent(in)  :: list
   integer(KIND_VAL),        intent(in)  :: key
   type(SkipNode(KIND_VAL)), pointer    :: node
   logical,                  intent(out) :: found
   integer                              :: i

   node => list%head
   do i = list%maxLevel, 1, -1
      do while (associated(node%forward(i)) .and. node%forward(i)%key < key)
         node => node%forward(i)
      end do
   end do

   node => node%forward(1)
   if (associated(node) .and. node%key == key) then
      found = .true.
   else
      node => null()
      found = .false.
   end if
end subroutine PROC_NAME_find_first_node


!-----------------------------------------------------------------
!  next_key – return the key that follows the *first* occurrence of KEY.
!-----------------------------------------------------------------
subroutine PROC_NAME_next_key(list, key, nextKey, found)
   type(SkipList(KIND_VAL)), intent(in)  :: list
   integer(KIND_VAL),        intent(in)  :: key
   integer(KIND_VAL),        intent(out) :: nextKey
   logical,                  intent(out) :: found
   type(SkipNode(KIND_VAL)), pointer    :: node
   logical                               :: dummy

   call PROC_NAME_find_first_node(list, key, node, dummy)
   if (.not. associated(node)) then
      found = .false.; return
   end if

   if (associated(node%forward(1))) then
      nextKey = node%forward(1)%key
      found   = .true.
   else
      found = .false.
   end if
end subroutine PROC_NAME_next_key


!-----------------------------------------------------------------
!  prev_key – return the key that precedes the *first* occurrence of KEY.
!-----------------------------------------------------------------
subroutine PROC_NAME_prev_key(list, key, prevKey, found)
   type(SkipList(KIND_VAL)), intent(in)  :: list
   integer(KIND_VAL),        intent(in)  :: key
   integer(KIND_VAL),        intent(out) :: prevKey
   logical,                  intent(out) :: found
   type(SkipNode(KIND_VAL)), pointer    :: x, prev
   integer                              :: i

   x => list%head
   prev => null()
   do i = list%maxLevel, 1, -1
      do while (associated(x%forward(i)) .and. x%forward(i)%key < key)
         x => x%forward(i)
      end do
   end do

   ! x now points to the greatest node with key < requested key (or head)
   if (associated(x) .and. x%key < key) then
      prev => x
   end if

   if (associated(prev)) then
      prevKey = prev%key
      found   = .true.
   else
      found = .false.
   end if
end subroutine PROC_NAME_prev_key


!-----------------------------------------------------------------
!  destroy_skiplist – release all allocated components.
!-----------------------------------------------------------------
subroutine PROC_NAME_destroy_skiplist(list)
   type(SkipList(KIND_VAL)), intent(inout) :: list
   integer :: i

   if (associated(list%head)) then
      if (allocated(list%head%forward)) deallocate(list%head%forward)
      deallocate(list%head)
   end if

   if (allocated(list%arena)) then
      do i = 1, size(list%arena)
         if (allocated(list%arena(i)%forward)) deallocate(list%arena(i)%forward)
      end do
      deallocate(list%arena)
   end if

   nullify(list%freePtr)
end subroutine PROC_NAME_destroy_skiplist

!=====================================================================
!  End of skiplist_template.inc
!=====================================================================
#endif
