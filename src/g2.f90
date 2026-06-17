!=====================================================================
!  generic_skiplist.f90
!  Pointer-based skip list - corrected version
!=====================================================================
module generic_skiplist
   implicit none
   private
   public :: SL_INIT, SL_DESTROY, SL_FIND, SL_INSERT, SL_DELETE
   public :: Skiplist, SkiplistNode, NodePtr, MAX_LEVELS
   public :: sl_print
   integer, parameter :: MAX_LEVELS = 16
   real,    parameter :: DEFAULT_P  = 0.5

   !=================================================================
   !  1) Wrapper type to allow arrays of pointers
   !=================================================================
   type :: NodePtr
      type(SkiplistNode), pointer :: node => null()
   end type NodePtr

   !=================================================================
   !  2) Node type - integer payload + allocatable array of NodePtrs
   !=================================================================
   type :: SkiplistNode
      integer :: data = 0
      type(NodePtr), allocatable :: forward(:)
   end type SkiplistNode

   !=================================================================
   !  3) One arena page - a contiguous block of nodes.
   !=================================================================
   type :: ArenaPage
      type(SkiplistNode), pointer :: nodes(:)   ! will be allocated later
   end type ArenaPage

   !=================================================================
   !  4) The container that owns the head, the free list and the arena
   !=================================================================
   type :: Skiplist
      integer                     :: max_levels = MAX_LEVELS
      real                        :: prob = DEFAULT_P
      type(SkiplistNode), pointer :: head  => null()
      type(SkiplistNode), pointer :: free_list => null()   ! LIFO free list
      integer                     :: page_size = 4096
      integer                     :: n_pages   = 0
      type(ArenaPage), allocatable :: pages(:)             ! array of pages
   end type Skiplist

   interface SL_INIT    ; module procedure sl_init    ; end interface
   interface SL_DESTROY ; module procedure sl_destroy ; end interface
   interface SL_FIND    ; module procedure sl_find    ; end interface
   interface SL_INSERT  ; module procedure sl_insert  ; end interface
   interface SL_DELETE  ; module procedure sl_delete  ; end interface

contains

   !=================================================================
   !  Random level generator (geometric distribution)
   !=================================================================
   integer function random_level(sl) result(level)
      type(Skiplist), intent(in) :: sl
      real                       :: r
      level = 1
      do while (level < sl%max_levels)
         call random_number(r)
         if (r >= sl%prob) exit
         level = level + 1
      end do
   end function random_level

   !=================================================================
   !  Allocate a fresh arena page and push all its nodes onto the free list.
   !=================================================================
!=================================================================
   !  Allocate a fresh arena page and push all its nodes onto the free list.
   !=================================================================
   subroutine allocate_new_page(sl)
      type(Skiplist), intent(inout) :: sl
      integer                       :: i, n
      type(SkiplistNode), pointer   :: node
      type(SkiplistNode), pointer   :: tmp
      type(ArenaPage), allocatable  :: temp_pages(:)  ! Temporary array for resizing

      !--- grow the pages array ---------------------------------------
      if (.not. allocated(sl%pages)) then
         allocate(sl%pages(1))
      else
         ! 1. Allocate a temporary array of the new size
         allocate(temp_pages(sl%n_pages + 1))
         
         ! 2. Safely transfer the node pointers to the temporary array
         do i = 1, sl%n_pages
            temp_pages(i)%nodes => sl%pages(i)%nodes
         end do
         
         ! 3. Move the allocation back to sl%pages 
         ! (move_alloc automatically handles deallocating the old sl%pages)
         call move_alloc(from=temp_pages, to=sl%pages)
      end if
      
      sl%n_pages = sl%n_pages + 1

      !--- allocate the node storage for the new page -----------------
      n = sl%page_size
      allocate(sl%pages(sl%n_pages)%nodes(n))

      !--- push every node of the page onto the LIFO free list --------
      do i = 1, n
         node => sl%pages(sl%n_pages)%nodes(i)
         node%data = 0
         
         if (allocated(node%forward)) deallocate(node%forward)
         allocate(node%forward(1))               ! one element will hold the link
         
         tmp => sl%free_list
         node%forward(1)%node => tmp             ! link to previous free node
         sl%free_list => node
      end do
   end subroutine allocate_new_page   

   !=================================================================
   !  SL_INIT - create a new empty skip list
   !=================================================================
   subroutine sl_init(sl, expected_capacity, max_levels)
      type(Skiplist), pointer, intent(out) :: sl
      integer,               intent(in)    :: expected_capacity
      integer,               intent(in), optional :: max_levels
      integer :: i

      allocate(sl)

      if (present(max_levels)) then
         if (max_levels < 1 .or. max_levels > MAX_LEVELS) &
            stop "SL_INIT: max_levels out of range"
         sl%max_levels = max_levels
      end if

      sl%prob      = DEFAULT_P
      sl%page_size = max(4096, expected_capacity)

      !--- allocate the first arena page (creates the free list) -----
      call allocate_new_page(sl)

      !--- create the sentinel head node -----------------------------
      allocate(sl%head)
      sl%head%data = -huge(0)                     ! sentinel payload
      
      allocate(sl%head%forward(sl%max_levels))
      do i = 1, sl%max_levels
         nullify(sl%head%forward(i)%node)         ! initialise to NULL()
      end do
   end subroutine sl_init

   !=================================================================
   !  Return a node to the arena (push onto free list)
   !=================================================================
   subroutine return_node_to_arena(sl, node)
      type(Skiplist), intent(inout) :: sl
      type(SkiplistNode), pointer   :: node

      if (.not. allocated(node%forward)) allocate(node%forward(1))
      node%forward(1)%node => sl%free_list
      sl%free_list => node
   end subroutine return_node_to_arena

   !=================================================================
   !  Take a fresh node from the arena, allocate its forward array,
   !  and initialise each element to NULL().
   !=================================================================
   subroutine take_node_from_arena(sl, level, node)
      type(Skiplist), intent(inout) :: sl
      integer,        intent(in)    :: level
      type(SkiplistNode), pointer   :: node
      integer                       :: i

      if (.not. associated(sl%free_list)) call allocate_new_page(sl)

      node => sl%free_list               ! pop from LIFO free list
      sl%free_list => node%forward(1)%node ! advance the head

      ! allocate forward array of the exact size we need
      if (allocated(node%forward)) then
         if (size(node%forward) /= level) then
            deallocate(node%forward)
            allocate(node%forward(level))
         end if
      else
         allocate(node%forward(level))
      end if

      ! initialise each forward pointer to NULL()
      do i = 1, level
         nullify(node%forward(i)%node)
      end do
   end subroutine take_node_from_arena

   !=================================================================
   !  SL_FIND - locate a key, fill the update(:) array with predecessor
   !  nodes on every level, and return the node that actually contains
   !  the key (or NULL() if absent).
   !=================================================================
   subroutine sl_find(sl, key, update, found_node)
      type(Skiplist), intent(in)               :: sl
      integer,        intent(in)               :: key
      type(NodePtr),  intent(inout)            :: update(:) ! Array of NodePtr
      type(SkiplistNode), pointer, intent(out) :: found_node

      type(SkiplistNode), pointer :: x, nxt
      integer                     :: i

      x => sl%head
      do i = sl%max_levels, 1, -1
         nxt => x%forward(i)%node
         
         ! Avoid Fortran's non-guaranteed short-circuit evaluation
         do while (associated(nxt))
            if (nxt%data >= key) exit
            x   => nxt
            nxt => x%forward(i)%node
         end do
         update(i)%node => x
      end do

      nxt => x%forward(1)%node
      if (associated(nxt)) then
         if (nxt%data == key) then
            found_node => nxt
         else
            found_node => null()
         end if
      else
         found_node => null()
      end if
   end subroutine sl_find

   !=================================================================
   !  SL_INSERT - insert a new key (duplicates are ignored)
   !=================================================================
   subroutine sl_insert(sl, key)
      type(Skiplist), intent(inout) :: sl
      integer,        intent(in)    :: key

      type(NodePtr), allocatable  :: update(:)
      type(SkiplistNode), pointer :: target
      type(SkiplistNode), pointer :: new_node
      integer                     :: node_level, i

      allocate(update(sl%max_levels))
      call sl_find(sl, key, update, target)

      if (associated(target)) then               ! duplicate → nothing to do
         deallocate(update)
         return
      end if

      node_level = random_level(sl)

      call take_node_from_arena(sl, node_level, new_node)
      new_node%data = key

      do i = 1, node_level
         new_node%forward(i)%node => update(i)%node%forward(i)%node
         update(i)%node%forward(i)%node => new_node
      end do

      deallocate(update)
   end subroutine sl_insert

   !=================================================================
   !  SL_DELETE - remove a key if it exists
   !=================================================================
   subroutine sl_delete(sl, key)
      type(Skiplist), intent(inout) :: sl
      integer,        intent(in)    :: key

      type(NodePtr), allocatable  :: update(:)
      type(SkiplistNode), pointer :: target
      integer                     :: i, lvl

      allocate(update(sl%max_levels))
      call sl_find(sl, key, update, target)

      if (.not. associated(target)) then
         deallocate(update)
         return
      end if

      lvl = size(target%forward)
      do i = 1, lvl
         if (associated(update(i)%node%forward(i)%node, target)) &
            update(i)%node%forward(i)%node => target%forward(i)%node
      end do

      call return_node_to_arena(sl, target)
      deallocate(update)
   end subroutine sl_delete

   !=================================================================
   !  SL_DESTROY - free everything (including arena pages)
   !=================================================================
   subroutine sl_destroy(sl)
      type(Skiplist), pointer, intent(inout) :: sl
      integer :: i, j

      if (.not. associated(sl)) return

      if (allocated(sl%pages)) then
         do i = 1, sl%n_pages
            if (associated(sl%pages(i)%nodes)) then
               do j = 1, size(sl%pages(i)%nodes)
                  if (allocated(sl%pages(i)%nodes(j)%forward)) &
                     deallocate(sl%pages(i)%nodes(j)%forward)
               end do
               deallocate(sl%pages(i)%nodes)
            end if
         end do
         deallocate(sl%pages)
      end if

      if (allocated(sl%head%forward)) deallocate(sl%head%forward)
      deallocate(sl%head)
      deallocate(sl)
   end subroutine sl_destroy

   !=================================================================
   !  Optional helper - print the list (level-1 chain)
   !=================================================================
   subroutine sl_print(sl)
      type(Skiplist), intent(in)  :: sl
      type(SkiplistNode), pointer :: cur
      integer :: cnt

      cur => sl%head%forward(1)%node
      cnt = 0
      print *, "Skip list (ascending order, level-1 chain):"
      do while (associated(cur))
         cnt = cnt + 1
         print *, cnt, cur%data
         cur => cur%forward(1)%node
      end do
   end subroutine sl_print

 end module generic_skiplist
 program test_skiplist
   use generic_skiplist
   implicit none

   type(Skiplist), pointer :: sl
   integer :: i

   call SL_INIT(sl, expected_capacity=2000, max_levels=12)

   do i = 1, 2000000
      call SL_INSERT(sl, i*10)
   end do

   call SL_INSERT(sl, 30)   ! duplicate – ignored
   call SL_DELETE(sl, 10)   ! present
   call SL_DELETE(sl, 150)  ! absent – harmless

   !call sl_print(sl)

   call SL_DESTROY(sl)
end program test_skiplist
