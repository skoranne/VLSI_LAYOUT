! program mainA
!   use MemoryAllocatorModule
!   implicit none

!   type(TwoLevelAllocator) :: my_pool
!   type(BlockType) :: temp_block
!   type(BlockType), pointer :: item_ptr
!   integer :: i,j

!   ! Setup allocator: Each chunk holds 100,000 blocks. Max 100 chunks (= 10 million items capacity)
!   call init_allocator(my_pool, chunk_size=100000, max_chunks=100)

!   ! Simulate inserting 2.5 million blocks
!   print *, "Populating allocator with 2,500,000 blocks..."
!   do i = 1, 2500000
!      do j = 1, DEQUE_BLOCK_SIZE
!         temp_block%coords(j) = j
!      end do
!      call allocate_element(my_pool, temp_block)
!   end do
!   print *, "Total elements managed:", my_pool%total_elements
!   print *, "Active chunks allocated:", my_pool%allocated_chunks

!   ! Retrieve a specific element using O(1) math mapping
!   item_ptr => get_element(my_pool, 1543210)

!   if (associated(item_ptr)) then
!      print *, "Verified Element #1543210: ", item_ptr%coords
!   end if

!   ! Release memory cleanly
!   call destroy_allocator(my_pool)
! end program mainA

program main
  use GenericAllocatorMod
  use GeometryModule
  implicit none
  integer(kind=8), parameter :: K_BLOCK_SIZE = 64
  integer(kind=8), parameter :: K_NODE_SIZE = 64  
  ! 4. Define BlockTypeA by extending the base class
  type, extends(GenericBlock) :: BlockTypeA
     integer(kind=4) :: slots(K_BLOCK_SIZE)
  end type BlockTypeA

  ! 5. Define GridTypeB by extending the base class
  type, extends(GenericBlock) :: GridTypeB
     type(Box) :: mbr(K_NODE_SIZE)
  end type GridTypeB

  ! Declare two separate allocators using the exact same module code
  type(TwoLevelAllocator) :: allocator_A
  type(TwoLevelAllocator) :: allocator_B

  ! Temporary concrete variables
  type(BlockTypeA) :: temp_A
  type(GridTypeB) :: temp_B

  ! Polymorphic pointers to safely point to retrieved elements
  class(GenericBlock), pointer :: generic_ptr
  !class(GenericBloc)           :: base_obj
  type(BlockTypeA), pointer    :: concrete_A_ptr
  type(GridTypeB), pointer     :: concrete_B_ptr
  integer :: i,j
  ! --- Test Allocator 1 (BlockTypeA) ---
  call init_allocator(allocator_A, chunk_size=256, max_chunks=256*256,sample_item=temp_A)
  do j=1,64
     temp_A%slots(j) = j
  end do
  do j=1,3000000
     call allocate_element(allocator_A, temp_A)
  end do
  
  print *, "--- Memory Sizes (Bytes) ---"
  !print *, "GenericBlock : ", storage_size(base_obj) / 8
  print *, "BlockTypeA   : ", storage_size(temp_A) / 8
  print *, "GridTypeB    : ", storage_size(temp_B) / 8
  ! --- Test Allocator 2 (GridTypeB) ---
  call init_allocator(allocator_B, chunk_size=500, max_chunks=10, sample_item=temp_B)
  do j=1,64
     call temp_B%mbr(j)%reset_to_infinity()
  end do
  call allocate_element(allocator_B, temp_B)

  ! --- Retrieve and Cast/Access Data safely using 'select type' ---
  generic_ptr => get_element(allocator_A, 1)

  select type (p => generic_ptr)
  type is (BlockTypeA)
     print *, "Retrieved from Allocator A (Coords):", p%slots(1:4)
  type is (GridTypeB)
     print *, "Retrieved from Allocator A (Grid entry 1,1):", p%mbr(1)
  end select

  generic_ptr => get_element(allocator_B, 1)

  select type (p => generic_ptr)
  type is (BlockTypeA)
     print *, "Retrieved from Allocator A (Coords):", p%slots(1:4)
  type is (GridTypeB)
     print *, "Retrieved from Allocator A (Grid entry 1,1):", p%mbr(1)
  end select

  ! Clean up memory allocations
  call destroy_allocator(allocator_A)
  call destroy_allocator(allocator_B)
end program main

