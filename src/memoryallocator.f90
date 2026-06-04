! File   : memoryallocator.f90
! Author : Sandeep Koranne (C) All rights reserved.
! block allocation in fortran for a type containing fixed size array of integers;
! I will potentially allocate million of these blocks can I implement a two level
! block allocator in Fortran. Please show me the code

module GenericAllocatorMod
    implicit none
    private
    
    public :: GenericBlock, ChunkType, TwoLevelAllocator, ElementContainer
    public :: init_allocator, allocate_element, get_element, destroy_allocator

    ! 1. The Abstract Base Class, derive your object from this to allocate it
    type, abstract :: GenericBlock
    end type GenericBlock

    ! FIX: Wrapper container holding a polymorphic allocatable entity
    type :: ElementContainer
        class(GenericBlock), allocatable :: item
    end type ElementContainer

    ! Level 1 Chunk: Houses standard Fortran pointer arrays targeting containers
    type :: ChunkType
        type(ElementContainer), pointer, dimension(:) :: elements => null()
    end type ChunkType

    ! Level 2 Master: Dynamically allocates the continuous concrete blocks
    type :: TwoLevelAllocator
        integer :: chunk_size       = 0
        integer :: max_chunks       = 0
        integer :: total_elements   = 0
        integer :: allocated_chunks = 0
        type(ChunkType), allocatable, dimension(:) :: chunk_list
        
        ! Fully resolved storage pool tracking standard containers
        type(ElementContainer), allocatable, dimension(:) :: storage_pool
    end type TwoLevelAllocator

contains

    subroutine init_allocator(alloc, chunk_size, max_chunks, sample_item)
        type(TwoLevelAllocator), intent(out) :: alloc
        integer, intent(in) :: chunk_size, max_chunks
        class(GenericBlock), intent(in) :: sample_item
        integer :: total_capacity, i

        alloc%chunk_size = chunk_size
        alloc%max_chunks = max_chunks
        alloc%total_elements = 0
        alloc%allocated_chunks = 0
        
        total_capacity = chunk_size * max_chunks
        
        ! Allocate container spaces cleanly
        allocate(alloc%storage_pool(total_capacity))
        
        ! Pre-mold internal slots dynamically with the shape copy rule
        do i = 1, total_capacity
            allocate(alloc%storage_pool(i)%item, source=sample_item)
        end do
        
        allocate(alloc%chunk_list(max_chunks))
    end subroutine init_allocator

    subroutine allocate_element(alloc, item)
        type(TwoLevelAllocator), intent(inout) :: alloc
        class(GenericBlock), intent(in) :: item
        integer :: target_chunk, local_idx, storage_idx

        alloc%total_elements = alloc%total_elements + 1
        target_chunk = (alloc%total_elements - 1) / alloc%chunk_size + 1
        local_idx    = mod(alloc%total_elements - 1, alloc%chunk_size) + 1
        storage_idx  = alloc%total_elements

        if (target_chunk > alloc%max_chunks) then
            print *, "Error: Exceeded maximum generic block allocator capacity."
            error stop
        end if

        ! Safe pointer array initialization matching the targets
        if (target_chunk > alloc%allocated_chunks) then
            allocate(alloc%chunk_list(target_chunk)%elements(alloc%chunk_size))
            alloc%allocated_chunks = target_chunk
        end if

        ! FIX #8304: Poly-allocatables support direct assignment cleanly via source clone
        if (allocated(alloc%storage_pool(storage_idx)%item)) deallocate(alloc%storage_pool(storage_idx)%item)
        allocate(alloc%storage_pool(storage_idx)%item, source=item)
        
        ! FIX #8524: Standard pointer-to-target mapping via absolute container references
        alloc%chunk_list(target_chunk)%elements(local_idx) = alloc%storage_pool(storage_idx)
    end subroutine allocate_element

    ! Extracted pointer returns the underlying nested data entity directly
    function get_element(alloc, global_idx) result(ptr)
        type(TwoLevelAllocator), intent(in), target :: alloc
        integer, intent(in) :: global_idx
        class(GenericBlock), pointer :: ptr
        integer :: target_chunk, local_idx

        if (global_idx < 1 .or. global_idx > alloc%total_elements) then
            ptr => null()
            return
        end if

        target_chunk = (global_idx - 1) / alloc%chunk_size + 1
        local_idx    = mod(global_idx - 1, alloc%chunk_size) + 1

        ! Point direct reference to item nested in container structure
        ptr => alloc%chunk_list(target_chunk)%elements(local_idx)%item
    end function get_element

    subroutine destroy_allocator(alloc)
        type(TwoLevelAllocator), intent(inout) :: alloc
        integer :: i

        do i = 1, alloc%allocated_chunks
            if (associated(alloc%chunk_list(i)%elements)) then
                deallocate(alloc%chunk_list(i)%elements)
            end if
        end do
        if (allocated(alloc%chunk_list)) deallocate(alloc%chunk_list)
        if (allocated(alloc%storage_pool)) deallocate(alloc%storage_pool)
        alloc%total_elements = 0
        alloc%allocated_chunks = 0
    end subroutine destroy_allocator

end module GenericAllocatorMod

module MemoryAllocatorModule
  implicit none
  private
  public :: BlockType, ChunkType, TwoLevelAllocator, &
       init_allocator, allocate_element, get_element, destroy_allocator, DEQUE_BLOCK_SIZE
  integer(kind=8), parameter :: DEQUE_BLOCK_SIZE = 16

  ! 1. Define your custom type with a fixed-size integer array
  type :: BlockType
     integer, dimension(DEQUE_BLOCK_SIZE) :: coords  ! Fixed-size array
  end type BlockType

  ! 2. Level 1: The contiguous chunk container
  type :: ChunkType
     type(BlockType), allocatable, dimension(:) :: elements
  end type ChunkType

  ! 3. Level 2: The Master Allocator containing pointers to chunks
  type :: TwoLevelAllocator
     integer :: chunk_size       = 0
     integer :: max_chunks       = 0
     integer :: total_elements   = 0
     integer :: allocated_chunks = 0
     type(ChunkType), allocatable, dimension(:) :: chunk_list
  end type TwoLevelAllocator

contains

  ! Initialize the master structure
  subroutine init_allocator(alloc, chunk_size, max_chunks)
    type(TwoLevelAllocator), intent(out) :: alloc
    integer, intent(in) :: chunk_size, max_chunks

    alloc%chunk_size = chunk_size
    alloc%max_chunks = max_chunks
    alloc%total_elements = 0
    alloc%allocated_chunks = 0

    ! Allocate the top-level array of chunks
    allocate(alloc%chunk_list(max_chunks))
  end subroutine init_allocator

  ! Add an element and automatically spawn a new chunk if needed
  subroutine allocate_element(alloc, item)
    type(TwoLevelAllocator), intent(inout) :: alloc
    type(BlockType), intent(in) :: item
    integer :: target_chunk, local_idx

    ! Calculate where the next element belongs
    alloc%total_elements = alloc%total_elements + 1
    target_chunk = (alloc%total_elements - 1) / alloc%chunk_size + 1
    local_idx    = mod(alloc%total_elements - 1, alloc%chunk_size) + 1

    ! Safety check for maximum bounds
    if (target_chunk > alloc%max_chunks) then
       print *, "Error: Exceeded maximum block allocator capacity."
       error stop
    end if

    ! Level 2 Dynamic Growth: Allocate chunk on demand if it doesn't exist
    if (target_chunk > alloc%allocated_chunks) then
       allocate(alloc%chunk_list(target_chunk)%elements(alloc%chunk_size))
       alloc%allocated_chunks = target_chunk
    end if

    ! Insert the item contiguously
    alloc%chunk_list(target_chunk)%elements(local_idx) = item
  end subroutine allocate_element

  ! Read/Write access to an element via pointer with O(1) mathematical lookup
  function get_element(alloc, global_idx) result(ptr)
    type(TwoLevelAllocator), intent(in), target :: alloc
    integer, intent(in) :: global_idx
    type(BlockType), pointer :: ptr
    integer :: target_chunk, local_idx

    if (global_idx < 1 .or. global_idx > alloc%total_elements) then
       ptr => null()
       return
    end if

    target_chunk = (global_idx - 1) / alloc%chunk_size + 1
    local_idx    = mod(global_idx - 1, alloc%chunk_size) + 1

    ptr => alloc%chunk_list(target_chunk)%elements(local_idx)
  end function get_element

  ! Clean up all allocations cleanly
  subroutine destroy_allocator(alloc)
    type(TwoLevelAllocator), intent(inout) :: alloc
    integer :: i

    do i = 1, alloc%allocated_chunks
       if (allocated(alloc%chunk_list(i)%elements)) then
          deallocate(alloc%chunk_list(i)%elements)
       end if
    end do
    if (allocated(alloc%chunk_list)) deallocate(alloc%chunk_list)
    alloc%total_elements = 0
    alloc%allocated_chunks = 0
  end subroutine destroy_allocator

end module MemoryAllocatorModule
