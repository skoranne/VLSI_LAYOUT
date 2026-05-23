!=====================================================================
! hash_table_mod.f90
!   Object-oriented hash table implementation: character(:) → integer
!   Separate chaining with dummy head nodes, djb2 hash function
!   Fortran 90/95 features only
!=====================================================================

module hash_table_mod
  implicit none
  private               ! hide everything, export only what we list below
  public :: HashTable
  !===================================================================
  ! 1.  Data structures
  !=====================================================

  ! Node type for linked list structure in hash buckets
  type :: node_type
     character(:), allocatable :: key   ! the string key
     integer                   :: val   ! the associated integer
     type(node_type), pointer  :: next  => null()
  end type node_type

  ! Hash table type containing all necessary state information
  ! Uses dummy head design for simplified list operations
  type :: HashTable
     integer                     :: nbuckets   ! number of hash buckets
     type(node_type), pointer    :: bucket(:) => null()   ! array of dummy head nodes
     integer                     :: nitems = 0   ! how many entries stored
     
     ! Type procedures (methods)
     contains
        procedure, pass :: init
        procedure, pass :: add
        procedure, pass :: put
        procedure, pass :: get
        procedure, pass :: remove
        procedure, pass :: clear
        procedure, pass :: num_items
        procedure, pass :: is_empty
  end type HashTable

  !===================================================================
  ! 2.  Public interface with type procedures
  !===================================================================

contains

  !-------------------------------------------------------------------
  !>  Initialize a hash table with the requested number of buckets.
  !>  The number of buckets is rounded up to the next power of two
  !>  (helps with the modulo operation).
  !>  @param ht Hash table to initialize
  !>  @param nbuckets Desired number of buckets (will be rounded up to power of 2)
  !-------------------------------------------------------------------
  subroutine init(ht, nbuckets)
    class(HashTable), intent(out) :: ht
    integer,          intent(in)  :: nbuckets
    integer :: i, nb

    ! Round up nbuckets to next power of two for distribution
    nb = 1
    do while (nb < nbuckets)
       nb = nb*2
    end do
    ht%nbuckets = nb
    ht%nitems   = 0

    ! Allocate bucket array with dummy head nodes
    allocate ( ht%bucket(nb) )
    ! Note: Dummy head nodes are automatically initialized with next => null()
  end subroutine init

  !-------------------------------------------------------------------
  !>  Add a (key,value) pair to the hash table.
  !>  If the key already exists, its value is overwritten.
  !>  @param ht Hash table to add to
  !>  @param key Character string key
  !>  @param val Integer value to associate with key
  !-------------------------------------------------------------------
  subroutine add(ht, key, val)
    class(HashTable), intent(inout) :: ht
    character(*),     intent(in)    :: key
    integer,          intent(in)    :: val

    call put(ht, key, val)
  end subroutine add

  !-------------------------------------------------------------------
  !>  Insert a (key,value) pair into the hash table.
  !>  If the key already exists, its value is overwritten.
  !>  @param ht Hash table to insert into
  !>  @param key Character string key
  !>  @param val Integer value to associate with key
  !>  @param status Optional logical output indicating .true. on insert, .false. on update
  !-------------------------------------------------------------------
  subroutine put(ht, key, val, status)
    class(HashTable), intent(inout) :: ht
    character(*),     intent(in)    :: key
    integer,          intent(in)    :: val
    logical, optional, intent(out)  :: status   ! .true. on insert, .false. on update

    integer               :: bucket_idx
    type(node_type), pointer :: cur, prev

    ! Calculate bucket index using hash function
    bucket_idx = hash_fun(key, ht%nbuckets) + 1   ! Fortran arrays are 1-based

    ! Start traversal from dummy head node
    cur => ht%bucket(bucket_idx)%next
    prev => null()

    ! Walk the chain looking for existing key
    do while ( associated(cur) )
       if ( cur%key == key ) exit
       prev => cur
       cur  => cur%next
    end do

    if ( associated(cur) ) then
       ! Key already present value
       cur%val = val
       if ( present(status) ) status = .false.
    else
       ! New entry - allocate node and insert at head of chain
       allocate ( cur )
       allocate ( character(len=len_trim(key)) :: cur%key )
       cur%key = key
       cur%val = val
       cur%next => ht%bucket(bucket_idx)%next
       ht%bucket(bucket_idx)%next => cur
       ht%nitems = ht%nitems + 1
       if ( present(status) ) status = .true.
    end if
  end subroutine put

  !-------------------------------------------------------------------
  !>  Retrieve the integer associated with a key.
  !>  Returns .true. in found if the key exists, otherwise .false
  !>  If key is not found, val is not modified.
  !>  @param ht Hash table to search
  !>  @param key Character string key to search for
  !>  @param val Output integer value associated with key (only valid if found)
  !>  @param found Output logical indicating whether key was found
  !-------------------------------------------------------------------
  subroutine get(ht, key, val, found)
    class(HashTable), intent(in)  :: ht
    character(*),     intent(in)  :: key
    integer,          intent(out) :: val
    logical,          intent(out) :: found

    integer               :: bucket_idx
    type(node_type), pointer :: cur

    ! Calculate bucket index using hash function
    bucket_idx = hash_fun(key,ht%nbuckets) + 1
    cur => ht%bucket(bucket_idx)%next

    ! Search through chain for key
    do while ( associated(cur) )
       if ( cur%key == key ) then
          val   = cur%val
          found = .true.
          return
       end if
       cur => cur%next
    end do

    found = .false.       ! not in table
  end subroutine get

  !-------------------------------------------------------------------
  !>  Remove key from the hash table.
  !>  If the key does not exist, the routine does nothing.
  !>  @param ht Hash table to remove from
  !>  @param key Character string key to remove
  !>  @param removed Output key was removed
  !-------------------------------------------------------------------
  subroutine remove(ht, key, removed)
    class(HashTable), intent(inout) :: ht
    character(*),     intent(in)    :: key
    logical,          intent(out)   :: removed

    integer               :: bucket_idx
    type(node_type), pointer :: cur, prev

    ! Calculate bucket index using hash function
    bucket_idx = hash_fun(key, ht%nbuckets) + 1
    cur => ht%bucket(bucket_idx)%next
    prev => null()

    ! Search for key in chain
    do while ( associated(cur) )
       if ( cur%key == key ) exit
       prev => cur
       cur  => cur%next
    end do

    if ( .not. associated(cur) ) then
       removed = .false.
       return
    end if

    ! Unlink the node from chain
    if ( associated(prev) ) then
       prev%next => cur%next
    else
       ht%bucket(bucket_idx)%next => cur%next
    end if

    ! Deallocate node memory (key is an allocatable component)
    if ( allocated(cur%key) ) deallocate (cur%key)
    deallocate (cur)
    ht%nitems = ht%nitems - 1
    removed = .true.
  end subroutine remove

  !-------------------------------------------------------------------
  !>  Clear all entries from the hash table.
  !>  Resets the table to empty state without.
  !>  @param ht Hash table to clear
  !-------------------------------------------------------------------
  subroutine clear(ht)
    class(HashTable), intent(inout) :: ht
    integer :: i
    type(node_type), pointer :: cur, nxt

    ! Traverse all buckets and free their nodes
    do i = 1, ht%nbuckets
       cur => ht%bucket(i)%next
       do while ( associated(cur) )
          nxt => cur%next
          if ( allocated(cur%key) ) deallocate (cur%key)
          deallocate (cur)
          cur => nxt
       end do
       ht%bucket(i)%next => null()
    end do

    ht%nitems=0
  end subroutine clear

  !-------------------------------------------------------------------
  !>  Return the number of stored items (convenient for diagnostics).
  !>  @param ht Hash table to query
  !>  @return Number of items currently stored
  !-------------------------------------------------------------------
  pure integer function num_items(ht)
    class(HashTable), intent(in) :: ht
    num_items = ht%nitems
  end function num_items

  !-------------------------------------------------------------------
  !>  Check if the hash table is empty.
  !>  Returns .true. if no items are stored, .false. otherwise.  @param ht Hash table to check
  !>  @return Logical indicating whether table is empty
  !-------------------------------------------------------------------
  function is_empty(ht)
    class(HashTable), intent(in) :: ht
    logical                       :: is_empty
    is_empty = (ht%nitems == 0)
  end function is_empty

  !-------------------------------------------------------------------
  !>  Destroy the hash table – free all memory.
  !>  This should be called when the hash table is no longer needed.
  !>  After calling this, the hash table is in an invalid state and must be
  !>  reinitialized using init before
  !>  @param ht Hash table to destroy
  !-------------------------------------------------------------------
  subroutine destroy(ht)
    class(HashTable), intent(inout) :: ht
    integer :: i
    type(node_type), pointer :: cur, nxt

    ! Traverse all buckets and free their nodes
    do i = 1, ht%nbuckets
       cur => ht%bucket(i)%next
       do while ( associated(cur) )
          nxt => cur%next
          if ( allocated(cur%key) ) deallocate (cur%key)
          deallocate (cur)
          cur => nxt
       end do
       ht%bucket(i)%next => null()
    end do
    ! Deallocate bucket array
    if( associated(ht%bucket) ) deallocate (ht%bucket)
    ht%nbuckets = 0
    ht%nitems   = 0
  end subroutine destroy

  !-------------------------------------------------------------------
  !>  djb2 hash function – works on any length character string.
  !>  Returns a positive integer in the range [0, nbuckets-1].
  !>  This implementation uses the classic djb2 algorithm by Daniel J. Bernstein
  !>  which provides excellent distribution properties for typical input data.
  !>  @param key Input character string to hash>  @param nbuckets Number of buckets in hash table
  !>  @return Hash value in range [0, nbuckets-1]
  !-------------------------------------------------------------------
  pure function hash_fun(key, nbuckets) result(h)
    character(*), intent(in) :: key
    integer,      intent(in) :: nbuckets
    integer                  :: h
    integer                  :: i, c

    h = 5381
    do i = 1, len_trim(key)
       c = iachar(key(i:i))
       h = mod( (h*33) + c, nbuckets)
    end do
  end function hash_fun

end module hash_table_mod
