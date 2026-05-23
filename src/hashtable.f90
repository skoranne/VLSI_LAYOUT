!=====================================================================
! hash_mod.f90
!   a small, generic hash table:  character(:)  →  integer
!   separate chaining, djb2 hash, fortran 90/95 features only
!=====================================================================
module hash_mod
  implicit none
  private               ! hide everything, export only what we list below
  public :: hash_type, hash_create, hash_put, hash_get, hash_remove, &
            hash_destroy, hash_nitems

  !===================================================================
  ! 1.  data structures
  !===================================================================
  type :: node_type
     character(:), allocatable :: key   ! the string key
     integer                   :: val   ! the associated integer
     type(node_type), pointer  :: next  => null()
  end type node_type

  ! here we use the dummy head design
  ! Each bucket in the hash table contains a dummy head node (sentinel)
  ! This eliminates special case handling for empty buckets or first element insertion/deletion
  ! The dummy head always exists, providing a consistent starting point for list traversal
  ! Simplifies insertion and deletion logic by removing
  ! Makes the code more uniform and less error-prone
  
  type :: hash_type
     integer                     :: nbuckets   ! number of hash buckets
     type(node_type), pointer    :: bucket(:) => null()   ! array of heads
     integer                     :: nitems = 0   ! how many entries stored
  end type hash_type

  !===================================================================
  ! 2.  public interface
  !===================================================================
contains

  !-------------------------------------------------------------------
  !>  initialise a hash table with the requested number of buckets.
  !>  the number of buckets is rounded up to the next power of two
  !>  (helps with the modulo operation).
  !-------------------------------------------------------------------
  subroutine hash_create (ht, nbuckets)
    type(hash_type), intent(out) :: ht
    integer,         intent(in)  :: nbuckets
    integer :: i, nb

    ! ---- make nb a power‑of‑two >= nbuckets (optional, but cheap)
    nb = 1
    do while (nb < nbuckets)
       nb = nb*2
    end do
    ht%nbuckets = nb
    ht%nitems   = 0

    allocate ( ht%bucket(nb) )
    !do i = 1, nb
    !   ht%bucket(i)%next => null()
    !end do
  end subroutine hash_create


  !-------------------------------------------------------------------
  !>  djb2 hash function – works on any length character string.
  !>  result is a positive integer in the range [0, nbuckets‑1].
  !-------------------------------------------------------------------
  pure function hash_fun (key, nbuckets) result (h)
    character(*), intent(in) :: key
    integer,      intent(in) :: nbuckets
    integer                  :: h
    integer                  :: i, c

    h = 5381
    do i = 1, len_trim(key)
       c = iachar(key(i:i))
       h = mod( (h*33) + c, nbuckets )
    end do
  end function hash_fun


  !-------------------------------------------------------------------
  !>  insert a (key,value) pair.
  !>  if the key already exists, its value is overwritten.
  !-------------------------------------------------------------------
  subroutine hash_put (ht, key, val, status)
    type(hash_type), intent(inout) :: ht
    character(*),    intent(in)    :: key
    integer,         intent(in)    :: val
    logical, optional, intent(out) :: status   ! .true. on insert, .false. on update

    integer               :: bucket_idx
    type(node_type), pointer :: cur, prev

    bucket_idx = hash_fun (key, ht%nbuckets) + 1   ! fortran arrays are 1‑based

    cur => ht%bucket(bucket_idx)%next
    prev => null()

    ! walk the chain looking for the key
    do while ( associated(cur) )
       if ( cur%key == key ) exit
       prev => cur
       cur  => cur%next
    end do

    if ( associated(cur) ) then
       ! ----- key already present → overwrite -----
       cur%val = val
       if ( present(status) ) status = .false.
    else
       ! ----- new entry: allocate a node and stick it at the head -----
       allocate ( cur )
       allocate ( character(len=len_trim(key)) :: cur%key )
       cur%key = key
       cur%val = val
       cur%next => ht%bucket(bucket_idx)%next
       ht%bucket(bucket_idx)%next => cur
       ht%nitems = ht%nitems + 1
       if ( present(status) ) status = .true.
    end if
  end subroutine hash_put


  !-------------------------------------------------------------------
  !>  retrieve the integer associated with a key.
  !>  returns .true. in found if the key exists, otherwise .false.
  !-------------------------------------------------------------------
  subroutine hash_get (ht, key, val, found)
    type(hash_type), intent(in)  :: ht
    character(*),    intent(in)  :: key
    integer,         intent(out) :: val
    logical,         intent(out) :: found

    integer               :: bucket_idx
    type(node_type), pointer :: cur

    bucket_idx = hash_fun (key, ht%nbuckets) + 1
    cur => ht%bucket(bucket_idx)%next

    do while ( associated(cur) )
       if ( cur%key == key ) then
          val   = cur%val
          found = .true.
          return
       end if
       cur => cur%next
    end do

    found = .false.       ! not in table
  end subroutine hash_get


  !-------------------------------------------------------------------
  !>  remove a key from the table.
  !>  if the key does not exist, the routine does nothing.
  !-------------------------------------------------------------------
  subroutine hash_remove (ht, key, removed)
    type(hash_type), intent(inout) :: ht
    character(*),    intent(in)    :: key
    logical,         intent(out)   :: removed

    integer               :: bucket_idx
    type(node_type), pointer :: cur, prev

    bucket_idx = hash_fun (key, ht%nbuckets) + 1
    cur => ht%bucket(bucket_idx)%next
    prev => null()

    do while ( associated(cur) )
       if ( cur%key == key ) exit
       prev => cur
       cur  => cur%next
    end do

    if ( .not. associated(cur) ) then
       removed = .false.
       return
    end if

    ! unlink the node
    if ( associated(prev) ) then
       prev%next => cur%next
    else
       ht%bucket(bucket_idx)%next => cur%next
    end if

    ! deallocate the node (key is an allocatable component)
    if ( allocated(cur%key) ) deallocate (cur%key)
    deallocate (cur)
    ht%nitems = ht%nitems - 1
    removed = .true.
  end subroutine hash_remove


  !-------------------------------------------------------------------
  !>  return the number of stored items (convenient for diagnostics).
  !-------------------------------------------------------------------
  function hash_nitems (ht) result (n)
    type(hash_type), intent(in) :: ht
    integer                     :: n
    n = ht%nitems
  end function hash_nitems


  !-------------------------------------------------------------------
  !>  destroy the hash table – free all memory.
  !-------------------------------------------------------------------
  subroutine hash_destroy (ht)
    type(hash_type), intent(inout) :: ht
    integer :: i
    type(node_type), pointer :: cur, nxt

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

    if( associated(ht%bucket) ) deallocate (ht%bucket)
    ht%nbuckets = 0
    ht%nitems   = 0
  end subroutine hash_destroy

end module hash_mod

