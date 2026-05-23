MODULE hash_mod_ptrhead
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: hash_type, hash_create, hash_put, hash_get, hash_remove, &
            hash_destroy, hash_nitems

  !-----------------------------------------------------------------
  ! 1.  Node type – unchanged, holds the real data
  !-----------------------------------------------------------------
  TYPE :: node_type
     CHARACTER(:), ALLOCATABLE :: key   ! key string
     INTEGER                   :: val   ! associated integer
     TYPE(node_type), POINTER  :: next  => NULL()
  END TYPE node_type

  !-----------------------------------------------------------------
  ! 2.  Hash table type – now an array of POINTERS to the first node
  !-----------------------------------------------------------------
  TYPE :: hash_type
     INTEGER                     :: nbuckets   ! number of buckets
     TYPE(node_type), POINTER    :: bucket(:) => NULL()   ! heads
     INTEGER                     :: nitems = 0
  END TYPE hash_type

CONTAINS

  !=================================================================
  !  Create an empty table
  !=================================================================
  SUBROUTINE hash_create (ht, nbuckets_requested)
    TYPE(hash_type), INTENT(OUT) :: ht
    INTEGER,         INTENT(IN)  :: nbuckets_requested
    INTEGER :: nb, i

    ! round up to a power of two (optional, but convenient)
    nb = 1
    DO WHILE (nb < nbuckets_requested)
       nb = nb*2
    END DO
    ht%nbuckets = nb
    ht%nitems   = 0

    ALLOCATE ( ht%bucket(nb) )
  END SUBROUTINE hash_create


  !=================================================================
  !  Hash function – djb2 (unchanged)
  !=================================================================
  PURE FUNCTION hash_fun (key, nbuckets) RESULT (h)
    CHARACTER(*), INTENT(IN) :: key
    INTEGER,      INTENT(IN) :: nbuckets
    INTEGER                  :: h, i, c

    h = 5381
    DO i = 1, LEN_TRIM(key)
       c = ichar(key(i:i))
       h = ishft(h,5) + h + c               ! h*33 + c
    END DO
    h = MOD(ABS(h), nbuckets) + 1           ! 1‑based index
  END FUNCTION hash_fun


  !=================================================================
  !  Insert / replace a key/value pair
  !=================================================================
  SUBROUTINE hash_put (ht, key, val)
    TYPE(hash_type), INTENT(INOUT) :: ht
    CHARACTER(*),    INTENT(IN)    :: key
    INTEGER,         INTENT(IN)    :: val
    INTEGER                     :: idx
    TYPE(node_type), POINTER    :: cur, prev, new_node

    idx = hash_fun(key, ht%nbuckets)

    cur => ht%bucket(idx)                  ! first real node (or null)

    !---------------------------------------------------------------
    ! Walk the chain looking for an existing key
    !---------------------------------------------------------------
    DO WHILE (ASSOCIATED(cur))
       IF (cur%key == key) THEN
          cur%val = val                     ! replace value, done
          RETURN
       END IF
       prev => cur
       cur  => cur%next
    END DO

    !---------------------------------------------------------------
    ! No existing key – prepend a new node
    !---------------------------------------------------------------
    ALLOCATE (new_node)
    new_node%key = key
    new_node%val = val
    new_node%next => ht%bucket(idx)        ! old head (may be null)

    ht%bucket(idx)%next => new_node            ! new node becomes the head
    ht%nitems = ht%nitems + 1
  END SUBROUTINE hash_put


  !=================================================================
  !  Retrieve a value (returns .FALSE. if key not present)
  !=================================================================
  FUNCTION hash_get (ht, key, value) RESULT (found)
    TYPE(hash_type), INTENT(IN)  :: ht
    CHARACTER(*),    INTENT(IN)  :: key
    INTEGER,         INTENT(OUT) :: value
    LOGICAL                      :: found
    INTEGER :: idx
    TYPE(node_type), POINTER :: cur

    idx = hash_fun(key, ht%nbuckets)
    cur => ht%bucket(idx)

    found = .FALSE.
    DO WHILE (ASSOCIATED(cur))
       IF (cur%key == key) THEN
          value = cur%val
          found = .TRUE.
          EXIT
       END IF
       cur => cur%next
    END DO
  END FUNCTION hash_get


  !=================================================================
  !  Delete a key (does nothing if key absent)
  !=================================================================
  SUBROUTINE hash_remove (ht, key)
    TYPE(hash_type), INTENT(INOUT) :: ht
    CHARACTER(*),    INTENT(IN)    :: key
    INTEGER :: idx
    TYPE(node_type), POINTER :: cur, prev, nxt

    idx = hash_fun(key, ht%nbuckets)

    cur => ht%bucket(idx)          ! first node (or NULL)
    prev => NULL()

    DO WHILE (ASSOCIATED(cur))
       IF (cur%key == key) EXIT
       prev => cur
       cur  => cur%next
    END DO

    IF (.NOT. ASSOCIATED(cur)) RETURN   ! key not found

    nxt => cur%next
    IF (ASSOCIATED(prev)) THEN
       prev%next => nxt                 ! unlink from middle / tail
    ELSE
       ht%bucket(idx)%next => nxt            ! we removed the head node
    END IF
    DEALLOCATE (cur)
    ht%nitems = ht%nitems - 1
  END SUBROUTINE hash_remove


  !=================================================================
  !  Number of stored elements (unchanged)
  !=================================================================
  FUNCTION hash_nitems (ht) RESULT (n)
    TYPE(hash_type), INTENT(IN) :: ht
    INTEGER                     :: n
    n = ht%nitems
  END FUNCTION hash_nitems


  !=================================================================
  !  Destroy the table – walk each chain and deallocate nodes
  !=================================================================
  SUBROUTINE hash_destroy (ht)
    TYPE(hash_type), INTENT(INOUT) :: ht
    INTEGER :: i
    TYPE(node_type), POINTER :: cur, nxt

    DO i = 1, ht%nbuckets
       cur => ht%bucket(i)
       DO WHILE (ASSOCIATED(cur))
          nxt => cur%next
          DEALLOCATE (cur)
          cur => nxt
       END DO
    END DO
    DEALLOCATE (ht%bucket)
    ht%nbuckets = 0
    ht%nitems   = 0
  END SUBROUTINE hash_destroy

END MODULE hash_mod_ptrhead
