! File    : datastructures.f90
! Author  : Sandeep Koranne (C) All rights.reserved.
! Purpose : Implementation of commonly used data structures
!         : hash table already implemented, this file has
!         : ring-buffer, LRU, union-find, graph, tree

module DataStructuresModule
  implicit none
  private
  public :: RingBuffer, UnionFind, LRU
  type :: LRU
  end type LRU
  !======================================================================
  !  Generic ring‑buffer (circular queue) for modern Fortran
  !======================================================================

  !===================================================================
  !  Parameterised derived type
  !    T – element type
  !    N – maximum number of elements (capacity)
  !===================================================================
  type :: BufferData
     integer :: my_value 
     ! Add your actual data fields here
  end type BufferData  
  type :: RingBuffer(N)
     !--- type parameters -------------------------------------------------
     integer, len :: N                     ! capacity (value parameter)
     !--- data storage ----------------------------------------------------
     !> later we will use fypp here
     type(integer), allocatable :: data(:)       ! circular storage, size = N
     !--- bookkeeping -----------------------------------------------------
     integer :: head = 1      ! index of the element that will be read next
     integer :: tail = 1      ! index where the next element will be written
     integer :: count = 0     ! current number of stored elements
   contains
     procedure, pass :: init   => rb_init
     procedure, pass :: push   => rb_push
     procedure, pass :: pop    => rb_pop
     procedure, pass :: empty  => rb_empty
     procedure, pass :: full   => rb_full
     procedure, pass :: size   => rb_size
     procedure, pass :: capacity => rb_capacity
  end type RingBuffer

  !===================================================================
  !  Type‑bound procedures (implementation follows the type definition)
  !===================================================================
  !==================================================================
  !  Derived type – the Union‑Find container
  !==================================================================
  type :: UnionFind
     integer, allocatable :: arr(:) ! parent array, 0 ⇒ element is a singleton
   contains
     procedure, pass :: init   => uf_init
     procedure, pass :: root   => uf_root   ! pure – no side‑effects
     procedure, pass :: merge  => uf_merge
     procedure, pass :: reduce => uf_reduce
     procedure, pass :: insert => uf_insert ! for singletons, arr(i)=i
  end type UnionFind

contains

  !-------------------------------------------------------------------
  !  Initialise the buffer – allocate the storage and reset counters
  !-------------------------------------------------------------------
  subroutine rb_init (self)
    class(RingBuffer(*)), intent(inout) :: self
    if (allocated(self%data)) deallocate(self%data)
    allocate(self%data(self%N))          ! N is the capacity
    self%head  = 1
    self%tail  = 1
    self%count = 0
  end subroutine rb_init

  !-------------------------------------------------------------------
  !  Push a value onto the tail of the buffer
  !  STAT = 0  → success
  !  STAT = → buffer already full (value not stored)
  !-------------------------------------------------------------------
  subroutine rb_push (self, val, stat)
    class(RingBuffer(*)), intent(inout) :: self
    type(integer),     intent(in)    :: val
    integer,              intent(out)   :: stat

    if (self%count == self%N) then          ! buffer full ?
       stat = 1
       return
    end if

    self%data(self%tail) = val
    self%tail = modulo(self%tail, self%N) + 1   ! wrap‑around
    self%count = self%count + 1
    stat = 0
  end subroutine rb_push

  !-------------------------------------------------------------------
  !  Pop a value from the head of the buffer
  !  STAT = 0  → success, VAL contains the element
  !  STAT = 1  → buffer empty (VALUE untouched)
  !-------------------------------------------------------------------
  subroutine rb_pop (self, val, stat)
    class(RingBuffer(*)), intent(inout) :: self
    type(integer),     intent(out)   :: val
    integer,              intent(out)   :: stat

    if (self%count == 0) then               ! buffer empty ?
       stat = 1
       return
    end if

    val = self%data(self%head)
    self%head = modulo(self%head, self%N) + 1   ! wrap‑around
    self%count = self%count - 1
    stat = 0
  end subroutine rb_pop

  !-------------------------------------------------------------------
  !  Query functions
  !-------------------------------------------------------------------
  pure function rb_empty (self) result(is_empty)
    class(RingBuffer(*)), intent(in) :: self
    logical :: is_empty
    is_empty = (self%count == 0)
  end function rb_empty

  pure function rb_full (self) result(is_full)
    class(RingBuffer(*)), intent(in) :: self
    logical :: is_full
    is_full = (self%count == self%N)
  end function rb_full
  
  pure function rb_size (self) result(cur_size)
    class(RingBuffer(*)), intent(in) :: self
    integer :: cur_size
    cur_size = self%count
  end function rb_size

  pure function rb_capacity (self) result(cap)
    class(RingBuffer(*)), intent(in) :: self
    integer :: cap
    cap = self%N
  end function rb_capacity

  !> UnionFind with path compression
  !> We use UnionFind to merge sets where each integer is coming
  !> from a apriori known UPPER_MAX; initially arr(i) = 0, for all i
  !> functions supported are root(x), merge(x,y)
  !> once we have merged everything, we can count |s| membership, for all i
  !> say this is K, then for all root, eg i != 0 (which are singleton), we
  !> MEMBERSHIP_TABLE[i+1]-MEMBERSHIP_TABLE[i] = K and
  !> MEMBERSHIP_TABLE[i] -> MEMBERS table which will contain all members
  !> of the set whose root is i.
  !> if arr(i) == 0 => i is a singleton and root(i) is i
  !> if arr(i) != 0 and arr(i) == i => i is the root of this set (|s|>1)
  !> if arr(i) != 0 and arr(i) != i => arr(i) is the root of this set

  !==================================================================
  !  Initialise the structure.
  !  MAX_N – maximum number of elements that can be stored.
  !  After initialisation arr(i) == 0 for all i its own set.
  !==================================================================
  subroutine uf_init (self, max_n)
    class(UnionFind), intent(inout) :: self
    integer,          intent(in)    :: max_n

    if (allocated(self%arr)) deallocate(self%arr)
    allocate(self%arr(max_n))
    self%arr = 0           ! every element starts as a singleton root
  end subroutine uf_init
  subroutine uf_insert (self, i)
    class(UnionFind), intent(inout) :: self
    integer,          intent(in)    :: i
    self%arr(i) = i        ! every non-singleton element points to self
  end subroutine uf_insert
  !==================================================================
  !  PURE function – find the root of element I.
  !  No path compression is performed here (otherwise the routine would
  !  have side‑effects and could not be PURE).
  !==================================================================
  pure function uf_root (self, i) result(r)
    class(UnionFind), intent(in) :: self
    integer,          intent(in) :: i
    integer                      :: r, cur
    if( self%arr(i) == 0 ) then
       r = i
       return
    end if
    cur = i
    do while (self%arr(cur) /= cur)
       cur = self%arr(cur)
    end do
    r = cur
  end function uf_root

  !==================================================================
  !  Merge the two sets that contain X and Y.
  !  The routine uses the pure function ROOT to locate the representatives
  !  and then makes one root the parent of the other.
  !==================================================================
  subroutine uf_merge (self, x, y)
    class(UnionFind), intent(inout) :: self
    integer,          intent(in)    :: x, y
    integer                         :: rx, ry

    rx = self%root(x)          ! find root of x (pure)
    ry = self%root(y)          ! find root of y (pure)
    if( rx == 0 .or. ry == 0 ) then
       stop "ERROR: singletons cannot be merged. Use insert(x) before"
       return
    end if
    if (rx == ry) return       ! already in the same set

    ! Simple union – make the root of X point to the root of Y.
    ! (You could add union‑by‑rank here if you wish.)
    if( rx < ry ) then
       self%arr(ry) = rx
       call self%reduce(ry)
    else
       self%arr(rx) = ry
       call self%reduce(rx)       
    end if
  end subroutine uf_merge

  !==================================================================
  !  Path‑compression routine.
  !  After a call to REDUCE(x) the entry arr(x) will contain the
  !  *final* root of the set that x belongs to, and all intermediate
  !  nodes on the search path are also !==================================================================
  subroutine uf_reduce (self, x)
    class(UnionFind), intent(inout) :: self
    integer,          intent(in)    :: x
    integer                         :: r, cur, next
    if( self%arr(x) == 0 ) then
       return !singletons cannot be further reduced. Could be an assertion here
    end if
    cur = x
    ! ---- first pass: find the root (same as ROOT) ------------------
    do while (self%arr(cur) /= cur)
       cur = self%arr(cur)
    end do
    r = cur

    ! ---- second pass: compress the path ---------------------------
    cur = x
    do while (self%arr(cur) /= cur)
       next        = self%arr(cur)   ! keep the next node before overwriting
       self%arr(cur) = r             ! point directly to the root
       cur = next
    end do
  end subroutine uf_reduce

end module DataStructuresModule

