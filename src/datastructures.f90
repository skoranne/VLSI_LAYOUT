! File    : datastructures.f90
! Author  : Sandeep Koranne (C) All rights.reserved.
! Purpose : Implementation of commonly used data structures
!         : hash table already implemented, this file has
!         : ring-buffer, LRU, union-find, graph, tree

module DataStructuresModule
  use iso_fortran_env, only : int32, int64
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
  type :: UnionFind(int_kind)
     integer, kind :: int_kind
     integer(kind=int_kind), allocatable :: arr(:) ! parent array, 0 ⇒ element is a singleton
   contains
     procedure, pass, private :: init_int32   => uf_init_int32
     procedure, pass, private :: init_int64   => uf_init_int64
     procedure, pass, private :: insert_int32 => uf_insert_int32 ! for singletons, arr(i)=i
     procedure, pass, private :: insert_int64 => uf_insert_int64 ! for singletons, arr(i)=i          
     procedure, pass, private :: root_int32   => uf_root_int32   ! pure – no side‑effects
     procedure, pass, private :: root_int64   => uf_root_int64   ! pure – no side‑effects     
     procedure, pass, private :: merge_int32  => uf_merge_int32
     procedure, pass, private :: merge_int64  => uf_merge_int64     
     procedure, pass, private :: reduce_int32 => uf_reduce_int32
     procedure, pass, private :: reduce_int64 => uf_reduce_int64
     procedure, pass, private :: fullreduce_int32 => uf_fullreduce_int32
     procedure, pass, private :: fullreduce_int64 => uf_fullreduce_int64
     procedure, pass, private :: count_roots_int32 => uf_count_roots_int32
     procedure, pass, private :: count_roots_int64 => uf_count_roots_int64
     procedure, pass, private :: uf_expand_roots_int32, uf_expand_roots_int64
     procedure, pass, private :: uf_contract_roots_int32, uf_contract_roots_int64     
     generic :: init => init_int32, init_int64
     generic :: insert => insert_int32, insert_int64     
     generic :: root => root_int32, root_int64
     generic :: merge => merge_int32, merge_int64
     generic :: reduce => reduce_int32, reduce_int64
     generic :: fullreduce => fullreduce_int32, fullreduce_int64
     generic :: count_roots => count_roots_int32, count_roots_int64
     generic :: expand_roots => uf_expand_roots_int32, uf_expand_roots_int64 !> singleton rectangle information is forgotten
     generic :: contract_roots => uf_contract_roots_int32, uf_contract_roots_int64 !> singleton rectangle information is computed
  end type UnionFind
  interface uf_init
     module procedure uf_init_int32
     module procedure uf_init_int64
  end interface uf_init
  interface uf_insert
     module procedure uf_insert_int32
     module procedure uf_insert_int64
  end interface uf_insert
  interface uf_root
     module procedure uf_root_int32
     module procedure uf_root_int64
  end interface uf_root
  interface uf_merge
     module procedure uf_merge_int32
     module procedure uf_merge_int64
  end interface uf_merge
  interface uf_reduce
     module procedure uf_reduce_int32
     module procedure uf_reduce_int64
  end interface uf_reduce
  interface uf_count_roots
     module procedure uf_count_roots_int32
     module procedure uf_count_roots_int64
  end interface uf_count_roots
  
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
  subroutine uf_init_int32 (self, max_n)
    class(UnionFind(int32)), intent(inout) :: self
    integer(kind=int32),     intent(in)    :: max_n

    if (allocated(self%arr)) deallocate(self%arr)
    allocate(self%arr(max_n))
    self%arr = 0           ! every element starts as a singleton root
  end subroutine uf_init_int32  
  subroutine uf_init_int64 (self, max_n)
    class(UnionFind(int64)), intent(inout) :: self
    integer(kind=int64),     intent(in)    :: max_n
    if (allocated(self%arr)) deallocate(self%arr)
    allocate(self%arr(max_n))
    self%arr = 0           ! every element starts as a singleton root
  end subroutine uf_init_int64
  
  subroutine uf_insert_int32 (self, i)
    class(UnionFind(int32)), intent(inout) :: self
    integer(kind=int32), intent(in)    :: i
    if( self%arr(i) == 0 ) then
       self%arr(i) = i        ! every non-singleton element points to self
    end if
  end subroutine uf_insert_int32
  subroutine uf_insert_int64 (self, i)
    class(UnionFind(int64)), intent(inout) :: self
    integer(kind=int64),     intent(in)    :: i
    if( self%arr(i) == 0 ) then
       self%arr(i) = i        ! every non-singleton element points to self
    end if
  end subroutine uf_insert_int64
  
  !==================================================================
  !  PURE function – find the root of element I.
  !  No path compression is performed here (otherwise the routine would
  !  have side‑effects and could not be PURE).
  !==================================================================
  pure function uf_root_int32 (self, i) result(r)
    class(UnionFind(int32)), intent(in) :: self
    integer(kind=int32),     intent(in) :: i
    integer(kind=int32)                 :: r, cur
    if( self%arr(i) == 0 ) then
       r = i
       return
    end if
    cur = i
    do while (self%arr(cur) /= cur)
       cur = self%arr(cur)
    end do
    r = cur
  end function uf_root_int32  
  pure function uf_root_int64 (self, i) result(r)
    class(UnionFind(int64)), intent(in) :: self
    integer(kind=int64),     intent(in) :: i
    integer(kind=int64)                 :: r, cur
    if( self%arr(i) == 0 ) then
       r = i
       return
    end if
    cur = i
    do while (self%arr(cur) /= cur)
       cur = self%arr(cur)
    end do
    r = cur
  end function uf_root_int64

  !==================================================================
  !  Merge the two sets that contain X and Y.
  !  The routine uses the pure function ROOT to locate the representatives
  !  and then makes one root the parent of the other.
  !==================================================================
  subroutine uf_merge_int32 (self, x, y)
    class(UnionFind(int32)), intent(inout) :: self
    integer(kind=int32),     intent(in)    :: x, y
    integer(kind=int32)                    :: rx, ry

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
  end subroutine uf_merge_int32
  subroutine uf_merge_int64 (self, x, y)
    class(UnionFind(int64)), intent(inout) :: self
    integer(kind=int64),     intent(in)    :: x, y
    integer(kind=int64)                    :: rx, ry

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
  end subroutine uf_merge_int64
  !==================================================================
  !  Path‑compression routine.
  !  After a call to REDUCE(x) the entry arr(x) will contain the
  !  *final* root of the set that x belongs to, and all intermediate
  !  nodes on the search path are also
  !==================================================================
  subroutine uf_reduce_int32 (self, x)
    class(UnionFind(int32)), intent(inout) :: self
    integer(kind=int32),     intent(in)    :: x
    integer(kind=int32)                    :: r, cur, next
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
  end subroutine uf_reduce_int32
  subroutine uf_reduce_int64 (self, x)
    class(UnionFind(int64)), intent(inout) :: self
    integer(kind=int64),     intent(in)    :: x
    integer(kind=int64)                    :: r, cur, next
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
  end subroutine uf_reduce_int64
  subroutine uf_fullreduce_int32 (self)
    class(UnionFind(int32)), intent(inout) :: self
    integer(kind=int32) :: n, i, root, current_label
    integer(kind=int32), allocatable   :: root_map(:)
    n = size(self%arr)
    allocate(root_map(n), source=0_int32)
    do i=1,size(self%arr)
       if( self%arr(i) /= 0 ) then
          call self%reduce_int32( i )
       end if
    end do
    !> after this atleast we know if arr(i) == i then i is a root
    current_label = 1
    ! Pass 1: Identify true roots and assign them sequential IDs
    do i = 1, n
       if (self%arr(i) == i) then
          root_map(i) = current_label
          current_label = current_label + 1
       end if
    end do
    do i = 1,n
       if( self%arr(i) > 0 ) then
          self%arr(i) = root_map( self%arr(i) )
       end if
    end do
  end subroutine uf_fullreduce_int32
  !> Full reduce also includes a relabel phase, where roots are sequentially
  !> assigned from 1..n, where n=num_roots.
  subroutine uf_fullreduce_int64 (self)
    class(UnionFind(int64)), intent(inout) :: self
    integer(kind=int64) :: n, i, root, current_label
    integer(kind=int64), allocatable   :: root_map(:)
    n = size(self%arr)
    allocate(root_map(n), source=0_int64)
    do i=1,size(self%arr)
       if( self%arr(i) /= 0 ) then
          call self%reduce_int64( i )
       end if
    end do
    !> after this atleast we know if arr(i) == i then i is a root
    current_label = 1
    ! Pass 1: Identify true roots and assign them sequential IDs
    do i = 1, n
       if (self%arr(i) == i) then
          root_map(i) = current_label
          current_label = current_label + 1
       end if
    end do
    do i = 1,n
       if( self%arr(i) > 0 ) then
          self%arr(i) = root_map( self%arr(i) )
       end if
    end do
  end subroutine uf_fullreduce_int64
  pure function uf_count_roots_int32 (self) result(retval)
    class(UnionFind(int32)), intent(in) :: self
    logical, allocatable :: seen(:)
    integer(kind=int32) :: min_val, max_val, i
    integer(kind=int32) :: retval
    ! 1. Handle the edge case of an empty array
    if (size(self%arr) == 0) then
       retval = 0
       return
    end if
    ! 2. Find the bounds of our data
    min_val = minval(self%arr)
    max_val = maxval(self%arr)
    
    ! 3. Allocate a boolean array mapped to our exact data range.
    !    'source=.false.' initializes all elements to false.
    allocate(seen(min_val:max_val), source=.false.)
    
    ! 4. Mark the index corresponding to the value as true
    do i = 1, size(self%arr)
       if( self%arr(i) > 0 ) then       
          seen(self%arr(i)) = .true.
       end if
    end do
    ! 5. The number of unique elements is just the count of true values!
    retval = count(seen)    
  end function uf_count_roots_int32
  pure function uf_count_roots_int64 (self) result(retval)
    class(UnionFind(int64)), intent(in) :: self
    logical, allocatable :: seen(:)
    integer(kind=int64) :: min_val, max_val, i
    integer(kind=int64) :: retval    
    ! 1. Handle the edge case of an empty array
    if (size(self%arr) == 0) then
       retval = 0
       return
    end if
    ! 2. Find the bounds of our data
    min_val = minval(self%arr)
    max_val = maxval(self%arr)
    
    ! 3. Allocate a boolean array mapped to our exact data range.
    !    'source=.false.' initializes all elements to false.
    allocate(seen(min_val:max_val), source=.false.)
    
    ! 4. Mark the index corresponding to the value as true
    do i = 1, size(self%arr)
       if( self%arr(i) > 0 ) then
          seen(self%arr(i)) = .true.
       end if
    end do
    ! 5. The number of unique elements is just the count of true values!
    retval = count(seen)    
  end function uf_count_roots_int64
  pure subroutine uf_contract_roots_int32(self)
    class(UnionFind(int32)), intent(inout) :: self
    logical, allocatable :: seen(:)
    integer(kind=int64) :: min_val, max_val, i
    integer(kind=int64) :: retval    
  end subroutine uf_contract_roots_int32
  
  pure subroutine uf_contract_roots_int64(self)
    class(UnionFind(int64)), intent(inout) :: self
    logical, allocatable :: seen(:)
    integer(kind=int64) :: min_val, max_val, i
    integer(kind=int64) :: retval    
  end subroutine uf_contract_roots_int64
  subroutine uf_expand_roots_int32(self)
    class(UnionFind(int32)), intent(inout) :: self
    integer(kind=int64) :: num_roots, i
    num_roots = self%count_roots()
    write(*,*) 'Incoming |Root| = ', num_roots, ' |S| = ', size(self%arr)
    do i=1,size(self%arr)
       if( self%arr(i) == 0 ) then
          self%arr(i) = num_roots + 1
          num_roots = num_roots + 1
       end if
    end do
    if( num_roots /= self%count_roots() ) then
       write(*,*) 'Outgoing |Root| = ', num_roots, ' while UF%count_roots = ', self%count_roots()
       error stop "INCONSISTENT EXPANSION of roots detected."
    end if    
  end subroutine uf_expand_roots_int32
  
  subroutine uf_expand_roots_int64(self)
    class(UnionFind(int64)), intent(inout) :: self
    integer(kind=int64) :: num_roots, i, min_val, max_val
    num_roots = self%count_roots()
    min_val = minval(self%arr)
    max_val = maxval(self%arr)    
    !write(*,*) 'Incoming |Root| = ', num_roots, ' |S| = ', size(self%arr), ' minval = ', min_val, ' maxval = ', max_val
    do i=1,size(self%arr)
       if( self%arr(i) == 0 ) then
          self%arr(i) = num_roots + 1
          num_roots = num_roots + 1
       end if
    end do
    min_val = minval(self%arr)
    max_val = maxval(self%arr)    
    !write(*,*) 'Out |Root| = ', num_roots, ' |S| = ', size(self%arr), ' minval = ', min_val, ' maxval = ', max_val    
    if( num_roots /= self%count_roots() ) then
       write(*,*) 'Outgoing |Root| = ', num_roots, ' while UF%count_roots = ', self%count_roots()
       error stop "INCONSISTENT EXPANSION of roots detected."
    end if
  end subroutine uf_expand_roots_int64
  
end module DataStructuresModule



