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

  ! Forward declare the pointer wrapper
  type :: SL_PTR
      type(SL_NODE), pointer :: Ptr => null()
  end type SL_PTR

  ! The Node Definition (Fixed-size forward array to prevent heap fragmentation)
  type :: SL_NODE
      SL_PAYLOAD             :: Value
      type(SL_PTR)           :: Forward(32)
      type(SL_PTR)           :: Backward(32)  ! NEW: For O(log N) reverse traversal
      type(SL_PTR)           :: NextFree      ! Used for the LIFO free list
  end type SL_NODE

  ! The SkipList Engine Definition
  type :: SL_TYPE
      type(SL_NODE), pointer             :: Header => null()
      integer                            :: MaxLevel
      type(SL_NODE), pointer             :: Arena(:) => null()
      type(SL_PTR)                       :: FreeHead
  end type SL_TYPE

#endif


! ==============================================================================
! SECTION 2: PROCEDURE IMPLEMENTATIONS
! ==============================================================================
#ifdef SKIPLIST_GENERATE_PROCEDURES

  subroutine SL_INIT(Sl, Capacity)
      type(SL_TYPE), intent(out) :: Sl
      integer(int64), intent(in) :: Capacity
      integer(int64) :: i
      integer :: j
      
      Sl%MaxLevel = 64
      ! Allocate the contiguous Arena (Capacity + 1 for the Header node)
      allocate(Sl%Arena(Capacity + 1))
      
      ! Initialize the LIFO Free List chain
      do i = 1, Capacity
          Sl%Arena(i)%NextFree%Ptr => Sl%Arena(i+1)
          do j = 1, Sl%MaxLevel
              Sl%Arena(i)%Forward(j)%Ptr => null()
          end do
      end do
      
      ! Terminate the last node in the Arena
      Sl%Arena(Capacity + 1)%NextFree%Ptr => null()
      do j = 1, Sl%MaxLevel
          Sl%Arena(Capacity + 1)%Forward(j)%Ptr => null()
      end do
      
      ! Anchor the Free List
      Sl%FreeHead%Ptr => Sl%Arena(1)
      
      ! Consume the first free node for the Sentinel Header
      Sl%Header => SL_GET_FREE(Sl)
  end subroutine SL_INIT

  subroutine SL_DESTROY(Sl)
      type(SL_TYPE), intent(inout) :: Sl
      if (associated(Sl%Arena)) deallocate(Sl%Arena)
      nullify(Sl%Header)
      nullify(Sl%FreeHead%Ptr)
  end subroutine SL_DESTROY
  ! function SL_GET_FREE3(list) result(node)
  !   type(SL_TYPE), intent(inout) :: Sl
  !   type(SL_NODE), pointer :: node
  !   integer :: j
    
  !   if (associated(list%FreeHead%ptr)) then
  !       ! Fast Path: Pop from LIFO Arena
  !       node => list%FreeHead%ptr
  !       list%FreeHead%ptr => node%NextFree%ptr
  !       node%NextFree%ptr => null()
  !   else
  !       ! Slow Path Fallback: Arena Exhausted, grab from OS Heap safely
  !       allocate(node)
  !       do j = 1, MAX_SKIP_LEVEL
  !           node%Forward(j)%ptr  => null()
  !           node%Backward(j)%ptr => null()
  !       end do
  !       node%NextFree%ptr => null()
  !   end if
  ! end function SL_GET_FREE3
  function SL_GET_FREE(Sl) result(Node)
      type(SL_TYPE), intent(inout) :: Sl
      type(SL_NODE), pointer :: Node
      
      if (.not. associated(Sl%FreeHead%Ptr)) then
          error stop "CRITICAL: SkipList LIFO Arena Exhausted!"
      end if
      
      ! Pop from LIFO head
      Node => Sl%FreeHead%Ptr
      Sl%FreeHead%Ptr => Node%NextFree%Ptr
      Node%NextFree%Ptr => null()
  end function SL_GET_FREE

  subroutine SL_RELEASE(Sl, Node)
      type(SL_TYPE), intent(inout) :: Sl
      type(SL_NODE), pointer :: Node
      integer :: i
      
      ! Wipe forward pointers to prevent ghost traversals
      do i = 1, Sl%MaxLevel
          Node%Forward(i)%Ptr => null()
      end do
      
      ! Push to LIFO head
      Node%NextFree%Ptr => Sl%FreeHead%Ptr
      Sl%FreeHead%Ptr => Node
  end subroutine SL_RELEASE

  function SL_RAND_LVL(Sl) result(Lvl)
      type(SL_TYPE), intent(in) :: Sl
      integer :: Lvl
      real :: r
      
      Lvl = 1
      call random_number(r)
      do while (r < 0.5 .and. Lvl < Sl%MaxLevel)
          Lvl = Lvl + 1
          call random_number(r)
      end do
  end function SL_RAND_LVL
subroutine SL_INSERT(Sl, Val)
      type(SL_TYPE), intent(inout) :: Sl
      SL_PAYLOAD, intent(in) :: Val
      type(SL_NODE), pointer :: Curr, NewNode
      type(SL_PTR) :: Update(Sl%MaxLevel)
      integer :: i, Lvl

      Curr => Sl%Header
      do i = Sl%MaxLevel, 1, -1
          do while (associated(Curr%Forward(i)%Ptr))
              if (Curr%Forward(i)%Ptr%Value < Val) then
                  Curr => Curr%Forward(i)%Ptr
              else
                  exit
              end if
          end do
          Update(i)%Ptr => Curr
      end do

      Lvl = SL_RAND_LVL(Sl)
      NewNode => SL_GET_FREE(Sl)
      NewNode%Value = Val
      
      ! Single-Pass Splice
      do i = 1, Lvl
          ! Update Forward links
          NewNode%Forward(i)%Ptr => Update(i)%Ptr%Forward(i)%Ptr
          Update(i)%Ptr%Forward(i)%Ptr => NewNode
          
          ! Set Backward links
          if (associated(NewNode%Forward(i)%Ptr)) then
             NewNode%Forward(i)%Ptr%Backward(i)%Ptr => NewNode
          end if
          NewNode%Backward(i)%Ptr => Update(i)%Ptr
      end do
  end subroutine SL_INSERT

  function SL_FIND(Sl, Val) result(Found)
      type(SL_TYPE), intent(in) :: Sl
      SL_PAYLOAD, intent(in) :: Val
      logical :: Found
      type(SL_NODE), pointer :: Curr
      integer :: i
      
      Found = .false.
      Curr => Sl%Header
      do i = Sl%MaxLevel, 1, -1
          do while (associated(Curr%Forward(i)%Ptr))
              if (Curr%Forward(i)%Ptr%Value < Val) then
                  Curr => Curr%Forward(i)%Ptr
              else
                  exit
              end if
          end do
      end do
      
      ! Drop to level 1 and check exact match
      Curr => Curr%Forward(1)%Ptr
      if (associated(Curr)) then
          if (Curr%Value == Val) Found = .true.
      end if
  end function SL_FIND
! ... [Existing procedures: SL_INIT, SL_DESTROY, etc.] ...

  subroutine SL_FIND_FIRST(Sl, Target, Node, Found)
      type(SL_TYPE), intent(in) :: Sl
      SL_PAYLOAD, intent(in) :: Target
      type(SL_NODE), pointer :: Node, Curr
      logical, intent(out) :: Found
      integer :: j

      Found = .false.
      Curr => Sl%Header
      do j = Sl%MaxLevel, 1, -1
          do while (associated(Curr%Forward(j)%Ptr))
              if (Curr%Forward(j)%Ptr%Value < Target) then
                  Curr => Curr%Forward(j)%Ptr
              else
                  exit
              end if
          end do
      end do

      Curr => Curr%Forward(1)%Ptr
      if (associated(Curr)) then
          if (Curr%Value == Target) then
              Node => Curr
              Found = .true.
          end if
      end if
  end subroutine SL_FIND_FIRST
subroutine SL_GET_NEXT(Sl, Target, NKey, Found)
      type(SL_TYPE), intent(in) :: Sl
      SL_PAYLOAD, intent(in) :: Target
      SL_PAYLOAD, intent(out) :: NKey
      logical, intent(out) :: Found
      type(SL_NODE), pointer :: Curr
      logical :: SubFound ! Local variable for internal calls

      Found = .false.
      call SL_FIND_FIRST(Sl, Target, Curr, SubFound)
      if (.not. SubFound) return

      ! Walk horizontally
      do while (associated(Curr))
          if (Curr%Value > Target) then
              NKey = Curr%Value
              Found = .true.
              return
          end if
          Curr => Curr%Forward(1)%Ptr
      end do
  end subroutine SL_GET_NEXT
  subroutine SL_GET_PREV(Sl, Target, PKey, Found)
      type(SL_TYPE), intent(in) :: Sl
      SL_PAYLOAD, intent(in) :: Target
      SL_PAYLOAD, intent(out) :: PKey
      logical, intent(out) :: Found
      type(SL_NODE), pointer :: Curr
      logical :: SubFound

      Found = .false.
      
      ! 1. Use the optimized O(log N) search to find the target node
      call SL_FIND_FIRST(Sl, Target, Curr, SubFound)
      if (.not. SubFound) return

      ! 2. Teleport to the immediate predecessor at Level 1 (O(1))
      if (associated(Curr%Backward(1)%Ptr)) then
          ! Ensure we aren't at the Sentinel Header
          if (.not. associated(Curr%Backward(1)%Ptr, Sl%Header)) then
              PKey = Curr%Backward(1)%Ptr%Value
              Found = .true.
          end if
      end if
  end subroutine SL_GET_PREV
#endif

