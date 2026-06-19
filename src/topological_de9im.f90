! File   : design_impl.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: 
! Given the same constraints calculate the DE-9IM edge boundary region
! topological relation ship matrix for each element of A vs each element of
! B. Assume we have an UnionFind data structure which has been run on A and B
! separately. It can give the parent-id of each box index. The Egenhofer DE-9IM
! model can be found using RTree in O(n log n) time.


!=====================================================================
!  topological_de9im.f90   –  submodule containing the heavy code
!=====================================================================
submodule (DesignModule) TopologicalDE9IM
  use iso_fortran_env, only: int32, int64, real64
  use CommonModule
  use GeometryModule
  use RTreeModule
  use PNumMergeModule
  use DataStructuresModule
  use omp_lib  
  implicit none

  ! Dynamic vector to hold boxes per thread without locking
  type :: BoxVector
     type(Box), allocatable :: Elements(:)
     integer(kind=int64) :: Count
     integer(kind=int64) :: Capacity
  end type BoxVector

  ! Represents a raw geometric interaction between two fractured boxes
  type :: InteractionFeature
     integer(kind=int64) :: ParentA
     integer(kind=int64) :: ParentB
     integer(kind=int64) :: IntersectDim ! 2 (Area), 1 (Line), 0 (Point)
  end type InteractionFeature

  type :: FeatureVector
     type(InteractionFeature), allocatable :: Elements(:)
     integer(kind=int64) :: Count
     integer(kind=int64) :: Capacity
  end type FeatureVector

  ! The resulting DE-9IM representation for a unique pair of polygons
  type :: DE9IMMatrix
     integer(kind=int64) :: ParentA
     integer(kind=int64) :: ParentB
     ! Matrix mapping: (1:Interior, 2:Boundary, 3:Exterior)
     ! Values: -1 (F/Empty), 0 (Point), 1 (Line), 2 (Area)
     integer(kind=int64) :: Mat(3,3) 
  end type DE9IMMatrix

  type :: RTree
  end type RTree

contains

  !> Generates the DE-9IM relationship matrices for interacting polygon pairs
  subroutine GenerateDE9IM(TreeB, BoxesA, BoxesB, UnionFindA, UnionFindB, &
       ResultMatrices, NumResults)
    type(RTree), intent(in) :: TreeB
    type(Box), intent(in) :: BoxesA(:), BoxesB(:)
    integer(kind=int64), intent(in) :: UnionFindA(:), UnionFindB(:)
    type(DE9IMMatrix), allocatable, intent(out) :: ResultMatrices(:)
    integer(kind=int64), intent(out) :: NumResults

    integer(kind=int64) :: NumA, NumB, NumTotalFeatures
    integer(kind=int64) :: I, J, ThreadId, NumThreads, GlobalOffset
    integer(kind=int64) :: IDim, PA, PB

    integer(kind=int64), allocatable :: IntersectingIndices(:)
    integer(kind=int64) :: NumIntersections

    type(FeatureVector), allocatable :: ThreadFeatures(:)
    type(InteractionFeature), allocatable :: GlobalFeatures(:)

    NumA = size(BoxesA, kind=int64)
    NumB = size(BoxesB, kind=int64)
    NumThreads = omp_get_max_threads()

    allocate(ThreadFeatures(0:NumThreads-1))
    do I = 0, int(NumThreads - 1, kind=int64)
       call InitFeatureVector(ThreadFeatures(I), 1024_int64)
    end do

    ! ==========================================
    ! PHASE 1: Spatial Map & Feature Extraction
    ! ==========================================
    !$omp parallel default(none) &
    !$omp shared(BoxesA, BoxesB, TreeB, UnionFindA, UnionFindB, NumA, ThreadFeatures) &
    !$omp private(I, J, ThreadId, IntersectingIndices, NumIntersections, IDim, PA, PB)

    ThreadId = omp_get_thread_num()
    allocate(IntersectingIndices(128))

    !$omp do schedule(dynamic, 128)
    do I = 1, NumA
       ! 0-margin query to capture touching boundaries (1D/0D intersections)
       call QueryRTreeTouch(TreeB, BoxesA(I), IntersectingIndices, NumIntersections)

       PA = UnionFindA(I)

       do J = 1, NumIntersections
          PB = UnionFindB(IntersectingIndices(J))

          ! Classify the geometric intersection dimension
          IDim = CalculateIntersectionDim(BoxesA(I), BoxesB(IntersectingIndices(J)))

          if (IDim >= 0) then
             call PushFeatureVector(ThreadFeatures(ThreadId), InteractionFeature(PA, PB, IDim))
          end if
       end do
    end do
    !$omp end do

    deallocate(IntersectingIndices)
    !$omp end parallel

    ! ==========================================
    ! PHASE 2: Gather and Sort
    ! ==========================================
    NumTotalFeatures = 0
    do I = 0, int(NumThreads - 1, kind=int64)
       NumTotalFeatures = NumTotalFeatures + ThreadFeatures(I)%Count
    end do

    allocate(GlobalFeatures(NumTotalFeatures))

    GlobalOffset = 1
    do I = 0, int(NumThreads - 1, kind=int64)
       if (ThreadFeatures(I)%Count > 0) then
          GlobalFeatures(GlobalOffset : GlobalOffset + ThreadFeatures(I)%Count - 1) = &
               ThreadFeatures(I)%Elements(1 : ThreadFeatures(I)%Count)
          GlobalOffset = GlobalOffset + ThreadFeatures(I)%Count
       end if
       call DestroyFeatureVector(ThreadFeatures(I))
    end do
    deallocate(ThreadFeatures)

    ! Sort features strictly by ParentA, then ParentB
    ! (Assuming an external highly-optimized radix or quick sort)
    call SortInteractionFeatures(GlobalFeatures, NumTotalFeatures)

    ! ==========================================
    ! PHASE 3: Reduce to DE-9IM Matrices
    ! ==========================================
    ! Allocate maximum possible matrices; will truncate later
    allocate(ResultMatrices(NumTotalFeatures)) 
    NumResults = 0

    if (NumTotalFeatures > 0) then
       I = 1
       do while (I <= NumTotalFeatures)
          PA = GlobalFeatures(I)%ParentA
          PB = GlobalFeatures(I)%ParentB

          NumResults = NumResults + 1
          call InitializeMatrixDisjoint(ResultMatrices(NumResults), PA, PB)

          ! Consume all features belonging to the (PA, PB) polygon pair
          do while (I <= NumTotalFeatures .and. &
               GlobalFeatures(I)%ParentA == PA .and. &
               GlobalFeatures(I)%ParentB == PB)

             IDim = GlobalFeatures(I)%IntersectDim

             ! Apply topological promotion rules for logical polygons
             if (IDim == 2) then
                ! Area overlap: Interior intersects Interior
                ResultMatrices(NumResults)%Mat(1,1) = 2
                ! Polygons partially overlap, meaning boundaries intersect interiors
                ResultMatrices(NumResults)%Mat(1,2) = max(ResultMatrices(NumResults)%Mat(1,2), 1_int64)
                ResultMatrices(NumResults)%Mat(2,1) = max(ResultMatrices(NumResults)%Mat(2,1), 1_int64)
             else if (IDim == 1) then
                ! Line touch: Boundaries intersect
                ResultMatrices(NumResults)%Mat(2,2) = max(ResultMatrices(NumResults)%Mat(2,2), 1_int64)
             else if (IDim == 0) then
                ! Point touch
                ResultMatrices(NumResults)%Mat(2,2) = max(ResultMatrices(NumResults)%Mat(2,2), 0_int64)
             end if

             I = I + 1
          end do
       end do
    end if

  end subroutine GenerateDE9IM

  ! --- Core Topological Helpers ---

  !> Classifies the spatial intersection of two boxes.
  !> Returns: 2 (Area), 1 (Line shared edge), 0 (Vertex touch), -1 (Disjoint)
  pure function CalculateIntersectionDim(A, B) result(Dim)
    type(Box), intent(in) :: A, B
    integer(kind=int64) :: Dim
    integer(kind=K_COORDINATE_KIND) :: DX, DY

    ! Calculate overlap extents
    DX = min(A%XMax, B%XMax) - max(A%XMin, B%XMin)
    DY = min(A%YMax, B%YMax) - max(A%YMin, B%YMin)

    if (DX > 0 .and. DY > 0) then
       Dim = 2 ! 2D Area Overlap
    else if ((DX > 0 .and. DY == 0) .or. (DX == 0 .and. DY > 0)) then
       Dim = 1 ! 1D Line Overlap (Shared edge)
    else if (DX == 0 .and. DY == 0) then
       Dim = 0 ! 0D Point Overlap (Corner touch)
    else
       Dim = -1 ! Disjoint
    end if
  end function CalculateIntersectionDim

  !> Initializes a DE-9IM matrix to the standard "Disjoint" state (FF2FF2212)
  !> Note: In most layout scenarios, Exteriors always intersect in 2D.
  pure subroutine InitializeMatrixDisjoint(M, PA, PB)
    type(DE9IMMatrix), intent(inout) :: M
    integer(kind=int64), intent(in) :: PA, PB
    M%ParentA = PA
    M%ParentB = PB

    M%Mat(1,1) = -1; M%Mat(1,2) = -1; M%Mat(1,3) = 2  ! I(A) row
    M%Mat(2,1) = -1; M%Mat(2,2) = -1; M%Mat(2,3) = 1  ! B(A) row 
    M%Mat(3,1) =  2; M%Mat(3,2) =  1; M%Mat(3,3) = 2  ! E(A) row
  end subroutine InitializeMatrixDisjoint

  ! --- Feature Vector Implementations ---

  pure subroutine InitFeatureVector(Vec, InitialCapacity)
    type(FeatureVector), intent(out) :: Vec
    integer(kind=int64), intent(in) :: InitialCapacity
    allocate(Vec%Elements(InitialCapacity))
    Vec%Count = 0
    Vec%Capacity = InitialCapacity
  end subroutine InitFeatureVector

  pure subroutine PushFeatureVector(Vec, Item)
    type(FeatureVector), intent(inout) :: Vec
    type(InteractionFeature), intent(in) :: Item
    type(InteractionFeature), allocatable :: Temp(:)

    if (Vec%Count >= Vec%Capacity) then
       Vec%Capacity = Vec%Capacity * 2
       allocate(Temp(Vec%Capacity))
       Temp(1:Vec%Count) = Vec%Elements(1:Vec%Count)
       call move_alloc(Temp, Vec%Elements)
    end if

    Vec%Count = Vec%Count + 1
    Vec%Elements(Vec%Count) = Item
  end subroutine PushFeatureVector

  pure subroutine DestroyFeatureVector(Vec)
    type(FeatureVector), intent(inout) :: Vec
    if (allocated(Vec%Elements)) deallocate(Vec%Elements)
    Vec%Count = 0; Vec%Capacity = 0
  end subroutine DestroyFeatureVector

  ! Mock Interfaces
  pure subroutine QueryRTreeTouch(Tree, SearchBox, ResultIndices, NumFound)
    type(RTree), intent(in) :: Tree
    type(Box), intent(in) :: SearchBox
    integer(kind=int64), intent(out) :: ResultIndices(:), NumFound
    NumFound = 0
  end subroutine QueryRTreeTouch

  pure subroutine SortInteractionFeatures(Features, Count)
    type(InteractionFeature), intent(inout) :: Features(:)
    integer(kind=int64), intent(in) :: Count
    ! Assumes implementation of an optimized struct sort by ParentA, then ParentB
  end subroutine SortInteractionFeatures

end submodule TopologicalDE9IM

