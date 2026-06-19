! File   : design_impl.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Use submodule to move large blocks of code out of "interfaces"

!=====================================================================
!  design_impl.f90   –  submodule containing the heavy code
!=====================================================================
submodule (DesignModule) BooleanOpsRTree
  use iso_fortran_env, only: int32, int64, real64
  use CommonModule
  use GeometryModule
  use RTreeModule
  use PNumMergeModule
  use DataStructuresModule
  implicit none

  ! Dynamic vector to hold boxes per thread without locking
  type :: BoxVector
     type(Box), allocatable :: Elements(:)
     integer(kind=int64) :: Count
     integer(kind=int64) :: Capacity
  end type BoxVector

contains
  !> Computes A NOT B (Boolean Difference) producing strictly disjoint fragments
  subroutine CalculateNOTRTree(TreeA, TreeB, BoxesA, BoxesB, ResultBoxes, NumResults)
    type(RTree), intent(in) :: TreeA, TreeB
    type(Box), intent(in) :: BoxesA(:), BoxesB(:)
    type(Box), allocatable, intent(out) :: ResultBoxes(:)
    integer(kind=int64), intent(out) :: NumResults

    integer(kind=int64) :: NumA, NumB
    integer(kind=int64) :: I, J, K, FragIdx
    integer(kind=int64) :: NumThreads, ThreadId, GlobalOffset
    type(BoxVector), allocatable :: ThreadLocalResults(:)

    integer(kind=int64), allocatable :: IntersectingIndices(:)
    integer(kind=int64) :: NumIntersections

    type(BoxVector) :: SubjectFragments, NextFragments
    type(Box) :: CurrentSubject, SubtractorBox
    type(Box) :: FracturedBoxes(4)
    integer(kind=int64) :: NumFractured

    NumA = size(BoxesA, kind=int64)
    NumB = size(BoxesB, kind=int64)

    ! Initialize thread-local storage to prevent false sharing
    NumThreads = omp_get_max_threads()
    allocate(ThreadLocalResults(0:NumThreads-1))
    do I = 0, int(NumThreads - 1, kind=int64)
       call InitBoxVector(ThreadLocalResults(I), 1024_int64)
    end do

    !$omp parallel default(none) &
    !$omp shared(BoxesA, BoxesB, TreeB, NumA, ThreadLocalResults) &
    !$omp private(I, J, K, FragIdx, ThreadId, IntersectingIndices, NumIntersections, &
    !$omp         SubjectFragments, NextFragments, CurrentSubject, SubtractorBox, &
    !$omp         FracturedBoxes, NumFractured)

    ThreadId = omp_get_thread_num()

    ! Pre-allocate local buffers for intersection queries to avoid inner loop allocations
    allocate(IntersectingIndices(64)) ! Assumed max local density
    call InitBoxVector(SubjectFragments, 64_int64)
    call InitBoxVector(NextFragments, 64_int64)

    !$omp do schedule(dynamic, 128)
    do I = 1, NumA
       CurrentSubject = BoxesA(I)

       ! 1. Query R-Tree for B boxes intersecting CurrentSubject
       ! (Assuming QueryRTree populates IntersectingIndices and returns count)
       call QueryRTree(TreeB, CurrentSubject, IntersectingIndices, NumIntersections)

       if (NumIntersections == 0) then
          call PushBoxVector(ThreadLocalResults(ThreadId), CurrentSubject)
          cycle
       end if

       ! 2. Iterative fracturing (A NOT B)
       call ClearBoxVector(SubjectFragments)
       call PushBoxVector(SubjectFragments, CurrentSubject)

       do J = 1, NumIntersections
          SubtractorBox = BoxesB(IntersectingIndices(J))
          call ClearBoxVector(NextFragments)

          ! Subtract the current B-box from all active fragments of A
          do K = 1, SubjectFragments%Count
             call SubtractBox(SubjectFragments%Elements(K), SubtractorBox, FracturedBoxes, NumFractured)
             do FragIdx = 1, NumFractured
                call PushBoxVector(NextFragments, FracturedBoxes(FragIdx))
             end do
          end do

          ! Swap vectors
          call CopyBoxVector(NextFragments, SubjectFragments)
          if (SubjectFragments%Count == 0) exit ! Completely consumed
       end do

       ! 3. Store surviving fragments into thread-local accumulator
       do K = 1, SubjectFragments%Count
          call PushBoxVector(ThreadLocalResults(ThreadId), SubjectFragments%Elements(K))
       end do

    end do
    !$omp end do

    call DestroyBoxVector(SubjectFragments)
    call DestroyBoxVector(NextFragments)
    deallocate(IntersectingIndices)

    !$omp end parallel

    ! Reduction Phase: Prefix sum over thread-local counts
    NumResults = 0
    do I = 0, int(NumThreads - 1, kind=int64)
       NumResults = NumResults + ThreadLocalResults(I)%Count
    end do

    allocate(ResultBoxes(NumResults))

    ! Gather Phase: Copy to contiguous output array
    GlobalOffset = 1
    do I = 0, int(NumThreads - 1, kind=int64)
       if (ThreadLocalResults(I)%Count > 0) then
          ResultBoxes(GlobalOffset : GlobalOffset + ThreadLocalResults(I)%Count - 1) = &
               ThreadLocalResults(I)%Elements(1 : ThreadLocalResults(I)%Count)
          GlobalOffset = GlobalOffset + ThreadLocalResults(I)%Count
       end if
       call DestroyBoxVector(ThreadLocalResults(I))
    end do
    deallocate(ThreadLocalResults)

  end subroutine CalculateNOTRTree

  !> Fractures a Subject box against a Clip box into mutually exclusive rectangles
  pure subroutine SubtractBox(Subject, Clip, ResultArray, NumResults)
    type(Box), intent(in) :: Subject, Clip
    type(Box), intent(out) :: ResultArray(4)
    integer(kind=int64), intent(out) :: NumResults

    integer(kind=K_COORDINATE_KIND) :: IMinX, IMinY, IMaxX, IMaxY

    NumResults = 0

    ! Check for disjoint boxes
    if (Subject%XMax <= Clip%XMin .or. Subject%XMin >= Clip%XMax .or. &
         Subject%YMax <= Clip%YMin .or. Subject%YMin >= Clip%YMax) then
       NumResults = 1
       ResultArray(1) = Subject
       return
    end if

    ! Calculate intersection bounds
    IMinX = max(Subject%XMin, Clip%XMin)
    IMinY = max(Subject%YMin, Clip%YMin)
    IMaxX = min(Subject%XMax, Clip%XMax)
    IMaxY = min(Subject%YMax, Clip%YMax)

    ! 1. Left Box
    if (Subject%XMin < IMinX) then
       NumResults = NumResults + 1
       ResultArray(NumResults) = Box(Subject%XMin, Subject%YMin, IMinX, Subject%YMax)
    end if

    ! 2. Right Box
    if (Subject%XMax > IMaxX) then
       NumResults = NumResults + 1
       ResultArray(NumResults) = Box(IMaxX, Subject%YMin, Subject%XMax, Subject%YMax)
    end if

    ! 3. Bottom Box (Constrained by X-bounds of intersection to ensure disjointness)
    if (Subject%YMin < IMinY) then
       NumResults = NumResults + 1
       ResultArray(NumResults) = Box(IMinX, Subject%YMin, IMaxX, IMinY)
    end if

    ! 4. Top Box (Constrained by X-bounds of intersection)
    if (Subject%YMax > IMaxY) then
       NumResults = NumResults + 1
       ResultArray(NumResults) = Box(IMinX, IMaxY, IMaxX, Subject%YMax)
    end if

  end subroutine SubtractBox

  ! --- Vector Helper Implementations ---

  pure subroutine InitBoxVector(Vec, InitialCapacity)
    type(BoxVector), intent(out) :: Vec
    integer(kind=int64), intent(in) :: InitialCapacity
    allocate(Vec%Elements(InitialCapacity))
    Vec%Count = 0
    Vec%Capacity = InitialCapacity
  end subroutine InitBoxVector

  pure subroutine PushBoxVector(Vec, Item)
    type(BoxVector), intent(inout) :: Vec
    type(Box), intent(in) :: Item
    type(Box), allocatable :: Temp(:)

    if (Vec%Count >= Vec%Capacity) then
       Vec%Capacity = Vec%Capacity * 2
       allocate(Temp(Vec%Capacity))
       Temp(1:Vec%Count) = Vec%Elements(1:Vec%Count)
       call move_alloc(Temp, Vec%Elements)
    end if

    Vec%Count = Vec%Count + 1
    Vec%Elements(Vec%Count) = Item
  end subroutine PushBoxVector

  pure subroutine ClearBoxVector(Vec)
    type(BoxVector), intent(inout) :: Vec
    Vec%Count = 0
  end subroutine ClearBoxVector

  pure subroutine CopyBoxVector(Source, Dest)
    type(BoxVector), intent(in) :: Source
    type(BoxVector), intent(inout) :: Dest
    integer(kind=int64) :: I
    call ClearBoxVector(Dest)
    do I = 1, Source%Count
       call PushBoxVector(Dest, Source%Elements(I))
    end do
  end subroutine CopyBoxVector

  pure subroutine DestroyBoxVector(Vec)
    type(BoxVector), intent(inout) :: Vec
    if (allocated(Vec%Elements)) deallocate(Vec%Elements)
    Vec%Count = 0
    Vec%Capacity = 0
  end subroutine DestroyBoxVector

  ! Mock Interface for RTree querying 
  pure subroutine QueryRTree(Tree, SearchBox, ResultIndices, NumFound)
    type(RTree), intent(in) :: Tree
    type(Box), intent(in) :: SearchBox
    integer(kind=int64), intent(out) :: ResultIndices(:)
    integer(kind=int64), intent(out) :: NumFound
    ! Implementation traverses tree nodes intersecting SearchBox
    NumFound = 0 
  end subroutine QueryRTree



  !> Computes A XOR B producing strictly disjoint output fragments.
  !> Performs lock-free parallel execution by computing (A NOT B) U (B NOT A)
  subroutine CalculateXORRTree(TreeA, TreeB, BoxesA, BoxesB, ResultBoxes, NumResults)
    type(RTree), intent(in) :: TreeA, TreeB
    type(Box), intent(in) :: BoxesA(:), BoxesB(:)
    type(Box), allocatable, intent(out) :: ResultBoxes(:)
    integer(kind=int64), intent(out) :: NumResults

    integer(kind=int64) :: NumA, NumB
    integer(kind=int64) :: I, J, K, FragIdx
    integer(kind=int64) :: NumThreads, ThreadId, GlobalOffset
    type(BoxVector), allocatable :: ThreadLocalResults(:)

    integer(kind=int64), allocatable :: IntersectingIndices(:)
    integer(kind=int64) :: NumIntersections

    type(BoxVector) :: SubjectFragments, NextFragments
    type(Box) :: CurrentSubject, SubtractorBox
    type(Box) :: FracturedBoxes(4)
    integer(kind=int64) :: NumFractured

    NumA = size(BoxesA, kind=int64)
    NumB = size(BoxesB, kind=int64)

    ! Initialize thread-local storage 
    NumThreads = omp_get_max_threads()
    allocate(ThreadLocalResults(0:NumThreads-1))
    do I = 0, int(NumThreads - 1, kind=int64)
       call InitBoxVector(ThreadLocalResults(I), 2048_int64)
    end do

    ! Fork once to minimize OpenMP thread-spawning overhead
    !$omp parallel default(none) &
    !$omp shared(BoxesA, BoxesB, TreeA, TreeB, NumA, NumB, ThreadLocalResults) &
    !$omp private(I, J, K, FragIdx, ThreadId, IntersectingIndices, NumIntersections, &
    !$omp         SubjectFragments, NextFragments, CurrentSubject, SubtractorBox, &
    !$omp         FracturedBoxes, NumFractured)

    ThreadId = omp_get_thread_num()

    ! Pre-allocate local buffers to prevent heap contention
    allocate(IntersectingIndices(64)) 
    call InitBoxVector(SubjectFragments, 64_int64)
    call InitBoxVector(NextFragments, 64_int64)

    ! ==========================================
    ! PHASE 1: Compute A \ B (A NOT B)
    ! ==========================================
    !$omp do schedule(dynamic, 128)
    do I = 1, NumA
       CurrentSubject = BoxesA(I)

       call QueryRTree(TreeB, CurrentSubject, IntersectingIndices, NumIntersections)

       if (NumIntersections == 0) then
          call PushBoxVector(ThreadLocalResults(ThreadId), CurrentSubject)
          cycle
       end if

       call ClearBoxVector(SubjectFragments)
       call PushBoxVector(SubjectFragments, CurrentSubject)

       do J = 1, NumIntersections
          SubtractorBox = BoxesB(IntersectingIndices(J))
          call ClearBoxVector(NextFragments)

          do K = 1, SubjectFragments%Count
             call SubtractBox(SubjectFragments%Elements(K), SubtractorBox, FracturedBoxes, NumFractured)
             do FragIdx = 1, NumFractured
                call PushBoxVector(NextFragments, FracturedBoxes(FragIdx))
             end do
          end do

          call CopyBoxVector(NextFragments, SubjectFragments)
          if (SubjectFragments%Count == 0) exit 
       end do

       do K = 1, SubjectFragments%Count
          call PushBoxVector(ThreadLocalResults(ThreadId), SubjectFragments%Elements(K))
       end do
    end do
    !$omp end do

    ! ==========================================
    ! PHASE 2: Compute B \ A (B NOT A)
    ! ==========================================
    !$omp do schedule(dynamic, 128)
    do I = 1, NumB
       CurrentSubject = BoxesB(I)

       call QueryRTree(TreeA, CurrentSubject, IntersectingIndices, NumIntersections)

       if (NumIntersections == 0) then
          call PushBoxVector(ThreadLocalResults(ThreadId), CurrentSubject)
          cycle
       end if

       call ClearBoxVector(SubjectFragments)
       call PushBoxVector(SubjectFragments, CurrentSubject)

       do J = 1, NumIntersections
          SubtractorBox = BoxesA(IntersectingIndices(J))
          call ClearBoxVector(NextFragments)

          do K = 1, SubjectFragments%Count
             call SubtractBox(SubjectFragments%Elements(K), SubtractorBox, FracturedBoxes, NumFractured)
             do FragIdx = 1, NumFractured
                call PushBoxVector(NextFragments, FracturedBoxes(FragIdx))
             end do
          end do

          call CopyBoxVector(NextFragments, SubjectFragments)
          if (SubjectFragments%Count == 0) exit 
       end do

       do K = 1, SubjectFragments%Count
          call PushBoxVector(ThreadLocalResults(ThreadId), SubjectFragments%Elements(K))
       end do
    end do
    !$omp end do

    ! Cleanup private memory
    call DestroyBoxVector(SubjectFragments)
    call DestroyBoxVector(NextFragments)
    deallocate(IntersectingIndices)

    !$omp end parallel

    ! ==========================================
    ! PHASE 3: Reduction & Gather
    ! ==========================================
    NumResults = 0
    do I = 0, int(NumThreads - 1, kind=int64)
       NumResults = NumResults + ThreadLocalResults(I)%Count
    end do

    allocate(ResultBoxes(NumResults))

    GlobalOffset = 1
    do I = 0, int(NumThreads - 1, kind=int64)
       if (ThreadLocalResults(I)%Count > 0) then
          ResultBoxes(GlobalOffset : GlobalOffset + ThreadLocalResults(I)%Count - 1) = &
               ThreadLocalResults(I)%Elements(1 : ThreadLocalResults(I)%Count)
          GlobalOffset = GlobalOffset + ThreadLocalResults(I)%Count
       end if
       call DestroyBoxVector(ThreadLocalResults(I))
    end do
    deallocate(ThreadLocalResults)

  end subroutine CalculateXORRTree

  ! --- Assumes SubtractBox, InitBoxVector, PushBoxVector, ClearBoxVector, ---
  ! --- CopyBoxVector, DestroyBoxVector, and QueryRTree are implemented    ---
  ! --- identically to the BooleanOpsRTree module.                         ---
  !> Performs a topological "Under-Size" (Morphological Erosion) on a layer
  !> of edge-sharing rectangles. Maintains contiguous shapes without internal tearing.
  subroutine CalculateTopologyShrinkRTree(TreeA, BoxesA, ShrinkVal, ResultBoxes, NumResults)
    type(RTree), intent(in) :: TreeA
    type(Box), intent(in) :: BoxesA(:)
    integer(kind=K_COORDINATE_KIND), intent(in) :: ShrinkVal
    type(Box), allocatable, intent(out) :: ResultBoxes(:)
    integer(kind=int64), intent(out) :: NumResults

    integer(kind=int64) :: NumA, I, J, K, M, FragIdx
    integer(kind=int64) :: NumThreads, ThreadId, GlobalOffset
    type(BoxVector), allocatable :: ThreadLocalResults(:)

    integer(kind=int64), allocatable :: IntersectingIndices(:)
    integer(kind=int64) :: NumIntersections

    type(BoxVector) :: Margins, NextMargins, FinalFragments, NextFragments
    type(Box) :: CurrentSubject, CoreSubject, SearchHalo
    type(Box) :: Neighbor, BloatedNeighbor
    type(Box) :: FracturedBoxes(4)
    integer(kind=int64) :: NumFractured

    NumA = size(BoxesA, kind=int64)
    NumThreads = omp_get_max_threads()

    allocate(ThreadLocalResults(0:NumThreads-1))
    do I = 0, int(NumThreads - 1, kind=int64)
       call InitBoxVector(ThreadLocalResults(I), 2048_int64)
    end do

    !$omp parallel default(none) &
    !$omp shared(BoxesA, TreeA, NumA, ShrinkVal, ThreadLocalResults) &
    !$omp private(I, J, K, M, FragIdx, ThreadId, IntersectingIndices, NumIntersections, &
    !$omp         Margins, NextMargins, FinalFragments, NextFragments, &
    !$omp         CurrentSubject, CoreSubject, SearchHalo, Neighbor, BloatedNeighbor, &
    !$omp         FracturedBoxes, NumFractured)

    ThreadId = omp_get_thread_num()

    allocate(IntersectingIndices(128)) 
    call InitBoxVector(Margins, 16_int64)
    call InitBoxVector(NextMargins, 16_int64)
    call InitBoxVector(FinalFragments, 32_int64)
    call InitBoxVector(NextFragments, 32_int64)

    !$omp do schedule(dynamic, 64)
    do I = 1, NumA
       CurrentSubject = BoxesA(I)

       ! 1. Define the Core (Shrunken representation)
       CoreSubject = Box( &
            CurrentSubject%XMin + ShrinkVal, CurrentSubject%YMin + ShrinkVal, &
            CurrentSubject%XMax - ShrinkVal, CurrentSubject%YMax - ShrinkVal)

       call ClearBoxVector(Margins)

       ! 2. Extract initial margins: Margins = CurrentSubject \ CoreSubject
       ! (If Core is invalid/inverted due to being too thin, SubtractBox treats 
       ! it as empty, and the entire CurrentSubject becomes the Margin)
       if (CoreSubject%XMin < CoreSubject%XMax .and. CoreSubject%YMin < CoreSubject%YMax) then
          call SubtractBox(CurrentSubject, CoreSubject, FracturedBoxes, NumFractured)
          do FragIdx = 1, NumFractured
             call PushBoxVector(Margins, FracturedBoxes(FragIdx))
          end do
       else
          call PushBoxVector(Margins, CurrentSubject)
       end if

       ! If there are no margins (box was completely annihilated), nothing to process
       if (Margins%Count == 0) cycle

       ! 3. Query RTree for neighbors within ShrinkVal distance
       SearchHalo = Box( &
            CurrentSubject%XMin - ShrinkVal, CurrentSubject%YMin - ShrinkVal, &
            CurrentSubject%XMax + ShrinkVal, CurrentSubject%YMax + ShrinkVal)

       call QueryRTree(TreeA, SearchHalo, IntersectingIndices, NumIntersections)

       ! 4. Subtract bloated neighbors from Margins
       do J = 1, NumIntersections
          if (IntersectingIndices(J) == I) cycle ! Skip self

          Neighbor = BoxesA(IntersectingIndices(J))
          BloatedNeighbor = Box( &
               Neighbor%XMin - ShrinkVal, Neighbor%YMin - ShrinkVal, &
               Neighbor%XMax + ShrinkVal, Neighbor%YMax + ShrinkVal)

          call ClearBoxVector(NextMargins)

          do K = 1, Margins%Count
             call SubtractBox(Margins%Elements(K), BloatedNeighbor, FracturedBoxes, NumFractured)
             do FragIdx = 1, NumFractured
                call PushBoxVector(NextMargins, FracturedBoxes(FragIdx))
             end do
          end do

          call CopyBoxVector(NextMargins, Margins)
          if (Margins%Count == 0) exit ! Margins fully protected
       end do

       ! 5. Final Fracture: CurrentSubject \ ExposedMargins
       call ClearBoxVector(FinalFragments)
       call PushBoxVector(FinalFragments, CurrentSubject)

       do M = 1, Margins%Count
          call ClearBoxVector(NextFragments)

          do K = 1, FinalFragments%Count
             call SubtractBox(FinalFragments%Elements(K), Margins%Elements(M), FracturedBoxes, NumFractured)
             do FragIdx = 1, NumFractured
                call PushBoxVector(NextFragments, FracturedBoxes(FragIdx))
             end do
          end do

          call CopyBoxVector(NextFragments, FinalFragments)
       end do

       ! 6. Push surviving geometry to thread output
       do K = 1, FinalFragments%Count
          call PushBoxVector(ThreadLocalResults(ThreadId), FinalFragments%Elements(K))
       end do

    end do
    !$omp end do

    ! Cleanup private memory
    call DestroyBoxVector(Margins)
    call DestroyBoxVector(NextMargins)
    call DestroyBoxVector(FinalFragments)
    call DestroyBoxVector(NextFragments)
    deallocate(IntersectingIndices)

    !$omp end parallel

    ! ==========================================
    ! Reduction & Gather
    ! ==========================================
    NumResults = 0
    do I = 0, int(NumThreads - 1, kind=int64)
       NumResults = NumResults + ThreadLocalResults(I)%Count
    end do

    allocate(ResultBoxes(NumResults))

    GlobalOffset = 1
    do I = 0, int(NumThreads - 1, kind=int64)
       if (ThreadLocalResults(I)%Count > 0) then
          ResultBoxes(GlobalOffset : GlobalOffset + ThreadLocalResults(I)%Count - 1) = &
               ThreadLocalResults(I)%Elements(1 : ThreadLocalResults(I)%Count)
          GlobalOffset = GlobalOffset + ThreadLocalResults(I)%Count
       end if
       call DestroyBoxVector(ThreadLocalResults(I))
    end do
    deallocate(ThreadLocalResults)

  end subroutine CalculateTopologyShrinkRTree

  ! --- Assumes SubtractBox, InitBoxVector, PushBoxVector, ClearBoxVector, ---
  ! --- CopyBoxVector, DestroyBoxVector, and QueryRTree are implemented    ---


end submodule BooleanOpsRTree
