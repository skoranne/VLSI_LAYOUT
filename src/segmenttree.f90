module segment_tree_mod
   use iso_fortran_env, only: int32, int64
   implicit none
   private
   public :: calculate_union_area_fast

contains

   !--------------------------------------------------------------
   ! $O(N \log N)$ Union Area using a Segment Tree
   !--------------------------------------------------------------
   pure function calculate_union_area_fast(boxes) result(area)
      type(Box), intent(in) :: boxes(:)
      integer(int64) :: area

      integer :: n, num_y, i
      type(Event), allocatable :: events(:)
      integer(int32), allocatable :: y_vals(:), unique_y(:)
      integer(int64) :: current_x, dx, covered_y
      integer :: j1, j2
      
      ! Segment Tree Arrays
      integer(int32), allocatable :: tree_count(:)
      integer(int64), allocatable :: tree_length(:)
      integer(int64) :: tree_size

      n = size(boxes)
      area = 0_int64
      if (n == 0) return

      ! 1. Collect and compress Y coordinates
      allocate(y_vals(2*n))
      do i = 1, n
         y_vals(2*i - 1) = min(boxes(i)%y1, boxes(i)%y2)
         y_vals(2*i)     = max(boxes(i)%y1, boxes(i)%y2)
      end do

      call sort_int_array(y_vals)

      allocate(unique_y(2*n))
      num_y = 1
      unique_y(1) = y_vals(1)
      do i = 2, 2*n
         if (y_vals(i) /= unique_y(num_y)) then
            num_y = num_y + 1
            unique_y(num_y) = y_vals(i)
         end if
      end do

      ! 2. Create Event Queue
      allocate(events(2*n))
      do i = 1, n
         events(2*i - 1)%x          = min(boxes(i)%x1, boxes(i)%x2)
         events(2*i - 1)%y1         = min(boxes(i)%y1, boxes(i)%y2)
         events(2*i - 1)%y2         = max(boxes(i)%y1, boxes(i)%y2)
         events(2*i - 1)%lap_change = 1

         events(2*i)%x              = max(boxes(i)%x1, boxes(i)%x2)
         events(2*i)%y1             = min(boxes(i)%y1, boxes(i)%y2)
         events(2*i)%y2             = max(boxes(i)%y1, boxes(i)%y2)
         events(2*i)%lap_change     = -1
      end do

      call sort_events(events)

      ! 3. Initialize Segment Tree
      ! A segment tree requires 4 * N memory to safely hold all nodes
      tree_size = 4_int64 * num_y
      allocate(tree_count(tree_size))
      allocate(tree_length(tree_size))
      tree_count = 0
      tree_length = 0_int64

      ! 4. The Optimized Sweep Line Algorithm
      area = 0_int64
      current_x = events(1)%x

      do i = 1, 2*n
         dx = int(events(i)%x, int64) - current_x

         if (dx > 0) then
            ! O(1) Lookup: The root node of the tree (index 1) ALWAYS holds the total covered Y!
            covered_y = tree_length(1)
            area = area + (dx * covered_y)
            current_x = int(events(i)%x, int64)
         end if

         ! Get index boundaries
         j1 = binary_search_y(unique_y, num_y, events(i)%y1)
         j2 = binary_search_y(unique_y, num_y, events(i)%y2)

         ! O(log N) Update: Push the lap change into the Segment Tree
         if (j1 < j2) then
            call update_tree(1, 1, num_y - 1, j1, j2 - 1, events(i)%lap_change, &
                             unique_y, tree_count, tree_length)
         end if
      end do

   end function calculate_union_area_fast


   !--------------------------------------------------------------
   ! Segment Tree Recursive Update Function
   !--------------------------------------------------------------
   pure recursive subroutine update_tree(node, start_idx, end_idx, l, r, val, unique_y, count, length)
      integer, intent(in) :: node, start_idx, end_idx, l, r
      integer(int32), intent(in) :: val
      integer(int32), intent(in) :: unique_y(:)
      integer(int32), intent(inout) :: count(:)
      integer(int64), intent(inout) :: length(:)
      
      integer :: mid
      integer :: left_child, right_child

      ! If the current node perfectly matches the query range
      if (l <= start_idx .and. end_idx <= r) then
         count(node) = count(node) + val
      else
         ! Otherwise, split the query and push to children
         mid = start_idx + (end_idx - start_idx) / 2
         left_child = 2 * node
         right_child = 2 * node + 1

         if (l <= mid) then
            call update_tree(left_child, start_idx, mid, l, r, val, unique_y, count, length)
         end if
         if (r > mid) then
            call update_tree(right_child, mid + 1, end_idx, l, r, val, unique_y, count, length)
         end if
      end if

      ! Recalculate this node's total covered length
      if (count(node) > 0) then
         ! If this exact segment is fully covered by at least one box
         length(node) = int(unique_y(end_idx + 1) - unique_y(start_idx), int64)
      else if (start_idx == end_idx) then
         ! If it's a leaf node with 0 count
         length(node) = 0_int64
      else
         ! If it's a parent node, its length is the sum of its children's lengths
         length(node) = length(2 * node) + length(2 * node + 1)
      end if

   end subroutine update_tree

end module segment_tree_mod
