! File   : design_device_impl.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Layer-level GPU residency (see device_registry.f90 for the mechanism and the
!        : safety model). A layer's live boxes layer_boxes(1:n_used) and, when its R-tree is
!        : valid, tree%tree_nodes are kept on the device across CTR operations so that the
!        : kernel wrappers in interaction.f90 / pnummerge.f90 find them present and skip H2D.
!
! Invalidation: every place in design.f90 that reorders/replaces boxes or the tree goes through
! drop_rtree / rebuild_rtree / sort_layer_boxes / ClearLayer / CopyLayer / swap_layer, which
! call LayerDeviceRelease / LayerTreeBuilt here. As a second line of defence a reuse also
! requires the same host address, size, generation tag and a sampled content fingerprint, and
! MAGPARSER_GPU_RESIDENCY_CHECK=1 compares the full device copy with the host on every reuse.
submodule (DesignModule) DesignDeviceImplModule
  use iso_c_binding
  use iso_fortran_env, only: int32, int64
  use DeviceRegistryModule
  use RTreeBuilderGPU, only: gpu_offload_enabled
  use omp_lib, only: omp_in_parallel
  implicit none

  integer(int64), parameter :: M32 = int(z'FFFFFFFF', int64)
  integer, parameter :: K_FP_SAMPLES = 32

contains

  pure integer(int64) function mix(h, v)
    integer(int64), intent(in) :: h, v
    integer(int64) :: x
    x = iand(ieor(h, iand(v, M32)) * 16777619_int64, M32)
    mix = ieor(x, ishft(x, -13))
  end function mix

  pure integer(int64) function boxes_fingerprint(b, n) result(h)
    type(Box), intent(in) :: b(:)
    integer(int64), intent(in) :: n
    integer(int64) :: k, i
    h = iand(n, M32)
    do k = 0, K_FP_SAMPLES
       i = 1_int64 + ((n - 1_int64) * k) / K_FP_SAMPLES
       h = mix(h, int(b(i)%x1, int64)); h = mix(h, int(b(i)%y1, int64))
       h = mix(h, int(b(i)%x2, int64)); h = mix(h, int(b(i)%y2, int64))
    end do
  end function boxes_fingerprint

  pure integer(int64) function tree_fingerprint(t, root) result(h)
    type(RTreeNode), intent(in) :: t(:)
    integer(int64), intent(in) :: root
    integer(int64) :: k, i, n
    n = size(t, kind=int64)
    h = mix(iand(n, M32), root)
    do k = 0, K_FP_SAMPLES
       i = 1_int64 + ((n - 1_int64) * k) / K_FP_SAMPLES
       h = mix(h, int(t(i)%mbr%x1, int64)); h = mix(h, int(t(i)%mbr%y2, int64))
       h = mix(h, t(i)%ChildStart);         h = mix(h, int(t(i)%NumChildren, int64))
    end do
  end function tree_fingerprint

  !> New generation tag: called whenever the layer's boxes or tree change.
  module subroutine LayerDeviceTouch( L )
    type(Layer), intent(inout) :: L
    L%dev_gen = DeviceNextGeneration()
  end subroutine LayerDeviceTouch

  subroutine ensure_one( L )
    type(Layer), intent(inout), target :: L
    integer(c_size_t) :: nb
    integer(int64) :: nt
    logical :: ok
    if( L%n_used <= 0 .or. .not. allocated( L%layer_boxes ) ) return
    if( .not. gpu_offload_enabled( L%n_used ) ) return
    if( L%dev_gen == 0_int64 ) L%dev_gen = DeviceNextGeneration()
    nb = int( L%n_used, c_size_t ) * int( storage_size( L%layer_boxes(1) ) / 8, c_size_t )
    ok = DeviceAcquire( c_loc( L%layer_boxes(1) ), nb, L%dev_gen, &
         boxes_fingerprint( L%layer_boxes, L%n_used ), L%dev_boxes_id )
    L%device_resident = ok
    if( allocated( L%tree%tree_nodes ) .and. L%has( LAYER_STATE_RTREE ) ) then
       nt = size( L%tree%tree_nodes, kind=int64 )
       if( nt > 0 ) then
          nb = int( nt, c_size_t ) * int( storage_size( L%tree%tree_nodes(1) ) / 8, c_size_t )
          ok = DeviceAcquire( c_loc( L%tree%tree_nodes(1) ), nb, L%dev_gen, &
               tree_fingerprint( L%tree%tree_nodes, L%tree%root_index ), L%dev_tree_id )
       end if
    end if
  end subroutine ensure_one

  !> Make A (and B) resident / revalidate their device copies and pin them for one GPU call.
  !> Call immediately before invoking a GPU kernel wrapper on these layers, and call
  !> LayerDeviceDone with the same layers right after it.
  module subroutine LayerDeviceEnsure( A, B )
    type(Layer), intent(inout), target :: A
    type(Layer), intent(inout), target, optional :: B
    if( .not. DeviceRegistryEnabled() ) return
    if( omp_in_parallel() ) return   !> tile workers of the frame functions run on the host
    call DeviceResetPins()   !> serial context only: no GPU call can be in flight
    !$omp critical (magparser_layer_residency)
    call ensure_one( A )
    if( present( B ) ) then
       if( .not. same_layer( A, B ) ) call ensure_one( B )
    end if
    !$omp end critical (magparser_layer_residency)
  end subroutine LayerDeviceEnsure

  logical function same_layer( A, B )
    type(Layer), intent(in), target :: A, B
    type(c_ptr) :: pa, pb
    pa = c_loc( A ); pb = c_loc( B )      !> temporaries: gfortran rejects c_loc() inline here
    same_layer = c_associated( pa, pb )
  end function same_layer

  !> Unpin the device copies pinned by LayerDeviceEnsure (they stay resident for reuse).
  module subroutine LayerDeviceDone( A, B )
    type(Layer), intent(inout), target :: A
    type(Layer), intent(inout), target, optional :: B
    if( .not. DeviceRegistryEnabled() ) return
    if( omp_in_parallel() ) return
    call DeviceUnpin( A%dev_boxes_id ); call DeviceUnpin( A%dev_tree_id )
    if( present( B ) ) then
       if( .not. same_layer( A, B ) ) then
          call DeviceUnpin( B%dev_boxes_id ); call DeviceUnpin( B%dev_tree_id )
       end if
    end if
  end subroutine LayerDeviceDone

  !> Drop the layer's device copies (before its boxes/tree are reordered, replaced or freed).
  module subroutine LayerDeviceRelease( L )
    type(Layer), intent(inout), target :: L
    if( L%dev_boxes_id > 0 ) then
       if( allocated( L%layer_boxes ) .and. L%n_used > 0 ) then
          call DeviceReleaseId( L%dev_boxes_id, c_loc( L%layer_boxes(1) ) )
       else
          call DeviceReleaseId( L%dev_boxes_id, c_null_ptr )
       end if
    end if
    if( L%dev_tree_id > 0 ) then
       if( allocated( L%tree%tree_nodes ) ) then
          if( size( L%tree%tree_nodes ) > 0 ) then
             call DeviceReleaseId( L%dev_tree_id, c_loc( L%tree%tree_nodes(1) ) )
          else
             call DeviceReleaseId( L%dev_tree_id, c_null_ptr )
          end if
       else
          call DeviceReleaseId( L%dev_tree_id, c_null_ptr )
       end if
    end if
    L%dev_boxes_id = 0
    L%dev_tree_id = 0
    L%device_resident = .false.
    L%dev_gen = DeviceNextGeneration()
  end subroutine LayerDeviceRelease

  !> The layer's tree was (re)built over its current boxes: new generation; the next
  !> LayerDeviceEnsure uploads boxes+tree once and later operations reuse them.
  module subroutine LayerTreeBuilt( L )
    type(Layer), intent(inout), target :: L
    call LayerDeviceRelease( L )
  end subroutine LayerTreeBuilt

end submodule DesignDeviceImplModule
