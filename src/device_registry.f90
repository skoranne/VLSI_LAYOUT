! File   : device_registry.f90
! Author : Sandeep Koranne (C) 2026.
! Purpose: Persistent GPU residency for host arrays (layer boxes / R-tree nodes) across
!        : CTR operations, using OpenMP device memory routines.
!
! Mechanism
!   A host range [host, host+nbytes) is made resident with omp_target_alloc + omp_target_memcpy
!   + omp_target_associate_ptr. Afterwards every `map(to:/tofrom:)` of that range in any target
!   region finds it "present" and performs no transfer, so the existing kernel wrappers need no
!   signature change.
!
! Safety model (why this cannot silently use stale data)
!   * The owner (LayerDeviceEnsure in design_device_impl.f90) validates or (re)uploads its ranges
!     immediately before a GPU call and PINS them; LayerDeviceDone unpins after the call.
!     Validation = exact host address + size + owner generation tag + sampled fingerprint.
!   * Every wrapper calls DevicePrepare(ptr, nbytes) on each host array it is about to map.
!     A registered range overlapping that array is kept only if it is exactly that array and
!     currently pinned (i.e. validated by its owner for this very call); otherwise it is dropped.
!     Host memory that was freed and reused (layers are copied/reallocated in many places)
!     therefore can never alias an old device buffer.
!   * Pinned ranges are never evicted or dropped by other callers, so wrappers running
!     concurrently from several host threads (tiled frame operations) cannot free a buffer
!     another thread's kernel is using. All registry state is guarded by one named critical
!     section; when not inside a parallel region, leftover pins are reset (none can be live).
!   * MAGPARSER_GPU_RESIDENCY_CHECK=1 additionally copies a reused buffer back and compares it
!     byte-for-byte with the host copy (stops with "STALE DEVICE DATA" on mismatch).
!
! Environment
!   MAGPARSER_GPU_RESIDENCY=0        disable residency (per-call maps only; A/B timing)
!   MAGPARSER_GPU_MEM_BUDGET=<GB>    device bytes the registry may hold (default min(40% of free, 8 GB))
!   MAGPARSER_GPU_RESIDENCY_CHECK=1  verify reused buffers against the host (debug/regression)
!   MAGPARSER_GPU_RESIDENCY_STATS=1  print upload/reuse statistics at the end of the run
module DeviceRegistryModule
  use iso_c_binding
  use iso_fortran_env, only: int8, int32, int64, real64
  use omp_lib
  implicit none
  private
#if defined(__GFORTRAN__) && (__GNUC__ < 10)
  !> gfortran < 10: omp_lib has no Fortran interfaces for the OpenMP 4.5 device memory
  !> routines, but libgomp exports them as C functions (same semantics).
  interface
     type(c_ptr) function omp_target_alloc(size, device_num) bind(c, name='omp_target_alloc')
       import :: c_ptr, c_size_t, c_int
       integer(c_size_t), value :: size
       integer(c_int), value :: device_num
     end function omp_target_alloc
     subroutine omp_target_free(device_ptr, device_num) bind(c, name='omp_target_free')
       import :: c_ptr, c_int
       type(c_ptr), value :: device_ptr
       integer(c_int), value :: device_num
     end subroutine omp_target_free
     integer(c_int) function omp_target_is_present(ptr, device_num) bind(c, name='omp_target_is_present')
       import :: c_ptr, c_int
       type(c_ptr), value :: ptr
       integer(c_int), value :: device_num
     end function omp_target_is_present
     integer(c_int) function omp_target_memcpy(dst, src, length, dst_offset, src_offset, dst_device_num, &
          src_device_num) bind(c, name='omp_target_memcpy')
       import :: c_ptr, c_size_t, c_int
       type(c_ptr), value :: dst, src
       integer(c_size_t), value :: length, dst_offset, src_offset
       integer(c_int), value :: dst_device_num, src_device_num
     end function omp_target_memcpy
     integer(c_int) function omp_target_associate_ptr(host_ptr, device_ptr, size, device_offset, device_num) &
          bind(c, name='omp_target_associate_ptr')
       import :: c_ptr, c_size_t, c_int
       type(c_ptr), value :: host_ptr, device_ptr
       integer(c_size_t), value :: size, device_offset
       integer(c_int), value :: device_num
     end function omp_target_associate_ptr
     integer(c_int) function omp_target_disassociate_ptr(ptr, device_num) bind(c, name='omp_target_disassociate_ptr')
       import :: c_ptr, c_int
       type(c_ptr), value :: ptr
       integer(c_int), value :: device_num
     end function omp_target_disassociate_ptr
  end interface
#endif

  public :: DeviceRegistryEnabled, DeviceAcquire, DeviceUnpin, DeviceReleaseId, &
       DevicePrepare, DevicePrintStats, DeviceNextGeneration, DeviceDropAll, DeviceResetPins

  type :: DevEntry
     type(c_ptr)       :: host = c_null_ptr
     type(c_ptr)       :: dev  = c_null_ptr
     integer(c_size_t) :: nbytes = 0
     integer(int64)    :: tag = 0        !> owner generation
     integer(int64)    :: fp = 0         !> owner fingerprint of the contents
     integer           :: pins = 0       !> >0 while a GPU call validated by the owner is using it
     integer(int64)    :: last_use = 0
     logical           :: used = .false.
  end type DevEntry

  interface
     !> cudaMemGetInfo wrapper (gpu_morton.cu); returns 0 on success, nonzero if unavailable.
     integer(c_int) function magparser_gpu_mem_info(free_b, total_b) bind(c, name="magparser_gpu_mem_info")
       import :: c_int, c_size_t
       integer(c_size_t), intent(out) :: free_b, total_b
     end function magparser_gpu_mem_info
  end interface

  type(DevEntry), allocatable, save :: reg(:)
  integer,        save :: reg_state = -1          !> -1 uninitialised, 0 disabled, 1 enabled
  logical,        save :: check_mode = .false., stats_mode = .false.
  integer,        save :: dev_id = 0, host_id = -1
  integer(int64), save :: use_clock = 0, generation = 0
  integer(c_size_t), save :: budget = 0, in_use = 0, peak_in_use = 0
  integer(int64), save :: n_upload = 0, n_reuse = 0, n_evict = 0, n_stale = 0, n_fail = 0, n_busy = 0
  integer(int64), save :: bytes_upload = 0, bytes_reuse = 0

contains

  logical function env_true(name, default)
    character(len=*), intent(in) :: name
    logical, intent(in) :: default
    character(len=32) :: v
    integer :: l, st
    call get_environment_variable(name, v, l, st)
    if (st /= 0 .or. l == 0) then
       env_true = default
    else
       env_true = .not. (v(1:1) == '0' .or. v(1:1) == 'n' .or. v(1:1) == 'N' .or. v(1:1) == 'f' .or. v(1:1) == 'F')
    end if
  end function env_true

  subroutine registry_init()
    character(len=32) :: v
    integer :: l, st
    integer(c_size_t) :: free_b, total_b
    real(real64) :: gb
    reg_state = 0
    if (omp_get_num_devices() <= 0) return
    call get_environment_variable('OMP_TARGET_OFFLOAD', v, l, st)
    if (st == 0 .and. l > 0) then
       if (v(1:l) == 'DISABLED' .or. v(1:l) == 'disabled') return
    end if
    if (.not. env_true('MAGPARSER_GPU_RESIDENCY', .true.)) return
    check_mode = env_true('MAGPARSER_GPU_RESIDENCY_CHECK', .false.)
    stats_mode = env_true('MAGPARSER_GPU_RESIDENCY_STATS', .false.)
    dev_id  = omp_get_default_device()
    host_id = omp_get_initial_device()
    budget = 8_c_size_t * 1024_c_size_t**3
    if (magparser_gpu_mem_info(free_b, total_b) == 0 .and. free_b > 0) then
       ! Capped at 8 GB: on unified-memory parts (GB10/Grace) device copies come out of the same
       ! LPDDR as the host arrays they duplicate, and 40% of "free" there is ~30 GB of copies.
       budget = min( int(0.40_real64 * real(free_b, real64), c_size_t), 8_c_size_t * 1024_c_size_t**3 )
    end if
    call get_environment_variable('MAGPARSER_GPU_MEM_BUDGET', v, l, st)
    if (st == 0 .and. l > 0) then
       read(v(1:l), *, iostat=st) gb
       if (st == 0 .and. gb >= 0.0_real64) budget = int(gb * 1024.0_real64**3, c_size_t)
    end if
    allocate(reg(64))
    reg_state = 1
  end subroutine registry_init

  !> True if persistent residency is active (device present, offload and residency enabled).
  logical function DeviceRegistryEnabled() result(on)
    if (reg_state < 0) then
       !$omp critical (magparser_device_registry)
       if (reg_state < 0) call registry_init()
       !$omp end critical (magparser_device_registry)
    end if
    on = (reg_state == 1)
  end function DeviceRegistryEnabled

  !> Globally unique, monotonically increasing owner generation tag.
  integer(int64) function DeviceNextGeneration() result(g)
    !$omp atomic capture
    generation = generation + 1_int64
    g = generation
    !$omp end atomic
  end function DeviceNextGeneration

  pure integer(c_intptr_t) function addr(p)
    type(c_ptr), intent(in) :: p
    addr = transfer(p, 0_c_intptr_t)
  end function addr

  ! ---- internal helpers: caller holds the critical section ---------------------------------

  subroutine drop_entry(i)
    integer, intent(in) :: i
    integer :: rc
    if (.not. reg(i)%used) return
    rc = omp_target_disassociate_ptr(reg(i)%host, dev_id)
    if (rc /= 0) then
       ! Never free a buffer the runtime still maps: that would turn a stale mapping into a
       ! dangling one. Report and keep the entry (it stays unusable for reuse via its tag).
       write(*,'(A,I0)') 'WARNING: omp_target_disassociate_ptr failed, rc=', rc
       return
    end if
    call omp_target_free(reg(i)%dev, dev_id)
    in_use = in_use - reg(i)%nbytes
    reg(i) = DevEntry()
  end subroutine drop_entry

  logical function overlaps(i, a0, a1)
    integer, intent(in) :: i
    integer(c_intptr_t), intent(in) :: a0, a1
    overlaps = addr(reg(i)%host) < a1 .and. a0 < addr(reg(i)%host) + int(reg(i)%nbytes, c_intptr_t)
  end function overlaps

  !> Evict least-recently-used unpinned ranges until `need` fits in the budget.
  logical function make_room(need) result(ok)
    integer(c_size_t), intent(in) :: need
    integer :: i, victim
    integer(int64) :: oldest
    ok = .false.
    if (need > budget) return
    do while (in_use + need > budget)
       victim = 0; oldest = huge(oldest)
       do i = 1, size(reg)
          if (reg(i)%used .and. reg(i)%pins == 0 .and. reg(i)%last_use < oldest) then
             oldest = reg(i)%last_use; victim = i
          end if
       end do
       if (victim == 0) return
       call drop_entry(victim)
       if (reg(victim)%used) return       ! could not drop
       n_evict = n_evict + 1_int64
    end do
    ok = .true.
  end function make_room

  integer function free_slot() result(slot)
    type(DevEntry), allocatable :: tmp(:)
    integer :: i, n
    do i = 1, size(reg)
       if (.not. reg(i)%used) then
          slot = i; return
       end if
    end do
    n = size(reg)
    allocate(tmp(2*n))
    tmp(1:n) = reg
    call move_alloc(tmp, reg)
    slot = n + 1
  end function free_slot

  !> Debug: device copy must equal the host bytes.
  subroutine verify_entry(i)
    integer, intent(in) :: i
    integer(int8), allocatable, target :: buf(:)
    integer(int8), pointer :: hostb(:)
    integer :: rc
    allocate(buf(reg(i)%nbytes))
    rc = omp_target_memcpy(c_loc(buf), reg(i)%dev, reg(i)%nbytes, 0_c_size_t, 0_c_size_t, host_id, dev_id)
    call c_f_pointer(reg(i)%host, hostb, [reg(i)%nbytes])
    if (rc /= 0 .or. any(buf /= hostb)) then
       write(*,'(A,I0,A,I0,A)') 'STALE DEVICE DATA: resident range of ', reg(i)%nbytes, &
            ' bytes (tag ', reg(i)%tag, ') differs from host copy'
       error stop "STALE DEVICE DATA: a host mutation did not invalidate GPU residency"
    end if
  end subroutine verify_entry

  ! ---- public API ---------------------------------------------------------------------------

  !> Reset pins when no GPU call can be in flight (serial context). Called by LayerDeviceEnsure
  !> outside parallel regions so that a missed LayerDeviceDone can never pin a range forever.
  subroutine DeviceResetPins()
    integer :: i
    if (reg_state /= 1) return
    if (omp_in_parallel()) return
    !$omp critical (magparser_device_registry)
    do i = 1, size(reg)
       reg(i)%pins = 0
    end do
    !$omp end critical (magparser_device_registry)
  end subroutine DeviceResetPins

  !> Make [host, host+nbytes) resident (reusing the device copy when tag and fingerprint still
  !> match) and pin it. Returns .true. if the range is resident and pinned (id = registry slot);
  !> .false. if residency is disabled / does not fit / the range is busy (callers then rely on
  !> the per-call maps inside the kernel wrappers).
  logical function DeviceAcquire(host, nbytes, tag, fp, id) result(ok)
    type(c_ptr), intent(in) :: host
    integer(c_size_t), intent(in) :: nbytes
    integer(int64), intent(in) :: tag, fp
    integer, intent(inout) :: id
    integer :: i, rc
    integer(c_intptr_t) :: a0, a1
    logical :: busy, done
    type(c_ptr) :: d
    ok = .false.
    if (.not. DeviceRegistryEnabled()) then
       id = 0; return
    end if
    if (nbytes == 0 .or. .not. c_associated(host)) then
       id = 0; return
    end if
    a0 = addr(host); a1 = a0 + int(nbytes, c_intptr_t)
    !$omp critical (magparser_device_registry)
    done = .false.
    use_clock = use_clock + 1_int64
    ! Fast path: exact match still valid for this owner state.
    do i = 1, size(reg)
       if (.not. reg(i)%used) cycle
       if (addr(reg(i)%host) /= a0) cycle
       if (reg(i)%nbytes == nbytes .and. reg(i)%tag == tag .and. reg(i)%fp == fp) then
          reg(i)%pins = reg(i)%pins + 1
          reg(i)%last_use = use_clock
          n_reuse = n_reuse + 1_int64
          bytes_reuse = bytes_reuse + int(nbytes, int64)
          if (check_mode) call verify_entry(i)
          id = i; ok = .true.; done = .true.
       end if
       exit
    end do
    if (.not. done) then
       ! Anything overlapping is stale for this owner (or an old mapping of freed memory) unless
       ! another thread has it pinned, in which case we do not touch it and stay non-resident.
       busy = .false.
       do i = 1, size(reg)
          if (.not. reg(i)%used) cycle
          if (.not. overlaps(i, a0, a1)) cycle
          if (reg(i)%pins > 0) then
             busy = .true.
          else
             call drop_entry(i)
             if (reg(i)%used) busy = .true.
             n_stale = n_stale + 1_int64
          end if
       end do
       if (busy .or. omp_target_is_present(host, dev_id) /= 0) then
          ! Range is mapped by someone else right now (or could not be cleared): no residency.
          n_busy = n_busy + 1_int64
          id = 0; done = .true.
       end if
    end if
    if (.not. done) then
       if (.not. make_room(nbytes)) then
          n_fail = n_fail + 1_int64
          id = 0; done = .true.
       end if
    end if
    if (.not. done) then
       d = omp_target_alloc(nbytes, dev_id)
       if (.not. c_associated(d)) then
          n_fail = n_fail + 1_int64
          id = 0; done = .true.
       end if
    end if
    if (.not. done) then
       rc = omp_target_memcpy(d, host, nbytes, 0_c_size_t, 0_c_size_t, dev_id, host_id)
       if (rc == 0) rc = omp_target_associate_ptr(host, d, nbytes, 0_c_size_t, dev_id)
       if (rc /= 0) then
          call omp_target_free(d, dev_id)
          n_fail = n_fail + 1_int64
          id = 0
       else
          i = free_slot()
          reg(i) = DevEntry(host=host, dev=d, nbytes=nbytes, tag=tag, fp=fp, pins=1, last_use=use_clock, used=.true.)
          in_use = in_use + nbytes
          peak_in_use = max(peak_in_use, in_use)
          n_upload = n_upload + 1_int64
          bytes_upload = bytes_upload + int(nbytes, int64)
          id = i; ok = .true.
       end if
    end if
    !$omp end critical (magparser_device_registry)
  end function DeviceAcquire

  !> Unpin slot `id` after the GPU call that LayerDeviceEnsure prepared (keeps the copy resident).
  subroutine DeviceUnpin(id)
    integer, intent(in) :: id
    if (reg_state /= 1 .or. id <= 0) return
    !$omp critical (magparser_device_registry)
    if (id <= size(reg)) then
       if (reg(id)%used .and. reg(id)%pins > 0) reg(id)%pins = reg(id)%pins - 1
    end if
    !$omp end critical (magparser_device_registry)
  end subroutine DeviceUnpin

  !> Release the range in slot `id` if it still describes `host` (owner check), or
  !> unconditionally when host is null (the owner's array is already gone). Clears id.
  !> (A layer copied by intrinsic assignment may carry the same id; the owner check keeps a
  !> copy from freeing the original's buffer. A wrong drop would only cost a re-upload.)
  subroutine DeviceReleaseId(id, host)
    integer, intent(inout) :: id
    type(c_ptr), intent(in) :: host
    if (reg_state == 1 .and. id > 0) then
       !$omp critical (magparser_device_registry)
       if (id <= size(reg)) then
          if (reg(id)%used .and. reg(id)%pins == 0) then
             if (.not. c_associated(host) .or. addr(reg(id)%host) == addr(host)) call drop_entry(id)
          end if
       end if
       !$omp end critical (magparser_device_registry)
    end if
    id = 0
  end subroutine DeviceReleaseId

  !> Called by kernel wrappers for every host array they map. Drops registered ranges that
  !> overlap the array unless they are its exact (address and size), currently pinned copy.
  subroutine DevicePrepare(host, nbytes)
    type(c_ptr), intent(in) :: host
    integer(c_size_t), intent(in) :: nbytes
    integer :: i
    integer(c_intptr_t) :: a0, a1
    if (reg_state /= 1) return
    if (nbytes == 0 .or. .not. c_associated(host)) return
    a0 = addr(host); a1 = a0 + int(nbytes, c_intptr_t)
    !$omp critical (magparser_device_registry)
    do i = 1, size(reg)
       if (.not. reg(i)%used) cycle
       if (.not. overlaps(i, a0, a1)) cycle
       if (reg(i)%pins > 0) then
          if (addr(reg(i)%host) == a0 .and. nbytes == reg(i)%nbytes) cycle   ! validated for this call
          ! Pinned range that is not exactly this array: its owner is live, so this overlap is
          ! impossible unless memory is aliased; refuse rather than corrupt.
          error stop "DevicePrepare: array overlaps a pinned resident range of a different shape"
       end if
       call drop_entry(i)
       n_stale = n_stale + 1_int64
    end do
    !$omp end critical (magparser_device_registry)
  end subroutine DevicePrepare

  subroutine DeviceDropAll()
    integer :: i
    if (reg_state /= 1) return
    !$omp critical (magparser_device_registry)
    do i = 1, size(reg)
       call drop_entry(i)
    end do
    !$omp end critical (magparser_device_registry)
  end subroutine DeviceDropAll

  subroutine DevicePrintStats()
    if (reg_state /= 1 .or. .not. stats_mode) return
    write(*,'(A,I0,A,F0.3,A,I0,A,F0.3,A,I0,A,I0,A,I0,A,I0,A,F0.3,A,F0.3)') &
         'GPU_RESIDENCY uploads=', n_upload, ' upload_GB=', real(bytes_upload, real64)/1024.0_real64**3, &
         ' reuses=', n_reuse, ' reused_GB=', real(bytes_reuse, real64)/1024.0_real64**3, &
         ' evictions=', n_evict, ' stale_drops=', n_stale, ' busy=', n_busy, ' failures=', n_fail, &
         ' peak_GB=', real(peak_in_use, real64)/1024.0_real64**3, ' budget_GB=', real(budget, real64)/1024.0_real64**3
  end subroutine DevicePrintStats

end module DeviceRegistryModule
