! Load GDSII/OASIS into MAGPARSER Design structures via src/db C library
module LayoutDBLoaderModule
  use iso_c_binding
  use CommonModule
  use hash_mod
  use DesignModule
  use GeometryModule, only: Box
  use layoutdb_f_api
  implicit none
  private
  public :: LoadLayoutDBFile, LoadLayoutDBFileHier

  type :: ldb_load_ctx
     type(Design), pointer :: design => null()
     integer :: max_layers = 0
     ! Consecutive boxes nearly always share a layer: remember the last one
     ! so that the name formatting and hash lookup run once per run of boxes.
     integer(c_int) :: last_layer_num = -1, last_datatype = -1
     integer :: last_layer_idx = 0
  end type ldb_load_ctx

contains

  subroutine box_callback(ud, cell, layer_num, datatype, element_kind, x1,y1,x2,y2) bind(C)
    type(c_ptr), value :: ud
    type(c_ptr), value :: cell
    integer(c_int), value :: layer_num, datatype, element_kind
    integer(c_int64_t), value :: x1,y1,x2,y2
    type(ldb_load_ctx), pointer :: ctx
    integer :: layer_idx
    logical :: ins
    character(len=64) :: buf
    call c_f_pointer(ud, ctx)
    if(.not. associated(ctx)) return
    if(.not. associated(ctx%design)) return
    ! Zero-area shapes contribute nothing and are rejected downstream as INVALID BOX.
    if(x1 == x2 .or. y1 == y2) return
    if(ctx%last_layer_idx > 0 .and. layer_num == ctx%last_layer_num .and. datatype == ctx%last_datatype) then
       call store_box(ctx, ctx%last_layer_idx, x1, y1, x2, y2)
       return
    end if
    write(buf,'("L",I0,"_D",I0)') layer_num, datatype
    call hash_get(ctx%design%ht, trim(buf), layer_idx, ins)
    if(.not. ins) then
       layer_idx = int(hash_nitems(ctx%design%ht)) + 1
       ! count unique layers by tracking via canonical name; but simpler: assign new slot based on existing unique count
       ! find if any alias exists
       block
         character(len=64) :: k2,k3,k4,k5
         logical :: insa
         integer :: idx2
         write(k2,'("L",I0,"_",I0)') layer_num, datatype
         write(k3,'(I0,":",I0)') layer_num, datatype
         write(k4,'(I0,"_",I0)') layer_num, datatype
         write(k5,'(I0,"_D",I0)') layer_num, datatype
         call hash_get(ctx%design%ht, trim(k2), idx2, insa)
         if(.not. insa) call hash_get(ctx%design%ht, trim(k3), idx2, insa)
         if(.not. insa) call hash_get(ctx%design%ht, trim(k4), idx2, insa)
         if(.not. insa) call hash_get(ctx%design%ht, trim(k5), idx2, insa)
         if(insa) then
            layer_idx = idx2
            ins = .true.
         end if
       end block
    end if
    if(.not. ins) then
       ! determine next unique layer slot by counting layerNames entries
       layer_idx = 0
       block
         integer :: j
         do j=1,size(ctx%design%layerNames)
            if(len_trim(ctx%design%layerNames(j)) == 0) then
               layer_idx = j; exit
            end if
         end do
         if(layer_idx == 0) layer_idx = min(int(hash_nitems(ctx%design%ht))+1, ctx%max_layers)
       end block
       if(layer_idx > ctx%max_layers) return
       if(layer_idx <= size(ctx%design%layerNames)) ctx%design%layerNames(layer_idx) = trim(buf)
       ctx%design%layers(layer_idx)%n_used = 0
       ctx%design%layers(layer_idx)%n_alloc = 0
       ! register canonical and aliases
       block
         character(len=64) :: a1,a2,a3,a4
         logical :: ins2
         write(a1,'("L",I0,"_D",I0)') layer_num, datatype
         write(a2,'("L",I0,"_",I0)') layer_num, datatype
         write(a3,'(I0,":",I0)') layer_num, datatype
         write(a4,'(I0,"_",I0)') layer_num, datatype
         call hash_put(ctx%design%ht, trim(a1), layer_idx, ins2)
         call hash_put(ctx%design%ht, trim(a2), layer_idx, ins2)
         call hash_put(ctx%design%ht, trim(a3), layer_idx, ins2)
         call hash_put(ctx%design%ht, trim(a4), layer_idx, ins2)
       end block
    end if
    ctx%last_layer_num = layer_num
    ctx%last_datatype  = datatype
    ctx%last_layer_idx = layer_idx
    call store_box(ctx, layer_idx, x1, y1, x2, y2)
  end subroutine box_callback

  subroutine store_box(ctx, layer_idx, x1, y1, x2, y2)
    type(ldb_load_ctx), intent(inout) :: ctx
    integer, intent(in) :: layer_idx
    integer(c_int64_t), intent(in) :: x1, y1, x2, y2
    integer(kind=K_COORDINATE_KIND) :: bx1, by1, bx2, by2
    bx1 = int(min(x1,x2), kind=K_COORDINATE_KIND); bx2 = int(max(x1,x2), kind=K_COORDINATE_KIND)
    by1 = int(min(y1,y2), kind=K_COORDINATE_KIND); by2 = int(max(y1,y2), kind=K_COORDINATE_KIND)
    call push_box_local(ctx%design, layer_idx, bx1, by1, bx2, by2)
    associate(e => ctx%design%DESIGN_EXTENT)
      e%x1 = min(e%x1, bx1); e%y1 = min(e%y1, by1)
      e%x2 = max(e%x2, bx2); e%y2 = max(e%y2, by2)
    end associate
  end subroutine store_box

  subroutine push_box_local(d, layer_idx, x1,y1,x2,y2)
    type(Design), intent(inout) :: d
    integer, intent(in) :: layer_idx
    integer(kind=K_COORDINATE_KIND), intent(in) :: x1,y1,x2,y2
    type(Box), allocatable :: tmp(:)
    integer :: n, na, new_na
    associate(L => d%layers(layer_idx))
      n = int(L%n_used); na = int(L%n_alloc)
      if(.not. allocated(L%layer_boxes)) then
         allocate(L%layer_boxes(256)); L%n_alloc=256; na=256; L%n_used=0; n=0
      end if
      if(n >= na) then
         ! 1.5x growth reduces peak memory vs 2x while keeping amortized copies low
         new_na = max(na + na/2, na + 256)
         allocate(tmp(new_na))
         tmp(1:n)=L%layer_boxes(1:n)
         call move_alloc(tmp, L%layer_boxes)
         L%n_alloc = new_na
      end if
      L%n_used = L%n_used + 1
      L%layer_boxes(L%n_used)%x1 = min(x1,x2)
      L%layer_boxes(L%n_used)%y1 = min(y1,y2)
      L%layer_boxes(L%n_used)%x2 = max(x1,x2)
      L%layer_boxes(L%n_used)%y2 = max(y1,y2)
    end associate
  end subroutine push_box_local

  subroutine load_ldb_common(load_design, MAX_LAYERS, fh_out, ctx)
    type(Design), intent(inout), target :: load_design
    integer, intent(in) :: MAX_LAYERS
    type(c_ptr), intent(out) :: fh_out
    type(ldb_load_ctx), intent(out), target :: ctx
    character(kind=c_char), allocatable :: cpath(:)
    character(kind=c_char) :: errbuf(512)
    integer(c_size_t) :: esz
    integer :: i, n
    character(len=:), allocatable :: fname
    if (.not. allocated(load_design%fileName)) then
       write(*,*) 'ERROR: ldb_open failed: load_design%fileName not allocated'
       fh_out = c_null_ptr
       return
    end if
    n = len_trim(load_design%fileName)
    allocate(character(len=n) :: fname)
    fname = load_design%fileName(1:n)
    allocate(cpath(n+1))
    do i=1,n
       cpath(i) = fname(i:i)
    end do
    cpath(n+1) = c_null_char
    errbuf = c_null_char
    esz = 512
    block
       type(ldb_open_opts_c_t) :: opts
       opts%flags = LDB_OPEN_PROPERTY_F + LDB_OPEN_LOW_MEMORY_F
       opts%large_cell_box_threshold = 100000_c_int64_t
       fh_out = ldb_open_opts_c(cpath, opts, errbuf, esz)
    end block
    if(.not. c_associated(fh_out)) then
       block
          integer :: elen
          character(len=512) :: emsg
          emsg = ''
          elen = 0
          do i = 1, 512
             if (errbuf(i) == c_null_char) exit
             elen = i
             emsg(i:i) = errbuf(i)
          end do
          if (elen > 0) then
             write(*,*) 'ERROR: ldb_open failed for ', trim(fname), ': ', emsg(1:elen)
          else
             write(*,*) 'ERROR: ldb_open failed for ', trim(fname)
          end if
       end block
       return
    end if
    if(ldb_is_strict_c(fh_out) /= 0) write(*,*) 'INFO: OASIS STRICT mode detected/enforced'
    write(*,*) 'INFO: Loading layout DB file (GDSII/OASIS): ', trim(fname), ' format=', ldb_format_c(fh_out)
    allocate(load_design%layers(MAX_LAYERS))
    allocate(load_design%layerNames(MAX_LAYERS))
    load_design%layerNames = ''
    ! Each layer is registered under four aliases (L1_D2, L1_2, 1:2, 1_2).
    call hash_create(load_design%ht, 4*MAX_LAYERS)
    call load_design%DESIGN_EXTENT%reset_to_infinity()
    do i=1,MAX_LAYERS
       load_design%layers(i)%n_used=0; load_design%layers(i)%n_alloc=0
    end do
    ctx%design => load_design
    ctx%max_layers = MAX_LAYERS
  end subroutine load_ldb_common

  subroutine trim_design_layers(d)
    type(Design), intent(inout) :: d
    integer :: i, n
    type(Box), allocatable :: tmp(:)
    if(.not. allocated(d%layers)) return
    do i=1,size(d%layers)
       associate(L => d%layers(i))
         n = int(L%n_used)
         if(allocated(L%layer_boxes)) then
            if(n <= 0) then
               deallocate(L%layer_boxes); L%n_alloc = 0
            else if(int(L%n_alloc) /= n) then
               allocate(tmp(n))
               tmp = L%layer_boxes(1:n)
               call move_alloc(tmp, L%layer_boxes)
               L%n_alloc = n
            end if
         end if
       end associate
    end do
  end subroutine trim_design_layers

  ! One line per layer that received geometry.  (The hash table holds four
  ! aliases per layer, so its item count is not the number of layers.)
  subroutine report_layers(d, verb)
    type(Design), intent(in) :: d
    character(len=*), intent(in) :: verb
    integer :: i, n
    n = count(len_trim(d%layerNames) > 0)
    write(*,'(5A,I0,A)') 'INFO: ', verb, ' ', trim(d%fileName), ': ', n, ' layers'
    do i = 1, size(d%layerNames)
       if(len_trim(d%layerNames(i)) == 0) cycle
       write(*,'(A,I0,3A,I0,A)') '  Layer ', i, ': ', trim(d%layerNames(i)), ' (', d%layers(i)%n_used, ' boxes)'
    end do
  end subroutine report_layers

  subroutine LoadLayoutDBFile(load_design, MAX_LAYERS)
    type(Design), intent(inout), target :: load_design
    integer, intent(in) :: MAX_LAYERS
    type(c_ptr) :: fh
    type(ldb_load_ctx), target :: ctx
    type(c_funptr) :: cb
    integer(c_int) :: rc
    call load_ldb_common(load_design, MAX_LAYERS, fh, ctx)
    if(.not. c_associated(fh)) return
    cb = c_funloc(box_callback)
    rc = ldb_iterate_boxes_c(fh, cb, c_loc(ctx))
    call ldb_close_c(fh)
    call trim_design_layers(load_design)
    call report_layers(load_design, 'Loaded')
  end subroutine LoadLayoutDBFile

  subroutine LoadLayoutDBFileHier(load_design, MAX_LAYERS)
    type(Design), intent(inout), target :: load_design
    integer, intent(in) :: MAX_LAYERS
    type(c_ptr) :: fh
    type(ldb_load_ctx), target :: ctx
    type(c_funptr) :: cb
    integer(c_int) :: rc
    character(kind=c_char) :: topc(1)
    call load_ldb_common(load_design, MAX_LAYERS, fh, ctx)
    if(.not. c_associated(fh)) return
    cb = c_funloc(box_callback)
    topc(1) = c_null_char
    rc = ldb_iterate_boxes_hier_c(fh, topc, -1, cb, c_loc(ctx))
    call ldb_close_c(fh)
    call trim_design_layers(load_design)
    call report_layers(load_design, 'Hierarchically loaded')
  end subroutine LoadLayoutDBFileHier

end module LayoutDBLoaderModule
