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
    call push_box_local(ctx%design, layer_idx, int(x1,kind=K_COORDINATE_KIND), int(y1,kind=K_COORDINATE_KIND), &
                        int(x2,kind=K_COORDINATE_KIND), int(y2,kind=K_COORDINATE_KIND))
    if(int(x1,kind=K_COORDINATE_KIND) < ctx%design%DESIGN_EXTENT%x1) ctx%design%DESIGN_EXTENT%x1 = int(x1,kind=K_COORDINATE_KIND)
    if(int(y1,kind=K_COORDINATE_KIND) < ctx%design%DESIGN_EXTENT%y1) ctx%design%DESIGN_EXTENT%y1 = int(y1,kind=K_COORDINATE_KIND)
    if(int(x2,kind=K_COORDINATE_KIND) > ctx%design%DESIGN_EXTENT%x2) ctx%design%DESIGN_EXTENT%x2 = int(x2,kind=K_COORDINATE_KIND)
    if(int(y2,kind=K_COORDINATE_KIND) > ctx%design%DESIGN_EXTENT%y2) ctx%design%DESIGN_EXTENT%y2 = int(y2,kind=K_COORDINATE_KIND)
  end subroutine box_callback

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
    call hash_create(load_design%ht, MAX_LAYERS)
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

  subroutine LoadLayoutDBFile(load_design, MAX_LAYERS)
    type(Design), intent(inout), target :: load_design
    integer, intent(in) :: MAX_LAYERS
    type(c_ptr) :: fh
    type(ldb_load_ctx), target :: ctx
    type(c_funptr) :: cb
    integer(c_int) :: rc
    integer :: i
    call load_ldb_common(load_design, MAX_LAYERS, fh, ctx)
    if(.not. c_associated(fh)) return
    cb = c_funloc(box_callback)
    rc = ldb_iterate_boxes_c(fh, cb, c_loc(ctx))
    call ldb_close_c(fh)
    call trim_design_layers(load_design)
    write(*,*) 'INFO: Loaded ', hash_nitems(load_design%ht), ' layers from ', trim(load_design%fileName)
    do i = 1, hash_nitems(load_design%ht)
       write(*,*) '  Layer ', i, ': ', trim(load_design%layerNames(i))
    end do
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
    write(*,*) 'INFO: Hierarchically loaded ', hash_nitems(load_design%ht), ' layers from ', trim(load_design%fileName)
  end subroutine LoadLayoutDBFileHier

end module LayoutDBLoaderModule
