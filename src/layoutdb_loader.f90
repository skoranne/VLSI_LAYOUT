! Load GDSII/OASIS into MAGPARSER Design structures via src/db C library
module LayoutDBLoaderModule
  use iso_c_binding
  use CommonModule
  use hash_mod
  use DesignModule
  use GeometryModule
  use layoutdb_f_api
  implicit none
  private
  public :: LoadLayoutDBFile

  type :: ldb_load_ctx
     type(Design), pointer :: design => null()
     integer :: max_layers = 0
  end type ldb_load_ctx

contains

  subroutine box_callback(ud, cell, layer, datatype, element_kind, x1,y1,x2,y2) bind(C)
    type(c_ptr), value :: ud
    type(c_ptr), value :: cell
    integer(c_int), value :: layer, datatype, element_kind
    integer(c_int64_t), value :: x1,y1,x2,y2
    type(ldb_load_ctx), pointer :: ctx
    type(Layer), pointer :: L
    integer :: layer_idx
    logical :: ins
    character(len=64) :: buf
    call c_f_pointer(ud, ctx)
    if(.not. associated(ctx)) return
    if(.not. associated(ctx%design)) return
    write(buf,'("L",I0,"_",I0)') layer, datatype
    call hash_get(ctx%design%ht, trim(buf), layer_idx, ins)
    if(.not. ins) then
       layer_idx = hash_nitems(ctx%design%ht) + 1
       if(layer_idx > ctx%max_layers) return
       call hash_put(ctx%design%ht, trim(buf), layer_idx, ins)
       if(layer_idx <= size(ctx%design%layerNames)) ctx%design%layerNames(layer_idx) = trim(buf)
       ctx%design%layers(layer_idx)%n_used = 0
       ctx%design%layers(layer_idx)%n_alloc = 0
    end if
    L => ctx%design%layers(layer_idx)
    call push_box_local(L, int(x1,kind=K_COORDINATE_KIND), int(y1,kind=K_COORDINATE_KIND), &
                        int(x2,kind=K_COORDINATE_KIND), int(y2,kind=K_COORDINATE_KIND))
    if(int(x1,kind=K_COORDINATE_KIND) < ctx%design%DESIGN_EXTENT%x1) ctx%design%DESIGN_EXTENT%x1 = int(x1,kind=K_COORDINATE_KIND)
    if(int(y1,kind=K_COORDINATE_KIND) < ctx%design%DESIGN_EXTENT%y1) ctx%design%DESIGN_EXTENT%y1 = int(y1,kind=K_COORDINATE_KIND)
    if(int(x2,kind=K_COORDINATE_KIND) > ctx%design%DESIGN_EXTENT%x2) ctx%design%DESIGN_EXTENT%x2 = int(x2,kind=K_COORDINATE_KIND)
    if(int(y2,kind=K_COORDINATE_KIND) > ctx%design%DESIGN_EXTENT%y2) ctx%design%DESIGN_EXTENT%y2 = int(y2,kind=K_COORDINATE_KIND)
  end subroutine box_callback

  subroutine push_box_local(L, x1,y1,x2,y2)
    type(Layer), intent(inout) :: L
    integer(kind=K_COORDINATE_KIND), intent(in) :: x1,y1,x2,y2
    type(Box), allocatable :: tmp(:)
    integer :: n, na
    n = int(L%n_used); na = int(L%n_alloc)
    if(.not. allocated(L%layer_boxes)) then
       allocate(L%layer_boxes(4)); L%n_alloc=4; na=4; L%n_used=0; n=0
    end if
    if(n >= na) then
       allocate(tmp(max(4,na*2)))
       if(na>0 .and. allocated(L%layer_boxes)) tmp(1:n)=L%layer_boxes(1:n)
       call move_alloc(tmp, L%layer_boxes)
       L%n_alloc = max(4,na*2)
    end if
    L%n_used = L%n_used + 1
    L%layer_boxes(L%n_used)%x1 = min(x1,x2)
    L%layer_boxes(L%n_used)%y1 = min(y1,y2)
    L%layer_boxes(L%n_used)%x2 = max(x1,x2)
    L%layer_boxes(L%n_used)%y2 = max(y1,y2)
  end subroutine push_box_local

  subroutine LoadLayoutDBFile(load_design, MAX_LAYERS)
    type(Design), intent(inout), target :: load_design
    integer, intent(in) :: MAX_LAYERS
    type(c_ptr) :: fh
    character(kind=c_char), allocatable :: cpath(:)
    character(kind=c_char) :: errbuf(512)
    integer(c_size_t) :: esz
    integer :: i, n
    type(ldb_load_ctx), target :: ctx
    type(c_funptr) :: cb
    integer(c_int) :: rc
    character(len=:), allocatable :: fname

    fname = load_design%fileName
    n = len_trim(fname)
    allocate(cpath(n+1))
    do i=1,n
       cpath(i) = fname(i:i)
    end do
    cpath(n+1) = c_null_char
    errbuf = c_null_char
    esz = 512
    fh = ldb_open_c(cpath, errbuf, esz)
    if(.not. c_associated(fh)) then
       write(*,*) 'ERROR: ldb_open failed for ', trim(fname)
       return
    end if
    write(*,*) 'INFO: Loading layout DB file (GDSII/OASIS): ', trim(fname), ' format=', ldb_format_c(fh)

    allocate(load_design%layers(MAX_LAYERS))
    allocate(load_design%layerNames(MAX_LAYERS))
    call hash_create(load_design%ht, MAX_LAYERS)
    call load_design%DESIGN_EXTENT%reset_to_infinity()
    do i=1,MAX_LAYERS
       load_design%layers(i)%n_used=0; load_design%layers(i)%n_alloc=0
    end do

    ctx%design => load_design
    ctx%max_layers = MAX_LAYERS
    cb = c_funloc(box_callback)
    rc = ldb_iterate_boxes_c(fh, cb, c_loc(ctx))
    call ldb_close_c(fh)

    write(*,*) 'INFO: Loaded ', hash_nitems(load_design%ht), ' layers from ', trim(fname)
  end subroutine LoadLayoutDBFile

end module LayoutDBLoaderModule
