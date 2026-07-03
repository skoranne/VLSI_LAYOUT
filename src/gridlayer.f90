! File    : gridlayer.f90
! Author  : Sandeep Koranne (C) 2026
! Purpose : Better parallelism can be obtained by rows x cols grid

submodule (DesignModule) GridModule
  use iso_fortran_env, only: int64, real64
  use CommonModule
  use GeometryModule
  use MortonSortOMT
  use RTreeBuilderGPU
  use RTreeBuilder
  use GPUMergeModule
  use PNumMergeModule
  use KLDataModule
  use BoostPolygonAPIModule
  use SerializationModule
  implicit none
contains
  module subroutine populate_grid_from_layer(this, base_layer, n_rows, n_cols, layout_bounds)
    class(GridLayer), intent(inout) :: this
    type(Layer),      intent(in)    :: base_layer
    integer(kind=int64), intent(in) :: n_rows, n_cols
    type(Box),        intent(in)    :: layout_bounds
    type(Layer)                     :: grid
    integer                         :: rows, cols
    type(Box)                       :: cell_box

    ! 1. Initialize Grid dimensions
    this%rows = n_rows
    this%cols = n_cols
    call CreateGrid( base_layer, grid, rows, cols, 0) !> create over
    ! 2. Allocate the internal 2D array of layers
    if (allocated(this%sub_layers)) deallocate(this%sub_layers)
    allocate(this%sub_layers(n_rows, n_cols))

    ! 3. Populate each sub_layer using FilterLayer
    !$omp parallel do collapse(2) default(none) &
    !$omp private(cols, rows, cell_box) &
    !$omp shared(n_cols, n_rows, grid, base_layer, this)
    do cols = 1, n_cols
       do rows = 1, n_rows
          ! Calculate spatial bounding box for this specific grid cell
          cell_box = grid%layer_boxes( (rows-1)*n_cols+cols )

          ! Extract the overlapping boxes from base_layer into the sub_layer
          call FilterLayer(base_layer, cell_box, this%sub_layers(rows, cols))
       end do
    end do
    !$omp end parallel do   

  end subroutine populate_grid_from_layer

  module subroutine PerformOperation( gA, gB, gO, opcode )
    character(len=*), parameter :: functionName = "PerformOperation"
    class(Layer), intent(inout) :: gA, gB, gO
    integer                        :: opcode

    select type( A => gA )
    type is (Layer)
       select type( B => gB )
       type is (Layer)
          select case(opcode)
          case(K_BOOST_CONTROL_OR)
             call CalculateOR( A, B, gO )
          case default
             error stop "ERROR: Unsupported OPCODE"
          end select
       end select
    end select
    
    
    error stop "ERROR: Not implemented yet"
  end subroutine PerformOperation

  module subroutine populate_gridlayer_from_unit(this, base_layer)
    character(len=*), parameter :: functionName = "PerformOperation"    
    type(Layer), intent(inout) :: this
    type(GridLayer), intent(inout) :: base_layer
    call AnalyzeUnit(this%iunit)
  end subroutine populate_gridlayer_from_unit


  
end submodule GridModule
