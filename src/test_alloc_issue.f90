program test_alloc_issue
  use DesignModule
  use commonmodule, only: K_COORDINATE_KIND
  implicit none
  
  type(Layer) :: layer1, layer2
  integer(kind=K_COORDINATE_KIND) :: ivar(4)
  
  ! Initialize layer1 with some boxes
  layer1%n_used = 0
  layer1%n_alloc = 0
  layer1%layerState = 0
  
  ! Allocate something to layer1 to simulate a non-empty layer
  allocate(layer1%layer_boxes(10))
  layer1%n_used = 5
  layer1%n_alloc = 10
  
  write(*,*) 'Before Clear - layer1%layer_boxes allocated = ', allocated(layer1%layer_boxes)
  
  ! Clear layer1
  call ClearLayer(layer1)
  
  write(*,*) 'After Clear - layer1%layer_boxes allocated = ', allocated(layer1%layer_boxes)
  
  ! Initialize layer2 (not explicitly initialized - will have undefined state)
  ! This simulates what happens with memory layers in the control parser
  
  write(*,*) 'Before init - layer2%layer_boxes allocated = ', allocated(layer2%layer_boxes)
  
  ! Clear layer2 first (simulating what exec init does)
  call ClearLayer(layer2)
  
  write(*,*) 'After init Clear - layer2%layer_boxes allocated = ', allocated(layer2%layer_boxes)
  
  ! Now try to copy layer1 to layer2 (simulating COPY operation)
  write(*,*) 'Before Copy - layer2%layer_boxes allocated = ', allocated(layer2%layer_boxes)
  call CopyLayer(layer2, layer1)
  write(*,*) 'After Copy - layer2%layer_boxes allocated = ', allocated(layer2%layer_boxes), ' size = ', size(layer2%layer_boxes)
  
  ! Now try GROW operation (simulating what CalculateGROWLayer does)
  ivar = 0
  write(*,*) 'Before Grow - layer2%layer_boxes allocated = ', allocated(layer2%layer_boxes)
  call CalculateGROWLayer(layer2, layer1, ivar)
  write(*,*) 'After Grow - layer1%layer_boxes allocated = ', allocated(layer1%layer_boxes), ' size = ', size(layer1%layer_boxes)
  
  ! Cleanup - use ClearLayer instead since DeleteLayer might not be accessible
  call ClearLayer(layer1)
  call ClearLayer(layer2)
  
end program
