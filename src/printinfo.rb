layout = RBA::Layout::new
layout.read("a_out_merged.gds")

layer_info = RBA::LayerInfo.new(1, 0) # Example: Layer 67, Datatype 20
layer_index = layout.find_layer(layer_info)

if layer_index
  # This gets the region for EXACTLY layer 1/0
  region = RBA::Region.new(layout.top_cell.begin_shapes_rec(layer_index))
  
  # Area calculation (convert DBU^2 to um^2)
  area_um2 = region.area * (layout.dbu**2)
  puts "Layer 1/0 Area: #{area_um2} um^2"
else
  puts "Layer 1/0 not found!"
end
