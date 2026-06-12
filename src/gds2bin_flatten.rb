# File   : gds2bin_flatten.rb
# Author : Sandeep Koranne / Updated for Virtual Flattening
# Purpose: Extract flattened (x1,y1,x2,y2) bounding boxes to binary format

include RBA

# 1. Pull values from KLayout global variables (-rd) OR standard Ruby ARGV
input_file  = defined?($input_file)  ? $input_file  : ARGV[0]
output_file = defined?($output_file) ? $output_file : ARGV[1]

# 2. Check if both variables were successfully assigned
if input_file.nil? || output_file.nil?
  puts "ERROR: Missing arguments!"
  puts "Usage via KLayout batch mode:"
  puts "   klayout -b -r gds2bin.rb -rd input_file=design.oas -rd output_file=square.bin"
  exit(1)
end

# Initialize a new layout and read the GDS
layout = RBA::Layout::new
puts "INFO: Reading LAYOUT: #{input_file}"
layout.read(input_file)

top = layout.top_cell
bbox_um = top.dbbox
puts "INFO: Database Unit (DBU): #{layout.dbu} µm"
puts "--- Top Cell Extent ---"
puts "Bottom-Left  : (#{bbox_um.left}, #{bbox_um.bottom}) µm"
puts "Top-Right    : (#{bbox_um.right}, #{bbox_um.top}) µm"
puts "Total Width  : #{bbox_um.width} µm"
puts "Total Height : #{bbox_um.height} µm"
puts "-----------------------"

puts "INFO: Reading cell: #{top.name}"
layout.layer_indices.each do |li|
  layerInfo = layout.get_info( li )
  layer_number = layerInfo.layer
  layer_datatype = layerInfo.datatype
  
  shapeIterator = top.begin_shapes_rec(li)
  next if shapeIterator.at_end?
  
  # VIRTUAL FLATTENING: Feed the recursive iterator into the Region.
  # This converts all arrays, paths, and hierarchical instances into flat Polygons.
  layer_region = RBA::Region::new(shapeIterator)
  
  # Merging removes overlaps so inner edges aren't counted/written
  layer_region.merge 
  
  area_um2 = layer_region.area * (layout.dbu ** 2)
  perimeter_um = layer_region.perimeter * layout.dbu
  puts "INFO: Layer #{layer_number}/#{layer_datatype} -> True Area: #{area_um2.round(4)} µm², True Perimeter: #{perimeter_um.round(4)} µm"
  
  binFileName = "#{output_file}_L#{layer_number}_D#{layer_datatype}.bin"
  puts "INFO: Writing into BIN: #{binFileName}"
  
  f = File.open(binFileName, "wb")
  
  # CRITICAL CHANGE: Iterate over the flattened Region, NOT the shapeIterator!
  layer_region.each do |polygon|
    
    # In a Region, everything has already been converted to an RBA::Polygon.
    # We use 'is_rect?' to check if it's a simple orthogonal box.
    if polygon.is_box?
      b = polygon.bbox
      f.write([b.left, b.bottom, b.right, b.top].pack("l<l<l<l<"))
    else
    # Complex polygon -> Decompose into trapezoids
    single_shape_region = RBA::Region::new(polygon)
    fragments = single_shape_region.decompose_trapezoids(RBA::Polygon::TD_simple)
    
    fragments.each do |poly|
      b = poly.bbox
      f.write([b.left, b.bottom, b.right, b.top].pack("l<l<l<l<"))
    end
    end
  end
  
  f.close
  
  # CRITICAL FOR LARGE DESIGNS: Free the layer's memory before the next loop
  layer_region.clear
end
