# File   : gds2bin.rb
# Author : Sandeep Koranne
# Purpose: There is an internal KLayout binary format for (x1,y1,x2,y2)
# time klayout -b -r gds2bin.rb -rd input_file=square_polygons.gds -rd output_file=square
# INFO: Reading LAYOUT: MW_FLAT_67_20.gds
# INFO: Reading cell: MW4x4
# INFO: Writing into BIN: MW_FLAT_L67_D20.bin

# real    81m28.093s
# user    78m38.478s
# sys     0m23.230s

include RBA
# 1. Pull values from KLayout global variables (-rd) OR standard Ruby ARGV
input_file  = defined?($input_file)  ? $input_file  : ARGV[0]
output_file = defined?($output_file) ? $output_file : ARGV[1]

# 2. Check if both variables were successfully assigned
if input_file.nil? || output_file.nil?
  puts "ERROR: Missing arguments!"
  puts "Usage via KLayout batch mode:"
  puts "   klayout -b -r gds2bin.rb -rd input_file=design.oas -rd output_file=square.bin"
  puts "Usage via standard ruby:"
  puts "   ruby gds2bin.rb design.oas square.bin"
  exit(1)
end

# Initialize a new layout and read the GDS
layout = RBA::Layout::new
puts "INFO: Reading LAYOUT: #{input_file}"
layout.read(input_file)
# Since this is writing a single layer into a single file
# we dont need to count the number of boxes as we can
# use the size of the file / by 32-bytes and get number of boxes
top = layout.top_cell
puts "INFO: Reading cell: #{top.name}"
layout.layer_indices.each do |li|
  layerInfo = layout.get_info( li )
  layer_number = layerInfo.layer
  layer_datatype = layerInfo.datatype
  binFileName = "#{output_file}_L#{layer_number}_D#{layer_datatype}.bin"
  puts "INFO: Writing into BIN: #{binFileName}"
  f = File.open(binFileName, "wb" )
  shapeIterator = top.begin_shapes_rec(li)
  next if shapeIterator.at_end?
  region = RBA::Region::new(shapeIterator)
  
  # 3. If the region has shapes, decompose them and stream them out
  # if !region.is_empty?
  #   puts "INFO: Converting & Writing into BIN: #{binFileName}"
  #   f = File.open(binFileName, "wb")
  #       # KLayout internal engine: Fractures the region into a flat array of pure boxes
  #   # Polygon::TD_simple forces the engine to resolve shapes as rectangles
  #   box_shapes = region.decompose_trapezoids(Polygon::TD_simple)
  #   # Iterate through the generated Box objects inside the Shapes container
  #   box_shapes.each do |shape|
  #     if shape.is_box?
  #       b = shape.box
  #       f.write([b.left, b.bottom, b.right, b.top].pack("q<q<q<q<"))
  #     end
  #   end
  #   f.close
  # end
  
  while !shapeIterator.at_end?
    shape = shapeIterator.shape
    if shape.is_box?
      b = shape.box
      f.write( [ b.left, b.bottom, b.right, b.top ].pack("l<l<l<l<" ) )
    # The pack l is for 32-bit little-endian, for 64-bit use q
    elsif shape.is_polygon? || shape.is_path?
      single_shape_region = RBA::Region::new
      
      # Safely extract the polygon representation whether it's a native Polygon or a Path
      base_poly = shape.is_path? ? shape.path.polygon : shape.polygon
      geom_polygon = base_poly.transformed(shapeIterator.trans)
      
      single_shape_region.insert(geom_polygon)
      
      # Decompose into trapezoids
      fragments = single_shape_region.decompose_trapezoids(RBA::Polygon::TD_simple)
      
      fragments.each do |poly|
        # Extract the exact geometric bounding limits of each slice
        b = poly.bbox
        f.write([b.left, b.bottom, b.right, b.top].pack("l<l<l<l<"))
      end
      
    else
      # Safely skip Texts and other non-area geometries (like Edges)
      # puts "INFO: Ignoring non-geometric shape in LAYOUT"
    end
    # else
    #   #puts "INFO: Ignoring NON_BOXES in LAYOUT: #{input_file}"
    #   single_shape_region = RBA::Region::new
    #   geom_polygon = shape.polygon.transformed(shapeIterator.trans)
    #   single_shape_region.insert(geom_polygon)
    #   fragments = single_shape_region.decompose_trapezoids(Polygon::TD_simple)
    #   fragments.each do |poly|
    #     # Extract the exact geometric bounding limits of each slice
    #     b = poly.bbox
    #     f.write([b.left, b.bottom, b.right, b.top].pack("l<l<l<l<"))
    #   end
    # end
    shapeIterator.next
  end
  f.close
end

