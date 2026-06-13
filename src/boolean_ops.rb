# File   : boolean_ops.rb
# Purpose: Perform AND, OR, NOT, XOR on two GDS/OAS layouts using RBA::Region
# I have two layout files in GDS or OASIS or BIN format, how can I write a KLayout
# ruby script to perform some Boolean operation like AND, OR, NOT, XOR on them and
# produce an output GDS, OASIS or BIN file.
include RBA

# 1. Parse Input Arguments (-rd)
file_a   = defined?($file_a)   ? $file_a   : ARGV[0]
layer_a  = defined?($layer_a)  ? $layer_a  : ARGV[1] # e.g., "1/0"
file_b   = defined?($file_b)   ? $file_b   : ARGV[2]
layer_b  = defined?($layer_b)  ? $layer_b  : ARGV[3] # e.g., "2/0"
op       = defined?($op)       ? $op.upcase : ARGV[4] # AND, OR, NOT, XOR
out_file = defined?($out_file) ? $out_file : ARGV[5]

if [file_a, layer_a, file_b, layer_b, op, out_file].any?(&:nil?)
  puts "ERROR: Missing arguments."
  puts "Usage: klayout -b -r boolean_ops.rb -rd file_a=A.gds -rd layer_a=1/0 -rd file_b=B.gds -rd layer_b=2/0 -rd op=AND -rd out_file=out.oas"
  exit(1)
end

puts "INFO: Operation -> A(#{layer_a}) #{op} B(#{layer_b}) => #{out_file}"

def get_region_from_file_bin(filepath, layer_str)
  region = RBA::Region::new
  dbu = 0.001 # Default DBU assumption for custom BIN files
  
  if filepath.end_with?(".bin")
    # Custom BIN reader
    File.open(filepath, "rb") do |f|
      while chunk = f.read(16) # 4 integers * 4 bytes each
        left, bottom, right, top = chunk.unpack("l<l<l<l<")
        box = RBA::Box::new(left, bottom, right, top)
        region.insert(box)
      end
    end
    region.merge
  else
    # Standard GDS/OAS reader
    layout = RBA::Layout::new
    layout.read(filepath)
    dbu = layout.dbu
    top = layout.top_cell
    
    l, d = layer_str.split('/').map(&:to_i)
    layer_index = layout.find_layer(l, d)
    
    if layer_index
      iterator = top.begin_shapes_rec(layer_index)
      region = RBA::Region::new(iterator)
      region.merge
    end
  end
  
  return region, dbu
end

# Helper function to extract a layer string "L/D" into a KLayout Region
def get_region_from_file(filepath, layer_str)
  layout = RBA::Layout::new
  layout.read(filepath)
  top = layout.top_cell
  
  l, d = layer_str.split('/').map(&:to_i)
  layer_index = layout.find_layer(l, d)
  
  if layer_index.nil?
    puts "WARNING: Layer #{layer_str} not found in #{filepath}. Returning empty region."
    return RBA::Region::new, layout.dbu
  end
  
  # Extract shapes recursively (virtual flattening)
  iterator = top.begin_shapes_rec(layer_index)
  region = RBA::Region::new(iterator)
  region.merge # Clean up internal overlaps
  
  return region, layout.dbu
end

# 2. Load the Layouts and Extract Regions
puts "INFO: Loading File A..."
region_a, dbu_a = get_region_from_file(file_a, layer_a)

puts "INFO: Loading File B..."
region_b, dbu_b = get_region_from_file(file_b, layer_b)

if dbu_a != dbu_b
  puts "WARNING: Database Units (DBU) differ! A=#{dbu_a}, B=#{dbu_b}."
  puts "Boolean operations on differing DBUs may result in scaling errors."
end

# 3. Perform the Boolean Operation
puts "INFO: Executing #{op} operation (This may take a moment for large layouts)..."
result_region = case op
                when "AND" then region_a & region_b
                when "OR"  then region_a + region_b
                when "NOT" then region_a - region_b # A NOT B (A outside B)
                when "XOR" then region_a ^ region_b
                else
                  puts "ERROR: Unknown operation '#{op}'. Use AND, OR, NOT, or XOR."
                  exit(1)
                end

# 4. Write the Output
puts "INFO: Writing resulting region to #{out_file}..."
out_layout = RBA::Layout::new
out_layout.dbu = dbu_a # Inherit DBU from File A

out_top = out_layout.create_cell("TOP_BOOLEAN")
out_layer_index = out_layout.insert_layer(RBA::LayerInfo::new(100, 0)) # Output to layer 100/0

# Insert the boolean result into the new layout
out_top.shapes(out_layer_index).insert(result_region)

out_layout.write(out_file)
puts "INFO: Done! Output saved."
