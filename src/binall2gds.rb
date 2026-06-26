# File   : bin2gds.rb
# Purpose: Reverses gds2bin.rb by reading multiple 32-bit little-endian 
#          binary boxes and generating a single combined GDS layout.
# Usage  : klayout -b -r bin2gds.rb -rd input_dir=. -rd output_file=restored.gds -rd top_cell_name="FOO"

include RBA

# 1. Pull values from KLayout global variables (-rd) OR standard Ruby ARGV
# Default to current directory (".") if no input_dir is provided
input_dir   = defined?($input_dir)   ? $input_dir   : (ARGV[0] || ".")
output_file = defined?($output_file) ? $output_file : (ARGV[1] || "restored.gds")

# 2. Find all BIN files matching the pattern in the target directory
glob_pattern = File.join(input_dir, "*_*_L*_D*.bin")
binary_files = Dir.glob(glob_pattern)

if binary_files.empty?
  puts "ERROR: No binary files found matching #{glob_pattern}"
  puts "Usage via KLayout batch mode:"
  puts "   klayout -b -r bin2gds.rb -rd input_dir=/path/to/bins -rd output_file=restored.gds"
  exit(1)
end

# 3. Initialize a SINGLE new layout and create the top cell
layout = RBA::Layout::new
top_name = defined?($top_cell_name) ? $top_cell_name : "TOP"
top_cell = layout.create_cell(top_name)

total_box_count = 0
# A hash to keep track of Layer/Datatype indices so we don't create duplicates
layer_mapping = {}

puts "INFO: Found #{binary_files.size} binary files to process."

# 4. Loop over each discovered file
binary_files.each do |file_path|
  filename = File.basename(file_path)

  # Try to parse Layer and Datatype from the filename
  layer_num = 1
  datatype_num = 0
  filename_match = filename.match(/_L(\d+)_D(\d+)\.bin/i)
  
  if filename_match
    layer_num = filename_match[1].to_i
    datatype_num = filename_match[2].to_i
    puts "INFO: Processing #{filename} -> Layer: #{layer_num}, Datatype: #{datatype_num}"
  else
    puts "WARNING: Could not parse Layer/Datatype from #{filename}. Defaulting to L#{layer_num}_D#{datatype_num}."
  end

  # Check if we already created this layer in the layout. If not, create it.
  layer_key = [layer_num, datatype_num]
  unless layer_mapping.key?(layer_key)
    layer_mapping[layer_key] = layout.insert_layer(RBA::LayerInfo::new(layer_num, datatype_num))
  end
  layer_index = layer_mapping[layer_key]

  # Open the binary file and reconstruct boxes
  box_count = 0
  File.open(file_path, "rb") do |f|
    # Read 16 bytes at a time (4 integers * 4 bytes per 32-bit int = 16 bytes)
    while chunk = f.read(16)
      if chunk.bytesize == 16
        # Unpack the 32-bit little-endian binary data back into integers
        left, bottom, right, top_coord = chunk.unpack("l<l<l<l<")
        
        # Recreate the box
        box = RBA::Box::new(left, bottom, right, top_coord)
        
        # Insert the box into the layout at the correct layer
        top_cell.shapes(layer_index).insert(box)
        box_count += 1
        total_box_count += 1
      else
        puts "  WARNING: Ignoring incomplete chunk of #{chunk.bytesize} bytes at end of #{filename}."
      end
    end
  end
  
  puts "  -> Reconstructed #{box_count} boxes from #{filename}."
end
puts "Done! Merged #{binary_files.length} files into #{output_file}."
puts "INFO: Total reconstructed boxes across all files: #{total_box_count}."
puts "INFO: Writing LAYOUT: #{output_file}"
# 5. Write the layout to the GDS file
layout.write(output_file)
