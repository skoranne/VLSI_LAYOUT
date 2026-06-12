# File   : bin2gds.rb
# Purpose: Reverses gds2bin.rb by reading 32-bit little-endian binary boxes
#          and generating a GDS layout using KLayout.
# Usage  : klayout -b -r bin2gds.rb -rd input_file=MW_FLAT_L67_D20.bin -rd output_file=restored.gds -rd top_cell_name="FOO"

include RBA

# 1. Pull values from KLayout global variables (-rd) OR standard Ruby ARGV
input_file  = defined?($input_file)  ? $input_file  : ARGV[0]
output_file = defined?($output_file) ? $output_file : ARGV[1]

if input_file.nil? || output_file.nil?
  puts "ERROR: Missing arguments!"
  puts "Usage via KLayout batch mode:"
  puts "   klayout -b -r bin2gds.rb -rd input_file=square_L67_D20.bin -rd output_file=restored.gds"
  puts "Usage via standard ruby:"
  puts "   ruby bin2gds.rb square_L67_D20.bin restored.gds"
  exit(1)
end

# 2. Try to parse Layer and Datatype from the filename
# Matches patterns like "_L67_D20.bin"
layer_num = 1
datatype_num = 0

filename_match = input_file.match(/_L(\d+)_D(\d+)\.bin/i)
if filename_match
  layer_num = filename_match[1].to_i
  datatype_num = filename_match[2].to_i
  puts "INFO: Extracted Layer: #{layer_num}, Datatype: #{datatype_num} from filename."
else
  puts "WARNING: Could not parse Layer and Datatype from filename. Defaulting to L#{layer_num}_D#{datatype_num}."
end

# 3. Initialize a new layout and create a top cell
layout = RBA::Layout::new
output_directory = $out_dir || "./default_folder"
top_name = $top_cell_name || "TOP"
top_cell = layout.create_cell(top_name)

# Create the layer to hold the restored shapes
layer_index = layout.insert_layer(RBA::LayerInfo::new(layer_num, datatype_num))

puts "INFO: Reading BIN: #{input_file}"

# 4. Open the binary file and reconstruct boxes
box_count = 0

File.open(input_file, "rb") do |f|
  # Read 16 bytes at a time (4 integers * 4 bytes per 32-bit int = 16 bytes)
  while chunk = f.read(16)
    if chunk.bytesize == 16
      # Unpack the 32-bit little-endian binary data back into integers
      left, bottom, right, top_coord = chunk.unpack("l<l<l<l<")
      
      # Recreate the box (using top_coord so we don't overwrite the top_cell variable)
      box = RBA::Box::new(left, bottom, right, top_coord)
      
      # Insert the box into the layout
      top_cell.shapes(layer_index).insert(box)
      box_count += 1
    else
      puts "WARNING: Ignoring incomplete chunk of #{chunk.bytesize} bytes at end of file."
    end
  end
end

puts "INFO: Reconstructed #{box_count} boxes."
puts "INFO: Writing LAYOUT: #{output_file}"

# 5. Write the layout to the GDS file
layout.write(output_file)

puts "INFO: Done."
