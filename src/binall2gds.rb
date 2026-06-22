# File   : binall2gds.rb
# Purpose: Reverses gds2bin.rb by reading 32-bit little-endian binary boxes
#          and generating a GDS layout using KLayout.
# Usage  : klayout -b -r bin2gds.rb -rd input_file=MW_FLAT_L67_D20.bin -rd output_file=restored.gds -rd top_cell_name="FOO"

include RBA

# Find all matching files in the current folder
# This matches files starting with 'prefix', having '_L' and '_D', and ending in '.bin'
binary_files = Dir.glob("prefix*_L*_D*.bin")

# Setup the main GDS target
target_layout = RBA::Layout.new
target_layout.dbu = 0.001 
top_cell_index = target_layout.add_cell("TOP_ROOT")

binary_files.each do |file_path|
  puts "Processing: #{file_path}"
  
  # 1. Read the binary file into its own layout
  source_layout = RBA::Layout.new
  source_layout.read(file_path)
  
  # 2. Prevent naming clashes for shared cells
  load_options = RBA::LoadLayoutOptions.new
  load_options.scan_for_duplicates = true
  load_options.duplicate_cell_naming = RBA::LoadLayoutOptions::RenameCell
  
  # 3. Use the file name as the new unique cell name
  # Example: "prefixA_L1000_D10.bin" becomes cell "prefixA_L1000_D10"
  clean_cell_name = File.basename(file_path, ".*")
  
  source_cell = source_layout.top_cell
  if source_cell
    target_cell = target_layout.create_cell(clean_cell_name)
    
    # Map and copy the shapes
    cm = RBA::CellMapping.new
    cm.for_single_cell_full(target_layout, target_cell.cell_index, source_layout, source_cell.cell_index)
    target_layout.copy_tree_shapes(source_layout, cm)
    
    # 4. Put the cell at the origin (0, 0) under TOP_ROOT
    trans = RBA::Trans.new(RBA::Point.new(0, 0)) 
    target_layout.cell(top_cell_index).insert(RBA::CellInstArray.new(target_cell.cell_index, trans))
  end
end

# Save the final GDS
target_layout.write(output_gds)
puts "Done! Merged #{binary_files.length} files into #{output_gds}."
