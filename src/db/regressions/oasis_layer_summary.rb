# Per-layer summary of an OASIS/GDS file with the system KLayout (0.27.x, Ruby via RBA::):
#   klayout -b -r oasis_layer_summary.rb -rd f=file.oas
# Prints the top cell, its bbox, and per layer: L/D, layer name (from LAYERNAME), shape count,
# summed box area. Used to check bin2oas strict-mode output against the original.
ly = RBA::Layout.new
t0 = Time.now
ly.read($f)
STDOUT.puts "READ_S #{'%.2f' % (Time.now - t0)}"
top = ly.top_cell
STDOUT.puts "TOP #{top.name} BBOX #{top.bbox.left} #{top.bbox.bottom} #{top.bbox.right} #{top.bbox.top} DBU #{ly.dbu}"
ly.layer_indexes.sort_by { |li| [ly.get_info(li).layer, ly.get_info(li).datatype] }.each do |li|
  info = ly.get_info(li)
  n = 0
  area = 0
  top.shapes(li).each { |s| n += 1; area += s.bbox.area }
  next if n == 0
  STDOUT.puts "LAYER #{info.layer}/#{info.datatype} NAME '#{info.name}' N #{n} AREA #{area}"
end
