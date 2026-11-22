# Structural equivalence check for MPW_MERGE output (KLayout batch script).
#
#   klayout -b -r mpw_merge_check.rb -rd a=original.oas -rd b=merged.oas \
#           -rd ta=<top in original> -rd tb=<S<nnn>_top in merged>
#
# Walks both hierarchies from the given tops in parallel.  For every cell
# pair it compares, layer by layer, the shape count, the geometry (local
# XOR, no flattening) and the texts, and it compares the instances
# (transformation, array parameters, properties, child bounding box).  Cells
# may be renamed and identical cells merged; one original cell must never
# map to two different merged cells.  Exit status 0 = equivalent.

a = RBA::Layout.new; a.read($a)
b = RBA::Layout.new; b.read($b)
ta = a.cell_by_name($ta) or raise "no cell #{$ta} in #{$a}"
tb = b.cell_by_name($tb) or raise "no cell #{$tb} in #{$b}"

lmap = {}
a.layer_indexes.each do |li|
  inf = a.get_info(li)
  lmap[li] = b.find_layer(inf.layer, inf.datatype)
end
b_layers_known = lmap.values.compact

$bad = 0
def bad(msg)
  $bad += 1
  puts "MISMATCH: #{msg}" if $bad <= 30
end

def inst_key(ly, inst)
  props = inst.has_prop_id? ? ly.properties(inst.prop_id).sort_by(&:to_s).to_s : ""
  arr = inst.is_regular_array? ? "#{inst.a} #{inst.b} #{inst.na} #{inst.nb}" : ""
  [inst.cplx_trans.to_s, arr, props, ly.cell(inst.cell_index).bbox.to_s].join("|")
end

map = { ta => tb }
queue = [[ta, tb]]
cells = shapes = insts = 0
while (pr = queue.shift)
  ia, ib = pr
  ca, cb = a.cell(ia), b.cell(ib)
  cells += 1
  a.layer_indexes.each do |li|
    lb = lmap[li]
    sa = ca.shapes(li)
    sb = lb ? cb.shapes(lb) : nil
    na = sa.size; nb = sb ? sb.size : 0
    next if na == 0 && nb == 0
    shapes += na
    inf = a.get_info(li)
    if na != nb
      bad("#{ca.name} / #{cb.name} layer #{inf}: #{na} vs #{nb} shapes")
      next
    end
    x = RBA::Region.new(sa) ^ RBA::Region.new(sb)
    bad("#{ca.name} / #{cb.name} layer #{inf}: geometry differs (#{x.size} XOR polygons)") unless x.is_empty?
    txa = RBA::Texts.new(sa).each.map(&:to_s).sort
    txb = RBA::Texts.new(sb).each.map(&:to_s).sort
    bad("#{ca.name} / #{cb.name} layer #{inf}: texts differ") if txa != txb
  end
  b.layer_indexes.each do |lb|
    next if b_layers_known.include?(lb)
    bad("#{cb.name}: shapes on layer #{b.get_info(lb)} not in the original") if cb.shapes(lb).size > 0
  end
  la = ca.each_inst.map { |i| [inst_key(a, i), i.cell_index] }.sort_by(&:first)
  lb = cb.each_inst.map { |i| [inst_key(b, i), i.cell_index] }.sort_by(&:first)
  insts += la.size
  if la.size != lb.size
    bad("#{ca.name} / #{cb.name}: #{la.size} vs #{lb.size} instances")
    next
  end
  la.zip(lb).each do |(ka, ca_i), (kb, cb_i)|
    if ka != kb
      bad("#{ca.name} / #{cb.name}: instance #{ka} vs #{kb}")
      next
    end
    if map.key?(ca_i)
      bad("#{a.cell(ca_i).name} maps to #{b.cell(map[ca_i]).name} and #{b.cell(cb_i).name}") if map[ca_i] != cb_i
    else
      map[ca_i] = cb_i
      queue << [ca_i, cb_i]
    end
  end
end
merged = map.values.uniq.size
puts "#{$a} #{$ta} vs #{$b} #{$tb}: #{cells} cell pairs (#{map.size} original -> #{merged} merged), #{shapes} shapes, #{insts} instances, #{$bad} mismatches"
exit($bad == 0 ? 0 : 1)
