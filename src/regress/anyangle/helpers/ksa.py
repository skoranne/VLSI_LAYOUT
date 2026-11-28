import pya
ly = pya.Layout(); ly.read(f); top = ly.top_cell()
r = pya.Region(top.begin_shapes_rec(ly.find_layer(67, 20)))
m = r.merged(False, 2)
nv = sum(p.num_points_hull() + sum(p.num_points_hole(k) for k in range(p.holes())) for p in m.each())
print("KLSELF polygons=%d vertices=%d area=%d perimeter=%d" % (m.count(), nv, m.area(), m.perimeter()))
