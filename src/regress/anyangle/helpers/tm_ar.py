import pya
ly = pya.Layout(); ly.read(fa); r = pya.Region(); li = ly.find_layer(66, 20)
for t in ly.top_cells(): r.insert(pya.Region(t.begin_shapes_rec(li)))
m = r.merged()
s2 = 0; s1 = 0; bad = 0
for p in m.each():
    a2 = p.area2(); a = p.area(); s2 += a2; s1 += a
    # independent shoelace with Python ints
    def sh(pts):
        pts = list(pts); n = len(pts)
        return sum(pts[i].x * pts[(i+1)%n].y - pts[(i+1)%n].x * pts[i].y for i in range(n))
    e = abs(sh(p.each_point_hull())) - sum(abs(sh(p.each_point_hole(h))) for h in range(p.holes()))
    if e != a2:
        bad += 1
        if bad <= 5: print("mismatch", p.bbox(), "area2", a2, "shoelace", e, "holes", p.holes(), "pts", p.num_points())
print("region.area*2", 2*m.area(), "sum area2", s2, "sum area*2", 2*s1, "bad", bad)
