import pya
ly = pya.Layout(); ly.read(f)
for (l, d) in ((66, 20), (65, 20)):
    li = ly.find_layer(l, d)
    for p in pya.Region(ly.top_cell().begin_shapes_rec(li)).each():
        print("%d/%d" % (l, d), " ".join("%d,%d" % (q.x, q.y) for q in p.each_point_hull()))
