import pya
ly = pya.Layout(); ly.read(f)
x1, y1, x2, y2 = [int(v) for v in win.split(',')]
li = ly.find_layer(66, 20)
it = ly.top_cell().begin_shapes_rec_overlapping(li, pya.Box(x1, y1, x2, y2))
while not it.at_end():
    s = it.shape(); t = it.trans()
    print("KL", " ".join("%d,%d" % (p.x, p.y) for p in s.polygon.transformed(t).each_point_hull()), "| trans", t)
    it.next()
