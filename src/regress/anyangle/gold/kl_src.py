import pya
ly = pya.Layout(); ly.read(f)
x1, y1, x2, y2 = [int(v) for v in win.split(',')]
W = pya.Box(x1, y1, x2, y2)
for (l, d) in ((66, 20),):
    li = ly.find_layer(l, d)
    it = ly.top_cell().begin_shapes_rec_touching(li, W)
    while not it.at_end():
        s = it.shape(); t = it.trans()
        print(ly.cell(it.cell_index()).name, "trans=", t, "type=", "path" if s.is_path() else "box" if s.is_box() else "poly" if s.is_polygon() else "simple_poly" if s.is_simple_polygon() else "other", s)
        if s.is_path(): print("   path outline:", s.path.polygon().transformed(t))
        it.next()
