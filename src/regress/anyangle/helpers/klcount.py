import pya
for f in (a, b):
    ly = pya.Layout(); ly.read(f); top = ly.top_cell()
    for l, d in ((64,16),(68,16),(122,16)):
        li = ly.find_layer(l, d)
        n = 0; it = top.begin_shapes_rec(li)
        nt = 0
        while not it.at_end():
            if it.shape().is_text(): nt += 1
            else: n += 1
            it.next()
        r = pya.Region(top.begin_shapes_rec(li))
        print(f.split('/')[-1], "%d/%d shapes=%d texts=%d region_count=%d" % (l, d, n, nt, r.count()))
