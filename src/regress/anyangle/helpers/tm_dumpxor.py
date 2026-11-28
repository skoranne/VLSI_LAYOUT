import pya
ly = pya.Layout(); ly.read(fo); r = pya.Region(); li = ly.find_layer(66, 20)
for t in ly.top_cells(): r.insert(pya.Region(t.begin_shapes_rec(li)))
d = pya.Region()
for line in open(fd):
    f = line.split()
    v = list(map(int, f[1:]))
    if f[0] == "T":
        y1, y2, bl, br, tl, tr = v
        d.insert(pya.Polygon([pya.Point(bl, y1), pya.Point(br, y1), pya.Point(tr, y2), pya.Point(tl, y2)]))
    else:
        d.insert(pya.Polygon([pya.Point(v[i], v[i+1]) for i in range(0, len(v), 2)], True))
# raw per-shape comparison: output shapes vs intended, without merging
x = r ^ d
print("xor", x.area(), "n", x.count())
for i, p in enumerate(x.each()):
    if i < 6: print(p.bbox(), p.area2())
