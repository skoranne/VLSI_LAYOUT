import pya
d = pya.Region()
for line in open(fd):
    f = line.split(); v = list(map(int, f[1:]))
    if f[0] == "T":
        y1, y2, bl, br, tl, tr = v
        d.insert(pya.Polygon([pya.Point(bl, y1), pya.Point(br, y1), pya.Point(tr, y2), pya.Point(tl, y2)]))
    else:
        d.insert(pya.Polygon([pya.Point(v[i], v[i+1]) for i in range(0, len(v), 2)], True))
o = d.merged(False, 1)
print("overlap area2", sum(p.area2() for p in o.each()), "n", o.count())
for i, p in enumerate(o.each()):
    if i < 4: print(p)
m = d.merged()
print("merged area2", sum(p.area2() for p in m.each()))
