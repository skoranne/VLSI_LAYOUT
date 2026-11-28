import pya
bad = 0; tw = 0
for line in open(fd):
    f = line.split(); v = list(map(int, f[1:]))
    if f[0] == "T":
        y1, y2, bl, br, tl, tr = v
        if bl > br or tl > tr: tw += 1
        continue
    pts = [(v[i], v[i+1]) for i in range(0, len(v), 2)]
    n = len(pts); a2 = sum(pts[i][0]*pts[(i+1)%n][1] - pts[(i+1)%n][0]*pts[i][1] for i in range(n))
    p = pya.Polygon([pya.Point(x, y) for x, y in pts], True)
    m = sum(q.area2() for q in pya.Region(p).merged().each())
    if m != a2:
        bad += 1
        if bad <= 3: print("shoelace", a2, "union", m, "n", n, pts[:40])
print("bad", bad, "twisted", tw)
