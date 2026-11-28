import pya
shapes = []
for line in open(fd):
    f = line.split(); v = list(map(int, f[1:]))
    if f[0] == "T":
        y1, y2, bl, br, tl, tr = v
        pts = [(bl, y1), (br, y1), (tr, y2), (tl, y2)]
    else:
        pts = [(v[i], v[i+1]) for i in range(0, len(v), 2)]
    shapes.append(pts)
def a2(pts):
    n = len(pts); return sum(pts[i][0]*pts[(i+1)%n][1] - pts[(i+1)%n][0]*pts[i][1] for i in range(n))
d = pya.Region()
for k, pts in enumerate(shapes):
    p = pya.Polygon([pya.Point(x, y) for x, y in pts], True)
    d.insert(p)
m = d.merged()
# assign each shape to the merged component containing its first interior-ish point: use bbox centre test via Region interact in bulk
mp = list(m.each())
# spatial hash of components by bbox
import collections
grid = collections.defaultdict(list); G = 4000
for i, p in enumerate(mp):
    b = p.bbox()
    for gx in range(b.left // G, b.right // G + 1):
        for gy in range(b.bottom // G, b.top // G + 1): grid[(gx, gy)].append(i)
s = [0] * len(mp); found = 0; diffs = []
for pts in shapes:
    P = pya.Polygon([pya.Point(x, y) for x, y in pts], True)
    b = P.bbox(); c = pya.Point((b.left + b.right) // 2, (b.bottom + b.top) // 2)
    # a point strictly inside the shape: centroid of first triangle-ish; use KLayout's inside on candidates
    cand = grid[(b.left // G, b.bottom // G)]
    best = None
    cs = [i for i in cand if b.inside(mp[i].bbox())]
    if len(cs) == 1: best = cs[0]
    else:
        for i in cs:
            if pya.Region(mp[i]).interacting(pya.Region(P)).count(): best = i; break
    if best is None: continue
    s[best] += a2(pts); found += 1
nb = 0
for i, p in enumerate(mp):
    if s[i] != p.area2():
        nb += 1
        if nb <= 2:
            print("component", p.area2(), "shapes sum", s[i], p)
            b = p.bbox()
            for pts in shapes:
                if all(b.left <= x <= b.right and b.bottom <= y <= b.top for x, y in pts): print("   ", pts)
print("components", len(mp), "differing", nb, "assigned", found, "of", len(shapes))
