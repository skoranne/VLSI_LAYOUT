# Synthetic 45-degree layout (exact octagonal geometry on the grid) for TRAPMERGE / CTRAPEZOID
# output: octagons, 45-degree bent wires, diamonds, triangles, overlapping and abutting.
#   klayout -b -r mk_45.py -rd out=FILE.oas
import pya, random
random.seed(45)
ly = pya.Layout(); ly.dbu = 0.001
top = ly.create_cell("TOP")
L = ly.layer(2, 0)
def P(pts): return pya.Polygon([pya.Point(x, y) for x, y in pts])
for i in range(40):
    for j in range(40):
        cx, cy = 4000*i, 4000*j
        a, b = random.randint(200, 900), random.randint(100, 600)
        k = random.randint(0, 7)
        if k == 0:     # octagon
            top.shapes(L).insert(P([(cx-a, cy-a-b), (cx+a, cy-a-b), (cx+a+b, cy-a), (cx+a+b, cy+a), (cx+a, cy+a+b), (cx-a, cy+a+b), (cx-a-b, cy+a), (cx-a-b, cy-a)]))
        elif k == 1:   # diamond
            top.shapes(L).insert(P([(cx, cy-a), (cx+a, cy), (cx, cy+a), (cx-a, cy)]))
        elif k == 2:   # 45-degree bent wire (as a path)
            top.shapes(L).insert(pya.Path([pya.Point(cx-1500, cy-1000), pya.Point(cx-500, cy-1000), pya.Point(cx+500, cy), pya.Point(cx+500, cy+1500)], 2*(b//2+50)))
        elif k == 3:   # right triangles
            top.shapes(L).insert(P([(cx, cy), (cx+a, cy), (cx, cy+a)]))
            top.shapes(L).insert(P([(cx+a, cy+a), (cx, cy+a), (cx+a, cy)]))
        elif k == 4:   # overlapping octagon + rectangle
            top.shapes(L).insert(P([(cx-a, cy-a-b), (cx+a, cy-a-b), (cx+a+b, cy-a), (cx+a+b, cy+a), (cx+a, cy+a+b), (cx-a, cy+a+b), (cx-a-b, cy+a), (cx-a-b, cy-a)]))
            top.shapes(L).insert(pya.Box(cx-200, cy-1900, cx+300, cy+1900))
        elif k == 6:   # lone right triangles, each orientation (CTRAPEZOID 16-19, implied h)
            o = random.randint(0, 3)
            tri = [[(0, 0), (a, 0), (0, a)], [(0, 0), (a, a), (0, a)], [(0, 0), (a, 0), (a, a)], [(0, a), (a, 0), (a, a)]][o]
            top.shapes(L).insert(P([(cx + x, cy + y) for x, y in tri]))
            top.shapes(L).insert(pya.Box(cx - 1500, cy - 1500, cx - 1500 + b, cy - 1500 + a + b))
        elif k == 7:   # lone isosceles triangles, apex up/down/right/left (types 20-23, implied w or h)
            o = random.randint(0, 3)
            tri = [[(0, 0), (2*a, 0), (a, a)], [(0, a), (a, 0), (2*a, a)], [(0, 0), (a, a), (0, 2*a)], [(0, a), (a, 0), (a, 2*a)]][o]
            top.shapes(L).insert(P([(cx - 1800 + x, cy - 1800 + y) for x, y in tri]))
            top.shapes(L).insert(P([(cx + 300, cy), (cx + 300 + a, cy), (cx + 300 + a - b//2, cy + b//2), (cx + 300, cy + b//2)]))
        else:          # parallelogram and trapezoid
            top.shapes(L).insert(P([(cx-a, cy), (cx+a, cy), (cx+a+b, cy+b), (cx-a+b, cy+b)]))
            top.shapes(L).insert(P([(cx-a, cy+b+50), (cx+a, cy+b+50), (cx+a-b, cy+2*b+50), (cx-a+b, cy+2*b+50)]))
ly.write(out)
