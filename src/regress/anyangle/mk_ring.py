# Synthetic any-angle cases for TRAPMERGE: rotated square rings (holes), overlapping rotated
# rectangles, a 45-degree ring, nested rings. klayout -b -r mk_ring.py -rd out=FILE.oas
import pya, math
ly = pya.Layout(); ly.dbu = 0.001
top = ly.create_cell("TOP")
L = ly.layer(1, 0)
def rot(pts, a, cx, cy):
    c, s = math.cos(math.radians(a)), math.sin(math.radians(a))
    return [pya.Point(int(round(cx + x*c - y*s)), int(round(cy + x*s + y*c))) for x, y in pts]
def sq(h): return [(-h, -h), (h, -h), (h, h), (-h, h)]
k = 0
for a in (0, 15, 30, 45, 60, 75, 7.3):
    for j in range(3):
        cx, cy = 20000*k, 20000*j
        outer = rot(sq(5000 + 700*j), a, cx, cy)
        inner = rot(sq(2500 + 300*j), a + 3*j, cx, cy)
        p = pya.Polygon(outer); p.insert_hole(inner)
        top.shapes(L).insert(p)
        # overlapping bar through the ring
        top.shapes(L).insert(pya.Polygon(rot([(-7000, -400), (7000, -400), (7000, 400), (-7000, 400)], a + 20, cx, cy)))
        # small square inside the hole (island)
        top.shapes(L).insert(pya.Polygon(rot(sq(600), a + 10, cx + 1200, cy + 1000)))
    k += 1
ly.write(out)
