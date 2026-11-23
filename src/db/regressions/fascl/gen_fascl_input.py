# KLayout (Python) generator for the FASCL property regression input.
#   klayout -b -r gen_fascl_input.py -rd out=fascl_in.gds
# Layer 68/20: assorted polygons (rects, L, U, ring, two holes, overlapping boxes) that get
#              per-polygon-unique properties.
# Layer 69/20: repeated shapes (lattice, rows, irregular placements, identical L-shapes) that get
#              low-cardinality properties, so the OASIS writer can use repetitions.
import pya

ly = pya.Layout()
ly.dbu = 0.001
top = ly.create_cell("FASCL_TOP")
m1 = ly.layer(68, 20)
m2 = ly.layer(69, 20)

def box(li, x1, y1, x2, y2):
    top.shapes(li).insert(pya.Box(x1, y1, x2, y2))

def poly(li, pts, holes=()):
    p = pya.Polygon([pya.Point(x, y) for x, y in pts])
    for h in holes:
        p.insert_hole([pya.Point(x, y) for x, y in h])
    top.shapes(li).insert(p)

# --- 68/20
for i in range(4):                                  # 4 x 3 lattice of squares
    for j in range(3):
        box(m1, i * 1000, j * 1200, i * 1000 + 500, j * 1200 + 500)
for x in (6000, 6700, 7600, 8100, 9300):            # irregular row
    box(m1, x, 0, x + 300, 800)
poly(m1, [(0, 5000), (3000, 5000), (3000, 5600), (600, 5600), (600, 8000), (0, 8000)])   # L
poly(m1, [(5000, 5000), (9000, 5000), (9000, 9000), (5000, 9000)],
     [[(6000, 6000), (8000, 6000), (8000, 8000), (6000, 8000)]])                           # ring
poly(m1, [(10000, 0), (16000, 0), (16000, 4000), (10000, 4000)],
     [[(11000, 1000), (12000, 1000), (12000, 3000), (11000, 3000)],
      [(13500, 1000), (15000, 1000), (15000, 2500), (13500, 2500)]])                       # two holes
poly(m1, [(10000, 6000), (14000, 6000), (14000, 9000), (13500, 9000), (13500, 6500),
          (10500, 6500), (10500, 9000), (10000, 9000)])                                    # U
box(m1, 0, 10000, 2000, 11000)                      # two overlapping boxes -> one polygon
box(m1, 1500, 10500, 3000, 12000)
poly(m1, [(5000, 10000), (8000, 10000), (8000, 13000), (5000, 13000)],
     [[(5500, 10500), (7500, 10500), (7500, 12500), (5500, 12500)]])                       # ring with
box(m1, 6000, 11000, 7000, 12000)                   # an island inside its hole
box(m1, 20000, 20000, 21000, 21000)                 # boxes touching only at corners: MAGPARSER
box(m1, 21000, 21000, 22000, 22000)                 # numbers them as one polygon
box(m1, 22000, 20000, 23000, 21000)
poly(m1, [(25000, 20000), (29000, 20000), (29000, 24000), (25000, 24000)],              # ring whose
     [[(26000, 21000), (27000, 21000), (27000, 22000), (28000, 22000), (28000, 23000),   # hole is two
       (27000, 23000), (27000, 22000), (26000, 22000)]])                                 # corner-joined squares

# --- 69/20
for i in range(10):                                 # 10 x 10 lattice
    for j in range(10):
        box(m2, 20000 + 400 * i, 400 * j, 20000 + 400 * i + 200, 400 * j + 200)
for i in range(3):                                  # 2 x 3 identical L-shapes
    for j in range(2):
        x, y = 20000 + 3000 * i, 10000 + 4000 * j
        poly(m2, [(x, y), (x + 2000, y), (x + 2000, y + 500), (x + 500, y + 500), (x + 500, y + 2500), (x, y + 2500)])
for x in (30000, 30500, 31700, 32000, 34100):       # same y, irregular pitch
    box(m2, x, 0, x + 100, 300)
for (x, y) in ((30000, 3000), (31234, 5555), (33333, 4100), (35000, 7777)):   # scattered
    box(m2, x, y, x + 100, y + 300)
for y in (9000, 9400, 9900, 11000):                 # same x, irregular pitch
    box(m2, 36000, y, 36100, y + 300)

# --- 70/20: two rows of rectangles, one property (row y): consecutive equal PROPERTY records
m3 = ly.layer(70, 20)
for y in (50000, 51000):
    for i in range(5):
        box(m3, 1000 * i, y, 1000 * i + 200, y + 200)

ly.write(out)
print("wrote", out)
