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

# ---------------------------------------------------------------------------------------------
# Second file: outline measures + CONNECT/NET_ID stack (m1 71/20, via 72/20, m2 73/20,
# alternate via set 74/20 = 72/20 plus one via that joins the "m2 over m1 without via" pair).
# ---------------------------------------------------------------------------------------------
nl = pya.Layout()
nl.dbu = 0.001
nt = nl.create_cell("NETS")
M1, V, M2, V2 = nl.layer(71, 20), nl.layer(72, 20), nl.layer(73, 20), nl.layer(74, 20)
def nb(li, x1, y1, x2, y2):
    nt.shapes(li).insert(pya.Box(x1, y1, x2, y2))
def via(x1, y1, x2, y2):
    nb(V, x1, y1, x2, y2); nb(V2, x1, y1, x2, y2)
Y = 60000
nb(M1, 0, Y, 10000, Y + 200)                                         # straight wire: W end
nb(M1, 12000, Y, 16000, Y + 300); nb(M1, 15800, Y + 300, 16000, Y + 3000)   # L, unequal widths
nb(M1, 20000, Y, 21000, Y + 1000)                                    # square pad: no line end
nb(M1, 23000, Y, 27000, Y + 400); nb(M1, 24800, Y + 400, 25200, Y + 2000)   # T
nb(M1, 30000, Y, 30500, Y + 500); nb(M1, 30500, Y + 500, 31000, Y + 1000)   # corner-touching pair
nb(M1, 33000, Y, 37000, Y + 1000); nb(M1, 33000, Y + 3000, 37000, Y + 4000) # ring ...
nb(M1, 33000, Y + 1000, 34000, Y + 3000); nb(M1, 36000, Y + 1000, 37000, Y + 3000)
nb(M1, 34000, Y + 1800, 35000, Y + 2200)                             # ... with a stub into the hole
# nets
nb(M1, 40000, Y, 44000, Y + 200); nb(M2, 43000, Y - 1000, 43200, Y + 3000)
via(43050, Y + 50, 43150, Y + 150)                                   # inside m1 and m2: connects
nb(M1, 40000, Y + 1000, 42000, Y + 1200); nb(M2, 42050, Y + 900, 42400, Y + 1300)
via(42000, Y + 1050, 42100, Y + 1150)                                # abuts m1 edge: m2 only
nb(M1, 40000, Y + 2000, 41000, Y + 2200)
via(41000, Y + 2200, 41100, Y + 2300)                                # m1 corner only: own net
nb(M1, 46000, Y, 48000, Y + 200); nb(M2, 46500, Y - 1000, 46700, Y + 1000)  # m2 over m1, no via
nb(V2, 46550, Y + 50, 46650, Y + 150)                                # ... joined only in 74/20
nb(M1, 50000, Y, 51000, Y + 200); nb(M1, 51100, Y, 52000, Y + 200)
via(50950, Y + 50, 51150, Y + 150)                                   # straddles two m1: merges
nb(M1, 54000, Y, 55000, Y + 200); nb(M1, 56900, Y, 58000, Y + 200)   # m1-via-m2-via-m1 chain
nb(M2, 54700, Y + 400, 57200, Y + 600)
nb(M2, 54700, Y, 54950, Y + 600); nb(M2, 56950, Y, 57200, Y + 600)
via(54800, Y + 50, 54900, Y + 150); via(57000, Y + 50, 57100, Y + 150)
via(60000, Y, 60100, Y + 100)                                        # isolated via
nout = out.replace(".gds", "_net.gds") if out.endswith(".gds") else out + "_net.gds"
nl.write(nout)
print("wrote", nout)
