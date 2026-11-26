# Random tiny any-angle cases: two A shapes whose edges cross off-lattice + one B shape.
#   klayout -b -r kl_gen_synth.py -rd n=300 -rd seed=1 -rd dir=synth
import pya, random, os
random.seed(int(seed)); os.makedirs(dir, exist_ok=True)
def rpoly(cx, cy, r):
    # convex-ish random quad with slanted edges
    import math
    ang = sorted(random.uniform(0, 2 * math.pi) for _ in range(4))
    return pya.Polygon([pya.Point(int(cx + r * math.cos(a)), int(cy + r * math.sin(a))) for a in ang])
for i in range(int(n)):
    ly = pya.Layout(); ly.dbu = 0.001; top = ly.create_cell("CLIP")
    la, lb = ly.layer(66, 20), ly.layer(65, 20)
    for _ in range(2):
        top.shapes(la).insert(rpoly(random.randint(0, 30), random.randint(0, 30), random.randint(10, 40)))
    top.shapes(lb).insert(rpoly(random.randint(0, 30), random.randint(0, 30), random.randint(10, 40)))
    ly.write("%s/s%04d.oas" % (dir, i))
print("generated", n)
