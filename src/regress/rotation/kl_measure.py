# KLayout ground truth for the rotation regression: per layer of the flattened top cell, the
# merged region's polygon count, holes, vertices (hull + hole points), area and perimeter.
#   klayout -b -r kl_measure.py -rd f=FILE.oas [-rd layers=68/20,67/20]
# Prints  KLMEASURE file=F layer=L/D polygons= holes= vertices= area= perimeter= time=
# Merged semantics as MAGPARSER `exec measure`: closed set, corner contact joins
# (Region.merged(min_coherence=False)).
import pya, time, os

ly = pya.Layout()
t0 = time.time()
ly.read(f)
top = ly.top_cell()
want = None
if globals().get("layers", ""):
    want = set(tuple(int(v) for v in s.split("/")) for s in layers.split(","))
print("KLREAD file=%s top=%s %.1fs" % (os.path.basename(f), top.name, time.time() - t0), flush=True)
def emit(name, m, t1):
    npoly = holes = verts = 0
    for p in m.each():
        npoly += 1
        h = p.holes()
        holes += h
        verts += p.num_points_hull()
        for k in range(h):
            verts += p.num_points_hole(k)
    print("KLMEASURE file=%s layer=%s polygons=%d holes=%d vertices=%d area=%d perimeter=%d time=%.1f" %
          (os.path.basename(f), name, npoly, holes, verts, m.area(), m.perimeter(), time.time() - t1), flush=True)

def region(l, d):
    li = ly.find_layer(l, d)
    r = pya.Region() if li is None else pya.Region(top.begin_shapes_rec(li))
    r.merged_semantics = True
    return r

for li in ly.layer_indexes():
    info = ly.get_info(li)
    if want is not None and (info.layer, info.datatype) not in want:
        continue
    t1 = time.time()
    r = pya.Region(top.begin_shapes_rec(li))
    if r.is_empty():
        continue
    r.merged_semantics = True
    emit("%d/%d" % (info.layer, info.datatype), r.merged(False, 0), t1)

# derived layers of rot_ops.ctr.in (-rd derived=1)
if globals().get("derived", "0") == "1":
    diff, poly, li1, met1, met2 = region(65, 20), region(66, 20), region(67, 20), region(68, 20), region(69, 20)
    for name, fn in (("gate", lambda: poly & diff), ("sd", lambda: diff - poly), ("polyli", lambda: poly + li1),
                     ("pdxor", lambda: poly ^ diff), ("m1m2", lambda: met1 & met2)):
        t1 = time.time()
        emit(name, fn().merged(False, 0), t1)
