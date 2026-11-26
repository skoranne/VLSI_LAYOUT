# KLayout (Python, 0.28+) check of TRAPBOOL.exe -pnum output (any-angle polygon numbering).
#   klayout -b -r kl_pnum_check.py -rd fout=OUT.oas -rd pnum=PREFIX -rd ops=OR,AND -rd out=1000
# For op k: KLayout merges our output layer out+k-1 (min_coherence off: corner-touching pieces
# join, the PNUM rule) and every merged polygon is keyed by its lowest-then-leftmost vertex.
#   polys_kl / polys_ours   polygon counts
#   key_miss                our polygons without a KLayout polygon at the same key (and vice versa)
#   area_diff               polygons with the same key but a different 2A (KLayout's merge snaps
#                           vertices lying within a pixel of another edge; 0 at multiples of 90)
#   bbox_diff               polygons with the same key but a different bounding box
import pya, csv

ly = pya.Layout(); ly.read(fout)
base = int(out)
for k, op in enumerate(ops.split(',')):
    li = ly.find_layer(base + k, 0)
    R = pya.Region()
    if li is not None:
        for t in ly.top_cells():
            R.insert(pya.Region(t.begin_shapes_rec(li)))
    kl = {}
    for p in R.merged().each():
        pts = [(pt.y, pt.x) for pt in p.each_point_hull()]
        y, x = min(pts)
        b = p.bbox()
        kl[(x, y)] = (p.area2(), b.left, b.bottom, b.right, b.top)
    ours = {}
    with open("%s_%s.csv" % (pnum, op)) as f:
        for r in csv.DictReader(f):
            ours[(int(r["vx"]), int(r["vy"]))] = (int(r["area2"]), int(r["x1"]), int(r["y1"]), int(r["x2"]), int(r["y2"]))
    miss = sum(1 for key in ours if key not in kl) + sum(1 for key in kl if key not in ours)
    adiff = sum(1 for key in ours if key in kl and kl[key][0] != ours[key][0])
    bdiff = sum(1 for key in ours if key in kl and kl[key][1:] != ours[key][1:])
    amax = max([abs(kl[key][0] - ours[key][0]) for key in ours if key in kl] or [0])
    print("KLPNUM op=%s polys_kl=%d polys_ours=%d key_miss=%d area_diff=%d area_diff_max=%d bbox_diff=%d" %
          (op, len(kl), len(ours), miss, adiff, amax, bdiff), flush=True)
