# KLayout (Python) checker: MAGPARSER's flattened output must equal KLayout's flattening.
#   klayout -b -r hier_check.py -rd ref=hier_in.oas -rd oas=out.oas -rd layers=68/20,67/20
import pya, sys

ref_ly = pya.Layout(); ref_ly.read(ref)
out_ly = pya.Layout(); out_ly.read(oas)
ok = True
for spec in layers.split(","):
    l, d = (int(v) for v in spec.split("/"))
    ri = ref_ly.find_layer(l, d)
    oi = out_ly.find_layer(l, d)
    r = pya.Region()
    for t in ref_ly.top_cells():
        r += pya.Region(t.begin_shapes_rec(ri))
    o = pya.Region()
    if oi is not None:
        for t in out_ly.top_cells():
            o += pya.Region(t.begin_shapes_rec(oi))
    r.merge(); o.merge()
    x = r ^ o
    print("layer %s: ref area %d (%d polygons)  out area %d (%d polygons)  xor area %d" %
          (spec, r.area(), r.count(), o.area(), o.count(), x.area()))
    if r.is_empty() or not x.is_empty():
        ok = False
if not ok:
    print("HIER LOAD CHECK FAILED:", oas)
    sys.exit(1)
print("HIER LOAD CHECK PASSED:", oas)
