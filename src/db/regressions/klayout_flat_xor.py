# Ground-truth flat XOR of two layouts, per (layer, datatype), with KLayout
# 0.30.3 (Python only; see AGENTS.md for LD_LIBRARY_PATH):
#
#   klayout -b -r klayout_flat_xor.py -rd A=mkALU.gds -rd B=mkALU.oas
#
# Prints shape count and merged area per layer for both files and the XOR area.
# The last line gives the number of layers whose XOR is not empty.
import pya
a = pya.Layout(); a.read(A)
b = pya.Layout(); b.read(B)
print("A dbu", a.dbu, "cells", a.cells(), "top", a.top_cell().name, a.top_cell().bbox())
print("B dbu", b.dbu, "cells", b.cells(), "top", b.top_cell().name, b.top_cell().bbox())
keys = {}
for li in a.layer_indexes():
    i = a.get_info(li); keys.setdefault((i.layer, i.datatype), [None, None])[0] = li
for li in b.layer_indexes():
    i = b.get_info(li); keys.setdefault((i.layer, i.datatype), [None, None])[1] = li
bad = 0
for k in sorted(keys):
    la, lb = keys[k]
    ra = pya.Region(a.top_cell().begin_shapes_rec(la)) if la is not None else pya.Region()
    rb = pya.Region(b.top_cell().begin_shapes_rec(lb)) if lb is not None else pya.Region()
    x = ra ^ rb
    ax = x.area()
    if ax: bad += 1
    print("L%d/%d  A shapes %9d area %16d | B shapes %9d area %16d | XOR area %d%s" % (k[0], k[1], ra.count(), ra.area(), rb.count(), rb.area(), ax, "  <-- DIFF" if ax else ""))
print("layers with differences:", bad)
