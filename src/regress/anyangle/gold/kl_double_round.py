# Show KLayout's two-step rounding on a clip: B - A with A merged first (as kl_bool_check and
# most flows do) vs one Boolean over the raw shapes (a single rounding).
import pya
ly = pya.Layout(); ly.read(f)
def reg(l, d):
    r = pya.Region(ly.top_cell().begin_shapes_rec(ly.find_layer(l, d))); r.merged_semantics = False; return r
A, B = reg(66, 20), reg(65, 20)
print("A merged :", [str(p) for p in A.merged().each()])
two = B.merged() - A.merged()
one = B - A            # one EdgeProcessor pass over raw edges
print("B - A.merged()  :", [str(p) for p in two.each()])
print("B - A (raw, one):", [str(p) for p in one.each()])
