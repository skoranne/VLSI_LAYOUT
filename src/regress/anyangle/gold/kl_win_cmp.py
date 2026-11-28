# Compare full-design results with clip results inside a window (op AND, layer 1001).
import pya
def region(ly, l, d):
    r = pya.Region(); li = ly.find_layer(l, d)
    if li is not None:
        for t in ly.top_cells(): r.insert(pya.Region(t.begin_shapes_rec(li)))
    return r
x1, y1, x2, y2 = [int(v) for v in win.split(',')]
W = pya.Region(pya.Box(x1, y1, x2, y2))
full_in = pya.Layout(); full_in.read(fin)
full_out = pya.Layout(); full_out.read(fout)
cl_in = pya.Layout(); cl_in.read(cin)
cl_out = pya.Layout(); cl_out.read(cout)
WI = W.sized(400)
Af = (region(full_in, 66, 20) & WI); Bf = (region(full_in, 65, 20) & WI)
Ac = region(cl_in, 66, 20); Bc = region(cl_in, 65, 20)
print("raw A full-in-win(%d) vs clip(%d)" % (region(full_in,66,20).interacting(W).count(), Ac.count()))
KLf = (region(full_in, 66, 20).merged() & region(full_in, 65, 20).merged()) & W
KLc = (Ac.merged() & Bc.merged()) & W
Of = region(full_out, 1001, 0) & W
Oc = region(cl_out, 1001, 0) & W
print("KL full vs clip xor area:", (KLf ^ KLc).area())
print("ours full vs clip xor area:", (Of ^ Oc).area())
print("ours vs KL (full) xor area:", (Of ^ KLf).area(), " (clip):", (Oc ^ KLc).area())
