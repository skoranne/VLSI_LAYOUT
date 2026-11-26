# KLayout (Python, 0.28+) cross-check of TRAPMERGE.exe output.
#   klayout -b -r kl_merge_check.py -rd fin=IN.oas -rd fout=OUT.oas -rd layers=66/20,67/20
# Per layer: A = merged input layer (KLayout's own merge), M = TRAPMERGE output layer.
#   xor_area        area(A ^ M)
#   xor_wide1/2     area of (A ^ M) shrunk by 1 / 2 dbu: 0 means every difference is thinner than
#                   2 / 4 dbu (grid snapping and corner rounding move edges by < 1 dbu each way)
#   xor_maxdim      largest bbox side of any XOR polygon (dbu)
#   out_area2       2A of the output shapes as read, summed without merging (TRAPMERGE writes
#                   interior-disjoint shapes, so this must equal its merged 2A exactly)
#   out_merged_area2  2A of KLayout's merge of the output. KLayout's merge snaps vertices that lie
#                   within a pixel of another edge (near-degenerate slivers between abutting
#                   shapes), so it may differ from out_area2 by a few dbu^2 per such sliver
import pya, time

def region(ly, l, d):
    r = pya.Region()
    li = ly.find_layer(l, d)
    if li is None:
        return r
    for t in ly.top_cells():
        r.insert(pya.Region(t.begin_shapes_rec(li)))
    return r

ly_in = pya.Layout(); ly_in.read(fin)
ly_out = pya.Layout(); ly_out.read(fout)
for spec in layers.split(','):
    l, d = [int(v) for v in spec.split('/')]
    t0 = time.time()
    A = region(ly_in, l, d)
    M = region(ly_out, l, d)
    am = A.merged()
    t1 = time.time()
    X = am ^ M
    xa = X.area()
    w1 = X.sized(-1).area() if xa else 0
    w2 = X.sized(-2).area() if xa else 0
    a2_kl = sum(p.area2() for p in am.each())
    a2_out = sum(p.area2() for p in M.each())
    a2_outm = sum(p.area2() for p in M.merged().each())
    holes = sum(p.holes() for p in am.each())
    maxdim = 0
    for p in X.each():
        b = p.bbox()
        maxdim = max(maxdim, b.width(), b.height())
    print("KLMERGE layer=%d/%d kl_area2=%d out_area2=%d out_merged_area2=%d kl_holes=%d xor_area=%d xor_polys=%d xor_wide1=%d xor_wide2=%d "
          "xor_maxdim=%d kl_merged_polys=%d kl_merge_s=%.2f total_s=%.2f" %
          (l, d, a2_kl, a2_out, a2_outm, holes, xa, X.count(), w1, w2, maxdim, am.count(), t1 - t0, time.time() - t0),
          flush=True)
