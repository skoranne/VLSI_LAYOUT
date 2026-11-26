import pya, time
ly = pya.Layout(); ly.read(f); top = ly.top_cell()
def reg(l, d):
    r = pya.Region(top.begin_shapes_rec(ly.find_layer(l, d))); r.merged_semantics = True
    return r.merged(False, 0)
P, B, M1, V, MC = reg(66,20), reg(65,20), reg(68,20), reg(68,44), reg(67,44)
N = (B - P).merged(False, 0); G = (P & B).merged(False, 0)
def c(name, r): print("KLTOPO %-10s %d" % (name, r.count()), flush=True)
c("inside", MC.inside(M1)); c("notinside", MC.not_inside(M1))
c("interact", M1.overlapping(V)); c("outside", M1.not_interacting(V))
c("meet", P.interacting(B) - P.overlapping(B)); c("cut", P.overlapping(B) - P.inside(B))
c("contains", M1.covering(V))
c("n", N); c("g", G)
c("nmeet", N.interacting(G) - N.overlapping(G)); c("ninter", N.overlapping(G)); c("noutside", N.not_interacting(G))
c("ginside", G.inside(B)); c("dcontains", B.covering(G)); c("pinside", P.inside(B))
