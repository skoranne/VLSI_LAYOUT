# Dump a clip's input polygons, KLayout's Booleans and TRAPBOOL's results as JSON rings.
#   klayout -b -r kl_dump.py -rd fin=clip.oas -rd fout=ours.oas -rd a=66/20 -rd b=65/20
#           -rd ops=OR,AND,NOT,XOR,BNOTA -rd out=1000 -rd json=clip.json
import pya, json
def spec(s): return [int(v) for v in s.split('/')]
def region(ly, l, d):
    r = pya.Region(); li = ly.find_layer(l, d)
    if li is not None:
        for t in ly.top_cells(): r.insert(pya.Region(t.begin_shapes_rec(li)))
    return r
def rings(r, merged):
    out = []
    for p in (r.merged() if merged else r).each():
        out.append([[q.x, q.y] for q in p.each_point_hull()])
        for h in range(p.holes()):
            out.append([[q.x, q.y] for q in p.each_point_hole(h)])
    return out
def raw_polys(r):
    return [[[q.x, q.y] for q in p.each_point_hull()] for p in r.each()]   # raw shapes: no holes
li_ = pya.Layout(); li_.read(fin); lo = pya.Layout(); lo.read(fout)
Ar, Br = region(li_, *spec(a)), region(li_, *spec(b))
for p in list(Ar.each()) + list(Br.each()):
    assert p.holes() == 0, "raw input shape with holes"
A, B = Ar.merged(), Br.merged()
D = {"A": raw_polys(Ar), "B": raw_polys(Br), "kl": {}, "ours": {}}
for k, op in enumerate(ops.split(',')):
    R = {"AND": A & B, "OR": A + B, "NOT": A - B, "XOR": A ^ B, "BNOTA": B - A}[op]
    D["kl"][op] = rings(R, True)
    D["ours"][op] = rings(region(lo, int(out) + k, 0), False)   # pieces as written (disjoint)
json.dump(D, open(jout, 'w'))
print("KLDUMP ok")
