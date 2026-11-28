# KLayout (Python, 0.28+) cross-check of TRAPBOOL.exe output.
#   klayout -b -r kl_bool_check.py -rd fin=IN.oas -rd fout=OUT.oas -rd a=66/20 -rd b=65/20 \
#           -rd ops=OR,AND,NOT,XOR -rd out=1000
# Per op k (TRAPBOOL wrote it to layer out+k-1): R = KLayout's A op B on the input, M = our result.
#   kl_area2       2A of R (merged)
#   out_area2      2A of our shapes as read, summed without merging (interior-disjoint, so it must
#                  equal TRAPBOOL's area2 exactly)
#   xor_area       area(R ^ M); xor_wide1/2: (R ^ M) shrunk by 1 / 2 dbu (0: every difference is
#                  thinner than 2 / 4 dbu, i.e. snap rounding only); xor_maxdim: largest XOR bbox side
import pya, time

def region(ly, l, d):
    r = pya.Region()
    li = ly.find_layer(l, d)
    if li is None:
        return r
    for t in ly.top_cells():
        r.insert(pya.Region(t.begin_shapes_rec(li)))
    return r

def spec(s):
    l, d = [int(v) for v in s.split('/')]
    return l, d

ly_in = pya.Layout(); ly_in.read(fin)
ly_out = pya.Layout(); ly_out.read(fout)
t0 = time.time()
A = region(ly_in, *spec(a)).merged()
B = region(ly_in, *spec(b)).merged()
print("KLBOOL load_merge_s=%.2f A_polys=%d B_polys=%d" % (time.time() - t0, A.count(), B.count()), flush=True)
base = int(out)
for k, op in enumerate(ops.split(',')):
    t0 = time.time()
    if op == 'OR':
        R = A + B
    elif op == 'AND':
        R = A & B
    elif op == 'NOT':
        R = A - B
    elif op == 'XOR':
        R = A ^ B
    elif op == 'BNOTA':
        R = B - A
    elif op == 'A':
        R = A.dup()
    elif op == 'B':
        R = B.dup()
    else:
        raise RuntimeError("unknown op " + op)
    R.merge()
    t1 = time.time()
    M = region(ly_out, base + k, 0)
    X = R ^ M
    xa = X.area()
    w1 = X.sized(-1).area() if xa else 0
    w2 = X.sized(-2).area() if xa else 0
    a2_kl = sum(p.area2() for p in R.each())
    a2_out = sum(p.area2() for p in M.each())
    maxdim = 0
    for p in X.each():
        bb = p.bbox()
        maxdim = max(maxdim, bb.width(), bb.height())
    print("KLBOOL op=%s layer=%d/0 kl_area2=%d out_area2=%d xor_area=%d xor_polys=%d xor_wide1=%d "
          "xor_wide2=%d xor_maxdim=%d kl_polys=%d kl_op_s=%.2f total_s=%.2f" %
          (op, base + k, a2_kl, a2_out, xa, X.count(), w1, w2, maxdim, R.count(), t1 - t0, time.time() - t0),
          flush=True)
