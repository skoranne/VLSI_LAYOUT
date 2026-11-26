# Find where TRAPBOOL and KLayout disagree for one op and write minimal input clips there.
#   klayout -b -r kl_find_clips.py -rd fin=IN.oas -rd fout=TRAPBOOL_OUT.oas -rd a=66/20 -rd b=65/20
#           -rd ops=AND,XOR -rd out=1000 -rd nclip=4 -rd pad=40 -rd prefix=clip
# A clip keeps the raw input shapes of A and B that touch the window (bbox of one disagreement
# polygon grown by pad): whole shapes, never cut, so the clip's input is exactly the design's.
import pya
def spec(s): return [int(v) for v in s.split('/')]
def region(ly, l, d):
    r = pya.Region(); li = ly.find_layer(l, d)
    if li is not None:
        for t in ly.top_cells(): r.insert(pya.Region(t.begin_shapes_rec(li)))
    return r
li_ = pya.Layout(); li_.read(fin); lo = pya.Layout(); lo.read(fout)
la, lb = spec(a), spec(b)
Araw, Braw = region(li_, *la), region(li_, *lb)
Araw.merged_semantics = False; Braw.merged_semantics = False
A, B = Araw.merged(), Braw.merged()
OPL = {"OR": 0, "AND": 1, "NOT": 2, "XOR": 3, "BNOTA": 4}   # TRAPBOOL -ops OR,AND,NOT,XOR,BNOTA order
for op in ops.split(','):
  R = {"AND": A & B, "OR": A + B, "NOT": A - B, "XOR": A ^ B, "BNOTA": B - A}[op]
  M = region(lo, int(out) + OPL[op], 0)
  X = (R ^ M)
  polys = sorted(X.each(), key=lambda p: -max(p.bbox().width(), p.bbox().height()))
  picked = polys[:int(nclip) // 2] + sorted(X.each(), key=lambda p: -p.area())[:int(nclip) - int(nclip) // 2]
  for k, p in enumerate(picked):
      w = p.bbox().enlarged(int(pad), int(pad))
      wr = pya.Region(w)
      # raw flattened shapes, one polygon each, never merged (a merge would round crossings)
      ly = pya.Layout(); ly.dbu = li_.dbu; top = ly.create_cell("CLIP")
      na = nb = 0
      for src, spec_, lab in ((Araw, la, 'a'), (Braw, lb, 'b')):
          sh = top.shapes(ly.layer(spec_[0], spec_[1]))
          for q in src.each():
              if q.bbox().touches(w):
                  sh.insert(q)
                  if lab == 'a': na += 1
                  else: nb += 1
      ca, cb = na, nb
      fn = "%s_%s_%d.oas" % (prefix, op.lower(), k)
      ly.write(fn)
      print("CLIP %s window=%s xor_poly_area=%d xor_poly_bbox=%s A_shapes=%d B_shapes=%d" %
            (fn, w, p.area(), p.bbox(), ca, cb), flush=True)
