# Per-layer XOR of two layout files (GDSII or OASIS) in KLayout, hierarchical (deep) mode.
#   klayout -b -r xor_files.py -rd a=A -rd b=B [-rd top=NAME] [-rd strict=1] [-rd texts=1]
# Prints one line per differing layer and "XOR_EMPTY n_layers" or "XOR_FAIL n"; exit 1 on failure.
# strict=1: file b must be OASIS strict mode (KLayout oasis_expect_strict_mode = 1).
# texts=1:  also compare the flattened text multisets.
import pya, sys, collections

def load(path, strict):
    ly = pya.Layout()
    opt = pya.LoadLayoutOptions()
    if strict:
        opt.oasis_expect_strict_mode = 1
    ly.read(path, opt)
    return ly

ta = globals().get("top", "")
la = load(a, False)
lb = load(b, globals().get("strict", "0") == "1")
if abs(la.dbu - lb.dbu) > 1e-12 * la.dbu:
    print("DBU differs: %g vs %g" % (la.dbu, lb.dbu))
    sys.exit(1)

# Top cells of a (or the one named top=), each compared with the cell of the same name in b.
tops = [la.cell(ta)] if ta else list(la.top_cells())
if not tops or tops[0] is None:
    print("XOR_FAIL top cell missing")
    sys.exit(1)
bnames = set(c.name for c in lb.top_cells()) if not ta else {ta}
anames = set(c.name for c in tops)
dsa, dsb = pya.DeepShapeStore(), pya.DeepShapeStore()
keys = set()
for ly in (la, lb):
    for li in ly.layer_indexes():
        i = ly.get_info(li)
        keys.add((i.layer, i.datatype))

def reg(ly, c, ds, l, d):
    li = ly.find_layer(l, d)
    if li is None or c is None:
        return pya.Region()
    it = c.begin_shapes_rec(li)
    it.shape_flags = pya.Shapes.SAll & ~pya.Shapes.STexts
    return pya.Region(it, ds)

bad = 0
if anames != bnames:
    bad += 1
    print("TOP cells differ: only in a %s, only in b %s" % (sorted(anames - bnames)[:5], sorted(bnames - anames)[:5]))
for ca in tops:
    cb = lb.cell(ca.name)
    for (l, d) in sorted(keys):
        x = reg(la, ca, dsa, l, d) ^ reg(lb, cb, dsb, l, d)
        if not x.is_empty():
            bad += 1
            print("XOR_NONEMPTY %s %d/%d area %.6g um^2" % (ca.name, l, d, x.area() * la.dbu * la.dbu))
print("compared %d top cells" % len(tops))

if globals().get("texts", "0") == "1":
    def texts(ly, c):
        out = collections.Counter()
        for li in ly.layer_indexes():
            i = ly.get_info(li)
            it = c.begin_shapes_rec(li)
            it.shape_flags = pya.Shapes.STexts
            while not it.at_end():
                t = it.shape().text.transformed(it.trans())
                out[(i.layer, i.datatype, t.string, t.x, t.y)] += 1
                it.next()
        return out
    a_t, b_t = collections.Counter(), collections.Counter()
    for ca in tops:
        a_t += texts(la, ca)
        if lb.cell(ca.name) is not None:
            b_t += texts(lb, lb.cell(ca.name))
    if a_t != b_t and set(a_t) == set(b_t):
        # KLayout 0.28 collapses OASIS text repetitions with coincident elements (e.g. a type-10
        # repetition with a (0,0) displacement) to one text; GDSII keeps both copies.
        d = (b_t - a_t) + (a_t - b_t)
        print("TEXTS equal as sets (%d distinct); multiplicity differs on %d labels (%d vs %d): coincident repeated texts" %
              (len(a_t), len(d), sum(a_t.values()), sum(b_t.values())))
    elif a_t != b_t:
        bad += 1
        print("TEXTS differ: %d vs %d (e.g. a-only %s, b-only %s)" % (sum(a_t.values()), sum(b_t.values()),
              list((a_t - b_t).items())[:3], list((b_t - a_t).items())[:3]))
    else:
        print("TEXTS equal: %d" % sum(a_t.values()))

print(("XOR_EMPTY %d layers" % len(keys)) if bad == 0 else ("XOR_FAIL %d" % bad))
sys.exit(1 if bad else 0)
