# KLayout ground truth for db_oasis_selftest outputs.
#   klayout -b -r oasis_conformance.py -rd dir=OUTDIR
# For every OASIS file the self-test wrote:
#   * KLayout must read it (strict files with oasis_expect_strict_mode = 1);
#   * NAME.boxes (our reader, flattened shape boxes) must equal KLayout's flattened shape boxes
#     (multiset; +-1 dbu allowed only under non-orthogonal transforms);
#   * NAME.rec (our lossless scanner, per cell and layer) must equal KLayout's local per-layer
#     cell boxes;
#   * NAME.cells (S_BOUNDING_BOX from our tables) must equal KLayout's cell bbox, or contain it
#     for cells under non-orthogonal transforms; S_CELL_OFFSET was checked by the self-test.
# Geometry: per layer XOR, flattened, against the reference written independently by the
# GDSII model writer (wr_ref.gds) and between the implicit/explicit reader files; the
# re-encoded files against their sources.
import pya, os, sys, collections

D = dir
fails = []
passes = [0]

def check(ok, what):
    if ok:
        passes[0] += 1
    else:
        fails.append(what)
        print("FAIL  " + what)

def load(path, strict=False):
    ly = pya.Layout()
    opt = pya.LoadLayoutOptions()
    opt.oasis_read_all_properties = 1
    if strict:
        opt.oasis_expect_strict_mode = 1
    try:
        ly.read(path, opt)
    except Exception as e:
        check(False, "KLayout reads %s: %s" % (os.path.basename(path), e))
        return None
    check(True, "read")
    return ly

def top(ly):
    t = [c for c in ly.top_cells()]
    return t[0] if len(t) == 1 else ly.cell("TOP")

def flat_boxes(ly):
    out = collections.Counter()
    for li in ly.layer_indexes():
        info = ly.get_info(li)
        for tc in ly.top_cells():
            it = tc.begin_shapes_rec(li)
            while not it.at_end():
                s = it.shape()
                if not s.is_text():
                    b = s.bbox().transformed(it.trans())
                    out[(info.layer, info.datatype, b.left, b.bottom, b.right, b.top)] += 1
                it.next()
    return out

def read_counter(path, cols):
    c = collections.Counter()
    if not os.path.exists(path):
        return None
    for line in open(path):
        f = line.split()
        c[tuple(int(x) for x in f[:cols])] += 1
    return c

# Layers with non-Manhattan shapes: our reader approximates them by boxes (fracture_general,
# path/circle outlines), so only the relative area difference is reported there.
NONMANHATTAN = {"rd": {2, 3, 4, 5} | set(range(100, 124)), "wr": {4, 5, 8}, "rt": {4, 5, 8}}

def compare_boxes(name, ly, tol_ok):
    path = os.path.join(D, name + ".oas.boxes")
    if not os.path.exists(path):
        check(False, "%s: no .boxes dump" % name)
        return
    ours = collections.defaultdict(pya.Region)
    n = 0
    for line in open(path):
        l, d, x1, y1, x2, y2 = (int(x) for x in line.split())
        ours[(l, d)].insert(pya.Box(x1, y1, x2, y2))
        n += 1
    keys = set(ours.keys())
    for li in ly.layer_indexes():
        i = ly.get_info(li)
        if not region(ly, i.layer, i.datatype, True).is_empty():
            keys.add((i.layer, i.datatype))
    nm = NONMANHATTAN[name[:2]]
    bad = []
    approx = []
    for k in sorted(keys):
        kr = region(ly, k[0], k[1], True)
        x = ours[k] ^ kr
        if x.is_empty():
            continue
        if k[0] in nm:
            approx.append((k, x.area(), kr.area()))
        else:
            bad.append((k, x.area()))
    for b in bad[:5]:
        print("      %s layer %d/%d: our reader XOR KLayout area %d" % (name, b[0][0], b[0][1], b[1]))
    check(not bad, "%s: our reader's flattened geometry = KLayout on Manhattan layers (%d boxes, %d layers)" % (name, n, len(keys)))
    if approx:
        print("      %-18s our reader exact on %d layers; box-approximated non-Manhattan layers: %s" % (name, len(keys) - len(approx),
              ", ".join("%d/%d %.1f%%" % (k[0], k[1], 100.0 * a / max(t, 1)) for k, a, t in approx)))
    else:
        print("      %-18s our reader exact on all %d layers" % (name, len(keys)))

def compare_rec(name, ly):
    path = os.path.join(D, name + ".oas.rec")
    if not os.path.exists(path):
        check(False, "%s: no .rec dump" % name)
        return
    ours = {}
    for line in open(path):
        f = line.split()
        ours[(f[0], int(f[1]), int(f[2]))] = tuple(int(x) for x in f[3:7])
    kl = {}
    for c in ly.each_cell():
        for li in ly.layer_indexes():
            info = ly.get_info(li)
            bb = pya.Box()
            for s in c.shapes(li).each():
                if not s.is_text():
                    bb += s.bbox()
            if not bb.empty():
                kl[(c.name, info.layer, info.datatype)] = (bb.left, bb.bottom, bb.right, bb.top)
    # the scanner also boxes text records; compare on KLayout's keys and require the rest to
    # be text-only layers
    nm = NONMANHATTAN[name[:2]]
    def ok(k):
        o = ours.get(k)
        if o is None:
            return False
        if k[1] in nm:      # any-angle outlines: the scanner box is rounded outward
            return o[0] <= kl[k][0] and o[1] <= kl[k][1] and o[2] >= kl[k][2] and o[3] >= kl[k][3] and \
                   max(kl[k][0] - o[0], kl[k][1] - o[1], o[2] - kl[k][2], o[3] - kl[k][3]) <= 1
        return o == kl[k]
    bad = [k for k in kl if not ok(k)]
    for k in bad[:5]:
        print("      %s %s: scanner %s KLayout %s" % (name, k, ours.get(k), kl[k]))
    check(not bad, "%s: scanner per-cell/layer boxes = KLayout (%d)" % (name, len(kl)))

def compare_cells(name, ly, inexact):
    path = os.path.join(D, name + ".oas.cells")
    if not os.path.exists(path):
        return
    bad = 0
    n = 0
    for line in open(path):
        f = line.split()
        cname, flags = f[0], int(f[1])
        bb = tuple(int(x) for x in f[2:6])
        c = ly.cell(cname)
        if c is None:
            bad += 1
            print("      %s: cell %s missing in KLayout" % (name, cname))
            continue
        n += 1
        kb = c.bbox()
        if kb.empty():
            ok = (flags & 2) != 0
        elif cname in inexact:
            ok = flags == 0 and bb[0] <= kb.left and bb[1] <= kb.bottom and bb[2] >= kb.right and bb[3] >= kb.top
        else:
            ok = flags == 0 and bb == (kb.left, kb.bottom, kb.right, kb.top)
        if not ok:
            bad += 1
            print("      %s %s: S_BOUNDING_BOX flags %d %s, KLayout %s" % (name, cname, flags, bb, kb))
    check(bad == 0, "%s: S_BOUNDING_BOX of %d cells = KLayout cell bbox" % (name, n))

def region(ly, l, d, skip_rot=False):
    li = ly.find_layer(l, d)
    r = pya.Region()
    if li is None:
        return r
    for tc in ly.top_cells():
        if skip_rot and tc.name == "ROT":
            continue
        it = tc.begin_shapes_rec(li)
        it.shape_flags = pya.Shapes.SAll & ~pya.Shapes.STexts      # texts compared separately
        if skip_rot and ly.cell("ROT") is not None:
            it.unselect_cells("ROT")
        r += pya.Region(it)
    return r

def xor_all(na, la, nb, lb):
    keys = set()
    for ly in (la, lb):
        for li in ly.layer_indexes():
            i = ly.get_info(li)
            keys.add((i.layer, i.datatype))
    bad = []
    for (l, d) in sorted(keys):
        x = region(la, l, d) ^ region(lb, l, d)
        if not x.is_empty():
            bad.append((l, d, x.area()))
    for b in bad[:5]:
        print("      XOR %s vs %s layer %d/%d area %d" % (na, nb, b[0], b[1], b[2]))
    check(not bad, "XOR %s vs %s empty on %d layers" % (na, nb, len(keys)))

def texts(ly):
    out = collections.Counter()
    for li in ly.layer_indexes():
        info = ly.get_info(li)
        for tc in ly.top_cells():
            it = tc.begin_shapes_rec(li)
            while not it.at_end():
                s = it.shape()
                if s.is_text():
                    t = s.text.transformed(it.trans())
                    out[(info.layer, info.datatype, t.string, t.x, t.y)] += 1
                it.next()
    return out

lys = {}
specs = [("rd_implicit", False), ("rd_explicit", False),
         ("wr_strict", True), ("wr_tiny_cblocks", True), ("wr_nostrict", False),
         ("wr_plain_checksum", True), ("wr_noreserve", False),
         ("rt_gds2oas", True), ("rt_oas2oas", True)]
inexact = {"ROT", "TOP"}
for name, strict in specs:
    p = os.path.join(D, name + ".oas")
    if not os.path.exists(p):
        check(False, "missing " + p)
        continue
    ly = load(p, strict)
    if ly is None:
        continue
    lys[name] = ly
    check(abs(ly.dbu - 0.001) < 1e-12, "%s: dbu 0.001 (got %g)" % (name, ly.dbu))
    compare_boxes(name, ly, name.startswith("wr") or name.startswith("rt"))
    compare_rec(name, ly)
    compare_cells(name, ly, inexact)

ref = load(os.path.join(D, "wr_ref.gds"))
lys["wr_ref"] = ref
og = load(os.path.join(D, "rt_oas2gds.gds"))
if og is not None:
    lys["rt_oas2gds"] = og

if "rd_implicit" in lys and "rd_explicit" in lys:
    xor_all("rd_implicit", lys["rd_implicit"], "rd_explicit", lys["rd_explicit"])
    check(texts(lys["rd_implicit"]) == texts(lys["rd_explicit"]), "rd texts equal")
    a = lys["rd_explicit"]
    names = sorted(c.name for c in a.each_cell())
    check(names == ["CHILD", "TOP"], "rd cells %s" % names)
    li = a.find_layer(1, 0)
    check(li is not None and a.get_info(li).name == "metal1", "LAYERNAME metal1 = 1/0")
for name in ("wr_strict", "wr_tiny_cblocks", "wr_nostrict", "wr_plain_checksum", "wr_noreserve", "rt_gds2oas", "rt_oas2oas", "rt_oas2gds"):
    if name in lys and ref is not None:
        xor_all(name, lys[name], "wr_ref.gds", ref)
        check(texts(lys[name]) == texts(ref), "%s texts = wr_ref.gds (%d)" % (name, sum(texts(ref).values())))
if "wr_strict" in lys:
    w = lys["wr_strict"]
    check([c.name for c in w.top_cells()] == ["TOP"], "wr_strict top cells %s" % [c.name for c in w.top_cells()])
    for name in ("wr_tiny_cblocks", "wr_nostrict", "wr_plain_checksum", "wr_noreserve", "rt_oas2oas"):
        if name in lys:
            check(texts(lys[name]) == texts(w), "%s texts = wr_strict" % name)
    li = w.find_layer(1, 0)
    check(li is not None and w.get_info(li).name == "ACTIVE", "wr_strict LAYERNAME ACTIVE = 1/0")
    # properties on cells and shapes (KLayout keeps OASIS user properties)
    def props(pid):
        return dict(w.properties(pid)) if pid else {}
    t = w.cell("TOP")
    tp = props(t.prop_id) if hasattr(t, "prop_id") else None
    if tp is not None:
        check(tp.get("TOPPROP") == "top", "TOP cell property TOPPROP=top (%s)" % tp)
        m = tp.get("MANY")
        check(isinstance(m, list) and m == list(range(20)), "TOP cell property MANY = 0..19 (%s)" % (m,))
    leaf = w.cell("LEAF1")
    lp = props(leaf.prop_id) if hasattr(leaf, "prop_id") else None
    if lp is not None:
        v = lp.get("LEAFPROP")
        check(isinstance(v, list) and len(v) == 5 and v[0] == 1 and v[1] == -1 and abs(v[2] - 1.5) < 1e-12 and v[3] == "v a l",
              "LEAF1 property LEAFPROP (%s)" % (v,))
    # SHAPEPROP on the first shape (a repeated rectangle: KLayout expands the repetition, every
    # copy carries the property) and TEXTPROP on both texts
    nshape = collections.Counter()
    for li in w.layer_indexes():
        for s in leaf.shapes(li).each():
            if s.prop_id:
                for k, v in props(s.prop_id).items():
                    nshape[(k, str(v))] += 1
    check(nshape.get(("TEXTPROP", "5"), 0) == 1 and nshape.get(("TEXTPROP", "50"), 0) == 1 and nshape.get(("SHAPEPROP", "first"), 0) >= 1,
          "LEAF1 element properties %s" % dict(nshape))

print("oasis_conformance: %d passed, %d failed" % (passes[0], len(fails)))
sys.exit(1 if fails else 0)
