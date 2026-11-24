# Independent KLayout reference for MAGPARSER `exec defcheck` (no MAGPARSER code involved):
#   * the layout's nets: merged polygons per stack layer, a cut joins the routing polygons below
#     and above it that it overlaps with positive area (union-find)
#   * the DEF read by KLayout's own LEF/DEF reader (net names on the wire shapes); probes at
#     the midpoint of every wire segment and the centre of every box, and at the origin of every
#     via instance on its metal layers (a via takes the net of the DEF wire covering its origin)
#   * opens / shorts / missed DEF nets and the per-polygon NET_NAME, compared with MAGPARSER's
#     <prefix>_opens.csv, _shorts.csv, _misses.csv and a propdump CSV with a NAME column
#
#   klayout -b -r defcheck_kl.py -rd layout=x.oas -rd defp=x.def -rd lefs=a.lef[,b.lef]
#       -rd stack=SKY130|ASAP7|GT2N|GT2N_FULL [-rd lo=M1 -rd hi=M5] -rd prefix=tiny
#       [-rd polycsv=tiny_m1.csv -rd polylayer=met1] [-rd expect=clean|tiny_mut]
# Layer numbers are transcribed from the PDK maps (see gen_defcheck_input.py), not from MAGPARSER.
import pya, csv, sys, os

G = globals()
lo_name, hi_name = G.get("lo", ""), G.get("hi", "")
expect = G.get("expect", "clean")
SKY130 = [("li1", 67, 20, "R"), ("mcon", 67, 44, "C"), ("met1", 68, 20, "R"), ("via", 68, 44, "C"),
          ("met2", 69, 20, "R"), ("via2", 69, 44, "C"), ("met3", 70, 20, "R"), ("via3", 70, 44, "C"),
          ("met4", 71, 20, "R"), ("via4", 71, 44, "C"), ("met5", 72, 20, "R")]
ASAP7 = [("M1", 19, 0, "R"), ("V1", 21, 0, "C"), ("M2", 20, 0, "R")]
for i, (m, v) in enumerate([(30, 25), (40, 35), (50, 45), (60, 55), (70, 65), (80, 75), (90, 85)]):
    ASAP7 += [("V%d" % (i + 2), v, 0, "C"), ("M%d" % (i + 3), m, 0, "R")]
GT2N = [("M0", 20, 0, "R")]
for i in range(13):
    GT2N += [("V%d" % i, 22 + 5 * i, 0, "C"), ("M%d" % (i + 1), 25 + 5 * i, 0, "R")]
GT2N += [("V13", 87, 0, "C"), ("RDL", 90, 0, "R")]
BACK = [("BPR", 8, 0, "R"), ("BV0", 910, 0, "C"), ("BM1", 920, 0, "R"), ("BV1", 930, 0, "C"), ("BM2", 940, 0, "R"),
        ("BV2", 950, 0, "C"), ("BM3", 960, 0, "R"), ("BV3", 970, 0, "C"), ("BM4", 980, 0, "R"), ("BV4", 990, 0, "C"),
        ("BRDL", 1000, 0, "R")]
STACKS = {"SKY130": SKY130, "ASAP7": ASAP7, "GT2N": GT2N, "GT2N_BS": BACK,
          "GT2N_FULL": list(reversed(BACK)) + [("VBPR", 9, 0, "C")] + GT2N}
S = STACKS[stack]
names = [s[0] for s in S]
a = names.index(lo_name) if lo_name else 0
b = names.index(hi_name) if hi_name else len(S) - 1
S = S[a:b + 1]

fails = []
def fail(m):
    fails.append(m); print("FAIL:", m)

# ------------------------------------------------------------------ layout nets
ly = pya.Layout(); ly.read(layout)
top = ly.top_cell()
polys, off = [], [0]
for (nm, gl, gd, kd) in S:
    li = ly.find_layer(gl, gd)
    P = list(pya.Region(top.begin_shapes_rec(li)).merged().each()) if li is not None else []
    polys.append(P)
    off.append(off[-1] + len(P))
par = list(range(off[-1]))
def find(x):
    while par[x] != x:
        par[x] = par[par[x]]; x = par[x]
    return x
GRID = 4000
def grid_index(P):
    g = {}
    for j, p in enumerate(P):
        bb = p.bbox()
        for gx in range(bb.left // GRID, bb.right // GRID + 1):
            for gy in range(bb.bottom // GRID, bb.top // GRID + 1):
                g.setdefault((gx, gy), []).append(j)
    return g
gidx = [grid_index(P) for P in polys]
for c in range(1, len(S), 2):
    for r in (c - 1, c + 1):
        for i, cp in enumerate(polys[c]):
            bb, seen, rc = cp.bbox(), set(), None
            for gx in range(bb.left // GRID, bb.right // GRID + 1):
                for gy in range(bb.bottom // GRID, bb.top // GRID + 1):
                    for j in gidx[r].get((gx, gy), ()):
                        if j in seen: continue
                        seen.add(j)
                        pb = polys[r][j].bbox()
                        if not (max(bb.left, pb.left) < min(bb.right, pb.right) and max(bb.bottom, pb.bottom) < min(bb.top, pb.top)):
                            continue
                        if not (cp.is_box() and polys[r][j].is_box()):
                            if rc is None: rc = pya.Region(cp)
                            if (rc & pya.Region(polys[r][j])).area() <= 0: continue
                        x, y = find(off[c] + i), find(off[r] + j)
                        if x != y: par[max(x, y)] = min(x, y)

def poly_at(k, pt):                     # k = stack position, pt in layout dbu
    for j in gidx[k].get((pt.x // GRID, pt.y // GRID), ()):
        if polys[k][j].inside(pt):
            return j
    return None

# ------------------------------------------------------------------ DEF via KLayout
opt = pya.LoadLayoutOptions()
cfg = opt.lefdef_config
cfg.lef_files = [os.path.abspath(f) for f in lefs.split(",") if f]
cfg.produce_net_names = True
cfg.net_property_name = "NET"
cfg.read_lef_with_def = False
dl = pya.Layout(); dl.read(defp, opt)
dtop = dl.top_cell()
scale = dl.dbu / ly.dbu
def to_layout(x, y):
    return pya.Point(int(round(x * scale)), int(round(y * scale)))
pos_of = {nm: k for k, (nm, gl, gd, kd) in enumerate(S) if kd == "R"}
dlayers = {}
for li in dl.layer_indexes():
    nm = dl.get_info(li).name
    if nm in pos_of: dlayers[li] = pos_of[nm]

probes = []                              # (net, stack pos, layout point)
wires = {}                               # stack pos -> list of (DEF polygon in DEF dbu, net)
skipped = 0
for li, k in dlayers.items():
    it = dtop.begin_shapes_rec(li)
    while not it.at_end():
        s = it.shape()
        net = s.property("NET")
        if net is not None and it.path() == []:
            t = it.trans()
            wires.setdefault(k, []).append((s.polygon.transformed(t), net))
            if s.is_path():
                pts = [t * p for p in s.path.each_point()]
                for p, q in zip(pts, pts[1:]):
                    if p != q:
                        probes.append((net, k, to_layout((p.x + q.x) / 2.0, (p.y + q.y) / 2.0)))
            else:
                pg = s.polygon.transformed(t)
                ctr = pg.bbox().center()
                if pg.inside(ctr): probes.append((net, k, to_layout(ctr.x, ctr.y)))
                else: skipped += 1
        it.next()
wgrid = {k: grid_index([w[0] for w in W]) for k, W in wires.items()}

# KLayout keeps no net on via instances: take it from the DEF text (a small scan of its own:
# net name, current point, via name in NETS / SPECIALNETS), else from a DEF wire covering it.
def def_via_nets(path):
    import gzip
    txt = (gzip.open(path, "rt") if path.endswith(".gz") else open(path)).read().split()
    units, m, sec, net, x, y, i = 1000, {}, None, None, 0, 0, 0
    while i < len(txt):
        w = txt[i]
        if w == "UNITS" and txt[i + 1] == "DISTANCE": units = int(txt[i + 3])
        elif w in ("NETS", "SPECIALNETS") and sec is None and txt[i + 2] == ";": sec = w; i += 3; continue
        elif w == "END" and sec is not None and i + 1 < len(txt) and txt[i + 1] == sec: sec = None
        elif sec is not None:
            if w == "-":
                net = txt[i + 1].replace("\\", ""); i += 2; continue
            num = lambda s: s == "*" or s.lstrip("-").replace(".", "", 1).isdigit()
            if w == "(" and not (num(txt[i + 1]) and num(txt[i + 2])):      # ( comp pin ) list
                while txt[i] != ")": i += 1
                i += 1; continue
            if w == "(":
                if txt[i + 1] != "*": x = int(float(txt[i + 1]))
                if txt[i + 2] != "*": y = int(float(txt[i + 2]))
                while txt[i] != ")": i += 1
                nxt = txt[i + 1]
                if nxt not in ("(", ")", ";", "+", "NEW", "RECT", "MASK", "VIRTUAL") and not nxt.lstrip("-").isdigit() \
                        and nxt not in ("N", "S", "E", "W", "FN", "FS", "FE", "FW"):
                    m.setdefault((x, y, nxt), set()).add(net)
        i += 1
    return units, m
def_units, vianet = def_via_nets(defp)
vias_used = vias_orphan = 0
for inst in dtop.each_inst():
    vc = inst.cell
    if not vc.name.startswith("VIA_"): continue
    ks = sorted(k for li, k in dlayers.items() if not vc.bbox_per_layer(li).empty())
    if not ks: continue
    o = inst.trans.disp
    du = (int(round(o.x * dl.dbu * def_units)), int(round(o.y * dl.dbu * def_units)), vc.name[4:])
    netset = set(vianet.get(du, ()))
    if not netset:
        for k in ks:
            for j in wgrid.get(k, {}).get((o.x // GRID, o.y // GRID), ()):
                pg, net = wires[k][j]
                if pg.inside(o): netset.add(net)
    if len(netset) != 1:
        vias_orphan += 1; continue
    net = netset.pop()
    vias_used += 1
    for k in ks:
        probes.append((net, k, to_layout(o.x, o.y)))

# ------------------------------------------------------------------ aggregate
d2r, r2d, missnets, nmiss = {}, {}, set(), 0
for net, k, pt in probes:
    j = poly_at(k, pt)
    if j is None:
        missnets.add(net); nmiss += 1; continue
    r = find(off[k] + j)
    d2r.setdefault(net, set()).add(r)
    r2d.setdefault(r, set()).add(net)
opens = {n for n, rs in d2r.items() if len(rs) > 1}
shorts = {frozenset(ds) for r, ds in r2d.items() if len(ds) > 1}
print("reference: %d layout polygons, %d probes (%d via instances placed, %d without a unique wire net, %d shapes skipped)"
      % (off[-1], len(probes), vias_used, vias_orphan, skipped))
print("reference: opens=%d shorts=%d missed DEF nets=%d (probes %d) named nets=%d" %
      (len(opens), len(shorts), len(missnets), nmiss, len(r2d)))

# ------------------------------------------------------------------ compare with MAGPARSER
def rows(f):
    return list(csv.DictReader(open(f))) if os.path.exists(f) else None
mo, ms, mm = rows(prefix + "_opens.csv"), rows(prefix + "_shorts.csv"), rows(prefix + "_misses.csv")
if mo is None or ms is None or mm is None:
    fail("MAGPARSER report files %s_*.csv missing" % prefix)
else:
    m_opens = {r["def_net"] for r in mo}
    sh = {}
    for r in ms: sh.setdefault(r["extracted_net"], set()).add(r["def_net"])
    m_shorts = {frozenset(v) for v in sh.values()}
    m_miss = {r["def_net"] for r in mm}
    if m_opens != opens: fail("opens differ: MAGPARSER %s reference %s" % (sorted(m_opens), sorted(opens)))
    if m_shorts != shorts: fail("shorts differ: MAGPARSER %s reference %s" % (sorted(map(sorted, m_shorts)), sorted(map(sorted, shorts))))
    if m_miss != missnets: fail("missed DEF nets differ: MAGPARSER %s reference %s" % (sorted(m_miss), sorted(missnets)))
    print("MAGPARSER: opens=%d shorts=%d missed DEF nets=%d" % (len(m_opens), len(m_shorts), len(m_miss)))

polycsv = G.get("polycsv", "")
if polycsv:
    k = names[a:b + 1].index(G.get("polylayer"))
    key = {}
    for j, p in enumerate(polys[k]):
        bb = p.bbox()
        key[(bb.left, bb.bottom, bb.right, bb.top, p.area())] = j
    bad = nrow = nnamed = 0
    for r in csv.DictReader(open(polycsv)):
        nrow += 1
        kk = (int(r["x1"]), int(r["y1"]), int(r["x2"]), int(r["y2"]), int(r["area"]))
        if kk not in key:
            bad += 1
            if bad <= 3: print("  no reference polygon for", kk)
            continue
        ref = r2d.get(find(off[k] + key[kk]), set())
        got = set(r["NAME"].split("|")) if r["NAME"] else set()
        if r["NAME"].endswith("|..."):
            ok = (got - {"..."}) <= ref and len(ref) > 8
        else:
            ok = got == ref
        if r["NAME"]: nnamed += 1
        if not ok:
            bad += 1
            if bad <= 5: print("  polygon %s: MAGPARSER NAME=%r reference %s" % (kk, r["NAME"], sorted(ref)[:8]))
    if nrow != len(polys[k]): fail("%s: %d rows for %d reference polygons" % (polycsv, nrow, len(polys[k])))
    if bad: fail("%s: %d polygons with a different NET_NAME" % (polycsv, bad))
    print("NET_NAME: %d %s polygons (%d named) agree with the reference" % (nrow - bad, G.get("polylayer"), nnamed))

if expect == "clean":
    if opens or shorts or missnets: fail("expected a clean match, reference has opens/shorts/misses")
    if not r2d: fail("no named nets at all")
elif expect == "tiny_mut":
    if "_002_" not in opens: fail("the removed via did not open _002_")
    if not any({"_002_", "_003_"} <= s for s in shorts): fail("the met2 strap did not short _002_ and _003_")
    if "_001_" not in missnets: fail("the moved wire of _001_ was not missed")
print("DEFCHECK REFERENCE %s: %s" % ("FAILED" if fails else "PASSED", os.path.basename(layout)))
sys.exit(1 if fails else 0)
