# KLayout (Python) checker for the FASCL outline builtins and CONNECT / NET_ID.
#   klayout -b -r fascl_geom_check.py -rd ref=fascl_in.gds -rd refnet=fascl_in_net.gds \
#           -rd geom68=fascl_geom68.csv -rd geom71=fascl_geom71.csv \
#           -rd net71=fascl_net71.csv -rd net72=fascl_net72.csv -rd net73=fascl_net73.csv \
#           -rd net73b=fascl_net73b.csv
# Every expected value is recomputed here from the input layouts with KLayout geometry,
# independently of MAGPARSER:
#   PERIMETER  = polygon perimeter (outer + holes)
#   VERTICES   = corner count of the hull and every hole (collinear points dropped)
#   line end   = an edge whose two corners are convex and which is strictly shorter than both
#                neighbours; narrowest wins, ties by lowest endpoint (x, y) then direction;
#                WIDTH = edge length, LENGTH = shorter neighbour, DIR = outward normal
#   NET_ID     = union of cut/routing polygons that overlap with positive area, numbered by
#                first appearance over (stack position, polygon ordinal)
import pya, sys
import csv as csvmod

errors = []
def fail(msg):
    errors.append(msg)
    if len(errors) < 40:
        print("FAIL:", msg)

def opt(name, default=None):
    return globals().get(name, default)

def load(path):
    ly = pya.Layout(); ly.read(path); return ly

def polys(ly, l, d):
    li = ly.find_layer(l, d)
    r = pya.Region()
    if li is not None:
        for top in ly.top_cells():
            r += pya.Region(top.begin_shapes_rec(li))
    return [p for p in r.merged().each()]        # corner-touching boxes join (min_coherence off)

def key(p):
    b = p.bbox()
    return (b.left, b.bottom, b.right, b.top, p.area())

def read_csv(path):
    with open(path) as f:
        return list(csvmod.DictReader(f))

def rows_by_key(rows):
    return {(int(r["x1"]), int(r["y1"]), int(r["x2"]), int(r["y2"]), int(r["area"])): r for r in rows}

def compress(pts):
    """drop consecutive duplicates and collinear points (cyclic)"""
    changed = True
    while changed and len(pts) >= 3:
        changed = False
        out = []
        n = len(pts)
        for i in range(n):
            a, b, c = pts[i - 1], pts[i], pts[(i + 1) % n]
            if b == a or (a[0] == b[0] == c[0]) or (a[1] == b[1] == c[1]):
                changed = True
                continue
            out.append(b)
        pts = out
    return pts

def signed_area(pts):
    return sum(pts[i][0] * pts[(i + 1) % len(pts)][1] - pts[(i + 1) % len(pts)][0] * pts[i][1]
               for i in range(len(pts)))

def contours(p):
    """contours oriented with the interior on the left: hull CCW, holes CW"""
    res = []
    hull = compress([(q.x, q.y) for q in p.each_point_hull()])
    if signed_area(hull) < 0: hull.reverse()
    res.append(hull)
    for h in range(p.holes()):
        c = compress([(q.x, q.y) for q in p.each_point_hole(h)])
        if signed_area(c) > 0: c.reverse()
        res.append(c)
    return res

def dir_of(a, b):
    if b[0] > a[0]: return 0
    if b[1] > a[1]: return 1
    if b[0] < a[0]: return 2
    return 3

DIRS = {0: "E", 1: "N", 2: "W", 3: "S"}

def line_end(p):
    best = None
    for L in contours(p):
        n = len(L)
        if n < 4: continue
        el = lambda i: abs(L[(i + 1) % n][0] - L[i % n][0]) + abs(L[(i + 1) % n][1] - L[i % n][1])
        ed = lambda i: dir_of(L[i % n], L[(i + 1) % n])
        for i in range(n):
            dp, d, dn = ed(i - 1), ed(i), ed(i + 1)
            if (d - dp) % 4 != 1 or (dn - d) % 4 != 1:
                continue
            li, lp, ln = el(i), el(i - 1), el(i + 1)
            if not (li < lp and li < ln):
                continue
            lo = min(L[i], L[(i + 1) % n])
            cand = (li, lo[0], lo[1], (d + 3) % 4, min(lp, ln))
            if best is None or cand[:4] < best[:4]:
                best = cand
    if best is None:
        return ("", 0, 0)
    return (DIRS[best[3]], best[0], best[4])

def check_geom(ly, l, d, csv_path):
    rows = rows_by_key(read_csv(csv_path))
    ps = polys(ly, l, d)
    if len(ps) != len(rows):
        fail("%d/%d: %d polygons in KLayout, %d rows in %s" % (l, d, len(ps), len(rows), csv_path))
    for p in ps:
        r = rows.get(key(p))
        if r is None:
            fail("%d/%d: polygon %s missing from %s" % (l, d, key(p), csv_path)); continue
        nv = sum(len(c) for c in contours(p))
        led, lew, lel = line_end(p)
        got = (int(r["PERIM"]), int(r["NV"]), r["LED"], int(r["LEW"]), int(r["LEL"]))
        want = (p.perimeter(), nv, led, lew, lel)
        if got != want:
            fail("%d/%d polygon %s: (PERIM,NV,LED,LEW,LEL) got %s want %s" % (l, d, key(p), got, want))
    print("geometry %d/%d: %d polygons checked" % (l, d, len(ps)))
    return rows

def expected_nets(ly, stack, csvs):
    """stack: [(l,d)] routing/cut/routing...; csvs: rows of each member (for ordinals)"""
    members = []
    for k, (l, d) in enumerate(stack):
        rows = rows_by_key(csvs[k])
        ps = sorted(polys(ly, l, d), key=lambda p: int(rows[key(p)]["ordinal"]))
        members.append(ps)
    off = [0]
    for ps in members: off.append(off[-1] + len(ps))
    par = list(range(off[-1]))
    def find(x):
        while par[x] != x:
            par[x] = par[par[x]]; x = par[x]
        return x
    for c in range(1, len(stack) - 1, 2):
        for r in (c - 1, c + 1):
            for i, pc in enumerate(members[c]):
                rc = pya.Region(pc)
                for j, pr in enumerate(members[r]):
                    if not pc.bbox().overlaps(pr.bbox()): continue
                    if (rc & pya.Region(pr)).area() > 0:
                        a, b = find(off[c] + i), find(off[r] + j)
                        if a != b: par[max(a, b)] = min(a, b)
    lab, nxt, out = {}, 0, []
    for k, ps in enumerate(members):
        res = {}
        for i, p in enumerate(ps):
            root = find(off[k] + i)
            if root not in lab:
                nxt += 1; lab[root] = nxt
            res[key(p)] = lab[root]
        out.append(res)
    return out, nxt

def check_nets(ly, stack, csv_paths, col):
    csvs = [read_csv(p) for p in csv_paths]
    exp, nnets = expected_nets(ly, stack, csvs)
    for k, rows in enumerate(csvs):
        for r in rows:
            kk = (int(r["x1"]), int(r["y1"]), int(r["x2"]), int(r["y2"]), int(r["area"]))
            if kk not in exp[k]:
                fail("%s: row %s not a polygon of layer %s" % (csv_paths[k], kk, stack[k])); continue
            if int(r[col]) != exp[k][kk]:
                fail("%s: polygon %s %s=%s, expected %d" % (csv_paths[k], kk, col, r[col], exp[k][kk]))
    print("nets %s: %d nets, %d member layers checked" % (col, nnets, len(stack)))
    return nnets

ref = load(opt("ref"))
check_geom(ref, 68, 20, opt("geom68"))
refnet = load(opt("refnet"))
g71 = check_geom(refnet, 71, 20, opt("geom71"))

# --- backstops (independent of both implementations)
def row_at(rows, x1, y1):
    for k, r in rows.items():
        if k[0] == x1 and k[1] == y1: return r
    return None
r = row_at(g71, 0, 60000)
if r and (r["LED"], r["LEW"], r["LEL"], r["NV"], r["PERIM"]) != ("W", "200", "10000", "4", "20400"):
    fail("straight wire: %s" % dict(r))
r = row_at(g71, 20000, 60000)
if r and (r["LED"], r["LEW"]) != ("", "0"):
    fail("square pad must have no line end: %s" % dict(r))
r = row_at(g71, 30000, 60000)
if r and (r["NV"], r["LED"]) != ("8", ""):
    fail("corner-touching pair: %s" % dict(r))

stack = [(71, 20), (72, 20), (73, 20)]
n1 = check_nets(refnet, stack, [opt("net71"), opt("net72"), opt("net73")], "NET")
if n1 != 16:
    fail("expected 16 nets in the m1/via/m2 stack, got %d" % n1)
n71 = rows_by_key(read_csv(opt("net71")))
n72 = rows_by_key(read_csv(opt("net72")))
n73 = rows_by_key(read_csv(opt("net73")))
net = lambda rows, x1, y1: int(row_at(rows, x1, y1)["NET"])
if net(n72, 42000, 61050) == net(n71, 40000, 61000):
    fail("via abutting an m1 edge must not join that m1 polygon")
if net(n72, 41000, 62200) == net(n71, 40000, 62000):
    fail("via touching an m1 corner must not join that m1 polygon")
if net(n73, 46500, 59000) == net(n71, 46000, 60000):
    fail("m2 over m1 without a via must be separate nets")
if net(n71, 50000, 60000) != net(n71, 51100, 60000):
    fail("via straddling two m1 polygons must merge them")
if not (net(n71, 54000, 60000) == net(n73, 54700, 60000) == net(n71, 56900, 60000)):
    fail("m1-via-m2-via-m1 chain must be one net")

# after the via layer is replaced by 74/20 (one more via joins the m2-over-m1 pair)
# NET2 on 73/20: compare with KLayout nets of the 71/74/73 stack (ordinals of 74/20 are not
# dumped, so number the via layer by its own canonical order: left x, then bottom y).
def expected_m2_nets_after_change():
    m1 = rows_by_key(read_csv(opt("net71")))
    m2 = rows_by_key(read_csv(opt("net73b")))
    v = sorted(polys(refnet, 74, 20), key=lambda p: (p.bbox().left, p.bbox().bottom, p.bbox().right, p.bbox().top))
    fake_v = [{"x1": p.bbox().left, "y1": p.bbox().bottom, "x2": p.bbox().right, "y2": p.bbox().top,
               "area": p.area(), "ordinal": i + 1} for i, p in enumerate(v)]
    exp, nn = expected_nets(refnet, [(71, 20), (74, 20), (73, 20)],
                            [list(m1.values()), fake_v, list(m2.values())])
    return exp[2], nn
exp73b, nb = expected_m2_nets_after_change()
rows73b = rows_by_key(read_csv(opt("net73b")))
for kk, r in rows73b.items():
    if int(r["NET2"]) != exp73b.get(kk, -1):
        fail("fascl_net73b.csv: polygon %s NET2=%s, expected %s" % (kk, r["NET2"], exp73b.get(kk)))
if int(row_at(rows73b, 46500, 59000)["NET2"]) != net(n71, 46000, 60000):
    fail("after the via layer change the m2-over-m1 pair must share a net")
print("nets NET2 after via change: %d nets" % nb)

if errors:
    print("FASCL GEOMETRY/NET CHECK FAILED: %d error(s)" % len(errors))
    sys.exit(1)
print("FASCL GEOMETRY/NET CHECK PASSED")
