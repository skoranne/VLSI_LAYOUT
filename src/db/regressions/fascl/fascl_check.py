# KLayout (Python) check of MAGPARSER FASCL property OASIS output against the input layout.
#   klayout -b -r fascl_check.py -rd ref=fascl_in.gds -rd oas=fascl_full.oas -rd layers=68/20,69/20 \
#           -rd csv68=fascl_m1.csv -rd csv69=fascl_m2.csv -rd strict=1 -rd cblock=1
# Checks
#   * geometry: per layer, output XOR input is empty, and the output holds exactly one shape per
#     connected polygon of the input (same multiset of (bbox, area));
#   * 68/20 properties, recomputed here independently from the merged input polygons:
#       PNUM      canonical rank of (left edge x, lowest y on the left edge)
#       AREA_UM2  area * 1e-6 (FLOAT)        KIND  "rect" | "poly<nboxes>" (nboxes from the CSV)
#       INFO      [w, W, h, H, pn, PNUM, tag, "m1 <PNUM%3>"] (STRUCT as a value list)
#       BIG       1, present only when area > 1e6 (WHEN guard)
#       CHK       PNUM*1000 + W%7 + 2*(KIND=="rect") (reads of earlier properties)
#   * 69/20 properties: K = nboxes, SHAPE = "rect" | "ell";
#   * CSV dumps agree with the geometry (ordinal == PNUM, bbox, area);
#   * file structure: STRICT table offsets in END point at the table records, END is 256 bytes,
#     the cell body is CBLOCK-compressed when cblock=1.
# Exit status 0 = pass.
import pya
import sys
import csv as csvmod

errors = []
def fail(msg):
    errors.append(msg)
    if len(errors) < 40:
        print("FAIL:", msg)

def opt(name, default=None):
    return globals().get(name, default)

ref_path, oas_path = opt("ref"), opt("oas")
layers = [tuple(int(v) for v in s.split("/")) for s in opt("layers", "68/20").split(",")]
want_strict = opt("strict", "1") == "1"
want_cblock = opt("cblock", "1") == "1"

def load(path):
    ly = pya.Layout()
    ly.read(path)
    return ly

ref = load(ref_path)
out = load(oas_path)

def layer_polys(ly, l, d, with_props):
    li = ly.find_layer(l, d)
    res = []
    if li is None:
        return res
    for top in ly.top_cells():
        it = top.begin_shapes_rec(li)
        while not it.at_end():
            sh = it.shape()
            if sh.is_box() or sh.is_polygon() or sh.is_path():
                res.append((sh.polygon.transformed(it.trans()), sh.properties() if with_props else {}))
            it.next()
    return res

def region_of(polys):
    r = pya.Region()
    for p, _ in polys:
        r.insert(p)
    return r

def key(p):
    b = p.bbox()
    return (b.left, b.bottom, b.right, b.top, p.area())

def left_anchor(p):
    b = p.bbox()
    ys = [pt.y for pt in p.each_point_hull() if pt.x == b.left]
    return (b.left, min(ys))

def read_csv(path):
    if not path:
        return None
    with open(path) as f:
        return list(csvmod.DictReader(f))

csvs = {(68, 20): read_csv(opt("csv68")), (69, 20): read_csv(opt("csv69"))}

for (l, d) in layers:
    tag = "%d/%d" % (l, d)
    ref_polys = layer_polys(ref, l, d, False)
    out_polys = layer_polys(out, l, d, True)
    rin, rout = region_of(ref_polys), region_of(out_polys)
    x = rin ^ rout
    if not x.is_empty():
        fail("%s: XOR not empty (%d polygons, area %d)" % (tag, x.count(), x.area()))
    merged = list(rin.merged().each())
    if len(out_polys) != len(merged):
        fail("%s: %d output shapes vs %d input polygons" % (tag, len(out_polys), len(merged)))
    if sorted(key(p) for p in merged) != sorted(key(p) for p, _ in out_polys):
        fail("%s: output polygons are not the input's connected polygons" % tag)
    rows = csvs.get((l, d))
    by_bbox_area = {}
    if rows is not None:
        if len(rows) != len(merged):
            fail("%s: CSV has %d rows, expected %d" % (tag, len(rows), len(merged)))
        for r in rows:
            by_bbox_area[(int(r["x1"]), int(r["y1"]), int(r["x2"]), int(r["y2"]), int(r["area"]))] = r

    if (l, d) == (68, 20):
        order = sorted(merged, key=left_anchor)
        rank = {key(p): i + 1 for i, p in enumerate(order)}
        seen = set()
        for p, pr in out_polys:
            k = key(p)
            if k not in rank:
                fail("68/20 output polygon %s is not a polygon of the input" % (k,)); continue
            pn = rank[k]
            b = p.bbox()
            w, h, area = b.width(), b.height(), p.area()
            row = by_bbox_area.get(k)
            if row is None:
                fail("68/20 polygon %s missing from CSV" % (k,)); continue
            nb = int(row["nboxes"])
            if int(row["ordinal"]) != pn:
                fail("68/20 %s: CSV ordinal %s != canonical rank %d" % (k, row["ordinal"], pn))
            if nb == 1 and not p.is_box():
                fail("68/20 %s: one box but not a rectangle" % (k,))
            kind = "rect" if nb == 1 else "poly%d" % nb
            exp = {"PNUM": pn, "AREA_UM2": area / 1000000.0, "KIND": kind,
                   "INFO": ["w", w, "h", h, "pn", pn, "tag", "m1 %d" % (pn % 3)],
                   "CHK": pn * 1000 + w % 7 + 2 * (1 if kind == "rect" else 0)}
            if area > 1000000:
                exp["BIG"] = 1
            if set(pr.keys()) != set(exp.keys()):
                fail("68/20 PNUM %d: property names %s, expected %s" % (pn, sorted(pr.keys()), sorted(exp.keys())))
                continue
            for n, v in exp.items():
                got = pr[n]
                if isinstance(v, float):
                    ok = isinstance(got, float) and abs(got - v) <= 1e-12 * max(1.0, abs(v))
                else:
                    ok = (got == v) and type(got) == type(v)
                if not ok:
                    fail("68/20 PNUM %d: %s = %r (%s), expected %r" % (pn, n, got, type(got).__name__, v))
            if pn in seen:
                fail("68/20: PNUM %d appears twice" % pn)
            seen.add(pn)
        if seen != set(range(1, len(merged) + 1)):
            fail("68/20: PNUM values are not 1..%d" % len(merged))
    elif (l, d) == (69, 20):
        for p, pr in out_polys:
            row = by_bbox_area.get(key(p))
            if row is None:
                fail("69/20 polygon %s missing from CSV" % (key(p),)); continue
            nb = int(row["nboxes"])
            exp = {"K": nb, "SHAPE": "rect" if nb == 1 else "ell"}
            if pr != exp:
                fail("69/20 %s: properties %r, expected %r" % (key(p), pr, exp))
    elif (l, d) == (70, 20):
        for p, pr in out_polys:
            exp = {"ROWY": p.bbox().bottom}
            if pr != exp:
                fail("70/20 %s: properties %r, expected %r" % (key(p), pr, exp))
    print("layer %s: %d polygons checked" % (tag, len(out_polys)))

# ---- raw structure: START offset-flag, CBLOCK, STRICT table offsets in END
data = open(oas_path, "rb").read()
pos = [0]
def u8():
    v = data[pos[0]]; pos[0] += 1; return v
def uint():
    v, s = 0, 0
    while True:
        b = u8(); v |= (b & 0x7f) << s; s += 7
        if not b & 0x80:
            return v
def skip_real():
    t = uint()
    if t in (0, 1, 2, 3): uint()
    elif t in (4, 5): uint(); uint()
    elif t == 6: pos[0] += 4
    elif t == 7: pos[0] += 8
if data[:13] != b"%SEMI-OASIS\r\n":
    fail("bad magic")
pos[0] = 13
if u8() != 1:
    fail("START record expected")
n = uint(); pos[0] += n
skip_real()
offset_flag = uint()
if offset_flag == 0:
    for _ in range(12): uint()
if want_strict and offset_flag != 1:
    fail("STRICT output must keep table offsets in END (offset-flag 1)")
rec = u8()
if rec == 13:
    uint()
    rec = u8()
if want_cblock and rec != 34:
    fail("cell body is not CBLOCK-compressed (record %d)" % rec)
if not want_cblock and rec == 34:
    fail("unexpected CBLOCK")
end = data[-256:]
if end[0] != 2:
    fail("END record is not the last 256 bytes")
if offset_flag == 1:
    pos[0] = len(data) - 255
    expect_rec = [(3, 4), (5, 6), (7, 8), (9, 10), (11, 12), (30, 31)]
    names = ["CELLNAME", "TEXTSTRING", "PROPNAME", "PROPSTRING", "LAYERNAME", "XNAME"]
    for i in range(6):
        flag, off = uint(), uint()
        if want_strict and flag != 1:
            fail("%s table not flagged strict" % names[i])
        if off:
            if data[off] not in expect_rec[i]:
                fail("%s table offset %d points at record %d" % (names[i], off, data[off]))
        elif names[i] in ("CELLNAME", "PROPNAME", "PROPSTRING"):
            fail("%s table offset missing" % names[i])

# ---- record scan of the cell body: element records, repetition types, expanded instance count
import zlib
want_rep = opt("rep")            # "1": repetitions required, "0": forbidden, None: not checked

class Stream:
    def __init__(self, b):
        self.b, self.p = b, 0
    def u8(self):
        v = self.b[self.p]; self.p += 1; return v
    def uint(self):
        v, s = 0, 0
        while True:
            c = self.u8(); v |= (c & 0x7f) << s; s += 7
            if not c & 0x80:
                return v
    def skip(self, n):
        self.p += n
    def string(self):
        n = self.uint(); self.p += n
    def gdelta(self):
        if self.uint() & 1:
            self.uint()

scan = {"elements": 0, "instances": 0, "reps": {}, "props": 0, "prop_repeats": 0, "cblocks": 0}
modal_count = [1]

def repetition(s):
    t = s.uint()
    if t == 0:
        return modal_count[0], 0
    if t == 1:
        nx, ny = s.uint() + 2, s.uint() + 2; s.uint(); s.uint(); c = nx * ny
    elif t in (2, 3):
        c = s.uint() + 2; s.uint()
    elif t in (4, 6, 5, 7):
        n = s.uint(); c = n + 2
        if t in (5, 7): s.uint()
        for _ in range(n + 1): s.uint()
    elif t == 8:
        nx, ny = s.uint() + 2, s.uint() + 2; s.gdelta(); s.gdelta(); c = nx * ny
    elif t == 9:
        c = s.uint() + 2; s.gdelta()
    elif t in (10, 11):
        n = s.uint(); c = n + 2
        if t == 11: s.uint()
        for _ in range(n + 1): s.gdelta()
    else:
        raise ValueError("repetition type %d" % t)
    modal_count[0] = c
    return c, t

def prop_value(s):
    t = s.uint()
    if t in (0, 1, 2, 3, 8, 9, 13, 14, 15): s.uint()
    elif t in (4, 5): s.uint(); s.uint()
    elif t == 6: s.skip(4)
    elif t == 7: s.skip(8)
    elif t in (10, 11, 12): s.string()
    else: raise ValueError("property value type %d" % t)

def element(s, rid):
    info = s.u8()
    if info & 0x01: s.uint()
    if info & 0x02: s.uint()
    if rid == 20:
        if info & 0x40: s.uint()
        if info & 0x20: s.uint()
    else:
        if info & 0x20:
            t, n = s.uint(), s.uint()
            for _ in range(n):
                if t in (0, 1, 2, 3): s.uint()
                else: s.gdelta()
    if info & 0x10: s.uint()
    if info & 0x08: s.uint()
    c = 1
    if info & 0x04:
        c, t = repetition(s)
        scan["reps"][t] = scan["reps"].get(t, 0) + 1
    scan["elements"] += 1
    scan["instances"] += c

def body(s, stop_at_tables):
    while s.p < len(s.b):
        rid = s.u8()
        if rid in (15, 16, 0):
            continue
        if rid in (20, 21):
            element(s, rid)
        elif rid == 28:
            info = s.u8()
            if info & 0x04:
                if info & 0x02: s.uint()
                else: s.string()
            if not info & 0x08:
                n = info >> 4
                if n == 15: n = s.uint()
                for _ in range(n): prop_value(s)
            scan["props"] += 1
        elif rid == 29:
            scan["prop_repeats"] += 1
        elif rid == 34:
            scan["cblocks"] += 1
            s.uint(); s.uint(); zn = s.uint()
            inner = zlib.decompress(bytes(s.b[s.p:s.p + zn]), -15)
            s.skip(zn)
            body(Stream(inner), False)
        elif rid in (2, 3, 4, 7, 8, 9, 10, 11, 12) and stop_at_tables:
            return
        else:
            raise ValueError("unexpected record %d in cell body" % rid)

try:
    st = Stream(data)
    st.p = 13
    st.u8(); st.string()
    t = st.uint()
    if t in (0, 1, 2, 3): st.uint()
    elif t in (4, 5): st.uint(); st.uint()
    elif t == 6: st.skip(4)
    elif t == 7: st.skip(8)
    if st.uint() == 0:
        for _ in range(12): st.uint()
    if st.u8() != 13:
        fail("CELL record expected after START")
    st.uint()
    body(st, True)
except Exception as e:
    fail("record scan failed: %s" % e)
n_shapes = sum(len(layer_polys(out, l, d, False)) for (l, d) in layers)
print("records: %d elements -> %d instances, repetitions %s, %d PROPERTY + %d PROPERTY-REPEAT, %d CBLOCK"
      % (scan["elements"], scan["instances"], dict(sorted(scan["reps"].items())), scan["props"], scan["prop_repeats"],
         scan["cblocks"]))
if opt("mincblocks") and scan["cblocks"] < int(opt("mincblocks")):
    fail("expected at least %s CBLOCKs, found %d" % (opt("mincblocks"), scan["cblocks"]))
if scan["instances"] != n_shapes:
    fail("element records expand to %d instances, KLayout sees %d shapes" % (scan["instances"], n_shapes))
if want_rep == "1" and not scan["reps"]:
    fail("no repetitions written")
if want_rep == "0" and scan["reps"]:
    fail("repetitions written although disabled")
if opt("prepeat") == "1" and scan["prop_repeats"] == 0:
    fail("no PROPERTY-REPEAT records although consecutive elements share a property")

if errors:
    print("FASCL OASIS CHECK FAILED: %d problem(s) in %s" % (len(errors), oas_path))
    sys.exit(1)
print("FASCL OASIS CHECK PASSED: %s" % oas_path)
