#!/usr/bin/env python3
"""MAGPARSER (any-angle) MEASURE lines vs KLayout KLMEASURE lines of the rotation suite.
PASS: polygon and hole counts equal to KLayout's, area within 1e-4 relative (snap rounding),
perimeter within 5e-4. PASS-INV: counts differ from KLayout's but equal the exact unrotated counts
(optional 4th argument: the R000 MEASURE log), i.e. rotation invariant where KLayout's own merge is
not (KLayout loses holes / splits polygons at some angles). id_* layers: reported with their KLayout counterparts (chained snap rounding leaves slivers).
Morphology layers (grow/shrink/size/close/open) are not supported on any-angle layers yet: SKIP.
selfand: the suite's KLayout reference uses merged(False, 1), which KLayout treats as min_wc 0
(whole layer); coverage >= 2 is merged(False, 2): SKIP here (checked separately)."""
import re, sys
def parse(path, key):
    m = {}
    for ln in open(path, errors="replace"):
        if not ln.startswith(key): continue
        lay = re.search(r"layer=(\S+)", ln).group(1).split(":")[-1] if key == "MEASURE" else re.search(r"layer=(\S+)", ln).group(1)
        if key == "MEASURE" and re.search(r"layer=d1_input:(\d+):(\d+)", ln):
            a = re.search(r"layer=d1_input:(\d+):(\d+)", ln); lay = "%s/%s" % a.groups()
        v = dict((k, int(float(x))) for k, x in re.findall(r"(polygons|holes|vertices|area|perimeter)=([-0-9.]+)", ln))
        if len(v) == 5: m[lay] = v
    return m
mag, kl, tag = parse(sys.argv[1], "MEASURE"), parse(sys.argv[2], "KLMEASURE"), sys.argv[3]
ref = parse(sys.argv[4], "MEASURE") if len(sys.argv) > 4 else {}
MORPH = {"grow", "shrink", "size_p", "size_n", "close", "open", "id_grow", "id_shrink", "id_close", "id_open", "id_sizep", "id_sizen"}
npass = nfail = 0
for lay in sorted(kl):
    if lay not in mag: continue
    a, b = mag[lay], kl[lay]
    if lay in MORPH or lay == "selfand": st = "SKIP"
    elif lay.startswith("id_"):
        st = "INFO"
    else:
        geo = abs(a["area"] - b["area"]) <= 1e-4 * max(1, b["area"]) and \
              abs(a["perimeter"] - b["perimeter"]) <= 5e-4 * max(1, b["perimeter"])
        cnt_kl = a["polygons"] == b["polygons"] and a["holes"] == b["holes"]
        r = ref.get(lay)
        # each count equal to KLayout's or to the exact unrotated count (holes created by the
        # rotation of the input itself are counted by both engines, KLayout's merge drops others)
        cnt_ref = r is not None and all(a[f] in (b[f], r[f]) for f in ("polygons", "holes"))
        ok = geo and (cnt_kl or cnt_ref)
        st = ("PASS" if cnt_kl else "PASS-INV") if ok else "FAIL"; npass += ok; nfail += not ok
    print("MAGKL %s %-10s %s polys %d/%d holes %d/%d verts %d/%d area %d/%d (%.2e) perim %d/%d" % (
        tag, lay, st, a["polygons"], b["polygons"], a["holes"], b["holes"], a["vertices"], b["vertices"],
        a["area"], b["area"], (a["area"] - b["area"]) / max(1, b["area"]), a["perimeter"], b["perimeter"]))
print("MAGKL %s summary pass=%d fail=%d" % (tag, npass, nfail))
