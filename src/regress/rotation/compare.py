#!/usr/bin/env python3
"""Summarise a rotation-invariance run (run_rotation.sh).

    compare.py OUT_DIR/TAG [--detail]

Checks, per angle a (R000 = unrotated baseline):
  flatten   db_rotcheck --mode poly: exact flattened polygons == baseline rotated (all angles)
  boxpath   db_rotcheck --mode box: exact at multiples of 90; otherwise the area ratio of the
            rectangle-strip approximation is reported
  mag@0     MAGPARSER `exec measure` on R000 == KLayout on R000 (all five quantities, exact)
  mag(a)    MAGPARSER on R<a> vs MAGPARSER on R000
  kl(a)     KLayout on R<a> vs KLayout on R000 (what the grid rounding itself allows)

Invariance criterion for mag(a)/kl(a):
  multiples of 90 degrees: every quantity identical
  other angles (vertices snap to the grid, <= sqrt(2)/2 dbu each):
    polygons, holes, vertices identical; |dArea| <= 0.7072 * perimeter0;
    |dPerimeter| <= 1.5 * vertices0
  MAGPARSER failures at other angles are XFAIL: its layers are boxes, so rotated input is
  approximated by rectangle strips until the any-angle loader (phase 4) exists.
Exit status 1 when a check that must pass fails (flatten, boxpath at 90s, mag@0, mag at 90s).
"""
import os, re, sys

KEYS = ("polygons", "holes", "vertices", "area", "perimeter")


def parse_kv(line):
    return dict(re.findall(r"(\w+)=(\S+)", line))


def norm_layer(name):
    name = name.split(":", 1)[1] if ":" in name else name        # drop db prefix
    m = re.fullmatch(r"(\d+):(\d+)", name)
    return "%s/%s" % m.groups() if m else name


def read_measures(path, tag):
    out = {}
    if not os.path.exists(path):
        return None
    for line in open(path, errors="replace"):
        if line.startswith(tag + " "):
            kv = parse_kv(line)
            if not all(k in kv and re.fullmatch(r"-?\d+", kv[k]) for k in KEYS):
                continue                       # line cut by interleaved stderr
            out[norm_layer(kv["layer"])] = {k: int(kv[k]) for k in KEYS}
    return out


def read_rotcheck(path):
    if not os.path.exists(path):
        return None
    for line in open(path, errors="replace"):
        if line.startswith("ROTCHECK"):
            kv = parse_kv(line)
            kv["status"] = line.split()[1]
            return kv
    return {"status": "NORUN"}


def invariant(base, cur, ortho):
    """(ok, worst description) of cur against base for one layer."""
    if ortho:
        bad = [k for k in KEYS if base[k] != cur[k]]
        return (not bad, ",".join("%s %+d" % (k, cur[k] - base[k]) for k in bad))
    bad = [k for k in ("polygons", "holes", "vertices") if base[k] != cur[k]]
    da, dp = cur["area"] - base["area"], cur["perimeter"] - base["perimeter"]
    if abs(da) > 0.7072 * base["perimeter"]:
        bad.append("area")
    if abs(dp) > 1.5 * base["vertices"]:
        bad.append("perimeter")
    desc = ",".join("%s %+d" % (k, cur[k] - base[k]) for k in bad if k in ("polygons", "holes", "vertices"))
    if "area" in bad:
        desc += (",area %+.2e(%.1f%%)" % (da, 100.0 * da / base["area"] if base["area"] else 0))
    if "perimeter" in bad:
        desc += (",perim %+.2e" % dp)
    return (not bad, desc.strip(","))


def main():
    root = sys.argv[1].rstrip("/")
    detail = "--detail" in sys.argv
    tag = os.path.basename(root)
    angles = sorted(int(d[1:]) for d in os.listdir(root) if re.fullmatch(r"R\d{3}", d))
    get = lambda a, f: os.path.join(root, "R%03d" % a, f)
    mag0, kl0 = read_measures(get(0, "mag.log"), "MEASURE"), read_measures(get(0, "kl.log"), "KLMEASURE")
    hard_fail = 0
    print("# Rotation invariance: %s" % tag)
    if mag0 and kl0:
        common = sorted(set(mag0) & set(kl0))
        bad = [l for l in common if mag0[l] != kl0[l]]
        print("mag@0 vs KLayout@0: %d/%d layers identical%s" % (len(common) - len(bad), len(common),
              "" if not bad else "  MISMATCH: " + "; ".join(
                  "%s mag=%s kl=%s" % (l, [mag0[l][k] for k in KEYS], [kl0[l][k] for k in KEYS]) for l in bad)))
        hard_fail += len(bad)
    print()
    print("%5s | %-8s | %-22s | %-26s | %-26s" % ("angle", "flatten", "box path", "MAGPARSER invariance", "KLayout invariance"))
    print("-" * 120)
    details = []
    for a in angles:
        ortho = a % 90 == 0
        rp, rb = read_rotcheck(get(a, "rot_poly.log")), read_rotcheck(get(a, "rot_box.log"))
        flat = rp["status"] if rp else "-"
        if rp and rp["status"] != "PASS":
            hard_fail += 1
        if not rb:
            box = "-"
        elif ortho:
            box = rb["status"]
            hard_fail += rb["status"] != "PASS"
        else:
            box = "approx area x%s" % rb.get("area_ratio", "?")[:8]
        cols = []
        for which, base, tagname in (("mag", mag0, "MEASURE"), ("kl", kl0, "KLMEASURE")):
            cur = read_measures(get(a, which + ".log"), tagname)
            if not base or cur is None:
                cols.append("-")
                continue
            layers = sorted(set(base) & set(cur))
            res = [(l,) + invariant(base[l], cur[l], ortho) for l in layers]
            nok = sum(1 for r in res if r[1])
            status = "PASS" if nok == len(res) else ("FAIL" if ortho or which == "kl" else "XFAIL")
            if which == "mag" and ortho and nok != len(res):
                hard_fail += 1
            cols.append("%s %d/%d" % (status, nok, len(res)))
            for l, ok, desc in res:
                if not ok:
                    details.append("R%03d %-3s %-8s %s" % (a, which, l, desc))
        print("%5d | %-8s | %-22s | %-26s | %-26s" % (a, flat, box, cols[0], cols[1]))
    if detail and details:
        print("\nPer-layer deviations (layer: quantity delta):")
        for d in details:
            print("  " + d)
    print("\nRESULT %s hard_failures=%d" % ("PASS" if hard_fail == 0 else "FAIL", hard_fail))
    return 1 if hard_fail else 0


if __name__ == "__main__":
    sys.exit(main())
