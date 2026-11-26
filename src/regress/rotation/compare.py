#!/usr/bin/env python3
"""Summarise a rotation-invariance run (run_rotation.sh).

    compare.py OUT_DIR/TAG [--detail]

Per angle a (R000 = unrotated baseline), from rot_poly.log, rot_box.log, mag.log, kl.log:
  flatten  db_rotcheck --mode poly: flattened polygons == baseline rotated (every angle, exact)
  box      db_rotcheck --mode box (MAGPARSER's loader iterator): exact at multiples of 90;
           otherwise rectangle strips, area ratio shown
  inv      invariance: measurements of every input/result layer on R<a> vs on R000
  eqv      equivalence: MAGPARSER vs KLayout on the same file R<a>, same operations
  ids      identity layers (id_*) are empty, and area(A+B) + area(A*B) = area(A) + area(B)

Criteria. Multiples of 90 degrees: all quantities identical, identity areas 0.
Other angles (vertices snap to the grid, <= sqrt(2)/2 dbu each): polygons, holes, vertices
identical; |dArea| <= 0.7072 * perimeter; |dPerimeter| <= 1.5 * vertices; identity
areas <= 0.7072 * perimeter of the inputs involved.
MAGPARSER failures at non-orthogonal angles are XFAIL until it has an any-angle engine
(phase 3/4); its layers are boxes, so a rotated design reaches it as rectangle strips.
KLayout columns are the reference: they show what exact any-angle processing achieves.
Exit status 1 on a hard failure: flatten at any angle, or box/MAGPARSER at multiples of 90.
"""
import os, re, sys

KEYS = ("polygons", "holes", "vertices", "area", "perimeter")
BOOL_IDS = ("id_xorself", "id_sym", "id_xornamed", "id_and2", "id_framenot", "id_notat", "id_nottilde", "id_notand")
MORPH_IDS = ("id_grow", "id_shrink", "id_close", "id_open", "id_sizep", "id_sizen")


def parse_kv(line):
    return dict(re.findall(r"(\w+)=(\S+)", line))


def norm_layer(name):
    name = name.split(":", 1)[1] if ":" in name else name
    m = re.fullmatch(r"(\d+):(\d+)", name)
    return "%s/%s" % m.groups() if m else name


def read_measures(path, tag):
    if not os.path.exists(path):
        return None
    out = {}
    for line in open(path, errors="replace"):
        if line.startswith(tag + " "):
            kv = parse_kv(line)
            if not all(k in kv and re.fullmatch(r"-?\d+", kv[k]) for k in KEYS):
                continue                       # line cut by interleaved stderr
            out[norm_layer(kv["layer"])] = {k: int(kv[k]) for k in KEYS}
    return out or None


def read_rotcheck(path):
    if not os.path.exists(path):
        return None
    for line in open(path, errors="replace"):
        if line.startswith("ROTCHECK"):
            kv = parse_kv(line)
            kv["status"] = line.split()[1]
            return kv
    return {"status": "NORUN"}


def close_enough(base, cur, ortho):
    """'' when cur matches base under the criterion, else a description of the deviation."""
    if ortho:
        return ",".join("%s %+d" % (k, cur[k] - base[k]) for k in KEYS if base[k] != cur[k])
    bad = ["%s %+d" % (k, cur[k] - base[k]) for k in ("polygons", "holes", "vertices") if base[k] != cur[k]]
    da, dp = cur["area"] - base["area"], cur["perimeter"] - base["perimeter"]
    if abs(da) > 0.7072 * max(base["perimeter"], cur["perimeter"]):
        bad.append("area %+.3g (%.3f%%)" % (da, 100.0 * da / base["area"] if base["area"] else float("inf")))
    if abs(dp) > 1.5 * max(base["vertices"], cur["vertices"]):
        bad.append("perim %+.3g" % dp)
    return ",".join(bad)


def compare_sets(base, cur, ortho, skip_ids=True):
    layers = sorted(l for l in set(base) & set(cur) if not (skip_ids and l.startswith("id_")))
    res = [(l, close_enough(base[l], cur[l], ortho)) for l in layers]
    return sum(1 for _, d in res if not d), len(res), [(l, d) for l, d in res if d]


def identities(m, ortho):
    """(n_ok, n, failures) for the identity layers of one measurement set."""
    if not m:
        return 0, 0, []
    per = lambda *ls: sum(m[l]["perimeter"] for l in ls if l in m)
    checks = []
    for name in BOOL_IDS + MORPH_IDS:
        if name not in m:
            continue
        ref = per("65/20", "66/20") if name in BOOL_IDS else per("68/20")
        tol = 0 if ortho else 0.7072 * ref
        checks.append((name, m[name]["area"] <= tol, "area %d (tol %.0f)" % (m[name]["area"], tol)))
    if all(l in m for l in ("or_ab", "and", "65/20", "66/20")):
        d = m["or_ab"]["area"] + m["and"]["area"] - m["65/20"]["area"] - m["66/20"]["area"]
        tol = 0 if ortho else 0.7072 * per("65/20", "66/20")
        checks.append(("incl_excl", abs(d) <= tol, "A+B + A*B - A - B = %d (tol %.0f)" % (d, tol)))
    fails = [(n, d) for n, ok, d in checks if not ok]
    return len(checks) - len(fails), len(checks), fails


def status(nok, n, ortho, xfail_ok):
    if n == 0:
        return "-"
    if nok == n:
        return "PASS %d/%d" % (nok, n)
    return ("XFAIL" if (xfail_ok and not ortho) else "FAIL") + " %d/%d" % (nok, n)


def main():
    root = sys.argv[1].rstrip("/")
    detail = "--detail" in sys.argv
    tag = os.path.basename(root)
    angles = sorted(int(d[1:]) for d in os.listdir(root) if re.fullmatch(r"R\d{3}", d))
    get = lambda a, f: os.path.join(root, "R%03d" % a, f)
    mag0, kl0 = read_measures(get(0, "mag.log"), "MEASURE"), read_measures(get(0, "kl.log"), "KLMEASURE")
    hard, details = 0, []
    print("# Rotation invariance: %s" % tag)
    hdr = "%5s | %-7s | %-16s | %-11s | %-11s | %-11s | %-11s | %-11s" % (
        "angle", "flatten", "box path", "mag inv", "kl inv", "mag=kl", "mag ids", "kl ids")
    print(hdr)
    print("-" * len(hdr))
    for a in angles:
        ortho = a % 90 == 0
        rp, rb = read_rotcheck(get(a, "rot_poly.log")), read_rotcheck(get(a, "rot_box.log"))
        flat = rp["status"] if rp else "-"
        hard += bool(rp and rp["status"] != "PASS")
        if not rb:
            box = "-"
        elif ortho:
            box = rb["status"]
            hard += rb["status"] != "PASS"
        else:
            box = "strips x%.5f" % float(rb.get("area_ratio", "nan"))
        mag, kl = read_measures(get(a, "mag.log"), "MEASURE"), read_measures(get(a, "kl.log"), "KLMEASURE")
        cols = []
        for who, base, cur, xf in (("mag inv", mag0, mag, True), ("kl inv", kl0, kl, False), ("mag=kl", kl, mag, True)):
            if not base or not cur:
                cols.append("-")
                continue
            nok, n, f = compare_sets(base, cur, ortho)
            cols.append(status(nok, n, ortho, xf))
            details += ["R%03d %-7s %-10s %s" % (a, who, l, d) for l, d in f]
            hard += bool(who != "kl inv" and ortho and nok != n)
        for who, m, xf in (("mag ids", mag, True), ("kl ids", kl, False)):
            nok, n, f = identities(m, ortho)
            cols.append(status(nok, n, ortho, xf))
            details += ["R%03d %-7s %-10s %s" % (a, who, l, d) for l, d in f]
            hard += bool(who == "mag ids" and ortho and nok != n)
        print("%5d | %-7s | %-16s | %-11s | %-11s | %-11s | %-11s | %-11s" % ((a, flat, box) + tuple(cols)))
    if detail and details:
        print("\nDeviations (angle check layer: what differs):")
        for d in details:
            print("  " + d)
    print("\nRESULT %s hard_failures=%d" % ("PASS" if hard == 0 else "FAIL", hard))
    return 1 if hard else 0


if __name__ == "__main__":
    sys.exit(main())
