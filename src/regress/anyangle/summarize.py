#!/usr/bin/env python3
"""Summary of run_merge.sh:  summarize.py out/TAG [--perf]

Per angle and layer (TRAPMERGE.exe = any-angle single-layer OR; KLayout = reference):
  anom   internal checks of the merge and of the exact output (must be 0)
  exact  the output OASIS, read back by KLayout, has exactly the merged area (raw sum of shape
         2A identical; the shapes are interior-disjoint)
  kmd    KLayout's merge of our output minus its raw 2A: KLayout snaps vertices lying within a
         pixel of another edge, so near-degenerate slivers move by a few dbu^2 (0 at 90 multiples)
  xor    area of (KLayout merge of the input) XOR (our output)
  thin   every XOR difference is thinner than 2 dbu (XOR shrunk by 1 dbu is empty)
  dA     our 2A minus our 2A at R000 (rotation invariance; exact 0 at multiples of 90)
  dA_kl  the same for KLayout's merge
Pass: anom 0 and exact at every angle; at multiples of 90 also xor 0, kmd 0, dA 0 and 2A ==
KLayout's; otherwise thin and |kmd| <= 1e-8 * 2A.  Exit 1 on any failure.
"""
import os, re, sys

def kv(line):
    return dict(re.findall(r"(\w+)=(\S+)", line))

def read_merge(path):
    out, tot = {}, {}
    if not os.path.exists(path):
        return out, tot
    for line in open(path, errors="replace"):
        if line.startswith("TRAPMERGE layer="):
            d = kv(line)
            out.setdefault(d["layer"], {}).update(d)
        elif line.startswith("TRAPMERGE file="):
            tot.update(kv(line))
        elif line.startswith("TIME"):
            f = line.split()
            tot["wall"], tot["rss_kb"] = f[1], f[4]
        elif line.startswith("TRAPMERGE out:"):
            tot.update(kv(line))
    return out, tot

def read_kl(path):
    out = {}
    if os.path.exists(path):
        for line in open(path, errors="replace"):
            if line.startswith("KLMERGE"):
                d = kv(line); out[d["layer"]] = d
    return out

def main():
    root = sys.argv[1].rstrip("/")
    perf = "--perf" in sys.argv
    angles = sorted(int(d[1:]) for d in os.listdir(root) if re.fullmatch(r"R\d{3}", d))
    m0, _ = read_merge(os.path.join(root, "R000", "merge.log"))
    k0 = read_kl(os.path.join(root, "R000", "kl.log"))
    fails = 0
    print("# Any-angle single-layer OR: %s" % os.path.basename(root))
    hdr = "%5s %-6s %9s %9s %5s %5s %6s %12s %4s %12s %12s %7s %7s %6s" % (
        "angle", "layer", "traps", "out_poly", "anom", "exact", "kmd", "xor", "thin", "dA", "dA_kl", "merge_s", "kl_s", "result")
    print(hdr); print("-" * len(hdr))
    for a in angles:
        m, tot = read_merge(os.path.join(root, "R%03d" % a, "merge.log"))
        k = read_kl(os.path.join(root, "R%03d" % a, "kl.log"))
        ortho = a % 90 == 0
        for layer in sorted(m):
            d = m[layer]; kk = k.get(layer)
            anom = int(d.get("anomalies", -1)) + int(d.get("out_anomalies", 0))
            if d.get("area_integral") != "T":
                anom += 1
            a2 = int(d.get("area2", 0))
            dA = a2 - int(m0[layer]["area2"]) if layer in m0 else None
            res = []
            if anom != 0:
                res.append("anom")
            exact = xor = thin = dAk = kmd = "-"
            if kk:
                exact = "yes" if int(kk["out_area2"]) == a2 else "NO"
                if exact != "yes": res.append("exact")
                kmd_v = int(kk.get("out_merged_area2", kk["out_area2"])) - int(kk["out_area2"])
                kmd = str(kmd_v)
                if (ortho and kmd_v != 0) or abs(kmd_v) > 1e-8 * max(a2, 1): res.append("kmd")
                xor = kk["xor_area"]; thin = "yes" if int(kk["xor_wide1"]) == 0 else "NO"
                if layer in k0: dAk = str(int(kk["kl_area2"]) - int(k0[layer]["kl_area2"]))
                if ortho:
                    if int(xor) != 0 or int(kk["kl_area2"]) != a2: res.append("xor")
                elif thin != "yes":
                    res.append("thin")
            if ortho and dA not in (None, 0):
                res.append("dA")
            fails += bool(res)
            print("%5d %-6s %9s %9s %5d %5s %6s %12s %4s %12s %12s %7s %7s %6s" % (
                a, layer, d.get("traps"), d.get("out_polygons", "-"), anom, exact, kmd, xor, thin,
                "-" if dA is None else dA, dAk, d.get("t_total"), kk.get("kl_merge_s", "-") if kk else "-",
                "PASS" if not res else "FAIL:" + ",".join(res)))
    if perf:
        print("\n# Merge time breakdown (s) per layer")
        cols = ("t_edges", "t_grid", "t_cross", "t_hot", "t_hits", "t_frag", "t_sweep", "t_stitch", "t_total", "t_write")
        print("%5s %-6s %9s %9s %9s " % ("angle", "layer", "polygons", "edges", "inexact") + " ".join("%8s" % c[2:] for c in cols))
        for a in angles:
            m, tot = read_merge(os.path.join(root, "R%03d" % a, "merge.log"))
            for layer in sorted(m):
                d = m[layer]
                print("%5d %-6s %9s %9s %9s " % (a, layer, d.get("polygons"), d.get("edges"), d.get("inexact")) +
                      " ".join("%8s" % d.get(c, "-") for c in cols))
            print("%5d  load_s=%s wall=%s maxrss_kb=%s threads=%s" % (a, tot.get("load_s"), tot.get("wall"), tot.get("rss_kb"), tot.get("threads")))
    print("\nRESULT %s failures=%d" % ("PASS" if fails == 0 else "FAIL", fails))
    return 1 if fails else 0

if __name__ == "__main__":
    sys.exit(main())
