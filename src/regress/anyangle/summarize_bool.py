#!/usr/bin/env python3
"""Summary of run_bool.sh:  summarize_bool.py out/bool_TAG

Per layer pair, angle and op (TRAPBOOL.exe = any-angle two-layer Boolean; KLayout = reference):
  anom   internal checks (sweep + exact output), must be 0
  ident  the exact area identities among the ops (XOR = OR - AND, OR + AND = A + B, ...) hold
  exact  our output read back by KLayout has exactly TRAPBOOL's 2A (raw sum, interior-disjoint)
  xor    area((KLayout A op B) XOR ours); thin: XOR shrunk by 1 dbu is empty (snap rounding only)
  dA     our 2A minus our 2A at R000 (rotation invariance; 0 at multiples of 90)
Pass: anom 0, ident and exact everywhere; at multiples of 90 also xor 0 and 2A == KLayout's;
otherwise thin.  Exit 1 on any failure.
"""
import os, re, sys

def kv(line):
    return dict(re.findall(r"(\w+)=(\S+)", line))

def main():
    root = sys.argv[1]
    bad = 0
    for pair in sorted(os.listdir(root)):
        pd = os.path.join(root, pair)
        if not os.path.isdir(pd):
            continue
        print("== %s" % pair)
        print("%-5s %-6s %5s %5s %5s %14s %8s %5s %16s %9s %8s %8s" %
              ("angle", "op", "anom", "ident", "exact", "xor", "xor_max", "thin", "area2", "dA", "t_bool", "t_kl"))
        base = {}
        for ang in sorted(os.listdir(pd)):
            d = os.path.join(pd, ang)
            a = int(ang[1:])
            b, kl, tb, tk, ident_ok = {}, {}, "-", "-", True
            if os.path.exists(os.path.join(d, "bool.log")):
                for line in open(os.path.join(d, "bool.log"), errors="replace"):
                    if line.startswith("TRAPBOOL op="):
                        x = kv(line); b[x["op"]] = x
                    elif "identity" in line and "FAIL" in line:
                        ident_ok = False
                    elif line.startswith("TIME"):
                        tb = line.split()[1]
            if os.path.exists(os.path.join(d, "kl.log")):
                for line in open(os.path.join(d, "kl.log"), errors="replace"):
                    if line.startswith("KLBOOL op="):
                        x = kv(line); kl[x["op"]] = x
                    elif line.startswith("TIME"):
                        tk = line.split()[1]
            for op, x in b.items():
                k = kl.get(op, {})
                anom = int(x.get("anomalies", 1)) + int(x.get("out_anomalies", 1))
                a2 = int(x["area2"])
                exact = k and int(k["out_area2"]) == a2
                xa = int(k["xor_area"]) if k else -1
                thin = k and int(k["xor_wide1"]) == 0
                if a == 0:
                    base[op] = a2
                dA = a2 - base[op] if op in base else 0
                ok = anom == 0 and ident_ok and exact
                if a % 90 == 0:
                    ok = ok and xa == 0 and k and int(k["kl_area2"]) == a2
                else:
                    ok = ok and thin
                bad += 0 if ok else 1
                print("%-5d %-6s %5d %5s %5s %14d %8s %5s %16d %9d %8s %8s %s" %
                      (a, op, anom, "ok" if ident_ok else "FAIL", "ok" if exact else "FAIL", xa,
                       k.get("xor_maxdim", "-"), "ok" if thin else "no", a2, dA, tb, tk, "" if ok else "<-- FAIL"))
    print("RESULT: %s" % ("PASS" if bad == 0 else "FAIL (%d)" % bad))
    sys.exit(1 if bad else 0)

main()
