#!/usr/bin/env python3
"""Summarise gold logs: worst boundary deviation (Linf) and area error, ours vs KLayout."""
import glob, re, sys
rows = []
for f in sys.argv[1:]:
    for l in open(f):
        m = re.search(r'op=(\w+)\s+exact_area=([\d.e+-]+).*ours: area=([\d.e+-]+) dA=([\d.e+-]+) H2=[\d.]+ Hinf=([\d.]+).*kl: area=([\d.e+-]+) dA=([\d.e+-]+) H2=[\d.]+ Hinf=([\d.]+)', l)
        if m: rows.append((f, m.group(1), float(m.group(5)), float(m.group(8)), float(m.group(4)), float(m.group(7))))
n = len(rows)
ko = [r for r in rows if r[2] > 0.5 + 1e-9]; kk = [r for r in rows if r[3] > 0.5 + 1e-9]
print("GOLDSUM op_cases=%d  ours: max_Hinf=%.4f over_half=%d  KLayout: max_Hinf=%.4f over_half=%d" %
      (n, max(r[2] for r in rows), len(ko), max(r[3] for r in rows), len(kk)))
print("GOLDSUM area error sum: ours=%.3f KLayout=%.3f  cases ours closer=%d KLayout closer=%d equal=%d" %
      (sum(r[4] for r in rows), sum(r[5] for r in rows), sum(r[4] < r[5] - 1e-9 for r in rows),
       sum(r[5] < r[4] - 1e-9 for r in rows), sum(abs(r[4] - r[5]) <= 1e-9 for r in rows)))
for r in sorted(kk, key=lambda r: -r[3])[:6]:
    print("GOLDSUM KLayout over 1/2: %s %s KL_Hinf=%.4f ours_Hinf=%.4f" % (r[0], r[1], r[3], r[2]))
for r in ko: print("GOLDSUM OURS OVER 1/2: %s %s %.4f" % (r[0], r[1], r[2]))
