#!/usr/bin/env python3
"""Exact rational gold check of two-layer Booleans on a small clip.

E = A op B computed exactly (Fractions, no rounding): a slab sweep over all edges of A, B and
the candidate results, cut at every vertex row and every pairwise edge crossing (exact rational
y). Inside a slab no edges cross, so membership is constant between consecutive edges and every
area is an exact rational. A, B: raw input shapes (union = covered by any shape). Candidates
C (ours = TRAPBOOL's disjoint pieces, kl = KLayout's merged polygons with holes): membership by
crossing parity of all their rings (pieces are interior-disjoint).

Per op and candidate:  dA = area(C xor E) (exact), H = boundary deviation: max over sampled
points of C xor E of the distance to the exact boundary dE (L2 and Linf). Snap rounding moves
every edge inside the Linf 1/2-pixel tube of the original edge, so a single rounding gives
H_inf <= 1/2 (slivers thinner than one pixel may vanish, they lie within 1/2 of dE); every
further rounding of computed vertices can add another 1/2.
Assertions (self-check mode, --assert): our exact area == TRAPBOOL's area2/2 when given,
exact identities on our results (OR + AND = A' + B' etc. hold on the snapped operands, so here:
area(OR)+area(AND) == area(NOT)+area(BNOTA)+2*area(AND)), and our H_inf <= bound (default 0.5 +
1e-9 dbu = half a pixel, the snap-rounding tube).
Usage: gold_check.py clip.json [--area2 OP=V ...] [--assert] [--bound 0.5]
"""
import json, sys, math
from fractions import Fraction as F

def edges_of(rings, tag, pid0=0):
    E = []
    for k, r in enumerate(rings):
        n = len(r)
        for i in range(n):
            (x0, y0), (x1, y1) = r[i], r[(i + 1) % n]
            if y0 == y1: continue
            if y0 > y1: x0, y0, x1, y1 = x1, y1, x0, y0
            E.append((F(x0), F(y0), F(x1), F(y1), tag, pid0 + k))
    return E

def xat(e, y):
    x0, y0, x1, y1 = e[0], e[1], e[2], e[3]
    return x0 + (x1 - x0) * (y - y0) / (y1 - y0)

def cross_y(e, f):
    # y where e and f (as lines) cross, if inside both y ranges
    ax, ay, bx, by = e[0], e[1], e[2], e[3]; cx, cy, dx, dy = f[0], f[1], f[2], f[3]
    lo, hi = max(ay, cy), min(by, dy)
    if lo >= hi: return None
    d0 = xat(e, lo) - xat(f, lo); d1 = xat(e, hi) - xat(f, hi)
    if d0 == 0 or d1 == 0 or (d0 > 0) == (d1 > 0): return None
    return lo + (hi - lo) * d0 / (d0 - d1)

def slabs(edges):
    ys = set()
    for e in edges: ys.add(e[1]); ys.add(e[3])
    n = len(edges)
    for i in range(n):
        for j in range(i + 1, n):
            y = cross_y(edges[i], edges[j])
            if y is not None: ys.add(y)
    return sorted(ys)

OPS = {"OR": lambda a, b: a or b, "AND": lambda a, b: a and b, "NOT": lambda a, b: a and not b,
       "XOR": lambda a, b: a != b, "BNOTA": lambda a, b: b and not a}

def sweep(edges, ys, member):
    """Yield (y0, y1, [(xl0, xl1, xr0, xr1, flags)]) intervals between consecutive edges; flags =
    member(state) for the state between them."""
    for y0, y1 in zip(ys, ys[1:]):
        ym = (y0 + y1) / 2
        act = [e for e in edges if e[1] <= y0 and e[3] >= y1]
        act.sort(key=lambda e: xat(e, ym))
        state = {}
        out = []
        for i, e in enumerate(act[:-1]):
            key = (e[4], e[5]); state[key] = state.get(key, 0) ^ 1
            f = act[i + 1]
            out.append((xat(e, y0), xat(e, y1), xat(f, y0), xat(f, y1), member(state)))
        yield y0, y1, out

def area_of(pieces):
    s = F(0)
    for y0, y1, xl0, xl1, xr0, xr1 in pieces:
        s += ((xr0 - xl0) + (xr1 - xl1)) * (y1 - y0) / 2
    return s

def seg_dist(px, py, ax, ay, bx, by, inf):
    vx, vy = bx - ax, by - ay
    if not inf:
        L = vx * vx + vy * vy
        t = 0 if L == 0 else max(0.0, min(1.0, ((px - ax) * vx + (py - ay) * vy) / L))
        return math.hypot(ax + vx * t - px, ay + vy * t - py)
    # Linf: f(t) = max(|ux + vx t|, |uy + vy t|) is convex piecewise linear; its minimum is at
    # t = 0, 1, a zero of either term or where the terms are equal in magnitude.
    ux, uy = ax - px, ay - py
    cand = [0.0, 1.0]
    if vx: cand.append(-ux / vx)
    if vy: cand.append(-uy / vy)
    if vx != vy: cand.append((uy - ux) / (vx - vy))
    if vx != -vy: cand.append(-(ux + uy) / (vx + vy))
    return min(max(abs(ux + vx * t), abs(uy + vy * t)) for t in (max(0.0, min(1.0, c)) for c in cand))

def dist_point_trap(px, py, t, inf):
    y0, y1, xl0, xl1, xr0, xr1 = t
    if y0 <= py <= y1:
        a = (py - y0) / (y1 - y0) if y1 > y0 else 0
        if xl0 + (xl1 - xl0) * a <= px <= xr0 + (xr1 - xr0) * a: return 0.0
    pts = [(xl0, y0), (xr0, y0), (xr1, y1), (xl1, y1)]
    return min(seg_dist(px, py, *pts[i], *pts[(i + 1) % 4], inf) for i in range(4))

def nearest(px, py, others, inf):
    best = float('inf')
    for (bx1, by1, bx2, by2, t) in others:
        dx = max(bx1 - px, 0.0, px - bx2); dy = max(by1 - py, 0.0, py - by2)
        lb = max(dx, dy) if inf else math.hypot(dx, dy)
        if lb >= best: continue
        best = min(best, dist_point_trap(px, py, t, inf))
        if best == 0: break
    return best

def boxed(traps):
    out = []
    for t in traps:
        f = tuple(float(v) for v in t)
        y0, y1, xl0, xl1, xr0, xr1 = f
        out.append((min(xl0, xl1), y0, max(xr0, xr1), y1, f))
    return out

def samples(t, k=4):
    y0, y1, xl0, xl1, xr0, xr1 = [float(v) for v in t]
    for i in range(k + 1):
        a = i / k; y = y0 + (y1 - y0) * a
        xl = xl0 + (xl1 - xl0) * a; xr = xr0 + (xr1 - xr0) * a
        for j in range(k + 1):
            yield xl + (xr - xl) * j / k, y

def main():
    js = json.load(open(sys.argv[1]))
    args = sys.argv[2:]
    do_assert = "--assert" in args
    bound = float(args[args.index("--bound") + 1]) if "--bound" in args else 0.5
    area2 = {}
    if "--area2" in args:
        for kv in args[args.index("--area2") + 1:]:
            if kv.startswith("--"): break
            k, v = kv.split("="); area2[k] = int(v)
    A, B = js["A"], js["B"]
    failures = []
    exact_area = {}; ours_area = {}
    for op in js["ours"]:
        res = {}
        for cand in ("ours", "kl"):
            C = js[cand][op]
            xs = [q[0] for r in A + B + C for q in r]; ys_ = [q[1] for r in A + B + C for q in r]
            frame = [[[min(xs) - 4, min(ys_) - 4], [max(xs) + 4, min(ys_) - 4], [max(xs) + 4, max(ys_) + 4], [min(xs) - 4, max(ys_) + 4]]]
            edges = edges_of(A, "A") + edges_of(B, "B") + edges_of(C, "C") + edges_of(frame, "F")
            ys = slabs(edges)
            def member(st):
                a = any(v for (t, _), v in st.items() if t == "A")
                b = any(v for (t, _), v in st.items() if t == "B")
                c = sum(v for (t, _), v in st.items() if t == "C") % 2 == 1
                return OPS[op](a, b), c
            Ep, Cp, Dp, Np = [], [], [], []
            for y0, y1, ivs in sweep(edges, ys, member):
                for xl0, xl1, xr0, xr1, (e, c) in ivs:
                    if xl0 == xr0 and xl1 == xr1: continue   # zero width: coincident edges (hole cut lines)
                    t = (y0, y1, xl0, xl1, xr0, xr1)
                    if e: Ep.append(t)
                    else: Np.append(t)
                    if c: Cp.append(t)
                    if e != c: Dp.append((t, e))
            aE, aC, aD = area_of(Ep), area_of(Cp), area_of([t for t, _ in Dp])
            # Hausdorff over the difference: points of E\C to C, points of C\E to E
            h2 = hinf = 0.0
            # deviation of a point of C xor E = its distance to the exact boundary dE:
            # points of E\C measured to the complement of E, points of C\E to E
            bN, bE = boxed(Np), boxed(Ep)
            for t, in_e in Dp:
                if t[0] == t[1]: continue
                other = bN if in_e else bE
                for px, py in samples(t):
                    h2 = max(h2, nearest(px, py, other, False))
                    hinf = max(hinf, nearest(px, py, other, True))
            res[cand] = (aE, aC, aD, h2, hinf)
        aE = res["ours"][0]; exact_area[op] = aE; ours_area[op] = res["ours"][1]
        assert res["kl"][0] == aE   # same exact E either way
        print("GOLD op=%-5s exact_area=%s | ours: area=%s dA=%.6g H2=%.4f Hinf=%.4f | "
              "kl: area=%s dA=%.6g H2=%.4f Hinf=%.4f" % (
                  op, float(aE), float(res["ours"][1]), float(res["ours"][2]), res["ours"][3], res["ours"][4],
                  float(res["kl"][1]), float(res["kl"][2]), res["kl"][3], res["kl"][4]))
        if op in area2 and res["ours"][1] * 2 != area2[op]:
            failures.append("%s: exact area of our pieces %s != TRAPBOOL area2/2 %s" % (op, res["ours"][1], F(area2[op], 2)))
        if res["ours"][4] > bound + 1e-9:
            failures.append("%s: our Hausdorff Linf %.6f > bound %.3f" % (op, res["ours"][4], bound))
    o = ours_area
    if all(k in o for k in ("OR", "AND", "NOT", "BNOTA", "XOR")):
        if o["OR"] != o["NOT"] + o["AND"] + o["BNOTA"]: failures.append("identity OR = NOT + AND + BNOTA")
        if o["XOR"] != o["OR"] - o["AND"]: failures.append("identity XOR = OR - AND")
    for f in failures: print("GOLD FAIL", f)
    print("GOLD %s" % ("PASS" if not failures else "FAIL"))
    if do_assert and failures: sys.exit(1)

if __name__ == "__main__":
    main()
