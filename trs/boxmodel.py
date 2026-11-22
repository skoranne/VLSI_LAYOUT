#!/usr/bin/env python3
"""Engine-faithful box model of MAGPARSER topological selectors (design_topo_impl.f90).

Boxes are closed, axis-aligned, integer, nondegenerate (x1<x2, y1<y2).
A 'cell' (cx,cy) is the unit square [cx,cx+1]x[cy,cy+1]; with integer coordinates,
regions are equal up to measure zero iff they contain the same cells.
"""
from itertools import product

# Pairwise classes of box a w.r.t. box b (DISJ excluded from types: it sets no flag)
CLASSES = ['PT', 'ET', 'PO', 'EQ', 'INS', 'INB', 'CONS', 'CONB']
PT, ET, PO, EQ, INS, INB, CONS, CONB = range(8)
DISJ = -1
OV = {PO, EQ, INS, INB, CONS, CONB}          # positive-area overlap

def cls(a, b):
    ax1, ay1, ax2, ay2 = a; bx1, by1, bx2, by2 = b
    if ax1 > bx2 or ax2 < bx1 or ay1 > by2 or ay2 < by1:
        return DISJ
    bca = ax1 >= bx1 and ay1 >= by1 and ax2 <= bx2 and ay2 <= by2
    acb = bx1 >= ax1 and by1 >= ay1 and bx2 <= ax2 and by2 <= ay2
    share = ax1 == bx1 or ax2 == bx2 or ay1 == by1 or ay2 == by2
    ix1, iy1, ix2, iy2 = max(ax1, bx1), max(ay1, by1), min(ax2, bx2), min(ay2, by2)
    if a == b: return EQ
    if bca: return INB if share else INS
    if acb: return CONB if share else CONS
    if ix1 < ix2 and iy1 < iy2: return PO
    if (ix1 < ix2 and iy1 == iy2) or (ix1 == ix2 and iy1 < iy2): return ET
    return PT

def tmask(a, B):
    m = 0
    for b in B:
        c = cls(a, b)
        if c != DISJ: m |= 1 << c
    return m

def has(T, *cs): return any((T >> c) & 1 for c in cs)

# Selector predicates, transcribed from the select case at design_topo_impl.f90:221-244
BASE = {
    'INTERACT':  lambda T: has(T, *OV),
    'INSIDE':    lambda T: has(T, EQ, INS, INB),
    'OUTSIDE':   lambda T: T == 0,
    'TOUCH':     lambda T: has(T, ET) and not has(T, *OV),
    'CUT':       lambda T: has(T, *OV) and not has(T, EQ, INS, INB),
    'DISJOINT':  lambda T: T == 0,
    'MEET':      lambda T: has(T, ET, PT) and not has(T, *OV),
    'EQUAL':     lambda T: has(T, EQ),
    'CONTAINS':  lambda T: has(T, EQ, CONS, CONB),
    'COVERS':    lambda T: has(T, EQ, CONB),
    'COVEREDBY': lambda T: has(T, EQ, INB),
    'OVERLAPS':  lambda T: has(T, PO),
}
SEL = dict(BASE)
for k, f in BASE.items():
    SEL['NOT_' + k] = (lambda f: (lambda T: not f(T)))(f)

def idisjoint(b, c):
    return not (max(b[0], c[0]) < min(b[2], c[2]) and max(b[1], c[1]) < min(b[3], c[3]))

def in_cell(cell, b):
    return b[0] <= cell[0] < b[2] and b[1] <= cell[1] < b[3]

# Forbidden class pairs for healed (pairwise interior-disjoint) B -- derived by hand,
# confirmed by search below, proved in lean/Trs/BoxTopo.lean.
FORBIDDEN = [(EQ, PO), (EQ, INS), (EQ, INB), (EQ, CONS), (EQ, CONB),
             (INS, PT), (INS, ET), (INS, PO), (INS, INB), (INS, CONS), (INS, CONB),
             (INB, PO), (INB, CONS), (INB, CONB)]

def realizable_healed(T):
    return not any(has(T, p) and has(T, q) for p, q in FORBIDDEN)

R_HEALED = [T for T in range(256) if realizable_healed(T)]

def point_types():
    """Point (cell) types for the single-pair (A,B) fragment: (mA, mB, T)."""
    pts = []
    for mB in (False, True):
        pts.append((False, mB, 0))
    for T in R_HEALED:
        for mB in (False, True):
            if has(T, EQ, INS, INB) and not mB: continue      # C1
            if mB and not has(T, *OV): continue               # C2
            pts.append((True, mB, T))
    return pts

A_BOX = (0, 0, 6, 6)

def search_witnesses(G=(-2, 9)):
    lo, hi = G
    cands = {c: [] for c in range(8)}
    rng = range(lo, hi)
    for x1, x2 in product(rng, rng):
        if x1 >= x2: continue
        for y1, y2 in product(rng, rng):
            if y1 >= y2: continue
            b = (x1, y1, x2, y2); c = cls(A_BOX, b)
            if c != DISJ: cands[c].append(b)
    found = {}
    def dfs(classes, chosen):
        if not classes:
            return list(chosen)
        c = classes[0]
        for b in cands[c]:
            if all(idisjoint(b, o) for o in chosen):
                r = dfs(classes[1:], chosen + [b])
                if r is not None: return r
        return None
    for T in range(256):
        cl = [c for c in range(8) if (T >> c) & 1]
        # order: most constrained first
        cl.sort(key=lambda c: len(cands[c]))
        w = dfs(cl, [])
        if w is not None:
            assert tmask(A_BOX, w) == T
            found[T] = w
    return found

def cell_witness(T, mB, B):
    for cx in range(A_BOX[0], A_BOX[2]):
        for cy in range(A_BOX[1], A_BOX[3]):
            inb = any(in_cell((cx, cy), b) for b in B)
            if inb == mB: return (cx, cy)
    return None

if __name__ == '__main__':
    W = search_witnesses()
    print('realizable (search):', len(W), ' predicted by FORBIDDEN:', len(R_HEALED),
          ' agree:', sorted(W) == R_HEALED)
    pts = point_types()
    missing = [(mA, mB, T) for (mA, mB, T) in pts if mA and cell_witness(T, mB, W[T]) is None]
    print('point types:', len(pts), ' with mA: ', sum(1 for p in pts if p[0]), ' lacking a cell witness:', missing)
