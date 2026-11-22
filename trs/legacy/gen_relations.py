#!/usr/bin/env python3
from collections import OrderedDict

NAMES=['A','B','C','D']
def atom(n,i):
    N=1<<n; m=0
    for p in range(N):
        if (p>>i)&1: m|=1<<p
    return m
def full(n): N=1<<n; return (1<<N)-1

# Build expression pool: map mask -> shortest repr; also collect all zero reprs
def gen(n, maxd=3):
    F=full(n)
    # reprs[mask]=shortest string
    reprs={}
    zeros=set()
    def add(m,s,d):
        if m==0: zeros.add(s)
        r=reprs.get(m)
        if r is None or d<r[1] or (d==r[1] and len(s)<len(r[0])):
            reprs[m]=(s,d)
    # atoms
    for i in range(n):
        add(atom(n,i),NAMES[i],0)
    # unary not
    base=list(reprs.items())
    for m,(s,d) in base:
        add(F&~m,f"NOT({s})",d+1)
    # binary
    for depth in range(2,maxd+1):
        items=list(reprs.items())
        for m1,(s1,d1) in items:
            for m2,(s2,d2) in items:
                nd=max(d1,d2)+1
                if nd!=depth: continue
                # Boolean
                add(m1&m2,f"({s1} AND {s2})",nd)
                add(m1|m2,f"({s1} OR {s2})",nd)
                add(m1&~m2,f"({s1} NOT {s2})",nd)
                add(m2&~m1,f"({s2} NOT {s1})",nd)
                add(m1^m2,f"({s1} XOR {s2})",nd)
                # Topological selectors (set-valued)
                add(m1&m2,f"({s1} INSIDE {s2})",nd)
                add(m1&~m2,f"({s1} OUTSIDE {s2})",nd)
                # Egenhofer/9E-DIM inspired selectors as set-valued in pure set model
                add(m1&m2,f"({s1} OVERLAPS {s2})",nd)
                add(m1&m2,f"({s1} CONTAINS_PART {s2})",nd) # A ∩ B when B⊆A? keep simple
    # filter zeros to unique canonical by normalizing spaces
    z=sorted(zeros, key=lambda s:(s.count('('),len(s),s))
    return z, reprs

if __name__=='__main__':
    for n in [2,3]:
        z,reprs=gen(n,3)
        print(f'n={n} zeros={len(z)} exprs={len(reprs)}')
        for s in z[:60]:
            print(' ',s)
