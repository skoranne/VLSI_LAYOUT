#!/usr/bin/env python3
import os
NAMES=['A','B','C','D']
def atom(n,i):
    N=1<<n; m=0
    for p in range(N):
        if (p>>i)&1: m|=1<<p
    return m
def full(n): return (1<<(1<<n))-1

def gen(n,maxd=3):
    F=full(n)
    best={}
    def add(m,disp,lean,d):
        b=best.get(m)
        if b is None or d<b[2] or (d==b[2] and len(disp)<len(b[0])):
            best[m]=(disp,lean,d)
    for i in range(n): add(atom(n,i),NAMES[i],NAMES[i],0)
    for m,(disp,lean,d) in list(best.items()): add(F&~m,f"(NOT {disp})",f"(! {lean})",d+1)
    for depth in range(2,maxd+1):
        items=list(best.items())
        for m1,(d1,l1,p1) in items:
            for m2,(d2,l2,p2) in items:
                nd=max(p1,p2)+1
                if nd!=depth: continue
                add(m1&m2,f"({d1} AND {d2})",f"(sand {l1} {l2})",nd)
                add(m1|m2,f"({d1} OR {d2})",f"(sor {l1} {l2})",nd)
                add(m1&~m2,f"({d1} NOT {d2})",f"(snot {l1} {l2})",nd)
                add(m1^m2,f"({d1} XOR {d2})",f"(sxor {l1} {l2})",nd)
                add(m1&m2,f"({d1} INSIDE {d2})",f"(sinside {l1} {l2})",nd)
                add(m1&~m2,f"({d1} OUTSIDE {d2})",f"(soutside {l1} {l2})",nd)
                add(m1&m2,f"({d1} OVERLAP {d2})",f"(soverlap {l1} {l2})",nd)
                add(m1&~m2,f"({d1} DISJOINT_PART {d2})",f"(sdisjoint_part {l1} {l2})",nd)
    return best

def main():
    for n in [2,3,4]:
        best=gen(n,3)
        # group by mask
        bymask={}
        for m,(d,l,dp) in best.items(): bymask.setdefault(m,[]).append((d,l,dp))
        # identities E == A etc. i.e. same mask as atom
        for i in range(n):
            mA=atom(n,i)
            rhs=NAMES[i]
            # find all exprs with same mask as A? only A itself in best; but we want all syntactic E==A
            pass
    # better: collect all exprs with all forms
    # regenerate collecting all zeros and all equivalences to atoms
    pass

# collect all forms
def gen_all(n,maxd=3):
    F=full(n)
    best={}
    all_forms={} # mask -> list of (disp,lean,d)
    def add(m,disp,lean,d):
        all_forms.setdefault(m,[])
        all_forms[m].append((disp,lean,d))
        b=best.get(m)
        if b is None or d<b[2] or (d==b[2] and len(disp)<len(b[0])):
            best[m]=(disp,lean,d)
    for i in range(n): add(atom(n,i),NAMES[i],NAMES[i],0)
    for m,(disp,lean,d) in list(best.items()): add(F&~m,f"(NOT {disp})",f"(! {lean})",d+1)
    for depth in range(2,maxd+1):
        items=list(best.items())
        for m1,(d1,l1,p1) in items:
            for m2,(d2,l2,p2) in items:
                nd=max(p1,p2)+1
                if nd!=depth: continue
                for m,disp,lean in [
                    (m1&m2,f"({d1} AND {d2})",f"(sand {l1} {l2})"),
                    (m1|m2,f"({d1} OR {d2})",f"(sor {l1} {l2})"),
                    (m1&~m2,f"({d1} NOT {d2})",f"(snot {l1} {l2})"),
                    (m1^m2,f"({d1} XOR {d2})",f"(sxor {l1} {l2})"),
                    (m1&m2,f"({d1} INSIDE {d2})",f"(sinside {l1} {l2})"),
                    (m1&~m2,f"({d1} OUTSIDE {d2})",f"(soutside {l1} {l2})"),
                ]:
                    add(m,disp,lean,nd)
    return best, all_forms

if __name__=='__main__':
    for n in [2,3,4]:
        best,forms=gen_all(n,3)
        A=atom(n,0)
        zeros=[d for d,l,dp in forms.get(0,[])]
        aeq=[d for d,l,dp in forms.get(A,[]) if d!='A']
        print(f'n={n} zeros={len(set(zeros))} eqA={len(set(aeq))}')
        for x in sorted(set(aeq))[:20]: print(' ',x)
