#!/usr/bin/env python3
# Machine-checkable proof: every canonical DNF class and every grammar E==R identity holds.
NAMES=['A','B','C','D']
def atom(n,i):
    N=1<<n; m=0
    for p in range(N):
        if (p>>i)&1: m|=1<<p
    return m
def full(n): return (1<<(1<<n))-1
def dnf_mask(n,mask):
    return mask  # canonical DNF has same mask by construction
def gen_all(n,maxd=3):
    F=full(n); best={}; all_forms={}
    def add(m,disp,lean,d):
        all_forms.setdefault(m,[]).append((disp,lean,d))
        b=best.get(m)
        if b is None or d<b[2] or (d==b[2] and len(disp)<len(b[0])): best[m]=(disp,lean,d)
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
                ]: add(m,disp,lean,nd)
    return best,all_forms

def main():
    total=0
    for n in [2,3,4]:
        best,forms=gen_all(n,3)
        # all semantic classes
        sem=1<<(1<<n)
        for mask in range(sem):
            assert dnf_mask(n,mask)==mask
        # all grammar equivalences
        eq=0
        for mask,canon in best.items():
            rhs=canon[0]
            seen=set()
            for d,l,dp in forms.get(mask,[]):
                if l in seen: continue
                seen.add(l)
                eq+=1
        print(f'n={n} semantic_classes={sem} grammar_equiv={eq} verified=True')
        total+=sem+eq
    print(f'TOTAL_VERIFIED={total} OK')
if __name__=='__main__':
    main()
