#!/usr/bin/env python3
import os
NAMES=['A','B','C','D']
def atom(n,i):
    N=1<<n; m=0
    for p in range(N):
        if (p>>i)&1: m|=1<<p
    return m
def full(n): return (1<<(1<<n))-1
def gen_all(n,maxd=3):
    F=full(n)
    best={}
    all_forms={}
    def add(m,disp,lean,d):
        all_forms.setdefault(m,[]).append((disp,lean,d))
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
def uniq(forms):
    seen=set(); out=[]
    for d,l,dp in forms:
        if l in seen: continue
        seen.add(l); out.append((d,l,dp))
    out.sort(key=lambda x:(x[2],len(x[0]),x[0]))
    return out
def main():
    lean_path='/home/skoranne/GITHUB/VLSI_LAYOUT/RelationsEquivAll.lean'
    lean=[]
    lean.append('/- RelationsEquivAll.lean: all grammar E==R equivalences for all reachable semantic classes -/')
    lean.append('namespace VLSI.RelationsEquivAll')
    lean.append('abbrev S := Bool')
    lean.append('def sand (a b : S) : S := a && b')
    lean.append('def sor  (a b : S) : S := a || b')
    lean.append('def snot (a b : S) : S := a && !b')
    lean.append('def sxor (a b : S) : S := xor a b')
    lean.append('def sinside (a b : S) : S := a && b')
    lean.append('def soutside (a b : S) : S := a && !b')
    lean.append('')
    summary=[]
    for n in [2,3,4]:
        best,forms=gen_all(n,3)
        vars=' '.join(NAMES[:n])
        total=0
        for mask,canon in best.items():
            rhs_disp,rhs_lean,_=canon
            us=uniq(forms.get(mask,[]))
            us=[x for x in us if x[1]!=rhs_lean]
            for idx,(d,l,dp) in enumerate(us):
                thm=f"theorem e{n}_{mask}_{idx} ({vars} : S) : {l} = {rhs_lean} := by cases {' <;> cases '.join(NAMES[:n])} <;> rfl"
                lean.append(thm)
                total+=1
        summary.append((n,len(best),total))
        lean.append('')
    lean.append('end VLSI.RelationsEquivAll')
    with open(lean_path,'w') as f: f.write('\n'.join(lean))
    for n,masks,total in summary:
        print(f'n={n} reachable_masks={masks} equiv_identities={total}')
if __name__=='__main__':
    main()
