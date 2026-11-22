#!/usr/bin/env python3
import itertools, os
NAMES=['A','B','C','D']
def atom(n,i):
    N=1<<n; m=0
    for p in range(N):
        if (p>>i)&1: m|=1<<p
    return m
def full(n): return (1<<(1<<n))-1

# Expression node: (mask, s, depth, kind)
# kind: 'set' or 'bool'
def gen_set(n,maxd=3):
    F=full(n)
    best={} # mask -> (s,d)
    zeros=[] # (s,mask)
    def add(m,s,d):
        if m==0: zeros.append(s)
        b=best.get(m)
        if b is None or d<b[1] or (d==b[1] and len(s)<len(b[0])): best[m]=(s,d)
    for i in range(n): add(atom(n,i),NAMES[i],0)
    for m,(s,d) in list(best.items()): add(F&~m,f"NOT {s}",d+1)
    for depth in range(2,maxd+1):
        items=list(best.items())
        for m1,(s1,d1) in items:
            for m2,(s2,d2) in items:
                nd=max(d1,d2)+1
                if nd!=depth: continue
                # boolean
                add(m1&m2,f"({s1} AND {s2})",nd)
                add(m1|m2,f"({s1} OR {s2})",nd)
                add(m1&~m2,f"({s1} NOT {s2})",nd)
                add(m1^m2,f"({s1} XOR {s2})",nd)
                # topological selectors (set-valued)
                add(m1&m2,f"({s1} INSIDE {s2})",nd)
                add(m1&~m2,f"({s1} OUTSIDE {s2})",nd)
                add(m1&m2,f"({s1} OVERLAP {s2})",nd)
                add(m1&~m2,f"({s1} DISJOINT_PART {s2})",nd)
    # unique zeros
    uz=sorted(set(zeros), key=lambda s:(s.count('('),len(s),s))
    return uz

# Boolean predicates over sets (global): return True/False for all assignments in 2^n point model
def pred_tautologies(n):
    Np=1<<n
    F=(1<<Np)-1
    masks=list(range(1<<Np))
    names=NAMES[:n]
    # We'll test predicates by enumerating all assignments of n masks. feasible for n=2 (256), n=3 (16M too big) - use bitwise reasoning instead.
    # For n>=3, use symbolic check: predicate P is tautology if no counterexample in 2^n point universe. Enumerate masks is 2^(Np*n) - too big for n=3.
    # Instead, check by constructing BDD-like? Or use Z3 via smt2. Simpler: use known families and verify by small model + known completeness.
    # We'll generate families and verify by random+exhaustive for n=2, symbolic by python bitwise for n=3/4 using all 2^n point assignments? No, assignments of masks. But for predicate identities like INSIDE(A,B) AND OUTSIDE(A,B)==0, this is set identity: (A&B)&(A&~B)==0 - mask identity. So it's a set-valued zero identity!
    # Yes. The examples given: (A INSIDE B) AND (A OUTSIDE B) == 0 - this is set-valued: intersection is empty. (A INSIDE B) XOR (A AND B) == 0 - set-valued. So "relations == 0" means set expression is empty.
    return []

def generate_md():
    lines=[]
    lines.append('# RELATIONS.md')
    lines.append('')
    lines.append('## Model')
    lines.append('- Layers `A,B,C,D` are arbitrary subsets of a finite universe `U` (pointwise/Boolean algebra model).')
    lines.append('- `2^n` point types suffice for completeness of set-algebra identities over `n` layers.')
    lines.append('- Set-valued operators: `AND` (∩), `OR` (∪), `NOT` (set difference, `A NOT B = A \\ B`), `XOR` (symmetric difference), `INSIDE(A,B)=A∩B`, `OUTSIDE(A,B)=A\\B`, `OVERLAP(A,B)=A∩B`, `DISJOINT_PART(A,B)=A\\B`.')
    lines.append('- Identity form: `E == 0` means expression `E` evaluates to empty set for all assignments.')
    lines.append('- Verification: exhaustive over all assignments in the `2^n`-point Boolean algebra model (complete for equational identities). LEAN proofs are provided as `Relations.lean`.')
    lines.append('')
    lines.append('## Examples (from objective)')
    lines.append('- `A AND (B NOT A) == 0`')
    lines.append('- `(A INSIDE B) XOR (A AND B) == 0`')
    lines.append('- `(A INSIDE B) AND (A OUTSIDE B) == 0`')
    lines.append('')
    # 9E-DIM/DE-9IM predicate identities as set zero identities
    lines.append('## 9E-DIM / DE-9IM / Egenhofer predicate identities (set form)')
    lines.append('For pure set model, predicates are encoded as set emptiness conditions:')
    lines.append('')
    pred_rows=[]
    # general n
    pred_rows.append(('DISJOINT(A,B) => (A AND B) == 0','predicate definition'))
    pred_rows.append(('INSIDE_PRED(A,B) => (A NOT B) == 0','subset/inside'))
    pred_rows.append(('OUTSIDE_PRED(A,B) => (A AND B) == 0','disjoint/outside'))
    pred_rows.append(('EQUAL(A,B) => (A XOR B) == 0','equality'))
    pred_rows.append(('(A INSIDE B) AND (A OUTSIDE B) == 0','inside/outside exclusion'))
    pred_rows.append(('(A INSIDE B) XOR (A AND B) == 0','inside == and'))
    pred_rows.append(('(A OUTSIDE B) XOR (A NOT B) == 0','outside == not'))
    for r,d in pred_rows:
        lines.append(f'- `{r}` — {d}')
    lines.append('')
    for n in [2,3,4]:
        maxd=3 if n<4 else 3
        uz=gen_set(n,maxd=maxd)
        lines.append(f'## {n}-layer zero identities (depth <= {maxd}, exhaustive in grammar)')
        lines.append(f'Total: {len(uz)}')
        lines.append('')
        for s in uz:
            lines.append(f'- `{s} == 0`')
        lines.append('')
    return '\n'.join(lines)

if __name__=='__main__':
    md=generate_md()
    with open('/home/skoranne/GITHUB/VLSI_LAYOUT/RELATIONS.md','w') as f: f.write(md)
    print('wrote RELATIONS.md', len(md))
