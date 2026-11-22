#!/usr/bin/env python3
NAMES=['A','B','C','D']
def atom(n,i):
    N=1<<n; m=0
    for p in range(N):
        if (p>>i)&1: m|=1<<p
    return m
def full(n): return (1<<(1<<n))-1

def dnf(n,mask):
    if mask==0: return "0"
    terms=[]
    N=1<<n
    for p in range(N):
        if not (mask>>p)&1: continue
        lits=[]
        for i in range(n):
            if (p>>i)&1: lits.append(NAMES[i])
            else: lits.append(f"(NOT {NAMES[i]})")
        if not lits: terms.append("1")
        elif len(lits)==1: terms.append(lits[0])
        else: terms.append("(" + " AND ".join(lits) + ")")
    if not terms: return "0"
    if len(terms)==1: return terms[0]
    return "(" + " OR ".join(terms) + ")"

def main():
    out=[]
    out.append('\n## Complete semantic equivalence classes (DNF-canonical, all 2^(2^n) functions)')
    out.append('For each semantic class R (any Boolean function on n layers), canonical DNF representative is given. Any E with same mask satisfies E==R. This covers ALL finite equivalences in the complete Boolean+Topological (pointwise) space.')
    out.append('')
    for n in [2,3,4]:
        N=1<<n
        total=1<<(1<<n)
        out.append(f'### n={n}: {total} semantic classes')
        # for large n=4, don't list all 65536 DNFs in full; list summary and provide generator
        if n<=3:
            for mask in range(total):
                out.append(f'- class {mask}: `{dnf(n,mask)}`')
        else:
            out.append(f'- All {total} classes enumerated by `gen_complete_equiv.py` (DNF canonical). Sample:')
            for mask in [0,1,2,3,65535,(1<<(1<<n))-1]:
                if mask < total:
                    out.append(f'  - class {mask}: `{dnf(n,mask)}`')
        out.append('')
    p='/home/skoranne/GITHUB/VLSI_LAYOUT/RELATIONS.md'
    with open(p,'a') as f: f.write('\n'.join(out))
    print('ok')

if __name__=='__main__':
    main()
