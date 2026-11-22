#!/usr/bin/env python3
"""Differential test: src/TRS_OPT.exe (Fortran, read by MAGPARSER) vs trs_dag.jl (Julia).
Random CTR programs (with redefinitions, CONNECT ZONEs, opaque ops, word operators) are
optimized by both; the emitted CTRs and the statistics must be identical.

  python3 trs/fortran_vs_julia.py [--programs 300] [--ops 80]
"""
import argparse, os, random, subprocess, sys, tempfile
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

BOOL = ['*', '+', 'NOT', '^', 'AND2', 'OR2', 'XOR', '@']
SEL = ['INSIDE', 'OUTSIDE', 'INTERACT', 'TOUCH', 'CUT', 'MEET', 'EQUAL', 'CONTAINS', 'COVERS',
       'COVEREDBY', 'OVERLAPS', 'DISJOINT', 'NOT_INSIDE', 'NOT_OUTSIDE', 'NOT_INTERACT',
       'NOT INSIDE', 'NOT TOUCH']
OPAQUE = ['AND nothing', 'SIZE 10', 'COPY nothing', 'INSIDE EDGE']

def program(rnd, nops):
    inputs = ['d:la', 'd:lb', 'd:lc']
    names = [f'm:v{i}' for i in range(max(4, nops // 5))]
    defined, body = [], ['program fvj', 'decl design d = x.mag', 'decl memory m = nothing']
    zone = 0
    for i in range(nops):
        if zone == 0 and rnd.random() < 0.03:
            zone = rnd.randint(1, 3); body.append(f'START CONNECT ZONE {zone}')
        elif zone and rnd.random() < 0.2:
            zone = 0; body.append('END CONNECT ZONE')
        pool = inputs + defined
        x = rnd.choice(pool); y = x if rnd.random() < 0.2 else rnd.choice(pool)
        r = rnd.random()
        if r < 0.05:
            op = rnd.choice(OPAQUE)
            body.append(f'exec run {rnd.choice(names)} = {x} {op}' + (f' {y}' if 'EDGE' in op else ''))
        else:
            op = rnd.choice(SEL) if r < 0.4 else rnd.choice(BOOL)
            lhs = rnd.choice(names)
            body.append(f'exec run {lhs} = {x} {op} {y}')
            if lhs not in defined: defined.append(lhs)
        if rnd.random() < 0.02:
            body.append(f'# comment {i}')
        if rnd.random() < 0.03:
            body.append(f'var trs_enable {rnd.choice([0, 1])}')
    if zone: body.append('END CONNECT ZONE')
    for i, o in enumerate(rnd.sample(defined, min(5, len(defined)))):
        body.append(f'exec run m:out{i} = {o} COPY nothing')
    body.append(f'exec output o1:la = {rnd.choice(defined)}')
    return body + ['end']

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--programs', type=int, default=300)
    ap.add_argument('--ops', type=int, default=80)
    a = ap.parse_args()
    work = tempfile.mkdtemp(prefix='trs_fvj_')
    rnd = random.Random(7)
    srcs = []
    for k in range(a.programs):
        p = os.path.join(work, f'p{k}.ctr')
        open(p, 'w').write('\n'.join(program(rnd, rnd.randint(5, a.ops))) + '\n')
        srcs.append(p)
    # one Julia process for all programs (startup is slow)
    jl = os.path.join(work, 'run.jl')
    open(jl, 'w').write(
        'ARGS_ORIG = copy(ARGS)\n'
        f'empty!(ARGS); push!(ARGS, "{ROOT}/relations_trs.json", "{srcs[0]}", "/dev/null")\n'
        f'redirect_stdout(devnull) do; include("{ROOT}/trs_dag.jl"); end\n'
        f'rules = load_rules("{ROOT}/relations_trs.json")\n'
        'for s in ARGS_ORIG\n'
        '  P = parse_ctr(s); optimize!(P, rules); st = write_ctr(s * ".jl", P)\n'
        '  open(s * ".jlstat", "w") do io; r = st[:removed] + st[:orphan]\n'
        '    println(io, length(P.nodes), " ", r, " ", st[:copy], " ", st[:replace], " ", st[:kept_empty]); end\n'
        'end\n')
    subprocess.run(['julia', jl] + srcs, check=True)
    bad = rew = 0
    exe = os.path.join(ROOT, 'src', 'TRS_OPT.exe')
    for s in srcs:
        r = subprocess.run([exe, os.path.join(ROOT, 'relations_trs.rules'), s, s + '.f90'],
                           capture_output=True, text=True, check=True)
        f = [l.rstrip() for l in open(s + '.f90')]
        j = [l.rstrip() for l in open(s + '.jl')]
        rew += sum(1 for l in f if l.startswith('# TRS:'))
        if f != j:
            bad += 1; print('MISMATCH', s)
    print(f'{a.programs} programs, {rew} TRS annotations, {bad} mismatches (work {work})')
    sys.exit(1 if bad else 0)

if __name__ == '__main__':
    main()
