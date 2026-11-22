#!/usr/bin/env python3
"""End-to-end check of the CTR TRS optimizer: random CTR programs are optimized, then the original
and the optimized program run side by side in MAGPARSER.exe and every output is XOR-asserted equal.

Optimizers:
  fortran  src/TRS_OPT.exe (src/trs_optimizer.f90, the code MAGPARSER runs at compile time)
  julia    trs_dag.jl (reference implementation)
  engine   the compile-time pass inside MAGPARSER itself: the reference copy is frozen with
           `var trs_enable 0`, the second copy is optimized by the engine while parsing
           (`var trs_rules relations_trs.rules`).

  python3 trs/dag_check.py [--opt fortran|julia|engine] [--programs 20] [--ops 60] [--work DIR]
"""
import argparse, os, random, re, subprocess, sys, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from engine_check import make_design, ROOT

BOOL = ['*', '+', 'NOT', '^']
SEL = ['INSIDE', 'OUTSIDE', 'INTERACT', 'TOUCH', 'CUT', 'MEET', 'EQUAL', 'CONTAINS', 'COVERS',
       'COVEREDBY', 'OVERLAPS', 'DISJOINT', 'NOT_INSIDE', 'NOT_OUTSIDE', 'NOT_INTERACT']

def random_program(rnd, design, nops, nout=6):
    inputs = ['d:la', 'd:lb', 'd:lc']
    names = [f'm:v{i}' for i in range(max(4, nops // 4))]   # few names -> many redefinitions
    defined = []
    body = []
    for _ in range(nops):
        pool = inputs + defined
        x = rnd.choice(pool)
        # bias toward redundancy: reuse operands and wrap earlier results
        y = x if rnd.random() < 0.15 else rnd.choice(pool)
        op = rnd.choice(SEL) if rnd.random() < 0.35 else rnd.choice(BOOL)
        if op in SEL and rnd.random() < 0.5:
            y = rnd.choice(inputs)
        # no in-place ops (lhs = operand): the engine aliases the buffers and the result is
        # unreliable even without TRS (see RELATIONS_TRS.tex, engine issues)
        lhs = rnd.choice([nm for nm in names if nm not in (x, y)])
        body.append(f'exec run {lhs} = {x} {op} {y}')
        if lhs not in defined:
            defined.append(lhs)
    outs = rnd.sample(defined, min(nout, len(defined)))
    sinks = []
    for i, o in enumerate(outs):
        body.append(f'exec run m:out{i} = {o} COPY nothing')
        sinks.append(f'm:out{i}')
    head = ['program dag_check', f'decl design d = {design}', 'decl memory m = nothing']
    return head + body + ['end'], sinks

def run_opt(a, src, opt):
    if a.opt == 'julia':
        cmd = ['julia', os.path.join(ROOT, 'trs_dag.jl'), os.path.join(ROOT, 'relations_trs.json'), src, opt]
    else:
        cmd = [os.path.join(ROOT, 'src', 'TRS_OPT.exe'), os.path.join(ROOT, 'relations_trs.rules'), src, opt]
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        print(r.stdout, r.stderr); sys.exit(2)
    return r.stdout

def counts(text):
    nb = int(re.search(r'Ops before:\s+(\d+)', text).group(1))
    na = int(re.search(r'Ops after:\s+(\d+)', text).group(1))
    return nb, na

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--exe', default=os.path.join(ROOT, 'src', 'MAGPARSER.exe'))
    ap.add_argument('--opt', choices=['fortran', 'julia', 'engine'], default='fortran')
    ap.add_argument('--programs', type=int, default=20)
    ap.add_argument('--ops', type=int, default=60)
    ap.add_argument('--work', default=None)
    a = ap.parse_args()
    work = a.work or tempfile.mkdtemp(prefix='trs_dag_')
    rnd = random.Random(1)
    tot_before = tot_after = fails = 0
    for k in range(a.programs):
        design = os.path.join(work, f'rand{k}.mag')
        make_design(design, 100 + k)
        prog, sinks = random_program(rnd, design, a.ops)
        body = [l for l in prog if l.startswith('exec run')]
        comb = ['program dag_check_combined', f'var temp_folder {work}/', 'var temporary_layers 2',
                'var abort_on_assert_zero 1', f'decl design d = {design}',
                'decl memory m = nothing', 'decl memory n = nothing']
        if a.opt == 'engine':
            # the engine optimizes the n: copy while parsing; m: and the asserts are frozen
            comb += [f'var trs_rules {os.path.join(ROOT, "relations_trs.rules")}', 'var trs_enable 0']
            comb += body + ['var trs_enable 1']
            comb += [re.sub(r'\bm:', 'n:', l) for l in body] + ['var trs_enable 0']
        else:
            src = os.path.join(work, f'p{k}.ctr'); opt = os.path.join(work, f'p{k}.opt.ctr')
            open(src, 'w').write('\n'.join(prog) + '\n')
            nb, na = counts(run_opt(a, src, opt))
            tot_before += nb; tot_after += na
            optlines = [l for l in open(opt).read().splitlines() if l.startswith('exec run')]
            comb += body + [re.sub(r'\bm:', 'n:', l) for l in optlines]
        for i, s in enumerate(sinks):
            comb.append(f'exec run m:x{i} = {s} ^ {s.replace("m:", "n:")}')
            comb.append(f'exec run m:az{i} = m:x{i} ASSERT_ZERO nothing')
        comb.append('end')
        cpath = os.path.join(work, f'p{k}.combined.ctr')
        open(cpath, 'w').write('\n'.join(comb) + '\n')
        env = dict(os.environ); env.pop('MAGPARSER_TRS_RULES', None)
        res = subprocess.run([a.exe, cpath, '64'], cwd=work, capture_output=True, text=True, env=env)
        out = res.stdout + res.stderr
        if res.returncode != 0 and 'ASSERT_ZERO' not in out:
            print(f'program {k}: engine exited {res.returncode}\n{out[-1500:]}'); sys.exit(2)
        if a.opt == 'engine':
            if 'TRS optimized control file' not in out:
                print(f'program {k}: engine did not run the TRS pass\n{out[-1500:]}'); sys.exit(2)
            nb, na = counts(out)
            nb -= len(body) + 2 * len(sinks); na -= len(body) + 2 * len(sinks)   # frozen part
            tot_before += nb; tot_after += na
        nf = len(re.findall(r'ASSERT_ZERO fail', out))
        fails += nf
        print(f'program {k}: ops {nb} -> {na}, outputs {len(sinks)}, mismatches {nf}')
    print(f'TOTAL [{a.opt}]: {a.programs} programs, ops {tot_before} -> {tot_after}, output mismatches {fails}')
    print('work dir:', work)
    sys.exit(1 if fails else 0)

if __name__ == '__main__':
    main()
