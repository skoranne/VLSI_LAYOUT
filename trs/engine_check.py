#!/usr/bin/env python3
"""Empirical cross-check of the Lean-verified rules against the real engine (MAGPARSER.exe).

Generates small random designs on a coarse grid (so shared edges, equal boxes, containment and
corner contacts all occur), instantiates every rule of relations_trs.json with the layers
(A,B,C) = several permutations of (la,lb,lc), asserts each relation with ASSERT_ZERO, and
reports failures.  Negative controls (identities that are NOT valid in the box model) are
expected to fail and confirm the check has teeth.

  python3 trs/engine_check.py [--exe src/MAGPARSER.exe] [--seeds 3] [--work DIR]
"""
import argparse, json, os, random, re, subprocess, sys, tempfile

ROOT = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..'))

NEGATIVE = [  # (lhs, rhs, why it is not a box-model identity)
    ('((A INSIDE B) ^ (A * B))', '0', 'INSIDE keeps whole boxes; A*B also has partial overlaps'),
    ('((A CONTAINS B) NOT B)', '0', 'a CONTAINS box extends beyond B'),
    ('((A OUTSIDE B) ^ (A NOT B))', '0', 'OUTSIDE drops boxes that only partly leave B'),
]

def tokens(s): return re.findall(r'\(|\)|[^\s()]+', s)

def parse(s):
    t = tokens(s); pos = [0]
    def p():
        x = t[pos[0]]; pos[0] += 1
        if x == '(':
            l = p(); op = t[pos[0]]; pos[0] += 1; r = p(); pos[0] += 1
            return (op, l, r)
        return x
    return p()

def make_design(path, seed, nbox=40, grid=10, span=24):
    rnd = random.Random(seed)
    layers = {'la': [], 'lb': [], 'lc': []}
    def rbox():
        x1 = rnd.randrange(0, span) * grid; y1 = rnd.randrange(0, span) * grid
        w = rnd.randrange(1, 6) * grid; h = rnd.randrange(1, 6) * grid
        return (x1, y1, x1 + w, y1 + h)
    for name in layers:
        layers[name] = [rbox() for _ in range(nbox)]
    # force coincidences: equal boxes, nested boxes, edge and corner neighbours
    for b in rnd.sample(layers['la'], 8):
        layers['lb'].append(b)
        x1, y1, x2, y2 = b
        layers['lb'].append((x2, y1, x2 + grid, y2))                 # edge touch
        layers['lc'].append((x2, y2, x2 + grid, y2 + grid))          # corner touch
        if x2 - x1 > 2 * grid and y2 - y1 > 2 * grid:
            layers['lc'].append((x1 + grid, y1 + grid, x2 - grid, y2 - grid))  # strictly inside
    with open(path, 'w') as f:
        f.write('magic\ntech sky130A\ntimestamp 0\n')
        for name, bs in layers.items():
            f.write(f'<< {name} >>\n')
            for b in bs:
                f.write('rect %d %d %d %d\n' % b)
        f.write('<< end >>\n')

def build_ctr(rules, design, perm, workdir):
    lines = ['program trs_engine_check', f'var temp_folder {workdir}/', 'var temporary_layers 2',
             'var abort_on_assert_zero 1', f'decl design d = {design}', 'decl memory m = nothing']
    env = {'A': 'd:' + perm[0], 'B': 'd:' + perm[1], 'C': 'd:' + perm[2]}
    memo = {}; counter = [0]; checks = {}
    def emit(t):
        if isinstance(t, str):
            return env[t]
        key = repr(t)
        if key in memo: return memo[key]
        op, l, r = t
        a, b = emit(l), emit(r)
        counter[0] += 1; nm = f'm:t{counter[0]}'
        lines.append(f'exec run {nm} = {a} {op} {b}')
        memo[key] = nm
        return nm
    def check(tag, lhs, rhs):
        x = emit(parse(lhs))
        if rhs != '0':
            y = emit(parse(rhs))
            counter[0] += 1; d = f'm:t{counter[0]}'
            lines.append(f'exec run {d} = {x} ^ {y}')
            x = d
        counter[0] += 1
        lines.append(f'exec run m:az{counter[0]} = {x} ASSERT_ZERO nothing')
        checks[len(lines)] = tag
    for r in rules:
        check(r['name'], r['lhs'], r['rhs'])
    for i, (l, rr, _) in enumerate(NEGATIVE):
        check(f'NEGATIVE_{i}', l, rr)
    lines.append('end')
    return '\n'.join(lines) + '\n', checks

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--exe', default=os.path.join(ROOT, 'src', 'MAGPARSER.exe'))
    ap.add_argument('--rules', default=os.path.join(ROOT, 'relations_trs.json'))
    ap.add_argument('--seeds', type=int, default=3)
    ap.add_argument('--work', default=None)
    a = ap.parse_args()
    rules = json.load(open(a.rules))['rules']
    work = a.work or tempfile.mkdtemp(prefix='trs_engine_')
    perms = [('la', 'lb', 'lc'), ('lb', 'la', 'lc'), ('lc', 'la', 'lb')]
    total_fail = 0; neg_fail = set(); ran = 0
    for seed in range(a.seeds):
        design = os.path.join(work, f'rand{seed}.mag')
        make_design(design, seed)
        for perm in perms:
            ctr, checks = build_ctr(rules, design, perm, work)
            cpath = os.path.join(work, f'check_{seed}_{"".join(p[1] for p in perm)}.ctr')
            open(cpath, 'w').write(ctr)
            res = subprocess.run([a.exe, cpath, '64'], cwd=work, capture_output=True, text=True)
            out = res.stdout + res.stderr
            if res.returncode != 0 and 'ASSERT_ZERO' not in out:
                print(f'seed {seed} perm {perm}: engine exited {res.returncode}\n{out[-2000:]}')
                sys.exit(2)
            failed = set()
            for m in re.finditer(r'ASSERT_ZERO fail on layer\. Line:\s*(\d+)', out):
                ln = int(m.group(1))
                tag = checks.get(ln) or checks.get(ln + 1)
                failed.add(tag or f'line {ln}')
            neg = {t for t in failed if t and t.startswith('NEGATIVE')}
            bad = failed - neg
            neg_fail |= neg
            total_fail += len(bad); ran += len(checks)
            print(f'seed {seed} (A,B,C)={perm}: {len(checks) - len(NEGATIVE)} rules, '
                  f'{len(bad)} rule failures, negative controls failing as expected: {len(neg)}/{len(NEGATIVE)}')
            for t in sorted(bad, key=str)[:20]:
                print('   FAIL', t)
    print(f'TOTAL: {ran} assertions, {total_fail} rule failures; '
          f'negative controls that failed at least once: {len(neg_fail)}/{len(NEGATIVE)}')
    print('work dir:', work)
    sys.exit(1 if total_fail else 0)

if __name__ == '__main__':
    main()
