#!/usr/bin/env python3
"""MAGPARSER regression harness.

  regress.py run     [--tier quick|full|large] [--bin-dir DIR] [--out DIR] [--modes gpu,cpu,check]
                     [--only SUBSTR]
  regress.py compare BASE_JSON NEW_JSON [--time]

`run` executes every case of the selected tier(s), writes per-run logs and a
results.json under --out, and prints a PASS/FAIL summary.  MAGPARSER cases are run
with MAGPARSER_LAYER_DIGEST=1 so that every operation's output layer is reduced to
(n, area, hsum, hxor), an order-independent digest of its box set; GPU-mode
(OMP_TARGET_OFFLOAD=MANDATORY) and CPU-mode (OMP_TARGET_OFFLOAD=DISABLED) digests
must agree, and `compare` checks that a modified build reproduces the baseline.

Exit status of `run`/`compare` is the number of failures (0 = all good).
"""
import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path

SRC = Path(__file__).resolve().parent.parent          # .../VLSI_LAYOUT/src
ROOT = SRC.parent
HOME = Path.home()
NVF_BIN = "/opt/nvidia/hpc_sdk/Linux_aarch64/26.1/compilers/bin"

# Path rewrites applied to `decl design` lines (mac paths and Git-LFS pointer files).
PATH_MAP = {
    "/Users/skoranne/": str(HOME) + "/",
    "../data/FPU/FPU_8x8.oas": str(HOME / "FPU_8x8_FLAT.oas"),
    "../data/SDT/SDT_16.oas": "/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SMALL_SDT/SDT_16.oas",
}

MW_BIN = "/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/MW4x4_FLAT_DATA/MW4x4_FLAT_L70_D20.bin"
OASIS_CORPUS = [
    HOME / "bsc_julia/examples/or_out_root/or_out_counter/Counter8.oas",
    HOME / "bsc_julia/examples/or_out_root/or_out_fft16/FFT16Complex16.oas",
    Path("/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SMALL_SDT/SDT_16.oas"),
    HOME / "bsc_julia/examples/or_out_root/or_out_full_llm/FullLLMEngine_Qwen.oas",
    Path("/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/MW4x4_FLAT_DATA/MW4x4_HIER.oas"),
    HOME / "bsc_julia/examples/or_out_root/or_out_sr1m_mega_pnr/SRImageProc1M_final.oas",
    HOME / "FPU_8x8_FLAT.oas",
]


def cases():
    """(name, tier, kind, spec) tuples."""
    c = []
    c.append(("boolean_ops", "quick", "ctr", {"ctr": SRC / "regress_boolean_ops.ctr"}))
    c.append(("coverage_fast", "quick", "ctr", {"ctr": SRC / "COVERAGE_FAST.ctr"}))
    for p in sorted((SRC / "topo_regressions/ctrs").glob("*.ctr")):
        c.append(("topo_" + p.stem, "quick", "ctr", {"ctr": p}))
    c.append(("ovlp_sq_gds", "quick", "ctr", {"ctr": SRC / "db/regressions/ovlp_sq_gds.ctr"}))
    c.append(("hier_ovlp", "quick", "ctr", {"ctr": SRC / "db/regressions/hier_ovlp.ctr"}))
    c.append(("hier_fft16", "quick", "ctr", {"ctr": SRC / "db/regressions/hier_fft16.ctr"}))
    c.append(("sdt_oasis", "quick", "ctr", {"ctr": SRC / "db/regressions/SDT_oasis.ctr", "max_layers": 64}))
    c.append(("db_xor_quick", "quick", "cmd", {"cmd": ["bash", str(SRC / "db/regressions/xor_regression.sh"), "--quick"]}))
    c.append(("oasis_extent", "quick", "oasis", {"files": OASIS_CORPUS[:4]}))
    # full tier
    c.append(("mw4x4_hier_oasis", "full", "ctr", {"ctr": SRC / "db/regressions/MW4x4_HIER_oasis.ctr", "max_layers": 64}))
    c.append(("sdt_mag", "full", "ctr", {"ctr": SRC / "SDT.ctr", "max_layers": 64}))
    c.append(("mw4x4_selftest", "full", "ctr", {"ctr": SRC / "MW_4x4_SELFTEST.ctr", "max_layers": 64}))
    c.append(("fpu_sdt_allops", "full", "ctr", {"ctr": ROOT / "Regressions/fpu_sdt_allops_selfcheck.ctr", "max_layers": 64}))
    c.append(("test_interaction_mw", "full", "exe", {"exe": "test_interaction.exe", "args": [MW_BIN, "1", "0", "0", "0"]}))
    c.append(("oasis_extent_big", "full", "oasis", {"files": OASIS_CORPUS[4:]}))
    c.append(("db_xor_full", "full", "cmd", {"cmd": ["bash", str(SRC / "db/regressions/xor_regression.sh")]}))
    # large tier (timing)
    c.append(("fpu8x8", "large", "ctr", {"ctr": SRC / "FPU8x8.ctr"}))
    for p in sorted((ROOT / "Regressions").glob("boolean_perf_*.ctr")):
        c.append(("perf_" + p.stem, "large", "ctr", {"ctr": p}))
    return c


TIERS = {"quick": ["quick"], "full": ["quick", "full"], "large": ["quick", "full", "large"], "large-only": ["large"], "full-only": ["full"]}

DIGEST_RE = re.compile(r"LAYER_DIGEST line=(\d+) layer=(\S+) n=(-?\d+) area=(-?\d+) hsum=(-?\d+) hxor=(-?\d+)")
TIME_RE = re.compile(r"^\s*(\S.{0,19}?)\s+([0-9.]+) CPU seconds\.\s+([0-9.]+) REAL seconds\.", re.M)
FAIL_PATTERNS = ["ASSERT_ZERO fail", "Aborting RUN", "STALE DEVICE", "Error termination", "Segmentation fault",
                 "illegal memory access", "FATAL ERROR"]

MODE_ENV = {
    "gpu": {"OMP_TARGET_OFFLOAD": "MANDATORY"},
    "cpu": {"OMP_TARGET_OFFLOAD": "DISABLED"},
    "check": {"OMP_TARGET_OFFLOAD": "MANDATORY", "MAGPARSER_GPU_RESIDENCY_CHECK": "1"},
}


def rewrite_ctr(ctr: Path, out_dir: Path, tmp_dir: Path) -> Path:
    """Copy a ctr with temp_folder redirected to tmp_dir and relative/remapped design paths made absolute."""
    lines = []
    for line in ctr.read_text().splitlines():
        s = line.strip()
        if s.startswith("var temp_folder"):
            line = f"var temp_folder {tmp_dir}/"
        m = re.match(r"^(\s*decl\s+(design|output)\s+\S+\s*=\s*)(\S+)(.*)$", line)
        if m and m.group(3) != "nothing":
            path = m.group(3)
            for k, v in PATH_MAP.items():
                if path.startswith(k) or path == k:
                    path = path.replace(k, v, 1)
            # Output .mag files are templates that must exist (they name the KLBIN targets),
            # so outputs are resolved like inputs rather than redirected.
            if not os.path.isabs(path):
                path = str((ctr.parent / path).resolve())
            line = m.group(1) + path + m.group(4)
        lines.append(line)
    dst = out_dir / ctr.name
    dst.write_text("\n".join(lines) + "\n")
    return dst


def base_env(bin_dir: Path):
    env = dict(os.environ)
    if os.path.isdir(NVF_BIN):
        env["PATH"] = NVF_BIN + ":" + env.get("PATH", "")
    env.setdefault("OMP_NUM_THREADS", str(os.cpu_count() or 4))
    env["MAGPARSER_LAYER_DIGEST"] = "1"
    env["MAGPARSER_GPU_RESIDENCY_STATS"] = "1"
    return env


def parse_log(text: str):
    digests = [dict(line=int(m[1]), layer=m[2], n=int(m[3]), area=int(m[4]), hsum=int(m[5]), hxor=int(m[6]))
               for m in DIGEST_RE.finditer(text)]
    times = [dict(op=m[1].strip(), cpu=float(m[2]), real=float(m[3])) for m in TIME_RE.finditer(text)]
    fails = [p for p in FAIL_PATTERNS if p in text]
    return digests, times, fails


def run_proc(cmd, cwd, env, log: Path, timeout):
    t0 = time.time()
    # stderr goes to a side file: Fortran runtime warnings written to stderr otherwise
    # interleave mid-line with stdout and corrupt LAYER_DIGEST records.
    with open(log, "w") as fh, open(str(log) + ".err", "w") as fe:
        try:
            rc = subprocess.run(cmd, cwd=cwd, env=env, stdout=fh, stderr=fe, timeout=timeout).returncode
        except subprocess.TimeoutExpired:
            rc = -999
    err = Path(str(log) + ".err").read_text(errors="replace")
    if err.strip():
        with open(log, "a") as fh:
            fh.write("\n--- STDERR ---\n" + err)
    return rc, time.time() - t0


def run_ctr_case(name, spec, mode, bin_dir, out, env, timeout):
    d = out / name / mode
    shutil.rmtree(d, ignore_errors=True)
    d.mkdir(parents=True)
    tmp = d / "tmp"
    tmp.mkdir()
    ctr = rewrite_ctr(Path(spec["ctr"]), d, tmp)
    e = dict(env)
    e.update(MODE_ENV[mode])
    log = d / "run.log"
    rc, wall = run_proc([str(bin_dir / "MAGPARSER.exe"), str(ctr), str(spec.get("max_layers", 10))], spec["ctr"].parent, e, log, timeout)
    text = log.read_text(errors="replace")
    digests, times, fails = parse_log(text)
    shutil.rmtree(tmp, ignore_errors=True)
    ok = rc == 0 and not fails
    resid = re.search(r"^GPU_RESIDENCY .*$", text, re.M)
    return dict(name=name, mode=mode, kind="ctr", rc=rc, wall=round(wall, 3), ok=ok, fails=fails,
                digests=digests, times=times, log=str(log), residency=resid.group(0) if resid else "")


def run_exe_case(name, spec, mode, bin_dir, out, env, timeout):
    d = out / name / mode
    d.mkdir(parents=True, exist_ok=True)
    e = dict(env)
    e.update(MODE_ENV[mode])
    log = d / "run.log"
    exe = bin_dir / spec["exe"]
    if not exe.exists():
        return dict(name=name, mode=mode, kind="exe", rc=-1, ok=False, fails=["missing " + str(exe)], wall=0, log=str(log))
    rc, wall = run_proc([str(exe)] + [str(a) for a in spec["args"]], SRC, e, log, timeout)
    text = log.read_text(errors="replace")
    _, times, fails = parse_log(text)
    keys = {k: v.strip() for k, v in re.findall(r"\|((?:CPU|GPU)[^|]*)\|\s*=\s*(\S+)", text)}
    return dict(name=name, mode=mode, kind="exe", rc=rc, wall=round(wall, 3), ok=(rc == 0 and not fails), fails=fails,
                counts=keys, times=times, log=str(log))


def run_cmd_case(name, spec, bin_dir, out, env, timeout):
    d = out / name
    d.mkdir(parents=True, exist_ok=True)
    log = d / "run.log"
    rc, wall = run_proc(spec["cmd"], SRC, env, log, timeout)
    text = log.read_text(errors="replace")
    nfail = len(re.findall(r"^\[FAIL\]", text, re.M))
    return dict(name=name, mode="-", kind="cmd", rc=rc, wall=round(wall, 3), ok=(rc == 0 and nfail == 0),
                fails=[l for l in text.splitlines() if l.startswith("[FAIL]")], log=str(log))


def run_oasis_case(name, spec, bin_dir, out, env, timeout, backends):
    """Run db_extent_by_cell_layer + db_hier_extent on each OASIS file for every CBLOCK backend; outputs must agree."""
    d = out / name
    d.mkdir(parents=True, exist_ok=True)
    res = dict(name=name, mode="-", kind="oasis", ok=True, fails=[], files={}, wall=0.0)
    for f in spec["files"]:
        if not Path(f).exists():
            res["files"][str(f)] = {"skipped": True}
            continue
        per = {}
        for be in backends:
            e = dict(env)
            if be != "default":
                e["LDB_CBLOCK_DECOMP"] = be
            outs = {}
            for tool in ("db_extent_by_cell_layer", "db_hier_extent"):
                exe = bin_dir / tool
                if not exe.exists():
                    exe = SRC / "db" / tool
                log = d / f"{Path(f).stem}.{tool}.{be}.log"
                rc, wall = run_proc([str(exe), str(f)], SRC, e, log, timeout)
                res["wall"] += wall
                text = log.read_text(errors="replace")
                # strip timing / backend-specific INFO noise before comparing
                body = "\n".join(l for l in text.splitlines()
                                 if not re.search(r"(time|seconds|INFO|CBLOCK|backend|nvcomp|ms\b)", l, re.I))
                outs[tool] = dict(rc=rc, wall=round(wall, 3), body_hash=hash(body), info=[
                    l for l in text.splitlines() if re.search(r"(INFO|CBLOCK)", l)][:4], body=body[-2000:])
                if rc != 0:
                    res["ok"] = False
                    res["fails"].append(f"{Path(f).name}:{tool}:{be} rc={rc}")
            per[be] = outs
        bes = list(per)
        for tool in ("db_extent_by_cell_layer", "db_hier_extent"):
            ref = per[bes[0]][tool]["body"]
            for be in bes[1:]:
                if per[be][tool]["body"] != ref:
                    res["ok"] = False
                    res["fails"].append(f"{Path(f).name}:{tool}: backend {be} differs from {bes[0]}")
        for be in per:
            for tool in per[be]:
                per[be][tool].pop("body_hash", None)
        res["files"][str(f)] = per
    res["wall"] = round(res["wall"], 3)
    return res


def digest_key(dg):
    return (dg["line"], dg["layer"])


def compare_digests(a, b):
    """Return list of mismatch strings between two digest lists (keyed by ctr line + layer)."""
    da = {digest_key(x): x for x in a}
    db = {digest_key(x): x for x in b}
    msgs = []
    for k in sorted(set(da) | set(db)):
        x, y = da.get(k), db.get(k)
        if x is None or y is None:
            msgs.append(f"line {k[0]} {k[1]}: present only in {'B' if x is None else 'A'}")
            continue
        if (x["n"], x["area"], x["hsum"], x["hxor"]) != (y["n"], y["area"], y["hsum"], y["hxor"]):
            msgs.append(f"line {k[0]} {k[1]}: A(n={x['n']},area={x['area']},h={x['hsum']}) "
                        f"B(n={y['n']},area={y['area']},h={y['hsum']})")
    return msgs


def cmd_run(args):
    bin_dir = Path(args.bin_dir).resolve()
    out = Path(args.out).resolve()
    out.mkdir(parents=True, exist_ok=True)
    env = base_env(bin_dir)
    for kv in args.env:
        k, _, v = kv.partition("=")
        env[k] = v
    modes = args.modes.split(",")
    backends = args.oasis_backends.split(",")
    results = []
    selected = [c for c in cases() if c[1] in TIERS[args.tier] and (not args.only or args.only in c[0])]
    for name, tier, kind, spec in selected:
        if kind == "ctr":
            if not Path(spec["ctr"]).exists():
                print(f"[SKIP] {name}: missing {spec['ctr']}")
                continue
            per_mode = {}
            for mode in modes:
                r = run_ctr_case(name, spec, mode, bin_dir, out, env, args.timeout)
                results.append(r)
                per_mode[mode] = r
                print(f"[{'PASS' if r['ok'] else 'FAIL'}] {name:32s} {mode:5s} rc={r['rc']:<4} {r['wall']:8.2f}s "
                      f"digests={len(r['digests'])} {' '.join(r['fails'])}", flush=True)
            if "gpu" in per_mode and "cpu" in per_mode:
                ga, ca = per_mode["gpu"]["digests"], per_mode["cpu"]["digests"]
                if not (per_mode["gpu"]["ok"] and per_mode["cpu"]["ok"]):
                    common = {digest_key(x) for x in ga} & {digest_key(x) for x in ca}
                    ga = [x for x in ga if digest_key(x) in common]
                    ca = [x for x in ca if digest_key(x) in common]
                mm = compare_digests(ga, ca)
                if mm:
                    per_mode["gpu"]["ok"] = False
                    per_mode["gpu"].setdefault("fails", []).append(f"GPU!=CPU digests ({len(mm)})")
                    print(f"[FAIL] {name:32s} GPU vs CPU digest mismatch: " + "; ".join(mm[:5]))
        elif kind == "exe":
            for mode in [m for m in modes if m != "check"]:
                r = run_exe_case(name, spec, mode, bin_dir, out, env, args.timeout)
                results.append(r)
                print(f"[{'PASS' if r['ok'] else 'FAIL'}] {name:32s} {mode:5s} rc={r['rc']:<4} {r['wall']:8.2f}s "
                      f"{r.get('counts', '')} {' '.join(r['fails'])}", flush=True)
        elif kind == "cmd":
            r = run_cmd_case(name, spec, bin_dir, out, env, args.timeout)
            results.append(r)
            print(f"[{'PASS' if r['ok'] else 'FAIL'}] {name:32s} rc={r['rc']:<4} {r['wall']:8.2f}s {' | '.join(r['fails'][:3])}", flush=True)
        elif kind == "oasis":
            r = run_oasis_case(name, spec, bin_dir, out, env, args.timeout, backends)
            results.append(r)
            print(f"[{'PASS' if r['ok'] else 'FAIL'}] {name:32s} {r['wall']:8.2f}s {' | '.join(r['fails'][:3])}", flush=True)
    for r in results:
        r["tier_run"] = args.tier
    (out / "results.json").write_text(json.dumps(results, indent=1))
    nfail = sum(1 for r in results if not r["ok"])
    print(f"== SUMMARY: {len(results) - nfail} PASS, {nfail} FAIL, results in {out / 'results.json'} ==")
    return nfail


def cmd_compare(args):
    A = json.loads(Path(args.base).read_text())
    B = json.loads(Path(args.new).read_text())
    ia = {(r["name"], r["mode"]): r for r in A}
    ib = {(r["name"], r["mode"]): r for r in B}
    nfail = 0
    for k in sorted(set(ia) & set(ib)):
        a, b = ia[k], ib[k]
        msgs = []
        if a["ok"] and not b["ok"]:
            msgs.append("REGRESSION: baseline passed, new fails: " + " ".join(b.get("fails", [])))
        if not a["ok"] and b["ok"]:
            msgs.append("FIXED: baseline failed, new passes")
        if a.get("kind") == "ctr" and a["ok"] and b["ok"]:
            mm = compare_digests(a["digests"], b["digests"])
            if mm:
                msgs.append(f"DIGEST DIFF ({len(mm)}): " + "; ".join(mm[:4]))
        elif a.get("kind") == "ctr" and not a["ok"] and not b["ok"] and a.get("digests") and b.get("digests"):
            # Both runs stopped early (pre-existing ctr problems): compare the common prefix.
            ka = {digest_key(x) for x in a["digests"]}
            kb = {digest_key(x) for x in b["digests"]}
            common = ka & kb
            mm = compare_digests([x for x in a["digests"] if digest_key(x) in common],
                                 [x for x in b["digests"] if digest_key(x) in common])
            if mm:
                msgs.append(f"DIGEST DIFF on common prefix ({len(mm)}): " + "; ".join(mm[:4]))
            else:
                msgs.append(f"common-prefix digests equal ({len(common)})")
        if a.get("kind") == "exe" and a.get("counts") != b.get("counts") and a["ok"]:
            msgs.append(f"COUNTS DIFF {a.get('counts')} -> {b.get('counts')}")
        bad = any(m.startswith(("REGRESSION", "DIGEST", "COUNTS")) for m in msgs)
        nfail += bad
        tag = "DIFF" if bad else ("NOTE" if msgs else "SAME")
        speed = ""
        if a.get("wall") and b.get("wall"):
            speed = f"{a['wall']:8.2f}s -> {b['wall']:8.2f}s ({a['wall'] / max(b['wall'], 1e-3):5.2f}x)"
        print(f"[{tag}] {k[0]:32s} {k[1]:5s} {speed} {' | '.join(msgs)}")
        if args.time and a.get("times") and b.get("times"):
            ta, tb = {}, {}
            for t in a["times"]:
                ta[t["op"]] = ta.get(t["op"], 0) + t["real"]
            for t in b["times"]:
                tb[t["op"]] = tb.get(t["op"], 0) + t["real"]
            for op in sorted(ta, key=lambda o: -ta[o])[:8]:
                if ta[op] > 0.05:
                    print(f"        {op:22s} {ta[op]:9.2f}s -> {tb.get(op, float('nan')):9.2f}s")
    for k in sorted(set(ia) - set(ib)):
        print(f"[MISS] {k[0]} {k[1]} only in baseline")
    print(f"== COMPARE: {nfail} case(s) differ/regress ==")
    return nfail


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("run")
    r.add_argument("--tier", default="quick", choices=list(TIERS))
    r.add_argument("--bin-dir", default=str(SRC))
    r.add_argument("--out", default=str(SRC / "regress/out"))
    r.add_argument("--modes", default="gpu,cpu")
    r.add_argument("--oasis-backends", default="serial,cpu,gpu,default")
    r.add_argument("--only", default="")
    r.add_argument("--env", action="append", default=[], help="extra KEY=VALUE for every run")
    r.add_argument("--timeout", type=int, default=7200)
    c = sub.add_parser("compare")
    c.add_argument("base")
    c.add_argument("new")
    c.add_argument("--time", action="store_true")
    a = ap.parse_args()
    sys.exit(min(cmd_run(a) if a.cmd == "run" else cmd_compare(a), 125))


if __name__ == "__main__":
    main()
