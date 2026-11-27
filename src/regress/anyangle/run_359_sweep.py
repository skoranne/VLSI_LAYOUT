#!/usr/bin/env python3
"""1-degree (0..359) rotation-invariance sweep for any OASIS/GDS file or preset designs.

Accepts either:
  - A normal OASIS/GDS file (flat or hierarchical): --input /path/to/layout.oas
  - Named presets: --designs SDT16,MW,FPU,MW_4x4 (also MW_8x8_1L, MW_8x8_5L, MW_8x8_10L, SDT16_6x6)

Rotation method: instantiates the top cell(s) in a wrapper cell with placement rotation
ICplxTrans(1.0, angle_deg, False, 0, 0), preserving full hierarchy unchanged.

Stages (each independently cached on disk; skipped if already complete unless --force):
  gen     : KLayout writes ROT_DIR/<TAG>_R<aaa>.oas for each missing angle
  merge   : Single-layer any-angle MERGE (TRAPMERGE.exe) + per-polygon rotational-invariant
            properties (AREA, PERIMETER, VERTICES) via MAGPARSER FASCL propdump (gzipped CSV)
  bool    : Two-layer any-angle Booleans (TRAPBOOL.exe -ops OR,AND,NOT,XOR,BNOTA,A,B)
            with exact algebraic identity checks (optional --bool-pnum for per-polygon CSVs)
  topo    : Two-layer any-angle Topologicals (MAGPARSER: INTERACT, NOT_INTERACT, CUT, TOUCH,
            INSIDE, OUTSIDE, OVERLAPS, COVERS, COVEREDBY, CONTAINS, DISJOINT, MEET, EQUAL)
  summary : Aggregate rotation-invariance report vs R000 (summary.tsv)

Examples:
  ./run_359_sweep.py --input /path/to/stdcell.oas --angles 0-359
  ./run_359_sweep.py --designs SDT16,MW,FPU,MW_4x4 --angles 0-359
  ./run_359_sweep.py --input /home/skoranne/MICROWATT.oas --angles 0-359 --force
"""
import argparse
import gzip
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
SRC = HERE.parent.parent

DEFAULT_MERGE_LAYERS = ["66/20", "65/20", "68/20"]
DEFAULT_BOOL_PAIRS = [("66/20", "65/20"), ("68/20", "69/20")]
DEFAULT_TOPO_PAIR = ("66/20", "65/20")

DEFAULT_DESIGNS = {
    "SDT16": "/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SMALL_SDT/SDT_16.oas",
    "MW": "/home/skoranne/MICROWATT.oas",
    "MW_4x4": "/home/skoranne/MW4x4.oas",
    "FPU": "/home/skoranne/FPU_8x8_FLAT.oas",
    "SDT16_6x6": "/home/skoranne/SDT16_6x6_L67_D20.oas",
    "MW_8x8_1L": "/scratch1/skoranne/LARGE_DESIGNS/MW_8x8/MW_8x8_1L.oas",
    "MW_8x8_5L": "/scratch1/skoranne/LARGE_DESIGNS/MW_8x8/MW_8x8_5L.oas",
    "MW_8x8_10L": "/scratch1/skoranne/LARGE_DESIGNS/MW_8x8/MW_8x8_10L.oas",
}

TOPO_OPS = [
    ("interact", "INTERACT"),
    ("not_interact", "NOT_INTERACT"),
    ("cut", "CUT"),
    ("touch", "TOUCH"),
    ("inside", "INSIDE"),
    ("outside", "OUTSIDE"),
    ("overlaps", "OVERLAPS"),
    ("covers", "COVERS"),
    ("coveredby", "COVEREDBY"),
    ("contains", "CONTAINS"),
    ("disjoint", "DISJOINT"),
    ("meet", "MEET"),
    ("equal", "EQUAL"),
]


def parse_angles(spec: str):
    out = []
    for part in spec.split(","):
        part = part.strip()
        if not part:
            continue
        if "-" in part:
            rng, *step_s = part.split(":")
            lo, hi = [int(x) for x in rng.split("-", 1)]
            step = int(step_s[0]) if step_s else 1
            out.extend(range(lo, hi + 1, step))
        else:
            out.append(int(part))
    seen = set()
    res = []
    for a in out:
        if a < 0 or a > 359:
            raise ValueError(f"Angle out of 0..359: {a}")
        if a not in seen:
            seen.add(a)
            res.append(a)
    return res


def parse_pairs(spec: str):
    pairs = []
    for item in spec.split(","):
        item = item.strip()
        if not item:
            continue
        la, lb = item.split(":", 1)
        pairs.append((la.strip(), lb.strip()))
    return pairs


def wait_mem(min_gb: int):
    if min_gb <= 0:
        return
    for _ in range(720):
        try:
            with open("/proc/meminfo") as f:
                for ln in f:
                    if ln.startswith("MemAvailable:"):
                        gb = int(ln.split()[1]) // 1048576
                        if gb >= min_gb:
                            return
        except OSError:
            return
        time.sleep(10)


def run_cmd(cmd, log_path: Path, cwd=None):
    log_path.parent.mkdir(parents=True, exist_ok=True)
    t0 = time.time()
    with open(log_path, "w") as lf:
        p = subprocess.run(cmd, stdout=lf, stderr=subprocess.STDOUT, cwd=cwd)
    return p.returncode, time.time() - t0


def gzip_file(src: Path, dst: Path):
    tmp = Path(str(dst) + ".tmp")
    with open(src, "rb") as fin, gzip.open(tmp, "wb", compresslevel=6) as fout:
        shutil.copyfileobj(fin, fout)
    os.replace(tmp, dst)
    src.unlink(missing_ok=True)


def stage_gen(tag: str, src: Path, angles: list, rot_dir: Path, force: bool, klayout: str, top: str):
    if not src.is_file():
        print(f"[gen] ERROR: missing source {src}", file=sys.stderr)
        return False
    missing = []
    for a in angles:
        p = rot_dir / f"{tag}_R{a:03d}.oas"
        if force or not (p.is_file() and p.stat().st_size > 0):
            missing.append(a)
    if not missing:
        print(f"[gen] {tag}: cached ({len(angles)} angles on disk)")
        return True
    print(f"[gen] {tag}: generating {len(missing)}/{len(angles)} rotated OASIS files...", flush=True)
    cmd = [
        klayout, "-b", "-r", str(HERE / "gen_rotated_1deg.py"),
        "-rd", f"src={src}",
        "-rd", f"out={rot_dir}",
        "-rd", f"name={tag}",
        "-rd", f"angles={','.join(str(a) for a in missing)}",
        "-rd", f"force={1 if force else 0}",
    ]
    if top:
        cmd += ["-rd", f"top={top}"]
    rc, dt = run_cmd(cmd, rot_dir / f"gen_{tag}.log")
    print(f"[gen] {tag}: rc={rc} ({dt:.1f}s)", flush=True)
    return rc == 0


def stage_merge(tag: str, merge_layers: list, a: int, oas_in: Path, out_dir: Path, args):
    d = out_dir / tag / "merge" / f"R{a:03d}"
    done = d / ".done"
    if done.is_file() and not (args.force or args.force_merge):
        return True
    d.mkdir(parents=True, exist_ok=True)
    wait_mem(args.min_mem_gb)

    tm_oas = d / "merge.oas"
    rc, dt = run_cmd([str(args.trapmerge), str(oas_in), str(tm_oas)] + merge_layers, d / "trapmerge.log")
    if rc != 0:
        print(f"[merge] {tag} R{a:03d}: TRAPMERGE rc={rc} ({dt:.1f}s)", flush=True)
        return False
    if not args.keep_oas:
        tm_oas.unlink(missing_ok=True)

    tmp_dir = d / "tmp"
    tmp_dir.mkdir(exist_ok=True)
    ctr = d / "merge_props.ctr"
    lines = [
        f"program MERGE_{tag}_R{a:03d}",
        f"var temp_folder {tmp_dir}/",
        "var temporary_layers 2",
        "var anyangle 1",
        f"decl design d1 = {oas_in}",
        "decl memory t1 = nothing",
        "exec init t1:nothing",
        "exec measure " + " ".join(f"d1:{ld.replace('/', ':')}" for ld in merge_layers),
    ]
    raw_csvs = []
    for ld in merge_layers:
        L, D = ld.split("/")
        csv_p = d / f"props_L{L}_D{D}.csv"
        raw_csvs.append(csv_p)
        lines.append(
            f"exec run PROPERTY:P_AREA,P_PERIM,P_VERT = d1:{L}:{D} "
            f"FASCL [ (id) P_AREA = AREA(id); P_PERIM = PERIMETER(id); P_VERT = VERTICES(id) ] t1:nothing"
        )
        lines.append(f"exec propdump d1:{L}:{D} {csv_p}")
    lines.append("end")
    ctr.write_text("\n".join(lines) + "\n")

    rc2, dt2 = run_cmd([str(args.magparser), str(ctr), "256"], d / "mag_merge.log", cwd=d)
    shutil.rmtree(tmp_dir, ignore_errors=True)
    if rc2 != 0:
        print(f"[merge] {tag} R{a:03d}: MAGPARSER rc={rc2} ({dt2:.1f}s)", flush=True)
        return False
    for csv_p in raw_csvs:
        if csv_p.is_file():
            gzip_file(csv_p, Path(str(csv_p) + ".gz"))
    done.write_text(f"ok trapmerge={dt:.2f}s mag={dt2:.2f}s\n")
    print(f"[merge] {tag} R{a:03d}: OK (trapmerge={dt:.1f}s, fascl={dt2:.1f}s)", flush=True)
    return True


def stage_bool(tag: str, bool_pairs: list, a: int, oas_in: Path, out_dir: Path, args):
    ok_all = True
    ops = "OR,AND,NOT,XOR,BNOTA,A,B"
    for la, lb in bool_pairs:
        ptag = f"{la.replace('/', '-')}_{lb.replace('/', '-')}"
        d = out_dir / tag / "bool" / ptag / f"R{a:03d}"
        done = d / ".done"
        if done.is_file() and not (args.force or args.force_bool):
            continue
        d.mkdir(parents=True, exist_ok=True)
        wait_mem(args.min_mem_gb)

        bout = d / "bool.oas"
        cmd = [str(args.trapbool), str(oas_in), str(bout), la, lb, "-ops", ops]
        if args.bool_pnum:
            cmd += ["-pnum", str(d / "pnum")]
        rc, dt = run_cmd(cmd, d / "bool.log")
        if rc != 0:
            print(f"[bool] {tag} {ptag} R{a:03d}: TRAPBOOL rc={rc} ({dt:.1f}s)", flush=True)
            ok_all = False
            continue
        for csv_p in d.glob("pnum_*.csv"):
            gzip_file(csv_p, Path(str(csv_p) + ".gz"))
        kl_dt = 0.0
        if args.klayout:
            rc_kl, kl_dt = run_cmd([
                args.klayout_bin, "-b", "-r", str(HERE / "kl_bool_check.py"),
                "-rd", f"fin={oas_in}", "-rd", f"fout={bout}",
                "-rd", f"a={la}", "-rd", f"b={lb}", "-rd", f"ops={ops}", "-rd", "out=1000",
            ], d / "kl.log")
            if rc_kl != 0:
                print(f"[bool] {tag} {ptag} R{a:03d}: KLayout rc={rc_kl}", flush=True)
                ok_all = False
        if not args.keep_oas:
            bout.unlink(missing_ok=True)
        done.write_text(f"ok bool={dt:.2f}s kl={kl_dt:.2f}s\n")
        print(f"[bool] {tag} {ptag} R{a:03d}: OK ({dt:.1f}s)", flush=True)
    return ok_all


def stage_topo(tag: str, topo_pair: tuple, a: int, oas_in: Path, out_dir: Path, args):
    d = out_dir / tag / "topo" / f"R{a:03d}"
    done = d / ".done"
    if done.is_file() and not (args.force or args.force_topo):
        return True
    d.mkdir(parents=True, exist_ok=True)
    wait_mem(args.min_mem_gb)

    la, lb = topo_pair
    LA, DA = la.split("/")
    LB, DB = lb.split("/")
    tmp_dir = d / "tmp"
    tmp_dir.mkdir(exist_ok=True)
    ctr = d / "topo.ctr"
    lines = [
        f"program TOPO_{tag}_R{a:03d}",
        f"var temp_folder {tmp_dir}/",
        "var temporary_layers 2",
        "var anyangle 1",
        f"decl design d1 = {oas_in}",
        "decl memory t1 = nothing",
        "exec init t1:nothing",
        f"exec measure d1:{LA}:{DA} d1:{LB}:{DB}",
    ]
    for name, op in TOPO_OPS:
        lines.append(f"exec run t1:{name} = d1:{LA}:{DA} {op} d1:{LB}:{DB}")
    lines.append("exec measure " + " ".join(f"t1:{name}" for name, _ in TOPO_OPS))
    lines.append("end")
    ctr.write_text("\n".join(lines) + "\n")

    rc, dt = run_cmd([str(args.magparser), str(ctr), "256"], d / "topo.log", cwd=d)
    shutil.rmtree(tmp_dir, ignore_errors=True)
    if rc != 0:
        print(f"[topo] {tag} R{a:03d}: MAGPARSER rc={rc} ({dt:.1f}s)", flush=True)
        return False
    done.write_text(f"ok topo={dt:.2f}s\n")
    print(f"[topo] {tag} R{a:03d}: OK ({dt:.1f}s)", flush=True)
    return True


def parse_measures(log_path: Path):
    m = {}
    if not log_path.is_file():
        return m
    for ln in log_path.read_text(errors="replace").splitlines():
        if not ln.startswith("MEASURE "):
            continue
        ml = re.search(r"layer=(\S+)", ln)
        if not ml:
            continue
        lay = ml.group(1)
        vals = {k: int(float(v)) for k, v in re.findall(r"(polygons|holes|vertices|area|perimeter)=([-0-9.]+)", ln)}
        if len(vals) == 5:
            m[lay] = vals
    return m


def parse_trapbool(log_path: Path):
    m = {}
    if not log_path.is_file():
        return m
    for ln in log_path.read_text(errors="replace").splitlines():
        if ln.startswith("TRAPBOOL op="):
            mo = re.search(r"op=(\S+).*?area2=(\d+).*?anomalies=(\d+)", ln)
            if mo:
                m[mo.group(1)] = {"area2": int(mo.group(2)), "anomalies": int(mo.group(3))}
    return m


def stage_summary(tags: list, angles: list, out_dir: Path):
    rows = ["design\tstage\tlayer_or_op\tangle\tpolygons\tref_polygons\tarea\tref_area\trel_dA\tstatus"]
    for tag in tags:
        ref_m = parse_measures(out_dir / tag / "merge" / "R000" / "mag_merge.log")
        for a in angles:
            cur = parse_measures(out_dir / tag / "merge" / f"R{a:03d}" / "mag_merge.log")
            for lay, v in sorted(cur.items()):
                r = ref_m.get(lay, v)
                rel = (v["area"] - r["area"]) / max(1, r["area"])
                st = "PASS" if abs(rel) <= 1e-3 and v["polygons"] == r["polygons"] else "CHECK"
                rows.append(f"{tag}\tmerge\t{lay}\t{a:03d}\t{v['polygons']}\t{r['polygons']}\t{v['area']}\t{r['area']}\t{rel:.3e}\t{st}")

        broot = out_dir / tag / "bool"
        if broot.is_dir():
            for pdir in sorted(broot.iterdir()):
                if not pdir.is_dir():
                    continue
                ref_b = parse_trapbool(pdir / "R000" / "bool.log")
                for a in angles:
                    cur = parse_trapbool(pdir / f"R{a:03d}" / "bool.log")
                    for op, v in sorted(cur.items()):
                        r = ref_b.get(op, v)
                        rel = (v["area2"] - r["area2"]) / max(1, r["area2"])
                        st = "PASS" if v["anomalies"] == 0 and abs(rel) <= 1e-3 else "CHECK"
                        rows.append(f"{tag}\tbool:{pdir.name}\t{op}\t{a:03d}\t-\t-\t{v['area2']}\t{r['area2']}\t{rel:.3e}\t{st}")

        ref_t = parse_measures(out_dir / tag / "topo" / "R000" / "topo.log")
        for a in angles:
            cur = parse_measures(out_dir / tag / "topo" / f"R{a:03d}" / "topo.log")
            for lay, v in sorted(cur.items()):
                if not lay.startswith("t1:"):
                    continue
                r = ref_t.get(lay, v)
                rel = (v["area"] - r["area"]) / max(1, r["area"])
                st = "PASS" if abs(rel) <= 1e-3 and v["polygons"] == r["polygons"] else "CHECK"
                rows.append(f"{tag}\ttopo\t{lay}\t{a:03d}\t{v['polygons']}\t{r['polygons']}\t{v['area']}\t{r['area']}\t{rel:.3e}\t{st}")

    summary_path = out_dir / "summary.tsv"
    summary_path.parent.mkdir(parents=True, exist_ok=True)
    summary_path.write_text("\n".join(rows) + "\n")
    print(f"[summary] wrote {summary_path} ({len(rows) - 1} records)")


def main():
    ap = argparse.ArgumentParser(description="Resumable 1-degree (0..359) any-angle accuracy sweep")
    ap.add_argument("--input", default="", help="Path to any OASIS/GDS file (flat or hierarchical)")
    ap.add_argument("--name", default="", help="Tag name for --input (default: input file stem)")
    ap.add_argument("--top", default="", help="Optional top cell name for --input")
    ap.add_argument("--designs", default="SDT16,MW,FPU,MW_4x4",
                    help="Comma-separated preset designs when --input is omitted")
    ap.add_argument("--merge-layers", default=",".join(DEFAULT_MERGE_LAYERS),
                    help="Comma-separated L/D list for merge stage (default: 66/20,65/20,68/20)")
    ap.add_argument("--bool-pairs", default=",".join(f"{a}:{b}" for a, b in DEFAULT_BOOL_PAIRS),
                    help="Comma-separated LA/DA:LB/DB pairs for bool stage (default: 66/20:65/20,68/20:69/20)")
    ap.add_argument("--topo-pair", default=f"{DEFAULT_TOPO_PAIR[0]}:{DEFAULT_TOPO_PAIR[1]}",
                    help="LA/DA:LB/DB pair for topo stage (default: 66/20:65/20)")
    ap.add_argument("--angles", default="0-359", help="Angles spec: 0-359, 0-359:15, 0,1,2 (default: 0-359)")
    ap.add_argument("--stages", default="gen,merge,bool,topo,summary", help="Stages to run")
    ap.add_argument("--rot-dir", default="/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/ROTATION_1DEG")
    ap.add_argument("--out-dir", default=str(HERE / "out" / "sweep359"))
    ap.add_argument("--force", action="store_true", help="Recompute all stages even if outputs exist on disk")
    ap.add_argument("--force-gen", action="store_true", help="Regenerate rotated OASIS files")
    ap.add_argument("--force-merge", action="store_true", help="Recompute single-layer merge + properties")
    ap.add_argument("--force-bool", action="store_true", help="Recompute two-layer Booleans")
    ap.add_argument("--force-topo", action="store_true", help="Recompute two-layer Topologicals")
    ap.add_argument("--bool-pnum", action="store_true", help="Also write per-polygon PNUM CSVs in bool stage")
    ap.add_argument("--klayout", action="store_true", help="Also run KLayout cross-check in bool stage")
    ap.add_argument("--keep-oas", action="store_true", help="Keep intermediate merge.oas/bool.oas files")
    ap.add_argument("--min-mem-gb", type=int, default=24, help="Wait until MemAvailable >= N GB")
    ap.add_argument("--klayout-bin", default="klayout")
    ap.add_argument("--magparser", default=str(SRC / "MAGPARSER.exe"))
    ap.add_argument("--trapmerge", default=str(SRC / "TRAPMERGE.exe"))
    ap.add_argument("--trapbool", default=str(SRC / "TRAPBOOL.exe"))
    args = ap.parse_args()

    merge_layers = [x.strip() for x in args.merge_layers.split(",") if x.strip()]
    bool_pairs = parse_pairs(args.bool_pairs)
    topo_pair = parse_pairs(args.topo_pair)[0]
    angles = parse_angles(args.angles)
    stages = {s.strip() for s in args.stages.split(",") if s.strip()}
    rot_dir = Path(args.rot_dir)
    out_dir = Path(args.out_dir)

    jobs = []
    if args.input:
        src = Path(args.input).resolve()
        tag = args.name.strip() or src.stem
        jobs.append((tag, src))
    else:
        for t in [x.strip() for x in args.designs.split(",") if x.strip()]:
            if t not in DEFAULT_DESIGNS:
                ap.error(f"Unknown design {t}; choose from {','.join(DEFAULT_DESIGNS)} or use --input")
            jobs.append((t, Path(DEFAULT_DESIGNS[t])))

    for tag, src in jobs:
        if "gen" in stages:
            stage_gen(tag, src, angles, rot_dir, args.force or args.force_gen, args.klayout_bin, args.top)
        for a in angles:
            oas_in = rot_dir / f"{tag}_R{a:03d}.oas"
            if not oas_in.is_file():
                print(f"[skip] missing {oas_in}")
                continue
            if "merge" in stages:
                stage_merge(tag, merge_layers, a, oas_in, out_dir, args)
            if "bool" in stages:
                stage_bool(tag, bool_pairs, a, oas_in, out_dir, args)
            if "topo" in stages:
                stage_topo(tag, topo_pair, a, oas_in, out_dir, args)

    if "summary" in stages:
        stage_summary([t for t, _ in jobs], angles, out_dir)


if __name__ == "__main__":
    main()
