#!/usr/bin/env python3
"""Summarize GPU-study runs produced by run_study.sh.

  summarize.py OUTROOT [OUTROOT2 ...]          per-case/mode tables (markdown) + JSON on stdout with --json

For every OUTROOT/<case>/<mode>/ it parses:
  run.log    per-op "<name> <cpu> CPU seconds. <wall> REAL seconds." lines (StartMarkTime/StopMarkTime),
             PHASE_SUMMARY lines (MAGPARSER_PHASE_TIMERS=1), GPU residency stats
  time.txt   /usr/bin/time -v (max RSS, wall)
  mem.trace  MemAvailable samples -> peak system memory consumed by the run
  run.meta   exit code / watchdog kills
  stats_*.csv  nsys stats reports (kernel time totals), if present
"""
import csv
import json
import re
import sys
from collections import OrderedDict
from pathlib import Path

OP_RE = re.compile(r"^\s*(\S.{0,19}?)\s+([0-9.]+) CPU seconds\.\s+([0-9.]+) REAL seconds\.", re.M)
PH_RE = re.compile(r"^PHASE_SUMMARY (\S+)\s+(\d+)\s+([0-9.]+)\s+(\d+)", re.M)


def parse_run(d: Path):
    r = OrderedDict(path=str(d))
    log = (d / "run.log").read_text(errors="replace") if (d / "run.log").exists() else ""
    ops = OrderedDict()
    for m in OP_RE.finditer(log):
        name = m[1].strip()
        o = ops.setdefault(name, {"calls": 0, "cpu": 0.0, "wall": 0.0})
        o["calls"] += 1
        o["cpu"] += float(m[2])
        o["wall"] += float(m[3])
    r["ops"] = ops
    r["phases"] = OrderedDict((m[1], {"calls": int(m[2]), "wall": float(m[3]), "items": int(m[4])})
                              for m in PH_RE.finditer(log))
    meta = (d / "run.meta").read_text() if (d / "run.meta").exists() else ""
    m = re.search(r"RC (-?\d+)", meta)
    r["rc"] = int(m[1]) if m else None
    r["watchdog"] = "WATCHDOG" in meta
    t = (d / "time.txt").read_text() if (d / "time.txt").exists() else ""
    m = re.search(r"Maximum resident set size \(kbytes\): (\d+)", t)
    r["max_rss_gb"] = round(int(m[1]) / 1048576, 2) if m else None
    m = re.search(r"Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): (\S+)", t)
    if m:
        parts = [float(x) for x in m[1].split(":")]
        r["wall_s"] = round(sum(p * 60 ** i for i, p in enumerate(reversed(parts))), 1)
    tr = d / "mem.trace"
    if tr.exists():
        mb = [int(l.split()[2]) for l in tr.read_text().splitlines() if len(l.split()) >= 3]
        if mb:
            r["sys_mem_used_peak_gb"] = round((mb[0] - min(mb)) / 1024, 1)
    kern = {}
    for f in d.glob("stats_cuda_gpu_kern_sum.csv"):
        with open(f) as fh:
            for row in csv.DictReader(fh):
                kern[row.get("Name", "?")[:60]] = float(row.get("Total Time (ns)", 0)) / 1e9
    if kern:
        r["gpu_kernels_s"] = dict(sorted(kern.items(), key=lambda kv: -kv[1])[:15])
    return r


def table(runs):
    out = []
    for case, modes in runs.items():
        out.append(f"\n### {case}\n")
        names = []
        for r in modes.values():
            for n in r["ops"]:
                if n not in names:
                    names.append(n)
        hdr = "| op | " + " | ".join(f"{m} wall s" for m in modes) + (" | gpu/cpu |" if {"gpu", "cpu"} <= modes.keys() else " |")
        out.append(hdr)
        out.append("|" + "---|" * (hdr.count("|") - 1))
        for n in names:
            vals = [modes[m]["ops"].get(n, {}).get("wall") for m in modes]
            row = f"| {n} | " + " | ".join("" if v is None else f"{v:.2f}" for v in vals)
            if {"gpu", "cpu"} <= modes.keys():
                g = modes["gpu"]["ops"].get(n, {}).get("wall")
                c = modes["cpu"]["ops"].get(n, {}).get("wall")
                row += " | " + (f"{c / g:.2f}x" if g and c and g > 0.005 else "")
            out.append(row + " |")
        for m, r in modes.items():
            out.append(f"\n{m}: rc={r['rc']} watchdog={r['watchdog']} wall={r.get('wall_s')}s "
                       f"maxRSS={r.get('max_rss_gb')}GB sysmem_peak={r.get('sys_mem_used_peak_gb')}GB")
            if r["phases"]:
                out.append("  phases: " + ", ".join(f"{k}={v['wall']:.1f}s/{v['calls']}" for k, v in r["phases"].items()))
    return "\n".join(out)


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    runs = OrderedDict()
    for root in args:
        for d in sorted(Path(root).glob("*/*/")):
            if (d / "run.log").exists():
                runs.setdefault(f"{Path(root).name}/{d.parent.name}", OrderedDict())[d.name] = parse_run(d)
    if "--json" in sys.argv:
        print(json.dumps(runs, indent=1))
    else:
        print(table(runs))


if __name__ == "__main__":
    main()
