#!/bin/bash
# Viewer benchmarks on the MW_8x8 strict OASIS files (see MAGVIEWER_TODO.md section 6).
#   bench_mw8x8.sh [OUT] [files...]    default files: /scratch1/skoranne/LARGE_DESIGNS/MW_8x8/MW_8x8_{1,5,10}L.oas
# Per file, one heavy job at a time, each under guard.sh (MemAvailable watchdog):
#   1. db_oasinfo     table-only open (strict tables, layer names, cell bbox)
#   2. test_viewer_index.exe  headless load + index + 200 pan/zoom windows (TVI_VERIFY=0)
#   3. MAGVIEWER.exe  MAGVIEWER_BENCH=1 scripted zoom/pan path, only when DISPLAY is set
#                     (MAGVIEWER_EGL=1 is passed for X servers without GLX)
#   4. KLayout (system 0.27.8, Ruby) full load, only with BENCH_KLAYOUT=1
# Writes OUT/<file>.<step>.log and OUT/summary.txt.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
OUT=${1:-$HERE/out}; shift || true
FILES=("$@")
[ ${#FILES[@]} -eq 0 ] && FILES=(/scratch1/skoranne/LARGE_DESIGNS/MW_8x8/MW_8x8_1L.oas \
                                 /scratch1/skoranne/LARGE_DESIGNS/MW_8x8/MW_8x8_5L.oas \
                                 /scratch1/skoranne/LARGE_DESIGNS/MW_8x8/MW_8x8_10L.oas)
mkdir -p "$OUT"
export OMP_PROC_BIND=TRUE OMP_PLACES=cores NVCOMPILER_ACC_MEM_MANAGE=0
S=$OUT/summary.txt
for f in "${FILES[@]}"; do
  b=$(basename "$f" .oas)
  echo "== $b ($(du -h "$f" | cut -f1))" | tee -a "$S"
  "$SRC/db/db_oasinfo" --verify-strict "$f" > "$OUT/$b.oasinfo.log" 2>&1
  grep -E "OPEN_MS|STRICT |VERIFY|^CELL '" "$OUT/$b.oasinfo.log" | sed 's/^/  tables: /' | tee -a "$S"
  "$HERE/guard.sh" "$OUT/$b.tvi.log" "$SRC/test_viewer_index.exe" "$f" 64 200 0.001
  grep -E "MAGVIEWER read|^LOAD|^QUERY|^RC" "$OUT/$b.tvi.log" | sed 's/^/  headless: /' | tee -a "$S"
  if [ -n "${DISPLAY:-}" ]; then
    MAGVIEWER_BENCH=1 MAGVIEWER_EGL=${MAGVIEWER_EGL:-1} MAGVIEWER_SCREENSHOT="$OUT/$b.bench.ppm" \
      "$HERE/guard.sh" "$OUT/$b.gui.log" "$SRC/MAGVIEWER.exe" "$f"
    grep -E "^PERF|MAGVIEWER read|^RC" "$OUT/$b.gui.log" | sed 's/^/  gui: /' | tee -a "$S"
    awk '/^BENCH/{n++; q+=$6; d+=$8; if ($6+$8>m) m=$6+$8} END{if(n) printf "  gui: bench steps %d  mean query %.1f ms  mean draw %.1f ms  max frame %.1f ms\n", n, q/n, d/n, m}' \
      "$OUT/$b.gui.log" | tee -a "$S"
  fi
  if [ "${BENCH_KLAYOUT:-0}" = 1 ]; then
    "$HERE/guard.sh" "$OUT/$b.klayout.log" /usr/bin/klayout -b -r "$SRC/db/regressions/oasis_layer_summary.rb" -rd f="$f"
    grep -E "READ_S|^RC|WATCHDOG" "$OUT/$b.klayout.log" | sed 's/^/  klayout: /' | tee -a "$S"
  fi
done
