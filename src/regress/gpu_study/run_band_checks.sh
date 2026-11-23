#!/bin/bash
# Banded-execution correctness checks on SMALL_SDT (each run memory-guarded by run_study.sh):
#   check_band_ref      unbanded reference outputs (GROW, SIZE +/-, FRAMENOT, AND) -> KLBIN
#   check_band_cmp      the same ops banded (100K-box bands); XOR vs reference must be empty
#   check_band_interact banded vs unbanded INTERACT and GROW in one run (var band_boxes)
#   check_small_oas     set identities with forced banding
# Extra env (e.g. MAGPARSER_COMPACT_FORCE_WIDE=1, MAGPARSER_BAND_MERGE_MAX=1) is passed through.
# Usage: run_band_checks.sh [BIN] [OUTROOT]
cd "$(dirname "$0")"
BIN=${1:-../../MAGPARSER.exe}; OUT=${2:-out_band_checks}
R=/tmp/magparser_band_check; mkdir -p $R
{ echo magic; echo tech sky130A; echo timestamp 1
  for l in g s sn fn an; do echo "<< $l >>"; echo "KLBIN $R/ref_$l.bin"; done; echo "<< end >>"; } > $R/band_ref.mag
F="MAGPARSER_XOR_VIA_NOT=1 MAGPARSER_OR_VIA_NOT=1 MAGPARSER_PRESORT_GPU=1 MAGPARSER_GPU_MEM_BUDGET=4"
C="MAGPARSER_COMPACT_LAYERS=1 MAGPARSER_COMPACT_BUDGET_GB=0.05"
rc=0
run() { MAXL=40 ./run_study.sh "$BIN" "$OUT" "$@" >/dev/null 2>&1
        r=$(tail -1 "$OUT/$1/gpu/run.meta" | awk '{print $2}'); echo "$1: rc=$r"; [ "$r" = 0 ] || rc=1; }
run check_band_ref gpu $F MAGPARSER_BAND_BOXES=0
run check_band_cmp gpu $F $C MAGPARSER_BAND_BOXES=100000
run check_band_interact gpu $F $C
run check_small_oas gpu $F $C MAGPARSER_BAND_BOXES=100000
exit $rc
