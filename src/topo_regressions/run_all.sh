#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
SRC="$ROOT/src"
EXE="$SRC/MAGPARSER.exe"
export DYLD_LIBRARY_PATH=/opt/intel/oneapi/intelpython/python3.9/envs/2022.2.1/lib:/usr/local/opt/zstd/lib:/opt/intel/oneapi/compiler/2022.2.1/mac/compiler/lib:/usr/lib
if [[ -z "${OMP_NUM_THREADS:-}" ]]; then export OMP_NUM_THREADS="$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"; fi
CTR_DIR="$SRC/topo_regressions/ctrs"
pass=0; fail=0; total=0
start=$(date +%s)
for ctr in "$CTR_DIR"/batch_*.ctr "$CTR_DIR"/extra_*.ctr; do
  total=$((total+1))
  name=$(basename "$ctr")
  out="/tmp/topo_${name}.log"
  if "$EXE" "$ctr" 10 >"$out" 2>&1; then
    # also ensure no ASSERT_ZERO fail messages (error stop would have caused nonzero, but be safe)
    if grep -q "ASSERT_ZERO fail" "$out"; then
      echo "[FAIL] $name (assert message)"; fail=$((fail+1))
    else
      echo "[PASS] $name"; pass=$((pass+1))
    fi
  else
    rc=$?
    echo "[FAIL] $name rc=$rc"; tail -20 "$out" || true; fail=$((fail+1))
  fi
done
end=$(date +%s)
echo "== TOPO SUMMARY: PASS=$pass FAIL=$fail TOTAL=$total TIME=$((end-start))s =="
exit $fail
