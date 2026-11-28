#!/bin/bash
# OVER_UNDER / UNDER_OVER 1dbu offset regression: Clipper2 reference vs MAGPARSER TrapMorph.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../../.." && pwd)
SCRATCH=/scratch1/skoranne/CURVILINEAR_KERNELS/bench/mems_testcases
WORK=${1:-$(mktemp -d /tmp/nurbs_ou_XXXXXX)}
mkdir -p "$WORK"
status=0

python3 "$HERE/over_under_ref.py" "$HERE/testcases" "$WORK" 1.0 > "$WORK/ref.log" 2>&1 || { cat "$WORK/ref.log"; exit 1; }
cat "$WORK/ref.log"

if [ -x "$SRC/test_nurbs_offset.exe" ]; then
  "$SRC/test_nurbs_offset.exe" "$WORK" > "$WORK/mag.log" 2>&1 || { cat "$WORK/mag.log"; status=1; }
  cat "$WORK/mag.log"
  # compare
  python3 - <<PY || status=1
import json,sys
ref=json.load(open('$WORK/ref.json'))
mag=json.load(open('$WORK/mag.json'))
for k in ref:
    r=ref[k]; m=mag.get(k)
    if not m: print('MISS',k); sys.exit(1)
    for op in ['over_under','under_over']:
        ra=r[op]['area']; ma=m[op]['area']
        # tessellated polygon comparison: allow small topological difference
        pct=abs(ra-ma)/max(abs(ra),1.0)*100
        print(f'{k} {op} ref={ra:.6f} mag={ma:.6f} diff_pct={pct:.4f}')
        if pct>2.0: print('FAIL',k,op); sys.exit(1)
print('COMPARE PASS')
PY
else
  echo "SKIP: test_nurbs_offset.exe not built (MAGPARSER comparison unavailable)"
fi
exit $status
