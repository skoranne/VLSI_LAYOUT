#!/bin/bash
# FASCL per-polygon property regression: GDS input -> MAGPARSER FASCL lambdas -> OASIS output,
# verified with KLayout (geometry XOR, every property value, STRICT tables, CBLOCK).
#
#   bash src/db/regressions/fascl/run_fascl_regression.sh [MAGPARSER.exe] [workdir]
#
# KLAYOUT may point at a KLayout >= 0.28 with Python; the default is the 0.30.3 build from AGENTS.md.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../../.." && pwd)
EXE=${1:-$SRC/MAGPARSER.exe}
WORK=${2:-$(mktemp -d /tmp/fascl_regr_XXXXXX)}
KL_HOME=/scratch1/skoranne/OSS_EDA_TOOLS/klayout-0.30.3/bin-release
KLAYOUT=${KLAYOUT:-$KL_HOME/klayout}
if [ -z "${KLAYOUT_NO_LDPATH:-}" ] && [ -d "$KL_HOME" ]; then
  export LD_LIBRARY_PATH="$KL_HOME/:/usr/local/lib64/:/scratch1/skoranne/intel/oneapi/mpi/2021.14/opt/mpi/libfabric/lib:/scratch1/skoranne/intel/oneapi/mpi/2021.14/lib:/scratch1/skoranne/intel/oneapi/umf/0.9/lib:/scratch1/skoranne/intel/oneapi/compiler/2025.0/opt/compiler/lib:/scratch1/skoranne/intel/oneapi/compiler/2025.0/lib:/scratch1/skoranne/intel/oneapi/tbb/2022.0/lib:/scratch1/skoranne/CAD/CONDA/envs/idp/lib:${LD_LIBRARY_PATH:-}"
fi
mkdir -p "$WORK" && cd "$WORK" || exit 2
echo "workdir: $WORK"
rm -f fascl_*.oas fascl_*.csv
"$KLAYOUT" -b -r "$HERE/gen_fascl_input.py" -rd out=fascl_in.gds || exit 2
cp "$HERE/fascl_props.ctr" "$HERE/fascl_out.mag" .
"$EXE" fascl_props.ctr 16 > magparser.log 2>&1
rc=$?
if [ $rc -ne 0 ]; then echo "MAGPARSER failed ($rc), see $WORK/magparser.log"; tail -20 magparser.log; exit 1; fi

status=0
check() {   # oas layers strict cblock rep (1 required / 0 forbidden / - unchecked)
  "$KLAYOUT" -b -r "$HERE/fascl_check.py" -rd ref=fascl_in.gds -rd oas="$1" -rd layers="$2" \
      -rd csv68=fascl_m1.csv -rd csv69=fascl_m2.csv -rd strict="$3" -rd cblock="$4" -rd rep="$5" \
      -rd prepeat="${6:-}" -rd mincblocks="${7:-}" || status=1
}
check fascl_full.oas         68/20,69/20,70/20 1 1 1     # STRICT + CBLOCK + repetitions (defaults)
check fascl_smallcb.oas      68/20,69/20,70/20 1 1 1 "" 20  # 64-byte CBLOCKs split on record boundaries
check fascl_norep.oas        68/20,69/20,70/20 1 1 0 1   # no repetitions: PROPERTY-REPEAT on 70/20
check fascl_plain_m1.oas     68/20       0 0 -   # no CBLOCK, offsets in START (68/20 props are unique)
check fascl_copy_L68_D20.oas 68/20       1 1 -   # COPY into an output db, .mag names an .oas file

full=$(stat -c %s fascl_full.oas); norep=$(stat -c %s fascl_norep.oas)
if [ "$full" -ge "$norep" ]; then
  echo "FAIL: repetitions did not shrink the file ($full >= $norep bytes)"; status=1
else
  echo "repetitions: $full bytes vs $norep bytes without"
fi
# the lambda engine must reject bad programs with a clear error
cat > fascl_bad.ctr <<'EOF'
program fascl_bad
decl design d1 = fascl_in.gds
PROPERTY:X = d1:L68_20 FASCL [ (id) X = "a" * 2 ] d1:nothing
end
EOF
if "$EXE" fascl_bad.ctr 16 > bad.log 2>&1; then
  echo "FAIL: a lambda runtime type error was not reported"; status=1
elif ! grep -q "FASCL runtime error" bad.log; then
  echo "FAIL: missing FASCL runtime error message"; tail -5 bad.log; status=1
else
  echo "runtime error reported: $(grep -m1 'FASCL runtime error' bad.log)"
fi
if [ $status -eq 0 ]; then echo "FASCL REGRESSION PASSED"; else echo "FASCL REGRESSION FAILED"; fi
exit $status
