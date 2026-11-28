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

full=$(stat -f %z fascl_full.oas 2>/dev/null || stat -c %s fascl_full.oas); norep=$(stat -f %z fascl_norep.oas 2>/dev/null || stat -c %s fascl_norep.oas)
if [ "$full" -ge "$norep" ]; then
  echo "FAIL: repetitions did not shrink the file ($full >= $norep bytes)"; status=1
else
  echo "repetitions: $full bytes vs $norep bytes without"
fi
# outline builtins and CONNECT / NET_ID, recomputed independently in KLayout
"$KLAYOUT" -b -r "$HERE/fascl_geom_check.py" -rd ref=fascl_in.gds -rd refnet=fascl_in_net.gds \
    -rd geom68=fascl_geom68.csv -rd geom71=fascl_geom71.csv -rd net71=fascl_net71.csv \
    -rd net72=fascl_net72.csv -rd net73=fascl_net73.csv -rd net73b=fascl_net73b.csv || status=1
if grep -q "member geometry changed, recomputing nets" magparser.log; then
  echo "nets recomputed after a member layer changed"
else
  echo "FAIL: CONNECT nets were not recomputed after the via layer changed"; status=1
fi
# a statement that must fail with the given message
expect_error() {   # name message ctr-body-line
  printf 'program %s\ndecl design d1 = fascl_in.gds\ndecl design d2 = fascl_in_net.gds\n%s\nend\n' "$1" "$3" > "$1.ctr"
  if "$EXE" "$1.ctr" 16 > "$1.log" 2>&1; then
    echo "FAIL: $1 did not fail"; status=1
  elif ! grep -q "$2" "$1.log"; then
    echo "FAIL: $1: missing message '$2'"; tail -3 "$1.log"; status=1
  else
    echo "$1 rejected: $(grep -m1 "$2" "$1.log" | sed 's/^ *//')"
  fi
}
expect_error fascl_nonet "not part of a CONNECT" \
  'PROPERTY:N = d1:L70_20 FASCL [ (id) NET_ID(id) ] d1:nothing'
expect_error fascl_conn2 "expected routing cut routing" 'CONNECT d2:L71_20 d2:L72_20'
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
