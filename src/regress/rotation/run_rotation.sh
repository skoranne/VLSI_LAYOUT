#!/bin/bash
# Rotation-invariance regression.
#   run_rotation.sh TAG [ANGLES]       e.g.  run_rotation.sh SDT16S "0 15 30 ... 180"
# Inputs: $ROT_DIR/TAG_R<aaa>.oas from gen_rotated.py (TAG_R000 is the unrotated baseline).
# For each angle, under a MemAvailable watchdog, one job at a time:
#   1. db_rotcheck --mode poly   exact flatten (LDB_OPEN_SHAPES polygons) vs baseline rotated here
#   2. db_rotcheck --mode box    the box iterator MAGPARSER loads with
#   3. MAGPARSER on rot_ops.ctr.in   (MEASURE lines, LAYER_DIGEST)
#   4. KLayout kl_measure.py         (KLMEASURE lines: ground truth on the same file)
# Results in $OUT/TAG/R<aaa>/{rot_poly,rot_box,mag,kl}.log; compare.py summarises.
# Env: ROT_DIR, OUT, SKIP_KL=1, SKIP_MAG=1, SKIP_DB=1, MIN_AVAIL_GB (default 12),
#      MAG_ORTHO_ONLY=1 (MAGPARSER only at multiples of 90: other angles are strip-approximated
#      box data, several times larger; used for the large design)
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
TAG=$1
ANGLES=${2:-"0 15 30 45 60 75 90 105 120 135 150 165 180"}
ROT_DIR=${ROT_DIR:-/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/ROTATION}
OUT=${OUT:-$HERE/out}
MIN_AVAIL_GB=${MIN_AVAIL_GB:-12}
LAYERS=65/20,66/20,67/20,68/20,69/20,66/44,67/44

# guarded LOG CMD...: run CMD (stdout to LOG, stderr to LOG.err); SIGKILL it if MemAvailable
# drops below MIN_AVAIL_GB. Resumable: a step whose LOG ends in "RC 0" is not run again.
# Before starting, wait (up to WAIT_MIN minutes) until MemAvailable >= MIN_AVAIL_GB + START_GB;
# a step killed by the watchdog is retried once.
START_GB=${START_GB:-8}
WAIT_MIN=${WAIT_MIN:-60}
avail_gb() { awk '/MemAvailable/{print int($2/1048576)}' /proc/meminfo; }
guarded() {
  local log=$1; shift
  if [ -f "$log" ] && tail -1 "$log" | grep -q '^RC 0$' && ! grep -q WATCHDOG "$log"; then return 0; fi
  local try
  for try in 1 2; do
    local w=0
    while [ "$(avail_gb)" -lt $((MIN_AVAIL_GB + START_GB)) ] && [ $w -lt $((WAIT_MIN * 6)) ]; do sleep 10; w=$((w+1)); done
    guarded_once "$log" "$@"
    grep -q WATCHDOG "$log" || return 0
    echo "RETRY $log after watchdog kill" >&2
  done
}
guarded_once() {
  local log=$1; shift
  /usr/bin/time -f "TIME %e s MAXRSS %M KB" "$@" > "$log" 2> "$log.err" &
  local pid=$!
  ( while kill -0 $pid 2>/dev/null; do
      av=$(awk '/MemAvailable/{print int($2/1048576)}' /proc/meminfo)
      if [ "$av" -lt "$MIN_AVAIL_GB" ]; then
        echo "WATCHDOG KILL avail=${av}GB" >> "$log"; pkill -KILL -P $pid; kill -KILL $pid; break
      fi
      sleep 0.5
    done ) &
  local wd=$!
  wait $pid; local rc=$?
  kill $wd 2>/dev/null; wait $wd 2>/dev/null
  echo "RC $rc" >> "$log"
}

BASE=$ROT_DIR/${TAG}_R000.oas
[ -f "$BASE" ] || { echo "missing $BASE"; exit 2; }
for a in $ANGLES; do
  A=$(printf %03d "$a")
  F=$ROT_DIR/${TAG}_R$A.oas
  D=$OUT/$TAG/R$A
  mkdir -p "$D/tmp"
  [ -f "$F" ] || { echo "missing $F"; continue; }
  if [ "${SKIP_DB:-0}" != 1 ]; then
    guarded "$D/rot_poly.log" "$SRC/db/db_rotcheck" --mode poly --angle "$a" "$BASE" "$F"
    guarded "$D/rot_box.log"  "$SRC/db/db_rotcheck" --mode box  --angle "$a" "$BASE" "$F"
  fi
  if [ "${SKIP_MAG:-0}" != 1 ] && { [ "${MAG_ORTHO_ONLY:-0}" != 1 ] || [ $((a % 90)) -eq 0 ]; }; then
    sed -e "s|@FILE@|$F|" -e "s|@TAG@|${TAG}_R$A|" -e "s|@TMP@|$D/tmp|" "$HERE/rot_ops.ctr.in" > "$D/rot_ops.ctr"
    ( cd "$D" && guarded "$D/mag.log" env MAGPARSER_LAYER_DIGEST=1 OMP_PROC_BIND=TRUE OMP_PLACES=cores \
        NVCOMPILER_ACC_MEM_MANAGE=0 "$SRC/MAGPARSER.exe" "$D/rot_ops.ctr" 256 )
  fi
  if [ "${SKIP_KL:-0}" != 1 ]; then
    guarded "$D/kl.log" klayout -b -r "$HERE/kl_measure.py" -rd f="$F" -rd layers=$LAYERS -rd derived=1
  fi
  echo "DONE $TAG R$A $(grep -h ROTCHECK "$D"/rot_*.log 2>/dev/null | awk '{print $2"/"$3}' | tr '\n' ' ')"
done
