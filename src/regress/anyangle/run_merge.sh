#!/bin/bash
# Any-angle single-layer OR (TRAPMERGE.exe) on the rotated designs of regress/rotation, checked
# against KLayout.  ./run_merge.sh TAG ["ANGLES"] ; logs in out/TAG/R<a>/{merge,kl}.log
# Env: LAYERS (default SkyWater 130 poly diff li1 met1 met2), SKIP_KL=1, START_GB (wait for this
# much MemAvailable before each step, default 24).
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
TAG=$1
ANGLES=${2:-"0 15 30 45 60 75 90 105 120 135 150 165 180"}
DIR=${DESIGN_DIR:-/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/ROTATION}
LAYERS=${LAYERS:-"66/20 65/20 67/20 68/20 69/20"}
START_GB=${START_GB:-24}
OUT=$HERE/out/$TAG
wait_mem() {
    for _ in $(seq 1 720); do
        a=$(awk '/MemAvailable/ {print int($2/1048576)}' /proc/meminfo)
        [ "$a" -ge "$START_GB" ] && return 0
        sleep 10
    done
    return 1
}
for a in $ANGLES; do
    A=$(printf "%03d" "$a")
    F=$DIR/${TAG}_R$A.oas
    D=$OUT/R$A; mkdir -p "$D"
    [ -f "$F" ] || { echo "missing $F"; continue; }
    if ! grep -q "^TRAPMERGE out:" "$D/merge.log" 2>/dev/null; then
        wait_mem
        /usr/bin/time -f "TIME %e s MAXRSS %M KB" "$SRC/TRAPMERGE.exe" "$F" "$D/merged.oas" $LAYERS > "$D/merge.log" 2>&1
    fi
    if [ "${SKIP_KL:-0}" != 1 ] && ! [ "$(grep -c '^KLMERGE' "$D/kl.log" 2>/dev/null)" = "$(echo $LAYERS | wc -w)" ]; then
        wait_mem
        /usr/bin/time -f "TIME %e s MAXRSS %M KB" klayout -b -r "$HERE/kl_merge_check.py" -rd fin="$F" \
            -rd fout="$D/merged.oas" -rd layers=$(echo $LAYERS | tr ' ' ',') > "$D/kl.log" 2>&1
    fi
    echo "R$A done"
done
