#!/bin/bash
# Any-angle two-layer Booleans (TRAPBOOL.exe) on the rotated designs of regress/rotation, checked
# against KLayout.  ./run_bool.sh TAG ["ANGLES"] ; logs in out/bool_TAG/<A>_<B>/R<a>/{bool,kl}.log
# Env: PAIRS (default "66/20:65/20 68/20:69/20 67/20:68/20" = poly x diff, met1 x met2, li1 x met1),
# OPS (default OR,AND,NOT,XOR,BNOTA,A,B), SKIP_KL=1, START_GB (MemAvailable to wait for, default 24),
# TRAPBOOL (binary, default ../../TRAPBOOL.exe).
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
TAG=$1
ANGLES=${2:-"0 15 30 45 60 75 90 105 120 135 150 165 180"}
DIR=${DESIGN_DIR:-/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/ROTATION}
PAIRS=${PAIRS:-"66/20:65/20 68/20:69/20 67/20:68/20"}
OPS=${OPS:-OR,AND,NOT,XOR,BNOTA,A,B}
EXE=${TRAPBOOL:-$SRC/TRAPBOOL.exe}
START_GB=${START_GB:-24}
NOPS=$(echo "$OPS" | tr ',' '\n' | wc -l)
wait_mem() {
    for _ in $(seq 1 720); do
        a=$(awk '/MemAvailable/ {print int($2/1048576)}' /proc/meminfo)
        [ "$a" -ge "$START_GB" ] && return 0
        sleep 10
    done
    return 1
}
for pr in $PAIRS; do
    LA=${pr%%:*}; LB=${pr##*:}
    PD=$HERE/out/bool_$TAG/$(echo "${LA}_${LB}" | tr '/' '-')
    for a in $ANGLES; do
        A=$(printf "%03d" "$a")
        F=$DIR/${TAG}_R$A.oas
        D=$PD/R$A; mkdir -p "$D"
        [ -f "$F" ] || { echo "missing $F"; continue; }
        if ! grep -q "^TRAPBOOL out:" "$D/bool.log" 2>/dev/null; then
            wait_mem
            /usr/bin/time -f "TIME %e s MAXRSS %M KB" "$EXE" "$F" "$D/bool.oas" "$LA" "$LB" -ops "$OPS" > "$D/bool.log" 2>&1
        fi
        if [ "${SKIP_KL:-0}" != 1 ] && ! [ "$(grep -c '^KLBOOL op=' "$D/kl.log" 2>/dev/null)" = "$NOPS" ]; then
            wait_mem
            /usr/bin/time -f "TIME %e s MAXRSS %M KB" klayout -b -r "$HERE/kl_bool_check.py" -rd fin="$F" \
                -rd fout="$D/bool.oas" -rd a="$LA" -rd b="$LB" -rd ops="$OPS" -rd out=1000 > "$D/kl.log" 2>&1
        fi
        echo "$LA x $LB R$A done"
    done
done
