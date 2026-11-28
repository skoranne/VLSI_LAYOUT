#!/bin/bash
# Rational gold check of one clip: TRAPBOOL (ours) and KLayout vs the exact rational Boolean.
# Usage: gold_run.sh clip.oas [A B]   (env TRAPBOOL, KLAYOUT, BOUND)
set -eu
clip=$1; a=${2:-66/20}; b=${3:-65/20}; base=${clip%.oas}
TB=${TRAPBOOL:-$(dirname "$0")/../../../TRAPBOOL.exe}; KL=${KLAYOUT:-klayout}
ops=OR,AND,NOT,XOR,BNOTA
"$TB" "$clip" "${base}_ours.oas" "$a" "$b" -ops $ops > "${base}_trapbool.log" 2>&1
# TRAPBOOL's own self-checks: exact identities and zero stitch/trace anomalies
grep -q "identity XOR=OR-AND OK" "${base}_trapbool.log" && grep -q "identity OR=NOT+AND+BNOTA OK" "${base}_trapbool.log" \
    || { echo "GOLD FAIL TRAPBOOL identity check"; exit 1; }
if grep -E "anomalies=[1-9]" "${base}_trapbool.log"; then echo "GOLD FAIL TRAPBOOL anomalies"; exit 1; fi
area2=$(grep -oE "op=[A-Z]+ layer=[0-9/]+ area2=[0-9-]+" "${base}_trapbool.log" | sed -E 's/op=([A-Z]+) layer=[0-9\/]+ area2=/\1=/' | tr '\n' ' ')
"$KL" -b -r "$(dirname "$0")/kl_dump.py" -rd fin="$clip" -rd fout="${base}_ours.oas" -rd a="$a" -rd b="$b" \
     -rd ops=$ops -rd out=1000 -rd jout="${base}.json" > "${base}_kl.log" 2>&1
python3 "$(dirname "$0")/gold_check.py" "${base}.json" --assert --bound "${BOUND:-0.5}" --area2 $area2
