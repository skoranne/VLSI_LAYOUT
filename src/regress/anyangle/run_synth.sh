#!/bin/bash
# Synthetic any-angle cases for TRAPMERGE.exe vs KLayout (exact: xor 0, identical 2A):
# rotated rings with holes/islands (mk_ring.py, layer 1/0) and 45-degree shapes incl. bent
# paths (mk_45.py, layer 2/0; output must be RECTANGLE/CTRAPEZOID only).  ./run_synth.sh [DIR]
set -eu
HERE=$(cd "$(dirname "$0")" && pwd); SRC=$(cd "$HERE/../.." && pwd)
T=${1:-$HERE/out/synth}; mkdir -p "$T"; fail=0
for c in "ring 1/0" "45 2/0"; do
    set -- $c
    klayout -b -r "$HERE/mk_$1.py" -rd out="$T/$1.oas"
    "$SRC/TRAPMERGE.exe" "$T/$1.oas" "$T/$1_out.oas" "$2" > "$T/$1.merge.log"
    klayout -b -r "$HERE/kl_merge_check.py" -rd fin="$T/$1.oas" -rd fout="$T/$1_out.oas" -rd layers="$2" > "$T/$1.kl.log"
    a2=$(grep -o "area2=[0-9]*" "$T/$1.merge.log" | cut -d= -f2)
    an=$(grep -o "anomalies=[0-9]*" "$T/$1.merge.log" | cut -d= -f2 | paste -sd+ | bc)
    line=$(grep KLMERGE "$T/$1.kl.log")
    ok=1
    [ "$an" = 0 ] || ok=0
    echo "$line" | grep -q "kl_area2=$a2 out_area2=$a2 .*xor_area=0 " || ok=0
    [ "$1" != 45 ] || ! grep -q "polygons=[1-9]" <(grep "TRAPMERGE out:" "$T/$1.merge.log") || ok=0
    echo "$1: $([ $ok = 1 ] && echo PASS || echo FAIL) anomalies=$an $(grep 'TRAPMERGE out:' "$T/$1.merge.log" | cut -d: -f2)"
    [ $ok = 1 ] || fail=1
done
exit $fail
