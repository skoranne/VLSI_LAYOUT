#!/bin/bash
# Rational gold regression: every clip in clips/*.oas through gold_run.sh.
# A clip passes when TRAPBOOL's results lie within the snap-rounding tube (Hausdorff Linf <= 0.5
# dbu) of the exact rational Boolean, its exact piece area equals its reported area2, and the
# exact identities hold. KLayout's distance from the gold result is reported for comparison.
# Usage: run_gold.sh [clip.oas ...]    (env TRAPBOOL, KLAYOUT)
cd "$(dirname "$0")"
clips=("$@"); [ ${#clips[@]} -eq 0 ] && clips=(clips/*.oas)
pass=0; fail=0
for c in "${clips[@]}"; do
    case $c in *_ours.oas) continue;; esac
    pair=$(head -c 0 /dev/null; cat "${c%.oas}.layers" 2>/dev/null || echo "66/20 65/20")
    if ./gold_run.sh "$c" $pair > "${c%.oas}_gold.log" 2>&1; then pass=$((pass+1)); r=PASS; else fail=$((fail+1)); r=FAIL; fi
    echo "GOLDCLIP $c $r"; grep "^GOLD op" "${c%.oas}_gold.log" | sed 's/^/   /'
done
echo "GOLD summary pass=$pass fail=$fail"
[ $fail -eq 0 ]
