#!/bin/bash
# defcheck regression: built-in SKY130 / ASAP7 / GT2N stacks (CONNECT STACK, var tech) and the
# DEF cross-check (exec defcheck, NET_NAME), every report verified against an independent
# KLayout LEF/DEF reference (defcheck_kl.py).
#
#   bash src/db/regressions/defcheck/run_defcheck_regression.sh [MAGPARSER.exe] [workdir] [--scale]
#
# --scale also checks SDT_16.oas (SKY130, 1.9M polygons) against the DEF it was made from, if
# the LibreLane run is present under /scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS.
# KLAYOUT may point at a KLayout >= 0.28 with Python; the default is the 0.30.3 build from AGENTS.md.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../../.." && pwd)
SCALE=0
ARGS=()
for a in "$@"; do
  if [ "$a" = "--scale" ]; then SCALE=1; else ARGS+=("$a"); fi
done
EXE=${ARGS[0]:-$SRC/MAGPARSER.exe}
WORK=${ARGS[1]:-$(mktemp -d /tmp/defcheck_regr_XXXXXX)}
KL_HOME=/scratch1/skoranne/OSS_EDA_TOOLS/klayout-0.30.3/bin-release
KLAYOUT=${KLAYOUT:-$KL_HOME/klayout}
if [ -z "${KLAYOUT_NO_LDPATH:-}" ] && [ -d "$KL_HOME" ]; then
  export LD_LIBRARY_PATH="$KL_HOME/:/usr/local/lib64/:/scratch1/skoranne/intel/oneapi/mpi/2021.14/opt/mpi/libfabric/lib:/scratch1/skoranne/intel/oneapi/mpi/2021.14/lib:/scratch1/skoranne/intel/oneapi/umf/0.9/lib:/scratch1/skoranne/intel/oneapi/compiler/2025.0/opt/compiler/lib:/scratch1/skoranne/intel/oneapi/compiler/2025.0/lib:/scratch1/skoranne/intel/oneapi/tbb/2022.0/lib:/scratch1/skoranne/CAD/CONDA/envs/idp/lib:${LD_LIBRARY_PATH:-}"
fi
mkdir -p "$WORK" && cd "$WORK" || exit 2
echo "workdir: $WORK"
rm -f ./*_opens.csv ./*_shorts.csv ./*_misses.csv ./*_names.csv
cp "$HERE"/data/* "$HERE/defcheck.ctr" .
"$KLAYOUT" -b -r "$HERE/gen_defcheck_input.py" -rd outdir=. -rd datadir=. || exit 2
"$EXE" defcheck.ctr 64 > magparser.log 2>&1
rc=$?
if [ $rc -ne 0 ]; then echo "MAGPARSER failed ($rc), see $WORK/magparser.log"; grep -E "ERROR|DEFCHECK RESULT" magparser.log | tail -8; exit 1; fi
grep -E "^CONNECT STACK|^DEFCHECK RESULT" magparser.log

status=0
ref() {   # layout def lefs stack prefix polycsv polylayer expect [lo hi]
  "$KLAYOUT" -b -r "$HERE/defcheck_kl.py" -rd layout="$1" -rd defp="$2" -rd lefs="$3" -rd stack="$4" \
      -rd prefix="$5" -rd polycsv="$6" -rd polylayer="$7" -rd expect="$8" -rd lo="${9:-}" -rd hi="${10:-}" \
      2>&1 | grep -v "^Warning: Macro not found" || status=1
  [ "${PIPESTATUS[0]}" -eq 0 ] || status=1
}
ref tiny_soc.oas  tiny_soc.def.gz sky130_fd_sc_hd__nom.tlef SKY130    tiny  tiny_m1.csv  met1 clean
ref tiny_mut.oas  tiny_mut.def    sky130_fd_sc_hd__nom.tlef SKY130    mut   mut_m1.csv   met1 tiny_mut
ref synth_asap7.oas synth_asap7.def synth_asap7.lef        ASAP7     asap7 asap7_m2.csv M2   clean
ref synth_gt2n.oas  synth_gt2n.def  synth_gt2n.lef         GT2N_FULL gt2n  gt2n_m0.csv  M0   clean
ref synth_gt2n.oas  synth_gt2n.def  synth_gt2n.lef         GT2N      gt2s  ""           ""   clean M1 M5

# the mutated TinySoC must report exactly one miss, and at least one open and one short
line=$(grep "^DEFCHECK RESULT" magparser.log | sed -n 3p)
case "$line" in
  *"opens=0 "*|*"shorts=0 "*|*"misses=0 "*) echo "FAIL: mutated TinySoC not detected: $line"; status=1 ;;
  *) echo "mutations detected: $line" ;;
esac
# the slice keeps the other layers' probes out of the check
if grep -A8 "vs synth_gt2n.def" magparser.log | grep -q "routing layers: M1 M2 M3 M4 M5 "; then
  echo "GT2N M1..M5 slice used"
else
  echo "FAIL: CONNECT STACK GT2N g2 M1 M5 did not give the M1..M5 slice"; status=1
fi

# a statement that must fail with the given message
expect_error() {   # name message ctr-body-lines
  printf 'program %s\nvar temp_folder /tmp\ndecl design t1 = tiny_soc.oas\ndecl design t2 = tiny_mut.oas\n%b\nend\n' "$1" "$3" > "$1.ctr"
  if "$EXE" "$1.ctr" 64 > "$1.log" 2>&1; then
    echo "FAIL: $1 did not fail"; status=1
  elif ! grep -q "$2" "$1.log"; then
    echo "FAIL: $1: missing message '$2'"; tail -3 "$1.log"; status=1
  else
    echo "rejected as expected: $1"
  fi
}
expect_error neg_tech "unknown technology" "CONNECT STACK SKY999 t1"
expect_error neg_slice "must start and end on routing" "CONNECT STACK SKY130 t1 mcon met2"
expect_error neg_noconnect "is not part of a CONNECT" "exec defcheck t1:L68_20 tiny_soc.def.gz"
expect_error neg_nodef "cannot open DEF" "CONNECT STACK SKY130 t1\nexec defcheck t1:met1 no_such.def"
expect_error neg_name "exec defcheck" "CONNECT STACK SKY130 t1\nPROPERTY:N = t1:met1 FASCL [ (id) N = NET_NAME(id) ] t1:nothing"
expect_error neg_strict "does not match the DEF" "CONNECT STACK SKY130 t2\nexec defcheck t2:met1 tiny_mut.def - strict lef=sky130_fd_sc_hd__nom.tlef"
expect_error neg_var "unknown technology" "var tech TSMC3"

if [ $SCALE -eq 1 ]; then
  OAS=/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SMALL_SDT/SDT_16.oas
  DEF=/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SDT_16_FINAL/final/def/SDT_16.def
  if [ -f "$OAS" ] && [ -f "$DEF" ]; then
    printf 'program sdt\nvar temp_folder /tmp\nvar max_layers 64\ndecl design d1 = %s\nCONNECT STACK SKY130 d1\nexec defcheck d1:met1 %s sdt lef=sky130_fd_sc_hd__nom.tlef strict\nend\n' "$OAS" "$DEF" > sdt.ctr
    if "$EXE" sdt.ctr 64 > sdt.log 2>&1; then
      grep -E "^CONNECT #|^DEFCHECK RESULT" sdt.log
    else
      echo "FAIL: SDT_16 does not match its DEF"; grep -E "ERROR|DEFCHECK" sdt.log | tail -5; status=1
    fi
  else
    echo "scale check skipped: $OAS or $DEF not found"
  fi
fi

if [ $status -eq 0 ]; then echo "DEFCHECK REGRESSION PASSED"; else echo "DEFCHECK REGRESSION FAILED"; fi
exit $status
