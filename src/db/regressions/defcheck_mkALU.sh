#!/bin/bash
# DEF cross-check regression on the 64-bit ALU (SKY130 HD, OpenLane):
#   mkALU.def (and the same file gzip'ed) vs mkALU.oas / mkALU.gds with db_defcheck.
#
#   bash src/db/regressions/defcheck_mkALU.sh [--gds] [workdir]
#
# Cases
#   1  plain .def  + tech/cell LEFs vs .oas : all checks PASS, XOR on 15 layers is 0
#   2  .def.gz     + LEFs           vs .oas : same result through the zlib reader
#   3  .def.gz     without LEF      vs .oas : probe-only mode, ROUTING PASS, COMPONENTS SKIP, XOR on the 4 pin layers only
#   4  mutated .def.gz (one cell moved 460, one L1M1_PR_MR via deleted, one met2 wire
#      stretched 1 um)                       : exit 1 and each mutation reported where it is
#   5  (--gds) .def.gz + LEFs vs .gds        : all checks PASS (the GDS path holds 3.5 GB)
# This regression is the answer to "is there no li1 (67/20) in mkALU.oas?": the top cell has
# 328948 li1 rectangles, one per DEF L1M1_PR_MR via, and the 261 standard cells carry every
# li1 PIN/OBS rectangle of the SKY130 HD LEF.
#
# Overrides: MKALU_DIR (design dir with def/ gds/), SKY130_HD (PDK libs.ref/sky130_fd_sc_hd).
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
DB=$(cd "$HERE/.." && pwd)
GDS=0; WORK=""
for a in "$@"; do if [ "$a" = "--gds" ]; then GDS=1; else WORK=$a; fi; done
WORK=${WORK:-$(mktemp -d /tmp/defcheck_mkALU_XXXXXX)}
MKALU_DIR=${MKALU_DIR:-/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/DESIGNS/64bit_ALU}
SKY130_HD=${SKY130_HD:-/scratch1/skoranne/OPENLANE/OpenLane.working/pdks/volare/sky130/versions/e8294524e5f67c533c5d0c3afa0bcc5b2a5fa066/sky130A/libs.ref/sky130_fd_sc_hd}
DEF=$MKALU_DIR/def/mkALU.def
OAS=$MKALU_DIR/gds/mkALU.oas
for f in "$DEF" "$OAS" "$SKY130_HD/techlef/sky130_fd_sc_hd__nom.tlef" "$SKY130_HD/lef/sky130_fd_sc_hd.lef" "$SKY130_HD/lef/sky130_ef_sc_hd.lef"; do
  [ -f "$f" ] || { echo "SKIP: missing $f"; exit 0; }
done
LEFS=(--lef "$SKY130_HD/techlef/sky130_fd_sc_hd__nom.tlef" --lef "$SKY130_HD/lef/sky130_fd_sc_hd.lef" --lef "$SKY130_HD/lef/sky130_ef_sc_hd.lef")

make -C "$DB" db_defcheck >/dev/null || { echo "FAIL: build db_defcheck"; exit 1; }
X=$DB/db_defcheck
mkdir -p "$WORK" && echo "workdir: $WORK"
status=0
pass() { echo "PASS  $1"; }
fail() { echo "FAIL  $1 (see $2)"; status=1; }
need() {   # log pattern...
  local log=$1; shift
  for p in "$@"; do grep -qE -- "$p" "$log" || { echo "      missing: $p"; return 1; }; done
}

[ -f "$WORK/mkALU.def.gz" ] || gzip -c "$DEF" > "$WORK/mkALU.def.gz"
# Mutations: ANTENNA_1 moved +460 in x; the first L1M1_PR_MR via of net _000000_ removed;
# the met2 wire ( 1270750 804270 ) ( * 808350 ) of net _000001_ stretched to 809350.
awk 'BEGIN{a=0;b=0;c=0}
 /- ANTENNA_1 sky130_fd_sc_hd__diode_2 \+ PLACED \( 660100 859520 \) N ;/ && !a {sub("660100","660560"); a=1}
 /NEW li1 \( 1276730 779450 \) L1M1_PR_MR/ && !b {b=1; next}
 /NEW met2 \( 1270750 804270 \) \( \* 808350 \)/ && !c {sub("808350","809350"); c=1}
 {print}' "$DEF" | gzip -c > "$WORK/mutated.def.gz"

ALLPASS=("^CHECK DIE +PASS" "^CHECK COMPONENTS PASS  DEF placed 301203 .*DEF-only 0  layout-only 0" \
         "^CHECK ROUTING +PASS .*vias 922416 \(stack-rule 0\).*missed probes 0" \
         "^CHECK XOR +PASS  layers checked 15  skipped 0  total XOR area 0" \
         "li1 +67/20 +drawing DEF area +9506597200 +layout area +9506597200 +XOR +0 +\(DEF rects 328948, layout rects 328948" \
         "^CHECK LEF +PASS  macros used 261" "li1 +67/20 +LEF rects +8453 +on drawing +8453" "^DEFCHECK PASS")

"$X" "${LEFS[@]}" "$DEF" "$OAS" > "$WORK/1_def_oas.log" 2>&1; rc=$?
[ $rc -eq 0 ] && need "$WORK/1_def_oas.log" "${ALLPASS[@]}" && pass "1 .def vs .oas" || fail "1 .def vs .oas" "$WORK/1_def_oas.log"

"$X" "${LEFS[@]}" "$WORK/mkALU.def.gz" "$OAS" > "$WORK/2_defgz_oas.log" 2>&1; rc=$?
[ $rc -eq 0 ] && need "$WORK/2_defgz_oas.log" "${ALLPASS[@]}" && pass "2 .def.gz vs .oas" || fail "2 .def.gz vs .oas" "$WORK/2_defgz_oas.log"

"$X" "$WORK/mkALU.def.gz" "$OAS" > "$WORK/3_nolef.log" 2>&1; rc=$?
[ $rc -eq 0 ] && need "$WORK/3_nolef.log" "^CHECK DIE +PASS" "^CHECK COMPONENTS SKIP" \
    "^CHECK ROUTING +PASS .*stack-rule 838572.*missed probes 0" "^CHECK XOR +PASS  layers checked 4  skipped 11" "^DEFCHECK PASS" \
  && pass "3 .def.gz vs .oas, no LEF (probes only)" || fail "3 no LEF" "$WORK/3_nolef.log"

"$X" "${LEFS[@]}" --samples 4 "$WORK/mutated.def.gz" "$OAS" > "$WORK/4_mutated.log" 2>&1; rc=$?
[ $rc -eq 1 ] && need "$WORK/4_mutated.log" \
    "^CHECK COMPONENTS FAIL .*DEF-only 1  layout-only 1" \
    "DEF-only +sky130_fd_sc_hd__diode_2 at \(660560 859520\) N" \
    "layout-only sky130_fd_sc_hd__diode_2 at \(660100 859520\) N" \
    "^CHECK ROUTING +FAIL .*missed probes 3" "miss wire met2 endpoint at \(1270750 809350\)" \
    "^CHECK XOR +FAIL .*total XOR area 202200" \
    "li1 +67/20 +drawing .*XOR +28900 " "met2 +69/20 +drawing .*XOR +107800 " \
    "xor li1 67/20 window \(1276627 779273\) \(1277408 780055\): DEF area 0 layout area 28900" \
    "^DEFCHECK FAIL \(3 failing checks\)" \
  && pass "4 mutated .def.gz detected and located" || fail "4 mutated .def.gz (rc=$rc)" "$WORK/4_mutated.log"

if [ $GDS -eq 1 ]; then
  "$X" "${LEFS[@]}" "$WORK/mkALU.def.gz" "$MKALU_DIR/gds/mkALU.gds" > "$WORK/5_defgz_gds.log" 2>&1; rc=$?
  [ $rc -eq 0 ] && need "$WORK/5_defgz_gds.log" "${ALLPASS[@]}" && pass "5 .def.gz vs .gds" || fail "5 .def.gz vs .gds" "$WORK/5_defgz_gds.log"
fi

[ $status -eq 0 ] && echo "defcheck_mkALU: PASS" || echo "defcheck_mkALU: FAIL"
exit $status
