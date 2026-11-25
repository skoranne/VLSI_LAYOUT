#!/bin/bash
# DEF -> OASIS regression for db_def2oas (names and DEF properties kept in the OASIS).
#
#   bash src/db/regressions/def2oas_mkALU.sh [--klayout-xor] [workdir]
#
# Cases
#   1  data/def2oas_props.def (4 components from LEF abstracts, pins, a special net, net and
#      component + PROPERTY, SOURCE/WEIGHT/FIXED, ROW/TRACKS/PROPERTYDEFINITIONS):
#      KLayout reads every name / property back (def2oas_props_check.py)
#   2  mkALU.def.gz + LEFs + --cells mkALU.oas -> mkALU_def.oas: exit 0, 301203 placements
#   3  db_defcheck mkALU.def vs mkALU_def.oas: all checks PASS, XOR on 15 layers is 0
#   4  KLayout: all 301203 placements named (property 61), routing shapes carry DEF_NET
#   5  (--klayout-xor, ~7 min) KLayout XOR of mkALU_def.oas vs the OpenLane mkALU.oas:
#      0 on every layer except 81/14 (a P&R marker box that is not in the DEF)
# Overrides: MKALU_DIR, SKY130_HD (as defcheck_mkALU.sh), KLAYOUT_HOME (KLayout 0.30 bin dir).
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
DB=$(cd "$HERE/.." && pwd)
XOR=0; WORK=""
for a in "$@"; do if [ "$a" = "--klayout-xor" ]; then XOR=1; else WORK=$a; fi; done
WORK=${WORK:-$(mktemp -d /tmp/def2oas_mkALU_XXXXXX)}
MKALU_DIR=${MKALU_DIR:-/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/DESIGNS/64bit_ALU}
SKY130_HD=${SKY130_HD:-/scratch1/skoranne/OPENLANE/OpenLane.working/pdks/volare/sky130/versions/e8294524e5f67c533c5d0c3afa0bcc5b2a5fa066/sky130A/libs.ref/sky130_fd_sc_hd}
KLAYOUT_HOME=${KLAYOUT_HOME:-/scratch1/skoranne/OSS_EDA_TOOLS/klayout-0.30.3/bin-release}
DEF=$MKALU_DIR/def/mkALU.def
OAS=$MKALU_DIR/gds/mkALU.oas
for f in "$DEF" "$OAS" "$SKY130_HD/techlef/sky130_fd_sc_hd__nom.tlef" "$SKY130_HD/lef/sky130_fd_sc_hd.lef" "$SKY130_HD/lef/sky130_ef_sc_hd.lef"; do
  [ -f "$f" ] || { echo "SKIP: missing $f"; exit 0; }
done
LEFS=(--lef "$SKY130_HD/techlef/sky130_fd_sc_hd__nom.tlef" --lef "$SKY130_HD/lef/sky130_fd_sc_hd.lef" --lef "$SKY130_HD/lef/sky130_ef_sc_hd.lef")
make -C "$DB" db_def2oas db_defcheck >/dev/null || { echo "FAIL: build"; exit 1; }
mkdir -p "$WORK" && echo "workdir: $WORK"
status=0
pass() { echo "PASS  $1"; }
fail() { echo "FAIL  $1 (see $2)"; status=1; }
KL=""
if [ -x "$KLAYOUT_HOME/klayout" ]; then
  KL="$KLAYOUT_HOME/klayout"
  export LD_LIBRARY_PATH="$KLAYOUT_HOME/:/usr/local/lib64/:/scratch1/skoranne/intel/oneapi/compiler/2025.0/lib:/scratch1/skoranne/CAD/CONDA/envs/idp/lib:${LD_LIBRARY_PATH:-}"
fi

# 1 synthetic DEF with every kind of name / property
"$DB/db_def2oas" "${LEFS[@]}" "$HERE/data/def2oas_props.def" "$WORK/props.oas" > "$WORK/1_props.log" 2>&1 || fail "1 db_def2oas props" "$WORK/1_props.log"
if [ -n "$KL" ]; then
  "$KL" -b -r "$HERE/def2oas_props_check.py" -rd F="$WORK/props.oas" >> "$WORK/1_props.log" 2>&1
  if grep -q "^DEF2OAS_PROPS PASS" "$WORK/1_props.log"; then pass "1 props DEF: names and properties read back by KLayout"; else fail "1 props DEF" "$WORK/1_props.log"; fi
else echo "SKIP  1 KLayout check (no $KLAYOUT_HOME/klayout)"; fi

# 2 mkALU conversion (from .def.gz)
gzip -c "$DEF" > "$WORK/mkALU.def.gz"
"$DB/db_def2oas" "${LEFS[@]}" --cells "$OAS" "$WORK/mkALU.def.gz" "$WORK/mkALU_def.oas" > "$WORK/2_convert.log" 2>&1; rc=$?
if [ $rc -eq 0 ] && grep -q "301203 placements" "$WORK/2_convert.log" && grep -q "261 from --cells, 0 LEF abstracts, 0 missing" "$WORK/2_convert.log"; then
  pass "2 mkALU.def.gz -> OASIS"; else fail "2 mkALU.def.gz -> OASIS (rc $rc)" "$WORK/2_convert.log"; fi

# 3 the converted OASIS matches the DEF
"$DB/db_defcheck" "${LEFS[@]}" "$DEF" "$WORK/mkALU_def.oas" > "$WORK/3_defcheck.log" 2>&1; rc=$?
if [ $rc -eq 0 ] && grep -q "CHECK XOR        PASS  layers checked 15  skipped 0  total XOR area 0" "$WORK/3_defcheck.log" \
   && grep -q "^DEFCHECK PASS" "$WORK/3_defcheck.log"; then pass "3 db_defcheck DEF vs converted OASIS"; else fail "3 db_defcheck" "$WORK/3_defcheck.log"; fi

# 4 / 5 KLayout: names, and optionally XOR against the OpenLane OASIS
if [ -n "$KL" ]; then
  cat > "$WORK/names.py" <<'PY'
import pya
ly = pya.Layout(); ly.read(F); t = ly.top_cell()
n = named = 0
for i in t.each_inst():
    n += 1
    if i.property(61): named += 1
li = ly.find_layer(68, 20); m = withnet = 0
for s in t.shapes(li).each():
    m += 1
    if s.property("DEF_NET") is not None: withnet += 1
print("NAMES instances %d named %d met1 %d with_net %d" % (n, named, m, withnet))
PY
  "$KL" -b -r "$WORK/names.py" -rd F="$WORK/mkALU_def.oas" > "$WORK/4_names.log" 2>&1
  if grep -Eq "^NAMES instances 301203 named 301203 met1 ([0-9]+) with_net \1$" "$WORK/4_names.log"; then
    pass "4 KLayout: every placement named, every met1 shape has DEF_NET"; else fail "4 KLayout names" "$WORK/4_names.log"; fi
  if [ $XOR -eq 1 ]; then
    "$KL" -b -r "$HERE/klayout_flat_xor.py" -rd A="$OAS" -rd B="$WORK/mkALU_def.oas" > "$WORK/5_xor.log" 2>&1
    if grep -q "^layers with differences: 1$" "$WORK/5_xor.log" && grep -q "^L81/14 .*<-- DIFF" "$WORK/5_xor.log"; then pass "5 KLayout XOR vs mkALU.oas: 0 except 81/14"; else fail "5 KLayout XOR" "$WORK/5_xor.log"; fi
  fi
else echo "SKIP  4/5 KLayout checks"; fi
[ $status -eq 0 ] && echo "def2oas_mkALU: PASS" || echo "def2oas_mkALU: FAIL"
exit $status
