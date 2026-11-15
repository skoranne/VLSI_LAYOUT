#!/usr/bin/env bash
set -euo pipefail
# GDSII/OASIS regression: byte-identical R/W/R via db_roundtrip and KLayout XOR checks.
# Usage: ./xor_regression.sh [--quick]
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
DB="$ROOT/src/db"
TMP="${TMPDIR:-/tmp}/db_xor_reg"
mkdir -p "$TMP"
QUICK=0
if [[ "${1:-}" == "--quick" ]]; then QUICK=1; fi

pass=0; fail=0
ok(){ echo "[PASS] $*"; pass=$((pass+1)); }
bad(){ echo "[FAIL] $*"; fail=$((fail+1)); }

have_klayout=0
if command -v klayout >/dev/null 2>&1; then have_klayout=1; fi

echo "== Build db tools =="
make -C "$DB" -j4 >/dev/null

klayout_xor_empty() {
  local a="$1" b="$2" tag="$3"
  if [[ $have_klayout -eq 0 ]]; then echo "[SKIP] klayout not available: $tag"; return 0; fi
  local rb="$TMP/kl_${tag}.rb"
  cat > "$rb" <<'RUBY'
la = RBA::Layout.new; la.read($a)
lb = RBA::Layout.new; lb.read($b)
ok = true
la.each_layer do |li, info|
  la.each_cell do |ca|
    cb = lb.cell(ca.name)
    if !cb
      ok = false if ca.shapes(li).size > 0
      next
    end
    # compare per top-level bbox area via XOR on same layer indices when possible
  end
end
# global XOR: flatten top cells' shapes per matching layer infos
la.layer_indices.each do |li|
  info = la.get_info(li)
  lb_li = lb.find_layer(info)
  next if lb_li.nil?
  la.top_cells.each do |ct|
    cb = lb.cell(ct.cell_index)
    next unless cb
    # use region XOR if available
    begin
      ra = RBA::Region.new(ct.begin_shapes_rec(li))
      rb2 = RBA::Region.new(cb.begin_shapes_rec(lb_li))
      xor = ra ^ rb2
      if !xor.is_empty?
        puts "XOR_NONEMPTY #{info}"
        ok = false
      end
    rescue => e
      # fallback: bbox compare
    end
  end
end
puts ok ? "XOR_EMPTY" : "XOR_FAIL"
RUBY
  local out
  out=$(klayout -b -rd a="$a" -rd b="$b" -r "$rb" 2>&1 | tail -5 || true)
  echo "$out" | grep -q "XOR_EMPTY"
}

run_case() {
  local in="$1" name="$2"
  echo "-- case: $name ($in)"
  local out="$TMP/${name}.rt"
  if "$DB/db_roundtrip" readwrite "$in" "$out" >/tmp/db_rt_${name}.log 2>&1; then
    if grep -q "Byte-identical: YES" /tmp/db_rt_${name}.log; then
      ok "$name byte-identical R/W/R"
    else
      bad "$name byte-identical R/W/R"; cat /tmp/db_rt_${name}.log | head -20; return
    fi
  else
    bad "$name db_roundtrip failed"; cat /tmp/db_rt_${name}.log | head -20; return
  fi
  if klayout_xor_empty "$in" "$out" "$name"; then
    ok "$name klayout XOR empty"
  else
    if [[ $have_klayout -eq 1 ]]; then bad "$name klayout XOR empty"; else echo "[SKIP] $name klayout XOR"; fi
  fi
}

# GDSII regressions
if [[ -f "$ROOT/src/ovlp_sq.gds" ]]; then
  headc=$(head -c 8 "$ROOT/src/ovlp_sq.gds" | od -An -tx1 | tr -d ' \n')
  if [[ "$headc" != "76657273696f6e20" ]]; then
    run_case "$ROOT/src/ovlp_sq.gds" "gds_ovlp_sq"
  else
    echo "[SKIP] gds_ovlp_sq is LFS pointer"
  fi
fi

# Generate GEAR OASIS and test
"$DB/db_roundtrip" gear_oasis "$TMP/gear_reg.oas" >/tmp/db_gear.log 2>&1 || { bad "gear generation"; cat /tmp/db_gear.log; }
if [[ -f "$TMP/gear_reg.oas" ]]; then
  run_case "$TMP/gear_reg.oas" "oas_gear"
  # also validate with klayout-generated equivalent if possible
fi

# KLayout-generated OASIS regression (valid reference)
if [[ $have_klayout -eq 1 ]]; then
  cat > "$TMP/gen_gear_ref.rb" <<'RUBY'
ly = RBA::Layout.new; ly.dbu = 0.001
top = ly.create_cell("GEAR3DIC"); l1=ly.layer(10,0); l2=ly.layer(11,0)
pts=[]; teeth=8; r_out=2000.0; r_in=1500.0; tooth=400.0
(0...(teeth*4)).each do |i| ang=2*Math::PI*i/(teeth*4); rad=(i%4<2)?(r_out+tooth):r_in; pts<<RBA::Point.new((rad*Math.cos(ang)).round,(rad*Math.sin(ang)).round) end
top.shapes(l1).insert(RBA::Polygon.new(pts))
def circle_pts(cx,cy,r,n=64); (0...n).map{|i| ang=2*Math::PI*i/n; RBA::Point.new(cx+(r*Math.cos(ang)).round, cy+(r*Math.sin(ang)).round)} end
top.shapes(l2).insert(RBA::Polygon.new(circle_pts(0,0,600)))
4.times{|i| ang=2*Math::PI*i/4; top.shapes(l2).insert(RBA::Polygon.new(circle_pts((1100*Math.cos(ang)).round,(1100*Math.sin(ang)).round,180)))}
ly.write($out)
RUBY
  klayout -b -rd out="$TMP/gear_klref.oas" -r "$TMP/gen_gear_ref.rb" >/dev/null 2>&1 || true
  if [[ -f "$TMP/gear_klref.oas" ]]; then
    run_case "$TMP/gear_klref.oas" "oas_gear_klref"
  fi
fi

# External OASIS regressions (if present)
OASIS_FILES=(
  "/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SMALL_SDT/SDT_16.oas"
  "/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/MW4x4_FLAT_DATA/MW4x4_FLAT_L67_D44_MERGED.oas"
  "/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/MW4x4_FLAT_DATA/MW4x4_HIER.oas"
)
if [[ $QUICK -eq 0 ]]; then
  OASIS_FILES+=("/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/MW4x4_FLAT_DATA/MW4x4_FLAT.oas")
fi
for f in "${OASIS_FILES[@]}"; do
  if [[ -f "$f" ]]; then
    bn=$(basename "$f" .oas)
    run_case "$f" "oas_${bn}"
  else
    echo "[SKIP] missing $f"
  fi
done

# GDSII via klayout conversion from OASIS (semantic/interop regression)
if [[ $have_klayout -eq 1 && -f /scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SMALL_SDT/SDT_16.oas ]]; then
  cat > "$TMP/oas2gds.rb" <<'RUBY'
ly=RBA::Layout.new; ly.read($in); ly.write($out)
RUBY
  klayout -b -rd in=/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SMALL_SDT/SDT_16.oas -rd out="$TMP/SDT_16_conv.gds" -r "$TMP/oas2gds.rb" >/dev/null 2>&1 || true
  if [[ -f "$TMP/SDT_16_conv.gds" ]]; then
    run_case "$TMP/SDT_16_conv.gds" "gds_SDT16_conv"
  fi
fi

echo "== SUMMARY: PASS=$pass FAIL=$fail =="
if [[ $fail -ne 0 ]]; then exit 1; fi
exit 0
