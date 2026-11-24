#!/bin/bash
# Hierarchical GDSII/OASIS load: `decl design d1 = file.oas|gds` must flatten every placement
# (rotations, mirrors, arrays, nested cells, OASIS repetitions) exactly as KLayout does.
# usage: run_hier_load_regression.sh [MAGPARSER.exe] [workdir]
HERE="$(cd "$(dirname "$0")" && pwd)"
EXE="${1:-$HERE/../../../MAGPARSER.exe}"
WORK="${2:-$(mktemp -d)}"
KLAYOUT="${KLAYOUT:-/scratch1/skoranne/OSS_EDA_TOOLS/klayout-0.30.3/bin-release/klayout}"
export LD_LIBRARY_PATH="/scratch1/skoranne/OSS_EDA_TOOLS/klayout-0.30.3/bin-release/:/usr/local/lib64/:${LD_LIBRARY_PATH}"
mkdir -p "$WORK" && cd "$WORK" || exit 1
echo "workdir: $WORK"

"$KLAYOUT" -b -r "$HERE/gen_hier_input.py" -rd out=hier_in.oas || exit 1
"$KLAYOUT" -b -r "$HERE/gen_hier_input.py" -rd out=hier_in.gds || exit 1

status=0
for f in hier_in.oas hier_in.gds; do
  b="${f%.*}_${f##*.}"
  cat > "$b.ctr" <<EOF
program hier_load
var temp_folder /tmp
decl design d1 = $f
exec oasout d1 ${b}_flat.oas
end
EOF
  if ! "$EXE" "$b.ctr" 8 > "$b.log" 2>&1; then
    echo "MAGPARSER failed on $f, see $WORK/$b.log"; status=1; continue
  fi
  "$KLAYOUT" -b -r "$HERE/hier_check.py" -rd ref="$f" -rd oas="${b}_flat.oas" -rd layers=68/20,67/20 || status=1
done
if [ $status -eq 0 ]; then echo "HIER LOAD REGRESSION PASSED"; else echo "HIER LOAD REGRESSION FAILED"; fi
exit $status
