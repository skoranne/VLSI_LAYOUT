#!/bin/bash
# Build the MW_8x8 viewer test case: flat, STRICT-mode OASIS with S_BOUNDING_BOX, S_CELL_OFFSET
# and LAYERNAMEs, made as the 2x2 tiling of MW4x4_FLAT (16 MW dies -> 64), keeping the 1 / 5 / 10
# layers with the most boxes. Geometry is ordered in 200 um Morton bins (spatially local CBLOCKs).
#   make_mw8x8.sh [OUTDIR]     (default /scratch1/skoranne/LARGE_DESIGNS/MW_8x8)
# Each run is memory-guarded (killed if MemAvailable < 24 GB) and verified with db_oasinfo.
set -u
SRC=${MW4X4_MAG:-/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/MW4x4_FLAT_DATA/MW4x4_FLAT.mag}
OUT=${1:-/scratch1/skoranne/LARGE_DESIGNS/MW_8x8}
DB=$(cd "$(dirname "$0")/../../db" && pwd)
mkdir -p "$OUT"
make -s -C "$DB" bin2oas db_oasinfo || exit 1
guard() {   # guard LOG cmd...
  local log=$1; shift
  /usr/bin/time -f "RSS_KB %M WALL %e" -o "$log.time" "$@" > "$log" 2>&1 &
  local pid=$! av
  while kill -0 $pid 2>/dev/null; do
    av=$(awk '/MemAvailable/{print int($2/1048576)}' /proc/meminfo)
    if [ "$av" -lt 24 ]; then echo "WATCHDOG KILL avail=${av}GB" >> "$log"; kill -9 $pid; fi
    sleep 1
  done
  wait $pid; local rc=$?
  echo "RC $rc $(tail -1 "$log.time")" >> "$log"
  return $rc
}
rc=0
for k in 1 5 10; do
  f=$OUT/MW_8x8_${k}L.oas
  guard "$OUT/MW_8x8_${k}L.log" "$DB/bin2oas" --largest $k --tile 2x2 --bin 200000 --cell MW_8x8 "$SRC" "$f" || rc=1
  tail -2 "$OUT/MW_8x8_${k}L.log"
  "$DB/db_oasinfo" --verify-strict "$f" | grep -E "VERIFY|OPEN_MS|^CELL '" || rc=1
done
exit $rc
