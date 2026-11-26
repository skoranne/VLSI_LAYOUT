#!/bin/bash
# Any-angle MAGPARSER on the rotation suite: regress/rotation/rot_ops.ctr.in with `var anyangle 1`
# at every angle, compared with the KLayout measures of the rotation run (kl.log of
# regress/rotation/out/TAG/Rxxx). Usage: run_mag_rot.sh TAG ["R000 R030 ..."]
# env: MAGPARSER (default ../../MAGPARSER.exe), ROT_DIR (rotated .oas), FORCE_TRAP=1 (also send
# Manhattan layers through the engine).
set -u
TAG=${1:?TAG}; HERE=$(cd "$(dirname "$0")" && pwd); SRC=$(cd "$HERE/../.." && pwd)
MAG=${MAGPARSER:-$SRC/MAGPARSER.exe}
ROT_DIR=${ROT_DIR:-/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/ROTATION}
ANGLES=${2:-$(ls -d $SRC/regress/rotation/out/$TAG/R* 2>/dev/null | xargs -n1 basename | tr '\n' ' ')}
OUT=$HERE/out/mag_$TAG; mkdir -p "$OUT"
for R in $ANGLES; do
  d=$OUT/$R; mkdir -p "$d/tmp"
  sed -e "s#@FILE@#$ROT_DIR/${TAG}_$R.oas#; s#@TAG@#${TAG}_$R#; s#@TMP@#$d/tmp#; s#^var temporary_layers 2#var temporary_layers 2\nvar anyangle 1#" \
      "$SRC/regress/rotation/rot_ops.ctr.in" > "$d/rot_ops.ctr"
  ( cd "$d" && MAGPARSER_FORCE_TRAP=${FORCE_TRAP:-0} /usr/bin/time -f "TIME %e s MAXRSS %M KB" \
      "$MAG" rot_ops.ctr 256 > mag.log 2> mag.log.err )
  rm -rf "$d/tmp"
  python3 "$HERE/compare_mag_kl.py" "$d/mag.log" "$SRC/regress/rotation/out/$TAG/$R/kl.log" "$R" "$SRC/regress/rotation/out/$TAG/R000/mag.log" | tee "$d/compare.txt"
  grep TIME "$d/mag.log.err" | sed "s/^/$R /"
done
