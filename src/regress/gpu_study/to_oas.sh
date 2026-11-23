#!/bin/bash
# to_oas.sh in.ctr out.ctr MAGFILE OASFILE
# Rewrite a study CTR from a KLBIN .mag design to its OASIS conversion (bin2oas): swaps the
# decl path and maps Magic layer names to the L<layer>_D<datatype> names the OASIS loader uses,
# using the .mag file's own "<< name >> / KLBIN ..._L<l>_D<d>.bin" table.
in=$1; out=$2; mag=$3; oas=$4
sedf=$(mktemp)
awk '/^<< /{n=$2} /^KLBIN /{ if (match($2, /L[0-9]+_D[0-9]+/)) printf "s/\\([a-z0-9_]*\\):%s\\b/\\1:%s/g\n", n, substr($2, RSTART, RLENGTH) }' "$mag" > "$sedf"
sed -e "s#$mag#$oas#" -f "$sedf" "$in" > "$out"
rm -f "$sedf"
