#!/usr/bin/env bash
# Large-scale self-checking MAGPARSER regression using FPU_8x8 and SDT_16.
# Exercises most defined operations (topological predicates, boolean/char+text ops,
# EXTENT/COPY/AND-merge/GROW/SIZE/GRID, hierextent/hierinfo, decl/init/flush/delete, etc.)
# with ASSERT_ZERO-based identity checks. Exit code 0 on success.
#
# Note: With native OASIS reader as default, correct parsing of FPU_8x8.oas/SDT_16.oas
# requires KLayout-assisted open (LDB_OASIS_KLAYOUT=1) when in-house native parsing is
# not yet semantically complete for these files. Set LDB_OASIS_KLAYOUT=0 to exercise the
# pure native path.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT/src"
CTR="${1:-$ROOT/Regressions/fpu_sdt_allops_selfcheck.ctr}"
EXE="${MAGPARSER_EXE:-$SRC/MAGPARSER.exe}"

# KLayout-assisted OASIS open for correct FPU/SDT layer semantics by default.
export LDB_OASIS_KLAYOUT="${LDB_OASIS_KLAYOUT:-1}"

# Runtime libs (macOS/ifort oneapi as used in this repo environment)
if [[ "$(uname -s)" == "Darwin" ]]; then
  export DYLD_LIBRARY_PATH="${DYLD_LIBRARY_PATH:-/opt/intel/oneapi/intelpython/python3.9/envs/2022.2.1/lib:/usr/local/opt/zstd/lib:/opt/intel/oneapi/compiler/2022.2.1/mac/compiler/lib:/usr/lib}"
fi

detect_ncpu() {
  sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4
}
export OMP_NUM_THREADS="${OMP_NUM_THREADS:-$(detect_ncpu)}"

mkdir -p /tmp/fpu_sdt_allops

if [[ ! -x "$EXE" ]]; then
  echo "ERROR: $EXE not found or not executable. Build MAGPARSER.exe first (e.g. make -f Makefile.mac in src/)." >&2
  exit 1
fi

echo "==> Running FPU+SDT all-ops self-check"
echo "    exe: $EXE"
echo "    ctr: $CTR"
echo "    LDB_OASIS_KLAYOUT=$LDB_OASIS_KLAYOUT  OMP_NUM_THREADS=$OMP_NUM_THREADS"

cd "$SRC"
"$EXE" "$CTR" 10
echo "==> FPU+SDT all-ops self-check PASSED"
