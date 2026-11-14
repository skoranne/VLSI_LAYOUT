#!/usr/bin/env bash
# Run Boolean AND/OR/NOT/XOR regression for MAGPARSER.
# Prefers NVIDIA/nvf build (Makefile.nvf). Run from src/ or from repo root.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SRC="$ROOT/src"
cd "$SRC"

NVF_BIN="${NVF_BIN:-/opt/nvidia/hpc_sdk/Linux_aarch64/26.1/compilers/bin}"
export PATH="$NVF_BIN:$PATH"

BUILD_NVF="${BUILD_NVF:-0}"
if [[ "$BUILD_NVF" == "1" ]]; then
  echo "==> Building MAGPARSER.exe with Makefile.nvf"
  make -f Makefile.nvf clean || true
  make -f Makefile.nvf all
fi

EXE="${MAGPARSER_EXE:-./MAGPARSER.exe}"
if [[ ! -x "$EXE" ]]; then
  echo "ERROR: $EXE not found or not executable. Set MAGPARSER_EXE or BUILD_NVF=1." >&2
  exit 1
fi

CTR="${1:-regress_boolean_ops.ctr}"
echo "==> Running boolean regression: $CTR"
echo "    exe: $EXE"
"$EXE" "$CTR"
echo "==> Boolean regression PASSED"
