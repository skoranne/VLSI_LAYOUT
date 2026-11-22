#!/usr/bin/env bash
# Build all Lean proofs, export the Lean-defined relation lists, and check that the
# independent Python generator produces identical lists.  Exit status 0 = all verified.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
export PATH="$HOME/.elan/bin:$PATH"
cd "$here/../lean"
lake build
out="$(mktemp -d)"
lake env lean --run Export.lean "$out"
python3 "$here/gen_relations.py" --check "$out"
if grep -rn --include='*.lean' -E '\bsorry\b|native_decide' Trs; then
  echo "FAIL: sorry/native_decide found"; exit 1
fi
echo "VERIFIED: Lean build, Lean/Python list agreement, no sorry/native_decide"
