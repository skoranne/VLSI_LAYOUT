# src/db regressions

- `xor_regression.sh [--quick]`: builds `db` tools and runs byte-identical R/W/R + KLayout XOR checks.
  - GDSII: `src/ovlp_sq.gds` (if real/binary), klayout-converted GDS.
  - OASIS: db-generated gear, klayout-generated gear ref, and external files under `/scratch1/...` (SDT/MW4x4*) when present; `--quick` skips `MW4x4_FLAT.oas` (459M).
  - Requires: bash, C++ toolchain, `klayout` (optional but used for XOR/refs when available).
- `ctr_oasis_gds.ctr`: sample CTR showing `decl design ... = file.gds|file.oas` usage (run with MAGPARSER in a fully-built toolchain env).

Run:
```sh
bash src/db/regressions/xor_regression.sh --quick
```
