# src/db regressions

## db-level (R/W/R + KLayout XOR)
- `xor_regression.sh [--quick]`: builds `db` tools and runs byte-identical R/W/R + KLayout XOR checks.
  - GDSII: `src/ovlp_sq.gds` (if real/binary), klayout-converted GDS.
  - OASIS: db-generated gear, klayout-generated gear ref, and external files under `/scratch1/...` (SDT/MW4x4*) when present; `--quick` skips `MW4x4_FLAT.oas` (459M).
  - Requires: bash, C++ toolchain, `klayout` (optional but used for XOR/refs when available).

```sh
bash src/db/regressions/xor_regression.sh --quick
```

## MAGPARSER CTR regressions (OASIS/GDSII direct frontend)
Run with a fully built MAGPARSER (e.g. `make -C src -f Makefile.ifx` or `Makefile.nvf` on target host):
- `SDT_oasis.ctr`: equivalent flow to `src/SDT.ctr` but loads `/scratch1/.../SMALL_SDT/SDT_16.oas` via OASIS frontend (layers `65:20 diff, 66:20 poly, 67:20 li1, 68:20 met1`) and runs the same boolean/size/XOR/ASSERT_ZERO checks.
- `MW4x4_HIER_oasis.ctr`: OASIS direct-frontend regression on `/scratch1/.../MW4x4_HIER.oas` with boolean/size/XOR/ASSERT_ZERO checks.
- `ovlp_sq_gds.ctr`: GDSII direct-frontend sanity/XOR regression using `src/ovlp_sq.gds`.
- `ctr_oasis_gds.ctr`: minimal sample showing `decl design ... = file.gds|file.oas` and layer refs.

Example:
```sh
cd src && ./MAGPARSER.exe db/regressions/SDT_oasis.ctr 16
```
Notes: external `/scratch1/...` paths and some in-repo layouts may need to be present (non-LFS) on the test machine; adjust `temp_folder`/paths as needed for x86-64+IFX runs.
