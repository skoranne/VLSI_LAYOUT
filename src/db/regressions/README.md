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

## FASCL per-polygon properties -> OASIS (`fascl/`)
- `run_fascl_regression.sh [MAGPARSER.exe] [workdir]`: generates `fascl_in.gds` with KLayout
  (`gen_fascl_input.py`: rects, L/U shapes, rings, two holes, overlapping and corner-touching boxes,
  lattices and irregular placements), runs `fascl_props.ctr` (FASCL lambdas defining INT, FLOAT,
  STRING, STRUCT and WHEN-guarded properties; `propdump`; `oasout` with STRICT+CBLOCK+repetitions,
  64-byte CBLOCKs, no repetitions, plain; `.mag` output of a COPY to `*.oas`) and checks each OASIS
  file with `fascl_check.py`: geometry XOR, one shape per connected polygon, every property value
  recomputed from the input, CSV dumps, STRICT table offsets, CBLOCKs, repetition types and instance
  counts, PROPERTY-REPEAT; plus a lambda runtime type error. Exit 0 = pass. See `src/FASCL.md`.
```sh
bash src/db/regressions/fascl/run_fascl_regression.sh src/MAGPARSER.exe
```

## MPW_MERGE (shuttle assembly)
- `mpw_merge_check.rb`: KLayout batch script that compares one slot of an `MPW_MERGE`
  output with its original file. It walks the hierarchy cell by cell and compares
  shape counts, a per-layer local XOR, the texts, and instances with their transforms,
  arrays and properties. Exit status 0 means the two are equivalent.
```sh
src/db/MPW_MERGE --verify -o two.oas slot-001/.../caravel_00070f74.oas slot-002/.../caravel_0007bc4e.oas
klayout -b -r src/db/regressions/mpw_merge_check.rb -rd a=slot-001/.../caravel_00070f74.oas \
        -rd b=two.oas -rd ta=caravel_00070f74 -rd tb=S001_caravel_00070f74
```
