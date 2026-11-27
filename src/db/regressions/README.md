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

Example (the 2nd argument is `max_layers`; SDT_16.oas has 33 drawn layers, layers beyond the
limit are dropped with a `WARNING: ... layer(s) not loaded` line):
```sh
cd src && ./MAGPARSER.exe db/regressions/SDT_oasis.ctr 64
```

## Hierarchical GDSII/OASIS load (`hier_load/`)
- `run_hier_load_regression.sh [MAGPARSER.exe] [workdir]`: KLayout builds a hierarchical layout
  (`gen_hier_input.py`: all 8 orthogonal orientations, a mirrored regular array, a 3-level
  rotated/mirrored nest, abutting flipped rows, irregular OASIS repetitions) as `.oas` and `.gds`;
  MAGPARSER loads each with `decl design` and writes it with `exec oasout`; `hier_check.py` XORs the
  result with KLayout's flattening per layer. Guards the fix where `decl design` loaded only each
  cell's local shapes (SDT_16.oas lost the standard-cell met1 rails, 3% of the area).
```sh
bash src/db/regressions/hier_load/run_hier_load_regression.sh src/MAGPARSER.exe
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

## DEF cross-check (`db_defcheck`)
- `defcheck_mkALU.sh [--gds] [workdir]`: checks `64bit_ALU/gds/mkALU.oas` (and with `--gds`, `mkALU.gds`)
  against `def/mkALU.def`, and against the same DEF gzip'ed, using the SKY130 HD tech LEF and cell LEFs.
  Checks: DIEAREA, every COMPONENT against a top-cell placement, point probes for every wire/via/RECT/PIN,
  a per-layer XOR of the DEF geometry with the top cell (15 layers, 0 area), and every LEF PIN/OBS
  rectangle against the cell's geometry. It also runs a mutated `.def.gz` that must fail with each error
  located. Overrides: `MKALU_DIR`, `SKY130_HD`. Takes about 1 minute (1.5 minutes with `--gds`).
```sh
bash src/db/regressions/defcheck_mkALU.sh
src/db/db_defcheck --lef tech.tlef --lef cells.lef design.def.gz layout.oas   # one-off
```

## DEF -> OASIS (`db_def2oas`)
- `def2oas_mkALU.sh [--klayout-xor] [workdir]`:
  - Converts `data/def2oas_props.def`, a small DEF that holds every kind of name and property.
    KLayout must read all of them back (`def2oas_props_check.py`): instance names (property
    61), DEF_STATUS/ATTR/PROPERTY, DEF_NET/DEF_PIN on shapes, pin labels, net and pin info,
    ROW/TRACKS/PROPERTYDEFINITIONS, and the transforms of N/FS/FN/S placements.
  - Converts `mkALU.def.gz` with the cells from `mkALU.oas`. db_defcheck must PASS with 0 XOR
    on 15 layers, and KLayout must see all 301203 placements named and DEF_NET on every met1
    shape.
  - `--klayout-xor` (+7 min) also XORs the result against `mkALU.oas`. The only expected
    difference is 81/14, a P&R marker that is not in the DEF.
```sh
bash src/db/regressions/def2oas_mkALU.sh
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
