# KLAYOUT_OPERATIONS Plan

## Scope
- Source analyzed: `/Users/skoranne/Downloads/KLAYOUT_OPS.pdf` (KLayout DRC Reference: Layer Object, 98 pages, extracted via `pypdf`).
- CTR parser analyzed: `src/controlparser.f90` (`ParseControlFile`, `exec run` operator dispatch, top-level keywords).
- Supporting implementation: `src/commonmodule.f90`, `src/design.f90`, `src/design_impl.f90`, `src/design_boolean_impl.f90`, `src/design_topo_impl.f90`, `src/band_exec.f90`.
- Note: PDF text extraction has glyph artifacts (`Xll/Ull`=`fill`, `Xrst/Urst`=`first_edges`, `Yatten`=`flatten`). Names below use KLayout intent where obvious.

## KLayout Operation Inventory (from PDF)

### Boolean / merge
| KLayout op | Meaning | MAGPARSER CTR status |
|---|---|---|
| `&`, `and` | Boolean AND | `*`, `AND`, `AND2`, `K_BOOST_CONTROL_AND` |
| `+`, `\|`, `or`, `join` | Boolean OR/join | `+`, `OR2`, `K_BOOST_CONTROL_OR` |
| `-`, `not`, `andnot` | Boolean NOT/difference | `@`, `!`, `NOT`, `FRAMENOT`, `BOOSTNOT`, `K_BOOST_CONTROL_NOT` |
| `^`, `xor` | Boolean XOR | `^`, `XOR`, `K_BOOST_CONTROL_XOR` |
| `merge`, `merged` | Merge polygons | Partial: `K_BOOST_CONTROL_MERGE` exists in `CommonModule`; no direct CTR keyword dispatch found |
| `merge_props`, `merged_props` | Merge with properties | Missing |

### Topological selectors / DRC checks
| KLayout op | Meaning | MAGPARSER CTR status |
|---|---|---|
| `interacting`, `select_interacting` | Touch/overlap select | `INTERACT` / `K_TOPO_INTERACT`; banded path exists |
| `not_interacting`, `select_not_interacting` | Complement | `NOT_INTERACT` |
| `inside`, `select_inside`, `in` | Inside containment | `INSIDE`, `INSIDE_EDGE`, `COINCIDENT_*` variants; `in` alias missing |
| `outside`, `select_outside` | Outside | `OUTSIDE`, `OUTSIDE_EDGE`, `COINCIDENT_*` variants |
| `overlapping`, `select_overlapping` | Overlap select | `OVERLAPS`/`NOT_OVERLAPS` (topological); KLayout `overlapping` semantics need mapping check |
| `covering`, `select_covering`, `enclosing`, `enclosed` | Cover/enclose | `COVERS`, `COVEREDBY`, `CONTAINS` present; KLayout naming/alias gap |
| `touch`, `cut`, `meet`, `disjoint`, `equal`, `covers`, `coveredby`, `contains` | Explicit relations | Present as `TOUCH/CUT/MEET/DISJOINT/EQUAL/COVERS/COVEREDBY/CONTAINS` and NOT variants |
| `separation`/`sep`, `space`, `width`, `overlap`, `notch`, `isolated`/`iso`, `enclosing` DRC | Edge-pair DRC checks | Missing as CTR DRC-check ops (MAGPARSER has topological selects, not edge-pair DRC rule output) |

### Geometry transforms / shape ops
| KLayout op | Meaning | MAGPARSER CTR status |
|---|---|---|
| `size`, `sized` | Polygon sizing | `SIZE` (single value), `K_BOOST_CONTROL_SIZE` |
| `move`, `moved` | Translation | Missing |
| `rotate`, `rotated` | Rotation | Missing |
| `scale`, `scaled` | Scaling | Missing |
| `transform`, `transformed` | General transform | Missing |
| `snap`, `snapped` | Grid snap | Missing |
| `extents` | BBox per object | Partial: `EXTENT` gives overall extent; per-object extents missing |
| `bbox` | Overall bbox | Partial via `EXTENT` |
| `flatten` | Flatten hierarchy | `decl hierdesign` + load flatten path; no layer-op `FLATTEN` keyword |
| `smooth`/`smoothed`, `rounded_corners` | Contour smoothing | Missing |
| `holes`, `hulls` | Polygon holes/hulls | Missing |
| `fill` (`fill`/`fill_with_left`) | Tiling fill | `GRID` is related but not equivalent to KLayout fill patterns |

### Filters / metrics / selection
| KLayout op | Meaning | MAGPARSER CTR status |
|---|---|---|
| `area`, `perimeter` | Metrics | Missing as layer filter/metric op |
| `with_area`, `without_area`, `with_perimeter`, ... | Property/bbox filters | Missing |
| `with_density`, `without_density` | Density tiles | Missing |
| `rectangles`, `squares`, `rectilinear`, `non_*` | Shape class filters | Missing |
| `texts`, `polygons`, `edges`, `edge_pairs` | Object-type layers | Missing (MAGPARSER is box/polygon-layer oriented) |
| `select`, `select_if`, `evaluate` | Expression-driven selection | Missing |

### Net/advanced
| KLayout op | Meaning | MAGPARSER CTR status |
|---|---|---|
| `nets` | Net extraction | Missing |
| `drc` | Generic DRC expression | Missing |
| `insert`, `collect*`, `pull_*`, `split_*` | Layer/dataflow utilities | Missing |

## MAGPARSER CTR Inventory (from parser)

### Top-level CTR keywords
`program`, `var`, `decl`, `systeminfo`, `info`, `exec`, `flush`, `delete`, `end`. (`#` comment lines ignored.)

### `decl` forms
`decl output <db> = <file>`, `decl design <db> = <file>`, `decl hierdesign <db> = <file>`, `decl memory <db> = nothing`.

### `var` settings
`temp_folder`, `band_boxes`, `abort_on_xor`, `abort_on_assert_zero`, `debug_verbosity`, `temporary_layers`, `cluster_tile_boxes/cluster_tiles` (cluster frontend directive), `trs_rules/trs_enable` (TRS compile-time directive).

### `exec` forms
`exec init <db:layer>`, `exec run <lhs> = <rhs1> <op> <rhs2>`, `exec hierextent <db>`, `exec hierinfo <db>`, `exec modedesign <name> = <file>`. Parser comment mentions `run/push/group`, but `push/group` dispatch is not implemented in `controlparser.f90`; `EvaluateExpression` exists but is inactive/commented in exec path.

### `exec run` operators (single-char)
| Op | Dispatch | Notes |
|---|---|---|
| `+` | `BandedRun(K_BAND_OR)` or `CalculateBoostOperation(...,K_BOOST_CONTROL_OR,0)` | Band-eligible fast path |
| `*` | `BandedRun(K_BAND_AND)` or `CalculateAND` | |
| `@` | `BandedRun(K_BAND_NOT)` or `CalculateNOT` | |
| `~` | `BandedRun(K_BAND_FRAMENOT)` or `CalculateFrameNOT` | MAGPARSER-specific |
| `!` | `BandedRun(K_BAND_BOOSTNOT)` or `CalculateBoostOperation(...,K_BOOST_CONTROL_NOT,0)` | |
| `^` | `BandedRun(K_BAND_XOR)` or `CalculateBoostOperation(...,K_BOOST_CONTROL_XOR,0)` | abort_on_xor check |

### `exec run` text operators
`OR2`, `ASSERT_ZERO`, `AND2`, `NOT`, `FRAMENOT`, `EXTENT`, `AND` (single-layer self-overlap), `XOR`, `INTERACT`, `NOT_INTERACT`, `CUT`, `NOT_CUT`, `TOUCH`, `NOT_TOUCH`, `INSIDE`, `NOT_INSIDE`, `OUTSIDE`, `NOT_OUTSIDE`, `INSIDE_EDGE`, `NOT_INSIDE_EDGE`, `OUTSIDE_EDGE`, `NOT_OUTSIDE_EDGE`, `COINCIDENT_INSIDE_EDGE`, `NOT_COINCIDENT_INSIDE_EDGE`, `COINCIDENT_OUTSIDE_EDGE`, `NOT_COINCIDENT_OUTSIDE_EDGE`, `EQUAL`, `NOT_EQUAL`, `DISJOINT`, `NOT_DISJOINT`, `MEET`, `NOT_MEET`, `OVERLAPS`, `NOT_OVERLAPS`, `COVERS`, `NOT_COVERS`, `COVEREDBY`, `NOT_COVEREDBY`, `CONTAINS`, `NOT_CONTAINS`, `COPY`, `GRID`, `GROW`, `SHRINK`, `SIZE`, `WORMHOLE` (parsed/logged, implementation TBD).

## Existing Comparison / Benchmark Infrastructure
- `src/boolean_ops.rb`: KLayout RBA batch boolean op (AND/OR/NOT/XOR) over two GDS/OASIS/bin inputs, writes output layout; good seed for op-by-op golden harness.
- `src/db/regressions/xor_regression.sh`: db roundtrip + optional KLayout per-layer XOR check (flatten top cell, `Region ^ Region` empty). Supports GDS/OASIS, quick mode, external designs.
- `src/db/regressions/klayout_flat_xor.py`: KLayout 0.30.3 Python flat per-layer shape count/area/XOR comparison.
- `src/db/regressions/mpw_merge_check.rb`: hierarchy-aware cell/layer/instance/property/XOR equivalence check.
- `src/topo_regressions/`: CTR/topology regression corpus and generators (`ctrs*`, `gen_*.py`).
- MAGPARSER in-run checks: `ASSERT_ZERO` op, `abort_on_xor`, `MAGPARSER_LAYER_DIGEST` env (order-independent digest for cross-run comparison), `band_boxes`/banded paths.

## Gap Analysis

### High-priority (KLayout DRC/core parity)
1. **Aliases/normalization**: accept `OR/AND/NOT/XOR/JOIN/MERGE/MINUS` text aliases mapping to existing ops.
2. **MERGE op**: expose `K_BOOST_CONTROL_MERGE` as CTR `MERGE`/`MERGED`.
3. **MOVE/TRANSLATE**: add `MOVE dx dy` (layer transform; easy, high utility).
4. **SCALE/ROTATE/MIRROR/TRANSFORM**: add affine transform ops; define MAGPARSER coordinate/precision semantics.
5. **EXTENTS per-object**: add `EXTENTS` (plural) returning bbox per shape; keep `EXTENT` as overall bbox.
6. **SIZE variants**: support anisotropic `SIZE dx dy` and optional corner mode if representable.
7. **AREA/PERIMETER filters**: `WITH_AREA`, `WITH_PERIMETER`, `AREA` metric output/report command.
8. **DRC checks**: `WIDTH`, `SPACE/SEPARATION`, `ENCLOSING`, `OVERLAP`, `NOTCH` as edge-pair-producing/reporting ops; requires edge-pair layer/data model or report sink.

### Medium-priority
9. **Shape filters**: `RECTANGLES`, `SQUARES`, `RECTILINEAR`, `HOLES`, `HULLS`.
10. **Snap/grid**: `SNAP`, `ONGRID`.
11. **Fill**: KLayout-style `FILL`; compare with existing `GRID`.
12. **Split/pull**: `SPLIT_*`, `PULL_*` convenience wrappers over existing topological selects.
13. **Density**: `WITH_DENSITY`/`WITHOUT_DENSITY` tiled metrics.

### Low-priority / architectural
14. **Edges/edge-pairs/texts/polygons object model**: KLayout has typed layers; MAGPARSER currently box/layer-centric. Requires data model extension.
15. **Expression engine**: `select_if`, `evaluate`, `drc` generic expressions.
16. **Properties/net extraction**: `nets`, `select_props`, `map_props`.

## Implementation Plan

### Phase 0 - Baseline and spec
- [ ] Freeze operation semantics table: KLayout name, MAGPARSER name, operand types, output type, coordinate units, empty-layer behavior.
- [ ] Add `docs/` or extend this file with canonical CTR syntax for each op.
- [ ] Add parser acceptance tests for aliases and new keywords (`src/test_*` or CTR fixtures under `src/*.ctr`).

### Phase 1 - Low-risk aliases and missing simple ops
- [ ] `src/controlparser.f90`: normalize aliases (`OR->+`, `AND->*` text, `XOR`, `NOT`, `JOIN`, `MERGE`, `SUBTRACT/MINUS` where safe).
- [ ] Expose `MERGE` via `CalculateBoostOperation(...,K_BOOST_CONTROL_MERGE,...)`.
- [ ] Implement `MOVE dx dy`, `EXTENTS` per-object bbox, `COPY` alias sanity, `FLATTEN` no-op/explicit wrapper where applicable.
- [ ] Add unit CTRs: `src/test_klayout_alias_ops.ctr`, expected-output checks.

### Phase 2 - Transform/sizing parity
- [ ] Implement affine transform core in Fortran/C++ (reuse Boost.Geometry if appropriate): `MOVE`, `SCALE`, `ROTATE`, `MIRROR`, `TRANSFORM`.
- [ ] Extend `SIZE` to `dx dy` and mode flags; keep backward-compatible single-value path.
- [ ] Add precision/DBU tests; validate against KLayout `klayout -b -r`.

### Phase 3 - Filters and metrics
- [ ] Add shape metrics: area/perimeter/bbox width/height/aspect ratio.
- [ ] Implement `WITH_AREA/WITHOUT_AREA/WITH_PERIMETER/...` as layer-producing filters.
- [ ] Add report mode for metrics (CSV/stdout) to support benchmarking.

### Phase 4 - DRC edge-pair checks
- [ ] Design edge/edge-pair representation (new layer kind or separate container).
- [ ] Implement `WIDTH`, `SPACE/SEPARATION`, `ENCLOSING/INSIDE/OUTSIDE` DRC checks producing edge pairs/markers.
- [ ] Map KLayout DRC options subset: projection, metrics, min/max distances.

### Phase 5 - Validation and performance comparison
- [ ] Build op-by-op conformance matrix: same input GDS/OASIS/MAG, compare MAGPARSER vs KLayout.
- [ ] Reuse/extend existing harnesses: `src/boolean_ops.rb` for KLayout boolean reference, `src/db/regressions/klayout_flat_xor.py` for per-layer XOR/count/area, `src/db/regressions/xor_regression.sh` for db roundtrip+XOR; add CTR-driven MAGPARSER vs KLayout wrapper.
- [ ] Use KLayout batch mode (`klayout -b -r ...`) as golden reference; normalize before compare (merged, DBU/precision aligned, flat vs flat unless explicitly labeled).
- [ ] Correctness checks: `MAGPARSER_LAYER_DIGEST`, output box/polygon count, per-layer area, XOR-empty diff (KLayout Region XOR).
- [ ] Benchmark methodology:
  - Inputs: small synthetic, `examples/*.mag`, `data/FPU`, `data/SDT`, MPW OASIS (`/scratch1/.../mpw-007/slot-*` when available).
  - Ops: AND/OR/NOT/XOR/SIZE/GROW/SHRINK/INTERACT/INSIDE/OUTSIDE/MERGE/MOVE/EXTENTS as implemented.
  - Metrics: wall time, RSS peak, output count, area/digest, XOR area vs KLayout.
  - Runs: warm/cold, single-thread, OpenMP, banded/GPU where supported; KLayout flat/deep labeled separately.
  - Record hardware, compiler (`ifx`/`nvfortran`), flags, git rev, KLayout version.
- [ ] Produce `KLAYOUT_OPERATIONS_BENCH.md` with tables and raw logs under `build_validation/` or `logs/`.

## Risks / Decisions
- **Data model mismatch**: KLayout layers can be polygons/edges/edge-pairs/texts; MAGPARSER is primarily box/layer with RTree/Boost polygon paths. DRC parity may require a typed-layer abstraction.
- **Semantic mismatch**: KLayout `inside/outside/interacting` have edge/polygon variants and hierarchical/deep behavior; MAGPARSER ops are flat/box-level in many paths. Document exact semantics per op.
- **Performance fairness**: compare equivalent modes (flat-to-flat, merged-to-merged, same DBU/precision). Avoid comparing KLayout deep hierarchical against MAGPARSER flat unless explicitly labeled.
- **Correctness before speed**: add golden-output diffs before GPU/banded optimization of new ops.

## Suggested first implementation slice
1. `MERGE` keyword + alias normalization in `src/controlparser.f90`.
2. `MOVE` and `EXTENTS` (per-shape bbox) with tests.
3. Conformance harness script: run same op in MAGPARSER CTR and KLayout Python/Ruby batch, compare area/count/digest.
