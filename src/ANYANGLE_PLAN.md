# Any-Angle (Non-Rectilinear) Engine: Status, TODOs, GPU & 100B-Shape Plan

## 1. Current Status (`anyangle-booleans` branch)

| Capability | Status | Modules / Entrypoints | Verification |
|---|---|---|---|
| Exact integer/i128 predicates & canonical trapezoids | Done | `wide_int.f90`, `exact_predicates.f90`, `trapezoid.f90` (`!$omp declare target`) | `test_wide_int.exe`, `test_predicates.exe`, `test_trapoverlap.exe` |
| Single-layer OR (snap rounding + band-parallel sweep) | Done | `trap_merge.f90`, `TRAPMERGE.exe` | `test_trapmerge.exe`, `regress/anyangle/run_merge.sh` |
| Two-layer Booleans (`OR`, `AND`, `NOT`, `XOR`, `BNOTA`, `SELF2`) | Done | `trap_merge.f90`, `TRAPBOOL.exe`, `anyangle_ops.f90` | `test_trapbool.exe`, `regress/anyangle/run_bool.sh`, rational gold `regress/anyangle/gold/run_gold.sh` (max `H_inf <= 0.5 dbu` vs KLayout `1.49 dbu`) |
| Canonical PNUM & exact lattice outlines / hole counts | Done | `trap_pnum.f90`, `db/trap_out.cpp` (`ldb_trapset_polys`, `ldb_trapset_measure`, `ldb_trapset_loops`) | `kl_pnum_check.py`, `exec measure` |
| Polygon-level Topologicals (`INTERACT`, `CUT`, `TOUCH`, `INSIDE`, `OUTSIDE`, `EQUAL`, `DISJOINT`, `MEET`, `OVERLAPS`, `COVERS`, `COVEREDBY`, `CONTAINS` + `NOT_*`) | Done | `trap_topo.f90`, `anyangle_ops.f90` | `regress/anyangle/helpers/kl_topo.py` |
| Morphology (`GROW`, `SHRINK`, `SIZE`, closing/opening, isotropic mode=2 & anisotropic E/N/W/S) | Done | `trap_morph.f90`, `anyangle_ops.f90`, `controlparser.f90` | `test_trapmorph.exe`, `SDT16S_R015` vs KLayout (`pass=7 fail=0`; `id_close` 4.8x and `id_open` 10.2x lower error than KLayout) |
| FASCL properties (`AREA`, `PERIMETER`, `VERTICES`, bbox) & `CONNECT` / `NET_ID` | Done | `fascl.f90`, `fascl_connect.f90`, `property_table.f90` | `db/regressions/fascl/run_fascl_regression.sh` |

---

## 2. Remaining Functional TODOs (Concrete Implementation Plan)

### TODO-1: Any-angle OASIS output with FASCL properties (`exec oasout`)
- **Gap**: `prop_oasis.f90:132` rejects any-angle layers.
- **Plan**:
  1. Group `L%traps` by PNUM ordinal (`L%props%poly_boxes` / `poly_start`), call `ldb_trapset_polys` per PNUM polygon (or extend `db/prop_oasis_writer.cpp` with `fpo_begin_anyangle_layer` accepting CSR lattice polygons directly).
  2. Emit OASIS `RECTANGLE`/`CTRAPEZOID`/`TRAPEZOID`/`POLYGON` records with attached per-polygon `PROPERTY` records and strict tables.
  3. Verify round-trip via KLayout Python reader (`pya.Layout` + `region ^ gold == 0` and property equality).

### TODO-2: Edge-level Topological Selectors (`*_EDGE`)
- **Gap**: `AnyAngleTopoCode` in `anyangle_ops.f90` only handles polygon-level operators.
- **Plan**:
  1. Extract unbridged oriented boundary segments of `A` and `B` via `ldb_trapset_loops`.
  2. Spatial grid index on `B`'s trapezoids + boundary segments; classify each edge of `A` against `B` using exact `i128` segment-trapezoid / collinear-overlap predicates (coincident, interior, exterior, crossing).
  3. Output selected edge segments (or thin markers/edge-layer representation matching `design_topo_impl.f90`).

### TODO-3: `TRAPBOOL.exe` Morphology & Topological CLI + Rational Morphology Gold
- **Plan**:
  1. Extend `trapbool_main.f90` with `-morph GROW:d,SHRINK:d` and `-topo OP` so standalone regression scripts can test morphology and topologicals without full CTR overhead.
  2. Extend `regress/anyangle/gold/gold_check.py` with an exact rational Minkowski-sum / edge-offset oracle (exact rational offset lines + exact miter intersections for `d`) on rotated clips (`15°`, `30°`, `45°`) to bound `H_inf` of `TrapMorph` against exact rational morphology (just as `gold_check.py` does for Booleans).
  3. Eliminate residual 1-dbu concave-notch pinholes in `TrapMorph` on rotated staircased edges by merging near-collinear consecutive collar segments before snap rounding (or offsetting the simplified contour when edge length <= 2 dbu).

---

## 3. Accuracy & Verification Strategy

1. **Exact Rational-Integer Gold (`fractions.Fraction`)**:
   - Booleans: slab sweep cut at every exact vertex and crossing (`regress/anyangle/gold/gold_check.py`), asserting `H_inf <= 0.5 dbu` (theoretical snap-rounding tube) and exact set identities (`OR = NOT + AND + BNOTA`, `XOR = OR - AND`).
   - Morphology: exact rational offset-polygon oracle on 15°/30°/45° clips asserting `H_inf <= 1.0 dbu` and exact inclusion (`A - GROW(A) == 0`, `SHRINK(A) - A == 0`).
   - Topologicals: exact rational DE-9IM matrix (`I/B/E` intersections) on random & rotated clips.
2. **15°-Increment Rotation Invariance (`0°, 15°, ..., 180°`)**:
   - Full designs (`SDT16S`, `MW`, `SDT6X6`) via `regress/anyangle/run_mag_rot.sh` and `regress/anyangle/run_bool.sh`.
   - Check polygon count, hole count, `|dA| <= 0.7072 * P`, `|dP| <= 1.5 * V`, and algebraic identities.
3. **Differential Fuzzing**:
   - `test_trapmerge.exe`, `test_trapbool.exe`, `test_trapmorph.exe`, `test_trapoverlap.exe` with brute-force exact winding-number sampling + band-count invariance (`nbands = 1` vs `nbands = K`).

---

## 4. GPU Deployment Plan (NVIDIA Grace-Blackwell GB10 / H100 / B200)

Current GPU footprint: `gpu_morton.cu` (CUB/Thrust radix sort for `CompactSortPairs`), `trapezoid.f90` (`!$omp declare target` on all exact predicates). `TrapSnapRound`, `TrapSweepUnion`, `TrapPNum`, `TrapTopo`, `TrapMorph` currently execute on CPU OpenMP.

| Phase | Bottleneck on Large Layouts | GPU Kernel Design | Expected Speedup |
|---|---|---|---|
| **G1: Snap-Rounding Grid & Crossings** (`TrapSnapRound`) | `t_grid`, `t_cross`, `t_hits` (~55–70% of Boolean time) | 1. GPU edge->cell registration (2-pass count + CUB prefix sum + scatter).<br>2. GPU CUB radix sort `(cell_id, edge_id)` (`gpu_morton.cu`).<br>3. CUDA kernel (1 warp/cell, `__int128` exact orientation & rational crossing) emitting hot pixels.<br>4. GPU CUB sort + unique hot pixels.<br>5. GPU fragment split & identical-fragment winding reduction (`cub::DeviceReduce::ReduceByKey`). | 8–20x |
| **G2: Band-Parallel Sweep** (`TrapSweepUnion`) | `t_sweep` (~25–35% of Boolean time) | Partition Y into `B = 4096..65536` narrow horizontal bands (so active-edge set per band fits in GPU L1/shared memory / L2). Launch 1 warp or CTA per band using a flat sorted active array (replacing pointer-chasing treap for small active sets `<= 256`, falling back to CPU/HBM treap only for ultra-dense bands). | 5–15x |
| **G3: Morphology Collar Generation** (`TrapMorph`) | `t_loops` + `t_collar` | Embarrassingly parallel CUDA kernel: 1 thread per loop vertex computes `(ox, oy)`, miter/bevel cap, and emits CSR collar polygons directly in device memory feeding **G1** (zero host-device round-trip on GB10 unified memory). | 10–30x |
| **G4: Topological & CONNECT Pair Filtering** (`TrapTopo`, `fascl_connect`) | `t_pairs` broad + narrow phase | Reuse `trapezoid.f90`'s existing `!$omp declare target` `trap_overlap_positive` / `trap_contact` kernels over GPU spatial grid / LBVH pairs. | 10–25x |

---

## 5. 100-Billion-Shape Scalability Architecture (Out-of-Core Banded Streaming)

At **100B shapes**:
- Raw vertices (~400B int32 pairs) = ~3.2 TB; canonical trapezoids (~150B * 40 B) = ~6 TB.
- **Cannot reside uncompressed in host RAM or GPU HBM.**

### Streaming Pipeline (extending `band_exec.f90` + `db/mvi` + `CompactLayer`)
1. **Compressed On-Disk Spatial Tile/Band Index (`MVI` / `CompressedTrapezoidChunk`)**:
   - Store any-angle layers as Y-partitioned bands of compressed canonical trapezoids (delta-encoded `y1, dy` + dictionary-encoded slope `(dx, dy)` + zstd/nvCOMP decompression directly into GPU memory). Typical VLSI designs have <64 distinct slopes per rotated layer -> ~4–8 B/trapezoid compressed (~600 GB–1.2 TB on NVMe/Lustre for 100B shapes, ~50–100 MB per active band).
2. **Band-by-Band Streaming Execution (`AnyAngleBandedRun`)**:
   - Divide die Y-extent into `M` bands sized so `(A_band + B_band + halo)` fits in GPU memory (~2–4 GB/band, e.g., 50M–100M shapes/band).
   - **Halo `H`**:
     - Booleans (`OR/AND/NOT/XOR`): `H = 1 dbu` (snap-rounding pixel radius!).
     - Morphology (`GROW/SHRINK/SIZE d`): `H = |d| + 1 dbu`.
     - Clip input edges/trapezoids to `[y_lo - H, y_hi + H]`, run GPU **G1 + G2** on band `k`, discard output outside `[y_lo, y_hi]`, stitch exact seam trapezoids with band `k-1` in a streaming 1-row boundary buffer, and compress/write band `k` to disk.
   - **Double-buffered async pipeline**: NVMe/nvCOMP decode band `k+1` || GPU compute band `k` || nvCOMP encode/write band `k-1`.
3. **Out-of-Core PNUM / Topologicals / CONNECT**:
   - Pass 1 (per band, streaming): local PNUM + local seam-edge table at `y = y_hi`.
   - Seam Resolve (host/GPU, tiny): union-find across `M-1` band seams (`O(perimeter)` rather than `O(area)`).
   - Pass 2 (per band, streaming): relabel local polygon IDs to canonical global IDs and evaluate FASCL / topological selection.

---

## 6. Cross-Compiler Performance Discipline (`ifx` vs `nvfortran`)

- **Intel `ifx` polymorphism hazard**: Never use `class(...)`, type-bound procedures on concrete types, or derived types with `allocatable` components inside OpenMP hot loops (`ifx` emits static global RTTI/descriptor writes causing L1 cache-line ping-pong across Xeon cores; see commits `55b8c2f`, `7138cb2`, and `AGENTS.md`). All `trap_*.f90` modules use pure POD/SoA `type(...)` arrays.
- **`nvfortran` 26.1 workarounds**: Avoid `associate` blocks inside `recursive` functions and pass assumed-size `ids(*)` instead of assumed-shape `ids(:)` in recursive FASCL evaluators (`fascl.f90`).
