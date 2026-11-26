# Any-angle single-layer OR (merge)

`TRAPMERGE.exe IN OUT.oas L/D [L/D ...] [-b NBANDS] [-round]` (built in `src/` with
`make -f Makefile.nvf TRAPMERGE.exe`) merges every polygon of each layer, at any angle:

1. **Snap rounding** (Hobby / Guibas-Marimont, `trap_merge.f90`). Every vertex and every edge
   crossing gets a hot pixel. Each edge is rerouted through the pixel centres it meets. After
   this no fragments cross, and overlapping fragments are identical. Exact int128 arithmetic
   throughout.
2. **Band-parallel sweep** under the nonzero winding rule. Output is maximal horizontal
   trapezoids with lattice-line sides, stitched across bands, so the result does not depend
   on the band count.
3. **Exact OASIS output** (`db/trap_out.cpp`):
   - A trapezoid whose four corners are on the grid becomes one RECTANGLE, CTRAPEZOID
     (45-degree shapes) or TRAPEZOID record.
   - Trapezoids with corners between grid points are joined across those corners into regions
     whose vertices are all on the grid. These are written as POLYGONs, with holes bridged
     exactly.
   - Nothing is rounded. `-round` writes rounded per-trapezoid records instead (lossy).

The only geometric change is snap rounding: every output edge stays within one pixel of the
exact union.

| script | what |
|--------|------|
| `run_synth.sh` | rotated rings with holes and islands (`mk_ring.py`); 45-degree shapes and bent paths (`mk_45.py`, which must produce RECTANGLE/CTRAPEZOID records only). Must match KLayout exactly: XOR 0 and identical 2A |
| `run_merge.sh TAG ["ANGLES"]` | the rotated designs of `../rotation` (SkyWater 130 layers poly, diff, li1, met1, met2), each checked by `kl_merge_check.py` |
| `summarize.py out/TAG [--perf]` | pass/fail table, rotation invariance (dA) and the time breakdown |

Pass criteria:
- anomalies 0 (internal checks of the sweep and the output);
- the output, read back by KLayout, has exactly our merged area (raw sum of the shapes'
  2A; the shapes are interior-disjoint);
- `kmd` (KLayout's merge of our output minus its raw 2A) is tiny: 0 at multiples of 90, and
  at most 1e-8 relative otherwise. KLayout's merge snaps a vertex lying within a pixel of
  another edge. For example, a trapezoid corner 0.4 dbu from a neighbouring 106/107-slope
  edge shrinks a sliver hole of 2A 361 to 319. That is KLayout rounding, not an output
  error;
- at multiples of 90 degrees: XOR 0 against KLayout's merge and identical area;
- at other angles: every XOR sliver is thinner than 2 dbu (`xor_wide1` = 0), i.e. only
  snap-rounding differences.

The unit fuzz test is `src/test_trapmerge.exe [ncases] [seed]`. It checks against brute-force
winding numbers, 1-band vs k-band identity, fragment non-crossing, and area integrality.
