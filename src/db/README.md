# src/db: GDSII/OASIS reader-writer (WIP)

Goal and constraints: see `dbplan.txt`.

Planned components:
- `layoutdb.h/.cpp`: core C API + C++ implementation (mmap reader, record parsers, lazy repetitions)
- GDSII stream parser/writer, OASIS parser/writer (CBLOCK via nvComp when available)
- FFI-friendly C API for Fortran `iso_c_binding` and Julia `ccall`
- Test CLIs: `db_extent`, `db_extent_by_cell_layer`, `db_hierarchy`, `db_propnames`

Build notes: integrate with `src/` makefiles/toolchains as needed; avoid hardcoding local paths.

## bin2oas: KLBIN layer sets to compact OASIS

`bin2oas in.mag out.oas` (or `bin2oas out.oas L:D file.bin ...`) writes the KLBIN files
named in a Magic `.mag` (`KLBIN .../X_L<layer>_D<datatype>.bin`) as one flat OASIS cell.
Each layer is sorted by shape (w,h), then row and x. RECTANGLE records use modal
width, height and layer with XYRELATIVE deltas, so most records carry only an x delta.
The stream is deflated in CBLOCKs (`--cblock-mb`, default 8). Measured sizes:
SMALL_SDT went from 176 MB to 12.6 MB, and SDT16 (456M boxes) from 30 GB to 110 MB
(0.24 B/box). Options are `--grid N` (grid steps per micron in START, default 1000),
`--dedup` (drop exact duplicate boxes) and `--cell NAME`. CTRs reference the layers as
`d1:L66_D20` (or `66:20`). `src/regress/gpu_study/to_oas.sh` rewrites a KLBIN-based
CTR to use them.

## Compact layers, banded operations and MAGVIEWER

Streamed flat OASIS feeds the compact layer store (`src/compact_layer.f90`). Each box is
stored as a Morton code of its centre plus a shape id. Keys are packed in blocks of 128,
with one MBR per block. When `2*cbits + sbits > 62`, the layer switches to a wide layout
that keeps the shape ids in a separate stream.

**Operations** (`MAGPARSER_COMPACT_LAYERS=1`):
- Layers are compacted when they finish loading, expanded when an op needs them, and kept
  within `MAGPARSER_COMPACT_BUDGET_GB` by an LRU.
- An expanded layer keeps its packed copy. If the layer is unchanged, re-compacting it
  costs nothing.
- `src/band_exec.f90` runs these ops in y-bands over compact inputs, so whole layers are
  never expanded: `+ * @ ~ ! ^`, GROW and SIZE (with a halo), and INTERACT when A is
  healed.
- Band size is set by `MAGPARSER_BAND_BOXES` (default 48M; 0 turns banding off) or by
  `var band_boxes N` in the CTR. Banded outputs above `MAGPARSER_BAND_MERGE_MAX` boxes
  (default 400M) stay multi-part.
- To check banded results against unbanded ones: `src/regress/gpu_study/run_band_checks.sh`.
- `MAGPARSER_COMPACT_FORCE_WIDE=1` forces the wide layout, for testing.

**MAGVIEWER** builds its quantized R-tree index directly from the compact blocks
(`CompressedSpatialIndex%build_from_compact`). SDT16 (422M unique boxes) loads into a
1.56 GB index with a 4.5 GB peak. `MAGVIEWER_COMPACT=0` selects the legacy BoxCodec/zstd
chunks, whose multi-threaded encoder has a known race. To test load and pan/zoom queries
without a display: `test_viewer_index.exe file.oas [max_layers] [windows] [frac]`
(`TVI_VERIFY=1` adds a brute-force oracle).

## MPW_MERGE: multi-project-wafer shuttle assembly

`MPW_MERGE` combines the per-slot OASIS files of an MPW shuttle into one OASIS
file. The dies sit on a grid separated by scribe lanes, and the tool also
writes an analysis report. It never flattens geometry or expands repetitions.
Each cell is copied record by record, and only its reference numbers are
renumbered.

```sh
make -C src/db MPW_MERGE
src/db/MPW_MERGE --verify -o mpw007.oas --report mpw007.txt --json mpw007.json \
    --dir /scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SKY130/mpw-007     # slot-*/tapeout/outputs/oas/*.oas
src/db/MPW_MERGE --analyze-only --dir ...                            # report only
```

**Cell sharing.** Cells with identical contents are written once, whatever
they are called. The tape-out flow prefixes every project's cells
(`BF_sky130_fd_sc_hd__inv_1`, `G2_...`), so matching names alone finds almost
nothing to share.

When one name carries different contents in different slots, the version used
by the most slots keeps the name and the others become `S<nnn>_<name>`. Each
slot's top cell is always renamed `S<nnn>_<top>`.

Two options change this:
- `--dedup-by-name` shares a cell only when both its name and its contents match.
- `--prefix-all` renames every cell and shares nothing.

**Assembly.** The grid defaults to the most nearly square arrangement, 8×5 for
40 slots; `--cols` and `--rows` override it. The site pitch is the largest die
plus the scribe width (`--scribe` µm, default 100). Each die is centred in its
site.

Three cells are generated:
- `MPW_SCRIBE` holds the scribe lanes as rectangles on `--scribe-layer`
  (default 255/0).
- `MPW_LABELS` holds a `slot-NNN <top>` text under each die on
  `--label-layer` (default 255/1).
- `MPW_TOP`, or the name given with `--top`, places everything.

**How it works.** The tool makes three passes:
1. **Analyse** every slot in parallel. It reads the name tables, then gives each
   cell two hashes, both computed with references replaced by what they refer to:
   - a content hash, in which a child counts by its contents;
   - a named hash, in which a child counts by its name.

   It also computes hierarchical bounding boxes from the shape boxes and the
   placement transforms.
2. **Resolve** output cells and names.
3. **Write.** Cells are recompressed into CBLOCKs of about 1 MiB. The
   TEXTSTRING, PROPNAME and PROPSTRING tables go at the end of the file and
   contain only the entries that are used; the forward references this needs
   are allowed by OASIS. Properties outside cells describe the input files, for
   example `S_CELL_OFFSET`, so they are dropped.

**Code layout.**
- `oasis_codec.hpp`: byte codec shared with `oasis.cpp` and `oasis_writer.cpp`.
- `oasis_rec.{hpp,cpp}`: lossless record scanner. It hands over each record's
  exact bytes plus the positions of its reference fields, and the boxes and
  placements of shapes.
- `mpw_merge.cpp`: the tool itself.

**Checking.**
- `--verify` re-reads the output: every record must decode, every reference
  must resolve, the cell count must match and `MPW_TOP` must be the only top.
- `regressions/mpw_merge_check.rb` compares one slot of a merged file with its
  original file in KLayout:

  ```sh
  klayout -b -r src/db/regressions/mpw_merge_check.rb -rd a=orig.oas -rd b=merged.oas \
      -rd ta=caravel_00070f74 -rd tb=S001_caravel_00070f74
  ```

  It walks both hierarchies cell by cell and compares, for each pair, the shape
  counts, a per-layer local XOR, the texts and the instances (transform, array,
  properties). Deep-mode `strmxor` gives the same answer but takes over an hour
  per slot.
