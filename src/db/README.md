# src/db: GDSII/OASIS reader-writer (WIP)

Goal and constraints: see `dbplan.txt`.

Planned components:
- `layoutdb.h/.cpp`: core C API + C++ implementation (mmap reader, record parsers, lazy repetitions)
- GDSII stream parser/writer, OASIS parser/writer (CBLOCK via nvComp when available)
- FFI-friendly C API for Fortran `iso_c_binding` and Julia `ccall`
- Test CLIs: `db_extent`, `db_extent_by_cell_layer`, `db_hierarchy`, `db_propnames`

Build notes: integrate with `src/` makefiles/toolchains as needed; avoid hardcoding local paths.

## bin2oas: KLBIN layer sets to compact, strict-mode OASIS

`bin2oas in.mag out.oas` (or `bin2oas out.oas L:D file.bin ...`) writes the KLBIN files
named in a Magic `.mag` (`KLBIN .../X_L<layer>_D<datatype>.bin`) as one flat OASIS cell,
through the streaming writer described below. Each layer is sorted by shape (w,h), then row
and x. `RectArrays` turns constant-pitch runs, and identical runs stacked at a constant row
pitch, into RECTANGLE repetitions (types 1-3). Arrays of fewer than `--rep-min` rectangles
(default 8) stay single RECTANGLEs, because short repetitions deflate worse than modal
rectangles. The other records use modal width, height and layer with XYRELATIVE deltas,
so most carry only an x delta. The stream is deflated in CBLOCKs (`--cblock-mb`, default 8).
`--no-rep` turns off array detection and `--no-crc` drops the CRC32 in END. On 4M routed
MW_4x4 boxes the old and new encoders both give 8.2 MB (2.05 B/box), with an empty KLayout
XOR between them. Routed metal has few true arrays, so DEFLATE already captures most of its
regularity. Memory is one layer's boxes (16 B/box) plus the open stacks. Measured sizes:
SMALL_SDT went from 176 MB to 12.6 MB, and SDT16 (456M boxes) from 30 GB to 110 MB
(0.24 B/box). CTRs reference the layers as `d1:L66_D20` (or `66:20`).
`src/regress/gpu_study/to_oas.sh` rewrites a KLBIN-based CTR to use them.

Output is **strict mode** by default (`--no-strict` gives the old layout):
- START has offset-flag 1 and END holds the table offsets.
- The CELL is written by reference number.
- A strict CELLNAME table after the geometry carries `S_CELL_OFFSET` (byte offset of the
  CELL record) and `S_BOUNDING_BOX`. The file property `S_TOP_CELL` follows START.
- A strict LAYERNAME table names each layer after its `.mag` section (`li1` for 67/20);
  generic sections such as `L66_D15` keep their number.
- Every CBLOCK holds one layer and starts with XYABSOLUTE and a fully specified
  RECTANGLE, so a CBLOCK decodes without the modal state of the one before it.

Other options:
- `--grid N`: grid steps per micron in START (default 1000).
- `--dedup`: drop exact duplicate boxes.
- `--cell NAME`: cell name.
- `--largest K`: keep the K layers with the most boxes.
- `--tile NXxNY` and `--tile-gap G`: write NX x NY translated copies of the layer set.
  The input is sorted once, and the copies never exist in memory.
- `--bin B`: order geometry in B x B database-unit bins along a Morton curve, each bin sorted
  by shape then row. CBLOCKs then cover compact regions, at some cost in size: 200 um bins
  made SMALL_SDT 9% larger.

`db_oasinfo [--verify-strict] file.oas` reads only the tables (`db/oasis_tables.cpp`,
C API `ldb_oas_info_*` in `layoutdb.h`): unit, top cell, LAYERNAMEs, and each cell's
`S_CELL_OFFSET` and `S_BOUNDING_BOX`. It also checks that every offset points at its CELL
record. Tables stored inside a CBLOCK are supported, and properties may be named inline or
by reference. Reading the tables takes about 0.1 ms at any file size; KLayout-written strict
files read as well. `regressions/oasis_layer_summary.rb` prints per-layer counts, areas and
names with the system KLayout, for checking writer output.

## OASIS writer, GDSII writer and spec conformance

`oasis_writer.{hpp,cpp}` is a streaming SEMI P39-1105 writer. Records go to the file as they
are made. The writer keeps one CBLOCK buffer, the name tables, and a few words per cell and
placement, but never the geometry.
- **Compaction.** It reuses every modal variable: layer and datatype, geometry w/h,
  XYRELATIVE positions, placement cell, text string, repetition, polygon and path point lists,
  path half-width and extensions, circle radius, and the last property name and values
  (record 29).
- **Point lists** use the shortest type: 0/1 (implicit Manhattan), 2, 3, or the shorter of
  4 and 5. Four-vertex rectangles given as polygons become RECTANGLEs, and squares use the
  S bit.
- **Repetitions.** Callers pass lattices or offset lists. They are normalised to the
  shortest type 1-11, and reuse of the previous repetition is written as type 0.
  `RectArrays` finds arrays in a sorted rectangle stream.
- **CBLOCK** is raw DEFLATE. CELL, START and END are never compressed (35.4). Optional
  `independent_cblocks` resets the modal state at each CBLOCK, for parallel and
  random-access readers (`db_mvi` reports `dependent=0`).
- **Strict mode** is the default:
  - START offset-flag 1, with the table offsets in END.
  - CELL, text strings, property names and property strings are all referenced by number.
  - The CELLNAME, TEXTSTRING, PROPNAME, PROPSTRING and LAYERNAME tables come after the
    cells, each starting its own CBLOCK (13.9).
  - `S_TOP_CELL` and `S_BOUNDING_BOXES_AVAILABLE` follow START (patched in place when the top
    cells are only known at close). Every CELLNAME carries `S_CELL_OFFSET` and a hierarchical
    `S_BOUNDING_BOX`.
  - END is exactly 256 bytes, with CRC32 (default) or CHECKSUM32.
- **Property values** are written as n-, a- or b-strings according to their content, so
  empty strings and strings with spaces are valid. Reals use the shortest real type.

C API (`layoutdb.h`):
- `ldb_write_oasis_opts(f, path, &opts)` writes the in-memory model.
  `ldb_oasis_write_opts_t` has `strict`, `cblock_kb`, `repetitions`, `validation`,
  `reencode`, `independent_cblocks` and `deflate_level`. A file read from OASIS is copied
  byte for byte unless `reencode=1`.
- `ldb_write_gdsii` copies a GDSII source losslessly. For any other model it writes HEADER
  600, UNITS from `ldb_dbu_per_micron`, SREF/AREF (STRANS bit array, MAG, ANGLE, COLROW),
  BOUNDARY, PATH (PATHTYPE, WIDTH, BGNEXTN/ENDEXTN) and TEXT. Values that do not fit GDSII
  are reported as errors.
- `db_roundtrip convert in out.{oas,gds} [--no-rep] [--no-strict] [--no-cblock]
  [--independent] [--checksum]` re-encodes any input through the model.

The model:
- Keeps the database unit (`ldb_dbu_per_micron`) and TEXT elements (`LdbCell::texts`).
- With `LDB_OPEN_SHAPES`, it also keeps exact OASIS polygons and paths next to the fractured
  boxes (`LdbCell::polys_extra`), so re-encoding is lossless for non-Manhattan shapes. This
  is opt-in: on the flat merged MW4x4 li1 file it would push peak memory from 18 GB past
  40 GB.
- Properties are attached as in 31.8-31.9: to the file after START, to the cell after CELL
  or its CELLNAME, and to elements (kind 2) and placements (kind 3).

**Conformance checks.**
- `db_oasis_selftest OUTDIR` runs 164 checks:
  - Codec against Tables 1, 2, 4, 5 and 8. Figure 6 shows six different polygons, one per
    point-list type; each is decoded and re-encoded no longer than printed.
  - CRC32 and CHECKSUM32 reference values (14.4.3, 14.5.2), and rejection of malformed
    values.
  - Two hand-assembled files using every record type 0-34 (implicit and explicit
    references), every repetition type, all 26 CTRAPEZOID types, all property value types,
    and properties on file, cell, element and placement.
  - A random hierarchical layout written in five writer configurations plus a GDSII twin.
- `klayout -b -r regressions/oasis_conformance.py -rd dir=OUTDIR` compares all of these
  with KLayout 0.28.16 (74 checks):
  - KLayout reads every file, with `oasis_expect_strict_mode` on strict files.
  - Our reader's flattened geometry matches KLayout's exactly on Manhattan layers.
  - Scanner boxes, `S_BOUNDING_BOX`, LAYERNAMEs and texts match.
  - Properties match, and the per-layer XOR against the GDSII twin is empty.
- `regressions/xor_files.py` (`-rd a= -rd b= [-rd strict=1] [-rd texts=1]`) runs a deep-mode
  per-layer XOR of every top cell, plus a text comparison. Results:

  | input | output | XOR | texts |
  |---|---|---|---|
  | sky130_fd_sc_hd.gds 4.2 MB, 442 cells | 387 KB OASIS, and back to GDSII | empty | 6170 equal |
  | SDT_16_conv.gds 84 MB | 4.1 MB strict OASIS (1.1 s, 0.8 GB) | empty | 1.35M equal |
  | mkCPU.gds 215 MB | 13.7 MB strict OASIS (3.3 s, 2.5 GB) | empty | 1.06M equal |
  | MICROWATT.oas 24.7 MB | 20.4 MB re-encoded strict (6.5 s, 1.3 GB) | empty, 39 layers | equal as sets (*) |
  | MW4x4.oas 24.8 MB (16 x MICROWATT) | 20.5 MB re-encoded strict | empty, 39 layers | not compared (about 100M flat labels) |

  (*) MICROWATT.oas stores some duplicated library labels as a type-10 repetition with a
  (0,0) displacement. Our reader expands it to the two labels it encodes, which matches the
  original GDSII library. KLayout 0.28 keeps one of them.

Spec points this work settled:
- The CTRAPEZOID modal width/height is always the bounding box (29.7). Types 16-19, 22, 23
  and 25 have no height, and types 20-21 no width.
- A RECTANGLE with both S and H set is an error.
- CBLOCKs may not nest, and only comp-type 0 exists.
- Real denominators must be non-zero, and integers wider than 64 bits are rejected.
- The joint shape of non-Manhattan paths is left to the application (27.10). Bounding boxes
  use mitred joints (`ldbgeom::path_bbox`).
- KLayout rejects a GDSII STRANS whose data type is not bit-array (1).

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

**MAGVIEWER** (plan: `src/MAGVIEWER_TODO.md`; build `make -f Makefile.nvf MAGVIEWER.exe`)
builds its quantized R-tree index directly from the compact blocks
(`CompressedSpatialIndex%build_from_compact`). SDT16 (422M unique boxes) loads into a
1.56 GB index with a 4.5 GB peak. `MAGVIEWER_COMPACT=0` selects the legacy BoxCodec/zstd
chunks, whose multi-threaded encoder has a known race. To test load and pan/zoom queries
without a display: `test_viewer_index.exe file.oas [max_layers] [windows] [frac]`
(`TVI_VERIFY=1` adds a brute-force oracle).

## Sidecar index (.mvi), parser header, and language front ends

**Parser header.** `oasis_parser.hpp` holds the OASIS record parser (`namespace oasis_parse`), shared by:
- `oasis.cpp` (whole-file model and `ldb_stream_boxes`);
- `oasis_sidecar.cpp`.

For speculative decoding, the parser has three extra modes:
- **Tracking** (`Parser::track`): the first use of an undefined modal variable throws `Parser::Dependent`.
- **Skip** (`Parser::skip`): drops boxes that have already been emitted.
- **Top-level hook:** a callback at each record outside CBLOCKs.

**Sidecar.** `oasis_sidecar.cpp` (`ldb_mvi_*` in `layoutdb.h`) builds and reads `<file>.oas.mvi` for flat OASIS. It records:
- units (CBLOCKs and inline runs);
- per (unit, layer) counts and bboxes;
- modal snapshots;
- a per-layer density pyramid.

`ldb_mvi_decode` decodes any set of units in parallel (`LDB_THREADS`). Hierarchical files return 2. Plan and numbers: `src/MAGVIEWER_TODO.md` §0.

```
db_mvi build file.oas [out.mvi] [--grid N]   # MW_8x8_10L (4.1 B boxes): 11 s, 94 MB
db_mvi info  file.oas                        # units, dependent units, per-layer counts/coverage
db_mvi check file.oas                        # every layer: sidecar decode == streaming parser
db_mvi bench file.oas [frac] [n]             # random windows: units touched, decode time
```

**Fortran** (`src/layout_files.f90`, `LayoutFilesModule`, part of `libmagparser.a`): OO wrappers that own the C handles.
- `LayoutFile`: GDSII/OASIS model, extent, top cells, properties, boxes, write.
- `OasisTables`: strict tables, layer names, cell offsets and bboxes.
- `LayoutSidecar`: density levels as zero-copy array views, units in a window, parallel decode into `Box` arrays.

`src/viewer_tiles.f90` (`TiledLayout`) builds the viewer's tile LRU and density images on top of these wrappers. Test: `test_layout_files.exe file.oas`.

**Julia** (`julia/LayoutDB`, over `liblayoutdb.so`; `make liblayoutdb.so`):
- `LayoutFile`, `boxes`, `polygons`, `properties`, `cell_extents`, `write_oasis`/`write_gds`;
- `stream_boxes`;
- `OasisTables`;
- `Sidecar` with `density` (zero-copy), `units_in` and `decode`;
- `dbu_per_micron`.

Run the tests with `julia --project=julia/LayoutDB -e 'using Pkg; Pkg.test()'`. Set `LAYOUTDB_TEST_OAS=flat_strict.oas` to add a real design. JuliaXLS uses this package through `bsc_julia/src/PhysicalImplementation/layout_io.jl`. `ENV["LAYOUTDB_LIB"]` overrides the library path.

## db_defcheck: layout vs DEF (+ LEF) cross-check

`db_defcheck [--lef F]... [--stack SPEC] [--top CELL] design.def[.gz] layout.{oas,gds}`

This tool checks that a layout really is the DEF it was generated from. It reads the DEF
through zlib (`gzopen`/`gzread`), so a `.def.gz` is read as it stands and plain files work
the same way. It keeps only the COMPONENTS list in memory; it turns every routing shape into
probes and rectangles as it reads.

- **DIE**: DIEAREA must equal the top cell's boundary (`--boundary`, default 235/4).
- **COMPONENTS**: each placed component must match a top-cell instance with the same cell,
  lower-left corner of the LEF SIZE box and orientation (N/S/E/W/FN/FS/FE/FW map to GDS
  mirror + rotation). The comparison runs in both directions. The check is skipped when the
  cell sizes are unknown.
- **ROUTING**: every wire (end points, mid point, inset corners), via (rectangles from DEF
  VIAS, which can be generated from a VIARULE, or from a LEF VIA; otherwise the stack rule),
  RECT, `+ RECT`/`+ POLYGON` and PIN shape must land on top-cell geometry of its layer.
- **XOR**: the DEF geometry, rebuilt from the tech LEF wire widths, is XORed per layer with
  the top cell using a union-area sweep over 50 µm tiles. Differing tiles are refined to
  windows of about 1 µm for the report.
- **LEF**: every PIN/OBS RECT of each LEF macro the layout uses must be covered by that
  cell's own geometry on the drawing datatype.

The layer stack defaults to SKY130 (`li1:67/20:67/16 mcon:67/44 met1:68/20:68/16 ... met5:72/20:72/16`).
On mkALU (301k cells, 2.4 M DEF lines) it takes about 19 s and 0.9 GB with an OASIS input.
Regression: `regressions/defcheck_mkALU.sh`.

## db_def2oas: DEF -> OASIS keeping names and DEF properties

`db_def2oas [--lef F]... [--cells lib.{oas,gds}]... [--stack SPEC] [--boundary L/D] [--no-net-props] [--no-conns] [--no-cblock] design.def[.gz] out.oas`

The tool shares its DEF/LEF reader (`def_reader.hpp`) with db_defcheck. It writes a
hierarchical OASIS. Standard cells are copied from `--cells`; macros missing there get a LEF
abstract (boundary plus PIN/OBS rectangles). The top cell holds one PLACEMENT per component,
plus the routing, vias, RECTs and pins as rectangles, plus DIEAREA on the boundary layer.
The OASIS keeps the following names and properties, and KLayout reads all of them:

- **Placements**:
  - `S_GDS_PROPERTY` (61, component name): the attribute KLayout and Magic use for
    instance names. MAGVIEWER's hover line shows it.
  - `DEF_STATUS` FIXED/COVER.
  - `DEF_ATTR` (SOURCE, WEIGHT, HALO, ... clauses).
  - `DEF_PROPERTY` (name, value pairs).
- **Shapes**: `DEF_NET` on every routing, via, RECT and pin shape; `DEF_PIN` on pin shapes. Pin
  names are also written as TEXT on the pin datatype. Shapes are grouped by layer, then net,
  so a repeated net name costs one byte (PROPERTY-REPEAT).
- **Top cell**:
  - `DEF_NET_INFO`: one tab-separated value per net (name, SPECIAL/REGULAR, `+` clauses,
    connections), including nets that have no routing.
  - `DEF_PIN_INFO`.
  - `DEF_ROW`, `DEF_TRACKS`, `DEF_GCELLGRID`, `DEF_PROPERTYDEFINITIONS`, `DEF_VERSION`, ...
  - Sections that are not modelled (REGIONS, GROUPS, BLOCKAGES, FILLS, ...), verbatim.
  - `DEF_UNITS`.

On mkALU (301k components, 91k nets) the conversion takes 8 s and 0.9 GB and writes 26 MB.
The result passes db_defcheck, including a 0 XOR on all 15 layers. A KLayout XOR against
the OpenLane `mkALU.oas` is 0 on every layer except 81/14: that is a marker box the P&R
flow adds and is not in the DEF.
Regression: `regressions/def2oas_mkALU.sh [--klayout-xor]`.

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
- `oasis_codec.hpp`: byte codec shared with `oasis.cpp`, `oasis_rec.cpp` and `oasis_writer.cpp`.
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
