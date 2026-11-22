# src/db: GDSII/OASIS reader-writer (WIP)

Goal and constraints: see `dbplan.txt`.

Planned components:
- `layoutdb.h/.cpp`: core C API + C++ implementation (mmap reader, record parsers, lazy repetitions)
- GDSII stream parser/writer, OASIS parser/writer (CBLOCK via nvComp when available)
- FFI-friendly C API for Fortran `iso_c_binding` and Julia `ccall`
- Test CLIs: `db_extent`, `db_extent_by_cell_layer`, `db_hierarchy`, `db_propnames`

Build notes: integrate with `src/` makefiles/toolchains as needed; avoid hardcoding local paths.

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
