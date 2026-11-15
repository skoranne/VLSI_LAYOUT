# AGENTS.md

Guidance for agentic work in this VLSI layout research repository (Fortran 90 + C/C++/CUDA, OpenMP/GPU offload focus).

## Build and executables
- Primary build location is `src/`; most `make` targets must be run from `src/`.
- Two toolchain-specific makefiles (no root `Makefile`):
  - `src/Makefile.nvf`: NVIDIA HPC SDK (`nvfortran`, CUDA/OpenMP target offload flags like `-mp=gpu -cuda`; builds `event_sorter.o`/`gpu_morton.o` via `nvcc -arch=sm_121`). Selected on non-x86_64 in `src/start_development.sh`.
  - `src/Makefile.ifx`: Intel/HDF5 toolchain (`HDF5_FC`/`h5fc`/`mpiifx`, optional `DEBUG=1`, `PROFILE=1`; CPU stubs for `event_sorter.cu`/`gpu_morton.cu` compiled with g++). Selected on x86_64 in `src/start_development.sh`.
- Common targets: `all` -> `MAGPARSER.exe`; `unit_test` builds `test_contour.exe test_morton.exe test_polygonfracture.exe test_boxmerge.exe test_skiplist.exe`; `clean` removes `*.o *.mod *.a *.exe` in `src/`. Other explicit targets include `CONVERT.exe`, `test_interaction.exe`, `test_repetition.exe`, `BOOST_XOR_CHECKER.exe`, `generate_bin.exe`, `MAGVIEWER.exe` (ifx), and `ALL_EXE` aggregates.
- Convenience shell helpers in `src/start_development.sh` set `BUILD_HOME`/`buildMakeFile` and define `bc/bx/bd` (`clean`, `DEBUG=0 all`, `DEBUG=1 all`). Source it if needed, but note it hardcodes paths.
- `src/DEPENDENCIES.txt` is `include`d by both makefiles and defines `LIB_OBJS` and module/object dependencies; update it when adding/changing Fortran module dependencies.
- `version_mod.f90` is generated from git state by the makefiles. Prebuilt binaries also exist in top-level `bin/`.

## Running / workflow
- Main driver: `MAGPARSER.exe` consumes `.ctr` control files (see `README.md`, `src/*.ctr` e.g. `src/check_interactions.ctr`, `src/square.ctr`, `src/MW_4x4.ctr`). `src/runmag.sh` runs `$INSTALL_PATH/bin/MAGPARSER.exe` with `OMP_PROC_BIND=TRUE`, `OMP_PLACES=cores`.
- Some control files are authored as `m4` templates that `include('.../src/op_template.m4')` and use macros like `RUN_LOOP`/`OP1` (`src/square.ctr`, README example). Expand with `m4` as appropriate before/when running if using template form.
- `.mag` files are Magic layout inputs/outputs; `.bin` files are intermediate/binary layer data. Conversion/helpers: `src/gds2bin.{sh,rb}`, `src/bin2gds.{sh,rb}`, `src/binall2gds.{sh,rb}`, `src/kconvert.{sh,rb}`, `src/createmagic.sh` (many invoke `klayout` and embed install paths).

## Architecture / code map (high level)
- Fortran core modules/impl in `src/`: parsing/control (`mainmagparser.f90`, `magparser.f90`, `controlparser.f90`), design/geometry (`design*.f90`, `geometry*.f90`, `datastructures.f90`), spatial/index/ops (`rtree.f90`, `morton.f90`, `pnummerge.f90`, `RLEMerge.f90`, `interaction.f90`, `contour.f90`, `polygon_fracture.f90`, `box*`), I/O/serialization (`hdfdata.f90`, `kldata.f90`, `serialization.f90`, `boxbytestream.f90`).
- GPU/OpenMP and native pieces: `gpu_morton.cu`/`gpu_morton_sort.f90`, `event_sorter.cu`/`event_sorter_apif.f90`, `systeminfo_gpu.f90`, `boost_polygon_api.{cpp,h}` + Fortran API, `snappy_api.cpp`.
- `src/db/`: planned/active GDSII+OASIS reader/writer library (C/C++ core with Fortran/Julia-callable API). Requirements tracked in `src/db/dbplan.txt`: mmap I/O, efficient/compressed in-memory representation (unions/entropy encoding), lazy OASIS repetition expansion, nvComp for OASIS CBLOCK decompression, full record support incl. PROPERTY/MULTIGON, and test programs for whole-file extent, per-cell/layer extent, cell hierarchy, and property name table. Sample `.gds`/`.oas` inputs exist in `src/` and `data/FPU`, `data/SDT`.
- Data/examples: `examples/` (small `.mag`), `data/` (images, SDT/FPU datasets; some large/split archives like `SDT6x6_FLAT.mag.gz.part*`).

## Repo-specific gotchas
- Many scripts/makefiles contain machine-specific absolute paths (e.g. `/scratch1/skoranne/...`, `/home/skoranne/GITHUB/VLSI_LAYOUT/...`, hardcoded Boost/HDF5/lib dirs, `BUILD_HOME`). Verify/adjust for the local environment instead of assuming portability.
- `src/` contains generated/build artifacts and many ad-hoc test programs/logs (`*.o`, `*.mod`, `*.exe`, logs, sample `.mag/.ctr/bin`); avoid blanket cleanup that deletes needed outputs. `make clean` is scoped to build products in `src/`.
- Ifx vs nvf builds differ in preprocessor/CUDA handling and some linked objects; prefer the makefile matching the available compiler/toolchain.
- No CI workflow or repo-level automated test runner was found; validation is typically via building specific targets and running relevant executables/control files against sample designs.
