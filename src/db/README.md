# src/db: GDSII/OASIS reader-writer (WIP)

Goal and constraints: see `dbplan.txt`.

Planned components:
- `layoutdb.h/.cpp`: core C API + C++ implementation (mmap reader, record parsers, lazy repetitions)
- GDSII stream parser/writer, OASIS parser/writer (CBLOCK via nvComp when available)
- FFI-friendly C API for Fortran `iso_c_binding` and Julia `ccall`
- Test CLIs: `db_extent`, `db_extent_by_cell_layer`, `db_hierarchy`, `db_propnames`

Build notes: integrate with `src/` makefiles/toolchains as needed; avoid hardcoding local paths.
