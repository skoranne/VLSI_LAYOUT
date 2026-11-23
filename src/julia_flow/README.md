# Julia → Verilog → GDSII/OASIS flow

The super-resolution image-processing datapath example (`sr_imageproc_datapath.jl` and the older
`sr_imageproc_1m_datapath.jl` / `sr_imageproc_1m_hier.jl` generators) has moved to the JuliaXLS
repository: `bsc_julia/examples/sr_imageproc` (github.com/skoranne/bsc_julia).

Generated collateral is not checked into git. The HLS outputs (`xls_out/`) and the 1M-PE netlists
(`sr_imageproc_1m_262k.v`, `sr_imageproc_1m_hier.v`, `sr_imageproc_1m_hier9.v`) are in
`~/juliaxls_examples/sr_imageproc/`, where OpenROAD is run.

## MAGPARSER verification of the resulting layout

```bash
# Extent/layer verification
src/db/db_extent SRImageProc1M.gds
src/db/db_extent_by_cell_layer SRImageProc1M.gds

# MAGPARSER CTR load
bin/MAGPARSER.exe <control.ctr> <max_layers>
```

## Hierarchical geometrical operations

MAGPARSER and `src/db` support hierarchical GDSII/OASIS loading with transform-aware flattening
(SREF/AREF instances, translation/magnification/mirror/rotation/arrays), hierarchical extent
computation, and hierarchy-aware iteration.

```bash
src/db/db_hier_extent design.gds [top_cell]
# In CTR: decl hierdesign d = design.gds  /  exec hierextent d
```
