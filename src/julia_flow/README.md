# Julia → Verilog → GDSII/OASIS Flow

This directory contains Julia generators and flow scripts for creating hardware datapaths
in Julia, synthesizing to Verilog, and converting to GDSII/OASIS for MAGPARSER verification.

## Super-Resolution + Image Processing Datapath (>= 1M gates)

`sr_imageproc_datapath.jl` generates a parameterized, scalable Verilog datapath implementing:
- Streaming line buffers (3-line window)
- 2x bilinear upsample/interpolation datapath
- Parallel convolution filter banks (blur, sharpen, edge/Sobel-style) as MAC PE arrays
- Reduction trees combining filter outputs for super-resolution + enhancement

### Generate Verilog

```bash
# Test (768 PEs)
julia sr_imageproc_datapath.jl --test -o sr_test.v

# Big (65,536 PEs)
julia sr_imageproc_datapath.jl --big -o sr_imageproc_big.v

# >= 1M gates (589,824 PEs, ~1.03M Sky130 mapped cells)
julia sr_imageproc_datapath.jl --1m -o sr_imageproc_1m.v
```

### Synthesize, PnR, GDSII/OASIS (IIC-OSIC-TOOLS / Sky130)

```bash
# Yosys synthesis (Sky130 HD)
docker run --rm -v $PWD:/work hpretl/iic-osic-tools:latest bash -lc '
  export PDK_ROOT=/foss/pdks
  LIB=$PDK_ROOT/sky130A/libs.ref/sky130_fd_sc_hd/lib/sky130_fd_sc_hd__tt_025C_1v80.lib
  yosys -p "read_verilog -sv /work/sr_imageproc_1m.v; hierarchy -top SRImageProc1M; synth -top SRImageProc1M; dfflibmap -liberty $LIB; abc -liberty $LIB; stat; write_verilog -noattr /work/SRImageProc1M_synth.v"
'

# OpenROAD PnR -> DEF, then KLayout DEF->GDS->OASIS (see bsc_julia/scripts/run_openroad_sky130.sh pattern)
```

### MAGPARSER verification

```bash
# Extent/layer verification
src/db/db_extent SRImageProc1M.gds
src/db/db_extent_by_cell_layer SRImageProc1M.gds

# MAGPARSER CTR load
bin/MAGPARSER.exe <control.ctr> <max_layers>
```

## Hierarchical geometrical operations

MAGPARSER and `src/db` now support hierarchical GDSII/OASIS loading with transform-aware
flattening (SREF/AREF instances, translation/magnification/mirror/rotation/arrays),
hierarchical extent computation, and hierarchy-aware iteration.

```bash
src/db/db_hier_extent design.gds [top_cell]
# In CTR: decl hierdesign d = design.gds  /  exec hierextent d
```
