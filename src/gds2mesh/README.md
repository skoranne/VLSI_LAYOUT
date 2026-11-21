# GDS2Mesh

Julia-based GDSII/OASIS to Gmsh mesh converter.

Parses layout files using the C++ `layoutdb` library in `VLSI_LAYOUT/src/db` (via `ccall`),
then generates Gmsh `.geo` geometry for 2D surface meshing and 3D extrusion meshing.

## Features

- Read GDSII and OASIS via `layoutdb` C API (`liblayoutdb.dylib`/`.so`)
- Extract boxes, extents, layer/datatype inventory
- Generate 2D Gmsh geometry (per-layer physical surfaces)
- Generate 3D extruded Gmsh geometry (per-layer physical volumes, optional air box)
- Run Gmsh CLI to produce `.msh`

## Build dependency

Build the shared library in `../db`:

```bash
cd VLSI_LAYOUT/src/db
g++ -O2 -std=c++17 -Wall -Wextra -I. -DLAYOUTDB_USE_ZLIB -shared -fPIC \
  -o liblayoutdb.dylib \
  layoutdb.cpp layoutdb_hier.cpp gdsii.cpp oasis.cpp oasis_writer.cpp \
  db_stats.cpp nvcomp_support.cpp lefdef_export.cpp -lz
```

## Usage

```julia
push!(LOAD_PATH, "path/to/VLSI_LAYOUT/src/gds2mesh/src")
include("path/to/VLSI_LAYOUT/src/gds2mesh/src/GDS2Mesh.jl")
using .GDS2Mesh

# Open layout
lf = open_layout("design.gds")

# Inspect
println(get_extent(lf))
println(get_layers(lf))

# Define layer stack
# LayerDef(layer, name; datatype=-1, zmin=0.0, zmax=0.1e-6)
stack = [
    LayerDef(10, "metal1"; datatype=0, zmin=0.0e-6,  zmax=0.2e-6),
    LayerDef(20, "metal2"; datatype=0, zmin=0.5e-6,  zmax=0.7e-6),
]

opts = MeshOptions(dbu=1e-9, mesh_size_min=0.05e-6, mesh_size_max=1.0e-6)

# 2D mesh
mesh2d(lf, stack, "design_2d.geo"; opts=opts)
run_gmsh("design_2d.geo"; dim=2, out="design_2d.msh")

# 3D extrusion mesh
mesh3d(lf, stack, "design_3d.geo"; opts=opts, air_margin=2e-6)
run_gmsh("design_3d.geo"; dim=3, out="design_3d.msh")

close(lf)
```

## Layer stack

`LayerDef(layer, name; datatype=-1, zmin=0.0, zmax=0.0)`

- `datatype=-1` matches any datatype on that layer
- `zmin/zmax` are in meters (used for 3D extrusion)
- 3D extrusion assumes non-overlapping z-ranges per physical layer group

## Notes

- Geometry comes from `ldb_iterate_boxes` (element bounding boxes). This is robust for rectilinear/box geometry; general polygon vertex fidelity requires extending the C API.
- `mesh3d` uses `BooleanFragments` and assigns physical volumes by z-center heuristic.
- Set `dbu` in `MeshOptions` to match your layout database units (e.g. `1e-9` for nm).
- `run_gmsh` requires `gmsh` on PATH (or pass `gmsh_bin=...`).

## Tests

```bash
cd VLSI_LAYOUT/src/gds2mesh
julia --project -e 'push!(LOAD_PATH,"src"); include("src/GDS2Mesh.jl"); using .GDS2Mesh; include("test/runtests.jl")'
```
