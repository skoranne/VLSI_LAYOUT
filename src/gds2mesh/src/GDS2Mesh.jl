"""
    GDS2Mesh

Julia module for converting GDSII/OASIS layout files to Gmsh meshes.

Uses the C++ layoutdb library (VLSI_LAYOUT/src/db) via ccall for parsing,
and generates Gmsh .geo files for 2D and 3D extruded meshes.

# Quick start

```julia
using GDS2Mesh

# Open layout
lf = open_layout("design.gds")

# Inspect layers
boxes = get_boxes(lf)
layers = layer_datatypes(boxes)
println(layers)

# Define layer stack (layer, name; zmin, zmax in meters)
stack = [
    LayerDef(10, "metal1"; zmin=0.0,    zmax=0.2e-6),
    LayerDef(20, "metal2"; zmin=0.5e-6,  zmax=0.7e-6),
]

# Generate 2D mesh
mesh2d(lf, stack, "design_2d.geo"; opts=MeshOptions(dbu=1e-9))

# Generate 3D extruded mesh
mesh3d(lf, stack, "design_3d.geo"; opts=MeshOptions(dbu=1e-9), air_margin=2e-6)

# Run gmsh
run_gmsh("design_3d.geo"; dim=3, out="design_3d.msh")
```
"""
module GDS2Mesh

using Printf

export LayoutDB, LayoutFile, BoxElem, CellLayerExtent, Property, LdbExtent
export open_layout, close_layout, get_boxes, get_boxes_flat, get_extent, get_layers
export LayerDef, MeshOptions
export write_geo_2d, write_geo_2d_simple, write_geo_3d_extrude
export mesh2d, mesh3d, run_gmsh
export layer_datatypes, filter_boxes, merge_overlapping_boxes

include("layoutdb.jl")
using .LayoutDB: LayoutFile, BoxElem, CellLayerExtent, Property, LdbExtent,
                 LDB_OPEN_DEFAULT, LDB_OPEN_STRICT, LDB_OPEN_PROPERTY, LDB_OPEN_LOW_MEMORY,
                 libpath, format_name, memory_bytes, extent_whole,
                 iterate_boxes, iterate_boxes_flat, extent_by_cell_layer, iterate_properties,
                 print_hierarchy, layer_datatypes, filter_boxes, close_layout

include("mesh2d.jl")
include("mesh3d.jl")

# --- High-level convenience API ---

"""
    open_layout(path; flags=LDB_OPEN_DEFAULT, large_cell_threshold=0)

Open a GDSII or OASIS layout file. Returns a `LayoutFile`.
"""
function open_layout(path::AbstractString; flags::UInt32=LDB_OPEN_DEFAULT,
                     large_cell_threshold::UInt64=UInt64(0))
    return LayoutFile(path; flags=flags, large_cell_threshold=large_cell_threshold)
end

"""
    get_boxes(lf::LayoutFile)

Iterate all box elements in the layout. Returns `Vector{BoxElem}`.
"""
function get_boxes(lf::LayoutFile)
    return iterate_boxes(lf)
end

"""
    get_boxes_flat(lf::LayoutFile)

Iterate all box elements with hierarchy flattened (SREF/AREF expanded).
Returns `Vector{BoxElem}`.
"""
function get_boxes_flat(lf::LayoutFile)
    return iterate_boxes_flat(lf)
end

"""
    get_extent(lf::LayoutFile)

Get the whole-file extent. Returns `LdbExtent`.
"""
function get_extent(lf::LayoutFile)
    return extent_whole(lf)
end

"""
    get_layers(lf::LayoutFile)

Get all (layer, datatype) pairs present in the layout.
"""
function get_layers(lf::LayoutFile)
    boxes = get_boxes(lf)
    return layer_datatypes(boxes)
end

"""
    mesh2d(lf::LayoutFile, layer_defs::Vector{LayerDef}, outpath::String;
           opts::MeshOptions=MeshOptions(), simple::Bool=true, flatten::Bool=false)

Generate a 2D Gmsh .geo file from a layout file.
If `simple=true` (default), uses direct rectangle creation (no BooleanFragments).
If `flatten=true`, expands SREF/AREF hierarchy before meshing.
"""
function mesh2d(lf::LayoutFile, layer_defs::Vector{LayerDef}, outpath::String;
                opts::MeshOptions=MeshOptions(), simple::Bool=true, flatten::Bool=false)
    boxes = flatten ? get_boxes_flat(lf) : get_boxes(lf)
    if simple
        return write_geo_2d_simple(boxes, layer_defs, outpath; opts=opts)
    else
        return write_geo_2d(boxes, layer_defs, outpath; opts=opts)
    end
end

"""
    mesh3d(lf::LayoutFile, layer_defs::Vector{LayerDef}, outpath::String;
           opts::MeshOptions=MeshOptions(), air_margin::Float64=0.0,
           air_box=nothing, flatten::Bool=false)

Generate a 3D extruded Gmsh .geo file from a layout file.
If `flatten=true`, expands SREF/AREF hierarchy before meshing.
"""
function mesh3d(lf::LayoutFile, layer_defs::Vector{LayerDef}, outpath::String;
                opts::MeshOptions=MeshOptions(), air_margin::Float64=0.0,
                air_box::Union{NTuple{6,Float64},Nothing}=nothing, flatten::Bool=false,
                absorbing_surfaces::Bool=false, pec_volume_names::Vector{String}=String[])
    boxes = flatten ? get_boxes_flat(lf) : get_boxes(lf)
    return write_geo_3d_extrude(boxes, layer_defs, outpath; opts=opts,
                                air_margin=air_margin, air_box=air_box,
                                absorbing_surfaces=absorbing_surfaces, pec_volume_names=pec_volume_names)
end

"""
    run_gmsh(geopath::String; dim::Int=3, out::String="", format::String="msh2",
             gmsh_bin::String="gmsh", extra_args::Vector{String}=String[])

Run Gmsh on a .geo file to generate a mesh.
Returns the output mesh path.
"""
function run_gmsh(geopath::String; dim::Int=3, out::String="", format::String="msh2",
                  gmsh_bin::String="gmsh", extra_args::Vector{String}=String[])
    if isempty(out)
        base = splitext(geopath)[1]
        out = base * ".msh"
    end
    args = String[gmsh_bin, "-$(dim)", geopath, "-o", out, "-format", format]
    append!(args, extra_args)
    opts = MeshOptions() # dummy, just for verbose check
    println("Running: ", join(args, " "))
    cmd = Cmd(args)
    result = run(pipeline(ignorestatus(cmd), stdout=stdout, stderr=stderr))
    if !isfile(out) || filesize(out) == 0
        error("Gmsh failed (exit=$(result.exitcode)) and did not produce output: $out")
    elseif !success(result)
        error("Gmsh exited with code $(result.exitcode) while generating $out; refusing to accept possibly invalid mesh")
    end
    println("Mesh written: $out ($(filesize(out)) bytes)")
    return out
end

end # module
