#!/usr/bin/env julia
# Example: GDS/OASIS -> Gmsh 2D/3D mesh
# Usage:
#   julia --project examples/convert.jl input.gds --dbu 1e-9 --dim 3 --out out.geo

using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))

push!(LOAD_PATH, joinpath(@__DIR__, "..", "src"))
include(joinpath(@__DIR__, "..", "src", "GDS2Mesh.jl"))
using .GDS2Mesh

function parse_args(args)
    opts = Dict{String,String}()
    pos = String[]
    i = 1
    while i <= length(args)
        a = args[i]
        if startswith(a, "--") && i < length(args)
            opts[a[3:end]] = args[i+1]
            i += 2
        else
            push!(pos, a)
            i += 1
        end
    end
    return pos, opts
end

pos, kv = parse_args(ARGS)
isempty(pos) && error("Usage: convert.jl <input.gds|.oas> [--dbu 1e-9] [--dim 2|3] [--out out.geo] [--air-margin 1e-6] [--mesh-max 1e-6]")

input = pos[1]
dbu = parse(Float64, get(kv, "dbu", "1e-9"))
dim = parse(Int, get(kv, "dim", "3"))
outgeo = get(kv, "out", splitext(input)[1] * (dim == 3 ? "_3d.geo" : "_2d.geo"))
air_margin = parse(Float64, get(kv, "air-margin", "1e-6"))
mesh_max = parse(Float64, get(kv, "mesh-max", "1e-6"))
mesh_min = parse(Float64, get(kv, "mesh-min", "0.1e-6"))

lf = open_layout(input)
println("Format: ", GDS2Mesh.LayoutDB.format_name(lf))
println("Extent: ", get_extent(lf))
layers = get_layers(lf)
println("Layers: ", layers)

# Default: all layers with 0.1um thickness, stacked with 0.1um gap
stack = LayerDef[]
let z = 0.0
    for (i, (layer, dtype)) in enumerate(layers)
        push!(stack, LayerDef(layer, "L$(layer)_$(dtype)"; datatype=dtype, zmin=z, zmax=z + 0.1e-6))
        z += 0.2e-6
    end
end

opts = MeshOptions(dbu=dbu, mesh_size_min=mesh_min, mesh_size_max=mesh_max)
if dim == 3
    mesh3d(lf, stack, outgeo; opts=opts, air_margin=air_margin)
else
    mesh2d(lf, stack, outgeo; opts=opts)
end
close(lf)
println("Wrote: $outgeo")
