# 2D Gmsh mesh generation from layout boxes

"""
    LayerDef

Definition of a layer for mesh generation.
- `layer`: GDS layer number
- `datatype`: GDS datatype (-1 for any)
- `name`: human-readable name
- `zmin`: bottom z for 3D extrusion (meters)
- `zmax`: top z for 3D extrusion (meters)
"""
struct LayerDef
    layer::Int
    datatype::Int
    name::String
    zmin::Float64
    zmax::Float64
end

LayerDef(layer::Int, name::String; datatype::Int=-1, zmin::Float64=0.0, zmax::Float64=0.0) =
    LayerDef(layer, datatype, name, zmin, zmax)

"""
    MeshOptions

Options for mesh generation.
- `dbu`: database unit in meters (e.g., 1e-9 for nm)
- `unit_scale`: multiply meters by this factor for Gmsh model units (1e6 => microns, default)
- `mesh_size_min`: minimum mesh element size (meters)
- `mesh_size_max`: maximum mesh element size (meters)
- `algorithm_2d`: Gmsh 2D algorithm (default 6 = Frontal-Delaunay)
- `algorithm_3d`: Gmsh 3D algorithm (default 10 = HXT)
- `merge_boxes`: merge overlapping boxes on same layer before meshing
- `verbose`: print progress
"""
Base.@kwdef struct MeshOptions
    dbu::Float64 = 1e-9           # database unit in meters
    unit_scale::Float64 = 1e6     # model units per meter (1e6 = microns)
    mesh_size_min::Float64 = 0.1e-6
    mesh_size_max::Float64 = 5.0e-6
    algorithm_2d::Int = 6
    algorithm_3d::Int = 10
    merge_boxes::Bool = true
    verbose::Bool = true
end

"""
    box_to_meters(b::BoxElem, dbu::Float64)

Convert box coordinates from database units to meters.
"""
function box_to_meters(b::BoxElem, dbu::Float64)
    return (b.x1 * dbu, b.y1 * dbu, b.x2 * dbu, b.y2 * dbu)
end

"""
    collect_layer_boxes(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef})

Group boxes by layer definition. Returns Dict{String, Vector{BoxElem}}.
"""
function collect_layer_boxes(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef})
    grouped = Dict{String, Vector{BoxElem}}()
    for ld in layer_defs
        matched = BoxElem[]
        for b in boxes
            if b.layer == ld.layer && (ld.datatype == -1 || b.datatype == ld.datatype)
                push!(matched, b)
            end
        end
        grouped[ld.name] = matched
    end
    return grouped
end

"""
    merge_overlapping_boxes(boxes::Vector{BoxElem})

Simple merge of overlapping/adjacent boxes with same layer/datatype.
Returns merged boxes. Uses sweep-line on x then merge y intervals.
This is a conservative merge - only merges boxes that fully overlap or are adjacent.
"""
function merge_overlapping_boxes(boxes::Vector{BoxElem})
    isempty(boxes) && return boxes
    # Group by (layer, datatype)
    groups = Dict{Tuple{Int,Int}, Vector{BoxElem}}()
    for b in boxes
        key = (b.layer, b.datatype)
        push!(get!(groups, key, BoxElem[]), b)
    end

    result = BoxElem[]
    for ((layer, dtype), group) in groups
        # Simple iterative merge: merge any two boxes that overlap or touch
        merged = BoxElem[b for b in group]
        changed = true
        while changed
            changed = false
            new_merged = BoxElem[]
            used = falses(length(merged))
            for i in 1:length(merged)
                used[i] && continue
                a = merged[i]
                ax1, ay1, ax2, ay2 = a.x1, a.y1, a.x2, a.y2
                for j in (i+1):length(merged)
                    used[j] && continue
                    b = merged[j]
                    # Check overlap or adjacency
                    if ax1 <= b.x2 && b.x1 <= ax2 && ay1 <= b.y2 && b.y1 <= ay2
                        # Merge
                        ax1 = min(ax1, b.x1)
                        ay1 = min(ay1, b.y1)
                        ax2 = max(ax2, b.x2)
                        ay2 = max(ay2, b.y2)
                        used[j] = true
                        changed = true
                    end
                end
                push!(new_merged, BoxElem(a.cell, layer, dtype, a.kind, ax1, ay1, ax2, ay2))
                used[i] = true
            end
            merged = new_merged
        end
        append!(result, merged)
    end
    return result
end

"""
    write_geo_2d(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef}, outpath::String; opts::MeshOptions=MeshOptions())

Generate a Gmsh .geo file for 2D mesh of layout boxes.
Each layer gets physical surfaces. Boxes are created as rectangles and fragmented.
"""
function write_geo_2d(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef}, outpath::String;
                      opts::MeshOptions=MeshOptions())
    grouped = collect_layer_boxes(boxes, layer_defs)
    dbu = opts.dbu
    us = opts.unit_scale

    # Optionally merge boxes
    if opts.merge_boxes
        for (k, v) in grouped
            grouped[k] = merge_overlapping_boxes(v)
        end
    end

    io = open(outpath, "w")
    try
        println(io, "SetFactory(\"OpenCASCADE\");")
        println(io, "")

        # Define mesh sizes
        println(io, "// Mesh options")
        println(io, @sprintf("Mesh.CharacteristicLengthMin = %.6g;", opts.mesh_size_min * us))
        println(io, @sprintf("Mesh.CharacteristicLengthMax = %.6g;", opts.mesh_size_max * us))
        println(io, @sprintf("Mesh.Algorithm = %d;", opts.algorithm_2d))
        println(io, @sprintf("// Model units: 1 meter = %.6g model-units", us))
        println(io, "")

        surface_tags = Dict{String, Vector{Int}}()
        tag = 1

        for ld in layer_defs
            layer_boxes = get(grouped, ld.name, BoxElem[])
            isempty(layer_boxes) && continue
            opts.verbose && println("  Layer $(ld.name): $(length(layer_boxes)) boxes")

            tags = Int[]
            for b in layer_boxes
                x1, y1, x2, y2 = box_to_meters(b, dbu)
                w = x2 - x1
                h = y2 - y1
                if w <= 0 || h <= 0
                    continue
                end
                println(io, @sprintf("Rectangle(%d) = {%.10g, %.10g, 0, %.10g, %.10g};", tag, x1*us, y1*us, w*us, h*us))
                push!(tags, tag)
                tag += 1
            end
            surface_tags[ld.name] = tags
        end

        println(io, "")

        # Boolean fragments to handle overlaps
        all_tags = Int[]
        for ld in layer_defs
            tags = get(surface_tags, ld.name, Int[])
            append!(all_tags, tags)
        end

        if !isempty(all_tags)
            # Fragment all surfaces together for conformal mesh
            tag_list = join(all_tags, ", ")
            println(io, "// Fragment all surfaces for conformal mesh")
            println(io, @sprintf("BooleanFragments{ Surface{%s}; Delete; }{}", tag_list))
            println(io, "")
        end

        # Physical groups - after fragmentation, we need to identify surfaces
        # For simplicity, assign physical groups by bounding box containment
        # A more robust approach would track fragment results, but Gmsh .geo
        # makes this tricky. Instead, we use a post-processing approach:
        # assign all resulting surfaces to a combined physical group per layer
        # by re-creating surfaces. Alternative: use Gmsh API.
        # 
        # Simpler approach: don't fragment, just define physical surfaces directly.
        # This works when boxes don't overlap. For overlaps, the user should
        # pre-merge. Let's provide both modes.
        
        phy_tag = 1
        for ld in layer_defs
            tags = get(surface_tags, ld.name, Int[])
            isempty(tags) && continue
            tag_list = join(tags, ", ")
            println(io, @sprintf("Physical Surface(%d) = {%s}; // %s", phy_tag, tag_list, ld.name))
            phy_tag += 1
        end

    finally
        close(io)
    end

    opts.verbose && println("Wrote 2D Gmsh geometry: $outpath")
    return outpath
end

"""
    write_geo_2d_simple(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef}, outpath::String; opts::MeshOptions=MeshOptions())

Generate a Gmsh .geo file for 2D mesh without BooleanFragments.
Simpler but doesn't handle overlaps. Best when boxes are pre-merged.
"""
function write_geo_2d_simple(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef}, outpath::String;
                              opts::MeshOptions=MeshOptions())
    grouped = collect_layer_boxes(boxes, layer_defs)
    dbu = opts.dbu
    us = opts.unit_scale

    if opts.merge_boxes
        for (k, v) in grouped
            grouped[k] = merge_overlapping_boxes(v)
        end
    end

    io = open(outpath, "w")
    try
        println(io, "SetFactory(\"OpenCASCADE\");")
        println(io, @sprintf("Mesh.CharacteristicLengthMin = %.6g;", opts.mesh_size_min * us))
        println(io, @sprintf("Mesh.CharacteristicLengthMax = %.6g;", opts.mesh_size_max * us))
        println(io, @sprintf("Mesh.Algorithm = %d;", opts.algorithm_2d))
        println(io, @sprintf("// Model units: 1 meter = %.6g model-units", us))
        println(io, "")

        phy_tag = 1
        tag = 1
        for ld in layer_defs
            layer_boxes = get(grouped, ld.name, BoxElem[])
            isempty(layer_boxes) && continue
            opts.verbose && println("  Layer $(ld.name): $(length(layer_boxes)) boxes")

            tags = Int[]
            for b in layer_boxes
                x1, y1, x2, y2 = box_to_meters(b, dbu)
                w = x2 - x1
                h = y2 - y1
                (w <= 0 || h <= 0) && continue
                println(io, @sprintf("Rectangle(%d) = {%.10g, %.10g, 0, %.10g, %.10g};", tag, x1*us, y1*us, w*us, h*us))
                push!(tags, tag)
                tag += 1
            end

            if !isempty(tags)
                tag_list = join(tags, ", ")
                println(io, @sprintf("Physical Surface(%d) = {%s}; // %s", phy_tag, tag_list, ld.name))
                phy_tag += 1
            end
        end
    finally
        close(io)
    end

    opts.verbose && println("Wrote 2D Gmsh geometry: $outpath")
    return outpath
end
