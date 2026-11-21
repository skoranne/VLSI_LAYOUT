# 3D Gmsh mesh generation by extrusion from layout boxes

"""
    write_geo_3d_extrude(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef}, outpath::String;
                         opts::MeshOptions=MeshOptions(), air_margin::Float64=0.0)

Generate a Gmsh .geo file for 3D mesh by extruding 2D layout boxes.
Each box on a layer is extruded from zmin to zmax (defined in LayerDef).

If `air_margin > 0`, an air box surrounding the entire structure is added
(margin in meters on each side).

Physical volumes are created per layer name.
"""
function write_geo_3d_extrude(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef}, outpath::String;
                              opts::MeshOptions=MeshOptions(), air_margin::Float64=0.0,
                              air_box::Union{NTuple{6,Float64},Nothing}=nothing)
    grouped = collect_layer_boxes(boxes, layer_defs)
    dbu = opts.dbu

    if opts.merge_boxes
        for (k, v) in grouped
            grouped[k] = merge_overlapping_boxes(v)
        end
    end

    # Compute global bounding box in meters
    gxmin, gymin, gzmin = Inf, Inf, Inf
    gxmax, gymax, gzmax = -Inf, -Inf, -Inf
    for ld in layer_defs
        for b in get(grouped, ld.name, BoxElem[])
            x1, y1, x2, y2 = box_to_meters(b, dbu)
            gxmin = min(gxmin, x1); gymin = min(gymin, y1)
            gxmax = max(gxmax, x2); gymax = max(gymax, y2)
        end
        gzmin = min(gzmin, ld.zmin)
        gzmax = max(gzmax, ld.zmax)
    end

    if !isfinite(gxmin)
        error("No boxes found for any layer definition")
    end

    io = open(outpath, "w")
    try
        println(io, "SetFactory(\"OpenCASCADE\");")
        println(io, "")
        println(io, "// Mesh options")
        println(io, @sprintf("Mesh.CharacteristicLengthMin = %.6g;", opts.mesh_size_min))
        println(io, @sprintf("Mesh.CharacteristicLengthMax = %.6g;", opts.mesh_size_max))
        println(io, @sprintf("Mesh.Algorithm = %d;", opts.algorithm_2d))
        println(io, @sprintf("Mesh.Algorithm3D = %d;", opts.algorithm_3d))
        println(io, "")

        vol_tag = 1
        layer_vol_tags = Dict{String, Vector{Int}}()

        # Air box (optional)
        air_tag = -1
        if air_box !== nothing
            ax, ay, az, adx, ady, adz = air_box
            println(io, @sprintf("// Air box"))
            println(io, @sprintf("Box(%d) = {%.10g, %.10g, %.10g, %.10g, %.10g, %.10g};", vol_tag, ax, ay, az, adx, ady, adz))
            air_tag = vol_tag
            vol_tag += 1
            println(io, "")
        elseif air_margin > 0
            ax = gxmin - air_margin
            ay = gymin - air_margin
            az = gzmin - air_margin
            adx = (gxmax - gxmin) + 2 * air_margin
            ady = (gymax - gymin) + 2 * air_margin
            adz = (gzmax - gzmin) + 2 * air_margin
            println(io, @sprintf("// Air box (margin=%.6g)", air_margin))
            println(io, @sprintf("Box(%d) = {%.10g, %.10g, %.10g, %.10g, %.10g, %.10g};", vol_tag, ax, ay, az, adx, ady, adz))
            air_tag = vol_tag
            vol_tag += 1
            println(io, "")
        end

        # Create 3D boxes for each layer
        for ld in layer_defs
            layer_boxes = get(grouped, ld.name, BoxElem[])
            isempty(layer_boxes) && continue
            opts.verbose && println("  Layer $(ld.name): $(length(layer_boxes)) boxes, z=[ $(ld.zmin), $(ld.zmax) ]")

            tags = Int[]
            z0 = ld.zmin
            dz = ld.zmax - ld.zmin
            if dz <= 0
                opts.verbose && println("    Skipping layer $(ld.name): zero or negative thickness")
                continue
            end

            for b in layer_boxes
                x1, y1, x2, y2 = box_to_meters(b, dbu)
                w = x2 - x1
                h = y2 - y1
                (w <= 0 || h <= 0) && continue
                println(io, @sprintf("Box(%d) = {%.10g, %.10g, %.10g, %.10g, %.10g, %.10g};",
                                     vol_tag, x1, y1, z0, w, h, dz))
                push!(tags, vol_tag)
                vol_tag += 1
            end
            layer_vol_tags[ld.name] = tags
        end

        println(io, "")

        # BooleanFragments for conformal mesh if multiple volumes
        all_vol_tags = Int[]
        if air_tag > 0
            push!(all_vol_tags, air_tag)
        end
        for ld in layer_defs
            append!(all_vol_tags, get(layer_vol_tags, ld.name, Int[]))
        end

        if length(all_vol_tags) > 1
            tag_list = join(all_vol_tags, ", ")
            println(io, "// Fragment all volumes for conformal mesh")
            println(io, @sprintf("v() = BooleanFragments{ Volume{%s}; Delete; }{};", tag_list))
            println(io, "")

            # After fragmentation, original tags may be gone. We need to identify
            # which volumes belong to which physical group. This is tricky in .geo.
            # Strategy: use bounding box matching on the resulting volumes.
            # We'll generate Gmsh script that does this.
            println(io, "// Identify volumes by bounding box after fragmentation")
            println(io, "eps = 1e-12;")

            phy_tag = 1
            # Collect layer volume tags; anything not in a layer becomes air
            println(io, "assigned[] = {};")
            # For each layer, find volumes inside the layer's z range
            for ld in layer_defs
                tags = get(layer_vol_tags, ld.name, Int[])
                isempty(tags) && continue
                z0 = ld.zmin
                z1 = ld.zmax
                safe_name = replace(ld.name, "\"" => "")
                lname = replace(safe_name, r"[^A-Za-z0-9_]" => "_")
                println(io, @sprintf("layerVols_%s() = {};", lname))
                println(io, @sprintf("For i In {0 : #v()-1}"))
                println(io, @sprintf("  bb() = BoundingBox Volume{v(i)};"))
                println(io, @sprintf("  zc = (bb(2) + bb(5)) / 2;"))
                println(io, @sprintf("  If (zc >= %.10g - eps && zc <= %.10g + eps)", z0, z1))
                println(io, @sprintf("    layerVols_%s() += {v(i)};", lname))
                println(io, @sprintf("    assigned[] += {v(i)};"))
                println(io, @sprintf("  EndIf"))
                println(io, @sprintf("EndFor"))
                println(io, @sprintf("If (#layerVols_%s() > 0)", lname))
                println(io, @sprintf("  Physical Volume(%d) = {layerVols_%s()}; // %s", phy_tag, lname, safe_name))
                println(io, @sprintf("EndIf"))
                phy_tag += 1
            end

            # Air = all volumes not assigned to any layer
            if air_tag > 0
                println(io, "airVols() = {};")
                println(io, "For i In {0 : #v()-1}")
                println(io, "  found = 0;")
                println(io, "  For j In {0 : #assigned[]-1}")
                println(io, "    If (assigned[j] == v(i))")
                println(io, "      found = 1;")
                println(io, "    EndIf")
                println(io, "  EndFor")
                println(io, "  If (found == 0)")
                println(io, "    airVols() += {v(i)};")
                println(io, "  EndIf")
                println(io, "EndFor")
                println(io, "If (#airVols() > 0)")
                println(io, @sprintf("  Physical Volume(%d) = {airVols()}; // air", phy_tag))
                println(io, "EndIf")
                phy_tag += 1
            end

        else
            # No fragmentation needed - single volume or simple case
            phy_tag = 1
            if air_tag > 0
                println(io, @sprintf("Physical Volume(%d) = {%d}; // air", phy_tag, air_tag))
                phy_tag += 1
            end
            for ld in layer_defs
                tags = get(layer_vol_tags, ld.name, Int[])
                isempty(tags) && continue
                tag_list = join(tags, ", ")
                println(io, @sprintf("Physical Volume(%d) = {%s}; // %s", phy_tag, tag_list, ld.name))
                phy_tag += 1
            end
        end

    finally
        close(io)
    end

    opts.verbose && println("Wrote 3D Gmsh geometry: $outpath")
    return outpath
end
