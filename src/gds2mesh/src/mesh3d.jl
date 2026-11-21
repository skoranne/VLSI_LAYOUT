# 3D Gmsh mesh generation by extrusion from layout boxes

"""
    write_geo_3d_extrude(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef}, outpath::String;
                         opts::MeshOptions=MeshOptions(), air_margin::Float64=0.0)

Generate a Gmsh .geo file for 3D mesh by extruding 2D layout boxes.
Each box on a layer is extruded from zmin to zmax (defined in LayerDef, meters).

Geometry is written in model units scaled by `opts.unit_scale` (default 1e6 => microns)
to avoid OpenCASCADE tolerance issues with meter-scale micron features. Set `opts.unit_scale=1.0`
to write meters.

If `air_margin > 0`, an air box surrounding the entire structure is added (meters).
Physical volumes are created per layer name.
"""
function write_geo_3d_extrude(boxes::Vector{BoxElem}, layer_defs::Vector{LayerDef}, outpath::String;
                              opts::MeshOptions=MeshOptions(), air_margin::Float64=0.0,
                              air_box::Union{NTuple{6,Float64},Nothing}=nothing,
                              absorbing_surfaces::Bool=false,
                              pec_volume_names::Vector{String}=String[])
    grouped = collect_layer_boxes(boxes, layer_defs)
    dbu = opts.dbu
    us = opts.unit_scale

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
        println(io, @sprintf("Mesh.CharacteristicLengthMin = %.6g;", opts.mesh_size_min * us))
        println(io, @sprintf("Mesh.CharacteristicLengthMax = %.6g;", opts.mesh_size_max * us))
        println(io, @sprintf("Mesh.Algorithm = %d;", opts.algorithm_2d))
        println(io, @sprintf("Mesh.Algorithm3D = %d;", opts.algorithm_3d))
        println(io, @sprintf("// Model units: 1 meter = %.6g model-units", us))
        println(io, "")

        vol_tag = 1
        layer_vol_tags = Dict{String, Vector{Int}}()

        # Air box (optional)
        air_tag = -1
        if air_box !== nothing
            ax, ay, az, adx, ady, adz = air_box .* us
            println(io, @sprintf("// Air box"))
            println(io, @sprintf("Box(%d) = {%.10g, %.10g, %.10g, %.10g, %.10g, %.10g};", vol_tag, ax, ay, az, adx, ady, adz))
            air_tag = vol_tag
            vol_tag += 1
            println(io, "")
        elseif air_margin > 0
            ax = (gxmin - air_margin) * us
            ay = (gymin - air_margin) * us
            az = (gzmin - air_margin) * us
            adx = ((gxmax - gxmin) + 2 * air_margin) * us
            ady = ((gymax - gymin) + 2 * air_margin) * us
            adz = ((gzmax - gzmin) + 2 * air_margin) * us
            println(io, @sprintf("// Air box (margin=%.6g m)", air_margin))
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
                                     vol_tag, x1*us, y1*us, z0*us, w*us, h*us, dz*us))
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
            println(io, @sprintf("eps = %.6g;", 1e-9 * us))

            phy_tag = 1
            # Collect layer volume tags; anything not in a layer becomes air
            println(io, "assigned() = {};")
            # For each layer, find volumes inside the layer's z range
            for ld in layer_defs
                tags = get(layer_vol_tags, ld.name, Int[])
                isempty(tags) && continue
                z0 = ld.zmin * us
                z1 = ld.zmax * us
                safe_name = replace(ld.name, "\"" => "")
                lname = replace(safe_name, r"[^A-Za-z0-9_]" => "_")
                println(io, @sprintf("layerVols_%s() = {};", lname))
                println(io, @sprintf("For i In {0 : #v()-1}"))
                println(io, @sprintf("  bb() = BoundingBox Volume{v(i)};"))
                println(io, @sprintf("  zc = (bb(2) + bb(5)) / 2;"))
                println(io, @sprintf("  If (zc >= %.10g - eps && zc <= %.10g + eps)", z0, z1))
                println(io, @sprintf("    layerVols_%s() += {v(i)};", lname))
                println(io, @sprintf("    assigned() += {v(i)};"))
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
                println(io, "  For j In {0 : #assigned()-1}")
                println(io, "    If (assigned(j) == v(i))")
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

        # Optional Palace physical surface tagging
        if absorbing_surfaces || !isempty(pec_volume_names)
            println(io, "")
            println(io, "// Physical surface tagging (Palace)")
            # collect all boundary surfaces
            println(io, "allSurfs() = {};" )
            # use all volumes
            println(io, "allVolsList() = {};")
            if length(all_vol_tags) > 1
                println(io, "allVolsList() += v();")
            else
                if air_tag > 0
                    println(io, @sprintf("allVolsList() += {%d};", air_tag))
                end
                for ld in layer_defs
                    tags = get(layer_vol_tags, ld.name, Int[])
                    isempty(tags) && continue
                    println(io, @sprintf("allVolsList() += {%s};", join(tags, ", ")))
                end
            end
            println(io, "For i In {0:#allVolsList()-1}")
            println(io, "  allSurfs() += Boundary{ Volume{allVolsList(i)}; };")
            println(io, "EndFor")
            println(io, "absbSurfs() = {};")
            println(io, "pecSurfs() = {};")
            surf_eps = 1e-6 * us
            println(io, @sprintf("seps = %.10g;", surf_eps))
            # outer boundary -> absorbing
            if absorbing_surfaces
                # compute model bounds in model units
                if !isfinite(gxmin)
                    gxmin2,gymin2,gzmin2,gxmax2,gymax2,gzmax2 = 0.0,0.0,0.0,0.0,0.0,0.0
                else
                    gxmin2,gymin2,gzmin2 = gxmin,gymin,gzmin
                    gxmax2,gymax2,gzmax2 = gxmax,gymax,gzmax
                end
                am = air_margin > 0 ? air_margin : 0.0
                println(io, @sprintf("bxmin=%.10g; bxmax=%.10g; bymin=%.10g; bymax=%.10g; bzmin=%.10g; bzmax=%.10g;", (gxmin2-am)*us, (gxmax2+am)*us, (gymin2-am)*us, (gymax2+am)*us, (gzmin2-am)*us, (gzmax2+am)*us))
                println(io, "For i In {0:#allSurfs()-1}")
                println(io, "  ss = Abs(allSurfs(i));")
                println(io, "  bb() = BoundingBox Surface{ss};")
                println(io, "  onOuter = (bb(0) < bxmin+seps || bb(3) > bxmax-seps || bb(1) < bymin+seps || bb(4) > bymax-seps || bb(2) < bzmin+seps || bb(5) > bzmax-seps);")
                println(io, "  If (onOuter) absbSurfs() += {ss}; EndIf")
                println(io, "EndFor")
                println(io, "If (#absbSurfs() > 0)")
                println(io, @sprintf("  Physical Surface(%d) = {absbSurfs()}; // absorbing", phy_tag))
                println(io, "EndIf")
                phy_tag += 1
            end
            # PEC from named volumes
            if !isempty(pec_volume_names)
                for pname in pec_volume_names
                    safe = replace(replace(pname, "\"" => ""), r"[^A-Za-z0-9_]" => "_")
                    println(io, @sprintf("pecS_%s() = {};", safe))
                    if length(all_vol_tags) > 1
                        # after fragment, find volumes by zc matching layer
                        ld_match = findfirst(ld -> ld.name == pname, layer_defs)
                        if ld_match !== nothing
                            ld = layer_defs[ld_match]
                            println(io, "For i In {0:#v()-1}")
                            println(io, "  bb() = BoundingBox Volume{v(i)};")
                            println(io, "  zc = (bb(2)+bb(5))/2;")
                            println(io, @sprintf("  If (zc >= %.10g-seps && zc <= %.10g+seps)", ld.zmin*us, ld.zmax*us))
                            println(io, "    pecS_$(safe)() += Boundary{ Volume{v(i)}; };")
                            println(io, "  EndIf")
                            println(io, "EndFor")
                        end
                    else
                        tags = get(layer_vol_tags, pname, Int[])
                        if !isempty(tags)
                            println(io, @sprintf("For k In {%s}", join(tags, ",")))
                            println(io, "  pecS_$(safe)() += Boundary{ Volume{k}; };")
                            println(io, "EndFor")
                        end
                    end
                    println(io, @sprintf("If (#pecS_%s() > 0)", safe))
                    println(io, @sprintf("  Physical Surface(%d) = {pecS_%s()}; // pec:%s", phy_tag, safe, pname))
                    println(io, "EndIf")
                    phy_tag += 1
                end
            end
        end

    finally
        close(io)
    end

    opts.verbose && println("Wrote 3D Gmsh geometry: $outpath")
    return outpath
end
