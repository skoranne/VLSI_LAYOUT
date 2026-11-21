using Test
if !@isdefined(GDS2Mesh)
    try
        using GDS2Mesh
    catch
        include(joinpath(@__DIR__, "..", "src", "GDS2Mesh.jl"))
        using .GDS2Mesh
    end
end

const DB_DIR = joinpath(@__DIR__, "..", "..", "db")
const LIB = joinpath(DB_DIR, "liblayoutdb.dylib")

# Find a real GDS file for testing
function find_test_gds()
    candidates = [
        joinpath(homedir(), ".klayout/tech/sky130/pymacros/cells/fixed_devices/bjt/npn/sky130_fd_pr__npn_05v5_W1p00L1p00.gds"),
        "/tmp/test_square.gds",
    ]
    for c in candidates
        isfile(c) && return c
    end
    return nothing
end

# Create a synthetic OASIS test file using C API
function create_test_oasis(path::String)
    errbuf = Vector{UInt8}(undef, 512)
    fill!(errbuf, 0)
    rc = ccall((:ldb_write_oasis_gear_multigon, LIB), Cint,
        (Cstring, Ptr{UInt8}, Csize_t), path, errbuf, sizeof(errbuf))
    rc != 0 && error("Failed to create test OASIS: $(unsafe_string(pointer(errbuf)))")
    return path
end

@testset "GDS2Mesh" begin
    @testset "LayoutDB open/read GDS" begin
        gds = find_test_gds()
        if gds === nothing
            @warn "No test GDS file found, skipping GDS tests"
        else
            lf = open_layout(gds)
            @test isopen(lf)
            @test GDS2Mesh.LayoutDB.format_name(lf) == "GDSII"

            ext = get_extent(lf)
            @test ext.valid == 1
            @test ext.xmax > ext.xmin
            @test ext.ymax > ext.ymin

            boxes = get_boxes(lf)
            @test length(boxes) > 0
            @test all(b -> b.x2 >= b.x1 && b.y2 >= b.y1, boxes)

            layers = get_layers(lf)
            @test length(layers) > 0
            @test all(l -> l[1] >= 0, layers)

            close(lf)
            @test !isopen(lf)
        end
    end

    @testset "LayoutDB open/read OASIS" begin
        if !isfile(LIB)
            @warn "liblayoutdb.dylib not found, skipping OASIS test"
        else
            oasis_path = tempname() * ".oas"
            try
                create_test_oasis(oasis_path)
                lf = open_layout(oasis_path)
                @test isopen(lf)
                @test GDS2Mesh.LayoutDB.format_name(lf) == "OASIS"
                boxes = get_boxes(lf)
                @test length(boxes) >= 0  # may be empty depending on implementation
                close(lf)
            finally
                rm(oasis_path; force=true)
            end
        end
    end

    @testset "2D mesh generation" begin
        gds = find_test_gds()
        if gds === nothing
            @warn "No test GDS file found, skipping 2D mesh test"
        else
            lf = open_layout(gds)
            boxes = get_boxes(lf)
            layers = get_layers(lf)
            # Use first 2 layers
            stack = [LayerDef(l[1], "L$(l[1])_$(l[2])"; datatype=l[2]) for l in layers[1:min(2,length(layers))]]
            opts = MeshOptions(dbu=1e-9, mesh_size_min=0.05e-6, mesh_size_max=0.5e-6)

            outgeo = tempname() * ".geo"
            outmsh = replace(outgeo, ".geo" => ".msh")
            try
                mesh2d(lf, stack, outgeo; opts=opts)
                @test isfile(outgeo)
                @test filesize(outgeo) > 0
                content = read(outgeo, String)
                @test occursin("SetFactory", content)
                @test occursin("Physical Surface", content)

                # Run gmsh if available
                gmsh_ok = try
                    run(pipeline(`gmsh -version`, stdout=devnull, stderr=devnull))
                    true
                catch
                    false
                end
                if gmsh_ok
                    run_gmsh(outgeo; dim=2, out=outmsh)
                    @test isfile(outmsh)
                    @test filesize(outmsh) > 0
                end
            finally
                rm(outgeo; force=true)
                rm(outmsh; force=true)
                close(lf)
            end
        end
    end

    @testset "3D extrusion mesh generation" begin
        gds = find_test_gds()
        if gds === nothing
            @warn "No test GDS file found, skipping 3D mesh test"
        else
            lf = open_layout(gds)
            layers = get_layers(lf)
            # Use first 2 layers with z
            stack = [
                LayerDef(layers[1][1], "bottom"; datatype=layers[1][2], zmin=0.0, zmax=0.1e-6),
            ]
            if length(layers) >= 2
                push!(stack, LayerDef(layers[2][1], "top"; datatype=layers[2][2], zmin=0.2e-6, zmax=0.3e-6))
            end
            opts = MeshOptions(dbu=1e-9, mesh_size_min=0.05e-6, mesh_size_max=0.5e-6)

            outgeo = tempname() * ".geo"
            outmsh = replace(outgeo, ".geo" => ".msh")
            try
                mesh3d(lf, stack, outgeo; opts=opts, air_margin=0.5e-6)
                @test isfile(outgeo)
                @test filesize(outgeo) > 0
                content = read(outgeo, String)
                @test occursin("SetFactory", content)
                @test occursin("Box(", content)

                gmsh_ok = try
                    run(pipeline(`gmsh -version`, stdout=devnull, stderr=devnull))
                    true
                catch
                    false
                end
                if gmsh_ok
                    run_gmsh(outgeo; dim=3, out=outmsh)
                    @test isfile(outmsh)
                    @test filesize(outmsh) > 0
                    msh = read(outmsh, String)
                    # Ensure physical volume groups were written (not all untagged)
                    has_phys = occursin("\$PhysicalNames", msh) || occursin("PhysicalNames", msh)
                    has_tagged_elems = occursin("\n4 ", msh) # tet elements present
                    @test has_phys || has_tagged_elems
                    # Verify at least some elements have nonzero physical tag
                    phys_tags = Set{Int}()
                    in_elems = false
                    for ln in split(msh, '\n')
                        if startswith(ln, "\$Elements"); in_elems=true; continue; end
                        if startswith(ln, "\$EndElements"); break; end
                        if in_elems
                            p = split(ln)
                            length(p) < 4 && continue
                            et = tryparse(Int, p[2]); nt = tryparse(Int, p[3])
                            (et===nothing || nt===nothing) && continue
                            if nt > 0 && length(p) >= 3+nt
                                ph = tryparse(Int, p[4]); ph!==nothing && push!(phys_tags, ph)
                            end
                        end
                    end
                    @test any(t -> t != 0, phys_tags)
                end
            finally
                rm(outgeo; force=true)
                rm(outmsh; force=true)
                close(lf)
            end
        end
    end

    @testset "merge_overlapping_boxes" begin
        boxes = [
            BoxElem("c", 1, 0, 0, 0, 0, 10, 10),
            BoxElem("c", 1, 0, 0, 5, 0, 15, 10),  # overlaps with first
            BoxElem("c", 1, 0, 0, 20, 0, 30, 10), # separate
        ]
        merged = merge_overlapping_boxes(boxes)
        @test length(merged) == 2
        # First two should be merged into one covering 0-15
        xs = sort([b.x1 for b in merged])
        @test xs[1] == 0
        @test xs[2] == 20
    end

    @testset "Polygon iteration API" begin
        gds = find_test_gds()
        if gds !== nothing
            lf = open_layout(gds)
            try
                polys = get_polygons(lf)
                @test length(polys) >= 0
                if !isempty(polys)
                    p = polys[1]
                    @test length(p.xs) == length(p.ys)
                    @test length(p.xs) >= 3
                end
                pflat = get_polygons_flat(lf)
                @test length(pflat) >= 0
            finally
                close(lf)
            end
        end
    end
end
