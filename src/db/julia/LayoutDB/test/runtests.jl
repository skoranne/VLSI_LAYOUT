# LayoutDB tests: GDSII -> OASIS round trip through the C++ library, streaming decode, strict
# tables, and the sidecar (density + parallel unit decode) checked against the streaming parser.
# Optional: LAYOUTDB_TEST_OAS=/path/flat_strict.oas adds the same checks on a real design.
using LayoutDB, Test

const SRC = normpath(joinpath(@__DIR__, "..", "..", "..", ".."))       # VLSI_LAYOUT/src

sortcols(m) = m[:, sortperm(collect(eachcol(m)))]
same_boxes(a, b) = keys(a) == keys(b) && all(sortcols(a[k]) == sortcols(Int64.(b[k])) for k in keys(a))

function sidecar_checks(oas)
    ref = stream_boxes(oas)
    mvi = tempname() * ".mvi"
    s = Sidecar(oas; mvi = mvi)
    @test s.built
    @test s.total_boxes == sum(size(m, 2) for m in values(ref))
    for (i, L) in enumerate(layers(s))
        xy = decode(s, units_in(s, i), i)
        @test sortcols(Int64.(xy)) == sortcols(ref[(Int(L.layer), Int(L.datatype))])
        cnt, cov = density(s, i, 0)
        @test sum(Int64, cnt) == L.count                         # every box counted once
        top, _ = density(s, i, s.nlevels - 1)
        @test size(top) == (1, 1) && top[1, 1] == L.count        # pyramid reduces to one tile
    end
    close(s)
    s2 = Sidecar(oas; mvi = mvi)                                  # second open reuses the file
    @test !s2.built
    close(s2); rm(mvi; force = true)
end

@testset "LayoutDB" begin
    gds = joinpath(SRC, "ovlp_sq.gds")
    @testset "GDSII model" begin
        f = LayoutFile(gds)
        @test format(f) == :GDSII
        @test extent(f) !== nothing
        @test !isempty(topcells(f))
        @test dbu_per_micron(gds) > 0
        ce = cell_extents(f)
        @test haskey(ce, topcells(f)[1])
        b = boxes(f)
        @test !isempty(b)
        global gds_boxes = b
        oas = tempname() * ".oas"
        write_oasis(f, oas)
        close(f)
        global tmp_oas = oas
    end
    @testset "OASIS round trip" begin
        g = LayoutFile(tmp_oas)
        @test format(g) == :OASIS
        @test dbu_per_micron(tmp_oas) ≈ dbu_per_micron(gds)
        @test same_boxes(gds_boxes, boxes(g))
        @test same_boxes(gds_boxes, stream_boxes(tmp_oas))
        @test_throws ErrorException write_gds(g, tempname() * ".gds")   # GDSII writer: GDSII-sourced files only
        gds2 = tempname() * ".gds"                                      # lossless GDSII rewrite
        write_gds(LayoutFile(gds), gds2)
        @test read(gds2) == read(gds)
        rm(gds2; force = true)
    end
    @testset "tables" begin
        t = OasisTables(tmp_oas)
        @test t.unit > 0
        @test t.open_ms >= 0
    end
    @testset "sidecar (small, non-strict)" begin
        sidecar_checks(tmp_oas)
    end
    big = get(ENV, "LAYOUTDB_TEST_OAS", "")
    if !isempty(big)
        @testset "design $(basename(big))" begin
            t = OasisTables(big)
            @test t.strict
            @test all(c.offset >= 0 for c in cells(t))
            @test !isempty(layernames(t))
            sidecar_checks(big)
        end
    end
    rm(tmp_oas; force = true)
end
