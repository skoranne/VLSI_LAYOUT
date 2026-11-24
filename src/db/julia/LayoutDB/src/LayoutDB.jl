"""
    LayoutDB

Julia bindings to the VLSI_LAYOUT layout library (`src/db`, C++): GDSII and OASIS readers
and writers, the OASIS strict-mode table reader, the streaming flat-OASIS box decoder, and the
MAGVIEWER sidecar index (random access by CBLOCK, density pyramid, parallel decode).

The shared library is `liblayoutdb.so` (`make -C src/db liblayoutdb.so`). It is found through
`ENV["LAYOUTDB_LIB"]`, else next to this package (`src/db/liblayoutdb.so`).

```julia
using LayoutDB
f = LayoutFile("chip.gds")                 # GDSII or OASIS, whole model in memory
extent(f), topcells(f), format(f)
b = boxes(f)                               # Dict((layer, datatype) => 4×n Int64 [x1 y1 x2 y2]')
props = properties(LayoutFile("chip.oas"; properties = true))   # net ids / names, ...

t = OasisTables("big.oas")                 # strict tables only, < 1 ms at any size
t.unit, t.top, layername(t, 67, 20), cells(t)

s = Sidecar("big.oas")                     # builds big.oas.mvi once (seconds per billion boxes)
cnt, cov = density(s, 1, 0)                # layer 1, finest level: zero-copy matrices
u = units_in(s, 1, x1, y1, x2, y2)         # CBLOCKs of layer 1 meeting a window
xy = decode(s, u, 1)                       # 4×n Int32, decoded in parallel in C++

stream_boxes("flat.oas")                   # flat OASIS straight to per-layer arrays
```
Coordinates are database units (`OasisTables(...).unit` steps per micron for OASIS).
Layer indices of a `Sidecar` are 1-based; unit ids are the sidecar's own (0-based) numbers.
"""
module LayoutDB

export LayoutFile, OasisTables, OasisCell, Sidecar, SidecarLayer, LayoutProperty
export format, isstrict, extent, topcells, cell_extents, properties, boxes, polygons, write_gds, write_oasis
export stream_boxes, layername, layernames, cells, dbu_per_micron
export layers, density, level_for, units_in, unit_boxes, decode, density_level_extent

const _lib = Ref{String}("")

function libpath()
    if isempty(_lib[])
        p = get(ENV, "LAYOUTDB_LIB", normpath(joinpath(@__DIR__, "..", "..", "..", "liblayoutdb.so")))
        isfile(p) || error("LayoutDB: shared library not found at $p (build: make -C src/db liblayoutdb.so, or set LAYOUTDB_LIB)")
        _lib[] = p
    end
    return _lib[]
end

# ccall needs a constant library name; resolve it once through a handle.
const _handle = Ref{Ptr{Cvoid}}(C_NULL)
function sym(name::Symbol)
    if _handle[] == C_NULL
        _handle[] = Base.Libc.Libdl.dlopen(libpath())
    end
    return Base.Libc.Libdl.dlsym(_handle[], name)
end

const ERRN = 1024
errbuf() = zeros(UInt8, ERRN)
cstring(b::Vector{UInt8}) = unsafe_string(pointer(b))

struct CExtent
    xmin::Int64
    ymin::Int64
    xmax::Int64
    ymax::Int64
    valid::Cint
end

struct COpenOpts
    flags::UInt32
    large_cell_box_threshold::UInt64
end

# ============================================================================ LayoutFile
"""
    LayoutFile(path; properties=false, strict=false)

Open a GDSII or OASIS file (whole model in memory). `properties=true` keeps PROPERTY records
(net ids and names, metrology tags, ...). Closed by `close(f)` or the finalizer.
"""
mutable struct LayoutFile
    h::Ptr{Cvoid}
    path::String
    function LayoutFile(path::AbstractString; properties::Bool = false, strict::Bool = false)
        o = Ref(COpenOpts(UInt32((strict ? 1 : 0) | (properties ? 2 : 0)), 0))
        e = errbuf()
        h = ccall(sym(:ldb_open_opts), Ptr{Cvoid}, (Cstring, Ref{COpenOpts}, Ptr{UInt8}, Csize_t),
                  path, o, e, ERRN)
        h == C_NULL && error("LayoutDB: cannot open $path: $(cstring(e))")
        f = new(h, String(path))
        finalizer(close, f)
        return f
    end
end

function Base.close(f::LayoutFile)
    if f.h != C_NULL
        ccall(sym(:ldb_close), Cvoid, (Ptr{Cvoid},), f.h)
        f.h = C_NULL
    end
    return nothing
end

checkopen(f::LayoutFile) = f.h == C_NULL && error("LayoutDB: file is closed")

"`:GDSII`, `:OASIS` or `:unknown`."
function format(f::LayoutFile)
    checkopen(f)
    k = ccall(sym(:ldb_format), Cint, (Ptr{Cvoid},), f.h)
    k == 1 ? :GDSII : k == 2 ? :OASIS : :unknown
end

isstrict(f::LayoutFile) = (checkopen(f); ccall(sym(:ldb_is_strict), Cint, (Ptr{Cvoid},), f.h) != 0)

"Flattened extent `(x1, y1, x2, y2)` of the top cell(s), or `nothing` when empty."
function extent(f::LayoutFile; top::Union{Nothing,AbstractString} = nothing)
    checkopen(f)
    e = Ref(CExtent(0, 0, 0, 0, 0))
    rc = ccall(sym(:ldb_extent_whole_hier), Cint, (Ptr{Cvoid}, Cstring, Ref{CExtent}), f.h,
               top === nothing ? C_NULL : top, e)
    (rc != 0 || e[].valid == 0) && return nothing
    return (e[].xmin, e[].ymin, e[].xmax, e[].ymax)
end

function topcells(f::LayoutFile)
    checkopen(f)
    n = ccall(sym(:ldb_top_cell_count), Cint, (Ptr{Cvoid},), f.h)
    [unsafe_string(ccall(sym(:ldb_top_cell_name), Cstring, (Ptr{Cvoid}, Cint), f.h, i - 1)) for i in 1:n]
end

"A property: `target` is `:file`, `:cell`, `:element` or `:instance`."
struct LayoutProperty
    target::Symbol
    cell::String
    name::String
    value::String
end

const _targets = (:file, :cell, :element, :instance)

function _prop_cb(ud::Ptr{Cvoid}, kind::Cint, cell::Cstring, name::Cstring, value::Cstring)
    v = unsafe_pointer_to_objref(ud)::Vector{LayoutProperty}
    push!(v, LayoutProperty(_targets[clamp(kind + 1, 1, 4)], cell == C_NULL ? "" : unsafe_string(cell),
                            name == C_NULL ? "" : unsafe_string(name), value == C_NULL ? "" : unsafe_string(value)))
    return nothing
end

"All properties (open the file with `properties=true` to keep element / cell ones)."
function properties(f::LayoutFile)
    checkopen(f)
    v = LayoutProperty[]
    cb = @cfunction(_prop_cb, Cvoid, (Ptr{Cvoid}, Cint, Cstring, Cstring, Cstring))
    GC.@preserve v ccall(sym(:ldb_iterate_properties), Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}), f.h, cb,
                         pointer_from_objref(v))
    return v
end

# Box collector: per (layer, datatype) a flat Int64 vector x1,y1,x2,y2,...
mutable struct BoxBag
    d::Dict{Tuple{Int,Int},Vector{Int64}}
    only::Union{Nothing,Tuple{Int,Int}}
end

function _box_cb(ud::Ptr{Cvoid}, cell::Cstring, layer::Cint, dt::Cint, kind::Cint,
                 x1::Int64, y1::Int64, x2::Int64, y2::Int64)
    bag = unsafe_pointer_to_objref(ud)::BoxBag
    key = (Int(layer), Int(dt))
    (bag.only === nothing || bag.only == key) || return nothing
    v = get!(Vector{Int64}, bag.d, key)
    push!(v, x1, y1, x2, y2)
    return nothing
end

_as_matrices(d) = Dict(k => reshape(v, 4, :) for (k, v) in d)

"""
    boxes(f; flat=true, layer=nothing)

Boxes per `(layer, datatype)` as 4×n `Int64` matrices (rows x1, y1, x2, y2). Polygons and
paths become their rectangle cover. `flat=true` expands the hierarchy; `layer=(l, d)` keeps one.
"""
function boxes(f::LayoutFile; flat::Bool = true, layer::Union{Nothing,Tuple{Int,Int}} = nothing)
    checkopen(f)
    bag = BoxBag(Dict{Tuple{Int,Int},Vector{Int64}}(), layer)
    cb = @cfunction(_box_cb, Cvoid, (Ptr{Cvoid}, Cstring, Cint, Cint, Cint, Int64, Int64, Int64, Int64))
    GC.@preserve bag ccall(sym(flat ? :ldb_iterate_boxes_flat : :ldb_iterate_boxes), Cint,
                           (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}), f.h, cb, pointer_from_objref(bag))
    return _as_matrices(bag.d)
end

mutable struct ExtBag
    d::Dict{String,NTuple{4,Int64}}
    only::Union{Nothing,Tuple{Int,Int}}
end

function _cell_ext_cb(ud::Ptr{Cvoid}, cell::Cstring, layer::Cint, dt::Cint, e::CExtent)
    e.valid == 0 && return nothing
    bag = unsafe_pointer_to_objref(ud)::ExtBag
    (bag.only === nothing || bag.only == (Int(layer), Int(dt))) || return nothing
    d = bag.d
    k = unsafe_string(cell)
    o = get(d, k, (typemax(Int64), typemax(Int64), typemin(Int64), typemin(Int64)))
    d[k] = (min(o[1], e.xmin), min(o[2], e.ymin), max(o[3], e.xmax), max(o[4], e.ymax))
    return nothing
end

"""
    cell_extents(f) -> Dict{String, NTuple{4,Int64}}

Extent `(x1, y1, x2, y2)` of each cell's own shapes (all layers, placements not expanded),
e.g. the outline of every standard cell in a library GDS (`layer=(235, 4)`: sky130 prBoundary).
"""
function cell_extents(f::LayoutFile; layer::Union{Nothing,Tuple{Int,Int}} = nothing)
    checkopen(f)
    bag = ExtBag(Dict{String,NTuple{4,Int64}}(), layer)
    cb = @cfunction(_cell_ext_cb, Cvoid, (Ptr{Cvoid}, Cstring, Cint, Cint, CExtent))
    GC.@preserve bag ccall(sym(:ldb_extent_by_cell_layer), Cint, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}), f.h, cb,
                           pointer_from_objref(bag))
    return bag.d
end

mutable struct PolyBag
    d::Dict{Tuple{Int,Int},Vector{Matrix{Int64}}}
end

function _poly_cb(ud::Ptr{Cvoid}, cell::Cstring, layer::Cint, dt::Cint, kind::Cint, n::Cint,
                  xs::Ptr{Int64}, ys::Ptr{Int64})
    bag = unsafe_pointer_to_objref(ud)::PolyBag
    m = Matrix{Int64}(undef, 2, n)
    for i in 1:n
        m[1, i] = unsafe_load(xs, i); m[2, i] = unsafe_load(ys, i)
    end
    push!(get!(Vector{Matrix{Int64}}, bag.d, (Int(layer), Int(dt))), m)
    return nothing
end

"Polygons per `(layer, datatype)`: vectors of 2×n vertex matrices."
function polygons(f::LayoutFile; flat::Bool = true)
    checkopen(f)
    bag = PolyBag(Dict{Tuple{Int,Int},Vector{Matrix{Int64}}}())
    cb = @cfunction(_poly_cb, Cvoid, (Ptr{Cvoid}, Cstring, Cint, Cint, Cint, Cint, Ptr{Int64}, Ptr{Int64}))
    GC.@preserve bag ccall(sym(flat ? :ldb_iterate_polygons_flat : :ldb_iterate_polygons), Cint,
                           (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}), f.h, cb, pointer_from_objref(bag))
    return bag.d
end

function _write(f::LayoutFile, s::Symbol, path::AbstractString)
    checkopen(f)
    e = errbuf()
    rc = ccall(sym(s), Cint, (Ptr{Cvoid}, Cstring, Ptr{UInt8}, Csize_t), f.h, path, e, ERRN)
    rc == 0 || error("LayoutDB: writing $path failed: $(cstring(e))")
    return path
end
"""
    write_gds(f, path)

Lossless rewrite of a file that was read as GDSII (its record stream is kept). Files read as
OASIS cannot be written as GDSII (the library has no GDSII writer for the cell model).
"""
write_gds(f::LayoutFile, path::AbstractString) = _write(f, :ldb_write_gdsii, path)
"Write the cell model as OASIS (any source format)."
write_oasis(f::LayoutFile, path::AbstractString) = _write(f, :ldb_write_oasis, path)

"""
    stream_boxes(path; layer=nothing)

Flat OASIS: decode every box straight into per-layer 4×n `Int64` matrices without building the
cell model (the fastest way to get all geometry). Errors on hierarchical files.
"""
function stream_boxes(path::AbstractString; layer::Union{Nothing,Tuple{Int,Int}} = nothing)
    bag = BoxBag(Dict{Tuple{Int,Int},Vector{Int64}}(), layer)
    cb = @cfunction(_box_cb, Cvoid, (Ptr{Cvoid}, Cstring, Cint, Cint, Cint, Int64, Int64, Int64, Int64))
    e = errbuf()
    rc = GC.@preserve bag ccall(sym(:ldb_stream_boxes), Cint, (Cstring, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{UInt8}, Csize_t),
                                path, cb, pointer_from_objref(bag), e, ERRN)
    rc == 2 && error("LayoutDB: $path is hierarchical; use boxes(LayoutFile(path))")
    rc == 0 || error("LayoutDB: streaming $path failed ($rc): $(cstring(e))")
    return _as_matrices(bag.d)
end

"""
    dbu_per_micron(path) -> Float64

Database units per micron of a GDSII (UNITS record) or OASIS (START unit) file, read from the
file header without parsing geometry.
"""
function dbu_per_micron(path::AbstractString)
    open(path) do io
        magic = read(io, min(13, filesize(path)))
        if length(magic) >= 13 && String(magic) == "%SEMI-OASIS\r\n"
            return OasisTables(path).unit
        end
        seek(io, 0)
        while !eof(io)
            len = Int(ntoh(read(io, UInt16))); rec = ntoh(read(io, UInt16))
            len < 4 && error("LayoutDB: bad GDSII record in $path")
            if rec == 0x0305                                   # UNITS: user/db, meters/db
                read(io, 8); m = _gds_real(read(io, 8))
                return round(1e-6 / m; sigdigits = 12)
            end
            skip(io, len - 4)
        end
        error("LayoutDB: no UNITS record in $path")
    end
end

# GDSII 8-byte excess-64 base-16 real
function _gds_real(b::Vector{UInt8})
    sgn = (b[1] & 0x80) != 0 ? -1.0 : 1.0
    ex = Int(b[1] & 0x7f) - 64
    mant = 0.0
    for i in 2:8
        mant = mant * 256 + b[i]
    end
    return sgn * mant / 2.0^56 * 16.0^ex
end

# ============================================================================ OasisTables
"A CELLNAME entry: `offset` (S_CELL_OFFSET, -1 if absent), `bbox` (S_BOUNDING_BOX or `nothing`)."
struct OasisCell
    name::String
    offset::Int64
    bbox::Union{Nothing,NTuple{4,Int64}}
end

"""
    OasisTables(path)

The strict-mode tables of an OASIS file, read without touching geometry: fields `unit`
(steps per micron), `top` (S_TOP_CELL), `strict`, `offsets_ok`, `open_ms`.
"""
mutable struct OasisTables
    h::Ptr{Cvoid}
    unit::Float64
    top::String
    strict::Bool
    offsets_ok::Bool
    open_ms::Float64
    function OasisTables(path::AbstractString)
        e = errbuf()
        h = ccall(sym(:ldb_oas_info_open), Ptr{Cvoid}, (Cstring, Ptr{UInt8}, Csize_t), path, e, ERRN)
        h == C_NULL && error("LayoutDB: cannot read OASIS tables of $path: $(cstring(e))")
        b = zeros(UInt8, 1024)
        top = ccall(sym(:ldb_oas_info_top), Cint, (Ptr{Cvoid}, Ptr{UInt8}, Cint), h, b, 1024) != 0 ? cstring(b) : ""
        t = new(h, ccall(sym(:ldb_oas_info_unit), Cdouble, (Ptr{Cvoid},), h), top,
                ccall(sym(:ldb_oas_info_strict), Cint, (Ptr{Cvoid},), h) != 0,
                ccall(sym(:ldb_oas_info_offsets_ok), Cint, (Ptr{Cvoid},), h) != 0,
                ccall(sym(:ldb_oas_info_open_ms), Cdouble, (Ptr{Cvoid},), h))
        finalizer(close, t)
        return t
    end
end

function Base.close(t::OasisTables)
    if t.h != C_NULL
        ccall(sym(:ldb_oas_info_close), Cvoid, (Ptr{Cvoid},), t.h)
        t.h = C_NULL
    end
    return nothing
end

"LAYERNAME of `layer`/`datatype`, or `nothing`."
function layername(t::OasisTables, layer::Integer, datatype::Integer)
    b = zeros(UInt8, 512)
    ccall(sym(:ldb_oas_info_layer_name), Cint, (Ptr{Cvoid}, Cint, Cint, Ptr{UInt8}, Cint), t.h, layer,
          datatype, b, 512) != 0 ? cstring(b) : nothing
end

"The LAYERNAME table: `(name, layers, datatypes, text)` with ranges (`typemax(Int64)` = unbounded)."
function layernames(t::OasisTables)
    n = ccall(sym(:ldb_oas_info_n_layernames), Cint, (Ptr{Cvoid},), t.h)
    out = NamedTuple{(:name, :layers, :datatypes, :text),Tuple{String,UnitRange{Int64},UnitRange{Int64},Bool}}[]
    for i in 0:n-1
        llo, lhi, dlo, dhi = Ref{Int64}(0), Ref{Int64}(0), Ref{Int64}(0), Ref{Int64}(0)
        txt = Ref{Cint}(0); b = zeros(UInt8, 512)
        ccall(sym(:ldb_oas_info_layername), Cint,
              (Ptr{Cvoid}, Cint, Ref{Int64}, Ref{Int64}, Ref{Int64}, Ref{Int64}, Ref{Cint}, Ptr{UInt8}, Cint),
              t.h, i, llo, lhi, dlo, dhi, txt, b, 512)
        hi(v) = v[] < 0 ? typemax(Int64) : v[]
        push!(out, (name = cstring(b), layers = llo[]:hi(lhi), datatypes = dlo[]:hi(dhi), text = txt[] != 0))
    end
    return out
end

function cells(t::OasisTables)
    n = ccall(sym(:ldb_oas_info_n_cells), Cint, (Ptr{Cvoid},), t.h)
    out = OasisCell[]
    for i in 0:n-1
        b = zeros(UInt8, 1024); off = Ref{Int64}(0); fl = Ref{Cint}(0); bb = zeros(Int64, 4)
        ccall(sym(:ldb_oas_info_cell), Cint, (Ptr{Cvoid}, Cint, Ptr{UInt8}, Cint, Ref{Int64}, Ref{Cint}, Ptr{Int64}),
              t.h, i, b, 1024, off, fl, bb)
        ok = fl[] >= 0 && (fl[] & 6) == 0
        push!(out, OasisCell(cstring(b), off[], ok ? (bb[1], bb[2], bb[3], bb[4]) : nothing))
    end
    return out
end

# ============================================================================ Sidecar
struct SidecarLayer
    layer::Int64
    datatype::Int64
    count::Int64
    bbox::NTuple{4,Int64}
end

"""
    Sidecar(oas; mvi="", grid=0)

Open (building on first use) the MAGVIEWER sidecar index of a flat OASIS file. Fields:
`unit`, `bbox`, `tile`, `nx`, `ny`, `nlevels`, `nlayers`, `nunits`, `total_boxes`,
`ndependent` (units that need a modal snapshot), `build_s`, `built`, `path`.
"""
mutable struct Sidecar
    h::Ptr{Cvoid}
    path::String
    unit::Float64
    bbox::NTuple{4,Int64}
    tile::Int64
    nx::Int
    ny::Int
    nlevels::Int
    nlayers::Int
    nunits::Int64
    total_boxes::Int64
    ndependent::Int64
    build_s::Float64
    built::Bool
    function Sidecar(oas::AbstractString; mvi::AbstractString = "", grid::Integer = 0)
        e = errbuf(); built = Ref{Cint}(0)
        h = ccall(sym(:ldb_mvi_open_or_build), Ptr{Cvoid}, (Cstring, Cstring, Cint, Ref{Cint}, Ptr{UInt8}, Csize_t),
                  oas, mvi, grid, built, e, ERRN)
        h == C_NULL && error("LayoutDB: no sidecar for $oas: $(cstring(e))")
        u = Ref{Cdouble}(0); bb = zeros(Int64, 4); tile = Ref{Int64}(0); nx = Ref{Cint}(0); ny = Ref{Cint}(0)
        nlev = Ref{Cint}(0); nl = Ref{Cint}(0); nu = Ref{Int64}(0); tot = Ref{Int64}(0); nd = Ref{Int64}(0)
        bs = Ref{Cdouble}(0)
        ccall(sym(:ldb_mvi_info), Cint,
              (Ptr{Cvoid}, Ref{Cdouble}, Ptr{Int64}, Ref{Int64}, Ref{Cint}, Ref{Cint}, Ref{Cint}, Ref{Cint},
               Ref{Int64}, Ref{Int64}, Ref{Int64}, Ref{Cdouble}), h, u, bb, tile, nx, ny, nlev, nl, nu, tot, nd, bs)
        s = new(h, unsafe_string(ccall(sym(:ldb_mvi_path), Cstring, (Ptr{Cvoid},), h)), u[], Tuple(bb), tile[],
                nx[], ny[], nlev[], nl[], nu[], tot[], nd[], bs[], built[] != 0)
        finalizer(close, s)
        return s
    end
end

function Base.close(s::Sidecar)
    if s.h != C_NULL
        ccall(sym(:ldb_mvi_close), Cvoid, (Ptr{Cvoid},), s.h)
        s.h = C_NULL
    end
    return nothing
end

function layers(s::Sidecar)
    map(1:s.nlayers) do i
        l, d, c = Ref{Int64}(0), Ref{Int64}(0), Ref{Int64}(0); bb = zeros(Int64, 4)
        ccall(sym(:ldb_mvi_layer), Cint, (Ptr{Cvoid}, Cint, Ref{Int64}, Ref{Int64}, Ref{Int64}, Ptr{Int64}),
              s.h, i - 1, l, d, c, bb)
        SidecarLayer(l[], d[], c[], Tuple(bb))
    end
end

"""
    density(s, layer, level) -> (counts::Matrix{UInt32}, coverage::Matrix{Float32})

Zero-copy views of one pyramid level (0 = finest) of a layer: boxes centred in each tile and
covered area / tile area. Index `[i, j]` is tile column i, row j. Valid while `s` is open.
"""
function density(s::Sidecar, layer::Integer, level::Integer = 0)
    ax, ay = Ref{Cint}(0), Ref{Cint}(0); pc, pv = Ref{Ptr{UInt32}}(C_NULL), Ref{Ptr{Float32}}(C_NULL)
    rc = ccall(sym(:ldb_mvi_density), Cint, (Ptr{Cvoid}, Cint, Cint, Ref{Cint}, Ref{Cint}, Ref{Ptr{UInt32}}, Ref{Ptr{Float32}}),
               s.h, layer - 1, level, ax, ay, pc, pv)
    rc == 0 || error("LayoutDB: no density for layer $layer level $level")
    return unsafe_wrap(Matrix{UInt32}, pc[], (ax[], ay[])), unsafe_wrap(Matrix{Float32}, pv[], (ax[], ay[]))
end

"World rectangle of tile `[i, j]` at `level`."
function density_level_extent(s::Sidecar, level::Integer, i::Integer, j::Integer)
    t = s.tile << level
    x = s.bbox[1] + (i - 1) * t; y = s.bbox[2] + (j - 1) * t
    return (x, y, x + t, y + t)
end

"Finest level whose tiles are at least `min_tile` database units."
level_for(s::Sidecar, min_tile::Real) = something(findfirst(l -> s.tile * 2.0^l >= min_tile, 0:s.nlevels-1), s.nlevels) - 1

"Units (CBLOCKs) holding boxes of `layer` (0 = any) that meet the window."
function units_in(s::Sidecar, layer::Integer, x1::Integer, y1::Integer, x2::Integer, y2::Integer)
    f = sym(:ldb_mvi_units_in)
    n = ccall(f, Int64, (Ptr{Cvoid}, Cint, Int64, Int64, Int64, Int64, Ptr{Int64}, Int64), s.h, layer - 1, x1, y1, x2, y2,
              C_NULL, 0)
    ids = Vector{Int64}(undef, n)
    n > 0 && ccall(f, Int64, (Ptr{Cvoid}, Cint, Int64, Int64, Int64, Int64, Ptr{Int64}, Int64), s.h, layer - 1, x1, y1,
                   x2, y2, ids, n)
    return ids
end
units_in(s::Sidecar, layer::Integer = 0) = units_in(s, layer, typemin(Int64), typemin(Int64), typemax(Int64), typemax(Int64))

unit_boxes(s::Sidecar, u::Integer, layer::Integer = 0) =
    ccall(sym(:ldb_mvi_unit_boxes), Int64, (Ptr{Cvoid}, Int64, Cint, Ptr{Int64}), s.h, u, layer - 1, C_NULL)

"""
    decode(s, units, layer=0) -> Matrix{Int32}  (4×n: x1, y1, x2, y2)

Decode the boxes of `layer` (0 = all) in the given units, in parallel (C++ threads,
`LDB_THREADS`), unit after unit in the order given.
"""
function decode(s::Sidecar, units::AbstractVector{<:Integer}, layer::Integer = 0)
    u = Vector{Int64}(units)
    need = sum((unit_boxes(s, k, layer) for k in u); init = 0)
    out = Matrix{Int32}(undef, 4, need)
    e = errbuf()
    n = ccall(sym(:ldb_mvi_decode), Int64,
              (Ptr{Cvoid}, Ptr{Int64}, Int64, Cint, Ptr{Int32}, Int64, Ptr{Int64}, Ptr{UInt8}, Csize_t),
              s.h, u, length(u), layer - 1, out, need, C_NULL, e, ERRN)
    n == typemin(Int64) && error("LayoutDB: decode failed: $(cstring(e))")
    n < 0 && error("LayoutDB: decode needs $(-n) boxes, sidecar said $need")
    return out
end

Base.show(io::IO, s::Sidecar) = print(io, "Sidecar($(s.path): $(s.nlayers) layers, $(s.total_boxes) boxes, ",
                                      "$(s.nunits) units, grid $(s.nx)×$(s.ny), $(s.nlevels) levels)")
Base.show(io::IO, t::OasisTables) = print(io, "OasisTables(unit $(t.unit), top \"$(t.top)\", strict $(t.strict))")
Base.show(io::IO, f::LayoutFile) = print(io, "LayoutFile($(f.path))")

end # module
