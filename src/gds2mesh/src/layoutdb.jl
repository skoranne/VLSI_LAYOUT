# Julia ccall wrapper for layoutdb C API
# Connects to liblayoutdb shared library in VLSI_LAYOUT/src/db

module LayoutDB

using Printf

LIB_PATH = ""

function __init__()
    global LIB_PATH
    # Look for liblayoutdb.dylib relative to this package or in known locations
    candidates = [
        joinpath(@__DIR__, "..", "..", "db", "liblayoutdb.dylib"),
        joinpath(@__DIR__, "..", "..", "db", "liblayoutdb.so"),
        joinpath(@__DIR__, "..", "..", "..", "db", "liblayoutdb.dylib"),
        "/Users/skoranne/GITHUB/VLSI_LAYOUT/src/db/liblayoutdb.dylib",
        "/Users/skoranne/GITHUB/VLSI_LAYOUT/src/db/liblayoutdb.so",
    ]
    for c in candidates
        p = abspath(c)
        if isfile(p)
            LIB_PATH = p
            return
        end
    end
    # Fallback: try to find in PATH or common locations
    LIB_PATH = "liblayoutdb"
end

function libpath()
    global LIB_PATH
    isempty(LIB_PATH) && __init__()
    return LIB_PATH
end

# --- C struct mirrors ---

struct LdbExtent
    xmin::Int64
    ymin::Int64
    xmax::Int64
    ymax::Int64
    valid::Cint
end

LdbExtent() = LdbExtent(0, 0, 0, 0, 0)

# Open flags
const LDB_OPEN_DEFAULT    = UInt32(0)
const LDB_OPEN_STRICT     = UInt32(1)
const LDB_OPEN_PROPERTY   = UInt32(2)
const LDB_OPEN_LOW_MEMORY = UInt32(4)

struct LdbOpenOpts
    flags::UInt32
    large_cell_box_threshold::UInt64
end

# --- Opaque handle ---
const LdbFilePtr = Ptr{Cvoid}

# --- Box element (from callback) ---
struct BoxElem
    cell::String
    layer::Int
    datatype::Int
    kind::Int  # 0=boundary/polygon,1=path,2=box/rect,3=sref,4=aref,5=other
    x1::Int64
    y1::Int64
    x2::Int64
    y2::Int64
end

# --- Cell/layer extent ---
struct CellLayerExtent
    cell::String
    layer::Int
    datatype::Int
    extent::LdbExtent
end

# --- Property ---
struct Property
    target_kind::Int  # 0=file,1=cell,2=element,3=instance
    cell::String
    name::String
    value::String
end

# --- High-level API ---

mutable struct LayoutFile
    handle::LdbFilePtr
    path::String
    format::Int  # 1=GDSII, 2=OASIS, 0=unknown

    function LayoutFile(path::AbstractString; flags::UInt32=LDB_OPEN_DEFAULT, large_cell_threshold::UInt64=UInt64(0))
        errbuf = Vector{UInt8}(undef, 512)
        fill!(errbuf, 0)
        opts = LdbOpenOpts(flags, large_cell_threshold)
        libpath()
        handle = ccall((:ldb_open_opts, LIB_PATH), LdbFilePtr,
            (Cstring, Ref{LdbOpenOpts}, Ptr{UInt8}, Csize_t),
            String(path), opts, errbuf, sizeof(errbuf))
        if handle == C_NULL
            err = GC.@preserve errbuf unsafe_string(pointer(errbuf))
            error("Failed to open layout '$path': $err")
        end
        fmt = ccall((:ldb_format, LIB_PATH), Cint, (LdbFilePtr,), handle)
        lf = new(handle, String(path), Int(fmt))
        finalizer(close_layout, lf)
        return lf
    end
end

function close_layout(lf::LayoutFile)
    if lf.handle != C_NULL
        libpath()
        ccall((:ldb_close, LIB_PATH), Cvoid, (LdbFilePtr,), lf.handle)
        lf.handle = C_NULL
    end
    return nothing
end

function Base.close(lf::LayoutFile)
    close_layout(lf)
    return nothing
end

function Base.isopen(lf::LayoutFile)
    return lf.handle != C_NULL
end

function format_name(lf::LayoutFile)
    lf.format == 1 ? "GDSII" : lf.format == 2 ? "OASIS" : "Unknown"
end

function memory_bytes(lf::LayoutFile)
    lf.handle == C_NULL && return 0
    libpath()
    return Int(ccall((:ldb_memory_bytes, LIB_PATH), Csize_t, (LdbFilePtr,), lf.handle))
end

function extent_whole(lf::LayoutFile)
    lf.handle == C_NULL && error("Layout file is closed")
    ext = Ref(LdbExtent())
    libpath()
    rc = ccall((:ldb_extent_whole, LIB_PATH), Cint, (LdbFilePtr, Ref{LdbExtent}), lf.handle, ext)
    rc != 0 && error("ldb_extent_whole failed")
    return ext[]
end

# --- Box iteration ---

function _box_cb(ud::Ptr{Cvoid}, cell::Cstring, layer::Cint, datatype::Cint, kind::Cint,
                 x1::Int64, y1::Int64, x2::Int64, y2::Int64)::Cvoid
    boxes_ref = unsafe_pointer_to_objref(ud)::Vector{BoxElem}
    push!(boxes_ref, BoxElem(unsafe_string(cell), Int(layer), Int(datatype), Int(kind),
                             x1, y1, x2, y2))
    return nothing
end

function iterate_boxes(lf::LayoutFile)
    lf.handle == C_NULL && error("Layout file is closed")
    boxes = BoxElem[]
    cb = @cfunction(_box_cb, Cvoid, (Ptr{Cvoid}, Cstring, Cint, Cint, Cint, Int64, Int64, Int64, Int64))

    GC.@preserve boxes begin
        libpath()
        rc = ccall((:ldb_iterate_boxes, LIB_PATH), Cint,
            (LdbFilePtr, Ptr{Cvoid}, Ptr{Cvoid}),
            lf.handle, cb, pointer_from_objref(boxes))
        rc != 0 && error("ldb_iterate_boxes failed")
    end
    return boxes
end

function iterate_boxes_flat(lf::LayoutFile)
    lf.handle == C_NULL && error("Layout file is closed")
    boxes = BoxElem[]
    cb = @cfunction(_box_cb, Cvoid, (Ptr{Cvoid}, Cstring, Cint, Cint, Cint, Int64, Int64, Int64, Int64))
    GC.@preserve boxes begin
        libpath()
        rc = ccall((:ldb_iterate_boxes_flat, LIB_PATH), Cint,
            (LdbFilePtr, Ptr{Cvoid}, Ptr{Cvoid}),
            lf.handle, cb, pointer_from_objref(boxes))
        rc != 0 && error("ldb_iterate_boxes_flat failed")
    end
    return boxes
end

struct PolyElem
    cell::String
    layer::Int
    datatype::Int
    kind::Int
    xs::Vector{Int64}
    ys::Vector{Int64}
end

function _poly_cb(ud::Ptr{Cvoid}, cell::Cstring, layer::Cint, datatype::Cint, kind::Cint,
                  npts::Cint, xs::Ptr{Int64}, ys::Ptr{Int64})::Cvoid
    ref = unsafe_pointer_to_objref(ud)::Vector{PolyElem}
    n = Int(npts)
    xv = Vector{Int64}(undef, n); yv = Vector{Int64}(undef, n)
    for i in 1:n
        xv[i] = unsafe_load(xs, i); yv[i] = unsafe_load(ys, i)
    end
    push!(ref, PolyElem(unsafe_string(cell), Int(layer), Int(datatype), Int(kind), xv, yv))
    return nothing
end

function iterate_polygons(lf::LayoutFile)
    lf.handle == C_NULL && error("Layout file is closed")
    polys = PolyElem[]
    cb = @cfunction(_poly_cb, Cvoid, (Ptr{Cvoid}, Cstring, Cint, Cint, Cint, Cint, Ptr{Int64}, Ptr{Int64}))
    GC.@preserve polys begin
        libpath()
        rc = ccall((:ldb_iterate_polygons, LIB_PATH), Cint, (LdbFilePtr, Ptr{Cvoid}, Ptr{Cvoid}), lf.handle, cb, pointer_from_objref(polys))
        rc != 0 && error("ldb_iterate_polygons failed")
    end
    return polys
end

function iterate_polygons_flat(lf::LayoutFile)
    lf.handle == C_NULL && error("Layout file is closed")
    polys = PolyElem[]
    cb = @cfunction(_poly_cb, Cvoid, (Ptr{Cvoid}, Cstring, Cint, Cint, Cint, Cint, Ptr{Int64}, Ptr{Int64}))
    GC.@preserve polys begin
        libpath()
        rc = ccall((:ldb_iterate_polygons_flat, LIB_PATH), Cint, (LdbFilePtr, Ptr{Cvoid}, Ptr{Cvoid}), lf.handle, cb, pointer_from_objref(polys))
        rc != 0 && error("ldb_iterate_polygons_flat failed")
    end
    return polys
end

# --- Cell/layer extent iteration ---

function _cell_layer_cb(ud::Ptr{Cvoid}, cell::Cstring, layer::Cint, datatype::Cint, ext::LdbExtent)::Cvoid
    ref = unsafe_pointer_to_objref(ud)::Vector{CellLayerExtent}
    push!(ref, CellLayerExtent(unsafe_string(cell), Int(layer), Int(datatype), ext))
    return nothing
end

function extent_by_cell_layer(lf::LayoutFile)
    lf.handle == C_NULL && error("Layout file is closed")
    results = CellLayerExtent[]
    cb = @cfunction(_cell_layer_cb, Cvoid, (Ptr{Cvoid}, Cstring, Cint, Cint, LdbExtent))

    GC.@preserve results begin
        libpath()
        rc = ccall((:ldb_extent_by_cell_layer, LIB_PATH), Cint,
            (LdbFilePtr, Ptr{Cvoid}, Ptr{Cvoid}),
            lf.handle, cb, pointer_from_objref(results))
        rc != 0 && error("ldb_extent_by_cell_layer failed")
    end
    return results
end

# --- Property iteration ---

function _prop_cb(ud::Ptr{Cvoid}, target_kind::Cint, cell::Cstring,
                  propname::Cstring, propvalue::Cstring)::Cvoid
    ref = unsafe_pointer_to_objref(ud)::Vector{Property}
    push!(ref, Property(Int(target_kind), unsafe_string(cell),
                        unsafe_string(propname), unsafe_string(propvalue)))
    return nothing
end

function iterate_properties(lf::LayoutFile)
    lf.handle == C_NULL && error("Layout file is closed")
    props = Property[]
    cb = @cfunction(_prop_cb, Cvoid, (Ptr{Cvoid}, Cint, Cstring, Cstring, Cstring))

    GC.@preserve props begin
        libpath()
        rc = ccall((:ldb_iterate_properties, LIB_PATH), Cint,
            (LdbFilePtr, Ptr{Cvoid}, Ptr{Cvoid}),
            lf.handle, cb, pointer_from_objref(props))
        rc != 0 && error("ldb_iterate_properties failed")
    end
    return props
end

# --- Hierarchy ---

function print_hierarchy(lf::LayoutFile; io::IO=stdout)
    lf.handle == C_NULL && error("Layout file is closed")
    # Write to temp file, read back
    tmppath = tempname()
    fp = ccall(:fopen, Ptr{Cvoid}, (Cstring, Cstring), tmppath, "w")
    fp == C_NULL && error("Failed to open temp file for hierarchy")
    try
        libpath()
        rc = ccall((:ldb_print_hierarchy, LIB_PATH), Cint, (LdbFilePtr, Ptr{Cvoid}), lf.handle, fp)
        rc != 0 && error("ldb_print_hierarchy failed")
    finally
        ccall(:fclose, Cint, (Ptr{Cvoid},), fp)
    end
    text = read(tmppath, String)
    rm(tmppath; force=true)
    write(io, text)
    return text
end

# --- Convenience ---

"""Return unique (layer, datatype) pairs found in the layout."""
function layer_datatypes(boxes::Vector{BoxElem})
    pairs = Set{Tuple{Int,Int}}()
    for b in boxes
        push!(pairs, (b.layer, b.datatype))
    end
    return sort!(collect(pairs))
end

"""Filter boxes by layer and optionally datatype."""
function filter_boxes(boxes::Vector{BoxElem}; layer::Union{Int,Nothing}=nothing,
                      datatype::Union{Int,Nothing}=nothing,
                      cell::Union{String,Nothing}=nothing)
    filter(boxes) do b
        (layer === nothing || b.layer == layer) &&
        (datatype === nothing || b.datatype == datatype) &&
        (cell === nothing || b.cell == cell)
    end
end

end # module
