# dagref.jl -- Julia reference implementation of the dummy DAG ops.
#
# Must stay bit-identical to dummy/dagop.F90. Used by
#   * the coordinator, to verify every result and as a last-resort local executor
#   * agents without a working Fortran compiler (fallback mode)
#   * agents' post-build self test (Fortran output must equal this)
#
# Box file format: stream of little-endian int32 (x1,y1,x2,y2) per box.
# area output:      3 little-endian int64 (area, count, checksum mod P).
module DagRef

export run_op, ARGSPEC, NINPUTS, describe

const P = Int64(2147483647)

# positional parameter order passed to dagop after the input files
const ARGSPEC = Dict{String,Vector{String}}(
    "gen"    => ["seed", "n", "extent", "maxsize"],
    "grow"   => ["d"],
    "shrink" => ["d"],
    "and"    => ["x1", "y1", "x2", "y2"],
    "or"     => String[],
    "area"   => String[],
)
const NINPUTS = Dict("gen" => 0, "grow" => 1, "shrink" => 1, "and" => 1, "or" => 2, "area" => 1)

function decode(b::AbstractVector{UInt8})
    length(b) % 16 == 0 || error("box blob length $(length(b)) not a multiple of 16")
    v = ltoh.(reinterpret(Int32, Vector{UInt8}(b)))
    reshape(Vector{Int32}(v), 4, :)
end
encode(m::AbstractMatrix{Int32}) = Vector{UInt8}(reinterpret(UInt8, htol.(vec(Matrix(m)))))

function gen(seed::Int64, n::Int64, extent::Int64, maxsize::Int64)
    (n >= 0 && extent > 0 && maxsize > 0 && extent + maxsize < typemax(Int32)) || error("bad gen params")
    s = mod(seed, P); s == 0 && (s = 1)
    out = Matrix{Int32}(undef, 4, n)
    for i in 1:n
        s = mod(s * 48271, P); x = mod(s, extent)
        s = mod(s * 48271, P); y = mod(s, extent)
        s = mod(s * 48271, P); w = 1 + mod(s, maxsize)
        s = mod(s * 48271, P); h = 1 + mod(s, maxsize)
        out[:, i] .= Int32.((x, y, x + w, y + h))
    end
    out
end

function keep_nonempty(a::Matrix{Int64})
    keep = [a[1, i] < a[3, i] && a[2, i] < a[4, i] for i in 1:size(a, 2)]
    Matrix{Int32}(a[:, keep])
end

function grow(a::Matrix{Int32}, d::Int64)
    b = Int64.(a)
    b[1, :] .-= d; b[2, :] .-= d; b[3, :] .+= d; b[4, :] .+= d
    keep_nonempty(b)
end

function clip(a::Matrix{Int32}, x1, y1, x2, y2)
    b = Int64.(a)
    b[1, :] .= max.(b[1, :], x1); b[2, :] .= max.(b[2, :], y1)
    b[3, :] .= min.(b[3, :], x2); b[4, :] .= min.(b[4, :], y2)
    keep_nonempty(b)
end

function area(a::Matrix{Int32})
    ar = Int64(0); cs = Int64(0)
    for i in 1:size(a, 2)
        x1, y1, x2, y2 = Int64.(a[:, i])
        ar += (x2 - x1) * (y2 - y1)
        cs = mod(cs + mod(31x1 + 17y1 + 13x2 + 7y2, P), P)
    end
    Vector{UInt8}(reinterpret(UInt8, htol.(Int64[ar, size(a, 2), cs])))
end

pint(p, k) = haskey(p, k) ? parse(Int64, p[k]) : error("missing parameter '$k'")

"Run one op. `params` maps names to strings, `inputs` are raw box blobs."
function run_op(op::AbstractString, params::AbstractDict, inputs::Vector{Vector{UInt8}})
    haskey(NINPUTS, op) || error("unknown op '$op'")
    length(inputs) == NINPUTS[op] || error("op $op expects $(NINPUTS[op]) inputs, got $(length(inputs))")
    if op == "gen"
        encode(gen(pint(params, "seed"), pint(params, "n"), pint(params, "extent"), pint(params, "maxsize")))
    elseif op == "grow"
        encode(grow(decode(inputs[1]), pint(params, "d")))
    elseif op == "shrink"
        encode(grow(decode(inputs[1]), -pint(params, "d")))
    elseif op == "and"
        encode(clip(decode(inputs[1]), pint(params, "x1"), pint(params, "y1"), pint(params, "x2"), pint(params, "y2")))
    elseif op == "or"
        vcat(inputs[1], inputs[2])
    else
        area(decode(inputs[1]))
    end
end

"Short human-readable summary of a result blob."
function describe(op::AbstractString, b::Vector{UInt8})
    if op == "area" && length(b) == 24
        v = ltoh.(reinterpret(Int64, copy(b)))
        "area=$(v[1]) n=$(v[2]) cs=$(v[3])"
    else
        "$(length(b) ÷ 16) boxes"
    end
end

end # module
