# MiniJSON.jl -- dependency-free JSON writer (dashboard API only needs output).
module MiniJSON
export tojson

tojson(x) = (io = IOBuffer(); w(io, x); String(take!(io)))

function w(io, s::AbstractString)
    write(io, '"')
    for c in s
        if c == '"';      write(io, "\\\"")
        elseif c == '\\'; write(io, "\\\\")
        elseif c == '\n'; write(io, "\\n")
        elseif c == '\r'; write(io, "\\r")
        elseif c == '\t'; write(io, "\\t")
        elseif c < ' ';   print(io, "\\u", string(UInt16(c), base = 16, pad = 4))
        else              write(io, c)
        end
    end
    write(io, '"')
end
w(io, s::Symbol) = w(io, String(s))
w(io, x::Bool) = print(io, x ? "true" : "false")
w(io, x::Integer) = print(io, x)
w(io, x::AbstractFloat) = isfinite(x) ? print(io, Float64(round(x, sigdigits = 6))) : print(io, "null")
w(io, ::Nothing) = print(io, "null")
function w(io, d::Union{AbstractDict,NamedTuple})
    write(io, '{'); first = true
    for (k, v) in pairs(d)
        first || write(io, ','); first = false
        w(io, string(k)); write(io, ':'); w(io, v)
    end
    write(io, '}')
end
function w(io, v::Union{AbstractVector,Tuple,AbstractSet})
    write(io, '['); first = true
    for x in v
        first || write(io, ','); first = false
        w(io, x)
    end
    write(io, ']')
end
w(io, x) = w(io, string(x))

end # module
