# CompactWire.jl -- Julia codec for the CompactLayer network form "CLW1"
# (src/compact_wire.f90).  Boxes are a 4×n Matrix{Int32} (x1,y1,x2,y2 per column).
#
# encode_layer: boxes -> CLW1 cold form (narrow keys; falls back to raw form 2 when the
#               layer needs wide keys or has inverted boxes).  Mirrors CompactEncode +
#               CompactPack bit for bit: origin = min centre sums, grid = gcd, sorted
#               unique (w,h) shape table, key = morton(q) << sbits | shape, sorted,
#               duplicates removed, 128-key blocks with fixed-width deltas.
# decode_layer: CLW1 (hot/cold, narrow/wide, raw) -> boxes.
module CompactWire

export encode_layer, decode_layer, wire_info, empty_layer

const MAGIC = UInt8['C', 'L', 'W', '1']
const HDR = 96
const BLOCK = 128

function spread_bits(v::Int64)
    r = v & 0x00000000FFFFFFFF
    r = (r | (r << 16)) & 0x0000FFFF0000FFFF
    r = (r | (r << 8))  & 0x00FF00FF00FF00FF
    r = (r | (r << 4))  & 0x0F0F0F0F0F0F0F0F
    r = (r | (r << 2))  & 0x3333333333333333
    r = (r | (r << 1))  & 0x5555555555555555
    Int64(r)
end
function compact_bits(v::Int64)
    r = UInt64(reinterpret(UInt64, v)) & 0x5555555555555555
    r = (r | (r >> 1))  & 0x3333333333333333
    r = (r | (r >> 2))  & 0x0F0F0F0F0F0F0F0F
    r = (r | (r >> 4))  & 0x00FF00FF00FF00FF
    r = (r | (r >> 8))  & 0x0000FFFF0000FFFF
    r = (r | (r >> 16)) & 0x00000000FFFFFFFF
    Int64(r)
end
bitlen(v::Integer) = v <= 0 ? 0 : 64 - leading_zeros(Int64(v))

# ------------------------------------------------------------------ writer helpers
w32!(io, v) = write(io, htol(Int32(v)))
w64!(io, v) = write(io, htol(Int64(v)))

function header!(io, form, wide, n, n_input, ox, oy, grid, cbits, sbits, nshape, nblk, npacked, nsid)
    write(io, MAGIC); w32!(io, 1); w32!(io, form); w32!(io, wide ? 1 : 0)
    w64!(io, n); w64!(io, n_input); w64!(io, ox); w64!(io, oy); w64!(io, grid)
    w32!(io, cbits); w32!(io, sbits)
    w64!(io, nshape); w64!(io, nblk); w64!(io, npacked); w64!(io, nsid)
end

empty_layer() = (io = IOBuffer(); header!(io, 1, false, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0); take!(io))

function raw_layer(b::AbstractMatrix{Int32})
    io = IOBuffer()
    header!(io, 2, false, size(b, 2), size(b, 2), 0, 0, 1, 0, 0, 0, 0, 0, 0)
    write(io, htol.(vec(Matrix(b))))
    take!(io)
end

"Boxes (4×n Int32) -> CLW1 bytes (cold CompactLayer, or raw form if not encodable)."
function encode_layer(b::AbstractMatrix{Int32})
    n = size(b, 2)
    n == 0 && return empty_layer()
    x1 = Int64.(b[1, :]); y1 = Int64.(b[2, :]); x2 = Int64.(b[3, :]); y2 = Int64.(b[4, :])
    any(x2 .< x1) || any(y2 .< y1) ? (return raw_layer(b)) : nothing
    sx = x1 .+ x2; sy = y1 .+ y2
    ox = minimum(sx); oy = minimum(sy)
    g = Int64(0)
    for i in 1:n
        g = gcd(g, sx[i] - ox); g = gcd(g, sy[i] - oy)
    end
    g == 0 && (g = 1)
    qx = (sx .- ox) .÷ g; qy = (sy .- oy) .÷ g
    qmax = max(maximum(qx), maximum(qy))
    wh = ((x2 .- x1) .<< 32) .| (y2 .- y1)
    shapes = sort!(unique(wh))
    nshape = length(shapes)
    cbits = max(1, bitlen(qmax)); sbits = bitlen(nshape - 1)
    (2cbits + sbits > 62 || cbits > 31) && return raw_layer(b)      # would need wide keys
    sid = [searchsortedfirst(shapes, wh[i]) - 1 for i in 1:n]
    keys = [((spread_bits(qx[i]) | (spread_bits(qy[i]) << 1)) << sbits) | sid[i] for i in 1:n]
    sort!(keys); unique!(keys)
    nu = length(keys)
    # cold form: per block base key + fixed-width deltas, LSB-first bit stream
    nblk = cld(nu, BLOCK)
    base = Vector{Int64}(undef, nblk); width = Vector{UInt8}(undef, nblk); bitoff = zeros(Int64, nblk + 1)
    for k in 1:nblk
        i0 = (k - 1) * BLOCK + 1; i1 = min(nu, k * BLOCK)
        base[k] = keys[i0]
        dmax = Int64(0)
        for i in i0+1:i1; dmax = max(dmax, keys[i] - keys[i-1]); end
        width[k] = bitlen(dmax)
        bitoff[k+1] = bitoff[k] + Int64(width[k]) * (i1 - i0)
    end
    packed = zeros(UInt64, bitoff[end] ÷ 64 + 2)
    for k in 1:nblk
        w = Int(width[k]); w == 0 && continue
        i0 = (k - 1) * BLOCK + 1; i1 = min(nu, k * BLOCK); pos = bitoff[k]
        for i in i0+1:i1
            d = UInt64(keys[i] - keys[i-1]); word = pos ÷ 64 + 1; off = pos % 64
            packed[word] |= d << off
            off + w > 64 && (packed[word+1] |= d >> (64 - off))
            pos += w
        end
    end
    io = IOBuffer()
    header!(io, 1, false, nu, n, ox, oy, g, cbits, sbits, nshape, nblk, length(packed), 0)
    write(io, htol.(Int32.(shapes .>> 32))); write(io, htol.(Int32.(shapes .& 0xFFFFFFFF)))
    write(io, htol.(base)); write(io, htol.(bitoff)); write(io, width)
    write(io, zeros(UInt8, cld(nblk, 8) * 8 - nblk))
    write(io, htol.(packed))
    take!(io)
end

# ------------------------------------------------------------------ reader
struct Rd; b::Vector{UInt8}; end
r32(r, p) = ltoh(reinterpret(Int32, r.b[p+1:p+4])[1])
r64(r, p) = ltoh(reinterpret(Int64, r.b[p+1:p+8])[1])
ra(T, r, p, m) = m == 0 ? T[] : ltoh.(collect(reinterpret(T, r.b[p+1:p+sizeof(T)*m])))

"Header fields of a CLW1 buffer as a NamedTuple."
function wire_info(b::Vector{UInt8})
    length(b) >= HDR && b[1:4] == MAGIC || error("not a CLW1 buffer")
    r = Rd(b)
    (version = r32(r, 4), form = r32(r, 8), wide = (r32(r, 12) & 1) == 1, n = r64(r, 16), n_input = r64(r, 24),
     ox = r64(r, 32), oy = r64(r, 40), grid = r64(r, 48), cbits = r32(r, 56), sbits = r32(r, 60),
     nshape = r64(r, 64), nblk = r64(r, 72), npacked = r64(r, 80), nsid = r64(r, 88), bytes = length(b))
end

getfield_bits(buf::Vector{UInt64}, i::Int, w::Int) = begin
    w == 0 && return Int64(0)
    pos = (i - 1) * w; word = pos ÷ 64 + 1; off = pos % 64
    v = buf[word] >> off
    off + w > 64 && (v |= buf[word+1] << (64 - off))
    Int64(v & ((UInt64(1) << w) - 1))
end

"CLW1 bytes -> 4×n Matrix{Int32}."
function decode_layer(b::Vector{UInt8})
    h = wire_info(b); r = Rd(b); n = Int(h.n)
    h.version == 1 || error("unsupported CLW version $(h.version)")
    n == 0 && return Matrix{Int32}(undef, 4, 0)
    if h.form == 2
        return reshape(ra(Int32, r, HDR, 4n), 4, n)
    end
    p = HDR; ns = Int(h.nshape)
    sw = Int64.(ra(Int32, r, p, ns)); p += 4ns
    sh = Int64.(ra(Int32, r, p, ns)); p += 4ns
    keys = Vector{Int64}(undef, n); sid = nothing
    if h.form == 0
        keys .= ra(Int64, r, p, n); p += 8n
        h.wide && (sid = Int64.(ra(Int32, r, p, n)))
    elseif h.form == 1
        nb = Int(h.nblk)
        base = ra(Int64, r, p, nb); p += 8nb
        bitoff = ra(Int64, r, p, nb + 1); p += 8(nb + 1)
        width = b[p+1:p+nb]; p += cld(nb, 8) * 8
        packed = reinterpret(UInt64, ra(Int64, r, p, Int(h.npacked))); p += 8 * Int(h.npacked)
        if h.wide
            sp = collect(reinterpret(UInt64, ra(Int64, r, p, Int(h.nsid))))   # once, not per box
            sb = Int(h.sbits)
            sid = [getfield_bits(sp, i, sb) for i in 1:n]
        end
        packed = collect(packed)
        for k in 1:nb
            i0 = (k - 1) * BLOCK + 1; i1 = min(n, k * BLOCK); w = Int(width[k])
            mask = (UInt64(1) << w) - 1; pos = bitoff[k]
            keys[i0] = base[k]
            for i in i0+1:i1
                v = UInt64(0)
                if w > 0
                    word = pos ÷ 64 + 1; off = pos % 64
                    v = packed[word] >> off
                    off + w > 64 && (v |= packed[word+1] << (64 - off))
                    v &= mask
                end
                keys[i] = keys[i-1] + Int64(v); pos += w
            end
        end
    else
        error("unknown CLW form $(h.form)")
    end
    out = Matrix{Int32}(undef, 4, n)
    smask = (Int64(1) << h.sbits) - 1
    for i in 1:n
        if h.wide
            m = keys[i]; s = sid[i] + 1
        else
            m = keys[i] >> h.sbits; s = (keys[i] & smask) + 1
        end
        sx = compact_bits(m) * h.grid + h.ox; sy = compact_bits(m >> 1) * h.grid + h.oy
        w = sw[s]; hh = sh[s]
        x1 = (sx - w) ÷ 2; y1 = (sy - hh) ÷ 2
        out[1, i] = x1; out[2, i] = y1; out[3, i] = x1 + w; out[4, i] = y1 + hh
    end
    out
end

end # module
