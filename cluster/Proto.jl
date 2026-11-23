# Proto.jl -- wire protocol shared by coordinator and agents.
#
# Frame (all integers little-endian, independent of host arch / Julia version):
#   u32 magic | u8 type | u32 hdr_len | hdr bytes | u64 blob_len | blob bytes
# hdr is UTF-8 "key=value" lines; '\' and newline in values are escaped.
# No Julia Serialization is used, so x86_64 / aarch64 / macOS and different
# Julia versions interoperate.
module Proto

export Msg, MsgType, sendmsg, recvmsg, sha_hex
export REGISTER, WELCOME, HEARTBEAT, CODE, READY, TASK, RESULT, REJECT, DRAIN, SHUTDOWN, CRASH

using SHA

const MAGIC    = 0x43534C56          # "VLSC"
const MAX_HDR  = 1 << 20
const MAX_BLOB = Int64(4) << 30

@enum MsgType::UInt8 begin
    REGISTER  = 1   # agent -> coord : identity, capabilities, token
    WELCOME   = 2   # coord -> agent : incarnation, run id
    HEARTBEAT = 3   # both ways      : load / liveness
    CODE      = 4   # coord -> agent : worker source (blob) + sha
    READY     = 5   # agent -> coord : build finished, accepting tasks
    TASK      = 6   # coord -> agent : op, params, concatenated inputs
    RESULT    = 7   # agent -> coord : status, output blob
    REJECT    = 8   # agent -> coord : cannot run task (memory / busy)
    DRAIN     = 9   # coord -> agent : informational
    SHUTDOWN  = 10  # coord -> agent : exit cleanly and remove files
    CRASH     = 11  # coord -> agent : chaos testing, die without cleanup
end

struct Msg
    type::MsgType
    hdr::Dict{String,String}
    blob::Vector{UInt8}
end
Msg(t::MsgType, hdr=Dict{String,String}(), blob::Vector{UInt8}=UInt8[]) =
    Msg(t, Dict{String,String}(string(k) => string(v) for (k, v) in hdr), blob)

sha_hex(b::AbstractVector{UInt8}) = bytes2hex(sha256(b))

escv(s::AbstractString) = replace(s, "\\" => "\\\\", "\n" => "\\n", "\r" => "\\r")
function unescv(s::AbstractString)
    io = IOBuffer(); esc = false
    for c in s
        if esc
            write(io, c == 'n' ? '\n' : c == 'r' ? '\r' : c); esc = false
        elseif c == '\\'
            esc = true
        else
            write(io, c)
        end
    end
    String(take!(io))
end

function encode(m::Msg)
    hb = Vector{UInt8}(codeunits(join((k * "=" * escv(v) for (k, v) in m.hdr), "\n")))
    io = IOBuffer(sizehint = 17 + length(hb) + length(m.blob))
    write(io, htol(MAGIC)); write(io, UInt8(m.type))
    write(io, htol(UInt32(length(hb)))); write(io, hb)
    write(io, htol(UInt64(length(m.blob)))); write(io, m.blob)
    take!(io)
end

"Send one frame. `lk` serialises concurrent writers on the same socket."
function sendmsg(io::IO, lk::Base.AbstractLock, m::Msg)
    data = encode(m)
    lock(lk) do
        write(io, data); flush(io)
    end
    nothing
end

function readexact(io::IO, n::Integer)
    b = read(io, n)
    length(b) == n || throw(EOFError())
    b
end

"Receive one frame; throws on EOF or malformed input (caller closes the socket)."
function recvmsg(io::IO)
    magic = ltoh(read(io, UInt32))
    magic == MAGIC || error("bad magic 0x$(string(magic, base=16))")
    t = read(io, UInt8)
    (1 <= t <= 11) || error("bad message type $t")
    hl = ltoh(read(io, UInt32))
    hl <= MAX_HDR || error("header too large ($hl)")
    hs = String(readexact(io, hl))
    bl = ltoh(read(io, UInt64))
    bl <= MAX_BLOB || error("blob too large ($bl)")
    blob = bl == 0 ? UInt8[] : readexact(io, bl)
    hdr = Dict{String,String}()
    for line in split(hs, '\n'; keepempty = false)
        i = findfirst('=', line)
        i === nothing && error("malformed header line")
        hdr[line[1:prevind(line, i)]] = unescv(line[nextind(line, i):end])
    end
    Msg(MsgType(t), hdr, blob)
end

end # module
