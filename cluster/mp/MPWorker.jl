# MPWorker.jl -- runs inside every Distributed.jl worker (shipped by mpcoord.jl over the
# Distributed connection itself; nothing is copied to the node).
#
# Each worker supervises one long-lived MAGPARSER RPC server:
#   MAGPARSER.exe --rpc-serve 127.0.0.1:<port>      (MAGPARSER_RPC_TOKEN in env)
# MAGPARSER dials back to this worker on loopback; requests carry an op name, up to 8
# integer parameters and CompactLayer "CLW1" blobs, and the reply carries one CLW1 blob.
# Layer bytes reach this worker as Vector{UInt8} arguments of Distributed remotecalls, so
# data moves master -> worker -> MAGPARSER entirely in memory.
#
# If MAGPARSER dies (crash, error stop), the call throws with the tail of its log and the
# next call starts a fresh server. The server's scratch dir (its cwd) is always removed.
module MPWorker

using Sockets, Random

mutable struct Server
    proc::Any
    sock::Any
    workdir::String
    version::String
end

const CFG = Dict{String,Any}()
const SRV = Ref{Union{Nothing,Server}}(nothing)
const LK = ReentrantLock()
const STATS = Dict{String,Any}("calls" => 0, "restarts" => 0, "busy" => "", "busy_since" => 0.0)

readexact(io, n) = (b = read(io, n); length(b) == n || throw(EOFError()); b)
rd32(io) = ltoh(reinterpret(Int32, readexact(io, 4))[1])
rd64(io) = ltoh(reinterpret(Int64, readexact(io, 8))[1])

function logtail(s::Union{Nothing,Server}, n = 1200)
    s === nothing && return ""
    f = joinpath(s.workdir, "magparser.log")
    isfile(f) || return ""
    t = read(f, String)
    length(t) > n ? "..." * last(t, n) : t
end

function stop!()
    s = SRV[]; SRV[] = nothing
    s === nothing && return
    try
        s.sock !== nothing && isopen(s.sock) && (send_request(s.sock, "QUIT", Int64[], Vector{UInt8}[]); read_reply(s.sock))
    catch
    end
    try close(s.sock) catch end
    t = time()
    while process_running(s.proc) && time() - t < 2; sleep(0.05); end
    process_running(s.proc) && try kill(s.proc) catch end
    rm(s.workdir; recursive = true, force = true)
end

function start!()
    stop!()
    exe = CFG["magparser"]
    srv = listen(IPv4(127, 0, 0, 1), 0)
    port = Int(getsockname(srv)[2])
    tok = randstring(RandomDevice(), 24)
    wd = mktempdir(CFG["tmp"]; prefix = "vlsi_mp_$(CFG["run"])_", cleanup = false)
    env = Dict{String,String}("MAGPARSER_RPC_TOKEN" => tok)
    for (k, v) in CFG["env"]; env[string(k)] = string(v); end
    cmd = addenv(Cmd(`$exe --rpc-serve 127.0.0.1:$port`; dir = wd), env)
    logf = joinpath(wd, "magparser.log")
    proc = run(pipeline(ignorestatus(cmd); stdout = logf, stderr = logf); wait = false)
    s = Server(proc, nothing, wd, "")
    SRV[] = s
    acc = @async try accept(srv) catch; nothing end
    timedwait(() -> istaskdone(acc) || !process_running(proc), 60.0; pollint = 0.1)
    sock = istaskdone(acc) ? fetch(acc) : nothing
    close(srv)
    if sock === nothing
        why = process_running(proc) ? "did not connect back within 60 s" : "exited with code $(proc.exitcode) without connecting back"
        txt = logtail(s); stop!()
        error("MAGPARSER $why (does $exe support --rpc-serve?)\n" * txt)
    end
    s.sock = sock
    try
        String(readexact(sock, 4)) == "MPHI" || error("bad hello")
        rd32(sock) == 1 || error("RPC version mismatch")
        String(readexact(sock, rd32(sock))) == tok || error("token mismatch")
        send_request(sock, "PING", Int64[], Vector{UInt8}[])
        st, msg, _, _, _ = read_reply(sock)
        st == 0 || error("PING failed: $msg")
        s.version = msg
    catch e
        stop!()
        rethrow()
    end
    STATS["restarts"] += 1
    s
end

function send_request(sock, op::AbstractString, params::Vector{Int64}, layers::Vector{Vector{UInt8}})
    length(op) <= 32 || error("op name too long")
    length(params) <= 8 || error("at most 8 parameters")
    io = IOBuffer()
    write(io, "MPRQ"); write(io, htol(Int32(1)))
    opb = zeros(UInt8, 32); opb[1:length(op)] .= codeunits(op); write(io, opb)
    write(io, htol(Int32(length(params))))
    p = zeros(Int64, 8); p[1:length(params)] .= params; write(io, htol.(p))
    write(io, htol(Int32(length(layers))))
    for l in layers
        write(io, htol(Int64(length(l)))); write(io, l)
    end
    write(sock, take!(io)); flush(sock)
end

function read_reply(sock)
    String(readexact(sock, 4)) == "MPRS" || error("bad reply magic")
    st = rd32(sock); us = rd64(sock); nout = rd64(sock)
    msg = String(readexact(sock, rd32(sock)))
    blob = readexact(sock, rd64(sock))
    (Int(st), msg, Int(nout), us / 1e6, blob)
end

"Configure the worker and start its MAGPARSER server; returns node information."
function init(magparser::String, tmp::String, run::String, env::Dict)
    isfile(magparser) || error("MAGPARSER.exe not found at $magparser on $(gethostname())")
    CFG["magparser"] = magparser; CFG["tmp"] = tmp; CFG["run"] = run; CFG["env"] = env
    # remove scratch dirs of crashed runs (> 12 h old)
    for d in readdir(tmp; join = true)
        startswith(basename(d), "vlsi_mp_") && isdir(d) && time() - mtime(d) > 12 * 3600 &&
            rm(d; recursive = true, force = true)
    end
    s = lock(LK) do; start!(); end
    atexit(stop!)
    info()
end

function mem_avail()
    if Sys.islinux() && isfile("/proc/meminfo")
        for l in eachline("/proc/meminfo")
            startswith(l, "MemAvailable:") && return parse(Int, split(l)[2]) * 1024
        end
    end
    Int(Sys.free_memory())
end

info() = Dict("host" => gethostname(), "arch" => string(Sys.ARCH), "os" => string(Sys.KERNEL),
              "ncpu" => Sys.CPU_THREADS, "mem_total" => Int(Sys.total_memory()), "mem_avail" => mem_avail(),
              "julia" => string(VERSION), "magparser" => get(CFG, "magparser", ""),
              "version" => SRV[] === nothing ? "" : SRV[].version, "pid" => getpid())

"Liveness/load probe used by the coordinator's heartbeat."
status() = Dict("load1" => Sys.loadavg()[1], "mem_avail" => mem_avail(), "calls" => STATS["calls"],
                "restarts" => STATS["restarts"], "busy" => STATS["busy"],
                "busy_for" => STATS["busy"] == "" ? 0.0 : time() - STATS["busy_since"],
                "alive" => SRV[] !== nothing && process_running(SRV[].proc))

"Run one op on the local MAGPARSER; returns (status, msg, n_out, seconds, CLW1 blob)."
function call(op::String, params::Vector{Int64}, layers::Vector{Vector{UInt8}}, tag::String = op)
    lock(LK) do
        s = SRV[]
        (s === nothing || !process_running(s.proc)) && (s = start!())
        STATS["busy"] = tag; STATS["busy_since"] = time()
        try
            send_request(s.sock, op, params, layers)
            r = read_reply(s.sock)
            STATS["calls"] += 1
            r
        catch e
            tailtxt = logtail(s)
            stop!()
            error("MAGPARSER server on $(gethostname()) failed during $op: $(sprint(showerror, e))\n$tailtxt")
        finally
            STATS["busy"] = ""
        end
    end
end

end # module
