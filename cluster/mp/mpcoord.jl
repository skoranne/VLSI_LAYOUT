#!/usr/bin/env julia
# mpcoord.jl -- MAGPARSER RPC cluster on Julia's Distributed.jl
#
#   julia mpcoord.jl --hosts mp_hosts.toml --dag dags/mp_example.dag [--http 8080]
#         [--http-bind 127.0.0.1] [--journal DIR] [--verify cross|none] [--linger S]
#   julia mpcoord.jl --hosts mp_hosts.toml --ctr design.ctr      (CLUSTER mode, see CtrFront.jl)
#         [--tui auto|on|off] [--tui-file FILE] [--log FILE]       (ASCII dashboard, ../tui.jl)
#         [--tile-boxes N] [--tiles N]       (override the CTR vars cluster_tile_boxes / cluster_tiles)
#
# * Every [[host]] needs the full path of `julia` and of `MAGPARSER.exe` on that machine.
#   Hosts whose MAGPARSER is missing, whose MAGPARSER has no RPC server mode, whose Julia
#   version differs from the coordinator's (Distributed requires matching versions) or
#   that cannot be reached are reported and removed from the cluster for this run.
# * Workers are started with Distributed.addprocs over ssh (tunnelled). The worker code
#   (MPWorker.jl) is sent through the Distributed connection: no scp, no NFS, no files.
# * Layers move as CompactLayer network form (CLW1) inside remotecall arguments and
#   results; each worker hands them to a long-lived `MAGPARSER.exe --rpc-serve` process
#   over loopback. No CTR or BIN files are used anywhere.
# * Failure handling: heartbeats (remotecall), suspect/dead detection, requeue on worker
#   loss, retry on MAGPARSER crash, drain/kill/add at runtime, content-addressed journal.
# * Verification: every result is recomputed on a different node (another architecture
#   when available) and XOR(result, recomputed) must be empty.
# * Tiles: an OR/AND/NOT/XOR/BOOSTNOT/FRAMENOT/GROW/SIZE whose inputs hold more than
#   cluster_tile_boxes boxes (CTR var, default 10 million; --tile-boxes) is split into y-band
#   tiles exactly like MAGPARSER's own tile manager (band_exec.f90: TilePlan, TileExtract;
#   the node runs TILE_<op> = TileApply + TileClip), the tiles run on different nodes, and
#   the clipped tile results are concatenated (bands are disjoint, so no merge is needed).
#   Each tile is verified like any other task.
# * Accounting: every remote call logs the REMOTE RUNTIME (MAGPARSER time on the node and the
#   RPC round trip) and the NETWORK bytes sent/received; totals per node and per run at the end.

using Distributed, TOML, Printf, Dates, Random, SHA, Sockets

const HERE = @__DIR__
const MPWORKER_SRC = read(joinpath(HERE, "MPWorker.jl"), String)
include(joinpath(HERE, "CompactWire.jl"))
include_string(Main, MPWORKER_SRC, joinpath(HERE, "MPWorker.jl"))   # same module path as on workers
include(joinpath(HERE, "..", "dagref.jl"))
include(joinpath(HERE, "..", "MiniJSON.jl"))
include(joinpath(HERE, "..", "dashboard.jl"))
include(joinpath(HERE, "..", "tui.jl"))
using .CompactWire, .DagRef, .MiniJSON, .Dashboard, .AsciiDash

const T0 = time()
const RUNID = Dates.format(now(), "yyyymmdd_HHMMSS") * "_" * string(getpid())
const MAX_ATTEMPTS = 3

# ---------------------------------------------------------------- options / config
function parse_opts(args)
    o = Dict("hosts" => "", "dag" => "", "ctr" => "", "http" => "", "http-bind" => "127.0.0.1", "journal" => "",
             "verify" => "cross", "linger" => "0", "tui" => "auto", "tui-file" => "", "log" => "",
             "tile-boxes" => "", "tiles" => "")
    i = 1
    while i <= length(args)
        k = replace(args[i], r"^--" => "")
        (haskey(o, k) && i < length(args)) || error("bad option $(args[i])")
        o[k] = args[i+1]; i += 2
    end
    (isempty(o["hosts"]) || isempty(o["dag"]) == isempty(o["ctr"])) &&
        error("usage: mpcoord.jl --hosts FILE (--dag FILE | --ctr FILE) [options]")
    o["tui"] in ("auto", "on", "off") || error("--tui must be auto, on or off")
    o
end
const O = parse_opts(ARGS)
const HCFG = TOML.parsefile(O["hosts"])
const HOSTS = Dict{String,Dict{String,Any}}(h["name"] => h for h in get(HCFG, "host", Any[]))
const HTTP_PORT = parse(Int, isempty(O["http"]) ? string(get(get(HCFG, "coordinator", Dict()), "http", 8080)) : O["http"])

# ---------------------------------------------------------------- state
mutable struct Node
    name::String; cfg::Dict{String,Any}
    pids::Vector{Int}; free::Vector{Int}; running::Dict{Int,Tuple{String,Float64}}
    state::Symbol; draining::Bool; inc::Int
    info::Dict{String,Any}; load1::Float64; mem_avail::Int; last_hb::Float64
    done::Int; failed::Int; note::String
end
Node(name, cfg) = Node(name, cfg, Int[], Int[], Dict{Int,Tuple{String,Float64}}(), :joining, false, 0,
                       Dict{String,Any}(), 0.0, 0, time(), 0, 0, "")

mutable struct MTask
    name::String; op::String; inputs::Vector{String}; params::Vector{Int64}; pstr::String
    state::Symbol; node::String; pid::Int; attempts::Int; aid::String
    key::String; input_shas::Vector{String}; result_sha::String; n_out::Int
    started::Float64; duration::Union{Nothing,Float64}; not_before::Float64
    note::String; verified::Union{Nothing,Bool}; vnote::String
end

const ST = ReentrantLock()
const NODES = Dict{String,Node}()
const TASKS = MTask[]
const TIDX = Dict{String,Int}()
const STORE = Dict{String,Vector{UInt8}}()
const JOURNAL = Dict{String,String}()
const EVENTS = Tuple{Float64,String,String}[]
const FINISHED = Ref(false)
const VERIFIED = Ref{Union{Nothing,Bool}}(nothing)
const INC = Ref(0)
const STOPPING = Ref(false)
const TUI_STOP = Ref(false)
const CTR_MODE = !isempty(O["ctr"])
const TILES = Dict{String,Vector{String}}()        # tiled task => its tile task names
# accounting: per node and per kind ("work" | "verify"): calls, MAGPARSER secs on the node,
# RPC round-trip secs, bytes sent to the node, bytes received from it
const ACCT = Dict{Tuple{String,String},Vector{Float64}}()
const TSTAT = Dict{String,NTuple{4,Float64}}()     # task => (remote secs, rpc secs, sent, recv)
function account!(node, kind, secs, rpc, sent, recv)
    lock(ST) do
        a = get!(ACCT, (node, kind), zeros(5))
        a .+= (1, secs, rpc, sent, recv)
    end
end
fmtb(b) = b < 1024 ? @sprintf("%d B", b) : b < 1024^2 ? @sprintf("%.1f KiB", b / 1024) :
          b < 1024^3 ? @sprintf("%.2f MiB", b / 1024^2) : @sprintf("%.2f GiB", b / 1024^3)
# ASCII dashboard: redrawn in place on a terminal; the event log then goes only to --log
const TUI_TTY = O["tui"] == "on" || (O["tui"] == "auto" && stdout isa Base.TTY)
const LOGIO = isempty(O["log"]) ? nothing : open(O["log"], "w")

function event(kind, msg)
    t = time() - T0
    lock(ST) do
        push!(EVENTS, (t, kind, msg)); length(EVENTS) > 300 && popfirst!(EVENTS)
    end
    s = @sprintf("[mp %7.1fs] %-9s %s\n", t, kind, msg)
    TUI_TTY || (print(s); flush(stdout))
    LOGIO === nothing || (print(LOGIO, s); flush(LOGIO))
end

# ---------------------------------------------------------------- DAG
# NAME = gen seed=.. n=.. extent=.. maxsize=..      (layer made on the coordinator)
# NAME = OP in1 [in2] [k=v ...]                      (MAGPARSER op over RPC)
const NLAYERS = Dict("OR" => 2, "OR2" => 2, "AND" => 2, "AND2" => 2, "NOT" => 2, "FRAMENOT" => 2, "XOR" => 2,
                     "BOOSTNOT" => 2, "SIZE2" => 2, "GROW" => 1, "SHRINK" => 1, "SIZE" => 1, "EXTENT" => 1,
                     "COPY" => 1, "MERGE" => 1, "GRID" => 1, "ASSERT_ZERO" => 1)
# ops that never go to a node: layers made/read on the coordinator, and the ASSERT_ZERO check
const LOCAL_OPS = ("gen", "src", "ASSERT_ZERO")
for t in ("INTERACT", "CUT", "TOUCH", "INSIDE", "OUTSIDE", "INSIDE_EDGE", "OUTSIDE_EDGE", "COINCIDENT_INSIDE_EDGE",
          "COINCIDENT_OUTSIDE_EDGE", "EQUAL", "DISJOINT", "MEET", "OVERLAPS", "COVERS", "COVEREDBY", "CONTAINS")
    NLAYERS[t] = 2; NLAYERS["NOT_" * t] = 2
end

function op_params(op, kv, where)
    g(k) = haskey(kv, k) ? parse(Int64, kv[k]) : error("$where: $op needs $k=")
    if op == "GROW" || (op == "SHRINK" && !haskey(kv, "d"))
        haskey(kv, "d") ? fill(parse(Int64, kv["d"]), 4) : [g("e"), g("n"), g("w"), g("s")]
    elseif op == "SHRINK" || op == "SIZE" || op == "SIZE2"
        [g("d")]
    else
        Int64[]
    end
end

function parse_dag(path)
    for (ln, raw) in enumerate(eachline(path))
        line = strip(split(raw, '#')[1]); isempty(line) && continue
        m = match(r"^(\w+)\s*=\s*(\w+)\s*(.*)$", line)
        m === nothing && error("$path:$ln: expected 'NAME = OP inputs... k=v...'")
        name, op, rest = String.(m.captures)
        haskey(TIDX, name) && error("$path:$ln: $name defined twice")
        inputs = String[]; kv = Dict{String,String}()
        for tok in split(rest)
            if occursin('=', tok)
                k, v = split(tok, '='; limit = 2); kv[k] = v
            else
                haskey(TIDX, tok) || error("$path:$ln: input $tok not defined above")
                push!(inputs, tok)
            end
        end
        if op == "gen"
            isempty(inputs) || error("$path:$ln: gen takes no inputs")
            params = Int64[parse(Int64, kv[k]) for k in ("seed", "n", "extent", "maxsize")]
        else
            op = uppercase(op)
            haskey(NLAYERS, op) || error("$path:$ln: unknown MAGPARSER op $op")
            length(inputs) == NLAYERS[op] || error("$path:$ln: $op takes $(NLAYERS[op]) layer(s)")
            params = op_params(op, kv, "$path:$ln")
        end
        push!(TASKS, MTask(name, op, inputs, params, join(["$k=$v" for (k, v) in kv], " "), :waiting, "", 0, 0, "",
                           "", String[], "", 0, 0.0, nothing, 0.0, "", nothing, ""))
        TIDX[name] = length(TASKS)
    end
end

include(joinpath(HERE, "CtrFront.jl"))

task_key(t) = bytes2hex(sha256(join([t.op; string.(t.params); t.input_shas], "\n")))
const JDIR = O["journal"]
function journal_load()
    isempty(JDIR) && return
    mkpath(joinpath(JDIR, "blobs"))
    f = joinpath(JDIR, "journal.log"); isfile(f) || return
    for l in eachline(f)
        p = split(l); length(p) == 2 && isfile(joinpath(JDIR, "blobs", p[2])) && (JOURNAL[p[1]] = p[2])
    end
    event("journal", "loaded $(length(JOURNAL)) results from $JDIR")
end
function journal_add(key, sha, blob)
    isempty(JDIR) && return
    bf = joinpath(JDIR, "blobs", sha)
    isfile(bf) || (write(bf * ".tmp", blob); mv(bf * ".tmp", bf; force = true))
    open(io -> println(io, key, " ", sha), joinpath(JDIR, "journal.log"), "a")
end

# ---------------------------------------------------------------- node bring-up
shq(s) = "'" * replace(string(s), "'" => "'\\''") * "'"
islocal(h) = get(h, "ssh", "local") in ("local", "")
rcmd(h, script) = islocal(h) ? `bash -c $script` :
    `ssh -o BatchMode=yes -o ConnectTimeout=10 $(h["ssh"]) $script`
sshflags() = `-o BatchMode=yes -o ConnectTimeout=10 -o ServerAliveInterval=5`

"Reject a node for this run, telling the user why."
function reject(n::Node, why)
    lock(ST) do; n.state = :rejected; n.note = why; end
    event("removed", "$(n.name): $why")
    println(stderr, "NOTE: node $(n.name) removed from the cluster for this run: $why"); flush(stderr)
end

function bring_up(name)
    h = HOSTS[name]
    n = lock(ST) do
        old = get(NODES, name, nothing)
        old !== nothing && old.state in (:joining, :alive, :suspect) && return nothing
        NODES[name] = Node(name, h)
    end
    n === nothing && return
    for k in ("julia", "magparser")
        if !haskey(h, k) || isempty(string(h[k]))
            return reject(n, "hosts file has no full path for `$k`")
        end
    end
    julia, mag, tmp = string(h["julia"]), string(h["magparser"]), string(get(h, "tmp", "/tmp"))
    # 1. reachability, MAGPARSER presence, Julia version (Distributed needs matching versions)
    out = try
        read(rcmd(h, "test -x $(shq(mag)) || { echo NOMAG; exit 0; }; test -d $(shq(tmp)) || { echo NOTMP; exit 0; }; " *
                     "$(shq(julia)) --version 2>&1 || echo NOJULIA"), String)
    catch e
        return reject(n, "cannot reach host over ssh ($(get(h, "ssh", "local"))): $(sprint(showerror, e))")
    end
    occursin("NOMAG", out) && return reject(n, "MAGPARSER.exe not found or not executable at $mag")
    occursin("NOTMP", out) && return reject(n, "tmp directory $tmp does not exist")
    m = match(r"julia version (\S+)", out)
    m === nothing && return reject(n, "Julia not runnable at $julia ($(strip(out)))")
    rv = VersionNumber(m.captures[1])
    if (rv.major, rv.minor) != (VERSION.major, VERSION.minor)
        return reject(n, "Julia $rv on the node but $VERSION on the coordinator; Distributed.jl needs the same " *
                         "version (e.g. `juliaup add $VERSION` there and point `julia` at it)")
    end
    # 2. Distributed workers (code travels over the Distributed connection, nothing is copied)
    nw = Int(get(h, "workers", 1))
    flags = `--startup-file=no --threads=1`
    t = @async islocal(h) ? addprocs(nw; exename = julia, exeflags = flags, dir = tmp, topology = :master_worker) :
                            addprocs([(string(h["ssh"]), nw)]; exename = julia, exeflags = flags, dir = tmp,
                                     tunnel = true, sshflags = sshflags(), topology = :master_worker)
    if timedwait(() -> istaskdone(t), 120.0) != :ok
        return reject(n, "Distributed.addprocs did not finish within 120 s")
    end
    pids = try fetch(t) catch e; return reject(n, "addprocs failed: $(first(sprint(showerror, e), 300))") end
    try
        Distributed.remotecall_eval(Main, pids, :(include_string(Main, $MPWORKER_SRC, "MPWorker.jl")))
        env = Dict{String,String}(string(k) => string(v) for (k, v) in get(h, "env", Dict()))
        infos = asyncmap(p -> remotecall_fetch(MPWorker.init, p, mag, tmp, RUNID, env), pids)
        lock(ST) do
            n.pids = pids; n.free = copy(pids); n.info = infos[1]; n.inc = (INC[] += 1)
            n.mem_avail = infos[1]["mem_avail"]; n.last_hb = time(); n.state = :alive
        end
        i = infos[1]
        event("join", @sprintf("%s: %s/%s %d cpu %.1f GB julia %s, %d worker(s) pids %s, %s", name, i["arch"], i["os"],
              i["ncpu"], i["mem_total"] / 2^30, i["julia"], length(pids), join(pids, ","), i["version"]))
    catch e
        msg = sprint(showerror, e isa RemoteException ? e.captured.ex : e)
        try rmprocs(pids; waitfor = 10) catch end
        why = occursin("connecting back", msg) || occursin("did not connect back", msg) ?
              "MAGPARSER at $mag has no working RPC server mode (rebuild it from this commit): $(first(msg, 200))" :
              "worker init failed: $(first(msg, 300))"
        reject(n, why)
    end
end

# ---------------------------------------------------------------- execution
function requeue!(t::MTask, why; backoff = true)
    t.state = :ready; t.node = ""; t.pid = 0; t.aid = ""
    t.not_before = backoff ? time() + min(0.5 * 2.0^t.attempts, 8.0) : 0.0
    t.note = why
end

"A worker vanished: drop it from its node, requeue its task, kill the node if no workers remain."
function worker_lost(n::Node, pid::Int, why)
    tasks = String[]
    lock(ST) do
        filter!(!=(pid), n.pids); filter!(!=(pid), n.free)
        if haskey(n.running, pid)
            tn = n.running[pid][1]; delete!(n.running, pid)
            t = TASKS[TIDX[tn]]
            t.state == :running && t.pid == pid && (requeue!(t, "requeued: worker $pid on $(n.name) lost"); push!(tasks, tn))
        end
        isempty(n.pids) && n.state in (:alive, :suspect, :joining) && (n.state = :dead)
    end
    event("lost", "$(n.name) worker $pid ($why)" * (isempty(tasks) ? "" : "; requeued $(join(tasks, ", "))"))
    n.state == :dead && event("dead", "$(n.name): no workers left")
end

function complete!(t::MTask, blob::Vector{UInt8}, who, aid, elapsed, nout)
    lock(ST) do
        (t.state == :running && t.aid == aid) || return false
        sha = bytes2hex(sha256(blob))
        STORE[sha] = blob; t.result_sha = sha; t.n_out = nout
        t.state = :done; t.node = who; t.duration = elapsed; t.note = ""
        journal_add(t.key, sha, blob)
        true
    end
end

function run_on(n::Node, pid::Int, t::MTask, aid::String)
    blobs = [STORE[s] for s in t.input_shas]
    t0 = time()
    try
        st, msg, nout, secs, blob = remotecall_fetch(MPWorker.call, pid, t.op, t.params, blobs, t.name)
        rpc = time() - t0; sent = sum(length, blobs; init = 0); recv = length(blob)
        lock(ST) do; delete!(n.running, pid); pid in n.pids && push!(n.free, pid); end
        account!(n.name, "work", secs, rpc, sent, recv)
        if st == 0
            if complete!(t, blob, n.name, aid, rpc, nout)
                n.done += 1
                lock(ST) do; TSTAT[t.name] = (secs, rpc, sent, recv); end
                event("done", @sprintf("%s %s on %s/pid %d: %d boxes (%.2f B/box) | REMOTE RUNTIME MAGPARSER %.3fs, RPC %.3fs | NETWORK sent %s, received %s",
                      t.name, t.op, n.name, pid, nout, length(blob) / max(nout, 1), secs, rpc, fmtb(sent), fmtb(recv)))
            end
        else
            n.failed += 1
            lock(ST) do; t.state = :failed; t.note = "MAGPARSER status $st: $msg"; end
            event("fail", "$(t.name) on $(n.name): MAGPARSER status $st: $msg")
        end
    catch e
        sleep(0.5)                                         # let Distributed deregister a dead worker
        if !(pid in workers())
            worker_lost(n, pid, "died during $(t.name)")
        else
            lock(ST) do
                delete!(n.running, pid); pid in n.pids && push!(n.free, pid)
                t.state == :running && t.aid == aid && requeue!(t, "attempt $(t.attempts) failed on $(n.name)")
            end
            n.failed += 1
            ex = e isa RemoteException ? e.captured.ex : e
            event("retry", "$(t.name) on $(n.name): $(first(sprint(showerror, ex), 400))")
        end
    end
end

wire_boxes(sha) = Int(CompactWire.wire_info(STORE[sha]).n)

# ---------------------------------------------------------------- tiles
const TILE_OPS = ("OR", "AND", "NOT", "FRAMENOT", "BOOSTNOT", "XOR", "GROW", "SIZE")
const HUGE_COORD = Int64(2)^40                     # beyond any int32 coordinate (band_exec K_HUGE_COORD)
tile_threshold() = isempty(O["tile-boxes"]) ? (CTR_MODE ? CTR_VARS["cluster_tile_boxes"] : 10_000_000) : parse(Int, O["tile-boxes"])
tile_count() = isempty(O["tiles"]) ? (CTR_MODE ? CTR_VARS["cluster_tiles"] : 0) : parse(Int, O["tiles"])
tile_halo(t) = t.op == "GROW" ? maximum(abs, t.params) : t.op == "SIZE" ? abs(t.params[1]) : 0
should_tile(t) = t.op in TILE_OPS && tile_threshold() > 0 &&
                 sum(wire_boxes(s) for s in t.input_shas; init = 0) > tile_threshold()
# band_exec.f90 tile_is_empty: a tile whose result is empty for this op
tile_empty(op, na, nb) = op in ("OR", "XOR") ? (na == 0 && nb == 0) : op == "AND" ? (na == 0 || nb == 0) : na == 0

"TilePlan (band_exec.f90): nt band edges at equal-count quantiles of the inputs' y centres."
function tile_plan(layers, nt)
    ntot = sum(size(b, 2) for b in layers)
    stride = max(1, ntot ÷ 1_000_000)               # band_exec samples one y per 64-box block
    ys = Int64[]
    for b in layers, i in 1:stride:size(b, 2)
        push!(ys, (Int64(b[2, i]) + Int64(b[4, i])) ÷ 2)
    end
    sort!(ys); ns = length(ys)
    edges = Vector{Int64}(undef, nt + 1)
    edges[1] = -HUGE_COORD; edges[end] = HUGE_COORD
    for k in 1:nt-1
        edges[k+1] = ns > 0 ? ys[clamp(k * ns ÷ nt, 1, ns)] : edges[k]
    end
    edges
end

"TileExtract (band_exec.f90): the boxes meeting y in [lo, hi] (closed), whole."
function tile_extract(b, lo, hi)
    keep = [Int64(b[4, i]) >= lo && Int64(b[2, i]) <= hi for i in 1:size(b, 2)]
    b[:, keep]
end

"Split task t into y-band tile tasks (runs on the coordinator, outside the state lock)."
function split_task!(t::MTask)
    ts = time()
    layers = [decode_layer(STORE[s]) for s in t.input_shas]
    ntot = sum(size(b, 2) for b in layers)
    slots = lock(ST) do; sum(length(n.pids) for n in values(NODES) if n.state == :alive; init = 0); end
    nt = tile_count() > 0 ? tile_count() : max(cld(ntot, tile_threshold()), slots, 2)
    edges = tile_plan(layers, nt); halo = tile_halo(t)
    p4 = vcat(t.params, zeros(Int64, 4 - length(t.params)))
    subs = MTask[]; sent = 0; nmax = 0
    for k in 1:nt
        lo, hi = edges[k], edges[k+1]
        hi <= lo && continue                          # empty band (repeated quantile)
        parts = [tile_extract(b, lo - halo, hi + halo) for b in layers]
        tile_empty(t.op, size(parts[1], 2), length(parts) > 1 ? size(parts[2], 2) : 0) && continue
        shas = String[]
        for b in parts
            blob = encode_layer(b); sha = bytes2hex(sha256(blob)); sent += length(blob)
            lock(ST) do; STORE[sha] = blob; end; push!(shas, sha)
        end
        nmax = max(nmax, sum(size(b, 2) for b in parts))
        st = MTask("$(t.name)#$k", "TILE_" * t.op, copy(t.inputs), vcat(Int64[lo, hi, 1], p4), "tile $k/$nt",
                   :ready, "", 0, 0, "", "", shas, "", 0, 0.0, nothing, 0.0, "", nothing, "")
        st.key = task_key(st); push!(subs, st)
    end
    lock(ST) do
        for st in subs
            if haskey(JOURNAL, st.key)
                sha = JOURNAL[st.key]; STORE[sha] = read(joinpath(JDIR, "blobs", sha))
                st.result_sha = sha; st.n_out = wire_boxes(sha); st.state = :done; st.node = "journal"; st.duration = 0.0
            end
            push!(TASKS, st); TIDX[st.name] = length(TASKS)
        end
        TILES[t.name] = [st.name for st in subs]
        t.state = :tiled; t.started = ts; t.note = "$(length(subs)) tiles"
    end
    event("tile", @sprintf("%s %s: %d input boxes > cluster_tile_boxes %d -> %d tiles (%d non-empty), <= %d boxes/tile, halo %d DBU; split %.2fs, %s of tile inputs",
          t.name, t.op, ntot, tile_threshold(), nt, length(subs), nmax, halo, time() - ts, fmtb(sent)))
end

"All tiles of t are done: concatenate their clipped results into t's result."
function join_task!(t::MTask)
    names = TILES[t.name]
    subs = [TASKS[TIDX[x]] for x in names]
    parts = [decode_layer(STORE[st.result_sha]) for st in subs]
    boxes = isempty(parts) ? Matrix{Int32}(undef, 4, 0) : reduce(hcat, parts)
    blob = encode_layer(boxes)
    ok = complete!(t, blob, "tiles×$(length(subs))", t.aid, time() - t.started, size(boxes, 2))
    st4 = [get(TSTAT, x, (0.0, 0.0, 0.0, 0.0)) for x in names]
    nodes = sort(unique(st.node for st in subs))
    ok && event("join", @sprintf("%s %s: %d tiles on %s -> %d boxes in %.2fs | REMOTE RUNTIME MAGPARSER sum %.3fs (max tile %.3fs), RPC sum %.3fs | NETWORK sent %s, received %s",
                t.name, t.op, length(subs), join(nodes, ","), size(boxes, 2), time() - t.started,
                sum(first, st4; init = 0.0), maximum(first, st4; init = 0.0), sum(x -> x[2], st4; init = 0.0),
                fmtb(sum(x -> x[3], st4; init = 0.0)), fmtb(sum(x -> x[4], st4; init = 0.0))))
end

function schedule_tick()
    now = time(); jobs = Tuple{Node,Int,MTask,String}[]; gens = MTask[]
    splits = MTask[]; joins = MTask[]; asserts = MTask[]
    lock(ST) do
        for t in TASKS
            if t.state == :waiting && all(TASKS[TIDX[i]].state == :done for i in t.inputs)
                t.input_shas = [TASKS[TIDX[i]].result_sha for i in t.inputs]
                t.key = task_key(t)
                if haskey(JOURNAL, t.key)
                    sha = JOURNAL[t.key]; STORE[sha] = read(joinpath(JDIR, "blobs", sha))
                    t.result_sha = sha; t.n_out = wire_boxes(sha); t.state = :done; t.node = "journal"; t.duration = 0.0
                    event("journal", "$(t.name) restored")
                elseif t.op == "gen" || t.op == "ASSERT_ZERO"
                    t.state = :running; t.node = "coordinator"; t.aid = randstring(8)
                    push!(t.op == "gen" ? gens : asserts, t)
                elseif should_tile(t)
                    t.state = :splitting; t.node = "coordinator"; push!(splits, t)
                else
                    t.state = :ready
                end
            end
        end
        for (pn, names) in TILES
            t = TASKS[TIDX[pn]]
            t.state == :tiled || continue
            sts = [TASKS[TIDX[x]].state for x in names]
            if any(==(:failed), sts)
                t.state = :failed; t.note = "a tile failed"; event("fail", "$(t.name): a tile failed")
            elseif all(==(:done), sts)
                t.state = :running; t.aid = randstring(8); push!(joins, t)
            end
        end
        for t in TASKS
            (t.state == :ready && t.not_before <= now) || continue
            if t.attempts >= MAX_ATTEMPTS
                t.state = :failed; t.note = "gave up after $MAX_ATTEMPTS attempts: $(t.note)"
                event("fail", "$(t.name): $(t.note)"); continue
            end
            need = 64 * sum(wire_boxes(s) for s in t.input_shas; init = 0) + 256 * 2^20
            best = nothing; score = (-Inf, -Inf)
            for n in values(NODES)
                (n.state == :alive && !n.draining && !isempty(n.free)) || continue
                n.mem_avail - Int(get(n.cfg, "reserve_mb", 1024)) * 2^20 >= need || continue
                s = (length(n.free) / length(n.pids), -n.load1 / max(get(n.info, "ncpu", 1), 1))
                s > score && (best = n; score = s)
            end
            best === nothing && continue
            pid = pop!(best.free); aid = randstring(8)
            t.state = :running; t.node = best.name; t.pid = pid; t.aid = aid; t.attempts += 1; t.started = now
            best.running[pid] = (t.name, now)
            push!(jobs, (best, pid, t, aid))
        end
        FINISHED[] = all(t -> t.state in (:done, :failed), TASKS)
    end
    for t in gens
        p = t.params
        boxes = DagRef.gen(p[1], p[2], p[3], p[4])
        blob = encode_layer(boxes)
        complete!(t, blob, "coordinator", t.aid, 0.0, Int(CompactWire.wire_info(blob).n))
        event("gen", @sprintf("%s: %d boxes -> %d B CLW1 (%.2f B/box)", t.name, p[2], length(blob), length(blob) / p[2]))
    end
    for t in splits
        try split_task!(t) catch e
            lock(ST) do; t.state = :failed; t.note = "tile split failed"; end
            event("fail", "$(t.name): tile split failed: $(sprint(showerror, e))")
        end
    end
    for t in joins; join_task!(t); end
    for t in asserts
        n = wire_boxes(t.input_shas[1]); bad = n > 0
        complete!(t, encode_layer(Matrix{Int32}(undef, 4, 0)), "coordinator", t.aid, 0.0, 0)
        event("assert", bad ? "************ ASSERT_ZERO fail: $(t.inputs[1]) has $n boxes ($(t.name))" :
                              "ASSERT_ZERO ok: $(t.inputs[1]) is empty")
        bad && CTR_MODE && CTR_VARS["abort_on_assert_zero"] > 1 &&
            lock(ST) do; t.state = :failed; t.note = "ASSERT_ZERO: $n boxes"; end
    end
    for (n, pid, t, aid) in jobs
        event("dispatch", "$(t.name) ($(t.op)) → $(n.name) pid $pid attempt $(t.attempts)")
        @async run_on(n, pid, t, aid)
    end
end

function monitor_tick()
    now = time()
    for n in collect(values(NODES))
        n.state in (:alive, :suspect) || continue
        for pid in copy(n.pids)                               # Distributed noticed a dead worker
            pid in workers() || worker_lost(n, pid, "worker process gone")
        end
        isempty(n.pids) && continue
        age = now - n.last_hb
        if age > 20
            event("dead", "$(n.name): no heartbeat for $(round(age, digits = 1))s, removing its workers")
            pids = copy(n.pids)
            for p in pids; worker_lost(n, p, "heartbeat timeout"); end
            @async try rmprocs(pids; waitfor = 5) catch end
            continue
        elseif age > 8 && n.state == :alive
            n.state = :suspect; event("suspect", "$(n.name): no heartbeat for $(round(age, digits = 1))s")
        end
        if n.draining && isempty(n.running)
            pids = copy(n.pids); n.pids = Int[]; n.free = Int[]; n.state = :left
            event("drain", "$(n.name) drained, removing workers $(join(pids, ","))")
            @async try rmprocs(pids; waitfor = 10) catch end
            continue
        end
        pid = n.pids[1]
        @async try
            s = remotecall_fetch(MPWorker.status, pid)
            lock(ST) do
                n.last_hb = time(); n.load1 = s["load1"]; n.mem_avail = s["mem_avail"]
                n.state == :suspect && (n.state = :alive; event("alive", "$(n.name) heartbeat resumed"))
            end
        catch
        end
    end
end

# ---------------------------------------------------------------- verification
"Wait for a free worker on node n (or any alive node when n === nothing)."
function acquire(pred)
    while true
        r = lock(ST) do
            for n in values(NODES)
                (n.state == :alive && !isempty(n.free) && pred(n)) || continue
                return (n, pop!(n.free))
            end
            nothing
        end
        r !== nothing && return r
        any(n -> n.state == :alive && pred(n), values(NODES)) || return nothing
        sleep(0.05)
    end
end
release(n, pid) = lock(ST) do; pid in n.pids && push!(n.free, pid); end

function verify_task(t::MTask)
    prod = get(NODES, t.node, nothing)
    parch = prod === nothing ? "" : get(prod.info, "arch", "")
    # prefer another architecture, then another node, then any worker
    choice = nothing
    for pred in (n -> n.name != t.node && get(n.info, "arch", "") != parch, n -> n.name != t.node, n -> true)
        any(n -> n.state == :alive && pred(n), values(NODES)) || continue
        choice = acquire(pred); choice !== nothing && break
    end
    choice === nothing && (t.verified = nothing; t.vnote = "no node available"; return)
    n, pid = choice
    try
        blobs = [STORE[s] for s in t.input_shas]
        t0 = time()
        st, msg, nout, s1, again = remotecall_fetch(MPWorker.call, pid, t.op, t.params, blobs, "verify:" * t.name)
        account!(n.name, "verify", s1, time() - t0, sum(length, blobs; init = 0), length(again))
        st == 0 || error("recompute failed: $msg")
        t0 = time()
        st, msg, nx, s2, xb = remotecall_fetch(MPWorker.call, pid, "XOR", Int64[], [STORE[t.result_sha], again], "verify:" * t.name)
        account!(n.name, "verify", s2, time() - t0, length(STORE[t.result_sha]) + length(again), length(xb))
        st == 0 || error("XOR failed: $msg")
        t.verified = nx == 0
        same = again == STORE[t.result_sha] ? "byte-identical" : "same region, different boxes"
        t.vnote = nx == 0 ? "on $(n.name): $same" : "on $(n.name): XOR has $nx boxes"
        nx == 0 || event("verify", "MISMATCH $(t.name): $(t.node) vs $(n.name), XOR = $nx boxes")
    catch e
        t.verified = false; t.vnote = "verify error: $(first(sprint(showerror, e), 200))"
    finally
        release(n, pid)
    end
end

# ---------------------------------------------------------------- dashboard glue
"One line saying where the job runs, e.g. 'CLUSTER MODE: running distributed on 3 nodes: ...'."
function banner()
    al = sort([n for n in values(NODES) if n.state in (:alive, :suspect)]; by = x -> x.name)
    archs = join(["$(n.name) ($(get(n.info, "arch", "?")), $(length(n.pids)) worker$(length(n.pids) == 1 ? "" : "s"))" for n in al], ", ")
    (CTR_MODE ? "CLUSTER MODE: " : "") * "running distributed on $(length(al)) node$(length(al) == 1 ? "" : "s"): $archs"
end

state_json() = tojson(state_dict())

function state_dict()
    now = time()
    lock(ST) do
        counts = Dict(string(s) => count(t -> t.state == s, TASKS) for s in (:waiting, :ready, :running, :done, :failed, :tiled))
        active = Set(n.name for n in values(NODES) if n.state in (:joining, :alive, :suspect))
        Dict(
            "run" => RUNID, "banner" => banner(), "uptime" => now - T0, "finished" => FINISHED[], "verified" => VERIFIED[], "counts" => counts,
            "nodes" => [Dict("name" => n.name, "arch" => get(n.info, "arch", "?"), "os" => get(n.info, "os", "?"),
                             "julia" => get(n.info, "julia", "?"), "ncpu" => get(n.info, "ncpu", 0),
                             "mem_total" => get(n.info, "mem_total", 0), "mem_avail" => n.mem_avail, "load1" => n.load1,
                             "max_jobs" => max(length(n.pids), 1), "state" => n.state == :rejected ? :dead : n.state,
                             "draining" => n.draining, "incarnation" => n.inc, "hb_age" => now - n.last_hb,
                             "done" => n.done, "failed" => n.failed,
                             "mode" => n.state == :rejected ? "removed: " * n.note : "MAGPARSER RPC",
                             "fc" => replace(get(n.info, "version", ""), "MAGPARSER-RPC 1 " => ""), "np" => length(n.pids),
                             "running" => [Dict("task" => "$(a) (pid $p)", "elapsed" => now - b) for (p, (a, b)) in n.running])
                        for n in sort(collect(values(NODES)); by = x -> x.name)],
            "spare" => sort([h for h in keys(HOSTS) if !(h in active)]),
            "tasks" => [Dict("name" => t.name, "op" => t.op * (isempty(t.pstr) ? "" : " " * t.pstr), "inputs" => t.inputs,
                             "state" => t.state, "node" => t.node * (t.pid > 0 && t.state == :running ? " pid $(t.pid)" : ""),
                             "attempts" => t.attempts, "duration" => t.duration,
                             "note" => isempty(t.vnote) ? t.note : t.vnote,
                             "result" => t.state == :done ? @sprintf("%d boxes, %d B CLW1", t.n_out, length(STORE[t.result_sha])) *
                                         (t.verified === nothing ? "" : t.verified ? " ✓" : " ✗") :
                                         t.state == :tiled ? "$(count(x -> TASKS[TIDX[x]].state == :done, TILES[t.name]))/$(length(TILES[t.name])) tiles done" : "")
                        for t in TASKS],
            "events" => [Dict("t" => e[1], "kind" => e[2], "msg" => e[3]) for e in EVENTS])
    end
end

"ASCII dashboard loop: redraw the terminal and/or append a timestamped frame to --tui-file."
function tui_loop()
    tf = isempty(O["tui-file"]) ? nothing : open(O["tui-file"], "w")
    (TUI_TTY || tf !== nothing) || return
    w = TUI_TTY ? max(100, displaysize(stdout)[2]) : 118
    try
        while true
            frame = AsciiDash.render(state_dict(); width = w)
            TUI_TTY && (print("\e[H\e[J", frame); flush(stdout))
            tf === nothing || (println(tf, "---- frame @ ", Dates.format(now(), "HH:MM:SS.s")); print(tf, frame); flush(tf))
            (STOPPING[] || TUI_STOP[]) && break
            sleep(1.0)
        end
    finally
        tf === nothing || close(tf)
    end
end

function action(a, q)
    if a in ("drain", "undrain", "kill")
        n = get(NODES, get(q, "node", ""), nothing)
        n === nothing && return "unknown node"
        if a == "kill"
            event("chaos", "killing all workers of $(n.name)")
            for p in copy(n.pids)
                @async try remote_do(() -> ccall(:_exit, Cvoid, (Cint,), 9), p) catch end
            end
        else
            lock(ST) do; n.draining = a == "drain"; end; event(a, n.name)
        end
        return "$a $(n.name): ok"
    elseif a == "add"
        h = get(q, "host", ""); haskey(HOSTS, h) || return "host $h is not in the hosts file"
        @async bring_up(h); return "bringing up $h"
    end
    "unknown action $a"
end

"REMOTE RUNTIME and NETWORK totals per node and for the run (to stdout and the log)."
function acct_report()
    tot = Dict(k => zeros(5) for k in ("work", "verify"))
    for name in sort(unique(first.(collect(keys(ACCT)))))
        for kind in ("work", "verify")
            a = get(ACCT, (name, kind), nothing); a === nothing && continue
            tot[kind] .+= a
            event("acct", @sprintf("%-12s %-6s %4d calls | REMOTE RUNTIME MAGPARSER %8.3fs, RPC %8.3fs | NETWORK sent %10s, received %10s",
                  name, kind, a[1], a[2], a[3], fmtb(a[4]), fmtb(a[5])))
        end
    end
    for kind in ("work", "verify")
        a = tot[kind]; a[1] > 0 || continue
        event("acct", @sprintf("TOTAL %-6s %4d calls | REMOTE RUNTIME MAGPARSER %.3fs, RPC %.3fs | NETWORK sent %s, received %s (%s)",
              kind, a[1], a[2], a[3], fmtb(a[4]), fmtb(a[5]), fmtb(a[4] + a[5])))
    end
end

# ---------------------------------------------------------------- main
function cleanup()
    STOPPING[] = true
    ws = filter(!=(1), workers())
    isempty(ws) || try rmprocs(ws; waitfor = 15) catch end        # workers stop MAGPARSER, remove scratch dirs
    for (name, h) in HOSTS                                       # belt and braces: this run's dirs only
        haskey(NODES, name) && NODES[name].state != :rejected || continue
        tmp = string(get(h, "tmp", "/tmp"))
        try run(rcmd(h, "rm -rf $(shq(tmp))/vlsi_mp_$(RUNID)_*")) catch end
    end
    event("cleanup", "workers removed, scratch dirs of run $RUNID deleted")
end

function main()
    nbool = 0
    CTR_MODE ? (nbool = parse_ctr(O["ctr"])) : parse_dag(O["dag"])
    journal_load()
    web = Dashboard.serve(state_json, action; host = O["http-bind"], port = HTTP_PORT)
    event("start", "run $RUNID: $(length(TASKS)) tasks, julia $VERSION, dashboard http://$(O["http-bind"]):$HTTP_PORT/")
    @sync for (h, c) in HOSTS
        get(c, "autostart", true) && @async bring_up(h)
    end
    alive = [n.name for n in values(NODES) if n.state == :alive]
    isempty(alive) && error("no usable nodes; see the NOTE lines above")
    event("cluster", "active nodes: $(join(sort(alive), ", "))")
    nops = count(t -> !(t.op in ("gen", "src")), TASKS)
    event("cluster", banner() * (CTR_MODE ? "; $nops ops ($nbool Boolean AND/OR/NOT/XOR) from $(basename(O["ctr"]))" : "; $nops ops"))
    tui = @async try tui_loop() catch e; event("error", "tui: $(sprint(showerror, e))") end
    # heartbeat clocks start now: a node that joined early must not look silent just because
    # a slower node (e.g. an 8 GB board) was still coming up before monitoring began
    lock(ST) do; for n in values(NODES); n.state == :alive && (n.last_hb = time()); end; end
    @async while !STOPPING[]
        try monitor_tick() catch e; event("error", "monitor: $(sprint(showerror, e))") end
        sleep(2.0)
    end
    t0 = time()
    while !FINISHED[]
        try schedule_tick() catch e; event("error", "scheduler: $(sprint(showerror, e, catch_backtrace()))") end
        any(n -> n.state in (:alive, :suspect, :joining), values(NODES)) || (event("error", "all nodes lost"); break)
        sleep(0.1)
    end
    event("finish", @sprintf("DAG finished in %.1fs", time() - t0))
    if O["verify"] == "cross"
        todo = [t for t in TASKS if t.state == :done && !(t.op in LOCAL_OPS) && !haskey(TILES, t.name)]
        event("verify", "cross-checking $(length(todo)) results on other nodes (XOR must be empty)" *
              (isempty(TILES) ? "" : "; a tiled op is verified through its tiles"))
        @sync for t in todo; @async verify_task(t); end
        for (pn, names) in TILES                      # a tiled op is the union of its verified tiles
            t = TASKS[TIDX[pn]]; t.state == :done || continue
            vs = [TASKS[TIDX[x]].verified for x in names]
            t.verified = all(==(true), vs); t.vnote = "$(count(==(true), vs))/$(length(vs)) tiles verified"
        end
        vt = [t for t in TASKS if t.state == :done && !(t.op in LOCAL_OPS)]
        VERIFIED[] = all(t -> t.verified == true, vt) && all(t -> t.state == :done, TASKS)
        event("verify", VERIFIED[] ? "PASSED" : "FAILED")
    else
        VERIFIED[] = all(t -> t.state == :done, TASKS)
    end
    CTR_MODE && write_ctr_outputs()
    TUI_TTY && (sleep(1.2); TUI_STOP[] = true; wait(tui); println())   # leave the final frame on screen
    println("\n", rpad("task", 16), rpad("op", 14), rpad("node", 14), rpad("tries", 6), rpad("time", 8),
            rpad("boxes", 9), rpad("CLW1 B", 10), "verification")
    for t in TASKS
        sz = t.state == :done ? length(STORE[t.result_sha]) : 0
        println(rpad(t.name, 16), rpad(t.op, 14), rpad(t.node, 14), rpad(t.attempts, 6),
                rpad(t.duration === nothing ? "-" : @sprintf("%.2fs", t.duration), 8), rpad(t.n_out, 9), rpad(sz, 10),
                t.state != :done ? string(t.state, " ", t.note) : t.op in LOCAL_OPS ? "-" :
                t.verified === nothing ? t.vnote : (t.verified ? "✓ " : "✗ ") * t.vnote)
    end
    println()
    for n in sort(collect(values(NODES)); by = x -> x.name)
        @printf("node %-12s %-8s %-9s done=%d failed=%d %s\n", n.name, get(n.info, "arch", "-"), n.state, n.done, n.failed,
                n.state == :rejected ? n.note : get(n.info, "version", ""))
    end
    acct_report()
    println(banner())
    linger = parse(Float64, O["linger"])
    linger > 0 && (event("linger", "dashboard stays up $(linger)s"); sleep(linger))
    close(web)
    VERIFIED[] == true
end

Base.exit_on_sigint(false)
ok = false
try
    global ok = main()
catch e
    e isa InterruptException ? event("stop", "interrupted") : event("error", sprint(showerror, e, catch_backtrace()))
finally
    cleanup()
end
exit(ok ? 0 : 1)
