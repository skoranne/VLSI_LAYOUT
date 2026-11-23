#!/usr/bin/env julia
# mpcoord.jl -- MAGPARSER RPC cluster on Julia's Distributed.jl
#
#   julia mpcoord.jl --hosts mp_hosts.toml --dag dags/mp_example.dag [--http 8080]
#         [--http-bind 127.0.0.1] [--journal DIR] [--verify cross|none] [--linger S]
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

using Distributed, TOML, Printf, Dates, Random, SHA, Sockets

const HERE = @__DIR__
const MPWORKER_SRC = read(joinpath(HERE, "MPWorker.jl"), String)
include(joinpath(HERE, "CompactWire.jl"))
include_string(Main, MPWORKER_SRC, joinpath(HERE, "MPWorker.jl"))   # same module path as on workers
include(joinpath(HERE, "..", "dagref.jl"))
include(joinpath(HERE, "..", "MiniJSON.jl"))
include(joinpath(HERE, "..", "dashboard.jl"))
using .CompactWire, .DagRef, .MiniJSON, .Dashboard

const T0 = time()
const RUNID = Dates.format(now(), "yyyymmdd_HHMMSS") * "_" * string(getpid())
const MAX_ATTEMPTS = 3

# ---------------------------------------------------------------- options / config
function parse_opts(args)
    o = Dict("hosts" => "", "dag" => "", "http" => "", "http-bind" => "127.0.0.1", "journal" => "",
             "verify" => "cross", "linger" => "0")
    i = 1
    while i <= length(args)
        k = replace(args[i], r"^--" => "")
        (haskey(o, k) && i < length(args)) || error("bad option $(args[i])")
        o[k] = args[i+1]; i += 2
    end
    (isempty(o["hosts"]) || isempty(o["dag"])) && error("usage: mpcoord.jl --hosts FILE --dag FILE [options]")
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

function event(kind, msg)
    t = time() - T0
    lock(ST) do
        push!(EVENTS, (t, kind, msg)); length(EVENTS) > 300 && popfirst!(EVENTS)
    end
    @printf("[mp %7.1fs] %-9s %s\n", t, kind, msg); flush(stdout)
end

# ---------------------------------------------------------------- DAG
# NAME = gen seed=.. n=.. extent=.. maxsize=..      (layer made on the coordinator)
# NAME = OP in1 [in2] [k=v ...]                      (MAGPARSER op over RPC)
const NLAYERS = Dict("OR" => 2, "OR2" => 2, "AND" => 2, "NOT" => 2, "XOR" => 2, "BOOSTNOT" => 2,
                     "GROW" => 1, "SHRINK" => 1, "EXTENT" => 1, "COPY" => 1, "MERGE" => 1)
for t in ("INTERACT", "CUT", "TOUCH", "INSIDE", "OUTSIDE", "INSIDE_EDGE", "OUTSIDE_EDGE", "COINCIDENT_INSIDE_EDGE",
          "COINCIDENT_OUTSIDE_EDGE", "EQUAL", "DISJOINT", "MEET", "OVERLAPS", "COVERS", "COVEREDBY", "CONTAINS")
    NLAYERS[t] = 2; NLAYERS["NOT_" * t] = 2
end

function op_params(op, kv, where)
    g(k) = haskey(kv, k) ? parse(Int64, kv[k]) : error("$where: $op needs $k=")
    if op == "GROW" || (op == "SHRINK" && !haskey(kv, "d"))
        haskey(kv, "d") ? fill(parse(Int64, kv["d"]), 4) : [g("e"), g("n"), g("w"), g("s")]
    elseif op == "SHRINK"
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
        lock(ST) do; delete!(n.running, pid); pid in n.pids && push!(n.free, pid); end
        if st == 0
            if complete!(t, blob, n.name, aid, time() - t0, nout)
                n.done += 1
                event("done", @sprintf("%s %s on %s/pid %d: %d boxes, %d B CLW1 (%.2f B/box), MAGPARSER %.3fs, total %.2fs",
                      t.name, t.op, n.name, pid, nout, length(blob), length(blob) / max(nout, 1), secs, time() - t0))
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

function schedule_tick()
    now = time(); jobs = Tuple{Node,Int,MTask,String}[]; gens = MTask[]
    lock(ST) do
        for t in TASKS
            if t.state == :waiting && all(TASKS[TIDX[i]].state == :done for i in t.inputs)
                t.input_shas = [TASKS[TIDX[i]].result_sha for i in t.inputs]
                t.key = task_key(t)
                if haskey(JOURNAL, t.key)
                    sha = JOURNAL[t.key]; STORE[sha] = read(joinpath(JDIR, "blobs", sha))
                    t.result_sha = sha; t.n_out = wire_boxes(sha); t.state = :done; t.node = "journal"; t.duration = 0.0
                    event("journal", "$(t.name) restored")
                elseif t.op == "gen"
                    t.state = :running; t.node = "coordinator"; t.aid = randstring(8); push!(gens, t)
                else
                    t.state = :ready
                end
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
        st, msg, nout, _, again = remotecall_fetch(MPWorker.call, pid, t.op, t.params, blobs, "verify:" * t.name)
        st == 0 || error("recompute failed: $msg")
        st, msg, nx, _, _ = remotecall_fetch(MPWorker.call, pid, "XOR", Int64[], [STORE[t.result_sha], again], "verify:" * t.name)
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
function state_json()
    now = time()
    lock(ST) do
        counts = Dict(string(s) => count(t -> t.state == s, TASKS) for s in (:waiting, :ready, :running, :done, :failed))
        active = Set(n.name for n in values(NODES) if n.state in (:joining, :alive, :suspect))
        tojson(Dict(
            "run" => RUNID, "uptime" => now - T0, "finished" => FINISHED[], "verified" => VERIFIED[], "counts" => counts,
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
                                         (t.verified === nothing ? "" : t.verified ? " ✓" : " ✗") : "")
                        for t in TASKS],
            "events" => [Dict("t" => e[1], "kind" => e[2], "msg" => e[3]) for e in EVENTS]))
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
    parse_dag(O["dag"]); journal_load()
    web = Dashboard.serve(state_json, action; host = O["http-bind"], port = HTTP_PORT)
    event("start", "run $RUNID: $(length(TASKS)) tasks, julia $VERSION, dashboard http://$(O["http-bind"]):$HTTP_PORT/")
    @sync for (h, c) in HOSTS
        get(c, "autostart", true) && @async bring_up(h)
    end
    alive = [n.name for n in values(NODES) if n.state == :alive]
    isempty(alive) && error("no usable nodes; see the NOTE lines above")
    event("cluster", "active nodes: $(join(sort(alive), ", "))")
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
        todo = [t for t in TASKS if t.state == :done && t.op != "gen"]
        event("verify", "cross-checking $(length(todo)) results on other nodes (XOR must be empty)")
        @sync for t in todo; @async verify_task(t); end
        VERIFIED[] = all(t -> t.verified == true, todo) && all(t -> t.state == :done, TASKS)
        event("verify", VERIFIED[] ? "PASSED" : "FAILED")
    else
        VERIFIED[] = all(t -> t.state == :done, TASKS)
    end
    println("\n", rpad("task", 10), rpad("op", 13), rpad("node", 14), rpad("tries", 6), rpad("time", 8),
            rpad("boxes", 9), rpad("CLW1 B", 10), "verification")
    for t in TASKS
        sz = t.state == :done ? length(STORE[t.result_sha]) : 0
        println(rpad(t.name, 10), rpad(t.op, 13), rpad(t.node, 14), rpad(t.attempts, 6),
                rpad(t.duration === nothing ? "-" : @sprintf("%.2fs", t.duration), 8), rpad(t.n_out, 9), rpad(sz, 10),
                t.state != :done ? string(t.state, " ", t.note) : t.op == "gen" ? "-" :
                t.verified === nothing ? t.vnote : (t.verified ? "✓ " : "✗ ") * t.vnote)
    end
    println()
    for n in sort(collect(values(NODES)); by = x -> x.name)
        @printf("node %-12s %-8s %-9s done=%d failed=%d %s\n", n.name, get(n.info, "arch", "-"), n.state, n.done, n.failed,
                n.state == :rejected ? n.note : get(n.info, "version", ""))
    end
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
