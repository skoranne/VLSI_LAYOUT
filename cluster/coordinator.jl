#!/usr/bin/env julia
# coordinator.jl -- control plane of the VLSI compute-cluster prototype.
#
#   julia coordinator.jl --dag dags/example.dag [--hosts hosts.toml] [--port 7700]
#         [--bind 0.0.0.0] [--advertise ADDR] [--http 8080] [--http-bind 127.0.0.1]
#         [--journal DIR] [--token T] [--no-launch] [--linger S] [--cost-scale X]
#         [--local-fallback-after S] [--keep-remote]
#
# Responsibilities (Erlang/OTP-style supervisor written in plain Julia):
#   * launch agents over ssh (code shipped through the ssh pipe, no NFS) or
#     accept agents started by hand anywhere (they dial in with the token)
#   * membership: joining -> alive -> suspect -> dead, incarnation numbers,
#     drain / undrain / kill, runtime add of hosts from hosts.toml
#   * DAG scheduling onto least-loaded nodes, memory-aware, retries with
#     backoff, requeue on node death, local Julia fallback as last resort
#   * content-addressed results + append-only journal => restartable
#   * verification of every result against the Julia reference
#   * dashboard on http://127.0.0.1:8080
#   * cleanup of every remote directory it created

const HERE = @__DIR__
include(joinpath(HERE, "Proto.jl"))
include(joinpath(HERE, "MiniJSON.jl"))
include(joinpath(HERE, "dagref.jl"))
include(joinpath(HERE, "dashboard.jl"))
using .Proto, .MiniJSON, .DagRef, .Dashboard
using Sockets, TOML, SHA, Printf, Random, Dates

const HB_PERIOD = 2.0
const SUSPECT_AFTER = 3HB_PERIOD
const DEAD_AFTER = 5HB_PERIOD
const MAX_ATTEMPTS = 3
const T0 = time()

# ---------------------------------------------------------------- options
function parse_opts(args)
    o = Dict{String,String}("dag" => "", "hosts" => "", "port" => "", "bind" => "0.0.0.0", "advertise" => "",
        "http" => "", "http-bind" => "127.0.0.1", "journal" => "", "token" => "", "linger" => "0",
        "cost-scale" => "1.0", "local-fallback-after" => "30", "no-launch" => "0", "keep-remote" => "0")
    flags = Set(["no-launch", "keep-remote"])
    i = 1
    while i <= length(args)
        k = replace(args[i], r"^--" => "")
        if k in flags
            o[k] = "1"; i += 1
        elseif haskey(o, k) && i < length(args)
            o[k] = args[i+1]; i += 2
        else
            error("bad option $(args[i])")
        end
    end
    isempty(o["dag"]) && error("--dag FILE is required")
    o
end
const O = parse_opts(ARGS)
const HOSTCFG = isempty(O["hosts"]) ? Dict{String,Any}() : TOML.parsefile(O["hosts"])
cfg(k, d) = !isempty(get(O, k, "")) ? O[k] : string(get(get(HOSTCFG, "coordinator", Dict()), replace(k, "-" => "_"), d))
const PORT = parse(Int, cfg("port", 7700))
const HTTP_PORT = parse(Int, cfg("http", 8080))
const ADVERTISE = cfg("advertise", "127.0.0.1")
const TOKEN = isempty(O["token"]) ? bytes2hex(rand(RandomDevice(), UInt8, 16)) : O["token"]
const RUNID = Dates.format(now(), "yyyymmdd_HHMMSS") * "_" * string(getpid())
const COST_SCALE = parse(Float64, O["cost-scale"])
const LOCAL_AFTER = parse(Float64, O["local-fallback-after"])
const HOSTS = Dict{String,Dict{String,Any}}(h["name"] => h for h in get(HOSTCFG, "host", Any[]))
const CODE_SRC = read(joinpath(HERE, "dummy", "dagop.F90"))
const CODE_SHA = sha_hex(CODE_SRC)

# ---------------------------------------------------------------- state
mutable struct Node
    name::String; host::String; arch::String; os::String; julia::String
    ncpu::Int; mem_total::Int; mem_avail::Int; load1::Float64; max_jobs::Int
    incarnation::Int; state::Symbol; draining::Bool
    sock::Any; wlock::ReentrantLock; last_hb::Float64
    running::Set{String}; agent_running::Vector{Tuple{String,Float64}}
    done::Int; failed::Int
    mode::String; fc::String; np::Int; note::String
end

mutable struct DTask
    name::String; op::String; inputs::Vector{String}; params::Dict{String,String}; cost_ms::Int
    state::Symbol            # :waiting :ready :running :done :failed
    node::String; aid::String; attempts::Int
    key::String; input_shas::Vector{String}; result_sha::String
    started::Float64; duration::Union{Nothing,Float64}; not_before::Float64
    rejected_by::Set{String}; note::String; verified::Union{Nothing,Bool}
end

const ST = ReentrantLock()
const NODES = Dict{String,Node}()
const TASKS = DTask[]
const TIDX = Dict{String,Int}()
const STORE = Dict{String,Vector{UInt8}}()      # sha -> blob
const JOURNAL = Dict{String,String}()           # task key -> result sha
const EVENTS = Tuple{Float64,String,String}[]
const LAUNCHED = Dict{String,Tuple{String,String}}()   # host name -> (remote dir, pid)
const INC = Ref(0)
const LAUNCH_SEQ = Ref(0)
const LAST_ALIVE = Ref(time())
const FINISHED = Ref(false)
const VERIFIED = Ref{Union{Nothing,Bool}}(nothing)

function event(kind, msg)
    t = time() - T0
    lock(ST) do
        push!(EVENTS, (t, kind, msg)); length(EVENTS) > 200 && popfirst!(EVENTS)
    end
    @printf("[coord %7.1fs] %-9s %s\n", t, kind, msg); flush(stdout)
end

# ---------------------------------------------------------------- DAG
function parse_dag(path)
    for (ln, raw) in enumerate(eachline(path))
        line = strip(split(raw, '#')[1])
        isempty(line) && continue
        mt = match(r"^(\w+)\s*=\s*(\w+)\s*(.*)$", line)
        mt === nothing && error("$path:$ln: expected 'NAME = op inputs... k=v...'")
        name, op, rest = mt.captures
        haskey(DagRef.NINPUTS, op) || error("$path:$ln: unknown op $op")
        haskey(TIDX, name) && error("$path:$ln: $name defined twice")
        inputs = String[]; params = Dict{String,String}()
        for tok in split(rest)
            if occursin('=', tok)
                k, v = split(tok, '='; limit = 2); params[k] = v
            else
                haskey(TIDX, tok) || error("$path:$ln: input $tok not defined above (DAG must be topological)")
                push!(inputs, tok)
            end
        end
        length(inputs) == DagRef.NINPUTS[op] || error("$path:$ln: $op needs $(DagRef.NINPUTS[op]) inputs")
        for k in DagRef.ARGSPEC[op]
            haskey(params, k) || error("$path:$ln: $op needs parameter $k")
        end
        cost = round(Int, parse(Int, pop!(params, "cost_ms", "0")) * COST_SCALE)
        push!(TASKS, DTask(name, op, inputs, params, cost, :waiting, "", "", 0, "", String[], "",
                           0.0, nothing, 0.0, Set{String}(), "", nothing))
        TIDX[name] = length(TASKS)
    end
    isempty(TASKS) && error("$path: empty DAG")
end

task_key(t) = sha_hex(Vector{UInt8}(codeunits(join([t.op; sort(["$k=$v" for (k, v) in t.params]); t.input_shas], "\n"))))

# ---------------------------------------------------------------- journal
const JDIR = O["journal"]
function journal_load()
    isempty(JDIR) && return
    mkpath(joinpath(JDIR, "blobs"))
    f = joinpath(JDIR, "journal.log")
    isfile(f) || return
    for l in eachline(f)
        p = split(l)
        length(p) == 2 && isfile(joinpath(JDIR, "blobs", p[2])) && (JOURNAL[p[1]] = p[2])
    end
    event("journal", "loaded $(length(JOURNAL)) completed results from $JDIR")
end
function journal_add(key, sha, blob)
    isempty(JDIR) && return
    bf = joinpath(JDIR, "blobs", sha)
    isfile(bf) || (write(bf * ".tmp", blob); mv(bf * ".tmp", bf; force = true))
    open(joinpath(JDIR, "journal.log"), "a") do io
        println(io, key, " ", sha); flush(io)
    end
end

# ---------------------------------------------------------------- remote launch
shq(s) = "'" * replace(string(s), "'" => "'\\''") * "'"
islocal(h) = get(h, "ssh", "local") in ("local", "")
rcmd(h, script) = islocal(h) ? `bash -c $script` :
    `ssh -o BatchMode=yes -o ConnectTimeout=10 -o ServerAliveInterval=5 $(h["ssh"]) $script`

function launch_host(hname)
    h = HOSTS[hname]
    haskey(LAUNCHED, hname) && cleanup_remote(hname)   # relaunch: kill old agent, remove its dir
    tmp = get(h, "tmp", "/tmp")
    dir = "$tmp/vlsi_cluster_$(RUNID)_$(hname)_$(LAUNCH_SEQ[] += 1)"
    julia = get(h, "julia", islocal(h) ? joinpath(Sys.BINDIR, "julia") : "julia")
    try
        event("launch", "$hname: bootstrapping $(get(h, "ssh", "local")):$dir")
        # sweep directories left behind by crashed runs (>12 h old), then create ours
        run(rcmd(h, "find $(shq(tmp)) -maxdepth 1 -name 'vlsi_cluster_*' -mmin +720 -exec rm -rf {} + 2>/dev/null; mkdir -p $(shq(dir))"))
        run(pipeline(`tar -cf - -C $HERE Proto.jl dagref.jl agent.jl`, rcmd(h, "tar -xf - -C $(shq(dir))")))
        opts = ["--coord", get(h, "coord", "$ADVERTISE:$PORT"), "--name", hname, "--workdir", "$dir/work",
                "--selfdestruct", dir, "--max-jobs", string(get(h, "max_jobs", 2)), "--mpi-np", string(get(h, "mpi_np", 2)),
                "--mem-cap-mb", string(get(h, "mem_cap_mb", 0)), "--reserve-mb", string(get(h, "reserve_mb", 512)),
                "--chaos", string(get(h, "chaos", 0)), "--fortran", string(get(h, "fortran", "auto"))]
        script = "cd $(shq(dir)) || exit 1; VLSI_CLUSTER_TOKEN=$(shq(TOKEN)) nohup $(shq(julia)) --startup-file=no agent.jl " *
                 join(shq.(opts), " ") * " > $(shq(dir))/agent.log 2>&1 < /dev/null & echo \$!"
        pid = strip(read(rcmd(h, script), String))
        lock(ST) do; LAUNCHED[hname] = (dir, pid); end
        event("launch", "$hname: agent started, remote pid $pid")
        true
    catch e
        event("error", "$hname: launch failed: $(sprint(showerror, e))")
        false
    end
end

function cleanup_remote(hname)
    haskey(LAUNCHED, hname) || return
    dir, pid = LAUNCHED[hname]
    if O["keep-remote"] == "1"
        event("cleanup", "$hname: keeping $dir (--keep-remote)"); return
    end
    try
        run(rcmd(HOSTS[hname], "kill $pid 2>/dev/null; sleep 0.3; kill -9 $pid 2>/dev/null; rm -rf $(shq(dir))"); wait = true)
        event("cleanup", "$hname: removed $dir")
    catch e
        event("error", "$hname: cleanup failed: $(sprint(showerror, e))")
    end
    delete!(LAUNCHED, hname)
end

# ---------------------------------------------------------------- membership
function requeue!(t::DTask, why; backoff = true)
    t.state = :ready; t.node = ""; t.aid = ""
    t.not_before = backoff ? time() + min(0.5 * 2.0^t.attempts, 8.0) : 0.0
    t.note = why
end

"Declare node dead (or left). Only acts if `inc` is still the current incarnation."
function node_down(name, inc, reason)
    tasks = String[]
    changed = lock(ST) do
        n = get(NODES, name, nothing)
        (n === nothing || n.incarnation != inc || n.state in (:dead, :left)) && return false
        n.state = n.state == :leaving ? :left : :dead
        try close(n.sock) catch end
        for tn in n.running
            t = TASKS[TIDX[tn]]
            if t.state == :running && t.node == name
                requeue!(t, "requeued: $name $(n.state)"); push!(tasks, tn)
            end
        end
        empty!(n.running); empty!(n.agent_running)
        true
    end
    changed || return
    n = NODES[name]
    event(n.state == :left ? "left" : "dead", "$name ($reason)" * (isempty(tasks) ? "" : "; requeued $(join(tasks, ", "))"))
end

function handle_conn(sock)
    peer = try string(getpeername(sock)[1]) catch; "?" end
    timer = Timer(_ -> close(sock), 15)
    m = try recvmsg(sock) catch; close(sock); return end
    close(timer)
    if m.type != REGISTER || get(m.hdr, "token", "") != TOKEN
        event("reject", "connection from $peer: bad handshake or token"); close(sock); return
    end
    name = m.hdr["name"]
    inc = (INC[] += 1)
    old = get(NODES, name, nothing)
    if old !== nothing && !(old.state in (:dead, :left))
        node_down(name, old.incarnation, "superseded by new incarnation $inc")
    end
    gi(k, d = 0) = something(tryparse(Int, get(m.hdr, k, "")), d)
    n = Node(name, get(m.hdr, "host", peer), get(m.hdr, "arch", "?"), get(m.hdr, "os", "?"), get(m.hdr, "julia", "?"),
             gi("ncpu", 1), gi("mem_total"), gi("mem_avail"), 0.0, gi("max_jobs", 1),
             inc, :joining, false, sock, ReentrantLock(), time(), Set{String}(), Tuple{String,Float64}[],
             old === nothing ? 0 : old.done, old === nothing ? 0 : old.failed, "building", "", 1, "")
    lock(ST) do; NODES[name] = n; end
    event("join", "$name from $peer: $(n.arch)/$(n.os), $(n.ncpu) cpu, $(round(n.mem_total / 2^30, digits = 1)) GB, julia $(n.julia), incarnation $inc")
    try
        sendmsg(sock, n.wlock, Msg(WELCOME, Dict("inc" => inc, "run" => RUNID)))
        sendmsg(sock, n.wlock, Msg(CODE, Dict("sha" => CODE_SHA, "file" => "dagop.F90"), CODE_SRC))
        while true
            m = recvmsg(sock)
            lock(ST) do
                n.last_hb = time()
                n.state == :suspect && (n.state = :alive; event("alive", "$name heartbeat resumed"))
            end
            if m.type == HEARTBEAT
                n.load1 = something(tryparse(Float64, get(m.hdr, "load1", "")), 0.0)
                n.mem_avail = something(tryparse(Int, get(m.hdr, "mem_avail", "")), n.mem_avail)
                n.agent_running = [(String(a), parse(Float64, b)) for (a, b) in
                                   (split(x, ':') for x in split(get(m.hdr, "running", ""), ','; keepempty = false))]
            elseif m.type == READY
                n.mode = m.hdr["mode"]; n.fc = get(m.hdr, "fc", ""); n.np = parse(Int, get(m.hdr, "np", "1"))
                n.note = get(m.hdr, "note", "")
                n.state == :joining && (n.state = :alive)
                event("ready", "$name worker=$(n.mode) $(n.fc) np=$(n.np) $(get(m.hdr, "launcher", ""))" *
                               (isempty(n.note) ? "" : " [$(first(n.note, 200))]"))
            elseif m.type == RESULT
                on_result(n, m)
            elseif m.type == REJECT
                on_reject(n, m)
            end
        end
    catch e
        node_down(name, inc, e isa EOFError || e isa Base.IOError ? "connection closed" : sprint(showerror, e))
    end
end

# ---------------------------------------------------------------- results
function complete!(t::DTask, blob::Vector{UInt8}, who::String, aid::String, elapsed)
    lock(ST) do
        (t.state == :running && t.aid == aid) || return false
        sha = sha_hex(blob)
        STORE[sha] = blob
        t.result_sha = sha; t.state = :done; t.node = who
        t.duration = elapsed; t.note = ""
        journal_add(t.key, sha, blob)
        true
    end
end

function on_result(n::Node, m::Msg)
    tname = m.hdr["task"]; aid = m.hdr["aid"]
    t = TASKS[TIDX[tname]]
    lock(ST) do; delete!(n.running, tname); end
    if t.aid != aid || t.state != :running
        event("stale", "ignored late result of $tname from $(n.name)"); return
    end
    if m.hdr["status"] == "ok" && sha_hex(m.blob) == m.hdr["sha"]
        if complete!(t, m.blob, n.name, aid, parse(Float64, m.hdr["elapsed"]))
            n.done += 1
            event("done", "$tname on $(n.name) [$(m.hdr["mode"]) np=$(m.hdr["np"])] $(m.hdr["elapsed"])s → $(DagRef.describe(t.op, m.blob))")
        end
    else
        n.failed += 1
        err = m.hdr["status"] == "ok" ? "checksum mismatch in transfer" : get(m.hdr, "error", "?")
        lock(ST) do; requeue!(t, "attempt $(t.attempts) failed on $(n.name): $(first(err, 200))"); end
        event("fail", "$tname on $(n.name): $(first(err, 300))")
    end
end

function on_reject(n::Node, m::Msg)
    tname = m.hdr["task"]; t = TASKS[TIDX[tname]]
    lock(ST) do
        delete!(n.running, tname)
        if t.aid == m.hdr["aid"] && t.state == :running
            push!(t.rejected_by, n.name); t.attempts -= 1
            requeue!(t, "rejected by $(n.name): $(m.hdr["reason"])"; backoff = false)
        end
    end
    event("reject", "$tname by $(n.name): $(m.hdr["reason"])")
end

# ---------------------------------------------------------------- scheduler
usable(n) = n.state == :alive && !n.draining && length(n.running) < n.max_jobs

function pick_node(t::DTask, need::Int)
    best = nothing; bestscore = (-Inf, -Inf)
    for n in values(NODES)
        usable(n) || continue
        n.name in t.rejected_by && continue
        n.mem_avail - 512 * 2^20 >= need || continue
        score = ((n.max_jobs - length(n.running)) / n.max_jobs, -n.load1 / max(n.ncpu, 1))   # fraction free: small nodes compete fairly
        score > bestscore && (best = n; bestscore = score)
    end
    best
end

function dispatch(n::Node, t::DTask, aid::String, inc::Int)
    blobs = [STORE[s] for s in t.input_shas]
    hdr = Dict{String,String}("task" => t.name, "aid" => aid, "op" => t.op, "cost_ms" => string(t.cost_ms),
                              "in_sizes" => join(length.(blobs), ","))
    for (k, v) in t.params; hdr["p_" * k] = v; end
    try
        sendmsg(n.sock, n.wlock, Msg(TASK, hdr, reduce(vcat, blobs; init = UInt8[])))
    catch e
        node_down(n.name, inc, "send failed: $(sprint(showerror, e))")
    end
end

function run_local(t::DTask, aid::String)
    @async begin
        t0 = time()
        try
            out = DagRef.run_op(t.op, t.params, [STORE[s] for s in t.input_shas])
            complete!(t, out, "coordinator", aid, time() - t0) &&
                event("done", "$(t.name) locally on coordinator (fallback) → $(DagRef.describe(t.op, out))")
        catch e
            lock(ST) do; t.state = :failed; t.note = "local fallback failed: $(sprint(showerror, e))"; end
            event("fail", "$(t.name): local fallback failed: $(sprint(showerror, e))")
        end
    end
end

function schedule_tick()
    now = time(); sends = Tuple{Node,DTask,String,Int}[]; locals = Tuple{DTask,String}[]; evs = String[]
    lock(ST) do
        any(n -> n.state == :alive && !n.draining, values(NODES)) && (LAST_ALIVE[] = now)
        for t in TASKS
            if t.state == :waiting && all(TASKS[TIDX[i]].state == :done for i in t.inputs)
                t.input_shas = [TASKS[TIDX[i]].result_sha for i in t.inputs]
                t.key = task_key(t)
                if haskey(JOURNAL, t.key) && (sha = JOURNAL[t.key]; true)
                    STORE[sha] = read(joinpath(JDIR, "blobs", sha))
                    t.result_sha = sha; t.state = :done; t.node = "journal"; t.duration = 0.0
                    push!(evs, "$(t.name) restored from journal")
                else
                    t.state = :ready
                end
            end
        end
        for t in TASKS
            (t.state == :ready && t.not_before <= now) || continue
            need = 4 * sum(length(STORE[s]) for s in t.input_shas; init = 0) + 64 * 2^20
            n = t.attempts < MAX_ATTEMPTS ? pick_node(t, need) : nothing
            aid = randstring(8)
            if n !== nothing
                t.state = :running; t.node = n.name; t.aid = aid; t.attempts += 1; t.started = now
                push!(n.running, t.name); push!(sends, (n, t, aid, n.incarnation))
            else
                alive = [x for x in values(NODES) if x.state == :alive && !x.draining]
                why = t.attempts >= MAX_ATTEMPTS ? "retry limit reached" :
                      (!isempty(alive) && all(x -> x.name in t.rejected_by, alive)) ? "rejected by every node" :
                      (isempty(alive) && now - LAST_ALIVE[] > LOCAL_AFTER) ? "no nodes for $(LOCAL_AFTER)s" : ""
                if !isempty(why)
                    t.state = :running; t.node = "coordinator"; t.aid = aid; t.started = now
                    t.note = "local fallback: $why"; push!(locals, (t, aid))
                end
            end
        end
        FINISHED[] = all(t -> t.state in (:done, :failed), TASKS)
    end
    foreach(e -> event("journal", e), evs)
    for (n, t, aid, inc) in sends
        event("dispatch", "$(t.name) ($(t.op)) → $(n.name) attempt $(t.attempts)")
        @async dispatch(n, t, aid, inc)
    end
    for (t, aid) in locals
        event("fallback", "$(t.name) runs on coordinator: $(t.note)"); run_local(t, aid)
    end
end

function monitor_tick()
    now = time(); down = Tuple{String,Int}[]; pings = Node[]; leave = Node[]
    lock(ST) do
        for n in values(NODES)
            n.state in (:dead, :left) && continue
            age = now - n.last_hb
            if age > DEAD_AFTER
                push!(down, (n.name, n.incarnation))
            elseif age > SUSPECT_AFTER && n.state == :alive
                n.state = :suspect; event("suspect", "$(n.name): no heartbeat for $(round(age, digits = 1))s")
            end
            n.state != :leaving && push!(pings, n)
            if n.draining && isempty(n.running) && n.state in (:alive, :suspect)
                n.state = :leaving; push!(leave, n)
            end
        end
    end
    for (name, inc) in down; node_down(name, inc, "heartbeat timeout"); end
    for n in pings; @async try sendmsg(n.sock, n.wlock, Msg(HEARTBEAT)) catch end; end
    for n in leave
        event("drain", "$(n.name) drained and idle, shutting it down")
        @async try sendmsg(n.sock, n.wlock, Msg(SHUTDOWN)) catch end
    end
end

# ---------------------------------------------------------------- dashboard glue
function state_json()
    now = time()
    lock(ST) do
        counts = Dict(string(s) => count(t -> t.state == s, TASKS) for s in (:waiting, :ready, :running, :done, :failed))
        connected = Set(n.name for n in values(NODES) if !(n.state in (:dead, :left)))
        tojson(Dict(
            "run" => RUNID, "uptime" => now - T0, "finished" => FINISHED[], "verified" => VERIFIED[], "counts" => counts,
            "nodes" => [Dict("name" => n.name, "arch" => n.arch, "os" => n.os, "julia" => n.julia, "ncpu" => n.ncpu,
                             "mem_total" => n.mem_total, "mem_avail" => n.mem_avail, "load1" => n.load1,
                             "max_jobs" => n.max_jobs, "state" => n.state, "draining" => n.draining,
                             "incarnation" => n.incarnation, "hb_age" => now - n.last_hb, "done" => n.done,
                             "failed" => n.failed, "mode" => n.mode, "fc" => n.fc, "np" => n.np,
                             "running" => [Dict("task" => a, "elapsed" => b) for (a, b) in n.agent_running])
                        for n in sort(collect(values(NODES)); by = x -> x.name)],
            "spare" => sort([h for h in keys(HOSTS) if !(h in connected)]),
            "tasks" => [Dict("name" => t.name, "op" => t.op, "inputs" => t.inputs, "state" => t.state, "node" => t.node,
                             "attempts" => t.attempts, "duration" => t.duration, "note" => t.note,
                             "result" => t.state == :done ? DagRef.describe(t.op, STORE[t.result_sha]) *
                                         (t.verified === nothing ? "" : t.verified ? " ✓" : " ✗") : "")
                        for t in TASKS],
            "events" => [Dict("t" => e[1], "kind" => e[2], "msg" => e[3]) for e in EVENTS]))
    end
end

function action(a, q)
    if a in ("drain", "undrain", "kill")
        name = get(q, "node", ""); n = get(NODES, name, nothing)
        n === nothing && return "unknown node $name"
        if a == "kill"
            event("chaos", "kill requested for $name")
            @async try sendmsg(n.sock, n.wlock, Msg(CRASH)) catch end
        else
            lock(ST) do; n.draining = a == "drain"; end
            event(a, name)
        end
        return "$a $name: ok"
    elseif a == "add"
        h = get(q, "host", "")
        haskey(HOSTS, h) || return "host $h is not in the hosts file"
        @async launch_host(h)
        return "launching agent on $h"
    end
    "unknown action $a"
end

# ---------------------------------------------------------------- main
function verify()
    ok = true
    for t in TASKS
        t.state == :done || (ok = false; continue)
        ref = DagRef.run_op(t.op, t.params, [STORE[s] for s in t.input_shas])
        t.verified = sha_hex(ref) == t.result_sha
        t.verified || (ok = false; event("verify", "MISMATCH in $(t.name) (computed on $(t.node))"))
    end
    VERIFIED[] = ok
    ok
end

function shutdown_all()
    nodes = lock(ST) do; [n for n in values(NODES) if !(n.state in (:dead, :left))]; end
    for n in nodes
        lock(ST) do; n.state = :leaving; end
        try sendmsg(n.sock, n.wlock, Msg(SHUTDOWN)) catch end
    end
    t = time()
    while time() - t < 5 && any(n -> !(n.state in (:dead, :left)), nodes); sleep(0.2); end
    @sync for h in collect(keys(LAUNCHED)); @async cleanup_remote(h); end
end

function main()
    parse_dag(O["dag"]); journal_load()
    srv = listen(parse(IPAddr, O["bind"]), PORT)
    web = Dashboard.serve(state_json, action; host = O["http-bind"], port = HTTP_PORT)
    event("start", "run $RUNID: $(length(TASKS)) tasks, agents dial $ADVERTISE:$PORT, dashboard http://$(O["http-bind"]):$HTTP_PORT/")
    isempty(O["token"]) || event("start", "token fixed by --token (manual agents: VLSI_CLUSTER_TOKEN=...)")
    @async while isopen(srv)
        s = try accept(srv) catch; break end
        @async handle_conn(s)
    end
    @async while true; try monitor_tick() catch e; event("error", "monitor: $(sprint(showerror, e))") end; sleep(1.0); end
    if O["no-launch"] != "1"
        for (h, c) in HOSTS
            get(c, "autostart", true) && @async launch_host(h)
        end
    end
    t0 = time()
    while !FINISHED[]
        try schedule_tick() catch e; event("error", "scheduler: $(sprint(showerror, e))"); end
        sleep(0.1)
    end
    ok = verify()
    elapsed = time() - t0
    event("finish", @sprintf("DAG finished in %.1fs, verification %s", elapsed, ok ? "PASSED" : "FAILED"))
    println("\n", rpad("task", 12), rpad("op", 8), rpad("node", 14), rpad("tries", 7), rpad("time", 9), "result")
    for t in TASKS
        println(rpad(t.name, 12), rpad(t.op, 8), rpad(t.node, 14), rpad(t.attempts, 7),
                rpad(t.duration === nothing ? "-" : @sprintf("%.2fs", t.duration), 9),
                t.state == :done ? DagRef.describe(t.op, STORE[t.result_sha]) * (t.verified ? "  ✓" : "  ✗ MISMATCH") : string(t.state))
    end
    println()
    for n in sort(collect(values(NODES)); by = x -> x.name)
        @printf("node %-12s %-8s %-7s worker=%-12s done=%d failed=%d\n", n.name, n.arch, n.os, n.mode, n.done, n.failed)
    end
    linger = parse(Float64, O["linger"])
    if linger > 0
        event("linger", "dashboard stays up for $(linger)s (Ctrl-C to stop)")
        sleep(linger)
    end
    close(srv); close(web)
    ok
end

Base.exit_on_sigint(false)
ok = false
try
    global ok = main()
catch e
    e isa InterruptException ? event("stop", "interrupted") : (event("error", sprint(showerror, e, catch_backtrace())))
finally
    shutdown_all()
end
exit(ok ? 0 : 1)
