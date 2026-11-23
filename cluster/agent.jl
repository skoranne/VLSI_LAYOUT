#!/usr/bin/env julia
# agent.jl -- per-node daemon of the VLSI compute-cluster prototype.
#
#   julia agent.jl --coord HOST:PORT[,HOST2:PORT2] [--name NAME] [--workdir DIR]
#                  [--max-jobs N] [--mpi-np N] [--mem-cap-mb N] [--reserve-mb N]
#                  [--selfdestruct DIR] [--orphan-timeout S] [--task-timeout S]
#                  [--chaos P] [--fortran FC|none]
# Token: env VLSI_CLUSTER_TOKEN (or --token).
#
# The agent dials OUT to the coordinator (so a node joins simply by starting
# an agent), receives the worker source over TCP, builds it locally (MPI if an
# MPI Fortran wrapper works, else serial, else the Julia reference), runs
# tasks in per-task temp dirs that are always deleted, and reconnects with
# backoff (trying every listed coordinator) if the link drops. With no
# coordinator for --orphan-timeout seconds it exits and removes its files.

const HERE = @__DIR__
include(joinpath(HERE, "Proto.jl"))
include(joinpath(HERE, "dagref.jl"))
using .Proto, .DagRef
using Sockets, Printf, Random

# ---------------------------------------------------------------- options
function parse_opts(args)
    o = Dict{String,String}(
        "name" => gethostname(), "max-jobs" => string(clamp(Sys.CPU_THREADS ÷ 4, 1, 4)),
        "mpi-np" => "2", "mem-cap-mb" => "0", "reserve-mb" => "512", "orphan-timeout" => "300",
        "task-timeout" => "600", "chaos" => "0", "fortran" => "auto", "workdir" => "", "selfdestruct" => "",
        "token" => get(ENV, "VLSI_CLUSTER_TOKEN", ""), "coord" => "")
    i = 1
    while i <= length(args)
        a = args[i]
        startswith(a, "--") || error("unexpected argument $a")
        k = a[3:end]
        haskey(o, k) || error("unknown option $a")
        i + 1 <= length(args) || error("missing value for $a")
        o[k] = args[i + 1]; i += 2
    end
    isempty(o["coord"]) && error("--coord HOST:PORT is required")
    o
end

const O = parse_opts(ARGS)
const NAME = O["name"]
const MAXJOBS = parse(Int, O["max-jobs"])
const RESERVE = parse(Int, O["reserve-mb"]) * 2^20
const MEMCAP = parse(Int, O["mem-cap-mb"]) * 2^20
const CHAOS = parse(Float64, O["chaos"])
const TASK_TIMEOUT = parse(Float64, O["task-timeout"])
const WORKDIR = isempty(O["workdir"]) ? mktempdir(tempdir(); prefix = "vlsi_cluster_agent_", cleanup = false) : O["workdir"]
mkpath(WORKDIR)

logmsg(s) = (println(stdout, "[", NAME, " ", round(time() - T0; digits = 1), "s] ", s); flush(stdout))
const T0 = time()

# ---------------------------------------------------------------- state
mutable struct Build
    mode::String        # "fortran-mpi" | "fortran" | "julia" | "none"
    sha::String
    bin::String
    fc::String
    launcher::Vector{String}  # e.g. ["mpirun", "--oversubscribe", "-np", "2"]
    np::Int
    note::String
end
const BUILD = Build("none", "", "", "", String[], 1, "")
const RUNNING = Dict{String,Tuple{Float64,Any}}()   # task -> (start, Process|nothing)
const CONN = Ref{Any}(nothing)
const WLOCK = Ref{ReentrantLock}(ReentrantLock())
const QUIT = Ref(false)

function cleanup()
    for (_, (_, p)) in RUNNING
        p !== nothing && process_running(p) && try kill(p) catch end
    end
    rm(WORKDIR; recursive = true, force = true)
    sd = O["selfdestruct"]
    isempty(sd) || rm(sd; recursive = true, force = true)
end
atexit(cleanup)

function mem_avail()
    a = Int(Sys.free_memory())
    if Sys.islinux() && isfile("/proc/meminfo")
        for l in eachline("/proc/meminfo")
            if startswith(l, "MemAvailable:")
                a = parse(Int, split(l)[2]) * 1024; break
            end
        end
    end
    MEMCAP > 0 ? min(a, MEMCAP) : a
end
mem_total() = MEMCAP > 0 ? min(Int(Sys.total_memory()), MEMCAP) : Int(Sys.total_memory())

function send(m::Msg)
    s = CONN[]
    s === nothing && return false
    try
        sendmsg(s, WLOCK[], m); true
    catch e
        logmsg("send failed: $(sprint(showerror, e))"); try close(s) catch end; false
    end
end

# ---------------------------------------------------------------- build
function run_capture(cmd; timeout = 120.0)
    out = IOBuffer()
    p = run(pipeline(ignorestatus(cmd); stdout = out, stderr = out); wait = false)
    t = Timer(_ -> process_running(p) && kill(p), timeout)
    wait(p); close(t)
    success(p), String(take!(out))
end

"Ask the Fortran binary for a tiny gen and compare with the Julia reference."
function selftest(launch::Vector{String}, bin::String, dir::String)
    out = joinpath(dir, "selftest.bin")
    rm(out; force = true)
    ok, log = run_capture(Cmd(`$launch $bin gen $out 7 257 100000 300`; dir = dir); timeout = 60)
    ok && isfile(out) || return false, log
    ref = DagRef.run_op("gen", Dict("seed" => "7", "n" => "257", "extent" => "100000", "maxsize" => "300"), Vector{UInt8}[])
    read(out) == ref ? (true, "") : (false, "self-test output differs from reference")
end

function mpi_launcher(fc::String, np::Int)
    d = dirname(something(Sys.which(fc), fc))
    cands = [joinpath(d, "mpirun"), joinpath(d, "mpiexec")]
    for c in ("mpirun", "mpiexec")
        p = Sys.which(c); p === nothing || push!(cands, p)
    end
    for c in cands
        isfile(c) || continue
        ok, v = run_capture(`$c --version`; timeout = 20)
        flags = occursin(r"Open MPI|OpenRTE"i, v) ? ["--oversubscribe"] : String[]
        return [c; flags; "-np"; string(np)]
    end
    String[]
end

function build_code(src::Vector{UInt8}, sha::String)
    if BUILD.sha == sha && BUILD.mode != "none"
        return
    end
    bdir = joinpath(WORKDIR, "build", sha[1:16]); mkpath(bdir)
    srcf = joinpath(bdir, "dagop.F90"); write(srcf, src)
    np = parse(Int, O["mpi-np"]); notes = String[]
    cands = O["fortran"] == "auto" ?
        [("mpif90", true), ("mpifort", true), ("mpiifx", true), ("mpiifort", true),
         ("gfortran", false), ("ifx", false), ("nvfortran", false), ("flang", false)] :
        O["fortran"] == "none" ? Tuple{String,Bool}[] : [(O["fortran"], occursin("mpi", O["fortran"]))]
    t0 = time()
    for (fc, mpi) in cands
        Sys.which(fc) === nothing && continue
        bin = joinpath(bdir, "dagop_" * fc)
        ok, log = run_capture(Cmd(`$fc -O2 $(mpi ? String[] : ["-DNO_MPI"]) -o $bin $srcf`; dir = bdir))
        if !ok
            push!(notes, "$fc: compile failed: " * first(log, 300)); continue
        end
        if mpi && np > 1
            l = mpi_launcher(fc, np)
            if !isempty(l)
                ok, log = selftest(l, bin, bdir)
                if ok
                    BUILD.mode, BUILD.bin, BUILD.fc, BUILD.launcher, BUILD.np = "fortran-mpi", bin, fc, l, np
                    @goto done
                end
                push!(notes, "$fc: mpirun self-test failed: " * first(log, 300))
            end
        end
        ok, log = selftest(String[], bin, bdir)           # singleton / serial
        if ok
            BUILD.mode, BUILD.bin, BUILD.fc, BUILD.launcher, BUILD.np = "fortran", bin, fc, String[], 1
            @goto done
        end
        push!(notes, "$fc: self-test failed: " * first(log, 300))
    end
    BUILD.mode, BUILD.bin, BUILD.fc, BUILD.launcher, BUILD.np = "julia", "", "", String[], 1
    @label done
    BUILD.sha = sha
    BUILD.note = join(notes, " | ")
    logmsg(@sprintf("build: mode=%s fc=%s np=%d (%.1fs) %s", BUILD.mode, BUILD.fc, BUILD.np, time() - t0, BUILD.note))
end

# ---------------------------------------------------------------- tasks
function tail(f, n = 1500)
    isfile(f) || return ""
    s = read(f, String)
    length(s) > n ? "..." * last(s, n) : s
end

function start_task(m::Msg)
    tname = m.hdr["task"]; aid = m.hdr["aid"]
    sizes = [parse(Int, x) for x in split(get(m.hdr, "in_sizes", ""), ','; keepempty = false)]
    need = 4 * sum(sizes; init = 0) + 64 * 2^20
    avail = mem_avail() - RESERVE
    reason = need > avail ? @sprintf("memory: need %.0f MB, available %.0f MB", need / 2^20, avail / 2^20) :
             length(RUNNING) >= MAXJOBS ? "busy" :
             BUILD.mode == "none" ? "not built" : ""
    if !isempty(reason)
        logmsg("reject $tname: $reason")
        send(Msg(REJECT, Dict("task" => tname, "aid" => aid, "reason" => reason)))
        return
    end
    if CHAOS > 0 && rand() < CHAOS
        logmsg("CHAOS: crashing while starting $tname")
        @async (sleep(rand()); ccall(:_exit, Cvoid, (Cint,), 137))
    end
    RUNNING[tname] = (time(), nothing)
    @async run_task(m, tname, aid, sizes)
end

function run_task(m::Msg, tname, aid, sizes)
    dir = mktempdir(WORKDIR; prefix = "task_")
    t0 = time(); status = "ok"; err = ""; out = UInt8[]
    op = m.hdr["op"]
    params = Dict(k[3:end] => v for (k, v) in m.hdr if startswith(k, "p_"))
    cost = get(m.hdr, "cost_ms", "0")
    try
        inputs = Vector{UInt8}[]; off = 0
        for s in sizes
            push!(inputs, m.blob[off+1:off+s]); off += s
        end
        if BUILD.mode in ("fortran", "fortran-mpi")
            infiles = String[]
            for (i, b) in enumerate(inputs)
                f = joinpath(dir, "in_$i.bin"); write(f, b); push!(infiles, f)
            end
            outf = joinpath(dir, "out.bin"); errf = joinpath(dir, "log.txt")
            args = [op; outf; infiles; [params[k] for k in DagRef.ARGSPEC[op]]]
            cmd = addenv(Cmd(`$(BUILD.launcher) $(BUILD.bin) $args`; dir = dir), "DAGOP_SLEEP_MS" => cost)
            p = run(pipeline(ignorestatus(cmd); stdout = errf, stderr = errf); wait = false)
            RUNNING[tname] = (t0, p)
            timer = Timer(_ -> process_running(p) && kill(p), TASK_TIMEOUT)
            wait(p); close(timer)
            success(p) || error("dagop exited with $(p.exitcode): " * tail(errf))
            out = read(outf)
        else
            sleep(parse(Int, cost) / 1000)              # simulated cost without burning the event loop
            out = DagRef.run_op(op, params, inputs)
        end
    catch e
        status = "error"; err = first(sprint(showerror, e), 2000)
        logmsg("task $tname failed: $err")
    finally
        rm(dir; recursive = true, force = true)
        delete!(RUNNING, tname)
    end
    send(Msg(RESULT, Dict("task" => tname, "aid" => aid, "status" => status, "error" => err,
                          "sha" => sha_hex(out), "elapsed" => @sprintf("%.3f", time() - t0),
                          "mode" => BUILD.mode, "np" => BUILD.np), out))
end

function kill_running()
    for (_, (_, p)) in RUNNING
        p !== nothing && process_running(p) && try kill(p) catch end
    end
end

# ---------------------------------------------------------------- session
function heartbeat_hdr()
    now = time()
    Dict("load1" => @sprintf("%.2f", Sys.loadavg()[1]), "mem_avail" => mem_avail(),
         "running" => join(("$k:" * @sprintf("%.1f", now - t) for (k, (t, _)) in RUNNING), ","))
end

function session(sock)
    CONN[] = sock; WLOCK[] = ReentrantLock()
    last_rx = Ref(time())
    send(Msg(REGISTER, Dict("name" => NAME, "token" => O["token"], "host" => gethostname(),
        "arch" => string(Sys.ARCH), "os" => string(Sys.KERNEL), "ncpu" => Sys.CPU_THREADS,
        "mem_total" => mem_total(), "mem_avail" => mem_avail(), "julia" => string(VERSION),
        "pid" => getpid(), "max_jobs" => MAXJOBS, "code_sha" => BUILD.sha)))
    hb = @async while isopen(sock)
        send(Msg(HEARTBEAT, heartbeat_hdr())) || break
        if time() - last_rx[] > 15
            logmsg("coordinator silent for 15 s, dropping link"); close(sock); break
        end
        sleep(2)
    end
    try
        while true
            m = recvmsg(sock); last_rx[] = time()
            if m.type == WELCOME
                logmsg("registered with coordinator, incarnation $(m.hdr["inc"]) run $(m.hdr["run"])")
            elseif m.type == CODE
                @async begin
                    try
                        build_code(m.blob, m.hdr["sha"])
                    catch e
                        BUILD.mode = "julia"; BUILD.note = sprint(showerror, e)
                    end
                    send(Msg(READY, Dict("mode" => BUILD.mode, "fc" => BUILD.fc, "np" => BUILD.np,
                                         "launcher" => join(BUILD.launcher, " "), "note" => first(BUILD.note, 1000))))
                end
            elseif m.type == TASK
                start_task(m)
            elseif m.type == SHUTDOWN
                logmsg("shutdown requested"); QUIT[] = true; break
            elseif m.type == CRASH
                logmsg("CRASH requested (chaos)"); ccall(:_exit, Cvoid, (Cint,), 137)
            end
        end
    catch e
        isopen(sock) && logmsg("link error: $(sprint(showerror, e))")
    finally
        CONN[] = nothing
        try close(sock) catch end
    end
end

function main()
    coords = split(O["coord"], ','; keepempty = false)
    logmsg("agent pid=$(getpid()) arch=$(Sys.ARCH) workdir=$WORKDIR coordinators=$(join(coords, ' '))")
    backoff = 1.0; orphan_since = time(); k = 0
    orphan = parse(Float64, O["orphan-timeout"])
    while !QUIT[]
        c = coords[k % length(coords) + 1]; k += 1
        h, p = rsplit(c, ':'; limit = 2)
        sock = try
            connect(h, parse(Int, p))
        catch e
            if time() - orphan_since > orphan
                logmsg("no coordinator for $(orphan)s, exiting"); break
            end
            sleep(backoff); backoff = min(2backoff, 10.0); nothing
        end
        sock === nothing && continue
        backoff = 1.0
        session(sock)
        kill_running()
        orphan_since = time()
        QUIT[] || sleep(1 + rand())
    end
    logmsg("exiting, removing $(WORKDIR)" * (isempty(O["selfdestruct"]) ? "" : " and $(O["selfdestruct"])"))
end

Base.exit_on_sigint(false)
try
    main()
catch e
    e isa InterruptException || rethrow()
end
