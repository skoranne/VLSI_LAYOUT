# tui.jl -- ASCII dashboard for the cluster coordinators (stdlib only).
#
#   AsciiDash.render(state::AbstractDict; width = 118) -> String
#
# `state` is the same Dict the HTML dashboard serves as JSON (/api/state): run, uptime, counts,
# nodes[] (name, arch, state, load1, ncpu, mem_total, mem_avail, max_jobs, running[], done, failed),
# tasks[] (name, op, inputs, state, node, duration, result), events[] (t, kind, msg), plus the
# optional "banner" line. Pure ASCII apart from the ✓/✗ already present in task results.
module AsciiDash

using Printf
export render

bar(frac, w) = (f = clamp(isfinite(frac) ? frac : 0.0, 0.0, 1.0); k = round(Int, f * w); "[" * "#"^k * "."^(w - k) * "]")
cut(s, w) = (s = string(s); length(s) <= w ? rpad(s, w) : first(s, w - 1) * "~")

function render(s::AbstractDict; width::Int = 118, max_tasks::Int = 40, max_events::Int = 8)
    io = IOBuffer()
    line(c = '-') = println(io, string(c)^width)
    # count real ops only: CLUSTER-mode source layers ("src") are not work
    ops = filter(t -> !startswith(string(t["op"]), "src"), s["tasks"]); ntask = length(ops)
    cnt(st) = count(t -> string(t["state"]) == st, ops)
    done, running, ready, waiting, failed = cnt("done"), cnt("running"), cnt("ready"), cnt("waiting"), cnt("failed")
    line('=')
    @printf(io, "VLSI CLUSTER  run %s   up %6.1fs   %s\n", s["run"], s["uptime"],
            s["finished"] ? (s["verified"] === nothing ? "FINISHED" : s["verified"] ? "FINISHED, VERIFIED" : "FINISHED, VERIFY FAILED") : "RUNNING")
    haskey(s, "banner") && !isempty(s["banner"]) && println(io, s["banner"])
    @printf(io, "progress %s %3d%%   done %d  running %d  ready %d  waiting %d  failed %d  (of %d)\n",
            bar(done / max(ntask, 1), 40), round(Int, 100 * done / max(ntask, 1)), done, running, ready, waiting, failed, ntask)
    line()
    println(io, rpad("NODE", 13), rpad("ARCH", 9), rpad("STATE", 9), rpad("LOAD", 19), rpad("MEM USED", 19),
            rpad("SLOTS", 7), rpad("DONE", 6), rpad("FAIL", 6), "RUNNING")
    for n in s["nodes"]
        load = n["ncpu"] > 0 ? n["load1"] / n["ncpu"] : 0.0
        mem = n["mem_total"] > 0 ? 1 - n["mem_avail"] / n["mem_total"] : 0.0
        run = isempty(n["running"]) ? (string(n["state"]) == "dead" ? string(n["mode"]) : "idle") :
              join([@sprintf("%s %.1fs", r["task"], r["elapsed"]) for r in n["running"]], ", ")
        println(io, cut(n["name"], 13), cut(n["arch"], 9), cut(string(n["state"]) * (n["draining"] ? "*" : ""), 9),
                bar(load, 10), @sprintf(" %5.1f  ", n["load1"]), bar(mem, 10), @sprintf(" %4.1fG  ", (n["mem_total"] - n["mem_avail"]) / 2^30),
                cut(@sprintf("%d/%d", length(n["running"]), n["max_jobs"]), 7), cut(n["done"], 6), cut(n["failed"], 6),
                cut(run, max(width - 88, 12)))
    end
    isempty(s["nodes"]) && println(io, "  (no nodes connected yet)")
    line()
    println(io, rpad("TASK", 14), rpad("OP", 14), rpad("INPUTS", 22), rpad("STATE", 9), rpad("NODE", 18), rpad("TIME", 8), "RESULT")
    shown = 0
    # running first, then the rest in DAG order; source layers only while nothing else is shown
    order = vcat(filter(t -> string(t["state"]) == "running", s["tasks"]),
                 filter(t -> string(t["state"]) != "running", s["tasks"]))
    for t in order
        startswith(string(t["op"]), "src") && continue
        shown >= max_tasks && (println(io, "  ... ", ntask - shown, " more"); break)
        st = string(t["state"])
        mark = st == "running" ? ">" : st == "done" ? " " : st == "failed" ? "!" : "."
        println(io, mark, cut(t["name"], 13), cut(t["op"], 14), cut(join(t["inputs"], " "), 22), cut(st, 9), cut(t["node"], 18),
                cut(t["duration"] === nothing ? "" : @sprintf("%.2fs", t["duration"]), 8), cut(t["result"], max(width - 85, 12)))
        shown += 1
    end
    line()
    ev = s["events"]
    for e in ev[max(1, end - max_events + 1):end]
        println(io, cut(@sprintf("%7.1fs %-9s %s", e["t"], e["kind"], e["msg"]), width))
    end
    line('=')
    String(take!(io))
end

end # module
