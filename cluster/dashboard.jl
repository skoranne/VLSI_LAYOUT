# dashboard.jl -- tiny stdlib-only HTTP server for the cluster dashboard.
#   GET  /            single-page dashboard (polls /api/state every second)
#   GET  /api/state   JSON snapshot
#   POST /api/<action>?node=X | ?host=X   (drain, undrain, kill, add)
module Dashboard

using Sockets
export serve

function urldecode(s::AbstractString)
    io = IOBuffer(); i = 1; b = codeunits(s)
    while i <= length(b)
        c = b[i]
        if c == UInt8('%') && i + 2 <= length(b)
            write(io, parse(UInt8, String(b[i+1:i+2]); base = 16)); i += 3
        else
            write(io, c == UInt8('+') ? UInt8(' ') : c); i += 1
        end
    end
    String(take!(io))
end

function respond(sock, status, ctype, body)
    write(sock, "HTTP/1.1 $status\r\nContent-Type: $ctype\r\nContent-Length: $(sizeof(body))\r\n" *
                "Cache-Control: no-store\r\nConnection: close\r\n\r\n", body)
end

function handle(sock, state_json::Function, action::Function)
    t = Timer(_ -> close(sock), 10)
    try
        parts = split(readline(sock))
        length(parts) >= 2 || return
        method, target = parts[1], parts[2]
        clen = 0
        while true
            h = readline(sock)
            isempty(strip(h)) && break
            lowercase(h) |> l -> startswith(l, "content-length:") && (clen = parse(Int, strip(h[16:end])))
        end
        clen > 0 && read(sock, min(clen, 65536))
        path, qs = occursin('?', target) ? split(target, '?'; limit = 2) : (target, "")
        q = Dict{String,String}()
        for kv in split(qs, '&'; keepempty = false)
            k, v = occursin('=', kv) ? split(kv, '='; limit = 2) : (kv, "")
            q[urldecode(k)] = urldecode(v)
        end
        if method == "GET" && (path == "/" || path == "/index.html")
            respond(sock, "200 OK", "text/html; charset=utf-8", PAGE)
        elseif method == "GET" && path == "/api/state"
            respond(sock, "200 OK", "application/json", state_json())
        elseif method == "POST" && startswith(path, "/api/")
            msg = action(String(path[6:end]), q)
            respond(sock, "200 OK", "text/plain; charset=utf-8", msg)
        else
            respond(sock, "404 Not Found", "text/plain", "not found")
        end
    catch e
        e isa Base.IOError || e isa EOFError || @warn "dashboard request failed" exception = e
    finally
        close(t); close(sock)
    end
end

"Start serving in the background; returns the listening server."
function serve(state_json::Function, action::Function; host = "127.0.0.1", port = 8080)
    srv = listen(parse(IPAddr, host), port)
    @async while isopen(srv)
        s = try accept(srv) catch; break end
        @async handle(s, state_json, action)
    end
    srv
end

const PAGE = raw"""<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>VLSI Cluster</title>
<style>
:root{--bg:#f6f7f9;--card:#fff;--fg:#1d2330;--mut:#667085;--line:#e3e6ea;--ok:#1a7f4b;--warn:#b26b00;--bad:#c0362c;--acc:#2f5bd3;--bar:#e9edf3}
@media (prefers-color-scheme:dark){:root{--bg:#12151b;--card:#1b2029;--fg:#e6e9ef;--mut:#98a2b3;--line:#2b3240;--ok:#3ccf8e;--warn:#f5b041;--bad:#ff6b5e;--acc:#7aa2ff;--bar:#2a3140}}
*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--fg);font:14px/1.45 system-ui,-apple-system,Segoe UI,sans-serif}
header{padding:14px 16px;border-bottom:1px solid var(--line);display:flex;flex-wrap:wrap;gap:12px;align-items:baseline}
h1{font-size:18px;margin:0}h2{font-size:15px;margin:18px 0 8px}.mut{color:var(--mut)}main{padding:0 16px 32px;max-width:1400px;margin:auto}
.prog{height:10px;background:var(--bar);border-radius:5px;overflow:hidden;display:flex;min-width:200px;flex:1}
.prog i{display:block;height:100%}
.grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(300px,1fr));gap:12px}
.card{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:12px}
.row{display:flex;justify-content:space-between;gap:8px;align-items:center}
.badge{font-size:12px;padding:1px 8px;border-radius:10px;border:1px solid currentColor}
.alive{color:var(--ok)}.joining,.suspect,.leaving{color:var(--warn)}.dead{color:var(--bad)}.left{color:var(--mut)}
.meter{height:6px;background:var(--bar);border-radius:3px;overflow:hidden;margin:2px 0 6px}.meter i{display:block;height:100%;background:var(--acc)}
.kv{font-size:12px;color:var(--mut)}button{font:inherit;font-size:12px;padding:2px 10px;border-radius:6px;border:1px solid var(--line);background:transparent;color:var(--fg);cursor:pointer}
button:hover{border-color:var(--acc)}table{width:100%;border-collapse:collapse;font-size:13px}th,td{text-align:left;padding:4px 6px;border-bottom:1px solid var(--line);white-space:nowrap}
.scroll{overflow-x:auto;background:var(--card);border:1px solid var(--line);border-radius:10px}
.st-done{color:var(--ok)}.st-running{color:var(--acc)}.st-failed{color:var(--bad)}.st-ready,.st-waiting{color:var(--mut)}
.task{font-size:12px;font-family:ui-monospace,monospace}.ev{font-family:ui-monospace,monospace;font-size:12px}
</style></head><body>
<header><h1>VLSI Cluster</h1><span class="mut" id="sum"></span><div class="prog" id="prog"></div></header>
<main>
<h2>Nodes</h2><div class="grid" id="nodes"></div>
<div id="sparewrap"><h2>Configured hosts not connected</h2><div id="spare" class="kv"></div></div>
<h2>Tasks</h2><div class="scroll"><table id="tasks"></table></div>
<h2>Events</h2><div class="scroll" style="max-height:320px;overflow-y:auto"><table id="events"></table></div>
</main>
<script>
const E=s=>String(s??"").replace(/[&<>"']/g,c=>({"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}[c]));
const MB=b=>(b/1048576).toFixed(0)+" MB", GB=b=>(b/1073741824).toFixed(1)+" GB";
async function act(a,k,v){const r=await fetch(`/api/${a}?${k}=${encodeURIComponent(v)}`,{method:"POST"});alert(await r.text());tick()}
function node(n){
  const mem=n.mem_total?1-n.mem_avail/n.mem_total:0, load=n.ncpu?Math.min(1,n.load1/n.ncpu):0;
  const run=n.running.map(r=>`<div class="task">▶ ${E(r.task)} <span class="mut">${r.elapsed.toFixed(1)}s</span></div>`).join("")||'<div class="kv">idle</div>';
  const btn=["alive","suspect","joining"].includes(n.state)?`<button onclick="act('${n.draining?"undrain":"drain"}','node','${E(n.name)}')">${n.draining?"Undrain":"Drain"}</button> <button onclick="act('kill','node','${E(n.name)}')">Kill</button>`:"";
  return `<div class="card"><div class="row"><b>${E(n.name)}</b><span class="badge ${n.state}">${n.state}${n.draining?" · draining":""}</span></div>
  <div class="kv">${E(n.arch)} · ${E(n.os)} · ${n.ncpu} cpu · julia ${E(n.julia)} · inc ${n.incarnation}</div>
  <div class="kv">worker: ${E(n.mode)}${n.fc?" ("+E(n.fc)+", np "+n.np+")":""} · heartbeat ${n.hb_age.toFixed(1)}s ago</div>
  <div class="kv" style="margin-top:6px">load ${n.load1.toFixed(2)} / ${n.ncpu}</div><div class="meter"><i style="width:${(load*100).toFixed(0)}%"></i></div>
  <div class="kv">memory ${GB(n.mem_total-n.mem_avail)} used of ${GB(n.mem_total)}</div><div class="meter"><i style="width:${(mem*100).toFixed(0)}%"></i></div>
  <div class="row kv"><span>slots ${n.running.length}/${n.max_jobs} · done ${n.done} · failed ${n.failed}</span><span>${btn}</span></div>${run}</div>`}
async function tick(){
  let s; try{s=await (await fetch("/api/state")).json()}catch(e){document.getElementById("sum").textContent="coordinator unreachable";return}
  const c=s.counts, tot=s.tasks.length||1;
  document.getElementById("sum").textContent=`run ${s.run} · up ${s.uptime.toFixed(0)}s · ${c.done}/${s.tasks.length} done · ${c.running} running · ${c.ready} ready · ${c.waiting} waiting · ${c.failed} failed${s.finished?" · FINISHED":""}${s.verified!==null?(s.verified?" · verified ✓":" · VERIFY FAILED"):""}`;
  const seg=(k,col)=>`<i style="width:${100*c[k]/tot}%;background:${col}"></i>`;
  document.getElementById("prog").innerHTML=seg("done","var(--ok)")+seg("running","var(--acc)")+seg("failed","var(--bad)");
  document.getElementById("nodes").innerHTML=s.nodes.map(node).join("")||'<div class="kv">no agents connected yet</div>';
  document.getElementById("sparewrap").style.display=s.spare.length?"":"none";
  document.getElementById("spare").innerHTML=s.spare.map(h=>`<span style="margin-right:14px">${E(h)} <button onclick="act('add','host','${E(h)}')">Add</button></span>`).join("");
  document.getElementById("tasks").innerHTML="<tr><th>task</th><th>op</th><th>inputs</th><th>state</th><th>node</th><th>attempts</th><th>time</th><th>result</th><th>note</th></tr>"+
    s.tasks.map(t=>`<tr><td>${E(t.name)}</td><td>${E(t.op)}</td><td>${E(t.inputs.join(" "))}</td><td class="st-${t.state}">${t.state}</td><td>${E(t.node)}</td><td>${t.attempts}</td><td>${t.duration!==null?t.duration.toFixed(2)+"s":""}</td><td>${E(t.result)}</td><td class="mut">${E(t.note)}</td></tr>`).join("");
  document.getElementById("events").innerHTML=s.events.slice().reverse().map(e=>`<tr class="ev"><td>${e.t.toFixed(1)}s</td><td>${E(e.kind)}</td><td style="white-space:normal">${E(e.msg)}</td></tr>`).join("");
}
tick();setInterval(tick,1000);
</script></body></html>
"""

end # module
