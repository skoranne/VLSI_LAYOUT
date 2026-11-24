# CtrFront.jl -- CLUSTER mode front-end: run a MAGPARSER .ctr file's layer operations on the
# RPC cluster. Included by mpcoord.jl (uses its MTask/TASKS/TIDX/STORE and event()).
#
# Supported CTR subset (same meaning as MAGPARSER's controlparser.f90):
#   decl design dN = file.mag          flat Magic layout (rect lines, or MAGPARSER's "KLBIN f.bin" layer
#                                      sections); every dN:layer used becomes a source layer
#   decl output oN = file.mag          results named oN:* are written at the end. As in MAGPARSER, if
#                                      file.mag exists and maps layers to "KLBIN f.bin", each result is
#                                      written to its .bin (raw int32 x1 y1 x2 y2); otherwise file.mag
#                                      is written as a flat rect layout
#   decl memory mN = nothing           in-memory results (not written)
#   exec run X:out = A:l1 OP B:l2      OP:  *  AND (intersection)     +  OR  (Boost)
#                                           ^  XOR (Boost)            @  NOT (frame)
#                                           !  NOT (Boost, BOOSTNOT)  ~  FRAMENOT
#                                      words: OR2, AND2 (frame AND), NOT, FRAMENOT, XOR, and the
#                                      topological selects, one or more words (INTERACT,
#                                      NOT INTERACT, COINCIDENT INSIDE EDGE, ...)
#   exec run X:out = A:l1 AND B        single-layer merge of A (CTR word AND is NOT intersection;
#                                      MAGPARSER ignores B), likewise AND nothing
#   exec run X:out = A:l1 EXTENT|COPY|GRID nothing
#   exec run X:out = A:l1 GROW nothing e n w s        (microns, x1000 to DBU as in MAGPARSER)
#   exec run X:out = A:l1 SHRINK nothing d | e n w s
#   exec run X:out = A:l1 SIZE nothing|B:l2 d
#   exec run X:out = A:l1 ASSERT_ZERO nothing          checked on the coordinator
#   var cluster_tile_boxes N   ops whose inputs hold more than N boxes (default 10000000) are
#                              split into y-band tiles that run on different nodes (see mpcoord.jl)
#   var cluster_tiles N        force N tiles for those ops (default: max(ceil(boxes/N), slots))
#   var abort_on_assert_zero N N > 1: a failing ASSERT_ZERO fails the run (as in MAGPARSER)
#   program, systeminfo, info, other vars, exec init, flush, end   accepted, no effect here
# Anything else is rejected with its line number, so a CTR never runs half-understood.

const CTR_SYM = Dict("*" => "AND", "+" => "OR", "^" => "XOR", "@" => "NOT", "!" => "BOOSTNOT", "~" => "FRAMENOT")
const CTR_UNARY = Dict("AND" => "MERGE", "EXTENT" => "EXTENT", "COPY" => "COPY", "GRID" => "GRID",
                       "ASSERT_ZERO" => "ASSERT_ZERO")
const CTR_BINARY = ("OR2", "AND2", "NOT", "FRAMENOT", "XOR")
const CTR_PRECISION = 1000                         # InitPrecision(1000) in controlparser.f90
const CTR_VARS = Dict{String,Int}("cluster_tile_boxes" => 10_000_000, "cluster_tiles" => 0, "abort_on_assert_zero" => 0)
# multi-word topological operators, longest first (same table as controlparser.f90)
const CTR_TOPO_WORDS = sort([split(replace(k, "_" => " ")) for k in keys(NLAYERS) if occursin('_', k) &&
                             !(k in ("ASSERT_ZERO",))]; by = length, rev = true)

"CTR length (microns) -> DBU, truncating like Fortran int(used_precision*r)."
ctr_dbu(x) = trunc(Int64, CTR_PRECISION * parse(Float64, x))
const CTR_OUTPUTS = Dict{String,String}()          # oN => output .mag path
const CTR_TECH = Ref("sky130A")

read_klbin(f) = (b = read(f); length(b) % 16 == 0 || error("$f: KLBIN size $(length(b)) is not a multiple of 16");
                 reshape(collect(ltoh.(reinterpret(Int32, b))), 4, :))
write_klbin(f, b::Matrix{Int32}) = write(f, htol.(vec(b)))

"KLBIN layer sections (layer => .bin path) of a MAGPARSER .mag template; empty if none."
function klbin_sections(path)
    out = Pair{String,String}[]; cur = ""
    isfile(path) || return out
    for l in eachline(path)
        l = strip(l)
        startswith(l, "<<") && (cur = strip(replace(l, "<<" => "", ">>" => "")))
        startswith(l, "KLBIN ") && push!(out, cur => String(strip(l[7:end])))
    end
    out
end

"Read a flat Magic .mag file -> Dict(layer => 4×n Matrix{Int32}), tech name."
function read_mag(path)
    layers = Dict{String,Vector{NTuple{4,Int32}}}(); cur = ""; tech = ""
    bins = Dict{String,Matrix{Int32}}()
    for (ln, raw) in enumerate(eachline(path))
        l = strip(raw)
        if startswith(l, "KLBIN ")
            f = String(strip(l[7:end])); isfile(f) || (f = joinpath(dirname(abspath(path)), f))
            bins[cur] = read_klbin(f)
        elseif startswith(l, "<<")
            cur = strip(replace(l, "<<" => "", ">>" => ""))
        elseif startswith(l, "tech ")
            tech = strip(l[6:end])
        elseif startswith(l, "rect ") && !(cur in ("", "labels", "end", "properties"))
            p = split(l)
            x1, y1, x2, y2 = parse.(Int32, p[2:5])
            (x2 > x1 && y2 > y1) && push!(get!(layers, cur, NTuple{4,Int32}[]), (x1, y1, x2, y2))
        elseif startswith(l, "use ")
            error("$path:$ln: hierarchical .mag (use $(split(l)[2])) is not supported in CLUSTER mode; flatten it first")
        end
    end
    d = Dict(k => reshape(collect(reinterpret(Int32, v)), 4, length(v)) for (k, v) in layers)
    merge!(d, bins), tech
end

"Write layers (name => 4×n Matrix{Int32}) as a flat Magic .mag file."
function write_mag(path, layers::Vector{Pair{String,Matrix{Int32}}}, tech)
    open(path, "w") do io
        println(io, "magic"); println(io, "tech ", tech); println(io, "timestamp ", round(Int, time()))
        for (name, b) in layers
            size(b, 2) == 0 && continue
            println(io, "<< ", name, " >>")
            for i in 1:size(b, 2)
                println(io, "rect ", b[1, i], " ", b[2, i], " ", b[3, i], " ", b[4, i])
            end
        end
        println(io, "<< end >>")
    end
end

function ctr_text(path)
    txt = read(path, String)
    occursin("include(", txt) || return txt
    read(Cmd(`m4 $path`; dir = dirname(abspath(path))), String)     # same expansion MAGPARSER does
end

ctr_resolve(f, ctrdir) = isabspath(f) || isfile(f) ? f : joinpath(ctrdir, f)

function add_task!(name, op, inputs, params, pstr)
    haskey(TIDX, name) && error("$name is defined twice (CLUSTER mode needs single assignment)")
    push!(TASKS, MTask(name, op, inputs, params, pstr, :waiting, "", 0, 0, "", "", String[], "", 0, 0.0,
                       nothing, 0.0, "", nothing, ""))
    TIDX[name] = length(TASKS)
end

"Parse a .ctr file into cluster tasks. Source layers are read and encoded right here."
function parse_ctr(path)
    ctrdir = dirname(abspath(path))
    designs = Dict{String,Dict{String,Matrix{Int32}}}()
    nbool = 0
    for (ln, raw) in enumerate(split(ctr_text(path), '\n'))
        line = strip(split(raw, '#')[1]); isempty(line) && continue
        w = split(line); kw = lowercase(w[1])
        where = "$path:$ln"
        if kw == "var" && length(w) >= 3 && haskey(CTR_VARS, w[2])
            CTR_VARS[w[2]] = parse(Int, w[3]); event("ctr", "var $(w[2]) = $(w[3])")
            continue
        elseif kw in ("program", "systeminfo", "info", "var", "flush", "end")
            continue
        elseif kw == "decl"
            (length(w) == 5 && w[4] == "=") || error("$where: expected 'decl KIND NAME = FILE'")
            kind, ns, f = lowercase(w[2]), String(w[3]), String(w[5])
            if kind == "design"
                fp = ctr_resolve(f, ctrdir)
                lay, tech = read_mag(fp)
                isempty(tech) || (CTR_TECH[] = tech)
                designs[ns] = lay
                event("ctr", @sprintf("design %s = %s: %d layers, %d rects", ns, fp, length(lay), sum(size(b, 2) for b in values(lay); init = 0)))
            elseif kind == "output"
                CTR_OUTPUTS[ns] = isabspath(f) ? f : joinpath(pwd(), f)
            elseif kind == "memory"
            else
                error("$where: decl $kind is not supported in CLUSTER mode")
            end
        elseif kw == "exec" && length(w) >= 2 && lowercase(w[2]) == "init"
            continue
        elseif kw == "exec" && length(w) >= 2 && lowercase(w[2]) == "run"
            tok = split(replace(line, r"\s*=\s*" => " = "; count = 1))
            (length(tok) >= 6 && tok[4] == "=") ||
                error("$where: not supported in CLUSTER mode (expected 'exec run X:out = A:l1 OP B:l2'): $line")
            out, a = String(tok[3]), String(tok[5]); r = String.(tok[6:end]); ur = uppercase.(r)
            occursin(':', out) || error("$where: result must be NS:layer, got $out")
            inputs = String[]; params = Int64[]
            function src(x)                                    # dN:layer -> source task, else earlier result
                haskey(TIDX, x) && return x
                occursin(':', x) || error("$where: $x is not a NS:layer name")
                ns, lay = split(x, ':'; limit = 2)
                haskey(designs, ns) || error("$where: $x is neither a design layer nor an earlier result")
                haskey(designs[ns], lay) || event("warn", "$where: $x is not in the design, using an empty layer")
                bx = get(designs[ns], lay, Matrix{Int32}(undef, 4, 0))
                blob = encode_layer(bx); sha = bytes2hex(sha256(blob)); STORE[sha] = blob
                add_task!(x, "src", String[], Int64[], "")
                t = TASKS[end]; t.state = :done; t.node = "coordinator"; t.result_sha = sha
                t.n_out = size(bx, 2); t.duration = 0.0
                event("src", @sprintf("%s: %d rects -> %d B CLW1", x, size(bx, 2), length(blob)))
                x
            end
            isnothing_(x) = x in ("nothing", "t1:nothing")
            nargs(k) = length(r) - 2 >= k || error("$where: $(ur[1]) needs $k value(s): $line")
            topo = findfirst(ws -> length(r) > length(ws) && ur[1:length(ws)] == ws, CTR_TOPO_WORDS)
            if haskey(CTR_SYM, r[1])                           # single-character operator
                length(r) == 2 || error("$where: expected 'A OP B': $line")
                rop = CTR_SYM[r[1]]; push!(inputs, src(a), src(r[2]))
            elseif topo !== nothing                            # NOT INTERACT, COINCIDENT INSIDE EDGE, ...
                ws = CTR_TOPO_WORDS[topo]
                rop = join(ws, "_"); push!(inputs, src(a), src(r[length(ws) + 1]))
            elseif length(r) >= 2 && ur[1] == "AND"            # CTR word AND = merge of A alone
                rop = "MERGE"; push!(inputs, src(a))
            elseif length(r) >= 2 && haskey(CTR_UNARY, ur[1])
                isnothing_(r[2]) || error("$where: $(ur[1]) needs 'nothing' as second layer")
                rop = CTR_UNARY[ur[1]]; push!(inputs, src(a))
            elseif length(r) >= 2 && ur[1] == "GROW"
                isnothing_(r[2]) || error("$where: GROW needs 'nothing' as second layer"); nargs(4)
                rop = "GROW"; push!(inputs, src(a)); params = ctr_dbu.(r[3:6])
            elseif length(r) >= 2 && ur[1] == "SHRINK"
                isnothing_(r[2]) || error("$where: SHRINK needs 'nothing' as second layer"); nargs(1)
                rop = "SHRINK"; push!(inputs, src(a))
                params = length(r) >= 6 ? ctr_dbu.(r[3:6]) : [ctr_dbu(r[3])]
            elseif length(r) >= 2 && ur[1] == "SIZE"
                nargs(1); push!(inputs, src(a)); params = [ctr_dbu(r[3])]
                rop = isnothing_(r[2]) ? "SIZE" : (push!(inputs, src(r[2])); "SIZE2")
            elseif length(r) == 2 && (ur[1] in CTR_BINARY ||                  # MAGPARSER has no word OR/BOOSTNOT
                                      (get(NLAYERS, ur[1], 0) == 2 && !(ur[1] in ("OR", "BOOSTNOT", "SIZE2"))))
                rop = ur[1]; push!(inputs, src(a), src(r[2]))
            else
                error("$where: operator '$(join(r, " "))' is not supported in CLUSTER mode")
            end
            rop in ("AND", "OR", "OR2", "XOR", "NOT", "BOOSTNOT") && (nbool += 1)
            # label: the CTR spelling when it differs (e.g. "(*)"), plus GROW/SHRINK/SIZE values
            pstr = rop in ("GROW", "SHRINK", "SIZE", "SIZE2") ? join(r[3:end], " ") :
                   rop == ur[1] ? "" : "(" * (haskey(CTR_SYM, r[1]) ? r[1] : join(r[1:end - 1], " ")) * ")"
            add_task!(out, rop, inputs, params, pstr)
        else
            error("$where: not supported in CLUSTER mode: $line")
        end
    end
    nbool
end

"Write every oN:* result to its declared output (KLBIN template .mag, or a flat rect .mag)."
function write_ctr_outputs()
    for (ns, f) in CTR_OUTPUTS
        lay = Pair{String,Matrix{Int32}}[]
        for t in TASKS
            startswith(t.name, ns * ":") && !(t.op in LOCAL_OPS) && !startswith(t.op, "TILE_") && t.state == :done || continue
            push!(lay, split(t.name, ':'; limit = 2)[2] => decode_layer(STORE[t.result_sha]))
        end
        secs = klbin_sections(f)
        if isempty(secs)
            write_mag(f, lay, CTR_TECH[])
            event("output", @sprintf("%s: %d layers, %d rects -> %s", ns, length(lay), sum(size(b, 2) for (_, b) in lay; init = 0), f))
        else
            res = Dict(lay); nw = 0
            for (name, bin) in secs
                haskey(res, name) || continue
                write_klbin(isabspath(bin) ? bin : joinpath(dirname(f), bin), res[name]); nw += 1
            end
            miss = setdiff(first.(lay), first.(secs))
            event("output", "$ns: $nw layers written as KLBIN per $f" *
                  (isempty(miss) ? "" : "; not in the template (kept in memory): $(join(miss, " "))"))
        end
    end
end
