#!/usr/bin/env julia
# TRS DAG optimizer for CTR files. Reads relations JSON, builds DAG, applies rewrites.

struct Rule
    name::String
    lhs::String
    rhs::String
    kind::String
end

mutable struct Node
    id::Int
    line::String
    op::String
    a::String
    b::String
    lhs::String
    zone::Int
    dead::Bool
end

function load_rules(path)
    txt = read(path,String)
    rules = Rule[]
    for m in eachmatch(r"\{[^\}]+\}", txt)
        blk = m.match
        nm = match(r"\"name\"\s*:\s*\"([^\"]+)\"", blk)
        lhs = match(r"\"lhs\"\s*:\s*\"([^\"]+)\"", blk)
        rhs = match(r"\"rhs\"\s*:\s*\"([^\"]+)\"", blk)
        kind = match(r"\"kind\"\s*:\s*\"([^\"]+)\"", blk)
        if nm!==nothing && lhs!==nothing && rhs!==nothing && kind!==nothing
            push!(rules, Rule(nm.captures[1], lhs.captures[1], rhs.captures[1], kind.captures[1]))
        end
    end
    return rules
end

function parse_ctr(path)
    lines = readlines(path)
    nodes = Node[]
    zone = 0
    for (i,ln) in enumerate(lines)
        s = strip(ln)
        if occursin(r"(?i)^START\s+CONNECT\s+ZONE\s+\d+", s)
            m = match(r"(?i)^START\s+CONNECT\s+ZONE\s+(\d+)", s); zone = parse(Int,m.captures[1]); continue
        elseif occursin(r"(?i)^END\s+CONNECT\s+ZONE\s+\d+", s)
            zone = 0; continue
        end
        m = match(r"^exec\s+run\s+(\S+)\s*=\s*(\S+)\s+(\S+)\s+(\S+)"i, s)
        if m !== nothing
            lhs,a,op,b = m.captures
            push!(nodes, Node(length(nodes)+1, ln, uppercase(String(op)), String(a), String(b), String(lhs), zone, false))
        end
    end
    return lines, nodes
end

# normalize expr string
norm(x) = uppercase(replace(strip(x), r"\s+"=>" "))

function expr(n::Node)
    return norm("$(n.a) $(n.op) $(n.b)")
end

# simple pattern matching for a few rules
function apply_rules!(nodes, rules)
    changed = 0
    for n in nodes
        n.dead && continue
        n.zone > 0 && continue  # conservative: no rewrites inside connectivity zones
        e = expr(n)
        # A NOT A / A XOR A -> 0 (net-safe: result empty)
        if n.a == n.b && n.op in ["NOT","@","XOR","^"]
            n.op="CONST0"; n.a="0"; n.b="0"; changed+=1; continue
        end
        # A AND A / A OR A -> A (geometry idempotent; net-id may change - skip in zones handled above)
        if n.a == n.b && n.op in ["AND","*","OR","+"]
            n.op="COPY"; n.b="nothing"; changed+=1; continue
        end
    end
    # second pass: definition lookup (last definition wins)
    defop = Dict{String,Node}()
    for n in nodes; defop[n.lhs]=n; end
    for n in nodes
        n.dead && continue
        n.zone > 0 && continue
        if n.op in ["AND","*"]
            # n.a = X, n.b = Y where Y is (Z NOT X)
            if haskey(defop, n.b)
                d = defop[n.b]
                if d.op in ["NOT","@"] && d.b == n.a
                    n.op="CONST0"; n.a="0"; n.b="0"; changed+=1
                end
            end
            if haskey(defop, n.a)
                d = defop[n.a]
                if d.op in ["NOT","@"] && d.b == n.b
                    n.op="CONST0"; n.a="0"; n.b="0"; changed+=1
                end
            end
        end
        # INSIDE/OUTSIDE same args -> 0
        if n.op in ["AND","*"] && haskey(defop,n.a) && haskey(defop,n.b)
            da,db = defop[n.a],defop[n.b]
            if da.a==db.a && da.b==db.b && ((da.op=="INSIDE" && db.op=="OUTSIDE") || (da.op=="OUTSIDE" && db.op=="INSIDE"))
                n.op="CONST0"; n.a="0"; n.b="0"; changed+=1
            end
        end
        # INSIDE XOR AND is NOT a valid shape-level zero identity; omitted.
        # distributive: (A AND B) OR (A AND C) where seen
    end
    return changed
end

function write_ctr(path, lines, nodes)
    # rebuild: replace exec lines with rewritten versions; keep others
    node_by_line = Dict{String,Node}()
    for n in nodes; node_by_line[n.line]=n; end
    out = String[]
    for ln in lines
        if haskey(node_by_line, ln)
            n = node_by_line[ln]
            if n.op=="CONST0"
                push!(out, "# TRS: rewritten to CONST0 (ALL_ZERO_BY_CONSTRUCTION)\n# $(ln)")
            elseif n.op=="COPY"
                push!(out, "exec run $(n.lhs) = $(n.a) COPY nothing  # TRS")
            else
                push!(out, ln)
            end
        else
            push!(out, ln)
        end
    end
    open(path,"w") do io; for l in out; println(io,l); end; end
end

function main()
    if length(ARGS) < 3
        println("Usage: julia trs_dag.jl relations.json input.ctr output.ctr"); exit(1)
    end
    rules = load_rules(ARGS[1])
    lines, nodes = parse_ctr(ARGS[2])
    before = count(n->!n.dead, nodes)
    zones = length(unique([n.zone for n in nodes if n.zone>0]))
    changed = apply_rules!(nodes, rules)
    after = count(n->!(n.dead || n.op=="CONST0"), nodes)
    write_ctr(ARGS[3], lines, nodes)
    println("Rules loaded: $(length(rules))")
    println("Before TRS ops: $before")
    println("After  TRS ops: $after")
    println("Rewritten/removed: $(before-after)")
    println("Connectivity zones seen: $zones")
    println("TODO: full CONNECT ZONE marker propagation in DAG transforms.")
end

main()
