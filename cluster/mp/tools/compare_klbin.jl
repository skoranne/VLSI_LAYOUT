#!/usr/bin/env julia
# compare_klbin.jl -- check CLUSTER-mode KLBIN outputs against a plain MAGPARSER run.
#   julia tools/compare_klbin.jl CLUSTER_DIR TRUTH_DIR [MAGPARSER.exe]
# 1. Every *.bin in CLUSTER_DIR is compared with TRUTH_DIR as a sorted box set. CLW1 is
#    Morton-ordered, so the file bytes differ from MAGPARSER's even when the boxes are equal.
#    MAGPARSER does not write empty layers, so a missing truth file counts as empty.
# 2. Layers whose box sets differ are checked for REGION equality with MAGPARSER itself:
#    XOR(cluster, truth) must be empty. Some ops (frame NOT) split the same region into
#    different rectangles depending on the input order. Needs the MAGPARSER.exe argument.
#    A negative control (XOR of two different layers) shows the check can fail.
# Exit 1 if any layer differs as a region (or as a box set, when no MAGPARSER is given).
rd(f) = isfile(f) ? (b = reinterpret(Int32, read(f)); sort([Tuple(b[i:i+3]) for i in 1:4:length(b)])) : NTuple{4,Int32}[]
cdir, tdir = abspath(ARGS[1]), abspath(ARGS[2]); mag = length(ARGS) >= 3 ? abspath(ARGS[3]) : ""
names = sort(filter(x -> endswith(x, ".bin"), readdir(cdir)))
diff = String[]
for f in names
    a = rd(joinpath(cdir, f)); t = rd(joinpath(tdir, f))
    a == t || push!(diff, f)
    println(rpad(f, 22), lpad(length(a), 9), "  ", a == t ? "same boxes as plain MAGPARSER" :
            "box sets differ ($(length(t)) boxes in truth)")
end

"XOR each (a, b) .bin pair with MAGPARSER; returns the box counts of the XOR results."
function region_xor(pairs)
    dir = mktempdir(); n = length(pairs)
    sec(io, i, f) = (println(io, "<< L$i >>"); println(io, "KLBIN ", f))
    for (file, k) in (("a.mag", 1), ("b.mag", 2))
        open(joinpath(dir, file), "w") do io
            println(io, "magic\ntech sky130A"); for (i, p) in enumerate(pairs); sec(io, i, p[k]); end; println(io, "<< end >>")
        end
    end
    open(joinpath(dir, "x.mag"), "w") do io
        println(io, "magic\ntech sky130A"); for i in 1:n; sec(io, i, "x$i.bin"); end; println(io, "<< end >>")
    end
    open(joinpath(dir, "x.ctr"), "w") do io
        println(io, "program region_check\ndecl design d1 = a.mag\ndecl design d2 = b.mag\ndecl output o1 = x.mag")
        for i in 1:n; println(io, "exec run o1:L$i = d1:L$i ^ d2:L$i"); end
        println(io, "flush\nend")
    end
    log = joinpath(dir, "log")
    ok = success(pipeline(Cmd(`$mag x.ctr $(max(20, 2n + 2))`; dir = dir,
                                 env = merge(ENV, Dict("MAGPARSER_GPU_MEM_BUDGET" => "1"))), stdout = log, stderr = log))
    ok || error("MAGPARSER region check failed:\n" * read(log, String)[max(1, end - 2000):end])
    r = [isfile(joinpath(dir, "x$i.bin")) ? filesize(joinpath(dir, "x$i.bin")) ÷ 16 : 0 for i in 1:n]
    rm(dir; recursive = true); r
end

bad = 0
if !isempty(diff) && !isempty(mag)
    empty_bin = tempname() * ".bin"; write(empty_bin, UInt8[])
    tf(f) = (p = joinpath(tdir, f); isfile(p) ? p : empty_bin)
    # negative control: two different non-empty layers must give a non-empty XOR
    ne = filter(f -> filesize(joinpath(cdir, f)) > 0, names)
    ctl = length(ne) >= 2 ? [(joinpath(cdir, ne[1]), joinpath(cdir, ne[2]))] : Tuple{String,String}[]
    r = region_xor(vcat([(joinpath(cdir, f), tf(f)) for f in diff], ctl))
    rm(empty_bin; force = true)
    println("region check with $(basename(mag)) (XOR cluster ^ plain must be empty):")
    for (i, f) in enumerate(diff)
        global bad += r[i] == 0 ? 0 : 1
        println("  ", rpad(f, 20), r[i] == 0 ? "same region (different rectangle split)" : "REGION DIFFERS: XOR has $(r[i]) boxes")
    end
    isempty(ctl) || println("  negative control $(ne[1]) ^ $(ne[2]): $(r[end]) boxes ",
                            r[end] > 0 ? "(check is live)" : "(WARNING: control is empty, check proves nothing)")
else
    bad = length(diff)
end
println(bad == 0 ? "PASSED: $(length(names)) layers equal to plain MAGPARSER ($(length(names) - length(diff)) identical box sets, $(length(diff)) equal regions)" :
                   "FAILED: $bad of $(length(names)) layers differ")
exit(bad == 0 ? 0 : 1)
