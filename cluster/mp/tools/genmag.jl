#!/usr/bin/env julia
# genmag.jl -- deterministic flat Magic .mag test layout for CLUSTER mode.
#   julia tools/genmag.jl OUT.mag [n_per_layer=200000] [extent=2000000]
# Layers metal1, metal2 (maxsize 800) and via1 (maxsize 60), made with DagRef.gen (same
# generator as the DAG 'gen' op), so the same arguments always give the same file.
include(joinpath(@__DIR__, "..", "..", "dagref.jl"))
using .DagRef

out = ARGS[1]
n = length(ARGS) >= 2 ? parse(Int64, ARGS[2]) : 200_000
extent = length(ARGS) >= 3 ? parse(Int64, ARGS[3]) : 2_000_000
open(out, "w") do io
    println(io, "magic\ntech sky130A\ntimestamp 0")
    for (layer, seed, maxsize, nn) in (("metal1", 11, 800, n), ("metal2", 12, 800, n), ("via1", 13, 60, n ÷ 2))
        b = DagRef.gen(seed, nn, extent, maxsize)
        println(io, "<< ", layer, " >>")
        for i in 1:size(b, 2)
            println(io, "rect ", b[1, i], " ", b[2, i], " ", b[3, i], " ", b[4, i])
        end
    end
    println(io, "<< end >>")
end
println("wrote $out: metal1/metal2 $n rects, via1 $(n ÷ 2) rects, extent $extent")
