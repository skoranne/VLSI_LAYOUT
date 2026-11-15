#!/usr/bin/env julia
# Hierarchical SR+ImageProc datapath targeting >=1M mapped Sky130 cells.
# Generates replicated engine submodules with unique coefficients + top-level integration.

function coeff(i, f, seed, taps)
    v = ((i*1103515245 + f*12345 + seed*2654435761 + 1013904223) % taps) - (taps ÷ 2)
    v == 0 ? 1 : v
end

function gen_engine(io, ename; W=8, CHANNELS=1, IMGW=32, PE_ROWS=128, PE_COLS=128, FILTERS=4, seed=0)
    total_pes = PE_ROWS * PE_COLS * FILTERS
    pes_per_filter = PE_ROWS * PE_COLS
    LB = IMGW
    AW = max(1, ceil(Int, log2(LB)))
    ACCW = 2*W + 20
    println(io, "module $ename(")
    println(io, "  input clk, input rst_n,")
    println(io, "  input in_valid,")
    println(io, "  input [$(W*CHANNELS-1):0] pixel_in,")
    println(io, "  output reg [$(W*CHANNELS-1):0] pixel_out,")
    println(io, "  output reg out_valid")
    println(io, ");")
    for ch in 0:CHANNELS-1
        println(io, "  reg signed [$(W-1):0] lb0_c$(ch) [0:$(LB-1)];")
        println(io, "  reg signed [$(W-1):0] lb1_c$(ch) [0:$(LB-1)];")
        println(io, "  reg signed [$(W-1):0] lb2_c$(ch) [0:$(LB-1)];")
    end
    println(io, "  reg [$(AW)-1:0] wptr;")
    for f in 0:FILTERS-1
        for i in 0:pes_per_filter-1
            gid = f*pes_per_filter + i
            addr = (i*7 + f*13 + 3 + seed) % LB
            ch = (i + f) % CHANNELS
            lbsel = i % 3
            cval = coeff(i, f, seed, 17)
            ac = abs(cval)
            mexpr = cval >= 0 ? "\$signed($(W+1)'sd$cval)" : "-\$signed($(W+1)'sd$ac)"
            println(io, "  wire signed [$(2*W):0] pe_mul_$(gid) = \$signed(lb$(lbsel)_c$(ch)[$addr]) * $mexpr;")
        end
    end
    gsz = 32
    ng = cld(pes_per_filter, gsz)
    for f in 0:FILTERS-1
        base = f*pes_per_filter
        for g in 0:ng-1
            s = g*gsz
            e = min((g+1)*gsz-1, pes_per_filter-1)
            terms = join(["\$signed({{$(ACCW-2*W-1){pe_mul_$(base+j)[$(2*W)]}}, pe_mul_$(base+j)})" for j in s:e], " + ")
            println(io, "  wire signed [$(ACCW):0] f$(f)_g$(g) = $terms;")
        end
        gterms = join(["f$(f)_g$(g)" for g in 0:ng-1], " + ")
        println(io, "  wire signed [$(ACCW):0] f$(f)_sum = $gterms;")
        println(io, "  reg signed [$(ACCW):0] f$(f)_acc;")
    end
    println(io, "  wire signed [$(ACCW):0] up_interp = f0_sum + (f1_sum >>> 1);")
    println(io, "  wire signed [$(ACCW):0] edge_enh = up_interp + f2_sum - (f0_sum >>> 2) + (f$(FILTERS-1)_sum >>> 3);")
    println(io, "  reg signed [$(ACCW):0] out_acc;")
    println(io, "  always @(posedge clk or negedge rst_n) begin")
    println(io, "    if(!rst_n) begin")
    println(io, "      wptr <= 0; out_valid <= 0; pixel_out <= 0; out_acc <= 0;")
    for f in 0:FILTERS-1; println(io, "      f$(f)_acc <= 0;"); end
    println(io, "    end else begin")
    println(io, "      out_valid <= in_valid;")
    println(io, "      if(in_valid) begin")
    for ch in 0:CHANNELS-1
        println(io, "        lb2_c$(ch)[wptr] <= lb1_c$(ch)[wptr];")
        println(io, "        lb1_c$(ch)[wptr] <= lb0_c$(ch)[wptr];")
        println(io, "        lb0_c$(ch)[wptr] <= \$signed(pixel_in[$((ch+1)*W-1):$(ch*W)]);")
    end
    println(io, "        wptr <= wptr + 1'b1;")
    for f in 0:FILTERS-1; println(io, "        f$(f)_acc <= f$(f)_sum;"); end
    println(io, "        out_acc <= edge_enh;")
    println(io, "        pixel_out <= out_acc[$(W-1):0];")
    println(io, "      end")
    println(io, "    end")
    println(io, "  end")
    println(io, "endmodule")
    return total_pes
end

function gen_top(; name="SRImageProc1M", NENG=10, cfg=(W=8, CHANNELS=1, IMGW=32, PE_ROWS=128, PE_COLS=128, FILTERS=4))
    W = cfg.W; CHANNELS = cfg.CHANNELS
    io = IOBuffer()
    total = 0
    for e in 1:NENG
        ename = "SREngine$(e)"
        total += gen_engine(io, ename; W=cfg.W, CHANNELS=cfg.CHANNELS, IMGW=cfg.IMGW, PE_ROWS=cfg.PE_ROWS, PE_COLS=cfg.PE_COLS, FILTERS=cfg.FILTERS, seed=e*7919)
        println(io, "")
    end
    # top integrates outputs (xor/reduce) so engines aren't optimized away
    println(io, "module $name(")
    println(io, "  input clk, input rst_n,")
    println(io, "  input in_valid,")
    println(io, "  input [$(W*CHANNELS-1):0] pixel_in,")
    println(io, "  output reg [$(W*CHANNELS-1):0] pixel_out,")
    println(io, "  output reg out_valid")
    println(io, ");")
    for e in 1:NENG
        println(io, "  wire [$(W*CHANNELS-1):0] px_$e; wire ov_$e;")
        println(io, "  SREngine$e u$e(.clk(clk),.rst_n(rst_n),.in_valid(in_valid),.pixel_in(pixel_in),.pixel_out(px_$e),.out_valid(ov_$e));")
    end
    parts = join(["px_$e" for e in 1:NENG], " ^ ")
    ovparts = join(["ov_$e" for e in 1:NENG], " | ")
    println(io, "  always @(posedge clk or negedge rst_n) begin")
    println(io, "    if(!rst_n) begin pixel_out <= 0; out_valid <= 0; end")
    println(io, "    else begin pixel_out <= $parts; out_valid <= $ovparts; end")
    println(io, "  end")
    println(io, "endmodule")
    return (verilog=String(take!(io)), total_pes=total)
end

if abspath(PROGRAM_FILE) == @__FILE__
    function main()
        # 10 engines * 65536 PEs = 655360 PEs, ~1.14M cells expected
        n = 10
        cfg = (W=8, CHANNELS=1, IMGW=32, PE_ROWS=128, PE_COLS=128, FILTERS=4)
        out = joinpath(@__DIR__, "sr_imageproc_1m_hier.v")
        i = 1
        while i <= length(ARGS)
            a = ARGS[i]
            if a == "--n" && i < length(ARGS); i+=1; n=parse(Int,ARGS[i])
            elseif a == "-o" && i < length(ARGS); i+=1; out=ARGS[i]
            end
            i += 1
        end
        res = gen_top(; name="SRImageProc1M", NENG=n, cfg=cfg)
        open(out,"w") do f; write(f,res.verilog); end
        println("wrote $out engines=$n total_pes=$(res.total_pes) bytes=$(length(res.verilog))")
    end
    main()
end
