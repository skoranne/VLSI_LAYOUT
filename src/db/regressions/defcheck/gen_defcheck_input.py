# KLayout (Python) generator for the defcheck regression. Run:
#   klayout -b -r gen_defcheck_input.py -rd outdir=.
#
# 1. TinySoC mutations (data/tiny_soc.oas + data/tiny_soc.def.gz, a SKY130 LibreLane design):
#    tiny_mut.oas  - the M1M2_PR via cut of net _002_ at (31970,48110) removed (open) and an
#                    L-shaped met2 strap from _002_'s met2 wire to _003_'s (short)
#    tiny_mut.def  - net _001_'s met1 wire moved off the die (miss)
# 2. Synthetic designs for ASAP7 and GT2N (no routed designs of those exist here): a LEF, a DEF
#    and the matching OASIS, with layer numbers transcribed from the PDKs' own maps (NOT from
#    MAGPARSER's tech_stacks.f90, so a wrong built-in number shows up as misses):
#      ASAP7: asap7_pdk_r1p7/cdslib/asap7_TechLib_10/asap7_TechLib.layermap and
#             OpenROAD-flow-scripts platforms/asap7/KLayout/asap7.lyt (M1 19 ... V8 85)
#      GT2N : GT2N techlib/gt2_techfile.layermap and platform gt2n.lyt (M0 20 ... BRDL 1000,
#             connection BPR,VBPR,M0)
#    Each has ladder nets climbing every routing layer (vias stated on the lower or the upper
#    layer, one via from the DEF VIAS section), two nets crossing on adjacent layers without a
#    via, an isolated wire, and a special net with stripes, a via and a RECT.
#    ASAP7: DEF 4000 units/um on a 4000/um layout; GT2N: DEF 2000/um on a 4000/um layout.
import pya, gzip, os

outdir = globals().get("outdir", ".")
here = os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else "."
datadir = globals().get("datadir", os.path.join(outdir, "data"))

# ---------------------------------------------------------------- 1. TinySoC mutations
ly = pya.Layout()
ly.read(os.path.join(datadir, "tiny_soc.oas"))
top = ly.top_cell()
ly.flatten(top.cell_index(), -1, True)
via = ly.find_layer(68, 44)
met2 = ly.find_layer(69, 20)
pt = pya.Point(31970, 48110)
removed = 0
for s in list(top.shapes(via).each()):
    if s.polygon.inside(pt) or s.bbox().contains(pt):
        top.shapes(via).erase(s)
        removed += 1
assert removed >= 1, "via of _002_ not found"
# strap: _002_ met2 runs x=31970, y 48110..51170; _003_ met2 runs x=8510, y 66810..68510
top.shapes(met2).insert(pya.Box(31900, 50000, 32040, 67070))
top.shapes(met2).insert(pya.Box(8440, 66930, 32040, 67070))
ly.write(os.path.join(outdir, "tiny_mut.oas"))
src = gzip.open(os.path.join(datadir, "tiny_soc.def.gz"), "rt").read()
old = "+ ROUTED met1 ( 43470 55250 ) ( 46230 * )"
assert src.count(old) == 1, "net _001_ wire not found"
open(os.path.join(outdir, "tiny_mut.def"), "w").write(src.replace(old, "+ ROUTED met1 ( 43470 200000 ) ( 46230 * )"))
print("tiny_mut: removed %d via shapes, strap added, _001_ wire moved" % removed)

# ---------------------------------------------------------------- 2. synthetic stacks
ASAP7 = dict(name="asap7", def_units=4000, layout_per_um=4000,
             routing=[("M1", 19), ("M2", 20), ("M3", 30), ("M4", 40), ("M5", 50), ("M6", 60), ("M7", 70), ("M8", 80), ("M9", 90)],
             cuts=[("V1", 21), ("V2", 25), ("V3", 35), ("V4", 45), ("V5", 55), ("V6", 65), ("V7", 75), ("V8", 85)])
GT2N_BACK = [("BRDL", 1000), ("BM4", 980), ("BM3", 960), ("BM2", 940), ("BM1", 920), ("BPR", 8)]
GT2N_BACK_CUTS = [("BV4", 990), ("BV3", 970), ("BV2", 950), ("BV1", 930), ("BV0", 910)]
GT2N_FRONT = [("M0", 20)] + [("M%d" % i, 20 + 5 * i) for i in range(1, 14)] + [("RDL", 90)]
GT2N_FRONT_CUTS = [("V%d" % i, 22 + 5 * i) for i in range(0, 14)]
GT2N = dict(name="gt2n", def_units=2000, layout_per_um=4000,
            routing=GT2N_BACK + GT2N_FRONT, cuts=GT2N_BACK_CUTS + [("VBPR", 9)] + GT2N_FRONT_CUTS)


def synth(st):
    R, C = st["routing"], st["cuts"]
    assert len(C) == len(R) - 1
    du = st["def_units"]
    f = st["layout_per_um"] // du                 # DEF unit -> layout unit
    W, E, CUT, L = 40, 20, 10, 800                # wire width, via enclosure, cut half size, segment
    nR = len(R)
    name = "synth_" + st["name"]
    # --- LEF
    lef = ["VERSION 5.8 ;", 'BUSBITCHARS "[]" ;', 'DIVIDERCHAR "/" ;',
           "UNITS", "  DATABASE MICRONS %d ;" % du, "END UNITS", "MANUFACTURINGGRID %.6f ;" % (1.0 / du)]
    for i, (rn, _) in enumerate(R):
        lef += ["LAYER %s" % rn, "  TYPE ROUTING ;", "  DIRECTION %s ;" % ("HORIZONTAL" if i % 2 == 0 else "VERTICAL"),
                "  PITCH %.6f ;" % (4.0 * W / du), "  WIDTH %.6f ;" % (W / du), "END %s" % rn]
        if i < len(C):
            lef += ["LAYER %s" % C[i][0], "  TYPE CUT ;", "END %s" % C[i][0]]
    um = lambda v: "%.6f" % (v / du)
    for i in range(nR - 1):
        b, c, t = R[i][0], C[i][0], R[i + 1][0]
        lef += ["VIA VIA_%s_%s DEFAULT" % (b, t),
                "  LAYER %s ;" % b, "    RECT %s %s %s %s ;" % (um(-E), um(-E), um(E), um(E)),
                "  LAYER %s ;" % c, "    RECT %s %s %s %s ;" % (um(-CUT), um(-CUT), um(CUT), um(CUT)),
                "  LAYER %s ;" % t, "    RECT %s %s %s %s ;" % (um(-E), um(-E), um(E), um(E)),
                "END VIA_%s_%s" % (b, t)]
    lef += ["END LIBRARY"]
    open(os.path.join(outdir, name + ".lef"), "w").write("\n".join(lef) + "\n")

    # --- geometry + DEF
    out = pya.Layout()
    out.dbu = 1.0 / st["layout_per_um"]
    cell = out.create_cell(name.upper())
    lr = [out.layer(gl, 0) for _, gl in R]
    lc = [out.layer(gl, 0) for _, gl in C]

    def wire(i, p, q, w=W, ext=True):
        e = w // 2 if ext else 0
        x1, x2 = min(p[0], q[0]), max(p[0], q[0])
        y1, y2 = min(p[1], q[1]), max(p[1], q[1])
        if y1 == y2:
            bx = pya.Box((x1 - e) * f, (y1 - w // 2) * f, (x2 + e) * f, (y1 + w // 2) * f)
        else:
            bx = pya.Box((x1 - w // 2) * f, (y1 - e) * f, (x1 + w // 2) * f, (y2 + e) * f)
        cell.shapes(lr[i]).insert(bx)

    def via_geom(i, p):
        for li, h in ((lr[i], E), (lc[i], CUT), (lr[i + 1], E)):
            cell.shapes(li).insert(pya.Box((p[0] - h) * f, (p[1] - h) * f, (p[0] + h) * f, (p[1] + h) * f))

    nets = []
    SX = (nR + 2) * L
    for k in range(3):                             # ladders
        x, y = 2000 + k * SX, 2000
        stmts = []
        for i in range(nR):
            q = (x + L, y) if i % 2 == 0 else (x, y + L)
            wire(i, (x, y), q)
            stmts.append("%s ( %d %d ) ( %s %s )" % (R[i][0], x, y, q[0] if q[0] != x else "*", q[1] if q[1] != y else "*"))
            if i < nR - 1:
                via_geom(i, q)
                if k == 0 and i == 0:
                    stmts.append("%s ( %d %d ) DVIA" % (R[i][0], q[0], q[1]))
                elif (i + k) % 2 == 0:
                    stmts.append("%s ( %d %d ) VIA_%s_%s" % (R[i][0], q[0], q[1], R[i][0], R[i + 1][0]))
                else:                              # stated on the upper layer
                    stmts.append("%s ( %d %d ) VIA_%s_%s" % (R[i + 1][0], q[0], q[1], R[i][0], R[i + 1][0]))
            x, y = q
        nets.append(("lad%d" % k, stmts))
    # crossing nets on routing layers 0/1, no via
    XC, YC = 2000, 2000 + (nR + 2) * L
    wire(0, (XC - L, YC), (XC + L, YC))
    wire(1, (XC, YC - L), (XC, YC + L))
    nets.append(("xa", ["%s ( %d %d ) ( %d * )" % (R[0][0], XC - L, YC, XC + L)]))
    nets.append(("xb", ["%s ( %d %d ) ( * %d )" % (R[1][0], XC, YC - L, YC + L)]))
    # isolated wire on the top layer
    ti = nR - 1
    p, q = (XC + 3 * L, YC), (XC + 3 * L, YC + L) if ti % 2 else (XC + 4 * L, YC)
    wire(ti, p, q)
    nets.append(("iso", ["%s ( %d %d ) ( %s %s )" % (R[ti][0], p[0], p[1], q[0] if q[0] != p[0] else "*", q[1] if q[1] != p[1] else "*")]))
    # special net: stripe on layer 0, via up, stripe on layer 1, a RECT on layer 1
    SW = 60
    a, b, c2 = (6000, YC), (7600, YC), (7600, YC + L)
    wire(0, a, b, SW, ext=False)
    via_geom(0, b)
    wire(1, b, c2, SW, ext=False)
    rect = (7400, YC + L, 7800, YC + L + 400)
    cell.shapes(lr[1]).insert(pya.Box(rect[0] * f, rect[1] * f, rect[2] * f, rect[3] * f))
    diex, diey = 2000 + 3 * SX + 2000, YC + 4 * L

    d = ["VERSION 5.8 ;", 'DIVIDERCHAR "/" ;', 'BUSBITCHARS "[]" ;', "DESIGN %s ;" % name,
         "UNITS DISTANCE MICRONS %d ;" % du, "DIEAREA ( 0 0 ) ( %d %d ) ;" % (diex, diey),
         "VIAS 1 ;",
         "    - DVIA + RECT %s ( %d %d ) ( %d %d ) + RECT %s ( %d %d ) ( %d %d ) + RECT %s ( %d %d ) ( %d %d ) ;" %
         (R[0][0], -E, -E, E, E, C[0][0], -CUT, -CUT, CUT, CUT, R[1][0], -E, -E, E, E),
         "END VIAS",
         "SPECIALNETS 1 ;",
         "    - VDD + USE POWER",
         "      + ROUTED %s %d + SHAPE STRIPE ( %d %d ) ( %d * )" % (R[0][0], SW, a[0], a[1], b[0]),
         "      NEW %s 0 + SHAPE STRIPE ( %d %d ) VIA_%s_%s" % (R[0][0], b[0], b[1], R[0][0], R[1][0]),
         "      NEW %s %d + SHAPE STRIPE ( %d %d ) ( * %d )" % (R[1][0], SW, b[0], b[1], c2[1]),
         "      + RECT %s ( %d %d ) ( %d %d ) ;" % (R[1][0], rect[0], rect[1], rect[2], rect[3]),
         "END SPECIALNETS",
         "NETS %d ;" % len(nets)]
    for nm, stmts in nets:
        d.append("    - %s + USE SIGNAL" % nm)
        d.append("      + ROUTED " + stmts[0])
        for s in stmts[1:]:
            d.append("      NEW " + s)
        d[-1] += " ;"
    d += ["END NETS", "END DESIGN"]
    open(os.path.join(outdir, name + ".def"), "w").write("\n".join(d) + "\n")
    out.write(os.path.join(outdir, name + ".oas"))
    print("%s: %d routing layers, %d nets + VDD, DEF %d/um, layout %d/um" % (name, nR, len(nets), du, st["layout_per_um"]))


synth(ASAP7)
synth(GT2N)
