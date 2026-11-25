# KLayout (Python) generator for the hierarchical-load regression.
#   klayout -b -r gen_hier_input.py -rd out=hier_in.oas
# Child cells carry asymmetric L-shaped geometry on two layers so that any wrong
# rotation/mirror/offset changes the flattened result. Placements cover all eight
# orthogonal orientations, an AREF-style regular array, a 3-level nest, and many
# same-orientation placements that KLayout's OASIS writer compacts into repetitions.
import pya

ly = pya.Layout()
ly.dbu = 0.001
top = ly.create_cell("TOP")
leaf = ly.create_cell("LEAF")
mid = ly.create_cell("MID")
deep = ly.create_cell("DEEP")
L1 = ly.layer(68, 20)
L2 = ly.layer(67, 20)

# LEAF: asymmetric L on 68/20, an offset rectangle on 67/20
leaf.shapes(L1).insert(pya.Polygon([pya.Point(0, 0), pya.Point(400, 0), pya.Point(400, 100),
                                    pya.Point(100, 100), pya.Point(100, 250), pya.Point(0, 250)]))
leaf.shapes(L2).insert(pya.Box(150, 150, 350, 200))
# a rail that abuts across placements (like a standard-cell power rail)
leaf.shapes(L1).insert(pya.Box(0, 300, 500, 340))

# MID: two LEAFs (one mirrored) plus own geometry
mid.shapes(L1).insert(pya.Box(-50, -50, 0, 600))
mid.insert(pya.CellInstArray(leaf.cell_index(), pya.Trans(pya.Trans.R0, 0, 0)))
mid.insert(pya.CellInstArray(leaf.cell_index(), pya.Trans(pya.Trans.M0, 700, 700)))
# DEEP: rotated MID
deep.insert(pya.CellInstArray(mid.cell_index(), pya.Trans(pya.Trans.R90, 1000, 0)))
deep.shapes(L2).insert(pya.Box(0, 0, 30, 30))

# TOP: own geometry + all 8 orientations of LEAF
top.shapes(L1).insert(pya.Box(-2000, -2000, -1500, -1900))
codes = [pya.Trans.R0, pya.Trans.R90, pya.Trans.R180, pya.Trans.R270,
         pya.Trans.M0, pya.Trans.M45, pya.Trans.M90, pya.Trans.M135]
for k, c in enumerate(codes):
    top.insert(pya.CellInstArray(leaf.cell_index(), pya.Trans(c, 1200 * k, 5000)))
# regular array (mirrored) of LEAF: 5 x 3
top.insert(pya.CellInstArray(leaf.cell_index(), pya.Trans(pya.Trans.M0, 0, 9000),
                             pya.Vector(600, 0), pya.Vector(0, 800), 5, 3))
# nested, rotated and mirrored DEEP placements
top.insert(pya.CellInstArray(deep.cell_index(), pya.Trans(pya.Trans.R0, 12000, 0)))
top.insert(pya.CellInstArray(deep.cell_index(), pya.Trans(pya.Trans.M90, 16000, 3000)))
top.insert(pya.CellInstArray(deep.cell_index(), pya.Trans(pya.Trans.R270, 12000, 9000)))
# a row of abutting "standard cells" (rails merge across cells), alternating flip like cell rows
for i in range(40):
    top.insert(pya.CellInstArray(leaf.cell_index(), pya.Trans(pya.Trans.R0, 500 * i, -3000)))
    top.insert(pya.CellInstArray(leaf.cell_index(), pya.Trans(pya.Trans.M0, 500 * i, -3000 + 680)))
# irregular positions with the same orientation (irregular OASIS repetition)
for i, (x, y) in enumerate([(0, 0), (900, 37), (2300, 0), (4100, 512), (7000, 90)]):
    top.insert(pya.CellInstArray(leaf.cell_index(), pya.Trans(pya.Trans.R180, 30000 + x, y)))

opt = pya.SaveLayoutOptions()
opt.format = "OASIS"
opt.oasis_compression_level = 10      # let KLayout build repetitions
ly.write(out, opt)
print("wrote", out)
