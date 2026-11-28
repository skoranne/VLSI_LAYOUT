import pya
ly = pya.Layout(); ly.read(a)
li = ly.find_layer(64, 16)
for c in ly.each_cell():
    n = c.shapes(li).size()
    if n: print("KLCELL", c.name, n, "insts", c.parent_cells())
