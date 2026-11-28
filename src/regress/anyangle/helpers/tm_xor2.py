import pya
def reg(f):
    ly = pya.Layout(); ly.read(f); r = pya.Region(); li = ly.find_layer(66, 20)
    for t in ly.top_cells(): r.insert(pya.Region(t.begin_shapes_rec(li)))
    return r
a = reg(fa); b = reg(fb)
x = a ^ b
print("A", a.merged().area(), "B", b.merged().area(), "xor", x.area(), "n", x.count())
for i, p in enumerate(x.each()):
    if i < 8: print(p.bbox(), p.area())
