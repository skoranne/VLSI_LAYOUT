import pya
ly = pya.Layout(); ly.read(a)
c = ly.cell("sky130_fd_sc_hd__dfxtp_2")
for l,d in ((64,16),(68,16)):
    li = ly.find_layer(l, d)
    for s in c.shapes(li).each(): print("KL", l, d, s)
