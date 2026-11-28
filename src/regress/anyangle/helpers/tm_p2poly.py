import pya
ly = pya.Layout(); ly.read(fin)
top = ly.top_cell(); L = ly.layer(2, 0)
r = pya.Region(top.shapes(L)); top.shapes(L).clear(); top.shapes(L).insert(r)
ly.write(fout)
