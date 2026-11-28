import pya
r = pya.Region()
r.insert(pya.Box(0,0,10,10)); r.insert(pya.Box(5,0,15,10)); r.insert(pya.Box(100,0,110,10))
for wc in (0,1,2):
    m = r.merged(False, wc)
    print("min_wc", wc, "area", m.area(), "n", m.count())
