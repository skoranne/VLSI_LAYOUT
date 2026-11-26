# Rotated copies of a layout for the rotation-invariance regression.
#   klayout -b -r gen_rotated.py -rd src=IN.oas -rd out=DIR -rd name=TAG [-rd angles=0,15,...]
# For every angle a writes DIR/TAG_R<aaa>.oas: the source cells unchanged plus a new top cell
# TAG_R<aaa> holding one placement of the source top cell with rotation a (degrees, about the
# origin, no magnification or mirror).  The rotation is NOT applied to the shapes: the reader's
# flatten logic has to apply it.  OASIS strict mode, CBLOCK compression.
import pya, sys, os, time

angles = [int(a) for a in globals().get("angles", ",".join(str(15 * k) for k in range(0, 13))).split(",")]
t0 = time.time()
ly = pya.Layout()
ly.read(src)
tops = list(ly.top_cells())
if len(tops) != 1:
    print("ERROR: %s has %d top cells" % (src, len(tops)))
    sys.exit(1)
top = tops[0]
print("READ %s top=%s dbu=%g cells=%d in %.1fs" % (src, top.name, ly.dbu, ly.cells(), time.time() - t0))
opt = pya.SaveLayoutOptions()
opt.format = "OASIS"
opt.oasis_strict_mode = True
opt.oasis_compression_level = 2
opt.oasis_write_cblocks = True
for a in angles:
    tag = "%s_R%03d" % (name, a)
    wrap = ly.create_cell(tag)
    wrap.insert(pya.CellInstArray(top.cell_index(), pya.ICplxTrans(1.0, float(a), False, 0, 0)))
    path = os.path.join(out, tag + ".oas")
    t1 = time.time()
    ly.write(path, opt)
    print("WROTE %s angle=%d bytes=%d in %.1fs" % (path, a, os.path.getsize(path), time.time() - t1))
    ly.delete_cell(wrap.cell_index())
