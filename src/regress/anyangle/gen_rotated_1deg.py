# Generate rotated OASIS copies at arbitrary integer angles (e.g. 0..359) in one KLayout pass.
#   klayout -b -r gen_rotated_1deg.py -rd src=IN.oas -rd out=DIR -rd name=TAG \
#           -rd angles=0,1,2,...,359 [-rd force=0]
# Skips any DIR/TAG_R<aaa>.oas already on disk (size > 0) unless force=1.
import os
import sys
import time
import pya

force = int(globals().get("force", "0")) != 0
raw_angles = str(globals().get("angles", ",".join(str(a) for a in range(360))))
angles = [int(a) for a in raw_angles.split(",") if a.strip() != ""]

os.makedirs(out, exist_ok=True)
todo = []
for a in angles:
    path = os.path.join(out, "%s_R%03d.oas" % (name, a))
    if force or not (os.path.isfile(path) and os.path.getsize(path) > 0):
        todo.append((a, path))

if not todo:
    print("SKIP %s: all %d angles already on disk" % (name, len(angles)))
    sys.exit(0)

t0 = time.time()
ly = pya.Layout()
ly.read(src)
tops = list(ly.top_cells())
if len(tops) == 0:
    print("ERROR: %s has 0 top cells" % src)
    sys.exit(1)
top_name = str(globals().get("top", "")).strip()
if top_name:
    tc = ly.cell(top_name)
    if tc is None:
        print("ERROR: top cell %s not found in %s" % (top_name, src))
        sys.exit(1)
    top_list = [tc]
else:
    top_list = tops
print("READ %s tops=%s dbu=%g cells=%d todo=%d/%d in %.1fs" % (
    src, ",".join(c.name for c in top_list), ly.dbu, ly.cells(), len(todo), len(angles), time.time() - t0))

opt = pya.SaveLayoutOptions()
opt.format = "OASIS"
opt.oasis_strict_mode = True
opt.oasis_compression_level = 2
opt.oasis_write_cblocks = True

for a, path in todo:
    tag = "%s_R%03d" % (name, a)
    wrap = ly.create_cell(tag)
    tr = pya.ICplxTrans(1.0, float(a), False, 0, 0)
    for tc in top_list:
        wrap.insert(pya.CellInstArray(tc.cell_index(), tr))
    tmp = path + ".tmp"
    t1 = time.time()
    ly.write(tmp, opt)
    os.replace(tmp, path)
    print("WROTE %s angle=%d bytes=%d in %.1fs" % (path, a, os.path.getsize(path), time.time() - t1), flush=True)
    ly.delete_cell(wrap.cell_index())
