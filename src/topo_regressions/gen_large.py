import os

OUT = "/Users/skoranne/GITHUB/VLSI_LAYOUT/src/topo_regressions/ctrs_large"
os.makedirs(OUT, exist_ok=True)
for f in os.listdir(OUT):
    if f.endswith('.ctr'): os.remove(os.path.join(OUT,f))

PAIRS_OP = [("INTERACT","NOT_INTERACT"),("CUT","NOT_CUT"),("TOUCH","NOT_TOUCH"),("INSIDE","NOT_INSIDE"),("OUTSIDE","NOT_OUTSIDE")]

def add_checks(lines, A, B, tag):
    tests=[]
    for op,nop in PAIRS_OP:
        lines.append(f"exec run m:r_{tag}_{op} = {A} {op} {B}")
        lines.append(f"exec run m:r_{tag}_{nop} = {A} {nop} {B}")
        lines.append(f"exec run m:c_{tag}_{op} = m:r_{tag}_{op} * m:r_{tag}_{nop}")
        lines.append(f"exec run m:z_{tag}_{op} = m:c_{tag}_{op} ASSERT_ZERO nothing")
        tests.append(f"{tag}_{op}_comp")
    lines.append(f"exec run m:c_{tag}_ich = m:r_{tag}_INSIDE * m:r_{tag}_NOT_INSIDE")
    lines.append(f"exec run m:z_{tag}_ich = m:c_{tag}_ich ASSERT_ZERO nothing"); tests.append(f"{tag}_inside_chain")
    lines.append(f"exec run m:c_{tag}_io = m:r_{tag}_INSIDE * m:r_{tag}_OUTSIDE")
    lines.append(f"exec run m:z_{tag}_io = m:c_{tag}_io ASSERT_ZERO nothing"); tests.append(f"{tag}_in_out")
    lines.append(f"exec run m:c_{tag}_co = m:r_{tag}_CUT * m:r_{tag}_OUTSIDE")
    lines.append(f"exec run m:z_{tag}_co = m:c_{tag}_co ASSERT_ZERO nothing"); tests.append(f"{tag}_cut_out")
    lines.append(f"exec run m:c_{tag}_to = m:r_{tag}_TOUCH * m:r_{tag}_OUTSIDE")
    lines.append(f"exec run m:z_{tag}_to = m:c_{tag}_to ASSERT_ZERO nothing"); tests.append(f"{tag}_touch_out")
    lines.append(f"exec run m:c_{tag}_ci = m:r_{tag}_CUT * m:r_{tag}_INSIDE")
    lines.append(f"exec run m:z_{tag}_ci = m:c_{tag}_ci ASSERT_ZERO nothing"); tests.append(f"{tag}_cut_in")
    lines.append(f"exec run m:c_{tag}_ti = m:r_{tag}_TOUCH * m:r_{tag}_INSIDE")
    lines.append(f"exec run m:z_{tag}_ti = m:c_{tag}_ti ASSERT_ZERO nothing"); tests.append(f"{tag}_touch_in")
    lines.append(f"exec run m:c_{tag}_tc = m:r_{tag}_TOUCH * m:r_{tag}_CUT")
    lines.append(f"exec run m:z_{tag}_tc = m:c_{tag}_tc ASSERT_ZERO nothing"); tests.append(f"{tag}_touch_cut")
    return tests

def gen(design_path, name, layers, mode="design", tempdir=None):
    if tempdir is None: tempdir = f"/tmp/{name}/"
    prog = f"topo_{name}"
    lines = [f"program {prog}", f"var temp_folder {tempdir}", "var temporary_layers 2",
             "var abort_on_assert_zero 2", "var debug_verbosity 0",
             f"decl {mode} d = {design_path}", "decl memory m = nothing", "exec init m:nothing"]
    tests=[]
    idx=0
    for i,la in enumerate(layers):
        for j,lb in enumerate(layers):
            if i==j: continue
            tag=f"t{idx}"; idx+=1
            tests += add_checks(lines, f"d:{la}", f"d:{lb}", tag)
    lines.append("end")
    path=os.path.join(OUT, f"{name}.ctr")
    open(path,"w").write("\n".join(lines)+"\n")
    print(path, len(tests), "pairs", idx)
    return path, len(tests)

# Common Sky130-like layers present in SDT/FPU
layers9 = ["64:20","65:20","66:20","67:20","68:20","69:20","70:20","71:20","72:20"]
layers10 = layers9 + ["78:44"]

# SDT_16
gen("../data/SDT/SDT_16.oas","sdt16",layers10)
# mkFPU
gen("../data/FPU/mkFPU.oas","mkfpu",layers10)
# FPU_8x8 (hier)
gen("../data/FPU/FPU_8x8.oas","fpu8x8",layers9)
