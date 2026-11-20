#!/usr/bin/env python3
import os

ROOT = "/Users/skoranne/GITHUB/VLSI_LAYOUT"
OUT = os.path.join(ROOT, "src", "topo_regressions")
CTRDIR = os.path.join(OUT, "ctrs_working")
os.makedirs(CTRDIR, exist_ok=True)

OPS = ["INTERACT","NOT_INTERACT","CUT","NOT_CUT","TOUCH","NOT_TOUCH","INSIDE","NOT_INSIDE","OUTSIDE","NOT_OUTSIDE"]
PAIRS = [("INTERACT","NOT_INTERACT"),("CUT","NOT_CUT"),("TOUCH","NOT_TOUCH"),("INSIDE","NOT_INSIDE"),("OUTSIDE","NOT_OUTSIDE")]

# Available working designs
DESIGNS = [
    ("inv", "../examples/INV.mag", ["nwell","nmos","pmos","ndiff","pdiff","poly","metal1"]),
    ("top", "../examples/TOP.mag", ["nwell","metal1"]),
]

tests = []  # list of (desc, lines)
# Helper to add a full check set for (Aexpr, Bexpr, label) where Aexpr/Bexpr are db:layer refs
def add_checks(lines, mdb, tag, A, B, test_list):
    for op,nop in PAIRS:
        # op and not_op complement
        lines.append(f"exec run {mdb}:t_{tag}_{op}_r = {A} {op} {B}")
        lines.append(f"exec run {mdb}:t_{tag}_{nop}_r = {A} {nop} {B}")
        lines.append(f"exec run {mdb}:t_{tag}_{op}_ch = {mdb}:t_{tag}_{op}_r * {mdb}:t_{tag}_{nop}_r")
        lines.append(f"exec run {mdb}:t_{tag}_{op}_az = {mdb}:t_{tag}_{op}_ch ASSERT_ZERO nothing")
        test_list.append(f"{tag}_{op}_complement")
        # partition union == A
        lines.append(f"exec run {mdb}:t_{tag}_{op}_un = {mdb}:t_{tag}_{op}_r + {mdb}:t_{tag}_{nop}_r")
        lines.append(f"exec run {mdb}:t_{tag}_{op}_eq = {mdb}:t_{tag}_{op}_un ^ {A}")
        lines.append(f"exec run {mdb}:t_{tag}_{op}_ez = {mdb}:t_{tag}_{op}_eq ASSERT_ZERO nothing")
        test_list.append(f"{tag}_{op}_partition")
    # explicit chain required: (A INSIDE B) AND (A NOT INSIDE B) -> 0
    # already covered but add named form
    # cross chains
    lines.append(f"exec run {mdb}:t_{tag}_in_out = {mdb}:t_{tag}_INSIDE_r * {mdb}:t_{tag}_OUTSIDE_r")
    lines.append(f"exec run {mdb}:t_{tag}_in_out_z = {mdb}:t_{tag}_in_out ASSERT_ZERO nothing")
    test_list.append(f"{tag}_inside_and_outside_zero")
    lines.append(f"exec run {mdb}:t_{tag}_cut_out = {mdb}:t_{tag}_CUT_r * {mdb}:t_{tag}_OUTSIDE_r")
    lines.append(f"exec run {mdb}:t_{tag}_cut_out_z = {mdb}:t_{tag}_cut_out ASSERT_ZERO nothing")
    test_list.append(f"{tag}_cut_and_outside_zero")

# Build CTRs
ctr_files=[]
global_test_count=0
# CTR 1: INV base layer pairs (all ordered pairs among 7 layers, both directions in some cases) - hierarchical cell design
def make_inv_base():
    global global_test_count
    lines=[]
    prog="topo_inv_base"
    lines+= [f"program {prog}", f"var temp_folder /tmp/{prog}/", "var temporary_layers 2", "var abort_on_assert_zero 2",
             "decl design d = ../examples/INV.mag", "decl memory m = nothing", "exec init m:nothing"]
    layers=["nwell","nmos","pmos","ndiff","pdiff","poly","metal1"]
    tlist=[]
    idx=0
    for i,la in enumerate(layers):
        for j,lb in enumerate(layers):
            if i==j: continue
            tag=f"ib{idx}"; idx+=1
            add_checks(lines,"m",tag,f"d:{la}",f"d:{lb}",tlist)
    lines+=["flush","end"]
    path=os.path.join(CTRDIR,"inv_base.ctr")
    open(path,"w").write("\n".join(lines)+"\n")
    global_test_count += len(tlist)
    return path,len(tlist)

# CTR 2: TOP hierarchical design + pairs
def make_top_base():
    global global_test_count
    lines=[]
    prog="topo_top_hier"
    lines+= [f"program {prog}", f"var temp_folder /tmp/{prog}/", "var temporary_layers 2", "var abort_on_assert_zero 2",
             "decl design d = ../examples/TOP.mag", "decl memory m = nothing", "exec init m:nothing"]
    layers=["nwell","metal1"]
    tlist=[]; idx=0
    for i,la in enumerate(layers):
        for j,lb in enumerate(layers):
            if i==j: continue
            tag=f"tb{idx}"; idx+=1
            add_checks(lines,"m",tag,f"d:{la}",f"d:{lb}",tlist)
    # also self? maybe not meaningful but unique
    lines+=["flush","end"]
    path=os.path.join(CTRDIR,"top_hier.ctr")
    open(path,"w").write("\n".join(lines)+"\n")
    global_test_count += len(tlist)
    return path,len(tlist)

# CTR 3+: derived geometries on INV (GROW/SHRINK to create new shapes) - many unique cases
def make_inv_derived():
    global global_test_count
    paths=[]
    layers=["nwell","nmos","pmos","ndiff","pdiff","poly","metal1"]
    grow_vals=[1,2,5,10]
    shrink_vals=[1,2]
    # create multiple CTRs to keep size manageable
    ctr_idx=0
    batch=[]
    def flush_batch():
        nonlocal ctr_idx, batch
        if not batch: return
        prog=f"topo_inv_der_{ctr_idx}"
        lines=[f"program {prog}", f"var temp_folder /tmp/{prog}/", "var temporary_layers 2", "var abort_on_assert_zero 2",
               "decl design d = ../examples/INV.mag", "decl memory m = nothing", "exec init m:nothing"]
        tlist=[]
        for (tag, setup_lines, A, B) in batch:
            lines.extend(setup_lines)
            add_checks(lines,"m",tag,A,B,tlist)
        lines+=["flush","end"]
        path=os.path.join(CTRDIR,f"inv_der_{ctr_idx}.ctr")
        open(path,"w").write("\n".join(lines)+"\n")
        paths.append((path,len(tlist)))
        global global_test_count
        global_test_count += len(tlist)
        ctr_idx+=1; batch=[]
    tagi=0
    # GROW-derived layers vs originals and vs other derived
    for la in layers:
        for gv in grow_vals:
            gname=f"g_{la}_{gv}"
            # create grown layer once per batch context? easier to recreate; but in a CTR we can create once
            # We'll structure batches by derived base
            for lb in layers:
                if la==lb and gv==0: continue
                tag=f"id{tagi}"; tagi+=1
                setup=[f"exec run m:{gname} = d:{la} GROW nothing {gv} {gv} {gv} {gv}"]
                batch.append((tag, setup, f"m:{gname}", f"d:{lb}"))
                if len(batch)>=12: flush_batch()
                tag=f"id{tagi}"; tagi+=1
                batch.append((tag, [f"exec run m:{gname} = d:{la} GROW nothing {gv} {gv} {gv} {gv}"], f"d:{lb}", f"m:{gname}"))
                if len(batch)>=12: flush_batch()
    # SHRINK-derived (only where likely non-empty?) try small shrinks
    for la in layers:
        for sv in shrink_vals:
            sname=f"s_{la}_{sv}"
            for lb in ["poly","ndiff","pdiff","metal1","nwell"]:
                tag=f"id{tagi}"; tagi+=1
                setup=[f"exec run m:{sname} = d:{la} SHRINK nothing {sv} {sv} {sv} {sv}"]
                batch.append((tag, setup, f"m:{sname}", f"d:{lb}"))
                if len(batch)>=12: flush_batch()
                tag=f"id{tagi}"; tagi+=1
                batch.append((tag, [f"exec run m:{sname} = d:{la} SHRINK nothing {sv} {sv} {sv} {sv}"], f"d:{lb}", f"m:{sname}"))
                if len(batch)>=12: flush_batch()
    # derived vs derived (grow-grow different params/layers)
    combos=[("poly",2,"ndiff",1),("poly",5,"metal1",2),("ndiff",2,"pdiff",2),("nwell",1,"poly",5),("metal1",3,"nwell",1),("pmos",1,"nmos",1),("pdiff",1,"poly",3)]
    for la,ga,lb,gb in combos:
        tag=f"id{tagi}"; tagi+=1
        setup=[f"exec run m:ga_{tag} = d:{la} GROW nothing {ga} {ga} {ga} {ga}",
               f"exec run m:gb_{tag} = d:{lb} GROW nothing {gb} {gb} {gb} {gb}"]
        batch.append((tag, setup, f"m:ga_{tag}", f"m:gb_{tag}"))
        if len(batch)>=12: flush_batch()
        tag=f"id{tagi}"; tagi+=1
        batch.append((tag, setup, f"m:gb_{tag}", f"m:ga_{tag}"))
        if len(batch)>=12: flush_batch()
    flush_batch()
    return paths

p1,c1=make_inv_base(); print(p1,c1)
p2,c2=make_top_base(); print(p2,c2)
ps=make_inv_derived()
for p,c in ps: print(p,c)
print("TOTAL", global_test_count)
