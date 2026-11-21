#!/usr/bin/env python3
"""Generate comprehensive DE-9IM / 9E-DIM topological regression CTRs.

Covers existing MAGPARSER box/polygon topological select operators:
  INTERACT, NOT_INTERACT, CUT, NOT_CUT, TOUCH, NOT_TOUCH,
  INSIDE, NOT_INSIDE, OUTSIDE, NOT_OUTSIDE

For each case checks: complement, partition, and key DE-9IM exclusions.
"""
import os

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
OUT = os.path.join(ROOT, "src", "topo_regressions", "ctrs_working")
os.makedirs(OUT, exist_ok=True)

OPS = ["INTERACT","NOT_INTERACT","CUT","NOT_CUT","TOUCH","NOT_TOUCH","INSIDE","NOT_INSIDE","OUTSIDE","NOT_OUTSIDE"]
PAIRS = [("INTERACT","NOT_INTERACT"),("CUT","NOT_CUT"),("TOUCH","NOT_TOUCH"),("INSIDE","NOT_INSIDE"),("OUTSIDE","NOT_OUTSIDE")]

def add_checks(lines, mdb, tag, A, B, tlist):
    for op, nop in PAIRS:
        lines.append(f"exec run {mdb}:t_{tag}_{op}_r = {A} {op} {B}")
        lines.append(f"exec run {mdb}:t_{tag}_{nop}_r = {A} {nop} {B}")
        lines.append(f"exec run {mdb}:t_{tag}_{op}_ch = {mdb}:t_{tag}_{op}_r * {mdb}:t_{tag}_{nop}_r")
        lines.append(f"exec run {mdb}:t_{tag}_{op}_az = {mdb}:t_{tag}_{op}_ch ASSERT_ZERO nothing")
        tlist.append(f"{tag}_{op}_complement")
        lines.append(f"exec run {mdb}:t_{tag}_{op}_un = {mdb}:t_{tag}_{op}_r + {mdb}:t_{tag}_{nop}_r")
        lines.append(f"exec run {mdb}:t_{tag}_{op}_eq = {mdb}:t_{tag}_{op}_un ^ {A}")
        lines.append(f"exec run {mdb}:t_{tag}_{op}_ez = {mdb}:t_{tag}_{op}_eq ASSERT_ZERO nothing")
        tlist.append(f"{tag}_{op}_partition")
    # DE-9IM mutual exclusions
    excl = [
        ("INSIDE_r","OUTSIDE_r","inside_outside"),
        ("CUT_r","OUTSIDE_r","cut_outside"),
        ("TOUCH_r","OUTSIDE_r","touch_outside"),
        ("CUT_r","INSIDE_r","cut_inside"),
        ("TOUCH_r","INSIDE_r","touch_inside"),
        ("TOUCH_r","CUT_r","touch_cut"),
        ("INSIDE_r","NOT_INSIDE_r","inside_chain"),
    ]
    for a,b,name in excl:
        lines.append(f"exec run {mdb}:t_{tag}_{name} = {mdb}:t_{tag}_{a} * {mdb}:t_{tag}_{b}")
        lines.append(f"exec run {mdb}:t_{tag}_{name}z = {mdb}:t_{tag}_{name} ASSERT_ZERO nothing")
        tlist.append(f"{tag}_{name}")

def write_ctr(name, body_lines, tlist):
    path = os.path.join(OUT, name)
    prog = name.replace('.ctr','')
    header = [
        f"program {prog}",
        f"var temp_folder /tmp/{prog}/",
        "var temporary_layers 2",
        "var abort_on_assert_zero 2",
    ]
    with open(path, 'w') as f:
        f.write("\n".join(header + body_lines + ["flush", "end", ""]) )
    return path, len(tlist)

def gen_shape_de9im():
    """Synthetic box/shape cases representing canonical DE-9IM region-region relations."""
    lines = [
        "decl design d = ../examples/INV.mag",
        "decl memory m = nothing",
        "exec init m:nothing",
    ]
    t=[]
    # Use existing layers as bases, derive geometric DE-9IM configurations
    # EQUAL: A vs A (same layer)
    add_checks(lines, "m", "eq_poly", "d:poly", "d:poly", t)
    # CONTAINS/COVERS-like via GROW: grown contains original
    lines.append("exec run m:g_poly_5 = d:poly GROW nothing 5 5 5 5")
    add_checks(lines, "m", "cov_poly", "d:poly", "m:g_poly_5", t)
    add_checks(lines, "m", "cov_poly_r", "m:g_poly_5", "d:poly", t)
    # INSIDE/COVEREDBY: original inside grown
    lines.append("exec run m:g_ndiff_2 = d:ndiff GROW nothing 2 2 2 2")
    add_checks(lines, "m", "inside_nd", "d:ndiff", "m:g_ndiff_2", t)
    add_checks(lines, "m", "contains_nd", "m:g_ndiff_2", "d:ndiff", t)
    # DISJOINT/OUTSIDE: nwell vs metal1 in INV? use separate distant layers by shrinking/growing differently
    lines.append("exec run m:s_poly_1 = d:poly SIZE nothing -0.001")
    add_checks(lines, "m", "disj_sp", "m:s_poly_1", "d:nwell", t)
    # TOUCH/MEET: adjacent via grow to edge - approximate by grow by 0? use GROW 1 creates overlap; use boundary cases from original pairs
    add_checks(lines, "m", "pair_pn", "d:poly", "d:ndiff", t)
    add_checks(lines, "m", "pair_np", "d:ndiff", "d:poly", t)
    add_checks(lines, "m", "pair_pm", "d:poly", "d:metal1", t)
    add_checks(lines, "m", "pair_nw_m1", "d:nwell", "d:metal1", t)
    # OVERLAP/CUT: poly and ndiff overlap in INV
    add_checks(lines, "m", "ov_pnd", "d:poly", "d:ndiff", t)
    # Edge-touch via derived adjacent boxes: grow one, shrink other to create boundary adjacency
    lines.append("exec run m:g_m1_1 = d:metal1 GROW nothing 1 1 1 1")
    lines.append("exec run m:s_m1_1 = d:metal1 SIZE nothing -0.001")
    add_checks(lines, "m", "edge_gm1", "d:metal1", "m:g_m1_1", t)
    add_checks(lines, "m", "edge_sm1", "d:metal1", "m:s_m1_1", t)
    add_checks(lines, "m", "edge_gs", "m:g_m1_1", "m:s_m1_1", t)
    return write_ctr("de9im_shape.ctr", lines, t)

def gen_polygon_de9im():
    """Polygon/merged-layer cases: unioned/healed layers as polygon-like inputs."""
    lines = [
        "decl design d = ../examples/INV.mag",
        "decl memory m = nothing",
        "exec init m:nothing",
    ]
    t=[]
    # Build polygon-like layers by boolean ops
    lines.append("exec run m:poly_u_ndiff = d:poly + d:ndiff")
    lines.append("exec run m:poly_u_pdiff = d:poly + d:pdiff")
    lines.append("exec run m:diff_u = d:ndiff + d:pdiff")
    lines.append("exec run m:active = m:diff_u + d:poly")
    add_checks(lines, "m", "pg_ndiff", "m:poly_u_ndiff", "d:ndiff", t)
    add_checks(lines, "m", "pg_poly", "m:poly_u_ndiff", "d:poly", t)
    add_checks(lines, "m", "pg_active", "m:active", "d:poly", t)
    add_checks(lines, "m", "pg_active_nd", "m:active", "d:ndiff", t)
    add_checks(lines, "m", "pg_diff_poly", "m:diff_u", "d:poly", t)
    add_checks(lines, "m", "pg_diff_poly_r", "d:poly", "m:diff_u", t)
    # Polygon containment / inside
    lines.append("exec run m:g_active_3 = m:active GROW nothing 3 3 3 3")
    add_checks(lines, "m", "pg_inside", "m:active", "m:g_active_3", t)
    add_checks(lines, "m", "pg_contains", "m:g_active_3", "m:active", t)
    # Disjoint polygons
    add_checks(lines, "m", "pg_disj", "d:nwell", "m:diff_u", t)
    # Overlap polygons
    add_checks(lines, "m", "pg_ov", "m:poly_u_ndiff", "m:poly_u_pdiff", t)
    # Equal polygon
    add_checks(lines, "m", "pg_eq", "m:active", "m:active", t)
    return write_ctr("de9im_polygon.ctr", lines, t)

def gen_edge_de9im():
    """Edge/boundary-focused cases using thin/adjacent shapes to exercise edge-touch semantics."""
    lines = [
        "decl design d = ../examples/INV.mag",
        "decl memory m = nothing",
        "exec init m:nothing",
    ]
    t=[]
    # Edge-touch proxy: TOUCH is edge contact without positive-area overlap.
    # Create adjacent/sliver cases via GROW/SIZE differences.
    lines.append("exec run m:g_poly_1 = d:poly GROW nothing 1 1 1 1")
    lines.append("exec run m:g_poly_2 = d:poly GROW nothing 2 2 2 2")
    lines.append("exec run m:ring = m:g_poly_2 ~ m:g_poly_1")  # ring around poly - boundary adjacency
    add_checks(lines, "m", "edge_ring_poly", "d:poly", "m:ring", t)
    add_checks(lines, "m", "edge_ring_g1", "m:g_poly_1", "m:ring", t)
    add_checks(lines, "m", "edge_ring_g2", "m:g_poly_2", "m:ring", t)
    # Boundary contact via shrink/grow
    lines.append("exec run m:s_nd_1 = d:ndiff SIZE nothing -0.001")
    lines.append("exec run m:g_nd_1 = d:ndiff GROW nothing 1 1 1 1")
    add_checks(lines, "m", "edge_nd_s", "d:ndiff", "m:s_nd_1", t)
    add_checks(lines, "m", "edge_nd_g", "d:ndiff", "m:g_nd_1", t)
    add_checks(lines, "m", "edge_nd_sg", "m:s_nd_1", "m:g_nd_1", t)
    # Edge disjoint
    add_checks(lines, "m", "edge_disj", "m:ring", "d:nwell", t)
    # Edge overlap/cut
    lines.append("exec run m:g_m1_2 = d:metal1 GROW nothing 2 2 2 2")
    add_checks(lines, "m", "edge_cut", "m:g_m1_2", "d:nwell", t)
    # Edge-equal/inside
    add_checks(lines, "m", "edge_eq", "m:ring", "m:ring", t)
    add_checks(lines, "m", "edge_in", "m:ring", "m:g_poly_2", t)
    return write_ctr("de9im_edge.ctr", lines, t)

if __name__ == '__main__':
    all_paths=[]
    for fn in [gen_shape_de9im, gen_polygon_de9im, gen_edge_de9im]:
        p,c = fn()
        all_paths.append((p,c))
        print(f"{p}\t{c} tests")
    print(f"TOTAL\t{sum(c for _,c in all_paths)} tests")
