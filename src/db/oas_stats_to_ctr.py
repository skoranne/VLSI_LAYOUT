#!/usr/bin/env python3
"""
Convert db_oas_stats text output into a comprehensive, self-checking MAGPARSER CTR
for an OASIS design (decl hierdesign).

Usage:
  ./db_oas_stats MW4x4.oas --flat > /tmp/stats.txt
  python3 oas_stats_to_ctr.py /tmp/stats.txt --design MW4x4.oas -o MW4x4_OASIS_SELFTEST.ctr

Or pipe:
  ./db_oas_stats --flat MW4x4.oas | python3 oas_stats_to_ctr.py - --design MW4x4.oas

The generated CTR mirrors MW_4x4_SELFTEST.ctr: boolean ops, identity ASSERT_ZERO
checks, morphological ops, topological/DE-9IM, edge-topological ops, and layer-pair
coverage. Layers are chosen from the stats (preferring known sky130 names/sizes).
"""
from __future__ import annotations
import argparse, sys, os, re
from dataclasses import dataclass

@dataclass
class LayerStat:
    layer: int
    dtype: int
    name: str
    count: int
    xmin: int; ymin: int; xmax: int; ymax: int
    @property
    def token(self) -> str:
        return f"L{self.layer}_D{self.dtype}"
    @property
    def area_est(self) -> int:
        return max(0, self.xmax-self.xmin) * max(0, self.ymax-self.ymin)

# Known sky130-ish names by (layer,dtype) for nicer comments.
KNOWN = {
    (64,20): "nwell", (65,20): "diff", (65,44): "tap", (66,20): "poly", (66,44): "licon1",
    (67,16): "li1_pin", (67,20): "li1", (67,44): "mcon", (68,16): "met1_pin", (68,20): "met1",
    (68,44): "via", (69,16): "met2_pin", (69,20): "met2", (69,44): "via2",
    (70,16): "met3_pin", (70,20): "met3", (70,44): "via3",
    (71,16): "met4_pin", (71,20): "met4", (71,44): "via4",
    (72,16): "met5_pin", (72,20): "met5",
}

def parse_stats(lines):
    info = {"layers": [], "props": [], "texts": []}
    for line in lines:
        line = line.rstrip("\n")
        if not line: continue
        parts = line.split()
        tag = parts[0]
        if tag == "FILE": info["file"] = line.split(None,1)[1]
        elif tag == "FORMAT": info["format"] = parts[1]
        elif tag == "UNITS_PER_MICRON": info["dbu"] = parts[1]
        elif tag == "TOP": info["top"] = line.split(None,1)[1] if len(parts)>1 else "-"
        elif tag == "EXTENT":
            if len(parts) == 5: info["extent"] = tuple(parts[1:5])
            else: info["extent"] = None
        elif tag == "LAYER" and len(parts) >= 9:
            layer, dtype = int(parts[1]), int(parts[2])
            name = parts[3]
            count = int(parts[4])
            xmin,ymin,xmax,ymax = map(int, parts[5:9])
            if name == "-":
                name = KNOWN.get((layer,dtype), "-")
            info["layers"].append(LayerStat(layer,dtype,name,count,xmin,ymin,xmax,ymax))
        elif tag == "PROPERTY":
            info["props"].append(line)
        elif tag == "TEXT":
            info["texts"].append(line)
    return info

def pick_layers(layers):
    """Pick layers for comprehensive tests: prefer sky130 drawing layers in order."""
    by_key = {(l.layer,l.dtype): l for l in layers}
    preferred = [(65,20),(66,20),(67,20),(68,20),(69,20),(70,20),(71,20),(72,20),(64,20)]
    picked = [by_key[k] for k in preferred if k in by_key]
    # Fallback: largest-count drawing-like layers (dtype 20 common) or just largest
    if len(picked) < 4:
        rest = sorted([l for l in layers if l not in picked], key=lambda l: -l.count)
        for l in rest:
            if len(picked) >= 6: break
            picked.append(l)
    return picked

def lname(l: LayerStat) -> str:
    return f"d:{l.token}"

def emit_ctr(info, design_path, out_mag):
    layers = info.get("layers", [])
    picked = pick_layers(layers)
    if len(picked) < 2:
        raise SystemExit("need at least 2 layers in stats to generate CTR")
    # Name primary layers by role where possible
    role = {}
    for l in picked:
        nm = (l.name or "").lower()
        if nm == "diff": role["diff"] = l
        elif nm == "poly": role["poly"] = l
        elif nm == "li1": role["li1"] = l
        elif nm == "met1": role["met1"] = l
        elif nm == "met2": role["met2"] = l
    # Fallback roles by position
    def get(role_name, idx):
        return role.get(role_name, picked[min(idx, len(picked)-1)])
    diff = get("diff",0); poly = get("poly",1); li1 = get("li1",2); met1 = get("met1",3)
    met2 = role.get("met2", picked[4] if len(picked)>4 else met1)

    top = info.get("top","-")
    dbu = info.get("dbu","?")
    ext = info.get("extent")
    ext_s = " ".join(ext) if ext else "INVALID"
    L = []
    w = L.append
    w(f"# Auto-generated comprehensive self-checking CTR from OASIS stats")
    w(f"# Source: {info.get('file','?')}")
    w(f"# Format: {info.get('format','?')}  DBU/um: {dbu}  TOP: {top}")
    w(f"# Extent: {ext_s}")
    w(f"# Run: MAGPARSER.exe <this.ctr> {max(64, len(layers) + 8)}")
    w(f"# Layers ({len(layers)}):")
    for l in layers:
        nm = l.name if l.name != '-' else ''
        w(f"#   {l.token:<12} count={l.count:<12} bbox={l.xmin},{l.ymin},{l.xmax},{l.ymax} {nm}")
    w(f"#")
    w(f"# Selected roles: diff={diff.token} poly={poly.token} li1={li1.token} met1={met1.token} met2={met2.token}")
    w(f"program mw4x4_oasis_selftest")
    w(f"var temp_folder /tmp/mw4x4_oasis_selftest/")
    w(f"var temporary_layers 2")
    w(f"var abort_on_assert_zero 2")
    w(f"var debug 1")
    w(f"systeminfo")
    w(f"decl hierdesign d = {design_path}")
    w(f"decl memory m = nothing")
    w(f"decl output o = {out_mag}")
    w(f"info")
    w("")
    w("# --- Boolean basics ---")
    w(f"exec run o:diff_or_poly = {lname(diff)} + {lname(poly)}")
    w(f"exec run o:diff_and_poly = {lname(diff)} * {lname(poly)}")
    w(f"exec run o:diff_not_poly = {lname(diff)} @ {lname(poly)}")
    w(f"exec run o:diff_xor_poly = {lname(diff)} ^ {lname(poly)}")
    w(f"exec run o:diff_or2_poly = {lname(diff)} OR2 {lname(poly)}")
    w(f"exec run o:diff_and2_poly = {lname(diff)} AND2 {lname(poly)}")
    w(f"exec run o:diff_fnot_poly = {lname(diff)} ~ {lname(poly)}")
    w(f"exec run o:diff_bnot_poly = {lname(diff)} ! {lname(poly)}")
    w("")
    w("# --- Self-test identity: A AND (B NOT A) == 0 ---")
    pairs = [(diff,poly,"dp"), (poly,li1,"pl"), (li1,met1,"lm1"), (met1,met2,"m12")]
    seen = set()
    for a,b,suf in pairs:
        k = (a.token,b.token,suf)
        if k in seen: continue
        seen.add(k)
        w(f"exec run m:bnot_a_{suf} = {lname(b)} @ {lname(a)}")
        w(f"exec run m:self_{suf} = {lname(a)} * m:bnot_a_{suf}")
        w(f"exec run m:az_self_{suf} = m:self_{suf} ASSERT_ZERO nothing")
    w("")
    w("# --- Boolean identities ---")
    w(f"exec run m:xor_self = {lname(diff)} ^ {lname(diff)}")
    w(f"exec run m:az_xor_self = m:xor_self ASSERT_ZERO nothing")
    w(f"exec run m:not_self = {lname(diff)} @ {lname(diff)}")
    w(f"exec run m:az_not_self = m:not_self ASSERT_ZERO nothing")
    w(f"exec run m:and_self = {lname(diff)} * {lname(diff)}")
    w(f"exec run m:id_and = m:and_self ^ {lname(diff)}")
    w(f"exec run m:az_id_and = m:id_and ASSERT_ZERO nothing")
    w(f"exec run m:or_self = {lname(diff)} + {lname(diff)}")
    w(f"exec run m:id_or = m:or_self ^ {lname(diff)}")
    w(f"exec run m:az_id_or = m:id_or ASSERT_ZERO nothing")
    w(f"exec run m:not_sub = o:diff_not_poly @ {lname(diff)}")
    w(f"exec run m:az_not_sub = m:not_sub ASSERT_ZERO nothing")
    w("")
    w("# --- Morphological / derivations ---")
    w(f"exec run m:g_poly = {lname(poly)} GROW nothing 0.1 0.1 0.1 0.1")
    w(f"exec run m:s_poly = {lname(poly)} SHRINK nothing 0.1 0.1 0.1 0.1")
    w(f"exec run m:sz_poly = {lname(poly)} SIZE nothing 0.1")
    w(f"exec run o:poly_ring = m:g_poly ~ {lname(poly)}")
    w(f"exec run o:poly_inner = {lname(poly)} ~ m:s_poly")
    w(f"exec run m:g_diff = {lname(diff)} GROW nothing 0.05 0.05 0.05 0.05")
    w("")
    w("# --- Topological/DE-9IM box ops (diff vs poly) ---")
    def bin_pair(op, a, b, base, az=True):
        nop = "NOT_" + op
        w(f"exec run m:{base} = {lname(a)} {op} {lname(b)}")
        w(f"exec run m:n{base} = {lname(a)} {nop} {lname(b)}")
        w(f"exec run m:{base}_c = m:{base} * m:n{base}")
        w(f"exec run m:{base}_az = m:{base}_c ASSERT_ZERO nothing")
    bin_pair("INTERACT", diff, poly, "ti")
    w(f"exec run m:ti_u = m:ti + m:nti")
    w(f"exec run m:ti_eq = m:ti_u ^ {lname(diff)}")
    w(f"exec run m:ti_eqaz = m:ti_eq ASSERT_ZERO nothing")
    bin_pair("CUT", diff, poly, "tc")
    bin_pair("TOUCH", diff, poly, "tt")
    bin_pair("INSIDE", diff, poly, "tin")
    bin_pair("OUTSIDE", diff, poly, "to")
    w(f"exec run m:to_u = m:to + m:nto")
    w(f"exec run m:to_eq = m:to_u ^ {lname(diff)}")
    w(f"exec run m:to_eqaz = m:to_eq ASSERT_ZERO nothing")
    w("")
    w("# --- Egenhofer/DE-9IM relations ---")
    bin_pair("DISJOINT", diff, poly, "disj")
    bin_pair("MEET", diff, poly, "meet")
    bin_pair("OVERLAPS", diff, poly, "ov")
    w(f"exec run m:eqr = {lname(diff)} EQUAL {lname(diff)}")
    w(f"exec run m:neqr = {lname(diff)} NOT_EQUAL {lname(diff)}")
    w(f"exec run m:eqr_c = m:eqr * m:neqr")
    w(f"exec run m:eqr_az = m:eqr_c ASSERT_ZERO nothing")
    w(f"exec run m:cont = m:g_poly CONTAINS {lname(poly)}")
    w(f"exec run m:ncont = m:g_poly NOT_CONTAINS {lname(poly)}")
    w(f"exec run m:cont_c = m:cont * m:ncont")
    w(f"exec run m:cont_az = m:cont_c ASSERT_ZERO nothing")
    w(f"exec run m:covby = {lname(poly)} COVEREDBY m:g_poly")
    w(f"exec run m:ncovby = {lname(poly)} NOT_COVEREDBY m:g_poly")
    w(f"exec run m:covby_c = m:covby * m:ncovby")
    w(f"exec run m:covby_az = m:covby_c ASSERT_ZERO nothing")
    w("")
    w("# --- Edge topological / DE-9IM edge ops ---")
    for op,base in [("INSIDE_EDGE","ie"),("OUTSIDE_EDGE","oe"),("COINCIDENT_INSIDE_EDGE","cie"),("COINCIDENT_OUTSIDE_EDGE","coe")]:
        w(f"exec run m:{base} = {lname(poly)} {op} m:g_poly")
        w(f"exec run m:n{base} = {lname(poly)} NOT_{op} m:g_poly")
    for base in ["oe","cie","coe","nie"]:
        w(f"exec run m:{base}_az2 = m:{base} ASSERT_ZERO nothing")
    w("# edge complement/exclusion checks")
    w("exec run m:ie_exc1 = m:ie * m:oe")
    w("exec run m:ie_exc1z = m:ie_exc1 ASSERT_ZERO nothing")
    w("exec run m:ie_exc2 = m:ie * m:cie")
    w("exec run m:ie_exc2z = m:ie_exc2 ASSERT_ZERO nothing")
    w("exec run m:ie_exc3 = m:ie * m:coe")
    w("exec run m:ie_exc3z = m:ie_exc3 ASSERT_ZERO nothing")
    w("")
    w("# --- More layer-pair coverage ---")
    w(f"exec run o:li1_not_met1 = {lname(li1)} @ {lname(met1)}")
    w(f"exec run m:self_lm = {lname(met1)} * o:li1_not_met1")
    w(f"exec run m:az_self_lm = m:self_lm ASSERT_ZERO nothing")
    w(f"exec run m:met1_ti = {lname(met1)} INTERACT {lname(met2)}")
    w(f"exec run m:met1_tni = {lname(met1)} NOT_INTERACT {lname(met2)}")
    w(f"exec run m:met1_tic = m:met1_ti * m:met1_tni")
    w(f"exec run m:met1_tiaz = m:met1_tic ASSERT_ZERO nothing")
    w("")
    w("flush")
    w("end")
    return "\n".join(L) + "\n"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("stats", help="db_oas_stats output file, or - for stdin")
    ap.add_argument("--design", required=True, help="OASIS design path to put in decl hierdesign")
    ap.add_argument("-o","--output", help="write CTR to this file (default stdout)")
    ap.add_argument("--out-mag", default="MW4x4_OASIS_SELFTEST_output.mag", help="output mag name in CTR")
    args = ap.parse_args()
    if args.stats == "-":
        lines = sys.stdin.readlines()
    else:
        with open(args.stats) as f: lines = f.readlines()
    info = parse_stats(lines)
    ctr = emit_ctr(info, args.design, args.out_mag)
    if args.output:
        with open(args.output, "w") as f: f.write(ctr)
    else:
        sys.stdout.write(ctr)

if __name__ == "__main__":
    main()
