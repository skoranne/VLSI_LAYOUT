# KLayout (0.30, Python) check of db_def2oas output for data/def2oas_props.def:
#   klayout -b -r def2oas_props_check.py -rd F=props.oas
# Prints one line per check and "DEF2OAS_PROPS PASS|FAIL".
import pya
ly = pya.Layout(); ly.read(F); top = ly.top_cell()
ok = True
def check(what, cond):
    global ok
    print(("PASS  " if cond else "FAIL  ") + what)
    ok = ok and bool(cond)
cp = top.properties()
check("top cell props_demo", top.name == "props_demo")
check("DEF_UNITS 1000", cp.get("DEF_UNITS") == "1000")
check("DEF_ROW has 2 rows", isinstance(cp.get("DEF_ROW"), list) and len(cp["DEF_ROW"]) == 2)
nets = cp.get("DEF_NET_INFO", [])
check("DEF_NET_INFO has 4 nets incl. unrouted n_mid", len(nets) == 4 and any(n.startswith("n_mid\tREGULAR") for n in nets))
check("net property and connections kept",
      "n_a\tREGULAR\t+ USE SIGNAL + PROPERTY critical 1\t( PIN A ) ( u_inv A )" in nets)
check("DEF_PIN_INFO", cp.get("DEF_PIN_INFO") == ["A\t+ NET n_a + DIRECTION INPUT + USE SIGNAL",
                                                  "Y\t+ NET n_y + DIRECTION OUTPUT + USE SIGNAL"])
check("PROPERTYDEFINITIONS kept", "COMPONENT owner STRING" in str(cp.get("DEF_PROPERTYDEFINITIONS")))
inst = {}
for i in top.each_inst():
    p = i.properties(); inst[p.get(61)] = (i.cell.name, str(i.trans), p)
check("4 named placements", sorted(inst) == ["u_buf", "u_fill", "u_inv", "u_nand"])
check("u_inv  N  at (1380,0)", inst["u_inv"][:2] == ("sky130_fd_sc_hd__inv_1", "r0 1380,0"))
check("u_buf  FS at (4600,2720) -> m0 4600,5440", inst["u_buf"][1] == "m0 4600,5440")
check("u_nand FN at (9200,0)    -> m90 10580,0", inst["u_nand"][1] == "m90 10580,0")
check("u_fill S  at (12880,2720) -> r180 13800,5440", inst["u_fill"][1] == "r180 13800,5440")
check("u_buf DEF_STATUS FIXED", inst["u_buf"][2].get("DEF_STATUS") == "FIXED")
check("u_buf DEF_PROPERTY owner 'alu team'", inst["u_buf"][2].get("DEF_PROPERTY") == ["owner", "alu team"])
check("u_nand DEF_ATTR SOURCE TIMING + WEIGHT 5", inst["u_nand"][2].get("DEF_ATTR") == ["SOURCE TIMING", "WEIGHT 5"])
check("u_inv DEF_ATTR SOURCE NETLIST, no DEF_STATUS", inst["u_inv"][2].get("DEF_ATTR") == "SOURCE NETLIST" and "DEF_STATUS" not in inst["u_inv"][2])
shapes = []
for li in ly.layer_indexes():
    info = ly.get_info(li)
    for s in top.shapes(li).each(): shapes.append((info.layer, info.datatype, s, s.properties()))
routed = [x for x in shapes if (x[0], x[1]) in ((68, 20), (69, 20)) and x[2].is_box()]
check("every met1/met2 shape has DEF_NET", routed and all("DEF_NET" in x[3] for x in routed))
check("VPWR followpin on met1", any(x[3].get("DEF_NET") == "VPWR" and x[0] == 68 for x in routed))
pinlab = [x for x in shapes if x[2].is_text()]
check("pin labels A/Y on 69/16 with DEF_NET/DEF_PIN",
      sorted((x[2].text_string, x[0], x[1], x[3].get("DEF_NET"), x[3].get("DEF_PIN")) for x in pinlab) ==
      [("A", 69, 16, "n_a", "A"), ("Y", 69, 16, "n_y", "Y")])
pinbox = [x for x in shapes if (x[0], x[1]) == (69, 16) and x[2].is_box()]
check("pin datatype boxes carry DEF_PIN", len(pinbox) == 2 and all("DEF_PIN" in x[3] for x in pinbox))
check("DIEAREA on 235/4", any((x[0], x[1]) == (235, 4) and x[2].box == pya.Box(0, 0, 20000, 10880) for x in shapes))
print("DEF2OAS_PROPS " + ("PASS" if ok else "FAIL"))
