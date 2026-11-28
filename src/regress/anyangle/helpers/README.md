# Ad-hoc KLayout / diagnostic helpers (any-angle work)

These are kept for reproducibility. Most take `-rd` arguments: `klayout -b -r X.py -rd f=...`.

| script | purpose |
|---|---|
| `kl_topo.py` | KLayout polygon-level topological selects (interacting/inside/outside/touching) used as reference for `trap_topo.f90` |
| `ksa.py` | KLayout single-layer AND reference (`merged(False, 2)`: self-overlap, the correct selfand reference) |
| `kwc.py` | demonstrates `Region.merged(False, min_wc)` semantics (`min_wc=1` acts like plain merge) |
| `klcells.py`, `klcount.py`, `kldump.py` | per-cell / per-layer shape counts and dumps for flatten checks |
| `tm_ar.py`, `tm_xor2.py`, `tm_dumpxor.py` | TRAPMERGE area / XOR versus KLayout; dump XOR polygons |
| `tm_comp.py`, `tm_ovl.py`, `tm_shapechk.py`, `tm_shapechk2.py` | check dumped trapezoid lists (`y1 y2 ...` lines): components, overlaps, shape validity |
| `tm_p2poly.py` | rewrite a layer's shapes as merged polygons |
| `tm_pth.py` | KLayout path outline for a sample path, used to check `db` path outlines |
| `wd_run.sh` | runs `run_mag_rot.sh` behind a MemAvailable watchdog (edit the path at the top) |
