# Rotation-invariance regression

The same layout is placed at 13 rotations (0, 15, ..., 180 degrees) by **one top-cell
placement**. The shapes themselves are not rotated in the file, so the reader's flatten
logic has to apply the rotation. Every quantity checked is invariant under rotation, so each
rotated run must reproduce the unrotated baseline.

| step | tool | what must hold |
|------|------|----------------|
| flatten | `db/db_rotcheck --mode poly` | flattened polygons of `TAG_R<a>` == polygons of `TAG_R000` rotated by `a` with one rounding per vertex (half away from zero, like KLayout); exact at **every** angle |
| box path | `db/db_rotcheck --mode box` | `ldb_iterate_boxes_hier` (what MAGPARSER loads with) is exact at multiples of 90; at other angles boxes become rectangle strips and only the area ratio is reported |
| mag@0 | `MAGPARSER.exe` + `exec measure` | measurements of `TAG_R000` == KLayout's on the same file |
| mag(a) | `MAGPARSER.exe` | measurements of `R<a>` == those of `R000` (exact at multiples of 90, tolerance otherwise, see below) |
| kl(a) | KLayout `kl_measure.py` | the same comparison done by KLayout: how much the grid rounding itself moves the numbers |

Measurements (`exec measure d1:LAYER ...`, `src/measure.f90`) are taken on the merged layer, a
closed set, so shapes touching at a corner belong to one polygon (KLayout `merged(False)`):
polygons, holes, vertices, area (exact), perimeter (exact for Manhattan data).

`rot_ops.ctr.in` runs, on SkyWater 130 layers (A = poly 66/20, B = diff 65/20, C = li1 67/20,
M1 = met1 68/20, M2 = met2 69/20):

* **Booleans**: `*` and `AND2` (A AND B), `!` `@` `~` `FRAMENOT` (B NOT A), `+` (A OR C, A OR B),
  `^` and `XOR` (A XOR B), M1 AND M2, single-layer `AND` (self-overlap of C)
* **Morphology** on M1: `GROW` 0.05 um, `SHRINK` 0.03 um, `SIZE` +0.05 / -0.03 um, closing
  (grow then shrink 0.05) and opening (shrink then grow 0.03). Edge-offset sizing is rotation
  invariant, since each edge moves along its own normal; KLayout `Region.sized(d)` is the
  reference
* **Identities**: layers `id_*` must be empty at every angle: A^A, ((A+B)-A*B)^(A^B),
  `XOR`==`^`, `AND2`==`*`, `FRAMENOT`==`@`==`~`==`!`, (B-(A*B))^(B-A), M1-grow, shrink-M1,
  M1-closing, opening-M1, `SIZE`==`GROW`/`SHRINK`. Also area(A+B)+area(A*B) = area(A)+area(B)

`kl_measure.py -rd derived=1` computes the same layers, under the same names, in KLayout.
`compare.py` reports for every angle:

| column | meaning |
|--------|---------|
| mag inv / kl inv | every layer on R<a> vs the same layer on R000 |
| mag=kl | MAGPARSER vs KLayout on the same rotated file (equivalence) |
| mag ids / kl ids | identity layers empty; inclusion-exclusion holds |

At multiples of 90 everything must be exact. At other angles each vertex snaps to the grid
(at most sqrt(2)/2 dbu), so: polygons, holes and vertices identical; |dA| <= 0.7072 * P;
|dP| <= 1.5 * V; identity areas <= 0.7072 * perimeter of the inputs.
MAGPARSER failures at those angles are reported as **XFAIL**. Its layers are boxes, so a
rotated design reaches it as rectangle strips until the any-angle engine (phases 3-4 of the
any-angle plan) is in place. The KLayout columns show what exact any-angle processing
achieves on the same data; these cases are the acceptance test for that work.

## Running

```bash
# 1. inputs (KLayout 0.28+ with Python); writes DIR/TAG_R000.oas ... TAG_R180.oas
klayout -b -r gen_rotated.py -rd src=/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SMALL_SDT/SDT_16.oas \
        -rd out=/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/ROTATION -rd name=SDT16S
# 2. run (one job at a time, MemAvailable watchdog, resumable); SKIP_KL/SKIP_MAG/SKIP_DB=1 skip
#    steps; MAG_ORTHO_ONLY=1 runs MAGPARSER at multiples of 90 only (large designs: rotated box
#    data is strip-approximated, LDB_SLANT_STRIPS=1 by default here)
./run_rotation.sh SDT16S                    # or ./run_rotation.sh SDT16S "0 45 90"
# 3. summary (exit 1 on a hard failure)
python3 compare.py out/SDT16S --detail
```

Needs `db/db_rotcheck` (`make -C ../../db db_rotcheck`) and `MAGPARSER.exe` built in `src/`.

Designs used (inputs in `/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/ROTATION`):

| tag | source | flat polygons |
|-----|--------|---------------|
| SDT16S | SMALL_SDT/SDT_16.oas (hierarchical, 89 cells) | 10.0 M |
| MW | ~/MICROWATT.oas (hierarchical) | |
| SDT6X6 | SDT16_DATA/SDT16_6x6.oas (flat, one cell) | |

Note: SDT_16.oas contains repetitions with a zero displacement (two identical pin shapes,
layers 64/16, 68/16, 122/16). The reader keeps both copies, as the OASIS spec reads; KLayout
keeps one, so a KLayout rewrite (`TAG_R000`) has 12,458 fewer raw shapes than the original.
Merged geometry is the same either way.
