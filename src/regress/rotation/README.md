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
polygons, holes, vertices, area (exact), perimeter (exact for Manhattan data). The CTR
(`rot_ops.ctr.in`) measures seven SkyWater 130 layers and five boolean results (AND, two NOTs,
OR, XOR). GROW/SIZE are left out because their Manhattan corners are not rotation invariant.

Tolerance at angles that are not multiples of 90: polygons, holes and vertices identical;
|dA| <= 0.7072 * P0 (each vertex moves at most sqrt(2)/2 dbu); |dP| <= 1.5 * V0.
MAGPARSER failures at those angles are reported as **XFAIL**. Its layers are boxes, so a
rotated design is approximated by rectangle strips until the any-angle loader (phase 4 of
the any-angle plan) is in place. These cases are the acceptance test for that work.

## Running

```bash
# 1. inputs (KLayout 0.28+ with Python); writes DIR/TAG_R000.oas ... TAG_R180.oas
klayout -b -r gen_rotated.py -rd src=/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SMALL_SDT/SDT_16.oas \
        -rd out=/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/ROTATION -rd name=SDT16S
# 2. run (one job at a time, MemAvailable watchdog); SKIP_KL/SKIP_MAG/SKIP_DB=1 skip steps
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
