# Rational gold check of the any-angle Booleans

Shows where TRAPBOOL and KLayout disagree, and which of the two is right, by comparing both
against an exact rational Boolean (Python `fractions`, no rounding anywhere).

## Files
- `kl_find_clips.py`: finds the largest TRAPBOOL vs KLayout XOR polygons on a full design and
  writes minimal clips around them. A clip holds the **raw** flattened input shapes that touch
  the window, one polygon each, never merged (merging would already round crossings).
- `gold_run.sh clip.oas [A B]` does three things:
  1. runs TRAPBOOL with OR, AND, NOT, XOR and BNOTA, then applies TRAPBOOL's own self-checks (the
     exact identities and zero anomalies);
  2. dumps KLayout's Booleans with `kl_dump.py`, computed the usual way (`A.merged() op B.merged()`);
  3. runs `gold_check.py --assert`.
- `gold_check.py`: computes E = A op B exactly with a slab sweep cut at every vertex row and every
  exact edge crossing. For each candidate result C (ours, KLayout):
  - `dA` = exact area of C xor E;
  - `H` = boundary deviation, the maximum over C xor E of the distance to the exact boundary of E
    (L2 and Linf).
  - Assertions:
    - our `H_inf` is at most 1/2 dbu, the snap-rounding tube;
    - the exact area of our pieces equals TRAPBOOL's `area2`/2;
    - OR = NOT + AND + BNOTA and XOR = OR − AND hold exactly.
- `run_gold.sh`: the regression over every clip in `clips/*.oas`; `summarize_gold.py` reports the
  worst case for each tool.
- `kl_gen_synth.py`: random tiny cases (two crossing A shapes and one B shape).
- `kl_double_round.py`: shows KLayout's two-step rounding on one clip.
- `kl_win_cmp.py`, `kl_src.py`, `kl_src2.py`, `dump_flat.cpp`: diagnostics (full design vs clip
  inside a window, source shapes and their transforms, our flattened shapes).

## Why the two tools differ
Snap rounding moves each edge only within the Linf 1/2-pixel tube of the original edge. One
rounding of A ∪ B together therefore gives `H_inf <= 1/2`. Slivers thinner than a pixel may vanish,
but they lie within 1/2 dbu of the exact boundary.

KLayout's usual flow (`A.merged() & B.merged()`) rounds twice:
1. merging A rounds the crossings between A's own shapes and so tilts A's edges;
2. the Boolean then rounds the crossings with those tilted edges.

The second rounding can add another 1/2 dbu. `clips/synth_s0028.oas` shows it:
- B − A: the exact vertex is at (14.51, 14.40);
- ours puts it at (15,15), with H = 0.49;
- KLayout's merged flow puts it at (16,15), with H = 1.49;
- `B - A` in KLayout over the raw shapes (a single pass) gives (15,15), the same as ours.

## Results (2026-11-26)
| set | op-cases | ours max H_inf | KLayout max H_inf | KLayout > 1/2 |
|---|---|---|---|---|
| 200 synthetic | 1000 | 0.5000 | 1.4878 | 72 |
| SDT16S R045 clips (poly × diff) | 20 | 0.5000 | 0.7793 | 2 |

On the SDT16S clips the exact area error (sum of `dA`) is 46,048 for ours and 48,689 for KLayout.
Ours is closer in 9 cases, KLayout in 1, and they are equal in 10.

Run: `TRAPBOOL=../../../TRAPBOOL.exe ./run_gold.sh`
