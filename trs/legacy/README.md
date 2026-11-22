# Legacy TRS pipeline (superseded)

These files are the first version of the relation generator and its proofs. They are kept
for reference only; do not use them. Superseded by `lean/` (proofs), `trs/gen_relations.py`
(generator), `trs/verify.sh` (verification) and the regenerated `RELATIONS.md`.

Why they were replaced (see the review in RELATIONS_TRS.tex, Section "Corrections"):

- Topological selectors were modelled as pointwise set operations (`INSIDE = A∧B`,
  `CONTAINS = COVERS = COVEREDBY = A∧B`, `MEET_PART = DISJOINT_PART = A∖B`). The engine
  (`src/design_topo_impl.f90`) selects whole healed boxes, so these models prove false
  statements, e.g. `(A INSIDE B) XOR (A AND B) == 0`.
- The "3^n 9E-DIM" model used independent interior/boundary atoms and unregularized
  boundary points; it is not tied to the engine.
- The enumeration kept only one representative per semantic class, so "ALL identities"
  was a sample; depth ≤ 3 reaches 52657 of 65536 functions at n = 4, not all.
- `verify_complete_equiv.py` and `verify_de9im_relations.py` assert tautologies
  (`dnf_mask` returns its input; the 9E-DIM verifier only prints).
- `RelationsDE9IM.lean` contains no theorems; `Relations.lean`'s DE-9IM theorems are `x = x`.
- `RELATIONS.md` counts (857/2790/6394) were stale relative to the generator.
