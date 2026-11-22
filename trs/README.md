# trs/ — Term Rewrite Synthesis for CTR programs

| file | purpose |
|---|---|
| `../lean/` | Lean 4 proofs (`lake build`; Lean 4.36.0 via elan, core only) |
| `boxmodel.py` | Python transcription of the engine's box-level selector semantics |
| `gen_box_witness.py` | writes `lean/Trs/BoxWitness.lean` (one healed configuration per point type) |
| `gen_relations.py` | independent list generator; writes `../RELATIONS.md` and `../relations_trs.json`; `--check DIR` compares with Lean's export |
| `verify.sh` | build proofs, export Lean lists, compare with Python, scan for `sorry`/`native_decide` |
| `engine_check.py` | instantiate every rule on random designs and `ASSERT_ZERO` it in `src/MAGPARSER.exe` |
| `dag_check.py` | optimize random CTR programs (`--opt fortran` = `src/TRS_OPT.exe`, `julia` = `../trs_dag.jl`, `engine` = the pass inside MAGPARSER) and XOR-compare outputs in the engine |
| `fortran_vs_julia.py` | differential test: `src/TRS_OPT.exe` and `../trs_dag.jl` must emit identical CTRs on random programs |
| `../src/trs_optimizer.f90` | the compile-time TRS pass linked into MAGPARSER (reads `../relations_trs.rules`) |
| `legacy/` | superseded first version (see its README) |

Regenerate after changing semantics: `python3 gen_box_witness.py && python3 gen_relations.py && ./verify.sh`.

Key semantic facts (details in `../RELATIONS.md` and `../RELATIONS_TRS.tex`):
- CTR `AND` is single-layer self-overlap; intersection is `*` / `AND2`.
- Selectors keep whole healed boxes of A; their result depends on box decomposition, so a
  geometrically equal replacement is unsafe if the value later reaches a selector operand.
  Rewrites to the empty layer are always safe.
- Engines built before 2026-11-21 read freed memory in `CopyLayer` when output = input
  (`X = X COPY nothing`, or `X = X NOT Y` with empty Y). This is fixed in `src/design.f90`; the optimizer
  still never emits a self-COPY or an in-place replacement.
