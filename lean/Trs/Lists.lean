/-
  Trs/Lists.lean — the relation lists of RELATIONS.md, characterized and counted.

  For each list, `*_spec` states it is exactly the set of size-bounded terms satisfying the
  relation (sound AND complete, by the proven decision procedures), and the counts are
  checked by the kernel (`decide +kernel`, no native code).  RELATIONS.md is printed from
  these same definitions by `Export.lean` and cross-checked against the independent Python
  generator in trs/gen_relations.py.
-/
import Trs.Bool
import Trs.BoxComplete

set_option maxRecDepth 100000
set_option maxHeartbeats 0

namespace Trs.Bool

/-- Zero list: exactly the size-≤k terms that are empty in every set model. -/
theorem zeroTerms_spec (n k : Nat) (s : String) :
    s ∈ zeroTerms n k ↔ ∃ e ∈ termsUpTo n k, Valid e .zero ∧ e.render = s := by
  simp [zeroTerms, valid_iff_decide, and_assoc]

/-- Input list soundness: every entry `E == X` is a valid identity with X an input layer. -/
theorem inputTerms_sound (n k : Nat) (s : String) (h : s ∈ inputTerms n k) :
    ∃ e ∈ termsUpTo n k, ∃ v ∈ vars n, Valid e v ∧ s = e.render ++ " == " ++ v.render := by
  simp only [inputTerms, List.mem_filterMap, List.mem_filter, Option.map_eq_some_iff] at h
  obtain ⟨e, ⟨he, _⟩, v, hv, rfl⟩ := h
  have hv' := List.find?_some hv
  exact ⟨e, he, v, List.mem_of_find?_eq_some hv, (valid_iff_decide e v).mpr hv', rfl⟩

/-- Input list completeness: every non-atomic size-≤k term equal to some input is listed. -/
theorem inputTerms_complete (n k : Nat) (e : Expr n) (he : e ∈ termsUpTo n k)
    (hna : e ∉ vars n) (v : Expr n) (hv : v ∈ vars n) (hval : Valid e v) :
    ∃ w ∈ vars n, Valid e w ∧ e.render ++ " == " ++ w.render ∈ inputTerms n k := by
  have hd : decideEquiv e v = true := (valid_iff_decide e v).mp hval
  cases hf : (vars n).find? (fun w => decideEquiv e w) with
  | none => exact absurd (List.find?_eq_none.mp hf v hv) (by simp [hd])
  | some w =>
      refine ⟨w, List.mem_of_find?_eq_some hf, (valid_iff_decide e w).mpr (List.find?_some hf), ?_⟩
      simp only [inputTerms, List.mem_filterMap, List.mem_filter]
      exact ⟨e, ⟨he, by simpa using hna⟩, by simp [hf]⟩

theorem zero2 : (zeroTerms 2 3).length = 1306 := by decide +kernel
theorem zero3 : (zeroTerms 3 3).length = 4176 := by decide +kernel
theorem zero4 : (zeroTerms 4 3).length = 9532 := by decide +kernel
theorem input2 : (inputTerms 2 3).length = 1572 := by decide +kernel
theorem input3 : (inputTerms 3 3).length = 4986 := by decide +kernel
theorem input4 : (inputTerms 4 3).length = 11272 := by decide +kernel
theorem terms4 : (termsUpTo 4 3).length = 84036 := by decide +kernel

end Trs.Bool

namespace Trs.Box

theorem mzeroTerms_spec (k : Nat) (s : String) :
    s ∈ mzeroTerms k ↔ ∃ e ∈ mtermsUpTo k, HealedValid e .zero ∧ e.render = s := by
  simp [mzeroTerms, complete, and_assoc]

theorem matomTerms_sound (k : Nat) (s : String) (h : s ∈ matomTerms k) :
    ∃ e ∈ mtermsUpTo k, ∃ v ∈ matoms, HealedValid e v ∧ s = e.render ++ " == " ++ v.render := by
  simp only [matomTerms, List.mem_filterMap, List.mem_filter, Option.map_eq_some_iff] at h
  obtain ⟨e, ⟨he, _⟩, v, hv, rfl⟩ := h
  exact ⟨e, he, v, List.mem_of_find?_eq_some hv, (complete e v).mpr (List.find?_some hv), rfl⟩

theorem matomTerms_complete (k : Nat) (e : MExpr) (he : e ∈ mtermsUpTo k) (hna : e ∉ matoms)
    (v : MExpr) (hv : v ∈ matoms) (hval : HealedValid e v) :
    ∃ w ∈ matoms, HealedValid e w ∧ e.render ++ " == " ++ w.render ∈ matomTerms k := by
  have hd : decideM e v = true := (complete e v).mp hval
  cases hf : matoms.find? (fun w => decideM e w) with
  | none => exact absurd (List.find?_eq_none.mp hf v hv) (by simp [hd])
  | some w =>
      refine ⟨w, List.mem_of_find?_eq_some hf, (complete e w).mpr (List.find?_some hf), ?_⟩
      simp only [matomTerms, List.mem_filterMap, List.mem_filter]
      exact ⟨e, ⟨he, by simpa using hna⟩, by simp [hf]⟩

theorem mterms1 : (mtermsUpTo 1).length = 2730 := by decide +kernel
theorem mzero1 : (mzeroTerms 1).length = 353 := by decide +kernel
theorem matom1 : (matomTerms 1).length = 1283 := by decide +kernel
theorem matomEqs_val : matomEqs =
    ["(A DISJOINT B) == (A OUTSIDE B)", "(A NOT_DISJOINT B) == (A NOT_OUTSIDE B)"] := by
  decide +kernel

/-- Idempotence of every selector (healed A): σ(σ(A,B),B) = σ(A,B) cellwise. -/
theorem sel_idem (op : Op) (A B : List Box) (c : Cell) (hA : healedB A = true) :
    (engineLayer op (engineLayer op A B) B).any (inCell c) = cellEval A B c (.sel op) := by
  rw [nest_cell op op A B c hA]
  simp [cellEval]

end Trs.Box
