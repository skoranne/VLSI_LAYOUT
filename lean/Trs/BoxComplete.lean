/-
  Trs/BoxComplete.lean — completeness of the 9E-DIM point-type decision procedure.

  `complete` : an identity between mixed expressions (A, B, selectors σ(A,B), geometric
  Booleans) holds at every cell of every healed configuration iff it holds on all valid
  point types.  Soundness uses `ptOf_valid` + `cellEval_eq`; completeness uses the
  generated witness table, every entry of which is re-checked here by the kernel.
-/
import Trs.BoxTopo
import Trs.BoxWitness

namespace Trs.Box

set_option maxRecDepth 20000

def allPT : List PT :=
  allBools.flatMap fun mA => allBools.flatMap fun mB => allTy.map fun t => PT.mk mA mB t

def validPTs : List PT := allPT.filter PT.valid

theorem mem_validPTs (p : PT) (h : p.valid = true) : p ∈ validPTs := by
  obtain ⟨mA, mB, t⟩ := p
  simp only [validPTs, allPT, List.mem_filter, List.mem_flatMap, List.mem_map, allBools,
    List.mem_cons, List.mem_singleton]
  exact ⟨⟨mA, by cases mA <;> simp, mB, by cases mB <;> simp, t, mem_allTy t, rfl⟩, h⟩

theorem validPTs_length : validPTs.length = 71 := by decide +kernel

/-- Decision procedure: compare on the 71 valid point types. -/
def decideM (e₁ e₂ : MExpr) : Bool := validPTs.all fun p => ptEval p e₁ == ptEval p e₂

/-- Validity: equal at every cell of every healed configuration. -/
def HealedValid (e₁ e₂ : MExpr) : Prop :=
  ∀ A B : List Box, healedB A = true → healedB B = true → ∀ c : Cell,
    cellEval A B c e₁ = cellEval A B c e₂

def witOk (p : PT) : Bool :=
  match witTable.find? (·.1 == p) with
  | some (_, w) => healedB w.A && healedB w.B && ptOf w.A w.B w.c == p
  | none => false

/-- Every valid point type is realized by a checked healed configuration. -/
theorem witOk_all : validPTs.all witOk = true := by decide +kernel

theorem realize (p : PT) (hp : p ∈ validPTs) :
    ∃ A B : List Box, ∃ c : Cell, healedB A = true ∧ healedB B = true ∧ ptOf A B c = p := by
  have h := List.all_eq_true.mp witOk_all p hp
  unfold witOk at h
  split at h
  · rename_i q w _
    simp only [Bool.and_eq_true, beq_iff_eq] at h
    exact ⟨w.A, w.B, w.c, h.1.1, h.1.2, h.2⟩
  · exact absurd h (by simp)

theorem healed_ok {L : List Box} (h : healedB L = true) : ∀ b ∈ L, b.ok = true := by
  simp only [healedB, Bool.and_eq_true, List.all_eq_true] at h; exact h.1

theorem complete (e₁ e₂ : MExpr) : HealedValid e₁ e₂ ↔ decideM e₁ e₂ = true := by
  constructor
  · intro h
    simp only [decideM, List.all_eq_true, beq_iff_eq]
    intro p hp
    obtain ⟨A, B, c, hA, hB, rfl⟩ := realize p hp
    have := h A B hA hB c
    rwa [cellEval_eq A B c hA (healed_ok hB), cellEval_eq A B c hA (healed_ok hB)] at this
  · intro h A B hA hB c
    rw [cellEval_eq A B c hA (healed_ok hB), cellEval_eq A B c hA (healed_ok hB)]
    simp only [decideM, List.all_eq_true, beq_iff_eq] at h
    exact h _ (mem_validPTs _ (ptOf_valid A B c hA hB))

/-- Zero identities `E == 0`. -/
theorem complete_zero (e : MExpr) :
    (∀ A B : List Box, healedB A = true → healedB B = true → ∀ c, cellEval A B c e = false) ↔
      decideM e .zero = true := by
  rw [← complete]; rfl

/-! ### Exhaustive enumeration of mixed terms -/

def allBase : List Base :=
  [.interact, .inside, .outside, .touch, .cut, .disjoint, .meet, .equal, .contains, .covers,
   .coveredby, .overlaps]

def allOps : List Op :=
  (allBase.map fun b => Op.mk b false) ++ (allBase.map fun b => Op.mk b true)

/-- Atoms: A, B and all 24 selectors σ(A,B). -/
def matoms : List MExpr := [.la, .lb] ++ allOps.map MExpr.sel

def mnext (lv : List (List MExpr)) : List MExpr :=
  let s := lv.length
  (List.range s).flatMap fun i =>
    (lv.getD i []).flatMap fun x =>
      (lv.getD (s - 1 - i) []).flatMap fun y =>
        [MExpr.and x y, MExpr.or x y, MExpr.diff x y, MExpr.xor x y]

def mlevels : Nat → List (List MExpr)
  | 0 => [matoms]
  | k + 1 => let l := mlevels k; l ++ [mnext l]

def mtermsUpTo (k : Nat) : List MExpr := (mlevels k).flatten

def Base.name : Base → String
  | .interact => "INTERACT" | .inside => "INSIDE" | .outside => "OUTSIDE" | .touch => "TOUCH"
  | .cut => "CUT" | .disjoint => "DISJOINT" | .meet => "MEET" | .equal => "EQUAL"
  | .contains => "CONTAINS" | .covers => "COVERS" | .coveredby => "COVEREDBY"
  | .overlaps => "OVERLAPS"

def MExpr.render : MExpr → String
  | .la => "A"
  | .lb => "B"
  | .sel op => "(A " ++ (if op.neg then "NOT_" else "") ++ op.base.name ++ " B)"
  | .zero => "0"
  | .and x y => "(" ++ x.render ++ " * " ++ y.render ++ ")"
  | .or x y => "(" ++ x.render ++ " + " ++ y.render ++ ")"
  | .diff x y => "(" ++ x.render ++ " NOT " ++ y.render ++ ")"
  | .xor x y => "(" ++ x.render ++ " ^ " ++ y.render ++ ")"

/-- Terms (size ≤ k) that are identically empty in every healed configuration. -/
def mzeroTerms (k : Nat) : List String :=
  ((mtermsUpTo k).filter fun e => decideM e .zero).map MExpr.render

/-- Non-atomic terms (size ≤ k) identically equal to an atom (A, B or a selector). -/
def matomTerms (k : Nat) : List String :=
  ((mtermsUpTo k).filter fun e => !matoms.contains e).filterMap fun e =>
    (matoms.find? fun v => decideM e v).map fun v => e.render ++ " == " ++ v.render

/-- Atom pairs that coincide (`v₁ == v₂`, v₁ listed before v₂). -/
def matomEqs : List String :=
  (List.range matoms.length).flatMap fun i => (List.range matoms.length).filterMap fun j =>
    if i < j && decideM (matoms.getD i .zero) (matoms.getD j .zero) then
      some ((matoms.getD j .zero).render ++ " == " ++ (matoms.getD i .zero).render)
    else none

end Trs.Box
