/-
  Trs/Bool.lean — Boolean layer algebra: semantics, decision procedure, completeness.

  Expressions are CTR geometric Booleans over `n` input layers:
    `and` = CTR `*` / `AND2`,  `or` = CTR `+` / `OR2`,
    `diff` = CTR `NOT` / `@` / `!`,  `xor` = CTR `XOR` / `^`.
  `compl` (unary complement) has no CTR operator; it is only meaningful relative to
  an EXTENT universe, and is kept so that the theorems also cover that case.

  Main results
  * `valid_iff_bool`   : an identity holds for all layouts (all sets over any universe)
                         iff it holds for every Bool assignment of the n layers.
  * `decideEquiv_iff`  : `decideEquiv` (a 2^n-row truth-table check) returns `true`
                         iff the identity holds for every Bool assignment.
  * `valid_iff_decide` : together — `decideEquiv` is a sound and complete decision
                         procedure for the identities of the layer algebra.  No valid
                         relation can be missed: every candidate is decided.
-/
namespace Trs.Bool

inductive Expr (n : Nat) where
  | var   : Fin n → Expr n
  | zero  : Expr n
  | and   : Expr n → Expr n → Expr n
  | or    : Expr n → Expr n → Expr n
  | diff  : Expr n → Expr n → Expr n
  | xor   : Expr n → Expr n → Expr n
  | compl : Expr n → Expr n
  deriving DecidableEq, Repr

namespace Expr

/-- Bool (one point) semantics. -/
def eval : Expr n → (Fin n → Bool) → Bool
  | var i,    ρ => ρ i
  | zero,     _ => false
  | and a b,  ρ => a.eval ρ && b.eval ρ
  | or a b,   ρ => a.eval ρ || b.eval ρ
  | diff a b, ρ => a.eval ρ && !b.eval ρ
  | xor a b,  ρ => Bool.xor (a.eval ρ) (b.eval ρ)
  | compl a,  ρ => !a.eval ρ

/-- Set semantics: each layer is an arbitrary subset `L i` of an arbitrary universe `U`. -/
def evalSet {U : Type} : Expr n → (Fin n → U → Prop) → U → Prop
  | var i,    L, u => L i u
  | zero,     _, _ => False
  | and a b,  L, u => a.evalSet L u ∧ b.evalSet L u
  | or a b,   L, u => a.evalSet L u ∨ b.evalSet L u
  | diff a b, L, u => a.evalSet L u ∧ ¬ b.evalSet L u
  | xor a b,  L, u => (a.evalSet L u ∧ ¬ b.evalSet L u) ∨ (¬ a.evalSet L u ∧ b.evalSet L u)
  | compl a,  L, u => ¬ a.evalSet L u

open Classical in
/-- Transfer: set semantics at a point is Bool semantics of the membership vector. -/
theorem evalSet_iff {U : Type} (e : Expr n) (L : Fin n → U → Prop) (u : U) :
    e.evalSet L u ↔ e.eval (fun i => decide (L i u)) = true := by
  induction e with
  | var i => simp [evalSet, eval]
  | zero => simp [evalSet, eval]
  | and a b iha ihb => simp [evalSet, eval, iha, ihb]
  | or a b iha ihb => simp [evalSet, eval, iha, ihb]
  | diff a b iha ihb => simp [evalSet, eval, iha, ihb]
  | xor a b iha ihb =>
      simp only [evalSet, eval, iha, ihb]
      cases a.eval (fun i => decide (L i u)) <;> cases b.eval (fun i => decide (L i u)) <;> simp
  | compl a iha => simp [evalSet, eval, iha]

end Expr

open Expr

/-- An identity is *valid* if it holds pointwise for all layers over every universe. -/
def Valid (e₁ e₂ : Expr n) : Prop :=
  ∀ (U : Type) (L : Fin n → U → Prop) (u : U), e₁.evalSet L u ↔ e₂.evalSet L u

theorem valid_iff_bool (e₁ e₂ : Expr n) :
    Valid e₁ e₂ ↔ ∀ ρ : Fin n → Bool, e₁.eval ρ = e₂.eval ρ := by
  constructor
  · intro h ρ
    have := h Unit (fun i _ => ρ i = true) ()
    rw [evalSet_iff, evalSet_iff] at this
    simp only [Bool.decide_eq_true] at this
    cases h1 : e₁.eval ρ <;> cases h2 : e₂.eval ρ <;> simp_all
  · intro h U L u
    rw [evalSet_iff, evalSet_iff, h]

/-! ### Truth-table decision procedure -/

/-- All Bool lists of length `n`. -/
def allLists : Nat → List (List Bool)
  | 0 => [[]]
  | k + 1 => (allLists k).flatMap fun l => [false :: l, true :: l]

theorem mem_allLists : ∀ (n : Nat) (l : List Bool), l.length = n → l ∈ allLists n
  | 0, [], _ => by simp [allLists]
  | k + 1, b :: l, h => by
      simp only [List.length_cons, Nat.add_right_cancel_iff] at h
      simp only [allLists, List.mem_flatMap]
      exact ⟨l, mem_allLists k l h, by cases b <;> simp⟩

def envOf (l : List Bool) : Fin n → Bool := fun i => l.getD i.val false

/-- Truth-table equivalence check over all 2^n assignments. -/
def decideEquiv (e₁ e₂ : Expr n) : Bool :=
  (allLists n).all fun l => e₁.eval (envOf l) == e₂.eval (envOf l)

theorem envOf_ofFn (ρ : Fin n → Bool) : envOf (List.ofFn ρ) = ρ := by
  funext i
  simp [envOf, List.getD_eq_getElem?_getD]

theorem decideEquiv_iff (e₁ e₂ : Expr n) :
    decideEquiv e₁ e₂ = true ↔ ∀ ρ : Fin n → Bool, e₁.eval ρ = e₂.eval ρ := by
  constructor
  · intro h ρ
    simp only [decideEquiv, List.all_eq_true, beq_iff_eq] at h
    have := h (List.ofFn ρ) (mem_allLists n _ (by simp))
    rwa [envOf_ofFn] at this
  · intro h
    simp only [decideEquiv, List.all_eq_true, beq_iff_eq]
    intro l _
    exact h _

/-- Completeness of the decision procedure for the layer algebra. -/
theorem valid_iff_decide (e₁ e₂ : Expr n) : Valid e₁ e₂ ↔ decideEquiv e₁ e₂ = true := by
  rw [valid_iff_bool, decideEquiv_iff]

/-- Zero identities `E == 0` are the special case `e₂ = zero`. -/
theorem valid_zero_iff (e : Expr n) :
    (∀ (U : Type) (L : Fin n → U → Prop) (u : U), ¬ e.evalSet L u) ↔
      decideEquiv e .zero = true := by
  rw [← valid_iff_decide]
  simp [Valid, evalSet]

/-! ### Exhaustive term enumeration (CTR-expressible operators only)

`levels n k` lists, by operator count `s = 0..k`, every term built from the input layers
with `and/or/diff/xor` (no `zero`, no `compl`).  The generator `trs/gen_relations.py`
mirrors this order exactly, so the relation lists in RELATIONS.md can be checked against
it verbatim (see Trs/BoolLists.lean). -/

def vars (n : Nat) : List (Expr n) := (List.finRange n).map Expr.var

def nextLevel (lv : List (List (Expr n))) : List (Expr n) :=
  let s := lv.length
  (List.range s).flatMap fun i =>
    (lv.getD i []).flatMap fun x =>
      (lv.getD (s - 1 - i) []).flatMap fun y =>
        [Expr.and x y, Expr.or x y, Expr.diff x y, Expr.xor x y]

def levels (n : Nat) : Nat → List (List (Expr n))
  | 0 => [vars n]
  | k + 1 => let l := levels n k; l ++ [nextLevel l]

def termsUpTo (n k : Nat) : List (Expr n) := (levels n k).flatten

def varName (i : Nat) : String :=
  match i with
  | 0 => "A" | 1 => "B" | 2 => "C" | 3 => "D" | 4 => "E" | 5 => "F"
  | _ => "L" ++ toString i

/-- CTR-style rendering, identical to the generator's. -/
def Expr.render : Expr n → String
  | var i    => varName i.val
  | zero     => "0"
  | and a b  => "(" ++ a.render ++ " * " ++ b.render ++ ")"
  | or a b   => "(" ++ a.render ++ " + " ++ b.render ++ ")"
  | diff a b => "(" ++ a.render ++ " NOT " ++ b.render ++ ")"
  | xor a b  => "(" ++ a.render ++ " ^ " ++ b.render ++ ")"
  | compl a  => "(COMPL " ++ a.render ++ ")"

/-- Terms (size ≤ k) that are identically zero. -/
def zeroTerms (n k : Nat) : List String :=
  ((termsUpTo n k).filter fun e => decideEquiv e .zero).map Expr.render

/-- Terms (size 1..k) that are identically equal to an input layer, rendered `E == X`. -/
def inputTerms (n k : Nat) : List String :=
  ((termsUpTo n k).filter fun e => !(vars n).contains e).filterMap fun e =>
    ((vars n).find? fun v => decideEquiv e v).map fun v => e.render ++ " == " ++ v.render

end Trs.Bool
