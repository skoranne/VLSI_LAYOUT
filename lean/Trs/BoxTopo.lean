/-
  Trs/BoxTopo.lean — engine-faithful 9E-DIM / topological selector semantics.

  Source of truth: `CalculateTopologicalSelect` in src/design_topo_impl.f90.
  Every selector keeps or drops *whole boxes* of the first operand A, depending on how
  that box relates to the individual boxes of B.  Both operands have gone through
  `PreprocessLayer`, which heals them into boxes with pairwise disjoint interiors.

  Model / assumptions (stated once, used everywhere):
  * boxes are closed, axis-aligned, integer, non-degenerate (`Box.ok`);
  * layers are lists of boxes; a *healed* layer is pairwise interior-disjoint;
  * a *cell* `(cx,cy)` is the unit square [cx,cx+1]×[cy,cy+1].  With integer coordinates,
    two regions are equal up to measure zero iff they contain the same cells, so
    "pointwise" in this file means "cellwise";
  * the R-tree DFS visits every B box touching A's box (src stack is 256 deep and silently
    drops nodes beyond that; we assume it does not overflow), and the GPU path agrees
    with the CPU path.

  Main results
  * `engineSel_eq`  : the engine's selection of box `a` equals `selPred op (tyOf a B)`,
                      where `tyOf a B` records which of the 8 non-disjoint pairwise classes
                      (9 with DISJ) `a` has against B.
  * `tyOf_realizable` : for healed B, `tyOf a B` avoids 14 forbidden class pairs.
  * `ptOf_valid` + `cellEval_eq_ptEval` : every cell of every healed configuration has a
                      valid point type, and expressions evaluate through it.
  * `complete`      : an identity between mixed expressions (A, B, any selector σ(A,B),
                      geometric Booleans) holds in every healed configuration iff it holds on
                      all 71 valid point types — a sound and complete decision procedure.
  * `nest_eq`       : σ₂(σ₁(A,B),B) keeps exactly the boxes kept by both, so nested
                      selectors over the same B are conjunctions (hence σ(σ(A,B),B) = σ(A,B)).
-/
namespace Trs.Box

set_option maxRecDepth 20000

structure Box where
  x1 : Int
  y1 : Int
  x2 : Int
  y2 : Int
  deriving DecidableEq, Repr

def Box.ok (a : Box) : Bool := decide (a.x1 < a.x2) && decide (a.y1 < a.y2)

/-! ### Pairwise predicates, transcribed from design_topo_impl.f90 -/

/-- closed boxes intersect (the `cycle` test negated). -/
def meets (a b : Box) : Bool :=
  !(decide (a.x1 > b.x2) || decide (a.x2 < b.x1) || decide (a.y1 > b.y2) || decide (a.y2 < b.y1))
/-- `b_contains_a`. -/
def bca (a b : Box) : Bool :=
  decide (a.x1 ≥ b.x1) && decide (a.y1 ≥ b.y1) && decide (a.x2 ≤ b.x2) && decide (a.y2 ≤ b.y2)
/-- equal box coordinates. -/
def same (a b : Box) : Bool :=
  decide (a.x1 = b.x1) && decide (a.x2 = b.x2) && decide (a.y1 = b.y1) && decide (a.y2 = b.y2)
/-- shares at least one coordinate (used by coveredby / covers). -/
def share (a b : Box) : Bool :=
  decide (a.x1 = b.x1) || decide (a.x2 = b.x2) || decide (a.y1 = b.y1) || decide (a.y2 = b.y2)
/-- positive-area intersection. -/
def area (a b : Box) : Bool :=
  decide (max a.x1 b.x1 < min a.x2 b.x2) && decide (max a.y1 b.y1 < min a.y2 b.y2)
/-- 1-D intersection. -/
def has1d (a b : Box) : Bool :=
  (decide (max a.x1 b.x1 < min a.x2 b.x2) && decide (max a.y1 b.y1 = min a.y2 b.y2)) ||
  (decide (max a.x1 b.x1 = min a.x2 b.x2) && decide (max a.y1 b.y1 < min a.y2 b.y2))
/-- 0-D intersection. -/
def ptouch (a b : Box) : Bool :=
  decide (max a.x1 b.x1 = min a.x2 b.x2) && decide (max a.y1 b.y1 = min a.y2 b.y2)

/-! ### Per-box flags, exactly as accumulated in the Fortran loop (as `any` over B). -/

def insideAny (a : Box) (B : List Box) : Bool := B.any fun b => meets a b && bca a b
def containsAny (a : Box) (B : List Box) : Bool := B.any fun b => meets a b && bca b a
def equalAny (a : Box) (B : List Box) : Bool := B.any fun b => meets a b && same a b
def overlapAny (a : Box) (B : List Box) : Bool := B.any fun b => meets a b && area a b
def properAny (a : Box) (B : List Box) : Bool :=
  B.any fun b => meets a b && area a b && !bca a b && !bca b a
def touchAny (a : Box) (B : List Box) : Bool :=
  B.any fun b => meets a b && !area a b && has1d a b
def pointAny (a : Box) (B : List Box) : Bool :=
  B.any fun b => meets a b && !area a b && !has1d a b && ptouch a b
def outsideAll (a : Box) (B : List Box) : Bool :=
  !(B.any fun b => meets a b && (area a b || has1d a b || ptouch a b))
def coveredbyAny (a : Box) (B : List Box) : Bool :=
  B.any fun b => meets a b && ((!area a b && has1d a b && bca a b) || (bca a b && share a b))
def coversAny (a : Box) (B : List Box) : Bool :=
  B.any fun b => meets a b && ((!area a b && has1d a b && bca b a) || (bca b a && share a b))

inductive Base where
  | interact | inside | outside | touch | cut | disjoint | meet
  | equal | contains | covers | coveredby | overlaps
  deriving DecidableEq, Repr

/-- A CTR selector: a base relation, possibly negated (`NOT_INSIDE` etc.). -/
structure Op where
  base : Base
  neg  : Bool
  deriving DecidableEq, Repr

def engineBase (o : Base) (a : Box) (B : List Box) : Bool :=
  match o with
  | .interact  => overlapAny a B
  | .inside    => insideAny a B
  | .outside   => outsideAll a B
  | .touch     => touchAny a B && !overlapAny a B
  | .cut       => overlapAny a B && !insideAny a B
  | .disjoint  => outsideAll a B
  | .meet      => (touchAny a B || pointAny a B) && !overlapAny a B
  | .equal     => equalAny a B
  | .contains  => containsAny a B
  | .covers    => coversAny a B
  | .coveredby => coveredbyAny a B
  | .overlaps  => properAny a B

/-- The engine's decision for box `a` (the `select_it` value). -/
def engineSel (op : Op) (a : Box) (B : List Box) : Bool :=
  if op.neg then !engineBase op.base a B else engineBase op.base a B

/-- The engine's output layer: the selected boxes of A, unchanged. -/
def engineLayer (op : Op) (A B : List Box) : List Box := A.filter fun a => engineSel op a B

/-! ### Pairwise classes (Egenhofer's 8 for boxes, meet split into corner/edge) -/

inductive Cls where
  | disj | pt | et | po | eq | ins | inb | cons | conb
  deriving DecidableEq, Repr

def cls (a b : Box) : Cls :=
  if !meets a b then .disj
  else if same a b then .eq
  else if bca a b then (if share a b then .inb else .ins)
  else if bca b a then (if share a b then .conb else .cons)
  else if area a b then .po
  else if has1d a b then .et
  else .pt

def Cls.code : Cls → Nat
  | .disj => 0 | .pt => 1 | .et => 2 | .po => 3 | .eq => 4 | .ins => 5 | .inb => 6
  | .cons => 7 | .conb => 8

/-- Class test that `simp` can evaluate on constructors. -/
def Cls.is (c d : Cls) : Bool := c.code == d.code

/-- The type of a box against a layer: which classes occur. -/
structure Ty where
  pt : Bool
  et : Bool
  po : Bool
  eq : Bool
  ins : Bool
  inb : Bool
  cons : Bool
  conb : Bool
  deriving DecidableEq, Repr

def tyOf (a : Box) (B : List Box) : Ty :=
  ⟨B.any fun b => (cls a b).is .pt, B.any fun b => (cls a b).is .et,
   B.any fun b => (cls a b).is .po, B.any fun b => (cls a b).is .eq,
   B.any fun b => (cls a b).is .ins, B.any fun b => (cls a b).is .inb,
   B.any fun b => (cls a b).is .cons, B.any fun b => (cls a b).is .conb⟩

def Ty.ov (t : Ty) : Bool := t.po || t.eq || t.ins || t.inb || t.cons || t.conb
def Ty.inside (t : Ty) : Bool := t.eq || t.ins || t.inb

def basePred (o : Base) (t : Ty) : Bool :=
  match o with
  | .interact  => t.ov
  | .inside    => t.inside
  | .outside   => !(t.pt || t.et || t.ov)
  | .touch     => t.et && !t.ov
  | .cut       => t.ov && !t.inside
  | .disjoint  => !(t.pt || t.et || t.ov)
  | .meet      => (t.et || t.pt) && !t.ov
  | .equal     => t.eq
  | .contains  => t.eq || t.cons || t.conb
  | .covers    => t.eq || t.conb
  | .coveredby => t.eq || t.inb
  | .overlaps  => t.po

def selPred (op : Op) (t : Ty) : Bool := if op.neg then !basePred op.base t else basePred op.base t

/-! ### Pairwise lemmas: each engine flag is a set of classes (non-degenerate boxes). -/

section pairwise
variable {a b : Box}

/-- Coordinate meaning of each pairwise class (path conditions of `cls`). -/
def Spec (a b : Box) : Cls → Prop
  | .disj => b.x2 < a.x1 ∨ a.x2 < b.x1 ∨ b.y2 < a.y1 ∨ a.y2 < b.y1
  | .eq   => a.x1 = b.x1 ∧ a.x2 = b.x2 ∧ a.y1 = b.y1 ∧ a.y2 = b.y2
  | .inb  => b.x1 ≤ a.x1 ∧ b.y1 ≤ a.y1 ∧ a.x2 ≤ b.x2 ∧ a.y2 ≤ b.y2 ∧
             ¬(a.x1 = b.x1 ∧ a.x2 = b.x2 ∧ a.y1 = b.y1 ∧ a.y2 = b.y2) ∧
             (a.x1 = b.x1 ∨ a.x2 = b.x2 ∨ a.y1 = b.y1 ∨ a.y2 = b.y2)
  | .ins  => b.x1 < a.x1 ∧ b.y1 < a.y1 ∧ a.x2 < b.x2 ∧ a.y2 < b.y2
  | .conb => a.x1 ≤ b.x1 ∧ a.y1 ≤ b.y1 ∧ b.x2 ≤ a.x2 ∧ b.y2 ≤ a.y2 ∧
             ¬(a.x1 = b.x1 ∧ a.x2 = b.x2 ∧ a.y1 = b.y1 ∧ a.y2 = b.y2) ∧
             (a.x1 = b.x1 ∨ a.x2 = b.x2 ∨ a.y1 = b.y1 ∨ a.y2 = b.y2)
  | .cons => a.x1 < b.x1 ∧ a.y1 < b.y1 ∧ b.x2 < a.x2 ∧ b.y2 < a.y2
  | .po   => max a.x1 b.x1 < min a.x2 b.x2 ∧ max a.y1 b.y1 < min a.y2 b.y2 ∧
             ¬(b.x1 ≤ a.x1 ∧ b.y1 ≤ a.y1 ∧ a.x2 ≤ b.x2 ∧ a.y2 ≤ b.y2) ∧
             ¬(a.x1 ≤ b.x1 ∧ a.y1 ≤ b.y1 ∧ b.x2 ≤ a.x2 ∧ b.y2 ≤ a.y2)
  | .et   => ¬(b.x2 < a.x1 ∨ a.x2 < b.x1 ∨ b.y2 < a.y1 ∨ a.y2 < b.y1) ∧
             ¬(max a.x1 b.x1 < min a.x2 b.x2 ∧ max a.y1 b.y1 < min a.y2 b.y2) ∧
             ¬(b.x1 ≤ a.x1 ∧ b.y1 ≤ a.y1 ∧ a.x2 ≤ b.x2 ∧ a.y2 ≤ b.y2) ∧
             ¬(a.x1 ≤ b.x1 ∧ a.y1 ≤ b.y1 ∧ b.x2 ≤ a.x2 ∧ b.y2 ≤ a.y2) ∧
             ((max a.x1 b.x1 < min a.x2 b.x2 ∧ max a.y1 b.y1 = min a.y2 b.y2) ∨
              (max a.x1 b.x1 = min a.x2 b.x2 ∧ max a.y1 b.y1 < min a.y2 b.y2))
  | .pt   => ¬(b.x2 < a.x1 ∨ a.x2 < b.x1 ∨ b.y2 < a.y1 ∨ a.y2 < b.y1) ∧
             ¬(max a.x1 b.x1 < min a.x2 b.x2 ∧ max a.y1 b.y1 < min a.y2 b.y2) ∧
             ¬(b.x1 ≤ a.x1 ∧ b.y1 ≤ a.y1 ∧ a.x2 ≤ b.x2 ∧ a.y2 ≤ b.y2) ∧
             ¬(a.x1 ≤ b.x1 ∧ a.y1 ≤ b.y1 ∧ b.x2 ≤ a.x2 ∧ b.y2 ≤ a.y2) ∧
             ¬((max a.x1 b.x1 < min a.x2 b.x2 ∧ max a.y1 b.y1 = min a.y2 b.y2) ∨
               (max a.x1 b.x1 = min a.x2 b.x2 ∧ max a.y1 b.y1 < min a.y2 b.y2))

theorem cls_spec (a b : Box) : Spec a b (cls a b) := by
  unfold cls
  simp only [meets, same, bca, share, area, has1d, Bool.not_eq_true',
    Bool.and_eq_true, Bool.or_eq_true, decide_eq_true_eq]
  repeat' split
  all_goals simp only [Spec]
  all_goals simp_all
  all_goals omega

/-- Tactic: case on the class of (a,b), import its coordinate meaning, finish by omega. -/
macro "cls_cases" ha:ident hb:ident : tactic => `(tactic| (
  have hs := cls_spec a b
  simp only [Box.ok, Bool.and_eq_true, decide_eq_true_eq] at $ha:ident $hb:ident
  cases hc : cls a b <;> simp only [hc, Spec] at hs <;>
    simp only [meets, bca, same, share, area, has1d, ptouch, Bool.and_eq_true, Bool.or_eq_true,
      Bool.not_eq_true', Bool.or_eq_false_iff, Bool.and_eq_false_iff, decide_eq_true_eq,
      decide_eq_false_iff_not, reduceCtorEq, or_false,
      false_or, iff_false, iff_true, or_true, true_or] <;> omega))

/-- Engine pairwise sub-predicates (the per-pair contributions to each flag). -/
inductive PF where
  | inside | contains | equal | overlap | proper | touch | point | contact | coveredby | covers
  deriving DecidableEq, Repr

def pfEngine : PF → Box → Box → Bool
  | .inside,    a, b => meets a b && bca a b
  | .contains,  a, b => meets a b && bca b a
  | .equal,     a, b => meets a b && same a b
  | .overlap,   a, b => meets a b && area a b
  | .proper,    a, b => meets a b && area a b && !bca a b && !bca b a
  | .touch,     a, b => meets a b && !area a b && has1d a b
  | .point,     a, b => meets a b && !area a b && !has1d a b && ptouch a b
  | .contact,   a, b => meets a b && (area a b || has1d a b || ptouch a b)
  | .coveredby, a, b => meets a b && ((!area a b && has1d a b && bca a b) || (bca a b && share a b))
  | .covers,    a, b => meets a b && ((!area a b && has1d a b && bca b a) || (bca b a && share a b))

/-- Which classes make each engine sub-predicate true. -/
def pfCls : PF → Cls → Bool
  | .inside, .eq | .inside, .ins | .inside, .inb => true
  | .contains, .eq | .contains, .cons | .contains, .conb => true
  | .equal, .eq => true
  | .overlap, .po | .overlap, .eq | .overlap, .ins | .overlap, .inb
  | .overlap, .cons | .overlap, .conb => true
  | .proper, .po => true
  | .touch, .et => true
  | .point, .pt => true
  | .contact, .pt | .contact, .et | .contact, .po | .contact, .eq | .contact, .ins
  | .contact, .inb | .contact, .cons | .contact, .conb => true
  | .coveredby, .eq | .coveredby, .inb => true
  | .covers, .eq | .covers, .conb => true
  | _, _ => false

/-- Every engine sub-predicate is a function of the pairwise class. -/
theorem pf_eq (f : PF) (ha : a.ok) (hb : b.ok) : pfEngine f a b = pfCls f (cls a b) := by
  have hs := cls_spec a b
  simp only [Box.ok, Bool.and_eq_true, decide_eq_true_eq] at ha hb
  cases f <;> cases hc : cls a b <;> simp only [hc, Spec] at hs <;>
    simp only [pfEngine, pfCls, meets, bca, same, share, area, has1d, ptouch, Bool.and_eq_true,
      Bool.or_eq_true, Bool.not_eq_true', Bool.or_eq_false_iff, Bool.and_eq_false_iff,
      decide_eq_true_eq, decide_eq_false_iff_not, reduceCtorEq, or_false, false_or, iff_false,
      iff_true, or_true, true_or, beq_self_eq_true, Bool.or_true, Bool.true_or, Bool.not_true,
      Bool.not_false, Bool.or_false, Bool.false_or, reduceBEq, Bool.not_eq_eq_eq_not] <;> omega

end pairwise

/-! ### From pairs to a whole layer: flags are functions of the type -/

def Ty.empty : Ty := ⟨false, false, false, false, false, false, false, false⟩

def Ty.has (t : Ty) : Cls → Bool
  | .disj => false | .pt => t.pt | .et => t.et | .po => t.po | .eq => t.eq
  | .ins => t.ins | .inb => t.inb | .cons => t.cons | .conb => t.conb

def Ty.add (t : Ty) : Cls → Ty
  | .disj => t
  | .pt => { t with pt := true } | .et => { t with et := true } | .po => { t with po := true }
  | .eq => { t with eq := true } | .ins => { t with ins := true } | .inb => { t with inb := true }
  | .cons => { t with cons := true } | .conb => { t with conb := true }

/-- Lift a class predicate to a type: is some present class accepted? -/
def liftTy (g : Cls → Bool) (t : Ty) : Bool :=
  (g .pt && t.pt) || (g .et && t.et) || (g .po && t.po) || (g .eq && t.eq) ||
  (g .ins && t.ins) || (g .inb && t.inb) || (g .cons && t.cons) || (g .conb && t.conb)

def allBools : List Bool := [false, true]
def allTy : List Ty :=
  allBools.flatMap fun a => allBools.flatMap fun b => allBools.flatMap fun c =>
  allBools.flatMap fun d => allBools.flatMap fun e => allBools.flatMap fun f =>
  allBools.flatMap fun g => allBools.map fun h => (⟨a, b, c, d, e, f, g, h⟩ : Ty)
def allCls : List Cls := [.disj, .pt, .et, .po, .eq, .ins, .inb, .cons, .conb]

theorem mem_allTy (t : Ty) : t ∈ allTy := by
  obtain ⟨a, b, c, d, e, f, g, h⟩ := t
  cases a <;> cases b <;> cases c <;> cases d <;> cases e <;> cases f <;> cases g <;> cases h <;>
    decide

theorem mem_allCls (c : Cls) : c ∈ allCls := by cases c <;> simp [allCls]

/-- Finite quantification over types, discharged by `decide` on the enumeration. -/
theorem forall_ty {p : Ty → Bool} (h : allTy.all p = true) (t : Ty) : p t = true :=
  List.all_eq_true.mp h t (mem_allTy t)

theorem forall_ty_eq {f g : Ty → Bool} (h : allTy.all (fun t => f t == g t) = true) :
    ∀ t, f t = g t := fun t => by
  have := List.all_eq_true.mp h t (mem_allTy t); simpa using this

theorem forall_cls {p : Cls → Bool} (h : allCls.all p = true) (c : Cls) : p c = true :=
  List.all_eq_true.mp h c (mem_allCls c)

theorem tyOf_nil (a : Box) : tyOf a [] = Ty.empty := rfl

theorem tyOf_cons (a b : Box) (B : List Box) : tyOf a (b :: B) = (tyOf a B).add (cls a b) := by
  cases h : cls a b <;> simp [tyOf, Ty.add, List.any_cons, h, Cls.is, Cls.code]

theorem liftTy_add (g : Cls → Bool) (hg : g .disj = false) (t : Ty) (c : Cls) :
    liftTy g (t.add c) = (g c || liftTy g t) := by
  cases c <;> simp [liftTy, Ty.add, hg] <;> cases g .pt <;> cases g .et <;> cases g .po <;>
    cases g .eq <;> cases g .ins <;> cases g .inb <;> cases g .cons <;> cases g .conb <;>
    simp

theorem any_cls (g : Cls → Bool) (hg : g .disj = false) (a : Box) (B : List Box) :
    B.any (fun b => g (cls a b)) = liftTy g (tyOf a B) := by
  induction B with
  | nil => simp [tyOf_nil, liftTy, Ty.empty]
  | cons b B ih => rw [List.any_cons, ih, tyOf_cons, liftTy_add g hg]

theorem any_congr' {f g : Box → Bool} : ∀ {B : List Box}, (∀ b ∈ B, f b = g b) → B.any f = B.any g
  | [], _ => rfl
  | b :: B, h => by
      rw [List.any_cons, List.any_cons, h b (by simp), any_congr' (fun x hx => h x (by simp [hx]))]

/-- A layer-level engine flag equals the lifted class predicate on the type. -/
theorem flag_eq (f : PF) (a : Box) (B : List Box) (ha : a.ok) (hB : ∀ b ∈ B, b.ok = true) :
    B.any (pfEngine f a) = liftTy (pfCls f) (tyOf a B) := by
  rw [any_congr' (g := fun b => pfCls f (cls a b)) (fun b hb => pf_eq f ha (hB b hb))]
  exact any_cls _ (by cases f <;> rfl) a B

set_option maxRecDepth 20000 in
theorem engineSel_eq (op : Op) (a : Box) (B : List Box) (ha : a.ok) (hB : ∀ b ∈ B, b.ok = true) :
    engineSel op a B = selPred op (tyOf a B) := by
  have hi : insideAny a B = _ := flag_eq .inside a B ha hB
  have hc : containsAny a B = _ := flag_eq .contains a B ha hB
  have he : equalAny a B = _ := flag_eq .equal a B ha hB
  have ho : overlapAny a B = _ := flag_eq .overlap a B ha hB
  have hp : properAny a B = _ := flag_eq .proper a B ha hB
  have ht : touchAny a B = _ := flag_eq .touch a B ha hB
  have hq : pointAny a B = _ := flag_eq .point a B ha hB
  have hx : outsideAll a B = !_ := congrArg (!·) (flag_eq .contact a B ha hB)
  have hcb : coveredbyAny a B = _ := flag_eq .coveredby a B ha hB
  have hcv : coversAny a B = _ := flag_eq .covers a B ha hB
  obtain ⟨base, neg⟩ := op
  generalize tyOf a B = t at *
  cases base <;>
  simp only [engineSel, selPred, engineBase, basePred, hi, hc, he, ho, hp, ht, hq, hx, hcb, hcv] <;>
  clear hi hc he ho hp ht hq hx hcb hcv <;> cases neg <;> revert t <;>
  refine forall_ty_eq ?_ <;> decide +kernel

/-! ### Healed layers: realizable types -/

/-- Interior-disjoint (no positive-area overlap). -/
def idisj (b c : Box) : Bool := !area b c

def pairwiseB : List Box → Bool
  | [] => true
  | b :: B => B.all (idisj b) && pairwiseB B

/-- A healed layer: non-degenerate boxes with pairwise disjoint interiors. -/
def healedB (L : List Box) : Bool := L.all Box.ok && pairwiseB L

/-- Class pairs that cannot both occur against a healed layer. -/
def forbidden (c d : Cls) : Bool :=
  match c, d with
  | .eq, .po | .eq, .ins | .eq, .inb | .eq, .cons | .eq, .conb => true
  | .ins, .pt | .ins, .et | .ins, .po | .ins, .inb | .ins, .cons | .ins, .conb => true
  | .inb, .po | .inb, .cons | .inb, .conb => true
  | _, _ => false

def compat (c d : Cls) : Bool := !forbidden c d && !forbidden d c

def Ty.realizable (t : Ty) : Bool :=
  allCls.all fun c => allCls.all fun d => !(t.has c && t.has d && forbidden c d)

set_option maxHeartbeats 4000000 in
/-- Geometric core: two interior-disjoint boxes cannot realize a forbidden pair against `a`. -/
theorem pair_compat {a b b' : Box} (ha : a.ok) (hb : b.ok) (hb' : b'.ok) (hd : idisj b b' = true) :
    compat (cls a b) (cls a b') = true := by
  have h1 := cls_spec a b
  have h2 := cls_spec a b'
  simp only [Box.ok, Bool.and_eq_true, decide_eq_true_eq] at ha hb hb'
  simp only [idisj, area, Bool.not_eq_true', Bool.and_eq_false_iff, decide_eq_false_iff_not] at hd
  cases c1 : cls a b <;> cases c2 : cls a b' <;> simp only [c1, c2, Spec] at h1 h2 <;>
    first | rfl | (exfalso; omega)

def compatTy (c : Cls) (t : Ty) : Bool := allCls.all fun d => !t.has d || compat c d

theorem compatTy_of (a b : Box) (B : List Box) (h : ∀ b' ∈ B, compat (cls a b) (cls a b') = true) :
    compatTy (cls a b) (tyOf a B) = true := by
  induction B with
  | nil => rw [tyOf_nil]; exact forall_cls (p := fun c => compatTy c Ty.empty) (by decide +kernel) _
  | cons b' B ih =>
      rw [tyOf_cons]
      have h0 := h b' (by simp)
      have ih' := ih (fun x hx => h x (by simp [hx]))
      generalize cls a b = c at *
      generalize cls a b' = d at *
      generalize tyOf a B = t at *
      have key := forall_cls (p := fun c => allCls.all fun d => allTy.all fun t =>
        !(compat c d && compatTy c t) || compatTy c (t.add d)) (by decide +kernel) c
      have := List.all_eq_true.mp (List.all_eq_true.mp key d (mem_allCls d)) t (mem_allTy t)
      simp_all

theorem realizable_add (c : Cls) (t : Ty) (h1 : t.realizable = true) (h2 : compatTy c t = true) :
    (t.add c).realizable = true := by
  have key := forall_cls (p := fun c => allTy.all fun t =>
    !(t.realizable && compatTy c t) || (t.add c).realizable) (by decide +kernel) c
  have := forall_ty key t
  simp_all

theorem tyOf_realizable (a : Box) (B : List Box) (ha : a.ok) (hB : healedB B = true) :
    (tyOf a B).realizable = true := by
  induction B with
  | nil => rw [tyOf_nil]; decide
  | cons b B ih =>
      simp only [healedB, pairwiseB, List.all_cons, Bool.and_eq_true, List.all_eq_true] at hB
      obtain ⟨⟨hbok, hBok⟩, hbd, hBpw⟩ := hB
      rw [tyOf_cons]
      apply realizable_add _ _ (ih (by simp only [healedB, Bool.and_eq_true, List.all_eq_true]; exact ⟨hBok, hBpw⟩))
      exact compatTy_of a b B fun b' hb' => pair_compat ha hbok (hBok b' hb') (hbd b' hb')

/-! ### Cells and point types -/

abbrev Cell := Int × Int

def inCell (c : Cell) (b : Box) : Bool :=
  decide (b.x1 ≤ c.1) && decide (c.1 < b.x2) && decide (b.y1 ≤ c.2) && decide (c.2 < b.y2)

theorem area_of_cell {a b : Box} {c : Cell} (h1 : inCell c a = true) (h2 : inCell c b = true) :
    area a b = true := by
  simp only [inCell, area, Bool.and_eq_true, decide_eq_true_eq] at *
  omega

theorem meets_of_cell {a b : Box} {c : Cell} (h1 : inCell c a = true) (h2 : inCell c b = true) :
    meets a b = true := by
  simp only [inCell, meets, Bool.and_eq_true, Bool.not_eq_true', Bool.or_eq_false_iff,
    decide_eq_true_eq, decide_eq_false_iff_not] at *
  omega

/-- What a cell "sees": in A?, in B?, and the type of the (unique) A box containing it. -/
structure PT where
  mA : Bool
  mB : Bool
  t  : Ty
  deriving DecidableEq, Repr

/-- Constraints C0 (realizable), C1 (inside-type ⇒ in B), C2 (in B ⇒ overlap-type). -/
def PT.valid (p : PT) : Bool :=
  if p.mA then p.t.realizable && (!p.t.inside || p.mB) && (!p.mB || p.t.ov) else p.t == Ty.empty

def ptOf (A B : List Box) (c : Cell) : PT :=
  match A.find? (inCell c) with
  | some a => ⟨true, B.any (inCell c), tyOf a B⟩
  | none   => ⟨false, B.any (inCell c), Ty.empty⟩

theorem ptOf_valid (A B : List Box) (c : Cell) (hA : healedB A = true) (hB : healedB B = true) :
    (ptOf A B c).valid = true := by
  unfold ptOf
  split
  · rename_i a hfind
    have haA := List.mem_of_find?_eq_some hfind
    have hca := List.find?_some hfind
    have hAok : a.ok = true := by
      simp only [healedB, Bool.and_eq_true, List.all_eq_true] at hA; exact hA.1 a haA
    have hBok : ∀ b ∈ B, b.ok = true := by
      simp only [healedB, Bool.and_eq_true, List.all_eq_true] at hB; exact hB.1
    have hR := tyOf_realizable a B hAok hB
    have hI := flag_eq .inside a B hAok hBok
    have hO := flag_eq .overlap a B hAok hBok
    simp only [PT.valid, ite_true, hR, Bool.true_and, Bool.and_eq_true]
    constructor
    · -- C1: a lies inside some B box, so the cell is in B
      cases hin : (tyOf a B).inside
      · rfl
      · simp only [Bool.not_true, Bool.false_or]
        have h1 := forall_ty (p := fun t => !t.inside || liftTy (pfCls .inside) t)
          (by decide +kernel) (tyOf a B)
        have : liftTy (pfCls .inside) (tyOf a B) = true := by simpa [hin] using h1
        rw [← hI, List.any_eq_true] at this
        obtain ⟨b, hbB, hb⟩ := this
        simp only [pfEngine, bca, Bool.and_eq_true, decide_eq_true_eq] at hb
        rw [List.any_eq_true]
        refine ⟨b, hbB, ?_⟩
        simp only [inCell, Bool.and_eq_true, decide_eq_true_eq] at hca ⊢
        omega
    · -- C2: the cell is in some B box, which overlaps a with positive area
      cases hmb : B.any (inCell c)
      · rfl
      · simp only [Bool.not_true, Bool.false_or]
        obtain ⟨b, hbB, hb⟩ := List.any_eq_true.mp hmb
        have hov : B.any (pfEngine .overlap a) = true :=
          List.any_eq_true.mpr ⟨b, hbB, by
            simp [pfEngine, meets_of_cell hca hb, area_of_cell hca hb]⟩
        rw [hO] at hov
        have h2 := forall_ty (p := fun t => !liftTy (pfCls .overlap) t || t.ov)
          (by decide +kernel) (tyOf a B)
        simpa [hov] using h2
  · simp [PT.valid]

/-! ### Mixed expressions over A, B and selectors σ(A,B) -/

inductive MExpr where
  | la | lb | sel (op : Op) | zero
  | and (x y : MExpr) | or (x y : MExpr) | diff (x y : MExpr) | xor (x y : MExpr)
  deriving DecidableEq, Repr

/-- Engine semantics at a cell: layers and selector outputs as box sets, Booleans cellwise. -/
def cellEval (A B : List Box) (c : Cell) : MExpr → Bool
  | .la => A.any (inCell c)
  | .lb => B.any (inCell c)
  | .sel op => (engineLayer op A B).any (inCell c)
  | .zero => false
  | .and x y => cellEval A B c x && cellEval A B c y
  | .or x y => cellEval A B c x || cellEval A B c y
  | .diff x y => cellEval A B c x && !cellEval A B c y
  | .xor x y => Bool.xor (cellEval A B c x) (cellEval A B c y)

/-- Point-type semantics. -/
def ptEval (p : PT) : MExpr → Bool
  | .la => p.mA
  | .lb => p.mB
  | .sel op => p.mA && selPred op p.t
  | .zero => false
  | .and x y => ptEval p x && ptEval p y
  | .or x y => ptEval p x || ptEval p y
  | .diff x y => ptEval p x && !ptEval p y
  | .xor x y => Bool.xor (ptEval p x) (ptEval p y)

theorem any_filter (f g : Box → Bool) :
    ∀ L : List Box, (L.filter f).any g = L.any (fun a => f a && g a)
  | [] => rfl
  | x :: L => by
      by_cases hx : f x = true
      · simp [List.filter_cons, hx, any_filter f g L]
      · simp [List.filter_cons, hx, any_filter f g L]

/-- In a healed layer, at most one box contains a given cell. -/
theorem cell_unique {A : List Box} (hA : healedB A = true) {c : Cell} {a a' : Box}
    (h1 : a ∈ A) (h2 : a' ∈ A) (hc1 : inCell c a = true) (hc2 : inCell c a' = true) : a = a' := by
  induction A with
  | nil => simp at h1
  | cons x A ih =>
      simp only [healedB, pairwiseB, List.all_cons, Bool.and_eq_true, List.all_eq_true] at hA
      obtain ⟨⟨_, hAok⟩, hxd, hpw⟩ := hA
      have hA' : healedB A = true := by
        simp only [healedB, Bool.and_eq_true, List.all_eq_true]; exact ⟨hAok, hpw⟩
      have hx : ∀ y ∈ A, inCell c x = true → inCell c y = true → False := fun y hy hcx hcy => by
        have := hxd y hy
        simp [idisj, area_of_cell hcx hcy] at this
      simp only [List.mem_cons] at h1 h2
      rcases h1 with rfl | h1 <;> rcases h2 with rfl | h2
      · rfl
      · exact (hx a' h2 hc1 hc2).elim
      · exact (hx a h1 hc2 hc1).elim
      · exact ih hA' h1 h2

theorem any_cell_some {A : List Box} (hA : healedB A = true) {c : Cell} {a0 : Box}
    (h : A.find? (inCell c) = some a0) (p : Box → Bool) :
    A.any (fun a => inCell c a && p a) = p a0 := by
  have h0 := List.mem_of_find?_eq_some h
  have hc0 := List.find?_some h
  apply Bool.eq_iff_iff.mpr
  simp only [List.any_eq_true, Bool.and_eq_true]
  constructor
  · rintro ⟨a, ha, hca, hp⟩
    rwa [cell_unique hA ha h0 hca hc0] at hp
  · intro hp; exact ⟨a0, h0, hc0, hp⟩

theorem any_cell_none {A : List Box} {c : Cell} (h : A.find? (inCell c) = none) (p : Box → Bool) :
    A.any (fun a => inCell c a && p a) = false := by
  rw [List.find?_eq_none] at h
  apply Bool.eq_false_iff.mpr
  intro hc
  obtain ⟨a, ha, hp⟩ := List.any_eq_true.mp hc
  simp only [Bool.and_eq_true] at hp
  exact h a ha (by simp [hp.1])

/-- Evaluation at any cell of a healed configuration factors through its point type. -/
theorem cellEval_eq (A B : List Box) (c : Cell) (hA : healedB A = true)
    (hB : ∀ b ∈ B, b.ok = true) (e : MExpr) : cellEval A B c e = ptEval (ptOf A B c) e := by
  have hAok : ∀ a ∈ A, a.ok = true := by
    simp only [healedB, Bool.and_eq_true, List.all_eq_true] at hA; exact hA.1
  have hla : A.any (inCell c) = (ptOf A B c).mA := by
    have e1 : A.any (inCell c) = A.any (fun a => inCell c a && (fun _ => true) a) := by simp
    cases hf : A.find? (inCell c) with
    | none => rw [e1, any_cell_none hf]; simp [ptOf, hf]
    | some a0 => rw [e1, any_cell_some hA hf]; simp [ptOf, hf]
  have hlb : B.any (inCell c) = (ptOf A B c).mB := by
    unfold ptOf; split <;> rfl
  have hsel : ∀ op, (engineLayer op A B).any (inCell c) =
      ((ptOf A B c).mA && selPred op (ptOf A B c).t) := by
    intro op
    have e1 : (engineLayer op A B).any (inCell c) =
        A.any (fun a => inCell c a && engineSel op a B) := by
      rw [engineLayer, any_filter]; simp [Bool.and_comm]
    cases hf : A.find? (inCell c) with
    | none => rw [e1, any_cell_none hf]; simp [ptOf, hf]
    | some a0 =>
        rw [e1, any_cell_some hA hf]
        simp only [ptOf, hf, Bool.true_and]
        exact engineSel_eq op a0 B (hAok a0 (List.mem_of_find?_eq_some hf)) hB
  induction e with
  | la => exact hla
  | lb => exact hlb
  | sel op => exact hsel op
  | zero => rfl
  | and x y ihx ihy => simp [cellEval, ptEval, ihx, ihy]
  | or x y ihx ihy => simp [cellEval, ptEval, ihx, ihy]
  | diff x y ihx ihy => simp [cellEval, ptEval, ihx, ihy]
  | xor x y ihx ihy => simp [cellEval, ptEval, ihx, ihy]

/-- Nested selectors over the same B: the output keeps A's boxes, so σ₂(σ₁(A,B),B)
    selects exactly the boxes selected by both. -/
theorem nest_eq (op1 op2 : Op) (A B : List Box) :
    engineLayer op2 (engineLayer op1 A B) B = A.filter (fun a => engineSel op1 a B && engineSel op2 a B) := by
  simp [engineLayer, List.filter_filter, Bool.and_comm]

/-- Cellwise, a nested selector is the intersection of the two selectors (healed A). -/
theorem nest_cell (op1 op2 : Op) (A B : List Box) (c : Cell) (hA : healedB A = true) :
    (engineLayer op2 (engineLayer op1 A B) B).any (inCell c) =
      cellEval A B c (.and (.sel op1) (.sel op2)) := by
  rw [nest_eq]
  simp only [cellEval, engineLayer, any_filter]
  have e0 : A.any (fun a => (engineSel op1 a B && engineSel op2 a B) && inCell c a) =
      A.any (fun a => inCell c a && (engineSel op1 a B && engineSel op2 a B)) := by
    simp [Bool.and_comm, Bool.and_left_comm]
  have e1 : A.any (fun a => engineSel op1 a B && inCell c a) =
      A.any (fun a => inCell c a && engineSel op1 a B) := by simp [Bool.and_comm]
  have e2 : A.any (fun a => engineSel op2 a B && inCell c a) =
      A.any (fun a => inCell c a && engineSel op2 a B) := by simp [Bool.and_comm]
  rw [e0, e1, e2]
  cases hf : A.find? (inCell c) with
  | none => simp [any_cell_none hf]
  | some a0 => rw [any_cell_some hA hf, any_cell_some hA hf, any_cell_some hA hf]

end Trs.Box
