/- RelationsDE9IM.lean: all boundary-sensitive 9E-DIM/DE-9IM zero identities. -/
namespace VLSI.RelationsDE9IM
inductive P3 where | I | B | E deriving DecidableEq, Repr
open P3
abbrev S := Bool
def sand (a b : S) : S := a && b
def sor  (a b : S) : S := a || b
def snot (a b : S) : S := a && !b
def sxor (a b : S) : S := xor a b
-- selectors take region/interior/boundary/exterior masks as bools; in pointwise model we pass precomputed bools
-- For generated theorems, we use Bool vars per layer component.
