-- Manual companion proof for `verify safeSum law commutative` in
-- examples/modules/natural_app.av.
--
-- `aver proof --backend lean` exports the law as a `sorry` placeholder
-- because the generator's auto-proof path (simp + omega) doesn't close
-- a wrapper-equality goal like `Except.ok x = Except.ok y`. This file
-- discharges the universal claim by hand. Drop it next to the
-- generated `NaturalApp.lean` (or splice the body into the corresponding
-- `theorem safeSum_law_commutative` statement) to turn the sorry into a
-- real proof.
--
-- Build:
--   aver proof examples/modules/natural_app.av --module-root examples \
--     --backend lean --output out --name AverApp
--   # then splice / patch this proof into out/NaturalApp.lean
--   cd out && lake build

import AverCommon
import Modules.Natural.Natural

open Modules.Natural.Natural

-- safeSum is the consumer-side function from natural_app.av; its
-- signature has to match the generated one byte-for-byte for the
-- proof to discharge the sorry.
def safeSum (a : Int) (b : Int) : Except String Int :=
  match Modules.Natural.Natural.fromInt a with
  | .error msg => Except.error msg
  | .ok na => match Modules.Natural.Natural.fromInt b with
      | .error msg => Except.error msg
      | .ok nb => match Modules.Natural.Natural.add na nb with
          | .error msg => Except.error msg
          | .ok sum => Except.ok (Modules.Natural.Natural.toInt sum)

/-- safeSum is commutative for all Int (unbounded in the proof model).
    Three observations make this tractable:
      1. fromInt only branches on `n ≥ 0`, so the result depends solely
         on the sign — same predicate on both sides.
      2. add(Nat(a), Nat(b)) just feeds (a + b) back through fromInt,
         and Int.add_comm closes the arithmetic obligation on the
         only path that reaches `Except.ok`.
      3. Every error path produces the same constant message, so the
         four "at least one negative" arms are syntactically equal. -/
theorem safeSum_commutative : ∀ (a b : Int), safeSum a b = safeSum b a := by
  intro a b
  unfold safeSum
  unfold Modules.Natural.Natural.fromInt
  unfold Modules.Natural.Natural.add
  unfold Modules.Natural.Natural.toInt
  by_cases ha : a ≥ 0
  · by_cases hb : b ≥ 0
    · simp [ha, hb, Int.add_comm]
    · simp [ha, hb]
  · by_cases hb : b ≥ 0
    · simp [ha, hb]
    · simp [ha, hb]
