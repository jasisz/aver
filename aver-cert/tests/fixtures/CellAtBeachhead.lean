import SchemaCore
import StandardFace

set_option linter.unusedSimpArgs false
set_option linter.unusedVariables false
set_option maxRecDepth 100000
set_option maxHeartbeats 4000000

open AverCert
open AverCert.Schema
open CertPrelude

/-!
# Array-read beachhead — fused `Option.withDefault(Vector.get(vec, idx), d)`

Target witness: `cellAt(grid: Vector<Int>, idx: Int) -> Int =
Option.withDefault(Vector.get(grid, idx), 0)` (`examples/games/life.av`).

The template, face, representation relation, and the generic
template-implies-model theorem now live in the certificate wall
(`PlanLower.vectorGetOrDefaultTemplate`,
`StandardFace.vectorGetOrDefault_simulates_model`); this fixture REUSES them —
it never re-states the instruction list. What remains here are the source-law
corollaries for `cellAt` (in-bounds read and out-of-bounds default) and the
executable anti-vacuity guards over a concrete instantiation.

The `__aint_to_index` contract (the sixth host-contract slot, from
`builtins/bignum.rs` + `wat/to_index.wat`): for a represented integer `n`,
the helper returns `n` when `0 <= n < 2^31` and the out-of-bounds sentinel
`-1` otherwise. It is stated RELATIONALLY over the representation (`toIndexW`
in the obligation denotation), never as a concrete function.

SOUNDNESS-CRITICAL BOUND: `StandardFace.vecDomRepr` requires
`elems.length < 2^31` INSIDE the relation; see the wall comment.
-/

namespace CellAtDev

open AverCert.StandardFace
open AverCert.PlanLower

/-- Code table: the wall template is the single self function (index 1),
    with the canonical single carrier scratch local. -/
def cellAtCode (toIndexIdx boxIdx arrTy : Nat) (d : Int) : CodeTbl :=
  fun fn =>
    if fn = 1 then
      some { arity := 2, nlocals := 1,
             body := vectorGetOrDefaultTemplate toIndexIdx boxIdx arrTy d }
    else none

/-- LAW 1 (`cellAt` in-bounds read): on `0 <= i < len v` the fused template
    returns the represented `v[i]`. -/
theorem cellAt_in_bounds_read
    (carrier toIndexIdx boxIdx arrTy : Nat) (d : Int)
    (hIdx : toIndexIdx ≠ boxIdx)
    (S : CarrierSpec carrier)
    (toIndex : List WVal → Option WVal)
    (hToIndex : ∀ n w r, intRepr S n w → toIndex [w] = some r →
      r = .i32v (toIndexW n))
    (fuel : Nat) (v : List Int) (i : Int) (vs : List WVal) (w : WVal)
    (hDom : vecDomRepr carrier arrTy S (v, i) vs)
    (hin : 0 ≤ i ∧ i < (v.length : Int))
    (hRun : wFuncN (cellAtCode toIndexIdx boxIdx arrTy d)
      (vectorGetOrDefaultHostSlots carrier toIndexIdx boxIdx toIndex)
      fuel 1 vs = some w) :
    intRepr S (v[i.toNat]!) w := by
  have h := vectorGetOrDefault_simulates_model carrier toIndexIdx boxIdx arrTy d
    hIdx S toIndex hToIndex (cellAtCode toIndexIdx boxIdx arrTy d) 1
    (by simp [cellAtCode]) fuel v i vs w hDom hRun
  simpa [vecModel, hin] using h

/-- LAW 2 (`cellAt` out-of-bounds default): outside `[0, len v)` the fused
    template returns the represented literal default. -/
theorem cellAt_out_of_bounds_default
    (carrier toIndexIdx boxIdx arrTy : Nat) (d : Int)
    (hIdx : toIndexIdx ≠ boxIdx)
    (S : CarrierSpec carrier)
    (toIndex : List WVal → Option WVal)
    (hToIndex : ∀ n w r, intRepr S n w → toIndex [w] = some r →
      r = .i32v (toIndexW n))
    (fuel : Nat) (v : List Int) (i : Int) (vs : List WVal) (w : WVal)
    (hDom : vecDomRepr carrier arrTy S (v, i) vs)
    (hout : ¬(0 ≤ i ∧ i < (v.length : Int)))
    (hRun : wFuncN (cellAtCode toIndexIdx boxIdx arrTy d)
      (vectorGetOrDefaultHostSlots carrier toIndexIdx boxIdx toIndex)
      fuel 1 vs = some w) :
    intRepr S d w := by
  have h := vectorGetOrDefault_simulates_model carrier toIndexIdx boxIdx arrTy d
    hIdx S toIndex hToIndex (cellAtCode toIndexIdx boxIdx arrTy d) 1
    (by simp [cellAtCode]) fuel v i vs w hDom hRun
  simpa [vecModel, hout] using h

/-! ### Anti-vacuity guards

The theorems above are partial-correctness statements, vacuous if the
template always trapped. These executable runs pin concrete instantiations
of every hole (carrier 3, `to_index` 7, box 6, array type 4, default 0) on
the executable `__aint_to_index` reference face (`CertPrelude.toIndexRef`)
and check — decoded to `Int` through `carrierToInt` and evaluated by
`native_decide`, the established `CertPreludeSanity` guard idiom, OUTSIDE the
proof budget — that the template actually produces the modelled results:
in-bounds read, negative index, past-end index, `>= 2^31` index, and a big
(limb-carrying) index. -/

def demoVec : WVal :=
  .arr 4 [carrierSmall 3 10, carrierSmall 3 20, carrierSmall 3 30]

def demoRun (idx : Int) : Option WVal :=
  wFuncN (cellAtCode 7 6 4 0) (vectorGetOrDefaultHostSlots 3 7 6 toIndexRef) 1 1
    [demoVec, carrierSmall 3 idx]

example : (demoRun 1).bind carrierToInt = some 20 := by native_decide
example : (demoRun (-1)).bind carrierToInt = some 0 := by native_decide
example : (demoRun 3).bind carrierToInt = some 0 := by native_decide
example : (demoRun 2147483648).bind carrierToInt = some 0 := by native_decide
example :
    (wFuncN (cellAtCode 7 6 4 0) (vectorGetOrDefaultHostSlots 3 7 6 toIndexRef) 1 1
      [demoVec, .structv 3 [.i64v 0, .arr 5 [.i64v 1], .i32v 0]]).bind
      carrierToInt = some 0 := by native_decide

#print axioms AverCert.StandardFace.vectorGetOrDefault_simulates_model
#print axioms cellAt_in_bounds_read
#print axioms cellAt_out_of_bounds_default

end CellAtDev
