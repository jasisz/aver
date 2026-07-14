import CertPrelude
import SchemaCore
import PlanCheck
import PlanLower

set_option maxRecDepth 100000

namespace FieldProjectionSoundness
open CertPrelude AverCert.Schema

/-- Read field `fieldIdx` from a represented pair. -/
def pairProjection (fieldIdx : Nat) (a b : WVal) : WVal :=
  match fieldIdx with
  | 0 => a
  | _ => b

/-- Generic field-projection certificate shape.

Given accepted projection-plan metadata and the audited lowering, the emitted
unary function on a represented two-element struct reads field `fieldIdx` of
that struct.
-/
theorem generic_field_projection_certified
    (structIdx : Nat)
    (plan : FieldProjectionRawPlan)
    (code : CodeTbl) (host : HostTbl)
    (self : Nat)
    (hcheck : AverCert.PlanCheck.checkFieldProjectionRawPlan 2 plan = true)
    (instrs : List WInstr)
    (hlow : AverCert.PlanLower.lowerFieldProjectionBody structIdx 2 plan = some instrs)
    (hself : code self = some { arity := 1, nlocals := 3, body := instrs }) :
    ∀ (a b : WVal),
      wFuncN code host 1 self [.structv structIdx [a, b]] =
      some (pairProjection plan.fieldIdx a b) := by
  intro a b
  cases plan with
  | mk profile fIdx =>
      have hcheck' : profile = "field-projection-v1" ∧ 2 = 2 ∧ fIdx < 2 := by
        simpa [AverCert.PlanCheck.checkFieldProjectionRawPlan] using hcheck
      have hlt : fIdx < 2 := hcheck'.2.2
      have hinstrs :
          [.localGet 0, .localSet 2, .localGet 2, .refCast structIdx,
            .structGet structIdx fIdx, .localSet 1, .localGet 1] = instrs := by
        rw [AverCert.PlanLower.lowerFieldProjectionBody, hcheck] at hlow
        simpa using hlow
      subst hinstrs
      cases fIdx with
      | zero =>
          simp [pairProjection, wFuncN, hself, initLocals, wRunF]
      | succ k =>
          cases k with
          | zero =>
              simp [pairProjection, wFuncN, hself, initLocals, wRunF]
          | succ k =>
              have hbad : ¬ Nat.succ (Nat.succ k) < 2 := by
                exact Nat.not_lt_of_ge (Nat.succ_le_succ (Nat.succ_le_succ (Nat.zero_le k)))
              exact False.elim (hbad (by simpa using hlt))


end FieldProjectionSoundness
