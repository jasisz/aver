import CertPrelude
import SchemaCore
import PlanCheck
import PlanLower

set_option maxRecDepth 100000

namespace V3FieldProj
open CertPrelude AverCert.Schema

/-- Read field `fieldIdx` from a represented pair. -/
def pairProjection (fieldIdx : Nat) (a b : WVal) : WVal :=
  match fieldIdx with
  | 0 => a
  | _ => b

/-- Decidable admission check mirroring Rust's bare projection gate.
    `fieldCount` is the byte-derived struct field arity (for tuples this is 2).
    The profile guard protects the constructor shape.
 -/
def checkProjection (fieldCount : Nat) (plan : FieldProjectionRawPlan) : Bool :=
  AverCert.PlanCheck.checkFieldProjectionRawPlan fieldCount plan

/-- Value-shape invariant, extended with `structv`. -/
def ValOK {C : Nat} (_S : ReprSpec C) : FragTy → WVal → Prop
  | .ref, _ => True
  | .adtRef, w => ∃ t fs, w = .structv t fs
  | .i64, _ => False
  | .intCarrier, _ => False
  | .rawI32, _ => False
  | .boolI32, _ => False
  | .f64, _ => False

/-- Stack invariant shape kept from the template (value list, per-symbol-slot). -/
def StackOK {C : Nat} (S : ReprSpec C) (tys : List FragTy) (env : List WVal) :
    List Nat → List WVal → Prop
  | [], [] => True
  | id :: ids, w :: ws =>
      (∃ ty, env[id]? = some w ∧ tys[id]? = some ty ∧ ValOK S ty w) ∧
        StackOK S tys env ids ws
  | _, _ => False

/-- Parameters are directly copied into `locals` (no lifting or boxing in this
    family), so this is the same shape as the template. -/
def ParamsOK {C : Nat} (_S : ReprSpec C) (params : List WVal)
    (locals : List WVal) : Prop :=
  ∀ (i : Nat) (m : WVal), params[i]? = some m → ∃ v, locals[i]? = some v ∧ m = v

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

/-- Real tuple-projection artifacts (byte-bound by `tupleproj.avm` in the prompt flow). -/
def pairFstFieldProjectionPlan : FieldProjectionRawPlan :=
  ({ profile := "field-projection-v1", fieldIdx := 0 } : FieldProjectionRawPlan)

def pairSndFieldProjectionPlan : FieldProjectionRawPlan :=
  ({ profile := "field-projection-v1", fieldIdx := 1 } : FieldProjectionRawPlan)

def pairFstCode : CodeTbl :=
  fun fn =>
    if fn = 1 then
      some {
        arity := 1,
        nlocals := 3,
        body := [.localGet 0, .localSet 2, .localGet 2, .refCast 3,
          .structGet 3 0, .localSet 1, .localGet 1]
      }
    else none

def pairSndCode : CodeTbl :=
  fun fn =>
    if fn = 2 then
      some {
        arity := 1,
        nlocals := 3,
        body := [.localGet 0, .localSet 2, .localGet 2, .refCast 3,
          .structGet 3 1, .localSet 1, .localGet 1]
      }
    else none

/-- Subsumption at export `pairFst`: exact target shape of the generated
    `Certificate.lean` theorem. -/
theorem pairFst_from_generic (host : HostTbl) :
    ∀ (a b : WVal), wFuncN pairFstCode host 1 1 [.structv 3 [a, b]] = some a := by
  intro a b
  have hcheck :
      AverCert.PlanCheck.checkFieldProjectionRawPlan 2 ({ profile := "field-projection-v1", fieldIdx := 0 } : FieldProjectionRawPlan) = true := by
    simp [AverCert.PlanCheck.checkFieldProjectionRawPlan]
  have hlow : AverCert.PlanLower.lowerFieldProjectionBody 3 2 ({ profile := "field-projection-v1", fieldIdx := 0 } : FieldProjectionRawPlan) =
      some [.localGet 0, .localSet 2, .localGet 2, .refCast 3,
        .structGet 3 0, .localSet 1, .localGet 1] := by
    simpa [AverCert.PlanLower.lowerFieldProjectionBody, hcheck] using (rfl : (AverCert.PlanLower.lowerFieldProjectionBody 3 2 ({ profile := "field-projection-v1", fieldIdx := 0 } : FieldProjectionRawPlan)) = AverCert.PlanLower.lowerFieldProjectionBody 3 2 ({ profile := "field-projection-v1", fieldIdx := 0 } : FieldProjectionRawPlan))
  have hself : pairFstCode 1 = some
      { arity := 1, nlocals := 3,
        body := [.localGet 0, .localSet 2, .localGet 2, .refCast 3,
          .structGet 3 0, .localSet 1, .localGet 1] } := by
    simp [pairFstCode]
  have hcall :=
    generic_field_projection_certified 3 ({ profile := "field-projection-v1", fieldIdx := 0 } : FieldProjectionRawPlan)
      pairFstCode host 1 hcheck _ hlow hself a b
  simpa [pairProjection] using hcall

/-- Subsumption at export `pairSnd`: exact target shape of the generated
    `Certificate.lean` theorem. -/
theorem pairSnd_from_generic (host : HostTbl) :
    ∀ (a b : WVal), wFuncN pairSndCode host 1 2 [.structv 3 [a, b]] = some b := by
  intro a b
  have hcheck :
      AverCert.PlanCheck.checkFieldProjectionRawPlan 2 ({ profile := "field-projection-v1", fieldIdx := 1 } : FieldProjectionRawPlan) = true := by
    simp [AverCert.PlanCheck.checkFieldProjectionRawPlan]
  have hlow : AverCert.PlanLower.lowerFieldProjectionBody 3 2 ({ profile := "field-projection-v1", fieldIdx := 1 } : FieldProjectionRawPlan) =
      some [.localGet 0, .localSet 2, .localGet 2, .refCast 3,
        .structGet 3 1, .localSet 1, .localGet 1] := by
    simpa [AverCert.PlanLower.lowerFieldProjectionBody, hcheck] using (rfl : (AverCert.PlanLower.lowerFieldProjectionBody 3 2 ({ profile := "field-projection-v1", fieldIdx := 1 } : FieldProjectionRawPlan)) = AverCert.PlanLower.lowerFieldProjectionBody 3 2 ({ profile := "field-projection-v1", fieldIdx := 1 } : FieldProjectionRawPlan))
  have hself : pairSndCode 2 = some
      { arity := 1, nlocals := 3,
        body := [.localGet 0, .localSet 2, .localGet 2, .refCast 3,
          .structGet 3 1, .localSet 1, .localGet 1] } := by
    simp [pairSndCode]
  have hcall :=
    generic_field_projection_certified 3 ({ profile := "field-projection-v1", fieldIdx := 1 } : FieldProjectionRawPlan)
      pairSndCode host 2 hcheck _ hlow hself a b
  simpa [pairProjection] using hcall

/-- Guard-rail 1: out-of-bounds field index is rejected by the projection check. -/
example :
    AverCert.PlanCheck.checkFieldProjectionRawPlan 2
      ({ profile := "field-projection-v1", fieldIdx := 2 } : FieldProjectionRawPlan) = false := by
  simp [AverCert.PlanCheck.checkFieldProjectionRawPlan]

/-- Guard-rail 2: mutating the pinned struct index changes the canonical lower
    body, so the equality that the generic theorem requires does not hold. -/
example :
    ¬ (AverCert.PlanLower.lowerFieldProjectionBody 3 2 pairFstFieldProjectionPlan =
      some [.localGet 0, .localSet 2, .localGet 2, .refCast 4,
        .structGet 4 0, .localSet 1, .localGet 1]) := by
  simp [pairFstFieldProjectionPlan, AverCert.PlanLower.lowerFieldProjectionBody]

#print axioms generic_field_projection_certified
#print axioms pairFst_from_generic
#print axioms pairSnd_from_generic

end V3FieldProj
