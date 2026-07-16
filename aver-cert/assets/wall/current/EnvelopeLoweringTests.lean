/-
Ported soundness witnesses for `EnvelopeLowering`, checked against the REAL
wall modules (`AcceptedArtifactCore`, `PlanLower`, `PlanBytes`, `WasmSlice`,
`IntDispatchSoundness`) and the REAL `aver compile --target wasm-gc --certify`
fixture bytes:
  intbox.av       : type Box = Wrapped(Int) | Empty ; unwrap  (int-dispatch)
  minimal_mkop.av : type Op = Add(Int)|Neg(Int)|Zero ; mkOp   (constructor)
plus the adversarial byte-patched intbox modules (dispatch immediate 1->7,
1->2). Honest packages pass; every forge dies; the envelope is FORCED — with
no byte -> structure decoder anywhere in the meaning path.
-/
import EnvelopeLowering
import IntboxBytes
import MkopBytes
import IntboxTag7Bytes
import IntboxTag2Bytes

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.EnvelopeLowering.Tests

open AverCert.Schema
open CertPrelude
open AverCert.EnvelopeLowering

/-! ## §1 Fixture: intbox (int-dispatch), REAL bytes — honest pass -/

def unwrapName : List Nat := [117, 110, 119, 114, 97, 112]  -- "unwrap"

def unwrapPlan : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .test 1 .proj (.default 0) }

def unwrapHostTable : List (HostRole × Nat) := [(.box, 6)]

/-- The DECLARED envelope for `type Box = Wrapped(Int) | Empty`:
    root 0, ctors [payload, no-payload]. Carrier index 5 is DERIVED. -/
def intboxEnv : AdtEnvelope := ⟨0, [true, false]⟩

theorem intbox_carrier_derived : carrierIdxE intboxEnv = 5 := rfl

def intboxGroupBytes : List Nat :=
  (lowerAdtRecGroup intboxEnv).getD []

theorem intbox_group_lowered :
    lowerAdtRecGroup intboxEnv = some intboxGroupBytes := rfl

def intboxGroupCursor : Nat × Nat :=
  (typeCursorAt IntboxBytes.modBytes IntboxBytes.modLen 0).getD (0, 0)

theorem intbox_group_cursor :
    typeCursorAt IntboxBytes.modBytes IntboxBytes.modLen 0
      = some intboxGroupCursor := rfl

theorem intbox_group_pin :
    bytesPinnedAt IntboxBytes.modBytes IntboxBytes.modLen 0 intboxGroupBytes :=
  ⟨intboxGroupCursor, rfl, by decide +kernel, rfl⟩

def unwrapBinding : AverCert.WasmSlice.FuncBinding :=
  (AverCert.WasmSlice.funcBindingForExport
    IntboxBytes.modBytes IntboxBytes.modLen unwrapName).getD ⟨0, 0, []⟩

theorem intbox_binding :
    AverCert.WasmSlice.funcBindingForExport
      IntboxBytes.modBytes IntboxBytes.modLen unwrapName
      = some unwrapBinding := rfl

theorem intbox_typeIdx : unwrapBinding.typeIdx = 7 := rfl

def intboxSigCursor : Nat × Nat :=
  (typeCursorAt IntboxBytes.modBytes IntboxBytes.modLen 7).getD (0, 0)

theorem intbox_sig_cursor :
    typeCursorAt IntboxBytes.modBytes IntboxBytes.modLen 7
      = some intboxSigCursor := rfl

theorem intbox_sig_pin :
    bytesPinnedAt IntboxBytes.modBytes IntboxBytes.modLen
      unwrapBinding.typeIdx ((lowerDispatchSig intboxEnv).getD []) :=
  ⟨intboxSigCursor, rfl, by decide +kernel, rfl⟩

def unwrapBody : List WInstr :=
  (AverCert.PlanLower.lowerIntDispatchBody unwrapHostTable unwrapPlan).getD []

def unwrapCe : List Nat :=
  (AverCert.PlanBytes.lowerIntDispatchCodeEntry 5 unwrapHostTable unwrapPlan).getD []

def oUnwrapHonest : Obligation where
  export_ := "unwrap"
  policy := .simulatesModel
  carrier := 5
  code := fun fn => if fn = 1 then some ⟨1, 3, unwrapBody⟩ else none
  host := AverCert.AcceptedArtifact.intDispatchCanonicalHost 5 unwrapHostTable
  self := 1
  Dom := AdtVal intboxEnv
  Cod := Int
  domRepr := envDomRepr intboxEnv
  codRepr := @AverCert.Schema.intRepr 5
  model := envStructModel intboxEnv unwrapPlan.body

/-- HONEST PASS (int-dispatch): the lower-pinned face closes on the real
    bytes, envelope pin and signature pin by `rfl` — exactly like a body pin. -/
theorem unwrap_honest_passes :
    AdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
      unwrapName "unwrap" 5 unwrapHostTable intboxEnv unwrapPlan oUnwrapHonest := by
  refine ⟨⟨rfl, rfl, rfl, rfl, rfl, unwrapBody, unwrapCe, unwrapBinding,
      rfl, rfl, rfl, rfl, rfl, rfl⟩,
    rfl, rfl, rfl,
    ⟨intboxGroupBytes, intbox_group_lowered, intbox_group_pin⟩,
    ⟨unwrapBinding, intbox_binding, (lowerDispatchSig intboxEnv).getD [],
      rfl, intbox_sig_pin⟩,
    rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

/-- Source-compatibility: the plan-computed model AGREES with the source
    semantics of `unwrap`. -/
inductive Box | wrapped (n : Int) | empty

def unwrapSrc : Box → Int
  | .wrapped n => n
  | .empty => 0

def encBox : Box → AdtVal intboxEnv
  | .wrapped n => ⟨(1, some n), Or.inl ⟨rfl, n, rfl⟩⟩
  | .empty => ⟨(2, none), Or.inr ⟨rfl, rfl⟩⟩

theorem envStructModel_agrees_with_unwrap :
    ∀ b, envStructModel intboxEnv unwrapPlan.body (encBox b) = unwrapSrc b := by
  intro b; cases b <;> rfl

/-! ## §2 The envelope is FORCED (lower is injective-on-meaning here) -/

theorem group_forces_intbox (env : AdtEnvelope)
    (hroot : env.root = 0) (hlen : env.ctors.length = 2)
    (gb : List Nat) (hlow : lowerAdtRecGroup env = some gb)
    (cur1 : Nat)
    (htake : CertDecode.takeBytes gb.length cur1 = gb)
    (hbytes : CertDecode.takeBytes 37 cur1 =
      intboxGroupBytes ++ [0x60, 0x00, 0x00]) :
    env = intboxEnv := by
  obtain ⟨root, ctors⟩ := env
  have hroot' : root = 0 := hroot
  have hlen' : ctors.length = 2 := hlen
  subst hroot'
  obtain ⟨b1, b2, rfl⟩ : ∃ b1 b2, ctors = [b1, b2] := by
    rcases ctors with _ | ⟨b1, _ | ⟨b2, _ | ⟨b3, t⟩⟩⟩
    all_goals first
      | exact ⟨b1, b2, rfl⟩
      | (exfalso
         simp only [List.length_cons, List.length_nil] at hlen'
         omega)
  cases b1 <;> cases b2
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [false, false]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 37 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [false, true]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 37 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)
  · rfl
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [true, true]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 37 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)

/-- ENVELOPE UNIQUENESS on the real intbox bytes: ANY accepted claim has the
    honest envelope. The type structure is not an input; it is a THEOREM of
    the bytes plus the lowering. -/
theorem intbox_env_forced
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : AdtEnvelope) (plan : IntDispatchRawPlan) (o : Obligation)
    (hface : AdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
      unwrapName "unwrap" carrier hostTable env plan o) :
    env = intboxEnv := by
  obtain ⟨-, -, -, -,
    ⟨gb, hglow, gcur, hgcur, -, hgtake⟩,
    ⟨binding, hbind, sig, hslow, scur, hscur, -, hstake⟩, -⟩ := hface
  have hbeq : unwrapBinding = binding :=
    Option.some.inj (intbox_binding.symm.trans hbind)
  subst hbeq
  rw [intbox_typeIdx] at hscur
  have hscur' : intboxSigCursor = scur :=
    Option.some.inj (intbox_sig_cursor.symm.trans hscur)
  subst hscur'
  have hsiglen : sig.length = 7 := by
    change lowerDispatchSig env = some sig at hslow
    unfold lowerDispatchSig at hslow
    by_cases hc : checkAdtEnvelope env = true
    · rw [if_pos hc] at hslow; rw [← Option.some.inj hslow]; rfl
    · rw [if_neg hc] at hslow; exact absurd hslow (by simp)
  rw [hsiglen] at hstake
  have hsig : sig = [0x60, 0x01, 0x63, 0x00, 0x01, 0x63, 0x05] :=
    hstake.symm.trans (by exact rfl)
  obtain ⟨hroot, hlen⟩ := sig_forces_dispatch env sig hslow hsig
  rw [hroot] at hgcur
  have hgcur' : intboxGroupCursor = gcur :=
    Option.some.inj (intbox_group_cursor.symm.trans hgcur)
  subst hgcur'
  exact group_forces_intbox env hroot hlen gb hglow intboxGroupCursor.1 hgtake
    (by exact rfl)

/-! ## §3 The forges die — with NO decoder anywhere -/

def smallSpec (C : Nat) : CarrierSpec C where
  Repr := fun n v => v = carrierSmall C n
  car := fun n v h => Or.inl ⟨n, 0, h⟩
  smallIntro := fun _ => rfl
  smallElim := fun n s sg h => by
    simp [carrierSmall] at h
    exact h.1
  bigElim := fun n s lty les sg h => by
    simp [carrierSmall] at h

/-- FORGE `model := 17`: DIES for EVERY envelope. -/
def oForgeModel17 : Obligation :=
  { oUnwrapHonest with model := fun _ => (17 : Int) }

theorem forge_model_17_dies
    (hostTable : List (HostRole × Nat)) (env : AdtEnvelope) :
    ¬ AdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
        unwrapName "unwrap" 5 hostTable env unwrapPlan oForgeModel17 := by
  intro hface
  have henv := intbox_env_forced 5 hostTable env unwrapPlan oForgeModel17 hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, -, -, -, hmodel⟩ := hface
  have heq : (fun _ => (17 : Int)) = envStructModel intboxEnv unwrapPlan.body :=
    eq_of_heq hmodel
  have h17 := congrFun heq ⟨(1, some 5), Or.inl ⟨rfl, 5, rfl⟩⟩
  exact absurd h17 (by decide)

/-- FORGE `domRepr := False` (the vacuity smuggle): DIES. -/
def oForgeDomFalse : Obligation :=
  { oUnwrapHonest with domRepr := fun _ _ _ => False }

theorem forge_domRepr_false_dies
    (hostTable : List (HostRole × Nat)) (env : AdtEnvelope) :
    ¬ AdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
        unwrapName "unwrap" 5 hostTable env unwrapPlan oForgeDomFalse := by
  intro hface
  have henv := intbox_env_forced 5 hostTable env unwrapPlan oForgeDomFalse hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, -, hdomR, -, -⟩ := hface
  have heq : (fun _ _ _ => False) = envDomRepr intboxEnv := eq_of_heq hdomR
  have h := congrFun (congrFun (congrFun heq (smallSpec 5))
    ⟨(2, none), Or.inr ⟨rfl, rfl⟩⟩) [.structv 2 []]
  exact Eq.mp h.symm (by exact rfl)

/-- FORGED ENVELOPE (declare `Wrapped` payloadless to shift the model): DIES
    at the lower-pin. -/
theorem forge_flipped_envelope_dies
    (hostTable : List (HostRole × Nat)) (plan : IntDispatchRawPlan)
    (o : Obligation) :
    ¬ AdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
        unwrapName "unwrap" 5 hostTable ⟨0, [false, false]⟩ plan o := by
  intro hface
  have henv := intbox_env_forced 5 hostTable ⟨0, [false, false]⟩ plan o hface
  exact absurd henv (by decide)

/-! ### The realized adversarial witnesses, now against the lower face -/

def plan7 : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .test 7 .proj (.default 0) }

def plan2 : IntDispatchRawPlan :=
  { profile := "int-dispatch-v1", body := .test 2 .proj (.default 0) }

theorem tag7_code_gate_passes :
    AverCert.PlanBytes.lowerIntDispatchCodeEntry 5 unwrapHostTable plan7
      = some IntboxTag7Bytes.codeEntry := rfl

theorem tag2_code_gate_passes :
    AverCert.PlanBytes.lowerIntDispatchCodeEntry 5 unwrapHostTable plan2
      = some IntboxTag2Bytes.codeEntry := rfl

def tag7Binding : AverCert.WasmSlice.FuncBinding :=
  (AverCert.WasmSlice.funcBindingForExport
    IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen unwrapName).getD ⟨0, 0, []⟩

theorem tag7_binding :
    AverCert.WasmSlice.funcBindingForExport
      IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen unwrapName
      = some tag7Binding := rfl

def tag7GroupCursor : Nat × Nat :=
  (typeCursorAt IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen 0).getD (0, 0)

theorem tag7_group_cursor :
    typeCursorAt IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen 0
      = some tag7GroupCursor := rfl

def tag7SigCursor : Nat × Nat :=
  (typeCursorAt IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen 7).getD (0, 0)

theorem tag7_sig_cursor :
    typeCursorAt IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen 7
      = some tag7SigCursor := rfl

theorem tag7_env_forced
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : AdtEnvelope) (plan : IntDispatchRawPlan) (o : Obligation)
    (hface : AdtIntFaceLower IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen
      unwrapName "unwrap" carrier hostTable env plan o) :
    env = intboxEnv := by
  obtain ⟨-, -, -, -,
    ⟨gb, hglow, gcur, hgcur, -, hgtake⟩,
    ⟨binding, hbind, sig, hslow, scur, hscur, -, hstake⟩, -⟩ := hface
  have hbeq : tag7Binding = binding :=
    Option.some.inj (tag7_binding.symm.trans hbind)
  subst hbeq
  rw [show tag7Binding.typeIdx = 7 from rfl] at hscur
  have hscur' : tag7SigCursor = scur :=
    Option.some.inj (tag7_sig_cursor.symm.trans hscur)
  subst hscur'
  have hsiglen : sig.length = 7 := by
    change lowerDispatchSig env = some sig at hslow
    unfold lowerDispatchSig at hslow
    by_cases hc : checkAdtEnvelope env = true
    · rw [if_pos hc] at hslow; rw [← Option.some.inj hslow]; rfl
    · rw [if_neg hc] at hslow; exact absurd hslow (by simp)
  rw [hsiglen] at hstake
  have hsig : sig = [0x60, 0x01, 0x63, 0x00, 0x01, 0x63, 0x05] :=
    hstake.symm.trans (by exact rfl)
  obtain ⟨hroot, hlen⟩ := sig_forces_dispatch env sig hslow hsig
  rw [hroot] at hgcur
  have hgcur' : tag7GroupCursor = gcur :=
    Option.some.inj (tag7_group_cursor.symm.trans hgcur)
  subst hgcur'
  exact group_forces_intbox env hroot hlen gb hglow tag7GroupCursor.1 hgtake
    (by exact rfl)

/-- THE decoder-free KILL: on the byte-patched tag-7 module — where the exact
    CODE gate passes — no obligation and no envelope certifies the tag-7
    cascade: the forced envelope declares no constructor at tag 7. -/
theorem tag7_witness_dies
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : AdtEnvelope) (o : Obligation) :
    ¬ AdtIntFaceLower IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen
        unwrapName "unwrap" carrier hostTable env plan7 o := by
  intro hface
  have henv := tag7_env_forced carrier hostTable env plan7 o hface
  subst henv
  obtain ⟨-, -, hcasc, -⟩ := hface
  exact absurd hcasc (by decide)

def tag2Binding : AverCert.WasmSlice.FuncBinding :=
  (AverCert.WasmSlice.funcBindingForExport
    IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen unwrapName).getD ⟨0, 0, []⟩

theorem tag2_binding :
    AverCert.WasmSlice.funcBindingForExport
      IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen unwrapName
      = some tag2Binding := rfl

def tag2GroupCursor : Nat × Nat :=
  (typeCursorAt IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen 0).getD (0, 0)

theorem tag2_group_cursor :
    typeCursorAt IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen 0
      = some tag2GroupCursor := rfl

def tag2SigCursor : Nat × Nat :=
  (typeCursorAt IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen 7).getD (0, 0)

theorem tag2_sig_cursor :
    typeCursorAt IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen 7
      = some tag2SigCursor := rfl

theorem tag2_env_forced
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : AdtEnvelope) (plan : IntDispatchRawPlan) (o : Obligation)
    (hface : AdtIntFaceLower IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen
      unwrapName "unwrap" carrier hostTable env plan o) :
    env = intboxEnv := by
  obtain ⟨-, -, -, -,
    ⟨gb, hglow, gcur, hgcur, -, hgtake⟩,
    ⟨binding, hbind, sig, hslow, scur, hscur, -, hstake⟩, -⟩ := hface
  have hbeq : tag2Binding = binding :=
    Option.some.inj (tag2_binding.symm.trans hbind)
  subst hbeq
  rw [show tag2Binding.typeIdx = 7 from rfl] at hscur
  have hscur' : tag2SigCursor = scur :=
    Option.some.inj (tag2_sig_cursor.symm.trans hscur)
  subst hscur'
  have hsiglen : sig.length = 7 := by
    change lowerDispatchSig env = some sig at hslow
    unfold lowerDispatchSig at hslow
    by_cases hc : checkAdtEnvelope env = true
    · rw [if_pos hc] at hslow; rw [← Option.some.inj hslow]; rfl
    · rw [if_neg hc] at hslow; exact absurd hslow (by simp)
  rw [hsiglen] at hstake
  have hsig : sig = [0x60, 0x01, 0x63, 0x00, 0x01, 0x63, 0x05] :=
    hstake.symm.trans (by exact rfl)
  obtain ⟨hroot, hlen⟩ := sig_forces_dispatch env sig hslow hsig
  rw [hroot] at hgcur
  have hgcur' : tag2GroupCursor = gcur :=
    Option.some.inj (tag2_group_cursor.symm.trans hgcur)
  subst hgcur'
  exact group_forces_intbox env hroot hlen gb hglow tag2GroupCursor.1 hgtake
    (by exact rfl)

/-- Payloadless-projection witness (patched 1->2): `cascadeInEnv` rejects
    projecting a DECLARED payloadless constructor against the forced envelope. -/
theorem tag2_witness_dies
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : AdtEnvelope) (o : Obligation) :
    ¬ AdtIntFaceLower IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen
        unwrapName "unwrap" carrier hostTable env plan2 o := by
  intro hface
  have henv := tag2_env_forced carrier hostTable env plan2 o hface
  subst henv
  obtain ⟨-, -, hcasc, -⟩ := hface
  exact absurd hcasc (by decide)

/-! ## §4 Fixture: mkOp (named ADT constructor), REAL bytes -/

def mkOpName : List Nat := [109, 107, 79, 112]  -- "mkOp"

def mkOpSymPlan : SymRawPlan := { profile := "sym-fragment-v1", params := [.int], result := (.named "Op"), body := ({ nodes := [{ id := 0, ty := .int, kind := .param 0 }, { id := 1, ty := (.named "Op"), kind := .construct "Op" "add" [0] }], result := 1 } : SymBlock) }

def mkOpPlan : ConstructRawPlan :=
  { profile := "construct-v1", arity := 1, fields := [.local 0] }

/-- DECLARED envelope for `type Op = Add(Int) | Neg(Int) | Zero`. -/
def mkopEnv : AdtEnvelope := ⟨0, [true, true, false]⟩

theorem mkop_carrier_derived : carrierIdxE mkopEnv = 6 := rfl

def mkopGroupBytes : List Nat :=
  (lowerAdtRecGroup mkopEnv).getD []

theorem mkop_group_lowered :
    lowerAdtRecGroup mkopEnv = some mkopGroupBytes := rfl

def mkopGroupCursor : Nat × Nat :=
  (typeCursorAt MkopBytes.modBytes MkopBytes.modLen 0).getD (0, 0)

theorem mkop_group_cursor :
    typeCursorAt MkopBytes.modBytes MkopBytes.modLen 0
      = some mkopGroupCursor := rfl

theorem mkop_group_pin :
    bytesPinnedAt MkopBytes.modBytes MkopBytes.modLen 0 mkopGroupBytes :=
  ⟨mkopGroupCursor, rfl, by decide +kernel, rfl⟩

def mkOpBinding : AverCert.WasmSlice.FuncBinding :=
  (AverCert.WasmSlice.funcBindingForExport
    MkopBytes.modBytes MkopBytes.modLen mkOpName).getD ⟨0, 0, []⟩

theorem mkop_binding :
    AverCert.WasmSlice.funcBindingForExport
      MkopBytes.modBytes MkopBytes.modLen mkOpName = some mkOpBinding := rfl

theorem mkop_typeIdx : mkOpBinding.typeIdx = 8 := rfl

def mkopSigCursor : Nat × Nat :=
  (typeCursorAt MkopBytes.modBytes MkopBytes.modLen 8).getD (0, 0)

theorem mkop_sig_cursor :
    typeCursorAt MkopBytes.modBytes MkopBytes.modLen 8
      = some mkopSigCursor := rfl

theorem mkop_sig_pin :
    bytesPinnedAt MkopBytes.modBytes MkopBytes.modLen
      mkOpBinding.typeIdx ((lowerCtorSig mkopEnv).getD []) :=
  ⟨mkopSigCursor, rfl, by decide +kernel, rfl⟩

theorem mkop_add_payload : ctorPayload? mkopEnv 1 = some true := rfl

def mkOpBody : List WInstr :=
  (AverCert.PlanLower.lowerConstructBody 1 mkOpPlan).getD []

def mkOpCe : List Nat :=
  (AverCert.PlanBytes.lowerConstructCodeEntry 6 1 mkOpPlan).getD []

def oMkOpHonest : Obligation where
  export_ := "mkOp"
  policy := .simulatesModel
  carrier := 6
  code := fun fn => if fn = 1 then some ⟨1, 1, mkOpBody⟩ else none
  host := fun _ _ _ _ _ _ => none
  self := 1
  Dom := Int
  Cod := AdtVal mkopEnv
  domRepr := intArgDomRepr 6
  codRepr := envCodRepr mkopEnv
  model := envCtorModel mkopEnv 1 mkop_add_payload

/-- HONEST PASS (constructor): the lower-pinned constructor face closes on the
    real bytes by `rfl`. -/
theorem mkop_honest_passes :
    AdtCtorFaceLower MkopBytes.modBytes MkopBytes.modLen
      mkOpName "mkOp" 6 1 1 .i64 mkOpSymPlan mkopEnv mkOpPlan oMkOpHonest := by
  refine ⟨⟨rfl, rfl, rfl, rfl, rfl, rfl, mkOpBody, mkOpCe, mkOpBinding,
      rfl, rfl, rfl, rfl, Or.inl rfl, Or.inl rfl, rfl⟩,
    rfl, rfl, rfl, rfl,
    ⟨mkopGroupBytes, mkop_group_lowered, mkop_group_pin⟩,
    ⟨mkOpBinding, mkop_binding, (lowerCtorSig mkopEnv).getD [],
      rfl, mkop_sig_pin⟩,
    rfl, mkop_add_payload,
    HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩

/-- Source-compatibility for the constructor: the plan-computed model IS
    `Op.Add`. -/
inductive Op | add (n : Int) | neg (n : Int) | zero

def encOp : Op → AdtVal mkopEnv
  | .add n => ⟨(1, some n), Or.inl ⟨rfl, n, rfl⟩⟩
  | .neg n => ⟨(2, some n), Or.inl ⟨rfl, n, rfl⟩⟩
  | .zero => ⟨(3, none), Or.inr ⟨rfl, rfl⟩⟩

theorem envCtorModel_agrees_with_mkOp :
    ∀ n, envCtorModel mkopEnv 1 mkop_add_payload n = encOp (.add n) :=
  fun _ => rfl

/-! ### mkOp envelope uniqueness + the constructor forges -/

theorem group_forces_mkop (env : AdtEnvelope)
    (hroot : env.root = 0) (hlen : env.ctors.length = 3)
    (gb : List Nat) (hlow : lowerAdtRecGroup env = some gb)
    (cur1 : Nat)
    (htake : CertDecode.takeBytes gb.length cur1 = gb)
    (hbytes : CertDecode.takeBytes 45 cur1 =
      mkopGroupBytes ++ [0x60, 0x00, 0x00]) :
    env = mkopEnv := by
  obtain ⟨root, ctors⟩ := env
  have hroot' : root = 0 := hroot
  have hlen' : ctors.length = 3 := hlen
  subst hroot'
  obtain ⟨b1, b2, b3, rfl⟩ : ∃ b1 b2 b3, ctors = [b1, b2, b3] := by
    rcases ctors with _ | ⟨b1, _ | ⟨b2, _ | ⟨b3, _ | ⟨b4, t⟩⟩⟩⟩
    all_goals first
      | exact ⟨b1, b2, b3, rfl⟩
      | (exfalso
         simp only [List.length_cons, List.length_nil] at hlen'
         omega)
  cases b1 <;> cases b2 <;> cases b3
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [false, false, false]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 45 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [false, false, true]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 45 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [false, true, false]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 45 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [false, true, true]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 45 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [true, false, false]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 45 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [true, false, true]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 45 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)
  · rfl
  · have hgb : gb = (lowerAdtRecGroup ⟨0, [true, true, true]⟩).getD [] := by
      rw [hlow]; rfl
    subst hgb
    rw [takeBytes_take _ 45 cur1 (by decide), hbytes] at htake
    exact absurd htake (by decide)

theorem mkop_env_forced
    (carrier structIdx fieldCount : Nat) (elemTy : ConstructValType)
    (symPlan : SymRawPlan) (env : AdtEnvelope) (plan : ConstructRawPlan)
    (o : Obligation)
    (hface : AdtCtorFaceLower MkopBytes.modBytes MkopBytes.modLen
      mkOpName "mkOp" carrier structIdx fieldCount elemTy symPlan env plan o) :
    env = mkopEnv := by
  obtain ⟨-, -, -, -, -,
    ⟨gb, hglow, gcur, hgcur, -, hgtake⟩,
    ⟨binding, hbind, sig, hslow, scur, hscur, -, hstake⟩, -⟩ := hface
  have hbeq : mkOpBinding = binding :=
    Option.some.inj (mkop_binding.symm.trans hbind)
  subst hbeq
  rw [mkop_typeIdx] at hscur
  have hscur' : mkopSigCursor = scur :=
    Option.some.inj (mkop_sig_cursor.symm.trans hscur)
  subst hscur'
  have hsiglen : sig.length = 7 := by
    change lowerCtorSig env = some sig at hslow
    unfold lowerCtorSig at hslow
    by_cases hc : checkAdtEnvelope env = true
    · rw [if_pos hc] at hslow; rw [← Option.some.inj hslow]; rfl
    · rw [if_neg hc] at hslow; exact absurd hslow (by simp)
  rw [hsiglen] at hstake
  have hsig : sig = [0x60, 0x01, 0x63, 0x06, 0x01, 0x63, 0x00] :=
    hstake.symm.trans (by exact rfl)
  obtain ⟨hroot, hlen⟩ := sig_forces_ctor env sig hslow hsig
  rw [hroot] at hgcur
  have hgcur' : mkopGroupCursor = gcur :=
    Option.some.inj (mkop_group_cursor.symm.trans hgcur)
  subst hgcur'
  exact group_forces_mkop env hroot hlen gb hglow mkopGroupCursor.1 hgtake
    (by exact rfl)

/-- CONSTRUCTOR FORGE `model := Op.zero`: DIES. -/
def oMkOpForgeZero : Obligation :=
  { oMkOpHonest with model := fun _ => encOp .zero }

theorem mkop_forge_zero_dies (env : AdtEnvelope) :
    ¬ AdtCtorFaceLower MkopBytes.modBytes MkopBytes.modLen
        mkOpName "mkOp" 6 1 1 .i64 mkOpSymPlan env mkOpPlan oMkOpForgeZero := by
  intro hface
  have henv := mkop_env_forced 6 1 1 .i64 mkOpSymPlan env mkOpPlan
    oMkOpForgeZero hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, hpay, -, -, -, -, hmodel⟩ := hface
  have heq : (fun _ => encOp .zero) = envCtorModel mkopEnv 1 hpay :=
    eq_of_heq hmodel
  have hv := congrArg Subtype.val (congrFun heq 2)
  exact absurd (show ((3 : Nat), (none : Option Int)) = ((1 : Nat), some (2 : Int))
    from hv) (by decide)

/-- CONSTRUCTOR FORGE at the payloadless tag (`structIdx := 3`):
    UNCONSTRUCTIBLE. -/
theorem mkop_payloadless_structIdx_dies (env : AdtEnvelope) (o : Obligation) :
    ¬ AdtCtorFaceLower MkopBytes.modBytes MkopBytes.modLen
        mkOpName "mkOp" 6 3 1 .i64 mkOpSymPlan env mkOpPlan o := by
  intro hface
  have henv := mkop_env_forced 6 3 1 .i64 mkOpSymPlan env mkOpPlan o hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, hpay, -⟩ := hface
  exact absurd hpay (by decide)

/-- CONSTRUCTOR FORGE at a fabricated tag (`structIdx := 8`): same death. -/
theorem mkop_fabricated_structIdx_dies (env : AdtEnvelope) (o : Obligation) :
    ¬ AdtCtorFaceLower MkopBytes.modBytes MkopBytes.modLen
        mkOpName "mkOp" 6 8 1 .i64 mkOpSymPlan env mkOpPlan o := by
  intro hface
  have henv := mkop_env_forced 6 8 1 .i64 mkOpSymPlan env mkOpPlan o hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, hpay, -⟩ := hface
  exact absurd hpay (by decide)

/-- Effect-absence, instantiated on the honest mkOp package: its accepted body
    has no call — proved from the PLAN, no scan of `MkopBytes`. -/
theorem mkop_body_no_call : ∀ i ∈ mkOpBody, ∀ k, i ≠ .call k :=
  lowerConstructBody_no_call 1 mkOpPlan mkOpBody rfl

/-! ## §5 Axiom scrub (kernel-checked witness inventory) -/

#print axioms unwrap_honest_passes
#print axioms mkop_honest_passes
#print axioms intbox_env_forced
#print axioms mkop_env_forced
#print axioms forge_model_17_dies
#print axioms forge_domRepr_false_dies
#print axioms forge_flipped_envelope_dies
#print axioms tag7_code_gate_passes
#print axioms tag2_code_gate_passes
#print axioms tag7_witness_dies
#print axioms tag2_witness_dies
#print axioms mkop_forge_zero_dies
#print axioms mkop_payloadless_structIdx_dies
#print axioms mkop_fabricated_structIdx_dies
#print axioms envStructModel_agrees_with_unwrap
#print axioms envCtorModel_agrees_with_mkOp
#print axioms mkop_body_no_call

end AverCert.EnvelopeLowering.Tests
