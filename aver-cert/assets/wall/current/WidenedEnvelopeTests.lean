/-
GOAL 4 (critical soundness gate) + GOAL 3 + GOAL 5 for the WIDENED envelope,
checked against the REAL wall modules and the REAL fixture bytes reused from
`EnvelopeLoweringTests` (`IntboxBytes`, `IntboxTag7Bytes`, `IntboxTag2Bytes`,
`MkopBytes`).

The widened dispatch signature lowers byte-for-byte like the narrow one, and the
widened rec group of the honest fixture envelope is byte-identical to the narrow
one, so the fixture cursors/bindings carry over verbatim. The forcing here shows
that under the WIDENED profile the honest envelope is STILL uniquely forced, and
every forge STILL dies — the widening to non-hit payload shapes does not let any
forger survive.
-/
import WidenedEnvelope
import EnvelopeLoweringTests

set_option maxRecDepth 1000000
set_option maxHeartbeats 4000000

namespace AverCert.WidenedEnvelope.Tests

open AverCert.Schema
open CertPrelude
open AverCert.WidenedEnvelope
open AverCert.EnvelopeLowering
open AverCert.EnvelopeLowering.Tests

/-! ## §1 The widened honest envelopes (byte-identical rec groups) -/

/-- Widened `type Box = Wrapped(Int) | Empty`: hit + unit. -/
def wIntboxEnv : WAdtEnvelope := ⟨0, [.hit, .unit]⟩

theorem wIntbox_carrier : wCarrierIdx wIntboxEnv = 5 := rfl

/-- The widened rec group is byte-identical to the narrow one: the pin against
    the real `IntboxBytes` holds by `rfl`, exactly as narrow. -/
theorem wIntbox_group_is_narrow :
    wLowerAdtRecGroup wIntboxEnv = lowerAdtRecGroup intboxEnv := rfl

theorem wIntbox_sig_is_narrow :
    wLowerDispatchSig wIntboxEnv = lowerDispatchSig intboxEnv := rfl

/-! ## §2 Signature forcing (root + constructor count), widened -/

theorem wSig_forces_dispatch (env : WAdtEnvelope) (sig : List Nat)
    (hlow : wLowerDispatchSig env = some sig)
    (heq : sig = [0x60, 0x01, 0x63, 0x00, 0x01, 0x63, 0x05]) :
    env.root = 0 ∧ env.ctors.length = 2 := by
  unfold wLowerDispatchSig at hlow
  by_cases hc : checkWidenedEnvelope env = true
  · rw [if_pos hc] at hlow
    have h := (Option.some.inj hlow).trans heq
    have hroot : env.root = 0 := congrArg (fun l => l.getD 3 0) h
    have hcar : wCarrierIdx env = 5 := congrArg (fun l => l.getD 6 0) h
    refine ⟨hroot, ?_⟩
    unfold wCarrierIdx at hcar
    omega
  · rw [if_neg hc] at hlow; exact absurd hlow (by simp)

theorem wSig_forces_ctor (env : WAdtEnvelope) (sig : List Nat)
    (hlow : wLowerCtorSig env = some sig)
    (heq : sig = [0x60, 0x01, 0x63, 0x06, 0x01, 0x63, 0x00]) :
    env.root = 0 ∧ env.ctors.length = 3 := by
  unfold wLowerCtorSig at hlow
  by_cases hc : checkWidenedEnvelope env = true
  · rw [if_pos hc] at hlow
    have h := (Option.some.inj hlow).trans heq
    have hcar : wCarrierIdx env = 6 := congrArg (fun l => l.getD 3 0) h
    have hroot : env.root = 0 := congrArg (fun l => l.getD 6 0) h
    refine ⟨hroot, ?_⟩
    unfold wCarrierIdx at hcar
    omega
  · rw [if_neg hc] at hlow; exact absurd hlow (by simp)

/-! ## §3 Rec-group forcing over the finite shape vocabulary

The signature pins `root` and the constructor COUNT; the rec-group pin then
decides every constructor shape by a finite case split over `WCtor`. Because
every shape has a KNOWN byte length, each non-honest combination is refuted by a
closed `decide` on the pinned bytes — no unbounded payload, no substring gap. -/

theorem wGroup_forces_intbox (env : WAdtEnvelope)
    (hroot : env.root = 0) (hlen : env.ctors.length = 2)
    (gb : List Nat) (hlow : wLowerAdtRecGroup env = some gb)
    (cur1 : Nat)
    (htake : CertDecode.takeBytes gb.length cur1 = gb)
    (hbytes : CertDecode.takeBytes 37 cur1 =
      intboxGroupBytes ++ [0x60, 0x00, 0x00]) :
    env = wIntboxEnv := by
  obtain ⟨root, ctors⟩ := env
  have hroot' : root = 0 := hroot
  subst hroot'
  obtain ⟨c0, c1, rfl⟩ : ∃ c0 c1, ctors = [c0, c1] := by
    rcases ctors with _ | ⟨c0, _ | ⟨c1, _ | ⟨c2, t⟩⟩⟩
    all_goals first
      | exact ⟨c0, c1, rfl⟩
      | (exfalso; simp only [List.length_cons, List.length_nil] at hlen; omega)
  cases c0 <;> cases c1 <;>
    first
    | rfl
    | (have hg : gb = _ := (Option.some.inj hlow).symm
       subst hg
       rw [takeBytes_take _ 37 cur1 (by decide), hbytes] at htake
       exact absurd htake (by decide))

/-! ## §4 GOAL 4 — the widened Int-read face: envelope forced, forges die -/

/-- ENVELOPE UNIQUENESS on the real intbox bytes under the WIDENED profile: any
    accepted widened claim has the honest widened envelope. -/
theorem wIntbox_env_forced
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : WAdtEnvelope) (plan : IntDispatchRawPlan) (o : Obligation)
    (hface : WAdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
      unwrapName "unwrap" carrier hostTable env plan o) :
    env = wIntboxEnv := by
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
    change wLowerDispatchSig env = some sig at hslow
    unfold wLowerDispatchSig at hslow
    by_cases hc : checkWidenedEnvelope env = true
    · rw [if_pos hc] at hslow; rw [← Option.some.inj hslow]; rfl
    · rw [if_neg hc] at hslow; exact absurd hslow (by simp)
  rw [hsiglen] at hstake
  have hsig : sig = [0x60, 0x01, 0x63, 0x00, 0x01, 0x63, 0x05] :=
    hstake.symm.trans (by exact rfl)
  obtain ⟨hroot, hlen⟩ := wSig_forces_dispatch env sig hslow hsig
  rw [hroot] at hgcur
  have hgcur' : intboxGroupCursor = gcur :=
    Option.some.inj (intbox_group_cursor.symm.trans hgcur)
  subst hgcur'
  exact wGroup_forces_intbox env hroot hlen gb hglow intboxGroupCursor.1 hgtake
    (by exact rfl)

/-- The widened honest Int-read obligation for intbox. -/
def oWUnwrapHonest : Obligation where
  export_ := "unwrap"
  policy := .simulatesModel
  carrier := 5
  code := fun fn => if fn = 1 then some ⟨1, 3, unwrapBody⟩ else none
  host := AverCert.AcceptedArtifact.intDispatchCanonicalHost 5 unwrapHostTable
  self := 1
  Dom := WAdtVal wIntboxEnv
  Cod := Int
  domRepr := wEnvDomRepr wIntboxEnv
  codRepr := @AverCert.Schema.intRepr 5
  model := wEnvStructModel wIntboxEnv unwrapPlan.body

/-- HONEST PASS (widened Int-read): the full widened face closes on the REAL
    `IntboxBytes`, envelope pin and signature pin byte-identical to narrow. The
    widened face is non-vacuous end-to-end, not just at the rec-group column. -/
theorem wUnwrap_honest_passes :
    WAdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
      unwrapName "unwrap" 5 unwrapHostTable wIntboxEnv unwrapPlan oWUnwrapHonest := by
  refine ⟨⟨rfl, rfl, rfl, rfl, rfl, unwrapBody, unwrapCe, unwrapBinding,
      rfl, rfl, rfl, rfl, rfl, rfl⟩,
    rfl, rfl, rfl,
    ⟨intboxGroupBytes, ?_, intbox_group_pin⟩,
    ⟨unwrapBinding, intbox_binding, (lowerDispatchSig intboxEnv).getD [],
      ?_, intbox_sig_pin⟩,
    rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl, HEq.rfl⟩
  · exact intbox_group_lowered
  · rfl

/-- FORGE `model := 17`: DIES for EVERY widened envelope. -/
def oWForgeModel17 : Obligation :=
  { oWUnwrapHonest with model := fun _ => (17 : Int) }

theorem wForge_model_17_dies
    (hostTable : List (HostRole × Nat)) (env : WAdtEnvelope) :
    ¬ WAdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
        unwrapName "unwrap" 5 hostTable env unwrapPlan oWForgeModel17 := by
  intro hface
  have henv := wIntbox_env_forced 5 hostTable env unwrapPlan oWForgeModel17 hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, -, -, -, hmodel⟩ := hface
  have heq : (fun _ => (17 : Int)) = wEnvStructModel wIntboxEnv unwrapPlan.body :=
    eq_of_heq hmodel
  have h17 := congrFun heq ⟨(1, .int 5), Or.inl ⟨rfl, 5, rfl⟩⟩
  exact absurd h17 (by decide)

/-- FORGE `domRepr := False` (the vacuity smuggle): DIES. -/
def oWForgeDomFalse : Obligation :=
  { oWUnwrapHonest with domRepr := fun _ _ _ => False }

theorem wForge_domRepr_false_dies
    (hostTable : List (HostRole × Nat)) (env : WAdtEnvelope) :
    ¬ WAdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
        unwrapName "unwrap" 5 hostTable env unwrapPlan oWForgeDomFalse := by
  intro hface
  have henv := wIntbox_env_forced 5 hostTable env unwrapPlan oWForgeDomFalse hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, -, hdomR, -, -⟩ := hface
  have heq : (fun _ _ _ => False) = wEnvDomRepr wIntboxEnv := eq_of_heq hdomR
  have h := congrFun (congrFun (congrFun heq (smallSpec 5))
    ⟨(1, .int 0), Or.inl ⟨rfl, 0, rfl⟩⟩) [.structv 1 [carrierSmall 5 0]]
  exact Eq.mp h.symm ⟨carrierSmall 5 0, rfl, rfl⟩

/-- FORGED ENVELOPE (declare `Wrapped` a non-hit shape to shift the model):
    DIES at the lower-pin. -/
theorem wForge_flipped_envelope_dies
    (hostTable : List (HostRole × Nat)) (plan : IntDispatchRawPlan)
    (o : Obligation) :
    ¬ WAdtIntFaceLower IntboxBytes.modBytes IntboxBytes.modLen
        unwrapName "unwrap" 5 hostTable ⟨0, [.unit, .unit]⟩ plan o := by
  intro hface
  have henv := wIntbox_env_forced 5 hostTable ⟨0, [.unit, .unit]⟩ plan o hface
  exact absurd henv (by decide)

/-! ### The adversarial byte-patched witnesses, against the widened face -/

theorem wTag7_env_forced
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : WAdtEnvelope) (plan : IntDispatchRawPlan) (o : Obligation)
    (hface : WAdtIntFaceLower IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen
      unwrapName "unwrap" carrier hostTable env plan o) :
    env = wIntboxEnv := by
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
    change wLowerDispatchSig env = some sig at hslow
    unfold wLowerDispatchSig at hslow
    by_cases hc : checkWidenedEnvelope env = true
    · rw [if_pos hc] at hslow; rw [← Option.some.inj hslow]; rfl
    · rw [if_neg hc] at hslow; exact absurd hslow (by simp)
  rw [hsiglen] at hstake
  have hsig : sig = [0x60, 0x01, 0x63, 0x00, 0x01, 0x63, 0x05] :=
    hstake.symm.trans (by exact rfl)
  obtain ⟨hroot, hlen⟩ := wSig_forces_dispatch env sig hslow hsig
  rw [hroot] at hgcur
  have hgcur' : tag7GroupCursor = gcur :=
    Option.some.inj (tag7_group_cursor.symm.trans hgcur)
  subst hgcur'
  exact wGroup_forces_intbox env hroot hlen gb hglow tag7GroupCursor.1 hgtake
    (by exact rfl)

/-- The tag-7 forge dies under the widened profile: the forced widened envelope
    declares no `hit` constructor at tag 7, so `wCascadeInEnv` rejects. -/
theorem wTag7_witness_dies
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : WAdtEnvelope) (o : Obligation) :
    ¬ WAdtIntFaceLower IntboxTag7Bytes.modBytes IntboxTag7Bytes.modLen
        unwrapName "unwrap" carrier hostTable env plan7 o := by
  intro hface
  have henv := wTag7_env_forced carrier hostTable env plan7 o hface
  subst henv
  obtain ⟨-, -, hcasc, -⟩ := hface
  exact absurd hcasc (by decide)

theorem wTag2_env_forced
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : WAdtEnvelope) (plan : IntDispatchRawPlan) (o : Obligation)
    (hface : WAdtIntFaceLower IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen
      unwrapName "unwrap" carrier hostTable env plan o) :
    env = wIntboxEnv := by
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
    change wLowerDispatchSig env = some sig at hslow
    unfold wLowerDispatchSig at hslow
    by_cases hc : checkWidenedEnvelope env = true
    · rw [if_pos hc] at hslow; rw [← Option.some.inj hslow]; rfl
    · rw [if_neg hc] at hslow; exact absurd hslow (by simp)
  rw [hsiglen] at hstake
  have hsig : sig = [0x60, 0x01, 0x63, 0x00, 0x01, 0x63, 0x05] :=
    hstake.symm.trans (by exact rfl)
  obtain ⟨hroot, hlen⟩ := wSig_forces_dispatch env sig hslow hsig
  rw [hroot] at hgcur
  have hgcur' : tag2GroupCursor = gcur :=
    Option.some.inj (tag2_group_cursor.symm.trans hgcur)
  subst hgcur'
  exact wGroup_forces_intbox env hroot hlen gb hglow tag2GroupCursor.1 hgtake
    (by exact rfl)

/-- The tag-2 forge (project a DECLARED payloadless constructor) dies: the forced
    widened envelope declares tag 2 as `unit`, not `hit`. -/
theorem wTag2_witness_dies
    (carrier : Nat) (hostTable : List (HostRole × Nat))
    (env : WAdtEnvelope) (o : Obligation) :
    ¬ WAdtIntFaceLower IntboxTag2Bytes.modBytes IntboxTag2Bytes.modLen
        unwrapName "unwrap" carrier hostTable env plan2 o := by
  intro hface
  have henv := wTag2_env_forced carrier hostTable env plan2 o hface
  subst henv
  obtain ⟨-, -, hcasc, -⟩ := hface
  exact absurd hcasc (by decide)

/-! ## §5 GOAL 4 (cont.) — the widened constructor face -/

/-- Widened `type Op = Add(Int) | Neg(Int) | Zero`: hit + hit + unit. -/
def wMkopEnv : WAdtEnvelope := ⟨0, [.hit, .hit, .unit]⟩

theorem wMkop_carrier : wCarrierIdx wMkopEnv = 6 := rfl

theorem wMkop_group_is_narrow :
    wLowerAdtRecGroup wMkopEnv = lowerAdtRecGroup mkopEnv := rfl

theorem wGroup_forces_mkop (env : WAdtEnvelope)
    (hroot : env.root = 0) (hlen : env.ctors.length = 3)
    (gb : List Nat) (hlow : wLowerAdtRecGroup env = some gb)
    (cur1 : Nat)
    (htake : CertDecode.takeBytes gb.length cur1 = gb)
    (hbytes : CertDecode.takeBytes 45 cur1 =
      mkopGroupBytes ++ [0x60, 0x00, 0x00]) :
    env = wMkopEnv := by
  obtain ⟨root, ctors⟩ := env
  have hroot' : root = 0 := hroot
  subst hroot'
  obtain ⟨c0, c1, c2, rfl⟩ : ∃ c0 c1 c2, ctors = [c0, c1, c2] := by
    rcases ctors with _ | ⟨c0, _ | ⟨c1, _ | ⟨c2, _ | ⟨c3, t⟩⟩⟩⟩
    all_goals first
      | exact ⟨c0, c1, c2, rfl⟩
      | (exfalso; simp only [List.length_cons, List.length_nil] at hlen; omega)
  cases c0 <;> cases c1 <;> cases c2 <;>
    first
    | rfl
    | (have hg : gb = _ := (Option.some.inj hlow).symm
       subst hg
       rw [takeBytes_take _ 45 cur1 (by decide), hbytes] at htake
       exact absurd htake (by decide))

theorem wMkop_env_forced
    (carrier structIdx fieldCount : Nat) (elemTy : ConstructValType)
    (symPlan : SymRawPlan) (env : WAdtEnvelope) (plan : ConstructRawPlan)
    (o : Obligation)
    (hface : WAdtCtorFaceLower MkopBytes.modBytes MkopBytes.modLen
      mkOpName "mkOp" carrier structIdx fieldCount elemTy symPlan env plan o) :
    env = wMkopEnv := by
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
    change wLowerCtorSig env = some sig at hslow
    unfold wLowerCtorSig at hslow
    by_cases hc : checkWidenedEnvelope env = true
    · rw [if_pos hc] at hslow; rw [← Option.some.inj hslow]; rfl
    · rw [if_neg hc] at hslow; exact absurd hslow (by simp)
  rw [hsiglen] at hstake
  have hsig : sig = [0x60, 0x01, 0x63, 0x06, 0x01, 0x63, 0x00] :=
    hstake.symm.trans (by exact rfl)
  obtain ⟨hroot, hlen⟩ := wSig_forces_ctor env sig hslow hsig
  rw [hroot] at hgcur
  have hgcur' : mkopGroupCursor = gcur :=
    Option.some.inj (mkop_group_cursor.symm.trans hgcur)
  subst hgcur'
  exact wGroup_forces_mkop env hroot hlen gb hglow mkopGroupCursor.1 hgtake
    (by exact rfl)

theorem wMkop_add_payload : wCtorShape? wMkopEnv 1 = some .hit := rfl

/-- The widened honest constructor obligation for mkOp. -/
def oWMkOpHonest : Obligation where
  export_ := "mkOp"
  policy := .simulatesModel
  carrier := 6
  code := fun fn => if fn = 1 then some ⟨1, 1, mkOpBody⟩ else none
  host := fun _ _ _ _ _ _ => none
  self := 1
  Dom := Int
  Cod := WAdtVal wMkopEnv
  domRepr := wIntArgDomRepr 6
  codRepr := wEnvCodRepr wMkopEnv
  model := wEnvCtorModel wMkopEnv 1 wMkop_add_payload

/-- The `Op.Zero` widened value (tag 3, `unit` shape, opaque empty fields). -/
def wZeroVal : WAdtVal wMkopEnv :=
  ⟨(3, .opaqueFields []), Or.inr ⟨.unit, rfl, by decide, [], rfl⟩⟩

/-- CONSTRUCTOR FORGE `model := Op.Zero`: DIES (wrong result constructor tag). -/
def oWMkOpForgeZero : Obligation :=
  { oWMkOpHonest with model := fun _ => wZeroVal }

theorem wMkop_forge_zero_dies (env : WAdtEnvelope) :
    ¬ WAdtCtorFaceLower MkopBytes.modBytes MkopBytes.modLen
        mkOpName "mkOp" 6 1 1 .i64 mkOpSymPlan env mkOpPlan oWMkOpForgeZero := by
  intro hface
  have henv := wMkop_env_forced 6 1 1 .i64 mkOpSymPlan env mkOpPlan
    oWMkOpForgeZero hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, hpay, -, -, -, -, hmodel⟩ := hface
  have heq : (fun _ => wZeroVal) = wEnvCtorModel wMkopEnv 1 hpay :=
    eq_of_heq hmodel
  have hv : (3 : Nat) = 1 :=
    congrArg (fun z => (Subtype.val z).1) (congrFun heq 2)
  exact absurd hv (by decide)

/-- CONSTRUCTOR FORGE at the payloadless tag (`structIdx := 3`): the forced
    widened envelope declares tag 3 as `unit`, not `hit`. -/
theorem wMkop_payloadless_structIdx_dies (env : WAdtEnvelope) (o : Obligation) :
    ¬ WAdtCtorFaceLower MkopBytes.modBytes MkopBytes.modLen
        mkOpName "mkOp" 6 3 1 .i64 mkOpSymPlan env mkOpPlan o := by
  intro hface
  have henv := wMkop_env_forced 6 3 1 .i64 mkOpSymPlan env mkOpPlan o hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, hpay, -⟩ := hface
  exact absurd hpay (by decide)

/-- CONSTRUCTOR FORGE at a fabricated tag (`structIdx := 8`): out of range. -/
theorem wMkop_fabricated_structIdx_dies (env : WAdtEnvelope) (o : Obligation) :
    ¬ WAdtCtorFaceLower MkopBytes.modBytes MkopBytes.modLen
        mkOpName "mkOp" 6 8 1 .i64 mkOpSymPlan env mkOpPlan o := by
  intro hface
  have henv := wMkop_env_forced 6 8 1 .i64 mkOpSymPlan env mkOpPlan o hface
  subst henv
  obtain ⟨-, -, -, -, -, -, -, -, hpay, -⟩ := hface
  exact absurd hpay (by decide)

/-! ## §6 GOAL 5 — honest recovery of the out-of-profile shapes

The NARROW `AdtEnvelope` (`ctors : List Bool`) cannot even STATE a String / Float
/ Bool / List / Map constructor. The widened envelope lowers them, and the
rec-group pin CLOSES for these shapes against a real (synthetic) module whose
type section is exactly the widened lowering. This is the envelope-level coupling
test: each out-of-profile shape now provably satisfies the widened lower-pinned
face's rec-group column. (End-to-end acceptance for e.g. `json.av` additionally
needs its compiled code section, which travels the already-certified
int-dispatch path unchanged.) -/

/-- Little-endian byte assembly, the inverse of `CertDecode.takeBytes`. -/
def natOfBytesLE : List Nat → Nat
  | [] => 0
  | b :: rest => b + 256 * natOfBytesLE rest

/-- Minimal well-framed wasm module carrying `groupCount` rec groups whose bytes
    are `body`, as its type section. -/
def synthMod (groupCount : Nat) (body : List Nat) : Nat :=
  natOfBytesLE
    ([0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
      0x01, body.length + 1, groupCount] ++ body)

def synthModLen (body : List Nat) : Nat := 11 + body.length

/-- `type Json = JsonInt(Int) | JsonString(String) | JsonFloat(Float)
    | JsonBool(Bool) | JsonList(List) | JsonObject(Map) | JsonNull`: the widened
    `jsonInt` envelope, entirely OUTSIDE the narrow Bool profile. -/
def jsonEnv : WAdtEnvelope :=
  ⟨0, [.hit, .strBox, .floatBox, .boolBox, .listBox, .mapBox, .unit]⟩

theorem jsonEnv_out_of_narrow_profile : checkWidenedEnvelope jsonEnv = true := rfl

def jsonBytes : List Nat := (wLowerAdtRecGroup jsonEnv).getD []
def jsonMod : Nat := synthMod 1 jsonBytes
def jsonModLen : Nat := synthModLen jsonBytes

/-- The widened `jsonInt` match (return the Int of `JsonInt`, else 0) is
    plan-consistent with the widened `jsonEnv`. -/
theorem jsonInt_cascadeInEnv :
    wCascadeInEnv jsonEnv (.test 1 .proj (.default 0)) = true := rfl

/-- GOAL 5(a): the widened rec-group face is SATISFIED by the json envelope. -/
theorem jsonEnv_recovered :
    wAdtRecGroupFact.pinnedAt jsonMod jsonModLen 0 jsonEnv :=
  ⟨jsonBytes, rfl, ⟨_, rfl, by decide, by decide +kernel⟩⟩

/-- `type Box = StrBox(String) | Empty`: the widened `Box.StrBox(String)` match. -/
def boxEnv : WAdtEnvelope := ⟨0, [.strBox, .unit]⟩
def boxBytes : List Nat := (wLowerAdtRecGroup boxEnv).getD []
def boxMod : Nat := synthMod 1 boxBytes
def boxModLen : Nat := synthModLen boxBytes

/-- GOAL 5(b): the widened rec-group face is SATISFIED by the `Box.StrBox` env. -/
theorem boxEnv_recovered :
    wAdtRecGroupFact.pinnedAt boxMod boxModLen 0 boxEnv :=
  ⟨boxBytes, rfl, ⟨_, rfl, by decide, by decide +kernel⟩⟩

/-- The positive widened bridge specializes to the json envelope: every
    represented `Json` input relates to the widened `jsonInt` model, whatever its
    non-hit (String/Float/Bool/List/Map) payload shape. -/
theorem json_bridge_applies
    (S : CarrierSpec (wCarrierIdx jsonEnv)) (x : WAdtVal jsonEnv) (vs : List WVal)
    (hdom : wEnvDomRepr jsonEnv S x vs) :
    ∃ tag fields n,
      vs = [.structv tag fields] ∧
      IntDispatchSoundness.EvalCascade S (.test 1 .proj (.default 0)) tag fields n ∧
      ∀ w, S.Repr n w →
        intRepr S (wEnvStructModel jsonEnv (.test 1 .proj (.default 0)) x) w :=
  env_widenedIntDispatch_bridge jsonEnv
    { profile := "int-dispatch-v1", body := .test 1 .proj (.default 0) }
    jsonInt_cascadeInEnv S x vs hdom

/-! ## §7 GOAL 3 — multi-ADT modules: pin the plan-named root

Dropping the single-ADT-per-module constraint. A module can hold SEVERAL rec
groups; the lowering pins whichever root the plan names. Here two ADTs coexist:
`multiEnv1` at root 0 (six type indices, 0..5) and `multiEnv2` at root 6. Each is
independently pinned at its OWN root; the cursor navigation for one root parses
past — and discards — the other group entirely. -/

def multiEnv1 : WAdtEnvelope := ⟨0, [.hit, .unit]⟩
def multiEnv2 : WAdtEnvelope := ⟨6, [.hit, .strBox]⟩

def multiBytes1 : List Nat := (wLowerAdtRecGroup multiEnv1).getD []
def multiBytes2 : List Nat := (wLowerAdtRecGroup multiEnv2).getD []
def multiMod : Nat := synthMod 2 (multiBytes1 ++ multiBytes2)
def multiModLen : Nat := synthModLen (multiBytes1 ++ multiBytes2)

/-- GOAL 3(a): the FIRST ADT is pinned at its root 0, ignoring the second. -/
theorem multiAdt_root0_pin :
    wAdtRecGroupFact.pinnedAt multiMod multiModLen 0 multiEnv1 :=
  ⟨multiBytes1, rfl, ⟨_, rfl, by decide, by decide +kernel⟩⟩

/-- GOAL 3(b) — THE named-root pin: the SECOND ADT is pinned at its plan-named
    root 6, the cursor navigating past the entire first rec group first. This is
    the multi-entry rec-group navigation the narrow profile lacked. -/
theorem multiAdt_named_root_pin :
    wAdtRecGroupFact.pinnedAt multiMod multiModLen 6 multiEnv2 :=
  ⟨multiBytes2, rfl, ⟨_, rfl, by decide, by decide +kernel⟩⟩

#print axioms multiAdt_root0_pin
#print axioms multiAdt_named_root_pin

#print axioms jsonEnv_recovered
#print axioms boxEnv_recovered
#print axioms json_bridge_applies

#print axioms wUnwrap_honest_passes
#print axioms wIntbox_env_forced
#print axioms wForge_model_17_dies
#print axioms wForge_domRepr_false_dies
#print axioms wForge_flipped_envelope_dies
#print axioms wTag7_witness_dies
#print axioms wTag2_witness_dies
#print axioms wMkop_env_forced
#print axioms wMkop_forge_zero_dies
#print axioms wMkop_payloadless_structIdx_dies
#print axioms wMkop_fabricated_structIdx_dies

end AverCert.WidenedEnvelope.Tests
