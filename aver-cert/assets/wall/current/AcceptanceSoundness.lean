/-
ACCEPTANCE-SOUNDNESS ASSEMBLY (statement schema 9).

`fn_claim_discharges`: every obligation the wall derives from the plans holds
at its policy. One application of `GrammarSound.fn_certified_group` over ALL
plans (fuel induction: self, mutual and cross-group calls alike) gives every
planned function `FnCertified` at the one model of all plans; for an L3
obligation, `GrammarTotal.fn_certified_total_of_check` over its call group,
with every function outside the group taken from that result, adds `FnTotal`.
The only runtime premises are the named contracts of `Schema.HostContracts`
(and `Schema.HostTotal` at L3); the box helper is the wall's `boxRef`, and the
never-declared negation index is the trap-only function.

`accept_sound`: the conjuncts of `AcceptedArtifact.accepted` other than
`Holds` imply `Holds`, so `Holds` is derived, never asserted. The theorem is
about the plans' lowering (`codeOf`); the byte conjuncts of the acceptance
(`plansAccepted`) are what make that lowering the delivered code.
-/
import AcceptanceSoundnessCore
import ClaimAxes

open AverCert
open AverCert.Schema
open AverCert.Grammar
open AverCert.TypeTable
open AverCert.AcceptedArtifact
open CertPrelude

namespace AcceptanceSoundness

section Discharge
variable {s : Subject} {tt : TypeTable} {fns : List FnEntry}

/-- The named contracts give the grammar's Int contracts, with the wall's
    `boxRef` as the box helper. -/
theorem contracts_of {C : Nat} (S : CarrierSpec C) (h : HostFns) (hc : HostContracts S h) :
    Contracts S (boxRef C) h.add h.sub h.mul h.cmp h.eq where
  hBox := by
    intro n w hlo hhi hw
    simp only [boxRef, Option.some.injEq] at hw
    subst hw
    exact ⟨S.smallIntro n, (S.canonSmall n).mpr ⟨hlo, hhi⟩⟩
  hAdd := hc.add
  hSub := hc.sub
  hMul := hc.mul
  hCmp := hc.cmp
  hEq := hc.eq

/-- Every planned function is certified at the one model of all plans. -/
theorem fns_certified (hf : PlanFacts s tt fns)
    (S : CarrierSpec (mctxOf s tt fns).carrier) (h : HostFns) (hc : HostContracts S h) :
    ∀ f p, planOf fns f = some p →
      FnCertified S (mctxOf s tt fns) (codeOf (mctxOf s tt fns) fns)
        (hostOf (mctxOf s tt fns) h) f p.sig (fun fuel => modelOf fns fuel f) := by
  obtain ⟨hBox, hAdd, hSub, hMul, hNeg, hCmp, hEq, hConcat, hStreq, hToIndex, hDivmod,
    hClaims⟩ :=
    host_facts (M := mctxOf s tt fns) h hf.distinct
  have R : XHost S (mctxOf s tt fns) (hostOf (mctxOf s tt fns) h) :=
    ⟨⟨_, hConcat, fun parts c hr => hc.stringConcat _ parts c hr⟩,
     ⟨_, hStreq, fun a b r hr => hc.stringEq a b r hr⟩,
     ⟨_, hToIndex, hc.toIndex⟩,
     ⟨_, hDivmod, fun a b wa wb m r ha hb hne hm hr =>
        hc.divmod a b wa wb m r ha.1 hb.1 ha.2 hb.2 hne hm hr⟩⟩
  refine fn_certified_group S (boxRef _) h.add h.sub h.mul h.cmp h.eq (fun _ => none)
    (contracts_of S h hc) (fun _ _ _ _ hr => by cases hr) _ _ (mctxOf s tt fns) rfl
    hBox hAdd hSub hMul hNeg hCmp hEq R (planOf fns) (fun _ _ _ => none) ?_ ?_
  · intro f sig hs hG
    simp [mctxOf, hG] at hs
  · intro f p hp
    obtain ⟨e, he, rfl, rfl⟩ := planOf_some hp
    refine ⟨by simp [mctxOf, hp], hf.typed e he, hClaims e he, by simp [codeOf, hp]⟩

theorem obligationsOf_mem {o : Obligation} (ho : o ∈ obligationsOf s tt fns) :
    ∃ e ∈ fns, o = obligationOf s tt fns e := by
  unfold obligationsOf at ho
  obtain ⟨e, he, rfl⟩ := List.mem_map.mp ho
  exact ⟨e, (List.mem_filter.mp he).1, rfl⟩

theorem groupMembers_mem {g f : Nat} {p : FnPlan} (h : (f, p) ∈ groupMembers fns g) :
    ∃ e ∈ fns, e.funcIdx = f ∧ e.plan = p := by
  unfold groupMembers at h
  obtain ⟨e, he, hep⟩ := List.mem_map.mp h
  simp only [Prod.mk.injEq] at hep
  exact ⟨e, (List.mem_filter.mp he).1, hep.1, hep.2⟩

/-- The partial half of every derived obligation. -/
theorem obligation_holds (hf : PlanFacts s tt fns) {e : FnEntry} (he : e ∈ fns) :
    (obligationOf s tt fns e).holds := by
  intro S h hc fuel svs ws r hT hR hrun
  exact (fns_certified hf S h hc e.funcIdx e.plan (planOf_mem hf.nodup he)).2.2
    fuel svs ws r hT hR hrun

/-- The total half of an L3 obligation: its call group passed the wall's
    termination check, so every member returns at fuel `n.natAbs + 1`. -/
theorem obligation_total (hf : PlanFacts s tt fns) {e : FnEntry} (he : e ∈ fns)
    {role : TotalityRole} (hck : checkTermGroup (groupMembers fns e.group) = some role) :
    ∀ (S : CarrierSpec (obligationOf s tt fns e).carrier) (h : HostFns), HostContracts S h →
      HostTotal S h role →
      ∀ (svs : List SVal) (ws : List WVal),
        HasTyL (obligationOf s tt fns e).layout svs (obligationOf s tt fns e).sig.params →
        SReprL S (obligationOf s tt fns e).layout svs ws →
        ∃ n tl, svs = .i n :: tl ∧ ∃ r sv,
          wFuncN (obligationOf s tt fns e).code ((obligationOf s tt fns e).host h)
            (n.natAbs + 1) (obligationOf s tt fns e).self ws = some r ∧
          (obligationOf s tt fns e).model (n.natAbs + 1) svs = some sv ∧
          SRepr S (obligationOf s tt fns e).layout sv r ∧
          HasTy (obligationOf s tt fns e).layout sv (obligationOf s tt fns e).sig.ret := by
  intro S h hc ht
  let ms := groupMembers fns e.group
  have hnd := hf.nodup
  have hmsnd : (ms.map (·.1)).Nodup := by
    have hsub : ms.map (·.1) = (fns.filter (·.group == e.group)).map (·.funcIdx) := by
      simp [ms, groupMembers, Function.comp_def]
    rw [hsub]
    exact hnd.sublist (List.Sublist.map _ (List.filter_sublist))
  have hGall : ∀ m ∈ ms, groupOf ms m.1 = some m.2 := by
    intro m hm
    have := find?_key_nodup (fun y : Nat × FnPlan => y.1) hmsnd hm
    simp only [groupOf]
    rw [this]
    rfl
  have hGP : ∀ f p, groupOf ms f = some p → planOf fns f = some p := by
    intro f p hg
    obtain ⟨e', he', rfl, rfl⟩ := groupMembers_mem (groupOf_mem hg)
    exact planOf_mem hnd he'
  obtain ⟨hBox, hAdd, hSub, hMul, hNeg, hCmp, hEq, hConcat, hStreq, hToIndex, hDivmod,
    hClaims⟩ :=
    host_facts (M := mctxOf s tt fns) h hf.distinct
  have R : XHost S (mctxOf s tt fns) (hostOf (mctxOf s tt fns) h) :=
    ⟨⟨_, hConcat, fun parts c hr => hc.stringConcat _ parts c hr⟩,
     ⟨_, hStreq, fun a b r hr => hc.stringEq a b r hr⟩,
     ⟨_, hToIndex, hc.toIndex⟩,
     ⟨_, hDivmod, fun a b wa wb m r ha hb hne hm hr =>
        hc.divmod a b wa wb m r ha.1 hb.1 ha.2 hb.2 hne hm hr⟩⟩
  have hAll := fns_certified hf S h hc
  have key := fn_certified_total_of_check S (boxRef _) h.add h.sub h.mul h.cmp h.eq
    (fun _ => none) (contracts_of S h hc) (fun _ _ _ _ hr => by cases hr)
    (codeOf (mctxOf s tt fns) fns) (hostOf (mctxOf s tt fns) h) (mctxOf s tt fns) rfl
    hBox hAdd hSub hMul hNeg hCmp hEq R ms role hck (groupOf ms)
    (fun f p hg => groupOf_mem hg) hGall (modelOf fns)
    (by
      intro f sig hs _
      simp only [mctxOf, Option.map_eq_some_iff] at hs
      obtain ⟨p, hp, rfl⟩ := hs
      exact hAll f p hp)
    (by
      intro f p hg
      have hp := hGP f p hg
      obtain ⟨e', he', rfl, rfl⟩ := planOf_some hp
      exact ⟨by simp [mctxOf, hp], hf.typed e' he', hClaims e' he', by simp [codeOf, hp]⟩)
    (fun k _ _ => boxRef_total _ k) ht.add ht.sub ht.mul
    e.funcIdx e.plan (hGall (e.funcIdx, e.plan) (by
      simp only [ms, groupMembers]
      exact List.mem_map.mpr ⟨e, List.mem_filter.mpr ⟨he, by simp⟩, rfl⟩))
  intro svs ws hT hR
  obtain ⟨n, tl, hsv, r, sv, hrun, hm, hrep, hty⟩ := key.2 svs ws hT hR
  refine ⟨n, tl, hsv, r, sv, hrun, ?_, hrep, hty⟩
  show groupModel (fun _ _ _ => none) (planOf fns) (n.natAbs + 1) e.funcIdx svs = some sv
  rw [← groupModel_restrict (planOf fns) (groupOf ms) hGP]
  exact hm

/-- Every derived obligation holds at its policy. -/
theorem fn_claim_discharges (hf : PlanFacts s tt fns) :
    ∀ o ∈ obligationsOf s tt fns, obligationHolds o := by
  intro o ho
  obtain ⟨e, he, rfl⟩ := obligationsOf_mem ho
  unfold obligationHolds
  cases hpol : (obligationOf s tt fns e).policy with
  | simulatesModel => exact obligation_holds hf he
  | simulatesModelTotally =>
      refine ⟨obligation_holds hf he, ?_⟩
      have hax : (obligationOf s tt fns e).policy = (axesOf fns e).1 := rfl
      have hrole : (obligationOf s tt fns e).totalityRole = (axesOf fns e).2.2 := rfl
      unfold axesOf groupPolicy at hax hrole
      cases hck : checkTermGroup (groupMembers fns e.group) with
      | none =>
          rw [hck] at hax
          rw [hax] at hpol
          cases hpol
      | some role =>
          rw [hck] at hrole
          intro S h hc ht
          rw [hrole] at ht
          exact obligation_total hf he hck S h hc ht

end Discharge

/-- The statement at an artifact hash, without the generated `Module.lean`. -/
def holdsAtHash (wasmSha256 : String) (m : Manifest) : Prop :=
  m.subject.artifactHash = wasmSha256 ∧
  m.subject.profile = expectedProfile ∧
  artifactTargetAbiAccepted m.subject.target m.subject.abi = true ∧
  HoldsCore m

/-- ROOT THEOREM. The manifest's obligations are the derived ones and the plans
    pass the acceptance's byte facts, hence every certified export's emitted
    function simulates its plan's model (`holds`), and every L3 export also
    returns at the checked fuel (`holdsTotal`), under exactly the named runtime
    contracts. The hash and target/profile/ABI identity are premises: they are
    fixed statement metadata, pinned by the checker. -/
theorem accept_sound
    (wasmSha256 : String)
    (artifact : ArtifactData)
    (hHash : artifact.manifest.subject.artifactHash = wasmSha256)
    (hProfile : artifact.manifest.subject.profile = expectedProfile)
    (hTargetAbi : artifactTargetAbiAccepted artifact.manifest.subject.target
        artifact.manifest.subject.abi = true)
    (hDerived : obligationsDerived artifact)
    (hPlans : plansAccepted artifact = true) :
    holdsAtHash wasmSha256 artifact.manifest := by
  refine ⟨hHash, hProfile, hTargetAbi, ?_⟩
  rw [holdsCore_iff]
  intro o ho
  rw [hDerived] at ho
  exact fn_claim_discharges (planFacts_of_accepted artifact hPlans) o ho

/-! ### Non-vacuity

`Obligation.holds` quantifies over well-typed source arguments. The
acceptance's declaration check (`TypeTable.declsWellFormed`) makes that
quantification non-empty: every certified export has well-typed arguments,
and its result type has a value, so no accepted obligation is true merely
because its hypothesis cannot be met. -/

/-- Every certified export of an accepted artifact has well-typed arguments
    (and its result type is inhabited): its obligation is not vacuous. -/
theorem accepted_nonvacuous (artifact : ArtifactData)
    (hDerived : obligationsDerived artifact) (hPlans : plansAccepted artifact = true) :
    ∀ o ∈ artifact.manifest.obligations,
      (∃ svs, HasTyL o.layout svs o.sig.params) ∧ ∃ sv, HasTy o.layout sv o.sig.ret := by
  intro o ho
  rw [hDerived] at ho
  obtain ⟨e, he, rfl⟩ := obligationsOf_mem ho
  have hwf : declsWellFormed artifact.manifest.subject artifact.manifest.types
      artifact.manifest.fnPlans = true := by
    simp only [plansAccepted, Bool.and_eq_true] at hPlans
    exact hPlans.2
  have hti : typesInhabited (mctxOf artifact.manifest.subject artifact.manifest.types
      artifact.manifest.fnPlans) artifact.manifest.types artifact.manifest.fnPlans = true := by
    simp only [declsWellFormed, Bool.and_eq_true] at hwf
    exact hwf.2
  simp only [typesInhabited, Bool.and_eq_true, List.all_eq_true] at hti
  have hsig := hti.2 e he
  exact ⟨inhabitedL_sound (List.all_eq_true.mpr hsig.1), inhabited_sound hsig.2⟩

/-! ### S-3: the exact `ref.test` of an accepted artifact is the wasm test -/

/-- For every sum an accepted artifact declares, the interpreter's exact
    `ref.test` on two of its constructor structs agrees with the wasm subtype
    test, under the two wasm facts `GrammarSound.GcTestSpec` states about the
    rec group that opens the type section. -/
theorem refTest_exact_of_accepted (artifact : ArtifactData)
    (hPlans : plansAccepted artifact = true)
    {grp : List (List Nat × _root_.CertDecode.TypeEntry)}
    (hg : firstRecGroup artifact.modBytes artifact.modLen = some grp)
    {sub : Nat → Nat → Prop} (hspec : GcTestSpec (grp.map (·.1)) sub)
    {d : SumDecl} (hd : d ∈ artifact.manifest.types.sums) {a b : Nat}
    (ha : a < d.ctors.length) (hb : b < d.ctors.length) :
    let M := mctxOf artifact.manifest.subject artifact.manifest.types artifact.manifest.fnPlans
    M.ctorStruct d.tid a = M.ctorStruct d.tid b ↔
      sub (M.ctorStruct d.tid a) (M.ctorStruct d.tid b) := by
  intro M
  have hpin : S3Pin M d.tid d.ctors.length (grp.map (·.1)) = true := by
    simp only [plansAccepted, Bool.and_eq_true] at hPlans
    have htt := hPlans.1.1.1.2
    unfold typeTableConfirmed at htt
    simp only [hg, Bool.and_eq_true, List.all_eq_true] at htt
    have hs := htt.2.1.1.1.1.1.1.1.2 d hd
    simp only [sumConfirmed, Bool.and_eq_true] at hs
    exact hs.2
  exact ctor_refTest_exact hspec hpin ha hb

#print axioms fn_claim_discharges
#print axioms accept_sound
#print axioms refTest_exact_of_accepted
#print axioms accepted_nonvacuous

end AcceptanceSoundness
