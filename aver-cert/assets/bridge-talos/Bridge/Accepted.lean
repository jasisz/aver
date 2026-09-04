import Bridge.Coverage
import Bridge.Adapter
import Bridge.Bridge

set_option autoImplicit false

/-!
# Composition over the accepted artifact (brief §9 (4))

What the wall PINS for a record projection-compute claim
(`StandardFace.recordComputeDeclaredFace`, `symFragmentMatches`,
`symFragmentPlanAccepted`), read off the wall and taken here as hypotheses in
the wall's own shapes (verbatim in `AverMin.lean`):

* `hostTableBound roles claim.hostTable` — the claim's role table has distinct
  indices and agrees with the byte-derived role table;
* `classifyRecordCompute claim.hostTable plan = some face` — every node passes
  `recordComputeNodeOk claim.hostTable` (host calls cite the table's index for
  their role, `constI64` and `intSignCmp` literals are in band), every cited
  user-struct index is `face.structIdx`, and the plan is typed by
  `planTypedB face.structIdx tyOf plan.params plan.body.nodes` (the arguments
  of `structNew` and the results of `structGetUser` are Int carriers, `box`
  takes an `i64`, …); we take the three Bool facts, not the classifier itself
  (`fragNodeComputes` and the `Option` plumbing add nothing the bridge reads);
* an all-Int record declaration `.record face.structIdx fields`, non-empty,
  `checkRecordDecl`, whose `lowerTypeDecl` IS the type-section entry at
  `face.structIdx` (`typeSectionMatches`) — the bridge consumes the
  DECLARATION; the pin binds it to the bytes;
* `lowerExprFragmentBody claim.carrier plan = some body` — the body the
  obligation's code table carries at the export (`exprFragmentPlanAccepted`),
  with `nlocals = 1`.

This file proves the profile half: the translation environment is
`envOfClaim claim.hostTable claim.carrier [.record face.structIdx fields]` —
nothing but those pinned declarations — and the plan is in the profile
relative to it (`planInProfile_of_recordCompute`). The composition sentence
over it, stated at Talos's export boundary, is `recordCompute_runsExport`
(`RunsExport.lean`).

## What the declared envelope does NOT pin (the precise extra hypotheses)

1. `hi32 : constI32 literals in band` — `recordComputeNodeOk` pins the band
   of `constI64` and of the `intSignCmp` literal, not of `constI32` (the
   encoder emits only `i32.const 0` there, but the classifier does not say
   so); the byte pin excludes a >5-byte LEB only because a real module could
   not contain it, which the wall does not decode.
2. `harity : struct arity agreement` — for every `structNew face.structIdx
   args` node, `args.length = fields.length`, and for every `structGetUser
   face.structIdx field _` node, `field < fields.length`. The declaration's
   field COUNT is byte-pinned (`typeSectionMatches` at `face.structIdx`,
   `lowerTypeDecl` is injective on it) and the node's arity is byte-pinned
   (`PlanBytes` prints the plan), but no wall conjunct relates the two: a
   `struct.new` with the wrong operand count is invalid wasm, which the wall
   never validates. This is the envelope gap of brief §9 made precise.
3. `hne : face.structIdx ≠ claim.carrier` — byte-derived (the carrier entry is
   `isCarrier`, an `i64`-first struct; the record entry is a struct of
   references), but not a declared-data fact.

Everything else the bridge needs is derived below from the wall's conjuncts.
-/

namespace Bridge
open CertPrelude AverCert.Schema AverCert.PlanCheck AverCert.PlanLower AverCert.StandardFace
  RecordComputeBridge
open AverCert.Schema (CarrierSpec)
open Wasm Wasm.SmallStep

/-! ## The checker fixes node ids to positions -/

theorem checkNodes_positions (f : List FragNode → FragNode → Option FragTy) :
    ∀ (checked rest : List FragNode), checkBlockFuel.checkNodes f checked rest = true →
      ∀ n ∈ rest, (checked ++ rest)[n.id]? = some n
  | checked, [], _, n, hn => by simp at hn
  | checked, node :: rest, h, n, hn => by
      obtain ⟨hid, -, h'⟩ := checkNodes_cons f checked node rest h
      simp only [List.mem_cons] at hn
      rcases hn with rfl | hn
      · exact lookupNode_self hid
      · have := checkNodes_positions f (checked ++ [node]) rest h' n hn
        simpa using this

/-- Every node was checked against the prefix before it. -/
theorem checkNodes_infer (f : List FragNode → FragNode → Option FragTy) :
    ∀ (checked rest : List FragNode), checkBlockFuel.checkNodes f checked rest = true →
      ∀ n ∈ rest, ∃ pre post, checked ++ rest = pre ++ n :: post ∧ f pre n = some n.ty
  | checked, [], _, n, hn => by simp at hn
  | checked, node :: rest, h, n, hn => by
      obtain ⟨-, ⟨ty, hf, hty⟩, h'⟩ := checkNodes_cons f checked node rest h
      simp only [List.mem_cons] at hn
      rcases hn with rfl | hn
      · exact ⟨checked, rest, rfl, hty ▸ hf⟩
      · obtain ⟨pre, post, heq, hf'⟩ := checkNodes_infer f (checked ++ [node]) rest h' n hn
        exact ⟨pre, post, by simpa using heq, hf'⟩

/-- The arguments of a checked `box` call exist and are `i64` nodes. -/
theorem block_box_arg {cf : Nat} {params : List FragTy} {block : FragBlock}
    (h : checkBlockFuel (cf + 1) params block = true) :
    ∀ n ∈ block.nodes, ∀ f a, n.kind = .hostCall .box f [a] →
      ∃ m, lookupNode block.nodes a = some m ∧ m.ty = .i64 := by
  rw [checkBlockFuel_succ] at h
  simp only [Bool.and_eq_true] at h
  intro n hn f a hk
  obtain ⟨pre, post, heq, hinf⟩ := checkNodes_infer _ [] block.nodes h.1 n hn
  simp only [List.nil_append] at heq
  simp only [inferNodeKindTy, hk, hostCallResultTy?] at hinf
  split at hinf
  · rename_i hargs
    simp only [argsHaveTys, Bool.and_true] at hargs
    obtain ⟨m, hm, hmty⟩ := lookupNode_of_lookupTy (hasTy_lookupTy hargs)
    exact ⟨m, heq ▸ lookupNode_prefix _ hm, hmty⟩
  · simp at hinf

theorem block_positions {cf : Nat} {params : List FragTy} {block : FragBlock}
    (h : checkBlockFuel cf params block = true) : ∀ n ∈ block.nodes, block.nodes[n.id]? = some n := by
  cases cf with
  | zero => simp [checkBlockFuel] at h
  | succ cf =>
    rw [checkBlockFuel_succ] at h
    simp only [Bool.and_eq_true] at h
    intro n hn
    simpa using checkNodes_positions _ [] block.nodes h.1 n hn

/-! ## The face's typing facts, read through `tyOf` -/

/-- The classifier's type map (`StandardFace.classifyRecordCompute`). -/
def tyOfPlan (plan : ExprFragmentRawPlan) : Nat → FragTy :=
  fun nodeId => ((plan.body.nodes[nodeId]?).map (fun n => n.ty)).getD .i64

theorem tyOfPlan_self {plan : ExprFragmentRawPlan} {n : FragNode}
    (hpos : plan.body.nodes[n.id]? = some n) : tyOfPlan plan n.id = n.ty := by
  simp [tyOfPlan, hpos]

theorem tyOfPlan_arg {plan : ExprFragmentRawPlan} {a : Nat} {t : FragTy} (hne : t ≠ .i64)
    (h : tyOfPlan plan a = t) : ∃ n, lookupNode plan.body.nodes a = some n ∧ n.ty = t := by
  simp only [tyOfPlan] at h
  match hget : plan.body.nodes[a]? with
  | none => rw [hget] at h; exact absurd h.symm hne
  | some n => rw [hget] at h; exact ⟨n, hget, by simpa using h⟩

theorem mapM_scalarSort_intCarrier :
    ∀ (fields : List TypeDecl),
      (fields.all fun f => match f with | .intCarrier => true | _ => false) = true →
      fields.mapM scalarSort = some (List.replicate fields.length .car)
  | [], _ => rfl
  | f :: fs, h => by
      simp only [List.all_cons, Bool.and_eq_true] at h
      have hf : f = .intCarrier := by cases f <;> simp_all
      subst hf
      have ih := mapM_scalarSort_intCarrier fs h.2
      simp [List.mapM_cons, scalarSort, ih, List.replicate_succ]

/-- The one struct-table entry of the compute face's environment. -/
theorem envOfClaim_record (ht : List (HostRole × Nat)) (C structIdx : Nat) (fields : List TypeDecl)
    (hfields : (fields.all fun f => match f with | .intCarrier => true | _ => false) = true)
    (hdecl : checkRecordDecl (.record structIdx fields) = true) (hne : structIdx ≠ C) :
    structSorts? (envOfClaim ht C [.record structIdx fields]).structs structIdx =
      some (List.replicate fields.length .car) := by
  have hne' : ¬ (C = structIdx) := fun h => hne h.symm
  simp [envOfClaim, structSorts?, hne', declEntry?, hdecl, mapM_scalarSort_intCarrier fields hfields]

/-! ## The plan is in the profile -/

/-- Brief §9 (4), the profile half: a plan the compute-face classifier admits
    is in the bridge's profile relative to the claim's environment, given the
    three extra hypotheses of the header. -/
theorem planInProfile_of_recordCompute
    (roles : CertDecode.AddSub.Roles) (ht : List (HostRole × Nat)) (C structIdx : Nat)
    (fields : List TypeDecl) (plan : ExprFragmentRawPlan)
    (hbound : hostTableBound roles ht = true)
    (hok : (plan.body.nodes.all fun n => recordComputeNodeOk ht n.kind) = true)
    (hidx : ((plan.body.nodes.filterMap fun n => fragNodeStructIdx? n.kind).all (· == structIdx)) = true)
    (htyped : planTypedB structIdx (tyOfPlan plan) plan.params plan.body.nodes = true)
    (hparams : (plan.params.all (· == .adtRef)) = true)
    (hchk : checkBlockFuel AverCert.PlanCheck.maxFuel plan.params plan.body = true)
    (hfields : (fields.all fun f => match f with | .intCarrier => true | _ => false) = true)
    (hdecl : checkRecordDecl (.record structIdx fields) = true)
    (hne : structIdx ≠ C)
    (hi32 : ∀ n ∈ plan.body.nodes, ∀ v, n.kind = .constI32 v → i32Band v)
    (harity : ∀ n ∈ plan.body.nodes,
      (∀ args, n.kind = .structNew structIdx args → args.length = fields.length) ∧
      (∀ field value, n.kind = .structGetUser structIdx field value → field < fields.length)) :
    planInProfile (envOfClaim ht C [.record structIdx fields]) plan = true := by
  have hnd := hostTableBound_nodup hbound
  have hpos := block_positions hchk
  have hchk : checkBlockFuel (9999 + 1) plan.params plan.body = true := hchk
  have hrec := envOfClaim_record ht C structIdx fields hfields hdecl hne
  simp only [planInProfile, blockInProfile, List.all_eq_true]
  intro n hn
  have hok' : recordComputeNodeOk ht n.kind = true := by
    simp only [List.all_eq_true] at hok; exact hok n hn
  have htyped' : nodeTypedB structIdx (tyOfPlan plan) plan.params n = true := by
    simp only [planTypedB, List.all_eq_true] at htyped; exact htyped n hn
  have hself := tyOfPlan_self (hpos n hn)
  -- The only `.i64`-typed nodes of an admitted plan are `constI64` literals.
  have hi64 : ∀ m ∈ plan.body.nodes, m.ty = .i64 → ∃ v, m.kind = .constI64 v := by
    intro m hm hty
    have hokm : recordComputeNodeOk ht m.kind = true := by
      simp only [List.all_eq_true] at hok; exact hok m hm
    have htm : nodeTypedB structIdx (tyOfPlan plan) plan.params m = true := by
      simp only [planTypedB, List.all_eq_true] at htyped; exact htyped m hm
    have hselfm := tyOfPlan_self (hpos m hm)
    cases hk : m.kind
    case constI64 v => exact ⟨v, rfl⟩
    case «local» index =>
      simp only [nodeTypedB, hk, hselfm, beq_iff_eq] at htm
      have : FragTy.i64 ∈ plan.params := hty ▸ List.mem_of_getElem? htm
      simp only [List.all_eq_true, beq_iff_eq] at hparams
      exact absurd (hparams _ this) (by decide)
    case constBool value => simp [recordComputeNodeOk, hk] at hokm
    case constI32 value => simp [nodeTypedB, hk, hselfm, hty] at htm
    case constF64Bits bits => simp [recordComputeNodeOk, hk] at hokm
    case structGet field receiver => simp [recordComputeNodeOk, hk] at hokm
    case structGetUser tyIdx field value => simp [nodeTypedB, hk, hselfm, hty] at htm
    case refIsNull value => simp [recordComputeNodeOk, hk] at hokm
    case prim op args => simp [nodeTypedB, hk, hselfm, hty] at htm
    case hostCall role funcIdx args => cases role <;> simp [nodeTypedB, hk, hselfm, hty] at htm
    case selfCall tail funcIdx args => simp [recordComputeNodeOk, hk] at hokm
    case ifElse cond thenBlock elseBlock => simp [recordComputeNodeOk, hk] at hokm
    case vectorGetOrDefault arrTy toIndexIdx boxIdx default => simp [recordComputeNodeOk, hk] at hokm
    case structNew tyIdx args => simp [nodeTypedB, hk, hselfm, hty] at htm
    case intSignCmp op k scratch value => simp [nodeTypedB, hk, hselfm, hty] at htm
  show nodeInProfile (AverCert.PlanCheck.maxFuel) _ _ _ = true
  rw [show AverCert.PlanCheck.maxFuel = 9999 + 1 from rfl]
  cases hk : n.kind
  case «local» index => simp [nodeInProfile, hk]
  case constBool value => simp [recordComputeNodeOk, hk] at hok'
  case constI64 value =>
    simp only [recordComputeNodeOk, hk] at hok'
    simp only [nodeInProfile, hk, decide_eq_true_eq]
    exact (i64Band_iff value).mpr (by simpa [inI64Band] using hok')
  case constI32 value =>
    simp only [nodeInProfile, hk, decide_eq_true_eq]
    exact hi32 n hn value hk
  case constF64Bits bits => simp [recordComputeNodeOk, hk] at hok'
  case structGet field receiver => simp [recordComputeNodeOk, hk] at hok'
  case structGetUser tyIdx field value =>
    have hty : tyIdx = structIdx := by
      simp only [List.all_eq_true, List.mem_filterMap, beq_iff_eq] at hidx
      exact hidx tyIdx ⟨n, hn, by simp [fragNodeStructIdx?, hk]⟩
    subst hty
    simp only [nodeTypedB, hk, hself, Bool.and_eq_true, beq_iff_eq] at htyped'
    simp only [nodeInProfile, hk, hrec, htyped'.2, sortOfFragTy, beq_iff_eq]
    rw [List.getElem?_replicate]
    simp [(harity n hn).2 field value hk]
  case refIsNull value => simp [recordComputeNodeOk, hk] at hok'
  case prim op args =>
    cases op <;> simp [recordComputeNodeOk, hk] at hok' <;> simp [nodeInProfile, hk, primInProfile]
  case hostCall role funcIdx args =>
    have hrole : hostRoleIdx? ht role = some funcIdx ∧ args.length = roleArity role := by
      cases role <;> simp only [recordComputeNodeOk, hk, Bool.and_eq_true, beq_iff_eq] at hok' <;>
        first | exact ⟨hok'.1, hok'.2⟩ | simp at hok'
    obtain ⟨i, hslot, -⟩ := hostRoleIdx?_slotLookup C [.record structIdx fields] role ht funcIdx hnd hrole.1
    simp only [nodeInProfile, hk, hslot, beq_self_eq_true, Bool.and_self, Bool.true_and]
    cases role
    case box =>
      match args, hrole.2 with
      | [a], _ =>
        obtain ⟨m, hm, hmty⟩ := block_box_arg hchk n hn funcIdx a hk
        obtain ⟨v, hkv⟩ := hi64 m (List.mem_of_getElem? hm) hmty
        simp [hm, hkv]
    all_goals rfl
  case selfCall tail funcIdx args => simp [recordComputeNodeOk, hk] at hok'
  case ifElse cond thenBlock elseBlock => simp [recordComputeNodeOk, hk] at hok'
  case vectorGetOrDefault arrTy toIndexIdx boxIdx default => simp [recordComputeNodeOk, hk] at hok'
  case structNew tyIdx args =>
    have hty : tyIdx = structIdx := by
      simp only [List.all_eq_true, List.mem_filterMap, beq_iff_eq] at hidx
      exact hidx tyIdx ⟨n, hn, by simp [fragNodeStructIdx?, hk]⟩
    subst hty
    simp only [nodeTypedB, hk, hself, beq_self_eq_true, Bool.true_and, Bool.and_eq_true,
      List.all_eq_true, beq_iff_eq] at htyped'
    have hsorts : args.map (sortAt plan.body.nodes) = List.replicate fields.length .car := by
      rw [← (harity n hn).1 args hk, List.map_eq_replicate_iff]
      intro a ha
      obtain ⟨m, hm, hmty⟩ := tyOfPlan_arg (by decide) (htyped'.1 a ha)
      rw [sortAt_of_lookupNode hm, sortOfNode_of_ne m (by rw [hmty]; decide), hmty]
      rfl
    simp [nodeInProfile, hk, hrec, hsorts]
  case intSignCmp op k scratch value => simp [nodeInProfile, hk]

end Bridge
