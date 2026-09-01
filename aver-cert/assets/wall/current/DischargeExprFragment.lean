/-
Acceptance-soundness wiring for source expression fragments.

Acceptance pins the audited SymRawPlan encoder, checked representation plan,
canonical lowering, and exact code entry.  The independent obligation
domain/model face and the plan-evaluator result stay explicit, following the
established family-discharge pattern.  The bridge chooses the exact input
values: comparison fragments use `carrierSmall`, Bool fragments use `b32`,
and contracted integer fragments preserve arbitrary represented inputs.  For
partial host contracts, the successful byte run is exposed only to rule out a
missing host result; the audited generic still identifies the evaluator result
with the byte result.
-/
import AcceptanceSoundnessCore
import StandardFace

open AverCert
open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

namespace AcceptanceSoundness

/-- The semantic face not carried by `symFragmentPlanAccepted`.  It relates an
arbitrary obligation-domain representation to the generic theorem's honest
input values and pins the SymRawPlan-derived evaluator's result to the
obligation's independently declared model/codomain relation. -/
def exprFragmentSemanticBridge
    (claim : SymFragmentClaim) (plan : ExprFragmentRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  ∀ (S : CarrierSpec claim.obligation.carrier)
    (add sub mul stringEq : List WVal → Option WVal)
    (stringConcat : Nat → List WVal → Option WVal)
    (toIndex cmp eq : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (hMul : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      mul [va, vb] = some w → S.Repr (a * b) w)
    (hStringEq : ∀ a b w, stringEq [a, b] = some w →
      w = b32 (stringEqW a b))
    (hStringConcat : ∀ resultTy parts c,
      stringConcat resultTy [parts] = some c →
        stringConcatW resultTy parts = some c)
    (hToIndex : ∀ n v r, S.Repr n v → toIndex [v] = some r →
      r = .i32v (toIndexW n))
      (hCmp : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
        -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
        cmp [carrierSmall claim.obligation.carrier k1,
             carrierSmall claim.obligation.carrier k2] = some r →
          r = .i32v (cmpW k1 k2))
      (hEq : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
        -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
        eq [carrierSmall claim.obligation.carrier k1,
            carrierSmall claim.obligation.carrier k2] = some r →
          r = .i32v (eqW k1 k2))
    (fuel : Nat) (x : claim.obligation.Dom) (vs : List WVal) (w : WVal),
    claim.obligation.domRepr S x vs →
    wFuncN claim.obligation.code
      (claim.obligation.host add sub mul stringEq stringConcat toIndex cmp eq)
      (fuel + 1) claim.obligation.self vs = some w →
    ∃ (inputs : List WVal) (modelLocals : List WVal) (result : WVal),
      vs = inputs ∧
      inputs.length = plan.params.length ∧
      ExprFragmentSoundness.blockCallsOK
        (claim.obligation.host add sub mul stringEq stringConcat toIndex cmp eq)
        (fun g => (claim.obligation.code g).map (fun c => c.arity))
        plan.body ∧
      ExprFragmentSemantics.evalSymRawPlan
        claim.hostTable claim.structTable
        (claim.obligation.host add sub mul stringEq stringConcat toIndex cmp eq)
        (fun g => (claim.obligation.code g).map (fun c => c.arity))
        (fun g args => wFuncN claim.obligation.code
          (claim.obligation.host add sub mul stringEq stringConcat toIndex cmp eq) fuel g args)
        claim.obligation.carrier claim.plan
        (initLocals ⟨plan.params.length, exprFragmentNLocals plan, []⟩
          inputs) =
          some (.ok modelLocals [result]) ∧
      claim.obligation.codRepr S (claim.obligation.model x) result

/-- The audited expression-fragment generic currently owns exactly the
integer/Bool boundary admitted by the producer classifier. -/
def exprFragmentUsesAuditedGeneric (claim : SymFragmentClaim) : Bool :=
  claim.plan.params.all (fun ty => ty = .int || ty = .bool) &&
  (claim.plan.result = .int || claim.plan.result = .bool)

/-- Float semantics are deliberately outside the audited integer/Bool model. A
float at the source boundary is the only bespoke residual admitted below. -/
def exprFragmentHasFloatBoundary (claim : SymFragmentClaim) : Bool :=
  claim.plan.params.any (· = .float) || claim.plan.result = .float

/-- Projection-faced expression fragments are migrated, but their canonical
discharge lives in the audited field-projection wall. -/
def exprFragmentHasFieldProjection (claim : SymFragmentClaim) : Bool :=
  claim.plan.body.nodes.any (fun node =>
    match node.kind with
    | .projectField _ _ _ _ => true
    | _ => false)

/-- Tag-dispatch fragments (Option/Result `match` returning an Int constant)
also discharge through the symbolic generic — their operational model is a
conditional over boxed constants — but their source scrutinee is an ADT, so
they are outside the int/Bool `exprFragmentUsesAuditedGeneric` gate. The gate
here is the encoded representation shape: an `adtRef` scrutinee, an `intCarrier`
result, and a `struct.get.user` tag read. -/
def exprFragmentIsTagDispatch (claim : SymFragmentClaim) : Bool :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some plan =>
      plan.params = [.adtRef] && plan.result = .intCarrier &&
      plan.body.nodes.any (fun node =>
        match node.kind with
        | .structGetUser _ _ _ => true
        | _ => false)
  | none => false

/-- Fused vector-read fragments (`Option.withDefault(Vector.get(p0, p1), d)`)
discharge through the audited template theorem
(`StandardFace.vectorGetOrDefault_simulates_model`), not through the symbolic
generic: their operational content is the monolithic bounds-checked template,
whose semantics the interpreter clauses prove once, generically over every
hole. The gate is the encoded representation shape: exactly the single
monolithic node. -/
def exprFragmentIsVectorGetOrDefault (claim : SymFragmentClaim) : Bool :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some plan =>
      match AverCert.WasmSlice.exprVectorGetOrDefaultArrTy? plan with
      | some _ => true
      | none => false
  | none => false

/-- Record-parameter field-read fragments (`isMember(p) = p.isMember`) carry
no producer semantic premise at all: the checked record face
(`StandardFace.recordParamDeclaredFace`, a conjunct of `checkedFaces`) pins the
type-section entry by equality against the wall lowering of the Plan record
declaration, and the discharge below derives the obligation from
`recordParam_simulates_model` plus byte acceptance. The gate is the encoded
representation shape: exactly the recognized two-node scalar field read. -/
def exprFragmentIsRecordParam (claim : SymFragmentClaim) : Bool :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some plan => (AverCert.WasmSlice.exprRecordProjFace? plan).isSome
  | none => false

/-- Int value-versus-value comparison fragments (`a >= b`, `a == b`) carry NO
producer semantic premise: the checked face (`StandardFace.intCmpBoolFace`, a
conjunct of `checkedFaces`) pins the obligation's whole meaning — domain,
codomain, both representation relations, the single host slot AND the model —
to wall terms over the recognized shape, and the discharge below derives the
obligation from `StandardFace.intCmp_simulates_model` plus byte acceptance. The
gate is the encoded representation shape: exactly the pinned comparison nodes. -/
def exprFragmentIsIntCmpBool (claim : SymFragmentClaim) : Bool :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some plan => (AverCert.StandardFace.classifyIntCmpBool plan).isSome
  | none => false

/-- Int selection fragments (`match a < b { true -> a; false -> b }`), the same
way. Their result is a passthrough of an input local, so the codomain relation
is discharged by the chosen argument's own representation premise. -/
def exprFragmentIsIntSelect (claim : SymFragmentClaim) : Bool :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some plan => (AverCert.StandardFace.classifyIntSelect plan).isSome
  | none => false

/-- Routing marker for the record projection-compute face: the encoded plan
    exists and the compute classifier fires on it. Like the record-parameter
    arm, it contributes NO semantic premise — the discharge derives the
    obligation from the checked declared face. -/
def exprFragmentIsRecordCompute (claim : SymFragmentClaim) : Bool :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some plan =>
      (AverCert.StandardFace.classifyRecordCompute claim.hostTable plan).isSome
  | none => false

/-- Side condition for one source expression claim.  In-model claims must use
the symbolic generic. Projection claims may use the audited projection
generic. Fused vector-read claims discharge through the audited template
theorem. Only float-boundary claims may use a bespoke direct discharge.
Record-parameter claims contribute NO semantic premise: the arm only routes,
and the discharge derives their obligation from the checked record face. The two
Int comparison arms are the same shape — the face pins their model too, so the
only thing left to state is the partial-correctness policy the family runs
under. -/
def exprFragmentSideCondition (claim : SymFragmentClaim) : Prop :=
  (exprFragmentUsesAuditedGeneric claim = true ∧
    ∀ plan,
      AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
          claim.hostTable claim.structTable claim.plan = some plan →
        exprFragmentSemanticBridge claim plan) ∨
  (exprFragmentIsTagDispatch claim = true ∧
    ∀ plan,
      AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
          claim.hostTable claim.structTable claim.plan = some plan →
        exprFragmentSemanticBridge claim plan) ∨
  (exprFragmentIsVectorGetOrDefault claim = true ∧
    obligationHolds claim.obligation) ∨
  (exprFragmentHasFieldProjection claim = true ∧
    obligationHolds claim.obligation) ∨
  (exprFragmentHasFloatBoundary claim = true ∧
    obligationHolds claim.obligation) ∨
  (exprFragmentIsRecordParam claim = true) ∨
  (exprFragmentIsIntCmpBool claim = true ∧
    claim.obligation.policy = .simulatesModel) ∨
  (exprFragmentIsIntSelect claim = true ∧
    claim.obligation.policy = .simulatesModel) ∨
  (exprFragmentIsRecordCompute claim = true)

def exprFragmentSemanticBridges (artifact : ArtifactData) : Prop :=
  ∀ claim ∈ artifact.symFragmentClaims, exprFragmentSideCondition claim

theorem exprFragment_claim_discharges_generic
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (claim : SymFragmentClaim)
    (hMem : claim ∈ artifact.symFragmentClaims)
    (hBridge : ∀ plan,
      AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
          claim.hostTable claim.structTable claim.plan = some plan →
        exprFragmentSemanticBridge claim plan) :
    obligationHolds claim.obligation := by
  have hClaim : symFragmentClaimAccepted artifact.modBytes artifact.modLen claim :=
    allClaims_of_mem
      (symFragmentClaimAccepted artifact.modBytes artifact.modLen)
      artifact.symFragmentClaims hAcc claim hMem
  unfold symFragmentClaimAccepted symFragmentPlanAccepted at hClaim
  cases hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | none => simp [hEncode] at hClaim
  | some plan =>
      have hAccepted : symFragmentCarrierBound
            artifact.modBytes artifact.modLen claim.carrier claim.hostTable
            plan = true ∧
          AverCert.WasmSlice.hostTableFuncTypesMatch
            artifact.modBytes artifact.modLen claim.carrier
            claim.hostTable = true ∧
          exprFragmentPlanAccepted
            artifact.modBytes artifact.modLen claim.exportNameBytes claim.exportName
            claim.carrier plan claim.obligation := by
        simpa [hEncode] using hClaim
      have hAccepted := hAccepted.2.2
      rcases hAccepted with
        ⟨_hExport, hCarrier, body, codeEntry, binding, hPlanAccepted,
          _hFuncType, _hNominalTypes, hSelf, hCode⟩
      rcases hPlanAccepted with
        ⟨hCheck, hLowerExpr, _hCodeEntry, _hExactBinding⟩
      rcases hBridge plan hEncode with ⟨hPolicy, hSemantic⟩
      have hLower : AverCert.PlanLower.lowerBlock
          claim.obligation.carrier plan.body = some body := by
        simp only [AverCert.PlanLower.lowerExprFragmentBody, hCheck,
          if_true] at hLowerExpr
        simpa [hCarrier] using hLowerExpr
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨plan.params.length, exprFragmentNLocals plan, body⟩ := by
        simpa [← hSelf] using hCode
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat toIndex cmp eq
        hAdd hSub hMul hStringEq hStringConcat _hToIndex _hCmp _hEq fuel x vs w hDom hRun
      cases fuel with
      | zero => simp [wFuncN] at hRun
      | succ fuel =>
          rcases hSemantic S add sub mul stringEq stringConcat toIndex cmp eq
              hAdd hSub hMul hStringEq hStringConcat _hToIndex _hCmp _hEq fuel x vs w hDom hRun with
            ⟨inputs, modelLocals, result, rfl, hArity, hCalls, hEval, hCod⟩
          have hGeneric := ExprFragmentSoundness.exprfragment_generic_certified
            S claim.hostTable claim.structTable claim.obligation.code
            (claim.obligation.host add sub mul stringEq stringConcat toIndex cmp eq)
            claim.plan plan hEncode hCheck body hLower claim.obligation.self
            (exprFragmentNLocals plan) fuel hCodeSelf vs hArity hCalls
            modelLocals result hEval
          rw [hGeneric] at hRun
          have hResult : result = w := Option.some.inj hRun
          simpa [hResult] using hCod

/-- Face-derived discharge of one record-parameter claim, mirroring the
declared-index envelope columns: the checked record face
(`StandardFace.recordParamDeclaredFace`) supplies the equality-pinned Plan
declaration and the `HEq` meaning pins, byte acceptance supplies the exact
canonical body at the obligation's own code/self, and
`recordParam_simulates_model` (through `recordParam_transport`) closes the run.
No producer semantic premise participates. -/
theorem recordParam_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (claim : SymFragmentClaim)
    (hMem : claim ∈ artifact.symFragmentClaims)
    (hFace : AverCert.StandardFace.symFragmentMatches
      artifact.modBytes artifact.modLen artifact.manifest.subject.hostRoles claim)
    (hIs : exprFragmentIsRecordParam claim = true) :
    obligationHolds claim.obligation := by
  have hClaim : symFragmentClaimAccepted artifact.modBytes artifact.modLen claim :=
    allClaims_of_mem
      (symFragmentClaimAccepted artifact.modBytes artifact.modLen)
      artifact.symFragmentClaims hAcc claim hMem
  unfold symFragmentClaimAccepted symFragmentPlanAccepted at hClaim
  unfold exprFragmentIsRecordParam at hIs
  cases hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | none => simp [hEncode] at hIs
  | some plan =>
      simp only [hEncode, Option.isSome_iff_exists] at hIs
      obtain ⟨⟨structIdx, field⟩, hRec⟩ := hIs
      have hNone := AverCert.StandardFace.symFragmentFace_none_of_recordProj
        claim plan hEncode structIdx field hRec
      unfold AverCert.StandardFace.symFragmentMatches at hFace
      obtain ⟨-, hMatch⟩ := hFace
      simp only [hNone, hEncode, hRec] at hMatch
      have hAccepted : symFragmentCarrierBound artifact.modBytes artifact.modLen
            claim.carrier claim.hostTable plan = true ∧
          AverCert.WasmSlice.hostTableFuncTypesMatch artifact.modBytes artifact.modLen
            claim.carrier claim.hostTable = true ∧
          exprFragmentPlanAccepted artifact.modBytes artifact.modLen
            claim.exportNameBytes claim.exportName claim.carrier plan
            claim.obligation := by
        simpa [hEncode] using hClaim
      obtain ⟨-, -, hExpr⟩ := hAccepted
      obtain ⟨-, hCarrier, body, codeEntry, binding, hByteAccepted, -, -,
        hSelf, hCode⟩ := hExpr
      obtain ⟨hCheck, hLowerExpr, -, -⟩ := hByteAccepted
      obtain ⟨hparams, -, hplanBody⟩ :=
        AverCert.WasmSlice.exprRecordProjFace?_spec plan structIdx field hRec
      have hLower : AverCert.PlanLower.lowerBlock claim.carrier plan.body
          = some body := by
        simp only [AverCert.PlanLower.lowerExprFragmentBody, hCheck, if_true]
          at hLowerExpr
        exact hLowerExpr
      have hBody : body = recordProjTemplate structIdx field := by
        rw [hplanBody, AverCert.StandardFace.lowerBlock_recordProj] at hLower
        exact (Option.some.inj hLower).symm
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨1, exprFragmentNLocals plan, recordProjTemplate structIdx field⟩ := by
        rw [hSelf, hCode, hBody, hparams]
        rfl
      obtain ⟨hPolicy, -, hCarrierEq, decl, structIdx', field', fields, hfield,
        hdecl, hRec', -, -, -, -, -, hDomP, hCodP, hdomReprP, hcodReprP,
        hmodelP⟩ := hMatch
      rw [hRec] at hRec'
      injection hRec' with hpair
      injection hpair with hsi hfi
      subst hsi
      subst hfi
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat toIndex cmp eq
        _hAdd _hSub _hMul _hStringEq _hStringConcat _hToIndex _hCmp _hEq fuel x vs w hDom hRun
      exact AverCert.StandardFace.recordParam_transport claim.carrier fields
        structIdx field hfield claim.obligation.carrier claim.obligation.Dom
        claim.obligation.Cod claim.obligation.domRepr claim.obligation.codRepr
        claim.obligation.model hCarrierEq hDomP hCodP hdomReprP hcodReprP hmodelP
        claim.obligation.code
        (claim.obligation.host add sub mul stringEq stringConcat toIndex cmp eq)
        claim.obligation.self (exprFragmentNLocals plan) hCodeSelf
        S fuel x vs w hDom hRun

/-- Face-derived discharge of one Int-comparison claim, mirroring the
record-parameter column: the checked face supplies the `HEq` meaning pins and
the host-slot equality, byte acceptance supplies the exact canonical body at the
obligation's own code/self, and `StandardFace.intCmp_simulates_model` (through
`intCmp_transport`) closes the run under any helper obeying the `__aint_cmp` /
`__aint_eq` contract. No producer semantic premise participates. -/
theorem intCmpBool_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (claim : SymFragmentClaim)
    (hMem : claim ∈ artifact.symFragmentClaims)
    (hFace : AverCert.StandardFace.symFragmentMatches
      artifact.modBytes artifact.modLen artifact.manifest.subject.hostRoles claim)
    (hIs : exprFragmentIsIntCmpBool claim = true)
    (hPolicy : claim.obligation.policy = .simulatesModel) :
    obligationHolds claim.obligation := by
  have hClaim : symFragmentClaimAccepted artifact.modBytes artifact.modLen claim :=
    allClaims_of_mem
      (symFragmentClaimAccepted artifact.modBytes artifact.modLen)
      artifact.symFragmentClaims hAcc claim hMem
  unfold symFragmentClaimAccepted symFragmentPlanAccepted at hClaim
  unfold exprFragmentIsIntCmpBool at hIs
  cases hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | none => simp [hEncode] at hIs
  | some plan =>
      simp only [hEncode, Option.isSome_iff_exists] at hIs
      obtain ⟨face, hCls⟩ := hIs
      obtain ⟨hparams, -, hbody⟩ :=
        AverCert.StandardFace.classifyIntCmpBool_spec plan face hCls
      have hAccepted : symFragmentCarrierBound artifact.modBytes artifact.modLen
            claim.carrier claim.hostTable plan = true ∧
          AverCert.WasmSlice.hostTableFuncTypesMatch artifact.modBytes artifact.modLen
            claim.carrier claim.hostTable = true ∧
          exprFragmentPlanAccepted artifact.modBytes artifact.modLen
            claim.exportNameBytes claim.exportName claim.carrier plan
            claim.obligation := by
        simpa [hEncode] using hClaim
      obtain ⟨-, -, hExpr⟩ := hAccepted
      obtain ⟨-, -, body, codeEntry, binding, hByteAccepted, -, -, hSelf, hCode⟩ := hExpr
      obtain ⟨hCheck, hLowerExpr, -, -⟩ := hByteAccepted
      have hLower : AverCert.PlanLower.lowerBlock claim.carrier plan.body
          = some body := by
        simp only [AverCert.PlanLower.lowerExprFragmentBody, hCheck, if_true]
          at hLowerExpr
        exact hLowerExpr
      have hBody : body =
          AverCert.StandardFace.intCmpTemplate face.op face.helperIdx := by
        rw [hbody, AverCert.StandardFace.lowerBlock_intCmp] at hLower
        exact (Option.some.inj hLower).symm
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨2, exprFragmentNLocals plan,
            AverCert.StandardFace.intCmpTemplate face.op face.helperIdx⟩ := by
        rw [hSelf, hCode, hBody, hparams]
        rfl
      have hFaceSel := AverCert.StandardFace.symFragmentFace_intCmpBool
        claim plan face hEncode hCls
      unfold AverCert.StandardFace.symFragmentMatches at hFace
      obtain ⟨-, hMatch⟩ := hFace
      simp only [hFaceSel] at hMatch
      have hM : claim.obligation.carrier = claim.carrier ∧
          HEq claim.obligation.Dom (Int × Int) ∧
          HEq claim.obligation.Cod Bool ∧
          HEq claim.obligation.domRepr
            (AverCert.StandardFace.intPairSmallBandDomRepr claim.carrier) ∧
          HEq claim.obligation.codRepr (boolRepr (C := claim.carrier)) ∧
          claim.obligation.host = AverCert.StandardFace.intCmpHost face ∧
          HEq claim.obligation.model
            (AverCert.StandardFace.intCmpModel face.op) := hMatch
      obtain ⟨hcar, hDomT, hCodT, hdomReprT, hcodReprT, hhost, hmodelT⟩ := hM
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat toIndex cmp eq
        _hAdd _hSub _hMul _hStringEq _hStringConcat _hToIndex hCmp hEq fuel x vs w
        hDom hRun
      rw [hhost] at hRun
      exact AverCert.StandardFace.intCmp_transport claim.carrier face.helperIdx
        face.op claim.obligation.carrier claim.obligation.Dom claim.obligation.Cod
        claim.obligation.domRepr claim.obligation.codRepr claim.obligation.model
        hcar hDomT hCodT hdomReprT hcodReprT hmodelT S cmp eq hCmp hEq
        claim.obligation.code claim.obligation.self (exprFragmentNLocals plan)
        hCodeSelf fuel x vs w hDom hRun

/-- The same column for the Int selection shape. -/
theorem intSelect_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (claim : SymFragmentClaim)
    (hMem : claim ∈ artifact.symFragmentClaims)
    (hFace : AverCert.StandardFace.symFragmentMatches
      artifact.modBytes artifact.modLen artifact.manifest.subject.hostRoles claim)
    (hIs : exprFragmentIsIntSelect claim = true)
    (hPolicy : claim.obligation.policy = .simulatesModel) :
    obligationHolds claim.obligation := by
  have hClaim : symFragmentClaimAccepted artifact.modBytes artifact.modLen claim :=
    allClaims_of_mem
      (symFragmentClaimAccepted artifact.modBytes artifact.modLen)
      artifact.symFragmentClaims hAcc claim hMem
  unfold symFragmentClaimAccepted symFragmentPlanAccepted at hClaim
  unfold exprFragmentIsIntSelect at hIs
  cases hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | none => simp [hEncode] at hIs
  | some plan =>
      simp only [hEncode, Option.isSome_iff_exists] at hIs
      obtain ⟨face, hCls⟩ := hIs
      obtain ⟨hparams, -, hbody⟩ :=
        AverCert.StandardFace.classifyIntSelect_spec plan face hCls
      have hAccepted : symFragmentCarrierBound artifact.modBytes artifact.modLen
            claim.carrier claim.hostTable plan = true ∧
          AverCert.WasmSlice.hostTableFuncTypesMatch artifact.modBytes artifact.modLen
            claim.carrier claim.hostTable = true ∧
          exprFragmentPlanAccepted artifact.modBytes artifact.modLen
            claim.exportNameBytes claim.exportName claim.carrier plan
            claim.obligation := by
        simpa [hEncode] using hClaim
      obtain ⟨-, -, hExpr⟩ := hAccepted
      obtain ⟨-, -, body, codeEntry, binding, hByteAccepted, -, -, hSelf, hCode⟩ := hExpr
      obtain ⟨hCheck, hLowerExpr, -, -⟩ := hByteAccepted
      have hLower : AverCert.PlanLower.lowerBlock claim.carrier plan.body
          = some body := by
        simp only [AverCert.PlanLower.lowerExprFragmentBody, hCheck, if_true]
          at hLowerExpr
        exact hLowerExpr
      have hBody : body =
          AverCert.StandardFace.intSelectTemplate face.op face.helperIdx := by
        rw [hbody, AverCert.StandardFace.lowerBlock_intSelect] at hLower
        exact (Option.some.inj hLower).symm
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨2, exprFragmentNLocals plan,
            AverCert.StandardFace.intSelectTemplate face.op face.helperIdx⟩ := by
        rw [hSelf, hCode, hBody, hparams]
        rfl
      have hFaceSel := AverCert.StandardFace.symFragmentFace_intSelect
        claim plan face hEncode hCls
      unfold AverCert.StandardFace.symFragmentMatches at hFace
      obtain ⟨-, hMatch⟩ := hFace
      simp only [hFaceSel] at hMatch
      have hM : claim.obligation.carrier = claim.carrier ∧
          HEq claim.obligation.Dom (Int × Int) ∧
          HEq claim.obligation.Cod Int ∧
          HEq claim.obligation.domRepr
            (AverCert.StandardFace.intPairSmallBandDomRepr claim.carrier) ∧
          HEq claim.obligation.codRepr (intRepr (C := claim.carrier)) ∧
          claim.obligation.host = AverCert.StandardFace.intCmpHost face ∧
          HEq claim.obligation.model
            (AverCert.StandardFace.intSelectModel face.op) := hMatch
      obtain ⟨hcar, hDomT, hCodT, hdomReprT, hcodReprT, hhost, hmodelT⟩ := hM
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat toIndex cmp eq
        _hAdd _hSub _hMul _hStringEq _hStringConcat _hToIndex hCmp hEq fuel x vs w
        hDom hRun
      rw [hhost] at hRun
      exact AverCert.StandardFace.intSelect_transport claim.carrier face.helperIdx
        face.op claim.obligation.carrier claim.obligation.Dom claim.obligation.Cod
        claim.obligation.domRepr claim.obligation.codRepr claim.obligation.model
        hcar hDomT hCodT hdomReprT hcodReprT hmodelT S cmp eq hCmp hEq
        claim.obligation.code claim.obligation.self (exprFragmentNLocals plan)
        hCodeSelf fuel x vs w hDom hRun

/-! ### Record projection-compute discharge helpers

The wasm entry runs with `initLocals`' one-slot `.null` scratch pad appended
to the arguments, while the bridge's agreement corollary speaks about a
locals list pointwise `SRepr`-related to the source parameters — and no
source value represents `.null`. The admitted v1 node set never writes a
local and, on a typed plan, never reads past the parameter prefix, so the
pad is observationally inert: the "unpad" lemmas below erase it from a
successful plan-walker run. The remaining helpers convert classifier facts
into the bridge's admission/typing hypotheses and transport the face's
`HEq` pins onto the obligation fields, like `recordParam_transport`. -/

section RecordComputeDischarge

open ExprFragmentSemantics AverCert.PlanLower RecordComputeBridge

private theorem sreprAll_len {Repr : Int → WVal → Prop} {structIdx : Nat} :
    ∀ {ss : List RecordComputeBridge.SVal} {ws : List WVal},
      SReprAll Repr structIdx ss ws → ss.length = ws.length := by
  intro ss ws h
  induction h with
  | nil => rfl
  | cons _ _ ih => simp [ih]

private theorem planTyped_mem {structIdx : Nat} {tyOf : Nat → FragTy}
    {params : List FragTy} :
    ∀ {nodes : List FragNode}, planTyped structIdx tyOf params nodes →
      ∀ n ∈ nodes, nodeTyped structIdx tyOf params n := by
  intro nodes
  induction nodes with
  | nil => intro _ n hn; simp at hn
  | cons head tail ih =>
      intro h n hn
      rcases List.mem_cons.mp hn with rfl | htail
      · exact h.1
      · exact ih h.2 n htail

/-- The compute face's executable node admission implies the bridge's
    table-keyed admission, kind by kind. -/
private theorem recordComputeNodeOk_admits
    (hostTable : List (HostRole × Nat)) (kind : FragNodeKind)
    (h : AverCert.StandardFace.recordComputeNodeOk hostTable kind = true) :
    nodeAdmitted hostTable kind = true := by
  cases kind
  case hostCall role f args =>
    cases role <;>
      simp_all [AverCert.StandardFace.recordComputeNodeOk,
        RecordComputeBridge.nodeAdmitted]
  all_goals
    simp_all [AverCert.StandardFace.recordComputeNodeOk,
      RecordComputeBridge.nodeAdmitted]

/-- A fired compute classifier evaluated the bridge's Bool typing face at the
    pinned struct index — extracted by the same `split at` walk as
    `classifyRecordCompute_spec`. -/
private theorem classifyRecordCompute_typed
    (hostTable : List (HostRole × Nat)) (plan : ExprFragmentRawPlan)
    (face : AverCert.StandardFace.RecordComputeFace)
    (h : AverCert.StandardFace.classifyRecordCompute hostTable plan
      = some face) :
    planTypedB face.structIdx
      (fun nodeId => ((plan.body.nodes[nodeId]?).map (fun n => n.ty)).getD .i64)
      plan.params plan.body.nodes = true := by
  simp only [AverCert.StandardFace.classifyRecordCompute] at h
  split at h
  case isFalse => exact absurd h (by simp)
  case isTrue =>
    split at h
    case h_1 => exact absurd h (by simp)
    case h_2 i rest heq =>
      split at h
      case isFalse => exact absurd h (by simp)
      case isTrue hcond =>
        have hface : face = ⟨i⟩ := (Option.some.inj h).symm
        subst hface
        exact ((Bool.and_eq_true _ _).mp hcond).2

/-- A `local.get` below the parameter count reads the same value with and
    without the scratch pad, leaving the locals untouched. -/
private theorem wRunF_localGet_pad
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (base ext stack : List WVal) (index : Nat) (hlt : index < base.length) :
    ∃ v, base[index]? = some v ∧
      wRunF host ar callee [.localGet index] (base ++ ext) stack
        = some (.ok (base ++ ext) (v :: stack)) ∧
      wRunF host ar callee [.localGet index] base stack
        = some (.ok base (v :: stack)) := by
  refine ⟨base[index], List.getElem?_eq_getElem hlt, ?_, ?_⟩
  · simp [wRunF, List.getElem?_append_left hlt, List.getElem?_eq_getElem hlt]
  · simp [wRunF, List.getElem?_eq_getElem hlt]

/-- The other single instructions the admitted node set emits neither read
    nor write locals: with the same operand stack they either fail on both
    locals lists or succeed on both with the same result stack. -/
private theorem wRunF_single_pad
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (base ext stack : List WVal) (instr : WInstr)
    (hi : match instr with
      | .i64Const _ => True
      | .structGet _ _ => True
      | .structNew _ _ => True
      | .call _ => True
      | _ => False) :
    (wRunF host ar callee [instr] (base ++ ext) stack = none ∧
      wRunF host ar callee [instr] base stack = none) ∨
    (∃ st', wRunF host ar callee [instr] (base ++ ext) stack
        = some (.ok (base ++ ext) st') ∧
      wRunF host ar callee [instr] base stack = some (.ok base st')) := by
  cases instr
  case i64Const k =>
    exact Or.inr ⟨.i64v k :: stack, by simp [wRunF], by simp [wRunF]⟩
  case structGet ty field =>
    cases stack with
    | nil => exact Or.inl ⟨by simp [wRunF], by simp [wRunF]⟩
    | cons v st =>
        cases v
        case structv t fs =>
          by_cases ht : t = ty
          · subst ht
            cases hf : fs[field]? with
            | none => exact Or.inl ⟨by simp [wRunF, hf], by simp [wRunF, hf]⟩
            | some fv =>
                exact Or.inr ⟨fv :: st, by simp [wRunF, hf], by simp [wRunF, hf]⟩
          · exact Or.inl ⟨by simp [wRunF, ht], by simp [wRunF, ht]⟩
        all_goals exact Or.inl ⟨by simp [wRunF], by simp [wRunF]⟩
  case structNew ty nf =>
    cases hpa : popArgs nf stack with
    | none => exact Or.inl ⟨by simp [wRunF, hpa], by simp [wRunF, hpa]⟩
    | some p =>
        exact Or.inr ⟨.structv ty p.1 :: p.2,
          by simp [wRunF, hpa], by simp [wRunF, hpa]⟩
  case call fn =>
    cases hh : host fn with
    | some p =>
        cases hpa : popArgs p.1 stack with
        | none =>
            exact Or.inl ⟨by simp [wRunF, hh, hpa], by simp [wRunF, hh, hpa]⟩
        | some q =>
            cases hr : p.2 q.1 with
            | none =>
                exact Or.inl ⟨by simp [wRunF, hh, hpa, hr],
                  by simp [wRunF, hh, hpa, hr]⟩
            | some r =>
                exact Or.inr ⟨r :: q.2, by simp [wRunF, hh, hpa, hr],
                  by simp [wRunF, hh, hpa, hr]⟩
    | none =>
        cases ha : ar fn with
        | none =>
            exact Or.inl ⟨by simp [wRunF, hh, ha], by simp [wRunF, hh, ha]⟩
        | some a =>
            cases hpa : popArgs a stack with
            | none =>
                exact Or.inl ⟨by simp [wRunF, hh, ha, hpa],
                  by simp [wRunF, hh, ha, hpa]⟩
            | some q =>
                cases hc : callee fn q.1 with
                | none =>
                    exact Or.inl ⟨by simp [wRunF, hh, ha, hpa, hc],
                      by simp [wRunF, hh, ha, hpa, hc]⟩
                | some r =>
                    exact Or.inr ⟨r :: q.2, by simp [wRunF, hh, ha, hpa, hc],
                      by simp [wRunF, hh, ha, hpa, hc]⟩
  all_goals cases hi

/-- Pad erasure for the plan walker over admitted, prefix-reading nodes: a
    successful run with the scratch pad appended never touches it, so the
    same run succeeds on the bare parameter locals — and the admitted node
    set never returns early, so the result is always an `.ok` stack. -/
private theorem runNodesFuel_unpad
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostTable : List (HostRole × Nat)) (carrier : Nat)
    (base ext : List WVal) :
    ∀ (fuel : Nat) (nodes : List FragNode) (symStack : List Nat)
      (stack : List WVal) (out : Out),
      nodesAdmitted hostTable nodes = true →
      (∀ n ∈ nodes, ∀ index, n.kind = FragNodeKind.local index →
        index < base.length) →
      runNodesFuel host ar callee fuel carrier nodes symStack (base ++ ext)
        stack = some out →
      ∃ st', out = .ok (base ++ ext) st' ∧
        runNodesFuel host ar callee fuel carrier nodes symStack base stack
          = some (.ok base st') := by
  intro fuel
  induction fuel with
  | zero =>
      intro nodes symStack stack out _ _ hrun
      simp [runNodesFuel] at hrun
  | succ fuel ih =>
      intro nodes symStack stack out hAdm hIdx hrun
      cases nodes with
      | nil =>
          simp only [runNodesFuel, Option.some.injEq] at hrun
          exact ⟨stack, hrun.symm, by simp [runNodesFuel]⟩
      | cons node rest =>
          have hAdmPair : nodeAdmitted hostTable node.kind = true ∧
              nodesAdmitted hostTable rest = true := by
            simpa [nodesAdmitted, List.all_cons] using hAdm
          obtain ⟨hAdmN, hAdmR⟩ := hAdmPair
          have hIdxR : ∀ n ∈ rest, ∀ index,
              n.kind = FragNodeKind.local index → index < base.length :=
            fun n hn => hIdx n (List.mem_cons_of_mem _ hn)
          cases hk : node.kind
          case «local» index =>
            have hlt : index < base.length :=
              hIdx node List.mem_cons_self index hk
            obtain ⟨v, hv, hp1, hp2⟩ :=
              wRunF_localGet_pad host ar callee base ext stack index hlt
            simp only [runNodesFuel, hk, hp1] at hrun
            simp only [runNodesFuel, hk, hp2]
            exact ih rest (node.id :: symStack) (v :: stack) out hAdmR hIdxR hrun
          case constI64 value =>
            rcases wRunF_single_pad host ar callee base ext stack
                (.i64Const value) trivial with ⟨hp1, hp2⟩ | ⟨st', hp1, hp2⟩
            · simp [runNodesFuel, hk, hp1] at hrun
            · simp only [runNodesFuel, hk, hp1] at hrun
              simp only [runNodesFuel, hk, hp2]
              exact ih rest (node.id :: symStack) st' out hAdmR hIdxR hrun
          case structGetUser tyIdx field value =>
            simp only [runNodesFuel, hk] at hrun ⊢
            cases hpop : popExpected symStack value with
            | none => simp [hpop] at hrun
            | some symRest =>
                simp only [hpop] at hrun ⊢
                rcases wRunF_single_pad host ar callee base ext stack
                    (.structGet tyIdx field) trivial with
                  ⟨hp1, hp2⟩ | ⟨st', hp1, hp2⟩
                · simp [hp1] at hrun
                · simp only [hp1] at hrun
                  simp only [hp2]
                  exact ih rest (node.id :: symRest) st' out hAdmR hIdxR hrun
          case structNew tyIdx args =>
            simp only [runNodesFuel, hk] at hrun ⊢
            cases hpop : popExpectedAll symStack args.reverse with
            | none => simp [hpop] at hrun
            | some symRest =>
                simp only [hpop] at hrun ⊢
                rcases wRunF_single_pad host ar callee base ext stack
                    (.structNew tyIdx args.length) trivial with
                  ⟨hp1, hp2⟩ | ⟨st', hp1, hp2⟩
                · simp [hp1] at hrun
                · simp only [hp1] at hrun
                  simp only [hp2]
                  exact ih rest (node.id :: symRest) st' out hAdmR hIdxR hrun
          case hostCall role funcIdx args =>
            simp only [runNodesFuel, hk] at hrun ⊢
            cases hpop : popExpectedAll symStack args.reverse with
            | none => simp [hpop] at hrun
            | some symRest =>
                simp only [hpop] at hrun ⊢
                rcases wRunF_single_pad host ar callee base ext stack
                    (.call funcIdx) trivial with
                  ⟨hp1, hp2⟩ | ⟨st', hp1, hp2⟩
                · simp [hp1] at hrun
                · simp only [hp1] at hrun
                  simp only [hp2]
                  exact ih rest (node.id :: symRest) st' out hAdmR hIdxR hrun
          case constBool value => simp [nodeAdmitted, hk] at hAdmN
          case constI32 value => simp [nodeAdmitted, hk] at hAdmN
          case constF64Bits bits => simp [nodeAdmitted, hk] at hAdmN
          case structGet field receiver => simp [nodeAdmitted, hk] at hAdmN
          case refIsNull value => simp [nodeAdmitted, hk] at hAdmN
          case prim op args => simp [nodeAdmitted, hk] at hAdmN
          case selfCall tail funcIdx args => simp [nodeAdmitted, hk] at hAdmN
          case ifElse cond thenBlock elseBlock =>
            simp [nodeAdmitted, hk] at hAdmN
          case vectorGetOrDefault arrTy toIndexIdx boxIdx default =>
            simp [nodeAdmitted, hk] at hAdmN

/-- Block-level pad erasure: a successful padded block run is an `.ok` on a
    single value (the admitted node set never returns early), and the same
    block run succeeds on the bare parameter locals. -/
private theorem runBlockFuel_unpad
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (hostTable : List (HostRole × Nat)) (carrier fuel : Nat)
    (block : FragBlock) (base ext : List WVal) (out : Out)
    (hAdm : nodesAdmitted hostTable block.nodes = true)
    (hIdx : ∀ n ∈ block.nodes, ∀ index,
      n.kind = FragNodeKind.local index → index < base.length)
    (hrun : runBlockFuel host ar callee fuel carrier block (base ++ ext)
      = some out) :
    ∃ v, out = .ok (base ++ ext) [v] ∧
      runBlockFuel host ar callee fuel carrier block base
        = some (.ok base [v]) := by
  cases fuel with
  | zero => simp [runBlockFuel] at hrun
  | succ fuel =>
      simp only [runBlockFuel] at hrun ⊢
      cases hr : runNodesFuel host ar callee fuel carrier block.nodes []
          (base ++ ext) [] with
      | none => simp [hr] at hrun
      | some out0 =>
          obtain ⟨st', rfl, hr'⟩ := runNodesFuel_unpad host ar callee hostTable
            carrier base ext fuel block.nodes [] [] out0 hAdm hIdx hr
          rw [hr] at hrun
          rw [hr']
          cases st' with
          | nil => simp at hrun
          | cons v tail =>
              cases tail with
              | nil =>
                  have hout : Out.ok (base ++ ext) [v] = out := by
                    simpa using hrun
                  exact ⟨v, hout.symm, by simp⟩
              | cons w tail' => simp at hrun

/-- The compute face's template-implies-model core, at the face's concrete
    types: a successful `wFuncN` run of the canonically lowered body under
    the compute-face host slots yields a word representing the source
    evaluator's result — instruction-run success gives plan-walker success
    (`runBlock_complete`), the pad is erased, and the lockstep agreement
    (`sourceRunBlock_agrees`) lands on the model's value. -/
private theorem recordCompute_simulates_model
    (carrier structIdx : Nat) (hostTable : List (HostRole × Nat))
    (plan : ExprFragmentRawPlan) (body : List WInstr)
    (hDistinct : AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true)
    (hAdm : nodesAdmitted hostTable plan.body.nodes = true)
    (hTyB : planTypedB structIdx
      (fun nodeId => ((plan.body.nodes[nodeId]?).map (fun n => n.ty)).getD .i64)
      plan.params plan.body.nodes = true)
    (hLower : AverCert.PlanLower.lowerBlock carrier plan.body = some body)
    (code : CodeTbl) (self : Nat)
    (hCode : code self = some ⟨plan.params.length, 1, body⟩)
    (S : CarrierSpec carrier)
    (add sub mul eq : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (hMul : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      mul [va, vb] = some w → S.Repr (a * b) w)
    (fuel : Nat) (x : List RecordComputeBridge.SVal) (vs : List WVal) (w : WVal)
    (hdom : AverCert.StandardFace.recordComputeDomRepr carrier structIdx
      plan.params S x vs)
    (hRun : wFuncN code
      (AverCert.StandardFace.recordComputeSlots carrier add sub mul eq hostTable)
      fuel self vs = some w) :
    AverCert.StandardFace.recordComputeCodRepr carrier structIdx S
      (AverCert.StandardFace.recordComputeModel plan.body x) w := by
  obtain ⟨hSRepr, hLen, hTyIdx⟩ := hdom
  have hTy := planTypedB_sound hTyB
  have hvs : vs.length = plan.params.length := (sreprAll_len hSRepr).symm.trans hLen
  have hIdx : ∀ n ∈ plan.body.nodes, ∀ index,
      n.kind = FragNodeKind.local index → index < vs.length := by
    intro n hn index hkind
    have hnt := planTyped_mem hTy n hn
    simp only [RecordComputeBridge.nodeTyped, hkind] at hnt
    have hlt : index < plan.params.length := by
      rcases Nat.lt_or_ge index plan.params.length with h | h
      · exact h
      · rw [List.getElem?_eq_none h] at hnt
        simp at hnt
    omega
  have hbox : ∀ (n : Int) (bw : WVal),
      boxRef carrier [WVal.i64v n] = some bw → S.Repr n bw := by
    intro n bw hb
    simp only [boxRef, Option.some.injEq] at hb
    exact hb ▸ S.smallIntro n
  have hC : Contracts S.Repr (boxRef carrier) add sub mul :=
    ⟨hbox, hAdd, hSub, hMul⟩
  have hHost : ∀ role idx,
      role ∈ [HostRole.box, HostRole.add, HostRole.sub, HostRole.mul] →
      AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx →
      AverCert.StandardFace.recordComputeSlots carrier add sub mul eq
          hostTable idx =
        some (roleArity role, roleFn (boxRef carrier) add sub mul role) :=
    fun role idx hRole hLookup =>
      AverCert.StandardFace.recordComputeSlots_bind carrier add sub mul eq
        hostTable hDistinct role idx hRole hLookup
  have hlow : AverCert.PlanLower.lowerBlockFuel AverCert.PlanCheck.maxFuel
      carrier plan.body = some body := hLower
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      simp only [wFuncN, hCode, initLocals] at hRun
      split at hRun
      next ls v heq =>
        have hvw : v = w := Option.some.inj hRun
        subst hvw
        have hRB := runBlock_complete _ _ _ hostTable
          AverCert.PlanCheck.maxFuel carrier plan.body body _ _
          hAdm hlow heq (Or.inl ⟨ls, v, rfl⟩)
        obtain ⟨v', hout, hRB'⟩ := runBlockFuel_unpad _ _ _ hostTable carrier
          AverCert.PlanCheck.maxFuel plan.body vs (List.replicate 1 WVal.null)
          _ hAdm hIdx hRB
        injection hout with hls hst
        injection hst with hv' htail
        subst hv'
        obtain ⟨sv, hsrc, hsrepr⟩ := sourceRunBlock_agrees S.Repr structIdx
          (boxRef carrier) add sub mul hC _ _ _ carrier hostTable hHost
          AverCert.PlanCheck.maxFuel plan.body x vs vs v hAdm
          (fun nodeId =>
            ((plan.body.nodes[nodeId]?).map (fun n => n.ty)).getD .i64)
          plan.params hTy hTyIdx hSRepr hRB'
        exact ⟨sv, hsrc, hsrepr⟩
      next v heq =>
        have hRB := runBlock_complete _ _ _ hostTable
          AverCert.PlanCheck.maxFuel carrier plan.body body _ _
          hAdm hlow heq (Or.inr ⟨v, rfl⟩)
        obtain ⟨v', hout, -⟩ := runBlockFuel_unpad _ _ _ hostTable carrier
          AverCert.PlanCheck.maxFuel plan.body vs (List.replicate 1 WVal.null)
          _ hAdm hIdx hRB
        simp at hout
      next => simp at hRun

/-- The dependent-cast shell: the obligation's field values arrive as free
    variables with the declared face's `Eq`/`HEq` pins, `subst` collapses
    every pin (the face's `Dom`/`Cod` are concrete types), and the core
    theorem above closes the run. Mirrors `recordParam_transport`. -/
private theorem recordCompute_transport
    (claimCarrier : Nat) (face : AverCert.StandardFace.RecordComputeFace)
    (hostTable : List (HostRole × Nat)) (plan : ExprFragmentRawPlan)
    (body : List WInstr)
    (hDistinct : AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true)
    (hAdm : nodesAdmitted hostTable plan.body.nodes = true)
    (hTyB : planTypedB face.structIdx
      (fun nodeId => ((plan.body.nodes[nodeId]?).map (fun n => n.ty)).getD .i64)
      plan.params plan.body.nodes = true)
    (hLower : AverCert.PlanLower.lowerBlock claimCarrier plan.body = some body)
    (carrier : Nat) (Dom Cod : Type)
    (domRepr : CarrierSpec carrier → Dom → List WVal → Prop)
    (codRepr : CarrierSpec carrier → Cod → WVal → Prop)
    (model : Dom → Cod)
    (hcar : carrier = claimCarrier)
    (hDomT : HEq Dom (List RecordComputeBridge.SVal))
    (hCodT : HEq Cod (Option RecordComputeBridge.SVal))
    (hdomReprT : HEq domRepr (AverCert.StandardFace.recordComputeDomRepr
      claimCarrier face.structIdx plan.params))
    (hcodReprT : HEq codRepr (AverCert.StandardFace.recordComputeCodRepr
      claimCarrier face.structIdx))
    (hmodelT : HEq model (AverCert.StandardFace.recordComputeModel plan.body))
    (code : CodeTbl) (self : Nat)
    (hCode : code self = some ⟨plan.params.length, 1, body⟩)
    (S : CarrierSpec carrier)
    (add sub mul eq : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      add [va, vb] = some w → S.Repr (a + b) w)
    (hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      sub [va, vb] = some w → S.Repr (a - b) w)
    (hMul : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
      mul [va, vb] = some w → S.Repr (a * b) w)
    (fuel : Nat) (x : Dom) (vs : List WVal) (w : WVal)
    (hdom : domRepr S x vs)
    (hRun : wFuncN code
      (AverCert.StandardFace.recordComputeSlots claimCarrier add sub mul eq
        hostTable)
      fuel self vs = some w) :
    codRepr S (model x) w := by
  subst hcar
  have hD : Dom = List RecordComputeBridge.SVal := eq_of_heq hDomT
  subst hD
  have hCo : Cod = Option RecordComputeBridge.SVal := eq_of_heq hCodT
  subst hCo
  have e1 : domRepr = AverCert.StandardFace.recordComputeDomRepr carrier
      face.structIdx plan.params := eq_of_heq hdomReprT
  subst e1
  have e2 : codRepr = AverCert.StandardFace.recordComputeCodRepr carrier
      face.structIdx := eq_of_heq hcodReprT
  subst e2
  have e3 : model = AverCert.StandardFace.recordComputeModel plan.body :=
    eq_of_heq hmodelT
  subst e3
  exact recordCompute_simulates_model carrier face.structIdx hostTable plan
    body hDistinct hAdm hTyB hLower code self hCode S add sub mul eq
    hAdd hSub hMul fuel x vs w hdom hRun

/-- Face-derived discharge of one record projection-compute claim: the
    declared face pins the obligation's meaning to the wall's compute-face
    terms (plan-as-claim); byte acceptance pins the checked plan, its
    canonical lowering, and the exact code entry; the run then transports
    through the bridge: instruction-run success gives plan-walker success
    (`runBlock_complete`), the lockstep agreement gives a source result
    SRepr-related to the machine word, and that IS the model's value. -/
theorem recordCompute_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (claim : SymFragmentClaim)
    (hMem : claim ∈ artifact.symFragmentClaims)
    (hFace : AverCert.StandardFace.symFragmentMatches
      artifact.modBytes artifact.modLen artifact.manifest.subject.hostRoles claim)
    (hIs : exprFragmentIsRecordCompute claim = true) :
    obligationHolds claim.obligation := by
  have hClaim : symFragmentClaimAccepted artifact.modBytes artifact.modLen claim :=
    allClaims_of_mem
      (symFragmentClaimAccepted artifact.modBytes artifact.modLen)
      artifact.symFragmentClaims hAcc claim hMem
  unfold symFragmentClaimAccepted symFragmentPlanAccepted at hClaim
  unfold exprFragmentIsRecordCompute at hIs
  cases hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | none => simp [hEncode] at hIs
  | some plan =>
      simp only [hEncode, Option.isSome_iff_exists] at hIs
      obtain ⟨face, hFace'⟩ := hIs
      obtain ⟨hparams, hAllOk, hAny, -⟩ :=
        AverCert.StandardFace.classifyRecordCompute_spec claim.hostTable plan
          face hFace'
      have hproj : AverCert.WasmSlice.exprRecordProjFace? plan = none := by
        cases hp : AverCert.WasmSlice.exprRecordProjFace? plan with
        | none => rfl
        | some p =>
            obtain ⟨si, fi⟩ := p
            exact absurd
              (AverCert.StandardFace.exprRecordProjFace?_no_compute plan si fi hp)
              (by simp [hAny])
      have hNone := AverCert.StandardFace.symFragmentFace_none_of_recordCompute
        claim plan hEncode face hFace'
      unfold AverCert.StandardFace.symFragmentMatches at hFace
      obtain ⟨hBound, hMatch⟩ := hFace
      simp only [hNone, hEncode, hproj, hFace'] at hMatch
      obtain ⟨hPolicy, -, -, fields, resultTy, -, -, -, -, -, -, -,
        hMatches⟩ := hMatch
      have hM : claim.obligation.carrier = claim.carrier ∧
          HEq claim.obligation.Dom (List RecordComputeBridge.SVal) ∧
          HEq claim.obligation.Cod (Option RecordComputeBridge.SVal) ∧
          HEq claim.obligation.domRepr
            (AverCert.StandardFace.recordComputeDomRepr claim.carrier
              face.structIdx plan.params) ∧
          HEq claim.obligation.codRepr
            (AverCert.StandardFace.recordComputeCodRepr claim.carrier
              face.structIdx) ∧
          claim.obligation.host =
            AverCert.StandardFace.recordComputeHost claim.carrier
              claim.hostTable ∧
          HEq claim.obligation.model
            (AverCert.StandardFace.recordComputeModel plan.body) := hMatches
      obtain ⟨hcar, hDomT, hCodT, hdomReprT, hcodReprT, hhost, hmodelT⟩ := hM
      have hDistinct : AverCert.PlanCheck.hostTableIndicesDistinct
          claim.hostTable = true := by
        simp only [AverCert.StandardFace.hostTableBound, Bool.and_eq_true]
          at hBound
        exact hBound.1
      have hAdm : nodesAdmitted claim.hostTable plan.body.nodes = true := by
        simp only [RecordComputeBridge.nodesAdmitted, List.all_eq_true]
        intro n hn
        exact recordComputeNodeOk_admits claim.hostTable n.kind
          (List.all_eq_true.mp hAllOk n hn)
      have hTyB := classifyRecordCompute_typed claim.hostTable plan face hFace'
      have hAccepted : symFragmentCarrierBound artifact.modBytes artifact.modLen
            claim.carrier claim.hostTable plan = true ∧
          AverCert.WasmSlice.hostTableFuncTypesMatch artifact.modBytes
            artifact.modLen claim.carrier claim.hostTable = true ∧
          exprFragmentPlanAccepted artifact.modBytes artifact.modLen
            claim.exportNameBytes claim.exportName claim.carrier plan
            claim.obligation := by
        simpa [hEncode] using hClaim
      obtain ⟨-, -, hExpr⟩ := hAccepted
      obtain ⟨-, -, body, codeEntry, binding, hByteAccepted, -, -, hSelf,
        hCode⟩ := hExpr
      obtain ⟨hCheck, hLowerExpr, -, -⟩ := hByteAccepted
      have hLower : AverCert.PlanLower.lowerBlock claim.carrier plan.body
          = some body := by
        simp only [AverCert.PlanLower.lowerExprFragmentBody, hCheck, if_true]
          at hLowerExpr
        exact hLowerExpr
      have hCodeSelf : claim.obligation.code claim.obligation.self =
          some ⟨plan.params.length, exprFragmentNLocals plan, body⟩ := by
        rw [hSelf]; exact hCode
      rw [obligationHolds, hPolicy]
      intro S add sub mul stringEq stringConcat toIndex cmp eq
        hAdd hSub hMul _hStringEq _hStringConcat _hToIndex _hCmp _hEq
        fuel x vs w hDom hRun
      rw [hhost] at hRun
      exact recordCompute_transport claim.carrier face claim.hostTable plan
        body hDistinct hAdm hTyB hLower
        claim.obligation.carrier claim.obligation.Dom claim.obligation.Cod
        claim.obligation.domRepr claim.obligation.codRepr
        claim.obligation.model
        hcar hDomT hCodT hdomReprT hcodReprT hmodelT
        claim.obligation.code claim.obligation.self hCodeSelf
        S add sub mul eq hAdd hSub hMul fuel x vs w hDom hRun

end RecordComputeDischarge

theorem exprFragment_claim_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (claim : SymFragmentClaim)
    (hMem : claim ∈ artifact.symFragmentClaims)
    (hFace : AverCert.StandardFace.symFragmentMatches
      artifact.modBytes artifact.modLen artifact.manifest.subject.hostRoles claim)
    (hSide : exprFragmentSideCondition claim) :
    obligationHolds claim.obligation := by
  rcases hSide with hGeneric | hTagDispatch | hVectorGet | hProjection | hFloat |
    hRecord | hIntCmpBool | hIntSelect | hRecordCompute
  · exact exprFragment_claim_discharges_generic artifact hAcc claim hMem hGeneric.2
  · exact exprFragment_claim_discharges_generic artifact hAcc claim hMem hTagDispatch.2
  · exact hVectorGet.2
  · exact hProjection.2
  · exact hFloat.2
  · exact recordParam_claim_discharges artifact hAcc claim hMem hFace hRecord
  · exact intCmpBool_claim_discharges artifact hAcc claim hMem hFace
      hIntCmpBool.1 hIntCmpBool.2
  · exact intSelect_claim_discharges artifact hAcc claim hMem hFace
      hIntSelect.1 hIntSelect.2
  · exact recordCompute_claim_discharges artifact hAcc claim hMem hFace
      hRecordCompute

theorem exprFragment_discharges
    (artifact : ArtifactData)
    (hAcc : acceptedSymFragments artifact)
    (hFaces : allClaims (AverCert.StandardFace.symFragmentMatches
      artifact.modBytes artifact.modLen artifact.manifest.subject.hostRoles)
      artifact.symFragmentClaims)
    (hSemantic : exprFragmentSemanticBridges artifact) :
    ∀ o ∈ artifact.symFragmentClaims.map (·.obligation), obligationHolds o := by
  intro o hObligation
  rcases List.mem_map.mp hObligation with ⟨claim, hMem, rfl⟩
  exact exprFragment_claim_discharges artifact hAcc claim hMem
    (allClaims_of_mem _ artifact.symFragmentClaims hFaces claim hMem)
    (hSemantic claim hMem)

#print axioms intCmpBool_claim_discharges
#print axioms intSelect_claim_discharges
#print axioms recordCompute_claim_discharges

end AcceptanceSoundness
