-- Dependency-closed Lean-side artifact acceptance helpers.
--
-- This is the first bridge from expr-fragment byte-origin checking to the
-- schema obligation the final certificate theorem reasons about. It is still
-- per-fragment, not a full artifact predicate, but it pins the checked plan and
-- Wasm binding to `Schema.Obligation` fields instead of leaving them as loose
-- examples.
import CertPrelude
import SchemaCore
import PlanCheck
import PlanLower
import PlanBytes
import ExprFragmentAccepted
import WasmSlice

namespace AverCert.AcceptedArtifact
open AverCert.Schema
open CertPrelude

/-- Canonical locals count declared by `lowerExprFragmentBodyBytes`: every
    accepted expression fragment has one carrier scratch local. -/
def exprFragmentNLocals (_plan : ExprFragmentRawPlan) : Nat := 1

/-- Artifact-level acceptance for one expression-fragment export. The body,
    canonical code-entry bytes, and function binding are witnesses to the
    audited Lean predicate `ExprFragmentAccepted.accepted`, existentially
    quantified rather than accepted as checker-rendered parameters. This is the
    v2-shaped API: raw artifact bytes + raw plan + schema obligation. -/
def exprFragmentPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (plan : ExprFragmentRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
  obligation.carrier = carrier ∧
  ∃ body codeEntry binding,
    AverCert.ExprFragmentAccepted.accepted
      wasmBytes exportNameBytes carrier plan body codeEntry binding ∧
    obligation.self = binding.funcIdx ∧
    obligation.code binding.funcIdx =
      some { arity := plan.params.length,
             nlocals := exprFragmentNLocals plan, body := body }

/-- Artifact-level acceptance for one source-level symbolic fragment export.
    The source plan is still untrusted data: the audited checker/encoder must
    accept it and produce the representation-level expr-fragment plan before
    the existing byte-origin predicate is allowed to run. -/
def symFragmentPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (structTable : List (String × Nat))
    (plan : SymRawPlan)
    (obligation : Obligation) : Prop :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      hostTable structTable plan with
  | some exprPlan =>
      exprFragmentPlanAccepted
        wasmBytes exportNameBytes exportName carrier exprPlan obligation
  | none => False

/-- One source-level symbolic fragment claim inside an artifact certificate.
    This is the preferred v2 shape for fragments whose meaning can already be
    stated in Aver-level terms. `hostTable`/`structTable` are representation
    context (the byte-derived host-role and struct-type indices), not part of
    the source plan: a wrong table encodes to a representation plan that cannot
    match the byte-bound `exprFragmentPlans` the checker pins, so the claim
    fail-closes. -/
structure SymFragmentClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  hostTable       : List (HostRole × Nat)
  structTable     : List (String × Nat)
  plan            : SymRawPlan
  obligation      : Obligation

def symFragmentClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (claim : SymFragmentClaim) : Prop :=
  symFragmentPlanAccepted
    wasmBytes
    claim.exportNameBytes
    claim.exportName
    claim.carrier
    claim.hostTable
    claim.structTable
    claim.plan
    claim.obligation

/-- Canonical locals count declared by `lowerStringConcatBodyBytes`: the
    concatenation lowering always declares one carrier scratch local. -/
def stringConcatNLocals (_plan : StringConcatRawPlan) : Nat := 1

/-- Artifact-level acceptance for one String.concat export. The raw plan carries
    source-level chunks plus the encoder's data-index binding; the audited Lean
    lowerers rebuild both the semantic `WInstr` body and exact code-entry bytes,
    and the Wasm slicer binds those bytes to the exported function. -/
def stringConcatPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier resultTy containerTy concatFuncIdx : Nat)
    (symPlan : SymRawPlan)
    (plan : StringConcatRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
  obligation.carrier = carrier ∧
  ∃ body codeEntry binding,
    AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
    AverCert.PlanCheck.stringConcatPlanMatchesSymRawPlan symPlan plan = true ∧
    AverCert.PlanCheck.checkStringConcatRawPlan plan = true ∧
    AverCert.PlanLower.lowerStringConcatBody
      resultTy containerTy concatFuncIdx plan = some body ∧
    AverCert.PlanBytes.lowerStringConcatCodeEntry
      carrier resultTy containerTy concatFuncIdx plan = some codeEntry ∧
    AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
    binding.codeEntry = codeEntry ∧
    obligation.self = binding.funcIdx ∧
    obligation.code binding.funcIdx =
      some { arity := 1, nlocals := stringConcatNLocals plan, body := body }

def stringEqPlanAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier stringTy stringEqFuncIdx : Nat)
    (symPlan : SymRawPlan)
    (plan : StringEqRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
    AverCert.PlanCheck.stringEqPlanMatchesSymRawPlan symPlan plan = true ∧
    AverCert.PlanCheck.checkStringEqRawPlan plan = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerStringEqBody stringTy stringEqFuncIdx plan = some body ∧
      AverCert.PlanBytes.lowerStringEqCodeEntry carrier stringTy stringEqFuncIdx plan =
        some codeEntry ∧
      AverCert.WasmSlice.codeEntryForExport wasmBytes exportNameBytes = some codeEntry ∧
      AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
      binding.funcIdx = obligation.self ∧
      binding.codeEntry = codeEntry ∧
      obligation.code binding.funcIdx =
        some { arity := 1, nlocals := 2, body := body }

def stringEqPlanForExport
    (exportName : String) : List (String × StringEqRawPlan) →
    Option StringEqRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        stringEqPlanForExport exportName rest

def stringConcatPlanForExport
    (exportName : String) : List (String × StringConcatRawPlan) →
    Option StringConcatRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        stringConcatPlanForExport exportName rest

def constructPlanForExport
    (exportName : String) : List (String × ConstructRawPlan) →
    Option ConstructRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        constructPlanForExport exportName rest

/-- One source-level String.concat claim inside an artifact certificate. -/
structure StringConcatClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  resultTy        : Nat
  containerTy     : Nat
  concatFuncIdx   : Nat
  symPlan         : SymRawPlan
  obligation      : Obligation

/-- One source-level String.eq claim inside an artifact certificate. -/
structure StringEqClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  stringTy        : Nat
  stringEqFuncIdx : Nat
  symPlan         : SymRawPlan
  obligation      : Obligation

/-- One source-level ADT constructor claim inside an artifact certificate. The
    source `SymRawPlan` describes the Aver value being constructed; the
    target-bound `ConstructRawPlan` is taken from the manifest and checked
    against it before byte lowering. -/
structure ConstructClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  structIdx       : Nat
  fieldCount      : Nat
  elemTy          : ConstructValType
  symPlan         : SymRawPlan
  obligation      : Obligation

/-- One fuel-recursion claim inside an artifact certificate. Its byte-derived
    `RecursionRawPlan` lives in `manifest.recursionPlans` (a byte-origin veneer,
    no source `SymPlan`): the plan is checked against the fuel-recursion grammar
    (self-calls pinned to the byte-derived function binding, host calls pinned
    to the byte-derived role table), lowered to the self-recursive body and its
    exact code-entry bytes, and bound to the exported function. `hostTable` is
    representation context like `SymFragmentClaim`'s: byte-derived indices for
    the box/combinator/sub roles this export's obligation wires — a wrong table
    describes calls the module bytes cannot reproduce, so the claim fail-closes
    at the byte gate. The `obligation` is the unchanged fuel-induction
    obligation the manifest already pins; this claim only certifies where its
    body bytes came from. -/
structure RecursionClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  hostTable       : List (HostRole × Nat)
  obligation      : Obligation

/-- One mutual-recursion member claim inside an artifact certificate. Its
    byte-derived `MutualRawPlan` lives in `manifest.mutualPlans` (a byte-origin
    veneer, no source `SymPlan`): the plan is checked against the mutual member
    grammar (its cross member-call pinned IN the byte-derived SCC member set,
    host calls pinned to the byte-derived box/sub role table), lowered to the
    member body and its exact code-entry bytes, and bound to the exported
    member. `memberSet` is the byte-derived SCC self-index set and `hostTable`
    the byte-derived box/sub indices this SCC's shared obligation wires — both
    representation context like `RecursionClaim`'s `hostTable`; a wrong set or
    table describes calls the module bytes cannot reproduce, so the claim
    fail-closes at the byte gate. The `obligation` is the unchanged conjunction
    fuel-induction obligation the manifest already pins (each member cites its
    own conjunct); this claim only certifies where its body bytes came from. -/
structure MutualRecursionClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  memberSet       : List Nat
  hostTable       : List (HostRole × Nat)
  obligation      : Obligation

/-- One verbatim `ref.test`-dispatch claim inside an artifact certificate. Its
    byte-derived `VerbatimRawPlan` lives in `manifest.verbatimPlans` (a
    byte-origin veneer, no source `SymPlan`: `Cod := WVal` / `verbatimRepr`, no
    representation to name). The plan is checked structurally, lowered to the
    match body and its exact code-entry bytes, and bound to the exported
    function. Unlike the recursion/mutual families there are NO host/self calls
    to tie, so the code entry carries most of the binding — but not the function
    SIGNATURE nor the `array.new_data` payload CONTENTS, which the acceptance
    predicate binds separately (`verbatimFuncTypeMatches`, `verbatimPayloadsBound`)
    so no two distinct plans lower to the same accepted artifact; the
    `obligation` is the unchanged verbatim widened-match / variant-dispatch
    obligation the manifest already pins, and this claim only certifies where its
    body bytes came from. -/
structure VerbatimClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  obligation      : Obligation

/-- One Int-face `ref.test`-dispatch claim inside an artifact certificate. Its
    byte-derived `IntDispatchRawPlan` lives in `manifest.intDispatchPlans` (a
    byte-origin veneer, no source `SymPlan`: the `Cod := Int` variant-dispatch /
    widened-Int-match obligation, its `cases`-spine proof face and Int-valued
    model are unchanged and stay an independent read anchor — the plan claim
    only certifies where the body bytes came from). `hostTable` is
    representation context like `RecursionClaim`'s: the byte-derived box (and,
    when consumed, add/sub) indices this export's obligation wires. The plan
    names host helpers by ROLE only; the lowerers substitute table indices, so a
    wrong table describes calls the module bytes cannot reproduce and the claim
    fail-closes at the byte gate — PROVIDED the table maps roles to distinct
    indices, which the acceptance predicate checks (`hostTableIndicesDistinct`;
    a duplicated table would make the byte gate blind to an arm's role). -/
structure IntDispatchClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  hostTable       : List (HostRole × Nat)
  obligation      : Obligation

/-- One bare tuple/record projection claim. Only `plan.fieldIdx` is producer
    plan data. The remaining fields are reconstructed from validated Wasm by
    the checker and independently re-bound here to the type section and exact
    code entry. -/
structure FieldProjectionClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  structIdx       : Nat
  fieldCount      : Nat
  resultTy        : FieldProjectionResultTy
  obligation      : Obligation

/-- One byte-bound member of the artifact-wide composition call graph. The plan
    names only callees; `compositionFuncTable` below resolves every name through
    the module's export table before either lowerer can use an index. -/
structure CompositionMemberClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  plan            : CompositionRawPlan

/-- One certified composition root. `memberNames` is not trusted closure data:
    `compositionClaimAccepted` requires it to equal the transitive closure
    reached from `exportName` in the byte-bound member call graph. -/
structure CompositionClaim where
  exportName      : String
  carrier         : Nat
  hostTable       : List (HostRole × Nat)
  memberNames     : List String
  obligation      : Obligation

def stringEqClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : StringEqClaim) : Prop :=
  match stringEqPlanForExport claim.exportName manifest.stringEqPlans with
  | some plan =>
      stringEqPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.stringTy
        claim.stringEqFuncIdx
        claim.symPlan
        plan
        claim.obligation
  | none => False

def stringConcatClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : StringConcatClaim) : Prop :=
  match stringConcatPlanForExport claim.exportName manifest.stringConcatPlans with
  | some plan =>
      stringConcatPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.resultTy
        claim.containerTy
        claim.concatFuncIdx
        claim.symPlan
        plan
        claim.obligation
  | none => False

/-- Whether a source constructor claim is specifically a `List` constructor.
    The byte-level two-field cons-cell guards below apply only to this source
    family; ordinary one-field ADT constructors retain their existing exact
    code-entry binding. -/
def isListConstructSymPlan (symPlan : SymRawPlan) : Bool :=
  match symPlan.result with
  | .app1 "List" _ => true
  | _ => false

/-- A parametrized constructor result may only be certified when it is the
    recognized `List` cons shape (whose element type is byte-bound below).
    Any other parametrized result (`.app1 _`/`.app2 _`) is declined
    fail-closed until a per-shape type binding exists; monomorphic results
    stay bound by the struct index in the code entry. -/
def parametrizedConstructIsList (symPlan : SymRawPlan) : Bool :=
  match symPlan.result with
  | .app1 "List" _ => true
  | .app1 _ _ => false
  | .app2 _ _ _ => false
  | _ => true

def constructPlanAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (structIdx fieldCount : Nat)
    (elemTy : ConstructValType)
    (symPlan : SymRawPlan)
    (plan : ConstructRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
    AverCert.PlanCheck.constructPlanMatchesSymRawPlan symPlan plan = true ∧
    AverCert.PlanCheck.checkConstructRawPlan plan = true ∧
    plan.fields.length = fieldCount ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerConstructBody structIdx plan = some body ∧
      AverCert.PlanBytes.lowerConstructCodeEntry carrier structIdx plan = some codeEntry ∧
      AverCert.WasmSlice.codeEntryForExport wasmBytes exportNameBytes = some codeEntry ∧
      AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
      binding.funcIdx = obligation.self ∧
      binding.codeEntry = codeEntry ∧
      (isListConstructSymPlan symPlan = false ∨
        AverCert.WasmSlice.listConstructStructTypeMatches
          wasmBytes structIdx elemTy = true) ∧
      (isListConstructSymPlan symPlan = false ∨
        AverCert.WasmSlice.listConstructFuncTypeMatches
          wasmBytes binding.typeIdx plan.arity structIdx elemTy = true) ∧
      parametrizedConstructIsList symPlan = true ∧
      obligation.code binding.funcIdx =
        some { arity := plan.arity, nlocals := 1, body := body }

def constructClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : ConstructClaim) : Prop :=
  match constructPlanForExport claim.exportName manifest.constructPlans with
  | some plan =>
      constructPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.structIdx
        claim.fieldCount
        claim.elemTy
        claim.symPlan
        plan
        claim.obligation
  | none => False

/-- Canonical locals count declared by `lowerRecursionBodyBytes`: every
    accepted fuel-recursion shape has one carrier scratch local. -/
def recursionNLocals (_plan : RecursionRawPlan) : Nat := 1

/-- Artifact-level acceptance for one fuel-recursion export. The
    `RecursionRawPlan` is checked (generic block typing AND the
    context-sensitive fuel-recursion grammar: every `selfCall` must target the
    byte-derived binding's own function index and every host call must cite the
    byte-derived role table), lowered to the self-recursive `WInstr` body and
    the exact code-entry bytes, those bytes are bound to the exported function,
    and the binding's declared type-section entry must be the canonical
    certified signature `[(ref null carrier)^arity] → [(ref null carrier)]`.
    The self index is additionally tied to the obligation through
    `binding.funcIdx = obligation.self`. -/
def recursionPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (plan : RecursionRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkRecursionRawPlan plan = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerRecursionBody carrier plan = some body ∧
      AverCert.PlanBytes.lowerRecursionCodeEntry carrier plan = some codeEntry ∧
      AverCert.WasmSlice.codeEntryForExport wasmBytes exportNameBytes = some codeEntry ∧
      AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
      binding.funcIdx = obligation.self ∧
      binding.codeEntry = codeEntry ∧
      AverCert.PlanCheck.checkRecursionPlanShape binding.funcIdx hostTable plan = true ∧
      AverCert.WasmSlice.funcTypeMatches
        wasmBytes binding.typeIdx plan.params.length carrier = true ∧
      obligation.code binding.funcIdx =
        some { arity := plan.params.length,
               nlocals := recursionNLocals plan, body := body }

def recursionPlanForExport
    (exportName : String) : List (String × RecursionRawPlan) →
    Option RecursionRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        recursionPlanForExport exportName rest

def recursionClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : RecursionClaim) : Prop :=
  match recursionPlanForExport claim.exportName manifest.recursionPlans with
  | some plan =>
      recursionPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.hostTable
        plan
        claim.obligation
  | none => False

/-- Canonical locals count declared by `lowerMutualBodyBytes`: every accepted
    mutual-recursion member shape has one carrier scratch local. -/
def mutualNLocals (_plan : MutualRawPlan) : Nat := 1

/-- Artifact-level acceptance for one mutual-recursion member export. The
    `MutualRawPlan` is checked (generic block typing AND the context-sensitive
    mutual member grammar: the member-call must target an index IN the
    byte-derived SCC member set and every host call must cite the byte-derived
    box/sub role table), lowered to the member `WInstr` body and the exact
    code-entry bytes, those bytes are bound to the exported member, and the
    binding's declared type-section entry must be the canonical certified
    signature `[(ref null carrier)] → [(ref null carrier)]`. The member index is
    tied to the (shared) obligation through `binding.funcIdx = obligation.self`;
    `obligation.code binding.funcIdx` picks this member's arm out of the shared
    multi-arm code table. -/
def mutualPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (memberSet : List Nat)
    (hostTable : List (HostRole × Nat))
    (plan : MutualRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkMutualRawPlan plan = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerMutualBody carrier plan = some body ∧
      AverCert.PlanBytes.lowerMutualCodeEntry carrier plan = some codeEntry ∧
      AverCert.WasmSlice.codeEntryForExport wasmBytes exportNameBytes = some codeEntry ∧
      AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
      binding.funcIdx = obligation.self ∧
      binding.codeEntry = codeEntry ∧
      AverCert.PlanCheck.checkMutualPlanShape memberSet hostTable plan = true ∧
      AverCert.WasmSlice.funcTypeMatches
        wasmBytes binding.typeIdx plan.params.length carrier = true ∧
      obligation.code binding.funcIdx =
        some { arity := plan.params.length,
               nlocals := mutualNLocals plan, body := body }

def mutualPlanForExport
    (exportName : String) : List (String × MutualRawPlan) →
    Option MutualRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        mutualPlanForExport exportName rest

def mutualRecursionClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : MutualRecursionClaim) : Prop :=
  match mutualPlanForExport claim.exportName manifest.mutualPlans with
  | some plan =>
      mutualPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.memberSet
        claim.hostTable
        plan
        claim.obligation
  | none => False

/-- Whether one dispatch leaf's payload is byte-bound to the module. Only an
    `arrayNewData` leaf carries a payload; its claimed `bytes` must equal the
    exact contents of passive data segment `dataIdx` recovered from the module's
    data section. Every other leaf trivially satisfies the binding. -/
def verbatimLeafPayloadBound
    (wasmBytes : AverCert.WasmSlice.ByteSeq) : VerbatimLeaf → Bool
  | .arrayNewData _ dataIdx bytes =>
      match AverCert.WasmSlice.dataSegmentBytes wasmBytes dataIdx with
      | some contents => contents == bytes
      | none => false
  | _ => true

/-- Every `arrayNewData` leaf in a verbatim dispatch has a byte-bound payload.
    The canonical code-entry lowering pins each string literal's data-segment
    INDEX and copied LENGTH but not its CONTENTS, so without this an equal-length
    payload substitution keeps the code entry byte-identical while changing what
    the plan (hence the model) claims. -/
def verbatimPayloadsBound
    (wasmBytes : AverCert.WasmSlice.ByteSeq) : VerbatimDispatch → Bool
  | .leaf l => verbatimLeafPayloadBound wasmBytes l
  | .test _ hit rest =>
      verbatimLeafPayloadBound wasmBytes hit && verbatimPayloadsBound wasmBytes rest

/-- Canonical locals count declared by `lowerVerbatimLocalsBytes`: projecting
    plans declare the field scratch, scrutinee, and carrier locals; all other
    plans declare only the scrutinee and carrier locals. -/
def verbatimNLocals (plan : VerbatimRawPlan) : Nat :=
  if AverCert.PlanCheck.dispatchHasProjection plan.body then 3 else 2

/-- Artifact-level acceptance for one verbatim `ref.test`-dispatch export. The
    `VerbatimRawPlan` is checked structurally (`checkVerbatimRawPlan`), lowered to
    the match `WInstr` body and its exact code-entry bytes, and those bytes are
    bound to the exported function by name. There are no host/self calls to tie,
    so the code entry is nearly the whole binding — but the code entry alone does
    NOT determine the export's meaning: it omits the function SIGNATURE (a second
    `eqref` parameter leaves the locals + body bytes identical) and the
    `array.new_data` PAYLOAD CONTENTS (only the segment index and length are
    encoded). Two further conjuncts close both holes in-kernel:
    `verbatimFuncTypeMatches` forces the byte-derived type-section entry to be the
    unary ref-null or f64 signature declared by `plan.resultSig`, and `verbatimPayloadsBound`
    forces every literal's claimed bytes to equal the byte-pinned data segment. A
    body byte-noisier than the canonical dispatch lowering still fails the
    byte-equality gate. The member index is tied to the obligation through
    `binding.funcIdx = obligation.self`. -/
def verbatimPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (plan : VerbatimRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkVerbatimRawPlan plan = true ∧
    ∃ codeEntry binding,
      AverCert.PlanBytes.lowerVerbatimCodeEntry carrier plan = some codeEntry ∧
      AverCert.WasmSlice.codeEntryForExport wasmBytes exportNameBytes = some codeEntry ∧
      AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
      binding.funcIdx = obligation.self ∧
      binding.codeEntry = codeEntry ∧
      AverCert.WasmSlice.verbatimFuncTypeMatches
        wasmBytes binding.typeIdx plan.resultSig = true ∧
      verbatimPayloadsBound wasmBytes plan.body = true ∧
      obligation.code binding.funcIdx =
        some { arity := 1, nlocals := verbatimNLocals plan,
               body := AverCert.PlanLower.lowerVerbatimBody plan }

def verbatimPlanForExport
    (exportName : String) : List (String × VerbatimRawPlan) →
    Option VerbatimRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        verbatimPlanForExport exportName rest

def verbatimClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : VerbatimClaim) : Prop :=
  match verbatimPlanForExport claim.exportName manifest.verbatimPlans with
  | some plan =>
      verbatimPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        plan
        claim.obligation
  | none => False

/-- The canonical Int-face host slot chain: the byte-derived role table lowered
    to exactly the `if fn = idx then …` chain (in table order) the emitter's
    `{name}Host` definitions carry — a `box` entry wires the audited `boxRef`
    over the carrier, an `add`/`sub` entry wires that contract-slot PARAMETER.
    This reuses the same contract functions the checker-rendered obligation
    host is built from (`boxRef` and the abstract add/sub slots of
    `Obligation.host`); it defines no new semantics. -/
def intDispatchCanonicalSlots
    (carrier : Nat) (add sub : List WVal → Option WVal) :
    List (HostRole × Nat) → HostTbl
  | [] => fun _ => none
  | (role, idx) :: rest => fun fn =>
      if fn = idx then
        some (match role with
          | .box => ((1 : Nat), boxRef carrier)
          | .add => ((2 : Nat), add)
          | .sub => ((2 : Nat), sub))
      else intDispatchCanonicalSlots carrier add sub rest fn

/-- The canonical Int-face host BUILDER for a byte-derived role table: the
    whole `Obligation.host` value an honest Int-face dispatch obligation
    carries (mul/stringEq/stringConcat slots unused). The acceptance predicate
    requires `obligation.host` to EQUAL this builder — one extensional,
    definitional equality over the whole function, mirroring in-kernel the
    checker's whole-host `rfl` pin. A sampled probe is NOT enough here: in the
    standalone-artifact posture the obligation is claim data, so a slot could
    behave as its role's contract only on the probed inputs (e.g. add on `[]`
    but sub on every real two-argument list, or a box helper defined only at
    the probe's constant that traps on every other) — builder equality leaves
    no unsampled behaviour, so the table is a function of the obligation the
    model semantics actually reference, not a claim-supplied choice. -/
def intDispatchCanonicalHost
    (carrier : Nat) (hostTable : List (HostRole × Nat)) :
    (List WVal → Option WVal) → (List WVal → Option WVal) →
    (List WVal → Option WVal) → (List WVal → Option WVal) →
    (Nat → List WVal → Option WVal) → HostTbl :=
  fun add sub _mul _stringEq _stringConcat =>
    intDispatchCanonicalSlots carrier add sub hostTable

/-- Artifact-level acceptance for one Int-face `ref.test`-dispatch export. The
    `IntDispatchRawPlan` is checked structurally (`checkIntDispatchRawPlan`),
    lowered — parameterized by the claim's byte-derived host-role table, whose
    indices must be pairwise DISTINCT (`hostTableIndicesDistinct`: the plan
    names helpers by role only, so a duplicated table would let two plans
    differing in an arm's role lower to identical bytes) and which must EQUAL,
    through the obligation's own host builder, the canonical wiring
    (`obligation.host = intDispatchCanonicalHost carrier hostTable`: otherwise
    a role permutation in the plan plus a consistently permuted table cancels
    out byte-identically, and a sampled check would leave unsampled slot
    behaviour free) — to the match
    `WInstr` body and its exact code-entry bytes, and those bytes are bound to
    the exported function by name. The dispatch structure (tags, arm constants,
    operand order, roles) is pinned entirely by the byte-equality gate: every
    plan field reaches the lowered bytes. The code entry omits the function
    SIGNATURE (a second `eqref` parameter leaves the locals + body bytes
    identical), so `verbatimFuncTypeMatches` additionally forces the
    byte-derived type-section entry to be the unary
    `[eqref] → [(ref null carrier)]` signature — the same shape check the
    verbatim family uses, here with the Int carrier as the result heap type.
    The function index is tied to the obligation through
    `binding.funcIdx = obligation.self`, and the obligation's code table must
    carry exactly the plan-lowered body WITH the canonical byte-derived locals
    count (`armCount + 2`, exactly what the byte lowering declares in the
    locals vector): an existentially-free `nlocals` would let an honest-bytes
    artifact claim a 0-locals table whose body traps on its first `local.set`,
    making the partial-correctness obligation vacuously true. -/
def intDispatchPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (plan : IntDispatchRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkIntDispatchRawPlan plan = true ∧
    AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true ∧
    obligation.host = intDispatchCanonicalHost carrier hostTable ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerIntDispatchBody hostTable plan = some body ∧
      AverCert.PlanBytes.lowerIntDispatchCodeEntry carrier hostTable plan =
        some codeEntry ∧
      AverCert.WasmSlice.codeEntryForExport wasmBytes exportNameBytes = some codeEntry ∧
      AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
      binding.funcIdx = obligation.self ∧
      binding.codeEntry = codeEntry ∧
      AverCert.WasmSlice.verbatimFuncTypeMatches
        wasmBytes binding.typeIdx (.refNull carrier) = true ∧
      obligation.code binding.funcIdx =
        some { arity := 1,
               nlocals := AverCert.PlanCheck.intDispatchArmCount plan.body + 2,
               body := body }

def intDispatchPlanForExport
    (exportName : String) : List (String × IntDispatchRawPlan) →
    Option IntDispatchRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then
        some plan
      else
        intDispatchPlanForExport exportName rest

def intDispatchClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : IntDispatchClaim) : Prop :=
  match intDispatchPlanForExport claim.exportName manifest.intDispatchPlans with
  | some plan =>
      intDispatchPlanAccepted
        wasmBytes
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.hostTable
        plan
        claim.obligation
  | none => False

def fieldProjectionPlanForExport
    (exportName : String) : List (String × FieldProjectionRawPlan) →
    Option FieldProjectionRawPlan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then some plan
      else fieldProjectionPlanForExport exportName rest

/-- Plan-backed acceptance for the bare bind/cast projection lowering. The
    exact three-local layout is part of the canonical code-entry encoder and is
    also pinned in the semantic code table. The type-section guards bind the
    checker-derived struct index/count/result reference to the selected module
    field and to the exported unary function signature. -/
def fieldProjectionPlanAccepted
    (wasmBytes exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String) (carrier structIdx fieldCount : Nat)
    (resultTy : FieldProjectionResultTy)
    (plan : FieldProjectionRawPlan) (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
  obligation.carrier = carrier ∧
  AverCert.PlanCheck.checkFieldProjectionRawPlan fieldCount plan = true ∧
  ∃ body codeEntry binding,
    AverCert.PlanLower.lowerFieldProjectionBody structIdx fieldCount plan = some body ∧
    AverCert.PlanBytes.lowerFieldProjectionCodeEntry
      carrier structIdx fieldCount resultTy plan = some codeEntry ∧
    AverCert.WasmSlice.funcBindingForExport wasmBytes exportNameBytes = some binding ∧
    binding.codeEntry = codeEntry ∧
    AverCert.WasmSlice.projectionStructTypeMatches
      wasmBytes structIdx fieldCount plan.fieldIdx resultTy = true ∧
    AverCert.WasmSlice.projectionFuncTypeMatches
      wasmBytes binding.typeIdx structIdx resultTy = true ∧
    obligation.self = binding.funcIdx ∧
    obligation.code binding.funcIdx =
      some { arity := 1, nlocals := 3, body := body }

def fieldProjectionClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest)
    (claim : FieldProjectionClaim) : Prop :=
  match fieldProjectionPlanForExport claim.exportName manifest.fieldProjectionPlans with
  | some plan =>
      fieldProjectionPlanAccepted wasmBytes claim.exportNameBytes claim.exportName
        claim.carrier claim.structIdx claim.fieldCount claim.resultTy plan claim.obligation
  | none => False

/-! ### Cross-function composition: byte-bound member graph and closure -/

/-- Exact canonical locals count emitted by both composition byte lowerers. -/
def compositionNLocals (_plan : CompositionRawPlan) : Nat := 1

def compositionMemberBinding
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (member : CompositionMemberClaim) : Option (String × Nat) :=
  match AverCert.WasmSlice.funcBindingForExport wasmBytes member.exportNameBytes with
  | some binding => some (member.exportName, binding.funcIdx)
  | none => none

def compositionFuncTable
    (wasmBytes : AverCert.WasmSlice.ByteSeq) :
    List CompositionMemberClaim → Option (List (String × Nat))
  | [] => some []
  | member :: rest =>
      match compositionMemberBinding wasmBytes member,
            compositionFuncTable wasmBytes rest with
      | some binding, some bindings => some (binding :: bindings)
      | _, _ => none

def compositionMemberForName
    (name : String) : List CompositionMemberClaim → Option CompositionMemberClaim
  | [] => none
  | member :: rest =>
      if member.exportName == name then some member
      else compositionMemberForName name rest

def compositionMemberPlanAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (funcTable : List (String × Nat))
    (obligation : Obligation)
    (member : CompositionMemberClaim) : Prop :=
  AverCert.PlanCheck.checkCompositionRawPlan member.plan = true ∧
  ∃ body codeEntry binding,
    AverCert.PlanLower.lowerCompositionBody hostTable funcTable member.plan = some body ∧
    AverCert.PlanBytes.lowerCompositionCodeEntry carrier hostTable funcTable member.plan =
      some codeEntry ∧
    AverCert.WasmSlice.codeEntryForExport wasmBytes member.exportNameBytes = some codeEntry ∧
    AverCert.WasmSlice.funcBindingForExport wasmBytes member.exportNameBytes = some binding ∧
    binding.codeEntry = codeEntry ∧
    AverCert.WasmSlice.funcTypeMatches wasmBytes binding.typeIdx 1 carrier = true ∧
    obligation.code binding.funcIdx =
      some { arity := 1, nlocals := compositionNLocals member.plan, body := body }

def compositionNamedMembersAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (funcTable : List (String × Nat))
    (obligation : Obligation)
    (members : List CompositionMemberClaim) : List String → Prop
  | [] => True
  | name :: rest =>
      match compositionMemberForName name members with
      | some member =>
          compositionMemberPlanAccepted
            wasmBytes carrier hostTable funcTable obligation member ∧
          compositionNamedMembersAccepted
            wasmBytes carrier hostTable funcTable obligation members rest
      | none => False

def compositionPlanCallees (plan : CompositionRawPlan) : List String :=
  match plan.shape with
  | .selfSum => []
  | .chain callees => callees

def stringListNodup : List String → Bool
  | [] => true
  | x :: rest => !rest.contains x && stringListNodup rest

def stringListSetEq (xs ys : List String) : Bool :=
  xs.length == ys.length && xs.all (fun x => ys.contains x) &&
    ys.all (fun y => xs.contains y)

def compositionEdges
    (members : List CompositionMemberClaim) : List (String × List String) :=
  members.map (fun member => (member.exportName, compositionPlanCallees member.plan))

def compositionEdgeLookup
    (edges : List (String × List String)) (name : String) : Option (List String) :=
  match edges.find? (fun edge => edge.1 == name) with
  | some edge => some edge.2
  | none => none

/-- One breadth-expansion step of the byte-bound call graph. Missing targets
    fail closed; repeated vertices are set-like and do not enlarge the result. -/
def compositionReachStep
    (edges : List (String × List String)) :
    List String → List String → Option (List String)
  | reached, [] => some reached
  | reached, name :: rest =>
      match compositionEdgeLookup edges name with
      | some callees =>
          if callees.all (fun callee => (edges.map (fun edge => edge.1)).contains callee) then
            let next := callees.foldl
              (fun acc callee => if acc.contains callee then acc else acc ++ [callee]) reached
            compositionReachStep edges next rest
          else none
      | none => none

def compositionReachClosure
    (edges : List (String × List String)) : Nat → List String → Option (List String)
  | 0, reached => some reached
  | fuel + 1, reached =>
      match compositionReachStep edges reached reached with
      | some next => compositionReachClosure edges fuel next
      | none => none

/-- Numeric target indices strictly descend along every edge. Both endpoints
    come from byte-derived export bindings, so this rejects cycles without
    trusting a plan-supplied ordering or membership set. -/
def compositionEdgesDescend
    (funcTable : List (String × Nat))
    (edges : List (String × List String)) : Bool :=
  edges.all (fun edge =>
    match AverCert.PlanLower.compositionFuncIdx? funcTable edge.1 with
    | some self => edge.2.all (fun callee =>
        match AverCert.PlanLower.compositionFuncIdx? funcTable callee with
        | some target => target < self
        | none => false)
    | none => false)

/-- Closure membership is recomputed by following the call graph extracted
    from byte-bound plans. `memberNames` must equal that closure exactly; extra,
    omitted, duplicate, dangling, or cyclic members are rejected. -/
def compositionClosureBound
    (root : String)
    (memberNames : List String)
    (members : List CompositionMemberClaim)
    (funcTable : List (String × Nat)) : Bool :=
  let edges := compositionEdges members
  stringListNodup (members.map (fun member => member.exportName)) &&
    stringListNodup memberNames &&
    AverCert.PlanCheck.hostTableIndicesDistinct (funcTable.map (fun entry => (.add, entry.2))) &&
    compositionEdgesDescend funcTable edges &&
    (match compositionMemberForName root members with
     | some rootMember =>
         (match rootMember.plan.shape with
          | .chain _ => true
          | .selfSum => false) &&
         (match compositionReachClosure edges (members.length + 1) [root] with
          | some reached => stringListSetEq memberNames reached
          | none => false)
     | none => false)

def compositionClaimAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (members : List CompositionMemberClaim)
    (claim : CompositionClaim) : Prop :=
  claim.obligation.export_ = claim.exportName ∧
  claim.obligation.carrier = claim.carrier ∧
  AverCert.PlanCheck.checkCompositionHostTable claim.hostTable = true ∧
  claim.obligation.host = intDispatchCanonicalHost claim.carrier claim.hostTable ∧
  match compositionFuncTable wasmBytes members with
  | some funcTable =>
      compositionClosureBound claim.exportName claim.memberNames members funcTable = true ∧
      (match AverCert.PlanLower.compositionFuncIdx? funcTable claim.exportName with
       | some rootIdx => claim.obligation.self = rootIdx
       | none => False) ∧
      compositionNamedMembersAccepted wasmBytes claim.carrier claim.hostTable
        funcTable claim.obligation members claim.memberNames
  | none => False

/-- Aggregate source-level symbolic fragment acceptance for one artifact's
    source claim list. -/
def symFragmentClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq) :
    List SymFragmentClaim → Prop
  | [] => True
  | claim :: rest =>
      symFragmentClaimAccepted wasmBytes claim ∧
      symFragmentClaimsAccepted wasmBytes rest

/-- Aggregate source-level String.concat witness acceptance for one artifact's
    string claim list. -/
def stringConcatClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List StringConcatClaim → Prop
  | [] => True
  | claim :: rest =>
      stringConcatClaimAccepted wasmBytes manifest claim ∧
      stringConcatClaimsAccepted wasmBytes manifest rest

/-- Aggregate source-level String.eq witness acceptance for one artifact's
    string equality claim list. -/
def stringEqClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List StringEqClaim → Prop
  | [] => True
  | claim :: rest =>
      stringEqClaimAccepted wasmBytes manifest claim ∧
      stringEqClaimsAccepted wasmBytes manifest rest

/-- Aggregate source-level constructor witness acceptance for one artifact's
    constructor claim list. -/
def constructClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List ConstructClaim → Prop
  | [] => True
  | claim :: rest =>
      constructClaimAccepted wasmBytes manifest claim ∧
      constructClaimsAccepted wasmBytes manifest rest

/-- Aggregate fuel-recursion witness acceptance for one artifact's recursion
    claim list. -/
def recursionClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List RecursionClaim → Prop
  | [] => True
  | claim :: rest =>
      recursionClaimAccepted wasmBytes manifest claim ∧
      recursionClaimsAccepted wasmBytes manifest rest

/-- Aggregate mutual-recursion witness acceptance for one artifact's mutual
    claim list. -/
def mutualRecursionClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List MutualRecursionClaim → Prop
  | [] => True
  | claim :: rest =>
      mutualRecursionClaimAccepted wasmBytes manifest claim ∧
      mutualRecursionClaimsAccepted wasmBytes manifest rest

/-- Aggregate verbatim `ref.test`-dispatch acceptance for one artifact's verbatim
    claim list. -/
def verbatimClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List VerbatimClaim → Prop
  | [] => True
  | claim :: rest =>
      verbatimClaimAccepted wasmBytes manifest claim ∧
      verbatimClaimsAccepted wasmBytes manifest rest

/-- Aggregate Int-face `ref.test`-dispatch acceptance for one artifact's
    int-dispatch claim list. -/
def intDispatchClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List IntDispatchClaim → Prop
  | [] => True
  | claim :: rest =>
      intDispatchClaimAccepted wasmBytes manifest claim ∧
      intDispatchClaimsAccepted wasmBytes manifest rest

def fieldProjectionClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (manifest : AverCert.Schema.Manifest) :
    List FieldProjectionClaim → Prop
  | [] => True
  | claim :: rest =>
      fieldProjectionClaimAccepted wasmBytes manifest claim ∧
      fieldProjectionClaimsAccepted wasmBytes manifest rest

/-- Aggregate composition-root acceptance. All roots share the artifact-wide
    unique member table, while each root independently re-derives its reachable
    closure and pins every reachable body into its own shared `CodeTbl`. -/
def compositionClaimsAccepted
    (wasmBytes : AverCert.WasmSlice.ByteSeq)
    (members : List CompositionMemberClaim) : List CompositionClaim → Prop
  | [] => True
  | claim :: rest =>
      compositionClaimAccepted wasmBytes members claim ∧
      compositionClaimsAccepted wasmBytes members rest

/-- Every name claimed as a reachable member across all composition roots. Each
    claim's `memberNames` is pinned to its root's byte-derived reachable closure
    by `compositionClosureBound`, so this is the union of those closures. -/
def compositionClaimedNames : List CompositionClaim → List String
  | [] => []
  | claim :: rest => claim.memberNames ++ compositionClaimedNames rest

/-- Coverage: every artifact-wide composition member is named by (hence
    reachable from) some composition root. The byte-check
    (`compositionNamedMembersAccepted`) only inspects members NAMED by a claim's
    `memberNames`, while `claimsMatchManifest` equates the whole
    `compositionMembers` list to `manifest.compositionPlans`. Without this
    conjunct an ORPHAN member — present in `compositionMembers` (and therefore
    in `manifest.compositionPlans`) but reachable from no claimed root — would
    ride into the manifest with NO code-entry / signature / code-table /
    `nlocals` check: the "exists but is not constrained" class at
    manifest-coverage level. Requiring every member to appear in the union of
    the roots' `memberNames` — combined with `compositionNamedMembersAccepted`
    (byte-checks each named member) and `compositionClosureBound`
    (`memberNames` = the root's byte-derived closure) — makes
    `compositionMembers` EXACTLY the union of the claimed roots' reachable
    closures, so every member entry that reaches the manifest is byte-checked.
    Stated over the whole artifact (not per claim) because the union is a
    cross-claim fact: `quad` and `hex16` share one member table yet have
    different closures (`{quad, double}` vs `{hex16, quad, double}`), so no
    single root's closure equals the member set — only their union does. -/
def compositionMembersCovered
    (members : List CompositionMemberClaim)
    (claims : List CompositionClaim) : Bool :=
  let claimed := compositionClaimedNames claims
  members.all (fun member => claimed.contains member.exportName)

/-! ### Byte-derived SCC closure

`mutualRecursionClaimsAccepted` above certifies each member's body byte-origin
in ISOLATION. On its own that leaves the SCC's IDENTITY as unconstrained claim
data: a claim's `memberSet` could list extra/missing members, the members could
disagree on the set, and nothing forces the member-call graph to be one closed
cycle visiting every member. This section binds the SCC to bytes: it derives
each member's `(self, cross-target)` edge from the SAME byte-bound facts the
per-claim acceptance pins — `self` is the obligation's byte-pinned function
index (`= binding.funcIdx` from `mutualPlanAccepted`), `target` is read from the
byte-pinned plan (`lowerMutualCodeEntry plan = codeEntryForExport …`). It then
checks, purely over those byte-derived edges, that the claims form disjoint
CLOSED simple cycles and that every member's declared `memberSet` equals its own
cycle's vertex set — so `memberSet` is a function of the bytes, not a free
choice. Composed with the per-claim acceptance (which pins `self`/`target` to
bytes) this makes the negative-space closure property hold in-kernel. -/

/-- The byte-pinned member-call target of a checked mutual member plan: the
    tail `selfCall` index in the step arm of the fixed mutual grammar. Returns
    `none` for any other shape (fail-closed); the per-claim `checkMutualPlanShape`
    already forces this exact shape, so a claimed member always yields `some`. -/
def mutualPlanTarget (plan : MutualRawPlan) : Option Nat :=
  match plan.body.result, plan.body.nodes with
  | 4, [_, _, _, _, { kind := .ifElse 3 _ step, .. }] =>
      match step.result, step.nodes with
      | 4, [_, _, _, _, { kind := .selfCall true cc [3], .. }] => some cc
      | _, _ => none
  | _, _ => none

/-- The byte-derived edge and declared member set for one claim:
    `(self, cross-target, memberSet)`. `self` is the obligation's byte-pinned
    index; `target` is read from the byte-pinned manifest plan. -/
def mutualClaimEdge
    (manifest : AverCert.Schema.Manifest)
    (claim : MutualRecursionClaim) : Option (Nat × Nat × List Nat) :=
  match mutualPlanForExport claim.exportName manifest.mutualPlans with
  | some plan =>
      match mutualPlanTarget plan with
      | some t => some (claim.obligation.self, t, claim.memberSet)
      | none => none
  | none => none

def mutualClaimEdges
    (manifest : AverCert.Schema.Manifest) :
    List MutualRecursionClaim → Option (List (Nat × Nat × List Nat))
  | [] => some []
  | claim :: rest =>
      match mutualClaimEdge manifest claim, mutualClaimEdges manifest rest with
      | some m, some ms => some (m :: ms)
      | _, _ => none

/-- No repeated element (no two claims for the same byte-derived member index). -/
def natListNodup : List Nat → Bool
  | [] => true
  | x :: rest => !rest.contains x && natListNodup rest

/-- Set equality via mutual containment plus equal length: rejects extras,
    omissions AND duplicates (a duplicate makes the lengths differ). -/
def natListSetEq (xs ys : List Nat) : Bool :=
  xs.length == ys.length && xs.all (fun x => ys.contains x) &&
    ys.all (fun y => xs.contains y)

def natEdgeLookup : List (Nat × Nat) → Nat → Option Nat
  | [], _ => none
  | (a, b) :: rest, k => if a == k then some b else natEdgeLookup rest k

/-- Follow the target-chain from `start`, collecting visited members, until the
    next hop closes the walk. A SIMPLE closed cycle returns to `start` (the head
    of the visited list); a hop to any other already-visited node (a rho tail) or
    a hop to a non-member (dangling edge) fail-closes to `none`. -/
def followSccCycle (edges : List (Nat × Nat)) :
    Nat → Nat → List Nat → Option (List Nat)
  | 0, _, _ => none
  | fuel + 1, cur, visited =>
      match natEdgeLookup edges cur with
      | some nxt =>
          let visited := visited ++ [cur]
          if visited.contains nxt then
            (if visited.head? == some nxt then some visited else none)
          else
            followSccCycle edges fuel nxt visited
      | none => none

/-- The byte-derived closure check over all mutual members: the member indices
    are distinct (no duplicate claims), and every member sits on a SINGLE closed
    simple cycle of length ≥ 2 whose vertex set equals that member's declared
    `memberSet`. Since every target must be a member index and every member must
    lie on a cycle returning to itself, the edge relation is a disjoint union of
    pure cycles — exactly the mutual-recursion SCC shape — and `memberSet` is
    pinned to the byte-derived cycle rather than chosen by the claim. -/
def mutualMembersFormClosedSccs (members : List (Nat × Nat × List Nat)) : Bool :=
  let selfs := members.map (fun m => m.1)
  let edges := members.map (fun m => (m.1, m.2.1))
  natListNodup selfs &&
    members.all (fun m =>
      selfs.contains m.2.1 &&
        (match followSccCycle edges (members.length + 1) m.1 [] with
         | some cyc => decide (2 ≤ cyc.length) && natListSetEq m.2.2 cyc
         | none => false))

/-- The artifact's mutual claims form byte-derived closed SCCs. Composed with
    `mutualRecursionClaimsAccepted` (which pins each `self`/`target` to bytes)
    this establishes the whole closed-call-graph property in-kernel. -/
def mutualClaimsFormClosedSccs
    (manifest : AverCert.Schema.Manifest)
    (claims : List MutualRecursionClaim) : Prop :=
  match mutualClaimEdges manifest claims with
  | some members => mutualMembersFormClosedSccs members = true
  | none => False

/-- The source plans claimed by an artifact, projected into the same manifest
    surface used for pinning. Keeping this in the audited predicate means a
    self-checking artifact cannot prove acceptance for one claim list while
    advertising a different source-plan list in its manifest. -/
def symFragmentClaimPlanPairs
    (claims : List SymFragmentClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.plan))

/-- Representation plans induced by source-level claims. This is what keeps the
    artifact surface source-first: the byte-bound plan is computed by the
    audited encoder rather than carried as a separate claim. -/
def symFragmentClaimEncodedPlanPair?
    (claim : SymFragmentClaim) : Option (String × ExprFragmentRawPlan) :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | some exprPlan => some (claim.exportName, exprPlan)
  | none => none

def symFragmentClaimEncodedPlanPairs :
    List SymFragmentClaim → Option (List (String × ExprFragmentRawPlan))
  | [] => some []
  | claim :: rest =>
      match symFragmentClaimEncodedPlanPair? claim,
            symFragmentClaimEncodedPlanPairs rest with
      | some pair, some pairs => some (pair :: pairs)
      | _, _ => none

def stringConcatClaimExportNames
    (claims : List StringConcatClaim) : List String :=
  claims.map (fun c => c.exportName)

def stringConcatManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.stringConcatPlans.map (fun p => p.1)

def stringEqClaimExportNames
    (claims : List StringEqClaim) : List String :=
  claims.map (fun c => c.exportName)

def stringEqManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.stringEqPlans.map (fun p => p.1)

def constructClaimExportNames
    (claims : List ConstructClaim) : List String :=
  claims.map (fun c => c.exportName)

def constructManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.constructPlans.map (fun p => p.1)

/-- Recursion claims are pinned to `manifest.recursionPlans` by export name,
    mirroring the String.eq/constructor families: a self-checking artifact
    cannot advertise a different recursion-plan list than the claims it proves
    acceptance for. -/
def recursionClaimExportNames
    (claims : List RecursionClaim) : List String :=
  claims.map (fun c => c.exportName)

def recursionManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.recursionPlans.map (fun p => p.1)

/-- Mutual-recursion claims are pinned to `manifest.mutualPlans` by export name,
    mirroring the recursion family: a self-checking artifact cannot advertise a
    different mutual-plan list than the claims it proves acceptance for. -/
def mutualRecursionClaimExportNames
    (claims : List MutualRecursionClaim) : List String :=
  claims.map (fun c => c.exportName)

def mutualManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.mutualPlans.map (fun p => p.1)

/-- Verbatim claims are pinned to `manifest.verbatimPlans` by export name,
    mirroring the recursion/mutual families: a self-checking artifact cannot
    advertise a different verbatim-plan list than the claims it proves acceptance
    for. -/
def verbatimClaimExportNames
    (claims : List VerbatimClaim) : List String :=
  claims.map (fun c => c.exportName)

def verbatimManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.verbatimPlans.map (fun p => p.1)

/-- Int-face dispatch claims are pinned to `manifest.intDispatchPlans` by export
    name, mirroring the recursion/mutual/verbatim families: a self-checking
    artifact cannot advertise a different int-dispatch-plan list than the claims
    it proves acceptance for. -/
def intDispatchClaimExportNames
    (claims : List IntDispatchClaim) : List String :=
  claims.map (fun c => c.exportName)

def intDispatchManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.intDispatchPlans.map (fun p => p.1)

def fieldProjectionClaimExportNames
    (claims : List FieldProjectionClaim) : List String :=
  claims.map (fun c => c.exportName)

def fieldProjectionManifestPlanNames
    (manifest : AverCert.Schema.Manifest) : List String :=
  manifest.fieldProjectionPlans.map (fun p => p.1)

def compositionMemberPlanPairs
    (members : List CompositionMemberClaim) : List (String × CompositionRawPlan) :=
  members.map (fun member => (member.exportName, member.plan))

/-- The source `SymPlan`s carried by String.concat claims. These live in the
    manifest's common `symFragmentPlans` list; the byte-lowering-specific
    `StringConcatRawPlan` stays in `stringConcatPlans`. -/
def stringConcatClaimSymPlanPairs
    (claims : List StringConcatClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.symPlan))

def stringEqClaimSymPlanPairs
    (claims : List StringEqClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.symPlan))

def constructClaimSymPlanPairs
    (claims : List ConstructClaim) : List (String × SymRawPlan) :=
  claims.map (fun c => (c.exportName, c.symPlan))

/-- The checker-facing artifact data currently accepted by the Lean bridge.
    `symFragmentClaims` is the source-level expression surface. There is no
    artifact-level raw `ExprFragmentClaim`; representation plans are derived by
    the audited source encoder. Ordinary legacy obligations are still pinned by
    the verifier witness outside this artifact-level wrapper. -/
structure ArtifactData where
  wasmBytes          : AverCert.WasmSlice.ByteSeq
  manifest           : AverCert.Schema.Manifest
  symFragmentClaims  : List SymFragmentClaim
  stringEqClaims     : List StringEqClaim
  stringConcatClaims : List StringConcatClaim
  constructClaims    : List ConstructClaim
  recursionClaims    : List RecursionClaim
  mutualRecursionClaims : List MutualRecursionClaim
  verbatimClaims     : List VerbatimClaim
  intDispatchClaims  : List IntDispatchClaim
  fieldProjectionClaims : List FieldProjectionClaim
  compositionMembers : List CompositionMemberClaim
  compositionClaims  : List CompositionClaim

def acceptedSymFragments (artifact : ArtifactData) : Prop :=
  symFragmentClaimsAccepted artifact.wasmBytes artifact.symFragmentClaims

def acceptedStringConcatFragments (artifact : ArtifactData) : Prop :=
  stringConcatClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.stringConcatClaims

def acceptedStringEqFragments (artifact : ArtifactData) : Prop :=
  stringEqClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.stringEqClaims

def acceptedConstructFragments (artifact : ArtifactData) : Prop :=
  constructClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.constructClaims ∧
  (constructClaimExportNames artifact.constructClaims).Nodup

def acceptedRecursionFragments (artifact : ArtifactData) : Prop :=
  recursionClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.recursionClaims

def acceptedMutualRecursionFragments (artifact : ArtifactData) : Prop :=
  mutualRecursionClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.mutualRecursionClaims ∧
  mutualClaimsFormClosedSccs
    artifact.manifest
    artifact.mutualRecursionClaims

def acceptedVerbatimFragments (artifact : ArtifactData) : Prop :=
  verbatimClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.verbatimClaims

def acceptedIntDispatchFragments (artifact : ArtifactData) : Prop :=
  intDispatchClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.intDispatchClaims

def acceptedFieldProjectionFragments (artifact : ArtifactData) : Prop :=
  fieldProjectionClaimsAccepted
    artifact.wasmBytes
    artifact.manifest
    artifact.fieldProjectionClaims

def acceptedCompositionFragments (artifact : ArtifactData) : Prop :=
  compositionClaimsAccepted
    artifact.wasmBytes artifact.compositionMembers artifact.compositionClaims ∧
  compositionMembersCovered
    artifact.compositionMembers artifact.compositionClaims = true

def acceptedFragments (artifact : ArtifactData) : Prop :=
  acceptedSymFragments artifact ∧
  acceptedStringEqFragments artifact ∧
  acceptedStringConcatFragments artifact ∧
  acceptedConstructFragments artifact ∧
  acceptedRecursionFragments artifact ∧
  acceptedMutualRecursionFragments artifact ∧
  acceptedVerbatimFragments artifact ∧
  acceptedIntDispatchFragments artifact ∧
  acceptedFieldProjectionFragments artifact ∧
  acceptedCompositionFragments artifact

def expectedArtifactRoot : String :=
  "AverCert.Artifact.certificate"

def subjectMatchesArtifactRoot (artifact : ArtifactData) : Prop :=
  artifact.manifest.subject.artifactRoot = expectedArtifactRoot

def claimObligationExports (artifact : ArtifactData) : List String :=
  artifact.symFragmentClaims.map (fun c => c.obligation.export_) ++
  artifact.stringEqClaims.map (fun c => c.obligation.export_) ++
  artifact.stringConcatClaims.map (fun c => c.obligation.export_) ++
  artifact.constructClaims.map (fun c => c.obligation.export_) ++
  artifact.recursionClaims.map (fun c => c.obligation.export_) ++
  artifact.mutualRecursionClaims.map (fun c => c.obligation.export_) ++
  artifact.verbatimClaims.map (fun c => c.obligation.export_) ++
  artifact.intDispatchClaims.map (fun c => c.obligation.export_) ++
  artifact.fieldProjectionClaims.map (fun c => c.obligation.export_) ++
  artifact.compositionClaims.map (fun c => c.obligation.export_)

def claimObligations (artifact : ArtifactData) : List Obligation :=
  artifact.symFragmentClaims.map (fun c => c.obligation) ++
  artifact.stringEqClaims.map (fun c => c.obligation) ++
  artifact.stringConcatClaims.map (fun c => c.obligation) ++
  artifact.constructClaims.map (fun c => c.obligation) ++
  artifact.recursionClaims.map (fun c => c.obligation) ++
  artifact.mutualRecursionClaims.map (fun c => c.obligation) ++
  artifact.verbatimClaims.map (fun c => c.obligation) ++
  artifact.intDispatchClaims.map (fun c => c.obligation) ++
  artifact.fieldProjectionClaims.map (fun c => c.obligation) ++
  artifact.compositionClaims.map (fun c => c.obligation)

def claimObligationsInManifest
    (manifestObligations : List Obligation) : List Obligation → Prop
  | [] => True
  | obligation :: rest =>
      manifestObligations.find?
        (fun o => o.export_ = obligation.export_) = some obligation ∧
      claimObligationsInManifest manifestObligations rest

def fragmentClaimObligationsInManifest (artifact : ArtifactData) : Prop :=
  claimObligationsInManifest
    artifact.manifest.obligations
    (claimObligations artifact)

def claimsMatchManifest (artifact : ArtifactData) : Prop :=
  match symFragmentClaimEncodedPlanPairs artifact.symFragmentClaims with
  | some encodedSymExprPlans =>
      symFragmentClaimPlanPairs artifact.symFragmentClaims ++
          stringEqClaimSymPlanPairs artifact.stringEqClaims ++
          stringConcatClaimSymPlanPairs artifact.stringConcatClaims ++
          constructClaimSymPlanPairs artifact.constructClaims =
          artifact.manifest.symFragmentPlans ∧
      stringEqClaimExportNames artifact.stringEqClaims =
          stringEqManifestPlanNames artifact.manifest ∧
      stringConcatClaimExportNames artifact.stringConcatClaims =
          stringConcatManifestPlanNames artifact.manifest ∧
      constructClaimExportNames artifact.constructClaims =
          constructManifestPlanNames artifact.manifest ∧
      encodedSymExprPlans = artifact.manifest.exprFragmentPlans ∧
      recursionClaimExportNames artifact.recursionClaims =
          recursionManifestPlanNames artifact.manifest ∧
      mutualRecursionClaimExportNames artifact.mutualRecursionClaims =
          mutualManifestPlanNames artifact.manifest ∧
      verbatimClaimExportNames artifact.verbatimClaims =
          verbatimManifestPlanNames artifact.manifest ∧
      intDispatchClaimExportNames artifact.intDispatchClaims =
          intDispatchManifestPlanNames artifact.manifest ∧
      fieldProjectionClaimExportNames artifact.fieldProjectionClaims =
          fieldProjectionManifestPlanNames artifact.manifest ∧
      compositionMemberPlanPairs artifact.compositionMembers =
          artifact.manifest.compositionPlans
  | none => False

end AverCert.AcceptedArtifact
