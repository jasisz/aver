-- Dependency-closed Lean-side artifact acceptance helpers.
--
-- These predicates pin checked plans and Wasm bindings to the
-- `Schema.Obligation` fields used by the final certificate theorem.
import CertPrelude
import SchemaCore
import ArithTemplateDerisk
import PlanCheck
import PlanLower
import PlanBytes
import ExprFragmentAccepted
import WasmSlice
import CertDecode

namespace AverCert.AcceptedArtifact
open AverCert.Schema
open CertPrelude

/-- Select the first plan attached to an export name.  Every manifest plan
family uses this exact lookup; keeping it once prevents family-specific copies
from drifting in name comparison or first-match behaviour. -/
def namedPlanForExport {Plan : Type u} (exportName : String) :
    List (String × Plan) → Option Plan
  | [] => none
  | (name, plan) :: rest =>
      if name == exportName then some plan
      else namedPlanForExport exportName rest

/-- Canonical locals count declared by `lowerExprFragmentBodyBytes`: every
    accepted expression fragment has one carrier scratch local. -/
def exprFragmentNLocals (_plan : ExprFragmentRawPlan) : Nat := 1

/-- Artifact-level acceptance for one expression-fragment export. The body,
    canonical code-entry bytes, and function binding are witnesses to the
    audited Lean predicate `ExprFragmentAccepted.accepted`, existentially
    quantified rather than accepted as external parameters. -/
def exprFragmentPlanAccepted
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (plan : ExprFragmentRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
  obligation.carrier = carrier ∧
  ∃ body codeEntry binding,
    AverCert.ExprFragmentAccepted.accepted
      modBytes modLen exportNameBytes carrier plan body codeEntry binding ∧
    AverCert.WasmSlice.exprFragmentFuncTypeMatches
      modBytes modLen binding.typeIdx carrier plan.params plan.result = true ∧
    AverCert.WasmSlice.exprFragmentNominalTypesMatch
      modBytes modLen binding.typeIdx carrier plan = true ∧
    obligation.self = binding.funcIdx ∧
    obligation.code binding.funcIdx =
      some { arity := plan.params.length,
             nlocals := exprFragmentNLocals plan, body := body }

/-- Whether one representation-level fragment type names the Int carrier.
    Every `FragTy` constructor is listed: a new one makes this match
    non-exhaustive and stops the wall building, rather than silently defaulting
    to "does not name the carrier". -/
def fragTyIsIntCarrier : FragTy → Bool
  | .intCarrier => true
  | .f64 | .boolI32 | .i64 | .rawI32 | .ref | .adtRef => false

/-- Does any node of this block — or of any block nested inside it — carry the
    Int-carrier type?

    Every `FragNodeKind` constructor is listed explicitly, so a future
    block-carrying node cannot slip past the walk unvisited: adding one makes
    this match non-exhaustive and the wall stops building. Running out of fuel
    answers `true` (REQUIRE the binding), never `false`; the walk is used to
    decide whether a constraint applies, so its unreachable case must fail
    closed. -/
def fragBlockMentionsIntCarrierFuel : Nat → FragBlock → Bool
  | 0, _ => true
  | fuel + 1, block =>
      block.nodes.any fun node =>
        fragTyIsIntCarrier node.ty ||
          match node.kind with
          | .ifElse _ thenBlock elseBlock =>
              fragBlockMentionsIntCarrierFuel fuel thenBlock ||
                fragBlockMentionsIntCarrierFuel fuel elseBlock
          | .local _ | .constBool _ | .constI64 _ | .constI32 _
          | .constF64Bits _ | .structGet _ _ | .structGetUser _ _ _
          | .refIsNull _ | .prim _ _ | .hostCall _ _ _ | .selfCall _ _ _
          | .vectorGetOrDefault _ _ _ _ => false

/-- Whether an encoded fragment plan names the Int carrier ANYWHERE. A
    `FragTy` occurs in exactly three positions of an `ExprFragmentRawPlan` —
    the parameter list, the result, and `FragNode.ty` of every node in the body
    and in every nested block — and this visits all three. -/
def fragPlanMentionsIntCarrier (plan : ExprFragmentRawPlan) : Bool :=
  plan.params.any fragTyIsIntCarrier ||
    fragTyIsIntCarrier plan.result ||
    fragBlockMentionsIntCarrierFuel AverCert.PlanCheck.maxFuel plan.body

/-- When a source-fragment claim must present the byte-derived carrier index.
    Both triggers are DERIVED from the claim's own data — the encoded plan and
    the host table — never from a hand-listed set of families:

    * the encoded plan names `.intCarrier` anywhere, or
    * the host table is non-empty.

    Keying on the host table alone was unsound. `StandardFace.fragment`'s
    `domRepr` is `args = FragParams.encodeArgs carrier params values`, and
    `FragTy.encodeArg` sends an `.intCarrier` parameter to
    `CertPrelude.carrierSmall carrier value` — the CONCRETE three-field struct
    `structv carrier [i64v k, null, i32v 0]` at the claimed index. A generic
    Int fragment (`genericFragmentAllowed` forbids `.adtRef` parameters and
    `.adtRef`/`.intCarrier` RESULTS, but admits `.intCarrier` PARAMETERS; the
    Int-versus-constant comparison family lowers without consulting any host
    role) therefore models that layout while citing no role at all, so an
    empty table exempted a carrier-sensitive face.

    The residual permissive case is exactly "no `.intCarrier` in the encoded
    plan AND no role cited". There the claimed index cannot reach the face's
    meaning: `FragParams.encodeArgs`/`FragTy.resultRepr` mention `carrier` only
    on `.intCarrier`, the remaining representations (`boolRepr`,
    `floatBitsRepr`, `verbatimRepr`) and the projection face's
    `vs = [.structv structIdx [p.1, p.2]]` discard the `CarrierSpec` argument,
    and `host = emptyHost` presents no `boxRef carrier` slot. That case is what
    keeps projection and float/string-boundary fragments certifiable in
    carrierless modules.

    Interior node types are included even though only parameters and the result
    reach the face; the walk is total and conservative by construction, which
    is the side to err on. -/
def symFragmentCarrierBindingRequired
    (hostTable : List (HostRole × Nat)) (plan : ExprFragmentRawPlan) : Bool :=
  !hostTable.isEmpty || fragPlanMentionsIntCarrier plan

/-- Byte-derived carrier binding for a source fragment. When the claim's own
    data requires it (`symFragmentCarrierBindingRequired`), the claimed index
    must be the one the module's own type section decodes to:
    `CertDecode.carrierState` (three-state, fail-closed) must return
    `some (some carrier)`. A byte-provably carrierless module (`some none`) and
    a module whose type section does not decode (`none`) both reject.

    Because `CertDecode.TypeEntry.isCarrier` admits only a three-field struct
    whose field 0 is `i64` and whose field 2 is `i32`, this equality also pins
    the field COUNT of the type the `carrierSmall` layout is asserted at — a
    claim naming a four-field struct is rejected here rather than certifying a
    theorem quantified over states the module's type section forbids.

    This binding and the declared-type pin beside it are COMPLEMENTARY, not
    alternatives. `hostTableFuncTypesMatch` compares each helper's declared
    function type against the CLAIMED carrier, so on this family alone it was
    circular (the expr-fragment carrier is claim data bound to no decoder —
    see the scope note at `decodedStrictCarrierIndex`): a producer that
    declares the box helper AT a fake supertype and claims that same fake
    index satisfies the pin, while the template-pinned box BODY still builds
    structs at the real byte-derived carrier — the face's `boxRef` would be a
    fiction. The equality here removes the free reference point (catching
    "both consistently fake"); the declared-type pin then catches "claimed
    carrier right, helper type wrong", which no carrier equality can see. -/
def symFragmentCarrierBound
    (modBytes modLen carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (plan : ExprFragmentRawPlan) : Bool :=
  if symFragmentCarrierBindingRequired hostTable plan then
    CertDecode.carrierState modBytes modLen == some (some carrier)
  else
    true

/-- Artifact-level acceptance for one source-level symbolic fragment export.
    The source plan is still untrusted data: the audited checker/encoder must
    accept it and produce the representation-level expr-fragment plan before
    the existing byte-origin predicate is allowed to run.

    Two conjuncts of this predicate constrain the CARRIER REFERENCE POINT the
    proof faces are stated over: `symFragmentCarrierBound` forces the claimed
    index to be the decoded one whenever the encoded plan or the host table can
    make a face read it, and `hostTableFuncTypesMatch` forces every cited
    helper's DECLARED function type to be the one its role fixes over that
    index. Neither subsumes the other and both live here.

    The rest of the host-helper pinning lives in OTHER predicates and is not
    duplicated here: the role-to-index binding against the decoded role table
    is `StandardFace.hostTableBound` (whole-module face checking), and the
    helper BODY equality against the audited templates is `arithRoleCheck` /
    `arithTableCheck` further down this file. -/
def symFragmentPlanAccepted
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (structTable : List (String × Nat))
    (plan : SymRawPlan)
    (obligation : Obligation) : Prop :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      hostTable structTable plan with
  | some exprPlan =>
      symFragmentCarrierBound modBytes modLen carrier hostTable exprPlan = true ∧
      AverCert.WasmSlice.hostTableFuncTypesMatch
        modBytes modLen carrier hostTable = true ∧
      exprFragmentPlanAccepted
        modBytes modLen exportNameBytes exportName carrier exprPlan obligation
  | none => False

/-- One source-level symbolic fragment claim inside an artifact certificate.
    `hostTable`/`structTable` are representation context (host-role and
    struct-type indices), not part of the source plan.

    What a wrong table costs depends on whether the plan CITES it. A table
    entry the encoder actually resolves — a `hostCall` role, the fused
    vector-read helpers, a `projectField`/`tagMatch` struct name — is copied
    into the encoded plan and then into the lowered bytes, so a wrong entry
    yields a code entry the module does not contain and the byte gate rejects
    it. A table entry no node cites is NOT bound that way: for a role-free plan
    the byte gate never reads the table at all. Those entries are constrained
    only by the conjuncts stated at `symFragmentPlanAccepted`
    (`symFragmentCarrierBound`, `hostTableFuncTypesMatch`) and, at
    whole-module level, by `StandardFace.hostTableBound` against the decoded
    role table. -/
structure SymFragmentClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Nat
  hostTable       : List (HostRole × Nat)
  structTable     : List (String × Nat)
  plan            : SymRawPlan
  obligation      : Obligation

def symFragmentClaimAccepted
    (modBytes modLen : Nat)
    (claim : SymFragmentClaim) : Prop :=
  symFragmentPlanAccepted
    modBytes modLen
    claim.exportNameBytes
    claim.exportName
    claim.carrier
    claim.hostTable
    claim.structTable
    claim.plan
    claim.obligation

/-- Canonical locals count declared by `lowerStringConcatBodyBytes`: one carrier
    scratch local in a module that has an Int carrier struct, none in a module
    that provably has not. Reading the SAME `carrier?` the byte lowering reads
    keeps the semantic frame and the emitted prelude from ever disagreeing, and
    `decodedCodeAt` equates this count with the locals vector decoded from the
    real code section — so the number is byte-checked, not asserted. -/
def stringConcatNLocals (carrier? : Option Nat) : Nat :=
  match carrier? with
  | some _ => 1
  | none => 0

/-- Canonical whole-host builders for the two byte-exact string roles. -/
def stringEqCanonicalHost (funcIdx : Nat) :
    (List WVal → Option WVal) → (List WVal → Option WVal) →
    (List WVal → Option WVal) → (List WVal → Option WVal) →
    (Nat → List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) → HostTbl :=
  fun _add _sub _mul stringEq _stringConcat _toIndex _cmp _eq fn =>
    if fn = funcIdx then some (2, stringEq) else none

def stringConcatCanonicalHost (funcIdx resultTy : Nat) :
    (List WVal → Option WVal) → (List WVal → Option WVal) →
    (List WVal → Option WVal) → (List WVal → Option WVal) →
    (Nat → List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) → HostTbl :=
  fun _add _sub _mul _stringEq stringConcat _toIndex _cmp _eq fn =>
    if fn = funcIdx then some (1, stringConcat resultTy) else none

/-- Artifact-level acceptance for one String.concat export. The raw plan carries
    source-level chunks plus the encoder's data-index binding; the audited Lean
    lowerers rebuild both the semantic `WInstr` body and exact code-entry bytes,
    and the Wasm slicer binds those bytes to the exported function.

    `carrier` is the declared CARRIER STATE of the module, not a plan parameter,
    and the second conjunct pins it to `CertDecode.carrierState` recomputed from
    the same bytes. That single equality decides which locals prelude the byte
    lowering below synthesizes and which locals count the semantic frame carries:
    a module whose type section holds an Int carrier struct can present only
    `some idx` (for that exact index), a module whose type section decodes and
    holds none can present only `none`, and a module whose type section does not
    decode can present neither. The producer has no third option and no choice
    between the two, which is the whole point of stating the template selector
    against a decoder instead of leaving it as claim data.

    It is also STRICTLY stronger than the `obligation.carrier = carrier` it
    replaces: that conjunct only tied one declared field to another, whereas the
    pair below ties the claim to the bytes, and `decodedCarrierIndex` ties the
    obligation to the same decoded state. -/
def stringConcatPlanAccepted
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Option Nat)
    (resultTy containerTy concatFuncIdx : Nat)
    (stringHostRoles : List (Nat × CertDecode.StringHost.Role))
    (symPlan : SymRawPlan)
    (plan : StringConcatRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
  CertDecode.carrierState modBytes modLen = some carrier ∧
  obligation.carrier = carrier.getD 0 ∧
  stringHostRoles.contains (concatFuncIdx, .concat) = true ∧
  obligation.host = stringConcatCanonicalHost concatFuncIdx resultTy ∧
  ∃ body codeEntry binding,
    AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
    AverCert.PlanCheck.stringConcatPlanMatchesSymRawPlan symPlan plan = true ∧
    AverCert.PlanCheck.checkStringConcatRawPlan plan = true ∧
    AverCert.PlanLower.lowerStringConcatBody
      resultTy containerTy concatFuncIdx plan = some body ∧
    AverCert.PlanBytes.lowerStringConcatCodeEntry
      carrier resultTy containerTy concatFuncIdx plan = some codeEntry ∧
    AverCert.WasmSlice.exactFuncBindingForExport
      modBytes modLen exportNameBytes codeEntry = some binding ∧
    obligation.self = binding.funcIdx ∧
    obligation.code binding.funcIdx =
      some { arity := 1, nlocals := stringConcatNLocals carrier, body := body }

def stringEqPlanAccepted
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier stringTy stringEqFuncIdx : Nat)
    (stringHostRoles : List (Nat × CertDecode.StringHost.Role))
    (symPlan : SymRawPlan)
    (plan : StringEqRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    stringHostRoles.contains (stringEqFuncIdx, .eq) = true ∧
    obligation.host = stringEqCanonicalHost stringEqFuncIdx ∧
    AverCert.PlanCheck.checkSymRawPlan symPlan = true ∧
    AverCert.PlanCheck.stringEqPlanMatchesSymRawPlan symPlan plan = true ∧
    AverCert.PlanCheck.checkStringEqRawPlan plan = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerStringEqBody stringTy stringEqFuncIdx plan = some body ∧
      AverCert.PlanBytes.lowerStringEqCodeEntry carrier stringTy stringEqFuncIdx plan =
        some codeEntry ∧
      AverCert.WasmSlice.exactFuncBindingForExport
        modBytes modLen exportNameBytes codeEntry = some binding ∧
      binding.funcIdx = obligation.self ∧
      obligation.code binding.funcIdx =
        some { arity := 1, nlocals := 2, body := body }

def stringEqPlanForExport
    (exportName : String) : List (String × StringEqRawPlan) →
    Option StringEqRawPlan :=
  namedPlanForExport exportName

def stringConcatPlanForExport
    (exportName : String) : List (String × StringConcatRawPlan) →
    Option StringConcatRawPlan :=
  namedPlanForExport exportName

def constructPlanForExport
    (exportName : String) : List (String × ConstructRawPlan) →
    Option ConstructRawPlan :=
  namedPlanForExport exportName

/-- One source-level String.concat claim inside an artifact certificate.
    `carrier` is the module's carrier STATE — `none` for a module with no Int
    carrier struct — and is pinned to `CertDecode.carrierState` by
    `stringConcatPlanAccepted`. -/
structure StringConcatClaim where
  exportNameBytes : AverCert.WasmSlice.ByteSeq
  exportName      : String
  carrier         : Option Nat
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
    (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest)
    (claim : StringEqClaim) : Prop :=
  match stringEqPlanForExport claim.exportName manifest.stringEqPlans with
  | some plan =>
      stringEqPlanAccepted
        modBytes modLen
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.stringTy
        claim.stringEqFuncIdx
        manifest.subject.stringHostRoles
        claim.symPlan
        plan
        claim.obligation
  | none => False

def stringConcatClaimAccepted
    (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest)
    (claim : StringConcatClaim) : Prop :=
  match stringConcatPlanForExport claim.exportName manifest.stringConcatPlans with
  | some plan =>
      stringConcatPlanAccepted
        modBytes modLen
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.resultTy
        claim.containerTy
        claim.concatFuncIdx
        manifest.subject.stringHostRoles
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

def constructPlanAccepted
    (modBytes modLen : Nat)
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
      AverCert.WasmSlice.exactFuncBindingForExport
        modBytes modLen exportNameBytes codeEntry = some binding ∧
      binding.funcIdx = obligation.self ∧
      (isListConstructSymPlan symPlan = false ∨
        AverCert.WasmSlice.listConstructStructTypeMatches
          modBytes modLen structIdx elemTy = true) ∧
      (isListConstructSymPlan symPlan = false ∨
        AverCert.WasmSlice.listConstructFuncTypeMatches
          modBytes modLen binding.typeIdx plan.arity structIdx elemTy = true) ∧
      obligation.code binding.funcIdx =
        some { arity := plan.arity, nlocals := 1, body := body }

def constructClaimAccepted
    (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest)
    (claim : ConstructClaim) : Prop :=
  match constructPlanForExport claim.exportName manifest.constructPlans with
  | some plan =>
      constructPlanAccepted
        modBytes modLen
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
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (plan : RecursionRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkRecursionRawPlan plan = true ∧
    (match obligation.policy, obligation.termination? with
     | .simulatesModel, none => true
     | .simulatesModelTotally, some witness => AverCert.Schema.checkTerm plan witness
     | _, _ => false) = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerRecursionBody carrier plan = some body ∧
      AverCert.PlanBytes.lowerRecursionCodeEntry carrier plan = some codeEntry ∧
      AverCert.WasmSlice.exactFuncBindingForExport
        modBytes modLen exportNameBytes codeEntry = some binding ∧
      binding.funcIdx = obligation.self ∧
      AverCert.PlanCheck.checkRecursionPlanShape binding.funcIdx hostTable
        obligation.totalityRole plan = true ∧
      AverCert.WasmSlice.funcTypeMatches
        modBytes modLen binding.typeIdx plan.params.length carrier = true ∧
      AverCert.WasmSlice.hostTableFuncTypesMatch
        modBytes modLen carrier hostTable = true ∧
      obligation.code binding.funcIdx =
        some { arity := plan.params.length,
               nlocals := recursionNLocals plan, body := body }

def recursionPlanForExport
    (exportName : String) : List (String × RecursionRawPlan) →
    Option RecursionRawPlan :=
  namedPlanForExport exportName

def recursionClaimAccepted
    (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest)
    (claim : RecursionClaim) : Prop :=
  match recursionPlanForExport claim.exportName manifest.recursionPlans with
  | some plan =>
      recursionPlanAccepted
        modBytes modLen
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
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (memberSet : List Nat)
    (hostTable : List (HostRole × Nat))
    (plan : MutualRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    obligation.totalityRole = .addSub ∧
    AverCert.PlanCheck.checkMutualRawPlan plan = true ∧
    (match obligation.policy, obligation.termination? with
     | .simulatesModel, none => true
     | .simulatesModelTotally, some witness => AverCert.Schema.checkTermMutual plan witness
     | _, _ => false) = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerMutualBody carrier plan = some body ∧
      AverCert.PlanBytes.lowerMutualCodeEntry carrier plan = some codeEntry ∧
      AverCert.WasmSlice.exactFuncBindingForExport
        modBytes modLen exportNameBytes codeEntry = some binding ∧
      binding.funcIdx = obligation.self ∧
      AverCert.PlanCheck.checkMutualPlanShape memberSet hostTable plan = true ∧
      AverCert.WasmSlice.funcTypeMatches
        modBytes modLen binding.typeIdx plan.params.length carrier = true ∧
      AverCert.WasmSlice.hostTableFuncTypesMatch
        modBytes modLen carrier hostTable = true ∧
      obligation.code binding.funcIdx =
        some { arity := plan.params.length,
               nlocals := mutualNLocals plan, body := body }

def mutualPlanForExport
    (exportName : String) : List (String × MutualRawPlan) →
    Option MutualRawPlan :=
  namedPlanForExport exportName

def mutualRecursionClaimAccepted
    (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest)
    (claim : MutualRecursionClaim) : Prop :=
  match mutualPlanForExport claim.exportName manifest.mutualPlans with
  | some plan =>
      mutualPlanAccepted
        modBytes modLen
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
    (modBytes modLen : Nat) : VerbatimLeaf → Bool
  | .arrayNewData _ dataIdx bytes =>
      match AverCert.WasmSlice.dataSegmentBytes modBytes modLen dataIdx with
      | some contents => contents == bytes
      | none => false
  | _ => true

/-- Every `arrayNewData` leaf in a verbatim dispatch has a byte-bound payload.
    The canonical code-entry lowering pins each string literal's data-segment
    INDEX and copied LENGTH but not its CONTENTS, so without this an equal-length
    payload substitution keeps the code entry byte-identical while changing what
    the plan (hence the model) claims. -/
def verbatimPayloadsBound
    (modBytes modLen : Nat) : VerbatimDispatch → Bool
  | .leaf l => verbatimLeafPayloadBound modBytes modLen l
  | .test _ hit rest =>
      verbatimLeafPayloadBound modBytes modLen hit &&
        verbatimPayloadsBound modBytes modLen rest

/-- Canonical locals count declared by `lowerVerbatimLocalsBytes`: projecting
    plans declare the field scratch, scrutinee, and carrier locals; all other
    plans declare only the scrutinee and carrier locals. -/
def verbatimNLocals (plan : VerbatimRawPlan) : Nat :=
  if AverCert.PlanCheck.dispatchHasProjection plan.body then 3 else 2

/-- Artifact-level acceptance for one verbatim `ref.test`-dispatch export. The
    `VerbatimRawPlan` is checked against the canonical local count, lowered to the
    match `WInstr` body and its exact code-entry bytes, and those bytes are bound
    to the exported function by name. There are no host/self calls to tie,
    so the code entry is nearly the whole binding — but the code entry alone does
    NOT determine the export's meaning: it omits the function SIGNATURE (a second
    nominal-root parameter leaves the locals + body bytes identical) and the
    `array.new_data` PAYLOAD CONTENTS (only the segment index and length are
    encoded). Two further conjuncts close both holes in-kernel:
    `verbatimFuncTypeMatches` forces the byte-derived type-section entry to have
    one nullable concrete root parameter and the ref-null or f64 result declared
    by `plan.resultSig`, and `verbatimPayloadsBound`
    forces every literal's claimed bytes to equal the byte-pinned data segment. A
    body byte-noisier than the canonical dispatch lowering still fails the
    byte-equality gate. The member index is tied to the obligation through
    `binding.funcIdx = obligation.self`. -/
def verbatimPlanAccepted
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (plan : VerbatimRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkVerbatimPlan (verbatimNLocals plan) plan = true ∧
    ∃ codeEntry binding,
      AverCert.PlanBytes.lowerVerbatimCodeEntry carrier plan = some codeEntry ∧
      AverCert.WasmSlice.exactFuncBindingForExport
        modBytes modLen exportNameBytes codeEntry = some binding ∧
      binding.funcIdx = obligation.self ∧
      AverCert.WasmSlice.verbatimFuncTypeMatches
        modBytes modLen binding.typeIdx plan.resultSig = true ∧
      verbatimPayloadsBound modBytes modLen plan.body = true ∧
      obligation.code binding.funcIdx =
        some { arity := 1, nlocals := verbatimNLocals plan,
               body := AverCert.PlanLower.lowerVerbatimBody plan }

def verbatimPlanForExport
    (exportName : String) : List (String × VerbatimRawPlan) →
    Option VerbatimRawPlan :=
  namedPlanForExport exportName

def verbatimClaimAccepted
    (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest)
    (claim : VerbatimClaim) : Prop :=
  match verbatimPlanForExport claim.exportName manifest.verbatimPlans with
  | some plan =>
      verbatimPlanAccepted
        modBytes modLen
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
    This reuses the same contract functions the obligation host is built from
    (`boxRef` and the abstract add/sub slots of
    `Obligation.host`); it defines no new semantics. -/
def intDispatchCanonicalSlots
    (carrier : Nat) (add sub mul : List WVal → Option WVal) :
    List (HostRole × Nat) → HostTbl
  | [] => fun _ => none
  | (role, idx) :: rest => fun fn =>
      if fn = idx then
        some (match role with
          | .box => ((1 : Nat), boxRef carrier)
          | .add => ((2 : Nat), add)
          | .mul => ((2 : Nat), mul)
          | .sub => ((2 : Nat), sub)
          -- The Int-face dispatch grammar cites neither the to-index role nor
          -- either comparison role; a table entry for one of them wires a slot
          -- that always fails (trap-only). The arities are the real helper
          -- arities so the wiring stays honest about the shape it refuses.
          | .toIndex => ((1 : Nat), fun _ => none)
          | .cmp => ((2 : Nat), fun _ => none)
          | .eq => ((2 : Nat), fun _ => none))
      else intDispatchCanonicalSlots carrier add sub mul rest fn

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
    (Nat → List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) → HostTbl :=
  fun add sub mul _stringEq _stringConcat _toIndex _cmp _eq =>
    intDispatchCanonicalSlots carrier add sub mul hostTable

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
    SIGNATURE (a second nominal-root parameter leaves the locals + body bytes
    identical), so `verbatimFuncTypeMatches` additionally forces the
    byte-derived type-section entry to be the unary nominal-ref →
    `[(ref null carrier)]` signature — the same shape check the
    verbatim family uses, here with the Int carrier as the result heap type.
    The function index is tied to the obligation through
    `binding.funcIdx = obligation.self`, and the obligation's code table must
    carry exactly the plan-lowered body WITH the canonical byte-derived locals
    count (`bindArmCount + 2`, exactly what the byte lowering declares in the
    locals vector — a const/nullary arm spills no per-arm local): an
    existentially-free `nlocals` would let an honest-bytes artifact claim a
    0-locals table whose body traps on its first `local.set`, making the
    partial-correctness obligation vacuously true. -/
def intDispatchPlanAccepted
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (plan : IntDispatchRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkIntDispatchRawPlan plan = true ∧
    AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true ∧
    AverCert.WasmSlice.hostTableFuncTypesMatch
      modBytes modLen carrier hostTable = true ∧
    obligation.host = intDispatchCanonicalHost carrier hostTable ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerIntDispatchBody hostTable plan = some body ∧
      AverCert.PlanBytes.lowerIntDispatchCodeEntry carrier hostTable plan =
        some codeEntry ∧
      AverCert.WasmSlice.exactFuncBindingForExport
        modBytes modLen exportNameBytes codeEntry = some binding ∧
      binding.funcIdx = obligation.self ∧
      AverCert.WasmSlice.verbatimFuncTypeMatches
        modBytes modLen binding.typeIdx (.refNull carrier) = true ∧
      obligation.code binding.funcIdx =
        some { arity := 1,
               nlocals := AverCert.PlanCheck.bindArmCount plan.body + 2,
               body := body }

def intDispatchPlanForExport
    (exportName : String) : List (String × IntDispatchRawPlan) →
    Option IntDispatchRawPlan :=
  namedPlanForExport exportName

def intDispatchClaimAccepted
    (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest)
    (claim : IntDispatchClaim) : Prop :=
  match intDispatchPlanForExport claim.exportName manifest.intDispatchPlans with
  | some plan =>
      intDispatchPlanAccepted
        modBytes modLen
        claim.exportNameBytes
        claim.exportName
        claim.carrier
        claim.hostTable
        plan
        claim.obligation
  | none => False

def fieldProjectionPlanForExport
    (exportName : String) : List (String × FieldProjectionRawPlan) →
    Option FieldProjectionRawPlan :=
  namedPlanForExport exportName

/-- Plan-backed acceptance for the bare bind/cast projection lowering. The
    exact three-local layout is part of the canonical code-entry encoder and is
    also pinned in the semantic code table. The type-section guards bind the
    checker-derived struct index/count/result reference to the selected module
    field and to the exported unary function signature. -/
def fieldProjectionPlanAccepted
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
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
    AverCert.WasmSlice.exactFuncBindingForExport
      modBytes modLen exportNameBytes codeEntry = some binding ∧
    AverCert.WasmSlice.projectionStructTypeMatches
      modBytes modLen structIdx fieldCount plan.fieldIdx resultTy = true ∧
    AverCert.WasmSlice.projectionFuncTypeMatches
      modBytes modLen binding.typeIdx structIdx resultTy = true ∧
    obligation.self = binding.funcIdx ∧
    obligation.code binding.funcIdx =
      some { arity := 1, nlocals := 3, body := body }

def fieldProjectionClaimAccepted
    (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest)
    (claim : FieldProjectionClaim) : Prop :=
  match fieldProjectionPlanForExport claim.exportName manifest.fieldProjectionPlans with
  | some plan =>
      fieldProjectionPlanAccepted modBytes modLen claim.exportNameBytes claim.exportName
        claim.carrier claim.structIdx claim.fieldCount claim.resultTy plan claim.obligation
  | none => False

/-! ### Cross-function composition: byte-bound member graph and closure -/

/-- Exact canonical locals count emitted by both composition byte lowerers. -/
def compositionNLocals (_plan : CompositionRawPlan) : Nat := 1

def compositionMemberBinding
    (modBytes modLen : Nat)
    (member : CompositionMemberClaim) : Option (String × Nat) :=
  match AverCert.WasmSlice.funcBindingForExport modBytes modLen member.exportNameBytes with
  | some binding => some (member.exportName, binding.funcIdx)
  | none => none

def compositionFuncTable
    (modBytes modLen : Nat) :
    List CompositionMemberClaim → Option (List (String × Nat))
  | [] => some []
  | member :: rest =>
      match compositionMemberBinding modBytes modLen member,
            compositionFuncTable modBytes modLen rest with
      | some binding, some bindings => some (binding :: bindings)
      | _, _ => none

def compositionMemberForName
    (name : String) : List CompositionMemberClaim → Option CompositionMemberClaim
  | [] => none
  | member :: rest =>
      if member.exportName == name then some member
      else compositionMemberForName name rest

def compositionMemberPlanAccepted
    (modBytes modLen : Nat)
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
    AverCert.WasmSlice.exactFuncBindingForExport
      modBytes modLen member.exportNameBytes codeEntry = some binding ∧
    AverCert.WasmSlice.funcTypeMatches modBytes modLen binding.typeIdx 1 carrier = true ∧
    AverCert.WasmSlice.hostTableFuncTypesMatch
      modBytes modLen carrier hostTable = true ∧
    obligation.code binding.funcIdx =
      some { arity := 1, nlocals := compositionNLocals member.plan, body := body }

def compositionNamedMembersAccepted
    (modBytes modLen : Nat)
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
            modBytes modLen carrier hostTable funcTable obligation member ∧
          compositionNamedMembersAccepted
            modBytes modLen carrier hostTable funcTable obligation members rest
      | none => False

def compositionPlanCallees (plan : CompositionRawPlan) : List String :=
  match plan.shape with
  | .selfSum => []
  | .chain callees => callees

def stringListNodup (xs : List String) : Bool :=
  AverCert.WasmSlice.indexedNodup xs

def stringListSetEq (xs ys : List String) : Bool :=
  AverCert.WasmSlice.indexedNodup xs &&
    AverCert.WasmSlice.indexedNodup ys &&
    AverCert.WasmSlice.indexedSetEq xs ys

def compositionEdges
    (members : List CompositionMemberClaim) : List (String × List String) :=
  members.map (fun member => (member.exportName, compositionPlanCallees member.plan))

/-- One worklist step on an already-indexed composition graph. Missing targets
    fail closed; undiscovered callee edges are discovered exactly once. -/
def compositionReachStep
    (edgeIndex : Std.TreeMap String (List String))
    (memberNames : Std.TreeSet String) :
    Nat → List String → Std.TreeSet String → Std.TreeSet String → List String →
      Option (List String)
  | 0, _, _, _, _ => none
  | _fuel + 1, seen, seenSet, queuedSet, [] => some seen
  | fuel + 1, seen, seenSet, queuedSet, name :: work =>
      if seenSet.contains name then
        compositionReachStep edgeIndex memberNames fuel seen seenSet queuedSet work
      else
        match edgeIndex.get? name with
        | none => none
        | some callees =>
            if List.all callees (fun callee => memberNames.contains callee) then
              let (nextWork, nextQueuedSet) :=
                List.foldl
                  (fun state callee =>
                    let work := state.1
                    let discovered := state.2
                    if seenSet.contains callee || discovered.contains callee then
                      state
                      else
                        (callee :: work, discovered.insert callee))
                  (work, queuedSet) callees
              compositionReachStep edgeIndex memberNames fuel
                (name :: seen) (seenSet.insert name) nextQueuedSet nextWork
            else none

def compositionReachClosure
    (edges : List (String × List String)) : Nat → List String → Option (List String)
  | 0, reached => some reached
  | fuel, reached =>
      let edgeIndex := edges.foldl (fun index edge =>
        index.insert edge.1 edge.2) Std.TreeMap.empty
      let memberNameSet := AverCert.WasmSlice.orderedSet (edges.map (fun edge => edge.1))
      let queuedSet := AverCert.WasmSlice.orderedSet reached
      match compositionReachStep edgeIndex memberNameSet fuel [] Std.TreeSet.empty queuedSet reached with
      | some reachedSet => some (reachedSet.reverse)
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
    (modBytes modLen : Nat)
    (members : List CompositionMemberClaim)
    (claim : CompositionClaim) : Prop :=
  claim.obligation.export_ = claim.exportName ∧
  claim.obligation.carrier = claim.carrier ∧
  AverCert.PlanCheck.checkCompositionHostTable claim.hostTable = true ∧
  AverCert.WasmSlice.hostTableFuncTypesMatch
    modBytes modLen claim.carrier claim.hostTable = true ∧
  claim.obligation.host = intDispatchCanonicalHost claim.carrier claim.hostTable ∧
  match compositionFuncTable modBytes modLen members with
  | some funcTable =>
      compositionClosureBound claim.exportName claim.memberNames members funcTable = true ∧
      (match AverCert.PlanLower.compositionFuncIdx? funcTable claim.exportName with
       | some rootIdx => claim.obligation.self = rootIdx
       | none => False) ∧
      compositionNamedMembersAccepted modBytes modLen claim.carrier claim.hostTable
        funcTable claim.obligation members claim.memberNames
  | none => False

/-- Shared acceptance spine for every claim family. Family-specific views
    choose the checked predicate; the conjunction shape seen by consumers is
    unchanged. -/
def allClaims (accept : Claim → Prop) : List Claim → Prop
  | [] => True
  | claim :: rest => accept claim ∧ allClaims accept rest

/-- Recover one accepted claim from the shared conjunction spine.  Keeping this
    structural lemma beside `allClaims` gives every family discharge the same
    proof instead of maintaining one private copy per claim kind. -/
theorem allClaims_of_mem (accept : Claim → Prop)
    (claims : List Claim) (hAll : allClaims accept claims)
    (claim : Claim) (hMem : claim ∈ claims) : accept claim := by
  induction claims with
  | nil => simp at hMem
  | cons head tail ih =>
      simp only [allClaims] at hAll
      simp only [List.mem_cons] at hMem
      rcases hAll with ⟨hHead, hTail⟩
      rcases hMem with rfl | hMem
      · exact hHead
      · exact ih hTail hMem

def symFragmentClaimsAccepted (modBytes modLen : Nat)
    (claims : List SymFragmentClaim) : Prop :=
  allClaims (symFragmentClaimAccepted modBytes modLen) claims

/-- Aggregate source-level String.concat witness acceptance for one artifact's
    string claim list. -/
def stringConcatClaimsAccepted (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest) (claims : List StringConcatClaim) : Prop :=
  allClaims (stringConcatClaimAccepted modBytes modLen manifest) claims

/-- Aggregate source-level String.eq witness acceptance for one artifact's
    string equality claim list. -/
def stringEqClaimsAccepted (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest) (claims : List StringEqClaim) : Prop :=
  allClaims (stringEqClaimAccepted modBytes modLen manifest) claims

/-- Aggregate source-level constructor witness acceptance for one artifact's
    constructor claim list. -/
def constructClaimsAccepted (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest) (claims : List ConstructClaim) : Prop :=
  allClaims (constructClaimAccepted modBytes modLen manifest) claims

/-- Aggregate fuel-recursion witness acceptance for one artifact's recursion
    claim list. -/
def recursionClaimsAccepted (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest) (claims : List RecursionClaim) : Prop :=
  allClaims (recursionClaimAccepted modBytes modLen manifest) claims

/-- Aggregate mutual-recursion witness acceptance for one artifact's mutual
    claim list. -/
def mutualRecursionClaimsAccepted (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest) (claims : List MutualRecursionClaim) : Prop :=
  allClaims (mutualRecursionClaimAccepted modBytes modLen manifest) claims

/-- Aggregate verbatim `ref.test`-dispatch acceptance for one artifact's verbatim
    claim list. -/
def verbatimClaimsAccepted (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest) (claims : List VerbatimClaim) : Prop :=
  allClaims (verbatimClaimAccepted modBytes modLen manifest) claims

/-- Aggregate Int-face `ref.test`-dispatch acceptance for one artifact's
    int-dispatch claim list. -/
def intDispatchClaimsAccepted (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest) (claims : List IntDispatchClaim) : Prop :=
  allClaims (intDispatchClaimAccepted modBytes modLen manifest) claims

def fieldProjectionClaimsAccepted (modBytes modLen : Nat)
    (manifest : AverCert.Schema.Manifest) (claims : List FieldProjectionClaim) : Prop :=
  allClaims (fieldProjectionClaimAccepted modBytes modLen manifest) claims

/-- Aggregate composition-root acceptance. All roots share the artifact-wide
    unique member table, while each root independently re-derives its reachable
    closure and pins every reachable body into its own shared `CodeTbl`. -/
def compositionClaimsAccepted (modBytes modLen : Nat)
    (members : List CompositionMemberClaim) (claims : List CompositionClaim) : Prop :=
  allClaims (compositionClaimAccepted modBytes modLen members) claims

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
byte-pinned plan (joined to the export by `exactFuncBindingForExport`). It then
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
def natListNodup (xs : List Nat) : Bool :=
  AverCert.WasmSlice.indexedNodup xs

/-- Set equality via mutual containment plus equal length: rejects extras,
    omissions AND duplicates (a duplicate makes the lengths differ). -/
def natListSetEq (xs ys : List Nat) : Bool :=
  AverCert.WasmSlice.indexedNodup xs &&
    AverCert.WasmSlice.indexedNodup ys &&
    AverCert.WasmSlice.indexedSetEq xs ys

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

/-- Exact whole-module direct-call closure claimed by the producer and
    independently recomputed from the artifact bytes. `roots` are precisely the
    certified obligation function indices; every other admitted member is
    explicitly classified as a helper. -/
structure ClosureClaim where
  roots    : List Nat
  helpers  : List Nat
  admitted : List Nat

/-- The checker-facing artifact data currently accepted by the Lean bridge.
    `symFragmentClaims` is the source-level expression surface. There is no
    artifact-level raw `ExprFragmentClaim`; representation plans are derived by
    the audited source encoder. -/
structure ArtifactData where
  modBytes           : Nat
  modLen             : Nat
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
  closureFuel        : Nat
  closureClaim       : ClosureClaim

/-! ### In-kernel artifact binding

The source expression-fragment family retains its canonical plan-to-code-entry
byte equality. Every other accepted family is additionally tied to the full
profile decoder here: code-table entries, the carrier index, and (where the
family consumes them) struct-field counts are computed from `modBytes` by
`CertDecode`, then equated to the claim obligation already pinned into the
manifest. None of these equalities takes an externally supplied code, carrier,
or field literal. -/

def decodedCodeAt
    (modBytes modLen : Nat) (obligation : Obligation) (funcIdx : Nat) : Prop :=
  CertDecode.decodeCode modBytes modLen funcIdx = obligation.code funcIdx

def decodedCodeAtAll
    (modBytes modLen : Nat) (obligation : Obligation) : List Nat → Prop
  | [] => True
  | funcIdx :: rest =>
      decodedCodeAt modBytes modLen obligation funcIdx ∧
      decodedCodeAtAll modBytes modLen obligation rest

/-- Byte-derived binding of one obligation's carrier index, stated over the
    THREE states `CertDecode.carrierState` distinguishes rather than over a
    collapsed `Option Nat`:

    * `some (some idx)` — the module has an Int carrier struct, and the
      obligation must declare that exact index. Identical to the
      `decodeCarrier … = some obligation.carrier` this replaces, so nothing an
      existing certificate relies on moves.
    * `some none` — the type section decodes and holds no carrier struct. There
      is no index to bind, so the wall forces the reserved `0`. Writing the pin
      against `decodeCarrier` left this state with no admissible declaration at
      all, which is what made a carrierless module permanently uncertifiable.
    * `none` — the type section does not decode. No declaration is admissible,
      exactly as before.

    Matching on the state instead of mapping it through `Option.getD` matters:
    a collapsed reading sends both `some (some 0)` and `some none` to the same
    `some 0`, reintroducing at `0` precisely the ambivalence `carrierState` was
    introduced to remove at `none`. Both arms happen to force the same field
    today, but a family that later branches on the carrier state would inherit a
    decoder that cannot tell "the carrier is type 0" from "there is no carrier".

    SCOPE. This relaxation is admitted for the String.concat claim list ALONE
    (`decodedCarrierFreeClaims` below); every other NON-EXPRESSION-FRAGMENT
    family keeps the strict `decodedStrictCarrierIndex`, and the
    expression-fragment family is bound by neither (see
    `decodedStrictCarrierIndex`). The reserved index is only harmless for a family
    whose face provably never consults the `CarrierSpec`, and String.concat is
    the family this leg established that for: its `Dom`/`Cod` are `WVal`, its
    `domRepr` is `vs = [v]` and its `codRepr` is `verbatimRepr`, all of which
    discard the spec argument. Applying it uniformly would be unsound, and
    concretely so: `construct-v1`'s named face pins `HEq o.Dom Int` and
    `HEq o.domRepr (intArgDomRepr env.carrier)`, an INT-REPRESENTATION face, and
    it cites no arith role (`constructNamedFace` fixes `host = emptyHost`), so
    neither the arith-table carrier pin nor the role table constrains it. Under a
    uniform relaxation a module with no decodable carrier struct could carry a
    constructor claim whose representation face is stated over `CarrierSpec 0`
    while the module provably has no carrier struct at index `0` at all. -/
def decodedCarrierIndex
    (modBytes modLen : Nat) (obligation : Obligation) : Prop :=
  match CertDecode.carrierState modBytes modLen with
  | some (some idx) => obligation.carrier = idx
  | some none => obligation.carrier = 0
  | none => False

/-- The carrier binding every NON-EXPRESSION-FRAGMENT family but String.concat
    must satisfy: the module has a decodable Int-carrier struct and the
    obligation declares that exact index. A module with no carrier struct, or one
    whose type section does not decode, admits no claim in those families at all
    — which is the guarantee that held before the carrierless state was admitted
    anywhere, and the one that keeps a carrier-SENSITIVE face among them from
    ever being stated over an index the bytes do not license.

    The expression-fragment family is NOT among them and satisfies neither this
    binding nor `decodedCarrierIndex`: `symFragmentClaims` is deliberately absent
    from `decodedNonExprClaimFacts` (see the note there). What binds an
    `expr-fragment-v1` claim's carrier depends on whether the claim cites host
    roles. A ROLE-FREE claim is bound by `ExprFragmentAccepted.accepted` alone,
    purely by BYTE EQUALITY: the declared index is spliced into the synthesized
    locals prelude (`01 01 63 <carrier>`) and into every carrier-typed
    instruction immediate, and `WasmSlice.exactFuncBindingForExport` requires
    the result to equal the export's real code entry; the integer-family
    renders additionally pin the declared function type through
    `WasmSlice.funcTypeMatches`, which matches each `intCarrier` position
    against `(ref null carrier)`. Those equalities confirm the index against
    the CODE and the SIGNATURE but never against a decoder — a module can spell
    them out consistently at ANY index — which is exactly why a claim whose own
    data can make a face read the carrier (`symFragmentCarrierBindingRequired`:
    a non-empty `hostTable`, or an `.intCarrier` anywhere in the encoded plan)
    carries the additional decoder equality `symFragmentCarrierBound` inside
    `symFragmentPlanAccepted`: those faces model helpers or arguments at the
    claimed index, so that index must be the one `CertDecode.carrierState`
    derives from the bytes. Do not describe this
    file's decoded-claim bindings as covering "every family": they cover every
    family whose claims appear below. -/
def decodedStrictCarrierIndex
    (modBytes modLen : Nat) (obligation : Obligation) : Prop :=
  CertDecode.decodeCarrier modBytes modLen = some obligation.carrier

/-- Byte-derived facts every non-expression-fragment obligation must satisfy. -/
def decodedObligationFacts
    (modBytes modLen : Nat) (obligation : Obligation) (funcIndices : List Nat) : Prop :=
  decodedStrictCarrierIndex modBytes modLen obligation ∧
  decodedCodeAtAll modBytes modLen obligation funcIndices

/-- The same facts with the three-state carrier binding, for the one family
    whose face is carrier-inert. -/
def decodedCarrierFreeObligationFacts
    (modBytes modLen : Nat) (obligation : Obligation) (funcIndices : List Nat) : Prop :=
  decodedCarrierIndex modBytes modLen obligation ∧
  decodedCodeAtAll modBytes modLen obligation funcIndices

def decodedClaims
    {Claim : Type u}
    (modBytes modLen : Nat)
    (obligation : Claim → Obligation)
    (funcIndices : Claim → List Nat) : List Claim → Prop
  | [] => True
  | claim :: rest =>
      decodedObligationFacts modBytes modLen (obligation claim) (funcIndices claim) ∧
      decodedClaims modBytes modLen obligation funcIndices rest

/-- `decodedClaims` with the three-state carrier binding. Applied to the
    String.concat claim list only; see `decodedCarrierIndex`. -/
def decodedCarrierFreeClaims
    {Claim : Type u}
    (modBytes modLen : Nat)
    (obligation : Claim → Obligation)
    (funcIndices : Claim → List Nat) : List Claim → Prop
  | [] => True
  | claim :: rest =>
      decodedCarrierFreeObligationFacts modBytes modLen
        (obligation claim) (funcIndices claim) ∧
      decodedCarrierFreeClaims modBytes modLen obligation funcIndices rest

def decodedConstructStructFields
    (modBytes modLen : Nat) : List ConstructClaim → Prop
  | [] => True
  | claim :: rest =>
      CertDecode.decodeStructFieldCount modBytes modLen claim.structIdx =
        some claim.fieldCount ∧
      decodedConstructStructFields modBytes modLen rest

def decodedProjectionStructFields
    (modBytes modLen : Nat) : List FieldProjectionClaim → Prop
  | [] => True
  | claim :: rest =>
      CertDecode.decodeStructFieldCount modBytes modLen claim.structIdx =
        some claim.fieldCount ∧
      decodedProjectionStructFields modBytes modLen rest

def decodedCompositionNames
    (modBytes modLen : Nat)
    (members : List CompositionMemberClaim)
    (obligation : Obligation) : List String → Prop
  | [] => True
  | name :: rest =>
      match compositionMemberForName name members with
      | some member =>
          match compositionMemberBinding modBytes modLen member with
          | some (_, funcIdx) =>
              decodedCodeAt modBytes modLen obligation funcIdx ∧
              decodedCompositionNames modBytes modLen members obligation rest
          | none => False
      | none => False

def decodedCompositionClaims
    (modBytes modLen : Nat)
    (members : List CompositionMemberClaim) : List CompositionClaim → Prop
  | [] => True
  | claim :: rest =>
      decodedStrictCarrierIndex modBytes modLen claim.obligation ∧
      decodedCompositionNames modBytes modLen members claim.obligation claim.memberNames ∧
      decodedCompositionClaims modBytes modLen members rest

/-- Artifact-decoded facts for every non-expression-fragment family.
    Mutual obligations bind every member of their shared SCC `CodeTbl`;
    composition obligations bind every name in their byte-checked transitive
    closure. The source expression-fragment claim list is deliberately absent.
    Kept separate so the module-wide host-role guard has a literal
    one-conjunct-weakened GuardIso counterpart. -/
def decodedNonExprClaimFacts (artifact : ArtifactData) : Prop :=
  decodedClaims artifact.modBytes artifact.modLen
      (fun c : StringEqClaim => c.obligation)
      (fun c : StringEqClaim => [c.obligation.self])
      artifact.stringEqClaims ∧
  -- The ONE list here on the three-state carrier binding. Every other list
  -- below keeps the strict one, so among the families collected in THIS
  -- predicate a module with no decodable carrier struct carries String.concat
  -- claims and nothing else. That is not a statement about the whole artifact:
  -- `symFragmentClaims` is absent from this predicate entirely, and such a
  -- module can also carry expression-fragment claims — the `none, none` arith
  -- arm needs only `carrierHelperAbsent`, `hostTableBound roles []` is
  -- vacuously true, and generic and projection fragments cite no role at all.
  decodedCarrierFreeClaims artifact.modBytes artifact.modLen
      (fun c : StringConcatClaim => c.obligation)
      (fun c : StringConcatClaim => [c.obligation.self])
      artifact.stringConcatClaims ∧
  decodedClaims artifact.modBytes artifact.modLen
      (fun c : ConstructClaim => c.obligation)
      (fun c : ConstructClaim => [c.obligation.self])
      artifact.constructClaims ∧
  decodedConstructStructFields artifact.modBytes artifact.modLen artifact.constructClaims ∧
  decodedClaims artifact.modBytes artifact.modLen
      (fun c : RecursionClaim => c.obligation)
      (fun c : RecursionClaim => [c.obligation.self])
      artifact.recursionClaims ∧
  decodedClaims artifact.modBytes artifact.modLen
      (fun c : MutualRecursionClaim => c.obligation)
      (fun c : MutualRecursionClaim => c.memberSet)
      artifact.mutualRecursionClaims ∧
  decodedClaims artifact.modBytes artifact.modLen
      (fun c : VerbatimClaim => c.obligation)
      (fun c : VerbatimClaim => [c.obligation.self])
      artifact.verbatimClaims ∧
  decodedClaims artifact.modBytes artifact.modLen
      (fun c : IntDispatchClaim => c.obligation)
      (fun c : IntDispatchClaim => [c.obligation.self])
      artifact.intDispatchClaims ∧
  decodedClaims artifact.modBytes artifact.modLen
      (fun c : FieldProjectionClaim => c.obligation)
      (fun c : FieldProjectionClaim => [c.obligation.self])
      artifact.fieldProjectionClaims ∧
  decodedProjectionStructFields artifact.modBytes artifact.modLen artifact.fieldProjectionClaims ∧
  decodedCompositionClaims artifact.modBytes artifact.modLen
      artifact.compositionMembers artifact.compositionClaims

/-- Body bytes of the defined function at absolute wasm function index `idx`
    (the import base is applied before indexing the code section), or `none`
    when `idx` is not a defined-function index. The bytes are the code entry
    WITHOUT its leading size-LEB — the locals vector followed by the
    instruction body — matching exactly what `arithHelperBody` synthesizes.
    `codeLocs` isolates each entry (`entryN`, `entryLen`, size-LEB included);
    re-reading that size LEB yields the locals+body region as `esz` bytes. -/
def bodyBytesAtFuncIndex (n len idx : Nat) : Option (List Nat) :=
  match CertDecode.funcImportBase n len, CertDecode.codeLocs n len with
  | some nimp, some locs =>
      if nimp ≤ idx then
        match locs[idx - nimp]? with
        | some loc =>
            match CertDecode.readU loc.entryN loc.entryLen with
            | some (esz, bodyN, _) => some (CertDecode.takeBytes esz bodyN)
            | none => none
        | none => none
      else none
  | _, _ => none

/-- One declared arith role pinned by TEMPLATE equality: the real code-section
    body at the declared function index equals the canonical helper body
    synthesized from the declared indices alone (`arithHelperBody`). An unbound
    role (`none`) is vacuously pinned — no claim can cite an absent role, so no
    plan can use it. No byte is scanned to DISCOVER a role; a wrong declaration
    synthesizes the wrong bytes and fails this equality. -/
def arithRoleCheck (n len : Nat) (role : ArithTemplateDerisk.ArithRole)
    (idx? : Option Nat) (p : ArithTemplateDerisk.ArithHostParams) : Bool :=
  match idx? with
  | none => true
  | some idx =>
      bodyBytesAtFuncIndex n len idx == some (ArithTemplateDerisk.arithHelperBody role p)

/-- The whole-module arith host-role pin — declare-and-confirm, no fingerprint.
    A byte-provably carrierless module (`__rt_aint_from_i64` export absent)
    declares no table and no params. A carriered module declares both, and every
    declared role index is pinned to its canonical helper body by template
    equality. The three-state consistency between presence of the helper export
    and presence of the declaration is preserved: mixing `some`/`none` across the
    table and params fails closed. No byte scan DISCOVERS a role here: the
    module-wide arith scanner that predated this pin has been deleted from the
    wall rather than left in place as an unreachable decoder.

    A declared table also has to name a carrier that the TYPE SECTION shows:
    `carrierState n len = some (some p.carrier)`. Every other conjunct here reads
    the export section, the code section, or nothing at all, so without this one
    `p.carrier` was a free index that the wall only ever spliced into the helper
    bodies it synthesized — an arith table could be admitted by a module whose
    type section holds no Int-carrier struct anywhere. That is not hypothetical:
    `isCarrier` requires the third field's storage tag to be `0x7f`, so a
    perfectly working carrier whose flag field is a PACKED `i8` (tag `0x78`)
    decodes as no carrier at all, and a module built that way could pair a real
    Int runtime with a carrierless type section. Combined with the reserved
    carrier index that `decodedCarrierIndex` forces in the carrierless state,
    that would have let an Int-family claim wire the box role and state its
    obligation over `CarrierSpec 0` while the values the code actually builds
    live at a different struct index — the claim's representation face and the
    module's representation would simply be about different things.

    What this conjunct restores is the precondition every host-role-consuming
    family needs and none of them state for themselves: an admitted arith table
    now implies a byte-derived carrier struct at exactly the index the helper
    bodies splice.

    It is NOT what confines the carrierless state to `string-concat-v1`. Several
    families cite no arith role at all — String.eq, verbatim dispatch, field
    projection and named-ADT construction all reach acceptance with the table
    absent — so this conjunct never runs for them. What confines the carrierless
    state is that `decodedCarrierIndex` is wired to the String.concat claim list
    ALONE; every other NON-EXPRESSION-FRAGMENT family keeps
    `decodedStrictCarrierIndex`, which no module without a decodable carrier
    struct can satisfy. Both mechanisms are needed: this one stops a role-citing
    family from wiring an Int runtime the type section does not corroborate, and
    the scoping stops a role-FREE family that is nonetheless carrier-sensitive
    (`construct-v1`) from being stated over the reserved index.

    Neither mechanism reaches the expression-fragment family, whose claims are
    absent from `decodedNonExprClaimFacts`. A fragment claim that can make no
    face read its carrier — no role cited AND no `.intCarrier` in the encoded
    plan — has that field confirmed only by the byte equalities of
    `ExprFragmentAccepted.accepted`, and a carrierless module can therefore
    carry such claims as well as String.concat ones. Every other fragment claim
    carries its own decoder equality instead (`symFragmentCarrierBound`, inside
    `symFragmentPlanAccepted`), which pins the claimed carrier to the same
    `carrierState` this conjunct pins `ArithHostParams.carrier` to — so a
    role-citing fragment and the arith table it cites can never disagree about
    where the carrier lives.

    `box`, `toIndex`, `cmp` and `eq` carry a SECOND pin, to their runtime export
    names (#736 for `box`), and both pins are kept because they constrain
    different things. The name equality says at WHICH INDEX the role may be
    declared; the template equality says WHICH BYTES sit there. Only the name
    equality has any force on a `none` declaration, since `arithRoleCheck` is
    vacuous on `none` by design — dropping it would turn "this module exports no
    `__aint_to_index`" from a byte-proved fact into a producer's free choice.
    Conversely only the template equality constrains the code at an
    honestly-named export, which the name equality never reads.

    The `toIndex` pins are load-bearing, not decorative: the fused vector-read
    face wires an ABSTRACT contract function at the declared index and never
    interprets that function's body, so without them a package could declare
    any same-signature function as the index helper and certify a law the bytes
    do not satisfy. `none` on both sides is the honest reading for a carriered
    module that exports no `__aint_to_index`; a claim citing the role then fails
    to match, because `Subject.hostRoles` binds it to `none`.

    The two comparison roles inherit that argument WORD FOR WORD — a comparison
    face also wires an abstract contract at a declared index — and add one of
    their own. `cmp` and `eq` declare the SAME function type
    (`[(ref null carrier), (ref null carrier)] -> [i32]`), so
    `hostTableFuncTypesMatch` cannot separate them: without the name equalities
    an artifact could declare the equality helper as the `cmp` role and the
    three-way helper as the `eq` role and still satisfy every declared-type
    conjunct. The template equalities do separate them, since the two bodies
    differ — but only where a declaration is `some`. On `none` the templates say
    nothing at all, which is exactly the hole the name pin closes: a module that
    really does export `__aint_cmp` may not declare that role absent and thereby
    escape both pins.

    What none of this establishes: the template equality identifies the code
    behind a role, never its meaning. The add/sub/mul/box/index-extraction and
    comparison contracts stay explicit hypotheses of `Obligation.holds` and stay
    disclosed by `ClaimAxes`. Pinning bytes narrows the artifact, not the
    trusted-computing base. -/
def arithTableCheck (n len : Nat) (roles? : Option CertDecode.AddSub.Roles)
    (params? : Option ArithTemplateDerisk.ArithHostParams) : Bool :=
  match roles?, params? with
  | none, none => CertDecode.AddSub.carrierHelperAbsent n len
  | some roles, some p =>
      !CertDecode.AddSub.carrierHelperAbsent n len &&
      (CertDecode.carrierState n len == some (some p.carrier)) &&
      (roles.box == CertDecode.AddSub.boxIdx n len) &&
      (roles.toIndex == CertDecode.AddSub.toIndexIdx n len) &&
      (roles.cmp == CertDecode.AddSub.cmpIdx n len) &&
      (roles.eq == CertDecode.AddSub.eqIdx n len) &&
      ArithTemplateDerisk.checkArithHostParams p &&
      arithRoleCheck n len .box roles.box p &&
      arithRoleCheck n len .toIndex roles.toIndex p &&
      arithRoleCheck n len .add roles.add p &&
      arithRoleCheck n len .sub roles.sub p &&
      arithRoleCheck n len .mul roles.mul p &&
      arithRoleCheck n len .cmp roles.cmp p &&
      arithRoleCheck n len .eq roles.eq p
  | _, _ => false

/-- Bind the declared host-role table and arith indices to the module bytes by
    TEMPLATE equality. Replaces the earlier byte-fingerprint decode: the
    certificate DECLARES which function index carries each helper (plus the
    carrier/limb/sub-routine indices the bodies mention) and the wall
    SYNTHESIZES the canonical helper body from that declaration and pins the real
    code bytes equal to it. `box`, `toIndex`, `cmp` and `eq` stay name-bound as
    well, and the carrierless class stays proved by the absent
    `__rt_aint_from_i64` export (#736 intact). -/
def decodedHostRoleTable (artifact : ArtifactData) : Prop :=
  arithTableCheck artifact.modBytes artifact.modLen
    artifact.manifest.subject.hostRoleTable artifact.manifest.subject.arithParams = true

/-- Decode every String.eq/String.concat role exactly once for the whole module.
    Unlike add/sub, the result is a list because every matching function is
    classified independently; duplicate roles at distinct indices are retained. -/
def decodedStringHostRoles (artifact : ArtifactData) : Prop :=
  CertDecode.StringHost.roleTable artifact.modBytes artifact.modLen =
    some artifact.manifest.subject.stringHostRoles

/-- All in-kernel non-expression byte facts. Keeping `decodedHostRoleTable`
    outside every per-obligation fold is load-bearing for the
    200000-heartbeat budget. -/
def decodedNonExprFacts (artifact : ArtifactData) : Prop :=
  decodedHostRoleTable artifact ∧
  decodedStringHostRoles artifact ∧
  decodedNonExprClaimFacts artifact

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

/-- Coverage: every manifest obligation is claimed. The claims ⊆ manifest
    direction is `fragmentClaimObligationsInManifest`; without THIS conjunct an
    obligation present in `manifest.obligations` but claimed by no family rides
    into the accepted artifact with no byte-derived check — the
    "exists but is not constrained" class at the obligation level. -/
def manifestObligationsClaimed (artifact : ArtifactData) : Bool :=
  let claimed := claimObligationExports artifact
  let claimedIndex := AverCert.WasmSlice.orderedSet claimed
  artifact.manifest.obligations.all (fun o => claimedIndex.contains o.export_)

/-- Manifest obligations bind by export name; a duplicate name would let a
    second, unclaimed obligation ride behind a claimed one (find? sees only the
    first). Names must be pairwise distinct. -/
def manifestObligationExportsUnique (artifact : ArtifactData) : Bool :=
  (AverCert.WasmSlice.uniqueMap
    (artifact.manifest.obligations.map (fun obligation =>
      (obligation.export_, obligation)))).isSome

/-! ### Whole-module interface accounting and certified-closure isolation -/

def stringBytes (s : String) : AverCert.WasmSlice.ByteSeq :=
  s.toList.map Char.toNat

def byteSeqListNodup (xs : List AverCert.WasmSlice.ByteSeq) : Bool :=
  AverCert.WasmSlice.indexedNodup xs

def certifiedExportEntries
    (manifest : AverCert.Schema.Manifest) : List AverCert.WasmSlice.ExportEntry :=
  manifest.obligations.map (fun obligation =>
    { name := stringBytes obligation.export_, kind := 0, idx := obligation.self })

def declaredUncertifiedNames
    (manifest : AverCert.Schema.Manifest) : List AverCert.WasmSlice.ByteSeq :=
  manifest.subject.declaredUncertified.map (fun entry => stringBytes entry.1)

structure ExportKey where
  name : AverCert.WasmSlice.ByteSeq
  kind : Nat
  idx : Nat
deriving Ord

def exportEntryKey (entry : AverCert.WasmSlice.ExportEntry) : ExportKey :=
  ⟨entry.name, entry.kind, entry.idx⟩

/-- Every byte-derived module export is classified exactly once: either the
    function/name/index of a claimed obligation or an explicit uncertified
    declaration. Both declaration lists are duplicate-free, disjoint and have
    no phantom names absent from the export section. -/
def exportsAccounted (artifact : ArtifactData) : Bool :=
  match AverCert.WasmSlice.enumExports artifact.modBytes artifact.modLen with
  | none => false
  | some actual =>
      let certified := certifiedExportEntries artifact.manifest
      let declared := declaredUncertifiedNames artifact.manifest
      let actualNames := actual.map (fun entry => entry.name)
      let certifiedNames := certified.map (fun entry => entry.name)
      let actualNameIndex := AverCert.WasmSlice.orderedSet actualNames
      let declaredIndex := AverCert.WasmSlice.orderedSet declared
      let actualEntryIndex := AverCert.WasmSlice.orderedSet (actual.map exportEntryKey)
      let certifiedEntryIndex := AverCert.WasmSlice.orderedSet (certified.map exportEntryKey)
      byteSeqListNodup actualNames &&
      byteSeqListNodup certifiedNames &&
      byteSeqListNodup declared &&
      certifiedNames.all (fun name => !declaredIndex.contains name) &&
      actual.all (fun entry =>
        certifiedEntryIndex.contains (exportEntryKey entry) ||
          declaredIndex.contains entry.name) &&
      certified.all (fun entry => actualEntryIndex.contains (exportEntryKey entry)) &&
      declared.all actualNameIndex.contains

def capabilityBytes (capability : String × String) :
    AverCert.WasmSlice.ByteSeq × AverCert.WasmSlice.ByteSeq :=
  (stringBytes capability.1, stringBytes capability.2)

/-- The manifest capability list is exact (including import order), contains no
    duplicates, and is a subset of the kernel registry minted from the wasm-gc
    effect import mapping. Non-effect and forged imports therefore fail here. -/
def importsWithinCapabilities (artifact : ArtifactData) : Bool :=
  let declared := artifact.manifest.subject.capabilities
  stringListNodup (declared.map (fun capability => capability.1 ++ "." ++ capability.2)) &&
  declared.all (fun capability => AverCert.Schema.CAPABILITY_REGISTRY.contains capability) &&
  match AverCert.WasmSlice.enumImportNames artifact.modBytes artifact.modLen with
  | some actual => actual == declared.map capabilityBytes
  | none => false

/-- The manifest declares absence/presence and, when present, the exact start
    function index read from section 8. -/
def startAccounted (artifact : ArtifactData) : Bool :=
  AverCert.WasmSlice.startFuncIndex artifact.modBytes artifact.modLen ==
    some artifact.manifest.subject.start

/-- One union closure over all certified roots. Closure of a union is the union
    of each root's closure, while scanning shared helpers only once. The exact
    admitted set is partitioned into certified roots and helpers; imports and
    rejected opcodes make `closureFold` return `none`. Shared memory is rejected
    as a declaration-level hidden channel. -/
def closureIsolation (artifact : ArtifactData) : Bool :=
  let claim := artifact.closureClaim
  let certified := artifact.manifest.obligations.map (fun obligation => obligation.self)
  AverCert.WasmSlice.natListNodup claim.roots &&
  AverCert.WasmSlice.natListNodup claim.helpers &&
  AverCert.WasmSlice.natListNodup claim.admitted &&
  AverCert.WasmSlice.natSetEq claim.roots certified &&
  claim.roots.all (fun root => !AverCert.WasmSlice.natMem root claim.helpers) &&
  AverCert.WasmSlice.natSetEq claim.admitted (claim.roots ++ claim.helpers) &&
  AverCert.WasmSlice.noSharedMemory artifact.modBytes artifact.modLen &&
  match AverCert.WasmSlice.closureFold artifact.modBytes artifact.modLen
      artifact.closureFuel claim.roots [] with
  | some actual => AverCert.WasmSlice.natSetEq actual claim.admitted
  | none => false

def acceptedWholeModule (artifact : ArtifactData) : Prop :=
  CertDecode.moduleFramingValid artifact.modBytes artifact.modLen = true ∧
  exportsAccounted artifact = true ∧
  importsWithinCapabilities artifact = true ∧
  startAccounted artifact = true ∧
  closureIsolation artifact = true

def acceptedSymFragments (artifact : ArtifactData) : Prop :=
  symFragmentClaimsAccepted artifact.modBytes artifact.modLen artifact.symFragmentClaims

def acceptedStringConcatFragments (artifact : ArtifactData) : Prop :=
  stringConcatClaimsAccepted
    artifact.modBytes artifact.modLen
    artifact.manifest
    artifact.stringConcatClaims

def acceptedStringEqFragments (artifact : ArtifactData) : Prop :=
  stringEqClaimsAccepted
    artifact.modBytes artifact.modLen
    artifact.manifest
    artifact.stringEqClaims

def acceptedConstructFragments (artifact : ArtifactData) : Prop :=
  constructClaimsAccepted
    artifact.modBytes artifact.modLen
    artifact.manifest
    artifact.constructClaims ∧
  (constructClaimExportNames artifact.constructClaims).Nodup

def acceptedRecursionFragments (artifact : ArtifactData) : Prop :=
  recursionClaimsAccepted
    artifact.modBytes artifact.modLen
    artifact.manifest
    artifact.recursionClaims

def acceptedMutualRecursionFragments (artifact : ArtifactData) : Prop :=
  mutualRecursionClaimsAccepted
    artifact.modBytes artifact.modLen
    artifact.manifest
    artifact.mutualRecursionClaims ∧
  mutualClaimsFormClosedSccs
    artifact.manifest
    artifact.mutualRecursionClaims

def acceptedVerbatimFragments (artifact : ArtifactData) : Prop :=
  verbatimClaimsAccepted
    artifact.modBytes artifact.modLen
    artifact.manifest
    artifact.verbatimClaims

def acceptedIntDispatchFragments (artifact : ArtifactData) : Prop :=
  intDispatchClaimsAccepted
    artifact.modBytes artifact.modLen
    artifact.manifest
    artifact.intDispatchClaims

def acceptedFieldProjectionFragments (artifact : ArtifactData) : Prop :=
  fieldProjectionClaimsAccepted
    artifact.modBytes artifact.modLen
    artifact.manifest
    artifact.fieldProjectionClaims

/-- Besides the composition-claim acceptance this def carries the two
    ARTIFACT-WIDE manifest-coverage conjuncts (`manifestObligationsClaimed`,
    `manifestObligationExportsUnique`). Like `compositionMembersCovered` they
    are cross-claim facts about the whole manifest, not per-family facts, and
    `acceptedFragments` conjoins this def unconditionally for every artifact —
    composition claims present or not. -/
def acceptedCompositionFragments (artifact : ArtifactData) : Prop :=
  compositionClaimsAccepted
    artifact.modBytes artifact.modLen
      artifact.compositionMembers artifact.compositionClaims ∧
  compositionMembersCovered
    artifact.compositionMembers artifact.compositionClaims = true ∧
  manifestObligationsClaimed artifact = true ∧
  manifestObligationExportsUnique artifact = true

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
  acceptedCompositionFragments artifact ∧
  acceptedWholeModule artifact

def expectedArtifactRoot : String :=
  "AverCert.Artifact.certificate"

def subjectMatchesArtifactRoot (artifact : ArtifactData) : Prop :=
  artifact.manifest.subject.artifactRoot = expectedArtifactRoot

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
