/-
The semantic face of an accepted obligation is selected from the checked
claim and plan data.  The certificate may still provide source declarations
for models that cannot be reconstructed from Wasm, but it may not weaken the
standard domain, codomain, or representation relations of a known family.
-/
import AcceptedArtifactCore
import ConstructVerbatimSoundness
import FieldProjectionSoundness
import StringSoundness
import DeclaredEnvelopeAcceptTransport

namespace AverCert.StandardFace

open AverCert.Schema
open AverCert.AcceptedArtifact
open CertPrelude

/-- The complete host builder stored in an obligation. Faces bind the whole
    function, not a finite set of probes, so no unmentioned input can turn a
    claimed contract into a trap. -/
abbrev HostBuilder :=
  (List WVal → Option WVal) →
  (List WVal → Option WVal) →
  (List WVal → Option WVal) →
  (List WVal → Option WVal) →
  (Nat → List WVal → Option WVal) → HostTbl

def emptyHost : HostBuilder := fun _ _ _ _ _ _ => none

def decodedRoleIdx (roles : CertDecode.AddSub.Roles) : HostRole → Option Nat
  | .box => roles.box
  | .add => roles.add
  | .mul => roles.mul
  | .sub => roles.sub

/-- Every role/index pair used by a claim must agree with the unique table
    decoded from the module. Family checkers already require every role their
    plan consumes; distinct indices make the lookup extensional and reject
    duplicate or aliased role entries. -/
def hostTableBound
    (roles : CertDecode.AddSub.Roles)
    (hostTable : List (HostRole × Nat)) : Bool :=
  AverCert.PlanCheck.hostTableIndicesDistinct hostTable &&
    hostTable.all fun entry => decodedRoleIdx roles entry.1 == some entry.2

/-- A fully specified obligation face. `model? = none` keeps the source model
    as a read declaration; every other field remains fixed by the checked
    family. -/
structure FaceSpec where
  carrier : Nat
  Dom : Type
  Cod : Type
  domRepr : CarrierSpec carrier → Dom → List WVal → Prop
  codRepr : CarrierSpec carrier → Cod → WVal → Prop
  host : HostBuilder
  model? : Option (Dom → Cod) := none

/-- Known families have a complete standard face. The former `adtIntRead` /
    `adtConstructorRead` arms left `Dom`/`domRepr`/`model` unconstrained; user
    ADT claims are now pinned by the declared-index envelope faces below. -/
inductive StandardFace where
  | known (spec : FaceSpec)

/-- Dependent fields are compared with `HEq`: ordinary equality cannot state
    the relation before the domain and codomain types have been identified. -/
def StandardFace.Matches : StandardFace → Obligation → Prop
  | .known spec, obligation =>
      obligation.carrier = spec.carrier ∧
      HEq obligation.Dom spec.Dom ∧
      HEq obligation.Cod spec.Cod ∧
      HEq obligation.domRepr spec.domRepr ∧
      HEq obligation.codRepr spec.codRepr ∧
      obligation.host = spec.host ∧
      match spec.model? with
      | some model => HEq obligation.model model
      | none => True

def intList (carrier arity : Nat) (host : HostBuilder) : FaceSpec where
  carrier := carrier
  Dom := List Int
  Cod := Int
  domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = arity
  codRepr := intRepr
  host := host

def projection (carrier structIdx fieldIdx : Nat) : FaceSpec where
  carrier := carrier
  Dom := WVal × WVal
  Cod := WVal
  domRepr := fun _ p vs => vs = [.structv structIdx [p.1, p.2]]
  codRepr := verbatimRepr
  host := emptyHost
  model? := some (fun p =>
    FieldProjectionSoundness.pairProjection fieldIdx p.1 p.2)

def verbatim (carrier : Nat) (plan : VerbatimRawPlan) : FaceSpec where
  carrier := carrier
  Dom := WVal
  Cod := WVal
  domRepr := fun _ v vs => vs = [v]
  codRepr := verbatimRepr
  host := emptyHost
  model? := some (ConstructVerbatimSoundness.verbatimModel plan)

def stringEq
    (carrier stringTy helperIdx : Nat) (plan : StringEqRawPlan) : FaceSpec where
  carrier := carrier
  Dom := WVal
  Cod := WVal
  domRepr := fun _ v vs => vs = [v]
  codRepr := verbatimRepr
  host := stringEqCanonicalHost helperIdx
  model? := some (StringSoundness.evalStringEq stringTy plan)

def stringConcat
    (carrier resultTy containerTy helperIdx : Nat)
    (plan : StringConcatRawPlan) : FaceSpec where
  carrier := carrier
  Dom := WVal
  Cod := WVal
  domRepr := fun _ v vs => vs = [v]
  codRepr := verbatimRepr
  host := stringConcatCanonicalHost helperIdx resultTy
  model? := some (StringSoundness.evalStringConcat resultTy containerTy plan)

namespace FragTy

def denote : AverCert.Schema.FragTy → Type
  | .f64 => UInt64
  | .boolI32 => Bool
  | .intCarrier => Int
  | .i64 | .rawI32 | .ref | .adtRef => WVal

def encodeArg (carrier : Nat) :
    (ty : AverCert.Schema.FragTy) → FragTy.denote ty → WVal
  | .f64, bits => .f64v bits
  | .boolI32, value => b32 value
  | .intCarrier, value => carrierSmall carrier value
  | .i64, value | .rawI32, value | .ref, value | .adtRef, value => value

def resultRepr (carrier : Nat) :
    (ty : AverCert.Schema.FragTy) →
      CarrierSpec carrier → FragTy.denote ty → WVal → Prop
  | .f64 => floatBitsRepr
  | .boolI32 => boolRepr
  | .intCarrier => intRepr
  | .i64 | .rawI32 | .ref | .adtRef => verbatimRepr

end FragTy

namespace FragParams

/-- Right-associated product used by the emitted fragment obligations. -/
def denote : List AverCert.Schema.FragTy → Type
  | [] => Unit
  | [ty] => FragTy.denote ty
  | ty :: rest => FragTy.denote ty × denote rest

def encodeArgs (carrier : Nat) :
    (params : List AverCert.Schema.FragTy) → denote params → List WVal
  | [], _ => []
  | [ty], value => [FragTy.encodeArg carrier ty value]
  | ty :: next :: rest, values =>
      FragTy.encodeArg carrier ty values.1 ::
        encodeArgs carrier (next :: rest) values.2

end FragParams

def fragment
    (carrier : Nat)
    (params : List AverCert.Schema.FragTy)
    (result : AverCert.Schema.FragTy) : FaceSpec where
  carrier := carrier
  Dom := FragParams.denote params
  Cod := FragTy.denote result
  domRepr := fun _ values args => args = FragParams.encodeArgs carrier params values
  codRepr := FragTy.resultRepr carrier result
  host := emptyHost

def constructUnary (carrier structIdx : Nat) (plan : ConstructRawPlan) : FaceSpec where
  carrier := carrier
  Dom := WVal
  Cod := WVal
  domRepr := fun _ value args => args = [value]
  codRepr := verbatimRepr
  host := emptyHost
  model? := some (fun value => .structv structIdx
    (ConstructVerbatimSoundness.constructModelFields
      ([value] ++ List.replicate 1 .null) plan.fields))

def constructBinary (carrier structIdx : Nat) (plan : ConstructRawPlan) : FaceSpec where
  carrier := carrier
  Dom := WVal × WVal
  Cod := WVal
  domRepr := fun _ values args => args = [values.1, values.2]
  codRepr := verbatimRepr
  host := emptyHost
  model? := some (fun values => .structv structIdx
    (ConstructVerbatimSoundness.constructModelFields
      ([values.1, values.2] ++ List.replicate 1 .null) plan.fields))

structure IntAddFace where
  constant : Int
  boxIdx : Nat
  addIdx : Nat
deriving Repr, DecidableEq

def intAddHost (carrier : Nat) (face : IntAddFace) : HostBuilder :=
  fun add _ _ _ _ fn =>
    if fn = face.boxIdx then some (1, boxRef carrier)
    else if fn = face.addIdx then some (2, add)
    else none

/-- Exact `add(param0, box(k))` classifier. This family intentionally keeps
    the stronger `List Int`/`ReprAll` face instead of the small-value fragment
    representation. -/
def classifyIntAdd (plan : ExprFragmentRawPlan) : Option IntAddFace :=
  if plan.params = [.intCarrier] && plan.result = .intCarrier &&
      plan.body.result = 3 then
    match plan.body.nodes with
    | [n0, n1, n2, n3] =>
        match n0.kind, n1.kind, n2.kind, n3.kind with
        | .local 0, .constI64 constant,
          .hostCall .box boxIdx [1], .hostCall .add addIdx [0, 2] =>
            if n0.ty = .intCarrier && n1.ty = .i64 &&
                n2.ty = .intCarrier && n3.ty = .intCarrier then
              some { constant := constant, boxIdx := boxIdx, addIdx := addIdx }
            else none
        | _, _, _, _ => none
    | _ => none
  else none

/-! ### Tag-dispatch face (Option/Result `match` returning an Int constant)

An ADT whose discriminant is a tag FIELD (not a `ref.test` subtype): read the
i32 tag in field 0 of the scrutinee struct, compare to a literal, and return a
boxed integer constant on each arm. The face is stated OPERATIONALLY over the
representation — `Dom = (tag, payload)`, `model` reads the tag and branches —
so it never claims that a source constructor writes the tag into field 0. -/

structure TagDispatchFace where
  optIdx : Nat
  boxIdx : Nat
  tag    : Int
  thenC  : Int
  elseC  : Int
deriving Repr, DecidableEq

def tagDispatchHost (carrier boxIdx : Nat) : HostBuilder :=
  fun _add _sub _mul _stringEq _stringConcat fn =>
    if fn = boxIdx then some (1, boxRef carrier) else none

/-- The complete operational face of a tag-dispatch obligation. `Dom` carries the
    tag and payload; `domRepr` pins the scrutinee to the tagged struct; `model`
    reads the tag and returns the arm constant; the result is a represented
    integer. -/
def tagDispatch (carrier : Nat) (face : TagDispatchFace) : FaceSpec where
  carrier := carrier
  Dom := Int × WVal
  Cod := Int
  domRepr := fun _S p vs => vs = [.structv face.optIdx [.i32v p.1, p.2]]
  codRepr := intRepr
  host := tagDispatchHost carrier face.boxIdx
  model? := some (fun p => if p.1 = face.tag then face.thenC else face.elseC)

/-- One boxed-Int-constant arm: `[i64.const c, box bi]` yields `(bi, c)`. -/
def tagDispatchArm? (b : FragBlock) : Option (Nat × Int) :=
  match b.nodes, b.result with
  | [{ id := 0, ty := .i64, kind := .constI64 c },
     { id := 1, ty := .intCarrier, kind := .hostCall .box bi [0] }], 1 => some (bi, c)
  | _, _ => none

/-- Exact tag-dispatch classifier: `local0; struct.get.user optIdx 0; i32.const k;
    i32.eq; if (box thenC) (box elseC)`. Both arms must box through the same
    `box` helper. -/
def classifyTagDispatch (plan : ExprFragmentRawPlan) : Option TagDispatchFace :=
  if plan.params = [.adtRef] && plan.result = .intCarrier &&
      plan.body.result = 4 then
    match plan.body.nodes with
    | [n0, n1, n2, n3, n4] =>
        match n0.kind, n1.kind, n2.kind, n3.kind, n4.kind with
        | .local 0, .structGetUser optIdx 0 0, .constI32 tag, .prim .i32Eq [1, 2],
          .ifElse 3 hitBlk missBlk =>
            if n0.ty = .adtRef && n1.ty = .rawI32 && n2.ty = .rawI32 &&
                n3.ty = .boolI32 && n4.ty = .intCarrier then
              match tagDispatchArm? hitBlk, tagDispatchArm? missBlk with
              | some (boxIdx, thenC), some (boxIdx2, elseC) =>
                  if boxIdx = boxIdx2 then
                    some { optIdx := optIdx, boxIdx := boxIdx, tag := tag,
                           thenC := thenC, elseC := elseC }
                  else none
              | _, _ => none
            else none
        | _, _, _, _, _ => none
    | _ => none
  else none
def genericFragmentAllowedFuel : Nat → FragBlock → Bool
  | 0, _ => false
  | fuel + 1, block =>
      block.nodes.all fun node =>
        node.ty != .adtRef &&
        match node.kind with
        | .hostCall _ _ _ | .structGetUser _ _ _ => false
        | .ifElse _ thenBlock elseBlock =>
            genericFragmentAllowedFuel fuel thenBlock &&
              genericFragmentAllowedFuel fuel elseBlock
        | .selfCall _ _ _ => false
        | .local _ | .constBool _ | .constI64 _ | .constI32 _ |
          .constF64Bits _ | .structGet _ _ | .refIsNull _ |
          .prim _ _ => true

noncomputable def genericFragmentAllowed (plan : ExprFragmentRawPlan) : Bool :=
  !plan.params.contains .adtRef &&
    plan.result != .adtRef &&
    plan.result != .intCarrier &&
    genericFragmentAllowedFuel (sizeOf plan.body + 1) plan.body

noncomputable def symFragmentFace (claim : SymFragmentClaim) : Option StandardFace :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan with
  | none => none
  | some plan =>
      match classifyIntAdd plan with
      | some face =>
          some (.known
            (intList claim.carrier 1 (intAddHost claim.carrier face)))
      | none =>
          match AverCert.WasmSlice.exprProjectionFace? plan with
          | some (structIdx, fieldIdx) =>
              some (.known (projection claim.carrier structIdx fieldIdx))
          | none =>
              match classifyTagDispatch plan with
              | some face => some (.known (tagDispatch claim.carrier face))
              | none =>
                  if genericFragmentAllowed plan then
                    some (.known (fragment claim.carrier plan.params plan.result))
                  else none

def symFragmentMatches
    (roles : CertDecode.AddSub.Roles) (claim : SymFragmentClaim) : Prop :=
  hostTableBound roles claim.hostTable = true ∧
    match symFragmentFace claim with
    | some face => face.Matches claim.obligation
    | none => False

/-! ### Declared-index envelope faces (user ADT claims)

The plan/certificate DECLARES the ADT envelope (root, carrier, every
constructor's flattened index, shape, and payload target) plus the opaque
type-section prefix before the constructor entries; the wall CONFIRMS the whole
declaration with ONE byte-slice equality (`concatPinnedAt`). The declared
envelope then pins the obligation's `Dom`/`domRepr`/`codRepr`/`model` to wall
terms computed from the checked plan, closing the former free-model /
free-`domRepr` faces. The envelope and prefix live in the proof term of the
generated certificate, never in the public JSON manifest. -/

/-- The declared-envelope face carried by a named-ADT constructor claim. The
    declared hit constructor sits at the byte-pinned `structIdx`; the byte
    acceptance gate independently binds `elemTy`/`fieldCount` to the real type
    entry at `structIdx`, and requiring the Int-carrier payload here ties the
    declared hit shape to that byte-checked entry. -/
def constructNamedFace
    (modBytes modLen : Nat) (claim : ConstructClaim)
    (plan : ConstructRawPlan) : Prop :=
  claim.elemTy = .nullableRef claim.carrier ∧
  claim.fieldCount = 1 ∧
  claim.obligation.host = emptyHost ∧
  ∃ (typePrefix : List Nat) (env : AverCert.DeclaredIndexEnvelope.DIdxEnvelope)
    (hhit : AverCert.DeclaredIndexEnvelope.dCtorShape? env claim.structIdx =
      some .hit),
    AverCert.DeclaredIndexEnvelope.DIdxCtorFace
      modBytes modLen typePrefix env claim.structIdx hhit plan claim.obligation

/-- The declared-envelope face carried by an Int-dispatch claim. Every tested
    dispatch tag must be a declared hit constructor whose synthesized entry is
    byte-pinned at its declared index by the single `concatPinnedAt` equality;
    the obligation's meaning terms are the wall terms over the declared
    envelope and the checked plan. -/
def intDispatchDeclaredFace
    (modBytes modLen : Nat) (claim : IntDispatchClaim)
    (plan : IntDispatchRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  claim.obligation.host =
    intDispatchCanonicalHost claim.carrier claim.hostTable ∧
  ∃ (typePrefix : List Nat) (env : AverCert.DeclaredIndexEnvelope.DIdxEnvelope),
    AverCert.DeclaredIndexEnvelope.DIdxIntReadFace
      modBytes modLen typePrefix env plan claim.obligation

/-- The declared-envelope face carried by a String.concat claim. The literal
    chunks are required non-empty so the declared `resultTy` occurs inside the
    byte-matched code entry (`array.new_data resultTy`) — a chunk-less plan
    would leave the result element type a free semantic tag. -/
def stringConcatDeclaredFace
    (modBytes modLen : Nat) (claim : StringConcatClaim)
    (plan : StringConcatRawPlan) : Prop :=
  (plan.prefixes.isEmpty && plan.suffixes.isEmpty) = false ∧
  claim.obligation.host =
    stringConcatCanonicalHost claim.concatFuncIdx claim.resultTy ∧
  ∃ (typePrefix : List Nat) (env : AverCert.DeclaredIndexEnvelope.DIdxEnvelope),
    AverCert.DeclaredIndexEnvelope.DIdxStringConcatFace
      modBytes modLen typePrefix env claim.resultTy claim.containerTy plan
      claim.obligation


def stringEqMatches (manifest : Manifest) (claim : StringEqClaim) : Prop :=
  match stringEqPlanForExport claim.exportName manifest.stringEqPlans with
  | some plan =>
      (StandardFace.known
        (stringEq claim.carrier claim.stringTy claim.stringEqFuncIdx plan)).Matches
          claim.obligation
  | none => False

def stringConcatMatches
    (modBytes modLen : Nat) (manifest : Manifest)
    (claim : StringConcatClaim) : Prop :=
  match stringConcatPlanForExport claim.exportName manifest.stringConcatPlans with
  | some plan => stringConcatDeclaredFace modBytes modLen claim plan
  | none => False

def verbatimMatches (manifest : Manifest) (claim : VerbatimClaim) : Prop :=
  match verbatimPlanForExport claim.exportName manifest.verbatimPlans with
  | some plan =>
      (StandardFace.known (verbatim claim.carrier plan)).Matches claim.obligation
  | none => False

def fieldProjectionMatches
    (manifest : Manifest) (claim : FieldProjectionClaim) : Prop :=
  match fieldProjectionPlanForExport claim.exportName manifest.fieldProjectionPlans with
  | some plan =>
      (StandardFace.known
        (projection claim.carrier claim.structIdx plan.fieldIdx)).Matches claim.obligation
  | none => False

def constructMatches
    (modBytes modLen : Nat) (manifest : Manifest) (claim : ConstructClaim) : Prop :=
  match constructPlanForExport claim.exportName manifest.constructPlans with
  | none => False
  | some plan =>
      match claim.symPlan.result with
      | .app1 "List" _ =>
          match plan.arity with
          | 1 =>
              (StandardFace.known
                (constructUnary claim.carrier claim.structIdx plan)).Matches
                  claim.obligation
          | 2 =>
              (StandardFace.known
                (constructBinary claim.carrier claim.structIdx plan)).Matches
                  claim.obligation
          | _ => False
      | .named _ => constructNamedFace modBytes modLen claim plan
      | _ => False

def recursionMatches
    (manifest : Manifest) (roles : CertDecode.AddSub.Roles)
    (claim : RecursionClaim) : Prop :=
  hostTableBound roles claim.hostTable = true ∧
    match recursionPlanForExport claim.exportName manifest.recursionPlans with
    | some plan =>
        (StandardFace.known
          (intList claim.carrier plan.params.length
            (intDispatchCanonicalHost claim.carrier claim.hostTable))).Matches
              claim.obligation
    | none => False

def mutualMatches
    (manifest : Manifest) (roles : CertDecode.AddSub.Roles)
    (claim : MutualRecursionClaim) : Prop :=
  hostTableBound roles claim.hostTable = true ∧
    match mutualPlanForExport claim.exportName manifest.mutualPlans with
    | some _ =>
        (StandardFace.known
          (intList claim.carrier 1
            (intDispatchCanonicalHost claim.carrier claim.hostTable))).Matches
              claim.obligation
    | none => False

def intDispatchMatches
    (modBytes modLen : Nat)
    (manifest : Manifest) (roles : CertDecode.AddSub.Roles)
    (claim : IntDispatchClaim) : Prop :=
  hostTableBound roles claim.hostTable = true ∧
    match intDispatchPlanForExport claim.exportName manifest.intDispatchPlans with
    | some plan => intDispatchDeclaredFace modBytes modLen claim plan
    | none => False

def compositionMatches
    (members : List CompositionMemberClaim)
    (roles : CertDecode.AddSub.Roles) (claim : CompositionClaim) : Prop :=
  hostTableBound roles claim.hostTable = true ∧
    match compositionMemberForName claim.exportName members with
    | some _ =>
        (StandardFace.known
          (intList claim.carrier 1
            (intDispatchCanonicalHost claim.carrier claim.hostTable))).Matches
              claim.obligation
    | none => False

/-- Cross-family uniqueness closes the gap left by uniqueness within individual
    plan lists: one obligation export may be claimed by exactly one family. -/
def claimExportsUnique (artifact : ArtifactData) : Bool :=
  AverCert.WasmSlice.indexedNodup (claimObligationExports artifact)

/-- Report one fixed class for every claim in a family. The export and class
    stay paired throughout; the verifier never compares two independently
    ordered lists. -/
def fixedReportEntries {Claim : Type u}
    (className : String) (obligation : Claim → Obligation) :
    List Claim → List (String × String) :=
  List.map fun claim => ((obligation claim).export_, className)

def recursionReportEntry
    (manifest : Manifest) (claim : RecursionClaim) : Option (String × String) := do
  let plan ← recursionPlanForExport claim.exportName manifest.recursionPlans
  let className ← match plan.params with
    | [_] => some "self-recursive"
    | [_, _] => some "multi-argument self-recursive"
    | _ => none
  pure (claim.obligation.export_, className)

/-- Derive every public class label from the checked claim family and plan.
    This is report data only, but deriving it in the wall prevents the package
    producer from choosing a more favourable label for an accepted obligation. -/
def claimReportEntries (artifact : ArtifactData) : Option (List (String × String)) := do
  let recursion ← artifact.recursionClaims.mapM
    (recursionReportEntry artifact.manifest)
  pure <|
    fixedReportEntries "expr-fragment-v1"
      (fun c : SymFragmentClaim => c.obligation) artifact.symFragmentClaims ++
    fixedReportEntries "verbatim-string-eq"
      (fun c : StringEqClaim => c.obligation) artifact.stringEqClaims ++
    fixedReportEntries "verbatim-string-concat"
      (fun c : StringConcatClaim => c.obligation) artifact.stringConcatClaims ++
    fixedReportEntries "adt-constructor"
      (fun c : ConstructClaim => c.obligation) artifact.constructClaims ++
    recursion ++
    fixedReportEntries "mutual-recursive"
      (fun c : MutualRecursionClaim => c.obligation) artifact.mutualRecursionClaims ++
    fixedReportEntries "verbatim-dispatch"
      (fun c : VerbatimClaim => c.obligation) artifact.verbatimClaims ++
    fixedReportEntries "int-dispatch"
      (fun c : IntDispatchClaim => c.obligation) artifact.intDispatchClaims ++
    fixedReportEntries "field-projection"
      (fun c : FieldProjectionClaim => c.obligation) artifact.fieldProjectionClaims ++
    fixedReportEntries "cross-function-composition"
      (fun c : CompositionClaim => c.obligation) artifact.compositionClaims

def reportEntryFor
    (entries : List (String × String)) (obligation : Obligation) :
    Option (String × String) := do
  let className ← namedPlanForExport obligation.export_ entries
  pure (obligation.export_, className)

/-- Public report entries in manifest order. Cross-family uniqueness is checked
    before lookup, so a label can never be selected by first-match ambiguity. -/
def reportEntries (artifact : ArtifactData) : Option (List (String × String)) :=
  if claimExportsUnique artifact then do
    let entries ← claimReportEntries artifact
    artifact.manifest.obligations.mapM (reportEntryFor entries)
  else none

/-- Every claim is unique across families and carries the semantic face selected
    by its checked family and plan. This is deliberately conjoined with the
    established byte-origin predicates; moving reconstruction into Lean does
    not remove any existing acceptance gate. -/
def checkedFaces (artifact : ArtifactData) : Prop :=
  claimExportsUnique artifact = true ∧
  allClaims (symFragmentMatches artifact.manifest.subject.hostRoles)
    artifact.symFragmentClaims ∧
  allClaims (stringEqMatches artifact.manifest) artifact.stringEqClaims ∧
  allClaims (stringConcatMatches artifact.modBytes artifact.modLen
    artifact.manifest) artifact.stringConcatClaims ∧
  allClaims (constructMatches artifact.modBytes artifact.modLen
    artifact.manifest) artifact.constructClaims ∧
  allClaims (recursionMatches artifact.manifest
    artifact.manifest.subject.hostRoles) artifact.recursionClaims ∧
  allClaims (mutualMatches artifact.manifest
    artifact.manifest.subject.hostRoles) artifact.mutualRecursionClaims ∧
  allClaims (verbatimMatches artifact.manifest) artifact.verbatimClaims ∧
  allClaims (intDispatchMatches artifact.modBytes artifact.modLen
    artifact.manifest artifact.manifest.subject.hostRoles)
    artifact.intDispatchClaims ∧
  allClaims (fieldProjectionMatches artifact.manifest) artifact.fieldProjectionClaims ∧
  allClaims (compositionMatches artifact.compositionMembers
    artifact.manifest.subject.hostRoles) artifact.compositionClaims

end AverCert.StandardFace
