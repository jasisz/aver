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
  (Nat → List WVal → Option WVal) →
  (List WVal → Option WVal) →
  (List WVal → Option WVal) →
  (List WVal → Option WVal) → HostTbl

def emptyHost : HostBuilder := fun _ _ _ _ _ _ _ _ _ => none

def decodedRoleIdx (roles : CertDecode.AddSub.Roles) : HostRole → Option Nat
  | .box => roles.box
  | .add => roles.add
  | .mul => roles.mul
  | .sub => roles.sub
  | .toIndex => roles.toIndex
  | .cmp => roles.cmp
  | .eq => roles.eq

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
  fun add _ _ _ _ _ _ _ fn =>
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
  fun _add _sub _mul _stringEq _stringConcat _toIndex _cmp _eq fn =>
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
/-! ### Fused vector-read face (`Option.withDefault(Vector.get(vec, idx), d)`)

The wasm-gc emitter fuses this call pair into one fixed bounds-checked
`array.get` template (`PlanLower.vectorGetOrDefaultTemplate`): extract the
index through the `__aint_to_index` host helper, test `idx >= 0 (signed) AND
idx < len (unsigned)`, read the element on hit, box the literal default on
miss. The helper is bound only by its relational `toIndexW` contract (the
sixth host-contract slot), mirroring the add/sub/mul host contracts.

SOUNDNESS-CRITICAL BOUND: the representation relation requires
`elems.length < 2^31`. Without it, a state with a `>= 2^31`-element array
would "represent" a vector for which the model reads `v[i]` at
`i in [2^31, len)` while the machine's `to_index` collapses `i` to the `-1`
sentinel and returns the default. The bound lives INSIDE `vecDomRepr` — a
state carrying a larger array simply represents no `(v, i)` at all — never as
an asserted premise. It is also true of the actual runtime: no engine array
spans `2^31` entries (`wat/to_index.wat`). -/

structure VectorGetOrDefaultFace where
  arrTy      : Nat
  toIndexIdx : Nat
  boxIdx     : Nat
  d          : Int
deriving Repr, DecidableEq

/-- Host slots of the fused template: the abstract `__aint_to_index` contract
    slot and the audited box reference face. -/
def vectorGetOrDefaultHostSlots
    (carrier toIndexIdx boxIdx : Nat)
    (toIndex : List WVal → Option WVal) : HostTbl :=
  fun fn =>
    if fn = toIndexIdx then some (1, toIndex)
    else if fn = boxIdx then some (1, boxRef carrier) else none

def vectorGetOrDefaultHost
    (carrier : Nat) (face : VectorGetOrDefaultFace) : HostBuilder :=
  fun _add _sub _mul _stringEq _stringConcat toIndex _cmp _eq =>
    vectorGetOrDefaultHostSlots carrier face.toIndexIdx face.boxIdx toIndex

/-- Domain representation of the fused-read face: the machine state is exactly
    `[vector array, boxed index]`, the array has one represented element per
    model element, and — soundness-critical, see the section header — fewer
    than `2^31` elements. All witnesses live INSIDE the relation. -/
def vecDomRepr (carrier arrTy : Nat) (S : CarrierSpec carrier)
    (p : List Int × Int) (vs : List WVal) : Prop :=
  ∃ elems wi,
    vs = [.arr arrTy elems, wi] ∧
    elems.length = p.1.length ∧
    elems.length < 2147483648 ∧
    (∀ k, k < p.1.length → ∃ w, elems[k]? = some w ∧ intRepr S (p.1[k]!) w) ∧
    intRepr S p.2 wi

/-- The source model: in-bounds read, else the literal default. -/
def vecModel (d : Int) (p : List Int × Int) : Int :=
  if 0 ≤ p.2 ∧ p.2 < (p.1.length : Int) then p.1[p.2.toNat]! else d

/-- The complete face of the fused vector-read shape: the four template holes
    are the face data; domain, codomain, representations, and model are fixed
    by the family. -/
def vectorGetOrDefault
    (carrier : Nat) (face : VectorGetOrDefaultFace) : FaceSpec where
  carrier := carrier
  Dom := List Int × Int
  Cod := Int
  domRepr := vecDomRepr carrier face.arrTy
  codRepr := intRepr
  host := vectorGetOrDefaultHost carrier face
  model? := some (vecModel face.d)

/-- Exact fused vector-read classifier: the plan is the single monolithic
    template node over the pinned `(vector, index)` params. The helper indices
    must be distinct, or the host builder could not present both slots. -/
def classifyVectorGetOrDefault
    (plan : ExprFragmentRawPlan) : Option VectorGetOrDefaultFace :=
  if plan.params = [.adtRef, .intCarrier] && plan.result = .intCarrier &&
      plan.body.result = 0 then
    match plan.body.nodes with
    | [n0] =>
        match n0.kind with
        | .vectorGetOrDefault arrTy toIndexIdx boxIdx d =>
            if n0.ty = .intCarrier && toIndexIdx != boxIdx then
              some { arrTy := arrTy, toIndexIdx := toIndexIdx,
                     boxIdx := boxIdx, d := d }
            else none
        | _ => none
    | _ => none
  else none

/-! ### Record-parameter face (a Plan type declaration typed the parameter)

The certified Plan carries the user record declaration (`SchemaCore.TypeDecl`);
the wall LOWERS it (`lowerTypeDecl`) and the face pins the module's type-section
entry at the projected struct index to that lowering BY EQUALITY, so the layout
is a checked-by-equality witness, never trusted plan data. The declaration
itself sits under an existential, which is sound for the same reason the
declared-index envelope's existentials are: the equality pin forces the
declared bytes to be the module's real bytes, so the witness is not a free
choice — an Int field can only be declared where the real entry holds the
nullable carrier reference, a Bool field only at `i32`, a Float field only at
`f64`, and `.plain` kills the `.sub`/`.subFinal` doppelganger outright. The
meaning terms (`Dom`/`domRepr`/`Cod`/`codRepr`/`model`) are wall terms over the
declaration (`RecordFields`/`ReprFields`/`nthField`), pinned by `HEq` exactly
like `intDispatchDeclaredFace`. -/

/-- Domain representation of the record-parameter face: the machine state is
    exactly one struct at the pinned index whose fields represent the record
    denotation pointwise (`ReprFields`). -/
def recordParamDomRepr (carrier structIdx : Nat) (fields : List TypeDecl) :
    CarrierSpec carrier → RecordFields fields → List WVal → Prop :=
  fun S v vs => ∃ ws, vs = [.structv structIdx ws] ∧ ReprFields S fields ws v

/-- Codomain representation: the result represents the projected field under
    the single generic wall relation `ReprOf` (definitionally the scalar
    `intRepr`/`boolRepr`/`floatBitsRepr` at the admitted leaves). -/
def recordParamCodRepr (carrier : Nat) (fields : List TypeDecl)
    (field : Nat) (hfield : field < fields.length) :
    CarrierSpec carrier → RecordVal (fields[field]'hfield) → WVal → Prop :=
  fun S c w => ReprOf S (fields[field]'hfield) w c

/-- The source model of a record field read: the `field`-th component of the
    record denotation. -/
def recordParamModel (fields : List TypeDecl) (field : Nat)
    (hfield : field < fields.length) :
    RecordFields fields → RecordVal (fields[field]'hfield) :=
  fun v => nthField fields v field hfield

/-- The declared-record face carried by a record-parameter field-read claim.
    Every conjunct is load-bearing:

    * `checkRecordDecl` — the declaration is a stage-1 flat scalar record;
    * the TYPE-SECTION EQUALITY PIN — the module's entry at the projected
      struct index IS the wall lowering of the declaration (form `.plain`,
      full ordered field list, every storage, every mutability);
    * the PARAM BINDING — the certified export's declared parameter names
      exactly the pinned struct index (`recordParamFuncTypeMatches`);
    * the PLAN BINDING — the recognizer fires on this plan at the same
      `(structIdx, field)`, the field is in range, and the plan's declared
      result is the declared field's scalar fragment type;
    * the CARRIER BINDING — a declaration that mentions the Int carrier makes
      the face's meaning read the claimed carrier index (through
      `lowerTypeDecl` and `ReprFields`' `intRepr` leaf) even when the plan and
      host table never name it, so the claimed index must then be the decoded
      `CertDecode.carrierState` (the #767 lesson applied to declarations);
    * the `HEq` pins — the obligation's meaning fields are the wall terms over
      the declaration, exactly like `intDispatchDeclaredFace`;
    * `decl = .record structIdx fields` — the declaration's own index is bound
      to the pinned struct index (`lowerTypeDecl` never reads it, so leaving it
      free would be an unconstrained-witness label). -/
def recordParamDeclaredFace
    (modBytes modLen : Nat) (claim : SymFragmentClaim)
    (plan : ExprFragmentRawPlan) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  claim.obligation.host = emptyHost ∧
  claim.obligation.carrier = claim.carrier ∧
  ∃ (decl : TypeDecl) (structIdx field : Nat) (fields : List TypeDecl)
    (hfield : field < fields.length),
    decl = .record structIdx fields ∧
    AverCert.WasmSlice.exprRecordProjFace? plan = some (structIdx, field) ∧
    checkRecordDecl decl = true ∧
    scalarLeafFragTy? (fields[field]'hfield) = some plan.result ∧
    (typeDeclMentionsIntCarrier decl = true →
      CertDecode.carrierState modBytes modLen = some (some claim.carrier)) ∧
    AverCert.WasmSlice.typeSectionMatches
      (fun entry =>
        decide (lowerTypeDecl claim.carrier lowerTypeDeclFuel decl = some entry))
      modBytes modLen structIdx = true ∧
    AverCert.WasmSlice.recordParamFuncTypeMatches
      modBytes modLen claim.exportNameBytes structIdx = true ∧
    HEq claim.obligation.Dom (RecordFields fields) ∧
    HEq claim.obligation.Cod (RecordVal (fields[field]'hfield)) ∧
    HEq claim.obligation.domRepr (recordParamDomRepr claim.carrier structIdx fields) ∧
    HEq claim.obligation.codRepr (recordParamCodRepr claim.carrier fields field hfield) ∧
    HEq claim.obligation.model (recordParamModel fields field hfield)

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
        | .vectorGetOrDefault _ _ _ _ => false
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
                  match classifyVectorGetOrDefault plan with
                  | some face =>
                      some (.known (vectorGetOrDefault claim.carrier face))
                  | none =>
                      if genericFragmentAllowed plan then
                        some (.known (fragment claim.carrier plan.params plan.result))
                      else none

/-- No `FaceSpec` branch of the classify chain fires on a record-projection
    plan, so appending the record face on the chain's `none` arm neither
    shadows nor reorders any existing face. Shape by shape: `classifyIntAdd`
    needs an `.intCarrier` parameter list, `exprProjectionFace?` an `.adtRef`
    result, `classifyTagDispatch` a five-node body, `classifyVectorGetOrDefault`
    a two-parameter list, and the generic gate forbids `.adtRef` parameters —
    each contradicted by the recognized record shape. -/
theorem symFragmentFace_none_of_recordProj
    (claim : SymFragmentClaim) (plan : ExprFragmentRawPlan)
    (hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan = some plan)
    (structIdx field : Nat)
    (hRecord : AverCert.WasmSlice.exprRecordProjFace? plan
      = some (structIdx, field)) :
    symFragmentFace claim = none := by
  obtain ⟨hparams, hscalar, hbody⟩ :=
    AverCert.WasmSlice.exprRecordProjFace?_spec plan structIdx field hRecord
  have hresultNe : plan.result ≠ .adtRef := by
    intro h
    rw [h] at hscalar
    simp [AverCert.Schema.fragTyIsRecordScalar] at hscalar
  have hbodyResult : plan.body.result = 1 := by rw [hbody]
  unfold symFragmentFace
  rw [hEncode]
  have hIntAdd : classifyIntAdd plan = none := by
    unfold classifyIntAdd
    simp [hparams]
  have hProjection : AverCert.WasmSlice.exprProjectionFace? plan = none := by
    unfold AverCert.WasmSlice.exprProjectionFace?
    simp [hresultNe]
  have hTagDispatch : classifyTagDispatch plan = none := by
    unfold classifyTagDispatch
    simp [hbodyResult]
  have hVectorGet : classifyVectorGetOrDefault plan = none := by
    unfold classifyVectorGetOrDefault
    simp [hparams]
  have hGeneric : genericFragmentAllowed plan = false := by
    unfold genericFragmentAllowed
    simp [hparams]
  simp [hIntAdd, hProjection, hTagDispatch, hVectorGet, hGeneric]

def symFragmentMatches
    (modBytes modLen : Nat)
    (roles : CertDecode.AddSub.Roles) (claim : SymFragmentClaim) : Prop :=
  hostTableBound roles claim.hostTable = true ∧
    match symFragmentFace claim with
    | some face => face.Matches claim.obligation
    | none =>
        -- The record-parameter face fires strictly AFTER every `FaceSpec`
        -- branch (provably non-overlapping — `symFragmentFace_none_of_recordProj`
        -- shows the chain yields `none` on every recognized record plan, so
        -- this arm is the record shape's ONLY route). A plan matching neither
        -- remains `False`, exactly as before.
        match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
            claim.hostTable claim.structTable claim.plan with
        | some plan =>
            match AverCert.WasmSlice.exprRecordProjFace? plan with
            | some _ => recordParamDeclaredFace modBytes modLen claim plan
            | none => False
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
  allClaims (symFragmentMatches artifact.modBytes artifact.modLen
    artifact.manifest.subject.hostRoles) artifact.symFragmentClaims ∧
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

/-! ### The fused vector-read template-implies-model theorem

Generic over every template hole, the code table (pinned only at the self
entry, exactly what byte acceptance certifies), and any `toIndex` helper
obeying the relational `__aint_to_index` contract: running the fused template
on a represented `(vector, index)` yields a represented `vecModel`. Partial
correctness — vacuous on trap or fuel exhaustion, like `Obligation.holds`.
The generated certificate's fused-read side condition discharges through this
theorem; the semantics is proven by the audited interpreter clauses, never by
a byte-pin of the claim body. -/
set_option maxRecDepth 100000 in
set_option maxHeartbeats 4000000 in
theorem vectorGetOrDefault_simulates_model
    (carrier toIndexIdx boxIdx arrTy : Nat) (d : Int)
    (hIdx : toIndexIdx ≠ boxIdx)
    (S : CarrierSpec carrier)
    (toIndex : List WVal → Option WVal)
    (hToIndex : ∀ n w r, intRepr S n w → toIndex [w] = some r →
      r = .i32v (toIndexW n))
    (code : CodeTbl) (self : Nat)
    (hCode : code self = some
      ⟨2, 1, AverCert.PlanLower.vectorGetOrDefaultTemplate toIndexIdx boxIdx arrTy d⟩)
    (fuel : Nat) (v : List Int) (i : Int) (vs : List WVal) (w : WVal)
    (hDom : vecDomRepr carrier arrTy S (v, i) vs)
    (hRun : wFuncN code
      (vectorGetOrDefaultHostSlots carrier toIndexIdx boxIdx toIndex)
      fuel self vs = some w) :
    intRepr S (vecModel d (v, i)) w := by
  obtain ⟨elems, wi, rfl, hlen0, hbound, hall0, hwi0⟩ := hDom
  -- Re-state the relation components with `(v, i).fst/.snd` projected away so
  -- `omega` sees one atom per length.
  have hlen : elems.length = v.length := hlen0
  have hall : ∀ k, k < v.length → ∃ w, elems[k]? = some w ∧
      intRepr S (v[k]!) w := hall0
  have hwi : intRepr S i wi := hwi0
  have hbox : ¬(boxIdx = toIndexIdx) := fun h => hIdx h.symm
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      cases htix : toIndex [wi] with
      | none =>
          simp [wFuncN, hCode, AverCert.PlanLower.vectorGetOrDefaultTemplate,
            vectorGetOrDefaultHostSlots, initLocals, wRunF, popArgs, htix] at hRun
      | some r =>
          have hr := hToIndex i wi r hwi htix
          subst hr
          by_cases hin : 0 ≤ i ∧ i < (v.length : Int)
          · -- In bounds: the hit arm reads the represented element.
            have hlt31 : i < 2147483648 := by omega
            have ht : toIndexW i = i := by
              simp [toIndexW, hin.1, hlt31]
            rw [ht] at htix
            have hkn : i.toNat < v.length := by omega
            obtain ⟨wv, hw, hwr⟩ := hall i.toNat hkn
            have hidx : i.toNat < elems.length := by omega
            have hwv : elems[i.toNat] = wv := by
              simpa [List.getElem?_eq_getElem hidx] using hw
            have hemodI : i.emod 4294967296 = i := by
              show i % 4294967296 = i
              omega
            have hemodL : ((elems.length : Int)).emod 4294967296 =
                (elems.length : Int) := by
              show (elems.length : Int) % 4294967296 = (elems.length : Int)
              omega
            have hilen : i < (elems.length : Int) := by omega
            simp [wFuncN, hCode, AverCert.PlanLower.vectorGetOrDefaultTemplate,
              vectorGetOrDefaultHostSlots, initLocals, wRunF, popArgs, b32,
              htix, ht, hin.1, hemodI, hemodL, hilen, hbox, hw] at hRun
            subst hRun
            simpa [vecModel, hin, hwv] using hwr
          · -- Out of bounds: the miss arm boxes the literal default.
            have hmodel : vecModel d (v, i) = d := by
              simp only [vecModel]
              exact if_neg hin
            rw [hmodel]
            by_cases hsmall : 0 ≤ i ∧ i < 2147483648
            · -- The extracted index is `i` itself but fails the unsigned
              -- length test (`i >= len`).
              have ht : toIndexW i = i := by simp [toIndexW, hsmall]
              rw [ht] at htix
              have hemodI : i.emod 4294967296 = i := by
                show i % 4294967296 = i
                omega
              have hemodL : ((elems.length : Int)).emod 4294967296 =
                  (elems.length : Int) := by
                show (elems.length : Int) % 4294967296 = (elems.length : Int)
                omega
              have hnlt : ¬(i < (elems.length : Int)) := by omega
              simp [wFuncN, hCode, AverCert.PlanLower.vectorGetOrDefaultTemplate,
                vectorGetOrDefaultHostSlots, initLocals, wRunF, popArgs, b32,
                htix, ht, hsmall.1, hemodI, hemodL, hnlt, hbox, boxRef] at hRun
              subst hRun
              exact S.smallIntro d
            · -- The helper collapses the index to the sentinel `-1`, which
              -- fails the signed lower-bound test.
              have ht : toIndexW i = -1 := by simp [toIndexW, hsmall]
              rw [ht] at htix
              simp [wFuncN, hCode, AverCert.PlanLower.vectorGetOrDefaultTemplate,
                vectorGetOrDefaultHostSlots, initLocals, wRunF, popArgs, b32,
                htix, ht, hbox, boxRef] at hRun
              subst hRun
              exact S.smallIntro d

/-! ### The record-parameter transport (HEq pins onto the obligation fields)

Same discipline as `DeclaredEnvelopeAcceptTransport`: the obligation's field
values are supplied as ordinary universally quantified variables so `subst`
applies once each pin is turned into an `Eq`, and no cast residue survives. The
core is `SchemaCore.recordParam_simulates_model` — the single generic
template-implies-model theorem the certified Plan's record declarations
instantiate. -/

/-- The canonical lowering of the recognized record-projection body is exactly
    the two-instruction `recordProjTemplate` — by computation, for every
    carrier, struct index, field and declared node types. -/
theorem lowerBlock_recordProj (carrier structIdx field : Nat) (ty0 ty1 : FragTy) :
    AverCert.PlanLower.lowerBlock carrier
      { nodes := [{ id := 0, ty := ty0, kind := .local 0 },
                  { id := 1, ty := ty1, kind := .structGetUser structIdx field 0 }],
        result := 1 } =
    some (recordProjTemplate structIdx field) := rfl

/-- The dependent-cast core for the record-parameter face: with the obligation
    field values as free variables and the face's pins as `Eq`/`HEq`, a
    successful run of the pinned template on a represented record yields a
    represented model value — exactly `recordParam_simulates_model`, carried
    onto the pinned fields. Generic over the host table (the template makes no
    host call) and the declared-locals count. -/
theorem recordParam_transport
    (claimCarrier : Nat)
    (fields : List TypeDecl) (structIdx field : Nat)
    (hfield : field < fields.length)
    (carrier : Nat) (Dom Cod : Type)
    (domRepr : CarrierSpec carrier → Dom → List WVal → Prop)
    (codRepr : CarrierSpec carrier → Cod → WVal → Prop)
    (model : Dom → Cod)
    (hcar : carrier = claimCarrier)
    (hDom : HEq Dom (RecordFields fields))
    (hCod : HEq Cod (RecordVal (fields[field]'hfield)))
    (hdomRepr : HEq domRepr (recordParamDomRepr claimCarrier structIdx fields))
    (hcodRepr : HEq codRepr (recordParamCodRepr claimCarrier fields field hfield))
    (hmodel : HEq model (recordParamModel fields field hfield))
    (code : CodeTbl) (host : HostTbl) (self nlocals : Nat)
    (hCode : code self = some ⟨1, nlocals, recordProjTemplate structIdx field⟩)
    (S : CarrierSpec carrier) (fuel : Nat) (x : Dom) (vs : List WVal) (w : WVal)
    (hdom : domRepr S x vs)
    (hRun : wFuncN code host fuel self vs = some w) :
    codRepr S (model x) w := by
  subst hcar
  have hDomEq : Dom = RecordFields fields := eq_of_heq hDom
  subst hDomEq
  have hCodEq : Cod = RecordVal (fields[field]'hfield) := eq_of_heq hCod
  subst hCodEq
  have e1 : domRepr = recordParamDomRepr carrier structIdx fields := eq_of_heq hdomRepr
  subst e1
  have e2 : codRepr = recordParamCodRepr carrier fields field hfield := eq_of_heq hcodRepr
  subst e2
  have e3 : model = recordParamModel fields field hfield := eq_of_heq hmodel
  subst e3
  obtain ⟨ws, rfl, hrepr⟩ := hdom
  exact recordParam_simulates_model S structIdx field nlocals fields hfield
    host code self hCode fuel x ws w hrepr hRun

#print axioms symFragmentFace_none_of_recordProj
#print axioms recordParam_transport

end AverCert.StandardFace
