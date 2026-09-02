/-
The semantic face of an accepted obligation is selected from the checked
claim and plan data.  The certificate may still provide source declarations
for models that cannot be reconstructed from Wasm, but it may not weaken the
standard domain, codomain, or representation relations of a known family.
-/
import AcceptedArtifactCore
import ConstructVerbatimSoundness
import RecordComputeBridge
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

/-! ### Int value-versus-value comparison faces (`__aint_cmp` / `__aint_eq`)

Two Int VALUES compared against each other — `a >= b`, `a == b`, and the
`match a < b { true -> a; false -> b }` selection — leave the part of the
fragment grammar that lowers without a helper: the wasm-gc emitter calls a
runtime comparison helper and reads its raw `i32` verdict.
`genericFragmentAllowedFuel` rejects EVERY `.hostCall` node outright, so these
plans need an exact-pinned face — the same node-by-node discipline
`classifyIntAdd` uses — and NOT a widened generic gate.

Both faces are stated over the SMALL BAND (`intPairSmallBandDomRepr`: each
argument is the literal `carrierSmall` encoding of an integer in `[-2^63,
2^63)`), which is exactly the domain the two assumed helper contracts in
`Obligation.holds` are quantified over. That is not a convenience: a face
stated over the full representation relation would need a relational contract,
and a relational contract is REFUTABLE here — `CarrierSpec.smallIntro` admits
`carrierSmall C k` as a representation of `k` for every `k`, while `__aint_eq`
decides a `Small` against a limb-carrying `Big` structurally and `__aint_cmp`
decides on raw sign fields `CarrierSpec.bigElim` does not constrain. Widening
the certified domain to limb-carrying operands needs a carrier specification
that pins those fields, not a wider premise.

The three relational operators read `__aint_cmp`, whose `-1`/`0`/`1` verdict is
typed `rawI32` and is always consumed by `i32.const 0` plus a signed relational
operator; `==` reads `__aint_eq`, whose `0`/`1` result IS the source Boolean and
carries no tail. `<=` is deliberately absent: no admitted plan produces
`i32.le_s` (see the `FragPrim` note), so a `le` arm would be reachable by
nothing.

The result of the SELECTION face is a PASSTHROUGH of an input local — the
emitted `if` yields `local.get 0` or `local.get 1`, boxes nothing, and calls no
helper in either arm — so its codomain relation is carried straight from the
chosen argument's `S.Repr` premise. -/

/-- Comparison operators admitted on two Int VALUES. `le` is absent by
    construction: the plan grammar has no `i32.le_s` primitive to lower it to. -/
inductive IntCmpOp where
  | lt
  | gt
  | ge
  | eq
deriving Repr, DecidableEq

/-- Face data of both comparison shapes: which operator, and the resolved index
    of the single runtime helper it reads (`__aint_cmp` for the relational
    operators, `__aint_eq` for equality). Acceptance binds that index to the
    module bytes and to the decoded role table; `hostTableBound` additionally
    forces the role/index pair to be the decoded one and every claimed index to
    be distinct. -/
structure IntCmpFace where
  op        : IntCmpOp
  helperIdx : Nat
deriving Repr, DecidableEq

/-- The Boolean the source operator denotes on two exact integers. -/
def intCmpModel : IntCmpOp → Int × Int → Bool
  | .lt, p => decide (p.1 < p.2)
  | .gt, p => decide (p.2 < p.1)
  | .ge, p => decide (p.2 ≤ p.1)
  | .eq, p => decide (p.1 = p.2)

/-- The Int the source selection denotes: one of its own two arguments. -/
def intSelectModel (op : IntCmpOp) (p : Int × Int) : Int :=
  if intCmpModel op p then p.1 else p.2

/-- Which contract slot the operator's helper occupies. Equality reads the
    `__aint_eq` contract (`eqW`), the three relational operators read the
    `__aint_cmp` contract (`cmpW`); nothing reads both. -/
def intCmpHelper (op : IntCmpOp) (cmp eq : List WVal → Option WVal) :
    List WVal → Option WVal :=
  match op with
  | .eq => eq
  | .lt | .gt | .ge => cmp

/-- The single host slot both faces present: the claimed helper index, arity 2,
    wired to the contract-bound helper its operator names. Every other index is
    absent from this table — which is not what makes the face safe (a body
    calling an absent index is merely stuck, and a stuck run says nothing). The
    guarantee comes from the PINNED BODY: the classifiers admit exactly one
    node list, whose only call is to the claimed helper index, so the emitted
    body cannot call anything else in the first place. -/
def intCmpHostSlots (op : IntCmpOp) (helperIdx : Nat)
    (cmp eq : List WVal → Option WVal) : HostTbl :=
  fun fn => if fn = helperIdx then some (2, intCmpHelper op cmp eq) else none

def intCmpHost (face : IntCmpFace) : HostBuilder :=
  fun _add _sub _mul _stringEq _stringConcat _toIndex cmp eq =>
    intCmpHostSlots face.op face.helperIdx cmp eq

/-- Domain representation of both comparison faces: the machine state is exactly
    the two LITERAL small carriers of two band-bounded integers. This is the
    same domain the `_hCmp` / `_hEq` premises of `Obligation.holds` are
    quantified over, and it is deliberately narrower than `S.Repr`: see the
    section note above for why a relational domain would be unsound to assume
    here. The carrier specification is still a parameter — `S` is used, through
    `smallIntro`, to represent the selection face's passthrough result. -/
def intPairSmallBandDomRepr (carrier : Nat) (_S : CarrierSpec carrier)
    (p : Int × Int) (vs : List WVal) : Prop :=
  vs = [carrierSmall carrier p.1, carrierSmall carrier p.2] ∧
    -(2 ^ 63 : Int) ≤ p.1 ∧ p.1 < 2 ^ 63 ∧ -(2 ^ 63 : Int) ≤ p.2 ∧ p.2 < 2 ^ 63

/-- The emitted comparison body: read both arguments, call the helper, and —
    for the three relational operators — compare the verdict against
    `i32.const 0`. This is `PlanLower.lowerBlock` of the pinned node list
    (`lowerBlock_intCmp`), not an independent claim about the emitter. -/
def intCmpTemplate (op : IntCmpOp) (helperIdx : Nat) : List WInstr :=
  match op with
  | .eq => [.localGet 0, .localGet 1, .call helperIdx]
  | .lt => [.localGet 0, .localGet 1, .call helperIdx, .i32Const 0, .i32LtS]
  | .gt => [.localGet 0, .localGet 1, .call helperIdx, .i32Const 0, .i32GtS]
  | .ge => [.localGet 0, .localGet 1, .call helperIdx, .i32Const 0, .i32GeS]

/-- The emitted selection body: the comparison above followed by an `if` whose
    arms are bare argument reads. -/
def intSelectTemplate (op : IntCmpOp) (helperIdx : Nat) : List WInstr :=
  intCmpTemplate op helperIdx ++ [.ifElse [.localGet 0] [.localGet 1]]

/-- The three-way verdict is negative exactly below, positive exactly above and
    non-negative exactly at-or-above. These are the only facts about `cmpW` the
    faces need, and they are what makes the `i32.const 0` tail meaningful. -/
theorem cmpW_lt_iff (a b : Int) : cmpW a b < 0 ↔ a < b := by
  unfold cmpW
  split
  · omega
  · split <;> omega

theorem cmpW_gt_iff (a b : Int) : 0 < cmpW a b ↔ b < a := by
  unfold cmpW
  split
  · omega
  · split <;> omega

theorem cmpW_ge_iff (a b : Int) : 0 ≤ cmpW a b ↔ b ≤ a := by
  unfold cmpW
  split
  · omega
  · split <;> omega

/-- The signed relational primitive each operator's tail uses. The map is
    injective and total on the admitted operators; every other primitive
    declines, which is what keeps a `i32.and`- or `i32.eq`-tailed body out of
    this face. -/
def intCmpOfPrim? : FragPrim → Option IntCmpOp
  | .i32LtS => some .lt
  | .i32GtS => some .gt
  | .i32GeS => some .ge
  | _ => none

/-- Pinned node list of a relational comparison: both arguments, the
    `__aint_cmp` call typed `rawI32`, the `i32.const 0`, and the signed tail. -/
def intCmpRelBlock (prim : FragPrim) (helperIdx : Nat) : FragBlock :=
  { nodes :=
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .intCarrier, kind := .local 1 },
       { id := 2, ty := .rawI32, kind := .hostCall .cmp helperIdx [0, 1] },
       { id := 3, ty := .rawI32, kind := .constI32 0 },
       { id := 4, ty := .boolI32, kind := .prim prim [2, 3] }],
    result := 4 }

/-- Pinned node list of the equality comparison: both arguments and the
    `__aint_eq` call, whose result is already the source Boolean. -/
def intCmpEqBlock (helperIdx : Nat) : FragBlock :=
  { nodes :=
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .intCarrier, kind := .local 1 },
       { id := 2, ty := .boolI32, kind := .hostCall .eq helperIdx [0, 1] }],
    result := 2 }

def intCmpBlock (op : IntCmpOp) (helperIdx : Nat) : FragBlock :=
  match op with
  | .lt => intCmpRelBlock .i32LtS helperIdx
  | .gt => intCmpRelBlock .i32GtS helperIdx
  | .ge => intCmpRelBlock .i32GeS helperIdx
  | .eq => intCmpEqBlock helperIdx

/-- Exact Int-comparison classifier over two Int parameters yielding a Bool.
    Node ids, declared types, argument lists and the block result are all pinned
    by the literal patterns; only the operator and the helper index are read out
    of the plan. -/
def classifyIntCmpBool (plan : ExprFragmentRawPlan) : Option IntCmpFace :=
  if plan.params = [.intCarrier, .intCarrier] && plan.result = .boolI32 then
    match plan.body with
    | { nodes :=
          [{ id := 0, ty := .intCarrier, kind := .local 0 },
           { id := 1, ty := .intCarrier, kind := .local 1 },
           { id := 2, ty := .boolI32, kind := .hostCall .eq helperIdx [0, 1] }],
        result := 2 } => some { op := .eq, helperIdx := helperIdx }
    | { nodes :=
          [{ id := 0, ty := .intCarrier, kind := .local 0 },
           { id := 1, ty := .intCarrier, kind := .local 1 },
           { id := 2, ty := .rawI32, kind := .hostCall .cmp helperIdx [0, 1] },
           { id := 3, ty := .rawI32, kind := .constI32 0 },
           { id := 4, ty := .boolI32, kind := .prim prim [2, 3] }],
        result := 4 } =>
        match intCmpOfPrim? prim with
        | some op => some { op := op, helperIdx := helperIdx }
        | none => none
    | _ => none
  else none

/-- Structural content of a fired comparison recognizer: the parameters, the
    declared result, and the body ARE the pinned shape the face is proven over. -/
theorem classifyIntCmpBool_spec
    (plan : ExprFragmentRawPlan) (face : IntCmpFace)
    (h : classifyIntCmpBool plan = some face) :
    plan.params = [.intCarrier, .intCarrier] ∧ plan.result = .boolI32 ∧
      plan.body = intCmpBlock face.op face.helperIdx := by
  unfold classifyIntCmpBool at h
  split at h
  case isFalse => exact absurd h (by simp)
  case isTrue hcond =>
    simp only [Bool.and_eq_true, decide_eq_true_eq] at hcond
    obtain ⟨hparams, hresult⟩ := hcond
    split at h
    case h_1 helperIdx heq =>
      injection h with hface
      subst hface
      exact ⟨hparams, hresult, by rw [heq]; rfl⟩
    case h_2 helperIdx prim heq =>
      split at h
      case h_2 => exact absurd h (by simp)
      case h_1 op hop =>
        injection h with hface
        subst hface
        refine ⟨hparams, hresult, ?_⟩
        rw [heq]
        cases prim <;> simp [intCmpOfPrim?] at hop <;> subst hop <;> rfl
    case h_3 => exact absurd h (by simp)

/-- One arm of the selection: a bare argument read, no box and no host call. -/
def intSelectArm (localIdx : Nat) : FragBlock :=
  { nodes := [{ id := 0, ty := .intCarrier, kind := .local localIdx }], result := 0 }

def intSelectRelBlock (prim : FragPrim) (helperIdx : Nat) : FragBlock :=
  { nodes :=
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .intCarrier, kind := .local 1 },
       { id := 2, ty := .rawI32, kind := .hostCall .cmp helperIdx [0, 1] },
       { id := 3, ty := .rawI32, kind := .constI32 0 },
       { id := 4, ty := .boolI32, kind := .prim prim [2, 3] },
       { id := 5, ty := .intCarrier,
         kind := .ifElse 4 (intSelectArm 0) (intSelectArm 1) }],
    result := 5 }

def intSelectEqBlock (helperIdx : Nat) : FragBlock :=
  { nodes :=
      [{ id := 0, ty := .intCarrier, kind := .local 0 },
       { id := 1, ty := .intCarrier, kind := .local 1 },
       { id := 2, ty := .boolI32, kind := .hostCall .eq helperIdx [0, 1] },
       { id := 3, ty := .intCarrier,
         kind := .ifElse 2 (intSelectArm 0) (intSelectArm 1) }],
    result := 3 }

def intSelectBlock (op : IntCmpOp) (helperIdx : Nat) : FragBlock :=
  match op with
  | .lt => intSelectRelBlock .i32LtS helperIdx
  | .gt => intSelectRelBlock .i32GtS helperIdx
  | .ge => intSelectRelBlock .i32GeS helperIdx
  | .eq => intSelectEqBlock helperIdx

/-- Exact Int-selection classifier: the comparison above, followed by an `if`
    whose two arms are pinned — LITERALLY, inside the pattern — to the bare
    reads of parameter 0 and parameter 1 in that order. Nothing else is
    admitted in an arm, so the result cannot be a freshly boxed value. -/
def classifyIntSelect (plan : ExprFragmentRawPlan) : Option IntCmpFace :=
  if plan.params = [.intCarrier, .intCarrier] && plan.result = .intCarrier then
    match plan.body with
    | { nodes :=
          [{ id := 0, ty := .intCarrier, kind := .local 0 },
           { id := 1, ty := .intCarrier, kind := .local 1 },
           { id := 2, ty := .boolI32, kind := .hostCall .eq helperIdx [0, 1] },
           { id := 3, ty := .intCarrier,
             kind := .ifElse 2
               { nodes := [{ id := 0, ty := .intCarrier, kind := .local 0 }],
                 result := 0 }
               { nodes := [{ id := 0, ty := .intCarrier, kind := .local 1 }],
                 result := 0 } }],
        result := 3 } => some { op := .eq, helperIdx := helperIdx }
    | { nodes :=
          [{ id := 0, ty := .intCarrier, kind := .local 0 },
           { id := 1, ty := .intCarrier, kind := .local 1 },
           { id := 2, ty := .rawI32, kind := .hostCall .cmp helperIdx [0, 1] },
           { id := 3, ty := .rawI32, kind := .constI32 0 },
           { id := 4, ty := .boolI32, kind := .prim prim [2, 3] },
           { id := 5, ty := .intCarrier,
             kind := .ifElse 4
               { nodes := [{ id := 0, ty := .intCarrier, kind := .local 0 }],
                 result := 0 }
               { nodes := [{ id := 0, ty := .intCarrier, kind := .local 1 }],
                 result := 0 } }],
        result := 5 } =>
        match intCmpOfPrim? prim with
        | some op => some { op := op, helperIdx := helperIdx }
        | none => none
    | _ => none
  else none

theorem classifyIntSelect_spec
    (plan : ExprFragmentRawPlan) (face : IntCmpFace)
    (h : classifyIntSelect plan = some face) :
    plan.params = [.intCarrier, .intCarrier] ∧ plan.result = .intCarrier ∧
      plan.body = intSelectBlock face.op face.helperIdx := by
  unfold classifyIntSelect at h
  split at h
  case isFalse => exact absurd h (by simp)
  case isTrue hcond =>
    simp only [Bool.and_eq_true, decide_eq_true_eq] at hcond
    obtain ⟨hparams, hresult⟩ := hcond
    split at h
    case h_1 helperIdx heq =>
      injection h with hface
      subst hface
      exact ⟨hparams, hresult, by rw [heq]; rfl⟩
    case h_2 helperIdx prim heq =>
      split at h
      case h_2 => exact absurd h (by simp)
      case h_1 op hop =>
        injection h with hface
        subst hface
        refine ⟨hparams, hresult, ?_⟩
        rw [heq]
        cases prim <;> simp [intCmpOfPrim?] at hop <;> subst hop <;> rfl
    case h_3 => exact absurd h (by simp)

/-- The complete face of an Int comparison: the two represented arguments in,
    the Boolean the operator denotes out, the single helper slot, and — unlike
    the `classifyIntAdd` family — the MODEL fixed by the wall rather than
    declared by the certificate. -/
def intCmpBoolFace (carrier : Nat) (face : IntCmpFace) : FaceSpec where
  carrier := carrier
  Dom := Int × Int
  Cod := Bool
  domRepr := intPairSmallBandDomRepr carrier
  codRepr := boolRepr
  host := intCmpHost face
  model? := some (intCmpModel face.op)

/-- The complete face of an Int selection. The codomain relation is the
    ordinary `intRepr`, satisfied by the CHOSEN ARGUMENT's own representation
    premise — the body boxes nothing. -/
def intSelectFace (carrier : Nat) (face : IntCmpFace) : FaceSpec where
  carrier := carrier
  Dom := Int × Int
  Cod := Int
  domRepr := intPairSmallBandDomRepr carrier
  codRepr := intRepr
  host := intCmpHost face
  model? := some (intSelectModel face.op)

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
        | .hostCall _ _ _ | .structGetUser _ _ _ | .structNew _ _ => false
        | .ifElse _ thenBlock elseBlock =>
            genericFragmentAllowedFuel fuel thenBlock &&
              genericFragmentAllowedFuel fuel elseBlock
        | .selfCall _ _ _ => false
        | .vectorGetOrDefault _ _ _ _ => false
        -- The sign template writes the declared scratch local and reads the
        -- carrier's limb/sign fields; the generic face has no carrier facts
        -- to interpret either with, so it fail-closes here.
        | .intSignCmp _ _ _ _ => false
        | .local _ | .constBool _ | .constI64 _ | .constI32 _ |
          .constF64Bits _ | .structGet _ _ | .refIsNull _ |
          .prim _ _ => true

noncomputable def genericFragmentAllowed (plan : ExprFragmentRawPlan) : Bool :=
  !plan.params.contains .adtRef &&
    plan.result != .adtRef &&
    plan.result != .intCarrier &&
    genericFragmentAllowedFuel (sizeOf plan.body + 1) plan.body

/-! ### Record projection-compute face (plan-as-claim over one flat Int record)

The face admits k record parameters of ONE pinned struct type whose fields
are all Int carriers, a body over the bridge's v1 node set, and a
record/Int/Bool result. Its meaning is the plan itself: the obligation's
model RUNS the checked plan over source values
(`RecordComputeBridge.sourceRunBlock`), so the report shows the exact
expression the bytes compute and no per-shape model term exists to get
wrong. -/

/-- Host slots of the compute face: the byte-derived role table lowered to
    the canonical `if fn = idx` chain; `box` wires the audited `boxRef`,
    add/sub/mul/cmp/eq wire the obligation's contract slots, and the one role
    the face's grammar never cites (`toIndex`) wires a trap-only slot at its
    honest arity. -/
def recordComputeSlots
    (carrier : Nat) (add sub mul cmp eq : List WVal → Option WVal) :
    List (HostRole × Nat) → HostTbl
  | [] => fun _ => none
  | (role, idx) :: rest => fun fn =>
      if fn = idx then
        some (match role with
          | .box => ((1 : Nat), boxRef carrier)
          | .add => ((2 : Nat), add)
          | .mul => ((2 : Nat), mul)
          | .sub => ((2 : Nat), sub)
          | .eq => ((2 : Nat), eq)
          | .cmp => ((2 : Nat), cmp)
          | .toIndex => ((1 : Nat), fun _ => none))
      else recordComputeSlots carrier add sub mul cmp eq rest fn

def recordComputeHost (carrier : Nat) (hostTable : List (HostRole × Nat)) :
    HostBuilder :=
  fun add sub mul _stringEq _stringConcat _toIndex cmp eq =>
    recordComputeSlots carrier add sub mul cmp eq hostTable

structure RecordComputeFace where
  structIdx : Nat
deriving Repr, DecidableEq

private theorem recordComputeHostRoleIdx_mem_pair
    (hostTable : List (HostRole × Nat)) (role : HostRole) (idx : Nat)
    (hLookup : AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx) :
    (role, idx) ∈ hostTable := by
  induction hostTable with
  | nil => simp [AverCert.PlanCheck.hostRoleIdx?] at hLookup
  | cons head rest ih =>
      rcases head with ⟨headRole, headIdx⟩
      by_cases hRole : headRole = role
      · subst headRole
        simp [AverCert.PlanCheck.hostRoleIdx?] at hLookup
        subst idx
        simp
      · simp [AverCert.PlanCheck.hostRoleIdx?, hRole] at hLookup
        simp [ih hLookup]

/-- Discharge-facing binding lemma: over a byte-derived role table whose
    indices are pairwise distinct, the compute-face slots bind the index the
    table resolves for each ADMITTED role (`box`/`add`/`sub`/`mul`/`cmp`/`eq`)
    to exactly that role's arity and wired contract function — `box` wires the
    audited `boxRef`. This is the single fact the discharge needs to satisfy
    the bridge's `hHost` hypothesis. -/
theorem recordComputeSlots_bind
    (carrier : Nat) (add sub mul cmp eq : List WVal → Option WVal)
    (hostTable : List (HostRole × Nat))
    (hDistinct : AverCert.PlanCheck.hostTableIndicesDistinct hostTable = true)
    (role : HostRole) (idx : Nat)
    (hRole : role ∈ [HostRole.box, HostRole.add, HostRole.sub, HostRole.mul,
      HostRole.cmp, HostRole.eq])
    (hLookup : AverCert.PlanCheck.hostRoleIdx? hostTable role = some idx) :
    recordComputeSlots carrier add sub mul cmp eq hostTable idx =
      some (RecordComputeBridge.roleArity role,
        RecordComputeBridge.roleFn (boxRef carrier) add sub mul cmp eq role) := by
  induction hostTable with
  | nil => simp [AverCert.PlanCheck.hostRoleIdx?] at hLookup
  | cons head rest ih =>
      rcases head with ⟨headRole, headIdx⟩
      simp only [AverCert.PlanCheck.hostTableIndicesDistinct,
        AverCert.PlanCheck.natListNoDup, List.map_cons,
        Bool.and_eq_true] at hDistinct
      rcases hDistinct with ⟨hHeadFresh, hRestDistinct⟩
      by_cases hR : headRole = role
      · subst headRole
        simp [AverCert.PlanCheck.hostRoleIdx?] at hLookup
        subst idx
        simp only [List.mem_cons, List.not_mem_nil, or_false] at hRole
        rcases hRole with rfl | rfl | rfl | rfl | rfl | rfl <;>
          simp [recordComputeSlots, RecordComputeBridge.roleArity,
            RecordComputeBridge.roleFn]
      · have hTailLookup :
            AverCert.PlanCheck.hostRoleIdx? rest role = some idx := by
          simpa [AverCert.PlanCheck.hostRoleIdx?, hR] using hLookup
        have hPairMem : (role, idx) ∈ rest :=
          recordComputeHostRoleIdx_mem_pair rest role idx hTailLookup
        have hNe : idx ≠ headIdx := by
          intro hEqIdx
          subst idx
          simp at hHeadFresh
          exact hHeadFresh role hPairMem
        change (if idx = headIdx then _ else
          recordComputeSlots carrier add sub mul cmp eq rest idx) = _
        rw [if_neg hNe]
        exact ih hRestDistinct hTailLookup


/-- The user-struct index a node cites, if any. -/
def fragNodeStructIdx? : FragNodeKind → Option Nat
  | .structGetUser tyIdx _ _ => some tyIdx
  | .structNew tyIdx _ => some tyIdx
  | _ => none

/-- Executable admission of one node kind against the byte-derived role
    table: exactly the bridge's v1 node set, with every host call citing the
    table's index for its role at the role's arity. The two i64-band checks
    are the sign template's exactness condition and the boxing helper's
    canonicity condition; both are decided here rather than assumed. -/
def recordComputeNodeOk
    (hostTable : List (HostRole × Nat)) : FragNodeKind → Bool
  | .local _ => true
  | .constI64 value => AverCert.PlanCheck.inI64Band value
  | .constI32 _ => true
  | .structGetUser _ _ _ => true
  | .structNew _ _ => true
  | .prim .i32LtS args => args.length == 2
  | .prim .i32GtS args => args.length == 2
  | .prim .i32GeS args => args.length == 2
  | .intSignCmp _ constant _ _ => AverCert.PlanCheck.inI64Band constant
  | .hostCall role f args =>
      (AverCert.PlanCheck.hostRoleIdx? hostTable role == some f) &&
        (match role with
          | .box => args.length == 1
          | .add | .sub | .mul | .cmp | .eq => args.length == 2
          | _ => false)
  | _ => false

/-- The nodes that make a body COMPUTE rather than merely PROJECT — the
    classifier's any-fact, named so the non-overlap lemmas below can cite one
    term. Three kinds qualify: a construction, ANY host call (`cmp` and `eq`
    included — they leave the carrier and decide an order), and the inline
    sign template, which is the emitter's open-coded comparison of a computed
    carrier against a literal and is therefore exactly as computing as the
    `cmp` call it replaces.

    Leaving `.intSignCmp` out was a SILENT non-admission: a projection-only
    sign test (`f.num >= 0`, no host call anywhere in the body) matched
    neither the two-node projection face nor the compute face, so the producer
    emitted no plan at all and the export dropped to source-level-only with no
    stated reason. The two-node projection faces stay ruled out because their
    bodies carry none of the three. -/
def fragNodeComputes (n : FragNode) : Bool :=
  match n.kind with
  | .structNew _ _ => true
  | .hostCall _ _ _ => true
  | .intSignCmp _ _ _ _ => true
  | _ => false

/-- Classifier of the compute face: every parameter is an opaque record
    reference, every node is in the admitted set, at least one node computes
    (`fragNodeComputes` — which also rules the two-node projection faces out),
    the result is a record/Int/Bool, and every cited user-struct index agrees
    on ONE pinned index. -/
def classifyRecordCompute
    (hostTable : List (HostRole × Nat)) (plan : ExprFragmentRawPlan) :
    Option RecordComputeFace :=
  if plan.params.all (· == .adtRef) &&
      plan.body.nodes.all (fun n => recordComputeNodeOk hostTable n.kind) &&
      plan.body.nodes.any fragNodeComputes &&
      (plan.result == .adtRef || plan.result == .intCarrier ||
        plan.result == .boolI32) then
    match plan.body.nodes.filterMap (fun n => fragNodeStructIdx? n.kind) with
    | [] => none
    | i :: rest =>
        if rest.all (· == i) &&
            RecordComputeBridge.planTypedB i
              (fun nodeId =>
                ((plan.body.nodes[nodeId]?).map (fun n => n.ty)).getD .i64)
              plan.params plan.body.nodes then
          some { structIdx := i }
        else none
  else none

/-- The declared wasm result type of a compute-face plan: a record result is
    the pinned struct reference, an Int result the carrier reference, a Bool
    result the i32. Anything else has no certified signature. -/
def recordComputeResultValType (carrier structIdx : Nat) :
    FragTy → Option CertDecode.ValType
  | .adtRef => some (AverCert.WasmSlice.nullableRefType structIdx)
  | .intCarrier => some (AverCert.WasmSlice.nullableRefType carrier)
  | .boolI32 => some (.numeric 0x7f)
  | _ => none

/-- Domain representation of the compute face: pointwise-SRepr inputs whose
    source shapes match the plan's declared parameter types.

    DISCLOSURE: `SRepr` on an Int carrier means REPRESENTED AND CANONICAL, so
    this face states its claim about inputs (and record fields) that are in the
    runtime's normal form. That is every value the emitted module can build —
    each carrier it produces comes out of `__aint_normalize` — but it is an
    assumption about the inputs all the same, and it is the assumption the
    structural helpers (`__aint_cmp`, `__aint_eq`) and the inline sign template
    need to be exact. -/
def recordComputeDomRepr (carrier structIdx : Nat) (params : List FragTy) :
    CarrierSpec carrier → List RecordComputeBridge.SVal → List WVal → Prop :=
  fun S svs vs =>
    RecordComputeBridge.SReprAll S structIdx svs vs ∧
    svs.length = params.length ∧
    ∀ (i : Nat) (sv : RecordComputeBridge.SVal), svs[i]? = some sv →
      params[i]? = some (RecordComputeBridge.svalTy sv)

/-- Codomain representation: the model produced a source value and the
    machine word represents it. -/
def recordComputeCodRepr (carrier structIdx : Nat) :
    CarrierSpec carrier → Option RecordComputeBridge.SVal → WVal → Prop :=
  fun S o w => ∃ sv, o = some sv ∧
    RecordComputeBridge.SRepr S structIdx sv w

/-- The compute face's model IS the checked plan, run by the audited source
    evaluator (at the wall's audited fixed fuel `PlanCheck.maxFuel` — the same
    fuel the canonical lowering and the bridge's completeness speak at, so the
    discharge needs no fuel-monotonicity step). -/
def recordComputeModel (body : FragBlock) :
    List RecordComputeBridge.SVal → Option RecordComputeBridge.SVal :=
  fun svs =>
    RecordComputeBridge.sourceRunBlock AverCert.PlanCheck.maxFuel body svs

/-- The compute face as a standard face: the domain is the source values of
    the k record parameters, the codomain the (optional) source result, and
    the model IS the checked plan, run by the audited source evaluator. -/
noncomputable def recordCompute (carrier : Nat) (face : RecordComputeFace)
    (hostTable : List (HostRole × Nat)) (plan : ExprFragmentRawPlan) :
    FaceSpec where
  carrier := carrier
  Dom := List RecordComputeBridge.SVal
  Cod := Option RecordComputeBridge.SVal
  domRepr := recordComputeDomRepr carrier face.structIdx plan.params
  codRepr := recordComputeCodRepr carrier face.structIdx
  host := recordComputeHost carrier hostTable
  model? := some (recordComputeModel plan.body)

/-- The declared face carried by a record projection-compute claim. Byte
    conjuncts: the pinned struct's type-section entry IS the wall lowering of
    an all-Int flat record declaration at that index; the certified export's
    declared signature is EXACTLY k references to the pinned struct in and
    the declared result out; the classifier fired on this plan at this face;
    the byte-derived carrier is the claimed one. Meaning conjuncts: the
    obligation's Dom/Cod/representations/host/model are the wall's compute
    face terms over the checked plan (`StandardFace.Matches`). -/
def recordComputeDeclaredFace
    (modBytes modLen : Nat) (claim : SymFragmentClaim)
    (plan : ExprFragmentRawPlan) (face : RecordComputeFace) : Prop :=
  claim.obligation.policy = .simulatesModel ∧
  claim.obligation.carrier = claim.carrier ∧
  classifyRecordCompute claim.hostTable plan = some face ∧
  ∃ (fields : List TypeDecl) (resultTy : CertDecode.ValType),
    (fields.all fun f => match f with
      | .intCarrier => true
      | _ => false) = true ∧
    fields.length ≠ 0 ∧
    checkRecordDecl (.record face.structIdx fields) = true ∧
    CertDecode.carrierState modBytes modLen = some (some claim.carrier) ∧
    AverCert.WasmSlice.typeSectionMatches
      (fun entry =>
        decide (lowerTypeDecl claim.carrier lowerTypeDeclFuel
          (.record face.structIdx fields) = some entry))
      modBytes modLen face.structIdx = true ∧
    recordComputeResultValType claim.carrier face.structIdx plan.result =
      some resultTy ∧
    AverCert.WasmSlice.funcTypeMatchesExact
      modBytes modLen claim.exportNameBytes
      (List.replicate plan.params.length
        (AverCert.WasmSlice.nullableRefType face.structIdx))
      [resultTy] = true ∧
    (StandardFace.known
      (recordCompute claim.carrier face claim.hostTable plan)).Matches
      claim.obligation

/-- Structural content of a fired compute classifier: the four Bool facts of
    its admission condition (all-`.adtRef` parameters, every node admitted,
    at least one computing node, record/Int/Bool result). The pinned-index
    fact is deliberately absent — non-overlap needs only the condition. -/
theorem classifyRecordCompute_spec
    (hostTable : List (HostRole × Nat)) (plan : ExprFragmentRawPlan)
    (face : RecordComputeFace)
    (h : classifyRecordCompute hostTable plan = some face) :
    plan.params.all (· == .adtRef) = true ∧
    plan.body.nodes.all (fun n => recordComputeNodeOk hostTable n.kind) = true ∧
    plan.body.nodes.any fragNodeComputes = true ∧
    (plan.result == .adtRef || plan.result == .intCarrier ||
      plan.result == .boolI32) = true := by
  simp only [classifyRecordCompute] at h
  split at h
  case isTrue hcond =>
    simp only [Bool.and_eq_true] at hcond
    obtain ⟨⟨⟨h1, h2⟩, h3⟩, h4⟩ := hcond
    exact ⟨h1, h2, h3, h4⟩
  case isFalse => exact absurd h (by simp)

/-- An all-`.adtRef` parameter list is never one of the Int-carrier parameter
    lists the exact-pinned Int classifiers require (an empty list fails all
    three as well). -/
theorem allAdtRef_ne_intLists
    (params : List FragTy)
    (hAll : params.all (· == .adtRef) = true) :
    params ≠ [.intCarrier] ∧ params ≠ [.adtRef, .intCarrier] ∧
      params ≠ [.intCarrier, .intCarrier] := by
  refine ⟨?_, ?_, ?_⟩ <;> intro hEq <;> rw [hEq] at hAll <;> simp at hAll

/-- A fired tag-dispatch classifier's body carries an `i32.eq` primitive node
    (the five-node match's `n3`) — a kind the compute face's node admission
    rejects. -/
theorem classifyTagDispatch_hasPrim
    (plan : ExprFragmentRawPlan) (face : TagDispatchFace)
    (h : classifyTagDispatch plan = some face) :
    ∃ n ∈ plan.body.nodes, ∃ args, n.kind = .prim .i32Eq args := by
  unfold classifyTagDispatch at h
  split at h
  case isFalse => exact absurd h (by simp)
  case isTrue =>
    split at h
    case h_2 => exact absurd h (by simp)
    case h_1 n0 n1 n2 n3 n4 hnodes =>
      split at h
      case h_2 => exact absurd h (by simp)
      case h_1 optIdx tag hitBlk missBlk h0 h1 h2 h3 h4 =>
        exact ⟨n3, by rw [hnodes]; simp, [1, 2], h3⟩

/-- The compute face's node admission has no `i32.eq` arm: it admits exactly
    the three SIGNED RELATIONAL primitives that read a `__aint_cmp` verdict, so
    a body whose every node passes `recordComputeNodeOk` carries no `i32.eq` —
    which is the node the tag-dispatch face's five-node shape must have. -/
theorem recordComputeNodeOk_no_eqPrim
    (hostTable : List (HostRole × Nat)) (nodes : List FragNode)
    (hAll : nodes.all (fun n => recordComputeNodeOk hostTable n.kind) = true)
    (n : FragNode) (hMem : n ∈ nodes) (args : List Nat) :
    n.kind ≠ .prim .i32Eq args := by
  intro hkind
  simp only [List.all_eq_true] at hAll
  have hOk := hAll n hMem
  rw [hkind] at hOk
  simp [recordComputeNodeOk] at hOk

/-- The two-node opaque projection body computes nothing: no construction
    and no host call, against the compute classifier's any-fact. -/
theorem exprProjectionFace?_no_compute
    (plan : ExprFragmentRawPlan) (p : Nat × Nat)
    (h : AverCert.WasmSlice.exprProjectionFace? plan = some p) :
    plan.body.nodes.any fragNodeComputes = false := by
  unfold AverCert.WasmSlice.exprProjectionFace? at h
  split at h
  case isFalse => exact absurd h (by simp)
  case isTrue =>
    split at h
    case h_2 => exact absurd h (by simp)
    case h_1 n0 n1 hnodes =>
      split at h
      case h_2 => exact absurd h (by simp)
      case h_1 structIdx fieldIdx h0 h1 =>
        rw [hnodes]
        simp [fragNodeComputes, h0, h1]

/-- The two-node record-projection body computes nothing either. -/
theorem exprRecordProjFace?_no_compute
    (plan : ExprFragmentRawPlan) (structIdx field : Nat)
    (h : AverCert.WasmSlice.exprRecordProjFace? plan
      = some (structIdx, field)) :
    plan.body.nodes.any fragNodeComputes = false := by
  obtain ⟨-, -, hbody⟩ :=
    AverCert.WasmSlice.exprRecordProjFace?_spec plan structIdx field h
  rw [hbody]
  simp [fragNodeComputes]

/-- The generic gate's walker rejects every construction and host call, so a
    walked body has no node the compute classifier's any-fact counts. -/
theorem genericFragmentAllowedFuel_no_compute
    (fuel : Nat) (block : FragBlock)
    (h : genericFragmentAllowedFuel fuel block = true) :
    block.nodes.any fragNodeComputes = false := by
  cases fuel with
  | zero => simp [genericFragmentAllowedFuel] at h
  | succ fuel =>
      simp only [genericFragmentAllowedFuel, List.all_eq_true] at h
      simp only [List.any_eq_false]
      intro n hMem
      have hn := (Bool.and_eq_true _ _).mp (h n hMem) |>.2
      cases hkind : n.kind
      case structNew tyIdx args =>
        rw [hkind] at hn
        simp at hn
      case hostCall role funcIdx args =>
        rw [hkind] at hn
        simp at hn
      -- The generic walker fail-closes on the sign template (it has no carrier
      -- facts to read the limb/sign fields with), so a walked body carries none.
      case intSignCmp op constant scratch value =>
        rw [hkind] at hn
        simp at hn
      all_goals simp [fragNodeComputes, hkind]

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
                      match classifyIntCmpBool plan with
                      | some face =>
                          some (.known (intCmpBoolFace claim.carrier face))
                      | none =>
                          match classifyIntSelect plan with
                          | some face =>
                              some (.known (intSelectFace claim.carrier face))
                          | none =>
                              if genericFragmentAllowed plan then
                                some (.known
                                  (fragment claim.carrier plan.params plan.result))
                              else none

/-- No `FaceSpec` branch of the classify chain fires on a record-projection
    plan, so appending the record face on the chain's `none` arm neither
    shadows nor reorders any existing face. Shape by shape: `classifyIntAdd`
    needs an `.intCarrier` parameter list, `exprProjectionFace?` an `.adtRef`
    result, `classifyTagDispatch` a five-node body, `classifyVectorGetOrDefault`
    a two-parameter list, the two Int comparison classifiers a two-Int-carrier
    parameter list, and the generic gate forbids `.adtRef` parameters — each
    contradicted by the recognized record shape. -/
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
  have hIntCmpBool : classifyIntCmpBool plan = none := by
    unfold classifyIntCmpBool
    simp [hparams]
  have hIntSelect : classifyIntSelect plan = none := by
    unfold classifyIntSelect
    simp [hparams]
  have hGeneric : genericFragmentAllowed plan = false := by
    unfold genericFragmentAllowed
    simp [hparams]
  simp [hIntAdd, hProjection, hTagDispatch, hVectorGet, hIntCmpBool, hIntSelect,
    hGeneric]

/-- No `FaceSpec` branch of the classify chain fires on a compute-face plan,
    so trying the compute face on the chain's `none` arm neither shadows nor
    reorders any existing face. Parameter kills: every compute parameter is
    `.adtRef`, while `classifyIntAdd`, `classifyVectorGetOrDefault` and the
    two Int comparison classifiers pin Int-carrier parameter lists (an empty
    list fails all four as well). Body kills: `classifyTagDispatch` needs an
    `i32.eq` primitive the node admission rejects; `exprProjectionFace?` and
    the generic walker admit no construction or host call, against the
    classifier's any-fact; a nonempty all-`.adtRef` parameter list is
    rejected by the generic gate directly. -/
theorem symFragmentFace_none_of_recordCompute
    (claim : SymFragmentClaim) (plan : ExprFragmentRawPlan)
    (hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan = some plan)
    (face : RecordComputeFace)
    (hFace : classifyRecordCompute claim.hostTable plan = some face) :
    symFragmentFace claim = none := by
  obtain ⟨hparams, hAllOk, hAny, -⟩ :=
    classifyRecordCompute_spec claim.hostTable plan face hFace
  obtain ⟨hne1, hne2, hne3⟩ := allAdtRef_ne_intLists plan.params hparams
  unfold symFragmentFace
  rw [hEncode]
  have hIntAdd : classifyIntAdd plan = none := by
    unfold classifyIntAdd
    simp [hne1]
  have hProjection : AverCert.WasmSlice.exprProjectionFace? plan = none := by
    cases hp : AverCert.WasmSlice.exprProjectionFace? plan with
    | none => rfl
    | some p =>
        exact absurd (exprProjectionFace?_no_compute plan p hp) (by simp [hAny])
  have hTagDispatch : classifyTagDispatch plan = none := by
    cases ht : classifyTagDispatch plan with
    | none => rfl
    | some f =>
        obtain ⟨n, hMem, args, hkind⟩ := classifyTagDispatch_hasPrim plan f ht
        exact absurd hkind
          (recordComputeNodeOk_no_eqPrim claim.hostTable plan.body.nodes hAllOk
            n hMem args)
  have hVectorGet : classifyVectorGetOrDefault plan = none := by
    unfold classifyVectorGetOrDefault
    simp [hne2]
  have hIntCmpBool : classifyIntCmpBool plan = none := by
    unfold classifyIntCmpBool
    simp [hne3]
  have hIntSelect : classifyIntSelect plan = none := by
    unfold classifyIntSelect
    simp [hne3]
  have hGeneric : genericFragmentAllowed plan = false := by
    unfold genericFragmentAllowed
    cases hps : plan.params with
    | nil =>
        cases hw : genericFragmentAllowedFuel (sizeOf plan.body + 1) plan.body
          with
        | false => simp [hw]
        | true =>
            exact absurd (genericFragmentAllowedFuel_no_compute _ _ hw)
              (by simp [hAny])
    | cons p ps =>
        have hp : p = .adtRef := by
          rw [hps] at hparams
          simp at hparams
          exact hparams.1
        simp [hp]
  simp [hIntAdd, hProjection, hTagDispatch, hVectorGet, hIntCmpBool, hIntSelect,
    hGeneric]

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
            | none =>
                match classifyRecordCompute claim.hostTable plan with
                | some face =>
                    recordComputeDeclaredFace modBytes modLen claim plan face
                | none => False
        | none => False

/-! ### Declared-index envelope faces (user ADT claims)

The plan/certificate DECLARES ADT envelopes (root, carrier, every constructor's
flattened index, shape, and payload target) plus the opaque type-section prefix
before the constructor entries; the wall CONFIRMS those declarations with ONE
byte-slice equality (`concatPinnedAt`). The declared envelope then pins the
obligation's `Dom`/`domRepr`/`codRepr`/`model` to wall terms computed from the
checked plan, closing the former free-model / free-`domRepr` faces. String.concat
uses only the semantic-field transport below; its ABI/type-section pins live in
`stringConcatPlanAccepted` rather than a synthetic empty ADT envelope. -/

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

/-- The semantic face carried by a String.concat claim. The literal chunks are
    required non-empty so the declared `resultTy` occurs inside the byte-matched
    code entry (`array.new_data resultTy`) — a chunk-less plan would leave the
    result element type a free semantic tag. The exported/helper declared
    function types are byte-pinned by `stringConcatPlanAccepted`, not here. -/
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

/-! ### The Int comparison faces: chain selection, lowering, and the two
template-implies-model theorems

The classifiers fire strictly after every earlier `FaceSpec` branch and are
mutually exclusive with all of them by PARAMETER LIST alone (`classifyIntAdd`
takes one Int carrier; the projection, tag-dispatch and fused-read shapes all
take an `.adtRef`), and with each other by declared RESULT (`.boolI32` versus
`.intCarrier`). The generic gate below them still rejects both, because it
rejects every `.hostCall` node.

The two `simulates_model` theorems are the audited content of this leg: generic
over the carrier spec, the helper index, the declared-local count, the code
table (pinned only at the self entry, exactly what byte acceptance certifies),
the fuel, and ANY helper obeying the `__aint_cmp` / `__aint_eq` contract,
running the emitted body on the small carriers of two band-bounded integers
yields a represented model value. Partial correctness — vacuous on trap or fuel
exhaustion, like `Obligation.holds`. -/

theorem symFragmentFace_intCmpBool
    (claim : SymFragmentClaim) (plan : ExprFragmentRawPlan) (face : IntCmpFace)
    (hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan = some plan)
    (hCls : classifyIntCmpBool plan = some face) :
    symFragmentFace claim = some (.known (intCmpBoolFace claim.carrier face)) := by
  obtain ⟨hparams, _hresult, _hbody⟩ := classifyIntCmpBool_spec plan face hCls
  unfold symFragmentFace
  rw [hEncode]
  have hIntAdd : classifyIntAdd plan = none := by
    unfold classifyIntAdd
    simp [hparams]
  have hProjection : AverCert.WasmSlice.exprProjectionFace? plan = none := by
    unfold AverCert.WasmSlice.exprProjectionFace?
    simp [hparams]
  have hTagDispatch : classifyTagDispatch plan = none := by
    unfold classifyTagDispatch
    simp [hparams]
  have hVectorGet : classifyVectorGetOrDefault plan = none := by
    unfold classifyVectorGetOrDefault
    simp [hparams]
  simp [hIntAdd, hProjection, hTagDispatch, hVectorGet, hCls]

theorem symFragmentFace_intSelect
    (claim : SymFragmentClaim) (plan : ExprFragmentRawPlan) (face : IntCmpFace)
    (hEncode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      claim.hostTable claim.structTable claim.plan = some plan)
    (hCls : classifyIntSelect plan = some face) :
    symFragmentFace claim = some (.known (intSelectFace claim.carrier face)) := by
  obtain ⟨hparams, hresult, _hbody⟩ := classifyIntSelect_spec plan face hCls
  unfold symFragmentFace
  rw [hEncode]
  have hIntAdd : classifyIntAdd plan = none := by
    unfold classifyIntAdd
    simp [hparams]
  have hProjection : AverCert.WasmSlice.exprProjectionFace? plan = none := by
    unfold AverCert.WasmSlice.exprProjectionFace?
    simp [hparams]
  have hTagDispatch : classifyTagDispatch plan = none := by
    unfold classifyTagDispatch
    simp [hparams]
  have hVectorGet : classifyVectorGetOrDefault plan = none := by
    unfold classifyVectorGetOrDefault
    simp [hparams]
  have hIntCmpBool : classifyIntCmpBool plan = none := by
    unfold classifyIntCmpBool
    simp [hresult]
  simp [hIntAdd, hProjection, hTagDispatch, hVectorGet, hIntCmpBool, hCls]

/-- The carrier binding is NOT optional for either face, and does not depend on
    the host table being non-empty. Both faces pin an `.intCarrier` parameter
    list, which makes `fragPlanMentionsIntCarrier` — and therefore
    `symFragmentCarrierBindingRequired` — true whatever the table holds, so
    acceptance's `symFragmentCarrierBound` must present the DECODED carrier
    state (`CertDecode.carrierState`) and not a claimed index. The table trigger
    fires too (the encoder resolves a `hostCall` role only through the table, so
    a plan carrying one cannot come from an empty table), but this statement
    stands without it. -/
theorem classifyIntCmpBool_forcesCarrierBinding
    (hostTable : List (HostRole × Nat)) (plan : ExprFragmentRawPlan)
    (face : IntCmpFace) (h : classifyIntCmpBool plan = some face) :
    symFragmentCarrierBindingRequired hostTable plan = true := by
  obtain ⟨hparams, -, -⟩ := classifyIntCmpBool_spec plan face h
  simp [symFragmentCarrierBindingRequired, fragPlanMentionsIntCarrier, hparams,
    fragTyIsIntCarrier]

theorem classifyIntSelect_forcesCarrierBinding
    (hostTable : List (HostRole × Nat)) (plan : ExprFragmentRawPlan)
    (face : IntCmpFace) (h : classifyIntSelect plan = some face) :
    symFragmentCarrierBindingRequired hostTable plan = true := by
  obtain ⟨hparams, -, -⟩ := classifyIntSelect_spec plan face h
  simp [symFragmentCarrierBindingRequired, fragPlanMentionsIntCarrier, hparams,
    fragTyIsIntCarrier]

/-- The canonical lowering of each pinned comparison body is exactly its
    template — by computation, for every carrier, operator and helper index. -/
theorem lowerBlock_intCmp (carrier : Nat) (op : IntCmpOp) (helperIdx : Nat) :
    AverCert.PlanLower.lowerBlock carrier (intCmpBlock op helperIdx)
      = some (intCmpTemplate op helperIdx) := by
  cases op <;> rfl

theorem lowerBlock_intSelect (carrier : Nat) (op : IntCmpOp) (helperIdx : Nat) :
    AverCert.PlanLower.lowerBlock carrier (intSelectBlock op helperIdx)
      = some (intSelectTemplate op helperIdx) := by
  cases op <;> rfl

/-! ### The pinned node lists reproduce the measured witness bytes

`PlanBytes` lowers the pinned blocks to the exact code-entry bodies read off
the real modules in this leg's empirical stage: the ONE-element locals vector
holding an UNUSED carrier-typed local (not optional padding — the emitter
declares it), the two argument reads, the helper call, the `i32.const 0` and
signed tail, and — for the selection — an `if` whose block type is the INLINE
nullable-carrier-reference value type `63 <s33 carrier>`, never an empty or
`i32` block-type byte. The wide instantiation exercises the multi-byte
`uleb32`/`s33` splices that every measured module leaves untouched (all their
holes are below `0x80`). -/

/-- `nowMs >= deadlineMs` at carrier 2, helper index 9: the 14-byte body. -/
theorem intCmpBoolBytes_relational :
    AverCert.PlanBytes.lowerExprFragmentBodyBytes 2
        { profile := "expr-fragment-v1", params := [.intCarrier, .intCarrier],
          result := .boolI32, body := intCmpBlock .ge 9 } =
      some [0x01, 0x01, 0x63, 0x02, 0x20, 0x00, 0x20, 0x01, 0x10, 0x09,
            0x41, 0x00, 0x4e, 0x0b] := by
  rfl

/-- `a == b` at carrier 2, helper index 10: the 11-byte body, no tail. -/
theorem intCmpBoolBytes_equality :
    AverCert.PlanBytes.lowerExprFragmentBodyBytes 2
        { profile := "expr-fragment-v1", params := [.intCarrier, .intCarrier],
          result := .boolI32, body := intCmpBlock .eq 10 } =
      some [0x01, 0x01, 0x63, 0x02, 0x20, 0x00, 0x20, 0x01, 0x10, 0x0a,
            0x0b] := by
  rfl

/-- `match a < b { true -> a; false -> b }` at carrier 2, helper index 9: the
    23-byte body, both arms bare argument reads. -/
theorem intSelectBytes_relational :
    AverCert.PlanBytes.lowerExprFragmentBodyBytes 2
        { profile := "expr-fragment-v1", params := [.intCarrier, .intCarrier],
          result := .intCarrier, body := intSelectBlock .lt 9 } =
      some [0x01, 0x01, 0x63, 0x02, 0x20, 0x00, 0x20, 0x01, 0x10, 0x09,
            0x41, 0x00, 0x48, 0x04, 0x63, 0x02, 0x20, 0x00, 0x05, 0x20, 0x01,
            0x0b, 0x0b] := by
  rfl

/-- The same shape at carrier 200 and helper index 300, where both the block
    type and the call immediate need two bytes. -/
theorem intSelectBytes_wideIndices :
    AverCert.PlanBytes.lowerExprFragmentBodyBytes 200
        { profile := "expr-fragment-v1", params := [.intCarrier, .intCarrier],
          result := .intCarrier, body := intSelectBlock .gt 300 } =
      some [0x01, 0x01, 0x63, 0xc8, 0x01, 0x20, 0x00, 0x20, 0x01, 0x10, 0xac,
            0x02, 0x41, 0x00, 0x4a, 0x04, 0x63, 0xc8, 0x01, 0x20, 0x00, 0x05,
            0x20, 0x01, 0x0b, 0x0b] := by
  rfl

theorem intCmp_simulates_model
    (carrier helperIdx : Nat) (op : IntCmpOp)
    (S : CarrierSpec carrier)
    (cmp eq : List WVal → Option WVal)
    (hCmp : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
      -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      cmp [carrierSmall carrier k1, carrierSmall carrier k2] = some r →
        r = .i32v (cmpW k1 k2))
    (hEq : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
      -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      eq [carrierSmall carrier k1, carrierSmall carrier k2] = some r →
        r = .i32v (eqW k1 k2))
    (code : CodeTbl) (self nlocals : Nat)
    (hCode : code self = some ⟨2, nlocals, intCmpTemplate op helperIdx⟩)
    (fuel : Nat) (p : Int × Int) (vs : List WVal) (w : WVal)
    (hDom : intPairSmallBandDomRepr carrier S p vs)
    (hRun : wFuncN code (intCmpHostSlots op helperIdx cmp eq) fuel self vs = some w) :
    boolRepr S (intCmpModel op p) w := by
  obtain ⟨rfl, hlo1, hhi1, hlo2, hhi2⟩ := hDom
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      cases op with
      | lt =>
          cases hc : cmp [carrierSmall carrier p.1, carrierSmall carrier p.2] with
          | none =>
              simp [wFuncN, hCode, intCmpTemplate, intCmpHostSlots, intCmpHelper,
                initLocals, wRunF, popArgs, hc] at hRun
          | some r =>
              have hr := hCmp p.1 p.2 r hlo1 hhi1 hlo2 hhi2 hc
              subst hr
              simp [wFuncN, hCode, intCmpTemplate, intCmpHostSlots, intCmpHelper,
                initLocals, wRunF, popArgs, hc] at hRun
              simp [boolRepr, intCmpModel, ← hRun, b32, cmpW_lt_iff]
      | gt =>
          cases hc : cmp [carrierSmall carrier p.1, carrierSmall carrier p.2] with
          | none =>
              simp [wFuncN, hCode, intCmpTemplate, intCmpHostSlots, intCmpHelper,
                initLocals, wRunF, popArgs, hc] at hRun
          | some r =>
              have hr := hCmp p.1 p.2 r hlo1 hhi1 hlo2 hhi2 hc
              subst hr
              simp [wFuncN, hCode, intCmpTemplate, intCmpHostSlots, intCmpHelper,
                initLocals, wRunF, popArgs, hc] at hRun
              simp [boolRepr, intCmpModel, ← hRun, b32, cmpW_gt_iff]
      | ge =>
          cases hc : cmp [carrierSmall carrier p.1, carrierSmall carrier p.2] with
          | none =>
              simp [wFuncN, hCode, intCmpTemplate, intCmpHostSlots, intCmpHelper,
                initLocals, wRunF, popArgs, hc] at hRun
          | some r =>
              have hr := hCmp p.1 p.2 r hlo1 hhi1 hlo2 hhi2 hc
              subst hr
              simp [wFuncN, hCode, intCmpTemplate, intCmpHostSlots, intCmpHelper,
                initLocals, wRunF, popArgs, hc] at hRun
              simp [boolRepr, intCmpModel, ← hRun, b32, cmpW_ge_iff]
      | eq =>
          cases hc : eq [carrierSmall carrier p.1, carrierSmall carrier p.2] with
          | none =>
              simp [wFuncN, hCode, intCmpTemplate, intCmpHostSlots, intCmpHelper,
                initLocals, wRunF, popArgs, hc] at hRun
          | some r =>
              have hr := hEq p.1 p.2 r hlo1 hhi1 hlo2 hhi2 hc
              subst hr
              simp [wFuncN, hCode, intCmpTemplate, intCmpHostSlots, intCmpHelper,
                initLocals, wRunF, popArgs, hc] at hRun
              simp [boolRepr, intCmpModel, ← hRun, b32, eqW]

theorem intSelect_simulates_model
    (carrier helperIdx : Nat) (op : IntCmpOp)
    (S : CarrierSpec carrier)
    (cmp eq : List WVal → Option WVal)
    (hCmp : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
      -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      cmp [carrierSmall carrier k1, carrierSmall carrier k2] = some r →
        r = .i32v (cmpW k1 k2))
    (hEq : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
      -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      eq [carrierSmall carrier k1, carrierSmall carrier k2] = some r →
        r = .i32v (eqW k1 k2))
    (code : CodeTbl) (self nlocals : Nat)
    (hCode : code self = some ⟨2, nlocals, intSelectTemplate op helperIdx⟩)
    (fuel : Nat) (p : Int × Int) (vs : List WVal) (w : WVal)
    (hDom : intPairSmallBandDomRepr carrier S p vs)
    (hRun : wFuncN code (intCmpHostSlots op helperIdx cmp eq) fuel self vs = some w) :
    intRepr S (intSelectModel op p) w := by
  obtain ⟨rfl, hlo1, hhi1, hlo2, hhi2⟩ := hDom
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      cases op with
      | lt =>
          cases hc : cmp [carrierSmall carrier p.1, carrierSmall carrier p.2] with
          | none =>
              simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                intCmpHelper, initLocals, wRunF, popArgs, hc] at hRun
          | some r =>
              have hr := hCmp p.1 p.2 r hlo1 hhi1 hlo2 hhi2 hc
              subst hr
              by_cases hrel : p.1 < p.2
              · simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                  intCmpHelper, initLocals, wRunF, popArgs, hc, b32, cmpW_lt_iff,
                  hrel] at hRun
                simpa [intRepr, intSelectModel, intCmpModel, hrel, ← hRun] using S.smallIntro p.1
              · simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                  intCmpHelper, initLocals, wRunF, popArgs, hc, b32, cmpW_lt_iff,
                  hrel] at hRun
                simpa [intRepr, intSelectModel, intCmpModel, hrel, ← hRun] using S.smallIntro p.2
      | gt =>
          cases hc : cmp [carrierSmall carrier p.1, carrierSmall carrier p.2] with
          | none =>
              simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                intCmpHelper, initLocals, wRunF, popArgs, hc] at hRun
          | some r =>
              have hr := hCmp p.1 p.2 r hlo1 hhi1 hlo2 hhi2 hc
              subst hr
              by_cases hrel : p.2 < p.1
              · simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                  intCmpHelper, initLocals, wRunF, popArgs, hc, b32, cmpW_gt_iff,
                  hrel] at hRun
                simpa [intRepr, intSelectModel, intCmpModel, hrel, ← hRun] using S.smallIntro p.1
              · simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                  intCmpHelper, initLocals, wRunF, popArgs, hc, b32, cmpW_gt_iff,
                  hrel] at hRun
                simpa [intRepr, intSelectModel, intCmpModel, hrel, ← hRun] using S.smallIntro p.2
      | ge =>
          cases hc : cmp [carrierSmall carrier p.1, carrierSmall carrier p.2] with
          | none =>
              simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                intCmpHelper, initLocals, wRunF, popArgs, hc] at hRun
          | some r =>
              have hr := hCmp p.1 p.2 r hlo1 hhi1 hlo2 hhi2 hc
              subst hr
              by_cases hrel : p.2 ≤ p.1
              · simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                  intCmpHelper, initLocals, wRunF, popArgs, hc, b32, cmpW_ge_iff,
                  hrel] at hRun
                simpa [intRepr, intSelectModel, intCmpModel, hrel, ← hRun] using S.smallIntro p.1
              · simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                  intCmpHelper, initLocals, wRunF, popArgs, hc, b32, cmpW_ge_iff,
                  hrel] at hRun
                simpa [intRepr, intSelectModel, intCmpModel, hrel, ← hRun] using S.smallIntro p.2
      | eq =>
          cases hc : eq [carrierSmall carrier p.1, carrierSmall carrier p.2] with
          | none =>
              simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                intCmpHelper, initLocals, wRunF, popArgs, hc] at hRun
          | some r =>
              have hr := hEq p.1 p.2 r hlo1 hhi1 hlo2 hhi2 hc
              subst hr
              by_cases hrel : p.1 = p.2
              · -- `hrel` is an equation between the two operands, so it also
                -- rewrites the host-call ARGUMENTS; `hc` has to be moved to the
                -- same normal form or it stops matching the run.
                rw [hrel] at hc
                simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                  intCmpHelper, initLocals, wRunF, popArgs, hc, eqW,
                  hrel] at hRun
                simpa [intRepr, intSelectModel, intCmpModel, hrel, ← hRun] using S.smallIntro p.1
              · simp [wFuncN, hCode, intSelectTemplate, intCmpTemplate, intCmpHostSlots,
                  intCmpHelper, initLocals, wRunF, popArgs, hc, eqW,
                  hrel] at hRun
                simpa [intRepr, intSelectModel, intCmpModel, hrel, ← hRun] using S.smallIntro p.2

/-! ### The Int comparison transports (`HEq` pins onto the obligation fields)

Same discipline as `recordParam_transport`: the obligation's field values are
supplied as ordinary universally quantified variables so `subst` applies once
each pin is turned into an `Eq`, and no cast residue survives. -/

theorem intCmp_transport
    (claimCarrier helperIdx : Nat) (op : IntCmpOp)
    (carrier : Nat) (Dom Cod : Type)
    (domRepr : CarrierSpec carrier → Dom → List WVal → Prop)
    (codRepr : CarrierSpec carrier → Cod → WVal → Prop)
    (model : Dom → Cod)
    (hcar : carrier = claimCarrier)
    (hDomT : HEq Dom (Int × Int))
    (hCodT : HEq Cod Bool)
    (hdomRepr : HEq domRepr (intPairSmallBandDomRepr claimCarrier))
    (hcodRepr : HEq codRepr (boolRepr (C := claimCarrier)))
    (hmodel : HEq model (intCmpModel op))
    (S : CarrierSpec carrier)
    (cmp eq : List WVal → Option WVal)
    (hCmp : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
      -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      cmp [carrierSmall carrier k1, carrierSmall carrier k2] = some r →
        r = .i32v (cmpW k1 k2))
    (hEq : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
      -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      eq [carrierSmall carrier k1, carrierSmall carrier k2] = some r →
        r = .i32v (eqW k1 k2))
    (code : CodeTbl) (self nlocals : Nat)
    (hCode : code self = some ⟨2, nlocals, intCmpTemplate op helperIdx⟩)
    (fuel : Nat) (x : Dom) (vs : List WVal) (w : WVal)
    (hdom : domRepr S x vs)
    (hRun : wFuncN code (intCmpHostSlots op helperIdx cmp eq) fuel self vs = some w) :
    codRepr S (model x) w := by
  subst hcar
  have hD : Dom = (Int × Int) := eq_of_heq hDomT
  subst hD
  have hC : Cod = Bool := eq_of_heq hCodT
  subst hC
  have e1 : domRepr = intPairSmallBandDomRepr carrier := eq_of_heq hdomRepr
  subst e1
  have e2 : codRepr = boolRepr := eq_of_heq hcodRepr
  subst e2
  have e3 : model = intCmpModel op := eq_of_heq hmodel
  subst e3
  exact intCmp_simulates_model carrier helperIdx op S cmp eq hCmp hEq code self
    nlocals hCode fuel x vs w hdom hRun

theorem intSelect_transport
    (claimCarrier helperIdx : Nat) (op : IntCmpOp)
    (carrier : Nat) (Dom Cod : Type)
    (domRepr : CarrierSpec carrier → Dom → List WVal → Prop)
    (codRepr : CarrierSpec carrier → Cod → WVal → Prop)
    (model : Dom → Cod)
    (hcar : carrier = claimCarrier)
    (hDomT : HEq Dom (Int × Int))
    (hCodT : HEq Cod Int)
    (hdomRepr : HEq domRepr (intPairSmallBandDomRepr claimCarrier))
    (hcodRepr : HEq codRepr (intRepr (C := claimCarrier)))
    (hmodel : HEq model (intSelectModel op))
    (S : CarrierSpec carrier)
    (cmp eq : List WVal → Option WVal)
    (hCmp : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
      -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      cmp [carrierSmall carrier k1, carrierSmall carrier k2] = some r →
        r = .i32v (cmpW k1 k2))
    (hEq : ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 →
      -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      eq [carrierSmall carrier k1, carrierSmall carrier k2] = some r →
        r = .i32v (eqW k1 k2))
    (code : CodeTbl) (self nlocals : Nat)
    (hCode : code self = some ⟨2, nlocals, intSelectTemplate op helperIdx⟩)
    (fuel : Nat) (x : Dom) (vs : List WVal) (w : WVal)
    (hdom : domRepr S x vs)
    (hRun : wFuncN code (intCmpHostSlots op helperIdx cmp eq) fuel self vs = some w) :
    codRepr S (model x) w := by
  subst hcar
  have hD : Dom = (Int × Int) := eq_of_heq hDomT
  subst hD
  have hC : Cod = Int := eq_of_heq hCodT
  subst hC
  have e1 : domRepr = intPairSmallBandDomRepr carrier := eq_of_heq hdomRepr
  subst e1
  have e2 : codRepr = intRepr := eq_of_heq hcodRepr
  subst e2
  have e3 : model = intSelectModel op := eq_of_heq hmodel
  subst e3
  exact intSelect_simulates_model carrier helperIdx op S cmp eq hCmp hEq code self
    nlocals hCode fuel x vs w hdom hRun

#print axioms symFragmentFace_none_of_recordProj
#print axioms recordParam_transport
#print axioms classifyIntCmpBool_forcesCarrierBinding
#print axioms classifyIntSelect_forcesCarrierBinding
#print axioms intCmp_simulates_model
#print axioms intSelect_simulates_model
#print axioms intCmp_transport
#print axioms intSelect_transport

end AverCert.StandardFace
