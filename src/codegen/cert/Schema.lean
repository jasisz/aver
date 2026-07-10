-- AverCert statement schema (audited, fixed).
--
-- The single final certificate theorem is
--   `AverCert.Final.cert : AverCert.Schema.Holds manifest`.
-- A consumer trusts the certificate by checking THREE things: the theorem
-- NAME, the manifest LITERAL, and the content hash of THIS file plus the
-- semantics prelude. It never inspects the Lean syntax of the proof. Because
-- `Holds` and its denotations live here and this file's hash is pinned in the
-- checker, the meaning of the certificate cannot be weakened without changing
-- a hash the checker rejects.
import CertPrelude
import Module

namespace AverCert.Schema
open CertPrelude

/-- What the artifact is: its pinned hash, the emitted-fragment profile, the
    runtime ABI, the artifact-level theorem root, the certified export names,
    and the named runtime contracts every certificate is conditional on. Pure
    data, mirrored in `cert-manifest.json`. -/
structure Subject where
  artifactHash : String
  profile      : String
  abi          : String
  artifactRoot : String
  exports      : List String
  contracts    : List String

/-- The certification policy attached to a certified export. v0 ships exactly
    one constructor: the emitted body simulates the generated model. -/
inductive Policy where
  | simulatesModel

/-- Value representation types admitted by the `expr-fragment-v1` plan grammar.
    This is the Lean-data mirror of the Rust `ExprFragmentPlan` sidecar: v1
    proofs still use the rendered `Obligation` face below, but this
    representation grammar is the stable landing zone for v2
    `CheckPlan`/`LowersCodeEntry`. Source-level projection is explicit through
    `FragTy.sourceTy?` below rather than a raw `WVal` fallback. -/
inductive FragTy where
  | f64
  | boolI32
  | intCarrier
  | i64
  | rawI32
  | ref
  /-- Opaque user-ADT / record reference. Unlike `ref` (an Int-carrier limb),
      this is a whole user struct/array reference handled verbatim. The concrete
      wasm type index is never part of the type: it lives on the projecting
      node (`structGetUser`) and is bound to the module bytes by the byte-exact
      gate, mirroring how `hostCall` carries its resolved function index. -/
  | adtRef
deriving Repr, DecidableEq

/-- Source-level types for the planned `SymPlan` grammar. This intentionally
    has no raw `WVal` escape hatch: if a fragment value cannot be named as an
    Aver source type, it should not project to `SymPlan` yet. -/
inductive SymTy where
  | int
  | float
  | bool
  | string
  | named (name : String)
deriving Repr, DecidableEq

/-- Projection from representation-level fragment types into the source-level
    `SymPlan` type system. Raw wasm limbs and references deliberately return
    `none`; they need an explicit source constructor/encoder before they can
    participate in source-level certificates. -/
def FragTy.sourceTy? : FragTy → Option SymTy
  | .f64 => some .float
  | .boolI32 => some .bool
  | .intCarrier => some .int
  | .i64 => none
  | .rawI32 => none
  | .ref => none
  -- An opaque ADT reference names no single source type by itself; the source
  -- meaning lives in the `SymPlan` node that produced it.
  | .adtRef => none

/-- Source-level primitive operations admitted by the initial `SymPlan`
    scaffold. `intAdd` is exact integer addition on Aver `Int` (ℤ); its
    encoding binds to the runtime carrier `add` contract through the
    byte-derived host-role table. -/
inductive SymPrim where
  | floatAdd
  | floatMul
  | floatLe
  | intAdd
  | stringEq
  | stringConcat
deriving Repr, DecidableEq

/-- Source-level integer comparison against a literal. This is intentionally
    narrower than general `Int` comparison so the v1 encoder can stay canonical
    and avoid SSA/local sharing. -/
inductive SymIntCmp where
  | eq
  | lt
  | le
  | ge
deriving Repr, DecidableEq

mutual
  inductive SymNodeKind where
    | param (index : Nat)
    | constBool (value : Bool)
    | constInt (value : Int)
    | constFloatBits (bits : Nat)
    | constStringBytes (bytes : List Nat)
    | prim (op : SymPrim) (args : List Nat)
    | construct (typeName ctorName : String) (args : List Nat)
    /-- Source-level record/ADT field projection: read declared field `field`
        (source declaration order) of a value of the named user type. `fieldTy`
        is the field's source type; encoding binds the projection to the exact
        wasm struct type index through the byte-derived struct table. -/
    | projectField (typeName : String) (field : Nat) (fieldTy : SymTy) (value : Nat)
    | intConstCmp (op : SymIntCmp) (value : Nat) (constant : Int)
    | ifElse (cond : Nat) (thenBlock elseBlock : SymBlock)
  deriving Repr

  structure SymNode where
    id   : Nat
    ty   : SymTy
    kind : SymNodeKind
  deriving Repr

  structure SymBlock where
    nodes  : List SymNode
    result : Nat
  deriving Repr
end

/-- Raw, untrusted source-level symbolic plan. Future profiles should prefer
    this over the wasm-representation-shaped `ExprFragmentRawPlan`; a checked
    encoder/lowerer then binds it to exact wasm code-entry bytes. -/
structure SymRawPlan where
  profile : String
  params  : List SymTy
  result  : SymTy
  body    : SymBlock
deriving Repr

/-- Primitive operations admitted by `expr-fragment-v1`. -/
inductive FragPrim where
  | f64Add
  | f64Mul
  | f64Le
  | i64Eq
  | i64LeS
  | i64LtS
  | i64GeS
  | i32LtS
  | i32GtS
deriving Repr, DecidableEq

/-- Runtime host helper roles admitted by `expr-fragment-v1`. Each role fixes a
    representation-level type signature (checked by `PlanCheck`); the resolved
    wasm function index is carried on the node and bound to the module bytes by
    the byte-exact gate, and to the byte-derived role table by the Rust checker. -/
inductive HostRole where
  | box
  | add
  | sub
deriving Repr, DecidableEq

mutual
  /-- A single typed ANF node in an expression-fragment plan. -/
  inductive FragNodeKind where
    | local (index : Nat)
    | constBool (value : Bool)
    | constI64 (value : Int)
    | constI32 (value : Int)
    | constF64Bits (bits : Nat)
    | structGet (field : Nat) (receiver : Nat)
    /-- Projection of `field` out of a user struct of wasm type `tyIdx` (a whole
        record/ADT, not the Int carrier). The type index is node data bound to
        the module bytes by the byte-exact gate and validated against the
        byte-derived struct context by the Rust checker, mirroring `hostCall`'s
        resolved function index. -/
    | structGetUser (tyIdx : Nat) (field : Nat) (value : Nat)
    | refIsNull (value : Nat)
    | prim (op : FragPrim) (args : List Nat)
    | hostCall (role : HostRole) (funcIdx : Nat) (args : List Nat)
    /-- A self-recursive call to the function being certified. `tail` selects
        `return_call` (tail position, `0x12`) over `call` (`0x10`). `funcIdx` is
        the resolved self function index; it is bound to the module bytes by the
        byte-exact gate and validated against the byte-derived self index by the
        Rust checker, exactly as `hostCall` binds its resolved index. The plan
        never invents it. -/
    | selfCall (tail : Bool) (funcIdx : Nat) (args : List Nat)
    | ifElse (cond : Nat) (thenBlock elseBlock : FragBlock)
  deriving Repr

  /-- A typed value definition. `id` must match its position in the containing
      block; v1 Rust checks this, v2 Lean `CheckPlan` will. -/
  structure FragNode where
    id   : Nat
    ty   : FragTy
    kind : FragNodeKind
  deriving Repr

  /-- Ordered ANF block. `result` is the id of the value yielded by the block. -/
  structure FragBlock where
    nodes  : List FragNode
    result : Nat
  deriving Repr
end

/-- Raw, untrusted expression-fragment plan as Lean data. The artifact may
    provide this; only the checked plan produced by the trusted checker should
    be used for acceptance. -/
structure ExprFragmentRawPlan where
  profile : String
  params  : List FragTy
  result  : FragTy
  body    : FragBlock
deriving Repr

/-- Raw, untrusted fuel-recursion plan. It reuses the `expr-fragment` ANF
    grammar, but its body carries `selfCall` nodes and its value-if yields the
    Int carrier. The checked lowerer binds it to the exact self-recursive
    function code-entry bytes. This is a byte-origin veneer only: the
    fuel-induction proof face and the emitted `Module.lean` body literal are
    unchanged, so the plan claim never touches the proof. -/
structure RecursionRawPlan where
  profile : String
  params  : List FragTy
  result  : FragTy
  body    : FragBlock
deriving Repr

/-- Raw, untrusted mutual-recursion member plan. Like `RecursionRawPlan` it
    reuses the `expr-fragment` ANF grammar with a `selfCall` node and an
    Int-carrier value-if, but the call is a TAIL call to a SIBLING member of the
    byte-derived SCC rather than the member's own index. The checked lowerer
    binds it to the exact code-entry bytes of ONE member of a mutually-recursive
    SCC. This is a byte-origin veneer only: the conjunction fuel-induction proof
    face and the emitted shared `Module.lean` code literal are unchanged, so the
    plan claim never touches the proof. -/
structure MutualRawPlan where
  profile : String
  params  : List FragTy
  result  : FragTy
  body    : FragBlock
deriving Repr

/-- One String.concat literal chunk. `bytes` is the source-level content; `dataIdx`
    is the target binding needed to lower back to exact `array.new_data` code
    bytes. A later self-checking parser can derive `dataIdx` from the module's
    passive data section instead of carrying it in the raw plan. -/
structure StringConcatChunk where
  dataIdx : Nat
  bytes   : List Nat
deriving Repr

/-- Raw, untrusted String.concat witness. It is source-shaped around the value
    flow (`prefixes ++ input ++ suffixes`) but still carries the current wasm-gc
    encoder binding for each literal chunk, so the checked plan can lower to the
    exact function code-entry bytes. -/
structure StringConcatRawPlan where
  profile  : String
  prefixes : List StringConcatChunk
  suffixes : List StringConcatChunk
deriving Repr

/-- One literal used by the String.eq dispatch beachhead. `bytes` is the
    source-level string content; `dataIdx` is the target binding needed for the
    exact `array.new_data` code bytes. -/
structure StringEqChunk where
  dataIdx : Nat
  bytes   : List Nat
deriving Repr

/-- Result branch of the String.eq dispatch: either return the original input
    string or return one byte-derived literal. -/
inductive StringEqResult where
  | input
  | literal (chunk : StringEqChunk)
deriving Repr

/-- Raw, untrusted String.eq witness for a one-literal match:
    `if String.eq(input, needle) then hit else default`. It is source-shaped but
    still carries data segment bindings for exact byte lowering. -/
structure StringEqRawPlan where
  profile : String
  needle  : StringEqChunk
  hit     : StringEqResult
  default : StringEqResult
deriving Repr

/-- Target-bound constructor field used by `construct-v1`: either replay one
    source/local argument, or emit the null representation slot that the wasm-gc
    layout requires but the source constructor does not expose. -/
inductive ConstructField where
  | local (index : Nat)
  | null
deriving Repr, DecidableEq

/-- Raw, untrusted ADT constructor witness. The source-level `SymPlan` says
    "construct this Aver value"; this plan carries the current wasm-gc binding
    needed to lower that constructor to exact `struct.new` bytes. -/
structure ConstructRawPlan where
  profile   : String
  arity     : Nat
  structIdx : Nat
  fields    : List ConstructField
deriving Repr

/-- Pointwise lifting of an integer representation relation to argument lists.
    Kept as the standard domain representation for the v2 integer classes. -/
inductive ReprAll (R : Int → WVal → Prop) : List Int → List WVal → Prop
  | nil : ReprAll R [] []
  | cons {n v ns vs} : R n v → ReprAll R ns vs → ReprAll R (n :: ns) (v :: vs)

/-- The representation-relation faces a simulation certificate is stated over
    (the Int carrier `{i64 small, ref limbs, i32 sign}`). Bundled in the audited
    schema so `Obligation.holds` is self-contained. -/
structure CarrierSpec (C : Nat) where
  Repr : Int → WVal → Prop
  car : ∀ n v, Repr n v →
    (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
    (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg])
  smallIntro : ∀ k : Int, Repr k (carrierSmall C k)
  smallElim : ∀ n s sg, Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n
  bigElim : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0

/-- Standard representation of a single integer result. -/
def intRepr (S : CarrierSpec C) : Int → WVal → Prop := S.Repr

/-- Standard representation of a boolean result. -/
def boolRepr (_S : CarrierSpec C) (b : Bool) (w : WVal) : Prop := w = b32 b

/-- Standard bit-exact representation of a floating-point result. -/
def floatRepr (_S : CarrierSpec C) (x : Float) (w : WVal) : Prop := w = .f64v x.toBits

/-- Standard representation of a floating-point bit-pattern result. -/
def floatBitsRepr (_S : CarrierSpec C) (bits : UInt64) (w : WVal) : Prop := w = .f64v bits

/-- Standard representation for byte-level projections: the model value is the
    exact `WVal` the body returns. This deliberately does not inspect strings. -/
def verbatimRepr (_S : CarrierSpec C) (v : WVal) (w : WVal) : Prop := w = v

/-- One certified export. `code`/`host`/`self` pin the emitted body and its
    runtime wiring; `Dom`/`Cod` and their representation relations describe the
    typed source-model face the body is proven to simulate. `aver cert verify`
    re-derives `code`, `self` and `carrier` from the module bytes, so the
    obligation is bound to the artifact. -/
structure Obligation where
  export_ : String
  policy  : Policy
  carrier : Nat
  code    : CodeTbl
  host    :
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (Nat → List WVal → Option WVal) →
    HostTbl
  self    : Nat
  Dom     : Type
  Cod     : Type
  domRepr : CarrierSpec carrier → Dom → List WVal → Prop
  codRepr : CarrierSpec carrier → Cod → WVal → Prop
  model   : Dom → Cod

/-- Denotation of `simulatesModel`: under any representation `S` and host
    contracts obeying the named laws (integer add/sub/mul, String.eq byte
    equality, and String.concat byte concatenation), the emitted body run on a
    represented domain value yields a represented result of `model x`. Partial
    correctness — vacuous on trap or fuel exhaustion. Each contract is an
    assumed runtime law: the host helper wired to that slot computes the named
    operation on represented values. -/
def Obligation.holds (o : Obligation) : Prop :=
  ∀ (S : CarrierSpec o.carrier)
    (add sub mul stringEq : List WVal → Option WVal)
    (stringConcat : Nat → List WVal → Option WVal)
    (_hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w → S.Repr (a + b) w)
    (_hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w → S.Repr (a - b) w)
    (_hmul : ∀ a b va vb w, S.Repr a va → S.Repr b vb → mul [va, vb] = some w → S.Repr (a * b) w)
    (_hStringEq : ∀ a b w, stringEq [a, b] = some w → w = b32 (stringEqW a b))
    (_hStringConcat : ∀ resultTy parts c, stringConcat resultTy [parts] = some c → stringConcatW resultTy parts = some c)
    (fuel : Nat) (x : o.Dom) (vs : List WVal) (w : WVal),
    o.domRepr S x vs →
    wFuncN o.code (o.host add sub mul stringEq stringConcat) fuel o.self vs = some w →
    o.codRepr S (o.model x) w

structure Manifest where
  subject     : Subject
  symFragmentPlans : List (String × SymRawPlan)
  stringEqPlans : List (String × StringEqRawPlan)
  stringConcatPlans : List (String × StringConcatRawPlan)
  constructPlans : List (String × ConstructRawPlan)
  exprFragmentPlans : List (String × ExprFragmentRawPlan)
  recursionPlans : List (String × RecursionRawPlan)
  mutualPlans : List (String × MutualRawPlan)
  obligations : List Obligation

/-- The single audited certificate proposition: the manifest's pinned hash is
    the module hash, and every certified export carries `simulatesModel` and
    genuinely simulates its model. -/
def Holds (m : Manifest) : Prop :=
  m.subject.artifactHash = CertModule.wasmSha256
  ∧ ∀ o ∈ m.obligations, o.policy = Policy.simulatesModel ∧ o.holds

end AverCert.Schema
