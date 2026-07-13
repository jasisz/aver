-- AverCert dependency-closed statement schema core (audited, fixed).
--
-- The single final certificate theorem is
-- This file contains every artifact-independent schema definition. The thin
-- `Schema.lean` shim adds only the artifact-hash equality from `Module.lean`.
import CertPrelude
import CertDecode

namespace AverCert.Schema
open CertPrelude

/-- The finite host-capability registry, minted from wasm-gc's exhaustive
    `EffectName.import_pair` mapping.  Artifact manifests may declare only
    pairs in this kernel-owned list; the Wasm import section is independently
    enumerated and must match the declaration exactly. -/
def CAPABILITY_REGISTRY : List (String × String) := [
  ("aver", "console_print"),
  ("aver", "console_error"),
  ("aver", "console_warn"),
  ("aver", "time_unix_ms"),
  ("aver", "request_method"),
  ("aver", "request_url"),
  ("aver", "request_query"),
  ("aver", "request_body"),
  ("aver", "request_headers_load"),
  ("aver", "response_text"),
  ("aver", "response_set_header"),
  ("aver", "http_send"),
  ("aver", "http_add_request_header"),
  ("aver", "http_clear_request_headers"),
  ("aver", "env_get"),
  ("aver", "env_set"),
  ("aver", "console_read_line"),
  ("aver", "args_len"),
  ("aver", "args_get"),
  ("aver", "random_float"),
  ("aver", "random_int"),
  ("aver", "time_sleep"),
  ("aver", "time_now"),
  ("aver", "float_sin"),
  ("aver", "float_cos"),
  ("aver", "float_atan2"),
  ("aver", "float_pow"),
  ("aver", "terminal_enable_raw_mode"),
  ("aver", "terminal_disable_raw_mode"),
  ("aver", "terminal_clear"),
  ("aver", "terminal_move_to"),
  ("aver", "terminal_print"),
  ("aver", "terminal_set_color"),
  ("aver", "terminal_reset_color"),
  ("aver", "terminal_read_key"),
  ("aver", "terminal_size"),
  ("aver", "terminal_hide_cursor"),
  ("aver", "terminal_show_cursor"),
  ("aver", "terminal_flush"),
  ("aver", "disk_read_text"),
  ("aver", "disk_write_text"),
  ("aver", "disk_append_text"),
  ("aver", "disk_exists"),
  ("aver", "disk_delete"),
  ("aver", "disk_delete_dir"),
  ("aver", "disk_list_dir"),
  ("aver", "disk_make_dir"),
  ("aver", "tcp_connect"),
  ("aver", "tcp_write_line"),
  ("aver", "tcp_read_line"),
  ("aver", "tcp_close"),
  ("aver", "tcp_send"),
  ("aver", "tcp_ping"),
  ("aver", "http_get"),
  ("aver", "http_head"),
  ("aver", "http_delete"),
  ("aver", "http_post"),
  ("aver", "http_put"),
  ("aver", "http_patch"),
  ("aver", "record_enter_group"),
  ("aver", "record_set_branch"),
  ("aver", "record_exit_group")
]

/-- What the artifact is: its pinned hash, emitted-fragment profile, runtime
    ABI, artifact theorem root, the certified and explicitly uncertified export
    names, the exact effect-import capability surface, byte-derived start
    status, and the runtime contracts every certificate is conditional on.
    Pure data, mirrored in `cert-manifest.json`. -/
structure Subject where
  artifactHash : String
  profile      : String
  abi          : String
  artifactRoot : String
  exports      : List String
  declaredUncertified : List (String × String)
  capabilities : List (String × String)
  start        : Option Nat
  hostRoleTable : CertDecode.AddSub.Roles
  stringHostRoles : List (Nat × CertDecode.StringHost.Role)
  contracts    : List String

/-- The certification policy attached to a certified export. Partial simulation
    remains the default; the total preset additionally promises return at the
    fuel selected by the checked termination witness. -/
inductive Policy where
  | simulatesModel
  | simulatesModelTotally

/-- Closed measure vocabulary for the first total-correctness family. The
    parameter index is claim data; `checkTerm` below accepts it only when the
    byte-bound recursion plan descends that integer parameter by one. -/
inductive Measure where
  | intNatAbs (paramIdx : Nat)
deriving Repr, DecidableEq

/-- Non-canonical termination evidence attached to an obligation rather than
    its byte-origin plan. Multiple measures may justify the same code bytes;
    the kernel checks the selected measure against the pinned descent. -/
structure TerminationWitness where
  measure : Measure
  descent : Int
deriving Repr, DecidableEq

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
  | app1 (name : String) (arg : SymTy)
  | app2 (name : String) (left right : SymTy)
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
    | emptyList (elemTy : SymTy)
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
  | mul
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

/-! ### Termination-witness checking

`recursion-plan-v1` is separately checked and lowered byte-exactly by artifact
acceptance. The helpers here inspect the same raw plan and confirm the one L3
measure currently admitted: `Int.natAbs` of the sole parameter, guarded at
`n ≤ 0`, with a recursive argument computed as `sub(n, box 1)`. -/

def checkTermSmallFloor (paramIdx : Nat) (block : FragBlock) : Bool :=
  match block.result, block.nodes with
  | 3,
      [{ id := 0, ty := .intCarrier, kind := .local localIdx },
       { id := 1, ty := .i64, kind := .structGet 0 0 },
       { id := 2, ty := .i64, kind := .constI64 0 },
       { id := 3, ty := .boolI32, kind := .prim .i64LeS [1, 2] }] =>
      localIdx == paramIdx
  | _, _ => false

def checkTermBigFloor (paramIdx : Nat) (block : FragBlock) : Bool :=
  match block.result, block.nodes with
  | 3,
      [{ id := 0, ty := .intCarrier, kind := .local localIdx },
       { id := 1, ty := .rawI32, kind := .structGet 2 0 },
       { id := 2, ty := .boolI32, kind := .constBool false },
       { id := 3, ty := .boolI32, kind := .prim .i32LtS [1, 2] }] =>
      localIdx == paramIdx
  | _, _ => false

/-- The step arm selected by the canonical small/big carrier discriminator and
    non-positive floor guard. Returning `none` rejects any different guard. -/
def checkTermStep? (paramIdx : Nat) (body : FragBlock) : Option FragBlock :=
  match body.result, body.nodes with
  | 4,
      [{ id := 0, ty := .intCarrier, kind := .local localIdx },
       { id := 1, ty := .ref, kind := .structGet 1 0 },
       { id := 2, ty := .boolI32, kind := .refIsNull 1 },
       { id := 3, ty := .boolI32, kind := .ifElse 2 small big },
       { id := 4, ty := .intCarrier, kind := .ifElse 3 _base step }] =>
      if localIdx == paramIdx && checkTermSmallFloor paramIdx small &&
          checkTermBigFloor paramIdx big then some step else none
  | _, _ => none

/-- Check that one selected self-call argument is exactly
    `sub(local paramIdx, box(1))`. -/
def checkTermDescentArg (paramIdx : Nat) (step : FragBlock)
    (descentId : Nat) : Bool :=
  match step.nodes[descentId]? with
        | some { kind := .hostCall .sub _ [inputId, boxedOneId], .. } =>
            match step.nodes[inputId]?, step.nodes[boxedOneId]? with
            | some { kind := .local localIdx, .. },
              some { kind := .hostCall .box _ [oneId], .. } =>
                match step.nodes[oneId]? with
                | some { kind := .constI64 1, .. } => localIdx == paramIdx
                | _ => false
            | _, _ => false
        | _ => false

/-- Does one node in the step arm call self with a checked first-parameter
    descent? Unary recursion uses a non-tail one-argument call; accumulator
    recursion uses a tail two-argument call whose second argument is pinned by
    the independently checked recursion grammar. -/
def checkTermDescent (paramIdx : Nat) (step : FragBlock) : Bool :=
  step.nodes.any fun node =>
    match node.kind with
    | .selfCall false _ [descentId] =>
        checkTermDescentArg paramIdx step descentId
    | .selfCall true _ [descentId, _accId] =>
        checkTermDescentArg paramIdx step descentId
    | _ => false

/-- Kernel decision procedure for promoted descent-by-one recursion.
    It does not synthesise a measure: it checks the claimed `natAbs` parameter,
    the `-1` descent, the non-positive floor guard, and the exact recursive
    argument chain already pinned to the module bytes by the plan gate. -/
def checkTerm (plan : RecursionRawPlan) (witness : TerminationWitness) : Bool :=
  match witness.measure with
  | .intNatAbs paramIdx =>
      plan.profile == "recursion-plan-v1" &&
      (plan.params == [.intCarrier] ||
       plan.params == [.intCarrier, .intCarrier]) &&
      plan.result == .intCarrier &&
      paramIdx == 0 &&
      witness.descent == (-1 : Int) &&
      match checkTermStep? paramIdx plan.body with
      | some step => checkTermDescent paramIdx step
      | none => false

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

/-- Kernel decision procedure for one member of a promoted integer-countdown
    mutual SCC. The floor/measure checks are identical to `checkTerm`; the only
    intentional shape difference is that the byte-pinned recursive edge is a
    tail call to another member rather than a non-tail self call. SCC closure
    and target membership remain separate artifact-acceptance guards. -/
def checkTermMutual (plan : MutualRawPlan) (witness : TerminationWitness) : Bool :=
  match witness.measure with
  | .intNatAbs paramIdx =>
      plan.profile == "mutual-plan-v1" &&
      plan.params == [.intCarrier] &&
      plan.result == .intCarrier &&
      paramIdx == 0 &&
      witness.descent == (-1 : Int) &&
      match checkTermStep? paramIdx plan.body with
      | some step =>
          step.nodes.any fun node =>
            match node.kind with
            | .selfCall true _ [descentId] =>
                match step.nodes[descentId]? with
                | some { kind := .hostCall .sub _ [inputId, boxedOneId], .. } =>
                    match step.nodes[inputId]?, step.nodes[boxedOneId]? with
                    | some { kind := .local localIdx, .. },
                      some { kind := .hostCall .box _ [oneId], .. } =>
                        match step.nodes[oneId]? with
                        | some { kind := .constI64 1, .. } => localIdx == paramIdx
                        | _ => false
                    | _, _ => false
                | _ => false
            | _ => false
      | none => false

/-- A composition member carries only its semantic-free byte SHAPE. A chain
    names callee exports; numeric Wasm indices are resolved from those exports'
    byte-derived `FuncBinding`s by the acceptance predicate and are never plan
    data. -/
inductive CompositionShape where
  | selfSum
  | chain (callees : List String)
deriving Repr, DecidableEq

/-- Raw, untrusted cross-function composition plan. This is solely a
    byte-origin veneer over the existing independently-read model and the
    existing callee-composition simulation proof. -/
structure CompositionRawPlan where
  profile : String
  shape   : CompositionShape
deriving Repr, DecidableEq

/-- Selected result-reference shape for a bare tuple/record field projection.
    This is claim context recovered from the module's function signature and
    checked against the selected struct field; it is never plan-selected. -/
inductive FieldProjectionResultTy where
  | eqref
  | nullableRef (typeIdx : Nat)
  deriving Repr, DecidableEq

/-- Exact byte-level value type of a constructor field. Unlike `SymTy`, this
    is read back from the Wasm type section and therefore cannot be changed by
    relabelling a source plan. -/
inductive ConstructValType where
  | i32
  | i64
  | f64
  | eqref
  | nullableRef (typeIdx : Nat)
  deriving Repr, DecidableEq

/-- Raw byte-origin veneer for the bare tuple-destructuring projection family.
    The projected field index is the only plan datum. Struct identity/count,
    selected result-reference type, carrier and function binding are supplied
    separately from validated module bytes and checked by artifact acceptance. -/
structure FieldProjectionRawPlan where
  profile  : String
  fieldIdx : Nat
deriving Repr, DecidableEq

/-- One terminal leaf of a verbatim `ref.test`-dispatch arm (`verbatim-plan-v1`).
    `Cod := WVal`; each leaf is a byte-derived constant or a single-variant
    projection. The concrete wasm type/data indices are node data bound to the
    module bytes by the byte-exact gate, never trusted from the plan. -/
inductive VerbatimLeaf where
  /-- Project field `field` of the scrutinee cast to user struct type `tyIdx`,
      spilled through the field scratch local:
      `localGet S; refCast tyIdx; structGet tyIdx field; localSet F; localGet F`. -/
  | project (tyIdx field : Nat)
  /-- A String literal built by `array.new_data arrTy dataIdx` over `bytes`:
      `i32Const 0; i32Const bytes.length; arrayNewData arrTy bytes`. -/
  | arrayNewData (arrTy dataIdx : Nat) (bytes : List Nat)
  /-- The null reference default (`ref.null resultHeapTy`). -/
  | refNull
  /-- A float-bits constant (`f64.const bits`). -/
  | f64Bits (bits : Nat)
deriving Repr

/-- A right-nested `ref.test` dispatch cascade over the (spilled) scrutinee. Each
    `test` reads the scrutinee local and branches on `ref.test tyIdx`; the final
    `leaf` is the fall-through default. -/
inductive VerbatimDispatch where
  | leaf (l : VerbatimLeaf)
  | test (tyIdx : Nat) (hit : VerbatimLeaf) (rest : VerbatimDispatch)
deriving Repr

/-- The exact result signature claimed by a verbatim plan. Artifact acceptance
    checks this variant against the function type recovered from module bytes;
    it is not evidence for its own result kind. -/
inductive VerbatimResultSig where
  | refNull (heapTy : Nat)
  | f64Scalar
deriving Repr, DecidableEq

/-- Raw, untrusted verbatim `ref.test`-dispatch plan (`verbatim-plan-v1`). A
    byte-origin veneer: the `Cod := WVal` / `verbatimRepr` proof face and the
    emitted `Module.lean` body literal are unchanged, so the plan claim never
    touches the proof. The multi-use scrutinee is spilled to a scratch local
    (which pure ANF `FragBlock` cannot express), so this is its own grammar. -/
structure VerbatimRawPlan where
  profile        : String
  scrutineeLocal : Nat
  fieldLocal     : Nat
  resultSig      : VerbatimResultSig
  body           : VerbatimDispatch
deriving Repr

/-- The host-helper role an Int-face dispatch arm combines its projected
    payload through (`int-dispatch-v1`). Deliberately narrower than `HostRole`:
    an arm combinator is `add` or `sub`, never `box` (boxing appears only at the
    fixed positions the lowering emits it), so the illegal state is
    unrepresentable rather than checked. -/
inductive IntDispatchRole where
  | add
  | sub
deriving Repr, DecidableEq

/-- One hit arm of an Int-face `ref.test` dispatch (`int-dispatch-v1`,
    `Cod := Int`). Every arm projects the tested variant's first (Int-carrier)
    field and spills it through its own scratch local; the leaf then either
    returns it or combines it with a boxed integer constant through a contracted
    host helper. The resolved wasm indices of the box/add/sub helpers are NOT
    plan data: the lowerers take the byte-derived host-role table as a
    parameter, so the plan can only name roles. -/
inductive IntDispatchLeaf where
  /-- Return the projected payload: `… localSet F; localGet F`. -/
  | proj
  /-- Combine the projected payload with the boxed constant `k` through the
      `role` helper. `constFirst` selects the operand order `k ⊕ x` (the spill
      local defers the payload past the constant) vs `x ⊕ k`. -/
  | hostOp (role : IntDispatchRole) (k : Int) (constFirst : Bool)
deriving Repr

/-- A right-nested Int-face `ref.test` dispatch cascade over the spilled
    scrutinee. Each `test` reads the scrutinee local and branches on
    `ref.test tyIdx`; the terminal `default` is a boxed integer constant
    (`i64.const k; call box`). The scrutinee/field scratch locals are NOT plan
    data: they are a fixed function of the arm count (arm `i` spills to local
    `i+1`, the scrutinee is local `armCount+1`), exactly what the lowerers
    compute. -/
inductive IntDispatchCascade where
  | default (k : Int)
  | test (tyIdx : Nat) (hit : IntDispatchLeaf) (rest : IntDispatchCascade)
deriving Repr

/-- Raw, untrusted Int-face `ref.test`-dispatch plan (`int-dispatch-v1`, the
    `Cod := Int` ADT-match families: the general variant dispatch and the
    widened Int match). A byte-origin veneer: the `cases`-spine proof face, the
    Int-valued model and the emitted `Module.lean` body literal are unchanged,
    so the plan claim never touches the proof. Like `verbatim-plan-v1` the
    multi-use scrutinee is spilled to a scratch local (which pure ANF
    `FragBlock` cannot express); unlike it the arms consume contracted host
    helpers, whose indices are context (the claim's byte-derived role table) —
    never plan data. -/
structure IntDispatchRawPlan where
  profile : String
  body    : IntDispatchCascade
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
  termination? : Option TerminationWitness := none
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

/-- Denotation of `simulatesModelTotally`. In addition to the five existing
    partial host laws it assumes that integer add, sub, and mul return on
    represented operands. For every represented obligation-domain input, the
    first argument is exposed as the checked `Int.natAbs` counter and the body
    must return at fuel `natAbs n + 1`. The tail permits audited recursion
    families to carry additional represented arguments without weakening the
    counter/fuel binding. -/
def Obligation.holdsTotal (o : Obligation) : Prop :=
  ∀ (S : CarrierSpec o.carrier)
    (add sub mul stringEq : List WVal → Option WVal)
    (stringConcat : Nat → List WVal → Option WVal)
    (_hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w → S.Repr (a + b) w)
    (_hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w → S.Repr (a - b) w)
    (_hmul : ∀ a b va vb w, S.Repr a va → S.Repr b vb → mul [va, vb] = some w → S.Repr (a * b) w)
    (_hStringEq : ∀ a b w, stringEq [a, b] = some w → w = b32 (stringEqW a b))
    (_hStringConcat : ∀ resultTy parts c, stringConcat resultTy [parts] = some c → stringConcatW resultTy parts = some c)
    (_hAddTot : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
    (_hSubTot : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
    (_hMulTot : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, mul [va, vb] = some w)
    (x : o.Dom) (vs : List WVal), o.domRepr S x vs →
    ∃ n v tail, vs = v :: tail ∧ S.Repr n v ∧
      ∃ w, wFuncN o.code (o.host add sub mul stringEq stringConcat)
          (n.natAbs + 1) o.self vs = some w ∧
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
  compositionPlans : List (String × CompositionRawPlan)
  verbatimPlans : List (String × VerbatimRawPlan)
  intDispatchPlans : List (String × IntDispatchRawPlan)
  fieldProjectionPlans : List (String × FieldProjectionRawPlan)
  obligations : List Obligation

/-- The artifact-independent part of the audited certificate proposition:
    each export satisfies the denotation selected by its policy. -/
def HoldsCore (m : Manifest) : Prop :=
  ∀ o ∈ m.obligations,
    match o.policy with
    | .simulatesModel => o.holds
    | .simulatesModelTotally => o.holdsTotal

end AverCert.Schema
