-- AverCert dependency-closed statement schema core (audited, fixed).
--
-- The single final certificate theorem is
-- This file contains every artifact-independent schema definition. The thin
-- `Schema.lean` shim adds only the artifact-hash equality from `Module.lean`.
import CertPrelude
import CertDecode
import ArithTemplateDerisk

namespace AverCert.Schema
open CertPrelude

/-- The finite wasm-gc host-capability registry, minted from its exhaustive
    `EffectName.import_pair` mapping.  Artifact manifests may declare only
    pairs in this kernel-owned list; the Wasm import section is independently
    enumerated and must match the declaration exactly. -/
def WASM_GC_CAPABILITY_REGISTRY : List (String × String) := [
  ("aver", "console_print"),
  ("aver", "console_error"),
  ("aver", "console_warn"),
  ("aver", "time_unix_ms"),
  ("aver", "process_stop_requested"),
  ("aver", "provider_contract_violation"),
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
  ("aver", "disk_read_bytes"),
  ("aver", "disk_read_bytes_at"),
  ("aver", "disk_write_bytes"),
  ("aver", "disk_append_bytes"),
  ("aver", "disk_size"),
  ("aver", "disk_exists"),
  ("aver", "disk_delete"),
  ("aver", "disk_delete_dir"),
  ("aver", "disk_list_dir"),
  ("aver", "disk_make_dir"),
  ("aver", "disk_sync"),
  ("aver", "tcp_connect"),
  ("aver", "tcp_begin_connect"),
  ("aver", "tcp_dialled"),
  ("aver", "tcp_listen"),
  ("aver", "tcp_accept"),
  ("aver", "tcp_peer_address"),
  ("aver", "tcp_write_line"),
  ("aver", "tcp_write_bytes"),
  ("aver", "tcp_read_line"),
  ("aver", "tcp_read_bytes"),
  ("aver", "tcp_read_some"),
  ("aver", "tcp_poll"),
  ("aver", "tcp_close"),
  ("aver", "tcp_close_dial"),
  ("aver", "tcp_close_listener"),
  ("aver", "tcp_send"),
  ("aver", "tcp_send_bytes"),
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

/-- Exact standard canonical-ABI import surface emitted into wasip2 core
    modules. Interface versions and operation names are part of the boundary;
    there is deliberately no wildcard for the `wasi:` namespace. -/
def WASIP2_CAPABILITY_REGISTRY : List (String × String) := [
  ("wasi:cli/stdout@0.2.4", "get-stdout"),
  ("wasi:cli/stderr@0.2.4", "get-stderr"),
  ("wasi:io/streams@0.2.4", "[method]output-stream.blocking-write-and-flush"),
  ("wasi:clocks/wall-clock@0.2.4", "now"),
  ("wasi:random/random@0.2.4", "get-random-u64"),
  ("wasi:cli/environment@0.2.4", "get-arguments"),
  ("wasi:cli/environment@0.2.4", "get-environment"),
  ("wasi:cli/stdin@0.2.4", "get-stdin"),
  ("wasi:io/streams@0.2.4", "[method]input-stream.blocking-read"),
  ("wasi:io/streams@0.2.4", "[method]input-stream.subscribe"),
  ("wasi:clocks/monotonic-clock@0.2.4", "subscribe-duration"),
  ("wasi:io/poll@0.2.4", "poll"),
  ("wasi:io/poll@0.2.4", "[resource-drop]pollable"),
  ("wasi:filesystem/preopens@0.2.4", "get-directories"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.stat-at"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.open-at"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.read-via-stream"),
  ("wasi:filesystem/types@0.2.4", "[resource-drop]descriptor"),
  ("wasi:io/streams@0.2.4", "[resource-drop]input-stream"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.write-via-stream"),
  ("wasi:io/streams@0.2.4", "[resource-drop]output-stream"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.unlink-file-at"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.remove-directory-at"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.create-directory-at"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.sync"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.append-via-stream"),
  ("wasi:filesystem/types@0.2.4", "[method]descriptor.read-directory"),
  ("wasi:filesystem/types@0.2.4", "[method]directory-entry-stream.read-directory-entry"),
  ("wasi:filesystem/types@0.2.4", "[resource-drop]directory-entry-stream"),
  ("wasi:http/types@0.2.4", "[constructor]fields"),
  ("wasi:http/types@0.2.4", "[constructor]outgoing-request"),
  ("wasi:http/types@0.2.4", "[method]outgoing-request.set-scheme"),
  ("wasi:http/types@0.2.4", "[method]outgoing-request.set-authority"),
  ("wasi:http/types@0.2.4", "[method]outgoing-request.set-path-with-query"),
  ("wasi:http/outgoing-handler@0.2.4", "handle"),
  ("wasi:http/types@0.2.4", "[method]future-incoming-response.subscribe"),
  ("wasi:http/types@0.2.4", "[method]future-incoming-response.get"),
  ("wasi:http/types@0.2.4", "[method]incoming-response.status"),
  ("wasi:http/types@0.2.4", "[method]incoming-response.consume"),
  ("wasi:http/types@0.2.4", "[method]incoming-body.stream"),
  ("wasi:http/types@0.2.4", "[static]incoming-body.finish"),
  ("wasi:http/types@0.2.4", "[resource-drop]outgoing-request"),
  ("wasi:http/types@0.2.4", "[resource-drop]future-incoming-response"),
  ("wasi:http/types@0.2.4", "[resource-drop]incoming-response"),
  ("wasi:http/types@0.2.4", "[resource-drop]future-trailers"),
  ("wasi:http/types@0.2.4", "[resource-drop]incoming-body"),
  ("wasi:http/types@0.2.4", "[method]incoming-response.headers"),
  ("wasi:http/types@0.2.4", "[method]fields.entries"),
  ("wasi:http/types@0.2.4", "[resource-drop]fields"),
  ("wasi:http/types@0.2.4", "[method]outgoing-request.set-method"),
  ("wasi:http/types@0.2.4", "[method]outgoing-request.body"),
  ("wasi:http/types@0.2.4", "[method]outgoing-body.write"),
  ("wasi:http/types@0.2.4", "[static]outgoing-body.finish"),
  ("wasi:http/types@0.2.4", "[method]fields.append"),
  ("wasi:http/types@0.2.4", "[resource-drop]outgoing-body"),
  ("wasi:http/types@0.2.4", "[method]incoming-request.method"),
  ("wasi:http/types@0.2.4", "[method]incoming-request.path-with-query"),
  ("wasi:http/types@0.2.4", "[method]incoming-request.headers"),
  ("wasi:http/types@0.2.4", "[method]incoming-request.consume"),
  ("wasi:http/types@0.2.4", "[resource-drop]incoming-request"),
  ("wasi:http/types@0.2.4", "[constructor]outgoing-response"),
  ("wasi:http/types@0.2.4", "[method]outgoing-response.set-status-code"),
  ("wasi:http/types@0.2.4", "[method]outgoing-response.body"),
  ("wasi:http/types@0.2.4", "[static]response-outparam.set"),
  ("wasi:sockets/instance-network@0.2.4", "instance-network"),
  ("wasi:sockets/ip-name-lookup@0.2.4", "resolve-addresses"),
  ("wasi:sockets/ip-name-lookup@0.2.4", "[method]resolve-address-stream.resolve-next-address"),
  ("wasi:sockets/ip-name-lookup@0.2.4", "[method]resolve-address-stream.subscribe"),
  ("wasi:sockets/ip-name-lookup@0.2.4", "[resource-drop]resolve-address-stream"),
  ("wasi:sockets/tcp-create-socket@0.2.4", "create-tcp-socket"),
  ("wasi:sockets/tcp@0.2.4", "[method]tcp-socket.start-connect"),
  ("wasi:sockets/tcp@0.2.4", "[method]tcp-socket.finish-connect"),
  ("wasi:sockets/tcp@0.2.4", "[method]tcp-socket.subscribe"),
  ("wasi:sockets/tcp@0.2.4", "[method]tcp-socket.shutdown"),
  ("wasi:sockets/tcp@0.2.4", "[resource-drop]tcp-socket")
]

/-- Backwards-compatible name for the original wasm-gc-only registry. -/
def CAPABILITY_REGISTRY : List (String × String) := WASM_GC_CAPABILITY_REGISTRY

/-- Core wasm-gc module artifacts use the raw module bytes as the certified artifact. -/
def expectedWasmGcArtifactTarget : String := "wasm-gc"

/-- WASI 0.2 Component Model artifacts use a declared component envelope. -/
def expectedWasip2ArtifactTarget : String := "wasip2"

/-- Select the finite standard host-import registry from the manifest target.
    Unknown targets receive no standard imports and therefore fail closed. -/
def capabilityRegistryForTarget (target : String) : List (String × String) :=
  if target == expectedWasmGcArtifactTarget then WASM_GC_CAPABILITY_REGISTRY
  else if target == expectedWasip2ArtifactTarget then WASIP2_CAPABILITY_REGISTRY
  else []

theorem wasiStdoutIsWasip2Only :
    (capabilityRegistryForTarget expectedWasip2ArtifactTarget).contains
      ("wasi:cli/stdout@0.2.4", "get-stdout") = true ∧
    (capabilityRegistryForTarget expectedWasmGcArtifactTarget).contains
      ("wasi:cli/stdout@0.2.4", "get-stdout") = false ∧
    (capabilityRegistryForTarget expectedWasip2ArtifactTarget).contains
      ("wasi:cli/stdout@0.2.5", "get-stdout") = false := by
  native_decide

/-- Backwards-compatible alias for the historical wasm-gc-only target constant. -/
def expectedArtifactTarget : String := expectedWasmGcArtifactTarget

/-- The only emitted-fragment profile this schema currently admits. -/
def expectedProfile : String := "AverUserProfile/v1"

/-- Runtime ABI admitted for raw wasm-gc module artifacts. -/
def expectedRuntimeAbiWasmGc : String := "aver-wasm-gc/0"

/-- Runtime ABI admitted for WASI 0.2 Component Model artifacts. -/
def expectedRuntimeAbiWasip2 : String := "aver-wasip2/0"

/-- Backwards-compatible alias for the historical wasm-gc-only ABI constant. -/
def expectedRuntimeAbi : String := expectedRuntimeAbiWasmGc

/-- Target/ABI pairs admitted by this schema.  The byte-level envelope check
    lives at artifact acceptance time, where both the delivered target bytes and
    the embedded core-module bytes are available. -/
def artifactTargetAbiAccepted (target abi : String) : Bool :=
  (target == expectedWasmGcArtifactTarget && abi == expectedRuntimeAbiWasmGc) ||
  (target == expectedWasip2ArtifactTarget && abi == expectedRuntimeAbiWasip2)

/-- Full statement identity helper used by tests and documentation. -/
def artifactIdentityAccepted (target profile abi : String) : Bool :=
  profile == expectedProfile && artifactTargetAbiAccepted target abi

/-- What the artifact is: its pinned hash, explicit artifact target,
    emitted-fragment profile, runtime ABI, artifact theorem root, the certified
    and explicitly uncertified export names, the exact effect-import capability
    surface, byte-derived start status, and the runtime contracts every
    certificate is conditional on. Pure data, mirrored in `cert-manifest.json`.

    `hostRoleTable` is optional exactly like `start`: a module without the Int
    carrier helper has no host-role table at all (`none`), which the acceptance
    pin binds against the strict byte decoder returning `some none` — a
    byte-derived proof that the `__rt_aint_from_i64` helper export is absent.
    A module with the helper always carries `some` table, even when every role
    inside it is unbound; a module whose role scan fails decodes to the
    poisoned `none`, which no manifest value can match.

    `arithParams` declares the indices the canonical arith helper bodies are a
    function of (Int carrier struct, limb array, and the decompose/normalize/
    strip/umagCmp bignum sub-routine functions); it is `some` exactly when
    `hostRoleTable` is. The acceptance pin synthesizes each declared add/sub/mul
    helper body from these and confirms it byte-for-byte in the real module, so
    a wrong declaration fails the pin rather than riding a byte fingerprint. -/
structure Subject where
  artifactHash : String
  target       : String
  profile      : String
  abi          : String
  artifactRoot : String
  exports      : List String
  declaredUncertified : List (String × String)
  capabilities : List (String × String)
  start        : Option Nat
  hostRoleTable : Option CertDecode.AddSub.Roles
  arithParams : Option ArithTemplateDerisk.ArithHostParams
  stringHostRoles : List (Nat × CertDecode.StringHost.Role)
  contracts    : List String

/-- Claim-matching view of the optional module host-role table. An absent
    table binds no host roles, so any claim citing a box/add/mul/sub role
    fails to match — strictly fail-closed, never a default index. -/
def Subject.hostRoles (s : Subject) : CertDecode.AddSub.Roles :=
  match s.hostRoleTable with
  | some roles => roles
  | none => { box := none, add := none, mul := none, sub := none,
              toIndex := none, cmp := none, eq := none }

/-- The certification policy attached to a certified export. Partial simulation
    remains the default; the total preset additionally promises return at the
    fuel selected by the checked termination witness. -/
inductive Policy where
  | simulatesModel
  | simulatesModelTotally
deriving Repr, DecidableEq

/-- Extra totality premise selected for one total obligation.  The default
    preserves the shipped L3 contract: add/sub are total, while the partial mul
    law remains available but mul need not return.  The `.mul` role is reserved
    for a byte-checked unary recursion whose combine call is `Int.mul`. -/
inductive TotalityRole where
  | addSub
  | mul
deriving Repr, DecidableEq

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
    `Plans.lean` stores these values as the sole plan DATA representation; the
    checker validates and lowers them to artifact bytes. Source-level projection
    is explicit through `FragTy.sourceTy?` rather than a raw `WVal` fallback. -/
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
  | floatGe
  | floatLt
  | floatGt
  | floatEq
  | intAdd
  | intSub
  | intMul
  | stringEq
  | stringConcat
  /-- Source-level `Bool.and` (eager conjunction on Aver `Bool`). Both
      operands must already be source Booleans; the encoder lowers it to the
      representation `i32.and` over two `boolI32` values, where bitwise and
      logical conjunction coincide. -/
  | boolAnd
deriving Repr, DecidableEq

/-- Source-level integer comparison against a literal. This is intentionally
    narrower than general `Int` comparison so the v1 encoder can stay canonical
    and avoid SSA/local sharing. -/
inductive SymIntCmp where
  | eq
  | lt
  | le
  | ge
  | gt
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
    /-- Source-level comparison of two Int VALUES (`a >= b`, `a == b`). Unlike
        `intConstCmp`, which compares one parameter against a LITERAL and
        encodes to a carrier-shape test, this encodes to the runtime helper call
        the emitter really produces: the three-way `__aint_cmp` followed by a
        signed relational operator against `i32.const 0`, or `__aint_eq` alone.
        `le` has no admitted encoding — the plan grammar carries no `i32.le_s`
        primitive — so it fail-closes. -/
    | intCmp (op : SymIntCmp) (lhs rhs : Nat)
    /-- Operational tag-field dispatch over an ADT value (Option/Result). Reads
        the i32 discriminant in field 0 of the `typeName` struct that `scrutinee`
        holds, compares it to the literal `tag`, and evaluates `hit` when equal
        else `miss`. This is a REPRESENTATION-level meaning ("read field 0 == k,
        branch"), NOT a source-constructor relation: it never claims the tested
        constructor writes `tag` into field 0. The encoder binds `typeName` to
        the wasm struct index via the byte-derived struct table; the byte-exact
        gate confirms field 0 is the i32 tag. -/
    | tagMatch (typeName : String) (scrutinee : Nat) (tag : Int) (hit miss : SymBlock)
    | ifElse (cond : Nat) (thenBlock elseBlock : SymBlock)
    /-- Monolithic fused `Option.withDefault(Vector.get(p0, p1), default)`:
        read the `typeName` vector in param 0 at the Int index in param 1,
        yielding the element in bounds and the literal `default` otherwise.
        The whole bounds-checked template is ONE node (mirroring the emitter's
        single fused shape); the vector and index are pinned to params 0 and 1,
        never chosen by the plan. -/
    | vectorGetOrDefault (typeName : String) (default : Int)
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
  | f64Ge
  | f64Lt
  | f64Gt
  | f64Eq
  | i64Eq
  | i64LeS
  | i64LtS
  | i64GeS
  | i64GtS
  | i32Eq
  | i32LtS
  | i32GtS
  /-- `i32.ge_s`: the tail the emitter appends to a `__aint_cmp` call for a
      source-level `>=`. The signed relational family is admitted one member at
      a time, as a plan that needs it appears; `i32.le_s` has an interpreter
      clause and a `WInstr` constructor already but no admitted plan, so it is
      deliberately still outside `FragPrim`. -/
  | i32GeS
  /-- `i32.and` restricted to the Boolean domain: `PlanCheck` types it only
      over two `boolI32` operands (NOT the loose `hasI32Ty`), because bitwise
      AND of arbitrary raw i32 values can produce a non-Boolean result
      (`2 and 2 = 2`) and the interpreter models the operation on {0,1}. -/
  | i32And
deriving Repr, DecidableEq

/-- Runtime host helper roles admitted by `expr-fragment-v1`. Each role fixes a
    representation-level type signature (checked by `PlanCheck`); the resolved
    wasm function index is carried on the node and bound both to the module
    bytes and to the decoded role table by artifact acceptance. -/
inductive HostRole where
  | box
  | add
  | mul
  | sub
  /-- `__aint_to_index`: extract a wasm array index from a represented integer
      (`[0, 2^31)` passes through; anything else, including every big value,
      collapses to the `-1` out-of-bounds sentinel). Consumed only by the
      monolithic fused vector-read node, never as a standalone `hostCall`. -/
  | toIndex
  /-- `__aint_cmp`: three-way comparison of two Int carriers, yielding the raw
      `i32` sentinel `-1`/`0`/`1` (`CertPrelude.cmpW`). The emitter never reads
      it as a Boolean: it always follows the call with `i32.const 0` and a
      signed relational operator, so the node's result type is `rawI32`, not
      `boolI32`. The ASSUMED CONTRACT covers a CANONICAL CARRIER PAIR only —
      see the note on `Obligation.holds`. -/
  | cmp
  /-- `__aint_eq`: equality of two Int carriers, yielding the `0`/`1` wasm
      Boolean directly (`CertPrelude.eqW`). Unlike `cmp` its result IS the
      source-level Boolean, so the node's result type is `boolI32` and the
      emitter appends no comparison tail. Its assumed contract is over a
      canonical carrier pair for the same reason `cmp`'s is, and for one more:
      `__aint_eq` decides a `Small`/`Big` pair structurally. -/
  | eq
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
        struct context decoded from the artifact, mirroring `hostCall`'s
        resolved function index. -/
    | structGetUser (tyIdx : Nat) (field : Nat) (value : Nat)
    | refIsNull (value : Nat)
    | prim (op : FragPrim) (args : List Nat)
    | hostCall (role : HostRole) (funcIdx : Nat) (args : List Nat)
    /-- A self-recursive call to the function being certified. `tail` selects
        `return_call` (tail position, `0x12`) over `call` (`0x10`). `funcIdx` is
        the resolved self function index; it is bound to the module bytes by the
        byte-exact gate and validated against the decoded self index, exactly as
        `hostCall` binds its resolved index. The plan never invents it. -/
    | selfCall (tail : Bool) (funcIdx : Nat) (args : List Nat)
    | ifElse (cond : Nat) (thenBlock elseBlock : FragBlock)
    /-- Monolithic fused bounds-checked vector read: the exact emitter template
        `to_index/ge_s // to_index/len/lt_u // and // if (array.get) (box d)`
        over locals 0 (vector) and 1 (index). `arrTy` is the vector's wasm
        array type index; `toIndexIdx`/`boxIdx` are the resolved
        `__aint_to_index` / box helper indices, bound to the module bytes by
        the byte-exact gate and to the byte-derived role table by acceptance.
        The node reads locals directly and consumes no operand stack values. -/
    | vectorGetOrDefault (arrTy toIndexIdx boxIdx : Nat) (default : Int)
    /-- Construction of a user struct of wasm type `tyIdx` from `args` (source
        field order). The type index is node data bound to the module bytes by
        the byte-exact gate, mirroring how `structGetUser` binds its projection
        index. -/
    | structNew (tyIdx : Nat) (args : List Nat)
    /-- The emitter's monolithic sign template for comparing a COMPUTED Int
        carrier against an i64 literal, without calling `__aint_cmp`
        (`from_mir/builtins.rs::emit_aint_cmp_const`). The operand is already on
        the stack; the template stashes it in the scratch local, branches on
        `limbs = null`, and decides either by the native i64 compare of the
        `small` field against `constant` or — for a limb-carrying operand,
        whose value is outside the i64 band — by the sign field alone.

        `scratch` is the local slot the template writes; the checker pins it to
        `params.length`, the one declared scratch every plan-first island
        reserves, so the template can never clobber a parameter. `constant` is
        pinned to the i64 band, which is what makes the sign arm exact. Like
        `vectorGetOrDefault` this is ONE node: the whole instruction list
        lowers and runs together. -/
    | intSignCmp (op : SymIntCmp) (constant : Int) (scratch : Nat) (value : Nat)
  deriving Repr

  /-- A typed value definition. `id` must match its position in the containing
      block; `PlanCheck` enforces this before lowering. -/
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
  /-- Return the constant `k` WITHOUT reading a field: the arm of a nullary
      (payloadless) constructor, lowered to `i64.const k; call box` with NO
      projection prefix. Because it never touches the payload it is sound for a
      constructor that has none. -/
  | const (k : Int)
deriving Repr

/-- A right-nested Int-face `ref.test` dispatch cascade over the spilled
    scrutinee. Each `test` reads the scrutinee local and branches on
    `ref.test tyIdx`; the terminal `default` is a boxed integer constant
    (`i64.const k; call box`). The scrutinee/field scratch locals are NOT plan
    data: they are a fixed function of the BINDING-arm count (only `proj`/`hostOp`
    arms read a payload and spill one; a `const` arm spills none). The `i`-th
    binding arm spills to local `i+1`, the scrutinee is local `bindArmCount+1`,
    exactly what the lowerers compute. -/
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

/-- Pointwise lifting of an integer representation relation to argument lists;
    this is the standard domain representation for integer families. -/
inductive ReprAll (R : Int → WVal → Prop) : List Int → List WVal → Prop
  | nil : ReprAll R [] []
  | cons {n v ns vs} : R n v → ReprAll R ns vs → ReprAll R (n :: ns) (v :: vs)

/-- The representation-relation faces a simulation certificate is stated over
    (the Int carrier `{i64 small, ref limbs, i32 sign}`). Bundled in the audited
    schema so `Obligation.holds` is self-contained.

    `Canon` is the runtime's NORMAL FORM on carrier words: a value is `Small`
    (`limbs = null`) exactly when it fits the i64 band `[-2^63, 2^63)`, and
    `Big` otherwise, with tight limbs and a non-zero sign. Every carrier the
    emitted runtime builds is in that form, by TWO mechanisms and not one:

    * the i64 fast paths build a `Small` DIRECTLY with `struct.new` — the box
      helper `wat/from_i64.wat` is nothing else, and so are the both-`Small`
      non-overflow arms of `wat/addsub.wat` and `wat/mul.wat`. Those words are
      normal because the value provably fits the band, not because anything
      normalised them;
    * every path that can produce a limb-carrying result ends in the
      normalisation epilogue (`wat/normalize.wat`, called as `__aint_normalize`
      or inlined), which strips leading limbs and demotes an in-band magnitude
      back to `Small`.

    `Canon` names that state abstractly and the two axioms below say only what
    the helpers need:

    * `canonSmall` — a literal small carrier is canonical EXACTLY on the i64
      band. The forward direction is what the boxing helper's output needs;
      the backward one says an out-of-band `Small` is not in normal form, which
      is what separates the two shapes;
    * `canonBig` — a canonical carrier that CARRIES LIMBS represents a value
      outside the i64 band and has a non-zero sign.

    Nothing else about `Canon` is assumed, and the two axioms are exactly what
    the proofs consume — no more. In particular they do NOT establish that the
    real `wat/eq.wat` and `wat/cmp.wat` are exact on a canonical pair: that is
    an assumption, carried as an explicit hypothesis of `Obligation.holds` and
    validated empirically against the running helpers by
    `tests/cert_intcmp_differential.rs`. `Obligation.holds` quantifies over
    every `CarrierSpec`, and a specification whose `Canon` marks words the
    runtime would never build is admitted by this schema; the instance a
    verdict is read at is the runtime's own, where `Canon` is the normal form
    described above. -/
structure CarrierSpec (C : Nat) where
  Repr : Int → WVal → Prop
  Canon : WVal → Prop
  car : ∀ n v, Repr n v →
    (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
    (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg])
  smallIntro : ∀ k : Int, Repr k (carrierSmall C k)
  smallElim : ∀ n s sg, Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n
  bigElim : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0
  canonSmall : ∀ k : Int,
      Canon (carrierSmall C k) ↔ (-(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63)
  canonBig : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) →
      Canon (.structv C [.i64v s, .arr lty les, .i32v sg]) →
      ¬(-(2 ^ 63 : Int) ≤ n ∧ n < 2 ^ 63) ∧ sg ≠ 0

/-- The small-band shape of a canonical comparison contract: literal small
    carriers inside the i64 band are represented (`smallIntro`) and canonical
    (`canonSmall`), so the relational contract specialises to the band form the
    exact-shape comparison faces were written against. -/
theorem canonicalCmp_smallBand {C : Nat} (S : CarrierSpec C)
    (cmp : List WVal → Option WVal)
    (h : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
      cmp [va, vb] = some r → r = .i32v (cmpW a b)) :
    ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 → -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      cmp [carrierSmall C k1, carrierSmall C k2] = some r → r = .i32v (cmpW k1 k2) :=
  fun k1 k2 r hlo1 hhi1 hlo2 hhi2 hc =>
    h k1 k2 _ _ r (S.smallIntro k1) (S.smallIntro k2)
      ((S.canonSmall k1).mpr ⟨hlo1, hhi1⟩) ((S.canonSmall k2).mpr ⟨hlo2, hhi2⟩) hc

/-- Forget the canonicity of an arithmetic contract's RESULT. The faces that
    predate the canonical-carrier contract consume only the representation
    conclusion; this is the one-line adapter their discharges apply. -/
theorem carrierContract_weaken {C : Nat} {S : CarrierSpec C}
    {op : List WVal → Option WVal} {f : Int → Int → Int}
    (h : ∀ a b va vb w, S.Repr a va → S.Repr b vb → op [va, vb] = some w →
      S.Repr (f a b) w ∧ S.Canon w) :
    ∀ a b va vb w, S.Repr a va → S.Repr b vb → op [va, vb] = some w →
      S.Repr (f a b) w :=
  fun a b va vb w h1 h2 h3 => (h a b va vb w h1 h2 h3).1

/-- `canonicalCmp_smallBand` for the equality helper. -/
theorem canonicalEq_smallBand {C : Nat} (S : CarrierSpec C)
    (eq : List WVal → Option WVal)
    (h : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
      eq [va, vb] = some r → r = .i32v (eqW a b)) :
    ∀ k1 k2 r, -(2 ^ 63 : Int) ≤ k1 → k1 < 2 ^ 63 → -(2 ^ 63 : Int) ≤ k2 → k2 < 2 ^ 63 →
      eq [carrierSmall C k1, carrierSmall C k2] = some r → r = .i32v (eqW k1 k2) :=
  fun k1 k2 r hlo1 hhi1 hlo2 hhi2 hc =>
    h k1 k2 _ _ r (S.smallIntro k1) (S.smallIntro k2)
      ((S.canonSmall k1).mpr ⟨hlo1, hhi1⟩) ((S.canonSmall k2).mpr ⟨hlo2, hhi2⟩) hc

/-- Standard representation of a single integer result. -/
def intRepr (S : CarrierSpec C) : Int → WVal → Prop := S.Repr

/-- Standard representation of a boolean result. -/
def boolRepr (_S : CarrierSpec C) (b : Bool) (w : WVal) : Prop := w = b32 b

/-- Standard representation of a floating-point bit-pattern result. -/
def floatBitsRepr (_S : CarrierSpec C) (bits : UInt64) (w : WVal) : Prop := w = .f64v bits

/-- Standard representation for byte-level projections: the model value is the
    exact `WVal` the body returns. This deliberately does not inspect strings. -/
def verbatimRepr (_S : CarrierSpec C) (v : WVal) (w : WVal) : Prop := w = v

/-! ### User type declarations as certified-Plan members (`typedecl-v1`)

A user record/variant declaration joins the certified Plan exactly as an
expression node does: the wall LOWERS the declaration to its wasm-gc type-section
entry (`lowerTypeDecl`) and artifact acceptance pins that entry by EQUALITY, so
the layout is a checked-by-equality witness, never trusted plan data. The
representation relation is ONE generic recursive definition (`ReprOf`), audited
once here; per-type certificates are only instances of it.

Stage 1 admits FLAT records of scalar fields (`Int`/`Bool`/`Float`) with a
field-READ model. Nested records, variants, `String`/`List` fields and computed
comparisons are OUT: the lowerer and the representation relation fail closed on
them (`lowerScalarStorage` returns `none`, the fuel floor rejects recursion, and
the `variant` representation arm is `False`). -/

/-- A user type declaration carried in the certified Plan. `intCarrier`,
    `boolScalar` and `floatScalar` are the admitted scalar leaves; `record`
    carries its wasm struct type index and its source-order fields; `variant`
    is present but unreachable in stage 1 (its representation arm is `False`).
    `DecidableEq` is written by hand: the derived handler does not fire through
    the nested `List TypeDecl` occurrences. -/
inductive TypeDecl where
  | intCarrier
  | boolScalar
  | floatScalar
  | record (idx : Nat) (fields : List TypeDecl)
  | variant (idx root : Nat) (ctors : List TypeDecl)
deriving Repr

mutual
  def TypeDecl.decEq : (x y : TypeDecl) → Decidable (x = y)
    | .intCarrier, .intCarrier => isTrue rfl
    | .boolScalar, .boolScalar => isTrue rfl
    | .floatScalar, .floatScalar => isTrue rfl
    | .record i1 f1, .record i2 f2 =>
        if h : i1 = i2 then
          match TypeDecl.decEqList f1 f2 with
          | isTrue hf => isTrue (by subst h; subst hf; rfl)
          | isFalse hf => isFalse (by intro he; injection he with h1 h2; exact hf h2)
        else isFalse (by intro he; injection he with h1 h2; exact h h1)
    | .variant i1 r1 c1, .variant i2 r2 c2 =>
        if h : i1 = i2 ∧ r1 = r2 then
          match TypeDecl.decEqList c1 c2 with
          | isTrue hc =>
              isTrue (by obtain ⟨ha, hb⟩ := h; subst ha; subst hb; subst hc; rfl)
          | isFalse hc => isFalse (by intro he; injection he with h1 h2 h3; exact hc h3)
        else isFalse (by intro he; injection he with h1 h2 h3; exact h ⟨h1, h2⟩)
    | .intCarrier, .boolScalar | .intCarrier, .floatScalar | .intCarrier, .record ..
    | .intCarrier, .variant .. | .boolScalar, .intCarrier | .boolScalar, .floatScalar
    | .boolScalar, .record .. | .boolScalar, .variant .. | .floatScalar, .intCarrier
    | .floatScalar, .boolScalar | .floatScalar, .record .. | .floatScalar, .variant ..
    | .record .., .intCarrier | .record .., .boolScalar | .record .., .floatScalar
    | .record .., .variant .. | .variant .., .intCarrier | .variant .., .boolScalar
    | .variant .., .floatScalar | .variant .., .record .. =>
        isFalse (by intro he; nomatch he)
  def TypeDecl.decEqList : (x y : List TypeDecl) → Decidable (x = y)
    | [], [] => isTrue rfl
    | [], _ :: _ => isFalse (by intro he; nomatch he)
    | _ :: _, [] => isFalse (by intro he; nomatch he)
    | x :: xs, y :: ys =>
        match TypeDecl.decEq x y, TypeDecl.decEqList xs ys with
        | isTrue hx, isTrue hxs => isTrue (by subst hx; subst hxs; rfl)
        | isFalse hx, _ => isFalse (by intro he; injection he with h1 h2; exact hx h1)
        | _, isFalse hxs => isFalse (by intro he; injection he with h1 h2; exact hxs h2)
end

instance : DecidableEq TypeDecl := TypeDecl.decEq

/-- The wasm-gc field storage a scalar leaf lowers to. `intCarrier` is a
    nullable reference to the module's Int carrier struct index `C`; `boolScalar`
    is `i32`; `floatScalar` is `f64`. Record fields are IMMUTABLE (`mutability
    0`), unlike the carrier's own mutable fields. Non-scalar leaves fail closed. -/
def lowerScalarStorage (C : Nat) : TypeDecl → Option CertDecode.FieldType
  | .intCarrier => some ⟨.val (.ref 0x63 (Int.ofNat C)), 0⟩
  | .boolScalar => some ⟨.val (.numeric 0x7f), 0⟩
  | .floatScalar => some ⟨.val (.numeric 0x7c), 0⟩
  | _ => none

/-- Lower a Plan type declaration to its expected wasm-gc type-section entry.
    Stage 1: a `record` becomes a `.plain` struct whose fields are the pointwise
    scalar-storage lowering of its source-order fields; a field that is not a
    scalar leaf makes the whole `mapM` fail closed. `fuel` is the recursion floor
    for future nested records; at `0`, and for every non-record declaration, the
    lowering returns `none` (fail-closed). -/
def lowerTypeDecl (C : Nat) : Nat → TypeDecl → Option CertDecode.TypeEntry
  | 0, _ => none
  | _fuel + 1, .record _idx fields =>
      (fields.mapM (lowerScalarStorage C)).map (fun fts => ⟨.plain, .structType fts⟩)
  | _fuel + 1, _ => none

/-! The value denotation of a Plan type declaration: scalar leaves denote their
    source scalar, a record denotes the right-associated product of its fields'
    denotations (the `FragParams.denote` shape), and the stage-1-unreachable
    variant denotes `Unit`. -/
mutual
  def RecordVal : TypeDecl → Type
    | .intCarrier => Int
    | .boolScalar => Bool
    | .floatScalar => UInt64
    | .record _ fields => RecordFields fields
    | .variant _ _ _ => Unit
  def RecordFields : List TypeDecl → Type
    | [] => Unit
    | [f] => RecordVal f
    | f :: next :: rest => RecordVal f × RecordFields (next :: rest)
end

/-! The single generic representation relation between a Plan type declaration,
    the wasm-gc value that stores it, and its denotation. Scalar leaves bottom
    out at the EXISTING carrier / boolean / float-bits relations; a record is a
    struct at its declared index whose fields represent the denotation pointwise
    (`ReprFields`); the stage-1-unreachable variant arm is `False`. Audited once;
    every per-type certificate is an instance. -/
mutual
  def ReprOf (S : CarrierSpec C) :
      (decl : TypeDecl) → WVal → RecordVal decl → Prop
    | .intCarrier, w, v => intRepr S v w
    | .boolScalar, w, v => boolRepr S v w
    | .floatScalar, w, v => floatBitsRepr S v w
    | .record idx fields, w, v =>
        ∃ ws, w = .structv idx ws ∧ ReprFields S fields ws v
    | .variant _ _ _, _, _ => False
  def ReprFields (S : CarrierSpec C) :
      (fields : List TypeDecl) → List WVal → RecordFields fields → Prop
    | [], ws, _ => ws = []
    | [f], ws, v => ∃ w, ws = [w] ∧ ReprOf S f w v
    | f :: next :: rest, ws, v =>
        ∃ w wrest, ws = w :: wrest ∧ ReprOf S f w v.1 ∧
          ReprFields S (next :: rest) wrest v.2
end

/-- The `k`-th component of a heterogeneous record denotation. Defined by
    induction on the field list so the unread fields are skipped structurally. -/
def nthField : (fields : List TypeDecl) → RecordFields fields →
    (k : Nat) → (hk : k < fields.length) → RecordVal (fields[k]'hk)
  | [f], v, 0, _ => v
  | _f :: next :: rest, v, 0, _ => v.1
  | _f :: next :: rest, v, k+1, hk => nthField (next :: rest) v.2 k (by simpa using hk)
  | [], _, _, hk => absurd hk (by simp)
  | [_f], _, _k+1, hk => absurd hk (by simp)

/-- GENERIC record-read lemma (the non-flat core of the field-read bridge): the
    `k`-th stored value of a represented record is `some w`, and `w` represents
    the `k`-th field of the record denotation. Proved by INDUCTION over the field
    list — the fields before `k` are framed past, never case-split. -/
theorem readField_repr (S : CarrierSpec C) :
    ∀ (fields : List TypeDecl) (ws : List WVal) (v : RecordFields fields)
      (k : Nat) (hk : k < fields.length),
      ReprFields S fields ws v →
      ∃ w, ws[k]? = some w ∧ ReprOf S (fields[k]'hk) w (nthField fields v k hk)
  | [_f], ws, v, 0, _hk, hrepr => by
      obtain ⟨w, hws, hr⟩ := hrepr
      exact ⟨w, by simp [hws], hr⟩
  | _f :: next :: rest, ws, v, 0, _hk, hrepr => by
      obtain ⟨w, wrest, hws, hr, _⟩ := hrepr
      exact ⟨w, by simp [hws], hr⟩
  | _f :: next :: rest, ws, v, k+1, hk, hrepr => by
      obtain ⟨w, wrest, hws, _, hrest⟩ := hrepr
      have hk' : k < (next :: rest).length := by simpa using hk
      obtain ⟨w', hw', hr'⟩ := readField_repr S (next :: rest) wrest v.2 k hk' hrest
      exact ⟨w', by simp [hws, hw'], hr'⟩

/-- The emitted scalar-field-read body: `local.get 0; struct.get structIdx field`
    (`PlanLower` lowers a `structGetUser structIdx field 0` node to exactly this
    over param 0). -/
def recordProjTemplate (structIdx field : Nat) : List WInstr :=
  [.localGet 0, .structGet structIdx field]

/-- LOAD-BEARING generic bridge (template ⟹ model). Running the emitted
    scalar-field-read body on a value that represents a record yields a `w` that
    represents the `field`-th component of the record denotation, under the
    generic `ReprOf`. Generic over the carrier spec, the record's field list, the
    field index, the declared-local count, the host table (the body makes no host
    call), the code table (pinned only at the self entry, exactly what byte
    acceptance certifies), and the fuel. Partial correctness — vacuous on trap or
    fuel exhaustion, like `Obligation.holds`. The unread fields ride through via
    the field-list induction `readField_repr`, never a per-field case split. -/
theorem recordParam_simulates_model
    (S : CarrierSpec C) (structIdx field nlocals : Nat)
    (fields : List TypeDecl) (hfield : field < fields.length)
    (host : HostTbl) (code : CodeTbl) (self : Nat)
    (hCode : code self = some ⟨1, nlocals, recordProjTemplate structIdx field⟩)
    (fuel : Nat) (v : RecordFields fields) (ws : List WVal) (w : WVal)
    (hrepr : ReprFields S fields ws v)
    (hRun : wFuncN code host fuel self [.structv structIdx ws] = some w) :
    ReprOf S (fields[field]'hfield) w (nthField fields v field hfield) := by
  obtain ⟨w', hw', hr'⟩ := readField_repr S fields ws v field hfield hrepr
  cases fuel with
  | zero => simp [wFuncN] at hRun
  | succ fuel =>
      simp [wFuncN, hCode, recordProjTemplate, initLocals, wRunF, hw'] at hRun
      subst hRun
      exact hr'

/-- Whether a Plan type declaration is one of the three admitted scalar leaves.
    Every `TypeDecl` constructor is listed: a future constructor makes this
    match non-exhaustive and stops the wall building rather than silently
    classifying it. -/
def typeDeclIsScalarLeaf : TypeDecl → Bool
  | .intCarrier => true
  | .boolScalar => true
  | .floatScalar => true
  | .record _ _ => false
  | .variant _ _ _ => false

/-- Stage-1 record admission: a `record` head whose every field is a scalar
    leaf, with at least one field. Explicit arms over every constructor
    (fail-closed, no wildcard), so extending `TypeDecl` forces a decision
    here before any new shape can reach the record-parameter face. -/
def checkRecordDecl : TypeDecl → Bool
  | .record _ fields => !fields.isEmpty && fields.all typeDeclIsScalarLeaf
  | .intCarrier => false
  | .boolScalar => false
  | .floatScalar => false
  | .variant _ _ _ => false

/-! Whether a Plan type declaration mentions the Int carrier ANYWHERE. Explicit
    arms over every constructor; the list walk is structural, so there is no
    fuel to exhaust — a fuel-based variant would have to answer `true`
    (REQUIRE the byte-derived carrier binding) on exhaustion, never `false`.
    A declaration that mentions the carrier makes the record face's meaning
    read the claimed carrier index, so acceptance must pin that index to the
    decoded `CertDecode.carrierState`. -/
mutual
  def typeDeclMentionsIntCarrier : TypeDecl → Bool
    | .intCarrier => true
    | .boolScalar => false
    | .floatScalar => false
    | .record _ fields => typeDeclsMentionIntCarrier fields
    | .variant _ _ ctors => typeDeclsMentionIntCarrier ctors
  def typeDeclsMentionIntCarrier : List TypeDecl → Bool
    | [] => false
    | decl :: rest =>
        typeDeclMentionsIntCarrier decl || typeDeclsMentionIntCarrier rest
end

/-- The representation-level fragment type a scalar leaf reads back as:
    `intCarrier` fields flow as the boxed carrier reference, `boolScalar` as
    the Boolean i32, `floatScalar` as raw f64 bits. Non-leaves have no scalar
    fragment type (fail-closed). -/
def scalarLeafFragTy? : TypeDecl → Option FragTy
  | .intCarrier => some FragTy.intCarrier
  | .boolScalar => some FragTy.boolI32
  | .floatScalar => some FragTy.f64
  | .record _ _ => none
  | .variant _ _ _ => none

/-- The fragment result types a stage-1 record field read may declare — exactly
    the range of `scalarLeafFragTy?`. Every `FragTy` constructor is listed. -/
def fragTyIsRecordScalar : FragTy → Bool
  | .intCarrier => true
  | .boolI32 => true
  | .f64 => true
  | .i64 => false
  | .rawI32 => false
  | .ref => false
  | .adtRef => false

/-- The one canonical recursion budget for `lowerTypeDecl` wherever acceptance
    states the type-section equality pin. Stage 1 lowers only flat records, so
    any positive fuel suffices; naming one value keeps the pin's statement
    identical across the face, the fixtures, and the generated certificates. -/
abbrev lowerTypeDeclFuel : Nat := 8

/-! ### The pinned declaration is byte-determined (the existential is no choice)

The record face quantifies its `TypeDecl` existentially inside a proof term.
These inversion lemmas make the soundness argument formal: any declaration the
equality pin accepts lowers to a `.plain` struct (killing the `.sub`/`.subFinal`
doppelganger), is a record whose field list lowers pointwise to the decoded
storages, and can place an `.intCarrier` field exactly where the real entry
holds a concrete reference — so a reference storage in the pinned entry FORCES
the declaration to mention the Int carrier, and a scalar-leaf claim about a
field forces that field's exact storage. Guard-iso probes compose these with
`typeSectionMatches` monotonicity to refute the whole face on hostile bytes. -/

/-- Everything `lowerTypeDecl` produces is a `.plain` entry. -/
theorem lowerTypeDecl_plain (C fuel : Nat) (decl : TypeDecl)
    (e : CertDecode.TypeEntry) (h : lowerTypeDecl C fuel decl = some e) :
    e.form = .plain := by
  cases fuel with
  | zero => simp [lowerTypeDecl] at h
  | succ fuel =>
      cases decl <;> simp [lowerTypeDecl] at h
      case record idx fields =>
        obtain ⟨fts, -, hentry⟩ := h
        rw [← hentry]

/-- Everything `lowerTypeDecl` produces comes from a record declaration whose
    field list lowers pointwise to the entry's storages. -/
theorem lowerTypeDecl_recordFields (C fuel : Nat) (decl : TypeDecl)
    (e : CertDecode.TypeEntry) (h : lowerTypeDecl C fuel decl = some e) :
    ∃ idx fields fts, decl = TypeDecl.record idx fields ∧
      e = ⟨.plain, .structType fts⟩ ∧
      fields.mapM (lowerScalarStorage C) = some fts := by
  cases fuel with
  | zero => simp [lowerTypeDecl] at h
  | succ fuel =>
      cases decl <;> simp [lowerTypeDecl] at h
      case record idx fields =>
        obtain ⟨fts, hmap, hentry⟩ := h
        exact ⟨idx, fields, fts, rfl, hentry.symm, hmap⟩

/-- Pointwise inversion of a successful `mapM`: each produced element is the
    image of the element at the same position. -/
theorem mapM_getElem?_inv {α β : Type} (g : α → Option β) :
    ∀ (xs : List α) (ys : List β) (k : Nat) (y : β),
      xs.mapM g = some ys → ys[k]? = some y →
      ∃ x, xs[k]? = some x ∧ g x = some y
  | [], ys, k, y, hmap, hget => by
      have hys : ([] : List β) = ys := by simpa using hmap
      subst hys
      simp at hget
  | x :: xs, ys, k, y, hmap, hget => by
      rw [List.mapM_cons] at hmap
      cases hx : g x with
      | none => rw [hx] at hmap; simp at hmap
      | some b =>
          cases hxs : xs.mapM g with
          | none => rw [hx, hxs] at hmap; simp at hmap
          | some bs =>
              rw [hx, hxs] at hmap
              have hys : b :: bs = ys := by simpa using hmap
              subst hys
              cases k with
              | zero =>
                  simp only [List.getElem?_cons_zero] at hget
                  injection hget with hy
                  subst hy
                  exact ⟨x, by simp, hx⟩
              | succ k =>
                  simp only [List.getElem?_cons_succ] at hget
                  obtain ⟨x', hx', hgx'⟩ := mapM_getElem?_inv g xs bs k y hxs hget
                  exact ⟨x', by simpa using hx', hgx'⟩

/-- Only the `.intCarrier` leaf lowers to a concrete reference storage. -/
theorem lowerScalarStorage_ref_intCarrier (C : Nat) (f : TypeDecl)
    (r : Int) (m : Nat)
    (h : lowerScalarStorage C f = some ⟨.val (.ref 0x63 r), m⟩) :
    f = TypeDecl.intCarrier := by
  cases f <;> simp [lowerScalarStorage] at h ⊢

/-- Only the `.boolScalar` leaf lowers to the `i32` storage. -/
theorem lowerScalarStorage_i32_boolScalar (C : Nat) (f : TypeDecl) (m : Nat)
    (h : lowerScalarStorage C f = some ⟨.val (.numeric 0x7f), m⟩) :
    f = TypeDecl.boolScalar := by
  cases f <;> simp [lowerScalarStorage] at h ⊢

/-- A field list holding `.intCarrier` at any position mentions the carrier. -/
theorem typeDeclsMention_of_getElem? :
    ∀ (fields : List TypeDecl) (k : Nat),
      fields[k]? = some TypeDecl.intCarrier →
      typeDeclsMentionIntCarrier fields = true
  | [], k, h => by simp at h
  | f :: rest, 0, h => by
      simp at h
      subst h
      simp [typeDeclsMentionIntCarrier, typeDeclMentionsIntCarrier]
  | f :: rest, k + 1, h => by
      simp only [List.getElem?_cons_succ] at h
      have := typeDeclsMention_of_getElem? rest k h
      simp [typeDeclsMentionIntCarrier, this]

/-- Only `.boolScalar` names the Boolean fragment scalar. -/
theorem scalarLeafFragTy?_boolI32 (f : TypeDecl)
    (h : scalarLeafFragTy? f = some FragTy.boolI32) :
    f = TypeDecl.boolScalar := by
  cases f <;> simp [scalarLeafFragTy?] at h ⊢

/-- One certified export. `code`/`host`/`self` pin the emitted body and its
    runtime wiring; `Dom`/`Cod` and their representation relations describe the
    typed source-model face the body is proven to simulate. `AcceptedArtifact`
    decodes and binds the relevant code, function, type, and carrier facts from
    the artifact bytes. -/
structure Obligation where
  export_ : String
  policy  : Policy
  termination? : Option TerminationWitness := none
  totalityRole : TotalityRole := .addSub
  carrier : Nat
  code    : CodeTbl
  host    :
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (Nat → List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    (List WVal → Option WVal) →
    HostTbl
  self    : Nat
  Dom     : Type
  Cod     : Type
  domRepr : CarrierSpec carrier → Dom → List WVal → Prop
  codRepr : CarrierSpec carrier → Cod → WVal → Prop
  model   : Dom → Cod

/-- Denotation of `simulatesModel`: under any representation `S` and host
    contracts obeying the named laws (integer add/sub/mul, integer three-way
    comparison and equality, String.eq byte equality, and String.concat byte
    concatenation), the emitted body run on a represented domain value yields a
    represented result of `model x`. Partial correctness — vacuous on trap or
    fuel exhaustion. Each contract is an assumed runtime law: the host helper
    wired to that slot computes the named operation on represented values.

    The three arithmetic premises also conclude that the RESULT is canonical.
    That is a statement about the helper's output alone: every arm of
    `wat/addsub.wat` and `wat/mul.wat` either builds an in-band `Small`
    directly or ends in the normalisation epilogue (`wat/normalize.wat`), so
    whatever they return is in the runtime's normal form. Nothing is assumed
    about non-canonical inputs.

    The two comparison premises are EXACT on the result, like `_hToIndex`,
    because the helpers leave the carrier — they return a raw `i32` that no
    representation relation describes. They are quantified over a CANONICAL
    CARRIER PAIR: two represented operands that are both in the runtime's
    normal form. That scoping is load-bearing rather than stylistic. Both
    helpers decide STRUCTURALLY — `wat/eq.wat` compares shape and fields,
    `wat/cmp.wat` branches on the raw sign fields — so on an arbitrary pair
    they are not exact at all: a `Small` and a limb-carrying `Big` word can
    represent the same integer and still compare unequal. Canonicity is
    exactly the fact the proofs use to rule that pair out: `canonBig` puts a
    canonical limb-carrying word outside the i64 band, and the BACKWARD
    direction of `canonSmall` puts every canonical `Small` inside it, so the
    two shapes cannot denote the same integer.

    What the two axioms do NOT do is make the real helpers exact. Exactness is
    an assumption about `wat/cmp.wat` and `wat/eq.wat` at the runtime's own
    carrier specification, stated here as a hypothesis and checked empirically
    by `tests/cert_intcmp_differential.rs`. This denotation quantifies over
    every `CarrierSpec`; an instance that marks non-normal-form words canonical
    satisfies the schema, and simply is not the instance a verdict is read at.

    A NON-canonical operand is OUTSIDE THE CERTIFIED DOMAIN — the same
    epistemic position as `toIndexW`'s `-1` region, stated rather than assumed
    away: the obligation says nothing about it. It is also a state the emitted
    runtime never builds: the i64 fast paths construct an in-band `Small`
    directly (`wat/from_i64.wat`, and the both-`Small` arms of `wat/addsub.wat`
    and `wat/mul.wat`), and every arm that can produce limbs ends in the
    normalisation epilogue (`wat/normalize.wat`). Neither premise demands trap-freedom; a helper that
    returns `none` makes the premise vacuous and the run yields nothing,
    exactly as everywhere else in this denotation. -/
def Obligation.holds (o : Obligation) : Prop :=
  ∀ (S : CarrierSpec o.carrier)
    (add sub mul stringEq : List WVal → Option WVal)
    (stringConcat : Nat → List WVal → Option WVal)
    (toIndex cmp eq : List WVal → Option WVal)
    (_hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w →
      S.Repr (a + b) w ∧ S.Canon w)
    (_hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w →
      S.Repr (a - b) w ∧ S.Canon w)
    (_hmul : ∀ a b va vb w, S.Repr a va → S.Repr b vb → mul [va, vb] = some w →
      S.Repr (a * b) w ∧ S.Canon w)
    (_hStringEq : ∀ a b w, stringEq [a, b] = some w → w = b32 (stringEqW a b))
    (_hStringConcat : ∀ resultTy parts c, stringConcat resultTy [parts] = some c → stringConcatW resultTy parts = some c)
    (_hToIndex : ∀ n v r, S.Repr n v → toIndex [v] = some r → r = .i32v (toIndexW n))
    (_hCmp : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
      cmp [va, vb] = some r → r = .i32v (cmpW a b))
    (_hEq : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
      eq [va, vb] = some r → r = .i32v (eqW a b))
    (fuel : Nat) (x : o.Dom) (vs : List WVal) (w : WVal),
    o.domRepr S x vs →
    wFuncN o.code (o.host add sub mul stringEq stringConcat toIndex cmp eq) fuel o.self vs = some w →
    o.codRepr S (o.model x) w

/-- Denotation of `simulatesModelTotally`, with its totality assumptions selected
    by the obligation's byte-checked role.  The ordinary `.addSub` branch has
    exactly the pre-schema-60 premise surface: only integer add and sub must
    return on represented operands.  The `.mul` branch additionally assumes
    multiplication totality and is admitted only for a byte-pinned unary
    recursion whose combine role is `.mul`.  In either branch the first domain
    argument is the checked `Int.natAbs` counter and the body must return at fuel
    `natAbs n + 1`; the tail carries any additional represented arguments. -/
def Obligation.holdsTotal (o : Obligation) : Prop :=
  match o.totalityRole with
  | .addSub =>
      ∀ (S : CarrierSpec o.carrier)
        (add sub mul stringEq : List WVal → Option WVal)
        (stringConcat : Nat → List WVal → Option WVal)
        (toIndex cmp eq : List WVal → Option WVal)
        (_hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w →
          S.Repr (a + b) w ∧ S.Canon w)
        (_hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w →
          S.Repr (a - b) w ∧ S.Canon w)
        (_hmul : ∀ a b va vb w, S.Repr a va → S.Repr b vb → mul [va, vb] = some w →
          S.Repr (a * b) w ∧ S.Canon w)
        (_hStringEq : ∀ a b w, stringEq [a, b] = some w → w = b32 (stringEqW a b))
        (_hStringConcat : ∀ resultTy parts c, stringConcat resultTy [parts] = some c → stringConcatW resultTy parts = some c)
        (_hToIndex : ∀ n v r, S.Repr n v → toIndex [v] = some r → r = .i32v (toIndexW n))
        (_hCmp : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
          cmp [va, vb] = some r → r = .i32v (cmpW a b))
        (_hEq : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
          eq [va, vb] = some r → r = .i32v (eqW a b))
        (_hAddTot : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
        (_hSubTot : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
        (x : o.Dom) (vs : List WVal), o.domRepr S x vs →
        ∃ n v tail, vs = v :: tail ∧ S.Repr n v ∧
          ∃ w, wFuncN o.code (o.host add sub mul stringEq stringConcat toIndex cmp eq)
              (n.natAbs + 1) o.self vs = some w ∧
            o.codRepr S (o.model x) w
  | .mul =>
      ∀ (S : CarrierSpec o.carrier)
        (add sub mul stringEq : List WVal → Option WVal)
        (stringConcat : Nat → List WVal → Option WVal)
        (toIndex cmp eq : List WVal → Option WVal)
        (_hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w →
          S.Repr (a + b) w ∧ S.Canon w)
        (_hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w →
          S.Repr (a - b) w ∧ S.Canon w)
        (_hmul : ∀ a b va vb w, S.Repr a va → S.Repr b vb → mul [va, vb] = some w →
          S.Repr (a * b) w ∧ S.Canon w)
        (_hStringEq : ∀ a b w, stringEq [a, b] = some w → w = b32 (stringEqW a b))
        (_hStringConcat : ∀ resultTy parts c, stringConcat resultTy [parts] = some c → stringConcatW resultTy parts = some c)
        (_hToIndex : ∀ n v r, S.Repr n v → toIndex [v] = some r → r = .i32v (toIndexW n))
        (_hCmp : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
          cmp [va, vb] = some r → r = .i32v (cmpW a b))
        (_hEq : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
          eq [va, vb] = some r → r = .i32v (eqW a b))
        (_hAddTot : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, add [va, vb] = some w)
        (_hSubTot : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, sub [va, vb] = some w)
        (_hMulTot : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, mul [va, vb] = some w)
        (x : o.Dom) (vs : List WVal), o.domRepr S x vs →
        ∃ n v tail, vs = v :: tail ∧ S.Repr n v ∧
          ∃ w, wFuncN o.code (o.host add sub mul stringEq stringConcat toIndex cmp eq)
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
