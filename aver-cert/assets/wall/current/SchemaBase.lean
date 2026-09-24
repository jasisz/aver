-- AverCert statement base (audited, fixed).
--
-- The artifact-independent vocabulary the statement schema is written in:
-- the capability registries, the admitted target identities, the subject,
-- the policy axes, the Int carrier specification and the named contracts of
-- the Int helpers. `Grammar` builds the plan grammar on top of this file, and
-- `SchemaCore` states the obligation over that grammar.
import CertPrelude
import CertDecode
import ArithTemplateDerisk

namespace AverCert.Schema
open CertPrelude
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
  ("aver", "tcp_write_now"),
  ("aver", "tcp_read_line"),
  ("aver", "tcp_read_bytes"),
  ("aver", "tcp_read_some"),
  ("aver", "tcp_read_now"),
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
  ("aver", "record_exit_group"),
  ("aver", "wait_poll"),
  ("aver", "work_cancel"),
  ("aver", "work_begin"),
  ("aver", "work_take")
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
  ("wasi:io/streams@0.2.4", "[method]input-stream.read"),
  ("wasi:io/streams@0.2.4", "[method]output-stream.check-write"),
  ("wasi:io/streams@0.2.4", "[method]output-stream.write"),
  ("wasi:io/streams@0.2.4", "[method]output-stream.flush"),
  ("wasi:io/streams@0.2.4", "[method]output-stream.subscribe"),
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
  decide

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
  hostRoleTable : Option _root_.CertDecode.AddSub.Roles
  arithParams : Option _root_.ArithTemplateDerisk.ArithHostParams
  stringHostRoles : List (Nat × _root_.CertDecode.StringHost.Role)
  contracts    : List String

/-- Claim-matching view of the optional module host-role table. An absent
    table binds no host roles, so any claim citing a box/add/mul/sub role
    fails to match — strictly fail-closed, never a default index. -/
def Subject.hostRoles (s : Subject) : _root_.CertDecode.AddSub.Roles :=
  match s.hostRoleTable with
  | some roles => roles
  | none => { box := none, add := none, mul := none, sub := none,
              toIndex := none, cmp := none, eq := none, divmod := none }

/-- The certification policy attached to a certified export. Partial simulation
    remains the default; the total preset additionally promises return at the
    fuel selected by the checked termination witness. -/
inductive Policy where
  | simulatesModel
  | simulatesModelTotally
deriving Repr, DecidableEq

/-- Extra totality premise selected for one total obligation.  The default
    preserves the shipped L3 contract: add/sub are total, while the partial mul
    law remains available but mul need not return.  The `.mul` role is derived
    when a member of the checked call group multiplies
    (`GrammarTotal.groupRole`). -/
inductive TotalityRole where
  | addSub
  | mul
deriving Repr, DecidableEq

/-- Closed measure vocabulary of the total-correctness policy. The wall
    derives the one canonical witness from the plans
    (`GrammarTotal.canonicalWitness`, when `checkTermGroup` accepts the call
    group); it is never a manifest choice. -/
inductive Measure where
  | intNatAbs (paramIdx : Nat)
deriving Repr, DecidableEq

/-- Termination evidence reported with an L3 obligation: the measure and the
    descent the wall's termination check established over the plan. -/
structure TerminationWitness where
  measure : Measure
  descent : Int
deriving Repr, DecidableEq

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

/-- A represented Int whose carrier word is in the runtime's normal form. -/
def CanonRepr {C : Nat} (S : CarrierSpec C) (n : Int) (w : WVal) : Prop :=
  S.Repr n w ∧ S.Canon w

/-- The named Int helper contracts at their function values, as the grammar's
    simulation theorem consumes them. `box` is the boxing helper's meaning (its
    body is byte-pinned to the wall's template, and the obligation wires the
    wall's own `boxRef`), stated for an i64-band literal because that is the
    only literal the emitter boxes. The arithmetic helpers conclude canonical
    results; the comparison helpers are exact on a canonical pair. -/
structure Contracts {C : Nat} (S : CarrierSpec C)
    (box add sub mul cmp eq : List WVal → Option WVal) : Prop where
  hBox : ∀ n w, -(2 ^ 63 : Int) ≤ n → n < 2 ^ 63 → box [.i64v n] = some w →
    CanonRepr S n w
  hAdd : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
    add [va, vb] = some w → CanonRepr S (a + b) w
  hSub : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
    sub [va, vb] = some w → CanonRepr S (a - b) w
  hMul : ∀ a b va vb w, S.Repr a va → S.Repr b vb →
    mul [va, vb] = some w → CanonRepr S (a * b) w
  hCmp : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
    cmp [va, vb] = some r → r = .i32v (cmpW a b)
  hEq : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
    eq [va, vb] = some r → r = .i32v (eqW a b)

end AverCert.Schema
