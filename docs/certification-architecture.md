# Certification Architecture: Plan-First Canonical Lowering

Status: accepted direction, plan-first `expr-fragment-v1` path active; the
Lean witness now also checks the relevant export-to-code-entry byte slice for
expression fragments.

This document records the target architecture for Aver artifact certificates.
It supersedes trace-guided lifting as the certification core for new
certificate work. Trace/replay sidecars are not an acceptance path.

## Core Claim

The certification pipeline should stop asking:

```text
Can the verifier discover a certifiable plan from this Wasm body?
```

and instead ask:

```text
Does this verifier-checked plan lower canonically to exactly this Wasm body?
```

The design slogan is:

```text
certified export =
  byte-exact canonical encoding of a verifier-checked untrusted CertPlan
  plus a verifier-generated Lean challenge over that CheckedPlan.
```

A compiler-emitted plan is a witness, not authority. The actual Wasm bytes
remain the source of truth.

## Trust Model

Trusted:

- strict Wasm decoder and target function/code-entry slicer;
- module context builder for type, function, import, export and local spaces;
- `RawPlan -> CheckedPlan` type/refinement/effect checker;
- canonical lowerer and binary encoder for accepted plan profiles;
- host ABI and runtime contract registry;
- canonical plan/binding hashing;
- verifier-generated Lean challenge;
- audited Lean prelude/schema and the Lean kernel/checker path;
- axiom whitelist enforcement.

Not trusted:

- Aver compiler;
- emitted manifest JSON;
- emitted plan sidecars;
- any legacy trace/replay metadata;
- generated Lean proof text before kernel checking;
- source names/debug names except where byte-derived from exports;
- legacy recognizers.

This does not eliminate TCB. It reduces its shape: instead of a general-ish
Wasm lifter/replay checker, the trusted verifier checks a small typed plan and
regenerates one canonical byte sequence.

## SymPlan Direction

The long-term plan grammar should move toward a source-level semantic plan
(`SymPlan`) rather than a wasm-shaped plan. In that design, proof obligations
talk about Aver concepts:

```text
Int.add(x, y) : Int
Float.mul(a, b) : Float
Bool.and(p, q) : Bool
String.concat(a, b) : String
```

The target-specific facts live below that as representation and encoding
rules:

```text
Int    -> Aver Int carrier representation -> canonical wasm-gc bytes
Float  -> f64 bits                         -> canonical wasm-gc bytes
Bool   -> canonical i32 0/1                -> canonical wasm-gc bytes
String -> Aver string representation       -> canonical wasm-gc bytes
```

This is still a plan-first architecture. The artifact supplies untrusted plan
data, the checker validates it, and canonical lowering binds the checked plan
to exact code-entry bytes. The useful shift is that the proof surface can scale
with Aver's admitted source fragment while the encoder stays as the thin
target-specific layer. The current `expr-fragment-v1` definitions keep the
wasm representation type as `FragTy` and now record its source semantic face
separately. Rust also has a first `SymPlan` model that can project direct
source-level float/bool fragments and intentionally rejects representation-only
carrier-limb fragments. The audited Lean `Schema.lean` mirrors this direction
with `SymTy`, `SymPrim` and `SymRawPlan` data definitions. For fragments that
already project cleanly, `AcceptedArtifact.lean` accepts a source-level
`SymFragmentClaim`: Lean checks/encodes `SymRawPlan -> ExprFragmentRawPlan`,
then the existing byte-origin predicate binds that encoded representation plan
to the exact function bytes. Representation-only fragments remain on the
`ExprFragmentClaim` fallback until the source grammar grows explicit
constructors for them. The emitted manifest now uses `sym-fragment-v1.plan` as
the preferred sidecar for source-projectable fragments; `expr-fragment-v1.plan`
remains the fallback and the checked encoder target. Representation types are
therefore already moving into checked encoding details rather than the artifact
surface.

## Artifact Shape

The compiler emits:

```text
foo.wasm
cert/
  cert-manifest.json
  fragments/<export>.sym-fragment-v1.plan   # preferred when source-projectable
  fragments/<export>.expr-fragment-v1.plan
  PlanCheck.lean
  PlanLower.lean
  PlanBytes.lean
  WasmSlice.lean
  ExprFragmentAccepted.lean
  AcceptedArtifact.lean
  ArtifactBytes.lean
  Plans.lean
  Certificate.lean
  Manifest.lean
  ...
```

Trace files are not emitted for this profile. Debug traces, if reintroduced as
developer tooling, must remain outside certificate acceptance.

`PlanCheck.lean`, `PlanLower.lean`, `PlanBytes.lean`, `WasmSlice.lean` and
`ExprFragmentAccepted.lean` are audited checker code copied from the verifier
binary and hash-pinned in `cert-manifest.json`; cert-supplied files by those
names are ignored by `aver cert verify`. `ArtifactBytes.lean` is also
checker-owned: it is regenerated from the actual wasm bytes read by the
verifier, not trusted from the certificate directory.
`Plans.lean` carries the same expression-fragment plans as Lean data and pins
their Lean-side lowering and byte-origin slice:

```lean
def floatAddGoalPlan : ExprFragmentRawPlan := ...
example : PlanCheck.checkExprFragmentRawPlan floatAddGoalPlan = true := rfl
example :
  (CertModule.floatAddGoalCode selfIdx).map (fun c => c.body) =
    PlanLower.lowerExprFragmentBody carrier floatAddGoalPlan := rfl
example :
  PlanBytes.lowerExprFragmentCodeEntry carrier floatAddGoalPlan =
    some [/* exact code-entry bytes */] := rfl
example :
  WasmSlice.codeEntryForExport ArtifactBytes.wasmBytes [/* export name */] =
    some [/* exact code-entry bytes */] := rfl
example :
  WasmSlice.funcBindingForExport ArtifactBytes.wasmBytes [/* export name */] =
    some { funcIdx := selfIdx, codeIdx := codeIdx, typeIdx := typeIdx,
           codeEntry := [/* exact code-entry bytes */] } := rfl
example :
  ExprFragmentAccepted.accepted ArtifactBytes.wasmBytes [/* export name */]
    carrier floatAddGoalPlan floatAddGoalBody [/* exact code-entry bytes */]
    { funcIdx := selfIdx, codeIdx := codeIdx, typeIdx := typeIdx,
      codeEntry := [/* exact code-entry bytes */] }
example :
  AcceptedArtifact.exprFragmentPlanAccepted
    ArtifactBytes.wasmBytes [/* export name */] "floatAddGoal"
    carrier floatAddGoalPlan floatAddGoalOb
example :
  AcceptedArtifact.symFragmentPlanAccepted
    ArtifactBytes.wasmBytes [/* export name */] "floatAddGoal"
    carrier floatAddGoalSymPlan floatAddGoalOb
example :
  AcceptedArtifact.acceptedFragments
    { wasmBytes := ArtifactBytes.wasmBytes,
      symFragmentClaims :=
        [ { exportNameBytes := [/* export name */],
            exportName := "floatAddGoal",
            carrier := carrier,
            plan := floatAddGoalSymPlan,
            obligation := floatAddGoalOb } ],
      exprFragmentClaims :=
        [] }
```

This is not yet the v2 raw-byte in-kernel checker. In v1, Rust still
parses/checks the text sidecar, lowers it canonically and compares it to the
Wasm code-entry bytes. The verified plan is then rendered back as
checker-owned Lean `SymRawPlan` / `ExprFragmentRawPlan` data and pinned by
`CheckerWitness.lean` against `manifest.symFragmentPlans` and
`manifest.exprFragmentPlans`; the witness also checks that all source plans
pass `PlanCheck.checkSymRawPlan`, that they encode to their representation
plans, that all representation plans pass `PlanCheck.checkExprFragmentRawPlan`,
that `PlanLower.lowerExprFragmentBody` produces the byte-bound `WInstr` body,
and that `PlanBytes.lowerExprFragmentCodeEntry` produces the verifier-derived
canonical code-entry bytes. It also checks that `WasmSlice.funcBindingForExport`
routes the export through the byte-derived function index, defined-code index
and function-section type index before finding those same code-entry bytes in
the checker-owned artifact bytes, then proves the same facts through one
`ExprFragmentAccepted.accepted` predicate. For fragments with a source
projection, the artifact-level claim is now `SymFragmentClaim`: Lean checks
`encodeSymRawPlanToExprFragmentRawPlan symPlan = some exprPlan` before applying
the existing representation-plan acceptance predicate. For fragments without a
source projection, the fallback remains `ExprFragmentClaim`.
So `Plans.lean` is an untrusted data surface, but it cannot drift from the
sidecar/body pair without failing verifier-authored `rfl`.

The manifest is a routing and consistency witness. It may pin:

```text
wasm_sha256
profile_id
artifact_certificate_root
plan_hash
function_binding_hash
proof_hash
trusted_prelude_hash
trusted_schema_hash
trusted_plan_check_hash
trusted_plan_lower_hash
trusted_plan_bytes_hash
trusted_wasm_slice_hash
trusted_expr_fragment_accepted_hash
trusted_accepted_artifact_hash
artifact_data_hash
host_registry_hash
```

Every manifest fact that affects trust must be recomputed by the verifier from
Wasm bytes, checked plans, trusted registries or generated challenges. Manifest
agreement is necessary but never sufficient.

## Verifier Algorithm

For each certified export:

1. Decode the actual Wasm module and resolve the export to a defined function.
2. Build a byte-derived function binding:
   - module hash;
   - export name;
   - function index and code index;
   - actual function type;
   - actual local declarations;
   - raw code-entry bytes;
   - relevant type/import/host context.
3. Read the untrusted `RawPlan` sidecar.
4. Check the plan against the byte-derived function binding, selected profile,
   ABI rules and host registry, producing `CheckedPlan`.
5. Canonically lower `CheckedPlan` in the actual module context to the expected
   Wasm code-entry bytes.
6. Compare expected bytes with the actual bytes. Any difference rejects.
7. Canonically serialize and hash `CheckedPlan` and the function binding.
8. Compare computed hashes with manifest pins.
9. Generate the Lean challenge from `CheckedPlan`, the function binding and the
   selected specification face.
10. Check that the supplied proof proves exactly that verifier-generated
    challenge.
11. Run Lean/kernel checking and axiom whitelist enforcement.

The target equality gate is byte-exact canonical code-entry equality, not
semantic equivalence. Initially, an implementation may compare decoded `CodeTbl`
data while the byte encoder lands, but the architectural target is raw canonical
code-entry bytes so noncanonical LEB encodings, local declaration groupings,
dead instructions, alternate block encodings and redundant operations cannot
pass.

Current implementation status: `expr-fragment-v1` has moved past decoded
`CodeTbl` comparison and now checks the verifier-lowered raw code-entry bytes
against the bytes sliced from the actual Wasm code section. The checked bytes
include the body-size prefix, the local declaration vector and the final `end`.
The checker derives non-expression obligations with the legacy byte
classifiers, derives expression-fragment obligations from manifest-named plan
sidecars, checks each sidecar against byte-derived function facts, and only then
merges the two lists by the actual byte-derived function order before
generating `CheckerWitness.lean`. The old expr-fragment byte classifier is no
longer part of verifier-side admission or ordering. The remaining v1 residual
is that the Rust-side plan checker/lowerer and the non-expression byte
classifiers are still verifier TCB until the v2/v3 Lean `AverCert` path moves
checking/lowering into kernel-checked definitions.

Lean-side implementation status: `PlanCheck.lean` now structurally validates
the emitted `ExprFragmentRawPlan`, `PlanLower.lean` canonically lowers that
accepted raw plan to the measured `CertPrelude.WInstr` body used by
`Module.lean`, and `PlanBytes.lean` canonically lowers the same plan to the
exact code-entry byte sequence used by the current cert island. `WasmSlice.lean`
then parses the checker-regenerated `ArtifactBytes.wasmBytes` just far enough to
resolve an export name to the same `FuncBinding` (function index, defined-code
index, function-section type index and code-entry bytes). `ExprFragmentAccepted.lean`
packages those checks as one accepted-export predicate for the current
expr-fragment profile. `AcceptedArtifact.lean` exposes the v2-shaped bridge
from raw artifact bytes + source/raw plan + schema obligation to that predicate.
For source-projectable fragments this is a `SymFragmentClaim` that must encode
to the byte-bound representation plan in Lean; for representation-only
fragments it is still an `ExprFragmentClaim` fallback. The lowered body,
code-entry bytes and function binding are internal witnesses, not trusted
parameters. The emitted certificate now includes `Artifact.lean`, which defines
`AverCert.Artifact.data`, a parameterized `acceptedWithFinal` bridge, and the
self-checking `AverCert.Artifact.certificate : AcceptedArtifact.accepted data`
for the entire fragment claim set. The checker pins that `data` term to its own
reconstruction with `rfl`, type-ascribes `Final.cert : Schema.Holds manifest`,
and roots the axiom audit at `Artifact.certificate`. The audited
`AcceptedArtifact.accepted` predicate also requires the manifest subject to name
that same artifact root and requires fragment claim obligations to be present in
`manifest.obligations`.
This removes another slice of plan-to-semantics, plan-to-bytes, byte-origin and
schema-binding logic from unreviewed generated proof text while still preventing
the artifact from choosing the final theorem target. The remaining gap is full
module validation in Lean:
`WasmSlice.lean` is intentionally a relevant-subset slicer, while Rust still
hashes the artifact, performs the executable equality gate and derives the
complete obligation list for non-expression classes.

Producer-side status: the compiler now uses plan-first emission for the current
host-free scalar expression islands: Float/Bool expressions and Int
literal-comparison predicates. It derives an `ExprFragmentPlan` from MIR and
emits the function body through the same canonical plan lowerer used by the
verifier. More complex future Int-carrier fragments should move into this
`source/MIR -> CertPlan -> canonical Wasm` path instead of adding new
post-emission recognizers.

## Plan Shape

Plans are ANF/SSA-like, not labels for whole-function shapes.

Example:

```text
profile expr-fragment-v1
params f64 f64
result f64

v0 = Local(0) : f64
v1 = Local(1) : f64
v2 = Prim(F64Add, [v0, v1]) : f64
return v2
```

Canonical lowering:

```wasm
local.get 0
local.get 1
f64.add
```

For a boolean `if`:

```text
v0 = Local(0) : Bool01
v1 = If v0 {
       return Local(1) : Bool01
     } else {
       return Const(false) : Bool01
     } : Bool01
return v1
```

Canonical lowering:

```wasm
local.get 0
if (result i32)
  local.get 1
else
  i32.const 0
end
```

The checker ignores plan type annotations except as claims to verify. A
`CheckedPlan` stores the verifier-computed type, refinement, effect and
provenance for every value.

## V1 Profile

The first profile should be deliberately narrow.

Allowed in `expr-fragment-v1`:

- single-result functions;
- parameters as locals;
- no extra mutable compiler temporaries;
- constants needed by current scalar fragments;
- `local.get`;
- pure numeric and boolean primitive operations with exact Wasm semantics;
- structured value-producing `if`;
- no host calls at first, except later trusted total pure contracts when the
  registry/checker path exists.

Rejected in v1:

- `local.set` and `local.tee`;
- globals;
- memory/table operations;
- loops and unstructured branches;
- `return`, `return_call`, `br`, `br_if`, `br_table`;
- `drop`, `select`, `unreachable`;
- indirect/unknown calls;
- trapping ops unless explicit verifier-checkable safety obligations exist;
- nullable reference operations that need non-null proofs;
- any raw `i32` used as a canonical boolean.

`Bool01` is granted only from verifier-known sources:

- trusted ABI Bool parameters;
- comparisons;
- `i32.eqz`;
- `ref.test`/`ref.is_null` when admitted by profile;
- `i32.const 0` and `i32.const 1`;
- `if` whose branches are both `Bool01`;
- trusted total host contracts returning `Bool01`.

It is not granted because the plan says so.

## Sharing Rule

V1 should avoid SSA sharing that requires stack spilling.

Recommended rule:

```text
non-atom bindings are single-use;
Local and Const may be referenced multiple times because they lower to fresh
local.get/const instructions.
```

Later profiles may add deterministic local allocation and canonical
`local.set`/`local.get` sequences. Until then, a plan that needs sharing is not
certifiable.

## Host Calls

Host calls enter only through a trusted registry.

The plan may claim:

```text
v = HostCall(contract_id, args)
```

The verifier must independently resolve the actual Wasm callee/import,
signature and runtime ABI entry to a trusted contract. The plan cannot invent a
contract for an arbitrary call.

## Lean Binding

The verifier, not the compiler, owns the theorem target.

After `CheckedPlan` and byte equality succeed, the verifier generates a Lean
challenge from the checked plan and byte-derived binding. The supplied proof
must prove exactly that challenge. A theorem over a hash alone is not enough
unless the verifier-generated challenge also binds what that hash means.

Acceptance requires:

- expected theorem exists;
- theorem type definitionally equals the verifier-generated goal;
- Lean kernel/checker accepts it;
- axiom dependencies are exactly the approved whitelist;
- schema/prelude/plan-check/plan-lower/plan-bytes/wasm-slice/expr-fragment-accepted/toolchain hashes match
  policy.

## V2 Target

The v1 executable lowerer stays outside Lean:

```text
external verifier:
  canonicalLower(CheckedPlan) == actual Wasm code-entry bytes
```

For v2, move the semantic authority for lowering into Lean, but not by
running a Lean-compiled lowerer and trusting its output. Lean should
kernel-check a proposition that binds the checked plan to the actual bytes:

```text
Lean challenge:
  CheckPlan rawPlan moduleBinding registry profile = ok checkedPlan
  LowersCodeEntry checkedPlan moduleBinding actualCodeEntryBytes
  PlanSatisfies checkedPlan spec
```

The external verifier may still compute canonical bytes for speed and
diagnostics, but acceptance should rely on the checked theorem target. The
generated challenge should include byte-derived module facts and the actual
code-entry bytes, not only hashes.

A good v2 Lean shape is:

```text
match CheckPlan.check rawPlan moduleBinding registry profile with
| error _ => False
| ok checkedPlan =>
    LowersCodeEntry checkedPlan moduleBinding actualCodeEntryBytes
    and PlanSatisfies checkedPlan spec
```

Define both a reference function and a proof relation:

```text
lowerCodeEntry : CheckedPlan -> ModuleBinding -> Except LowerError ByteSeq
LowersCodeEntry : CheckedPlan -> ModuleBinding -> ByteSeq -> Prop
```

The function is useful for testing and generated proofs. The relation is the
acceptance object, because proofs can explain lowering structurally without
forcing the kernel to reduce one giant byte-producing function.

For larger bodies, prefer a span-based lowering certificate:

```text
LowersAt actualBytes node start end
```

Each plan node proves that its byte span is exactly the canonical encoding of
that node, composed from child spans. This is trace-like data only in the
benign sense: it proves canonical lowering of an already-checked plan. It must
not become trace-guided lifting from arbitrary Wasm.

The staged path is:

1. v2.0: Lean checks `RawPlan -> CheckedPlan`, lowering to actual bytes, and
   plan-level spec; external code still supplies most byte-origin facts.
2. v2.5: Lean checks the relevant module slicing that produced
   `ModuleBinding` and `actualCodeEntryBytes`. The current `WasmSlice.lean`
   export/code-entry pin is the first narrow piece of this stage for
   expression fragments.
3. v3: Lean checks the full relevant Wasm profile if the assurance tradeoff
   justifies the cost.

## Trace Sunset

Trace-guided replay is not part of the final acceptance architecture.

During migration it may help:

- debug the difference between current byte lifting and plan-first lowering;
- generate candidate plans for inspection;
- compare old and new classifiers in shadow mode.

It must not remain as an acceptance fallback. A fallback of:

```text
actual Wasm + trace -> CheckedPlan
```

would keep the lifter/replay checker in the TCB. The final verifier accepts
only:

```text
RawPlan -> CheckedPlan -> canonicalLower(CheckedPlan) == actual bytes
```

Sunset criteria:

- `expr-fragment-v1` plan-first covers every current certified scalar fragment;
- negative tests show operand swaps, extra instructions, bad `Bool01`, bad plan
  annotations and plan/proof/hash drift reject fail-closed;
- no required certificate is accepted only by trace replay;
- trace sidecars are not emitted;
- trace replay code is not part of the acceptance path.

## Migration Plan

1. Done: add a `RawPlan` parser for the existing fragment plan format.
2. Done: add `RawPlan -> CheckedPlan` type/refinement/effect checking.
3. Done: add canonical lowering from `CheckedPlan` to expected decoded `CodeTbl`.
4. Done: move the equality gate from trace replay to plan-first lowering.
5. Done: stop emitting trace sidecars.
6. Done for `expr-fragment-v1`: replace decoded `CodeTbl` comparison with
   byte-exact canonical code-entry comparison.
7. Done for `expr-fragment-v1`: render witness `code` and semantic face from
   the verifier-checked sidecar plan after code-entry equality succeeds.
8. Done for plan semantics and exact code-entry bytes: emit `Plans.lean` as
   Lean `ExprFragmentRawPlan` data, add the audited `PlanCheck.lean`
   structural checker, `PlanLower.lean` canonical `RawPlan -> WInstr body`
   lowerer and `PlanBytes.lean` canonical `RawPlan -> code-entry bytes`
   encoder, hash-pin all three, and make the verifier-authored witness prove
   every manifest plan passes the checker and lowers to both the byte-bound
   instruction body and canonical code-entry bytes.
9. Done for expr-fragment byte origin: add audited `WasmSlice.lean`, regenerate
   checker-owned `ArtifactBytes.lean` from the actual module bytes during
   verification, hash-pin the slicer, and make both `Plans.lean` and
   `CheckerWitness.lean` prove that each expression export resolves to the
   canonical code-entry bytes and byte-derived `FuncBinding`. The remaining v2
   work is full module binding extraction and non-expression obligation
   derivation in Lean.
10. Done for aggregate expr-fragment acceptance: add audited
    `ExprFragmentAccepted.lean` and make generated/checker-owned Lean prove one
    accepted-export predicate that composes `PlanCheck`, `PlanLower`,
    `PlanBytes` and `WasmSlice`. The predicate now carries the byte-derived
    function binding.
11. Done for the first artifact bridge: add audited `AcceptedArtifact.lean` and
    make checker-owned Lean prove raw artifact bytes + raw plan +
    `Schema.Obligation` imply accepted expr-fragment byte origin, plan lowering
    and obligation-code binding through `ArtifactData.acceptedExprFragments`
    for the aggregate expr-fragment claim list. The remaining v2 work is to
    carry checked signature/spec satisfaction and ordinary/recursive obligation
    families into the artifact-level predicate.
12. Done for artifact-carried bridge data: emit `Artifact.lean` with
    `AverCert.Artifact.data`, `acceptedWithFinal`, and
    `Artifact.certificate`, then make the verifier-authored witness pin that
    data to its checker reconstruction and root the axiom audit at
    `Artifact.certificate` after separately type-ascribing
    `Final.cert : Holds manifest`.
13. Done: remove the transitional byte classifier from expr-fragment
   admission/order; manifest entries are checked by plan-first lowering
   directly and merged by byte-derived function order.
14. Done: make producer-side expr-fragment certification require that the plan
    canonically lowers to the exact emitted code-entry bytes, and store the
    canonical lowered ops.
15. Done for current scalar expression islands: host-free Float/Bool and Int
    literal-comparison codegen now follows
    `MIR -> CertPlan -> canonical Wasm body`. Add future Int-carrier arithmetic
    by extending that plan grammar/lowerer path, not by adding byte recognizers.
16. Done for source-level bridge v0: source-projectable scalar fragments now
    enter artifact acceptance as `SymFragmentClaim`; Lean checks/encodes their
    `SymRawPlan` into the byte-bound `ExprFragmentRawPlan` before applying the
    existing accepted-export predicate. Representation-only fragments remain on
    an explicit fallback list.
17. Delete old whole-function scalar recognizer acceptance once plan-first has
   parity.

The implementation should move slowly, but every step should tighten the
acceptance path rather than add another permanent recognizer.
