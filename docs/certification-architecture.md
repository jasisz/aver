# Certification Architecture

Aver certificates bind a verifier-checked semantic plan to one exact Wasm
artifact and then ask Lean to prove the fixed certification statement for that
binding. The compiler may propose plans, manifests, and proof data, but it does
not choose the verifier's theorem target or trusted proof wall.

The central acceptance relation is:

```text
checked source/representation plan
  -> canonical instruction body and code-entry bytes
  == bytes selected from the named export in the artifact
  -> fixed Lean obligation over the source model
```

The Wasm bytes remain the artifact identity. A plan is an untrusted witness for
those bytes, not an alternative source of truth.

## Certificate Artifact

A certified build contains the Wasm module and a `cert/` directory:

```text
foo.wasm
cert/
  cert-manifest.json
  fragments/<export>.<profile>.plan
  AverCommon.lean and source-model modules
  Contracts.lean
  Module.lean
  Plans.lean
  Manifest.lean
  Certificate.lean
  Artifact.lean
  ArtifactSoundness.lean
  Final.lean
  ArtifactCertificate.lean
  ...copies of the audited checker modules...
```

The directory deliberately mixes untrusted artifact data with convenient
copies of audited modules. `aver cert verify` distinguishes them by ownership:

| Input | Treatment during verification |
|---|---|
| Wasm artifact | Read directly, hashed, decoded, and regenerated as checker-owned `ArtifactBytes.lean`. |
| Manifest, sidecars, models, and artifact-specific Lean modules | Treated as untrusted data; names and contents are gated before staging. |
| Audited Lean modules copied into the certificate | Ignored. Exact sources embedded in the verifier binary are staged instead. |
| Certificate lakefile, toolchain file, and `.lake` directory | Ignored. The verifier authors its own project and uses the pinned toolchain. |
| `CheckerWitness.lean` | Never accepted from the certificate; authored by the verifier for each check. |

Checker-owned Lean sources include:

- the schema and decoding layer: `SchemaCore`, `Schema`, `CertPrelude`,
  `CertDecode`, `PlanCheck`, `PlanLower`, `PlanBytes`, `WasmSlice`,
  `ExprFragmentAccepted`, `AcceptedArtifactCore`, and `AcceptedArtifact`;
- reusable semantics and soundness modules: `ExprFragmentSemantics`,
  `InterpreterSequencing`, `ExprFragmentSoundness`,
  `FieldProjectionSoundness`, `ConstructVerbatimSoundness`,
  `IntDispatchSoundness`, `StringSoundness`, `RecursionSoundness`,
  `MutualRecursionSoundness`, and `CompositionSoundness`;
- the acceptance wall: `AcceptanceSoundnessCore`, the `Discharge*` modules,
  and `AcceptanceSoundness`;
- the actual-byte module `ArtifactBytes` and the checker-authored lakefile and
  witness.

The manifest uses public certificate schema version `1`. It pins the Wasm hash,
the expected theorem and artifact root, sidecar hashes, and the exact SHA-256 of
every checker-owned wall module. The verifier recomputes every trust-bearing
pin; manifest agreement alone cannot produce acceptance.

The public proof roots are:

```lean
AverCert.Final.cert : AverCert.Schema.Holds manifest
AverCert.Artifact.certificate : AverCert.AcceptedArtifact.accepted
  AverCert.Artifact.data
```

`AverCert.Artifact.data` contains the manifest, artifact bytes, plan claims,
family claims, composition members, and whole-module closure facts.
The verifier requires that this term reduce to its independently reconstructed
artifact literal.

## Plan-First Binding

For source-projectable expression fragments, the sidecar carries a
source-shaped `SymPlan`. Its Lean form is a `SymRawPlan` over Aver concepts such
as integer comparison, floating-point arithmetic, boolean operations, and
string operations. The accepted chain is:

```text
SymRawPlan
  -> checked source plan
  -> ExprFragmentRawPlan
  -> checked representation plan
  -> WInstr body
  -> canonical Wasm code-entry bytes
```

`FragTy` and carrier details live in the representation layer. Source-level
projection is explicit; there is no generic raw-`WVal` escape hatch that lets a
representation-only plan claim an arbitrary Aver meaning. A fragment without
an admitted source or family bridge is declined.

The plan checker independently validates claimed types, refinements, effects,
arity, host roles, struct bindings, and result shape. Plan annotations do not
grant facts merely by being present. In particular, canonical booleans are
accepted only from checked `Bool01` sources such as boolean parameters,
comparisons, `i32.eqz`, admitted reference tests, constants `0`/`1`, and
branches whose results are both canonical booleans.

Canonical lowering is byte-exact rather than semantically permissive. For the
accepted expression profile it binds the body-size prefix, local declaration
vector, instruction bytes, and final `end`. Dead instructions, alternate local
groupings, noncanonical encodings, reordered operands, or a different export
binding do not match the plan.

String, constructor, verbatim, dispatch, recursion, mutual-recursion,
field-projection, and composition families use family-specific checked plans
and claims. Their numeric bindings are reconstructed from the module rather
than trusted from the sidecar. Host calls are admitted only through the audited
role/contract registry and must match the actual imported function, signature,
and ABI role.

## Verifier Algorithm

`aver cert verify` performs the following acceptance path:

1. Read the actual Wasm bytes, compute their SHA-256, parse
   `cert-manifest.json`, and require schema version `1`.
2. Compare the artifact hash, theorem name, artifact root, and every audited
   module hash with values compiled into the verifier.
3. Decode the module and derive export, function, type, local, import, host,
   struct, start-function, capability, and code-entry facts.
4. Derive non-expression certificate candidates from the decoded module.
   Expression candidates come from manifest-named sidecars; they are excluded
   from the non-expression byte classifiers.
5. Parse and check each selected sidecar in Rust, canonically lower it, and
   require exact equality with the code entry selected from the actual Wasm.
   The sidecar text must also equal the checker's canonical rendering and match
   its manifest hash.
6. Reject duplicate function orders and merge expression and non-expression
   obligations by byte-derived function order. Export names and report order
   therefore come from the module, not from JSON ordering.
7. Reconstruct runtime contracts, host/struct tables, module-envelope facts,
   checked plans, typed obligation faces, and the artifact literal used by the
   Lean witness.
8. Create a fresh Lean project. Stage certificate data only after a module-name
   gate and an elaboration-execution token scan; replace every checker-owned
   source, `ArtifactBytes.lean`, lakefile, and witness with verifier-authored
   content.
9. Build the project with the pinned Lean toolchain. A build failure is a
   rejection.
10. Run the checker-authored witness. It pins the manifest candidates,
    byte-derived names and module facts, checked plans, exact artifact data,
    semantic faces, final theorem type, and artifact acceptance root with Lean
    equalities and type ascriptions.
11. Ask Lean's axiom collector for the dependencies of the artifact root and
    require the exact whitelist `[propext, Classical.choice, Quot.sound]`.
12. Construct the human-readable report only after the witness succeeds, using
    byte-derived export names and kernel-confirmed manifest data.

The verifier has a fast witness and a diagnostic superset. The diagnostic
witness can localize a rejected conjunct, but it cannot upgrade a failure to an
acceptance. If the two modes disagree in that direction, verification fails
closed as an internal error.

Build caches are performance hints only. Cache keys include the schema,
toolchain, complete hash wall, and staged sources; cached Lake trees carry an
integrity manifest. A cache hit is still followed by the fresh project build
and checker-authored witness. A bad cache is discarded or causes rejection,
never certification.

## Lean Proof Chain

The kernel-checked dependencies have this shape:

```text
ArtifactBytes + Plans + Manifest
        |          |
        |          +-> PlanCheck -> PlanLower / PlanBytes
        +------------> WasmSlice / CertDecode
                            |
                            v
                    AcceptedArtifact claims
                            |
           family soundness + Discharge* theorems
                            |
                            v
                 AcceptanceSoundness.accept_sound
                            |
                            v
          ArtifactSoundness.accept_sound_holds
                            |
                            v
                      Final.cert
                            |
                            v
                 Artifact.certificate
```

`PlanCheck` validates plan structure. `PlanLower` reconstructs the measured
`WInstr` body. `PlanBytes` reconstructs canonical code-entry bytes.
`WasmSlice` resolves the named export through function and type indices and
requires the selected entry to equal those bytes. `CertDecode` derives the
non-expression code, carrier, host-role, and consumed-structure facts used by
the artifact predicate.

The family soundness modules prove reusable simulations. The `Discharge*`
modules connect accepted artifact claims and explicit semantic bridges to those
generic theorems. `AcceptanceSoundness.accept_sound` assembles all accepted
families into `Schema.Holds`. Artifact-specific side conditions remain explicit
in `AverCert.Artifact.dischargeSideConditions`; they cannot be silently replaced
by weaker manifest predicates.

The checker witness separately pins each obligation's `Dom`, `Cod`, `domRepr`,
`codRepr`, model, and inhabited domain to the standard face implied by its
byte-derived class. Vacuous faces such as an empty domain, always-false input
relation, always-true output relation, or weakened arity fail a kernel equality.

## Trust Boundary

Acceptance depends on the correctness of:

- the Rust verifier's hashing, Wasm decoding, module-context construction,
  sidecar parser/checker/lowerer, exact-byte comparison, non-expression
  classification, obligation reconstruction, and Lean project generation;
- the audited host ABI and runtime-contract registry;
- the checker-owned Lean schema, decoders, plan checks, lowerers, Wasm slicer,
  family soundness proofs, discharge wall, and acceptance assembly;
- the pinned Lean toolchain, elaborator, kernel, Lake invocation, and axiom
  collection;
- SHA-256 collision resistance and the semantic truth of the named runtime
  contracts assumed by an obligation.

The following inputs are outside the trust boundary:

- the Aver compiler and optimizer;
- manifest claims, ordering, theorem labels, report fields, and sidecar text;
- artifact-specific Lean definitions and proofs before checker binding and
  kernel checking;
- certificate copies of audited sources, the certificate lakefile/toolchain,
  and certificate build caches;
- source/debug names that are not independently recovered from Wasm exports.

Generated artifact-specific Lean is untrusted proof data, not a trusted proof
checker. The accepted theorem type is fixed by the verifier, artifact data is
pinned to the verifier reconstruction, and non-whitelisted axioms make the
witness fail.

## Fail-Closed Conditions

Verification rejects on any of these conditions:

- unsupported schema, missing or malformed manifest fields, or any hash-wall
  mismatch;
- malformed Wasm, ambiguous/invalid export binding, unsupported module shape,
  duplicate obligation order, or inconsistent whole-module facts;
- missing, malformed, unsupported, noncanonical, or hash-mismatched sidecars;
- plan type/refinement/effect failure, host/struct binding mismatch, lowering
  failure, or any byte difference from the selected code entry;
- disagreement between JSON candidates, byte-derived facts, generated Lean
  data, obligation faces, final theorem type, or artifact root;
- unsafe staged module names, rejected elaboration-execution tokens, Lean build
  failure, witness failure, or an axiom outside the whitelist;
- cache corruption, fast/diagnostic witness inconsistency, or any internal
  verifier inconsistency detected along the path.

Only exports represented by accepted obligations are reported as certified.
Other exports remain explicitly uncertified. Partial-correctness policies prove
the stated simulation when evaluation returns; totality is claimed only by a
total policy with its additional termination and runtime-contract premises.

## Known Trust Gaps and Limits

- Rust still parses, checks, and lowers sidecars and performs the executable
  byte-equality gate. Lean repeats structural plan checks, canonical lowering,
  byte encoding, and the relevant export slice, but does not replace all of the
  Rust path.
- Rust still performs substantial module validation and derives the complete
  non-expression obligation set. Lean binds many resulting code, carrier,
  export, host, and struct facts back to `ArtifactBytes`, but
  obligation-family selection remains part of the Rust TCB.
- `WasmSlice` and `CertDecode` are purpose-built relevant-subset decoders, not a
  complete independent validator for every Wasm feature.
- The token scan on untrusted Lean data is a deliberately strict defense
  against elaboration-time code execution, not a formal sandbox. The accepted
  result still depends on the pinned Lean elaborator/kernel boundary.
- Certification covers only admitted Wasm profiles and named runtime
  contracts. It does not establish arbitrary Wasm safety, equivalence for
  uncertified exports, or correctness of effects outside those contracts.
- A `SymPlan` expresses source meaning only for admitted source constructs.
  Representation-only shapes without a checked source or family bridge are not
  promoted to a source-level claim.

These limits are part of the stated guarantee: the checker rejects outside its
admitted surface instead of inferring a broader claim.

## Trace and Replay Exclusion

Execution traces and replay metadata are not certificate evidence. The
certificate format emits no `trace` or `trace_sha256` field, the verifier does
not read trace sidecars, and no acceptance branch reconstructs a plan from an
observed execution.

The only admitted direction is:

```text
untrusted plan
  -> independent checking
  -> canonical lowering
  -> exact artifact bytes
```

This direction is never reversed to `Wasm + trace -> accepted plan`. Traces may
be used by separate debugging tools, but they do not enter hashing, obligation
selection, theorem generation, witness checking, or the certification verdict.
