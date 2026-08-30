# Certification Architecture

The certificate verifier has one job: decide whether an untrusted certificate
package proves the fixed Aver statement about the exact WebAssembly bytes it
was given. The producer may suggest data, but it cannot choose the checker,
the accepted theorem shape, or the facts recovered from the artifact.

This document is the architecture: how the verifier reaches its verdict and why the trust boundary sits where it does. See [certification.md](certification.md) for the user guide, and [certificate-format.md](certificate-format.md) for the normative format reference, including the trust inventory and the versioning and freeze policy.

## Invariants

The design keeps five authority rules explicit:

1. The `.wasm` file passed to `aver-cert` is the artifact identity.
2. `Plans.lean` is the only authoritative plan data in the package.
3. The soundness wall, Lean toolchain, build files, `ArtifactBytes.lean`, and
   checker witness are checker-owned.
4. A positive verdict comes from the named Lean acceptance root, not from a
   Rust reconstruction of the producer's classification.
5. Every mismatch declines; diagnostics cannot upgrade a failed proof.

## Components

`aver-cert` is a standalone crate and executable with its own `0.1.x` release
line. It does not depend on `aver-lang`, `aver-rt`, or `aver-memory`.

Its default `verify` feature contains the checker and embedded wall. The
separate `producer` feature contains the certificate-emission engine consumed
by `aver-lang` during `aver compile --certify`. The verifier does not link or
invoke that engine on its positive path.

`aver cert ...` only locates a sibling `aver-cert` executable or one on `PATH`
and runs it with unchanged arguments, standard input, standard output,
standard error, and exit status.

## Ownership of inputs

| Input | Owner | Treatment |
|---|---|---|
| WebAssembly module | Caller | Validated, hashed, and encoded into checker-generated `ArtifactBytes.lean` |
| `cert-manifest.json` | Certificate | Untrusted transport/report data; versioned and pinned against Lean data |
| `Plans.lean` | Certificate | Untrusted but authoritative plan data; structurally checked and canonically lowered in Lean |
| Model and artifact-specific Lean modules | Certificate | Untrusted proof data; admitted only after staging gates, data pinning, and kernel checking |
| Soundness wall | Verifier | Embedded, selected by exact `wall_id`, and materialized by the checker |
| Lean toolchain and build files | Verifier | Pinned to Lean 4.32 and authored by the checker |
| `CheckerWitness.lean` | Verifier | Generated for this artifact; never accepted from the package |

The public package therefore contains no `.plan` files and no
`ArtifactBytes.lean`. A JSON plan AST is not needed for acceptance: using one
as a second authority would add a parser/translation boundary and create two
representations that must agree. Tooling may project `Plans.lean` to JSON for
inspection, but such a projection cannot affect the verdict.

## Acceptance flow

```text
actual app.wasm
  -> wasmparser Validator
  -> checker-generated ArtifactBytes.lean
  -> WasmSlice / CertDecode --------------------------+
                                                       |
package Plans.lean -> PlanCheck -> PlanLower/PlanBytes +-> StandardFace
package Lean model and artifact proof data -----------+-> ClaimAxes
                                                       |
family soundness and discharge theorems ---------------+
                                                       v
                              Artifact.certificate
                                                       |
checker-generated pins and report agreement -----------+
                                                       v
                              AverCertChecker.checked
                                                       |
                              axiom audit + leanchecker --fresh
                                                       v
                                      CERTIFIED
```

The steps are:

1. Read the actual module, run the standard WebAssembly validator, compute its
   SHA-256, and parse `cert-manifest.json`.
2. Require package format `1`, statement schema `5`, the expected artifact
   root, target/profile/ABI identity, the actual hash, and a `wall_id` embedded
   in this verifier.
3. Assemble a fresh project from the checker-owned wall and the allowed
   artifact-specific data. Checker-owned module names, build files, toolchain
   files, witnesses, and caches supplied by the package cannot replace the
   generated versions.
4. Generate `ArtifactBytes.lean` from the bytes read in step 1. The package has
   no opportunity to provide a different byte numeral.
5. Build the package data and wall under Lean 4.32. `WasmSlice` and
   `CertDecode` recover the export/function/type/code facts required by the
   accepted fragment.
6. Check the `Plans.lean` values and lower accepted plans to their canonical
   instruction bodies and code-entry bytes. Those values must match the
   function selected from `ArtifactBytes`.
7. Run `StandardFace` over the checked claims. It binds each class to its
   standard domain, codomain, representations, complete host function, model
   constraints, signature, carrier, and structure facts. Class names and host
   role labels cannot grant a weaker face.
8. Run `ClaimAxes`. It derives partial versus total policy, the canonical
   termination witness, totality role, and exact runtime-contract set from the
   family plans. These are outputs of the proof check, not manifest choices.
9. Check `AverCert.Artifact.certificate` and alias it at the fixed type as
   `AverCertChecker.checked`. The checker witness also pins the Lean manifest
   and atomically derived `(export, class)` report entries to the JSON envelope.
10. Collect the named root's axioms and reject any name outside the whitelist
    `[propext, Classical.choice, Quot.sound]`.
11. Replay the checker module with `lake env leanchecker --fresh`. Only after
    all checks succeed is the human-readable report constructed.

Build caches are disabled by default. Explicitly configured data or prelude
caches never replace the checker-authored witness or the final fresh-environment
replay, but their directories are trusted local state: the integrity manifests
detect accidental corruption, not an active writer able to replace `.olean`
outputs, Lake traces, and the manifest together. Package-supplied caches remain
ignored.

The separate `aver cert check` developer preflight runs steps 1–10 but omits
step 11. It therefore trusts the locally built or explicitly cached `.olean`
graph and emits `CHECKED`, not `CERTIFIED`. The checker-owned witness is still
written and elaborated after cache restoration on every invocation, so the
report pins and axiom whitelist continue to run. This mode is suitable for
inner-loop and source/manifest tamper tests, never for release or admission.

## Why one Rust Wasm validator remains

The Lean wall decodes and binds every byte fact used by an admitted claim, but
its relevant-subset decoder is not a complete WebAssembly stack/control typing
validator. `wasmparser::Validator` is therefore retained as one explicit gate
before Lean. Removing it today would weaken the guarantee that the artifact is
a valid WebAssembly module.

No other producer analysis is needed for a positive verdict. In particular,
the verifier does not re-run the producer's obligation classifier,
disassembler, candidate derivation, or reconstruction of
`AverCert.Artifact.data`.

For the planned `wasip2` target (#1146), the same rule applies to the component
wrapper: the producer may declare a `prefix ++ embedded_core_module ++ suffix`
split while constructing the component, but the trusted verifier path must not
rediscover the user core by walking component bytes. It should consume the
declaration, confirm byte equality against the caller-supplied component, and
then run the core-module checks on the declared embedded module.

## Lean acceptance wall

The wall is one hash-addressed unit. Its responsibilities are separated by
module:

- `PlanCheck`, `PlanLower`, and `PlanBytes` validate plan data and produce the
  canonical semantics and bytes;
- `WasmSlice` and `CertDecode` recover relevant facts from the actual module;
- `StandardFace` selects the complete admitted semantic face and binds host,
  signature, carrier, and structure facts;
- `ClaimAxes` derives policy, termination, totality, and contracts;
- family soundness and `Discharge*` modules prove the reusable simulations;
- `AcceptedArtifact` and the artifact-specific bridge assemble the single
  acceptance statement.

Changing any audited wall source or the pinned toolchain changes `wall_id`.
The manifest can request only an identity already embedded in the verifier;
there is no filesystem, environment, or network fallback for a replacement
wall.

## Trust boundary

A successful verdict depends on:

- the small Rust verifier path for file I/O, hashing, version checks, safe
  staging, process execution, and report pinning;
- `wasmparser::Validator` for full WebAssembly validity;
- the exact embedded Lean wall and Lean 4.32 elaborator/kernel/tooling;
- the canonical local Elan home used to resolve that pinned toolchain;
- SHA-256 collision resistance;
- the semantic truth and totality, where required, of named runtime contracts;
- the explicit source declarations that a binary cannot determine;
- any explicitly configured local build-cache directory.

It does not depend on:

- correctness of the Aver compiler, optimizer, or producer classifier;
- the order or truth of JSON report candidates before Lean pinning;
- package-supplied wall/build/toolchain/witness files;
- certificate caches when none are explicitly configured;
- diagnostic output.

`leanchecker --fresh` prevents the final replay from inheriting declarations
from the prior elaboration environment. It is still a component of the same
Lean 4.32 distribution, not an independent checker implementation. The
current architecture should not be described as having two diverse kernels.

## Read declarations and scope

For known non-ADT families, `StandardFace` fixes the semantic face available
from checked plans and bytes. User ADTs have information that WebAssembly
erases: source domain meaning, representation interpretation, and the intended
model. Those `Dom`/`Repr`/model components remain explicit read declarations.
The wall enforces the byte-derived structure, standard portions of the face,
and available non-vacuity checks, but cannot recover source semantics that are
not present in the artifact.

Certified closure isolation does not certify the rest of the module. Imports,
the start function, and uncertified exports are accounted for, while behavioral
claims remain limited to admitted obligations. Trace/replay recordings are not
certificate evidence and never enter plan selection, theorem construction, or
the verdict.

## Fail-closed behavior

Verification rejects malformed or invalid Wasm, hash/version/wall mismatches,
unsafe package structure, unsupported plans or classes, noncanonical
lowerings, byte/type/host/structure disagreement, weakened semantic faces,
incorrect policy or contract axes, Lean build failure, a mismatched named root,
non-whitelisted axioms, and failed fresh replay.

An artifact with no accepted exports is reported as having no behavioral
certificate and exits nonzero. A diagnostic can explain a decline, but cannot
turn it into acceptance.
