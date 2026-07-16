# Artifact Behavioral Certificates

An Aver artifact certificate states what selected exports of one exact
WebAssembly module compute. It is checked against the module bytes by Lean
4.32 and does not require trusting the Aver compiler that produced either the
module or the certificate package.

This is a behavioral proof, not a signature or a reproducible-build
attestation. A valid certificate binds all accepted claims to the supplied
`.wasm` bytes and to a fixed, checker-owned statement schema.

## Generate and verify

Install the compiler and the independently versioned verifier:

```bash
cargo install aver-lang --features wasm
cargo install aver-cert
```

The compiler needs the `wasm` feature for `--target wasm-gc --certify`.
Verification also requires a standard Elan installation; `aver-cert` selects
the pinned Lean 4.32 toolchain through Elan and installs it when necessary.

Generate a wasm-gc module and its certificate package:

```bash
aver compile app.av --target wasm-gc --certify -o out/
```

`--certify` cannot be combined with `--optimize`, because the certificate binds
the emitter's exact bytes. Reusing an output directory replaces its `cert/`
package, so use one output directory per artifact.

Run the fast preflight or strict verification directly with the standalone
checker:

```bash
aver-cert check out/app.wasm out/cert
aver-cert verify out/app.wasm out/cert
aver-cert explain out/app.wasm out/cert
```

If `aver-cert` is next to `aver` or on `PATH`, the same commands are available
through:

```bash
aver cert check out/app.wasm out/cert
aver cert verify out/app.wasm out/cert
aver cert explain out/app.wasm out/cert
```

`aver cert` is an exact subprocess shortcut. It forwards the original
arguments and standard streams to `aver-cert`; the compiler binary contains no
linked verifier and no alternate acceptance path. `inspect` is an alias of
`explain`.

`verify` exits successfully only when at least one export is certified and the
complete check passes. `explain` first performs that same check, then prints
the accepted exports, policies, semantic faces, runtime contracts, and the
explicitly declined surface.

`check` is an explicitly weaker developer/CI preflight. It performs the same
Rust gates, `lake build`, and fresh checker-witness elaboration, but trusts the
freshly built or explicitly cached `.olean` closure and skips the final
`leanchecker --fresh` whole-closure replay. It reports `CHECKED`, never
`CERTIFIED`; do not use it as a release or admission gate.

Build caches are disabled by default. `AVER_CERT_DATA_CACHE=/trusted/path`
opts into artifact-specific Lake output, while
`AVER_CERT_PRELUDE_CACHE=/trusted/path` also reuses artifact-independent wall
output. Those directories become trusted local state and must not be writable
by an attacker. Strict `verify` still authors a fresh checker witness and runs
the final whole-closure replay.

## What a successful certification means

The public proof root is:

```lean
AverCert.Artifact.certificate :
  AverCert.AcceptedArtifact.accepted AverCert.Artifact.data
```

The checker binds it to the named local root
`AverCertChecker.checked`. Acceptance establishes that:

- the proof concerns the exact bytes supplied to `aver-cert`;
- every certified export has one admitted class and its standard domain,
  codomain, representation relations, host behavior, and model constraints;
- policy, termination evidence, totality role, and disclosed runtime contracts
  are the canonical values derived from the checked plans;
- certified exports, other exports, imports/capabilities, the start function,
  and the reachable certified call surface are accounted for;
- the proof uses no Lean axioms outside the allowed whitelist:
  `propext`, `Classical.choice`, and `Quot.sound`.

Exports outside the admitted fragment are listed as uncertified with a reason.
The certificate makes no behavioral claim about them.

### Certification levels

Each obligation carries one of two policies:

| Level | Policy | Guarantee |
|---|---|---|
| L1 | `simulatesModel` | If evaluation returns a value, it is represented by the declared model result. Named runtime contracts remain explicit premises. |
| L3 | `simulatesModelTotally` | The simulation is total for the admitted inputs, using Lean-derived termination evidence and the required total runtime contracts. |

A package containing both policies reports `mixed L1/L3`. A totality claim is
never inferred from a JSON label: `ClaimAxes.lean` derives its policy,
termination witness, totality role, and exact contract set from the checked
family plans.

### Admitted families

The current schema admits these fail-closed families:

| Manifest class | Certified shape |
|---|---|
| `expr-fragment-v1` | Source-projectable scalar and representation fragments with canonical lowering |
| `verbatim-string-eq` | Audited `String.eq` leaf |
| `verbatim-string-concat` | Audited `String.concat` leaf |
| `adt-constructor` | Admitted user-ADT constructor shapes |
| `self-recursive` | Single-argument integer recursion |
| `multi-argument self-recursive` | Integer accumulator recursion |
| `mutual-recursive` | Admitted mutually recursive integer SCCs |
| `verbatim-dispatch` | Verbatim ADT/variant dispatch |
| `int-dispatch` | Integer-valued ADT/variant dispatch |
| `field-projection` | Byte- and type-bound field projection |
| `cross-function-composition` | Admitted direct-call composition closure |

Plans must lower canonically to the function selected from the actual module.
Unsupported instructions, signatures, host roles, types, call shapes, or
noncanonical encodings are declined. Exact-bit Float results involving
WebAssembly operations with nondeterministic NaN payloads are also declined
unless the admitted result relation can state the weaker behavior honestly.

## Package format

The first public package format is version `1`, and its certificate statement
schema is version `1`. A generated `cert/` directory contains:

- `cert-manifest.json`, a transport and reporting envelope;
- `Plans.lean`, the sole authoritative plan data;
- artifact-specific model, manifest, certificate, and proof modules.

There are no public `fragments/*.plan` sidecars. There is also no
certificate-supplied `ArtifactBytes.lean`: the verifier regenerates that module
from the `.wasm` file it actually reads.

`cert-manifest.json` records the format/schema versions, artifact hash,
embedded-wall identity, report candidates, declared uncertified exports,
capabilities, and other envelope metadata. It does not contain an
authoritative plan AST. During verification, the checker requires its report
view to agree with the Lean manifest and the class/order derived by
`StandardFace.lean`. The manifest's `dom`/`cod` strings are display-only and
are not pinned by the checker witness, so the CERTIFIED/CHECKED report prints
only the pinned class; `aver cert explain` shows the declared face, explicitly
labeled as manifest-declared.

The manifest's `format.wall_id` selects one exact soundness wall embedded in
the verifier. Files from the certificate package cannot replace the wall,
toolchain, build configuration, generated artifact bytes, or checker witness.

## Trust and explicit limits

The verification TCB consists of the small standalone `aver-cert` orchestration
path, the one retained `wasmparser::Validator` validity check, the selected
checker-owned Lean wall, Lean 4.32's elaborator/kernel and build tools, the
approved runtime contracts, and the cryptographic hash binding. The positive
verdict does not run the producer classifier, disassembler, or Rust obligation
rederivation.

The Lean wall independently checks the relevant byte slices and plan
lowerings. `StandardFace.lean` fixes the admitted semantic face and host
tables; `ClaimAxes.lean` fixes policy, termination, totality role, and runtime
contracts. The artifact proof is then checked through the named root and its
axiom closure.

The Lake build and witness elaboration import the built `.olean` closure.
`leanchecker --fresh` then kernel-checks that whole closure in a fresh Lean
declaration environment. It is part of the same Lean toolchain, not a
separately implemented or independently distributed kernel checker.

The canonical Elan installation directory is a local trust anchor. Every Lake,
Lean, and leanchecker subprocess otherwise starts with a cleared environment,
the exact pinned toolchain, disabled implicit Lake caches, and checker-owned
temporary paths. Ambient Lean/Lake search paths and toolchain overrides are not
forwarded.

Some source meaning cannot be reconstructed from WebAssembly alone. In
particular, ADT domain/representation and model declarations remain explicit
read declarations. They are not silently inferred from bytes; the theorem is
conditional on their stated meaning, while the wall still enforces the
standard byte-level face and non-vacuity checks available for that family.

See [Certification Architecture](certification-architecture.md) for the exact
data flow and trust boundary.
