# Certification Trust Inventory and Versioning Policy

This document is the honest, complete inventory of what a party relying on an `aver-cert` verdict trusts, and the policy for how the identities a certificate is issued under evolve over time. It complements [certification.md](certification.md) (the user guide) and [certification-architecture.md](certification-architecture.md) (the data flow and acceptance pipeline). "The verifier" below means the verify path in `aver-cert/src/verifier.rs` (`verify()` → `trusted_check()`) together with the files it stages and the processes it runs.

## What a verifying party trusts

A green `CERTIFIED` verdict is exactly as strong as the components below. Each one is load-bearing: a defect in any of them could admit a wrong certificate. They are listed individually rather than rounded down to "the Lean kernel", because several of them sit on the adversarial input path and are not themselves kernel-checked.

### 1. The Lean kernel and the axiom whitelist

The final authority for every accepted claim is Lean 4's kernel checking the proof of the named root `AverCert.Artifact.certificate` (re-exposed as `AverCertChecker.checked`). The proof may use no axioms outside the whitelist `propext`, `Classical.choice`, `Quot.sound` (`AXIOM_WHITELIST` in `verifier.rs`). The whitelist is enforced by a checker-authored elaboration guard in `CheckerWitness.lean` that collects the transitive axiom closure of the checked root and fails the build on any other name, including `sorryAx`. Two placement facts matter for trust: the witness is authored after any cache restore and is never cached, so the guard runs on every `verify` and `check` invocation; and the final `leanchecker --fresh` replay re-checks every term in the import closure from an empty environment but does not itself enforce any axiom policy, so the guard's location in the always-fresh witness is load-bearing, not redundant.

### 2. The embedded soundness wall, pinned by wall id

The statement schema and every acceptance predicate live in the 33 checker-owned Lean sources embedded in the verifier binary (`aver-cert/src/wall.rs`, materialized from `aver-cert/assets/wall/current/`). Their identity is the wall id: a SHA-256 over a domain-separated, filename-sorted, length-framed encoding of every wall source plus the Lean toolchain pin as a synthetic file. The id is computed, not assigned; the verifier recomputes it over its own embedded sources on first use and aborts on disagreement with the compiled-in `CURRENT_WALL_ID`, so a binary cannot silently ship a wall that does not match its advertised identity. A certificate's `format.wall_id` resolves only against embedded walls — there is no filesystem, environment, or network fallback. Trusting the wall includes trusting the schema of claims itself: a certificate is only as meaningful as `Obligation.holds`/`holdsTotal` and the acceptance conjuncts, and auditing those definitions is exactly what the wall id makes stable.

### 3. The pinned Lean toolchain and the local Elan installation

Elaboration, `lake`, and the final `leanchecker --fresh` replay all come from the one pinned Lean distribution (`leanprover/lean4:v4.32.0`, `aver-cert/assets/wall/current/lean-toolchain`). `leanchecker` is a fresh-environment replay from the same distribution, not an independently implemented second kernel; the architecture must not be described as having two diverse kernels. The toolchain is resolved through the canonical local Elan installation (`ELAN_HOME`, or `~/.elan`), whose `bin/elan` is canonicalized once and invoked by absolute path as `elan run --install <pin> lake ...` (`aver-cert/src/lean_process.rs`). The verifier pins the toolchain name, not the toolchain binaries' hashes: on first use Elan downloads and installs the pinned toolchain, so the Elan installation and its acquisition channel are a bootstrap trust anchor of the whole scheme.

### 4. The Rust transport harness

Rust owns transport, and a verifying party trusts it to do exactly these jobs correctly (`verifier.rs`, `wall.rs`, `lean_process.rs`, `format.rs`, `main.rs`): read the artifact bytes and compute their SHA-256; parse `cert-manifest.json` strictly (exact-object field checks, policy/termination coupling, and the candidate gate holding every witness-interpolated string to at most 200 bytes of printable ASCII with no `"` or `\`); stage package data through the gates — module-name sanitation against `^[A-Za-z][A-Za-z0-9_]*\.lean$`, case-insensitive shadow rejection against toolchain roots and every checker-owned name, and the twenty-token elaboration-code scan (`#eval`, `run_cmd`, `macro`, `unsafe`, `deriving`, `attribute`, `«`, `open Lean`, ...) that keeps package Lean files data-only; author the checker-owned `ArtifactBytes.lean`, lakefile, toolchain pin, and witness; run each Lean step as a hermetic subprocess (cleared environment, implicit Lake caches disabled, `TMPDIR` redirected into checker-owned directories) under a wall-clock timeout so a degenerate or hostile certificate cannot hang verification; and map results to the verdict and exit code. The harness performs no parallel verdict reconstruction — the facts come from the kernel — but a bug in any of these transport duties is a bug inside the TCB.

### 5. `wasmparser::validate_all`, a required pre-gate on the adversarial path

Before anything else, the verifier runs the standard Rust WebAssembly validator over the artifact bytes. This gate is required, and it is part of the adversarial TCB: the input is attacker-chosen, and the Lean wall does not subsume the check. Quoting the rationale in `verifier.rs`: "Artifact acceptance reasons about a valid WebAssembly module. The Lean wall decodes every trust-bearing section and instruction, but it is not yet a complete Wasm validation algorithm (stack/control typing included)." Every accepted certificate is therefore a statement about a validator-accepted module, with `wasmparser` as the authority for full validity.

### 6. The wall's Wasm model and canonical byte lowering, held by differential testing

The wall models WebAssembly: `CertPrelude.lean` defines the instruction semantics the proofs evaluate, `CertDecode.lean`/`WasmSlice.lean` decode the relevant bytes, and `PlanLower.lean`/`PlanBytes.lean` define the canonical lowering from checked plans to instruction bodies and code-entry bytes. The kernel proves these definitions coherent with each other and binds them to the exact artifact bytes, but it cannot prove that the model is faithful to what real WebAssembly engines execute — that faithfulness is an assumption of the scheme, held empirically by differential testing rather than by proof. The suites that hold it today: `tests/cert_decode_spec.rs` (the in-kernel decoder agrees term-for-term with an independent Python byte oracle, `tools/certkit/decode_ref.py`, and with the Rust rederivation across the certkit fixtures, plus a byte-mutation suite and an opcode-coverage matrix); `tests/cert_verify_spec.rs` (end-to-end acceptance plus fail-closed declines for each tampering class); and the cross-backend execution differentials `tests/cross_backend_proptest.rs`, `tests/cross_backend_stress.rs`, `tests/wasm_gc_carrier_i64_differential.rs`, `tests/bigint_literals_differential.rs`, and `tests/wasm_gc_perslot_int_unboxing_differential.rs`, which run the same emitter output under a real engine and require exact agreement with the VM reference semantics. Those execution suites currently exercise one production engine — the Wasmtime runtime embedded in the compiler's `--wasm-gc` run path; agreement with other engines is not yet pinned by a named suite.

### 7. SHA-256 collision resistance

Both bindings that make the scheme non-transferable are hashes: the artifact identity (`wasm_sha256` against the recomputed digest of the supplied bytes) and the wall identity of item 2. An adversary who can produce SHA-256 collisions can transplant a certificate onto different bytes or a different wall.

### 8. Named runtime contracts and explicit read declarations

The certified theorems are conditional. L1 theorems assume the named runtime contracts (the disclosed laws of the host helpers), and L3 theorems additionally assume the disclosed totality of the selected helpers; proving the shipped runtime satisfies them is a per-release obligation outside any certificate. Separately, source meaning that WebAssembly erases — user-ADT domain meaning, representation interpretation, and non-reconstructible models — enters as explicit read declarations: the wall enforces the byte-derived structure and the standard portions of the face, and the theorem is conditional on the declared meaning of the rest.

### 9. Explicitly configured build caches

Caches are disabled by default. If `AVER_CERT_DATA_CACHE` or `AVER_CERT_PRELUDE_CACHE` is set, that directory becomes trusted local state: its integrity manifests detect accidental corruption, not an active writer able to replace `.olean` outputs and Lake traces together. Even with caches configured, the checker witness is authored and elaborated fresh on every invocation and strict `verify` always runs the whole-closure replay, so caches accelerate the build but never substitute for either enforcement point.

## What a verifying party does not trust

- **The Aver compiler and the certificate producer.** The classifier, disassembler, obligation rederiver, and Lean renderer behind `aver compile --certify` are emission diagnostics; none of them is linked into or re-run on the verifier's acceptance path. A wrong producer yields a declined certificate, not a wrong verdict.
- **Manifest display strings.** The `certified[].dom`/`cod` prose, `level`, `theorem`, `wasm`, `source_level_only`, and every human-readable reason are declared-only transport data. They can lie without affecting the verdict; the CERTIFIED/CHECKED report prints only kernel-pinned facts, and `explain` labels declared values explicitly.
- **Rust-side classifications and report candidates.** The JSON's certified list is a set of candidates; every trust-bearing field is pinned by the checker witness against the Lean manifest and the byte-derived facts before it can appear on the trusted report. The order or truth of candidates before Lean pinning carries no authority.
- **Caches as evidence.** Package-supplied caches and build products are ignored entirely; opt-in local caches are performance hints that cannot skip the witness elaboration or the fresh replay (see item 9 for the trust they do carry as local state).
- **Package-supplied build infrastructure.** Wall sources, lakefile, toolchain pin, artifact-bytes module, and witness are checker-owned; packaged files with those names are ignored or rejected during staging.
- **Diagnostics.** A diagnostic can explain a decline; it cannot upgrade one.

## Version identities and what bumps them

Three identities in `aver-cert/src/format.rs` govern acceptance, and the verifier requires an exact match on all three — there is no version negotiation, no downgrade path, and no fallback wall resolution:

| Identity | Constant | Current value | What it versions |
|---|---|---|---|
| Package layout version | `FORMAT_VERSION` | `1` | The on-disk `cert/` package shape and the transport envelope as a container (`cert-manifest.json` `format.version`) |
| Statement schema version | `CERT_SCHEMA_VERSION` | `2` | The certificate statement schema: manifest fields and their meaning, obligation shapes, plan grammars (`cert-manifest.json` `schema_version`) |
| Wall identity | `CURRENT_WALL_ID` | `sha256:0d667eb0...` (full value in `format.rs`) | The exact bytes of the 33 embedded wall sources plus the Lean toolchain pin |

What changes bump which:

- **Package layout changes** — adding, removing, or restructuring package files or the envelope container — bump `FORMAT_VERSION`.
- **Statement schema changes** — any change to what a certificate can state or how it states it — bump `CERT_SCHEMA_VERSION`. The `1 → 2` bump is the worked example: schema 2 made the subject's `hostRoleTable` optional, with `null` pinned against a byte-derived proof of the Int box helper's absence.
- **Any wall source change** changes the wall id automatically, because the id is computed from the bytes. A proof-only hardening of a wall predicate changes the wall id without touching either version number. The converse coupling is one-way: a statement-schema change also changes the wall id, since the schema types are themselves wall sources (`Schema.lean`, `SchemaCore.lean`), but most wall-id changes are not schema bumps.
- The `aver-cert` crate's own `0.1.x` release version is a package-manager label, not an acceptance identity; none of the three checks reads it.

## Certificate lifetime and re-certification

A certificate does not expire by time. It stays verifiable exactly as long as the verifier binary in use embeds the wall it names, and each `aver-cert` release currently embeds exactly one wall. The lifecycle at a wall change is:

1. A new verifier release ships a changed wall (new `CURRENT_WALL_ID`).
2. That verifier declines every previously issued package at the envelope gate with `unsupported certificate wall ...; no embedded wall matches` — fail-closed, before any Lean step runs.
3. The holder re-certifies: re-run `aver compile --certify` with the compiler release paired to the new verifier. Because `--certify` binds the emitter's exact bytes, re-certification generally re-emits the module, and the new certificate is a statement about the new bytes. Verifying the re-issued package still requires no trust in the compiler.

Older verifier binaries keep verifying older packages indefinitely — a binary's embedded wall never changes — so archival verification of a historical artifact/package pair means archiving the matching verifier release alongside it. Whether a released verifier may embed several walls (a grace window for recent predecessors) and what the deprecation policy for retired walls should be are open maintainer decisions.

## Freeze policy

> **OPEN DECISION — wall and schema freeze criterion.** Neither `FORMAT_VERSION = 1` nor `CERT_SCHEMA_VERSION = 2` is declared frozen, and the wall currently changes at the tempo of ordinary software development: predicate hardenings recompute the wall id, and each such change ends the forward-verifiable life of previously issued certificates (previous section). "Audit the wall once" only becomes real once the wall has the tempo of a standard, so a freeze needs an explicit criterion. The proposal on the table — a maintainer decision, deliberately not adopted in this document — is to declare a freeze candidate only after all three hold: (a) module framing and validation move into the kernel-checked wall, so `wasmparser` leaves the adversarial input path (item 5 above); (b) the wall build gains a mechanical pin-completeness check, so that every subject-visible fact must be pinned as an equality against the byte-derived decoder for the wall to build at all; and (c) N consecutive months pass with no soundness-motivated change to any wall source or to the statement schema, with N fixed when the criterion is adopted. Until a freeze is declared, relying parties should expect the wall id to move between releases and plan for the re-certification path above.
