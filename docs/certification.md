# Artifact Behavioral Certificates

An Aver artifact certificate is a Lean proof about one exact WebAssembly artifact. It states what selected exports of that artifact compute. Lean 4.34 checks it against the delivered bytes, so you do not have to trust the Aver compiler that produced the artifact or the certificate.

The proof runs from meaning to bytes. For every certified function the certificate carries a plan: the function's optimized MIR body, printed as Lean data. The checker's Lean wall lowers the plan to wasm the same way the compiler's MIR emitter does, requires the result to equal the function's code entry in the artifact, and proves that the lowered code computes what the plan means. The bytes are never decoded back into meaning.

A certificate is not a signature and not a reproducible-build attestation. It is a behavioral proof bound to the artifact's hash and to a statement schema the checker owns.

This document is the user guide: what a certificate is and how to produce and check one. [Certification Architecture](certification-architecture.md) explains how the verifier reaches its verdict. The [Certificate Format Specification](certificate-format.md) is the normative reference for reimplementors, with the trust inventory and the versioning policy.

## Generate and verify

Install the compiler and the independently versioned verifier:

```bash
cargo install aver-lang --features wasm
cargo install aver-cert
```

The compiler needs the `wasm` feature for `--target wasm-gc --certify`; add `wasip2` for component output. Verification needs a standard Elan installation. `aver-cert` selects the pinned Lean 4.34 toolchain through Elan and installs it when necessary.

Generate an artifact and its certificate package:

```bash
aver compile app.av --target wasm-gc --certify -o out/
aver compile app.av --target wasip2 --certify -o out/
```

The certificate binds the bytes that `--certify` writes. A `--certify` build keeps a few string optimizations off (the buffer-building and chars-fusion passes and the byte sink), so its module can differ from a plain `aver compile` of the same source. Ship the module the certified build wrote.

On `wasm-gc`, `--certify --optimize` keeps the proof boundary in separate files. The certificate binds the emitter's exact `<name>.wasm`, and Binaryen writes `<name>.optimized.wasm` outside the proof. A Wasmtime pack may compile that derivative ahead of time to `<name>.cwasm`; neither derivative is certified. `wasip2` rejects `--optimize`, because Binaryen does not yet accept this component and wasm-gc combination. Reusing an output directory replaces its `cert/` package, so use one output directory per artifact.

Check the package with the standalone verifier:

```bash
aver-cert check out/app.wasm out/cert
aver-cert verify out/app.wasm out/cert
aver-cert explain out/app.wasm out/cert

aver-cert check out/app.component.wasm out/cert
aver-cert verify out/app.component.wasm out/cert
aver-cert explain out/app.component.wasm out/cert
```

If `aver-cert` is next to `aver` or on `PATH`, `aver cert check|verify|explain ...` runs the same commands. `aver cert` is an exact subprocess shortcut: it forwards the arguments and standard streams to `aver-cert`, and the compiler binary links no verifier. `inspect` is an alias of `explain`.

`verify` is the release check. It exits successfully only when at least one export is certified and every step passes, including the final `leanchecker --fresh` replay of the whole proof. It prints `CERTIFIED`.

`check` is a faster developer and CI preflight. It runs the same Rust gates, `lake build` and checker witness, but trusts the freshly built or cached `.olean` files and skips the final replay. It prints `CHECKED`, never `CERTIFIED`. Do not use it as a release or admission gate.

`explain` runs the same check as `verify`, then prints each certified export with its policy, class, facets and model line, the runtime contracts, the law-claims and source-bridges, and the declined functions with their reasons.

### Target matrix

| Compile target | Certified artifact | Core bytes checked by the wall | Status |
|---|---|---|---|
| `wasm-gc` | `<name>.wasm` | The delivered module itself | Supported |
| `wasip2` | `<name>.component.wasm` | The embedded core module the envelope declares | Supported |
| `rust` | Generated Cargo project | None | Unsupported: the wall is a Wasm byte wall |

For wasip2 the manifest hash is the hash of the whole component. The manifest declares the component as prefix, core module and suffix by length. The verifier splits the component at those lengths and checks equality; it never parses the component to find the core. The import registry depends on the target. wasm-gc admits the exact Aver host imports, and wasip2 admits the exact 80 canonical-ABI module and name pairs the compiler can emit, with their pinned WASI interface versions. Both targets also admit contract-derived custom-capability imports under an exact hashed namespace grammar. Any other import is refused with a reason.

### Environment variables

Build caches are off by default. `AVER_CERT_DATA_CACHE=/trusted/path` reuses artifact-specific Lake output, and `AVER_CERT_PRELUDE_CACHE=/trusted/path` reuses the build of the artifact-independent wall. A cache directory is trusted local state, so it must not be writable by an attacker. Even with caches, every run writes and elaborates a fresh checker witness, and `verify` still runs the final replay.

`AVER_CERT_BUILD_JOBS=N` lets `lake build` run N Lean workers at once. The default is 1. A package with more than 32 planned functions spreads its byte facts over several modules, and bridge step lemmas always come in slices of 24 per module. Lake builds those modules in parallel. Each worker gets the full heap ceiling, `AVER_CERT_MEMORY_LIMIT_MB` (16384 by default), so the two settings multiply.

`AVER_CERT_TIMINGS=1` prints how long each Lean step took, with Lake's per-module build times, to standard error. It is a diagnostic and does not change the verdict.

Every Lean step (the proof build, the witness check and the final replay) runs under a wall-clock limit of 15 minutes. When a step exceeds it, its whole process tree is stopped and the certificate is declined. `AVER_CERT_PHASE_TIMEOUT_SECS=N` replaces the limit, for slow machines or a first run that still installs the toolchain.

## What a successful certification means

The public proof root is:

```lean
AverCert.Artifact.certificate :
  AverCert.AcceptedArtifact.accepted AverCert.Artifact.data
```

The checker binds it to its own root `AverCertChecker.checked`. Acceptance establishes that:

- the proof is about the exact bytes given to `aver-cert`;
- every certified export's code entry is the wall's lowering of its plan, and the plan type-checks at the export's declared signature;
- the obligations in the manifest are exactly the ones the wall derives from the plans, policy and termination witness included, and the listed runtime contracts are exactly the ones the wall derives from the helpers the lowered code calls and from the L3 obligations;
- the declared type layout matches the type section, and every string literal matches its data segment;
- the exports, imports, start function and the call closure of the certified exports are accounted for;
- the proof uses no axiom outside `propext`, `Classical.choice` and `Quot.sound`.

For each certified export the theorem says: for every carrier specification and every set of runtime helpers that obey the named contracts, if the emitted function returns on well-typed, represented arguments, the result represents what the plan returns at the same fuel. Int arguments are assumed to be canonical carriers. Every carrier the runtime builds is canonical; a host that fabricates its own carrier words is outside the claim.

Exports the wall does not admit are listed as uncertified with a reason. The certificate makes no claim about them.

### Certification levels

| Level | Policy | Guarantee |
|---|---|---|
| L1 | `simulatesModel` | If the function returns, its result represents the plan's result. The named runtime contracts are explicit premises. |
| L3 | `simulatesModelTotally` | L1, and the function returns on every well-typed input within fuel `n.natAbs + 1`, where `n` is the first Int argument. It also assumes the add and sub helpers (and mul, when a member multiplies) always return. |

L3 is derived by the wall (`GrammarTotal.checkTermGroup`), never read from a manifest label. It admits one recursion shape per call group: every parameter is an Int, the result is an Int or a Bool, the body is `if n <= 0 then base else step`, the arms use only literals, parameters, Int `+ - *` and calls to group members, and every such call passes `n - 1` as its first argument. A package with both policies reports `mixed L1/L3`.

### What is admitted

Every certified export reports one class, `source-plan-v1`, with facets the wall derives from the plan: `recursive`, `mutual`, `calls`, `records`, `variants`, `strings`, `floats`.

The plan grammar is the admitted subset of optimized MIR. It covers Int, Bool, Float and String literals; locals and named `let`; calls to other planned functions, including self and mutual recursion and tail calls; Int `+ - *` and the six comparisons; Bool `and`, `or`, `not`, `==` and `!=`; Float comparisons other than `!=`; String `+`, `==`, `!=` and interpolation of String parts; `if`; records with two or more fields (create in declared order, and project); user variants, `Option` and `Result` (construct and match); matches on Int, Bool and String literals and flat tuple destructuring; `Option.withDefault` and `Result.withDefault`; `Option.withDefault(Vector.get(v, i), <literal>)`; `Int.div` and `Int.mod` by a nonzero literal, or fused under `Result.withDefault` with an Int default; the empty list and `List.prepend`.

A function is declined, with the MIR node or type named in the reason, when it has effects, uses raw i64 slots, negates an Int (the negation helper has no wall template yet), uses an Int literal outside the i64 range, does Float arithmetic, calls through a function value, matches on a list, or uses any other node outside the subset. A function is also declined when the producer's check finds that its plan does not lower to exactly its code entry.

## Package format

The package format is version `1` and the statement schema is version `9`. A `cert/` directory contains:

- `cert-manifest.json`, the transport and report envelope;
- `Plans.lean`, the plans and the declared type layout;
- `Manifest.lean`, `Module.lean`, the `Artifact*.lean` byte-fact modules, `Final.lean` and `ArtifactCertificate.lean`;
- the model modules under `AverModel/`, `Bridge.lean` with its proof modules, and `Laws.lean`, when the package declares source-bridges or law-claims.

The package does not supply `ArtifactBytes.lean`. The verifier generates it from the file it reads.

The manifest's `format.wall_id` selects one exact Lean wall embedded in the verifier. Package files cannot replace the wall, the toolchain, the build files, the artifact bytes or the checker witness.

Schema 9 rejects every earlier package. Regenerate old packages with `aver compile --certify` from the matching compiler before checking them with a schema-9 verifier.

## Plans, source functions and law-claims

The model of every obligation is the plan. The report line of each export says so: `model: plan (the export's optimized MIR body)`.

A source-bridge connects the plan to the function you wrote. It is a kernel-checked theorem that the plan computes the transpiled source function `<Module>.<fn>` through source-value encoders. The manifest declares only its structure: the export, the source function, a statement kind and one encoder per parameter and result. The verifier writes the statement from that structure and requires the package to prove exactly it.

There are two kinds. An `exact` bridge says that, above some fuel, the plan returns the encoded source result on every encoded argument; the producer uses it when the call closure has no recursion. An `adequate` bridge says that whatever the plan returns is the encoded source result. It is not a termination claim: a plan that never returns satisfies it.

A bridge is credited when its pin elaborates and its proof uses only whitelisted axioms. `check` and `verify` report `source-bridges: N of M credited`. `explain` then prints `model: plan ≡ <Module>.<fn>` for the export (with `wherever the plan returns` for an adequate bridge) and shows the statement the checker rendered.

A law-claim is a universal law of the source model, pinned together with the certificate. Its corollary is `(law) ∧ Holds`, reported as `law-claims: N of M credited`. When every source function the law mentions has a bridge, the claim gets a second corollary that also conjoins those bridges, reported as `bridged-laws: N of M credited`. The two counters move separately: a bridge that fails costs the bridge and the bridged corollary, never the law. Neither counter changes the verdict or the exit code.

Two things stay outside the bridge. The model definition `<Module>.<fn>` is emitted by the compiler with the certificate, so a bridge is proved relative to that definition. And the producer chooses the encoders, which are part of the statement. An export without a credited bridge keeps the weaker position: that its plan is your function rests on the compiler that printed it. `explain` lists why each such export got no bridge, from the package's `sourceBridgesDeclined` list.

## Trust and explicit limits

A verdict trusts the small `aver-cert` Rust path, the one retained `wasmparser::Validator` check, the embedded Lean wall, the Lean 4.34 toolchain, SHA-256, and the named runtime contracts. It does not run the producer, the plan printer or any Rust reconstruction of the claim.

The runtime contracts say what the Int, String and index helpers compute. The certificate pins each helper's body to a template, but it does not prove the contracts. The bignum sub-routines those helpers call are not pinned at all, and the theorem treats every helper as a pure function that changes nothing the caller can reach.

L3 "returns" is about the wall's interpreter, where fuel counts nested calls. A real engine can still run out of stack or memory on large inputs.

`leanchecker --fresh` replays the proof in a fresh environment, but it ships with the same Lean toolchain. The scheme does not yet have a second, independently written kernel.

The canonical Elan installation is a local trust anchor. Every Lake, Lean and leanchecker subprocess otherwise starts with a cleared environment, the exact pinned toolchain, no implicit Lake caches and checker-owned temporary paths.

The [Certificate Format Specification](certificate-format.md), section 12, lists every trusted item.
