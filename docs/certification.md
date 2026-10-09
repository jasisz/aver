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

The certificate binds the bytes that `--certify` writes, and they are the bytes a plain `aver compile` of the same source writes: `--certify` only adds the `cert/` package. To keep that true, the string optimizations (buffer building, chars fusion, the byte sink) leave alone every function the certificate could describe in its source form, in every wasm-gc and wasip2 build.

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

`explain` runs the same check as `verify`, then prints each certified export with its policy, class, facets and model line, the input domain (every Int input is assumed to be a canonical carrier), the runtime contracts, the law-claims and source-bridges, and the declined functions with their reasons.

### Target matrix

| Compile target | Certified artifact | Core bytes checked by the wall | Status |
|---|---|---|---|
| `wasm-gc` | `<name>.wasm` | The delivered module itself | Supported |
| `wasip2` | `<name>.component.wasm` | The embedded core module the envelope declares | Supported |
| `rust` | Generated Cargo project | None | Unsupported: the wall is a Wasm byte wall |

For wasip2 the manifest hash is the hash of the whole component. The manifest declares the component as prefix, core module and suffix by length. The verifier splits the component at those lengths and checks equality; it never parses the component to find the core. The import registry depends on the target. wasm-gc admits the exact Aver host imports and, for a program with job kinds, the four `aver:work/v1` scheduling imports (`submit`, `take`, `task`, `complete`), and wasip2 admits the exact 80 canonical-ABI module and name pairs the compiler can emit, with their pinned WASI interface versions. Both targets also admit contract-derived custom-capability imports under an exact hashed namespace grammar. Any other import is refused with a reason.

### Environment variables

Build caches are off by default. `AVER_CERT_DATA_CACHE=/trusted/path` reuses artifact-specific Lake output, and `AVER_CERT_PRELUDE_CACHE=/trusted/path` reuses the build of the artifact-independent wall. The data cache keys each package module on its source and the package modules it imports, so after a change to one function the modules that do not depend on it are reused. Lake still rebuilds any restored module whose inputs differ. A cache directory is trusted local state, so it must not be writable by an attacker. Only `check` uses them: `verify` ignores both variables, prints a notice, and builds from the staged sources alone. Every run writes and elaborates a fresh checker witness and runs the audit program, and `verify` runs the final replay.

Cache reuse follows generated Lean imports, not just source-level calls. Expensive source-bridge body proofs (`BridgeBodies<i>`) depend only on their own model/image slices and those of their direct callees, plus shared support and string literals; they do not import the global `Plans`/`Manifest` tables. Export assembly (`BridgeAssembly<i>`) is also independent of those tables: it proves the semantic conclusion assuming the export's image and its call closure's step proofs. A one-function edit can therefore reuse unrelated body and assembly proofs. The `BridgeSteps<i>` and `BridgeProof<i>` bindings still connect them to the authoritative plans, images and artifact obligations, and rebuild when those tables change. Source-model imports, shared string literals and changes to the function or export partitions can still invalidate multiple slices; reuse is not guaranteed per source function.

For incremental proof reuse, configure both the data cache and the prelude cache. Restoring a package module does not guarantee that Lake will skip it: rebuilding an uncached wall module can produce a different `.olean` even from unchanged source, invalidating its dependents. The pristine prelude cache keeps those wall inputs stable; Lake still validates the restored package outputs. When enabling the prelude cache after using the data cache alone, start a fresh data-cache directory: existing module entries retain their original traces rather than being replaced after a rebuild, so traces from the old wall build can otherwise keep forcing recompilation.

`AVER_CERT_BUILD_JOBS=N` lets `lake build` run N Lean workers at once. By default the checker runs as many workers as the machine has cores and as its available memory holds at 4 GiB each (`MemAvailable` on Linux; free, inactive and speculative pages on macOS), and at least one; one when it cannot read the available memory. The packages `aver compile --certify` writes keep every module near 2 GiB at most, so the 4 GiB budget leaves room for Lake and the system; a machine that checks packages with larger modules sets the variable itself. A package with more than 32 planned functions spreads its byte facts over several modules, and bridge step lemmas always come in slices of 24 per module. Lake builds those modules in parallel. Each worker gets the full heap ceiling, `AVER_CERT_MEMORY_LIMIT_MB` (16384 by default), so the two settings multiply.

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

The plan grammar is the admitted subset of optimized MIR. It covers Int, Bool, Float and String literals; locals and named `let`; calls to other planned functions, including self and mutual recursion and tail calls; Int `+ - *` and the six comparisons; Bool `and`, `or`, `not`, `==` and `!=`; Float comparisons other than `!=`; String `+`, `==`, `!=` and interpolation of String and Int parts; `if`; records with two or more fields (create in declared order, and project); user variants, `Option` and `Result` (construct and match); matches on Int, Bool and String literals, flat tuple destructuring, and a List match on `[]` and `[head, ..tail]`; `Option.withDefault` and `Result.withDefault`; `Int.div` and `Int.mod` by a nonzero literal, or fused under `Result.withDefault` with an Int default; list literals, empty or not, and `List.prepend`; and `List.len`, `List.reverse`, `List.concat`, `List.take`, `List.drop`, and `List.contains` over Ints, Strings and Bools. A non-empty literal calls the per-type cons helper once per item; the certificate carries that helper as a planned function and the wall checks that its plan is exactly one `List.prepend` of its two parameters. The other List operations call per-type runtime helpers, loops over the cons cells; the wall pins each helper's bytes to its own template and proves what that template computes, so they add no runtime contract (`List.contains` over Ints or Strings relies on the equality contract it calls). A function whose plan calls one of these helpers gets a source bridge like any other: its proof meets each helper call at the source's `List.length`, `List.reverse`, `++`, `List.take`, `List.drop` or `List.contains`. A function that touches a `Vector` is declined: on wasm-gc a `Vector` value is a version struct over a shared array, and the wall models it as the plain array of its elements. `Bytes`, the standard library's octet refinement, is admitted with its packed representation, a `(array (mut i8))` of its octets: a `Bytes` value may be a parameter, a result, a record or constructor field, or an `Option` or `Result` payload, and the plan reads the operations the emitter performs on it. `Bytes(values = xs)` calls the per-type `pack` helper, which stores each element's low 8 bits after the checked Int conversion (a Big Int traps); `bytes.values` calls `unpack`; `List.len(bytes.values)` is the inline `array.len`; and a construction over `List.concat`, `List.take` or `List.drop` of projections calls the helper that copies the bytes without unpacking. These helpers, and the checked conversion, are pinned to wall templates and proved like the List helpers, so `Bytes` adds no runtime contract (`unpack` boxes each byte with the box contract). A `Bytes` result of `2^31` bytes or more is not modelled, and a function that touches `Bytes` gets no source bridge yet. `e?` on a `Result` is admitted anywhere in a body whose own result is a `Result` with the same error type: the emitter tests the tag and, on `Err`, returns a fresh `Err` of the function's result type, and the wall's source meaning follows the same order of evaluation to the first `?` that meets an `Err`. `?` on a `Result<Unit, _>` is declined, and a function that uses `?` gets no source bridge yet. An Int interpolation part calls the runtime formatter `String.fromInt`; the wall pins its bytes to a template and proves that it writes the Int's decimal digits, after a `-` for a negative number, so it adds no runtime contract. Its Big Int branch is pinned but not modelled, so a claim says nothing about interpolating an Int outside the `i64` range, and a function that interpolates an Int gets no source bridge yet. A function that touches a `Vector` is declined: on wasm-gc a `Vector` value is a version struct over a shared array, and the wall models it as the plain array of its elements.

A function is declined, with the MIR node or type named in the reason, when it has effects, uses raw i64 slots, negates an Int (the negation helper has no wall template yet), uses an Int literal outside the i64 range, does Float arithmetic, calls through a function value, calls a `List` helper other than `List.prepend`, or uses any other node outside the subset. A function is also declined when the producer's check finds that its plan does not lower to exactly its code entry.

## Package format

The package format is version `1` and the statement schema is version `9`. A `cert/` directory contains:

- `cert-manifest.json`, the transport and report envelope;
- `Plans.lean`, the plans and the declared type layout;
- `Manifest.lean`, the `Artifact*.lean` byte-fact modules, `Final.lean` and `ArtifactCertificate.lean`;
- the model modules under `AverModel/`, `Bridge.lean` with its proof modules, and `Laws.lean`, when the package declares source-bridges or law-claims.

The package does not supply `ArtifactBytes.lean` or `Module.lean`. The verifier generates them from the file it reads and its hash.

The manifest's `format.wall_id` selects one exact Lean wall embedded in the verifier. Package files cannot replace the wall, the toolchain, the build files, the artifact bytes or the checker witness.

Schema 9 rejects every earlier package. Regenerate old packages with `aver compile --certify` from the matching compiler before checking them with a schema-9 verifier.

## Plans, source functions and law-claims

The model of every obligation is the plan. The report line of each export says so: `model: plan (the export's optimized MIR body)`.

A source-bridge connects the plan to the function you wrote. It is a kernel-checked theorem that the plan computes the transpiled source function `<Module>.<fn>` through source-value encoders. The manifest declares only its structure: the export, the source function, a statement kind and one encoder per parameter and result. The verifier writes the statement from that structure and requires the package to prove exactly it.

The proof decodes every argument the function can take: Ints, Bools, Strings, records, tuples, variants, `Option`, `Result`, and Lists of any of these, Lists of Lists included. A List of records, variants or Lists is decoded one cons cell at a time through its element's own decoder, which the producer derives from the element type of the plan. A function with a Float or a `Vector` argument gets no bridge, and neither does one whose statement would exceed the checker's size limits (very large records).

There are two kinds. An `exact` bridge says that, above some fuel, the plan returns the encoded source result on every encoded argument; the producer uses it when the call closure has no recursion. An `adequate` bridge says that whatever the plan returns is the encoded source result. It is not a termination claim: a plan that never returns satisfies it.

A bridge is credited when its pin elaborates and its proof uses only whitelisted axioms. `check` and `verify` report `source-bridges: N of M credited`. `explain` then prints `model: plan ≡ <Module>.<fn>` for the export (with `wherever the plan returns` for an adequate bridge) and shows the statement the checker rendered.

A law-claim is a universal law of the source model, pinned together with the certificate. Its corollary is `(law) ∧ Holds`, reported as `law-claims: N of M credited`. When every source function the law mentions has a bridge, the claim gets a second corollary that also conjoins those bridges, reported as `bridged-laws: N of M credited`. The two counters move separately: a bridge that fails costs the bridge and the bridged corollary, never the law. Neither counter changes the verdict or the exit code.

A law-claim is proved in the model as `aver proof` proves the law: by the proof steps the proof kernel checked, with Lean's tactics behind them. A `when`-law only steps prove is a law-claim with nothing but `sorry` behind its steps, and a law with a `by` line gets its plan's steps or no proof. When Lean does not accept such steps, the claim is reported as not credited, depending on `sorryAx`; a `when`-law without steps that no tactic arm claims is not a law-claim at all.

By default the producer bridges only the law cone: the functions a law-claim of the package mentions, and the functions their plans call, transitively, since a bridge needs its callees' bridges. These are all the bridges a bridged law can cite. Every other certified export, typically a function only `verify` examples or nothing at all reach, is listed in `sourceBridgesDeclined` with the reason `no law-claim reaches it (only verify examples or nothing do); pass --examples to bridge it`. `aver compile --certify --examples` bridges every certified export. The byte certificate, the law-claims and the bridges each law-claim cites are the same in both modes; only the number of bridges, and with it the size of the bridge proofs `aver cert check` builds, changes.

Two things stay outside the bridge. The model definition `<Module>.<fn>` is emitted by the compiler with the certificate, so a bridge is proved relative to that definition. And the producer chooses the encoders, which are part of the statement. An export without a credited bridge keeps the weaker position: that its plan is your function rests on the compiler that printed it. `explain` lists why each such export got no bridge, from the package's `sourceBridgesDeclined` list.

## Trust and explicit limits

A verdict trusts the small `aver-cert` Rust path, the one retained `wasmparser::Validator` check, the embedded Lean wall, the Lean 4.34 toolchain, SHA-256, and the named runtime contracts. It does not run the producer, the plan printer or any Rust reconstruction of the claim.

The runtime contracts say what the Int, String and index helpers compute. The certificate pins each helper's body to a template, but it does not prove the contracts. The bignum sub-routines those helpers call are not pinned at all, and the theorem treats every helper as a pure function that changes nothing the caller can reach.

L3 "returns" is about the wall's interpreter, where fuel counts nested calls. A real engine can still run out of stack or memory on large inputs.

After the build, a checker-authored audit program loads the built proof. It is compiled with no certificate module in scope. It refuses a package that extends the parser, declares an instance outside a small admitted set, or gives a bridge an encoder that does not list a whole record or sum. It also collects the axioms of the accepted root and of every pin.

`leanchecker --fresh` replays the proof in a fresh environment, but it ships with the same Lean toolchain. The scheme does not yet have a second, independently written kernel.

The canonical Elan installation is a local trust anchor. Every Lake, Lean and leanchecker subprocess otherwise starts with a cleared environment, the exact pinned toolchain, no implicit Lake caches and checker-owned temporary paths.

The [Certificate Format Specification](certificate-format.md), section 12, lists every trusted item.
