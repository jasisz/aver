# Certification Architecture

The verifier decides one thing: whether an untrusted certificate package proves the fixed Aver statement about the exact WebAssembly bytes it was given. The package supplies data and proofs. It cannot choose the checker, the theorem shape, or the facts read from the artifact.

This document describes how the verifier reaches its verdict and where the trust boundary sits. See [certification.md](certification.md) for the user guide, and [certificate-format.md](certificate-format.md) for the normative format, the trust inventory and the versioning policy.

## Meaning to bytes

The certificate carries meaning, and the wall produces bytes from it. Each certified function has a plan: its optimized MIR body (`src/ir/mir/expr.rs`, `MirExpr`), printed 1:1 into the Lean grammar of `Grammar.lean`. The wall lowers the plan with `GrammarLower`, requires the resulting code entry to equal the function's code entry in the artifact, and proves in `GrammarSound` that the lowered instructions compute the plan's meaning in the wall's wasm interpreter. Nothing decodes bytes into meaning.

This works because there is one emitter. Every function the compiler emits goes through the MIR body emitter (decision `MirEmitterIsTheOnlyEmitter`), and `GrammarLower` is a port of that emitter for the admitted nodes. It makes the same choices from the same tree by the same predicates: whether a comparison operand is re-emitted or stashed, which scratch local a match uses, the block type of an `if`, tail call or call. No plan field selects a lowering. If the emitter changes and the wall does not, the code entries stop matching and certificates decline.

## Invariants

1. The `.wasm` file passed to `aver-cert` is the artifact identity.
2. The plans and the declared type layout in the Lean manifest are the only plan data. The JSON manifest carries no plan.
3. The wall, the Lean toolchain, the build files, `ArtifactBytes.lean` and the checker witness belong to the checker.
4. A positive verdict comes from the named Lean root, not from a Rust reconstruction of what the producer did.
5. Every mismatch declines. A diagnostic cannot turn a failed proof into acceptance.

## Components

`aver-cert` is a standalone crate and executable on its own `0.1.x` release line. It does not depend on `aver-lang`, `aver-rt` or `aver-memory`.

Its default `verify` feature contains the checker and the embedded wall. The `plans` feature holds only the plan data types the compiler prints into. The `engine` feature (also named `producer`) adds the certificate producer that `aver compile --certify` uses. The verifier does not link or run the producer on its positive path.

`aver cert ...` finds a sibling `aver-cert` executable or one on `PATH` and runs it with unchanged arguments, standard streams and exit status.

## Producer

The producer runs inside `aver compile --certify` and is not trusted.

1. While emitting the module, the compiler prints each function's MIR body as a plan (`src/codegen/cert/plan_from_mir.rs`). The printer is mechanical. It declines a whole function with the name of the first MIR node, pattern or type outside the grammar, and it records the emitter's type indices in one module-wide type table.
2. `aver-cert/src/engine/produce.rs` offers a function only when its plan types, lowers to exactly its code entry, has exactly its declared function type, cites only confirmed layout, and calls only offered functions. The checks are Rust twins of the wall functions (`plan_check.rs`). They exist so that one bad function is declined with a reason instead of failing the whole package in Lean; the wall re-checks everything.
3. Offered functions are grouped into call SCCs, callees first. Exported functions become obligations; internal callees are planned and bound by function index.
4. `render_package.rs` writes the package. When the package declares source-bridges or law-claims, it also ships the Lean source model and the bridge proofs (`source_bridges.rs`, `law_claims.rs`).

Before it ships a model file, the producer runs the checker's own file-name, case-collision and token rules on it (`aver-cert/src/lean_gate.rs`). The two sides share one implementation, so the producer drops or declines exactly what the checker would refuse.

## Ownership of inputs

| Input | Owner | Treatment |
|---|---|---|
| WebAssembly module | Caller | Validated, hashed, and rendered into checker-generated `ArtifactBytes.lean` |
| `cert-manifest.json` | Certificate | Untrusted transport and report data, pinned against the Lean manifest |
| `Plans.lean`, `Manifest.lean` | Certificate | Untrusted data; the wall types, lowers and byte-checks every plan |
| Proof modules and the source model | Certificate | Untrusted; admitted after the staging gates and kernel checking |
| Lean wall | Verifier | Embedded, selected by exact `wall_id`, written by the checker |
| Lean toolchain and build files | Verifier | Pinned to Lean 4.34 and written by the checker |
| `CheckerWitness.lean` | Verifier | Written for this artifact on every run; never read from the package |
| `CheckerAudit.lean` | Verifier | Written for this artifact on every run; compiled without the package; never read from the package |

## Acceptance flow

```text
actual app.wasm
  -> wasmparser Validator
  -> checker-generated ArtifactBytes.lean
  -> CertDecode / WasmSlice ------------------------------+
                                                          |
package Plans.lean (plans, type table)                    |
  -> Grammar.tyOf (typing at the declared signature)      |
  -> GrammarLower (code entry) ----- equal to bytes ------+-> plansAccepted
  -> TypeTable (layout, S3Pin, data segments) ------------+
package Manifest.lean (subject, obligations)              |
  -> obligationsOf (obligations derived from the plans) --+-> obligationsDerived
  -> ClaimAxes (runtime contracts, report facets) --------+
                                                          v
          AcceptanceSoundness.accept_sound  ->  Artifact.certificate
                                                          |
          checker witness: pins, law and bridge pins -----+
                                                          v
                                  AverCertChecker.checked
                                                          |
      audit program (declarations, axioms) + leanchecker --fresh
                                                          v
                                                     CERTIFIED
```

The steps:

1. Read the module, run `wasmparser::Validator`, compute its SHA-256, and parse `cert-manifest.json`.
2. Require package format `1`, statement schema `9`, the expected root, target, profile and ABI, the actual hash, and a `wall_id` embedded in this verifier.
3. Stage a fresh build directory from the embedded wall and the package's Lean files, after the file-name, shadowing and token gates. Package files with checker-owned names are ignored.
4. Generate `ArtifactBytes.lean` from the bytes read in step 1. The package cannot supply another numeral.
5. Build under Lean 4.34. The package's `Artifact*.lean` modules prove the byte facts of `AcceptedArtifact.accepted` by `decide +kernel` against the staged bytes: every plan's lowering is its function's code entry, the type table matches the type section, the helper bodies match their templates, and the whole module is accounted for.
6. The package's `Final.lean` proves `Schema.Holds` from those facts with `AcceptanceSoundness.accept_sound`. That theorem applies `GrammarSound.fn_certified_group` to all plans at once, and `GrammarTotal.fn_certified_total_of_check` to each call group the totality check admits.
7. Write `CheckerWitness.lean` and elaborate it. It pins the Lean data to the JSON envelope and to the staged bytes by `rfl`, pins `ClaimAxes.reportEntries` and `ClaimAxes.reportFacets` as theorems, requires `AverCert.Artifact.certificate` at exactly the accepted type, and pins every declared law-claim and source-bridge at a statement the checker controls. The witness imports no `Lean`, runs no code, and names everything `_root_`-qualified with `nat_lit` numerals, so neither a package namespace nor a package instance changes what a pin says. A pin that does not elaborate declines the package.
8. Run the checker's audit program (`lake env lean --run CheckerAudit.lean`). It is compiled with only the Lean toolchain in scope and loads the built witness environment when it runs. It declines a package that declares under the reserved `AverCertChecker` prefix, extends the parser, declares a scoped instance or an instance outside the admitted forms (the class is read off the elaborated declaration), or whose bridge encoders do not list a record's fields or a sum's constructors exactly. It rejects any axiom outside `propext`, `Classical.choice` and `Quot.sound` under the accepted root or a report pin, and logs one audit line for every law, bridged-law and bridge pin; a pin whose axioms leave the whitelist loses only its own credit.
9. Replay `CheckerWitness` and its whole import closure (wall, artifact certificate, model, laws and bridges) with `lake env leanchecker --fresh CheckerWitness`. Only then is the report printed.

`aver cert check` runs steps 1 to 8 and skips step 9. It trusts the locally built or cached `.olean` files and prints `CHECKED`, never `CERTIFIED`. The witness and the audit program are still written and run on every run, so the pins, the declaration audit and the axiom audit always run.

Build caches are off by default. A configured data or prelude cache never replaces the witness, the audit program or the final replay, but its directory is trusted local state: its integrity manifest detects accidental corruption, not a writer who replaces the `.olean` files, the Lake traces and the manifest together. Caches shipped inside a package are ignored.

## Why one Rust Wasm validator remains

The wall decodes every byte fact an admitted claim uses, but its decoder does not do full stack and control typing. `wasmparser::Validator` therefore stays as one gate before Lean. Removing it would drop the guarantee that the artifact is a valid WebAssembly module.

No other producer analysis runs on the positive path. The verifier does not print plans, disassemble the module, or rebuild `AverCert.Artifact.data`.

For wasip2 the same rule covers the component wrapper. The manifest declares the component as `prefix ++ core ++ suffix` by length (`wasip2ComponentEnvelope`). The verifier splits only at those lengths, stages the whole component as checker-owned `ArtifactComponentBytes.lean`, and the wall's `artifactEnvelopeAccepted` checks that the split core equals the `ArtifactBytes.lean` module the decoders read. The verifier never walks the component to find the core.

## The Lean wall

The wall is one hash-addressed unit of 22 Lean files. By module:

- `CertPrelude`, `InterpreterSequencing`: the wasm instruction model and its interpreter;
- `CertDecode`, `WasmSlice`, `Wasip2Envelope`: decoders over the actual module and component bytes;
- `SchemaBase`, `SchemaCore`, `Schema`: the statement, meaning `Subject`, `TypeTable`, `FnEntry`, `Obligation` with `holds` and `holdsTotal`, `HostContracts`, `Manifest`, and `Holds`;
- `Grammar`: the plan grammar, its typing `tyOf` and its meaning `eval` and `groupModel`;
- `GrammarLower`: the lowering to instructions and code-entry bytes, and the byte pins `S3Pin` and `DataPin`;
- `GrammarSound`: the simulation theorem (`agreement`, `fn_certified_group`) and the exact `ref.test` argument;
- `GrammarTotal`: the L3 check (`checkTermGroup`) and `fn_certified_total`;
- `TypeTable`: the lowering context from the declarations, and their confirmation against the type and data sections;
- `DeclaredLayout`: the producer-declared module layout (where each function's code entry, type and export entry are), confirmed against the decoders once, with a proof that the checks reading it imply the decoder-based ones, so the declaration saves searching and decoding without changing what is accepted;
- `ArithTemplateDerisk`: the Int helper body templates;
- `AcceptedArtifactCore`, `AcceptedArtifact`: the derived obligations and the acceptance predicate;
- `ClaimAxes`: the required runtime contracts and the report data;
- `AcceptanceSoundnessCore`, `AcceptanceSoundness`: `fn_claim_discharges`, `accept_sound` and `accepted_nonvacuous`;
- `GrammarBridge`: the source-bridge statement kinds and proof engines;
- `ModelPrelude`: the checker-owned pieces of the source model that the token gate refuses in package text.

Changing any wall file or the pinned toolchain changes `wall_id`. The manifest can only name an identity already embedded in the verifier. There is no filesystem, environment or network fallback.

## Trust boundary

A verdict depends on:

- the small Rust verifier path for file reading, hashing, version checks, staging, process execution and report pinning;
- `wasmparser::Validator` for full WebAssembly validity;
- the embedded Lean wall and the Lean 4.34 elaborator, kernel and tools;
- the canonical local Elan home that resolves the pinned toolchain;
- SHA-256 collision resistance;
- the named runtime contracts, and their totality where L3 needs it. They cover code the certificate does not pin (the bignum sub-routines behind add, sub, mul, cmp and divmod), and they model every helper as a pure function of its argument values;
- two facts of the wasm GC specification behind the exact `ref.test` of a variant match, carried as the explicit hypothesis `GrammarSound.GcTestSpec`;
- for L3, the wall's interpreter as the meaning of "returns": fuel counts nested calls only, so stack exhaustion and allocation failure in a real engine are not covered;
- any explicitly configured build-cache directory.

It does not depend on:

- the Aver compiler, its optimizer, the plan printer or the producer;
- the JSON report candidates before Lean pins them;
- wall, build, toolchain or witness files inside the package;
- diagnostic output.

`leanchecker --fresh` keeps the final replay from inheriting declarations from the elaboration environment. It is still part of the same Lean distribution. The design does not yet have two independent kernels.

## Declarations and scope

User types need declarations the binary cannot supply. The type table says which struct index represents which source record, variant constructor, `Option`, `Result`, `List` or `Vector` instantiation. The wall confirms the layout of each entry against the type section, but a type id is still the plans' own name for a type: a wrong id renames a confirmed layout and cannot change it.

A certificate about some exports says nothing about the rest of the module. Imports, the start function and uncertified exports are accounted for; behavioral claims cover only certified exports. Trace and replay recordings are not evidence and never reach the verdict.

## Fail-closed behavior

The verifier rejects invalid Wasm, hash, version or wall mismatches, unsafe package files, refused package text (stage 7 in section 11 of the format reference), package instances or parser extensions outside the admitted forms, ill-typed plans, lowerings that differ from the code entry, a type table that does not match the bytes, obligations that differ from the derived ones, a wrong contract list, Lean build failures, a mismatched root, non-whitelisted axioms and a failed replay.

An artifact with no certified export is reported as having no behavioral certificate and exits nonzero.
