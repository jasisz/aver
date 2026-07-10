# Artifact Behavioral Certificates (`aver compile --certify`)

> Status: v0, certification level **L1** (conditional on named runtime
> contracts). The current goal matrix certifies 23 exports across 14 manifest
> classes: integer recursion/composition families, ADT/variant/verbatim
> families, scalar expression fragments, mutual recursion, and the
> `String.eq`/`String.concat` beachheads. `String.eq` and `String.concat` now
> emit source-level `sym-fragment-v1.plan` witnesses plus target-bound
> `string-eq-v1.plan` / `string-concat-v1.plan` witnesses that `aver cert
> verify` checks against byte-derived helper certificates and carries through
> Lean-side `AcceptedArtifact.StringEqClaim` /
> `AcceptedArtifact.StringConcatClaim`. Everything
> else is **declined fail-closed** and listed with a reason. This document is
> the contract; the emitted `cert-manifest.json` is its machine-readable form.

> Architecture direction: new certificate work should follow
> [plan-first canonical lowering](certification-architecture.md). Expression
> fragments no longer emit or accept trace sidecars; their plan sidecars are
> untrusted witnesses checked by verifier-owned canonical lowering, then merged
> with non-expression obligations by the byte-derived function order. The
> compiler now emits source-projectable Float/Bool and Int literal-predicate
> expression islands from
> `MIR -> SymPlan -> ExprFragmentPlan encoding -> canonical Wasm`; fragments
> that still only have representation meaning remain on an explicit
> `MIR -> ExprFragmentPlan -> canonical Wasm` fallback. New expression
> certificates should extend the source-level `SymPlan` path whenever the
> operation has Aver-level meaning, and use the representation fallback only for
> byte-layout facts that do not yet have a source constructor. `String.eq` and
> `String.concat` are the first non-expression source witnesses: their sidecars
> record source-level string operations and are checked by Rust against the
> byte-derived helper certs, then the artifact-level Lean bridge looks up the
> byte-bound target plans from `manifest.stringEqPlans` /
> `manifest.stringConcatPlans`. The same checked
> expression plans are also emitted as `Plans.lean` Lean data. The
> audited, hash-pinned `PlanCheck.lean` module structurally checks those raw
> plans in Lean (`checkExprFragmentRawPlan = true`), and `PlanLower.lean`
> canonically lowers accepted raw plans to the same `WInstr` body emitted in
> `Module.lean`. `PlanBytes.lean` also canonically lowers accepted raw plans to
> the exact code-entry byte sequence for the current certified island.
> `WasmSlice.lean` parses the checker-read module bytes just far enough to bind
> an export name to a `FuncBinding`: wasm function index, defined-code index,
> function-section type index, and code-entry bytes. `ExprFragmentAccepted.lean`
> aggregates those facts into one accepted-export predicate. `AcceptedArtifact.lean`
> exposes the v2-shaped bridge over raw artifact bytes, a source/raw plan, and
> a `Schema.Obligation`. Source-projectable fragments now enter artifact
> acceptance as `SymFragmentClaim`; Lean checks/encodes their `SymRawPlan` into
> the byte-bound `ExprFragmentRawPlan` before applying the accepted-export
> predicate. Their manifest sidecar is also source-first
> (`sym-fragment-v1.plan`); representation-only expr fragments are not admitted
> by the artifact-level bridge until the source grammar grows explicit
> constructors for them. The body, code-entry bytes, and function binding are
> internal witnesses to that bridge rather than separate authority parameters.
> The generated cert now also carries `Artifact.lean`: an untrusted artifact
> root defining `AverCert.Artifact.data`, the parameterized
> `acceptedWithFinal` bridge, and the self-checking
> `AverCert.Artifact.certificate : AcceptedArtifact.accepted data`. The checker
> pins that `data` term to its own reconstruction with `rfl`, type-ascribes
> `Final.cert` to `Schema.Holds manifest`, and roots the axiom audit at
> `Artifact.certificate`. `AcceptedArtifact.accepted` itself requires the
> manifest subject to name that same artifact root, and it rejects fragment
> claims whose obligation export is not present in `manifest.obligations`. The
> artifact therefore carries the v2-shaped bridge proof without letting it
> choose the final theorem target.
> The verifier-authored witness pins `manifest.symFragmentPlans` and
> `manifest.exprFragmentPlans` to checker-rendered terms, checks that each
> source plan encodes to its byte-bound representation plan, and re-checks the
> plan lowerings plus the relevant byte-origin slice. This is the v2 landing
> zone for moving the remaining module validation/raw-byte binding fully into
> `AverCert`.

An **Artifact Behavioral Certificate** (ABC) is a proof of *what the compiled binary does* that travels with the binary: not a signature over who built it, nor a hash of its bytes, but a machine-checkable statement about its **behavior** — pinned to those exact bytes and verifiable without trusting the compiler.

```
aver compile app.av --target wasm-gc --certify -o out/
```

emits the module (`app.wasm`) and, next to it, `cert/` — a self-contained Lean project whose theorems are about **those exact bytes**: the certificate pins `sha256(app.wasm)`, embeds the certified function bodies as data read back from the module, and proves that running those bodies under the semantics of the emitted fragment computes the function's Lean model. Behavioral laws proven about the model then transfer to statements about the bytes — the behavioral half of the ABC, and the part no signature or reproducible-build attestation can give you.

The consumer story is deliberately narrow: **checking a certificate means running the Lean kernel, not trusting Aver.** `aver cert verify` is a convenience orchestrator; the trust anchor is a small, audited, hash-pinned set of Lean files plus the kernel itself.

## Why this is possible: the certified fragment is small by construction

Certified user functions compile into a measured fragment of wasm-gc — 39 distinct opcodes:

| group | instructions |
|---|---|
| locals | `local.get`, `local.set` |
| constants | `i64.const`, `i32.const`, `f64.const` |
| references | `ref.null`, `ref.is_null`, `ref.test`, `ref.cast` |
| heap values | `struct.new`, `struct.get`, `array.new_fixed`, `array.new_data` |
| i64 tests | `i64.eqz`, `i64.eq`, `i64.le_s`, `i64.lt_s`, `i64.ge_s`, `i64.gt_s` |
| i32 | `i32.eq`, `i32.and`, `i32.lt_s`, `i32.le_s`, `i32.gt_s` |
| f64 | `f64.add`, `f64.sub`, `f64.mul`, `f64.div`, `f64.eq`, `f64.lt`, `f64.le`, `f64.ge`, `f64.gt` |
| control | `if`/`else`/`end` (folded into one structured conditional), `call`, `return_call`, `return` |

No `loop`. No `br`/`br_if`. No linear-memory instructions. No `call_indirect`. Not "rarely" — **never**, in any user function. This is not an accident of the current emitter; it is forced by source-language decisions:

- Aver has no unstructured control flow. Iteration is recursion, and recursion lowers to `call`/`return_call` (tail calls) — so user code contains no loops to model.
- Collections, strings and big integers are runtime types with one fixed implementation. Their loops live once, in the runtime, and are never inlined into user functions; user code reaches them through calls.
- Effects are declared in signatures and lower to imports. There is no ambient I/O for the compiler to scatter through a body.
- Every value is one of a few wasm-gc shapes (an integer carrier struct, arrays for strings, structs for user ADTs), so the value layer of the semantics is a short list of cases.

The consequence is the load-bearing fact of the whole feature: the operational semantics of everything a certified function can do fits in **one auditable Lean file** ([`tools/certkit/prelude/CertPrelude.lean`](../tools/certkit/prelude/CertPrelude.lean), about 400 lines) — small enough to read in a sitting, which is the audit budget an independent reviewer actually has.

Runtime helpers a body calls (integer boxing, bignum arithmetic, `String.eq` over the `WVal` byte-array representation, …) are **not interpreted** at L1. They enter the theorems as named contracts — explicit hypotheses listed in the manifest, re-derived from the wasm bytes by `aver cert verify`, and priced into the certification level. The fragment semantics itself is validated by a three-way differential harness (bytecode VM ↔ wasm engine ↔ the Lean interpreter, [`tools/certkit/`](../tools/certkit/)) with a fail-closed opcode coverage gate; it is tested against reality, not proven against the W3C spec, and it is published precisely so it can be audited.

## What the theorems say

Per certified export, the certificate proves, from the schema's fixed statement shapes:

- **Simulation**: for every input tuple in the class, running the body read back from the module bytes under the fragment semantics — with the named runtime contracts as hypotheses — returns exactly the encoding of the function's Lean model applied to those inputs. This is partial correctness: the theorem is conditional on the run returning a value, so a trap or fuel exhaustion makes no claim — the executable anti-vacuity guards below are what witness that certified bodies actually run. Recursive bodies, including two-argument accumulator recursion, are handled by a fuel-indexed interpreter plus a fuel-stability companion theorem.
- **Law transfer**: laws proven about the model compose with simulation into statements about the bytes themselves.

All of it is bundled behind **one final theorem** per certificate, `AverCert.Final.cert : Schema.Holds manifest`. The fixed, audited `SchemaCore.lean` contains the dependency-closed statement schema, while the thin `Schema.lean` shim adds the artifact-specific module-hash conjunct; `manifest` is a literal describing this artifact (hash, exports, contracts, profile, ABI, and artifact-level certificate root). The schema states each obligation over a typed source **domain** and **codomain** with explicit representation relations (`Dom`, `Cod`, `domRepr`, `codRepr`, `model`), so the same schema covers integer, projection and ADT classes. Certificates are bound to their schema version (`schema_version` in the manifest, currently 35): a checker refuses a certificate from a different schema generation with an explicit unsupported-version message rather than a misleading downstream error, so regenerate certificates with the toolchain that will verify them. A consumer never reads proof scripts; they check the final theorem's name and type, the artifact certificate root, the manifest literal, and the schema-core/schema/prelude/plan-check/plan-lower/plan-bytes/wasm-slice/expr-fragment-accepted/accepted-artifact-core/accepted-artifact hashes. If statement approval required inspecting arbitrary Lean syntax, the compiler bug would just move into a certificate-auditor bug — the schema exists so that it can't.

Anti-vacuity is enforced separately: executable guard `example`s must compute each certified function on at least one concrete input (these use `native_decide` and are deliberately **outside** the proof credit). The final theorems' axioms are collected by the kernel and must be exactly the whitelist `[propext, Classical.choice, Quot.sound]` — a smuggled `axiom`, `sorryAx` (an admitted goal) or `ofReduceBool` (native-code trust) fails verification.

## Trust model

`aver cert verify app.wasm out/cert` (exit 0 = certified, anything else = 1):

1. hashes the artifact and declines on mismatch with the pinned value;
2. assembles a **checker-owned build**: the audited `SchemaCore.lean` / `Schema.lean` / `PlanCheck.lean` / `PlanLower.lean` / `PlanBytes.lean` / `WasmSlice.lean` / `ExprFragmentAccepted.lean` / `AcceptedArtifactCore.lean` / `AcceptedArtifact.lean` / `CertPrelude.lean` come from the checker binary (never from the cert), `ArtifactBytes.lean` is regenerated from the actual artifact bytes the checker read, and the cert contributes data files only — each name-gated and scanned for elaboration-executing tokens; the cert's own lakefile and caches are never read;
3. builds from a clean cache under the pinned toolchain;
4. authors a **kernel witness** binding, via `rfl`: the computed artifact hash, the obligation count, the export names re-derived from the module's export section, and each obligation's code/body, host table, function index and carrier type **re-derived from the hash-verified bytes** — so the data the theorems reason about is forced to equal what the bytes actually contain. The witness also pins the obligation's **typed face** to the standard form of its byte-derived class: a `Nonempty Dom` proof for every obligation (so a `Dom := Empty` vacuity is rejected), and per class the codomain representation (`intRepr`/`verbatimRepr`), the integer domain representation with the byte-bound arity, and the projection domain representation — each by `HEq.rfl`. A manifest that weakens `Dom`/`Cod`/`domRepr`/`codRepr` fails one of these checks;
5. pins the cert-supplied `AverCert.Artifact.data` to the checker-reconstructed artifact data, type-ascribes the final theorem to `Schema.Holds manifest`, aliases the artifact-carried `Artifact.certificate` root, and runs the kernel's axiom collector against that artifact-level root.

The verdict is the witness process exit code. No byte of build output is ever parsed into the verdict or the CERTIFIED report.

What a consumer trusts, exhaustively: the Lean kernel and pinned toolchain; `SchemaCore.lean` + `Schema.lean` + `PlanCheck.lean` + `PlanLower.lean` + `PlanBytes.lean` + `WasmSlice.lean` + `ExprFragmentAccepted.lean` + `AcceptedArtifactCore.lean` + `AcceptedArtifact.lean` + `CertPrelude.lean` (hash-pinned in the manifest); the checker orchestration above — including, today, the audited disassembler and canonical code-entry equality for non-expression obligations (Rust components of the consumer binary, trusted by inspection). `PlanBytes.lean` mirrors the expr-fragment canonical byte encoder in Lean, `WasmSlice.lean` checks that the checker-read module bytes expose each expr-fragment export's exact `FuncBinding` (function index, code index, type index and code-entry bytes), `ExprFragmentAccepted.lean` packages those checks into one accepted-export predicate, and `AcceptedArtifactCore.lean` exposes the dependency-closed artifact-level bridge machinery; the thin `AcceptedArtifact.lean` shim adds its single `Schema.Holds` conjunction. Source-level `SymFragmentClaim`s must encode to byte-bound representation plans inside Lean; representation-only expr fragments are not admitted by that artifact-level bridge yet. The generated `Artifact.lean` is not trusted as a definition source; it is an artifact-carried witness whose `data` is pinned by the checker before its `certificate` root is audited. `certificate` is the artifact-carried wrapper around `acceptedWithFinal AverCert.Final.cert`; the checker separately type-ascribes `Final.cert` before using that root. Before any byte-derived fact is read — on both the producer and the verifier side — the module is passed through a full `wasmparser` validation (GC / tail-call / function-reference proposals enabled), so every subsequent rederivation is over a WELL-TYPED module: a forged result type, a nullability-mismatched signature, and malformed or trailing section bytes are all rejected up front rather than slipping past the relevant-subset slicer. Rust still does the executable sidecar check and full module reading, and `WasmSlice.lean` is a relevant-subset slicer rather than a full Wasm validator (though it now parses each type section and passive data section in full and requires exact payload exhaustion, so a valid entry followed by trailing bytes is rejected in-kernel too). Replacing that remaining byte-origin step with **in-kernel module validation and raw-byte lowering checks** — so the bytes-to-body step is itself a kernel computation and a hostile consumer can re-implement the entire check against the kernel alone — is measured and in progress. The Lean **model** of each function is data in the certificate: reading the model, or the named laws proven about it, is how a consumer knows *what* was certified; the schema keeps that reading surface small and fixed. The kernel witness pins every other obligation field to the bytes; the model itself is not byte-derivable — it is forced by the kernel proof instead (a tampered model makes the certificate fail to check), and its meaning is read, not re-derived.

**Read surface (what is declared vs. pinned).** As of this branch the checker pins the typed face of each obligation to the standard form of its byte-derived class (see trust-model step 4). Two items remain **read declarations**, not kernel-re-derived: the **model** of every class, and the **`domRepr` of the ADT classes** — the `variant match` domain (both the three-branch and the widened Int match) and the `constructor` domain/codomain are stated over a user-inductive `Repr` the checker cannot reconstruct from bytes alone. For those the checker still proves `Nonempty Dom` and forces the constructor's runtime behaviour with an executable interpreter tripwire, but the domain/codomain *representation* of a user type is read from the certificate, exactly like the model. Closing this residue (a per-build mechanical disjunction of the variants, and an in-kernel decoder for the model) is future work.

Source-level **type names** in plan claims (the `named:` types of `sym-fragment-v1` sidecars, e.g. the record name a field projection reads from) are the same kind of read declaration. The kernel-checked content of a projection claim is the **byte-derived struct identity** — the wasm struct type index and field index, validated against the module's own type section and pinned by canonical code-entry byte equality. The names are producer-asserted annotations: they are forced to be **consistent** — within a plan (every used name must be anchored by a projection, and a projection's claimed owner must be the declared type of the value it projects) and across the artifact's files (the sidecar, the Lean plan data and the artifact claims must all carry the same names, or the checker's kernel pins fail) — but they are not byte-derivable, exactly like the model. A producer that coherently relabels every surface at once has changed what the certificate *says*, not what it *proves*, the same way it could ship a different model. Binding claim names to true source provenance is planned future work.

## Certification levels

| level | meaning |
|---|---|
| L0 | source-level verification only; no artifact claim |
| **L1 (current)** | artifact claim, conditional on the named runtime contracts in the manifest |
| L2 | runtime contracts themselves proven against the shipped helper bodies |
| L3 | additionally totality/resource bounds ("bounded total correctness on valid inputs") |

The manifest always declares its level. A certificate never silently claims more than its level supports.

## Honest limits (v0)

The header's *23 exports across 14 manifest classes* and this section's *nine certified classes* count the same certified functions at two different granularities, not two conflicting totals. The nine-class taxonomy below is the coarse byte-recognizer grouping — the source shapes the classifier recognizes. The emitted `cert-manifest.json` enumerates that same surface more finely as 23 exports across 14 manifest classes, and tags each export with the acceptance path it rides. Eighteen of the 23 exports ride the plan-first `accepted-artifact-v1` path — the scalar `expr-fragment-v1` fragments plus the `adt-constructor`, `verbatim-string-eq`, `verbatim-string-concat`, fuel-recursion (`recursion-plan-v1`), mutual-recursion (`mutual-plan-v1`) and verbatim ADT-match (`verbatim-plan-v1`) bridge families; the remaining five ride the legacy `legacy-witness-v1` byte-recognizer path. The fuel-recursion bridge is a byte-origin veneer: the fuel-induction proof face and the emitted body literal are unchanged, but the manifest carries a checked `recursion-plan-v1` plan that the audited Lean checker validates context-sensitively — the plan must be one of the two recognised fuel-recursion grammars, every self-call must target the export's own byte-derived function index, every host call must cite the byte-derived box/combinator/sub role table, and the export's declared function type must be the canonical carrier signature — before the audited Lean lowerers reproduce it byte-for-byte and the Lean byte slicer finds those exact bytes inside the module. The recursive body's byte-origin is thereby established in-kernel instead of only by the Rust disassembler; a certified recursion body the canonical plan cannot reproduce byte-exactly (for example one carrying normalized local-alias hops) simply keeps the legacy witness route with no claim, fail-closed. The verbatim ADT-match bridge (`verbatim-plan-v1`) is the same kind of veneer for the `Cod := WVal`/`verbatimRepr` widened-match and variant-dispatch classes: because those bodies spill the multi-use scrutinee to a scratch local (which the pure-ANF expr-fragment grammar cannot express) the manifest carries a dedicated `VerbatimRawPlan` — a right-nested `ref.test` cascade of per-tag leaves (field projection, `array.new_data` string literal, `ref.null`, or `f64.const`). Because there is no host or self call to bind, the byte-equality gate (the audited Lean lowerer reproduces the exact code entry and the slicer finds it by export name) carries most of the binding — but the code entry alone does not determine the export's meaning, so two further in-kernel binds close the gaps the bytes leave open: the export's declared function type must be exactly the unary `[eqref] -> [(ref null resultHeapTy)]` verbatim signature (a second parameter leaves the locals and body bytes identical, so without this a binary export could be certified for a unary obligation), and every `array.new_data` leaf's claimed payload must equal the byte-pinned contents of its passive data segment (the code entry encodes only the segment index and copied length, so an equal-length payload substitution otherwise keeps identical bytes). With those, no two distinct plans lower to the same accepted artifact; a body byte-noisier than the canonical dispatch keeps the legacy route with no claim, fail-closed. The byte-recognizer families and the manifest classes therefore describe the same work, and `artifact_bridge_counts` in the manifest reports the 18/5 split directly.

- **Nine certified classes (byte-recognizer taxonomy).** Three integer: straight-line integer functions of the add-a-constant shape, single-argument structural self-recursion `f n = if n≤0 then base else <combine>` — one fuel-induction arm recognised structurally from the decoded instruction tree, where the base literal, the combinator's non-recursive operand (the input `n`, or a boxed constant) and its operand order (`n + f(n-1)`, `2 + f(n-1)`, `f(n-1) + n`) are all read from the bytes as data, and the combinator operation is `+` or `*` read from the model (the bignum `add` and `mul` helpers are not byte-distinguishable, so `n * f(n-1)` — factorial — certifies via the `mul` contract); only the descent `n-1` is pinned (`sumTo`'s `n + f(n-1)` is one instance, not a separate class). Two-argument accumulator self-recursion (`f(n, acc)`: the first integer parameter descends, the second is an accumulator updated in the straight-line fragment) is the same arm's tail-recursive shape, and an **Int -> Bool range predicate** — two nested comparisons against constants, `match cp >= k_lo { true -> cp <= k_hi; false -> false }` (the surrogate-range checks), certified over the canonical small-carrier domain where the bignum comparison arms are dead. Three ADT: a non-recursive **constructor** (one- or two-field), a non-recursive **field projection** (both a bare record access `u.field` and a two-component destructuring match `match p { (a, _) -> a }`, which lowers with a bind-and-cast preamble; the projected field must be 0 or 1 and is returned verbatim), and a non-recursive **ADT variant match** — one leaf-polymorphic `cases`-over-the-inductive arm certifying a single-argument match over a user inductive, its proof generated by walking the decoded `ref.test` branch tree, one case per constructor, closing each leaf with its own honest closer and one executable guard per constructor path. Each constructor's leaf is one of a small vocabulary: return the integer payload, combine the payload with an integer constant through the contracted runtime add/sub helper (constant on either side), box a default constant, or — for a variant the match does not model — treat it **opaquely**. The domain representation is chosen per function so no variant is forced into a representation it cannot inhabit: when every constructor carries a single integer payload the arm uses the faithful shared `<Ty>Repr`; when the body projects one integer variant of a *heterogeneous* inductive and boxes the default for every other (`match j { JsonInt(n) -> n; _ -> 0 }`, the other variants carrying strings, floats or nested lists), a per-function `<name>DomRepr` pins only the projected variant's byte-derived struct index and keeps the rest opaque, precisely because they cannot be represented as integer carriers. Same `cases` spine, honest Repr per leaf — the faithful all-integer dispatch (the historical frozen three-branch `Add x -> x; Neg x -> 0 - x; Zero -> 0` is one instance of it) and the one-hit widened projection are two ends of a single arm, not separate classes. The codomain is always pinned to `Int` (`intRepr`). Arms outside the leaf vocabulary (nested matches, payload-less constant arms, multiple helpers per contract role, a non-constant final arm) decline the whole function fail-closed. A fourth ADT class, the **verbatim widened match**, returns a raw `WVal` (`Cod := WVal`, `verbatimRepr`) rather than an integer — one `cases`-over-`WVal` arm whose per-tag leaf is chosen honestly, spanning a spectrum with two ends. At one end it projects a single variant's field verbatim with a byte-derived default for the rest — `match j { JsonList(items) -> items; _ -> [] }` (the empty list lowers to `ref.null`). At the other end *every* variant maps to a distinct byte-exact constant — most usefully a String literal, which lowers to an `array.new_data` over a data segment (`match code { EscapeCode.Quote -> "\""; EscapeCode.Newline -> "\n"; ... }`, an escape-code-to-string table). Either way the model is a `WVal -> WVal` lookup emitted from the recognised per-tag projections and data-segment constants, so it makes no claim about the value's meaning — only that each variant maps to that exact byte array — and needs no carrier or string representation; the domain is all of `WVal`, reusing the byte-exact array machinery with no new representation. (Because the interpreter's `ref.test` matches a type index for both the struct and array carriers and a constant arm never traps, the model dispatches on that index for both, keeping the all-inputs theorem honest — real inputs are always the enum's structs.) The same class also covers the first String beachhead: a one-literal string match such as `match c { "\"" -> "\\\""; _ -> c }` is certified by calling a byte-derived `String.eq` host slot and returning either a byte-exact literal or the input verbatim. The user body remains in the loop-free fragment; the compiler-generated `String.eq` helper is recognized only as a named L1 runtime contract, not interpreted in this proof, and the classifier declines if the helper shape changes. And **cross-function composition**: a unary integer caller whose body is a chain of calls into other user functions, certified over one shared code table that carries the whole (acyclic) call closure re-derived from the bytes — the caller's proof cites its callees' simulation lemmas, so mutating a callee's body fails the caller's certificate. Its v1 leaf dictionary is deliberately small (self-sum leaves and nested chains); callers reaching any other callee shape are declined by construction, not by a dead template. And **mutual recursion**: a strongly-connected cycle of unary integer functions where every member has the shape `f n = if n≤0 then base else g(n-1)`, tail-calls the next member (`return_call`), and the cross-call chain closes back to the start (`isEven`/`isOdd`, or any cycle length). The whole SCC shares ONE simulation proof — a single `induction fuel` over the *conjunction* of all members, where each cross-call at the decremented fuel is discharged by the matching conjunct of the induction hypothesis — on top of a conjunction fuel-irrelevance bridge and per-member base/step lemmas. The members share one code table and one `box`+`sub` host, both re-derived from the bytes (the primary, lowest-index member emits them once); each member is its own certified export whose obligation cites its conjunct, so mutating any member's body fails the shared proof. Only the descent `n-1` is pinned; the base literal and the cross-call target are read from the bytes as data. Everything else — including multi-argument recursion beyond the two-argument accumulator shape, cross-recursion whose members are not the fixed `if n≤0 then base else g(n-1)` shape, and any match outside the recognised dispatch grammar — is declined and listed under `source_level_only` with a reason. The classifier is fail-closed: a function whose emitted body does not match a certified template is declined even if it looks like it should fit.
- **Float claims are bit-level expression claims only.** Current float fragments certify Wasm `f64` operations over their raw `UInt64` bit representation through the measured `WInstr` semantics; they do not prove real-number algebra laws. Wasm NaN behavior remains a reason to keep float certificates narrow and semantics-explicit.
- **Runtime contracts are assumptions** at L1. They are few, named, byte-derived at verification time, and stable; the plan of record is to discharge them per release (L2), starting with the small integer helpers and then string helpers such as `String.eq`.
- The elaboration-executes-code token scan on cert data files is a raised bar, not a proof; the structural fix is the in-kernel decode path.
- `--certify` conflicts with `--optimize`: the certificate binds the exact emitted bytes.

## Usage

```
aver compile app.av --target wasm-gc --certify -o out/
aver cert verify  out/app.wasm out/cert    # full check; exit 0 iff CERTIFIED
aver cert explain out/app.wasm out/cert    # same full check + human-readable report
```

`explain` prints the kernel-confirmed CERTIFIED exports with their policies and runtime contracts, then the DECLINED list with reasons (the DECLINED side is informational and carries no claim). A certificate with zero certified exports reports `NO CERTIFIED EXPORTS` and exits 1 — a trust tool does not exit green for a certificate that claims nothing.

## Prior art, briefly

Proof-carrying code (Necula & Lee, 1997) proposed shipping proofs with binaries; it never became a product. Proof-producing compilation with mechanized target semantics exists — closest is the Lean-based verification pipeline for StarkWare's Cairo (arXiv:2501.15002). What this feature adds: the certificate is a **per-build artifact checked without the producer's toolchain**, the certified fragment is small because the source language was designed that way, the classifier is fail-closed rather than best-effort, and behavioral laws travel with the binary.
