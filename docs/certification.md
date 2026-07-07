# Artifact certificates (`aver compile --certify`)

> Status: v0, certification level **L1** (conditional on named runtime contracts). Ten certified function classes (four integer, five ADT, and cross-function composition); everything else is **declined fail-closed** and listed with a reason. This document is the contract; the emitted `cert-manifest.json` is its machine-readable form.

```
aver compile app.av --target wasm-gc --certify -o out/
```

emits the module (`app.wasm`) and, next to it, `cert/` — a self-contained Lean project whose theorems are about **those exact bytes**: the certificate pins `sha256(app.wasm)`, embeds the certified function bodies as data read back from the module, and proves that running those bodies under the semantics of the emitted fragment computes the function's Lean model. Behavioral laws proven about the model then transfer to statements about the bytes.

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

The consequence is the load-bearing fact of the whole feature: the operational semantics of everything a certified function can do fits in **one auditable Lean file** ([`tools/certkit/prelude/CertPrelude.lean`](../tools/certkit/prelude/CertPrelude.lean), under 400 lines) — small enough to read in a sitting, which is the audit budget an independent reviewer actually has.

Runtime helpers a body calls (integer boxing, bignum arithmetic, …) are **not interpreted**. They enter the theorems as named contracts — explicit hypotheses listed in the manifest and priced into the certification level. The fragment semantics itself is validated by a three-way differential harness (bytecode VM ↔ wasm engine ↔ the Lean interpreter, [`tools/certkit/`](../tools/certkit/)) with a fail-closed opcode coverage gate; it is tested against reality, not proven against the W3C spec, and it is published precisely so it can be audited.

## What the theorems say

Per certified export, the certificate proves, from the schema's fixed statement shapes:

- **Simulation**: for every input tuple in the class, running the body read back from the module bytes under the fragment semantics — with the named runtime contracts as hypotheses — returns exactly the encoding of the function's Lean model applied to those inputs. This is partial correctness: the theorem is conditional on the run returning a value, so a trap or fuel exhaustion makes no claim — the executable anti-vacuity guards below are what witness that certified bodies actually run. Recursive bodies, including two-argument accumulator recursion, are handled by a fuel-indexed interpreter plus a fuel-stability companion theorem.
- **Law transfer**: laws proven about the model compose with simulation into statements about the bytes themselves.

All of it is bundled behind **one final theorem** per certificate, `AverCert.Final.cert : Schema.Holds manifest`, where `Schema.lean` is a fixed, audited statement schema and `manifest` is a literal describing this artifact (hash, exports, contracts, profile, ABI). The schema states each obligation over a typed source **domain** and **codomain** with explicit representation relations (`Dom`, `Cod`, `domRepr`, `codRepr`, `model`), so the same schema covers integer, projection and ADT classes. Certificates are bound to their schema version (`schema_version` in the manifest, currently 4): a checker refuses a certificate from a different schema generation with an explicit unsupported-version message rather than a misleading downstream error, so regenerate certificates with the toolchain that will verify them. A consumer never reads proof scripts; they check the final theorem's name and type, the manifest literal, and the schema/prelude hashes. If statement approval required inspecting arbitrary Lean syntax, the compiler bug would just move into a certificate-auditor bug — the schema exists so that it can't.

Anti-vacuity is enforced separately: executable guard `example`s must compute each certified function on at least one concrete input (these use `native_decide` and are deliberately **outside** the proof credit). The final theorems' axioms are collected by the kernel and must be exactly the whitelist `[propext, Classical.choice, Quot.sound]` — a smuggled `axiom`, `sorryAx` (an admitted goal) or `ofReduceBool` (native-code trust) fails verification.

## Trust model

`aver cert verify app.wasm out/cert` (exit 0 = certified, anything else = 1):

1. hashes the artifact and declines on mismatch with the pinned value;
2. assembles a **checker-owned build**: the audited `Schema.lean` / `CertPrelude.lean` come from the checker binary (never from the cert), the cert contributes data files only — each name-gated and scanned for elaboration-executing tokens; the cert's own lakefile and caches are never read;
3. builds from a clean cache under the pinned toolchain;
4. authors a **kernel witness** binding, via `rfl`: the computed artifact hash, the obligation count, the export names re-derived from the module's export section, and each obligation's code/body, host table, function index and carrier type **re-derived from the hash-verified bytes** — so the data the theorems reason about is forced to equal what the bytes actually contain. The witness also pins the obligation's **typed face** to the standard form of its byte-derived class: a `Nonempty Dom` proof for every obligation (so a `Dom := Empty` vacuity is rejected), and per class the codomain representation (`intRepr`/`verbatimRepr`), the integer domain representation with the byte-bound arity, and the projection domain representation — each by `HEq.rfl`. A manifest that weakens `Dom`/`Cod`/`domRepr`/`codRepr` fails one of these checks;
5. type-ascribes the final theorem to `Schema.Holds manifest` and runs the kernel's axiom collector against the whitelist.

The verdict is the witness process exit code. No byte of build output is ever parsed into the verdict or the CERTIFIED report.

What a consumer trusts, exhaustively: the Lean kernel and pinned toolchain; `Schema.lean` + `CertPrelude.lean` (hash-pinned in the manifest, ~500 lines total); the checker orchestration above — including, today, the audited disassembler that re-derives bodies from bytes (a Rust component of the consumer binary, trusted by inspection). Replacing that last item with an **in-kernel decoder** — so the bytes-to-body step is itself a kernel computation and a hostile consumer can re-implement the entire check against the kernel alone — is measured and in progress. The Lean **model** of each function is data in the certificate: reading the model, or the named laws proven about it, is how a consumer knows *what* was certified; the schema keeps that reading surface small and fixed. The kernel witness pins every other obligation field to the bytes; the model itself is not byte-derivable — it is forced by the kernel proof instead (a tampered model makes the certificate fail to check), and its meaning is read, not re-derived.

**Read surface (what is declared vs. pinned).** As of this branch the checker pins the typed face of each obligation to the standard form of its byte-derived class (see trust-model step 4). Two items remain **read declarations**, not kernel-re-derived: the **model** of every class, and the **`domRepr` of the ADT classes** — the `variant match` domain (both the three-branch and the widened Int match) and the `constructor` domain/codomain are stated over a user-inductive `Repr` the checker cannot reconstruct from bytes alone. For those the checker still proves `Nonempty Dom` and forces the constructor's runtime behaviour with an executable interpreter tripwire, but the domain/codomain *representation* of a user type is read from the certificate, exactly like the model. Closing this residue (a per-build mechanical disjunction of the variants, and an in-kernel decoder for the model) is future work.

## Certification levels

| level | meaning |
|---|---|
| L0 | source-level verification only; no artifact claim |
| **L1 (current)** | artifact claim, conditional on the named runtime contracts in the manifest |
| L2 | runtime contracts themselves proven against the shipped helper bodies |
| L3 | additionally totality/resource bounds ("bounded total correctness on valid inputs") |

The manifest always declares its level. A certificate never silently claims more than its level supports.

## Honest limits (v0)

- **Ten certified classes.** Four integer: straight-line integer functions of the add-a-constant shape, single-argument structural self-recursion `f n = if n≤0 then base else <combine>` — one fuel-induction arm recognised structurally from the decoded instruction tree, where the base literal, the combinator's non-recursive operand (the input `n`, or a boxed constant) and its operand order (`n + f(n-1)`, `2 + f(n-1)`, `f(n-1) + n`) are all read from the bytes as data, and the combinator operation is `+` or `*` read from the model (the bignum `add` and `mul` helpers are not byte-distinguishable, so `n * f(n-1)` — factorial — certifies via the `mul` contract); only the descent `n-1` is pinned (`sumTo`'s `n + f(n-1)` is one instance, not a separate class). Two-argument accumulator self-recursion (`f(n, acc)`: the first integer parameter descends, the second is an accumulator updated in the straight-line fragment) is the same arm's tail-recursive shape, and an **Int -> Bool range predicate** — two nested comparisons against constants, `match cp >= k_lo { true -> cp <= k_hi; false -> false }` (the surrogate-range checks), certified over the canonical small-carrier domain where the bignum comparison arms are dead. Three ADT: a non-recursive **constructor** (one- or two-field), a non-recursive **field projection** (both a bare record access `u.field` and a two-component destructuring match `match p { (a, _) -> a }`, which lowers with a bind-and-cast preamble; the projected field must be 0 or 1 and is returned verbatim), and a non-recursive **variant dispatch**. The variant dispatch recognises any non-recursive single-argument match over a user inductive whose constructors all carry a single integer payload, when the emitted body is a chain of `ref.test` branches and every hit arm reduces to one of a small vocabulary of leaves — return the payload, or combine the payload with an integer constant through the contracted runtime add/sub helper (constant on either side) — with a boxed integer constant as the terminal arm. Within that vocabulary the arm count, arm order, per-arm semantics and the default value are free: the certificate proof is generated by walking the decoded branch tree, one case per constructor, closing each leaf with the fixed simp set, citing the add/sub contracts at host call sites, with one executable guard per constructor path. (The historical frozen three-branch `Add x -> x; Neg x -> 0 - x; Zero -> 0` template is one instance of this class, not a separate one.) Arms outside the leaf vocabulary (nested matches, payload-less constant arms, multiple helpers per contract role, a non-constant final arm) decline the whole function fail-closed. A distinct fourth ADT class, the **widened Int match**, reads one integer-payload variant of a *heterogeneous* inductive and boxes `0` for every other variant: the two-branch `match j { JsonInt(n) -> n; _ -> 0 }` where the other variants carry non-integer payloads (strings, floats, nested lists). It is a genuinely different proof shape, not a case of variant dispatch: it pins only the one projected variant's byte-derived struct index and treats the rest opaquely, precisely because it cannot represent the other variants as integer carriers. Its codomain is pinned to `Int` (`intRepr`) by the boxed-`0` default. A fifth ADT class, the **verbatim widened match**, is the same single-hit shape but returns the projected field as a raw `WVal` (`Cod := WVal`, `verbatimRepr`) with a null-reference default — `match j { JsonList(items) -> items; _ -> [] }` where the empty list lowers to `ref.null`. It makes no claim about the projected value's meaning, so it needs no carrier or string representation; the domain is all of `WVal`. And **cross-function composition**: a unary integer caller whose body is a chain of calls into other user functions, certified over one shared code table that carries the whole (acyclic) call closure re-derived from the bytes — the caller's proof cites its callees' simulation lemmas, so mutating a callee's body fails the caller's certificate. Its v1 leaf dictionary is deliberately small (self-sum leaves and nested chains); callers reaching any other callee shape are declined by construction, not by a dead template. Everything else — including mutual recursion, multi-argument recursion beyond the two-argument accumulator shape, and any match outside the recognised dispatch grammar — is declined and listed under `source_level_only` with a reason. The classifier is fail-closed: a function whose emitted body does not match a certified template is declined even if it looks like it should fit.
- **Floats are excluded from certified claims.** Wasm NaN payloads are engine-nondeterministic; observed bit-exactness in the differential harness is a measurement, never a promise.
- **Runtime contracts are assumptions** at L1. They are few, named, and stable, and the plan of record is to discharge them per release (L2), starting with the small integer helpers.
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
