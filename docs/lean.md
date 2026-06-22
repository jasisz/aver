# Lean Backend

Lean is the proof-export backend for Aver.

Use it when you want:
- Lean 4 artifacts for pure Aver code
- proof obligations for classified effectful code via Oracle lifting
- executable proof obligations from colocated `verify`
- universal theorems for supported `verify law` shapes, with explicit
  sampled/domain fallback for the rest
- a path from Aver code to formal verification

This is not a second execution runtime for effectful programs.

## Quick start

```bash
aver proof examples/formal/law_auto.av --verify-mode auto -o /tmp/law-auto-lean
cd /tmp/law-auto-lean && lake build
```

Requires a local Lean 4 toolchain (`lean` + `lake`). Aver generates the project, but does not bundle Lean itself.

## What it generates

Generates a Lean 4 project:

```
out/
  lakefile.lean
  lean-toolchain
  <Project>.lean
```

## Scope

- exports pure core logic: types, pure functions, and decisions
- emits lifted pure forms for classified effectful functions used by Oracle laws
- skips unclassified effectful functions and `main`
- turns colocated `verify` / `verify law` intent into Lean proof artifacts

Oracle-lifted laws make effects explicit as theorem parameters. For example,
`given rnd: Random.int = [fairDie]` becomes a proof-side oracle argument with
the derived `Random.int` oracle signature. See [oracle.md](oracle.md) for the
classified effect set, stub signatures, Oracle law syntax, and trace assertions.

## Verify emission

`verify` blocks become Lean proof obligations:

- default (`--verify-mode auto`): `example : <lhs> = <rhs> := by native_decide`
- fallback (`--verify-mode sorry`): `example : <lhs> = <rhs> := by sorry`
- theorem stubs (`--verify-mode theorem-skeleton`): named `theorem ... := by sorry`

`verify ... law ...` always emits expanded sample theorems from `given` domains:
- `theorem ..._sample_n := by native_decide`

The universal law theorem is always emitted as
`theorem <fn>_law_<name> : ∀ ..., lhs = rhs := by ...`. The body is:
- a real auto-proof when the law shape matches one of the supported strategies (see `Conservative auto-proofs currently cover:` below), or
- `sorry` with an inline comment (`-- verify law is sampled; universal proof must be provided manually`) when no strategy matches

The per-sample and `_checked_domain` conjunction theorems always emit
alongside as kernel-checked `native_decide` evidence, so the proof
obligation is real and visible even when the universal body is `sorry`.

`when` clauses translate to extra theorem premises in Lean — both the
sampled-disjunction (`x = sample₁ ∨ x = sample₂ ∨ ...`) and the
`when_expr = true` clause itself. Generic auto-proof strategies still
run on those guarded laws, but unmatched shapes fall back to the
`sorry` universal theorem plus per-sample theorems described above.
Parser/render roundtrip laws such as `parse(render(x)) = x` currently
live in that fallback bucket unless some other generic shape
discharges them first.

When `law <ident>` names an existing pure function and the law body compares `foo(args)` against `fooSpec(args)`, Aver treats that as a canonical spec reference:
- the generated theorem/comment uses the canonical `<fn>_eq_<spec>` naming
- `aver context` also records `fibSpec` as a spec for `fib`
- in `--verify-mode auto`, the universal theorem body is auto-proven when the law shape matches a strategy; otherwise it lands with `sorry` and a comment for the user to fill in

Example:

```aver
fn fibSpec(n: Int) -> Int
    match n
        0 -> 0
        1 -> 1
        _ -> fibSpec(n - 1) + fibSpec(n - 2)

verify fib law fibSpec
    given n: Int = [0, 1, 2, 3, 4, 5]
    fib(n) => fibSpec(n)
```

## Specs over invariants

This is the intended proof style in Aver:

- the author writes a simple pure spec function
- the author writes `verify impl law implSpec`
- the proof backend tries to connect implementation and spec

The goal is to avoid making the surface language proof-engineer-first.

Invariants still exist as a proof concept, especially for optimized implementations such as tail-recursive helpers, parsers with state, or accumulator-heavy code. But Aver tries to push those invariants down into the proof backend whenever possible, instead of making users write them first.

In short:

- user-facing Aver should prefer explicit specs
- the proof backend should absorb invariants where it can
- dropping to explicit invariant reasoning should be the exception, not the default workflow

If Aver cannot auto-prove the universal law shape in `--verify-mode auto`, the universal theorem body lands as `sorry` with an explanatory inline comment, and the per-sample + `_checked_domain` theorems still emit alongside as kernel-checked evidence. The proof obligation stays visible (and Lean will reject `lake build` until the user replaces the `sorry`), but the file compiles, so the case-level evidence remains useful even before someone closes the universal.

See [How the auto-prover decides](#how-the-auto-prover-decides) for the recognizer families currently covered.

The generated Lean prelude also includes one-character separator lemmas such as
`AverString.split` over `String.join(_, sep) ++ sep` for separator-free parts.
Those helper lemmas support exported code, but they are not themselves a claim
that delimiter-based parser/render laws are universally auto-proved.

## How the auto-prover decides

There is no proof search and no AI at proof time. Each `verify law` runs through a fixed, deterministic decision tree of shape recognizers; the first recognizer that matches pins a strategy, and each strategy emits a fixed tactic script. The Lean kernel — not the recognizer — is the judge: a law is credited `universal: true` only when its theorem's `#print axioms` stays inside `{propext, Classical.choice, Quot.sound}` (in particular `native_decide` proofs never count — they trust the compiler's evaluator via `Lean.ofReduceBool`). A strategy that does not close degrades to a caught `sorry`, never a false proof.

The recognizer families, roughly in routing order:

- **syntactic algebra** — reflexive, commutative / associative / identity / anti-commutative wrapper shapes over `Int` operators.
- **spec equivalence** — implementation-vs-spec laws closed structurally, by conservative `simp` cleanup, or by `omega` over linear `Int` arithmetic (including second-order recurrences with a pair-state tail-recursive worker, and `Int.max`/`Int.min` shapes).
- **induction** — structural induction on list/ADT givens, with generalizing variants (`induction xs generalizing n`) for accumulator-threaded and both-arguments-peeling shapes, and arm-level injection of bridge and sibling lemmas.
- **Map laws** — shipped `Map` lemmas (self-key and general-key) plus a map-fold homomorphism recognizer.
- **ground enumeration** — laws over fixed enum/ADT constructor arguments, and laws whose every given ranges over a finite domain (`Bool`, fieldless enums): exhaustive `cases` plus `rfl`/`decide`, which computes straight through fuel wrappers on closed values.
- **ring identities over records** — unconditional algebra laws of records with all-`Int` fields (for example exact rationals compared by cross-multiplication, `examples/data/rational.av`): after unfolding the call cone, both sides distribute and AC-normalize to the same polynomial via a fixed package of core `Int` ring lemmas — commutativity, associativity, distributivity, negation/subtraction laws all close kernel-genuine, no Mathlib.
- **builtin facts** — laws whose call cone bottoms out in builtins close by `simp` over the cone plus prelude spec lemmas the compiler ships (for example `Int.fromString_fromInt` and `String.slice` facts).
- **floor-division windows** — laws over a power-of-two function, a floor-halving binary-exponent search (`Result.withDefault(Int.div(a, 2), 0)` recursion), and the power-of-two window predicates built from them: positivity and the sum homomorphism of the power function, the scaled-significand window (`2^(n-1) <= sig(a, b, n) < 2^n` under `when b >= 1; a >= b; n >= 1`), and the bit-width product window. These `when`-laws are stated and proven in true universal form: the emitted proof combines functional induction over the well-founded definitions with the core floor-division bridges (`Int.le_ediv_iff_mul_le`, `Int.ediv_lt_iff_lt_mul`) — kernel-genuine, no Mathlib. See `tests/fixtures/floor_window.av`.
- **synthesized lemmas about your functions** — when a function matches a conservative shape gate (for example the canonical string-position scanner), the compiler synthesizes and kernel-proves a companion lemma about it and uses it in higher strategies such as the decimal render/parse roundtrip. When the gate does not match, nothing is emitted.
- **fallback** — bounded evidence only: per-sample and `_checked_domain` theorems via `native_decide`, plus the universal theorem with an honest `sorry`. `when`-laws get a guarded-domain enumeration instead; their bounded statements carry an explicit statement class and are never credited as universal.

When the recipes run out, the escalation path is more Aver, not Lean: split the hard law into helper laws — each one is a runnable test in milliseconds — and once proven, earlier laws in the same file become rewrite ammunition for the laws below them. A proven helper law committed in scope is picked up automatically by the law below it (the `SimpOverLemmas` feedback loop); the `the-method` agent loop can propose those helper laws for you.

Termination is part of the same honesty story: structural recursion over your own types and recognized well-founded shapes (for example quicksort's mutual recursion) emit genuine total definitions; the remaining recursive shapes are emitted fuel-wrapped, with the fuel budget derived from a synthesized size measure of the call-site arguments.

## Proof mode

Recommended mode:

```bash
aver proof my_module.av --verify-mode auto -o out/
```

That combination means:
- regular `verify` cases become executable Lean checks via `native_decide`
- supported `verify law` shapes get real universal proofs
- unsupported `verify law` shapes emit the universal theorem with a `sorry` body and an inline comment, plus the per-sample + `_checked_domain` theorems as kernel-checked evidence
- recursive pure code inside the supported proof subset is emitted as total Lean defs
- unsupported recursive pure functions are called out explicitly and emitted with `partial` fallback

When a law lands on the `sorry` fallback and you want to know why, see [transpilation.md → Debugging a law that didn't auto-prove](transpilation.md#debugging-a-law-that-didnt-auto-prove) for the `--emit-ir-after=law_lower` workflow.

The current proof export supports:
- single-function `Int` countdown on an `Int` parameter (`n -> n - 1`). Closed-world fns (no `exposes` clause, or absent from the list) with the canonical `match p { L -> base; _ -> rec(p-1, ...) }` body emit as a native aux def carrying a precondition extracted from the unique external caller's surrounding `match`/`if` guards — `(h_dom : n ≥ 0)` from `fib`'s `match (n < 0) { false -> ... }` arm, or compound predicates like `(h_dom : n > 2 ∧ n < 500)` from nested caller guards. The aux is wrapped by a thin public def preserving the original signature. `Lean omega` closes the per-callsite preservation obligation and the `Int.natAbs n` decrease automatically. Other countdown shapes (exposed fns, multi-caller fns, leading let-bindings) fall back to fuel-encoded helpers.
- single-function `Int` floor-division countdown by a literal divisor — every self-call shrinks an `Int` parameter through `Result.withDefault(Int.div(p, k), d)` with literal `k >= 2` (inlined, or through a unary wrapper like `fn half(a) = Result.withDefault(Int.div(a, 2), 0)`), and the guards enclosing every self-call provably imply `p >= 1` (e.g. `match p > 0`, or the binary-exponent pair `b >= 1` and `a >= 2 * b`). Emits a genuine well-founded def (`termination_by p.toNat`, kernel-checked decrease) instead of a kernel-opaque `partial def`; an unvalidated guard declines, never guesses.
- single-function second-order affine `Int` recurrences with `n < 0` guard, `0/1` case split, and a matching pair-state tail worker, emitted via a private `Nat` helper
- single-function structural recursion on any `List<_>` parameter
- single-function `String + pos` recursion on `(String, Int)` signatures
- mutual recursion SCC with first-parameter `Int` countdown
- mutual recursion SCC with ranked `String + pos` progress
- mutual recursion SCC with ranked structural descent over recursive parameters (emitted as native `mutual ... termination_by ... end` block when every SCC member has a `List`/`Vector` sizeOf measure; fuel-encoded otherwise)

## Checking an export (`--check` / `--check-json`)

`aver proof file.av --backend lean -o out/ --check` builds the export with
`lake` and gates on the result; `--check-json` prints one machine-readable
summary line. Fields of the Lean summary:

- `passed` — the build succeeded within the budgets (a bounded
  verify-on-domain still passes; this is deliberately the lenient gate)
- `sorries` — residual `sorry` count across the build output
  (`--sorry-budget N` tolerates up to N)
- `universal` — `true` only when EVERY law theorem in the export is
  kernel-genuine: its `#print axioms` stays inside
  `{propext, Classical.choice, Quot.sound}` (so `native_decide` and
  `sorry` never count), at least one theorem is explicitly classed
  universal, and the file has no sorries
- `universal_laws` — how many law theorems classed universal passed that
  same per-theorem axiom whitelist. On a file with sorries the axiom
  audit does not run at all, so this reports `0` — pin it together with
  `sorries == 0`, not instead of it
- `bounded_laws` — how many law theorems the emitter classed
  bounded-domain (stated only over the finite sample grid, e.g. guarded
  `when`-law enumerations); these never earn universal credit

Note: a law the emitter declines entirely (no theorem emitted — e.g. a
shape outside every strategy) appears in NEITHER counter; the counters
count emitted law THEOREMS, not `verify ... law` blocks in the source.
- `model_panicked` — the compiler-model panicked while evaluating a
  bounded sample; the check fails regardless of budgets
- `budget` — the active sorry budget

`universal_laws` and `bounded_laws` are sourced from the same per-theorem
statement-class markers and the same `#print axioms` audit the `universal`
bool keys on. A robust CI budget pins all four together:
`sorries == X`, `universal == true`, `universal_laws == N`,
`bounded_laws == M`.

## Minimizing a proof (`--minimize`)

`aver proof file.av --backend lean -o out/ --check --minimize` (Lean-only,
implies `--check`) rewrites each auto-proof to the tactic that actually closed
it. The auto-prover pins a deterministic `first | (t₁) | (t₂) | … | sorry`
PORTFOLIO at every law — it cannot know statically which alternative a given
goal needs — and `--minimize` resolves that hedge against a real build:

1. each `first` branch is prefixed with a `trace "AVERMIN:i:b"` marker and the
   project is built ONCE. `first` tries branches left-to-right and commits to
   the first that closes, tracing each it reaches, so the winning branch of
   portfolio `i` is the **highest** index `b` that appears in the build log
   (failed branches before it trace too — the markers are not rolled back);
2. each portfolio is collapsed to its winning branch and the project is
   RE-VERIFIED. If a theorem no longer closes (a mis-parsed winner), it keeps
   its original portfolio.

So minimization is **fail-safe** (it can never ship a proof that does not
build) and **status-preserving** (it keeps exactly the branch Lean committed
to). A law that closes for real drops its alternation and `sorry` floor and
reads like a hand-written proof; a law that only closed via its floor collapses
to a bare `sorry` — the honest gap is kept, never silently dropped, so the
`sorries` / `universal` numbers from `--check` are unchanged. It is an opt-in
polishing pass (it costs two extra `lake build`s), not part of the normal
verify loop.

## Refinement records (refinement-via-opaque)

An Aver `record X { v: Int }` paired with a validating smart constructor
`fn fromX(n: Int) -> Result<X, String>` whose body is the canonical
`match <pred(n)> { true -> Result.Ok(X(v = n)); false -> Result.Err(_) }`
shape lifts to a true refinement subtype in Lean:

```lean
abbrev Natural := { v : Int // v ≥ 0 }
```

The predicate from the smart constructor's bool guard rides in the
type itself, so a `verify add law commutative` over `Natural` quantifies
directly over the refined type:

```lean
theorem add_law_commutative : ∀ (a b : Natural), add a b = add b a := by
  intro a b
  unfold add fromInt
  simp [Int.add_comm, Int.mul_comm]
```

— one line, instead of the pre-0.22 `by_cases h_a : a ≥ 0 / by_cases
h_b : b ≥ 0 / unfold / hand-rolled tactic` plumbing per law shape. The
lift is automatic, no source-language change. Triggers for single-field
`Int` carriers; `Float` and `String` carriers keep the plain
structure path (IEEE 754 NaN breaks universal float laws, strings have
no universal algebraic structure to exploit).

Refinement records work the same way whether the type is declared in
the entry file or in a dependent module — `aver proof natural.av` and
`aver proof natural_app.av --module-root examples` both emit the same
lifted Subtype shape. Pre-0.22 cross-module fell back to the wrapper
shape and lost the one-line universal proof.

`verify ... law` blocks with a `when` clause keep the clause as a
theorem premise when it carries information beyond the refinement
type's invariant. A `when a >= 10` over `Natural` (invariant
`a.val >= 0`) shows up as a real `(a.val ≥ 10) -> ...` antecedent
on the universal; a redundant `when a >= 0` is dropped cleanly so the
universal stays in the one-line `∀ (a : Natural), ...` shape.

## Current end-to-end examples

These examples are currently smoke-tested end to end with
`aver proof --verify-mode auto` plus `lake build`:

- `examples/formal/law_auto.av`
- `examples/data/fibonacci.av`
- `examples/data/quicksort.av`
- `examples/data/rle.av`
- `examples/data/json.av`
- `examples/core/grok_s_language.av`
- `examples/refinement/natural/natural.av` — refinement-via-opaque (Int + `>= 0`)
- `examples/refinement/positive/positive.av` — refinement (Int + `>= 1`)
- `examples/refinement/int_range/int_range.av` — refinement with compound `Bool.and(n >= 0, n <= 100)`
- `examples/refinement/bigint/bigint.av` — refinement over `List<Int>` + mutual-rec digit arithmetic
- `examples/refinement/nonneg_float/nonneg_float.av` — Float carrier, structure path
- `examples/refinement/email/email.av` — String carrier, structure path

Other modules have unit coverage for proof-subset classification and generated
Lean snippets, but are not currently listed here as end-to-end smoke cases.

## Hard-fail guarantees

Lean codegen does not silently mask unresolved compiler internals:

- `Expr::Resolved` in codegen input is a hard codegen error
- `Type::Invalid` in codegen input is a hard codegen error
- `sorry` is emitted in two situations: explicit `--verify-mode sorry`, and `--verify-mode auto` universal-law theorems whose shape no auto-proof strategy covers (always paired with an inline comment + kernel-checked per-sample theorems alongside, so the obligation stays visible)
