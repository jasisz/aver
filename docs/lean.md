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

Every Aver module, including the entry module, emits its definitions inside
its corresponding Lean namespace. A function `decode` in module `Packet` is
therefore exposed as `Packet.decode`; theorem-skeleton declarations follow the
same rule, for example `Packet.decode_verify_1`.

Every file, the entry included, imports the whole dependency closure (each
file has to compile) but `open`s only the module's own direct `depends`.
The emitter spells every cross-module name it produces with its module
path: a function with the module that declares it, and a user type declared
in another module with its owner's path wherever it appears — signatures,
record fields, constructor expressions (`A.Fraction`, `X.Shape.circle`), and
a capability resource the same way (`Kv.Handle`) — while a module's own
types keep their bare names. That is what lets a type a direct dependency
merely re-exposes, two opened modules declaring the same type name, a user
type spelled like a Lean root type (`Sum`), and the resource of a capability
the entry only threads an operation of all resolve, and what lets a match
binder share a name with a function some distant module exports. A function
name that two direct dependencies both declare is hidden from both `open`s
(`open Script hiding named`), members of a `mutual` block included; the
Option constructors are spelled `Option.some` / `Option.none`, and a user
function or binder called `some` or `none` is renamed `some'` / `none'`
(like `id`, `max` and `min`) so neither ever meets the root alias.

Every emitted file, `AverCommon.lean` and the SHA-256 model included, sets
`autoImplicit false`: a type name the export leaves unresolved is a build
error (`Unknown identifier`), never an implicit type variable Lean binds on
its own and a theorem that quietly says something else.

## Scope

- exports pure core logic: types, pure functions, and decisions
- emits lifted pure forms for classified effectful functions used by Oracle laws
- skips unclassified effectful functions and `main`
- turns colocated `verify` / `verify law` intent into Lean proof artifacts

Oracle-lifted laws make effects explicit as theorem parameters. For example,
`given rnd: Random.int = [fairDie]` becomes a proof-side oracle argument with
the derived `Random.int` oracle signature. See [oracle.md](oracle.md) for the
classified effect set, stub signatures, Oracle law syntax, and trace assertions.

Lifting is per module, not per entry. A dependency file exports the effectful
functions some claim reaches exactly as the entry does — inside its own
namespace, with the same leading `(path : BranchPath)` and one oracle
parameter per classified effect — so `Infra.Store.get` becomes

```lean
def get (path : BranchPath)
        (rnd_Infra_Kv_get : BranchPath -> Int -> String -> Except String (Option String))
        (store : Store) (key : String) : Except String (Option String)
```

and a call site anywhere passes that callee's own oracle list:
`Infra.Store.get path rnd_Infra_Kv_get store (heightKey height)`. A tail call
carries the same arguments, and two effectful functions that call each other
are exported as one `mutual` block rather than a forward reference. A lifted
signature spells the types its effects carry by their owner module
(`Tcp.Connection`, `Bytes.Bytes`), and that owner is usually a standard module
the source never writes in `depends`; a file that exports a lifted function
therefore imports the owners of the constants it names, alongside its written
`depends`. Which functions a dependency exports follows the consumers: the roots are the
qualified calls the entry's `verify` blocks and their cones spell, closed over
the calls those functions make inside their own module. A function no claim
reaches is still not exported — a loop that reads the console forever has no
Lean meaning and nothing is being proven about it — so a program that proves
nothing about a dependency's effects exports exactly what it did before.

## Verify emission

`verify` blocks become Lean proof obligations:

- default (`--verify-mode auto`): `example : <lhs> = <rhs> := by decide +kernel` when the case's whole closure is known to reduce in the Lean kernel, `:= by native_decide` otherwise
- fallback (`--verify-mode sorry`): `example : <lhs> = <rhs> := by sorry`
- theorem stubs (`--verify-mode theorem-skeleton`): named `theorem ... := by sorry`

A case whose left side carries `?` states a `Result` computation: the left side is emitted inside a Lean `do` block so `?` short-circuits on `Result.Err`, and the expected side reads `Except.ok <expected>`. A case whose `?` hits `Err` fails under `aver verify`, and this keeps it false as a theorem too, rather than continuing with a default value. Cases without `?` are unaffected.

A law body carrying `?` takes the same shape, in all three statements a law emits — the quantified theorem, `<law>_checked_domain`, and every `<law>_sample_N` — so the three agree on what the law claims. The quantified form reads `∀ <givens>, (do pure (<lhs>)) = Except.ok <rhs>`, which asserts that the `?` reaches `Ok` for every value of the givens, not merely for the sampled ones. Laws without `?` are unaffected. A `?`-carrying law is not offered to later laws as a rewrite rule: its theorem relates `Result` computations, not the values the source mentions.

The tactic is chosen per case, and conservatively: `decide +kernel` costs no trust (the axiom closure stays inside Lean's core three, with no `Lean.ofReduceBool`) but only works when everything the case mentions unfolds in the kernel, so a case is routed there only when the emitter can positively establish that. Anything else — a `Float` anywhere in the closure, a fn this export spelled `partial def`, a mutual group, a case whose expected side is not a VM ground-truth literal, a case whose emitted equation is larger than the term budget — stays on `native_decide`. One boundary is stricter in the default automatic mode: if a sampled `verify` case or law transitively reaches a mutual-recursion fuel fallback whose seed is not a proven bound because a peer receives an opaque computed successor, the claim is declined rather than evaluated. Lean's `panic!` returns `default`, so `native_decide` could otherwise certify exhaustion as a real result. The generated file carries an explicit refusal comment, and `aver proof --check` reports and charges the structured `declined_claims` entry naming the affected functions. Explicit `sorry` and theorem-skeleton modes still emit their visibly unproven obligations. Fuel derived from a validated Int countdown, string-position bound, or ordinary ranked structural measure retains the existing native-evaluation path. The same refusal covers provider-owned pure capability operations: each is exported as `noncomputable opaque` (with a proposition-only `Nonempty` witness, never `Inhabited`, so no default can stand in for the provider), every function whose call cone reaches one is emitted inside a `noncomputable section`, and a sampled case or law whose cases, `given` values, template, or `when` guard reach such an operation is declined as a whole instead of being evaluated. Functions and claims that never touch a capability operation are emitted exactly as before, even in a module that also holds part of a provider cone.

The `Vector.get` / `Vector.set` family stays on `native_decide` even though it reduces in the kernel, because its exported model narrows a negative index to `0` where the runtime returns `Option.None`. That can walk the model down a branch the program never took. String code-point lookup is total on both sides: `String.firstCodePoint("")` is `Option.None`, so it no longer needs this exception.

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

## Law explanations in Aver

Automatic proof remains the default. If it cannot assemble an argument, a law can supply ordinary pure Boolean expressions with `because` and select lemmas with `using`:

```aver
fn selectionReason(value: Int) -> Bool
    match choose(value)
        Option.None -> value <= 0
        Option.Some(found) -> Bool.and(found == value, found > 0)

verify amount law selectionIsClamped
    given value: Int = [-3, 0, 1, 7]
    because selectionReason(value)
    using []
    amount(choose(value)) => Int.max(value, 0)
```

The complete executable example is [law_reasons.av](../tests/fixtures/law_reasons.av); [law_reasons_digits.av](../tests/fixtures/law_reasons_digits.av) composes four helper laws to prove minimality of a base-seven digit representation. Reason functions use normal Aver semantics and remain normal functions, including the usual `check` guidance. There is no separate assertion meaning for Boolean local bindings.

Multiple `because` lines are ordered steps. With original guard `H`, reasons `R1 … Rn`, and claim `P`, the backend proves `H → R1`, then each `H ∧ R1 ∧ … ∧ R(i-1) → Ri`, and finally `H ∧ R1 ∧ … ∧ Rn → P`. Without `when`, `H` is true. The exported original theorem still states `H → P`, and its audited dependency chain includes every explanation. An easy original claim cannot hide a failed reason.

The backend uses reason functions' case structure to preserve branch equations, including nested matches revealed by checked induction. Cases are split before Boolean conjunctions become sequential goals: the right-hand fact keeps that branch's premises and can use the checked left-hand fact. Local bindings can expose further cases but do not become assumptions. See [law_reason_match_branches.av](../tests/fixtures/law_reason_match_branches.av) for a chunked list traversal and a false strict-positivity control. It then applies the available lemmas and ordinary automation. `because true` leaves the work in the final implication; `because false` fails the first obligation; restating the claim as its own reason leaves the work in that reason. All three are legal and none bypasses proof checking.

`using [function.law, Module.function.law]` selects a set of lemma names. Order within the list is immaterial. Selected lemmas remain available in every explanation obligation and in the final implication. Omitting `using` keeps automatic selection; `using []` selects none. Local forward references are allowed, imported laws must be exposed by their subject's module, and unknown names or dependency cycles are errors. Explicit citations retain theorem scope and the usual transitive axiom audit: samples and `sorry`-tainted theorems cannot grant universal credit.

Typed empty list samples retain their concrete checked element type in Lean, including when nested inside polymorphic list operations. Unresolved generic element types are left for Lean to infer from context.

`aver verify` and `aver verify --hostile` execute every reason under the original guard, using the same sample expansion as the claim. These checks do not require Lean. `aver proof --check-json` and `proof_manifest.json` additionally report separate `obligations`, identified by `<function>.<law>.because1`, `.because2`, and `.implication`. Obligations do not inflate `universal_laws`. `--explain` includes residual goals for failed explanation obligations when the solver provides them; an implication may be universal while its reason and the original law remain failed.

For `using` and `because`, nonrecursive functions contribute their full kernel equation so the solver can see every match alternative, including a lookup's default branch. Equations of checked structural list functions and native subtractive countdown functions in the law's dependency cone are available to `grind` even without a recursive explanation. They stay out of recursive `simp` rewriting. Native mutual functions expose their kernel-generated equations through the same selection used to emit their checked definitions, including computed-list descent; a fuel fallback does not expose an original-function equation. The solver also uses Lean's existing guarded `List.take`/`List.drop` equations and the conversion of nonpositive `Int` counts to zero, matching Aver's slice semantics. Floor-division countdowns and fuel helpers remain opaque to this solver; cited laws can summarize them without expanding their recursive arithmetic.

Recursive explanations can also supply an induction plan when the function has a checked structural descent on a list. A recursive call contributes an induction hypothesis for the shorter list; its Boolean result must still establish the facts needed by the next step. Original guards and earlier reasons remain premises of that hypothesis, so they must hold at the recursive arguments. If Lean cannot construct a functional induction principle for a local match, the backend falls back to ordinary induction on that same checked list parameter, with the other givens generalized and all original premises retained. The same termination contract serves ordinary functions and explanations: a shrinking list takes precedence over a sibling counter's fuel model.

A single recursive function also reuses the shared cycle analysis for `List.drop` or `List.take` of a known cons-tail: the slice may equal that tail, but it is still shorter than the original list. This uses the existing length contract and native emitter, including when another list accumulator grows. Dafny also prefers this shared checked descent over its fallback parameter ordering. Slicing the whole input does not establish strict decrease. [law_reason_singleton_list.av](../tests/fixtures/law_reason_singleton_list.av) demonstrates direct induction over a chunked traversal without a separate step list, alongside nondecreasing and growing-call controls.

For example, the explanation can establish the property for `rest` before assembling the property for the whole list:

```aver
fn appendReason(items: List<Int>, suffix: List<Int>) -> Bool
    match items
        [] -> count(List.concat(items, suffix)) == count(items) + count(suffix)
        [head, ..rest] -> Bool.and(appendReason(rest, suffix), count(List.concat(items, suffix)) == count(items) + count(suffix))
```

[law_reasons_recursive.av](../tests/fixtures/law_reasons_recursive.av) contains the complete laws, including a changing counter, multiple reasons, and deliberately false base and step cases. This first induction path accepts direct calls on distinct law parameters, including imported functions. Composite arguments, mutual recursion, other recursion measures, and wrappers around recursive explanations do not yet select this induction path. A call on the unchanged list does not produce a checked induction hypothesis; passing declared samples cannot replace that proof.

Explanation obligations are universal. A stage the backend cannot prove remains failed; it does not silently substitute sampled evidence. Dafny export explicitly declines annotated laws because that backend does not yet implement their obligations. The syntax is provisional; [#1288](https://github.com/jasisz/aver/issues/1288) tracks the mechanism and acceptance checks.

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
- **induction** — structural induction on list/ADT givens, with generalizing variants (`induction xs generalizing n`) for accumulator-threaded and both-arguments-peeling shapes, and arm-level injection of bridge and sibling lemmas. Each arm of a `when`-law also tries a subject-first closer that needs no hypothesis from the induction: unfold the law's own function, split its guard, then unfold the rest of the cone, split the residual `if`s, normalize and hand the linear leftovers to `omega` — the shape of a law whose function decides between two list layouts and whose reader has to match on that decision. A `when` that bounds the list's length from below by a literal (`List.len(items) >= 2`) also gets, in the cons arm, one `rcases` exposing exactly the further conses the premise guarantees, so a claim that reads fixed positions of the list (a stack shuffle through `itemAt`, a `List.take` of that many items) evaluates by `simp_all` with no induction hypothesis at all. A single-list conditional law is stated universally only after a probe build of the file has shown that its portfolio closes; otherwise it keeps its sampled statement. A hard error in one candidate's probe (a heartbeat timeout in one arm) costs only that candidate, and the run says so; `AVER_SPECULATIVE_LOG=<file>` keeps the probe build log. A bare call name in a law is the function of the law's own module, whatever other modules of the program call theirs; a dependency's laws are proven against that dependency's own functions and blocks when the program is proven from its root entry.
- **Map laws** — shipped `Map` lemmas (self-key and general-key) plus a map-fold homomorphism recognizer.
- **ground enumeration** — laws over fixed enum/ADT constructor arguments, and laws whose every given ranges over a finite domain (`Bool`, fieldless enums): exhaustive `cases` plus `rfl`/`decide`, which computes straight through fuel wrappers on closed values.
- **ring identities over records** — unconditional algebra laws of records with all-`Int` fields (for example exact rationals compared by cross-multiplication, `examples/data/rational.av`): after unfolding the call cone, both sides distribute and AC-normalize to the same polynomial via a fixed package of core `Int` ring lemmas — commutativity, associativity, distributivity, negation/subtraction laws all close kernel-genuine, no Mathlib.
- **builtin facts** — laws whose call cone bottoms out in builtins close by `simp` over the cone plus prelude spec lemmas the compiler ships (for example `Int.fromString_fromInt` and `String.slice` facts).
- **floor-division windows** — laws over a power-of-two function, a floor-halving binary-exponent search (`Int.div(a, 2)` recursion — the literal divisor discharges to plain `Int`), and the power-of-two window predicates built from them: positivity and the sum homomorphism of the power function, the scaled-significand window (`2^(n-1) <= sig(a, b, n) < 2^n` under `when b >= 1; a >= b; n >= 1`), and the bit-width product window. These `when`-laws are stated and proven in true universal form: the emitted proof combines functional induction over the well-founded definitions with the core floor-division bridges (`Int.le_ediv_iff_mul_le`, `Int.ediv_lt_iff_lt_mul`) — kernel-genuine, no Mathlib. See `tests/fixtures/floor_window.av`. These windows reproduce the integer-arithmetic core of a classic hardware floating-point-divider correctness proof; reproducing a known-hard result is deliberate, because it calibrates how much of such a proof the engine carries on its own. The frame outlives any single theorem — the next targets on deck are the self-hosted interpreter's own evaluator core and an exact-decimal arithmetic library for regulated computation.
- **synthesized lemmas about your functions** — when a function matches a conservative shape gate (for example the canonical string-position scanner), the compiler synthesizes and kernel-proves a companion lemma about it and uses it in higher strategies such as the decimal render/parse roundtrip. When the gate does not match, nothing is emitted.
- **escaped-string roundtrip** — the canonical escaped-string parse/serialize roundtrip (JSON-style `parse(escape(s)) = Ok(s, …)` laws) is certified only when the escaper's control-character threshold is a literal between 1 and 32 (the JSON control range — a bound the kernel proof over the 16-branch hex-escape ladder relies on); any other threshold falls back to sampled evidence.
- **fuel induction over a well-founded countdown** — a law about a function exported as a native `termination_by param.toNat` def is proved by induction on a `Nat` fuel bounding that argument, with the induction hypothesis instantiated at the function's own self-call arguments, the countdown function unfolded exactly once per fuel step, and a closer whose simp set never holds that function (its unconditional unfold equation would loop). Earlier laws about the same countdown are cited as ground instances at the shrunk arguments rather than as rewrite rules; an earlier `when`-law is an implication, so its instance carries the premise discharged there by `omega` from the consumer law's own premise (`value / 256 < 256` from `value < 65536`), and a premise that does not hold at the shrunk arguments drops the citation instead of assuming it — this is how a bound on the length of a countdown writer's output certifies from the bound one digit below it. The bottom rung of such a ladder has no earlier law to cite and its own hypothesis is one step too weak, so the step's closer keeps one last attempt behind the others: a second unfold at the shrunk arguments, which reaches the countdown's base branch.
- **fallback** — bounded evidence only: per-sample and `_checked_domain` theorems via `native_decide`, plus the universal theorem with an honest `sorry`. Structured proof portfolios share a final composition attempt for unconditional laws: `grind` over earlier laws about functions in the call cone, keeping recursive functions folded. This attempt follows the strategy's own alternatives, including prelude simplification, so a wrapper can use its callees' laws even when the first strategy cannot close it. Guaranteed closers and hand proofs retain their original portfolios. `when`-laws get a guarded-domain enumeration instead; their bounded statements carry an explicit statement class and are never credited as universal.

Composition and fuel induction share one final equation-based attempt when the call cone contains a well-founded countdown. It gives `grind` the countdown equations, nonrecursive definitions, and earlier laws, including laws about the current subject. Lean bounds equation instantiation by term generation; the equations never enter recursive `simp` rewriting. This closes finite observations such as dropping the first digit or inspecting the leading digit without a new recognizer for either law. Structural recursive callees stay folded.

The conditional list-layout arithmetic step also splits Boolean givens before unfolding the layout and reader. Comparison normalization runs separately with `simp only`, preventing its negation rules from looping against the default simplifier.

When the recipes run out, the escalation path is more Aver, not Lean: split the hard law into helper laws — each one is a runnable test in milliseconds — and once proven, the laws about the functions a law calls become rewrite ammunition for it, wherever they sit in the file: the export declares a law's theorem before every law whose call cone reaches its function, so a helper's law may be written after the law that needs it, and `aver format` may move blocks freely. A proven helper law committed in scope is picked up automatically by the law below it (the `SimpOverLemmas` feedback loop); the `the-method` agent loop can propose those helper laws for you.

Termination is part of the same honesty story: structural recursion over your own types and recognized well-founded shapes (for example quicksort's mutual recursion) emit genuine total definitions; the remaining recursive shapes are emitted fuel-wrapped, with the fuel budget derived from a synthesized size measure of the call-site arguments.

## What "kernel-genuine" does and does not cover

Kernel-genuine is a precise, narrow claim: the Lean kernel checked the proof of *the theorem as translated*. It certifies the tactics, not the translation. The Aver→Lean statement translator — the code that turns your `verify law` into a Lean proposition — is part of the trusted base. If the translator renders `Int.div` with the wrong rounding, or mistranslates a `when` guard, the kernel will still happily certify a true theorem about the wrong statement, and `verify` will still be green.

We do not hide this, and we are not building a verified translator (that is CompCert-scale work for a much smaller risk). Instead we lean on a validation the corpus already provides: every `verify` example dual-runs on the VM and, as an exported Lean `example`, through the translation. Each such example is one point where the VM's semantics and the translated semantics are checked to agree — a pointwise translation-validation test that runs for free on every example in the suite. The negative-divisor rounding question above, for instance, is pinned by any example whose sample exercises a negative dividend.

The honest gap is coverage: nobody has yet measured *which* language constructs and edge cases the dual-run corpus actually exercises. Making that coverage measurable — a construct × edge-case matrix against the existing examples — is in progress. Until then, treat kernel-genuine as "the tactics are sound and the statement matches the runtime on every point we have tested," not "the statement is provably the one you wrote."

## Proof mode

Recommended mode:

```bash
aver proof my_module.av --verify-mode auto -o out/
```

That combination means:
- regular `verify` cases become executable Lean checks — `decide +kernel` where the closure reduces in the kernel, `native_decide` elsewhere
- supported `verify law` shapes get real universal proofs
- unsupported `verify law` shapes emit the universal theorem with a `sorry` body and an inline comment, plus the per-sample + `_checked_domain` theorems as kernel-checked evidence
- recursive pure code inside the supported proof subset is emitted as total Lean defs
- unsupported recursive pure functions are called out explicitly and emitted with `partial` fallback

When a law lands on the `sorry` fallback and you want to know why, see [transpilation.md → Debugging a law that didn't auto-prove](transpilation.md#debugging-a-law-that-didnt-auto-prove) for the `--emit-ir-after=law_lower` workflow.

The current proof export supports:
- single-function `Int` countdown on an `Int` parameter (`n -> n - 1`). Closed-world fns (no `exposes` clause, or absent from the list) with the canonical `match p { L -> base; _ -> rec(p-1, ...) }` body emit as a native aux def carrying a precondition extracted from the unique external caller's surrounding `match`/`if` guards — `(h_dom : n ≥ 0)` from `fib`'s `match (n < 0) { false -> ... }` arm, or compound predicates like `(h_dom : n > 2 ∧ n < 500)` from nested caller guards. The aux is wrapped by a thin public def preserving the original signature. `Lean omega` closes the per-callsite preservation obligation and the `Int.natAbs n` decrease automatically. Countdowns without a validated positive self-call guard retain fuel-encoded helpers.
- single-function guarded subtractive countdown on any `Int` parameter: every self-call subtracts a positive literal and its enclosing guards imply that parameter is positive. These functions emit a native `termination_by param.toNat` definition, including accumulator forms and exposed functions, independently of the laws present. The same guard analysis used for floor-division descent and the Lean kernel both check the decrease. Unguarded subtraction and negative ascent retain their existing fuel encoding. Artifact certificate models retain explicit fuel for subtractive countdowns because the certificate recursion wall consumes that representation; standalone law proofs use the native definition.
- single-function `Int` floor-division countdown by a literal divisor — every self-call shrinks an `Int` parameter through `Int.div(p, k)` with literal `k >= 2` (the discharged total form; the legacy `Result.withDefault(Int.div(p, k), d)` wrapper is still recognized), inlined or through a unary wrapper like `fn half(a) = Int.div(a, 2)`, and the guards enclosing every self-call provably imply `p >= 1` (e.g. `match p > 0`, or the binary-exponent pair `b >= 1` and `a >= 2 * b`). Emits a genuine well-founded def (`termination_by p.toNat`, kernel-checked decrease) instead of a kernel-opaque `partial def`; an unvalidated guard declines, never guesses.
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
- `sorry_laws` — the `fn.law` identities whose theorem carries a residual
  `sorry` in the gate build (present only when `sorries > 0`). This is the
  machine-readable "which law failed?" answer: the emitter maps each Lean
  `declaration uses 'sorry'` warning back to its law through the
  `-- aver:law-class` markers, so you no longer `lake build` the generated
  project by hand and grep the warning's line number against the emitted
  theorems
- `build_errors` — count of HARD lake/lean build errors (source-located
  `error: file.lean:L:C: …` diagnostics), distinct from `sorries`. A
  degraded proof arm should always fall to a caught `sorry`; a non-zero
  `build_errors` means a tactic escaped the `first | … | sorry` floor and
  `sorries` alone would read as an honest-looking result. Informational
  only — it does not change `passed` or the exit code
- `universal` — `true` only when EVERY law theorem in the export is
  kernel-genuine: its `#print axioms` stays inside
  `{propext, Classical.choice, Quot.sound}` (so `native_decide` and
  `sorry` never count), at least one theorem is explicitly classed
  universal, and the file has no sorries
- `universal_laws` — how many law theorems classed universal passed that
  same per-theorem axiom whitelist. The audit is per theorem, so on a
  file with sorries it still runs: a sorry-floored theorem's own axiom
  line carries `sorryAx` and records tier `failed`, while every
  kernel-clean sibling keeps its `universal` record and counts. Only the
  file-level `universal` bool keeps the "no sorries" conjunct — pin the
  count together with `sorries`, not instead of it
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

### Step zero: which law failed?

When a check reports `sorries: N > 0`, the first question is *which* law — and
the answer is already in the summary. Read `sorry_laws`: it names the failing
`fn.law` identities directly, no manual `lake build` + grep. Add `--explain` to
also get goal text where there is any to show: a failing law whose proof left a
partial goal gets it inline under `open_goals` (keyed by the same identity),
while a law that fails outright — its theorem is just a `sorry` — has no
residual goal to print and is named in `sorry_laws` only. The residual probe is
deliberately coarse, so on a file with both a proven and a failing law it can
also surface a goal from the *proven* law; that borrowed goal is reported
separately under `probe_of` and never in `open_goals`, so a healthy,
already-proven law is never mistaken for the failure. Only after `sorry_laws`
names the culprit should you reach for the
`--emit-ir-after=law_lower` workflow (linked above) to understand *why* that
specific law did not auto-prove.

## Law provenance (`-- aver:provenance`)

A law can carry a structured source comment recording who PRODUCED it — a
maintenance note for when it later breaks (recompute, re-conjecture, or ask the
author?). It is a comment, not grammar, because several producers exist (the
`--explain` calculator today, a conjecturer, future tools):

```
// aver:provenance <value> [k=v …]
verify <fn> law <name>
```

It is an ordinary Aver `//` line comment (the emitter's `-- aver:law-class`
markers use Lean's `--` because they live in generated Lean, not `.av` source).
`<value>` is an open-ended lowercase token (`calculated`, `conjectured`, …);
optional keys carry context (`from=<parent law>`, `tool=explain`). The marker
sits on the line(s) immediately above the `verify … law` block.

When `--check` writes the proof manifest it scans each proving law's source for
this marker and, if present, records the payload verbatim on that law's manifest
entry as a `provenance` field. An unmarked law gets no `provenance` key (authored
by default), so a corpus with no markers produces a byte-identical manifest. The
marker is SELF-DECLARED and UNVERIFIED — a hand-written law may claim any value
and it is recorded as claimed; provenance never grants proof credit (that still
comes only from the kernel and the manifest tier). `--explain`'s calculated-law
suggestions print this line above their pasteable `verify` block, so pasting a
calculated law and re-checking records `provenance: "calculated from=… tool=explain"`
automatically.

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

An Aver single-field record paired with a validating smart constructor
`fn fromX(value: T) -> Result<X, String>` whose body is the canonical
`match <pred(value)> { true -> Result.Ok(X(v = value)); false -> Result.Err(_) }`
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
lift is automatic, no source-language change. It supports `Int`, structural
containers (`List`, `Vector`, `Map`, `Result`, `Option`, tuples), and named
carriers, including one refinement nested inside another. The exporter orders
predicate functions and types by dependency, so a source file may declare the
record before its predicate. `Float` and `String` carriers keep the plain
structure path (IEEE 754 NaN breaks universal float laws, strings have no
universal algebraic structure to exploit).

For example, `Bytes(values: List<Int>)` may carry an `allInRange(values)`
invariant, and `Digest32(bytes: Bytes)` may add `hasLength32(bytes)`; both
invariants remain in their generated Subtypes rather than degrading to plain
structures.

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
- `examples/refinement/bigint/bigint.av` — opaque `List<Int>`-backed record + mutual-rec digit arithmetic
- `examples/refinement/nonneg_float/nonneg_float.av` — Float carrier, structure path
- `examples/refinement/email/email.av` — String carrier, structure path

Other modules have unit coverage for proof-subset classification and generated
Lean snippets, but are not currently listed here as end-to-end smoke cases.

## Hard-fail guarantees

Lean codegen does not silently mask unresolved compiler internals:

- `Expr::Resolved` in codegen input is a hard codegen error
- `Type::Invalid` in codegen input is a hard codegen error
- `sorry` is emitted in two situations: explicit `--verify-mode sorry`, and `--verify-mode auto` universal-law theorems whose shape no auto-proof strategy covers (always paired with an inline comment + kernel-checked per-sample theorems alongside, so the obligation stays visible)
