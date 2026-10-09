# Lean Backend

Lean is the proof-export backend for Aver.

Use it when you want:
- Lean 4 artifacts for pure Aver code
- proof obligations for classified effectful code via Oracle lifting
- executable proof obligations from colocated `verify`
- universal theorems for supported `verify law` shapes, with explicit sampled/domain fallback for the rest
- a path from Aver code to formal verification

It is not a second runtime for effectful programs.

## Quick start

```bash
aver proof examples/formal/law_auto.av --verify-mode auto -o /tmp/law-auto-lean
cd /tmp/law-auto-lean && lake build
```

You need a local Lean 4 toolchain (`lean` + `lake`). Aver generates the project but does not ship Lean itself.

`String.toLower` and `String.toUpper` use Aver's full Unicode definitions. Lean's native case functions cover only ASCII, so the exporter uses the same VM-checked mapping and context tables as wasm-gc. That covers expansions (`ß` becomes `SS`, `İ` becomes `i` plus a combining dot) and final sigma. `because` expressions that are definitionally equal can close by kernel reduction, which adds no `native_decide` assumption to universal laws.

## What it generates

The output is a Lean 4 project:

```
out/
  lakefile.lean
  lean-toolchain
  <Project>.lean
```

Each Aver module, the entry module included, emits its definitions inside the matching Lean namespace. A function `decode` in module `Packet` becomes `Packet.decode`. Theorem-skeleton declarations follow the same rule, for example `Packet.decode_verify_1`.

Every file, the entry included, imports the whole dependency closure, because each file has to compile. It `open`s only the module's own direct `depends`. The emitter writes every cross-module name it produces with its module path. A function gets the path of the module that declares it. A user type declared in another module gets its owner's path wherever it appears: in signatures, record fields and constructor expressions (`A.Fraction`, `X.Shape.circle`), and a capability resource is spelled the same way (`Kv.Handle`). A module's own types keep their bare names. This is what lets the following resolve: a type that a direct dependency only re-exposes, two opened modules that declare the same type name, a user type spelled like a Lean root type (`Sum`), and the resource of a capability whose operation the entry only threads through. It also lets a match binder share a name with a function that some distant module exports. A function name declared by two direct dependencies is hidden from both `open`s (`open Script hiding named`), members of a `mutual` block included. The Option constructors are spelled `Option.some` / `Option.none`, and a user function or binder called `some` or `none` is renamed `some'` / `none'` (as `id`, `max` and `min` are), so neither ever collides with the root alias.

Every emitted file sets `autoImplicit false`, including `AverCommon.lean` and the SHA-256 model. A type name the export leaves unresolved is therefore a build error (`Unknown identifier`). It never turns into an implicit type variable that Lean binds on its own, which would leave a theorem quietly saying something else.

## Scope

- exports pure core logic: types, pure functions, and decisions
- emits lifted pure forms for classified effectful functions used by Oracle laws
- skips unclassified effectful functions and `main`
- turns colocated `verify` / `verify law` intent into Lean proof artifacts

Oracle-lifted laws make effects explicit as theorem parameters. For example, `given rnd: Random.int = [fairDie]` becomes a proof-side oracle argument with the derived `Random.int` oracle signature. See [oracle.md](oracle.md) for the classified effect set, stub signatures, Oracle law syntax and trace assertions.

Lifting happens per module, so dependencies are lifted as well as the entry. A dependency file exports the effectful functions that some claim reaches in the same way the entry does: inside its own namespace, with the same leading `(path : BranchPath)` and one oracle parameter per classified effect. So `Infra.Store.get` becomes

```lean
def get (path : BranchPath)
        (rnd_Infra_Kv_get : BranchPath -> Int -> String -> Except String (Option String))
        (store : Store) (key : String) : Except String (Option String)
```

and a call site anywhere passes that callee's own oracle list: `Infra.Store.get path rnd_Infra_Kv_get store (heightKey height)`. A tail call carries the same arguments. Two effectful functions that call each other are exported as one `mutual` block instead of a forward reference. A lifted signature spells the types its effects carry by their owner module (`Tcp.Connection`, `Bytes.Bytes`). That owner is usually a standard module the source never lists in `depends`, so a file that exports a lifted function also imports the owners of the constants it names, next to its written `depends`. Which functions a dependency exports is decided by its consumers. The roots are the qualified calls spelled by the entry's `verify` blocks (its laws, plus its examples under `--examples`) and their cones, closed over the calls those functions make inside their own module. A function that no claim reaches is still not exported. A loop that reads the console forever has no Lean meaning, and nothing is being proven about it. A program that proves nothing about a dependency's effects therefore exports exactly what it did before.

## Verify emission

`aver proof` exports every law (`verify ... law`) with its samples, `because` and `using`. The examples, `verify` blocks without `law`, are exported only with `--examples`. Without the flag an example is not in the export at all: it is not a sorry, not a declined claim, and it roots nothing, so an effectful function that only an example calls is not lifted either. `aver verify` still runs every example. A `--gate` baseline holds laws only, so it gates a run with or without `--examples` the same way.

With `--examples`, `verify` blocks become Lean proof obligations:

- default (`--verify-mode auto`): `example : <lhs> = <rhs> := by decide +kernel` when the case's whole closure is known to reduce in the Lean kernel, `:= by native_decide` otherwise
- fallback (`--verify-mode sorry`): `example : <lhs> = <rhs> := by sorry`
- theorem stubs (`--verify-mode theorem-skeleton`): named `theorem ... := by sorry`

A case whose left side carries `?` states a `Result` computation. The left side is emitted inside a Lean `do` block so `?` short-circuits on `Result.Err`, and the expected side reads `Except.ok <expected>`. A case whose `?` hits `Err` fails under `aver verify`, and this shape keeps it false as a theorem too instead of continuing with a default value. Cases without `?` are unaffected.

A law body with `?` takes the same shape in all three statements a law emits: the quantified theorem, `<law>_checked_domain`, and every `<law>_sample_N`. The three therefore agree on what the law claims. The quantified form reads `∀ <givens>, (do pure (<lhs>)) = Except.ok <rhs>`. It asserts that the `?` reaches `Ok` for every value of the givens, beyond the sampled ones. Laws without `?` are unaffected. A law with `?` is not offered to later laws as a rewrite rule, because its theorem relates `Result` computations and not the values the source mentions.

A case of a function that performs an input effect, with no `given` for it, states its equation for every implementation of that effect: the function's oracle is a parameter of the statement. It is proved by `simp +decide` over the case's call cone, followed by `native_decide`. An opt-in simplifier proves closed string, byte-list and boolean computations by native evaluation before descending into their intermediate values or selecting the concrete branch. Container constructors are exposed afterwards by kernel reduction, so recursive container helpers cannot cycle through the pre-order step. This lets hex parsing, UTF-8 keys and fixture lookups eliminate the unused oracle. Every native replacement carries a proved equality, uses the same evaluation trust as an ordinary sampled case, and refuses free variables or metavariables. Universal law strategies do not enable this simplifier. A large fixture can still exhaust proof resources, and a branch that reaches kernel-opaque partial recursion can leave the symbolic oracle in the goal. When `aver verify` passed such a case, it is emitted as a named theorem, `__aver_verify_<fn>_<N>`, behind the same isolation guard as a law's proof (see `isolated_errors` below). `--check` charges an unfinished proof as a sorry and names it in `isolated_errors` and `isolated_cases`, and the rest of the module still builds. A case the VM failed, or never ran, stays a plain `example` outside the guard and fails the build at any sorry budget. So does a case with no symbolic oracle. The theorem name begins with `__`, which no source identifier can, so no user function can take it.

A case whose compared value has a large type, such as a `Result` of a tuple of maps, lists and records, is emitted under `set_option synthInstance.maxSize 4096 in`, so Lean can compose the `DecidableEq` instance the case is decided through.

The tactic is picked per case, conservatively. `decide +kernel` costs no trust: the axiom closure stays inside Lean's core three, with no `Lean.ofReduceBool`. It only works when everything the case mentions unfolds in the kernel, so a case goes there only when the emitter can positively establish that. Everything else stays on `native_decide`: a `Float` anywhere in the closure, a fn this export spelled `partial def`, a mutual group, numeric recursion anywhere in the call cone, a case whose expected side is not a VM ground-truth literal, or a case whose emitted equation is larger than the term budget. Numeric countdowns, recurrences and sequences growing toward a numeric bound stay native because a short equation (such as the length of a generated string) does not bound intermediate work, and kernel reduction has no heartbeat limit. Structural traversal retains its existing kernel path. This routing does not change universal law statements or their proof strategies.

The default automatic mode is stricter at one boundary. Suppose a sampled `verify` case or law transitively reaches a mutual-recursion fuel fallback whose seed is not a proven bound, because a peer receives an opaque computed successor. Then the claim is declined instead of evaluated. Lean's `panic!` returns `default`, so `native_decide` could otherwise certify exhaustion as a real result. The generated file carries an explicit refusal comment, and `aver proof --check` reports and charges the structured `declined_claims` entry naming the affected functions. Explicit `sorry` and theorem-skeleton modes still emit their visibly unproven obligations. Fuel derived from a validated Int countdown, a string-position bound or an ordinary ranked structural measure keeps the existing native-evaluation path.

`String.split` is kernel-eligible only when its separator is a literal containing zero or one Unicode character. A longer or unknown separator uses native evaluation: that branch calls Lean's `String.splitOn`, which does not reduce in the kernel.

The same refusal covers pure capability operations owned by a provider. Each is exported as `noncomputable opaque`, with a proposition-only `Nonempty` witness and never `Inhabited`, so no default value can stand in for the provider. Every function whose call cone reaches one is emitted inside a `noncomputable section`. A sampled case or law whose cases, `given` values, template or `when` guard reach such an operation is declined as a whole instead of being evaluated. Functions and claims that never touch a capability operation are emitted exactly as before, even in a module that also holds part of a provider cone.

`Vector.get` and `Vector.set` test a negative index before `Int.toNat` narrows it to `0`, so the exported model returns `Option.None` there, as the runtime does, and both reduce in the kernel. String code-point lookup is total on both sides (`String.firstCodePoint("")` is `Option.None`).

Every Aver `Int` is a Lean `Int` in the export. A bare Lean numeral with no expected type is a `Nat`, where `0 - 1 = 0`, so an `Int` literal is written `(n : Int)` unless Lean already expects an `Int` at that position: an argument of a user function, a field of a user record or constructor, or an operand next to a variable, field or call of type `Int`. The `*.len` and `String.byteLength` builtins cast their `Nat` result to `Int` the same way.

`verify ... law ...` always emits expanded sample theorems from `given` domains:
- `theorem ..._sample_n := by native_decide`

The universal law theorem is always emitted as `theorem <fn>_law_<name> : ∀ ..., lhs = rhs := by ...`. Its body is:
- a real auto-proof when the law shape matches one of the supported strategies (see `Conservative auto-proofs currently cover:` below), or
- `sorry` with an inline comment (`-- verify law is sampled; universal proof must be provided manually`) when no strategy matches

The per-sample and `_checked_domain` conjunction theorems are always emitted next to it as kernel-checked `native_decide` evidence. The proof obligation is real and visible even when the universal body is `sorry`.

A `when` clause becomes one premise: the law is stated for every input, `∀ givens, when_expr = true -> lhs = rhs`, never only over its sample domain (the samples are what `_checked_domain` and `_sample_N` state). Generic auto-proof strategies still run on these guarded laws. A guarded law whose proof comes from a strategy that targets its shape is a claim, and a proof Lean refuses is a sorry, like any other law. A guarded law that only a speculative strategy tries (a laws-as-lemmas `grind` over earlier laws, a multi-citation composition, a transparent premise chain, a single-list conditional induction), or that no strategy matches, is an attempt: when Lean does not close it, `--check` declines the law. It is reported in `declined_claims` as not proved for every input, charged against `--declined-budget` and not against `--sorry-budget`, and `aver verify` keeps checking its samples. Parser/render roundtrip laws such as `parse(render(x)) = x` usually land there unless some other generic shape discharges them first. One `lake build` decides it; there is no separate probe build.

When `law <ident>` names an existing pure function and the law body compares `foo(args)` against `fooSpec(args)`, Aver treats it as a canonical spec reference:
- the generated theorem/comment uses the canonical `<fn>_eq_<spec>` naming
- `aver context` also records `fibSpec` as a spec for `fib`
- in `--verify-mode auto`, the universal theorem body is auto-proven when the law shape matches a strategy; otherwise it is emitted with `sorry` and a comment for the user to fill in

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

Automatic proof is still the default. When it cannot put an argument together, a law can supply ordinary pure Boolean expressions with `because` and pick lemmas with `using`:

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

[law_reasons.av](../tests/fixtures/law_reasons.av) is the complete executable example. [law_reasons_digits.av](../tests/fixtures/law_reasons_digits.av) composes four helper laws to prove that a base-seven digit representation is minimal. Reason functions follow normal Aver semantics and stay normal functions, with the usual `check` guidance. Boolean local bindings have no separate assertion meaning.

Several `because` lines are ordered steps. Take original guard `H`, reasons `R1 … Rn` and claim `P`. The backend proves `H → R1`, then each `H ∧ R1 ∧ … ∧ R(i-1) → Ri`, and finally `H ∧ R1 ∧ … ∧ Rn → P`. Without `when`, `H` is true. The exported original theorem still states `H → P`, and its audited dependency chain includes every explanation, so an easy original claim cannot hide a failed reason.

The backend uses the case structure of reason functions to keep branch equations, including nested matches exposed by checked induction. Cases are split before Boolean conjunctions become sequential goals, so the right-hand fact keeps its branch's premises and can use the checked left-hand fact. Local bindings can expose more cases but do not become assumptions. [law_reason_match_branches.av](../tests/fixtures/law_reason_match_branches.av) shows a chunked list traversal and a false strict-positivity control. After splitting, the backend applies the available lemmas and ordinary automation. `because true` leaves the work to the final implication. `because false` fails the first obligation. Restating the claim as its own reason moves the work into that reason. All three are legal, and none of them skips proof checking.

`using [function.law, Module.function.law]` selects a set of lemma names; order in the list does not matter. Selected lemmas stay available in every explanation obligation and in the final implication. Without `using`, selection is automatic, and `using []` selects none. Local forward references are allowed. Imported laws must be exposed by their subject's module. Unknown names and dependency cycles are errors. Explicit citations keep theorem scope and the usual transitive axiom audit, so samples and `sorry`-tainted theorems cannot give universal credit.

A law guided by `because` or `using` stays citable when each `given` has only one example. Those examples do not restrict its universal statement. Proof credit still depends on the checked theorem and its dependencies.

Typed empty list samples keep their concrete checked element type in Lean, also when nested inside polymorphic list operations. Lean infers unresolved generic element types from context.

`aver verify` and `aver verify --hostile` run every reason under the original guard, with the same sample expansion as the claim. These checks do not need Lean. `aver proof --check-json` and `proof_manifest.json` also report separate `obligations`, identified by `<function>.<law>.because1`, `.because2` and `.implication`. Obligations do not count toward `universal_laws`. When the solver provides residual goals for failed explanation obligations, `--explain` includes them. An implication can be universal while its reason and the original law stay failed.

For `using` and `because`, nonrecursive functions contribute their full kernel equation, so the solver sees every match alternative, including a lookup's default branch. Equations of checked structural list functions and native subtractive countdown functions in the law's dependency cone are available to `grind` even without a recursive explanation. They stay out of recursive `simp` rewriting. Native mutual functions expose their kernel-generated equations through the same selection that emits their checked definitions, including computed-list descent. A fuel fallback exposes no original-function equation. The solver also uses Lean's existing guarded `List.take`/`List.drop` equations and the conversion of nonpositive `Int` counts to zero, which matches Aver's slice semantics. Floor-division countdowns stay out of recursive rewrite sets. If the earlier solver attempts leave a goal open, a finite sequence of one-step kernel equations can expose the next step. The explanation equation also unfolds in hypotheses, while other countdown equations unfold only in the goal so the induction summary is preserved. Fuel helpers stay opaque. Cited laws can summarize them without importing fuel equations.

Recursive explanations can also provide an induction plan when the function has checked structural descent on a list or a native `Int.toNat` countdown, including checked floor-division descent. A recursive call contributes an induction hypothesis at the smaller measure. Its Boolean result must still establish the facts the next step needs. Original guards and earlier reasons stay premises of that hypothesis, so they must hold at the recursive arguments. If Lean cannot build a functional induction principle for a local match, the backend falls back to well-founded induction on the same checked list length, generalizing the other givens and keeping all original premises. That hypothesis covers any shorter list, a computed slice included, and the slice must satisfy the original guards and earlier reasons at the recursive arguments. Ordinary functions and explanations share one termination contract: a shrinking list wins over a sibling counter's fuel model.

A single recursive function also reuses the shared cycle analysis for `List.drop` or `List.take` of a known cons-tail. The slice may equal that tail and is still shorter than the original list. This relies on the existing length contract and native emitter, also when another list accumulator grows. The analysis tracks origins through ordinary bindings, match aliases and nested slices. Singleton and mutual Lean definitions share the same kernel-checked length proof. Slicing the whole input does not establish strict decrease. [law_reason_singleton_list.av](../tests/fixtures/law_reason_singleton_list.av) shows direct induction over a chunked traversal without a separate step list, next to nondecreasing and growing-call controls. [law_reason_slice_aliases.av](../tests/fixtures/law_reason_slice_aliases.av) covers alias propagation, mutual recursion and a false guarded explanation that must stay unproved.

For example, the explanation can establish the property for `rest` before building it for the whole list:

```aver
fn appendReason(items: List<Int>, suffix: List<Int>) -> Bool
    match items
        [] -> count(List.concat(items, suffix)) == count(items) + count(suffix)
        [head, ..rest] -> Bool.and(appendReason(rest, suffix), count(List.concat(items, suffix)) == count(items) + count(suffix))
```

[law_reasons_recursive.av](../tests/fixtures/law_reasons_recursive.av) has the complete laws, including a changing counter, multiple reasons, and deliberately false base and step cases. This first induction path accepts direct calls on distinct law parameters, imported functions included. Composite arguments, mutual recursion, unchecked recursion measures and wrappers around recursive explanations do not select it yet. A call on the unchanged list produces no checked induction hypothesis, and passing declared samples cannot replace that proof.

[law_reason_integer_descent.av](../tests/fixtures/law_reason_integer_descent.av) exercises division by three, a false bound, a guard that fails at recursive arguments and an opaque nondecreasing function. [K5 Binade](../projects/k5_fdiv/domain/binade.av) uses the same mechanism to bracket its executable binary exponent, negative exponents included. Named Boolean facts are expanded before contextual simplification, so their arithmetic content is kept.

Explanation obligations are universal. A stage the backend cannot prove stays failed, and sampled evidence is never quietly used in its place. The syntax is provisional. [#1288](https://github.com/jasisz/aver/issues/1288) tracks the mechanism and acceptance checks.

## Specs over invariants

This is the proof style Aver is designed for:

- the author writes a simple pure spec function
- the author writes `verify impl law implSpec`
- the proof backend tries to connect implementation and spec

The aim is a surface language that does not put proof engineering first.

Invariants still exist as a proof concept, especially for optimized implementations such as tail-recursive helpers, parsers with state or accumulator-heavy code. Aver tries to push them down into the proof backend whenever it can, so users do not have to write them first.

Countdown induction also uses concrete lemma applications found in shared `ProofIR`. The search keeps the actual recursive accumulator, expands the source step inside the claim, and matches earlier ordinary laws against the resulting terms. Lean introduces admitted applications as local facts and normalizes sequence composition before unfolding a recursive consumer. It splits the source branch before trying the recursive premise. For example, `n >= 0` gives `n - 1 >= 0` only in the positive branch. It drops reflexive instances, which add no information and can loop as local simp rules. See the [roundtrip example](../tests/fixtures/source_recursion/roundtrip.av). The [Boolean counter](../tests/fixtures/source_recursion/boolean_counter.av) checks the same composition with a different list element type and subtractive descent.

In short:

- user-facing Aver should prefer explicit specs
- the proof backend should absorb invariants where it can
- explicit invariant reasoning should be the exception in the workflow

If Aver cannot auto-prove the universal law shape in `--verify-mode auto`, the universal theorem body is emitted as `sorry` with an explanatory inline comment. The per-sample and `_checked_domain` theorems are still emitted next to it as kernel-checked evidence. The proof obligation stays visible (and Lean will reject `lake build` until the user replaces the `sorry`), but the file compiles, so the case-level evidence is useful before anyone closes the universal.

See [How the auto-prover decides](#how-the-auto-prover-decides) for the recognizer families currently covered.

The generated Lean prelude also includes one-character separator lemmas, such as `AverString.split` over `String.join(_, sep) ++ sep` for separator-free parts. These helper lemmas support exported code. They do not claim that delimiter-based parser/render laws are universally auto-proved.

## How the auto-prover decides

Proof time involves no proof search and no AI. Each `verify law` goes through a fixed, deterministic decision tree of shape recognizers. The first recognizer that matches pins a strategy, and each strategy emits a fixed tactic script. The Lean kernel judges the result; the recognizer does not. A law is credited `universal: true` only when its theorem's `#print axioms` stays inside `{propext, Classical.choice, Quot.sound}`. In particular, `native_decide` proofs never count, because they trust the compiler's evaluator via `Lean.ofReduceBool`. A strategy that does not close degrades to a caught `sorry`, never to a false proof.

The recognizer families, roughly in routing order:

- **syntactic algebra**: reflexive, commutative / associative / identity / anti-commutative wrapper shapes over `Int` operators.
- **spec equivalence**: implementation-vs-spec laws closed structurally, by conservative `simp` cleanup, or by `omega` over linear `Int` arithmetic (including second-order recurrences with a pair-state tail-recursive worker, and `Int.max`/`Int.min` shapes).
- **induction**: structural induction on list/ADT givens, with generalizing variants (`induction xs generalizing n`) for accumulator-threaded and both-arguments-peeling shapes, and arm-level injection of bridge and sibling lemmas. Each arm of a `when`-law also tries a subject-first closer that needs no hypothesis from the induction. It unfolds the law's own function, splits its guard, unfolds the rest of the cone, splits the residual `if`s, normalizes, and hands the linear leftovers to `omega`. This fits a law whose function chooses between two list layouts and whose reader has to match on that choice. A `when` that bounds the list's length from below by a literal (`List.len(items) >= 2`) also gets one `rcases` in the cons arm, exposing exactly the further conses the premise guarantees. A claim that reads fixed positions of the list (a stack shuffle through `itemAt`, a `List.take` of that many items) then evaluates by `simp_all` with no induction hypothesis at all. A single-list conditional law is stated universally only after a probe build of the file has shown that its portfolio closes; otherwise it keeps its sampled statement. A hard error in one candidate's probe (a heartbeat timeout in one arm) costs only that candidate, and the run says so. `AVER_SPECULATIVE_LOG=<file>` keeps the probe build log. A bare call name in a law means the function of the law's own module, whatever other modules of the program call theirs. When the program is proven from its root entry, a dependency's laws are proven against that dependency's own functions and blocks.
- **Map laws**: shipped `Map` lemmas (self-key and general-key) plus a map-fold homomorphism recognizer. A law whose call cone stores into a map gets the facts the compiler ships about `Map.set`: the value read back under the key just stored, a read under any other key, membership under the key just stored, two stores under one key collapsing into one, the size never shrinking, and, when the map already holds the key, the size not changing at all. A cone that calls `Map.remove` also gets the fact that removal never grows a map. The store need not be the outermost call. A store written into a field of a record the cone rebuilds, `R.update(r, field = Map.set(r.field, k, v))`, is reached the same way; that is the usual form of a one-key update to a table held in a record. The membership premise of the size fact comes from wherever the law already has it: a `when`, or a `because` step that establishes it. The model behind these facts is a key-sorted association list with a key-canonical `set`. Storing under a key the map already holds replaces that entry in place, and storing under a new key inserts it in order, so no map the model can reach holds a key twice.
- **ground enumeration**: laws over fixed enum/ADT constructor arguments, and laws where every given ranges over a finite domain (`Bool`, fieldless enums). These use exhaustive `cases` plus `rfl`/`decide`, which computes straight through fuel wrappers on closed values.
- **ring identities over records**: unconditional algebra laws of records with all-`Int` fields (for example exact rationals compared by cross-multiplication, `examples/data/rational.av`). After the call cone is unfolded, both sides distribute and AC-normalize to the same polynomial through a fixed package of core `Int` ring lemmas. Commutativity, associativity, distributivity and the negation/subtraction laws all close kernel-genuine, without Mathlib.
- **builtin facts**: laws whose call cone bottoms out in builtins close by `simp` over the cone plus prelude spec lemmas the compiler ships (for example `Int.fromString_fromInt` and `String.slice` facts).
- **floor-division windows**: laws over a power-of-two function, a floor-halving binary-exponent search (`Int.div(a, 2)` recursion; the literal divisor discharges to plain `Int`), and the power-of-two window predicates built from them. That covers positivity and the sum homomorphism of the power function, the scaled-significand window (`2^(n-1) <= sig(a, b, n) < 2^n` under `when b >= 1; a >= b; n >= 1`), and the bit-width product window. These `when`-laws are stated and proven in true universal form. The emitted proof combines functional induction over the well-founded definitions with the core floor-division bridges (`Int.le_ediv_iff_mul_le`, `Int.ediv_lt_iff_lt_mul`), kernel-genuine and without Mathlib. See `tests/fixtures/floor_window.av`. These windows reproduce the integer-arithmetic core of a classic correctness proof for a hardware floating-point divider. Picking a known-hard result is deliberate: it measures how much of such a proof the engine carries on its own. The same setup is meant to serve beyond this one theorem. The next targets are the self-hosted interpreter's own evaluator core and an exact-decimal arithmetic library for regulated computation.
- **synthesized lemmas about your functions**: when a function matches a conservative shape gate (for example the canonical string-position scanner), the compiler synthesizes and kernel-proves a companion lemma about it and uses that lemma in higher strategies such as the decimal render/parse roundtrip. If the gate does not match, nothing is emitted.
- **escaped-string roundtrip**: the canonical escaped-string parse/serialize roundtrip (JSON-style `parse(escape(s)) = Ok(s, …)` laws) is certified only when the escaper's control-character threshold is a literal between 1 and 32. That is the JSON control range, and the kernel proof over the 16-branch hex-escape ladder relies on the bound. Any other threshold falls back to sampled evidence.
- **fuel induction over a well-founded countdown**: a law about a function exported as a native `termination_by param.toNat` def is proved by induction on a `Nat` fuel that bounds that argument. The induction hypothesis is instantiated at the function's own self-call arguments, the countdown function is unfolded exactly once per fuel step, and the closer's simp set never contains that function (its unconditional unfold equation would loop). Earlier laws about the same countdown are cited as ground instances at the shrunk arguments, not as rewrite rules. An earlier `when`-law is an implication, so its instance carries the premise, discharged there by `omega` from the consuming law's own premise (`value / 256 < 256` from `value < 65536`). If the premise does not hold at the shrunk arguments, the citation is dropped instead of assumed. This is how a bound on the length of a countdown writer's output certifies from the bound one digit below it. The bottom rung of such a ladder has no earlier law to cite, and its own hypothesis is one step too weak, so the step's closer keeps one last attempt behind the others: a second unfold at the shrunk arguments, which reaches the countdown's base branch.
- **claim-shape arms in core Lean**: four cheap arms keyed on the shape of the claim, never on a particular function. A literal bit mask (`Bits.and(x, 128)`, `Bits.and(v, 4259839)`) gets a closed form per mask, `∀ y, AverBits.and y L = Σ 2^a * (y / 2^a % 2^n)` with one term per run of ones in `L`. It is proved by a sign split and five bit lemmas that ship in the `AverBits` prelude (`and_of_nonneg`, `and_of_neg`, `nat_land_low`, `nat_land_bit`, `nat_land_split`, all proved in core), and the claim is then closed by `omega`. A function that matches an `Int` on literals and is compared with range predicates gets `split <;> simp_all <;> omega`. A `when`-law whose claim, premise or called cone multiplies two non-constant terms gets the core sign lemmas for each product (`Int.mul_pos`, `Int.mul_nonneg`, `Int.mul_nonpos_of_nonneg_of_nonpos` and the rest) and the nonnegativity of each square as hypotheses, then `omega` or `grind`. A `when`-law over a cone that branches gets `grind` over the unfolded cone, alone and after `simp`; this includes a claim that reaches its function below a comparison or an equation (`place(s, n).state.used <= capBytes()`, `(unheard(…) == Option.None) => …`). The `when` arms are part of the speculative probe, so a law they do not close keeps its sampled statement.
- **fallback**: bounded evidence only. That means per-sample and `_checked_domain` theorems via `native_decide`, plus the universal theorem with an explicit `sorry`. Structured proof portfolios share a final composition attempt for unconditional laws: `grind` over earlier laws about functions in the call cone, with recursive functions kept folded. This attempt comes after the strategy's own alternatives, including prelude simplification, so a wrapper can use its callees' laws even when the first strategy cannot close it. Guaranteed closers keep their original portfolios. `when`-laws get a guarded-domain enumeration instead. Their bounded statements carry an explicit statement class and are never credited as universal.

When the call cone contains a well-founded countdown, composition and fuel induction share one final equation-based attempt. It gives `grind` the countdown equations, nonrecursive definitions and earlier laws, including laws about the current subject. Lean bounds equation instantiation by term generation, and the equations never enter recursive `simp` rewriting. This closes finite observations such as dropping the first digit or inspecting the leading digit, with no new recognizer for either law. Structural recursive callees stay folded.

When a law compares a structural list worker with an `Int` accumulator against a wrapper of another worker at the same literal seed, the exporter unfolds the wrapper before functional induction. Both sides then expose that seed, and the induction hypothesis follows the updated accumulator at each recursive call. This also reaches private imported workers. The proof uses kernel-checked equations and must close completely; different worker updates or initial seeds receive no automatic proof credit.

The conditional list-layout arithmetic step also splits Boolean givens before unfolding the layout and reader. Comparison normalization runs separately with `simp only`, so its negation rules cannot loop against the default simplifier.

When the recipes run out, you escalate by writing more Aver, not Lean. Split the hard law into helper laws. Each one is a runnable test in milliseconds, and once proven, the laws about the functions a law calls become rewrite rules for it, wherever they sit in the file. The export declares a law's theorem before every law whose call cone reaches its function, so a helper's law may come after the law that needs it, and `aver format` may move blocks freely. A proven helper law committed in scope is picked up automatically by the law below it (the `SimpOverLemmas` feedback loop). The `the-method` agent loop can propose those helper laws for you.

Termination gets the same treatment. Structural recursion over your own types and recognized well-founded shapes (for example quicksort's mutual recursion) emit genuine total definitions. The remaining recursive shapes are emitted fuel-wrapped, with the fuel budget derived from a synthesized size measure of the call-site arguments.

## Proof steps

Before any recognizer, the compiler tries to write a law's proof as steps: proof data in which every step names one rule from a closed list and writes out each instance, intermediate term and rewrite position. Lean checks the term rendered from the steps, and `aver proof --backend aver` checks the same data with the kernel written in Aver, without Lean. The rules are judged in one place, the kernel in `tools/proof-kernel/`: the compiler runs it on every step proof it writes before any backend sees the proof, and keeps no second copy of the rules.

The compiler writes only certain steps. A step is certain when the compiler makes no choice that the outcome depends on, or when the author made that choice in the source: in the law itself, in its `using` list or in its `because` lines. When a producer cannot go on it stops with a refusal that names the place: the two sides where it stopped and the hypotheses in scope (set `AVER_STEPS_DEBUG=1` to print them). A refusal asks for the law to be split there into helper laws. It is not a sign to search harder. The few limits that remain (evaluation steps, the depth of Boolean splits, the size of a proof) are guard rails, and reaching one is reported by name like any other refusal.

What the compiler does without being asked is mechanical: it opens a definition at the arm the value of its subject selects, substituting local bindings in order; it computes closed arithmetic and comparisons; it splits on a Boolean the evaluation stopped at; and it splits a given of a finite type (`Bool`, a type whose variants carry nothing, records and tuples of those) into every one of its values.

Where evaluation stops at a call whose `match` reads a subject that is not a constructor yet, and the subject is of a sum type (its variants may carry fields), an `Option`, a `Result` or a list, the compiler splits on that subject: one case per constructor of its type, in the order Lean declares them (`[]` before `[h, ..t]`, `none` before `some`, `error` before `ok`), each under the hypothesis that the subject is that constructor applied to fresh names. The cases are the ones the `match` reads, so nothing is guessed; a catch-all arm is chosen by the constructors the arms before it exclude. A given of a finite type, or a field of one, is split into every value as above instead. A goal splits only on a function that does not recurse, where induction is not the step; a hypothesis that calls a function (a recursive `because` reason, an induction hypothesis about the tail) splits on the subject its `match` reads, so the hypothesis evaluates in each case. In each case every hypothesis about the subject is stated again about the constructor, a comparison evaluated, so it still applies once evaluation has read the subject as the constructor, and a hypothesis that now evaluates to the other Bool closes the case as impossible. The kernel written in Aver checks that the term split on is the subject of the named function's `match` at the given arguments, which gives it the type the patterns have, that there is one case per constructor of the type (for a list, the `[]` and `[h, ..t]` cases; for a sum, every constructor of a sum the script declares from the program, in order and with its number of fields, not only those an arm names), that the names are fresh, and that every case proves the same equation, which names none of them. The script declares each sum type a split reads in its `sums` section, as it declares the definitions, and a constructor declared in two sums is refused. Lean checks the split as `T.casesOn` with the motive `on = x → claim`, so it holds only for the constructors of the actual type. A pattern that ignores a field (`Option.Some(_)`, `[_, ..rest]`) is opened like one that names it: the wildcard field gets a fresh name of its own.

Where the arms of such a `match` are Int, text or Bool literals followed by one last catch-all arm, the split follows the arms instead: one case per literal arm, under the hypothesis that the subject is that literal, and one case for the catch-all, under one hypothesis `(subject == k) = false` for every literal `k` before it. The compiler chooses nothing: the cases are exactly the arms. A catch-all arm is opened at a subject that is not a literal only where hypotheses in scope state the subject differs from every literal arm before it, whether they come from this split or from Bool splits on `subject == k` the evaluation made. The kernel written in Aver reads the literals off the arms, so it checks that every arm has its case, that the catch-all case names one hypothesis per literal, and that these names are fresh. Lean checks a literal split as one `AverSteps.bool_cases` on `subject == k` per literal, the `true` branch turned into `subject = k` by `eq_of_beq`, and opens the catch-all arm with a lemma that takes the hypotheses `(subject == k) = false` as premises.

Induction follows the recursion of the function the law is about, the `f` of `verify f law …`, and no other. When `f` matches on one parameter and every recursive call passes there a part of the value its arm matched (the termination gate a recursive definition must pass before a step may open it at all), and the claim applies `f` to a given at that place, each arm of `f` becomes one case: the given becomes the arm's pattern, and each recursive call in the arm gives one hypothesis, the claim at that call's arguments. Other givens among `f`'s arguments vary with it, which is what an accumulator needs. When the case stops at a call of `f` on the same part with other values of those givens, as the right side of `revOnto(xs, acc) => List.concat(revOnto(xs, []), acc)` does at `revOnto(t, [h])`, the claim there is a hypothesis too: the induction proves the claim for every value of the varied givens, so the hypothesis holds at any of them. Only values that a call of `f` on that part carries in the stuck term are taken, and none whose right side, evaluated, holds its left side again. Lean checks the cases against `f.induct`, the induction principle it derives from `f` itself; the kernel written in Aver checks them against `f`'s arms and recursive calls. Each case is closed by evaluation, where the hypotheses and the laws the `using` list cites rewrite what evaluation stops at. When `f` does not recurse, the refusal names the functions in the claim that do; state the law about one of them. When the claim passes `f` different givens at the place it recurses on, as `append(append(xs, ys), zs) => append(xs, append(ys, zs))` passes `xs` to one call of `append` and `ys` to another, the law names the one to follow with an `induction xs` line, and the induction takes the first call, outermost first on the left, that passes it there. Without that line steps refuse and list the givens to choose from; a named given that no such call passes is refused too. Steps never pick one on their own.

An arm of `f` may split again around its recursive call: `match x < h` before inserting, or `match y` on another parameter before recursing on both, as `minus` and `drop` do. The cases are still one per arm of `f`, and a call's hypothesis may read only what the arm's pattern and the parameters bind at the place recursed on and at the varied places; a name the inner `match` binds may sit anywhere else. Lean's `f.induct` splits such an arm into more cases, so Lean checks these steps with the recursor of the given's type instead (`List.rec`, `Nat.rec`, `T.rec`), one case per constructor, each hypothesis the induction hypothesis of the part the call recurses on; the matches of such a proof are stated with `(generalizing := false)`, so a hypothesis about a case's names does not enter their motive. A `when` that mentions a given the induction varies is carried, like every other hypothesis that does: it holds in each case at the arm's pattern, and a recursive call's hypothesis is available once the `when` is proved at that call's arguments (the step proves it, by evaluation, before the case uses the hypothesis); a case may also do without a call's hypothesis. The recursor's motive is the claim for every value of the varied givens and under the carried hypotheses. A hypothesis that calls a function stuck only on its arguments' shape, `allPos(List.prepend(h, t))` or `isCons([])`, is evaluated where the claim stops: when it comes out as the other Bool the case cannot happen, and otherwise its `Bool.and` lines become hypotheses.

A function may also count an `Int` parameter `n` toward zero: a `match n <= 0` or `match n > 0`, the arm where `n` is at most 0 without a recursive call, and every call in the other arm passing `n - 1` or `n / k` in that place, for a literal `k` of at least 2 (`pow2`, `factorial`, a reason that walks a factor down, base-256 digits of a value). Where `n > 0` both are at least 0 and below `n`, so it stops for every `Int`, a negative one included. The calls of one arm may descend differently (`f(n / 2) + f(n / 3)`, or `n - 1` beside `n / 10`). A count down by more than one, a division by 1 or by a name, a `match n >= 0` or `match n == 0`, or a call in the arm where `n` is at most 0 is not opened. A step may open such a function only where a hypothesis states its comparison or its complement (`width > 0` for `width <= 0`); evaluation never splits on it, which would unroll one more level in every branch. One that divides opens also where linear arithmetic decides the comparison (`digit / 256 > 0` is false for `digit < 256`), and an attempt that opened it and failed is tried again with the call kept whole. The check that lets a step open the function also returns what it checked, the place of `n`, the comparison and each arm's calls, and a law about the function is proved by induction along it from that same description: one case per arm, the given `n` left as it is and the case's one name the hypothesis that the comparison has the arm's value; in the arm where `n` is above 0, one hypothesis per recursive call, the claim at the `Int` that call passes. Every hypothesis that mentions `n` or another given the claim passes the function, the `when` included, is carried: it holds in both cases, and a call's hypothesis is taken once they are proved at the call's values. Those other givens are generalised, so an accumulator that grows as `n` shrinks takes the value the call passes, and a further hypothesis may hold the claim at the `Int` a call passes and other values of them (the right side's call at an empty accumulator). No hypothesis exists in the arm where `n` is at most 0. Lean checks the cases against one prelude lemma, `AverSteps.int_measure_induct`, strong induction on `n.toNat` split on the comparison, with the claim for every value of the generalised givens and under the carried hypotheses as its motive; each hypothesis cites one small lemma that its call's `Int` is below `n` (`AverSteps.sub_one_lt`, `AverSteps.ediv_lt`). The kernel written in Aver checks that the given is an `Int` and builds the cases itself, from the arms and calls its own check returned. When evaluation is stuck under a hypothesis that calls a predicate choosing by a Bool (a reason that counts down, for example), that hypothesis is opened at the arm its guard selects, split on the guard when no hypothesis decides it, and the lines of that arm are cut in as hypotheses. A true `Int` equality lets one side stand for the other (the `int.eq.of_beq` rule): a hypothesis `x == 3`, or two comparisons that pin a variable to one literal from both sides, rewrite the variable to that literal. A hypothesis `f(x) == g(y)` between two terms rewrites its left side to its right side, for an `Int` and for every type whose `==` is equality in Lean too (`LawfulBEq`): `String`, `Bool`, lists, `Option`, `Result` and tuples of those, and records and sums that do not contain themselves, never a `Float` inside (`0.0 == -0.0`), a `Map` or a `Vector`; the rule is the same and Lean's `AverSteps.eq_of_beq` takes any such type, also where the term at hand is the body of `f(x)` for an `f` that only names another term (no `match`, no recursion), as `twoPow(n)` names `powerOfTwo(n)`; it is used only when no left side of a stated equality occurs in its right side, so one rewrite never feeds another. A comparison that mentions the left side of a stated equality is first decided as written, from the hypotheses that name it (`a != b` is false under `a == b`), and only then rewritten; `b != b` is false because `b == b` holds by the two orders.

Two arithmetic steps are decided rather than searched. Two Int terms that are the same polynomial over their atoms (anything that is not `+`, `-`, `*`, negation or a literal) are equal by the `ring` step: the kernel written in Aver normalises both sides itself, and Lean checks the same equation with the reflective normaliser its core already proves sound (`Lean.Grind.CommRing.norm_int`), without Mathlib. An Int comparison that the decided comparisons in scope settle is decided by the `linear` step: its opposite and those hypotheses, each read as `p >= 0`, add up with nonnegative weights to a negative constant. The producer finds the weights by Fourier–Motzkin elimination, first using each equality (two facts `p >= 0` and `-p >= 0`) to substitute one of its terms away, and writes them into the step; the kernel only checks that they add up, and Lean is given the same facts and its core `omega`. Text joining (`+` on `String`) and `Float` arithmetic are never read as Int arithmetic: the step data spells them `++` and `+.`, so no Int rule applies to them. Equality between values that may hold a Float (a Float, or a list or record of them) is spelled `==.` and `!=.`: `0.0 == -0.0` holds between values that differ and a NaN is not equal to itself, so no rule about equality applies to it.

A few more steps are read off what is in scope. A list literal with an element that is not a value (`[d]`) is evaluated to cells (`List.prepend(d, [])`), and a hypothesis or cited law written with the literal applies to the cells, by the same `cell` step. A `when` line that names a constant, a definition without parameters whose body computes to a value (`capBytes()`), is also cut in with the value, so the comparisons read it as arithmetic. Where evaluation has no Bool to split on, an undecided Int comparison inside `Bool.and`, `Bool.or` or `Bool.not` is split on. A value equals itself (`p == p` is true, the `bool.beq.refl` rule) for every type that holds no `Float`; Lean checks it with the derived `ReflBEq`. Two different constructors of one type are not equal whatever they hold (`Option.Some(t) == Option.None` is false, `Verdict.Invalid != Verdict.Valid` is true), and a constructor with no fields equals itself: the `compute` step reads that off the constructors' names when nothing in them is a variable, and Lean checks it by `decide`. A remainder by a positive literal is in range (`0 <= a % 256` and `a % 256 < 256`, the `int.mod_range` rule, Lean's `Int.emod_nonneg` and `Int.emod_lt_of_pos`): the `linear` step is given that fact for each such remainder in the comparison it decides or in the hypotheses it reads, and for each quotient or remainder `a / k * k + a % k = a` (the `int.div_mod_recompose` rule). A quotient whose dividend a hypothesis bounds by a literal, `0 <= a < m` with `m = n * k`, is also given `0 <= a / k < n` (the `int.div_range` rule), and so is a quotient of such a quotient, so `d % 256 == d` holds for `0 < d < 256` and two base-256 digits read back for `value < 65536`. Two sides that differ only in Int parts linear arithmetic shows equal are joined by `int.eq.of_beq` at each part, also inside a record, and such an `a == b` is true by `bool.beq.refl` once `a` is rewritten to `b`; not inside a record of a refined type, which Lean states with the proof of its invariant. A true Int equality in scope is also read as the two orders it implies, so a stated `x != y` that the orders make equal closes its case as impossible, and two sides that differ only in Int parts that are one polynomial (`(u + d) - u` and `d` inside a text) are joined by a `ring` step at each part.

Where nothing else applies, the calls inside an Int comparison the hypotheses decide are opened, so linear arithmetic reads their values instead of the calls: each call of a function that does not recurse and whose body is a `match` (`maxInt(a, b)` written `match a >= b`). A call evaluation takes a step on, its arguments literals (`maxInt(64, 64)`) or what its `match` reads decided by a hypothesis, is cut in as `(call == value) = true`, by `bool.beq.refl` once the call is evaluated. A call stopped at an undecided Bool is split on that Bool first, and the value of each case cut in; a call whose `match` reads a subject of a sum or list type, or an Int with literal arms, is split into the cases its arms read, as above. The cases are the `match`'s own, so nothing is guessed, and the kernels check the same `cases`, `unfold`, `have` and `linear` steps as everywhere else.

`using` is a set, in Lean and in steps alike, as it is in the language: the order of the list does not matter. Steps rewrite with the cited laws left to right, as each law is stated, trying them in the order they are defined and rewriting the outermost-leftmost position some law applies to. Where two cited laws rewrite the same position to different terms, the result would depend on which is tried first, so the producer refuses and names both; cite only one. A law whose right side holds its left side again with only its givens renamed, such as a commutativity law, would rewrite its own result forever. It is not used as a rewrite rule, and the refusal says so. A law whose right side holds its left side at smaller arguments, a recursion step, is used; if rewriting ever comes back to a term it already had, that is refused by name too. During evaluation, a cited law is not applied where its result holds the term it rewrote, or a call of a definition without `match` or recursion whose body is that term (`intoBytes(ops, [])` rewritten to `List.concat([], bytesOf(ops))` where `bytesOf(ops)` is `intoBytes(ops, [])`): evaluation would open that call and rewrite the same term again. A cited law that states an `Int` comparison and has no `when`, such as `pow2(k) >= 1`, is also a fact linear arithmetic may use: at each call in the comparison being decided that one of its own calls matches (`pow2(0 - k)` gives the instance `pow2(0 - k) >= 1`), so `pow2(0 - k) > 0` follows. A law with a `when` is used the same way only where its `when` at that instance is proved by what steps already prove (the hypotheses in scope, literals, linear arithmetic, the cited orders without a `when`); it is never assumed. Its comparison may also be stated through a predicate that only names it, such as `positiveProduct(a, b)` for `a * b > 0`, and a product in the law is matched against a product in the comparison factor by factor: `using [pow2.positive, positiveProduct.positiveFactors]` proves `pow2(0 - k) * pow2(n) > 0`, since both `when` conjuncts follow from `pow2.positive`.

A `using` list may also name builtin facts, such as `List.len.ofConcat` (`List.len(List.concat(a, b)) => List.len(a) + List.len(b)`). They hold for lists of any element type and for maps of any key and value type (`Map.get.afterSetOther`, `when k != k2`), their proofs are steps that travel with every citation and are checked before a step may use them, and Lean states each one once for every element type. `aver facts` lists them, and [builtin facts](builtin-facts.md) is the same list. Names starting with `List.`, `Map.` or `Vector.` are reserved for them; a fact about the order of a map's keys is not one, since that order depends on the key type. When evaluation stops at a term whose part a fact's left side matches, the report adds a hint under the law: ``hint: `List.len.ofConcat` rewrites `List.len(List.concat(xs, ys))`; add it to `using` ``. A hint matches the term as it stands, one fact at a time; the fact is not applied until the law cites it.

`aver proof --check` lists every law with what closed it: `closed by steps`, `closed by tactic`, or `not closed` followed by where the steps producer stopped. `--backend aver` prints the same for the steps it checks, and `--check-json` carries it in `closed_by`. A step proof the kernel written in Aver has accepted but Lean refuses is a bug in the producer or the kernel, not a reason to fall back: it fails the run and names the law. `tools/steps_ratchet.py` keeps both levels per law for the corpus (`tools/steps-baseline.json`), so a law that falls from steps to tactics, or a tactic portfolio withdrawn once steps replace it, is a visible change to that baseline. A law a change adds to the steps level was closed by the kernel written in Aver; on a pull request it is checked under Lean as well (`steps_ratchet.py --lean-gains`, only in the files that gained one), so the Aver kernel alone never earns that credit.

## What "kernel-genuine" does and does not cover

Kernel-genuine is a precise and narrow claim: the Lean kernel checked the proof of *the theorem as translated*. It certifies the tactics. It does not certify the translation. The Aver→Lean statement translator, the code that turns your `verify law` into a Lean proposition, is part of the trusted base. If the translator renders `Int.div` with the wrong rounding, or mistranslates a `when` guard, the kernel will still certify a true theorem about the wrong statement, and `verify` will still be green.

We state this openly. We are not building a verified translator; that is CompCert-scale work for a much smaller risk. Instead we rely on a check the corpus already provides. Every `verify` example runs twice: on the VM, and through the translation as an exported Lean `example`. Each such example is one point where the VM's semantics and the translated semantics must agree. It is a pointwise translation-validation test, and every example in the suite provides one at no extra cost. The negative-divisor rounding question above, for instance, is pinned by any example whose sample exercises a negative dividend.

The gap is coverage. Nobody has yet measured *which* language constructs and edge cases the dual-run corpus actually exercises. Work is in progress to make that measurable, as a construct × edge-case matrix against the existing examples. Until then, read kernel-genuine as "the tactics are sound and the statement matches the runtime on every point we have tested", and not as "the statement is provably the one you wrote."

## Proof mode

Recommended mode:

```bash
aver proof my_module.av --verify-mode auto -o out/
```

That combination means:
- regular `verify` cases become executable Lean checks: `decide +kernel` where the closure reduces in the kernel, `native_decide` elsewhere
- supported `verify law` shapes get real universal proofs
- unsupported `verify law` shapes emit the universal theorem with a `sorry` body and an inline comment, plus the per-sample and `_checked_domain` theorems as kernel-checked evidence
- recursive pure code inside the supported proof subset is emitted as total Lean defs
- unsupported recursive pure functions are called out explicitly and emitted with `partial` fallback

If a law lands on the `sorry` fallback and you want to know why, [transpilation.md → Debugging a law that didn't auto-prove](transpilation.md#debugging-a-law-that-didnt-auto-prove) describes the `--emit-ir-after=law_lower` workflow.

The current proof export supports:
- single-function `Int` countdown on an `Int` parameter (`n -> n - 1`). Closed-world fns (no `exposes` clause, or absent from the list) with the canonical `match p { L -> base; _ -> rec(p-1, ...) }` body emit as a native aux def. The aux def carries a precondition extracted from the `match`/`if` guards around the unique external caller: `(h_dom : n ≥ 0)` from `fib`'s `match (n < 0) { false -> ... }` arm, or compound predicates like `(h_dom : n > 2 ∧ n < 500)` from nested caller guards. A thin public def wraps the aux and keeps the original signature. `Lean omega` closes the per-callsite preservation obligation and the `Int.natAbs n` decrease automatically. Countdowns without a validated positive self-call guard keep fuel-encoded helpers. Their fuel is `natAbs(n) + 1`, enough for a countdown that stops at zero. A countdown that stops at another floor, `match to < from` stepping `to - 1`, gets `max(natAbs(to) + 1, natAbs(to - from) + 2)` when every self-call is guarded by `to >= from` and `from` is a literal (negative ones included) or a parameter passed on unchanged. ProofIR reports it as `Fuel { NatAbsOrFloorDistance }`. The seed is never below the zero-floor one, so a floor read wrong cannot make the model run out sooner. The members of a mutual group keep `natAbs(n) + 1`: their fuel is shared with the calls to each other, which the floor does not bound.
- single-function guarded subtractive countdown on any `Int` parameter: every self-call subtracts a positive literal, and its enclosing guards imply that parameter is positive. These functions emit a native `termination_by param.toNat` definition, including accumulator forms and exposed functions, whatever laws are present. The guard analysis used for floor-division descent and the Lean kernel both check the decrease. Unguarded subtraction and negative ascent keep their existing fuel encoding. Artifact certificate models keep explicit fuel for subtractive countdowns, because the certificate recursion wall consumes that representation. Standalone law proofs use the native definition.
- single-function `Int` descent by floor division. Every self-call shrinks an `Int` parameter either through `Int.div(p, k)` with literal `k >= 2` or by subtracting a positive literal, at least one call divides, and calls may descend differently (`f(p / 2) + f(p / 3)`, or `p - 1` beside `p / 4`). A division is written `Int.div(p, k)` with literal `k >= 2` (the discharged total form; the legacy `Result.withDefault(Int.div(p, k), d)` wrapper is still recognized), inlined or through a unary wrapper like `fn half(a) = Int.div(a, 2)`, and the guards around every self-call provably imply `p >= 1` (e.g. `match p > 0`, or the binary-exponent pair `b >= 1` and `a >= 2 * b`). This emits a genuine well-founded def (`termination_by p.toNat`, kernel-checked decrease) instead of a kernel-opaque `partial def`. A guard that cannot be validated makes it decline; it never guesses.
- single-function second-order affine `Int` recurrences with `n < 0` guard, `0/1` case split, and a matching pair-state tail worker, emitted via a private `Nat` helper
- single-function structural recursion on any `List<_>` parameter
- single-function `String + pos` recursion on `(String, Int)` signatures
- mutual recursion SCC with first-parameter `Int` countdown
- two-member `Int` walks at any parameter position: one member checks `n > 0` (countdown) or `n < target` (ascending), then delegates unchanged; the other takes exactly one step and returns, preserving the target. Their native measures are `(gap.toNat, 0)` and `((gap - 1).toNat, 1)`. The worker offset makes direct calls at or beyond the boundary total too. Original guards, every call edge and target preservation are checked; Lean proves each decrease. Oracle-lifted bodies use the same recognizer on their actual rewritten calls, so fixture cases can simplify through these definitions without evaluating a free oracle. Unguarded or nonprogressing cycles retain their previous opaque fallback.
- mutual recursion SCC with ranked `String + pos` progress
- mutual recursion SCC with ranked structural descent over recursive parameters (emitted as native `mutual ... termination_by ... end` block when every SCC member has a `List`/`Vector` sizeOf measure; fuel-encoded otherwise)

## Checking an export (`--check` / `--check-json`)

`aver proof file.av --backend lean -o out/ --check` builds the export with `lake` and gates on the result. `--check-json` prints one machine-readable summary line. Fields of the Lean summary:

- `passed`: the build succeeded within the budgets. A bounded verify-on-domain still passes; this gate is lenient on purpose.
- `sorries`: residual `sorry` count across the build output (`--sorry-budget N` tolerates up to N)
- `isolated_cases`: the `verify` cases among `isolated_errors`, as `<Module>.__aver_verify_<fn>_<N>` (present only when there are any). They are charged like the laws in `isolated_errors` but are not listed in `sorry_laws`.
- `sorry_laws`: the `fn.law` identities whose theorem carries a residual `sorry` in the gate build (present only when `sorries > 0`). This is the machine-readable answer to "which law failed?". The emitter maps each Lean `declaration uses 'sorry'` warning back to its law through the `-- aver:law-class` markers, so you no longer have to `lake build` the generated project by hand and match the warning's line number against the emitted theorems.
- `build_errors`: count of HARD lake/lean build errors (source-located `error: file.lean:L:C: …` diagnostics), counted apart from `sorries`, plus the proofs that failed behind the isolation guard (see `isolated_errors`). A degraded proof arm should always fall to a caught `sorry`. A non-zero `build_errors` means a tactic escaped the `first | … | sorry` floor, and `sorries` alone would look like a clean result. Informational only: it does not change `passed` or the exit code.
- `build_succeeded`, `build_status`, `build_exit_code`: the actual `lake build` process outcome, distinct from diagnostic counts. A compiler killed by the operating system can leave `build_errors` at zero while the build failed. The exit code is `null` when the process ended without one (for example by a signal); `build_status` includes the platform's status description. A failed build saves its output in `proof_backend.log` even without `--explain`, fails the check, and earns no law audit against potentially partial or stale build artifacts.
- `isolated_errors`: the `fn.law` identities whose proof failed to elaborate (present only when there are any). Each proof theorem of a law file is emitted behind `#guard_msgs (drop error, pass warning, pass info, pass trace) in`, so a proof that ends in an error `first` cannot catch (unsolved goals, a failed `rewrite`, a heartbeat timeout) no longer fails `lake build`. Lean adds that theorem with a synthetic `sorry`, the audit records the law as `failed`, and after the build the check reads the synthetic `sorry` back from the built modules. Each such proof counts in `sorries`, `sorry_laws` and `build_errors`, so it fails a zero sorry budget exactly as the hard error failed the build before. A guarded law that is only an attempt (see `when` above) is declined instead and counts in none of them. A guarded name counts as proved only when the built modules declare it as a theorem, and only once: when a function or another theorem already has the name, Lean rejects the guarded theorem as "already declared", the guard drops that error, and the check reports the name as failed instead of reading the other declaration as the proof. The other laws of the module keep their credit. If the check cannot tell which proofs failed, it fails closed. The `_sample_N` and `_checked_domain` theorems stay outside the guard: one of them failing is a counterexample, and it still fails the build.
- `universal`: `true` only when EVERY law theorem in the export is kernel-genuine. That means its `#print axioms` stays inside `{propext, Classical.choice, Quot.sound}` (so `native_decide` and `sorry` never count), at least one theorem is explicitly classed universal, and the file has no sorries.
- `universal_laws`: how many law theorems classed universal passed that same per-theorem axiom whitelist. The audit is per theorem, so it still runs on a file with sorries. A sorry-floored theorem's own axiom line carries `sorryAx` and records tier `failed`, while every kernel-clean sibling keeps its `universal` record and counts. Only the file-level `universal` bool keeps the "no sorries" conjunct, so pin the count together with `sorries`, not instead of it.
- `declined`, `declined_budget`, `declined_claims` (present only when something was declined): the claims the exporter refused to state, and the guarded laws whose attempted universal proof Lean did not close (see `when` above). Each entry names the claim and the reason. The count is gated by `--declined-budget N` (default 0), separately from the sorry budget.

Note: a declined law appears in neither `universal_laws` nor `sorries`. The counters count emitted law THEOREMS, not `verify ... law` blocks in the source.
- `model_panicked`: the compiler model panicked while evaluating a bounded sample. The check fails regardless of budgets.
- `budget`: the active sorry budget

`universal_laws` and the declined attempts come from the same per-theorem class markers and the same `#print axioms` audit that the `universal` bool is based on. A CI budget that holds up pins them together: `sorries == X`, `universal == true`, `universal_laws == N`, `declined == M`.

Before 0.31 a guarded law that no strategy could prove was stated over its sample domain only and counted in `bounded_laws`, with tier `bounded` in the manifest. That statement is gone, and so is the key. A `--gate` baseline written by 0.30 that holds `bounded` laws fails with a message saying so; each such law is either promoted to `universal` or reported MISSING because it is now declined. Review the change and re-record the baseline with `--write-baseline`.

The manifest (`proof_manifest.json` in the output directory) also stores a content hash per emitted declaration (`definitions`, keyed `Root.name`) and per law or obligation script (`scripts`, keyed by the same `fn.law` identities as the records). The hashes answer one question: when a law stops closing, did its own proof change, or did the model under it change? `aver proof file.av --check --compare-manifest <earlier proof_manifest.json>` answers it for every claim that did not close (a record at tier `failed`, a theorem that carries a `sorry`, a declined attempt, or a claim where the build located a hard error). The answer reads `--compare-manifest: <claim>: script unchanged; changed definitions in its cone: <names>`, where the cone is every emitted declaration the claim's theorem opens, transitively. `--check-json` carries the same report as a `changed` object keyed by claim. Each entry has `script` (`same`, `changed`, or `new` when the earlier manifest has no record of the claim) and `definitions` (the changed cone members, sorted). The comparison is diagnostic only and never changes `passed`, the tiers or the exit code. An earlier manifest that cannot be read or has no hashes is a harness error (exit 2), so it can never pass for a report that says nothing changed. Hashes ignore comment lines and the line-numbered hypothesis names of recursive matchers, so adding a line above a definition does not change its hash.

### Step zero: which law failed?

Start with a source-level explanation:

```sh
aver proof file.av --check --explain
```

The report points each open law or `because` step to its location in the `.av` file, imported modules included. It shows the goal, the universally quantified variables, `when` assumptions, earlier `because` results and explicit `using` citations. For a direct call that matches a cited law's argument pattern, the requirements are shown with the actual arguments substituted. For example, the deliberately incomplete law in `tests/fixtures/law_reason_constant_citation.av` produces:

```text
tests/fixtures/law_reason_constant_citation.av:39 — product.missingFactorGuard.because1
  This step has not been proved from the current assumptions.
  To prove: orderedProducts(0, a, b)
  For any a: Int, b: Int
  when a >= 0 [assumed]
  using orderedProducts.monotone [universal; source arguments substituted]
    provides orderedProducts(0, a, b) holds
    requires 0 <= a
    requires b >= 0
  Citation check (isolated): orderedProducts.monotone — direct application succeeded
    [closed in probe] 0 <= a
    [open in probe] 0 <= b
```

Here the available assumption does not establish `b >= 0`. The requirements list describes the cited law. It does not say that every listed condition is missing, or that the checker applied that citation. Unambiguous direct call patterns are substituted; other forms keep schematic parameters. After a successful counted build, supported direct applications also run in an isolated diagnostic copy. That run reports the cited law it actually tried and which premises the application closed or left open. The normalized premise may be spelled differently from the source requirement (`0 <= b` versus `b >= 0` above). Automatic citations are reported from these observed attempts too. Other proof strategies and final implications may have only source context. A failed application does not show that the law is inapplicable.

Every available citation and earlier proof step keeps its audited status. A probe that uses a failed, bounded or unchecked fact is marked as conditional. Each closed premise also audits the transitive axioms of its actual proof, including any additional global lemmas visible in the diagnostic import. A missing audit, or an axiom outside the counted checker's whitelist, keeps the closure conditional. Closing all of its premises still gives no proof credit. Unknown backend forms are marked unavailable instead of guessed. Probe details are saved separately in `proof_citations.log`, and the temporary copy never enters the counted build or manifest. Earlier `because` steps carry their audited status, and `failed` and `not_checked` steps must not be treated as proved facts. Later steps are never listed as assumptions for an earlier step.

`--check-json --explain` exposes the same report in `explanations`, keyed by `fn.law` or `fn.law.becauseN` / `fn.law.implication`. Observed attempts appear in `citation_attempts` with `phase: "diagnostic_direct_application"`, an `outcome`, `premises`, `available_laws` and `established_dependencies`. Closed premises include `closure_audited` and `proof_axioms`. These fields describe the isolated attempt. They are not the counted solver's search history. A computation limit is reported as `checker_limit`, separately from an unproved step. The reports can keep source context even when a hard checker error prevents the final axiom audit. Such an error does not show that the mathematical statement is false. Errors that cannot be mapped to a source step are labelled as checker errors, with technical details saved in `proof_backend.log` in the output directory. Reports are diagnostic only and never change proof credit, budgets or exit codes.

The existing `sorry_laws` identities, `open_goals` residuals and manifest `open_goal` fields are still available for tools that need backend detail. Laws without `because` can still use an isolated residual probe and calculated helper suggestions. A residual borrowed from a healthy law stays under `probe_of` and never goes into `open_goals`. Explanations show the source proof report before any usable helper suggestions. When suggestions are unavailable, a short notice is printed, and the technical extraction and translation details go to `proof_candidates.log`. Generated proof-step theorems are excluded from this legacy helper search. If a selected theorem cannot be supplied to a proof step, the report says `citation_unavailable` instead of suggesting a missing mathematical premise. Without `--explain`, none of the new diagnostic fields are emitted. The [IR snapshots](transpilation.md#debugging-a-law-that-didnt-auto-prove) and generated output are still useful for compiler debugging. Ordinary proof-step reports use Aver syntax directly.

## Law provenance (`-- aver:provenance`)

A law can carry a structured source comment that records who PRODUCED it. It is a maintenance note for when the law later breaks: recompute it, re-conjecture it, or ask the author? It is a comment and not grammar, because there are several producers (the `--explain` calculator today, a conjecturer, future tools):

```
// aver:provenance <value> [k=v …]
verify <fn> law <name>
```

It is an ordinary Aver `//` line comment. (The emitter's `-- aver:law-class` markers use Lean's `--` because they live in generated Lean, not in `.av` source.) `<value>` is an open-ended lowercase token (`calculated`, `conjectured`, …). Optional keys carry context (`from=<parent law>`, `tool=explain`). The marker goes on the line(s) directly above the `verify … law` block.

When `--check` writes the proof manifest, it scans each proving law's source for this marker. If one is present, it records the payload verbatim as a `provenance` field on that law's manifest entry. An unmarked law gets no `provenance` key (authored by default), so a corpus with no markers produces a byte-identical manifest. The marker is SELF-DECLARED and UNVERIFIED. A hand-written law may claim any value, and it is recorded as claimed. Provenance never grants proof credit; that still comes only from the kernel and the manifest tier. `--explain` prints this line above each pasteable `verify` block in its calculated-law suggestions, so pasting a calculated law and checking again records `provenance: "calculated from=… tool=explain"` automatically.

## Optional waterfall discovery (`--waterfall PATH`)

`aver proof file.av --check --waterfall /path/to/waterfall` adds bounded proof search after the existing automation. Accepted scripts are replayed without waterfall, saved in the generated Lean and in a rechecked cache, then audited by the normal project gate. Source guards, `using` scope and every `because` obligation stay part of the proof. The option is off by default.

[waterfall.md](waterfall.md) covers setup, limits, retained output and validation.

## Minimizing a proof (`--minimize`)

`aver proof file.av --backend lean -o out/ --check --minimize` (Lean-only, implies `--check`) rewrites each auto-proof to the tactic that actually closed it. The auto-prover pins a deterministic `first | (t₁) | (t₂) | … | sorry` PORTFOLIO at every law, because it cannot know statically which alternative a given goal needs. `--minimize` resolves that hedge against a real build:

1. each `first` branch gets a `trace "AVERMIN:i:b"` marker in front of it, and the project is built ONCE. `first` tries branches left to right and commits to the first that closes, tracing each one it reaches. The winning branch of portfolio `i` is therefore the **highest** index `b` in the build log (failed branches before it trace too; the markers are not rolled back);
2. each portfolio is collapsed to its winning branch and the project is RE-VERIFIED. If a theorem no longer closes (a mis-parsed winner), it keeps its original portfolio.

Minimization is therefore **fail-safe** (it can never ship a proof that does not build) and **status-preserving** (it keeps exactly the branch Lean committed to). A law that really closes loses its alternation and `sorry` floor and reads like a hand-written proof. A law that closed only through its floor collapses to a bare `sorry`. The gap stays visible and is never dropped silently, so the `sorries` / `universal` numbers from `--check` do not change. It is an opt-in polishing pass that costs two extra `lake build`s, and it is not part of the normal verify loop.

## Refinement records (refinement-via-opaque)

An Aver single-field record paired with a validating smart constructor `fn fromX(value: T) -> Result<X, String>`, whose body has the canonical `match <pred(value)> { true -> Result.Ok(X(v = value)); false -> Result.Err(_) }` shape, lifts to a true refinement subtype in Lean:

```lean
abbrev Natural := { v : Int // v ≥ 0 }
```

The predicate from the smart constructor's bool guard lives in the type itself, so a `verify add law commutative` over `Natural` quantifies directly over the refined type:

```lean
theorem add_law_commutative : ∀ (a b : Natural), add a b = add b a := by
  intro a b
  unfold add fromInt
  simp [Int.add_comm, Int.mul_comm]
```

That is one line. Before 0.22, each law shape needed its own `by_cases h_a : a ≥ 0 / by_cases h_b : b ≥ 0 / unfold / hand-rolled tactic` plumbing. The lift is automatic and needs no source-language change. It supports `Int`, structural containers (`List`, `Vector`, `Map`, `Result`, `Option`, tuples), and named carriers, including one refinement nested inside another. The exporter orders predicate functions and types by dependency, so a source file may declare the record before its predicate. `Float` and `String` carriers keep the plain structure path: IEEE 754 NaN breaks universal float laws, and strings have no universal algebraic structure to exploit.

For example, `Bytes(values: List<Int>)` may carry an `allInRange(values)` invariant, and `Digest32(bytes: Bytes)` may add `hasLength32(bytes)`. Both invariants stay in their generated Subtypes instead of degrading to plain structures.

Refinement records work the same way whether the type is declared in the entry file or in a dependent module. `aver proof natural.av` and `aver proof natural_app.av --module-root examples` both emit the same lifted Subtype shape. Before 0.22, the cross-module case fell back to the wrapper shape and lost the one-line universal proof.

A `verify ... law` block with a `when` clause keeps the clause as a theorem premise when it carries information beyond the refinement type's invariant. A `when a >= 10` over `Natural` (invariant `a.val >= 0`) shows up as a real `(a.val ≥ 10) -> ...` antecedent on the universal. A redundant `when a >= 0` is dropped, so the universal keeps the one-line `∀ (a : Natural), ...` shape.

## Current end-to-end examples

These examples are currently smoke-tested end to end with `aver proof --verify-mode auto` plus `lake build`:

- `examples/formal/law_auto.av`
- `examples/data/fibonacci.av`
- `examples/data/quicksort.av`
- `examples/data/rle.av`
- `examples/data/json.av`
- `examples/core/grok_s_language.av`
- `examples/refinement/natural/natural.av`: refinement-via-opaque (Int + `>= 0`)
- `examples/refinement/positive/positive.av`: refinement (Int + `>= 1`)
- `examples/refinement/int_range/int_range.av`: refinement with compound `Bool.and(n >= 0, n <= 100)`
- `examples/refinement/bigint/bigint.av`: opaque `List<Int>`-backed record + mutual-rec digit arithmetic
- `examples/refinement/nonneg_float/nonneg_float.av`: Float carrier, structure path
- `examples/refinement/email/email.av`: String carrier, structure path

Other modules have unit coverage for proof-subset classification and generated Lean snippets, but are not listed here as end-to-end smoke cases yet.

## Hard-fail guarantees

Lean codegen does not silently mask unresolved compiler internals:

- `Expr::Resolved` in codegen input is a hard codegen error
- `Type::Invalid` in codegen input is a hard codegen error
- `sorry` is emitted in two cases: explicit `--verify-mode sorry`, and `--verify-mode auto` universal-law theorems whose shape no auto-proof strategy covers. The second case always comes with an inline comment and kernel-checked per-sample theorems, so the obligation stays visible.

### Source-local proof arguments

Hand-written `proofs/lean/<fn>__<law>.lean` bodies are no longer loaded, from parent directories either. Existing files at those paths have no effect on proof export. Express intermediate facts with `because` and pick source laws with `using`. Lean checks the steps and audits their axioms together with the final claim. Removing a manual proof does not make an unproved law universal.
