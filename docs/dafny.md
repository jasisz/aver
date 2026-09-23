# Dafny Backend

Dafny is Aver's automated-verification backend. It emits `verify law` blocks as Dafny lemmas, and Z3 attempts the proofs without hand-written tactics.

Use it when you want:
- automated checking of `verify law` properties
- automated checking of Oracle-lifted laws over classified effects
- the Z3/SMT solver to attempt universal proofs for you
- a quick check that your laws hold before you spend time on Lean proof strategies

For an ordinary `verify law` block without proof guidance, the backend emits two things:

1. **Sample assertions**: concrete smoke tests from the `given` domain (e.g. `assert fib(5) == fibSpec(5)`), capped at 5 to avoid Z3 timeouts
2. **Universal lemma**: a `lemma` with `when` as `requires` and the law as `ensures`, proved by Z3

The samples may time out on deeply recursive computations. That is expected. The lemma is the main verification target.

For guided laws, the [guided proof fragment](dafny-guidance-spike.md) emits a separate lemma for every `because`, one for the final implication, and one for the parent law. Explicit `using` citations can cross module boundaries, and so can supported pure functions and record or sum types. Imported declarations keep their original identities and guards. A selected ordinary law is proved again by a separate universal lemma; its existing sample checks do not count as universal evidence. The strict whole-file gate must pass, because a verified caller does not make up for a cited supplier that failed. Use `aver verify` to execute the examples.

The fragment includes nonlinear integer arithmetic and exact `Int.div` / `Int.mod`. Nonzero literal divisors return `Int`. Zero or dynamic divisors keep Aver’s `Result<Int, String>` boundary, including the zero-divisor error. Quotient and remainder follow Euclidean semantics, also for negative operands. Checked integer and list descent can support guided induction with generalized givens, including changing accumulators. Quotient recursion needs a shared validated contract for descent by a fixed literal divisor of at least two.

Guidance admits checked native mutual recursion, named pure callbacks, and refinements whose emitted predicate matches a fully checked source cone. It still declines automatic citation selection, fuel-backed recursion, unsupported recursion patterns, arbitrary callback givens, effectful calls, provider resources and `Float`. Every called body and named field is checked, including imported dependencies. These restrictions apply to guided proof admission. The ordinary backend's wider emission and fallback paths are described below.

## Quick start

```bash
aver proof examples/data/fibonacci.av --backend dafny -o /tmp/fib-dafny
cd /tmp/fib-dafny && dafny verify --verify-included-files fibonacci.dfy
```

Requires [Dafny](https://github.com/dafny-lang/dafny) (4.x+) installed with Z3. On macOS: `brew install dafny`.

## Explanations in Aver

```bash
aver proof file.av --backend dafny --check --explain
aver proof file.av --backend dafny --check-json --explain
```

`--explain` maps checker errors back to the Aver law or `because` step: source location, goal, givens, original `when`, earlier reasons and explicit citations. A timeout is reported as `checker_limit`; it is not a counterexample. An error outside a mapped law is a checker error, and its technical details are saved in `proof_backend.log`. Dafny does not run Lean's citation probes or suggestion search.

JSON adds `explanations` for open steps and a `claims` inventory. Each claim records `exported` separately from `status`, which is one of `not_exported`, `unresolved` or `checked`. `checked` requires the complete strict module check, with no errors, timeouts, axioms, omissions or declined laws. A lemma in a failing module stays `unresolved` even if it has no error of its own, and so do its earlier reasons and cited laws. These statuses describe the emitted checks. They do not turn an ordinary bounded/sample fallback into a universal theorem. Exporting a claim gives it no proof credit by itself. Without `--explain`, the report schema and the checker gate are unchanged.

Shared ProofIR records induction instances for imported reason functions and local value aliases. Explicit Boolean branches carry their own guards and recursive arguments, and both targets use the same canonical function identities and checked measure. Nested matches that bind variables, and unsupported callbacks, are outside this planner's scope; backends may use their existing checked fallbacks for them. Every generated recursive lemma call still has to prove its decrease and its recursive premises.

## What it generates

An entry `.dfy` file, plus dependency module files and a shared prelude when needed, containing:

- **Prelude**: `Result<T,E>`, `Option<T>`, list/map/string helpers
- **Datatypes**: user-defined `record` → `datatype`, `type` (sum) → `datatype`
- **Functions**: pure Aver functions → `function` with `decreases` clauses
- **Oracle-lifted functions**: classified effectful functions become pure functions with explicit oracle/capability parameters
- **Lemmas**: `verify law` blocks → `lemma` with `ensures` and optional inductive hints

## What it does NOT generate

- `verify` cases (concrete assertions that are not laws). Z3 cannot efficiently compute deeply recursive functions on specific inputs; Lean's `native_decide` is the right tool for that
- Unclassified effectful functions. Only pure functions and Oracle-lifted classified effects are emitted
- `fn main()`. The entry point is skipped

## How it maps Aver → Dafny

| Aver | Dafny |
|---|---|
| `Int` | `int` |
| `Float` | legacy `real` approximation; not an IEEE model, and guidance declines it |
| `String` | `string` |
| `Bool` | `bool` |
| `Unit` | `()` |
| `List<T>` / `Vector<T>` | `seq<T>` |
| `Map<K,V>` | `map<K,V>`; `Map<K,Unit>` uses `set<K>` |
| `Result<T,E>` | `Result<T,E>` (prelude datatype) |
| `Option<T>` | `Option<T>` (prelude datatype) |
| `record Foo` | `datatype Foo = Foo(fields...)` |
| `type Bar = A \| B(Int)` | `datatype Bar = A \| B(b_0: int)` |
| `match x: true → a, false → b` | `if x then a else b` |
| `match n: 0 → base, _ → f(n-1)` | `if n == 0 then base else f(n-1)` |
| `match xs: [] → a, [h,..t] → b` | `if \|xs\| == 0 then a else var h := xs[0]; var t := xs[1..]; b` |
| `Int.div(a, b)` / `Int.mod(a, b)` | `Result`-wrapped Euclidean div/mod (Dafny guards the zero divisor). With a syntactic nonzero literal divisor the call is discharged to plain `Int` and renders as bare Euclidean `/` / `%`. Integer `/` is a type error; `/` is Float-only (Dafny `real`) |
| `verify f law name` | sample `method` + universal `lemma` |

## Pure propagation and structural expressions

`?` exits the enclosing Result-returning function. The Dafny lowering threads that exit through arguments, constructors, records and updates, containers, interpolation and match branches. It evaluates eager operands once, in source order, and keeps the same match-branch selection. `?!` evaluates the independent Results before picking the first error in source order. No error branch is replaced with a default value or assumed to succeed.

Primitive interpolation renders `Int` as exact decimal, `Bool` in lowercase and `String` unchanged. Structural text helpers define character indexing, slicing, joining, substring tests, splitting, replacement and Unicode whitespace trimming. `String.toLower` and `String.toUpper` have full Unicode definitions, including multi-scalar expansions and context-sensitive final sigma. Lean and Dafny use the same mapping and context tables as wasm-gc, checked exhaustively against the Rust standard library the VM uses. Case conversion imports no UTF-8 axioms. UTF-8 and byte-length operations keep their separate legacy opaque helpers.

Named callbacks are admitted only after their complete pure bodies and termination are checked. A callback that reintroduces an active function is refused. Arbitrary function-valued givens have no checked source totality domain. Maps support extensional operations, including Unit-valued sets. Sorted iteration still has no exact backend model. These limits are a different matter from missing expression syntax.

## Termination

Recursive functions fall into three buckets, based on the shared classifier in `codegen::recursion::detect`:

**Direct-recursion patterns** are emitted as normal Dafny `function`s with inferred `decreases` clauses:
- A shared, guarded subtractive countdown takes precedence over sequence-parameter heuristics, so a byte accumulator may grow while its width decreases. Both backends consume the same `ProofIR` contract, and no caller precondition is added.
- A list growing under `List.len(xs) < bound`, with a stable integer bound and a prepend/append that is statically nonempty, uses `decreases bound - |xs|`. Lean uses the same difference converted to `Nat`. The same contract covers strings growing by a nonempty literal under `String.len(text) < bound`. Negative bounds and an append that overshoots the bound stay total.
- List parameter → `decreases |xs|`
- String parameter → `decreases |s|`
- Int countdown (`match n { 0 -> …; _ -> recur(n-1, …) }`) → `requires n >= 0` + `decreases n`. Callers discharge the `requires` through Dafny's auto-inference from surrounding `if`/`match` shapes: `match (n < 0) { false -> worker(n) }` resolves to `n >= 0` automatically.
- Int countdown with an explicit `match n < 0` base → `decreases if n >= 0 then n else 0` (no `requires`; the body itself handles the negative case).
- Int floor-division countdown by a literal divisor → `decreases if p >= 0 then p else 0` with NO synthesized `requires`. The divisor form is `Int.div(p, k)` with literal `k >= 2` (the discharged total form) or the legacy `Result.withDefault(Int.div(p, k), d)` wrapper, inlined or through a unary wrapper like `half`. This applies when the function's own guards prove `p >= 1` at every recursive call (binary exponent search by halving, base-10⁹ digit peeling). A guard that cannot be validated falls back to the opaque form instead of guessing. `verify ... law` blocks over this class (power-of-two positivity and sum laws, the scaled-significand window of an integer ratio, the m-bit×n-bit product window) emit a proved support stack: division-window lemmas derived from the Euclidean identity, power algebra by self-call induction, and branch-split significand lemmas. With it the universal lemmas verify instead of being omitted; see `tests/fixtures/floor_window.av`.

**Mutual-recursion SCCs**: the preferred path picks sequence-length measures and matching ranks for structural mutual groups, then emits native `decreases` tuples:

```dafny
function fn(args): T
  decreases <selected_length_sum>, <rank>
{ <body with intra-SCC calls unchanged> }
```

The shared call-edge analysis decides which `List`/`Vector`/`String` parameters contribute their lengths. It can leave out a growing accumulator while keeping the input that drives progress. The rank orders the calls that leave the selected sum unchanged, and it always comes from the same analysis as the emitted measure. List tails are shorter; list heads are not shorter by length, and slices only do not grow. Dafny checks the native functions' termination. The older guarded path (all sequences plus a forwarded rank) remains as a fallback for older supported shapes.

Guided laws admit exactly the functions that are emitted natively, and still validate their complete bodies and dependency cones. Calling a native function does not make the caller natively recursive. Fuel and opaque fallbacks stay outside guided universal admission. Native function emission does not give you a general mutual induction tactic for arbitrary laws.

**Fuel fallback**: SCCs without a measurable parameter (mutual recursion over `Int` only) still go through fuel-guarded pairs, the same as Lean's `def fn__fuel (fuel : Nat) …`:

```dafny
function fn__fuel(fuel: nat, args): T
  decreases fuel
{
  if fuel == 0 then <total default for T>
  else var fuel' := fuel - 1; <body with intra-SCC calls → g__fuel(fuel', …)>
}

function fn(args): T { fn__fuel(<plan metric>, args) }
```

The fuel metric depends on the plan: `natAbs(n) + 1` for `MutualIntCountdown`, `(|s| + 1) * (rank * scc_size + 1)` for `MutualStringPosAdvance` / `MutualSizeOfRanked`. A per-type default-value generator handles scalars and Option/Result/Tuple/List, and walks the first variant for Named ADTs (a visiting set stops it from diverging on left-recursive types).

**Axiom fallback** (`function {:axiom} fn(args): T`, a signature without a body) is used for:
- SCCs whose return type has no obvious total default (left-recursive Named ADTs, function types).
- Functions whose propagation cannot be normalized (for example, an invalid non-Result return). Valid pure nested `?` and `?!` are normalized into explicit matches before ordinary, native-recursive or fuel emission.

A lemma whose `ensures` refers to an opaque fn (axiom or fuel-guarded) gets the body `assume {:axiom} <ensures>;`. This matches Lean's `sorry`: the lemma is accepted on trust, not derived by unfolding. Dafny still type-checks the whole file. Where the axiom fallback gets in the way, users add their own lemma proofs.

## Refinement records (refinement-via-opaque)

An Aver single-field record paired with a validating smart constructor `fn fromX(value: T) -> Result<X, String>`, whose body matches `match <pred(value)> { true -> Result.Ok(X(v = value)); false -> Result.Err(_) }`, lifts to a Dafny subset type:

```dafny
type Natural = v: int | v >= 0 witness 0
```

The predicate from the smart constructor's bool guard becomes the subset constraint. So `verify add law commutative` over `Natural` emits the universal lemma with an empty proof body, and Dafny's type-checker discharges the lift directly:

```dafny
lemma {:fuel add, 5} {:fuel fromInt, 5} add_commutative(a: Natural, b: Natural)
  ensures add(a, b) == add(b, a)
{ }
```

The lift supports `Int`, structural containers (`List`, `Vector`, `Map`, `Result`, `Option`, tuples), and named carriers, including nested refinements. When no concrete inhabitant is known, the exporter uses Dafny's `witness *`. It never drops the subset predicate because witness synthesis failed. `Float` / `String` carriers and multi-field refinement records keep the plain datatype shape. Cross-module emission is the same as standalone: `aver proof natural.av` and `aver proof natural_app.av --module-root examples` both generate the same `type Natural = ...` declaration.

A `verify ... law` block's `when` clause stays as a `requires` clause on the universal lemma when it says more than the refinement type's invariant. `when a >= 10` over `Natural` (invariant `a >= 0`) becomes `requires a >= 10`. A redundant `when a >= 0` is dropped, so the universal lemma signature stays `lemma add_law_commutative(a: Natural, b: Natural)`. Compound invariants (`Bool.and(n >= 0, n <= 100)`) are flattened on both sides of the comparison, so `IntRange`'s `when Bool.and(a >= 0, a <= 100)` is correctly recognised as equivalent to the subset constraint.

## Bounded-∀ universal over mutual-rec SCCs

A `verify <fn> law` with `given a: Int = [k₁, k₂, ...]` plus `given b: Int = [...]` over a mutual-recursion SCC emits the universal lemma as a bounded ∀ over the declared domain:

```dafny
lemma add_commutative(a: int, b: int)
  requires (a == 0 || a == 1 || ...) && (b == 0 || b == 1 || ...)
  ensures add(a, b) == add(b, a)
{
  if a == 0 && b == 0 { add_commutative_sample_1(); }
  else if a == 0 && b == 1 { add_commutative_sample_2(); }
  // ... per-(a, b) pair dispatch
}
```

The per-pair `add_commutative_sample_n` lemmas close as real proofs (no `assume {:axiom}` body). BigInt's `add_commutative` was the main test case. It went from 18 verified / 5 errors (with `assume {:axiom}` on the universal) to 36 verified / 0 errors, with the universal as a verified bounded ∀ over the declared domain. It falls back to `assume {:axiom}` only when the law's givens have no explicit literal domain (open-`Int` quantifier, oracle binding, etc.).

## Inductive lemma hints

For simple list folds and guarded countdowns, `ProofIR` records the law's induction driver and the actual recursive arguments, including accumulator updates and extra law givens. Dafny uses those instances in ordinary laws and in separately checked universal citation lemmas. A law that compares an arbitrary accumulator with an empty one receives both recursive instances. Each call must prove the original premises and a strict decrease; the plan supplies no axioms. Lean also consults the shared driver when it chooses functional induction for a matching explanation. Unsupported source shapes keep the existing strategies.

The plan also keeps the concrete source step when a fixed accumulator seed is not a theorem parameter. Shared application search expands that step in the claim and matches earlier ordinary laws against its subterms, including terms exposed by another lemma's result. It records canonical function identities and argument expressions in `ProofIR`. Dafny renders the admitted instances as lemma calls, and Lean's countdown induction uses the same instances as local facts. Both backends still check each supplier through their existing universal-law gates. The search currently covers earlier unconditional laws in the declaring module and in the consumer's function cone, with `Int`, `Bool`, `String` or list givens and a concrete user-function call on the left. Its finite search budgets limit the hints, never the quantified domain. Unsupported terms keep ordinary induction, and normalized reflexive instances are dropped.

[`roundtrip.av`](../tests/fixtures/source_recursion/roundtrip.av) shows an unbounded decimal collector/reader roundtrip that uses only Aver laws. The same source passes Lean and Dafny, and so does a renamed variant using radix 16.

The Dafny proof bodies also check the sequence identities needed to match singleton/empty concatenations and reversals. The list reverse helper has a checked length-preservation postcondition. See `tests/fixtures/source_recursion/` for positive, imported and false-supplier controls that both checkers run.

For `verify law` blocks with a single `given n: Int` where both sides use directly-recursive functions, the codegen generates an inductive proof structure:

```dafny
lemma fib_fibSpec(n: int)
  ensures fibSpec(n) == fib(n)
{
  if n < 0 {
  } else if n == 0 {
  } else if n == 1 {
  } else {
    fib_fibSpec(n - 1);
    fib_fibSpec(n - 2);  // if double recursion detected
  }
}
```

## Limitations

- **No verify cases**: Z3 times out on deep computations like `fib(12) == 144`. Dafny's own `errors` total does not see per-lemma timeouts, so `--check-json` has a separate additive `timeouts` field (the count of `… timed out after N seconds` lines) next to `errors`. A consumer counting failing laws must read both. `timeouts` is informational: it does not change `passed` or the exit code (the timed-out run still fails through Dafny's exit status)
- **Constructor collisions**: if a user type defines variants named `Ok`/`Err`, Dafny may report ambiguity errors
- **Opaque builtins**: `IntToString`, `FloatFromString`, `StringFirstCodePoint` etc. are declared without bodies. Z3 knows their signatures but cannot reason about their implementation
- **Complex laws**: checked integer/list induction can generalize changing accumulators, but more complex indirect recursion and multi-function arguments may still need extra source lemmas, or may fail to verify. Guided proofs decline mutual/fuel recursion instead of relying on the ordinary backend’s fallback encoding.

When a law's lemma comes out with an empty body, see [transpilation.md → Debugging a law that didn't auto-prove](transpilation.md#debugging-a-law-that-didnt-auto-prove). It describes the `--emit-ir-after=law_lower` workflow, which tells you whether the classifier matched a strategy or fell through to backend dispatch.

## End-to-end smoke tests

`tests/proof_spec.rs` runs `dafny verify` end-to-end on every IR-clean example as a gate. The flagship examples that still have gaps from before the IR migration are tracked with an explicit error budget. Drift in either direction fails the test: more errors is a regression, and fewer means a gap has closed and the budget should be lowered.

| Example | Error budget |
|---|---|
| `examples/data/rle.av` | 3 |
| `examples/data/quicksort.av` | 5 |
| `examples/data/json.av` | 89 |

The budgets are a regression net, not a target. The umbrella issue for closing them is [#114](https://github.com/jasisz/lumen-rs/issues/114).

## Comparison with Lean

See [docs/transpilation.md](transpilation.md) for a side-by-side comparison.

In short: Lean is the strongest check, with kernel-verified proofs. Dafny is the quick check, automated by Z3 with no tactic work. Use both.

### Proof arguments and search policy

Source lowering records claims, premises, source dependencies and induction arguments in ProofIR. A separate `codegen::proof_search` pass may add concrete `LawApplication` suggestions. Its explicit `ApplicationSearchBudget` limits discovery, and `ApplicationSearchReport` records counts and the steps that reached a limit. Running it with a zero budget removes the suggestions without changing the source obligations or the induction. Both backends still check suppliers and applications.

Dafny's fixed-count unfolding policy lives in `codegen::dafny::reasons::unfolding`, outside ProofIR. It keeps the existing small-literal and scalar-given restrictions and fuel budgets. These are partial solver heuristics. They are not source bounds, and Lean does not share them. The separate ordinary-law fuel policy in `law_search` is also Dafny-specific.

The list library checks reverse/append and double-reversal lemmas. Reaching `List.reverse` from an acyclic constructor no longer enables a universal reversal pool or changes induction. That experimental strategy was withdrawn after review. Signed-frame readback is again an open diagnostic target; it is not a required success and does not count as a coverage gain.

`tools/proof_search_matrix.py` records the actual same-source results and times for both backends, including equivalent source refactorings. It records failures and timeouts as such. A completed matrix does not mean its laws passed.

See the [measured comparison and remaining limits](proof-search-policy.md).
