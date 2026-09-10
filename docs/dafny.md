# Dafny Backend

Dafny is the automated-verification backend for Aver. It emits `verify law` blocks as Dafny lemmas and lets Z3 attempt proofs without hand-written tactics.

Use it when you want:
- automated checking of `verify law` properties
- automated checking of Oracle-lifted laws over classified effects
- Z3/SMT solver attempting universal proofs for you
- a quick validation of whether your laws hold before investing in Lean proof strategies

For an ordinary `verify law` block without proof guidance, the backend emits two things:

1. **Sample assertions** — concrete smoke tests from the `given` domain (e.g. `assert fib(5) == fibSpec(5)`), capped at 5 to avoid Z3 timeouts
2. **Universal lemma** — `lemma` with `when` as `requires` and the law as `ensures`, proved by Z3

The samples may time out on deeply recursive computations — that is expected. The lemma is the primary verification target.

For guided laws, the [guided proof fragment](dafny-guidance-spike.md) emits
separate lemmas for every `because`, the final implication and their parent law.
Explicit `using` citations can cross module boundaries, as can supported pure
functions and record or sum types. Imported declarations retain their original
identities and guards. A selected ordinary law is re-proved by a separate
universal lemma; its existing sample checks are not treated as universal evidence.
The strict whole-file gate must pass; a verified caller does not establish a
failed cited supplier. Use `aver verify` to execute the examples.

This fragment includes nonlinear integer arithmetic and exact `Int.div` /
`Int.mod`. Nonzero literal divisors return `Int`; zero or dynamic divisors retain
Aver’s `Result<Int, String>` boundary, including the zero-divisor error. Quotient
and remainder follow Euclidean semantics for negative operands too. Checked
integer and list descent can support guided induction with generalized givens,
including changing accumulators. Quotient recursion requires a shared validated
contract for descent by a fixed literal divisor of at least two.

Guidance admits checked native mutual recursion, named pure callbacks and
refinements whose emitted predicate matches a fully checked source cone. It
still declines automatic citation selection, fuel-backed recursion, unsupported
recursion patterns, arbitrary callback givens, effectful calls, provider resources
and `Float`. Every called body and named field
is checked, including imported dependencies. These restrictions concern guided
proof admission; the ordinary backend’s broader emission and fallback paths are
described below.

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

`--explain` maps checker errors to the Aver law or `because` step: source
location, goal, givens, original `when`, earlier reasons and explicit citations.
A timeout is `checker_limit`, not a counterexample. An error outside a mapped
law is a checker error; its technical details are saved in `proof_backend.log`.
Dafny does not run Lean's citation probes or suggestion search.

JSON adds `explanations` for open steps and a `claims` inventory. Each claim
records `exported` separately from `status`: `not_exported`, `unresolved`, or
`checked`. `checked` requires the complete strict module check, with no errors,
timeouts, axioms, omissions or declined laws. A lemma without its own error in
a failing module remains `unresolved`, including earlier reasons and cited
laws. These statuses describe emitted checks; they do not upgrade an ordinary
bounded/sample fallback to a universal theorem. Export alone supplies no proof
credit. Without `--explain` the report schema and the checker gate are unchanged.

Shared ProofIR records induction instances for imported reason functions and
local value aliases. Explicit Boolean branches carry their own guards and
recursive arguments; both targets use the same canonical function identities
and checked measure. Nested binder-bearing matches and unsupported callbacks
remain outside this planner's scope; backends may use their existing checked
fallbacks. Every generated recursive lemma call still has to prove its decrease
and recursive premises.

## What it generates

An entry `.dfy` file, with dependency module files and a shared prelude when
needed, containing:

- **Prelude**: `Result<T,E>`, `Option<T>`, list/map/string helpers
- **Datatypes**: user-defined `record` → `datatype`, `type` (sum) → `datatype`
- **Functions**: pure Aver functions → `function` with `decreases` clauses
- **Oracle-lifted functions**: classified effectful functions become pure functions with explicit oracle/capability parameters
- **Lemmas**: `verify law` blocks → `lemma` with `ensures` and optional inductive hints

## What it does NOT generate

- `verify` cases (non-law concrete assertions) — Z3 can't efficiently compute deeply recursive functions on specific inputs; Lean's `native_decide` is the right tool for this
- Unclassified effectful functions — only pure functions and Oracle-lifted classified effects are emitted
- `fn main()` — entry point is skipped

## How it maps Aver → Dafny

| Aver | Dafny |
|---|---|
| `Int` | `int` |
| `Float` | legacy `real` approximation; not an IEEE model and declined by guidance |
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
| `Int.div(a, b)` / `Int.mod(a, b)` | `Result`-wrapped Euclidean div/mod (Dafny guards the zero divisor); with a syntactic nonzero literal divisor the call is discharged to plain `Int` and renders as bare Euclidean `/` / `%`. Integer `/` is a type error, `/` is Float-only (Dafny `real`) |
| `verify f law name` | sample `method` + universal `lemma` |

## Pure propagation and structural expressions

`?` exits the enclosing Result-returning function. The Dafny lowering threads
that continuation through arguments, constructors, records and updates,
containers, interpolation and match branches. It evaluates eager operands once
in source order and preserves selection of match branches. `?!` evaluates the
independent Results before selecting the first error in source order. No error
branch is replaced with a default value or assumed success.

Primitive interpolation has exact decimal `Int`, lowercase `Bool` and identity
`String` rendering. Structural text helpers define character indexing, slicing,
joining, substring tests, splitting, replacement and Unicode whitespace trimming.
`String.toLower` and `String.toUpper` have full Unicode definitions, including
multi-scalar expansions and context-sensitive final sigma. Lean and Dafny use
the same mapping and context tables as wasm-gc, checked exhaustively against
the VM's Rust standard library. Case conversion imports no UTF-8 axioms;
UTF-8 and byte-length operations retain their separate legacy opaque helpers.

Named callbacks are admitted only after checking their complete pure bodies and
termination. A callback that reintroduces an active function is refused. Arbitrary
function-valued givens have no checked source totality domain. Maps support
extensional operations, including Unit-valued sets; sorted iteration still lacks
an exact backend model. These boundaries differ from missing expression syntax.

## Termination

Recursive functions fall into three buckets based on the shared classifier in `codegen::recursion::detect`:

**Direct-recursion patterns** — emitted as normal Dafny `function`s with inferred `decreases` clauses:
- A shared, guarded subtractive countdown takes precedence over sequence-parameter heuristics: a byte accumulator may grow while its width decreases. Both backends consume the same `ProofIR` contract; no caller precondition is added.
- A list growing under `List.len(xs) < bound`, with a stable integer bound and a statically nonempty prepend/append, uses `decreases bound - |xs|`. Lean uses the same difference converted to `Nat`. The same contract covers strings growing by a nonempty literal under `String.len(text) < bound`. Negative bounds and an append that overshoots the bound remain total.
- List parameter → `decreases |xs|`
- String parameter → `decreases |s|`
- Int countdown (`match n { 0 -> …; _ -> recur(n-1, …) }`) → `requires n >= 0` + `decreases n`. Callers discharge the `requires` via Dafny's auto-inference from surrounding `if`/`match` shapes — `match (n < 0) { false -> worker(n) }` resolves to `n >= 0` automatically.
- Int countdown with explicit `match n < 0` base → `decreases if n >= 0 then n else 0` (no `requires`, the body itself handles the negative case).
- Int floor-division countdown by a literal divisor (`Int.div(p, k)` with literal `k >= 2` — discharged total form — or the legacy `Result.withDefault(Int.div(p, k), d)` wrapper, inlined or through a unary wrapper like `half`) → `decreases if p >= 0 then p else 0` with NO synthesized `requires`, when the function's own guards prove `p >= 1` at every recursive call (binary exponent search by halving, base-10⁹ digit peeling). An unvalidated guard declines to the opaque form instead of guessing. `verify ... law` blocks over this class — power-of-two positivity and sum laws, the scaled-significand window of an integer ratio, the m-bit×n-bit product window — emit a proved support stack (division-window lemmas derived from the Euclidean identity, power algebra by self-call induction, branch-split significand lemmas) so the universal lemmas verify instead of being omitted; see `tests/fixtures/floor_window.av`.

**Mutual-recursion SCCs** — the preferred path chooses sequence-length measures and matching ranks for structural mutual groups, then emits native `decreases` tuples:

```dafny
function fn(args): T
  decreases <selected_length_sum>, <rank>
{ <body with intra-SCC calls unchanged> }
```

The shared call-edge analysis selects which `List`/`Vector`/`String` parameters
contribute their lengths. It can omit a growing accumulator while retaining the
input that drives progress. The rank orders calls leaving that selected sum
unchanged; it always comes from the same analysis as the emitted measure. List
tails are shorter, but list heads are not shorter by length, and slices are only
non-growing. Dafny checks the native functions' termination. The existing guarded
all-sequence/forwarded-rank path remains a fallback for older supported shapes.

Guided laws admit exactly the functions emitted natively, while still validating
their complete bodies and dependency cones. A caller of a native function does
not thereby acquire native recursion status. Fuel and opaque fallbacks remain
outside guided universal admission. Native function emission does not provide a
general mutual induction tactic for arbitrary laws.

**Fuel fallback** — SCCs without a measurable parameter (pure `Int`-only mutual recursion) still go through fuel-guarded pairs, parallel to Lean's `def fn__fuel (fuel : Nat) …`:

```dafny
function fn__fuel(fuel: nat, args): T
  decreases fuel
{
  if fuel == 0 then <total default for T>
  else var fuel' := fuel - 1; <body with intra-SCC calls → g__fuel(fuel', …)>
}

function fn(args): T { fn__fuel(<plan metric>, args) }
```

Fuel metric depends on the plan: `natAbs(n) + 1` for `MutualIntCountdown`, `(|s| + 1) * (rank * scc_size + 1)` for `MutualStringPosAdvance` / `MutualSizeOfRanked`. A per-type default-value generator handles scalars, Option/Result/Tuple/List, and walks the first variant for Named ADTs (visiting set prevents divergence on left-recursive types).

**Axiom fallback** (`function {:axiom} fn(args): T` — signature without body) — for:
- SCCs whose return type admits no obvious total default (left-recursive Named ADTs, function types).
- Functions whose propagation cannot be normalized (for example, an invalid non-Result return). Valid pure nested `?` and `?!` normalize into explicit matches before ordinary, native-recursive or fuel emission.

Lemmas whose `ensures` references an opaque fn (axiom or fuel-guarded) short-circuit their body to `assume {:axiom} <ensures>;` — parallel to Lean's `sorry`, accepted on trust rather than derived from unfolding. Dafny still type-checks the whole file; users add their own lemma proofs where the axiom fallback bites.

## Refinement records (refinement-via-opaque)

An Aver single-field record paired with a validating smart constructor
`fn fromX(value: T) -> Result<X, String>` whose body matches
`match <pred(value)> { true -> Result.Ok(X(v = value)); false -> Result.Err(_) }`
lifts to a Dafny subset type:

```dafny
type Natural = v: int | v >= 0 witness 0
```

The predicate from the smart constructor's bool guard becomes the
subset constraint, so `verify add law commutative` over `Natural`
emits the universal lemma with an empty proof body — Dafny's
type-checker discharges the lift directly:

```dafny
lemma {:fuel add, 5} {:fuel fromInt, 5} add_commutative(a: Natural, b: Natural)
  ensures add(a, b) == add(b, a)
{ }
```

The lift supports `Int`, structural containers (`List`, `Vector`, `Map`,
`Result`, `Option`, tuples), and named carriers, including nested refinements.
When no concrete inhabitant is known, the exporter uses Dafny's `witness *`;
it never drops the subset predicate merely because witness synthesis failed.
`Float` / `String` carriers and multi-field refinement records stay on the
plain datatype shape. Cross-module emit is identical to standalone — `aver proof natural.av` and
`aver proof natural_app.av --module-root examples` both generate the
same `type Natural = ...` declaration.

A `verify ... law` block's `when` clause stays as a `requires` clause
on the universal lemma when it carries information beyond the
refinement type's invariant. `when a >= 10` over `Natural` (invariant
`a >= 0`) shows up as `requires a >= 10`; redundant `when a >= 0` is
dropped cleanly so the universal lemma signature stays at
`lemma add_law_commutative(a: Natural, b: Natural)`. Compound
invariants (`Bool.and(n >= 0, n <= 100)`) flatten on both sides of
the comparison so `IntRange`'s `when Bool.and(a >= 0, a <= 100)` is
correctly recognised as equivalent to the subset constraint.

## Bounded-∀ universal over mutual-rec SCCs

A `verify <fn> law` with `given a: Int = [k₁, k₂, ...]` plus
`given b: Int = [...]` over a mutual-recursion SCC emits the universal
lemma as a bounded ∀ over the declared domain:

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

Per-pair `add_commutative_sample_n` lemmas close as real proofs
(no `assume {:axiom}` body). BigInt's `add_commutative` was the
canonical exercise — moved from 18 verified / 5 errors (and
`assume {:axiom}` on the universal) to 36 verified / 0 errors
with the universal as a verified bounded ∀ over the declared
domain. Falls back to `assume {:axiom}` only when the law's givens
have no explicit literal domain (open-`Int` quantifier, oracle
binding, etc.).

## Inductive lemma hints

For simple list folds and guarded countdowns, `ProofIR` records the law's
induction driver and the actual recursive arguments, including accumulator
updates and extra law givens. Dafny uses those instances in ordinary laws and
separately checked universal citation lemmas. A law comparing an arbitrary
accumulator with an empty one receives both recursive instances. Each call must
prove the original premises and a strict decrease; the plan supplies no axioms.
Lean also consults the shared driver when choosing functional induction for a
matching explanation. Unsupported source shapes retain the existing strategies.

The plan also retains the concrete source step when a fixed accumulator seed is
not a theorem parameter. Shared application search expands that step in the
claim and matches earlier ordinary laws against its subterms, including terms
exposed by another lemma's result. It records canonical function identities and
argument expressions in `ProofIR`. Dafny renders admitted instances as lemma
calls; Lean's countdown induction consumes the same instances as local facts.
Both backends still check each supplier through their existing universal-law
gates. The search currently covers earlier unconditional laws in the declaring
module and the consumer's function cone, with `Int`, `Bool`, `String`, or list
givens and a concrete user-function call on the left.
Its finite search budgets limit hints, never the quantified domain. Unsupported
terms retain ordinary induction, and normalized reflexive instances are dropped.

[`roundtrip.av`](../tests/fixtures/source_recursion/roundtrip.av) demonstrates an
unbounded decimal collector/reader roundtrip using only Aver laws. The same
source passes Lean and Dafny, including a renamed variant using radix 16.

The Dafny proof bodies additionally check sequence identities needed to match
singleton/empty concatenations and reversals. The list reverse helper has a
checked length-preservation postcondition. See `tests/fixtures/source_recursion/`
for positive, imported, and false-supplier controls exercised by both checkers.

For `verify law` blocks with a single `given n: Int` where both sides use directly-recursive functions, the codegen generates inductive proof structure:

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

- **No verify cases**: Z3 times out on deep computations like `fib(12) == 144`. Dafny's own `errors` total is blind to per-lemma timeouts, so `--check-json` carries a separate additive `timeouts` field (count of `… timed out after N seconds` lines) alongside `errors`; a consumer accounting for failing laws must read both. `timeouts` is informational — it does not change `passed` or the exit code (the timed-out run still fails via Dafny's exit status)
- **Constructor collisions**: if a user type defines variants named `Ok`/`Err`, Dafny may report ambiguity errors
- **Opaque builtins**: `IntToString`, `FloatFromString`, `StringFirstCodePoint` etc. are declared without bodies — Z3 knows their signatures but can't reason about their implementation
- **Complex laws**: checked integer/list induction can generalize changing accumulators, but more complex indirect recursion and multi-function arguments may still need additional source lemmas or fail to verify. Guided proofs decline mutual/fuel recursion instead of relying on the ordinary backend’s fallback encoding.

When a law's lemma comes out with an empty body, see [transpilation.md → Debugging a law that didn't auto-prove](transpilation.md#debugging-a-law-that-didnt-auto-prove) for the `--emit-ir-after=law_lower` workflow that tells you whether the classifier matched a strategy or fell through to backend dispatch.

## End-to-end smoke tests

`tests/proof_spec.rs` gates `dafny verify` on every IR-clean example end-to-end. The flagship examples that still carry pre-IR-migration gaps are tracked with an explicit error budget — drift either way (more errors = regression, fewer = a closed gap waiting on a lower budget) fails the test:

| Example | Error budget |
|---|---|
| `examples/data/rle.av` | 3 |
| `examples/data/quicksort.av` | 5 |
| `examples/data/json.av` | 89 |

The budgets are not a target; they are a regression net. The umbrella issue for closing them is [#114](https://github.com/jasisz/lumen-rs/issues/114).

## Comparison with Lean

See [docs/transpilation.md](transpilation.md) for a side-by-side comparison.

In short: Lean is the gold standard (kernel-verified proofs), Dafny is the quick check (Z3-automated, zero tactic effort). Use both.

### Proof arguments and search policy

Source lowering records claims, premises, source dependencies and induction
arguments in ProofIR. A separate `codegen::proof_search` pass may add concrete
`LawApplication` suggestions. Its explicit `ApplicationSearchBudget` limits
discovery; `ApplicationSearchReport` records counts and steps reaching limits.
Running it with a zero budget removes suggestions without changing the source
obligations or induction. Both backends still check suppliers and applications.

Dafny's fixed-count unfolding policy lives in
`codegen::dafny::reasons::unfolding`, outside ProofIR. It retains the existing
small-literal and scalar-given restrictions and fuel budgets. These are partial
solver heuristics, not source bounds or a strategy shared with Lean. The
separate ordinary-law fuel policy in `law_search` also remains Dafny-specific.

The list library checks reverse/append and double-reversal lemmas. Merely
reaching `List.reverse` from an acyclic constructor no longer enables a
universal reversal pool or changes induction. That experimental strategy was
withdrawn after review; signed-frame readback is again an open diagnostic
target, not a required success or a counted coverage gain.

`tools/proof_search_matrix.py` records actual same-source results and time for
both backends, including equivalent source refactorings. It records failures
and timeouts as such; a completed matrix is not a claim that its laws passed.

See the [measured comparison and remaining limits](proof-search-policy.md).
