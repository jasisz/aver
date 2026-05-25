# Dafny Backend

Dafny is the automated-verification backend for Aver. It emits `verify law` blocks as Dafny lemmas and lets Z3 attempt proofs without hand-written tactics.

Use it when you want:
- automated checking of `verify law` properties
- automated checking of Oracle-lifted laws over classified effects
- Z3/SMT solver attempting universal proofs for you
- a quick validation of whether your laws hold before investing in Lean proof strategies

For each `verify law` block, the backend emits two things:

1. **Sample assertions** — concrete smoke tests from the `given` domain (e.g. `assert fib(5) == fibSpec(5)`), capped at 5 to avoid Z3 timeouts
2. **Universal lemma** — `lemma` with `when` as `requires` and the law as `ensures`, proved by Z3

The samples may time out on deeply recursive computations — that is expected. The lemma is the primary verification target.

## Quick start

```bash
aver proof examples/data/fibonacci.av --backend dafny -o /tmp/fib-dafny
cd /tmp/fib-dafny && dafny verify fibonacci.dfy
```

Requires [Dafny](https://github.com/dafny-lang/dafny) (4.x+) installed with Z3. On macOS: `brew install dafny`.

## What it generates

A single `.dfy` file containing:

- **Prelude**: `Result<T,E>`, `Option<T>`, list/map/string helpers
- **Datatypes**: user-defined `record` → `datatype`, `type` (sum) → `datatype`
- **Functions**: pure Aver functions → `function` with `decreases` clauses
- **Oracle-lifted functions**: classified effectful functions become pure functions with explicit oracle/capability parameters
- **Lemmas**: `verify law` blocks → `lemma` with `ensures` and optional inductive hints

## What it does NOT generate

- `verify` cases (non-law concrete assertions) — Z3 can't efficiently compute deeply recursive functions on specific inputs; Lean's `native_decide` is the right tool for this
- Unclassified effectful functions — only pure functions and Oracle-lifted classified effects are emitted
- Functions using `?` (ErrorProp) — Dafny pure functions cannot express early-return Err propagation
- `fn main()` — entry point is skipped

## How it maps Aver → Dafny

| Aver | Dafny |
|---|---|
| `Int` | `int` |
| `Float` | `real` |
| `String` | `string` |
| `Bool` | `bool` |
| `List<T>` | `seq<T>` |
| `Map<K,V>` | `map<K,V>` |
| `Result<T,E>` | `Result<T,E>` (prelude datatype) |
| `Option<T>` | `Option<T>` (prelude datatype) |
| `record Foo` | `datatype Foo = Foo(fields...)` |
| `type Bar = A \| B(Int)` | `datatype Bar = A \| B(b_0: int)` |
| `match x: true → a, false → b` | `if x then a else b` |
| `match n: 0 → base, _ → f(n-1)` | `if n == 0 then base else f(n-1)` |
| `match xs: [] → a, [h,..t] → b` | `if \|xs\| == 0 then a else var h := xs[0]; var t := xs[1..]; b` |
| `x / y` | `x / y` (Dafny flags unproved non-zero divisor) |
| `verify f law name` | sample `method` + universal `lemma` |

## Termination

Recursive functions fall into three buckets based on the shared classifier in `codegen::recursion::detect`:

**Direct-recursion patterns** — emitted as normal Dafny `function`s with inferred `decreases` clauses:
- List parameter → `decreases |xs|`
- String parameter → `decreases |s|`
- Int countdown (`match n { 0 -> …; _ -> recur(n-1, …) }`) → `requires n >= 0` + `decreases n`. Callers discharge the `requires` via Dafny's auto-inference from surrounding `if`/`match` shapes — `match (n < 0) { false -> worker(n) }` resolves to `n >= 0` automatically.
- Int countdown with explicit `match n < 0` base → `decreases if n >= 0 then n else 0` (no `requires`, the body itself handles the negative case).

**Mutual-recursion SCCs** — preferred path emits as native `decreases` tuples when every member has a measurable `List`/`Vector`/`String` parameter (most BigInt-style SCCs):

```dafny
function fn(args): T
  decreases <sizeof_measure>, <rank>
{ <body with intra-SCC calls unchanged> }
```

The size measure sums `|seq_param|` for every `List`/`Vector`/`String` parameter; the rank is the recursion classifier's topo position over "same-measure" callees, so a call that keeps the size constant decreases lexicographically on rank instead. Z3 unfolds these to ground terms during proof obligations — no fuel ceiling on large literals (BigInt's `10⁹`).

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
- Single fns whose body uses `?` that the lowering pass can't elaborate into a pure match — keeps the name in scope for downstream references instead of silently dropping the fn.

Lemmas whose `ensures` references an opaque fn (axiom or fuel-guarded) short-circuit their body to `assume {:axiom} <ensures>;` — parallel to Lean's `sorry`, accepted on trust rather than derived from unfolding. Dafny still type-checks the whole file; users add their own lemma proofs where the axiom fallback bites.

## Refinement records (refinement-via-opaque)

An Aver `record X { v: Int }` paired with a validating smart
constructor `fn fromX(n: Int) -> Result<X, String>` whose body matches
`match <pred(n)> { true -> Result.Ok(X(v = n)); false -> Result.Err(_) }`
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

Triggers for single-field `Int` carriers; `Float` / `String` and
multi-field records stay on the plain `datatype X = X(v: int)` shape
(no universal algebraic laws to exploit). Cross-module emit is
identical to standalone — `aver proof natural.av` and
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

- **No verify cases**: Z3 times out on deep computations like `fib(12) == 144`
- **Constructor collisions**: if a user type defines variants named `Ok`/`Err`, Dafny may report ambiguity errors
- **Opaque builtins**: `IntToString`, `FloatFromString`, `CharToCode` etc. are declared without bodies — Z3 knows their signatures but can't reason about their implementation
- **Complex laws**: laws involving indirect recursion, accumulator patterns, or multi-function chains may not be provable by Z3 alone

When a law's lemma comes out with an empty body, see [transpilation.md → Debugging a law that didn't auto-prove](transpilation.md#debugging-a-law-that-didnt-auto-prove) for the `--emit-ir-after=law_lower` workflow that tells you whether the classifier matched a strategy or fell through to backend dispatch.

## End-to-end smoke tests

`tests/proof_spec.rs` gates `dafny verify` on every IR-clean example end-to-end. The flagship examples that still carry pre-IR-migration gaps are tracked with an explicit error budget — drift either way (more errors = regression, fewer = a closed gap waiting on a lower budget) fails the test:

| Example | Error budget |
|---|---|
| `examples/data/fibonacci.av` | 1 |
| `examples/data/rle.av` | 4 |
| `examples/data/quicksort.av` | 5 |
| `examples/data/json.av` | 89 |

The budgets are not a target; they are a regression net. The umbrella issue for closing them is [#114](https://github.com/jasisz/lumen-rs/issues/114).

## Comparison with Lean

See [docs/transpilation.md](transpilation.md) for a side-by-side comparison.

In short: Lean is the gold standard (kernel-verified proofs), Dafny is the quick check (Z3-automated, zero tactic effort). Use both.
