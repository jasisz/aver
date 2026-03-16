# Dafny Backend

Dafny is the automated-verification backend for Aver. It emits `verify law` blocks as Dafny lemmas and lets Z3 attempt proofs without hand-written tactics.

Use it when you want:
- automated checking of `verify law` properties
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
- **Lemmas**: `verify law` blocks → `lemma` with `ensures` and optional inductive hints

## What it does NOT generate

- `verify` cases (non-law concrete assertions) — Z3 can't efficiently compute deeply recursive functions on specific inputs; Lean's `native_decide` is the right tool for this
- Effectful functions — only pure functions are emitted
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

Recursive functions get automatic `decreases` clauses:
- Int parameter → `decreases if n >= 0 then n else 0`
- List parameter → `decreases |xs|`
- String parameter → `decreases |s|`

Scalar `match` arms are emitted as if-then-else chains so Dafny's verifier can see branch guards. Dafny may report "decreases clause might not decrease" for functions where the recursive branch is reachable with negative inputs — this is correct, as those functions genuinely don't terminate for all inputs.

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

## Comparison with Lean

See [docs/transpilation.md](transpilation.md) for a side-by-side comparison.

In short: Lean is the gold standard (kernel-verified proofs), Dafny is the quick check (Z3-automated, zero tactic effort). Use both.
