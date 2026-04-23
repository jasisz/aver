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
classified effect set, stub signatures, and trace-aware verify syntax.

## Verify emission

`verify` blocks become Lean proof obligations:

- default (`--verify-mode auto`): `example : <lhs> = <rhs> := by native_decide`
- fallback (`--verify-mode sorry`): `example : <lhs> = <rhs> := by sorry`
- theorem stubs (`--verify-mode theorem-skeleton`): named `theorem ... := by sorry`

`verify ... law ...` always emits expanded sample theorems from `given` domains:
- `theorem ..._sample_n := by native_decide`

When Aver can auto-prove the universal law shape, it also emits:
- `theorem <fn>_law_<name> : ∀ ..., lhs = rhs := by ...`

`when` clauses translate to extra theorem premises in Lean. Generic auto-proof
strategies still run on those guarded laws, but unmatched shapes fall back to
domain-guarded theorems over the explicit `given` cases. Parser/render
roundtrip laws such as `parse(render(x)) = x` currently live in that fallback
bucket unless some other generic shape discharges them first.

When `law <ident>` names an existing pure function and the law body compares `foo(args)` against `fooSpec(args)`, Aver treats that as a canonical spec reference:
- the generated theorem/comment uses the canonical `<fn>_eq_<spec>` naming
- `aver context` also records `fibSpec` as a spec for `fib`
- in `--verify-mode auto`, Aver emits the universal theorem only when the law shape is supported by a generic auto-proof strategy; otherwise it omits that theorem and leaves a comment instead

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

If Aver cannot auto-prove the universal law shape in `--verify-mode auto`, it omits that theorem and leaves a comment instead of emitting a fake `sorry` proof.

Conservative auto-proofs currently cover:
- reflexive law shape (`lhs` and `rhs` syntactically identical) → `rfl`
- commutative law on simple `Int` binary wrappers (`a + b`, `a * b`)
- associative law on same wrapper shape (`f(f(a,b),c) = f(a,f(b,c))`)
- identity law on same wrapper shape (`f(a,0)=a`, `f(0,a)=a`, `f(a,1)=a`, `f(1,a)=a`)
- direct structural induction on recursive sum types whose recursive fields are bare self-references, not container-wrapped occurrences
- canonical implementation-vs-spec laws when the backend can discharge them structurally
- canonical implementation-vs-spec laws that become definitionally equal after conservative `simp`-style identity cleanup (for example `x * x` vs `x * x + 0`)
- canonical implementation-vs-spec laws for second-order linear `Int` recurrences with a pair-state tail-recursive worker
- canonical implementation-vs-spec laws over linear `Int` arithmetic, proved via `change ...` and `omega`

The generated Lean prelude also includes one-character separator lemmas such as
`AverString.split` over `String.join(_, sep) ++ sep` for separator-free parts.
Those helper lemmas support exported code, but they are not themselves a claim
that delimiter-based parser/render laws are universally auto-proved.

## Proof mode

Recommended mode:

```bash
aver proof my_module.av --verify-mode auto -o out/
```

That combination means:
- regular `verify` cases become executable Lean checks via `native_decide`
- supported `verify law` shapes get real universal proofs
- unsupported `verify law` shapes stay as sample theorems and checked-domain
  theorems, with the universal theorem omitted
- recursive pure code inside the supported proof subset is emitted as total Lean defs
- unsupported recursive pure functions are called out explicitly and emitted with `partial` fallback

The current proof export supports:
- single-function `Int` countdown on an `Int` parameter (`n -> n - 1`)
- single-function second-order affine `Int` recurrences with `n < 0` guard, `0/1` case split, and a matching pair-state tail worker, emitted via a private `Nat` helper
- single-function structural recursion on any `List<_>` parameter
- single-function `String + pos` recursion on `(String, Int)` signatures
- mutual recursion SCC with first-parameter `Int` countdown
- mutual recursion SCC with ranked `String + pos` progress
- mutual recursion SCC with ranked structural descent over recursive parameters

## Current end-to-end examples

These examples are currently smoke-tested end to end with
`aver proof --verify-mode auto` plus `lake build`:

- `examples/formal/law_auto.av`
- `examples/data/fibonacci.av`
- `examples/data/quicksort.av`
- `examples/data/rle.av`
- `examples/data/json.av`
- `examples/core/grok_s_language.av`

Other modules have unit coverage for proof-subset classification and generated
Lean snippets, but are not currently listed here as end-to-end smoke cases.

## Hard-fail guarantees

Lean codegen does not silently mask unresolved compiler internals:

- `Expr::Resolved` in codegen input is a hard codegen error
- `Type::Unknown` in codegen input is a hard codegen error
- `sorry` can be emitted only when explicitly requested with `--verify-mode sorry`
