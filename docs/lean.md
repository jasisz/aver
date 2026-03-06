# Lean Backend

Lean is the proof-export backend for Aver.

Use it when you want:
- Lean 4 artifacts for pure Aver code
- executable proof obligations from colocated `verify`
- universal theorems for supported `verify law` shapes
- a path from Aver code to formal verification

This is not a second execution runtime for effectful programs.

## Quick start

```bash
aver proof examples/law_auto.av --verify-mode auto -o /tmp/law-auto-lean
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
- skips effectful functions and `main`
- turns colocated `verify` / `verify law` intent into Lean proof artifacts

## Verify emission

`verify` blocks become Lean proof obligations:

- default (`--verify-mode auto`): `example : <lhs> = <rhs> := by native_decide`
- fallback (`--verify-mode sorry`): `example : <lhs> = <rhs> := by sorry`
- theorem stubs (`--verify-mode theorem-skeleton`): named `theorem ... := by sorry`

`verify ... law ...` always emits expanded sample theorems from `given` domains:
- `theorem ..._sample_n := by native_decide`

When Aver can auto-prove the universal law shape, it also emits:
- `theorem <fn>_law_<name> : ∀ ..., lhs = rhs := by ...`

When `law <ident>` names an existing pure function and the law body compares `foo(args)` against `fooSpec(args)`, Aver treats that as a canonical spec reference:
- `verify fib law fibSpec` emits `theorem fib_eq_fibSpec : ∀ ..., fib ... = fibSpec ... := by ...`
- `aver context` also records `fibSpec` as a spec for `fib`

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
- direct implementation-vs-spec laws of the form `foo(args) = fooSpec(args)`

## Proof mode

Recommended mode:

```bash
aver proof my_module.av --verify-mode auto -o out/
```

That combination means:
- regular `verify` cases become executable Lean checks via `native_decide`
- supported `verify law` shapes get real universal proofs
- recursive pure code inside the supported proof subset is emitted as total Lean defs
- unsupported recursive pure functions are called out explicitly and emitted with `partial` fallback

The current proof export supports:
- single-function `Int` countdown on an `Int` parameter (`n -> n - 1`)
- single-function structural recursion on first `List<_>` parameter
- single-function `String + pos` recursion on `(String, Int)` signatures
- mutual recursion SCC with first-parameter `Int` countdown
- mutual recursion SCC with ranked `String + pos` progress
- mutual recursion SCC with ranked `sizeOf` descent

## Current end-to-end examples

These examples currently go through Lean export and `lake build` end to end:

- `examples/spec_laws.av`
- `examples/law_auto.av`
- `examples/map.av`
- `examples/fibonacci.av`
- `examples/rle.av`
- `examples/json.av`

These examples currently build with plain Lean export:

- `examples/hello.av`
- `examples/shapes.av`
- `examples/calculator.av`

## Hard-fail guarantees

Lean codegen does not silently mask unresolved compiler internals:

- `Expr::Resolved` in codegen input is a hard codegen error
- `Type::Unknown` in codegen input is a hard codegen error
- `sorry` can be emitted only when explicitly requested with `--verify-mode sorry`
