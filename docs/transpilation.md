# Transpilation (`aver compile`)

Aver programs can be transpiled to native projects. The generated code is human-readable, builds with the target language's standard toolchain, and runs without the Aver interpreter.

## Quick start

```bash
aver compile examples/hello.av -o /tmp/hello-rs
cd /tmp/hello-rs && cargo build && cargo run
```

Output:
```
Compiled examples/hello.av → /tmp/hello-rs/ [Rust]
  cd /tmp/hello-rs && cargo build && cargo run
```

## CLI flags

```
aver compile <FILE> [OPTIONS]

Options:
  -o, --output <OUTPUT>            Output directory for the generated project
  -t, --target <TARGET>            Transpilation target backend (default: rust)
      --name <NAME>                Project name (default: derived from file name)
      --module-root <MODULE_ROOT>  Resolve `depends [...]` from this root (default: current working directory)
      --lean-verify <LEAN_VERIFY>  Lean-only verify emission mode: auto | sorry | theorem-skeleton
      --lean-proof-mode            Lean-only fail-fast gate for proof-unsafe constructs
```

## Targets

### `rust` (default)

Generates a complete Cargo project:

```
out/
  Cargo.toml        # dependencies based on detected services
  src/
    main.rs         # all code in a single file
```

The generated `main.rs` includes:
- Runtime bridge (`aver_rt` module re-exporting the shared `aver-rt` crate)
- Shared runtime type imports for built-in service records (`Header`, `HttpResponse`, `HttpRequest`, `Tcp_Connection`) when needed
- User-defined types as Rust `struct`s and `enum`s
- All functions (including inlined module dependencies)
- `fn main()` entry point
- `#[cfg(test)]` verify blocks as Rust tests

#### Dependencies

`Cargo.toml` is generated around the shared `aver-rt` runtime crate. Service-specific runtime features are enabled only when needed:

| Aver service | Rust crate |
|-------------|------------|
| no `Http` effects | `aver-rt = { path = "...", version = "=0.0.1" }` |
| `Http` effects present | `aver-rt = { path = "...", version = "=0.0.1", features = ["http"] }` |

`ureq` is pulled transitively by `aver-rt/http`; generated projects no longer declare it directly.

#### Supported features

Everything the interpreter supports is transpilable:

| Feature | Status |
|---------|--------|
| Arithmetic, comparisons, string interpolation | OK |
| `match` with all pattern types | OK |
| `Result<T,E>`, `Option<T>` constructors + match | OK |
| User-defined sum types (`type Shape`) | OK |
| User-defined records (`record User`) | OK |
| Record update (`User.update(u, field = val)`) | OK |
| List literals, `List.*` operations | OK |
| Map literals, `Map.*` operations | OK |
| Tuple literals, tuple patterns | OK |
| Error propagation (`?`) | OK |
| Auto-memoization | OK |
| Tail-call optimization | OK |
| Module imports (`depends [X]`) | OK |
| `Console` service | OK |
| `Http` service | OK |
| `HttpServer` service (`listen`, `listenWith`) | OK |
| `Tcp` service (persistent connections) | OK |
| `Disk` service | OK |
| `Env` service | OK |
| `Time` service | OK |
| `verify` blocks → `#[cfg(test)]` | OK |
| Effect aliases (`effects X = [...]`) | OK (expanded at compile time) |

#### Running verify blocks

Verify blocks are emitted as `#[test]` functions:

```bash
aver compile examples/calculator.av -o /tmp/calc
cd /tmp/calc && cargo test
```

#### Module inlining

When a program has `depends [Examples.Fibonacci]`, the transpiler:
1. Loads the dependent `.av` file (recursively, with circular import detection)
2. Inlines all exported types and functions into the same `main.rs`
3. Prefixes names to avoid collisions: `Examples.Fibonacci.fib` → `examples_fibonacci_fib`

No Rust `mod` blocks are generated — everything lives at the top level for simplicity.

#### Service runtime architecture

Generated `main.rs` now re-exports `aver-rt` and, when needed, imports shared service record types. The actual service implementations live in `aver-rt`:

- **Tcp**: shared `aver-rt::tcp` runtime with persistent connection map
- **Http**: shared `aver-rt::http` client (enabled by the `http` feature)
- **HttpServer**: shared `aver-rt::http_server` loop and request/response types
- **Console / Time / Disk / Env**: shared helpers from `aver-rt`

### `lean` (experimental / WIP)

Generates a Lean 4 project:

```
out/
  lakefile.lean
  lean-toolchain
  <Project>.lean
```

Example:

```bash
aver compile examples/fibonacci.av -t lean -o /tmp/fib-lean
cd /tmp/fib-lean && lake build
```

#### Scope

- Transpiles pure core logic (types + pure functions + decisions).
- Skips effectful functions and `main`.
- Emits `verify` blocks as Lean proof obligations:
  - default (`--lean-verify auto`): `example : <lhs> = <rhs> := by native_decide`
  - optional fallback: `--lean-verify sorry` → `example : <lhs> = <rhs> := by sorry`
  - theorem stubs: `--lean-verify theorem-skeleton` → named `theorem ... := by sorry`
  - `verify ... law ...` emits both:
    - universal theorem skeleton: `theorem <fn>_law_<name> : ∀ ..., lhs = rhs := by ...`
    - expanded sample theorems from `given` domains: `theorem ..._sample_n := by native_decide`
  - conservative auto-proofs for universal law theorem (when `--lean-verify auto`):
    - reflexive law shape (`lhs` and `rhs` syntactically identical) → `rfl`
    - commutative law on simple `Int` binary wrappers (`a + b`, `a * b`)
    - associative law on same wrapper shape (`f(f(a,b),c) = f(a,f(b,c))`)
    - identity law on same wrapper shape (`f(a,0)=a`, `f(0,a)=a`, `f(a,1)=a`, `f(1,a)=a`)
  - all other `verify law` cases fall back to `sorry` in the universal theorem, while sample theorems still run.
- Optional strict gate: `--lean-proof-mode`
  - accepts only supported recursion schemes for total Lean emission:
    - single-function `Int` countdown on first parameter (`n -> n - 1`)
    - single-function structural recursion on first `List<_>` parameter
    - single-function `String+pos` advance (`(s, pos)` with recursive calls on same `s` and `pos + k`)
    - mutual recursion SCC with first-parameter `Int` countdown
    - mutual recursion SCC with `(String, Int)` where same-pos edges are rank-decreasing and remaining edges strictly advance `pos`
    - mutual recursion SCC with ranked `sizeOf` measure on selected parameters
  - rejects recursive pure functions outside that subset

#### Hard-fail guarantees

Lean codegen does not silently mask unresolved compiler internals:

- `Expr::Resolved` in codegen input is a hard codegen error.
- `Type::Unknown` in codegen input is a hard codegen error.
- `sorry` can be emitted only when explicitly requested (`--lean-verify sorry`), and is not used as fallback for internal compiler states.
- `--lean-proof-mode` rejects unsupported recursion patterns before files are generated.
- `--lean-proof-mode` requires `--lean-verify auto` (rejects `sorry` and `theorem-skeleton`).

## Adding a new target

To add a new transpilation target (e.g., `js`, `go`, `python`):

1. Add a variant to `Target` enum in `src/main/cli.rs`
2. Create `src/codegen/<target>/mod.rs` with a `pub fn transpile(ctx: &CodegenContext) -> ProjectOutput`
3. Add a match arm in `cmd_compile` (`src/main/commands.rs`)
4. Add `pub mod <target>;` in `src/codegen/mod.rs`

The `CodegenContext` struct is backend-agnostic — it contains the full type-checked AST, function signatures, memo info, and module dependencies. Each backend only needs to implement `transpile()`.
