# Rust Backend

Rust is the deployment backend for Aver.

Use it when you want:
- a native Cargo project
- a normal Rust build/test/run loop
- deployment without the Aver interpreter

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

## What it generates

Generates a complete Cargo project:

```
out/
  Cargo.toml
  src/
    main.rs
```

The generated `main.rs` includes:
- runtime bridge (`aver_rt` module re-exporting the shared `aver-rt` crate)
- shared runtime type imports for built-in service records when needed
- user-defined types as Rust `struct`s and `enum`s
- all functions, including inlined module dependencies
- `fn main()` entry point
- `#[cfg(test)]` verify blocks as Rust tests

## Runtime dependency

`Cargo.toml` is generated around the shared `aver-rt` runtime crate. Service-specific runtime features are enabled only when needed:

| Aver service | Rust crate |
|-------------|------------|
| no `Http` effects | `aver-rt = { version = "=0.1.0" }` |
| `Http` effects present | `aver-rt = { version = "=0.1.0", features = ["http"] }` |

`ureq` is pulled transitively by `aver-rt/http`; generated projects do not declare it directly.

For local runtime development from the Aver repository, set `AVER_RUNTIME_PATH` before running `aver compile` to force a path dependency instead of the crates.io release:

```bash
AVER_RUNTIME_PATH="$(pwd)/aver-rt" aver compile examples/hello.av -o /tmp/hello-rs
```

## Supported features

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

## Running verify blocks

Verify blocks are emitted as `#[test]` functions:

```bash
aver compile examples/calculator.av -o /tmp/calc
cd /tmp/calc && cargo test
```

## Module inlining

When a program has `depends [Examples.Fibonacci]`, the transpiler:
1. loads the dependent `.av` file recursively, with circular import detection
2. inlines all exported types and functions into the same `main.rs`
3. prefixes names to avoid collisions: `Examples.Fibonacci.fib` → `examples_fibonacci_fib`

No Rust `mod` blocks are generated. Everything lives at the top level.

## Service runtime architecture

Generated `main.rs` re-exports `aver-rt` and, when needed, imports shared service record types. The actual service implementations live in `aver-rt`:

- `Tcp`: shared `aver-rt::tcp` runtime with persistent connection map
- `Http`: shared `aver-rt::http` client, enabled by the `http` feature
- `HttpServer`: shared `aver-rt::http_server` loop and request/response types
- `Console`, `Time`, `Disk`, `Env`: shared helpers from `aver-rt`
