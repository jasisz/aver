# Rust Backend

Rust is the deployment backend for Aver.

Use it when you want:
- a native Cargo project
- a normal Rust build/test/run loop
- deployment without the Aver interpreter

## Quick start

```bash
aver compile examples/core/hello.av -o /tmp/hello-rs
cd /tmp/hello-rs && cargo build && cargo run
```

Output:
```
Compiled examples/core/hello.av → /tmp/hello-rs/ [Rust]
  cd /tmp/hello-rs && cargo build && cargo run
```

## What it generates

Generates a complete Cargo project:

```
out/
  Cargo.toml
  src/
    main.rs
    runtime_support.rs
    aver_generated/
      mod.rs
      entry/
        mod.rs
      ...
```

The generated project includes:
- `src/main.rs` with the runtime prelude and final entrypoint
- `src/runtime_support.rs` for the shared `aver-rt` bridge and shared runtime types
- `src/aver_generated/.../mod.rs` files that preserve the Aver module graph as Rust modules
- `src/verify.rs` when the entry module has `verify` blocks

The generated Rust keeps:
- user-defined types as Rust `struct`s and `enum`s inside their originating modules
- direct `depends [...]` modules as explicit Rust imports inside generated module files
- module-qualified Aver calls such as `Domain.Tasks.replayTask(...)` as qualified Rust paths
- `fn main()` in `src/main.rs` delegating to `aver_generated::entry::main()`
- `#[cfg(test)]` verify blocks as Rust tests for the entry module

`src/main.rs` includes:
- runtime bridge (`aver_rt` module re-exporting the shared `aver-rt` crate)
- shared runtime type imports for built-in service records when needed
- the root `aver_generated` module tree
- the final `fn main()` entry point

## Runtime dependency

`Cargo.toml` is generated around the shared `aver-rt` runtime crate. Service-specific runtime features are enabled only when needed:

| Aver service | Rust crate |
|-------------|------------|
| no `Http` effects | `aver-rt = { version = "=0.2.0" }` |
| `Http` effects present | `aver-rt = { version = "=0.2.0", features = ["http"] }` |

`ureq` is pulled transitively by `aver-rt/http`; generated projects do not declare it directly.

For local runtime development from the Aver repository, set `AVER_RUNTIME_PATH` before running `aver compile` to force a path dependency instead of the crates.io release:

```bash
AVER_RUNTIME_PATH="$(pwd)/aver-rt" aver compile examples/core/hello.av -o /tmp/hello-rs
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
| Exact method-level effects (`Http.get`, `Disk.readText`, etc.) | OK |

## Running verify blocks

Verify blocks are emitted as `#[test]` functions:

```bash
aver compile examples/core/calculator.av -o /tmp/calc
cd /tmp/calc && cargo test
```

## Module lowering

When a program has `depends [Data.Fibonacci]`, the transpiler:
1. loads the dependent `.av` file recursively, with circular import detection
2. lowers each Aver module into a Rust module under `src/aver_generated/...`
3. imports direct `depends [...]` modules explicitly in the generated Rust
4. keeps qualified calls module-qualified: `Data.Fibonacci.fib` becomes `crate::aver_generated::data::fibonacci::fib`

This avoids the old giant single-file output and keeps medium projects reviewable in generated Rust.

## Service runtime architecture

Generated Rust uses `aver-rt` as the shared runtime. The actual service implementations live there:

- `Tcp`: shared `aver-rt::tcp` runtime with persistent connection map
- `Http`: shared `aver-rt::http` client, enabled by the `http` feature
- `HttpServer`: shared `aver-rt::http_server` loop and request/response types
- `Console`, `Time`, `Disk`, `Env`, `Args`: shared helpers from `aver-rt`
