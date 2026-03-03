# Transpilation (`aver compile`)

Aver programs can be transpiled to standalone native projects. The generated code is human-readable, builds with the target language's standard toolchain, and runs without the Aver interpreter.

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
  -o, --output <DIR>          Output directory (default: out)
  -t, --target <TARGET>       Transpilation target (default: rust)
      --name <NAME>           Project/binary name (default: derived from file)
      --module-root <PATH>    Module resolution root (default: cwd)
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
- Runtime helpers (`aver_rt` module with `AverDisplay` trait, list operations, map support)
- Service runtimes (only when the program uses them — conditional emission)
- User-defined types as Rust `struct`s and `enum`s
- All functions (including inlined module dependencies)
- `fn main()` entry point
- `#[cfg(test)]` verify blocks as Rust tests

#### Dependencies

`Cargo.toml` is generated with only the dependencies needed by the program:

| Aver service | Rust crate |
|-------------|------------|
| `Http` | `ureq` (blocking HTTP client) |
| (no services) | no external dependencies |

`Tcp` and `HttpServer` use `std::net` — no extra crates needed.

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
| Pipe operator (`\|>`) | OK |
| Error propagation (`?`) | OK |
| Auto-memoization | OK |
| Tail-call optimization | OK |
| Module imports (`depends [X]`) | OK |
| `Console` service | OK |
| `Http` service | OK |
| `HttpServer` service (`listen`, `listenWith`) | OK |
| `Tcp` service (persistent connections) | OK |
| `Disk` service | OK |
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

Service runtimes are only emitted when the program actually uses them (detected via effect declarations):

- **Tcp**: Thread-local connection map, `AtomicU64` for connection IDs, `BufReader<TcpStream>` for persistent connections
- **Http**: `ureq` client with 10s timeout, maps to Aver's `HttpResponse` struct
- **HttpServer**: Blocking `TcpListener`, parses HTTP/1.1 requests, calls handler function per request. `listenWith` supports a generic context parameter (cloned per request)
- **Console**: Direct `println!`/`eprintln!` + `stdin().read_line()`
- **Disk**: `std::fs` operations mapped to Aver's `Result` type

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
  - `example : <lhs> = <rhs> := by sorry`

#### Hard-fail guarantees

Lean codegen does not silently mask unresolved compiler internals:

- `Expr::Resolved` in codegen input is a hard codegen error.
- `Type::Unknown` in codegen input is a hard codegen error.
- `sorry` is reserved for exported `verify` obligations (and not used as fallback for those internal states).

## Adding a new target

To add a new transpilation target (e.g., `js`, `go`, `python`):

1. Add a variant to `Target` enum in `src/main/cli.rs`
2. Create `src/codegen/<target>/mod.rs` with a `pub fn transpile(ctx: &CodegenContext) -> ProjectOutput`
3. Add a match arm in `cmd_compile` (`src/main/commands.rs`)
4. Add `pub mod <target>;` in `src/codegen/mod.rs`

The `CodegenContext` struct is backend-agnostic — it contains the full type-checked AST, function signatures, memo info, and module dependencies. Each backend only needs to implement `transpile()`.
