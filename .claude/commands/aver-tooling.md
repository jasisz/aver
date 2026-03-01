You are an expert at using the Aver language toolchain. When helping with Aver projects, use these tools and workflows.

## CLI commands

All commands run via `cargo run --` (dev) or `aver` (installed binary).

### `aver run <file.av>`

Execute an Aver program. Requires `fn main()` as entry point.

```bash
aver run examples/hello.av
aver run examples/app.av --module-root .
```

`--module-root <DIR>` — resolve `depends [...]` from this directory (default: cwd).

### `aver check <file.av>`

Static analysis warnings (not errors):
- Module has no `intent =`
- Function with effects/Result return has no `?` description
- File exceeds 250 lines

`fn main()` is exempt from `?` requirement.

### `aver verify <file.av>`

Run all `verify` blocks in the file. Each `left => right` case is an equality assertion.

```bash
aver verify examples/calculator.av
```

Exit code 0 = all pass, 1 = failures.

### `aver compile <file.av>`

Transpile Aver to a Rust/Cargo project.

```bash
aver compile examples/fibonacci.av -o /tmp/fib_out
aver compile app.av -o out --name my-app --module-root .
```

Flags:
- `-o <DIR>` — output directory (default: `out/`)
- `--name <NAME>` — project name (default: derived from filename)
- `--module-root <DIR>` — resolve `depends` from here
- `-t rust` — target backend (only Rust for now)

Generated project: `cd <output> && cargo build && cargo run`.

### `aver repl`

Interactive REPL with persistent state across lines.

Commands: `:quit`/`:q`, `:clear`/`:c`, `:env`, `:help`/`:h`.

Multi-line input: indent continuation lines; empty line submits block. Type-checked per entry.

### `aver replay <dir> [--test] [--diff]`

Replay a previously recorded execution.

```bash
aver run examples/services/console_demo.av --record recordings/
aver replay recordings/ --test --diff
```

`--record <DIR>` on `aver run` captures all effects as JSON.
`--test` asserts replayed output matches recording.
`--diff` shows differences.

### `aver context <file.av>`

Export project context as Markdown for LLM consumption.

```bash
aver context examples/calculator.av
aver context decisions/architecture.av --decisions-only
```

### `aver decisions [--docs]`

Export decision blocks (ADR-style).

`--docs` — generate/update decision documentation.

## Debugging workflow: verify-first

For logic bugs, the default workflow is:

1. **Reproduce** — write a failing `verify` case that captures the bug
2. **Fix** — modify the implementation until `aver verify` passes
3. **Keep** — the verify case stays as a permanent regression guard

```bash
# Step 1: Add failing case to the verify block
verify myFunction
    myFunction(edgeCase) => expectedResult

# Step 2: Run and see failure
aver verify file.av

# Step 3: Fix implementation, re-run until green
aver verify file.av
```

This is preferred over ad-hoc print debugging. Debugging artifacts become executable specs.

### When verify-first doesn't apply

- Profiling / performance analysis
- Debugging nondeterministic external systems (Http, Tcp)
- Latency issues
- Effect-related bugs (use `--record` / `replay` instead)

## Understanding error messages

### Parse errors

Format: `Parse error [L:C]: message`

Common causes:
- `if`/`else` used → "Unknown keyword. Use match instead."
- `val`/`var` used → "Unknown keyword 'val'. Bindings are just: x = 5"
- Missing indent after fn header → "Expected indented block"
- `match x:` with colon → colon is NOT used after match subject
- `User(name: "Alice")` → use `=` not `:` for record fields

### Type errors

Format: `Type error [L:C]: message`

Common causes:
- `x = []` without annotation → "Cannot infer element type of empty list. Add annotation: x: List<T> = []"
- Missing effect declaration → "Function 'f' uses effect [Console] but does not declare it"
- Wrong constructor → "Expected Result<Int, String>, got Int"
- Duplicate binding → "'x' is already defined in this scope"
- `Any` used as type → "'Any' is not a valid type annotation"

### Runtime errors

Format: `Runtime error: message`

- `?` on non-Result → "Error propagation (?) requires a Result value"
- Division by zero → caught by Rust runtime
- Index out of bounds in `List.get` → returns `Option.None` (not an error)

## Record/Replay for effect debugging

When debugging effectful code (Console, Http, Disk, Tcp):

```bash
# Record a session
aver run app.av --record sessions/run1/

# Replay deterministically
aver replay sessions/run1/ --test

# Compare two runs
aver replay sessions/run1/ --diff
```

The recording captures all effect inputs/outputs as JSON. Replay substitutes recorded values — no actual network/disk calls.

## Transpiler verification

After transpiling, verify the generated Rust project:

```bash
aver compile myapp.av -o /tmp/myapp
cd /tmp/myapp
cargo build          # must compile
cargo test           # verify blocks become #[test] functions
cargo run            # run the program
cargo build --release && time ./target/release/myapp  # benchmark
```

Key properties of generated code:
- No closures → no lifetime issues
- Immutable values → owned data with move semantics
- TCO functions → `loop { ... continue }` rewrite
- Memoized functions → `thread_local!` cache
- Effects are erased (enforced only at interpreter level)
- Last-use variables are moved, not cloned
- Copy types (i64, f64, bool) never get `.clone()`

## Module resolution

Modules are resolved from the module root (default: cwd):

```
depends [Examples.Fibonacci]
```

Resolves to `examples/fibonacci.av` (dots → directory separators, lowercase).

Use `--module-root` to set a different root:

```bash
aver run src/main.av --module-root src/
aver compile src/main.av -o out --module-root src/
```

Circular imports are a hard error with explicit chain: `A -> B -> A`.
