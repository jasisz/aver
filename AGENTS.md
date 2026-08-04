# Aver — AI Context File

## What is this?

Aver is a programming language designed for AI-assisted development. Its bytecode VM is written in Rust. The language prioritises human and machine readability: every function carries an optional prose description, and architectural decisions are first-class citizens expressed as `decision` blocks co-located with the code they describe. This file is the single entry point for any AI resuming work on this project — read it before touching any source file.

## Project philosophy

- Code is a letter to the next reader — who is increasingly an AI
- Every fragment must be self-sufficient (readable without context)
- Intent over implementation: signatures tell the full story
- Decisions are first-class citizens of the codebase

## AI discovery workflow

When entering this repo, do not start by reading raw source files exhaustively.

Prefer progressive discovery:
- start with `aver context <entry> --budget 10kb`
- use `--focus <fn_name>` to zoom into a specific function's dependency cone
- use the exported architecture map to choose the next module
- raise the budget or target a specific module only when the first map is too shallow

`aver context` is a navigation primitive for AI: start high, focus on what matters, then zoom in.

## Current status

### Language features

See [README.md](README.md) for project overview and workflow, [docs/language.md](docs/language.md) for the surface-language guide, and [docs/services.md](docs/services.md) for the standard library and effectful services.

Below: implementation details relevant to development only.

### Implementation notes

- **Constructor routing**: `Result.Ok(v)`, `Result.Err(v)`, `Option.Some(v)` route through `call_builtin` (`__ctor:Result.Ok` etc.). `Result` and `Option` registered as `Value::Namespace`. Match patterns use qualified names.
- **No flat builtins** (decision: `FullNamespaceEverywhere`). Namespace helpers live under their owning modules (`List`, `Vector`, `Map`, `String`, etc.), with pure list operations in `src/types/list.rs` and vector operations in `src/types/vector.rs`.
- **`Type::Named(String)`** in the type system: capitalized identifiers (including dotted names like `Tcp.Connection`) in type annotations resolve to named types. Compatible only with the same name or internal `Unknown` fallback.
- **`Tcp.Connection` record**: fields `id: String`, `host: String`, `port: Int`. No longer opaque — constructable via `Tcp.Connection(id = ..., host = ..., port = ...)`. Actual socket in thread-local `HashMap` keyed by `id`. `NEXT_ID: AtomicU64` generates "tcp-1", "tcp-2", etc.
- **Static type checker** (`src/types/checker/`): internal `Type::Unknown` recovery after earlier errors so analysis can continue. Bare `Unknown` does **not** satisfy concrete types in constraints — only nested `Unknown` is tolerated (gradual typing). Match pattern bindings are typed: `Result.Ok(x)` on `Result<Int, String>` gives `x: Int`.
- **TCO** (`src/tco.rs`): transform pass rewrites tail-position `FnCall` → `Expr::TailCall` in recursive SCCs. VM handles tail calls via frame reuse. Pipeline: `parse → tco_transform → typecheck → resolve → compile → execute`.
- **Compile-time variable resolution** (`src/resolver.rs`): `Ident("x")` → `Resolved(slot)` inside FnDef bodies.
- **Bytecode VM** (`src/vm/`): stack VM over `NanValue`, with language-shaped opcodes for lists, records, variants, wrappers, tuple literals/patterns, and tail calls. `src/vm/runtime.rs` is the host/effect bridge; `src/vm/execute.rs` is the core loop; `src/vm/compiler.rs` lowers resolved AST to bytecode.
- **`check` command**: warns when module has no `intent =`, function with effects/Result return has no `?` description, file exceeds 250 lines. `fn main()` is exempt from `?` requirement.
- **Entry-point effect enforcement**: `main`/top-level entry calls enforce declared effects at the entry boundary.
- **Opaque types** (`exposes opaque [T]`): module-level access control for types. An opaque type is visible in signatures (can be passed, returned, stored) but cannot be constructed, have its fields accessed, or be pattern-matched from outside the defining module. Enforced at compile time in the typechecker; `load_module_sigs` registers a dummy sig (type resolves) but omits field types, constructors, and variant info. Parser recognizes `exposes opaque` after the `Exposes` token by checking for `Ident("opaque")`.
- **WASM-GC backend** (`src/codegen/wasm_gc/`, feature-gated behind `--features wasm`): compiles Aver to wasm modules using the WebAssembly GC + tail-call proposals (typed structs/arrays, no linear-memory heap for first-class values). Two emission modes share the lowering pipeline: (a) `--target wasm-gc` for browsers / Workers / JS hosts via the `aver/*` host import ABI; (b) `--target wasip2` (and `aver run --wasip2`) for the WASI 0.2 / Component Model story — the same backend emits canonical-ABI WIT imports (`wasi:cli/stdout`, `wasi:filesystem/preopens`, `wasi:io/streams`, ...) and `src/codegen/wasip2/wrap.rs` wraps the core module via `wit-component`, no preview-1 adapter. Effect set on wasip2: Console, Time, Random, Args, Env (read), all Disk; Terminal/Http/Tcp/HttpServer rejected (host territory). The legacy linear-memory `--target wasm` backend was deleted in 0.18 Phase 1.8. ABI table in `abi.rs` is the single source of truth for import mappings.
- **Artifact certificates** (`aver-cert/`): `aver-cert` 0.1.x is an independently versioned verifier/process; `aver cert` is an exact subprocess shortcut. Public package version is `1` and manifest schema version is `2` (schema 2 made the subject `hostRoleTable` optional — `null` for modules without the Int box helper, pinned against a byte-derived proof of the helper's absence); `Plans.lean` is the sole authoritative plan data, while the verifier supplies the actual artifact bytes, Lean 4.32 wall, build, and witness. See [docs/certification.md](docs/certification.md) and [docs/certification-architecture.md](docs/certification-architecture.md).
- **Independent products** (`?!` / `!`): a tuple followed by `!` is a product of independent computations; `?!` adds Result unwrapping. `Expr::IndependentProduct(Vec<Spanned<Expr>>, bool)` in AST. Parser detects `?` + `!` or bare `!` after tuple in `parse_postfix`. Typechecker: `?!` verifies all elements are `Result<T, E>` with compatible error types and that elements are function calls; `!` infers as regular tuple. Interpreter: sequential evaluation with replay groups. Codegen: `std::thread::scope` with real parallelism. VM: `CALL_PAR` dispatches callable values plus per-branch arity, so aliases like `f = foo; (f(x), f(y))!` work. Replay: effects within a product share `group_id`, matched by `branch_path + effect_occurrence + effect_type + effect_args`, not execution order. See [docs/independence.md](docs/independence.md).

### Design omissions

See [docs/language.md](docs/language.md#what-aver-deliberately-omits) for the full list of intentional omissions (no `if`/`else`, no loops, no `null`, no exceptions, no mutable state, no magic).

## Architecture in one page

```
src/
  lexer.rs            — Converts source text to a flat Vec<Token>.
                        Manages an indent_stack to emit INDENT/DEDENT tokens
                        for significant indentation. Handles string interpolation
                        by collecting raw expression source inside "{ }".

  ast.rs              — Pure data: the Abstract Syntax Tree.
                        No logic, no methods. Defines TokenKind, Expr, Stmt,
                        FnDef, Module, VerifyBlock, DecisionBlock, TopLevel.

  parser/             — Recursive-descent parser consuming Vec<Token>.
                        Produces Vec<TopLevel>. Split into submodules:
                        core.rs    — Parser struct, token helpers, error type
                        expr.rs    — Expression parsing (precedence chain)
                        functions.rs — fn/verify/decision parsing
                        blocks.rs  — fn body, match arms, indented blocks
                        patterns.rs — match patterns
                        module.rs  — module block, effect sets, top-level dispatch
                        types.rs   — type annotations, record/sum type defs

  types/
    mod.rs            — enum Type, parse_type_str, compatible()
    int.rs            — Int.* (pure)
    float.rs          — Float.* (pure)
    string.rs         — String.* (pure)
    list.rs           — List.len/prepend/concat/reverse/contains/zip/take/drop (pure, recursive)
    vector.rs         — Vector.new/get/set/len/fromList/toList (pure, indexed O(1) COW)
    map.rs            — Map.* (pure)
    char.rs           — Char.toCode/fromCode (pure, not a type)
    bool.rs           — Bool.or/and/not (pure)
    byte.rs           — Byte.toHex/fromHex (pure, not a type)
    crypto.rs         — Crypto.sha256 (pure, not a type)
    checker/          — Static type checker. Split into submodules:
      mod.rs          — TypeChecker struct, constraint_compatible(), run_type_check_*
      infer.rs        — infer_type: expressions, calls, match, patterns
      flow.rs         — check_fn_body, check_stmts, effect propagation
      builtins.rs     — service_sigs, record_field_types registration
      modules.rs      — cross-module type checking, base signature merging
      check.rs        — check, check_body, check_with_loaded (driver)
      tests.rs        — unit tests for checker internals

  tco.rs              — Tail-call optimization transform pass.
                        Runs after parsing, before type-checking.

  call_graph.rs       — Call-graph analysis + Tarjan SCC algorithm.

  resolver.rs         — Compile-time variable resolution: Ident → Resolved(slot).

  replay/             — Deterministic replay runtime:
                        json.rs    — JSON parser/formatter + Value↔JSON roundtrip
                        session.rs — EffectRecord / SessionRecording encoding

  vm/                 — Bytecode compiler + virtual machine:
                        compiler.rs — lowers resolved AST into Aver-specific opcodes
                        execute.rs  — stack VM execution loop over `NanValue`
                        opcode.rs   — bytecode ISA (calls, match, records, tuples, TCO)
                        runtime.rs  — builtin/effect/record-replay host bridge
                        types.rs    — function chunks, call frames, code store

  checker.rs          — Verify block runner, module intent warnings, decision index.

  value.rs            — Value, RuntimeError, Env, EnvFrame, aver_repr, aver_display.

  source.rs           — parse_source(), find_module_file().

  main.rs             — CLI entry point, delegates to main/ submodules.
  main/
    cli.rs            — clap CLI definition (Commands enum)
    commands.rs       — cmd_run_vm, cmd_check, cmd_verify
    replay_cmd.rs     — cmd_replay
    repl.rs           — cmd_repl (interactive REPL)
    context_cmd.rs    — cmd_context
    context_data.rs   — project context data collection
    context_format.rs — Markdown context formatting
    shared.rs         — shared helpers (type checking, runtime policy)

  services/           — Effectful namespace adapters over shared `aver-rt` runtime:
    console.rs        — Console.print/error/warn/readLine  ! [Console.print] / friends
    http.rs           — Http.get/head/delete/post/put/patch via `aver_rt::http`
    http_server.rs    — HttpServer.listen/listenWith via `aver_rt::http_server`
    disk.rs           — Disk.readText/writeText/appendText/exists/delete/... via shared helpers
    tcp.rs            — Tcp.send/ping + connect/writeLine/readLine/close via `aver_rt::tcp`
    terminal.rs       — Terminal.* (raw mode, cursor, color, key input) via `aver_rt::terminal` [feature-gated]
```

## How to run

```bash
aver run examples/core/hello.av
aver run examples/core/calculator.av
aver run examples/core/lists.av
aver run examples/services/console_demo.av --record recordings/
aver replay recordings/ --test --diff
aver verify examples/core/calculator.av
aver verify examples/core/lists.av
aver check examples/core/hello.av
aver check examples/core/calculator.av
aver why examples/core/calculator.av
aver context decisions/architecture.av --decisions-only
aver context decisions/architecture.av --decisions-only -o docs/decisions.md
aver context examples/core/calculator.av

# WASM backends (requires: cargo build --features wasm,wasip2)
aver run examples/core/calculator.av --wasm-gc
aver run examples/core/calculator.av --wasip2
aver compile examples/core/calculator.av --target wasm-gc
aver compile examples/core/calculator.av --target wasip2
```

## Spec test suite

```bash
cargo test
```

Tests live in `tests/` and cover four layers:

| File | What it tests |
|---|---|
| `tests/lexer_spec.rs` | Token kinds, INDENT/DEDENT, string interpolation, comments |
| `tests/parser_spec.rs` | AST shape for all constructs (bindings, fns, match, verify, decision, module, type defs) |
| `tests/typechecker_spec.rs` | Valid programs pass; type errors, effect violations, assignment errors |
| `tests/eval_spec.rs` | VM runtime semantics: arithmetic, builtins, list ops, constructors, match, pipe, map/filter/fold, user-defined types, effects, modules, replay |

The `src/lib.rs` exports all modules as `pub mod` so integration tests can access them via `use aver::...`.

## Key data types

| Type | Location | Description |
|---|---|---|
| `TokenKind` | lexer.rs | Every possible token: literals, keywords, operators, structural (INDENT/DEDENT/NEWLINE/EOF) |
| `Token` | lexer.rs | `TokenKind` + source position (`line`, `col`) |
| `LexerError` | lexer.rs | Carry `msg`, `line`, `col`; formatted as `"Lexer error [L:C]: msg"` |
| `Literal` | ast.rs | `Int(i64)`, `Float(f64)`, `Str(String)`, `Bool(bool)` |
| `BinOp` | ast.rs | Arithmetic and comparison operators as enum variants |
| `Pattern` | ast.rs | Match arm pattern: `Wildcard`, `Literal`, `Ident`, `EmptyList`, `Cons`, `Constructor` |
| `StrPart` | ast.rs | Piece of an interpolated string: `Literal(String)` or `Parsed(Box<Expr>)` |
| `Expr` | ast.rs | Every expression form: `Literal`, `Ident`, `Resolved(u16)`, `Attr`, `FnCall`, `BinOp`, `Match`, `Constructor`, `ErrorProp`, `InterpolatedStr`, `List(Vec<Expr>)`, `Tuple(Vec<Expr>)`, `MapLiteral(Vec<(Expr, Expr)>)`, `RecordCreate { type_name, fields }`, `RecordUpdate { type_name, base, updates }`, `TailCall(Box<(String, Vec<Expr>)>)` |
| `Stmt` | ast.rs | `Binding(name, Option<type_ann>, expr)`, `Expr(expr)` |
| `FnBody` | ast.rs | `Expr(Expr)` for `= expr` shorthand, or `Block(Vec<Stmt>)` where Stmt is `Binding` or `Expr` |
| `FnDef` | ast.rs | Name, params, return type, effects, optional description, body |
| `Module` | ast.rs | Name, depends, exposes, intent string |
| `VerifyBlock` | ast.rs | Function name + list of `(left_expr, right_expr)` equality cases |
| `DecisionBlock` | ast.rs | Name, date, reason, chosen, rejected list, impacts list, optional author |
| `TopLevel` | ast.rs | Top-level item: `Module`, `FnDef`, `Verify`, `Decision`, `Stmt`, `TypeDef` |
| `TypeDef` | ast.rs | `Sum { name, variants: Vec<TypeVariant> }` or `Product { name, fields: Vec<(String, String)> }` |
| `Value` | value.rs | Runtime value: `Int`, `Float`, `Str`, `Bool`, `Unit`, `Ok(Box<Value>)`, `Err(Box<Value>)`, `Some(Box<Value>)`, `None`, `List(Vec<Value>)`, `Fn{...}`, `Builtin(String)`, `Variant { type_name, variant, fields }`, `Record { type_name, fields }`, `Namespace { name, members }` |
| `Env` | value.rs | `Vec<EnvFrame>` — scope stack (Owned or Slots frames), innermost last |
| `RuntimeError` | value.rs | Error enum: `Error(String)`, `TailCall(...)`, `Replay*` variants |
| `ParseError` | parser/core.rs | `msg`, `line`, `col`; formatted as `"Parse error [L:C]: msg"` |

## Extending the language

### How to add a new keyword

1. Add a variant to `TokenKind` in `src/lexer.rs`
2. Add a match arm in the `keyword()` function in `src/lexer.rs`
3. Add the corresponding AST node(s) to `src/ast.rs` if needed
4. Add a `parse_*` method in the appropriate `src/parser/*.rs` submodule and call it from `parse_top_level()` in `module.rs`
5. Add a handler in the VM compiler (`src/vm/compiler.rs`) to emit the appropriate opcodes

### How to add a new namespace function

All functions live in namespaces (e.g., `Int.abs`, `List.len`, `Console.print`). To add a new function to an existing namespace:

1. Add the implementation in the namespace's file (e.g., `src/types/int.rs` for pure, `src/services/console.rs` for effectful) inside `call()`:
   ```rust
   "Int.yourMethod" => {
       // validate args, return Ok(Value::...)
   }
   ```
2. Register the member name in `register()` so it appears in the namespace's `members` map.
3. Add the type signature in `src/types/checker/builtins.rs` in the corresponding sigs section.

To create a new pure namespace, follow the pattern in `src/types/char.rs` or `src/types/int.rs`: implement `register()`, `effects()`, and `call()`, add `pub mod` in `types/mod.rs`, wire dispatch in `src/vm/runtime.rs`. For effectful namespaces, use `src/services/` instead.

### How to add a new expression type

1. Add a variant to `Expr` in `ast.rs`
2. Parse it in `parser/expr.rs` (typically in `parse_atom` or a new precedence level)
3. Compile it in `src/vm/compiler.rs` to emit the appropriate opcodes
4. If it should appear in verify cases, update `expr_to_str` in `checker.rs`

## Known issues / edge cases

- **Unary minus** is implemented as `0 - operand`, which means the expression AST is slightly incorrect for float negation edge cases (e.g., `-0.0`)
- **String interpolation**: expressions inside `{...}` are parsed at parse time; invalid interpolation expressions are a hard `ParseError`. Nested braces are handled by a depth counter in the lexer, but the inner expression cannot span multiple lines
- **Verify block syntax** uses `=>` as a case separator (`left_expr => expected_expr`); both sides support full expressions including comparisons (`==`, `!=`, etc.) since `=>` is a distinct token (`FatArrow`) that cannot appear inside an expression
- **No check for duplicate function names**: defining a function twice silently shadows the earlier definition
- **`match` is a statement in `parse_fn_body`** (handled via `if check_exact(Match)`) but also an expression in `parse_atom`; this dual path works but means a `match` at statement position does not pass through the normal expression precedence chain
- **Nested match in match arms** is supported: arm body is `parse_expr()`, and `match` is a valid expression, so `Result.Err(_) -> match x ...` with an indented block works correctly
- **Effect list** (`! [Console.print, Http.get, Disk.readText]`) is method-level (`Namespace.method`), propagated statically and also enforced at runtime on function-call edges; no algebraic handlers yet
- **Entry-point effect enforcement**: `main`/top-level entry calls use `call_value_with_effects_pub(...)`, which pushes a synthetic call frame with declared effects so runtime checks apply uniformly at the entry boundary
- **`chosen` field in DecisionBlock** only accepts a bare identifier (not a string), so multi-word chosen values require a single CamelCase identifier
- **No `val`/`var` keywords**: bindings are `name = expr`, always immutable. Using `val` or `var` produces a parse error with a helpful message.
- **Unknown identifiers in expressions** are inferred as `Unknown` after emitting a type error so checking can continue; this can produce cascaded follow-up errors in large files

## Next steps (prioritised)

1. **`aver context --decisions-only` query flags** — `--impacts Module`, `--since 2024-01-01`, `--rejected Technology` for searchable architectural history

## Agreed direction: modules vs DI (2026-02-25)

- Keep the language explicit in phase 1: `depends [Examples.Foo]` resolves from an explicit module root (default current working directory, optional `--module-root` override), with no hidden env remapping or parent-directory fallback.
- Treat circular imports as a hard error with an explicit chain (`A -> B -> A`) rather than trying to support partial linking now.
- Keep concerns separate:
  - Module imports (`depends`) answer "where code comes from".
  - Capabilities/services (future effect runtime model) answer "how effects are provided".
- Aver favors self-contained modules: code dependencies are explicit via `depends`, and effect dependencies are explicit via full `! [Effect]` propagation through the call chain.
- If remapping is added later, prefer a versioned project manifest (`aver.toml`) over ad-hoc runtime flags so the mapping is visible to humans and AI agents.
- Service override (for example replacing `Console`) is postponed; if added, it should be explicit, contract-checked, and limited to test/dev profiles first.

## Deferred direction: concurrency shape (2026-02-25)

- Current language model is sequential: no `async`/`await`/promises.
- If concurrency is added, keep effects as capability (`what`) and concurrency as scheduling (`when`); do not overload `! [Effect]` with ordering semantics.
- MVP preference: `par` for homogeneous workloads (same result type), e.g. multiple `Http.get` calls.
- No `Any` fallback for mixed parallel results.
- For mixed-type parallelism, prefer positional fixed products (tuple-like return) rather than heterogeneous lists.
- Explicit `spawn`/`join` API is rejected for Aver (not postponed).

## Agreed direction: verify-first debugging (2026-02-25)

- For logic bugs, default workflow is: reproduce with a failing `verify` case, fix implementation, keep the case as a permanent regression guard.
- Prefer this over ad-hoc print debugging inside core functions; debugging artifacts should become executable specs when possible.
- This is especially AI-friendly: `verify` cases are declarative, reviewable, and reusable across sessions.
- Limits (still real): `verify` alone does not replace profiling, latency analysis, or debugging nondeterministic external systems.

## Releases: `tools/release.py` is the only path

Run `python3 tools/release.py X.Y.Z` (add `--dry-run` first for a sanity pass). The script is the single source of truth for the full release flow — it covers everything below in the right order, idempotently:

0. Editor grammar sync check (`editors/sync.py --check`)
1. Cascade-bump `aver-rt` / `aver-memory` / `aver-cert` / `aver-lang` / `aver-lsp` Cargo.toml + cross-crate dep pins; `aver-cert` keeps its independent 0.1.x line (first public release `0.1.0`, later source changes patch-bump it), and the cascade logic honours `PUBLISH_BLOCKERS` so `cargo publish` doesn't hit a resolution conflict
2. Bump the `tools/website/index.html` hero version badge
3. Regenerate self-host (`self_hosted/main.av` → `src/self_host/`)
4. Regenerate playground WASM artifacts (`tools/website/rebuild_playground.py`)
5. Verify (fmt + workspace clippy + aver-lang+wasm clippy + cargo test + bench scenarios + edge `--preset cloudflare` compile + `wasm-tools validate`)
5.5. Stamp the CHANGELOG header (`## X.Y.Z "Codename" (unreleased)` → `## X.Y.Z "Codename" — YYYY-MM-DD`)
6. `cargo publish` each changed crate in CRATE_ORDER
7. Git add + commit + tag + push + `gh release create` with notes from the CHANGELOG section

Flags worth knowing: `--dry-run`, `--skip-publish`, `--skip-playground`, `--skip-self-host`, `--deploy-edge` (the last one rebuilds + deploys `tools/edge` to Cloudflare and curl-smokes `/`, `/api`, `/fractal`).

**Don't bump versions or tag by hand.** The 0.19.0 "Echo" release was attempted manually first, which skipped the cascade bump, website badge, self-host regen, playground regen, the verify gates beyond unit tests, `cargo publish`, and `gh release create`. The lesson: even when individual steps feel obvious in isolation, the script's idempotent design + correct ordering is the only thing that catches the dependency chain at the right place.

## Agreed direction: file size and splitting policy (2026-02-26)

- Any Rust file above 500 lines must be reviewed for splitting during normal development.
- Split when it improves maintainability, testing, or separation of responsibilities.
- Do not split purely to satisfy a metric if the file is large but still cohesive (for example large spec tables).
- Treat 500 lines as a trigger for review, not an automatic hard failure.
