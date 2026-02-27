# Aver — AI Context File

## What is this?

Aver is a programming language designed for AI-assisted development. Its interpreter is written in Rust. The language prioritises human and machine readability: every function carries an optional prose description, and architectural decisions are first-class citizens expressed as `decision` blocks co-located with the code they describe. This file is the single entry point for any AI resuming work on this project — read it before touching any source file.

## Project philosophy

- Code is a letter to the next reader — who is increasingly an AI
- Every fragment must be self-sufficient (readable without context)
- Intent over implementation: signatures tell the full story
- Decisions are first-class citizens of the codebase

## Current status

### Language features

See [README.md](README.md#language-reference) for complete language reference (syntax, types, operators, patterns, modules).

Below: implementation details relevant to development only.

### Implementation notes

- **Constructor routing**: `Result.Ok(v)`, `Result.Err(v)`, `Option.Some(v)` route through `call_builtin` (`__ctor:Result.Ok` etc.). `Result` and `Option` registered as `Value::Namespace`. Match patterns use qualified names.
- **No flat builtins** (decision: `FullNamespaceEverywhere`). `List.map/filter/fold` stay in `interpreter/builtins.rs` because they need `self.call_value()`.
- **`Type::Named(String)`** in the type system: capitalized identifiers (including dotted names like `Tcp.Connection`) in type annotations resolve to named types. Compatible only with the same name or internal `Unknown` fallback.
- **`Tcp.Connection` opaque record**: fields `id: String`, `host: String`, `port: Int`. Actual socket in thread-local `HashMap` keyed by `id`. `NEXT_ID: AtomicU64` generates "tcp-1", "tcp-2", etc.
- **Static type checker** (`src/types/checker/`): internal `Type::Unknown` recovery after earlier errors so analysis can continue. Bare `Unknown` does **not** satisfy concrete types in constraints — only nested `Unknown` is tolerated (gradual typing). Match pattern bindings are typed: `Result.Ok(x)` on `Result<Int, String>` gives `x: Int`.
- **Auto-memoization** (`src/call_graph.rs`, `src/types/checker/memo.rs`, `src/interpreter/core.rs`): call graph built from AST, Tarjan SCC detects recursion, `call_fn_ref` checks/stores per-function HashMap cache (capped at 4096). Eligibility: pure + recursive + all params memo-safe (scalars, records/variants of scalars).
- **TCO** (`src/tco.rs`): transform pass rewrites tail-position `FnCall` → `Expr::TailCall` in recursive SCCs. Interpreter trampoline in `call_fn_ref`: self-TCO rebinds args, mutual TCO switches to target fn. Pipeline: `parse → tco_transform → typecheck → resolve → interpret`.
- **Compile-time variable resolution** (`src/resolver.rs`): `Ident("x")` → `Resolved(slot)` inside FnDef bodies. `EnvFrame::Slots(Vec<Rc<Value>>)` for O(1) lookup. REPL and sub-interpreters use unresolved path (`EnvFrame::Owned(HashMap)`).
- **`check` command**: warns when module has no `intent =`, function with effects/Result return has no `?` description, file exceeds 250 lines. `fn main()` is exempt from `?` requirement.
- **Entry-point effect enforcement**: `main`/top-level entry calls use `call_value_with_effects_pub(...)` with synthetic call frame.

### What is missing / known limitations

- No `if`/`else` — **this is intentional by design**; `match` is the only branching construct
- No loops (`for`, `while`) — **intentionally absent**; Aver has no imperative iteration; use `map`/`filter`/`fold`
- Field access works for `record` values (`u.name`) but not on sum type variants or other values

### What was explicitly NOT implemented yet (save for later)

- Effect handlers / row-polymorphic effects — runtime currently uses declared effect lists with call-edge capability checks; no handlers yet
- decision-query flags (`--impacts`, `--since`, etc.) for `aver context --decisions-only`

### Design omissions

See [README.md](README.md#what-aver-deliberately-omits) for the full list of intentional omissions (no `if`/`else`, no loops, no `null`, no exceptions, no mutable state, no magic).

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

  interpreter/        — Tree-walking evaluator. Split into submodules:
                        core.rs    — Interpreter struct, env management, memo
                        eval.rs    — eval_expr: literals, binops, match, interp strings
                        exec.rs    — exec_stmt, exec_body, exec_fn_def, exec_items
                        builtins.rs — call_builtin dispatch (constructors, services)
                        effects.rs — ExecutionMode (Normal/Record/Replay), effect interception
                        ops.rs     — call_fn_ref, tco_trampoline, call_value
                        patterns.rs — match_pattern, eval_match
                        api.rs     — public helpers (call_value_with_effects_pub, lookup)

  types/
    mod.rs            — enum Type, parse_type_str, compatible()
    int.rs            — Int.* (pure)
    float.rs          — Float.* (pure)
    string.rs         — String.* (pure)
    list.rs           — List.len/get/push/head/tail (pure)
    map.rs            — Map.* (pure)
    char.rs           — Char.toCode/fromCode (pure, not a type)
    byte.rs           — Byte.toHex/fromHex (pure, not a type)
    checker/          — Static type checker. Split into submodules:
      mod.rs          — TypeChecker struct, constraint_compatible(), run_type_check_*
      infer.rs        — infer_type: expressions, calls, match, patterns
      flow.rs         — check_fn_body, check_stmts, effect propagation
      builtins.rs     — service_sigs, record_field_types registration
      modules.rs      — cross-module type checking, base signature merging
      memo.rs         — is_memo_safe, memo_safe_types computation
      tests.rs        — unit tests for checker internals

  tco.rs              — Tail-call optimization transform pass.
                        Runs after parsing, before type-checking.

  call_graph.rs       — Call-graph analysis + Tarjan SCC algorithm.

  resolver.rs         — Compile-time variable resolution: Ident → Resolved(slot).

  replay/             — Deterministic replay runtime:
                        json.rs    — JSON parser/formatter + Value↔JSON roundtrip
                        session.rs — EffectRecord / SessionRecording encoding

  checker.rs          — Verify block runner, module intent warnings, decision index.

  value.rs            — Value, RuntimeError, Env, EnvFrame, aver_repr, aver_display.

  source.rs           — parse_source(), find_module_file().

  main.rs             — CLI entry point, delegates to main/ submodules.
  main/
    cli.rs            — clap CLI definition (Commands enum)
    commands.rs       — cmd_run, cmd_check, cmd_verify
    replay_cmd.rs     — cmd_replay
    repl.rs           — cmd_repl (interactive REPL)
    context_cmd.rs    — cmd_context
    context_data.rs   — project context data collection
    context_format.rs — Markdown context formatting
    shared.rs         — shared helpers (compile_program_for_exec, load_dep_modules)

  services/           — Effectful namespace implementations:
    console.rs        — Console.print/error/warn/readLine  ! [Console]
    http.rs           — Http.get/head/delete/post/put/patch  ! [Http]
    http_server.rs    — HttpServer.listen (standalone runtime)  ! [HttpServer]
    disk.rs           — Disk.readText/writeText/appendText/exists/delete/...  ! [Disk]
    tcp.rs            — Tcp.send/ping + connect/writeLine/readLine/close  ! [Tcp]
```

## How to run

```bash
cargo build
cargo run -- run examples/hello.av
cargo run -- run examples/calculator.av
cargo run -- run examples/lists.av
cargo run -- run examples/services/console_demo.av --record recordings/
cargo run -- replay recordings/ --test --diff
cargo run -- verify examples/calculator.av
cargo run -- verify examples/lists.av
cargo run -- check examples/hello.av
cargo run -- check examples/calculator.av
cargo run -- context decisions/architecture.av --decisions-only
cargo run -- decisions --docs
cargo run -- context examples/calculator.av
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
| `tests/eval_spec.rs` | Arithmetic, builtins, list ops, constructors, match, pipe, map/filter/fold, user-defined types |

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
| `Expr` | ast.rs | Every expression form: `Literal`, `Ident`, `Attr`, `FnCall`, `BinOp`, `Match`, `Pipe`, `Constructor`, `ErrorProp`, `InterpolatedStr`, `List(Vec<Expr>)`, `RecordCreate { type_name, fields }` |
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
5. Add execution logic in the appropriate `src/interpreter/*.rs` submodule (`eval.rs` for expressions, `exec.rs` for statements)

**Concrete example — adding `maintain` (goal-based looping):**

Aver has no `for`/`while`. Future iteration uses goal-based constructs. Here is the extension pattern for a `maintain` keyword:

```
// lexer.rs — add:
Maintain,

// keyword() — add:
"maintain" => Some(TokenKind::Maintain),

// ast.rs — add to Expr:
Maintain(Box<Expr>, Box<Expr>),  // condition, body block

// parser/expr.rs — add method:
fn parse_maintain(&mut self) -> Result<Expr, ParseError> {
    self.expect_exact(&TokenKind::Maintain)?;
    let cond = self.parse_expr()?;
    self.expect_exact(&TokenKind::Colon)?;
    // parse indented body block
}

// interpreter/eval.rs — add to eval_expr():
Expr::Maintain(cond, body) => {
    loop {
        match self.eval_expr(cond)? {
            Value::Bool(false) => break,
            Value::Bool(true)  => { self.eval_expr(body)?; }
            _ => return Err(RuntimeError::Error("maintain condition must be Bool".into())),
        }
    }
    Ok(Value::Unit)
}
```

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

To create a new pure namespace, follow the pattern in `src/types/char.rs` or `src/types/int.rs`: implement `register()`, `effects()`, and `call()`, add `pub mod` in `types/mod.rs`, wire dispatch in `interpreter/builtins.rs` and effects in `interpreter/effects.rs`. For effectful namespaces, use `src/services/` instead.

### How to add a new expression type

1. Add a variant to `Expr` in `ast.rs`
2. Parse it in `parser/expr.rs` (typically in `parse_atom` or a new precedence level)
3. Evaluate it in `interpreter/eval.rs` inside `eval_expr`'s `match expr` block
4. If it should appear in verify cases, update `expr_to_str` in `checker.rs`

## Known issues / edge cases

- **Unary minus** is implemented as `0 - operand`, which means the expression AST is slightly incorrect for float negation edge cases (e.g., `-0.0`)
- **String interpolation**: expressions inside `{...}` are parsed at parse time; invalid interpolation expressions are a hard `ParseError`. Nested braces are handled by a depth counter in the lexer, but the inner expression cannot span multiple lines
- **Verify block syntax** uses `=>` as a case separator (`left_expr => expected_expr`); both sides support full expressions including comparisons (`==`, `!=`, etc.) since `=>` is a distinct token (`FatArrow`) that cannot appear inside an expression
- **No check for duplicate function names**: defining a function twice silently shadows the earlier definition
- **`match` is a statement in `parse_fn_body`** (handled via `if check_exact(Match)`) but also an expression in `parse_atom`; this dual path works but means a `match` at statement position does not pass through the normal expression precedence chain
- **Nested match in match arms** is supported: arm body is `parse_expr()`, and `match` is a valid expression, so `Result.Err(_) -> match x ...` with an indented block works correctly
- **Effect list** (`! [Io, State]`) is propagated statically and also enforced at runtime on function-call edges; no algebraic handlers yet
- **Entry-point effect enforcement**: `main`/top-level entry calls use `call_value_with_effects_pub(...)`, which pushes a synthetic call frame with declared effects so runtime checks apply uniformly at the entry boundary
- **`chosen` field in DecisionBlock** only accepts a bare identifier (not a string), so multi-word chosen values require a single CamelCase identifier
- **No `val`/`var` keywords**: bindings are `name = expr`, always immutable. Using `val` or `var` produces a parse error with a helpful message.
- **Unknown identifiers in expressions** are inferred as `Unknown` after emitting a type error so checking can continue; this can produce cascaded follow-up errors in large files

## Next steps (prioritised)

1. **`aver decisions` query flags** — `--impacts Module`, `--since 2024-01-01`, `--rejected Technology` for searchable architectural history

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

## Agreed direction: file size and splitting policy (2026-02-26)

- Any Rust file above 500 lines must be reviewed for splitting during normal development.
- Split when it improves maintainability, testing, or separation of responsibilities.
- Do not split purely to satisfy a metric if the file is large but still cohesive (for example large spec tables).
- Treat 500 lines as a trigger for review, not an automatic hard failure.
