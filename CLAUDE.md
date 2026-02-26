# Aver — AI Context File

## What is this?

Aver is a programming language designed for AI-assisted development. Its interpreter is written in Rust. The language prioritises human and machine readability: every function carries an optional prose description, and architectural decisions are first-class citizens expressed as `decision` blocks co-located with the code they describe. This file is the single entry point for any AI resuming work on this project — read it before touching any source file.

## Project philosophy

- Code is a letter to the next reader — who is increasingly an AI
- Every fragment must be self-sufficient (readable without context)
- Intent over implementation: signatures tell the full story
- Decisions are first-class citizens of the codebase

## Current status

### What works

- Lexer with significant-indentation handling (Python-style INDENT / DEDENT tokens)
- String interpolation: `"Hello, {name}!"` is tokenised as `InterpStr`; expressions inside `{}` are parsed at parse time into `StrPart::Parsed(Expr)`
- Integer and float literals, bool literals, string literals
- **Immutable bindings only**: `name = expr` (no `val`/`var` keywords — they are parse errors). All bindings are immutable; no reassignment. Duplicate binding of the same name in the same scope is a type error.
- Function definitions (`fn`) with typed parameters, return type annotation, optional prose description (`? "..."`), and optional effect declaration (`! [Effect]`)
- Single-expression functions (`= expr` shorthand) and block-body functions
- Arithmetic: `+`, `-`, `*`, `/` with mixed Int/Float promotion
- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Pipe operator `|>` — passes the left-hand value as the sole argument to the right-hand function
- `match` expressions with patterns: wildcard `_`, literal, identifier binding, list patterns (`[]`, `[h, ..t]`), constructor (`Result.Ok(x)`, `Result.Err(x)`, `Option.Some(x)`, `Option.None`)
- **Namespaced constructors** (decision: `NamespacedConstructors`): `Result.Ok(v)`, `Result.Err(v)`, `Option.Some(v)`, `Option.None` — no bare `Ok`/`Err`/`Some`/`None`. `Result` and `Option` are registered as `Value::Namespace` in the interpreter. Constructors route through `call_builtin` (`__ctor:Result.Ok` etc.). Match patterns use qualified names (`Result.Ok(v)`, `Option.None`).
- Error propagation operator `?` on `Result` values — unwraps `Result.Ok`, raises `RuntimeError` on `Result.Err`
- `module` blocks with `intent:`, `exposes [...]`, and `depends [...]`; runtime import resolution from a module root (`--module-root`, default current working directory) with dot-path imports (`depends [Examples.Foo]`, `depends [Examples.Models.User]`)
- `verify` blocks — inline property tests co-located with functions, run via `aver verify`
- `decision` blocks — structured architectural decision records (ADR) with `date`, `reason`, `chosen`, `rejected`, `impacts`, `author`
- **List values**: `Value::List(Vec<Value>)` with literal syntax `[1, 2, 3]`, `["a", "b"]`, `[]`; printed as `[1, 2, 3]`
- **No flat builtins** — all functions live in namespaces (decision: `FullNamespaceEverywhere`)
- **`List` namespace** (`src/services/list_helpers.rs`): `List.len`, `List.map`, `List.filter`, `List.fold`, `List.get`, `List.push`, `List.head`, `List.tail`; `get`/`head`/`tail` return `Result.Ok(val)` or `Result.Err(msg)` — no effects
- **`Int` namespace** (`src/services/int_helpers.rs`): `Int.fromString`, `Int.fromFloat`, `Int.toString`, `Int.abs`, `Int.min`, `Int.max`, `Int.mod`, `Int.toFloat` — no effects
- **`Float` namespace** (`src/services/float_helpers.rs`): `Float.fromString`, `Float.fromInt`, `Float.toString`, `Float.abs`, `Float.floor`, `Float.ceil`, `Float.round`, `Float.min`, `Float.max` — no effects
- **`String` namespace** (`src/services/string_helpers.rs`): `String.length`, `String.byteLength`, `String.startsWith`, `String.endsWith`, `String.contains`, `String.slice`, `String.trim`, `String.split`, `String.replace`, `String.join`, `String.chars`, `String.fromInt`, `String.fromFloat`, `String.fromBool` — no effects
- **No closures**: all user-defined fns are top-level; at call time, the function sees globals (env[0]) + its own parameters — no closure capture at definition time
- **Auto-memoization** (`src/call_graph.rs`, `src/types/checker.rs`, `src/interpreter/mod.rs`): pure recursive functions with memo-safe arguments (scalars, records/variants of scalars) are automatically memoized at runtime. Call graph is built from AST, Tarjan SCC detects recursion, and `call_fn_ref` checks/stores a per-function HashMap cache (capped at 4096 entries). No keyword needed — the compiler detects eligibility.
- CLI subcommands: `run`, `verify`, `check`, `replay`, `context`, `repl`
- **Deterministic replay for effectful code** (`src/replay/`, `src/interpreter/mod.rs`, `src/main.rs`): `aver run --record <dir>` captures effect sequence + outcomes; `aver replay <file|dir> [--diff] [--test] [--check-args]` replays offline without real side effects and can fail CI on output mismatch
- `check` command: warns when a module has no `intent:` or a function with effects/Result return has no `?` description; warns if file exceeds 150 lines; `fn main()` is exempt from the `?` requirement
- **Static type checker** (`src/types/checker.rs`): `aver check`, `aver run`, and `aver verify` all run a type-check pass. Type errors are hard errors that block execution. The checker covers function bodies and top-level statements (including duplicate binding detection). `Any` annotations are removed from the language surface; the checker may still use internal `Type::Unknown` recovery after earlier errors so analysis can continue.
- **Effect propagation** is statically enforced (blocks `check`/`run`/`verify`), including `main()`: calling an effectful function requires declaring the same effect in the caller. Runtime also enforces call-edge capabilities as a backstop.
- **User-defined sum types** (`type` keyword): `type Shape` with variants `Circle(Float)`, `Rect(Float, Float)`, `Point`. Constructors are accessed with qualified syntax `Shape.Circle(5.0)` — no flat namespace pollution. Patterns: `Shape.Circle(r)`, `Shape.Point`. Registered as `Value::Namespace` in the interpreter env.
- **User-defined product types** (`record` keyword): `record User` with named fields `name: String`, `age: Int`. Constructed as `User(name: "Alice", age: 30)`. Field access via `u.name`. Positional destructuring in match: `User(name, age) -> name`.
- **`Type::Named(String)`** in the type system: capitalized identifiers (including dotted names like `Tcp.Connection`) in type annotations resolve to named types. Compatible only with the same name or internal `Unknown` fallback.
- **`Tcp.Connection` opaque record**: `Tcp.connect` returns `Result<Tcp.Connection, String>` — a record with fields `id: String`, `host: String`, `port: Int`. Persistent-connection methods (`writeLine`, `readLine`, `close`) accept `Tcp.Connection` instead of a bare string ID. The actual socket lives in a thread-local `HashMap` keyed by the `id` field.
- **Tail-call optimization** (`src/tco.rs`): a transform pass after parsing rewrites tail-position calls in recursive SCCs (self or mutual) from `FnCall` to `Expr::TailCall`. The interpreter's `call_fn_ref` uses a trampoline: when `TailCall` is returned, it rebinds args (self-TCO) or switches to the target fn (mutual TCO) without growing the call stack. Pipeline: `parse → tco_transform → typecheck → resolve → interpret`. Tail position = last expression in fn body, or each arm body in a `match` at tail position.

### What is missing / known limitations

- No `if`/`else` — **this is intentional by design**; `match` is the only branching construct
- No loops (`for`, `while`) — **intentionally absent**; Aver has no imperative iteration; use `map`/`filter`/`fold`
- Field access works for `record` values (`u.name`) but not on sum type variants or other values

### What was explicitly NOT implemented yet (save for later)

- Effect handlers / row-polymorphic effects — runtime currently uses declared effect lists with call-edge capability checks; no handlers yet
- decision-query flags (`--impacts`, `--since`, etc.) for `aver context --decisions-only`

### What will NEVER be in Aver (design decisions)

- `if`/`else` — `match` is the only branching construct; exhaustive matching forces explicit handling of all cases
- `for`/`while` loops — no imperative iteration; future iteration will be through higher-order functions (`map`, `filter`, `fold`) over lists
- `null` — `Option<T>` with `Some`/`None` only
- Exceptions — `Result<T, E>` with `Ok`/`Err` only; errors are values
- Global mutable state — no DI, no service blocks; mocking via function parameter: `fn f(log: Fn(String) -> Unit ! [Console])`
- Magic (decorators, implicit behaviour, runtime reflection)

## Architecture in one page

```
src/
  lexer.rs       — Converts source text to a flat Vec<Token>.
                   Manages an indent_stack to emit INDENT/DEDENT tokens
                   for significant indentation. Handles string interpolation
                   by collecting raw expression source inside "{ }".

  ast.rs         — Pure data: the Abstract Syntax Tree.
                   No logic, no methods. Defines TokenKind, Expr, Stmt,
                   FnDef, Module, VerifyBlock, DecisionBlock, TopLevel, Value
                   (actually Value lives in interpreter.rs).

  parser.rs      — Recursive-descent parser consuming Vec<Token>.
                   Produces Vec<TopLevel>. Each parse_* method corresponds
                   to one grammar construct. Expression precedence is
                   encoded in the call chain:
                   parse_pipe -> parse_comparison -> parse_additive ->
                   parse_multiplicative -> parse_unary -> parse_postfix ->
                   parse_call_or_atom -> parse_atom

  interpreter.rs — Tree-walking evaluator.
                   Env is a Vec<EnvFrame> (scope stack: Owned or Slots).
                   No flat builtins — all functions live in namespaces
                   (Console, List, Int, Float, String, Http, Disk, Tcp).
                   No closure capture — functions see globals (env[0]) at call time.
                   Auto-memoization cache in call_fn_ref for pure
                   recursive functions.

  tco.rs         — Tail-call optimization transform pass.
                   Runs after parsing, before type-checking.
                   Uses call_graph SCC to find recursive groups, then
                   rewrites tail-position FnCall → TailCall in each SCC.

  call_graph.rs  — Call-graph analysis + Tarjan SCC algorithm.
                   Builds fn→callee graph from AST, detects recursive
                   (self or mutual) functions for auto-memoization and TCO.

  replay/        — Deterministic replay runtime:
                   `json.rs` (JSON parser/formatter + Value<->JSON mapping)
                   and `session.rs` (EffectRecord / SessionRecording encoding).

  checker.rs     — Static analysis and test runner.
                   run_verify() executes VerifyBlock cases and prints
                   pass/fail with colour.
                   check_module_intent() warns on missing intent/desc.
                   index_decisions() filters TopLevel::Decision items.

  main.rs        — CLI entry point via clap.
                   Subcommands: run, verify, check, replay, context, repl.
                   cmd_run: registers FnDefs, runs Stmts, calls main().
                   cmd_run --record writes SessionRecording JSON traces.
                   cmd_replay re-executes with recorded effect outcomes.
                   cmd_verify: runs typecheck, registers FnDefs, runs all verify blocks.
                   cmd_check: line-count check + intent/desc warnings.
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
| `Stmt` | ast.rs | `Binding(name, expr)`, `Expr(expr)` |
| `FnBody` | ast.rs | `Expr(Expr)` for `= expr` shorthand, or `Block(Vec<Stmt>)` where Stmt is `Binding` or `Expr` |
| `FnDef` | ast.rs | Name, params, return type, effects, optional description, body |
| `Module` | ast.rs | Name, depends, exposes, intent string |
| `VerifyBlock` | ast.rs | Function name + list of `(left_expr, right_expr)` equality cases |
| `DecisionBlock` | ast.rs | Name, date, reason, chosen, rejected list, impacts list, optional author |
| `TopLevel` | ast.rs | Top-level item: `Module`, `FnDef`, `Verify`, `Decision`, `Stmt`, `TypeDef` |
| `TypeDef` | ast.rs | `Sum { name, variants: Vec<TypeVariant> }` or `Product { name, fields: Vec<(String, String)> }` |
| `Value` | interpreter.rs | Runtime value: `Int`, `Float`, `Str`, `Bool`, `Unit`, `Ok(Box<Value>)`, `Err(Box<Value>)`, `Some(Box<Value>)`, `None`, `List(Vec<Value>)`, `Fn{...}`, `Builtin(String)`, `Variant { type_name, variant, fields }`, `Record { type_name, fields }`, `Namespace { name, members }` |
| `Env` | interpreter.rs | `Vec<EnvFrame>` — scope stack (Owned or Slots frames), innermost last |
| `RuntimeError` | interpreter.rs | Single-variant error wrapping a `String` message |
| `ParseError` | parser.rs | `msg`, `line`, `col`; formatted as `"Parse error [L:C]: msg"` |

## Extending the language

### How to add a new keyword

1. Add a variant to `TokenKind` in `src/lexer.rs`
2. Add a match arm in the `keyword()` function in `src/lexer.rs`
3. Add the corresponding AST node(s) to `src/ast.rs` if needed
4. Add a `parse_*` method in `src/parser.rs` and call it from `parse_top_level()` or the appropriate sub-parser
5. Add execution logic in `src/interpreter.rs` (`eval_expr`, `exec_stmt`, or a new method)

**Concrete example — adding `maintain` (goal-based looping):**

Aver has no `for`/`while`. Future iteration uses goal-based constructs. Here is the extension pattern for a `maintain` keyword:

```
// lexer.rs — add:
Maintain,

// keyword() — add:
"maintain" => Some(TokenKind::Maintain),

// ast.rs — add to Expr:
Maintain(Box<Expr>, Box<Expr>),  // condition, body block

// parser.rs — add method:
fn parse_maintain(&mut self) -> Result<Expr, ParseError> {
    self.expect_exact(&TokenKind::Maintain)?;
    let cond = self.parse_expr()?;
    self.expect_exact(&TokenKind::Colon)?;
    // parse indented body block
}

// interpreter.rs — add to eval_expr():
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

1. Add the implementation in the namespace's service file (e.g., `src/services/int_helpers.rs`) inside `call()`:
   ```rust
   "Int.yourMethod" => {
       // validate args, return Ok(Value::...)
   }
   ```
2. Register the member name in `register()` so it appears in the namespace's `members` map.
3. Add the type signature in `src/typechecker.rs` in the corresponding `service_sigs` section.

To create a new namespace, follow the pattern in any `src/services/*_helpers.rs` file: implement `register()`, `effects()`, and `call()`, add `pub mod` in `services/mod.rs`, wire it up in `interpreter/mod.rs`.

### How to add a new expression type

1. Add a variant to `Expr` in `ast.rs`
2. Parse it in `parser.rs` (typically in `parse_atom` or a new precedence level)
3. Evaluate it in `interpreter.rs` inside `eval_expr`'s `match expr` block
4. If it should appear in verify cases, update `expr_to_str` in `checker.rs`

## Known issues / edge cases

- **Unary minus** is implemented as `0 - operand`, which means the expression AST is slightly incorrect for float negation edge cases (e.g., `-0.0`)
- **String interpolation**: expressions inside `{...}` are parsed at parse time; invalid interpolation expressions are a hard `ParseError`. Nested braces are handled by a depth counter in the lexer, but the inner expression cannot span multiple lines
- **Verify block syntax** uses `=>` as a case separator (`left_expr => expected_expr`); both sides support full expressions including comparisons (`==`, `!=`, etc.) since `=>` is a distinct token (`FatArrow`) that cannot appear inside an expression
- **No check for duplicate function names**: defining a function twice silently shadows the earlier definition
- **`match` is a statement in `parse_fn_body`** (handled via `if check_exact(Match)`) but also an expression in `parse_atom`; this dual path works but means a `match` at statement position does not pass through the normal expression precedence chain
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
