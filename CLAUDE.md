# Aver — AI Context File

## What is this?

Aver is a programming language designed for AI-assisted development. Its interpreter is written in Rust. The language prioritises human and machine readability: every function carries an optional prose description, every mutable variable can document why it exists, and architectural decisions are first-class citizens expressed as `decision` blocks co-located with the code they describe. This file is the single entry point for any AI resuming work on this project — read it before touching any source file.

## Project philosophy

- Code is a letter to the next reader — who is increasingly an AI
- Every fragment must be self-sufficient (readable without context)
- Intent over implementation: signatures tell the full story
- Decisions are first-class citizens of the codebase

## Current status

### What works

- Lexer with significant-indentation handling (Python-style INDENT / DEDENT tokens)
- String interpolation: `"Hello, {name}!"` is tokenised as `InterpStr` and evaluated at runtime by re-lexing the expression inside `{}`
- Integer and float literals, bool literals, string literals
- `val` (immutable binding) and `var` (mutable binding with optional `reason:` annotation); `var` supports reassignment with `name = expr` (bare assignment, no keyword)
- Function definitions (`fn`) with typed parameters, return type annotation, optional prose description (`? "..."`), and optional effect declaration (`! [Effect]`)
- Single-expression functions (`= expr` shorthand) and block-body functions
- Arithmetic: `+`, `-`, `*`, `/` with mixed Int/Float promotion
- Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Pipe operator `|>` — passes the left-hand value as the sole argument to the right-hand function
- `match` expressions with patterns: wildcard `_`, literal, identifier binding, list patterns (`[]`, `[h, ..t]`), constructor (`Ok(x)`, `Err(x)`, `Some(x)`, `None`)
- `Ok`, `Err`, `Some`, `None` constructors and corresponding value types
- Error propagation operator `?` on `Result` values — unwraps `Ok`, raises `RuntimeError` on `Err`
- `module` blocks with `intent:`, `exposes [...]`, and `depends [...]`; runtime import resolution from the entry file base directory (`depends [Foo]`, `depends [Models.User]`)
- `verify` blocks — inline property tests co-located with functions, run via `aver verify`
- `decision` blocks — structured architectural decision records (ADR) with `date`, `reason`, `chosen`, `rejected`, `impacts`, `author`
- **List values**: `Value::List(Vec<Value>)` with literal syntax `[1, 2, 3]`, `["a", "b"]`, `[]`; printed as `[1, 2, 3]`
- **List builtins**: `len(list)`, `map(list, fn)`, `filter(list, fn)`, `fold(list, init, fn)`, `get(list, n)`, `push(list, val)`, `head(list)`, `tail(list)`; `get`/`head`/`tail` return `Ok(val)` or `Err(msg)`
- Builtin functions: `print`, `str`, `int`, `abs`, `len` (strings and lists), `map`, `filter`, `fold`, `get`, `push`, `head`, `tail`
- Closures: functions capture their definition-time environment
- CLI with four subcommands: `run`, `verify`, `check`, `decisions`
- `check` command: warns when a module has no `intent:` or a function with effects/Result return has no `?` description; warns if file exceeds 150 lines; `fn main()` is exempt from the `?` requirement
- **Static type checker** (`src/typechecker.rs`): `aver check`, `aver run`, and `aver verify` all run a type-check pass. Type errors are hard errors that block execution. The checker covers function bodies and top-level statements (including assignment mutability rules). `Any` is a valid explicit type annotation (`x: Any`) and acts as a gradual-typing escape hatch — two `Any`-typed values are always compatible. The type system is *statically typed with explicit `Any` escape hatch*; unsound programs that use `Any` broadly may still pass the checker.
- **Effect propagation** is statically enforced (blocks `check`/`run`/`verify`), including `main()`: calling an effectful function requires declaring the same effect in the caller. Runtime does not add a separate capability layer.
- **User-defined sum types** (`type` keyword): `type Shape` with variants `Circle(Float)`, `Rect(Float, Float)`, `Point`. Constructors are accessed with qualified syntax `Shape.Circle(5.0)` — no flat namespace pollution. Patterns: `Shape.Circle(r)`, `Shape.Point`. Registered as `Value::Namespace` in the interpreter env.
- **User-defined product types** (`record` keyword): `record User` with named fields `name: String`, `age: Int`. Constructed as `User(name: "Alice", age: 30)`. Field access via `u.name`. Positional destructuring in match: `User(name, age) -> name`.
- **`Type::Named(String)`** in the type system: capitalized single-word identifiers in type annotations resolve to named types. Compatible only with the same name or `Any`.

### What is missing / known limitations

- No `if`/`else` — **this is intentional by design**; `match` is the only branching construct
- No loops (`for`, `while`) — **intentionally absent**; Aver has no imperative iteration; use `map`/`filter`/`fold`
- Field access works for `record` values (`u.name`) but not on sum type variants or other values
- The `effect` and `service` declaration keywords are tokenised but not parsed yet (`! [Effect]` on functions is parsed and enforced)
- The `?` error-propagation operator only works at the expression level; it does not short-circuit across function boundaries (no early return mechanism)
- `ErrPropSignal` struct exists in interpreter.rs but is not used — reserved for a proper early-return implementation
- No tail-call optimisation; deep recursion will overflow the stack

### What was explicitly NOT implemented yet (save for later)

- Effect system (algebraic effects / capabilities) — currently static-only propagation and checks (no runtime capability model, no handlers)
- Service blocks with `needs:` dependency injection
- Proper `?` short-circuit across function calls (requires a signal/exception mechanism or continuation)
- `aver decisions --impacts Module` and other query flags on the CLI

### What will NEVER be in Aver (design decisions)

- `if`/`else` — `match` is the only branching construct; exhaustive matching forces explicit handling of all cases
- `for`/`while` loops — no imperative iteration; future iteration will be through higher-order functions (`map`, `filter`, `fold`) over lists
- `null` — `Option<T>` with `Some`/`None` only
- Exceptions — `Result<T, E>` with `Ok`/`Err` only; errors are values
- Global mutable state — only `service` blocks with explicit `needs:` dependencies
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
                   Env is a Vec<HashMap<String,Value>> (scope stack).
                   Builtins: print, str, int, abs, len.
                   Closures are captured eagerly (flat HashMap snapshot).
                   String interpolation re-lexes and re-parses the raw
                   expression text stored inside StrPart::Expr.

  checker.rs     — Static analysis and test runner.
                   run_verify() executes VerifyBlock cases and prints
                   pass/fail with colour.
                   check_module_intent() warns on missing intent/desc.
                   index_decisions() filters TopLevel::Decision items.

  main.rs        — CLI entry point via clap.
                   Four subcommands: run, verify, check, decisions.
                   cmd_run: registers FnDefs, runs Stmts, calls main().
                   cmd_verify: runs typecheck, registers FnDefs, runs all verify blocks.
                   cmd_check: line-count check + intent/desc warnings.
                   cmd_decisions: pretty-prints all decision blocks.
```

## How to run

```bash
cargo build
cargo run -- run examples/hello.av
cargo run -- run examples/calculator.av
cargo run -- run examples/lists.av
cargo run -- verify examples/calculator.av
cargo run -- verify examples/lists.av
cargo run -- check examples/hello.av
cargo run -- check examples/calculator.av
cargo run -- decisions decisions/decisions.av
cargo run -- decisions decisions/architecture.av
cargo run -- run decisions/architecture.av
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
| `StrPart` | ast.rs | Piece of an interpolated string: `Literal(String)` or `Expr(String)` (raw source) |
| `Expr` | ast.rs | Every expression form: `Literal`, `Ident`, `Attr`, `FnCall`, `BinOp`, `Match`, `Pipe`, `Constructor`, `ErrorProp`, `InterpolatedStr`, `List(Vec<Expr>)`, `RecordCreate { type_name, fields }` |
| `Stmt` | ast.rs | `Val(name, expr)`, `Var(name, expr, reason)`, `Assign(name, expr)`, `Expr(expr)` |
| `FnBody` | ast.rs | `Expr(Expr)` for `= expr` shorthand, or `Block(Vec<Stmt>)` |
| `FnDef` | ast.rs | Name, params, return type, effects, optional description, body |
| `Module` | ast.rs | Name, depends, exposes, intent string |
| `VerifyBlock` | ast.rs | Function name + list of `(left_expr, right_expr)` equality cases |
| `DecisionBlock` | ast.rs | Name, date, reason, chosen, rejected list, impacts list, optional author |
| `TopLevel` | ast.rs | Top-level item: `Module`, `FnDef`, `Verify`, `Decision`, `Stmt`, `TypeDef` |
| `TypeDef` | ast.rs | `Sum { name, variants: Vec<TypeVariant> }` or `Product { name, fields: Vec<(String, String)> }` |
| `Value` | interpreter.rs | Runtime value: `Int`, `Float`, `Str`, `Bool`, `Unit`, `Ok(Box<Value>)`, `Err(Box<Value>)`, `Some(Box<Value>)`, `None`, `List(Vec<Value>)`, `Fn{...}`, `Builtin(String)`, `Variant { type_name, variant, fields }`, `Record { type_name, fields }`, `Namespace { name, members }` |
| `Env` | interpreter.rs | `Vec<HashMap<String, Value>>` — scope stack, innermost last |
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

### How to add a new builtin function

In `src/interpreter.rs`, two places:

1. Register the name in `Interpreter::new()`:
   ```rust
   for name in &["print", "str", "int", "abs", "len", "YOUR_BUILTIN"] {
   ```
2. Add a match arm in `call_builtin()`:
   ```rust
   "YOUR_BUILTIN" => {
       // validate args.len(), pattern-match args[0], return Ok(Value::...)
   }
   ```

### How to add a new expression type

1. Add a variant to `Expr` in `ast.rs`
2. Parse it in `parser.rs` (typically in `parse_atom` or a new precedence level)
3. Evaluate it in `interpreter.rs` inside `eval_expr`'s `match expr` block
4. If it should appear in verify cases, update `expr_to_str` in `checker.rs`

## Known issues / edge cases

- **Unary minus** is implemented as `0 - operand`, which means the expression AST is slightly incorrect for float negation edge cases (e.g., `-0.0`)
- **String interpolation re-lexing**: the raw text inside `{...}` is re-lexed and re-parsed at runtime with a fresh lexer. Nested braces are handled by a depth counter, but the inner expression cannot span multiple lines
- **Verify block syntax** uses `=>` as a case separator (`left_expr => expected_expr`); both sides support full expressions including comparisons (`==`, `!=`, etc.) since `=>` is a distinct token (`FatArrow`) that cannot appear inside an expression
- **No check for duplicate function names**: defining a function twice silently shadows the earlier definition
- **`match` is a statement in `parse_fn_body`** (handled via `if check_exact(Match)`) but also an expression in `parse_atom`; this dual path works but means a `match` at statement position does not pass through the normal expression precedence chain
- **Effect list** (`! [Io, State]`) is propagated and enforced statically on function-call edges; runtime does not enforce effects separately
- **`chosen` field in DecisionBlock** only accepts a bare identifier (not a string), so multi-word chosen values require a single CamelCase identifier
- **`reason` in `var`** expects a string literal on the same or next indented line; the block structure differs from `reason:` in decision blocks (decision uses multiline text, var uses a single string)
- **Unknown identifiers in expressions** are inferred as `Any` after emitting a type error so checking can continue; this can produce cascaded follow-up errors in large files

## Next steps (prioritised)

1. **Proper `?` short-circuit** — introduce `ErrPropSignal` as a Rust `Err` variant so that `?` on `Err` inside a function body exits the function early, not just the current expression
2. **Service blocks** — parse and execute `service Foo { needs: [...] }` as a lightweight DI container; dependencies are injected via constructor, no global state
3. **`aver decisions` query flags** — `--impacts Module`, `--since 2024-01-01`, `--rejected Technology` for searchable architectural history
4. **REPL mode** — `aver repl` subcommand for interactive exploration; useful during AI-assisted development sessions

## Agreed direction: modules vs DI (2026-02-25)

- Keep the language explicit in phase 1: `depends [Foo]` resolves from the entry file base directory only (`foo.av` / `Foo.av`), with no hidden CLI/env remapping.
- Treat circular imports as a hard error with an explicit chain (`A -> B -> A`) rather than trying to support partial linking now.
- Keep concerns separate:
  - Module imports (`depends`) answer "where code comes from".
  - Capabilities/services (future effect runtime model) answer "how effects are provided".
- Aver favors self-contained modules: code dependencies are explicit via `depends`, and effect dependencies are explicit via full `! [Effect]` propagation through the call chain.
- If remapping is added later, prefer a versioned project manifest (`aver.toml`) over ad-hoc runtime flags so the mapping is visible to humans and AI agents.
- Service override (for example replacing `Console`) is postponed; if added, it should be explicit, contract-checked, and limited to test/dev profiles first.
