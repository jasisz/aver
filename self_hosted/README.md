# Self-Hosted Aver Interpreter

Full Aver interpreter written in Aver. Parses, resolves, and evaluates all 55 example programs — including terminal games, HTTP services, JSON parser, Redis client, and more.

This directory is the source for `aver run --self-host`. End users who install Aver with `cargo install aver-lang` do not need to run `self_hosted/main.av` manually: the CLI bundles these sources, transpiles them to Rust on first use, builds a cached helper binary, and then reuses that helper for later `--self-host` runs. That helper cache is shared across guest projects for the same installed Aver build, while guest `aver.toml` is loaded at runtime from the requested module root. Running `self_hosted/main.av` directly is mainly for development inside this repository.

## Features

- Complete lexer with INDENT/DEDENT and string interpolation
- Recursive descent parser (expressions, match, fn defs, modules)
- Compile-time variable resolver (ExprVar → ExprSlot)
- Tree-walking evaluator with Map-based env and slot-based locals
- Float literals and arithmetic with Int↔Float promotion
- Pattern matching: literals, wildcards, cons, constructors, tuples
- Record construction and update (`Type.update(val, field = new)`)
- Map literals (`{"key" => value}`)
- Module loading with dedup (Map-tracked) and parent dir fallback
- Full service forwarding: Console, Disk, Http, Tcp, Terminal, Random, Time
- Escaped braces `{{`/`}}` in strings
- Dotted record constructors (`Tcp.Connection(id = ..., host = ..., port = ...)`)

## Architecture

```
domain/
  token.av          — Token types (36 variants)
  ast.av            — AST nodes: Expr, Pattern, Stmt, FnDef, Program
  value.av          — Runtime Val type (13 variants incl. Float)
  lexer/
    chars.av        — Character classification, ident/number reading
  lexer.av          — Tokenizer + INDENT/DEDENT post-processing
  parsermatch.av    — Pattern parsing, skip helpers, utilities
  parser/
    expr.av         — Expression parsing (precedence climbing)
  parser.av         — Statement, fn def, program parsing
  match.av          — Pattern matching engine
  resolver.av       — Compile-time variable resolution (ExprVar→ExprSlot)
  builtins.av       — Service dispatcher + IO builtins
  builtins/
    helpers.av      — Arg extraction (oneArg, twoArgs, expectList, etc.)
    list.av         — List.* builtins
    primitives.av   — Int.*, String.*, Float.*, Char.* builtins
    wrappers.av     — Result.*, Option.* constructors and combinators
  eval.av           — Tree-walking evaluator (Map + slot dual path)
main.av             — CLI entry, module loader, pipeline
verify.av           — Cross-module verify cases
```

## How to run

For normal installed usage:

```bash
aver run hello.av --self-host
aver replay recordings/ --self-host
```

For direct development on the self-host itself:

```bash
cd self_hosted

# Run any example through the self-hosted interpreter
aver run main.av --module-root . -- ../../examples/core/hello.av .

# Multi-module examples need correct module root
aver run main.av --module-root . -- ../../examples/games/life.av ../../examples/games

# Run on VM (~30x faster)
aver run --vm main.av --module-root . -- ../../examples/core/hello.av .

# Compile to native binary (fastest)
aver compile main.av --module-root . --output /tmp/aver-sh --name aver-sh
cd /tmp/aver-sh && cargo build --release
./target/release/aver-sh ~/path/to/example.av ~/path/to/module-root

# Run demos (no file argument)
aver run main.av --module-root .
```

## Test coverage

55/55 example programs pass:
- `core/` — hello, calculator, shapes, temperature, lambda, lists, etc.
- `data/` — fibonacci, json, map, quicksort, red-black tree, rle, date
- `formal/` — law_auto, spec_laws, trust_check
- `games/` — life, snake, wumpus, checkers, tetris, rogue (terminal games need real TTY)
- `apps/` — mission_control, notepad (HTTP server)
- `modules/` — cross-directory module imports
- `services/` — console, disk, http, mysql, redis, tcp, weather

## Pipeline

```
source → lex → parse → resolve → eval
                         ↓
                  ExprVar("x") → ExprSlot(n)
                  StmtBind("x", e) → StmtBindSlot(n, e)
```

Resolver assigns slot indices to local variables. Evaluator uses `Map<Int, Val>` for O(1) slot access in resolved functions, `Map<String, Val>` for top-level statements.
