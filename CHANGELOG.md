# Changelog

All notable changes to Aver are documented here.

## Unreleased

### Added
- `aver context --focus <symbol>` builds context around a specific function's dependency cone (callees, types, verify blocks, decisions)
- Priority scoring for `aver context` budget allocation: elements with more verify coverage, spec references, and focus relevance are included first
- Type-aware verify sample selection: scorer uses fn return type to pick diverse cases (Ok + Err for Result fns, true + false for Bool, per-constructor for sum types)
- Granular verify coverage warnings: checker now reports missing Result Ok/Err, Option Some/None, Bool true/false, and sum type variant cases

### Changed
- `src/checker.rs` split into `src/checker/` module (coverage, verify, intent, law)

## 0.5.4

### Added
- **Dafny verification backend** — `aver proof --backend dafny` generates a `.dfy` file with Z3-powered automated proofs for `verify law` blocks; complements Lean's `native_decide` / tactic approach
- **Random service** — `Random.int(min, max)` and `Random.float()` with `! [Random]` effect, backed by `aver_rt::random` (OS entropy via `rand` crate behind feature flag)
- **Shared `Builtin` enum** — `codegen/builtins.rs` defines all pure Aver builtins (~80 variants); adding a new builtin forces all backends to handle it via exhaustive match
- **Shared codegen utilities** — `codegen/common.rs` now provides `escape_string_literal`, `split_type_params`, `escape_reserved_word`, `parse_type_annotation`, and `to_lower_first` used by all three backends
- `docs/dafny.md` documenting the Dafny backend, its two-layer contract (sample assertions + universal lemma), and Lean vs Dafny comparison
- Wumpus example (`examples/apps/wumpus.av`) — Hunt the Wumpus with dodecahedron topology, effectful random, full verify coverage (30/30)

### Changed
- `aver proof` now accepts `--backend lean|dafny` (default: `lean`)
- **Lean auto-proof simplified** — removed brittle indirect-recursion and recursive map-presence strategies; the backend now honestly rejects patterns it can't prove instead of generating fragile proofs. Helper-law dependency hints guide users toward layered verification.
- Dafny emits `verify law` as both capped sample assertions (max 5) and a universal `lemma`; `verify` cases are not emitted (Z3 can't compute deep recursion)
- All three codegen backends (Lean, Dafny, Rust) now share reserved-word escaping, string literal escaping, and type annotation parsing via `codegen/common.rs`
- Rust codegen now gates `aver-rt` features (`http`, `random`) based on which services the program actually uses

## 0.5.3

### Added
- `projects/payment_ops` as a medium-size dirty-backoffice showcase covering provider normalization, replay, settlement reconciliation, manual-review cases, and audit trail

### Fixed
- Rust codegen ownership for list / tuple / map literals and record updates, so valid Aver programs no longer emit generated Rust that fails with move errors in these patterns

## 0.5.2

### Added
- `workflow_codegen_bench` for repeatable end-to-end comparisons between `aver run` and generated Rust on `projects/workflow_engine`

### Changed
- interpreter function bodies now lower to shared `ExprId`-based runtime nodes, so the evaluation hot path no longer carries cloned AST fragments through continuations
- `aver check` now tells users with non-tail-recursive functions to either rewrite them into tail recursion or make them a spec, and canonical spec functions no longer emit that warning
- generated Rust now pins `aver-rt = "=0.2.1"` so current codegen matches the shared runtime features it emits

### Fixed
- Rust codegen regressions around nested builtin-argument liveness, same-arity mutual tail calls, and memoized recursive named types, restoring generated builds for examples such as `grok_s_language`, `red_black_tree`, and `mysql`


## 0.5.1

### Added
- native LSP document formatting via the shared Aver formatter
- richer `aver-lsp` editor UX: effect-aware completion, verify/decision code lenses, contract-first hover, and document symbols with nested `verify`
- publishable `aver-lsp` crate metadata and docs for installing the language server separately from `aver-lang`

### Changed
- editor install docs now target `cargo install aver-lsp` plus editor extension installation, with local source-build setup kept as a development path

## 0.5.0

### Added
- `Args.get()` as an explicit runtime service for CLI arguments (`List<String>`)
- round-trip law coverage for naturally invertible examples, including `json`, `grok_s_language`, and `notepad/store`
- `aver context --budget` with `kb` / `mb` suffixes for prompt-sized exports
- `aver context` selection metadata in JSON and in the `--output` summary, including included depth and next-depth size
- modular Rust code generation that emits `src/aver_generated/...` instead of flattening all Aver code into one giant `main.rs`
- directory inputs for `aver check` and `aver verify`, so one command can walk a whole example or project tree
- `projects/workflow_engine` as a serious medium-sized Aver application core, covering projects, tasks, workflow rules, audit trail, notifications, and CLI/query flows
- `aver check` warnings for recursive functions that still contain non-tail recursive callsites after TCO, with accumulator-style guidance
- iterative interpreter expression evaluation backed by a heap continuation stack instead of the Rust call stack

### Changed
- **Breaking:** effect aliases (`effects X = [...]`) were removed; declare concrete method effects directly in `! [...]`
- **Breaking:** broad namespace declarations such as `! [Http]` no longer satisfy child effects like `Http.get`
- `aver verify` now checks only declared `left => right` examples; coverage-style diagnostics moved to `aver check`
- `aver check` now reports coverage hints as warnings and no longer exits non-zero because of warnings alone
- `aver context` now defaults to `--depth auto --budget 10kb` instead of walking dependencies without a budget
- `aver context --json` stays human/LLM-oriented: compact signatures, short verify strings, omitted empty sections, and skipped long verify cases
- examples were reorganized into `core/`, `data/`, `formal/`, `modules/`, `services/`, and `apps/` under a shared `--module-root examples`, while standalone showcase apps now live under `projects/`

### Fixed
- `aver verify --deps` now verifies transitive dependencies
- exposed sum types and constructors now resolve correctly across module boundaries
- fully-qualified constructor patterns now work consistently in parsing, typechecking, exhaustiveness, and runtime matching
- `Result<Unit, String>` now accepts `Unit` cleanly and renders `Unit` consistently
- `unused exposes` diagnostics now resolve real symbol usage from AST and point at the module's `exposes` line
- Rust codegen now resolves module-qualified Aver calls/types without flattening sibling modules into one ambiguous Rust namespace
- Rust codegen now routes `Args.get()` through `aver-rt`
- deep `AverList` teardown and `append -> match` / `tail` paths in `aver-rt`, removing shared stack-overflow cliffs for both the interpreter and generated Rust
- `String.slice` semantics are now shared between the interpreter and `aver-rt`, including negative-index clamping

## 0.4.0

### Added
- `aver proof` as a dedicated Lean proof-export command
- `aver --version`
- docs for `Unit`, `main` returning `Result<Unit, String>`, and the `HttpServer.listen` / `listenWith` callback model

### Changed
- **Breaking:** `aver compile` now targets Rust only
- **Breaking:** Lean export moved from `aver compile -t lean` to `aver proof`
- **Breaking:** Lean CLI flags were renamed from `--lean-verify` to `--verify-mode`
- **Breaking:** match patterns now reject positional record destructuring such as `User(name, age)`; bind the record and use field access instead
- **Breaking:** constructor patterns must now be qualified (`Shape.Circle`, `Result.Ok`, `Option.None`) instead of bare `Circle` / `Some` / `None`
- CLI/docs were split around two separate backend intents: deployment (`compile`) and proof export (`proof`)

### Fixed
- Lean proof export now respects qualified cross-module calls such as `Examples.Json.toString` during function emission ordering
- Lean prelude now injects built-in `Header`, `HttpRequest`, `HttpResponse`, and `Tcp.Connection` support when generated code references those runtime types
- Lean `List.get` now preserves Aver's `Int` index semantics, including negative indices returning `Option.None`
- `examples/notepad/routes.av` proof export now builds successfully under Lean with `aver proof --verify-mode auto`
- parser/typechecker/interpreter specs were aligned with the qualified-constructor pattern rules and explicit record binding model

## 0.3.0

### Added
- `aver-rt` as a shared Rust runtime crate for transpiled projects and interpreter adapters
- `aver check --deps` to run contract checks for transitive `depends [...]` modules
- deterministic replay now walks nested recording directories
- recursion-first list runtime based on persistent `AverList`

### Changed
- **Breaking:** function bodies now use indentation only; `fn ... = expr` shorthand was removed
- **Breaking:** `|>` pipe operator was removed
- **Breaking:** `List` was simplified to a recursion-first API: `len`, `get`, `prepend`, `append`, `concat`, `reverse`, `contains`, `zip`
- **Breaking:** `List.push`, `List.head`, `List.tail`, `List.map`, `List.filter`, `List.fold`, `List.find`, `List.any`, and `List.flatMap` were removed
- **Breaking:** removed `aver decisions`; decision export now lives under `aver context --decisions-only`
- Rust transpilation now depends on the published `aver-rt` crate by default, with optional `AVER_RUNTIME_PATH` override for local runtime hacking
- `aver check` contract diagnostics now always include line numbers
- Decision `impacts` now accepts both validated symbols and semantic strings
- `input`, `expect`, `case`, `where`, `effect`, `service`, `needs` are no longer reserved keywords
- README and docs were restructured around quickstart, AI-first positioning, and the current CLI/runtime model

### Fixed
- old `= expr` syntax now fails consistently in parser and formatter with an actionable migration error
- `decisions/*.av` updated to conform to strict impacts validation and namespaced console usage
- renamed `examples/type_errors.av` to `examples/test_errors.av` with expanded checker diagnostics coverage

## 0.2.3

### Added
- `verify ... law ...` blocks with typed `given` domains (`a..b` ranges and explicit lists)
- Lean emission for `verify law`: named law theorems, sample theorems, and universal theorem skeletons
- Lean verify modes in CLI: `--lean-verify auto|sorry|theorem-skeleton`
- `--lean-proof-mode` fail-fast gate for proof-unsafe Lean transpilation paths
- Deeper match exhaustiveness analysis for nested and recursive patterns

### Changed
- `aver check` now treats missing verify on pure non-trivial functions as an error
- `verify law` skips regular case-level target-call heuristics used by `verify` case blocks
- Decision block fields (`date`, `author`, `reason`, `chosen`, `impacts`, etc.) are contextual (no longer globally reserved keywords)
- File-based commands require exactly one `module` declaration as the first top-level item

### Fixed
- Exhaustiveness checker stack overflow on recursive sum types with 2+ variants
- Empty `verify` blocks are rejected explicitly
- Rust codegen now fails fast on unresolved unknown types instead of panicking
- Lean codegen/parser ordering and mutual-recursion proof-mode integration issues
- Multiple Clippy-level borrow/style issues in builtin dispatcher paths

## 0.2.2

### Added
- Lean transpilation target in CLI: `aver compile -t lean`
- Lean codegen backend module structure (`src/codegen/lean/*`)
- Transpilation docs for Lean target in README and `docs/transpilation.md`

### Fixed
- Shared deterministic function ordering via call-graph SCC topo order (callee-before-caller) for codegen backends
- Lean forward-reference failures in emitted code (e.g. helper emitted after use)
- Lean prelude: avoid reserved keyword `from` in generated `String.slice`
- Lean `AverMap.set` now preserves key order when updating existing keys

## 0.2.1

### Added
- HttpServer: skip real TCP server in `--record` mode (return Unit immediately)
- Example recordings: console_demo, disk_demo, http_demo, notepad
- Recording snapshots persist on every effect (long-running processes safe)
- Module sum types exported via `exposes`
- Map literal syntax in examples

### Fixed
- Silent body discard when `=` appears after bindings in fn body
- Architecture decision output formatting
- README: clarify no Int/Float promotion, document missing Disk methods

## 0.2.0

### Added
- **Aver-to-Rust transpiler** (`aver transpile`) with full service support, module inlining, last-use analysis, and copy-type elision
- **LSP server** and **VSCode extension** with diagnostics, hover, go-to-definition, and module dependency caching
- **Deterministic replay** (`--record` / `aver replay`) — record effectful runs, replay without I/O
- **Static match exhaustiveness checking** at compile time
- **Compile-time variable resolution** — `Ident` → `Resolved(slot)` for O(1) lookup in fn bodies
- **Auto-memoization** of pure recursive functions with memo-safe arguments
- **Tail-call optimization** — self and mutual recursion without stack overflow
- **Typed bindings** — `name: Type = expr` with type checker validation
- **Tuple values** and tuple destructuring in match patterns
- **Map type** with `Map.get`, `Map.set`, `Map.keys`, `Map.values`, `Map.has`, `Map.remove`, `Map.size`
- **Map literal syntax** — `{"key" => value, ...}`
- **Record update syntax** — `Type.update(base, field = newVal)`
- **Multiline expressions** inside `()`, `[]`, `{}` delimiters
- **`Char` namespace** — `Char.toCode`, `Char.fromCode` for Unicode operations
- **`Byte` namespace** — `Byte.toHex`, `Byte.fromHex`
- **`String` additions** — `String.charAt`, `String.toLower`, `String.toUpper`
- **`List` additions** — `List.find`, `List.any`, `List.contains`, `List.zip`, `List.flatMap`
- **`Result.withDefault`**, **`Option.withDefault`**, **`Option.toResult`** combinators
- **Generic type inference** for Option/Result combinators
- **JSON parser** (`examples/json.av`) — full RFC 8259 with `\uXXXX` surrogate pairs, control char validation
- **Persistent Tcp connections** — `Tcp.connect`/`writeLine`/`readLine`/`close` with opaque `Tcp.Connection`
- **Redis RESP client** (`examples/redis.av`)
- **HttpServer service** — `HttpServer.listen` and `HttpServer.listenWith` (explicit context parameter)
- **Weather microservice example** — HttpServer + Http + Redis cache
- **Notepad REST API example** — multi-module CRUD app with Disk persistence
- **Agent challenge infrastructure** — prepare.sh, evaluate.sh, 3 challenges for AI agent testing
- **Interactive REPL** (`aver repl`) — stateful, multi-line, type-checked
- **`aver context`** — project context export for LLM consumption (Markdown + `--json`)
- **`aver decisions`** — generated architecture decision docs
- **`aver check --strict`** mode
- Human-readable parser error messages via `TokenKind` Display
- Editor support: VSCode extension + Sublime Text syntax highlighting
- Prepared for crates.io publication as `aver-lang`

### Changed
- **Breaking:** `List.get`, `List.head`, `List.tail` now return `Option` (was raw value / error)
- **Breaking:** `String.length` renamed to `String.len`
- **Breaking:** `val`/`var` keywords removed — all bindings are `name = expr`, always immutable
- **Breaking:** flat builtins removed (`print`, `len`, `map`, `filter`, `fold`, `str`, `int`, `abs`, etc.) — use namespaced equivalents (`Console.print`, `List.len`, `List.map`, ...)
- **Breaking:** `Ok`/`Err`/`Some`/`None` keywords removed — use `Result.Ok`, `Result.Err`, `Option.Some`, `Option.None`
- **Breaking:** `Any` removed from surface syntax — `Type::Unknown` is internal only
- **Breaking:** colon-only type annotations (`x: Int` not `x Int`)
- Renamed `Network` service to `Http`
- Env uses `Rc<Value>` with slot-based frames for resolved functions
- Closures removed — functions see globals at call time, not capture time
- Pipeline: parse → TCO transform → typecheck → resolve → interpret
- Verify warnings only for pure non-trivial functions
- File size warning raised to 250 lines

### Fixed
- Constructor rules enforced: named fields required for records, positional for sum variants
- Empty list binding rejection without type annotation
- Pipe RHS parsing tightened
- Tuple memo hashing
- `{{ }}` brace escapes in string highlighting
- Module function scope and memo collisions
- Match arm body error message after unexpected newline
- LSP UTF-16 position handling

## 0.1.0

Initial release of the Aver language interpreter.

### Core language
- Significant indentation (Python-like)
- Immutable bindings (`name = expr`)
- Functions with descriptions (`?`), effect declarations (`! [Effect]`), and type annotations
- `match` as the only branching construct (no `if`/`else` by design)
- No loops — `List.map`/`filter`/`fold` for iteration
- String interpolation with `{expr}`
- `|>` pipe operator
- `?` error propagation operator

### Type system
- Static type checker with named types, generics, `Result<T, E>`, `Option<T>`
- User-defined sum types and records
- Function types with effect annotations (`Fn(A) -> B ! [Effect]`)
- List pattern matching (`[]`, `[h, ..t]`)

### Built-in namespaces
- `Int`, `Float`, `String`, `List` — pure operations
- `Console` — print, error, warn, readLine (`! [Console]`)
- `Http` — GET, HEAD, DELETE, POST, PUT, PATCH (`! [Http]`)
- `Disk` — readText, writeText, appendText, exists, delete, deleteDir, listDir, makeDir (`! [Disk]`)
- `Tcp` — send, ping (`! [Tcp]`)

### Module system
- `module` blocks with `intent`, `depends`, `exposes`
- Dot-path imports (`depends [Examples.Foo]`)
- Named effect sets (`effects AppIO = [Console, Disk]`)

### Tooling
- `aver run` — execute programs
- `aver verify` — run verify blocks as tests
- `aver check` — static analysis (types, effects, style)
- `verify` blocks — declarative equality-based test cases
- `decision` blocks — architectural decisions as code
