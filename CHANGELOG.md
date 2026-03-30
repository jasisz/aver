# Changelog

All notable changes to Aver are documented here.

## 0.7.2 (2026-03-29)

### Added
- **Structured error messages** — `aver check` shows source snippets, repair suggestions, and semantic error categories (`type-mismatch`, `unused-binding`, `missing-verify`). Use `--verbose` for full context on warnings.
- **Unused binding warnings** — `aver check` warns on bindings that are defined but never used. Prefix with `_` to silence.
- **`aver check --json`** — NDJSON output with `schema_version`, `kind`, and summary event. Clean machine-readable stream for editors and CI.
- **`aver verify --json`** — structured NDJSON output: `block-result` per verify block, `diagnostic` per failure, `summary` at end.
- **`aver verify --verbose`** — failure diagnostics with source snippets and full fields (given/law context for specs).
- **`aver replay --json`** — NDJSON output: `replay-result` per recording, `summary` at end.
- **Structured verify diagnostics** — verify failures now use the same diagnostic system as `aver check`: `fail[verify-mismatch]`, `fail[verify-runtime-error]`, `fail[verify-unexpected-err]` with `at:`, `block:`, `case:`, `expected:`/`actual:`, source snippets with carets. Normal mode caps to 3 diagnostics per block.
- **`Map<T, Unit>` as set** — Lean codegen emits `Finset T`, Dafny emits `set<T>`. See [docs](docs/language.md#sets).
- **Common Pushback FAQ** — [docs/pushback.md](docs/pushback.md) covers frequent questions and objections about the language.

### Changed
- **Inline variants (TAG 14)** — single-field variants whose payload is a small int (±268M), bool, unit, or none are now NaN-boxed inline (8 bytes, zero arena allocation). Pattern matching and field extraction skip arena indirection entirely.
- **Unified NDJSON format** — `check`, `verify`, and `replay` all emit `{"schema_version":1,"kind":"..."}` envelope. Summary events at end of each command.
- **Verify output redesign** — per-file grouping, one-line block summaries with failure type breakdown, streaming output. Skipped files (type errors) show count + hint about `--module-root`.
- **Codegen: let-destructuring** — single-arm irrefutable matches (`match x: (a, b) -> expr`) now emit `let (a, b) = x; expr` instead of a full match block.
- **Faster compiled code** — generated Rust is significantly faster across all benchmarks: pattern matching -66%, maps -13%, records -14%, vectors -19%. The self-hosted interpreter is 7-25% faster depending on workload. Fused IR ops (`IntModOrDefault`, `ListIndexGet`) eliminate intermediate allocations; codegen now skips unnecessary clones on Copy fields, drops `&` on numeric arithmetic, and matches borrowed params without cloning the subject.
- **LSP** — Vector namespace completions, updated List members, `exposes opaque` support in document symbols.
- **Editor highlighting** — VSCode and Sublime grammars updated with all current namespaces and keywords.
- Aver formatter keeps medium effect lists on one line when they fit.

### Fixed
- `Console.error`/`Console.warn` in self-hosted now route to stderr.
- `--with-self-host-support` enforces guest-entry contract.
- `aver check --json` no longer emits human-readable lines mixed with JSON.
- `aver replay` no longer duplicates "Replay:" prefix in error messages.

## 0.7.1 (2026-03-27)

### Changed
- `aver run --self-host` now caches its generated helper per installed Aver/self-host build instead of per guest `module_root`, so switching projects no longer forces a rebuild.
- Self-hosted guest `aver.toml` policy is now loaded at runtime from the guest module root and starts only at the guest boundary, matching scoped replay behavior.
- Cold `--self-host` runs now print short progress messages while Aver generates and builds the cached helper.
- `aver compile` now exposes runtime policy mode explicitly via `--policy embed|runtime`; plain codegen defaults to `embed`, while `--with-replay` defaults to `runtime`.

### Fixed
- `aver run --self-host` no longer misclassifies qualified user module calls like `Map.generateMap` or `Time.foo` as builtins just because they share a builtin namespace prefix. Self-hosted module programs such as `examples/games/rogue` now execute correctly again.

## 0.7.0 (2026-03-26)

**Breaking:** `List.get` and `List.append` removed. Use `Vector` for indexed access.

### Added
- **`Vector<T>`** — indexed sequence with O(1) get/set. API: `Vector.new`, `Vector.get`, `Vector.set`, `Vector.len`, `Vector.fromList`, `Vector.toList`. `Vector.set` returns `Option<Vector<T>>`.
- **Mutual TCO** in codegen — mutually recursive functions compiled to trampoline dispatch loops.
- **Namespace effect shorthand** — `! [Disk]` covers all `Disk.*` effects.
- **Self-host CLI path** — `aver run --self-host` and `aver replay --self-host` now run through the Aver-in-Aver interpreter compiled to a cached Rust binary.
- **Scoped generated replay runtime** — `aver compile --with-replay --guest-entry <fn>` emits replay support that starts record/replay and `aver.toml` policy at an explicit guest boundary instead of the process boundary.

### Changed
- **`List` is now purely recursive** — `prepend`, `head`, `tail`, `concat`, `reverse`, `contains`, `find`, `any`, `zip`. No indexed access.
- Idiomatic pattern: build with `List.prepend` → `List.reverse` → `Vector.fromList` (zero-copy on Flat lists).
- Compiled projects use LTO + `codegen-units = 1` for faster release builds.
- Self-hosted interpreter ~1.5× faster (COW maps, `Rc<str>` strings, Vector env).
- Installed `aver` now bundles the `self_hosted/` sources directly, so `aver run --self-host` bootstraps its cached helper binary automatically without a separate self-host install step.
- Generated Rust projects now target Rust 2024.
- `aver check` no longer suggests granular namespace effects on wrappers that also require the broad namespace transitively through a callee.
- `benches/comparison_bench.rs` now measures the real `aver run --self-host` CLI path instead of a stale standalone `aver-self` binary from `$HOME/.cargo/bin`.

### Removed
- `List.get`, `List.append`.

## 0.6.1

Highlights:
- VM is 25–54% faster across benchmarks; interpreter-to-VM speed ratios improved from 5–7× to 7–13×.
- Added `aver run --profile` for opcode/function-level VM profiling.
- Fixed several VM correctness and memory issues, including match fallthrough, deep-list return overflow, and request-local stable-space retention.

### Added
- `aver run --profile` — VM execution profile with opcode counts, function stats, and opcode-pair analysis.
- Game of Life example (`examples/games/life.av`) with terminal visualization and FPS counter.
- Self-hosted interpreter project in Aver (`self_hosted/`).

### Changed
- Added specialized VM handling for common unwrap/default, boolean branch, and fused-load patterns.
- Added frameless calls for small leaf functions to reduce hot-path call overhead.
- Bool `match` on `true/false` now compiles to a direct conditional branch.
- Refined VM value layout to reduce wrapper overhead and speed up dispatch.
- `Terminal.size` now returns a record with `width`/`height` fields instead of a tuple. Generated Rust requires `aver-rt >= 0.3.1`.
- `aver context --json` now uses `serde` serialization.
- Lean proof export now emits universal theorems with `sorry` when auto-proof fails.

### Fixed
- Exhaustiveness checker hang on recursive sum types.
- `MATCH_DISPATCH_CONST` fallthrough causing infinite recursion in patterns like `fib(n)`.
- Arena stack overflow on deep list returns.
- `HttpServer` callback stable-space retention across requests.
- Lean export reserved-word conflict for `toString`.

## 0.6.0

### Added
- **Bytecode VM** — `aver run --vm` compiles Aver to a stack-based bytecode VM with NaN-boxed values, region-based arena memory (young/yard/handoff/stable), dedicated list opcodes, structural persistent lists, and thin-function fast return paths. 5-9x faster than the tree-walking interpreter on compute-heavy workloads.
- **Terminal service** — `Terminal.*` namespace (12 methods) for raw-mode terminal I/O via crossterm: cursor control, colored output, non-blocking key input, screen management. Behind `terminal` cargo feature (enabled by default).
- **Terminal guard** — `aver run` installs a drop guard that restores terminal state (cursor, colors, raw mode) on exit, panic, or runtime error.
- **Bool namespace** — `Bool.or`, `Bool.and`, `Bool.not` pure builtins for logical combinators.

### Changed
- `aver-rt::AverList` now packs repeated `append` chains into segmented chunk spines, improving list-heavy workloads in both the interpreter and generated Rust.

## 0.5.5

### Added
- **Opaque types** — `exposes opaque [TypeName]` in module declarations. Types listed as opaque are visible in signatures but cannot be constructed, field-accessed, or pattern-matched from outside the defining module. Enforced at compile time by the typechecker. See `docs/language.md` for usage.
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
- `workflow_codegen_bench` for repeatable end-to-end comparisons between interpreter, VM, and generated Rust on `projects/workflow_engine`

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
