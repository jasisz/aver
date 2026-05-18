# Iron — internals robustness release plan

Working document for the **Iron** track. Follows 0.20.0 "Pulse"; release shape (patch vs minor) decided once full scope lands.

Tagline draft: *Iron in the frame — the type checker stops lying to itself about negation, recursion, and identity; the test bench gains a fuzzer that shakes the rest.*

## Scope (three layers)

### Layer 1 — AST + type system honesty

| ID  | item                                                       | status | nakład   | risk     | PR  |
|-----|------------------------------------------------------------|--------|----------|----------|-----|
| A0  | Occurs check in `bind_expected_var`                        | done   | 5 LOC    | low      | #28 |
| A1  | Unary minus → `Expr::Neg` (cross-backend + self-host)      | done   | ~530 LOC | medium   | stage 5 |
| A2  | Duplicate fn name detection (currently silent shadow)      | done   | ~20 LOC  | low      | #28 |
| A3  | `Type::Named` matching via `SymbolRegistry` canonical name | done   | ~300 LOC | high     | stage 8 |
| A4  | `Type::Invalid` cascade rigor — audit + document           | todo   | ~30 LOC  | low      | —   |
| A5  | `record_field_types` string keys → struct                  | todo   | ~200 LOC | medium   | —   |

### Layer 2 — runtime / replay rigor

| ID  | item                                                       | status | nakład    | risk     | PR  |
|-----|------------------------------------------------------------|--------|-----------|----------|-----|
| B1  | VM typed opcodes (OpAddInt etc.) — perf 2-5× on numerics   | todo   | ~300 LOC  | medium   | —   |
| B2  | VM symbol conflict panics → `Result<_, SymbolError>`       | todo   | ~50 LOC   | low      | —   |
| B3  | Replay determinism contract docs + invariant tests         | done   | ~200 LOC  | low      | #30 |
| B4  | Lexer edge cases (depend on fuzzer findings)               | todo   | TBD       | TBD      | —   |

### Layer 3 — test infrastructure

| ID  | item                                                       | status | nakład    | risk     | PR  |
|-----|------------------------------------------------------------|--------|-----------|----------|-----|
| C1  | Parser + lexer crash-resistance via `proptest` (bytes-in)  | done   | ~80 LOC   | low      | #28 |
| C2  | `proptest` matcher invariants + Type AST generators        | done   | ~150 LOC  | low      | #28 |
| C3  | Cross-backend differential property tests                  | done   | ~280 LOC  | medium   | stage 7 |
| C4  | Proof export CI gate (`lake build` via elan)               | done   | CI yaml   | low      | #29 |
| C5  | AFL++ via `afl.rs` — coverage-guided parser fuzz           | todo   | fuzz crate + CI | medium | — |

### Fuzzers considered

`proptest` alone gives us structured-input fuzzing for matcher invariants
and the replay-codec round-trip, plus a shallow byte-in attack on the
lexer via `prop::collection::vec(any::<u8>(), 0..N)`. What it does *not*
give us is **coverage-guided mutation**: when proptest happens to
generate a byte sequence that hits a new edge, it forgets and rolls
fresh. For a language parser that gates everything behind valid UTF-8
→ valid tokens → valid indentation, coverage feedback is the difference
between "rediscovering lexer rejection 10⁶ times" and "reaching deep
parser paths". C5 fills that gap.

| candidate         | stable Rust? | coverage-guided? | shipping cost                     | verdict for Iron |
|-------------------|--------------|------------------|-----------------------------------|------------------|
| `cargo-fuzz`      | no           | yes              | nightly toolchain in CI           | rejected         |
| AFL++ via `afl.rs`| yes          | yes              | fuzz crate + Linux CI job         | **chosen (C5)**  |
| `honggfuzz-rs`    | yes          | yes              | similar setup, less doc'd         | deferred         |
| `bolero`          | yes          | yes (via libFuzzer/AFL/honggfuzz) | overlaps proptest         | deferred         |
| `LibAFL`          | yes          | yes (custom)     | framework, not drop-in tool       | deferred         |
| `radamsa` / `zzuf`| yes          | no               | no feedback loop                  | rejected         |

`cargo-fuzz` is the default Rust fuzzer but its libFuzzer-sys backend
requires nightly `-Z` flags; pulling nightly into the main CI gate is
a real cost (toolchain churn, longer cache invalidation, divergence
from the rest of `dtolnay/rust-toolchain@stable`). AFL++ via `afl.rs`
runs on stable, has the best-documented corpus/dictionary/minimisation
tooling for parser frontends, and lets the harness live in a separate
workspace-excluded crate that doesn't affect regular `cargo build` /
`cargo test`. `honggfuzz-rs` is a credible secondary engine for an
overnight run on a follow-up release; for Iron, one fuzz engine is
enough and AFL++ has the sharper documentation for our use case
(parser dictionaries, `afl-tmin` crash reduction, `afl-cmin` corpus
minimisation). `bolero` would be the right answer if we wanted a
single abstraction layer over both property and byte fuzzing — but
proptest already owns the property side, and adding bolero just for
byte fuzzing buys nothing AFL++ doesn't give us. `LibAFL` is a
*framework* for building custom fuzzers, not a drop-in tool; the
right time to reach for it is when we want grammar-aware Aver-specific
mutators, which is post-Iron.

Sanitizers (`-Z sanitizer=address`) are nightly territory regardless
of which fuzzer we pick, so C5 ships without ASAN. Crashes that need
sanitizer-level diagnosis get triaged manually with a nightly toolchain
locally; CI stays stable.

### Findings logged

- **C3 #1 — wasm-gc `String.fromFloat` printer drifts from VM's ryu.**
  The first cross-backend property run (`cross_float_arithmetic_vm_vs_wasm_gc`,
  8 cases, shrunk in seconds) flagged
  `String.fromFloat((-853.6346) + (-827.3495))` as
  `-1680.9841000000001` on VM vs `-1680.9841000000002` on wasm-gc.
  Bit-level investigation confirmed the f64 result is identical on
  both backends — the divergence is in `String.fromFloat`, where the
  VM uses Rust's ryu shortest-roundtrip and the wasm-gc backend emits
  a hand-rolled WAT printer
  (`codegen/wasm_gc/builtins/mod.rs::emit_string_from_float`) whose
  tie-break disagrees with ryu in the last decimal digit. The C3
  property tolerates this with a relative-epsilon float compare;
  porting a real shortest-roundtrip algorithm to WAT is the proper
  fix and is out of Iron scope.

- **C3 #2 — wasm-gc `String.fromFloat` trapped on finite floats near
  `1e4`–`1e18` (FIXED in stage 7).** While re-running the same
  property at the CI's `PROPTEST_CASES=2000`, the shrinker landed on
  `String.fromFloat(((-0.5772) * (-61.8024)) * (-877.8128))`
  (≈ `-31313.64`). VM printed `-31313.641292803586`; wasm-gc trapped
  with `wasm trap: integer overflow`. Root cause: the WAT printer's
  shortest-roundtrip search loop incremented `N` up to a 15-digit
  cap and on each iteration ran
  `i64.trunc_f64_s(floor(abs_val * 10^N))`. For
  `abs_val ≈ 31313` and `N = 15` the product hits `3.13e19`, past
  `2^63 ≈ 9.22e18`, and `i64.trunc_f64_s` traps. Fix: an explicit
  overflow guard in the WAT loop bails out one iteration before the
  product would exceed `2^63`, accepting fewer fractional digits at
  very large magnitudes instead of a runtime crash. Shipped in the
  same C3 PR; CHANGELOG carries the user-facing entry.

---

## Unary minus (A1) — detailed plan

### Why

- `-0.0` literal loses sign bit under IEEE 754 (`0.0 - 0.0` = `+0.0`)
- `-1.5` parses as `BinOp(Sub, Int(0), Float(1.5))` — Int/Float mixed binop in typed AST
- Three backends (Lean / Rust codegen / replay) carry a pattern-match hack that re-recognizes the desugar
- Self-host parser carries the same shape in `self_hosted/domain/parser/expr.av`

### Approach

Variant **B** from the design consultation: ship `Expr::Neg(Box<Spanned<Expr>>)` as first-class AST node. Every backend handles `Neg` natively. Pattern-match hacks become dead code, removed.

### Touch surface

| file                                                       | change                                                   |
|------------------------------------------------------------|----------------------------------------------------------|
| `src/ast/expr.rs` (`Expr` enum)                            | add `Neg(Box<Spanned<Expr>>)` variant                    |
| `src/parser/expr.rs`                                       | `parse_unary` emits `Expr::Neg`                          |
| `src/types/checker/infer/expr.rs`                          | infer Neg: `T` <: numeric, return same `T`               |
| `src/codegen/lean/expr.rs`                                 | delete hack at line 60, add `Neg` case → `(-r)`          |
| `src/codegen/rust/expr.rs`                                 | delete hack at line 222, add `Neg` case → `-r`           |
| `src/codegen/wasm_gc/body.rs`                              | new `Neg` case → `i64.const 0; i64.sub` or `f64.neg`     |
| `src/codegen/dafny/mod.rs`                                 | `Neg` case → `-r`                                         |
| `src/replay/entry.rs`                                      | delete hack at line 70, add `Neg` case                   |
| `src/vm/compiler.rs` + `opcode.rs` + `execute.rs`          | optional dedicated `OP_NEG_*` opcodes (or re-desugar)    |
| `src/tco.rs`, `src/resolver.rs`                            | recurse into `Neg.operand`                                |
| `src/ir/{last_use,analyze,escape,buffer_build,interp_lower}.rs` | walker bump                                          |
| `src/codegen/wasm_gc/types_discovery.rs`                   | recurse `Neg.operand` in `collect_*_from_expr`           |
| `self_hosted/domain/ast.av`                                | add `ExprNeg(Expr)` variant                              |
| `self_hosted/domain/parser/expr.av` `parseNegAtom`         | emit `ExprNeg(...)` instead of `ExprSub(ExprInt(0),...)` |
| `self_hosted/domain/...` typechecker + codegen             | handle `ExprNeg`                                          |

### Test matrix

- `-1` → `Neg(Lit(1))`
- `-1.5` → `Neg(Lit(1.5))`
- `-0.0` → bit pattern preserved through compile + eval
- `-x` (non-literal) → `Neg(Ident(x))`
- `0 - x` (explicit) → `BinOp(Sub, ...)` (no rewrite back to Neg)
- nested `--x` / `-(-1.0)` if parser allows (decide reject vs allow)
- Lean witness positions (the `(-2) ∨ (-1) ∨ …` shape)
- replay trace rendering
- self-host parser parity (Rust parses `-1.5` and self-host parses `-1.5` → produce equivalent AST shapes for downstream consumers)

### Decision: `Float.neg` builtin?

Argument for: full source-level expressivity, enables Aver code to flip float sign without going through `0.0 - x` desugar.

Argument against: marginal demand, every in-source negation now hits `Expr::Neg` parser path which handles it without builtin.

**Default: don't add unless user code demands it.** Revisit if a user proposes a concrete use case.

---

## Fuzzer infrastructure (C1-C3) — detailed plan

### Setup

```
fuzz/
  Cargo.toml             # cargo-fuzz crate, libfuzzer-sys + workspace deps
  fuzz_targets/
    parse_bytes.rs       # arbitrary &[u8] → lexer → parser, assert no panic
    parse_then_format.rs # valid Aver → format → parse → AST equality
    typecheck_program.rs # arbitrary well-shaped TopLevel → checker, no panic
    cross_backend_eq.rs  # well-typed program → VM vs wasm-gc vs Rust same output
```

### Generator strategy

**Layer 1 — `arbitrary` `Type`:** recursive prop_oneof with depth + breadth caps. Covered above in chat plan.

**Layer 2 — well-typed Aver programs:** context-aware strategy. Generate random fn signatures with random param/return types, then generate body Exprs that respect expected return type via inverse type-checking (pick an expression form that fits, recurse into args).

**Layer 3 — random source bytes:** for crash-resistance fuzzing of lexer + parser. Both ASCII and UTF-8 fragments. Targeted edge cases as seed corpus: nested string interpolation depth, multi-byte chars in identifiers, malformed indentation.

### Property tests via `proptest`

In `src/types/checker/tests.rs`:
- `occurs_check_terminates(ty, name)` — random Type + name → no panic, returns bool
- `match_is_deterministic(a, b)` — running matcher twice produces identical subst
- `second_bind_consistent_or_rejects(a1, a2, var)` — binding the same var twice with different actuals fails the second time

In `tests/cross_backend_proptest.rs` (new file):
- `vm_wasm_gc_rust_agree_on_well_typed(program)` — generate well-typed Aver program, run on each backend, assert output equality

In `tests/replay_proptest.rs` (new file):
- `replay_is_deterministic(seed)` — generate effect sequence, record, replay twice, both replays match recording exactly

### CI integration

Short fuzz runs gating PRs:
```yaml
- cargo fuzz run parse_bytes -- -max_total_time=180
- cargo fuzz run typecheck_program -- -max_total_time=180
- cargo test --features proptest -- --quiet  # 10_000-case proptest sweep
```

Overnight fuzz job on `main`:
- 1-hour fuzz runs per target, archive interesting corpus, notify on find.

### Likely findings (preregistered hypotheses)

Based on the audit conducted while planning Iron:
1. Lexer panics on malformed UTF-8 / unterminated string interpolation at specific nesting depth
2. Parser panics on missing `expect` invariant in deep nested record / pattern shapes
3. TCO transform panics on shapes the checker recovered through `Type::Invalid` cascade
4. Type checker non-termination on specific recursive type defs that bypass the `RECURSIVE_TYPE_MAX_DEPTH` cap in exhaustiveness check
5. `wasm-gc` discovery walker confusion on deeply nested generics
6. Cross-backend divergence on arithmetic edge cases (i64 overflow, NaN propagation, sign-of-zero)

Each finding → its own bugfix PR within the Iron track.

---

## Sequencing

PR cadence — each item lands separately, mergeable in any order unless marked dependency:

1. #28 — A0 occurs check (open)
2. A2 — duplicate fn name (parallel-able)
3. C1 — cargo-fuzz setup + trivial targets (foundation)
4. C2 — proptest matcher invariants (depends on C1's harness)
5. A1 — unary minus → Expr::Neg (parallel-able with rest)
6. B2 — VM symbol Result (parallel-able)
7. C3 — cross-backend differential property tests (depends on C1)
8. Bugfix PRs from C1-C3 findings (variable)
9. A3 — Type::Named canonicalization
10. A4 — Type::Invalid audit
11. A5 — record key refactor
12. B1 — VM typed opcodes (perf track)
13. B3 — replay determinism docs + tests
14. C4 — proof export CI gate
15. release.py 0.21.0 "Iron"

## Timeline

- Per-PR focused work: 1-3h average
- 14 PRs × 2h ≈ 28h coding
- + review + iteration + integration → 1.5-2× → **4-6 weeks elapsed**

Possible accelerators: parallel PRs for independent items; drop low-value items if implementation reveals diminishing returns.
