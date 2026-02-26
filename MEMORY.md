# MEMORY

## 2026-02-26 — Deterministic Replay MVP

- Added execution modes in interpreter: `Normal`, `Record`, `Replay`.
- Effectful builtins now pass through `execute_effect(...)` in `src/interpreter/mod.rs`.
- New CLI workflows:
  - `aver run <file> --record <dir>`
  - `aver replay <recording-file-or-dir> [--diff] [--test] [--check-args]`
- Recording schema includes metadata required to re-run:
  - `program_file`, `module_root`, `entry_fn`, `request_id`, `timestamp`.
- Recording distinguishes outcomes:
  - `RecordedOutcome::Value(JsonValue)`
  - `RecordedOutcome::RuntimeError(String)`
- Replay validates effect order; optional argument validation with `--check-args`.
- Replay-safe value boundary: serializer rejects `Value::Fn`, `Value::Builtin`, `Value::Namespace`.

## 2026-02-26 — Replay module refactor

- Split large `src/replay.rs` into:
  - `src/replay/json.rs` (JSON parser/formatter + `Value <-> JsonValue` conversion + diff)
  - `src/replay/session.rs` (Effect/Session recording models + JSON mapping)
  - `src/replay/mod.rs` (stable re-exports)

## Testing notes

- Pure logic stays in `verify` blocks.
- Effectful logic is regression-tested via replay recordings.
- Added roundtrip tests for nested replay-safe values in `tests/eval_spec.rs`.
