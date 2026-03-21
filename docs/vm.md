# Aver VM

This document describes the current bytecode virtual machine used by `aver run --vm`, `aver verify --vm`, and `aver replay --vm`.

It is a design note, not a frozen spec. The opcode set and internal representation may still change while the VM matures.

## What It Is

The Aver VM is a small stack machine that executes the same surface language as the tree-walking interpreter.

It is intentionally **language-shaped**, not generic:

- opcodes model Aver concepts directly
- pattern matching is compiled to explicit match/destructure instructions
- tail calls are part of the ISA
- records, variants, wrappers, lists, and tuples are first-class runtime values

This is not a mini-JVM or a universal IR. It is a runtime designed around the constraints of Aver.

## Execution Model

The VM compiles resolved Aver AST into bytecode function chunks:

- `src/vm/compiler.rs` lowers AST to bytecode
- `src/vm/execute.rs` runs the stack machine
- `src/vm/opcode.rs` defines the ISA
- `src/vm/runtime.rs` handles builtins, effects, and record/replay at the host boundary

Execution is stack-based:

- locals live in the current frame
- operands are pushed onto the VM stack
- calls create or reuse frames
- returns leave one value on the caller stack

The VM now also marks conservatively-classified **thin functions** and **parent-thin functions**.

These are small helpers that do not use tail-call frame reuse, do not write globals, and do not emit obvious aggregate-construction opcodes such as `RECORD_UPDATE`, `WRAP`, `LIST_*`, `TUPLE_NEW`, or `VARIANT_NEW`.

When a thin function returns and the runtime can confirm that its local `young` / `yard` / `handoff` marks never moved, the VM skips the normal boundary relocation path entirely.
In practice this means many tiny Aver helpers now behave like:

- keep normal stack locals while running
- but do not pay full survivor/stable bookkeeping on return
- unless they actually created local heap state after all

`parent-thin` is narrower and more Aver-specific:

- it is meant for wrapper-like helpers, not for general small functions
- it borrows the caller `young` lane directly
- it avoids ordinary-return `handoff` as long as it never touches `yard` / `handoff`
- its local `young` scratch dies later at the caller boundary instead of forcing a helper-local relocation step

The classifier is deliberately less strict than a pure "single expression only" rule:

- small `match` helpers with local bindings can still be `parent-thin`
- field/tuple extraction and other tiny control-flow opcodes are allowed
- nullary variant constructors (`Status.Todo`) are treated as inline results, so they can still stay on the thin / parent-thin fast path
- list destructuring and obviously aggregate-building builtins still stay out of `parent-thin`

Execution-backed commands can run through the interpreter or through the VM:

```bash
aver run app.av
aver run app.av --vm
aver verify app.av
aver verify app.av --vm
aver replay recordings/ --vm
```

## Value Representation

The VM runs on `NanValue`, not on the higher-level `Value` enum.

The current layout is best thought of as **semantic tags first, storage second**.

Floats still use the plain IEEE path. Everything else is a tagged quiet-NaN:

```text
63      50 49  46 45                    0
┌────────┬──────┬────────────────────────┐
│ 0x7FFC │ tag  │       payload          │
│ 14 bits│ 4 bit│       46 bits          │
└────────┴──────┴────────────────────────┘
```

Current tags:

| Tag | Meaning | Payload shape |
|---:|---|---|
| `0` | `Immediate` | `false` / `true` / `Unit` |
| `1` | `Symbol` | fn / builtin / namespace / nullary-variant handle |
| `2` | `Int` | inline signed int or arena big-int |
| `3` | `String` | inline small string or arena string |
| `4` | `Some` | inline payload or boxed arena payload |
| `5` | `None` | singleton |
| `6` | `Ok` | inline payload or boxed arena payload |
| `7` | `Err` | inline payload or boxed arena payload |
| `8` | `List` | empty list or arena list |
| `9` | `Tuple` | arena tuple |
| `10` | `Map` | empty map or arena map |
| `11` | `Record` | arena record |
| `12` | `Variant` | arena payload variant |

The important convention in `v2` is that `bit45` is now mostly the **“does this value carry an arena reference?”** discriminator:

- `Int`: inline int vs arena big-int
- `String`: inline small string vs arena string
- `Some` / `Ok` / `Err`: inline payload vs boxed arena payload
- `List` / `Map`: empty singleton vs arena aggregate
- `Tuple` / `Record` / `Variant`: always arena-backed

That makes the representation much more regular than the older wrapper-heavy scheme.

### Inline Cases That Matter

The point of `v2` is not only compactness. It is to keep the common Aver shapes cheap:

- `Bool`, `Unit`, and `None` are pure inline singletons
- `Some(true)`, `Ok(Unit)`, `Err(None)` stay inline
- `Some(42)`, `Ok(-7)`, `Err(0)` stay inline as long as the int fits the wrapper-inline range
- `[]` and `Map.empty()` are real values under their normal collection tags, not exceptions hidden in `Immediate`
- strings up to 5 UTF-8 bytes stay inline under `TAG_STRING`
- nullary variants such as `Status.Todo` or `Color.Red` travel as `Symbol` handles instead of arena entries

That keeps `Result` / `Option` pipelines, empty collections, and short-string-heavy code from manufacturing arena churn just to move tiny values around.

### What Still Goes To The Arena

The arena is still where the real aggregate payloads live:

- large `Int`
- long `String`
- non-empty `List`
- non-empty `Map`
- `Tuple`
- `Record`
- payload-carrying `Variant`
- boxed wrapper payloads when `Some` / `Ok` / `Err` cannot stay inline

This is the main reason the VM can stay small without dragging a bigger object model through every helper call.

## Memory Model

The VM no longer uses one “grow forever” arena.

Instead it splits heap-backed values into four runtime spaces:

- `young` for short-lived temporaries created while evaluating the current step
- `yard` as a tail-position construction lane
- `handoff` as an ordinary-return construction lane
- `stable` as the canonical long-lived space

Each call frame records marks for the local `young`, `yard`, and `handoff` suffixes it owns.
That means the VM knows exactly which heap entries were created “during this frame” and can reclaim them in bulk.

### What Those Spaces Mean Today

Conceptually:

- `young` means “local scratch work”
- `yard` means “this value is being built for a tail-call path”
- `handoff` means “this value is being built for an ordinary return path”
- `stable` means “this value is safe to keep beyond the current frame boundary”

Implementation-wise, the current VM now splits boundary behavior by control-flow shape:

- values can still be *allocated* into `yard` or `handoff` in obvious tail/return positions
- at `TAIL_CALL_*` boundaries, live roots are kept in `yard`, so loop-carried state stays out of `stable`
- at ordinary `RETURN` boundaries to another Aver frame, live roots stay on the handoff path instead of being forced into `stable`
- parent-thin wrappers are the exception: they borrow caller `young` and skip ordinary-return handoff entirely unless they spill into `yard` / `handoff`
- pure-`handoff`, pure-`young`, and single-result mixed helper returns use fast ordinary-return paths
- larger mixed `young + handoff` graphs still fall back to full evacuation, because correctness matters more than over-eager survivor cleverness
- only globals, host-facing escapes, and top-level completion are canonicalized into `stable`
- then the frame-local `young` / `yard` / `handoff` suffixes are truncated or compacted as appropriate

This matters because it gives the VM a real survivor lane for TCO-heavy programs and for ordinary helper chains, without forcing every “survives one more call boundary” value through `stable`.

So the current VM is:

- region-style for local scratch memory
- yard-based for tail-call survivors
- handoff-based for ordinary helper returns, with a conservative fallback for larger mixed graphs
- stable-space based for globals, host-facing escapes, and top-level canonicalization
- explicit about which lanes are used during construction

That already gives us the most important property: frame-local garbage dies in bulk, and long-lived values stop pretending to live in temporary memory.

### Memory Flow

The easiest way to think about the VM is:

1. New local work starts in `young`.
2. In obvious tail-position construction, aggregates may be built in `yard`.
3. In obvious ordinary-return construction, aggregates may be built in `handoff`.
4. On `TAIL_CALL_*`, live roots are evacuated into `yard`.
5. On ordinary `RETURN` to another Aver frame, live roots stay on the handoff path.
6. On top-level completion or real escape boundaries, live roots are canonicalized into `stable`.
7. The frame-local `young` / `yard` / `handoff` suffixes are then truncated in one shot.

For helper-sized functions there are now two extra fast paths:

8. If a frame returns with unchanged local marks, the VM skips boundary promotion/truncation work for that frame and resumes the caller directly.
9. If a `parent-thin` frame only touched borrowed `young`, it returns directly to the caller without building ordinary-return handoff state at all.

That means the VM still distinguishes:

- local scratch work
- tail-position construction
- caller-facing return construction
- truly long-lived values

The important distinction now is:

- `yard` survives the next tail-call boundary
- `handoff` survives the next ordinary call/return boundary
- borrowed parent-`young` is the cheapest path of all, but only for very narrow wrapper-like helpers
- `stable` is for values that really outlive the current Aver call chain

### What Goes Where

Typical examples:

- `tmp = (x, y)` inside a function body:
  lives in `young`
- `List.prepend(n, acc)` used as the next argument of a tail-recursive call:
  can be built in `yard`, and stays in `yard` when the tail-call boundary is finalized
- `Result.Ok(value)` built just before returning from a helper:
  can be built in `handoff`, and stays in `handoff` while the caller continues
- a helper that built both local temporaries and one final returned aggregate:
  can still stay on the fast ordinary-return path when that returned aggregate is the only fresh handoff root; larger mixed graphs fall back to full evacuation
- storing a value into globals, returning from top-level, or passing a value across a host boundary:
  goes to `stable`

The point is not only speed. The point is that the runtime distinguishes “temporary while computing” from “safe to keep after this frame ends”.

### Why There Is Still No Full GC Loop

The VM still does not need a classical "GC everywhere" story:

- `young`, `yard`, and `handoff` are reclaimed by explicit boundary truncation
- `stable` is compacted from live roots at top-level completion or explicit escape boundaries

So there is still tracing and relocation, but not as one global always-on collector. Most memory dies because control flow tells us it can die, and only `stable` needs long-lived root-driven maintenance.

## List Representation

Lists in the VM are not just flat `Vec` payloads.

The current arena list storage supports four shapes:

- `Flat` for compact literal / materialized lists
- `Prepend` for cheap `List.prepend` and `LIST_CONS`
- `Concat` for cheap structural concatenation
- `Segments` for concat-tail views produced by repeated destructuring

Repeated `List.append` does not keep building a one-element-deep concat chain forever. The VM grows the right edge in flat chunks, so append-heavy code stays structural without turning indexed access into a totally degenerate tree walk.

This matters because the VM can now keep list construction aligned with Aver semantics instead of flattening on every prepend.

Pattern matching and destructuring (`MATCH_CONS`, `LIST_HEAD_TAIL`) use list helpers that understand these shapes directly. In particular, destructuring a `Concat` tail no longer rebuilds a fresh concat suffix on every step; it can carry a cheap segment-view instead.

Core list operations also have dedicated bytecode paths:

- `LIST_LEN`
- `LIST_GET`
- `LIST_APPEND`
- `LIST_PREPEND`
- `LIST_GET_MATCH`

That avoids paying full generic builtin-dispatch overhead for the most common list operations in real Aver programs.

In obvious tail-call positions, the VM can allocate new aggregate values directly into the frame yard instead of forcing an immediate young-to-yard copy on the next `TAIL_CALL_*`.

In obvious ordinary return positions, the VM can allocate new aggregate values directly into the frame handoff lane, so helper returns can survive into the caller without first pretending to be temporaries or globally-stable values.

## Symbol Table

The VM now keeps a single interned table of **compile-time-known names**:

- function names
- builtin/service members
- declared effect names
- type / field / variant names discovered while compiling

Each entry gets a stable `symbol_id`.

That lets the VM stop carrying string-ish dispatch state through hot paths:

- function values travel as inline `Int(symbol_id)`
- `CALL_VALUE` resolves `symbol_id -> function`
- `CALL_BUILTIN` carries `symbol_id` instead of a builtin name or arena string
- builtin effect checks compare interned effect ids instead of runtime strings

This is intentionally simple and Aver-shaped:

- one symbol table
- one inline handle format
- metadata attached to the symbol entry

Not every runtime value is a symbol. User data is still just data. But anything the compiler already knows by name no longer needs string dispatch during execution.

## Function References

One of the more unusual choices is that the VM separates:

- **runtime symbolic values** in `NanValue` via `TAG_SYMBOL`
- **VM-known callable ids** in bytecode and call dispatch via inline `symbol_id`

That means the runtime still has one shared symbolic handle class for things like:

- `Fn`
- `Builtin`
- `Namespace`
- nullary variants

while the hottest VM paths can still dispatch directly on interned symbol ids instead of names.

That means:

- a known top-level function can be passed around as a first-class value
- `CALL_VALUE` can dispatch without a separate closure object model
- the current VM does not need upvalues or captured environments
- the same inline handle model also works for other compile-time-known symbols such as builtins and effect names

This is an internal encoding choice, not a surface-language feature. At the language level, functions are still just Aver functions.

## Opcode Philosophy

The opcode set is deliberately semantic rather than minimal.

Examples:

- `TAIL_CALL_SELF`
- `TAIL_CALL_KNOWN`
- `MATCH_UNWRAP`
- `MATCH_CONS`
- `MATCH_TUPLE`
- `EXTRACT_FIELD`
- `EXTRACT_TUPLE_ITEM`
- `TUPLE_NEW`
- `LIST_LEN`
- `LIST_GET`
- `LIST_APPEND`
- `LIST_PREPEND`
- `LIST_GET_MATCH`

These opcodes exist because Aver already has strong opinions:

- `match` is the only branching construct
- `Result` and `Option` are explicit and common
- recursion and TCO matter more than loop machinery
- records, variants, and tuples are core language shapes

So instead of lowering everything into overly generic bytecode, the VM keeps those concepts visible.

## Match Lowering

Pattern matching is compiled into a short sequence of checks and destructuring steps.

Typical pieces are:

- tag checks (`MATCH_TAG`)
- wrapper checks/unwrapping (`MATCH_UNWRAP`)
- list shape checks (`MATCH_NIL`, `MATCH_CONS`, `LIST_HEAD_TAIL`)
- tuple shape checks (`MATCH_TUPLE`, `EXTRACT_TUPLE_ITEM`)
- variant checks (`MATCH_VARIANT`)
- field extraction (`EXTRACT_FIELD`)

This keeps the execute loop simple while preserving the structure of Aver patterns.

The current VM no longer uses arm-local match-region opcodes. In practice they were adding machinery at the wrong granularity for Aver: most functions are tiny, and the bigger wins came from better list/value placement and more semantic bytecode around common patterns such as `match List.get(xs, i)`.

## Recent Correctness Notes

Two recent fixes are worth calling out because they affected real example programs:

- mutual tail calls with a larger target `local_count` now resize the VM stack before clearing new locals, which removed a crash in large verify suites such as `examples/data/json.av`
- ordered string comparison in the VM now matches the interpreter, so examples like `examples/data/date.av` behave the same under `verify` and `verify --vm`

Those are not design shifts, but they matter because they closed the last obvious gaps between the interpreter semantics and the production VM path.

## Effects And Host Runtime

The VM enforces declared effects at runtime, like the interpreter does.

That logic does not live in the main execute loop. Instead:

- `src/vm/execute.rs` is the core machine
- `src/vm/runtime.rs` is the host/runtime bridge

`VmRuntime` is responsible for:

- builtin dispatch
- effect checking from interned effect ids attached to VM symbols
- record/replay integration
- CLI argument access

This split is intentional: the VM core should mostly be “bytecode mechanics”, while effectful services stay at the boundary.

## Callback Boundaries

`HttpServer.listen` and `HttpServer.listenWith` are special because they need to call back into Aver from host code.

Today that bridge works by:

- converting callback args into VM values
- resolving the callback's inline `symbol_id` back to a VM function
- calling that VM function
- converting the result back into host `Value`

This boundary is more complex than normal builtin calls and is one of the few places where the VM still has explicit host-runtime plumbing.

## Tail Calls

Tail calls are not an afterthought.

The compiler emits:

- `TAIL_CALL_SELF`
- `TAIL_CALL_KNOWN`

So recursive and mutual-recursive tail calls can reuse frames directly in the VM.

That matches the rest of Aver, where recursion is the normal control-flow mechanism instead of loops.

## Current Boundaries

What is still true today:

- the bytecode format is internal and not stable yet
- function values are modeled around top-level Aver functions, which matches the language today
- builtin calls are primarily compiled as direct builtin operations, not passed around as first-class VM values
- some host-service edges, especially callback-heavy ones like `HttpServer`, still need more runtime plumbing than the pure VM core

These are mostly implementation boundaries, not evidence that the VM is “toy” or “partial”. The VM should be thought of as a real runtime path whose internals are still settling.

## Why This Shape Fits Aver

The VM is small partly because Aver itself is narrow and explicit:

- one branching construct
- explicit effects
- no exceptions
- no hidden mutation model
- no closure-heavy execution model

That lets the VM stay simple in the good sense:

- fewer opcodes than a generic language VM
- more semantic opcodes than a minimal stack toy
- a direct correspondence between surface-language constructs and runtime behavior

That is the design goal: not “generic bytecode purity”, but a runtime that matches how Aver already wants programs to look.
