# Aver VM

This document describes the bytecode virtual machine used by `aver run`, `aver verify`, and `aver replay`.

It is a design note. The opcode set and the internal representation may still change while the VM matures, so treat nothing here as a frozen spec.

## What It Is

The Aver VM is the only execution backend for Aver programs.

Its design follows the language rather than a generic IR. Opcodes model Aver concepts directly. Pattern matching compiles to explicit match and destructure instructions. Tail calls are part of the ISA. Records, variants, wrappers, lists and tuples are ordinary runtime values.

It is a runtime built around Aver's constraints, with no ambition to be a mini-JVM or a universal IR.

## Execution Model

The VM compiles the resolved Aver AST into bytecode function chunks:

- `src/vm/compiler/` lowers the program to bytecode (`mir.rs` walks MIR and emits opcodes; MIR is the only VM codegen path)
- `src/vm/execute/` runs the stack machine (`dispatch.rs` is the opcode loop)
- `src/vm/opcode.rs` defines the ISA
- `src/vm/runtime.rs` handles builtins, effects, and record/replay at the host boundary

Execution is stack-based. Locals live in the current frame, operands are pushed onto the VM stack, calls create or reuse frames, and a return leaves one value on the caller's stack.

The VM also marks **thin functions** and **parent-thin functions**, using a conservative classifier.

A thin function is a small helper that does not use tail-call frame reuse, does not write globals, and does not emit obvious aggregate-construction opcodes such as `RECORD_UPDATE`, `WRAP`, `LIST_*`, `TUPLE_NEW`, or `VARIANT_NEW`.

When a thin function returns and the runtime can confirm that its local `young` / `yard` / `handoff` marks never moved, the VM skips the normal boundary relocation path. Many tiny Aver helpers therefore keep normal stack locals while running and pay no survivor/stable bookkeeping on return. The exception is a helper that did create local heap state after all.

`parent-thin` is narrower and more specific to Aver. It is meant for wrapper-like helpers, not small functions in general. Such a helper borrows the caller's `young` lane directly and avoids ordinary-return `handoff` as long as it never touches `yard` / `handoff`. Its local `young` scratch dies later, at the caller's boundary, so the helper needs no relocation step of its own.

The classifier is less strict than a "single expression only" rule. Small `match` helpers with local bindings can still be `parent-thin`. Field and tuple extraction and other tiny control-flow opcodes are allowed. Nullary variant constructors (`Status.Todo`) count as inline results, so they can stay on the thin / parent-thin fast path. List destructuring and builtins that obviously build aggregates keep a function out of `parent-thin`.

Every command that executes code runs through the VM:

```bash
aver run app.av
aver verify app.av
aver replay recordings/
```

## Value Representation

The VM runs on `NanValue`. It does not use the higher-level `Value` enum.

In the current layout the semantic tag comes first and the storage choice second.

Floats use the plain IEEE path. Everything else is a tagged quiet NaN:

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

In `v2`, `bit45` mostly answers one question: **does this value carry an arena reference?**

- `Int`: inline int vs arena big-int
- `String`: inline small string vs arena string
- `Some` / `Ok` / `Err`: inline payload vs boxed arena payload
- `List` / `Map`: empty singleton vs arena aggregate
- `Tuple` / `Record` / `Variant`: always arena-backed

This makes the representation much more regular than the older scheme, which leaned heavily on wrappers.

### Inline Cases That Matter

`v2` is compact, and it also keeps the common Aver shapes cheap:

- `Bool`, `Unit`, and `None` are pure inline singletons
- `Some(true)`, `Ok(Unit)`, `Err(None)` stay inline
- `Some(42)`, `Ok(-7)`, `Err(0)` stay inline as long as the int fits the wrapper-inline range
- `[]` and `{}` are real values under their normal collection tags, not exceptions hidden in `Immediate`
- strings up to 5 UTF-8 bytes stay inline under `TAG_STRING`
- nullary variants such as `Status.Todo` or `Color.Red` travel as `Symbol` handles instead of arena entries

So `Result` / `Option` pipelines, empty collections and code heavy on short strings do not create arena churn only to move tiny values around.

### What Still Goes To The Arena

The real aggregate payloads live in the arena:

- large `Int`
- long `String`
- non-empty `List`
- non-empty `Map`
- `Tuple`
- `Record`
- payload-carrying `Variant`
- boxed wrapper payloads when `Some` / `Ok` / `Err` cannot stay inline

This is the main reason the VM can stay small without carrying a bigger object model through every helper call.

## Memory Model

The VM no longer uses a single arena that grows forever.

Heap-backed values are split into four runtime spaces:

- `young` for short-lived temporaries created while evaluating the current step
- `yard` as a tail-position construction lane
- `handoff` as an ordinary-return construction lane
- `stable` as the canonical long-lived space

Each call frame records marks for the local `young`, `yard`, and `handoff` suffixes it owns. The VM therefore knows exactly which heap entries were created during this frame and can reclaim them in bulk.

### What Those Spaces Mean Today

Conceptually, `young` is local scratch work. `yard` holds a value being built for a tail-call path, and `handoff` a value being built for an ordinary return. `stable` holds values that are safe to keep past the current frame boundary.

In the implementation, boundary behavior depends on the shape of the control flow:

- values can still be *allocated* into `yard` or `handoff` in obvious tail/return positions
- at `TAIL_CALL_*` boundaries, live roots are kept in `yard`, so loop-carried state stays out of `stable`
- at ordinary `RETURN` boundaries to another Aver frame, live roots stay on the handoff path and are not forced into `stable`
- parent-thin wrappers are the exception: they borrow the caller's `young` and skip ordinary-return handoff entirely unless they spill into `yard` / `handoff`
- pure-`handoff`, pure-`young`, and single-result mixed helper returns use fast ordinary-return paths
- larger mixed `young + handoff` graphs fall back to full evacuation, because correctness matters more than clever survivor handling
- only globals, host-facing escapes, and top-level completion are canonicalized into `stable`
- after that, the frame-local `young` / `yard` / `handoff` suffixes are truncated or compacted as appropriate

This gives the VM a real survivor lane for TCO-heavy programs and for ordinary chains of helpers. A value that only has to survive one more call boundary does not have to pass through `stable`.

In summary, the current VM uses regions for local scratch memory, the yard for tail-call survivors, and handoff for ordinary helper returns (with a conservative fallback for larger mixed graphs). Stable space is for globals, host-facing escapes and top-level canonicalization. The VM is explicit about which lane each construction uses.

The most important property follows from that: frame-local garbage dies in bulk, and long-lived values no longer sit in temporary memory.

### Memory Flow

The simplest way to follow a value through the VM:

1. New local work starts in `young`.
2. In obvious tail-position construction, aggregates may be built in `yard`.
3. In obvious ordinary-return construction, aggregates may be built in `handoff`.
4. On `TAIL_CALL_*`, live roots are evacuated into `yard`.
5. On ordinary `RETURN` to another Aver frame, live roots stay on the handoff path.
6. On top-level completion or real escape boundaries, live roots are canonicalized into `stable`.
7. The frame-local `young` / `yard` / `handoff` suffixes are then truncated in one shot.

Helper-sized functions get two more fast paths:

8. If a frame returns with unchanged local marks, the VM skips boundary promotion/truncation work for that frame and resumes the caller directly.
9. If a `parent-thin` frame only touched borrowed `young`, it returns directly to the caller without building ordinary-return handoff state at all.

The VM keeps four cases apart: local scratch work, tail-position construction, construction of a value returned to the caller, and values that really live long.

What separates them is how long a value has to survive. `yard` survives the next tail-call boundary. `handoff` survives the next ordinary call/return boundary. Borrowed parent-`young` is the cheapest path, but only very narrow wrapper-like helpers can use it. `stable` is for values that outlive the current Aver call chain.

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

This helps speed, and it also lets the runtime tell "temporary while computing" apart from "safe to keep after this frame ends".

### Why There Is Still No Full GC Loop

The VM does not need a classical collector running everywhere. `young`, `yard`, and `handoff` are reclaimed by explicit truncation at boundaries. `stable` is compacted from live roots at top-level completion or at explicit escape boundaries.

There is still tracing and relocation, but no single global collector that is always on. Most memory dies because the control flow says it can die. Only `stable` needs long-lived maintenance driven by roots.

## List Representation

VM lists are more than flat `Vec` payloads. Arena list storage supports four shapes:

- `Flat` for compact literal / materialized lists
- `Prepend` for cheap `List.prepend` and `LIST_CONS`
- `Concat` for cheap structural concatenation
- `Segments` for concat-tail views produced by repeated destructuring

Repeated `List.append` does not build an ever-longer chain of one-element concats. The VM grows the right edge in flat chunks, so append-heavy code stays structural and indexed access does not degrade into a long tree walk.

As a result, list construction follows Aver semantics and the VM does not flatten on every prepend.

Pattern matching and destructuring (`MATCH_CONS`, `LIST_HEAD_TAIL`) use list helpers that understand these shapes directly. Destructuring a `Concat` tail, for example, no longer rebuilds a fresh concat suffix on every step. It can carry a cheap segment view instead.

The core list operations have their own bytecode paths:

- `LIST_LEN`
- `LIST_GET`
- `LIST_APPEND`
- `LIST_PREPEND`
- `LIST_GET_MATCH`

The most common list operations in real Aver programs therefore skip the generic builtin-dispatch overhead.

In obvious tail-call positions, the VM can allocate new aggregate values directly into the frame yard, which saves a young-to-yard copy on the next `TAIL_CALL_*`.

In obvious ordinary return positions, the VM can allocate new aggregate values directly into the frame's handoff lane. A helper's return value then survives into the caller without first being treated as a temporary or as a globally stable value.

## Symbol Table

The VM keeps one interned table of **names known at compile time**:

- function names
- builtin/service members
- declared effect names
- type / field / variant names discovered while compiling

Each entry gets a stable `symbol_id`.

Hot paths therefore carry no string-based dispatch state. Function values travel as inline `Int(symbol_id)`. `CALL_VALUE` resolves `symbol_id -> function`. `CALL_BUILTIN` carries `symbol_id` instead of a builtin name or an arena string. Builtin effect checks compare interned effect ids instead of runtime strings.

The scheme is simple on purpose: one symbol table, one inline handle format, and metadata attached to each symbol entry.

Not every runtime value is a symbol, and user data stays plain data. But nothing the compiler already knows by name needs string dispatch during execution.

## Function References

The VM keeps two things apart:

- **runtime symbolic values** in `NanValue` via `TAG_SYMBOL`
- **VM-known callable ids** in bytecode and call dispatch via inline `symbol_id`

The runtime has one shared class of symbolic handles for `Fn`, `Builtin`, `Namespace` and nullary variants. The hottest VM paths still dispatch directly on interned symbol ids instead of names.

A known top-level function can be passed around as a value. `CALL_VALUE` dispatches it without a separate closure object model, and the current VM needs no upvalues or captured environments. The same inline handle model works for other names known at compile time, such as builtins and effect names.

This is an internal encoding choice and has no surface-language counterpart. In the language, functions are ordinary Aver functions.

## Opcode Philosophy

The opcode set is semantic by design. Keeping it minimal is not a goal.

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

These opcodes exist because Aver already fixes a few things. `match` is the only branching construct. `Result` and `Option` are explicit and common. Recursion and TCO matter more than loop machinery. Records, variants and tuples are core language shapes.

The VM keeps those concepts visible in the bytecode instead of lowering everything into very generic instructions.

## Match Lowering

Pattern matching compiles into a short sequence of checks and destructuring steps. The usual pieces:

- tag checks (`MATCH_TAG`)
- wrapper checks/unwrapping (`MATCH_UNWRAP`)
- list shape checks (`MATCH_NIL`, `MATCH_CONS`, `LIST_HEAD_TAIL`)
- tuple shape checks (`MATCH_TUPLE`, `EXTRACT_TUPLE_ITEM`)
- variant checks (`MATCH_VARIANT`)
- field extraction (`EXTRACT_FIELD`)

The execute loop stays simple and the structure of Aver patterns is preserved.

The current VM no longer has arm-local match-region opcodes. They added machinery at the wrong granularity for Aver, where most functions are tiny. The bigger gains came from better placement of lists and values and from more semantic bytecode around common patterns such as `match List.get(xs, i)`.

## Recent Correctness Notes

Two recent fixes affected real example programs:

- mutual tail calls with a larger target `local_count` now resize the VM stack before clearing new locals, which removed a crash in large verify suites such as `examples/data/json.av`
- ordered string comparison was corrected, so examples like `examples/data/date.av` behave correctly under `verify`

Neither changed the design. They closed the last obvious correctness gaps in the VM path.

## Effects And Host Runtime

The VM enforces declared effects at runtime.

That logic lives outside the main execute loop:

- `src/vm/execute/` is the core machine (`dispatch.rs` holds the opcode loop)
- `src/vm/runtime.rs` is the host/runtime bridge

`VmRuntime` is responsible for:

- builtin dispatch
- effect checking from interned effect ids attached to VM symbols
- record/replay integration
- CLI argument access

The split is deliberate. The VM core should be mostly bytecode mechanics, and effectful services stay at the boundary.

## Higher-order calls

Named top-level functions can be passed directly to an ordinary Aver helper. The VM invokes them through its normal `CALL_VALUE` path, and there is no host-to-guest callback bridge. `HttpServer`, for example, runs entirely in Aver and calls its request handler like any other function value.

## Tail Calls

Tail calls were designed in from the start. The compiler emits:

- `TAIL_CALL_SELF`
- `TAIL_CALL_KNOWN`

Recursive and mutually recursive tail calls can therefore reuse frames directly in the VM.

This matches the rest of Aver, where recursion is the normal control-flow mechanism and there are no loops.

## Current Boundaries

What holds today:

- the bytecode format is internal and not stable yet
- function values are modeled around top-level Aver functions, which matches the language today
- builtin calls are mostly compiled as direct builtin operations and are not passed around as VM values

These are implementation boundaries. They do not make the VM a toy or a partial runtime. It is a real execution path whose internals are still settling.

## Why This Shape Fits Aver

The VM is small partly because Aver is narrow and explicit. It has one branching construct and explicit effects. It has no exceptions, no hidden mutation model and no execution model built on closures.

So the VM can stay simple. It has fewer opcodes than a generic language VM and more semantic opcodes than a minimal stack machine, and surface-language constructs map directly onto runtime behavior.

The design goal is a runtime that matches how Aver programs already look. Generic bytecode purity is not a goal.
