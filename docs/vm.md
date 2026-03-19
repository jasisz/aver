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

That matters because most runtime values are represented as one compact machine word:

- small integers can stay inline
- floats use NaN-boxing
- heap-backed values live in the VM arena and are referenced by tagged payloads

Common heap-backed shapes include:

- `List`
- `Tuple`
- `Record`
- `Variant`
- `Map`
- boxed wrapper payloads (`Result.Ok`, `Result.Err`, `Option.Some`)

This is the main reason the VM can stay small without dragging a large object model everywhere.

## Memory Model

The VM does not use a single “grow forever” arena anymore.

It now uses a small runtime model shaped around Aver's execution style:

- each frame records a young-region mark
- temporary heap values allocate into the young arena
- on `RETURN` and `TAIL_CALL_*`, only values that escape through roots are promoted
- promoted survivors move into a stable space instead of being recopied through young regions
- top-level completion compacts stable-space values from live roots

In practice this gives the VM region-style bulk cleanup for short-lived temporaries, without forcing the whole runtime into a full tracing GC model.

This fits Aver unusually well because values are immutable, effects are explicit, and there are relatively few hidden escape paths.

## List Representation

Lists in the VM are not just flat `Vec` payloads.

The current arena list storage supports three shapes:

- `Flat` for compact literal / materialized lists
- `Prepend` for cheap `List.prepend` and `LIST_CONS`
- `Concat` for cheap structural concatenation

This matters because the VM can now keep list construction aligned with Aver semantics instead of flattening on every prepend.

Pattern matching and destructuring (`MATCH_CONS`, `LIST_HEAD_TAIL`) use list helpers that understand these shapes directly.

## Function References

One of the more unusual choices is that **VM function values are encoded as inline `Int(fn_id)`**.

That means:

- a known top-level function can be passed around as a first-class value
- `CALL_VALUE` can dispatch without a separate closure object model
- the current VM does not need upvalues or captured environments

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

## Effects And Host Runtime

The VM enforces declared effects at runtime, like the interpreter does.

That logic does not live in the main execute loop. Instead:

- `src/vm/execute.rs` is the core machine
- `src/vm/runtime.rs` is the host/runtime bridge

`VmRuntime` is responsible for:

- builtin dispatch
- effect checking
- record/replay integration
- CLI argument access

This split is intentional: the VM core should mostly be “bytecode mechanics”, while effectful services stay at the boundary.

## Callback Boundaries

`HttpServer.listen` and `HttpServer.listenWith` are special because they need to call back into Aver from host code.

Today that bridge works by:

- converting callback args into VM values
- calling the target VM function by `fn_id`
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
