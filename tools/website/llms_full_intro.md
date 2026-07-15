# Aver — Deep Documentation Index

> Start with [`llms.txt`](https://averlang.dev/llms.txt). This file is the deeper companion: the toolchain in full plus a routing index for language docs, examples, and implementation context.

## Recommended read order

1. [`llms.txt`](https://averlang.dev/llms.txt) for syntax guardrails and the minimal working shape of an Aver file
2. [Language Guide](https://github.com/jasisz/aver/blob/main/docs/language.md) for the full surface-language contract
3. [Services (stdlib)](https://github.com/jasisz/aver/blob/main/docs/services.md) for stdlib and effect namespaces
4. one or two raw `.av` examples close to your task
5. [`AGENTS.md`](https://github.com/jasisz/aver/blob/main/AGENTS.md) only if you need repository internals or implementation details

## Primary docs

- [Language Guide](https://github.com/jasisz/aver/blob/main/docs/language.md) — complete surface-language reference
- [Services (stdlib)](https://github.com/jasisz/aver/blob/main/docs/services.md) — every namespace and its API
- [Common Pushback](https://github.com/jasisz/aver/blob/main/docs/pushback.md) — questions, objections, honest answers

## Advanced topics

- [Independence (`?!`)](https://github.com/jasisz/aver/blob/main/docs/independence.md) — parallel products
- [Constructors](https://github.com/jasisz/aver/blob/main/docs/constructors.md) — constructor routing rules
- [Oracle](https://github.com/jasisz/aver/blob/main/docs/oracle.md) — proof export for classified effectful functions via `verify fn trace` + `given` stubs, plus `--hostile`
- [Lean proof export](https://github.com/jasisz/aver/blob/main/docs/lean.md) — verify blocks to Lean 4
- [Dafny verification](https://github.com/jasisz/aver/blob/main/docs/dafny.md) — verify laws to Dafny / Z3
- [Effect and wasm-gc support](https://github.com/jasisz/aver/blob/main/docs/effects.md) — browser and host surface
- [WASI 0.2 target](https://github.com/jasisz/aver/blob/main/docs/wasip2.md) — Component Model deployment

## Canonical examples

- [Hello](https://github.com/jasisz/aver/blob/main/examples/core/hello.av) — minimal pure file
- [Calculator](https://github.com/jasisz/aver/blob/main/examples/core/calculator.av) — verify on basic arithmetic
- [Independent Fan-out](https://github.com/jasisz/aver/blob/main/examples/core/independent_fanout.av) — `!` / `?!` independent products
- [Quicksort](https://github.com/jasisz/aver/blob/main/examples/data/quicksort.av) — recursion + verify on a recursive algorithm
- [Oracle Trace](https://github.com/jasisz/aver/blob/main/examples/formal/oracle_trace.av) — `verify fn trace` with `given` stubs for classified effects
