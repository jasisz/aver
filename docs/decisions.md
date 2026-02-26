# Aver — Agreed Directions & Deferred Decisions

## Modules vs DI (2026-02-25)

- Keep the language explicit in phase 1: `depends [Examples.Foo]` resolves from an explicit module root (default current working directory, optional `--module-root` override), with no hidden env remapping or parent-directory fallback.
- Treat circular imports as a hard error with an explicit chain (`A -> B -> A`) rather than trying to support partial linking now.
- Keep concerns separate:
  - Module imports (`depends`) answer "where code comes from".
  - Capabilities/services (future effect runtime model) answer "how effects are provided".
- Aver favors self-contained modules: code dependencies are explicit via `depends`, and effect dependencies are explicit via full `! [Effect]` propagation through the call chain.
- If remapping is added later, prefer a versioned project manifest (`aver.toml`) over ad-hoc runtime flags so the mapping is visible to humans and AI agents.
- Service override (for example replacing `Console`) is postponed; if added, it should be explicit, contract-checked, and limited to test/dev profiles first.

## Concurrency shape (2026-02-25)

- Current language model is sequential: no `async`/`await`/promises.
- If concurrency is added, keep effects as capability (`what`) and concurrency as scheduling (`when`); do not overload `! [Effect]` with ordering semantics.
- MVP preference: `par` for homogeneous workloads (same result type), e.g. multiple `Http.get` calls.
- No `Any` fallback for mixed parallel results.
- For mixed-type parallelism, prefer positional fixed products (tuple-like return) rather than heterogeneous lists.
- Explicit `spawn`/`join` API is rejected for Aver (not postponed).

## Verify-first debugging (2026-02-25)

- For logic bugs, default workflow is: reproduce with a failing `verify` case, fix implementation, keep the case as a permanent regression guard.
- Prefer this over ad-hoc print debugging inside core functions; debugging artifacts should become executable specs when possible.
- This is especially AI-friendly: `verify` cases are declarative, reviewable, and reusable across sessions.
- Limits (still real): `verify` alone does not replace profiling, latency analysis, or debugging nondeterministic external systems.
