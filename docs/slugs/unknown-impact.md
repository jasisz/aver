# unknown-impact

**Severity:** `warning`
**Category:** `decisions`

A `decision` lists an impact symbol that doesn't resolve to any function or type in scope.

## When it fires

See `src/diagnostics/classify.rs` for the classifier predicate and
`src/checker/` for the source collector that generates the finding.

## Fix

If the diagnostic carries a `repair` field, follow it. Otherwise
consult the Aver language reference at `docs/language.md`.

## Schema

Emitted as a canonical [`Diagnostic`](../diagnostics-schema.md) with
`slug: "unknown-impact"`. LSP surfaces the slug in the `code` field so
editors can link here.
