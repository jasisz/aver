# type-mismatch

**Severity:** `error`
**Category:** `type-system`

A value's inferred type doesn't match the context requires.

## When it fires

See `src/diagnostics/classify.rs` for the classifier predicate and
`src/checker/` for the source collector that generates the finding.

## Fix

If the diagnostic carries a `repair` field, follow it. Otherwise
consult the Aver language reference at `docs/language.md`.

## Schema

Emitted as a canonical [`Diagnostic`](../diagnostics-schema.md) with
`slug: "type-mismatch"`. LSP surfaces the slug in the `code` field so
editors can link here.
