# verify-runtime-error

**Severity:** `fail`
**Category:** `verify`

A verify case crashed during evaluation (division by zero, pattern match failure, etc.).

## When it fires

See `src/diagnostics/classify.rs` for the classifier predicate and
`src/checker/` for the source collector that generates the finding.

## Fix

If the diagnostic carries a `repair` field, follow it. Otherwise
consult the Aver language reference at `docs/language.md`.

## Schema

Emitted as a canonical [`Diagnostic`](../diagnostics-schema.md) with
`slug: "verify-runtime-error"`. LSP surfaces the slug in the `code` field so
editors can link here.
