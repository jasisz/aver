# verify-unexpected-err

**Severity:** `fail`
**Category:** `verify`

A verify case propagated a `Result.Err` via `?` that the case didn't account for.

## When it fires

See `src/diagnostics/classify.rs` for the classifier predicate and
`src/checker/` for the source collector that generates the finding.

## Fix

If the diagnostic carries a `repair` field, follow it. Otherwise
consult the Aver language reference at `docs/language.md`.

## Schema

Emitted as a canonical [`Diagnostic`](../diagnostics-schema.md) with
`slug: "verify-unexpected-err"`. LSP surfaces the slug in the `code` field so
editors can link here.
