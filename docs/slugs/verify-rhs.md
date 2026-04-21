# verify-rhs

**Severity:** `error`
**Category:** `verify`

A verify case calls the target function on the right of `=>`. Right side must be the expected value, not another call.

## When it fires

See `src/diagnostics/classify.rs` for the classifier predicate and
`src/checker/` for the source collector that generates the finding.

## Fix

If the diagnostic carries a `repair` field, follow it. Otherwise
consult the Aver language reference at `docs/language.md`.

## Schema

Emitted as a canonical [`Diagnostic`](../diagnostics-schema.md) with
`slug: "verify-rhs"`. LSP surfaces the slug in the `code` field so
editors can link here.
