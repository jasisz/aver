# unused-effect

**Severity:** `warning`
**Category:** `effects`

An effect is declared in `! [...]` but the function body never actually uses it.

## When it fires

See `src/diagnostics/classify.rs` for the classifier predicate and
`src/checker/` for the source collector that generates the finding.

## Fix

If the diagnostic carries a `repair` field, follow it. Otherwise
consult the Aver language reference at `docs/language.md`.

## Schema

Emitted as a canonical [`Diagnostic`](../diagnostics-schema.md) with
`slug: "unused-effect"`. LSP surfaces the slug in the `code` field so
editors can link here.
