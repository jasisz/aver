# independence-hazard

**Severity:** `warning`
**Category:** `effects`

Independent product branches use effects that may conflict if reordered (e.g. shared output).

## When it fires

See `src/diagnostics/classify.rs` for the classifier predicate and
`src/checker/` for the source collector that generates the finding.

## Fix

If the diagnostic carries a `repair` field, follow it. Otherwise
consult the Aver language reference at `docs/language.md`.

## Schema

Emitted as a canonical [`Diagnostic`](../diagnostics-schema.md) with
`slug: "independence-hazard"`. LSP surfaces the slug in the `code` field so
editors can link here.
