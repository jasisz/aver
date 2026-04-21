# missing-description

**Severity:** `warning`
**Category:** `docs`

A function lacks a `? "..."` description. Descriptions teach intent to readers and LLMs.

## When it fires

See `src/diagnostics/classify.rs` for the classifier predicate and
`src/checker/` for the source collector that generates the finding.

## Fix

If the diagnostic carries a `repair` field, follow it. Otherwise
consult the Aver language reference at `docs/language.md`.

## Schema

Emitted as a canonical [`Diagnostic`](../diagnostics-schema.md) with
`slug: "missing-description"`. LSP surfaces the slug in the `code` field so
editors can link here.
