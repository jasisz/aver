# Aver Diagnostics — JSON Schema

Canonical JSON emitted by `aver check --json`, `aver verify --json`,
playground `aver_check` / `aver_verify` / `aver_why` / `aver_context`,
and the LSP (via `tower_lsp` types derived from this model).

Source of truth: `src/diagnostics/model.rs` (serde-derived).

## Bundle shape

Top-level object per analyzed file:

```json
{
  "schema_version": 1,
  "kind": "analysis",
  "file_label": "path/or/playground",
  "diagnostics": [Diagnostic, ...],
  "why_summary":     { ... } | null,
  "context_summary": { ... } | null,
  "verify_summary":  { ... } | null
}
```

`schema_version` bumps on breaking shape changes. Optional trailing
fields (`why_summary`, `context_summary`, `verify_summary`) are
present only when the caller opts in. **Invariant:** at most one
summary field is set per record — each command populates exactly the
summary it computes. `diagnostics` is omitted when empty.

## CLI NDJSON contract

Every multi-record CLI command (`aver check`, `aver verify`,
`aver why`) emits one bundle per analyzed file, one JSON object per
line, followed by a trailing `summary` record with counts. Each line
carries its own `schema_version` so consumers can grep, concat, or
tail streams without losing context.

```
{"schema_version":1,"kind":"analysis","file_label":"a.av",...}
{"schema_version":1,"kind":"analysis","file_label":"b.av",...}
{"schema_version":1,"kind":"summary","files":2,"passed":1,"failed":1}
```

Single-record playground calls (`aver_check`, `aver_verify`,
`aver_why`, `aver_context`) emit one bundle — the same shape as a
single NDJSON line above.

## `aver context --json` outlier

`aver context --json` has its own top-level schema (currently v6,
defined in `src/main/context_format.rs`) because the command emits a
single multi-module document optimized for LLM byte budgets, not a
stream of per-file bundles. Unifying it onto the NDJSON contract is
tracked as a future migration — the selection engine (depth / budget
/ focus / truncation) would need to adapt, and the canonical
`ContextSummary` would need a per-module trim mode.

Playground `aver_context` **does** use the canonical
`AnalysisReport` + `context_summary` shape, since the playground sees
a single file and has no byte budget concern.

## Diagnostic

```json
{
  "severity": "error" | "warning" | "fail" | "hint",
  "slug": "type-mismatch",
  "summary": "Type mismatch: expected Int, got String",
  "span": { "file": "app.av", "line": 12, "col": 5 },
  "fn_name": "bar",        // optional
  "intent": "...",          // optional
  "fields": [["expected", "Int"], ["actual", "String"]], // optional
  "conflict": "...",        // optional
  "repair": {
    "primary": "Change the expression to produce Int",
    "alternatives": [],
    "example": null
  },
  "regions": [AnnotatedRegion, ...],  // source-snippet regions
  "related":  [RelatedSpan, ...]
}
```

- `severity.fail` is reserved for verify / replay failures where a
  contract held but the observed result diverges.
- `severity.hint` is for LSP-only suggestions (e.g. verify hygiene).
- `slug` is the stable identifier; see `docs/slugs/*.md`.
- `fields` is an ordered list of `[key, value]` tuples (not a map) so
  display order is deterministic.

### Region

```json
{
  "source_lines": [{ "line_num": 12, "text": "..." }, ...],
  "underline": { "col": 5, "len": 3, "label": "declared Int" } | null
}
```

### RelatedSpan

```json
{
  "span": { "file": "app.av", "line": 20, "col": 1 },
  "label": "declared here"
}
```

## WhySummary

```json
{
  "file_label": "app.av",
  "total_lines": 120,
  "justified_lines": 72,
  "partial_lines": 30,
  "unjustified_lines": 18,
  "has_module_intent": true,
  "decisions": [{ "name": "...", "date": "...", "reason_prefix": "..." }],
  "functions": [{
    "name": "...",
    "lines": 12,
    "has_description": true,
    "is_effectful": false,
    "verify_cases": 3,
    "has_coverage_gaps": false,
    "has_decision_impact": false,
    "level": "justified" | "partial" | "unjustified",
    "missing": ["no verify", ...]
  }]
}
```

## ContextSummary

Module shape as seen from the entry file. Dependency bodies are **not
expanded** in the summary — `depends` lists names only. Callers that
want the full graph walk `FileContext` trees themselves.

```json
{
  "file_label": "app.av",
  "module_name": "App",
  "intent": "Payment ops entry.",
  "depends": ["Http", "Disk"],
  "exposes": ["run", "shutdown"],
  "exposes_opaque": ["Session"],
  "api_effects": ["Http.get", "Disk.readText"],
  "module_effects": ["Http.get", "Disk.readText", "Console.print"],
  "main_effects": ["Console.print"],
  "functions": [{
    "name": "run",
    "signature": "fn run(cmd: String) -> Result<Report, String> ! [Http.get]",
    "description": "Execute a single payment command.",
    "effects": ["Http.get"],
    "qualifiers": ["PURE", "RECURSIVE", "SAFE_ARGS"],
    "auto_memo": false,
    "auto_tco": false,
    "recursive_callsites": 0,
    "verify_count": 3,
    "verify_samples": ["run(\"ping\") => Result.Ok(...)", ...],
    "is_exposed": true,
    "specs": [],
    "direct_calls": ["Http.get", "format"]
  }],
  "types": [{ "name": "Report", "kind": "product", "fields_or_variants": [...] }],
  "decisions": [{ "name": "...", "date": "...", "reason_prefix": "...", "impacts": [...] }]
}
```

## Versioning rules

1. Adding a new optional field to any struct: **no bump**. Consumers
   must ignore unknown fields.
2. Renaming, removing, or changing the type of any field: **bump
   `schema_version` and document the migration here**.
3. Changing the meaning of a `severity` or `kind` value: **bump**.
4. Slug additions or classification refinements: **no bump** (slug is a
   discriminator, not a schema constraint).

## Schema history

- **v1** — initial canonical shape (commits 1–4 of the diagnostics
  unification). CLI `aver check --json` switched from per-record legacy
  JSON to `AnalysisReport` bundles in the same window.
