# Aver Diagnostics — JSON Schema

This is the JSON emitted by `aver check --json`, `aver verify --json`, `aver audit --json`, `aver why --json`, `aver context --json`, `aver format --check --json`, playground `aver_check` / `aver_verify` / `aver_why` / `aver_context` / `aver_audit` (plus their `_project` multi-file variants), and the LSP (via `tower_lsp` types derived from this model).

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

`schema_version` goes up on breaking shape changes. The optional trailing fields (`why_summary`, `context_summary`, `verify_summary`) are present only when the caller opts in. **At most one summary field is set per record: each command fills in exactly the summary it computes.** `diagnostics` is omitted when empty.

## CLI NDJSON contract

Every multi-record CLI command (`aver check`, `aver verify`, `aver audit`, `aver why`) emits one bundle per analyzed file, one JSON object per line, and then a final `summary` record with counts. Each line carries its own `schema_version`, so consumers can grep, concat or tail streams without losing context.

```
{"schema_version":1,"kind":"analysis","file_label":"a.av",...}
{"schema_version":1,"kind":"analysis","file_label":"b.av",...}
{"schema_version":1,"kind":"summary","files":1,"modules":2,"passed":1,"failed":1}
```

`aver check` and `aver verify` walk the whole program named by each input file (the entry plus everything it reaches through `depends [...]`), so they emit one bundle per module, leaves first. In their summaries `files` counts the inputs the command was pointed at, and `modules` counts the modules reported. For `check` that is every module checked (`passed` and `failed` count modules). For `verify` it is every module that declared at least one verify block (`blocks`, `cases_passed`, `cases_failed` sum over them). `verify` also reports `files_skipped`, the modules it could not check at all, and `blocks_unchecked`, the verify blocks those modules declare that nobody ran. Both are `0` on a run that checked everything. A non-zero `files_skipped` is how you tell "nothing was verified" apart from "nothing to verify". `aver audit` walks the program the same way: it audits every module it reaches once and counts it in `modules`, and `audit.check_errors`, `audit.verify_failures` and `audit.format_needed` sum over those modules.

Single-record playground calls (`aver_check`, `aver_verify`, `aver_why`, `aver_context`) emit one bundle, with the same shape as a single NDJSON line above.

## `aver context --json` outlier

`aver context --json` has its own top-level schema (currently v6, defined in `src/main/context_format.rs`) because the command emits a single multi-module document sized for LLM byte budgets instead of a stream of per-file bundles. Moving it onto the NDJSON contract is tracked as a future migration. The selection engine (depth / budget / focus / truncation) would need to adapt, and the canonical `ContextSummary` would need a per-module trim mode.

Playground `aver_context` does use the canonical `AnalysisReport` + `context_summary` shape. The playground sees a single file and has no byte budget to worry about.

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

- `severity.fail` is only for verify / replay failures where a contract held but the observed result diverges.
- `severity.hint` is for LSP-only suggestions (e.g. verify hygiene).
- `slug` is the stable identifier. See [`docs/diagnostics-slugs.md`](./diagnostics-slugs.md) for the full list grouped by category.
- `fields` is an ordered list of `[key, value]` tuples, so display order is deterministic. It is not a map.

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

The module's shape as seen from the entry file. The summary does not expand dependency bodies: `depends` lists names only. Callers that want the full graph walk `FileContext` trees themselves.

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
    "signature": "fn run(cmd: String) -> Result<Report, String>",
    "description": "Execute a single payment command.",
    "effects": ["Http.get"],
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

`signature` carries params + return type only. Effects are in the sibling `effects` array, so renderers can show them next to the signature without printing them twice.

## Versioning rules

1. Adding a new optional field to any struct: no bump. Consumers must ignore unknown fields.
2. Renaming, removing, or changing the type of any field: bump `schema_version` and document the migration here.
3. Changing the meaning of a `severity` or `kind` value: bump.
4. Slug additions or classification refinements: no bump. The slug is a discriminator; the schema does not constrain its values.

## Schema history

- **v1**: the first canonical shape (commits 1–4 of the diagnostics unification). CLI `aver check --json` switched from per-record legacy JSON to `AnalysisReport` bundles at the same time.
