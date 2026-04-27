//! Canonical diagnostic data model.
//!
//! Runtime-neutral and wasm-safe — no `colored`, no file IO, no VM.
//! Shared between CLI (`src/main/tty_render.rs`), LSP (`aver-lsp`),
//! and playground (`src/playground.rs`).

use serde::Serialize;

pub const SCHEMA_VERSION: u32 = 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Error,
    Warning,
    Fail,
    Hint,
}

#[derive(Clone, Debug, Serialize)]
pub struct Span {
    pub file: String,
    pub line: usize,
    pub col: usize,
}

#[derive(Clone, Debug, Serialize)]
pub struct SourceLine {
    pub line_num: usize,
    pub text: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct Underline {
    pub col: usize,
    pub len: usize,
    pub label: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct AnnotatedRegion {
    pub source_lines: Vec<SourceLine>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub underline: Option<Underline>,
}

impl AnnotatedRegion {
    pub fn single(source_lines: Vec<SourceLine>, underline: Option<Underline>) -> Vec<Self> {
        vec![Self {
            source_lines,
            underline,
        }]
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct RelatedSpan {
    pub span: Span,
    pub label: String,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct Repair {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub primary: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub alternatives: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub example: Option<String>,
}

impl Repair {
    pub fn primary(text: impl Into<String>) -> Self {
        Self {
            primary: Some(text.into()),
            alternatives: Vec::new(),
            example: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.primary.is_none() && self.alternatives.is_empty() && self.example.is_none()
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct Diagnostic {
    pub severity: Severity,
    pub slug: &'static str,
    pub summary: String,
    pub span: Span,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fn_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub intent: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub fields: Vec<(&'static str, String)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conflict: Option<String>,
    #[serde(skip_serializing_if = "Repair::is_empty")]
    pub repair: Repair,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub regions: Vec<AnnotatedRegion>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub related: Vec<RelatedSpan>,
    /// `true` when this diagnostic was raised by a case `aver verify
    /// --hostile` injected through boundary-value expansion (a binding
    /// the user did not write in the law's `given` clause). Lets tooling
    /// (CI dashboards, IDE filters, LSP code actions) cleanly separate
    /// "the law I already had stopped passing" from "hostile expansion
    /// found a boundary value that breaks it". Default `false`; only
    /// the hostile-aware verify pipeline sets it.
    #[serde(skip_serializing_if = "is_false", default)]
    pub from_hostile: bool,
}

fn is_false(b: &bool) -> bool {
    !*b
}

impl Diagnostic {
    pub fn is_warning(&self) -> bool {
        matches!(self.severity, Severity::Warning)
    }

    pub fn is_error(&self) -> bool {
        matches!(self.severity, Severity::Error | Severity::Fail)
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct AnalysisReport {
    pub schema_version: u32,
    pub kind: &'static str,
    pub file_label: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub diagnostics: Vec<Diagnostic>,
    /// File-local justification summary — present when the caller opts
    /// in via `AnalyzeOptions::include_why_summary`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub why_summary: Option<crate::diagnostics::why::WhySummary>,
    /// File-local context summary (module shape, functions, types,
    /// decisions) — present when `include_context_summary` is set.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context_summary: Option<crate::diagnostics::context::ContextSummary>,
    /// Per-verify-block pass/fail/skip counts — present when
    /// `include_verify_run` is set. Diagnostics list carries the failing
    /// case details; `verify_summary` gives the scorecard.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verify_summary: Option<VerifySummary>,
}

/// Per-block verify results. Mirrors what `aver verify` used to emit as
/// `block-result` NDJSON events — now folded into the analysis bundle
/// so a single record per file carries failures + scorecard.
#[derive(Clone, Debug, Serialize)]
pub struct VerifySummary {
    pub blocks: Vec<VerifyBlockResult>,
}

/// One formatter rule firing on one location. Emitted by
/// `try_format_source` alongside the rewritten text, then folded into a
/// canonical `needs-format` [`Diagnostic`] by the factory.
///
/// `rule` is a stable slug ("bad-operator-spacing", "effect-order",
/// "verify-block-order", …) consumed by LSP `code`, docs, and CI rules.
/// `before`/`after` are optional short snippets for teaching; long
/// rewrites can omit them.
#[derive(Clone, Debug, Serialize)]
pub struct FormatViolation {
    pub line: usize,
    pub col: usize,
    pub rule: &'static str,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub before: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub after: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct VerifyBlockResult {
    pub name: String,
    pub passed: usize,
    pub failed: usize,
    /// Combined skipped count (`when`-driven plus base-fail-driven).
    /// Kept for back-compat with consumers that don't care about the
    /// distinction; the split lives in the two fields below.
    pub skipped: usize,
    pub total: usize,
    /// Cases that originated from the user's declared `given` list (or
    /// the `verify` cases-form values they wrote literally). Always
    /// populated under `--hostile`; under a non-hostile run, equal to
    /// `passed/failed` and `hostile_*` are zero.
    #[serde(skip_serializing_if = "is_zero", default)]
    pub declared_passed: usize,
    #[serde(skip_serializing_if = "is_zero", default)]
    pub declared_failed: usize,
    /// Cases injected by `aver verify --hostile` boundary expansion.
    /// `hostile_failed > 0 && declared_failed == 0` is the canonical
    /// "your law passed on the values you wrote but breaks on the
    /// boundary" signal — encode the missing precondition as `when`,
    /// or drop the `law` form to a `verify` cases-form example.
    #[serde(skip_serializing_if = "is_zero", default)]
    pub hostile_passed: usize,
    #[serde(skip_serializing_if = "is_zero", default)]
    pub hostile_failed: usize,
    /// Cases skipped because the user's `when` predicate evaluated
    /// to false. Subset of `skipped`. When this equals `skipped` and
    /// no hostile profiles ran, the law is vacuously-under-hostile.
    #[serde(skip_serializing_if = "is_zero", default)]
    pub skipped_by_when: usize,
    /// Cases skipped because the same `case_expr` already failed in
    /// its un-effected base world. Aver doesn't run the VM for these
    /// — they would only re-confirm the same counter-example under
    /// harder worlds. Subset of `skipped`.
    #[serde(skip_serializing_if = "is_zero", default)]
    pub skipped_after_base_fail: usize,
}

fn is_zero(n: &usize) -> bool {
    *n == 0
}

impl AnalysisReport {
    pub fn new(file_label: impl Into<String>) -> Self {
        Self {
            schema_version: SCHEMA_VERSION,
            kind: "analysis",
            file_label: file_label.into(),
            diagnostics: Vec::new(),
            why_summary: None,
            context_summary: None,
            verify_summary: None,
        }
    }

    pub fn with_diagnostics(file_label: impl Into<String>, diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            schema_version: SCHEMA_VERSION,
            kind: "analysis",
            file_label: file_label.into(),
            diagnostics,
            why_summary: None,
            context_summary: None,
            verify_summary: None,
        }
    }

    pub fn to_json(&self) -> String {
        serde_json::to_string(self).unwrap_or_else(|_| "{}".to_string())
    }
}

/// Minimal JSON string escaping. Kept as a standalone helper so legacy
/// per-record CLI JSON (bit-for-bit parity with 0.9.x) can build output
/// without pulling serde into every format string.
pub fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}
