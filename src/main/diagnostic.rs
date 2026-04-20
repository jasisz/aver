//! Thin compatibility shim over `aver::diagnostics`.
//!
//! Historical users in `commands.rs` / `replay_cmd.rs` reach factories,
//! types, and `json_escape` through `super::diagnostic::*`. The canonical
//! home for all of them is `aver::diagnostics`; this file is intentionally
//! minimal and will be removed once every caller imports the canonical path
//! directly.

#![allow(unused_imports, deprecated)]

pub(super) use aver::diagnostics::{
    AnalysisReport, AnalyzeOptions, AnnotatedRegion, Diagnostic, RelatedSpan, Repair,
    SCHEMA_VERSION, Severity, SourceLine, Span, Underline, analyze_source,
    effect_violation_diagnostic, from_check_finding, from_type_error, json_escape,
    missing_verify_diagnostic, replay_effect_error_diagnostic,
    replay_output_mismatch_diagnostic, unused_binding_diagnostic,
    verify_mismatch_diagnostic, verify_runtime_error_diagnostic,
    verify_unexpected_err_diagnostic,
};
