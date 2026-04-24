//! Canonical diagnostic model, factories, and analysis pipeline.
//!
//! Runtime-neutral and wasm-safe: shared between CLI (`src/main/tty_render`),
//! the playground (`src/playground.rs`), and the LSP server.
//!
//! Entry point: [`analyze_source`] — source in, [`AnalysisReport`] out.
//!
//! Schema: JSON emitted via [`AnalysisReport::to_json`] carries
//! `schema_version: 1`. See `docs/diagnostics-schema.md`.

pub mod analyze;
pub mod classify;
pub mod context;
pub mod factories;
pub mod model;
#[cfg(feature = "runtime")]
pub mod verify_run;
#[cfg(feature = "runtime")]
pub mod vm_verify;
pub mod why;

pub use analyze::{AnalyzeOptions, analyze_source};
pub use factories::{
    from_check_finding, from_type_error, needs_format_diagnostic, replay_effect_error_diagnostic,
    replay_output_mismatch_diagnostic, unused_binding_diagnostic, verify_mismatch_diagnostic,
    verify_runtime_error_diagnostic, verify_unexpected_err_diagnostic,
};
pub use model::{
    AnalysisReport, AnnotatedRegion, Diagnostic, RelatedSpan, Repair, SCHEMA_VERSION, Severity,
    SourceLine, Span, Underline, json_escape,
};
