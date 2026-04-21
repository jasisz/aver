//! Browser-facing entry points for the Aver playground.

use std::collections::HashSet;

use crate::codegen;
use crate::diagnostics::{AnalyzeOptions, analyze_source};
use crate::source::parse_source;
use crate::tco;
use crate::types::checker::run_type_check_full;

/// Compile Aver source text to WASM bytes.
pub fn compile_to_wasm(source: &str) -> Result<Vec<u8>, String> {
    let mut items = parse_source(source)?;
    tco::transform_program(&mut items);

    let tc_result = run_type_check_full(&items, None);
    if !tc_result.errors.is_empty() {
        let msgs: Vec<String> = tc_result
            .errors
            .iter()
            .map(|e| format!("error[{}:{}]: {}", e.line, e.col, e.message))
            .collect();
        return Err(msgs.join("\n"));
    }

    let ctx = codegen::build_context(
        items,
        &tc_result,
        HashSet::new(),
        "playground".to_string(),
        vec![],
    );
    codegen::wasm::emit_wasm(&ctx)
}

/// Run the single-file analysis pipeline and return the canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) as JSON. Consumers
/// should parse the `diagnostics` array; an empty array means the file
/// passed every enabled check.
pub fn check_source(source: &str) -> String {
    let opts = AnalyzeOptions::new("playground");
    analyze_source(source, &opts).to_json()
}

/// Run analysis plus verify block execution and return the canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) as JSON. Verify
/// runs only when the source is typecheck-clean; callers see the same
/// mismatch/runtime-error diagnostics as `aver verify`.
pub fn verify_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_verify_run = true;
    analyze_source(source, &opts).to_json()
}

/// Run analysis plus the file-local "why" summary (per-function
/// justification signals) and return the canonical report as JSON.
pub fn why_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_why_summary = true;
    analyze_source(source, &opts).to_json()
}

/// Run analysis plus the file-local context summary (module shape,
/// functions, types, decisions) and return the canonical report as
/// JSON. Dependency bodies are not expanded — the playground sees the
/// entry file only; `depends` carries names for UI.
pub fn context_source(source: &str) -> String {
    let mut opts = AnalyzeOptions::new("playground");
    opts.include_context_summary = true;
    analyze_source(source, &opts).to_json()
}

/// Audit: three-axis health check — static analysis (every enabled
/// collector), verify block execution, and format-check. Equivalent of
/// the CLI `aver audit` but single-file. Returns a canonical
/// [`AnalysisReport`](crate::diagnostics::AnalysisReport) bundle with
/// diagnostics + verify_summary.
#[cfg(feature = "runtime")]
pub fn audit_source(source: &str) -> String {
    use crate::diagnostics::needs_format_diagnostic;

    let mut opts = AnalyzeOptions::new("playground");
    opts.include_verify_run = true;
    let mut report = analyze_source(source, &opts);

    // Dodaj format-check jako diagnostic w bundle (parity z CLI audit).
    #[cfg(feature = "tty-render")]
    if let Ok((formatted, violations)) = crate::format::try_format_source(source)
        && formatted != source
    {
        report.diagnostics.push(needs_format_diagnostic(
            "playground",
            &violations,
            source,
        ));
    }

    report.to_json()
}

/// Format the source and return the rewritten text. Non-mutating by
/// itself — caller (JS) replaces editor contents. Returns the original
/// source unchanged on parse error.
#[cfg(feature = "tty-render")]
pub fn format_source(source: &str) -> String {
    crate::format::try_format_source(source)
        .map(|(text, _violations)| text)
        .unwrap_or_else(|_| source.to_string())
}

#[cfg(feature = "playground")]
mod bindgen {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    pub fn aver_compile(source: &str) -> Result<Vec<u8>, JsError> {
        super::compile_to_wasm(source).map_err(|e| JsError::new(&e))
    }

    #[wasm_bindgen]
    pub fn aver_check(source: &str) -> String {
        super::check_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_verify(source: &str) -> String {
        super::verify_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_why(source: &str) -> String {
        super::why_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_context(source: &str) -> String {
        super::context_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_audit(source: &str) -> String {
        super::audit_source(source)
    }

    #[wasm_bindgen]
    pub fn aver_format(source: &str) -> String {
        super::format_source(source)
    }
}
