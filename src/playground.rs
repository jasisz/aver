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
}
