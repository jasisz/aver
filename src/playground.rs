//! Browser-facing entry points for the Aver playground.

use std::collections::HashSet;

use crate::codegen;
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

/// Run check pipeline (typecheck + lint) and return plain-text diagnostics.
pub fn check_source(source: &str) -> String {
    use crate::checker::check_module_intent_with_sigs_in;

    let mut lines: Vec<String> = Vec::new();

    let mut items = match parse_source(source) {
        Ok(items) => items,
        Err(e) => return format!("error: {}", e),
    };

    tco::transform_program(&mut items);
    let tc_result = run_type_check_full(&items, None);

    for e in &tc_result.errors {
        lines.push(format!("error[{}:{}]: {}", e.line, e.col, e.message));
    }

    let findings = check_module_intent_with_sigs_in(&items, Some(&tc_result.fn_sigs), None);
    for w in &findings.warnings {
        lines.push(format!("warning[{}]: {}", w.line, w.message));
    }

    if tc_result.errors.is_empty() && lines.is_empty() {
        lines.push("All checks passed.".to_string());
    } else if tc_result.errors.is_empty() {
        lines.insert(0, "Type check passed.".to_string());
    }

    lines.join("\n")
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
