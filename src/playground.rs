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

    let mut diags: Vec<String> = Vec::new();

    let mut items = match parse_source(source) {
        Ok(items) => items,
        Err(e) => {
            diags.push(format!(
                r#"{{"severity":"error","line":1,"col":0,"message":{}}}"#,
                json_str(&e)
            ));
            return format!("[{}]", diags.join(","));
        }
    };

    tco::transform_program(&mut items);
    let tc_result = run_type_check_full(&items, None);

    for e in &tc_result.errors {
        diags.push(format!(
            r#"{{"severity":"error","line":{},"col":{},"message":{}}}"#,
            e.line, e.col, json_str(&e.message)
        ));
    }

    let findings = check_module_intent_with_sigs_in(&items, Some(&tc_result.fn_sigs), None);
    for w in &findings.warnings {
        diags.push(format!(
            r#"{{"severity":"warning","line":{},"col":0,"message":{}}}"#,
            w.line, json_str(&w.message)
        ));
    }

    if diags.is_empty() {
        diags.push(r#"{"severity":"ok","line":0,"col":0,"message":"All checks passed."}"#.to_string());
    }

    format!("[{}]", diags.join(","))
}

fn json_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c < '\x20' => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
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
