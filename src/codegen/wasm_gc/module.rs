//! Top-level wasm module assembly.
//!
//! Walks the post-pipeline IR, emits a `(type ...)` per fn signature,
//! a `(func ...)` per Aver fn, and exports `_start` (calling `main`)
//! plus `main` itself. Validation runs `wasmparser` with GC + tail-call
//! features enabled before returning bytes — encoder bugs surface
//! immediately.

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};

use super::WasmGcError;
use super::body::{FnEntry, FnMap, emit_fn_body};
use super::types::{param_types, return_results};

use crate::ast::{FnDef, TopLevel};

pub(super) fn emit_module(items: &[TopLevel]) -> Result<Vec<u8>, WasmGcError> {
    let fn_defs: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();

    if fn_defs.is_empty() {
        return Err(WasmGcError::Validation(
            "module has no fn definitions".into(),
        ));
    }
    let main_idx = fn_defs
        .iter()
        .position(|fd| fd.name == "main")
        .ok_or_else(|| WasmGcError::Validation("module has no `main` fn".into()))?;

    let mut module = Module::new();

    // ── Type section ───────────────────────────────────────────────
    // One function type per fn — phase 2 has no type-sharing pass,
    // monomorphic-by-name is fine. `_start` gets type 0 (() -> ()).
    let mut types = TypeSection::new();
    types.ty().function([], []); // type 0: _start
    let mut fn_type_indices: Vec<u32> = Vec::with_capacity(fn_defs.len());
    for fd in &fn_defs {
        let params = param_types(&fd.params)?;
        let results = return_results(&fd.return_type)?;
        let idx = (fn_type_indices.len() as u32) + 1; // +1 for the _start type at 0
        types.ty().function(params, results);
        fn_type_indices.push(idx);
    }
    module.section(&types);

    // ── Function section ───────────────────────────────────────────
    // _start at index 0, then user fns in declaration order.
    let mut funcs = FunctionSection::new();
    funcs.function(0); // _start uses type 0
    for type_idx in &fn_type_indices {
        funcs.function(*type_idx);
    }
    module.section(&funcs);

    // Build the fn-name → wasm-fn-idx map for `body.rs::emit_fn_body`.
    // _start sits at wasm fn idx 0; user fns start at 1.
    let mut by_name: HashMap<String, FnEntry> = HashMap::new();
    for (i, fd) in fn_defs.iter().enumerate() {
        by_name.insert(
            fd.name.clone(),
            FnEntry {
                wasm_idx: (i as u32) + 1,
                return_type: fd.return_type.clone(),
            },
        );
    }
    let fn_map = FnMap { by_name };

    // ── Export section ─────────────────────────────────────────────
    let mut exports = ExportSection::new();
    exports.export("_start", ExportKind::Func, 0);
    for (i, fd) in fn_defs.iter().enumerate() {
        let wasm_idx = (i as u32) + 1;
        exports.export(&fd.name, ExportKind::Func, wasm_idx);
    }
    module.section(&exports);

    // ── Code section ───────────────────────────────────────────────
    let mut codes = CodeSection::new();

    // _start: call main, drop result if main returns a value.
    let main_idx_wasm = (main_idx as u32) + 1;
    let main_returns_value = !fn_defs[main_idx].return_type.trim().eq("Unit");
    let mut start = Function::new([]);
    start.instruction(&Instruction::Call(main_idx_wasm));
    if main_returns_value {
        start.instruction(&Instruction::Drop);
    }
    start.instruction(&Instruction::End);
    codes.function(&start);

    // User fns. `emit_fn_body` walks the Aver body and pushes wasm
    // instructions; it returns the list of extra locals (beyond
    // params) so we can build the wasm `Function` with the right
    // local count. We do a two-step: pre-collect locals first via
    // a dry run, then re-emit. Cleaner than threading partial state
    // back from `emit_fn_body`.
    for (i, fd) in fn_defs.iter().enumerate() {
        let self_wasm_idx = (i as u32) + 1;

        // Dry run: emit into a throwaway fn just to discover what
        // extra locals the body needs. Cheaper than threading a
        // pre-pass through every emit fn.
        let mut probe = Function::new([]);
        let extra_locals_dry = emit_fn_body(&mut probe, fd, &fn_map, self_wasm_idx)?;

        let local_groups: Vec<(u32, ValType)> = extra_locals_dry.iter().map(|v| (1, *v)).collect();
        let mut func = Function::new(local_groups);
        let _ = emit_fn_body(&mut func, fd, &fn_map, self_wasm_idx)?;
        codes.function(&func);
    }

    module.section(&codes);

    let bytes = module.finish();
    if let Err(e) = validate(&bytes) {
        // Dump invalid bytes to /tmp for inspection — `wasm-tools print`
        // can show what the encoder produced even when validation
        // refused it.
        let _ = std::fs::write("/tmp/aver_wasm_gc_invalid.wasm", &bytes);
        return Err(e);
    }
    Ok(bytes)
}

/// Validate emitted bytes with `wasmparser` configured for the wasm-gc
/// + tail-call feature set we target. Catches encoder bugs early and
/// pins the assumed feature surface.
fn validate(bytes: &[u8]) -> Result<(), WasmGcError> {
    use wasmparser::{Validator, WasmFeatures};

    let features = WasmFeatures::default()
        | WasmFeatures::GC
        | WasmFeatures::REFERENCE_TYPES
        | WasmFeatures::FUNCTION_REFERENCES
        | WasmFeatures::TAIL_CALL;
    let mut validator = Validator::new_with_features(features);
    validator
        .validate_all(bytes)
        .map_err(|e| WasmGcError::Validation(format!("{e}")))?;
    Ok(())
}
