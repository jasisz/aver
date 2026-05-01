//! Top-level wasm module assembly.
//!
//! Walks post-pipeline IR, assembles a wasm-gc module:
//!
//! 1. **Type section**, two layers in order:
//!    - User-type slots (records, variant constructors) — assigned by
//!      `TypeRegistry::build` so emit sites already know their indices.
//!    - Function types — one per Aver fn, plus type-0 reserved for
//!      `_start: () -> ()`.
//! 2. **Function section** — one entry per Aver fn referencing the
//!    function-type idx assigned in step 1.
//! 3. **Export section** — `_start` (always at fn idx 0) plus every
//!    user fn by name.
//! 4. **Code section** — `_start` calls `main` and drops any return
//!    value; user fns get their bodies from `body::emit_fn_body`.
//!
//! Validation runs `wasmparser` with GC + tail-call features before
//! returning bytes.

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};

use super::WasmGcError;
use super::body::{FnEntry, FnMap, emit_fn_body};
use super::types::{TypeRegistry, param_types, record_struct_type, return_results};

use crate::ast::{FnDef, TopLevel, TypeDef};

pub(super) fn emit_module(items: &[TopLevel]) -> Result<Vec<u8>, WasmGcError> {
    let registry = TypeRegistry::build(items);

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
    let mut types = TypeSection::new();

    // 1) User types in `TypeRegistry` order. Indices match what the
    //    registry recorded so emit sites can reference them directly.
    emit_user_types(&mut types, items, &registry)?;

    // 2) `_start` type — () -> ().
    types.ty().function([], []);
    let start_type_idx = registry.user_type_count;

    // 3) One fn type per user fn. `fn_type_indices[i]` is the wasm
    //    type idx for the i-th user fn (in declaration order).
    let mut fn_type_indices: Vec<u32> = Vec::with_capacity(fn_defs.len());
    for fd in &fn_defs {
        let params = param_types(&fd.params, Some(&registry))?;
        let results = return_results(&fd.return_type, Some(&registry))?;
        let idx = start_type_idx + 1 + (fn_type_indices.len() as u32);
        types.ty().function(params, results);
        fn_type_indices.push(idx);
    }
    module.section(&types);

    // ── Function section ───────────────────────────────────────────
    let mut funcs = FunctionSection::new();
    funcs.function(start_type_idx); // _start at wasm fn idx 0
    for type_idx in &fn_type_indices {
        funcs.function(*type_idx);
    }
    module.section(&funcs);

    // Build the fn-name → wasm-fn-idx map. _start at idx 0; user fns
    // start at 1.
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

    for (i, fd) in fn_defs.iter().enumerate() {
        let self_wasm_idx = (i as u32) + 1;
        // Dry run: discover extra locals by emitting into a throwaway
        // fn. Cheaper than threading a separate pre-pass.
        let mut probe = Function::new([]);
        let extra_locals_dry = emit_fn_body(&mut probe, fd, &fn_map, self_wasm_idx, &registry)?;

        let local_groups: Vec<(u32, ValType)> = extra_locals_dry.iter().map(|v| (1, *v)).collect();
        let mut func = Function::new(local_groups);
        let _ = emit_fn_body(&mut func, fd, &fn_map, self_wasm_idx, &registry)?;
        codes.function(&func);
    }

    module.section(&codes);

    let bytes = module.finish();
    if let Err(e) = validate(&bytes) {
        // Dump invalid bytes for `wasm-tools print` inspection.
        let _ = std::fs::write("/tmp/aver_wasm_gc_invalid.wasm", &bytes);
        return Err(e);
    }
    Ok(bytes)
}

fn emit_user_types(
    types: &mut TypeSection,
    items: &[TopLevel],
    registry: &TypeRegistry,
) -> Result<(), WasmGcError> {
    for item in items {
        match item {
            TopLevel::TypeDef(TypeDef::Product {
                name: _, fields, ..
            }) => {
                let st = record_struct_type(fields, registry)?;
                types.ty().struct_(st.fields.iter().copied());
            }
            TopLevel::TypeDef(TypeDef::Sum { variants, .. }) => {
                // Each variant constructor → its own struct type.
                // Phase 3a: no shared subtype hierarchy; each variant
                // stands alone, parent type is encoded as `(ref null eq)`
                // in user-facing slots.
                for v in variants {
                    let mut fields = Vec::new();
                    for ty in &v.fields {
                        let val_ty = super::types::aver_to_wasm(ty, Some(registry))?.ok_or(
                            WasmGcError::Validation(format!(
                                "variant `{}` field of type {ty} has no wasm representation",
                                v.name
                            )),
                        )?;
                        fields.push(wasm_encoder::FieldType {
                            element_type: wasm_encoder::StorageType::Val(val_ty),
                            mutable: false,
                        });
                    }
                    types.ty().struct_(fields);
                }
            }
            _ => {}
        }
    }
    Ok(())
}

/// Validate emitted bytes with `wasmparser` configured for the wasm-gc
/// + tail-call feature set we target.
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
