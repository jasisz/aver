//! Top-level wasm module assembly.
//!
//! Phase 1: hello-world. Emit a wasm-gc module for a program whose only
//! shape is `fn main() -> Int <literal>`. The module exports `_start`
//! and stores the literal value in a wasi-style exit code path so a
//! shell-out wasmtime invocation observes the value.
//!
//! Real lowering (compound types, match, tail calls) lands phase by
//! phase. Each phase keeps the module valid against `wasmparser` with
//! GC + tail-call features enabled.

use crate::ast::{Expr, FnBody, Literal, Stmt, TopLevel};

use super::WasmGcError;

/// Phase-1 ceiling — what `emit_module` accepts. Anything beyond this
/// shape returns `Unimplemented`. Each later phase relaxes the predicate
/// and adds its own emitter path.
struct Phase1Program<'a> {
    main_int_literal: i64,
    _items: &'a [TopLevel],
}

fn match_phase1(items: &[TopLevel]) -> Option<Phase1Program<'_>> {
    let main = items.iter().find_map(|it| match it {
        TopLevel::FnDef(fd) if fd.name == "main" => Some(fd),
        _ => None,
    })?;

    if !main.params.is_empty() {
        return None;
    }
    if main.return_type != "Int" {
        return None;
    }

    let FnBody::Block(stmts) = main.body.as_ref();
    if stmts.len() != 1 {
        return None;
    }
    let expr = match &stmts[0] {
        Stmt::Expr(e) => &e.node,
        _ => return None,
    };
    let n = match expr {
        Expr::Literal(Literal::Int(i)) => *i,
        _ => return None,
    };
    Some(Phase1Program {
        main_int_literal: n,
        _items: items,
    })
}

/// Build the wasm bytes. Validates the result with `wasmparser` configured
/// for GC + tail-call before returning so callers can't see an invalid
/// module from this entry point.
pub(super) fn emit_module(items: &[TopLevel]) -> Result<Vec<u8>, WasmGcError> {
    let Some(program) = match_phase1(items) else {
        return Err(WasmGcError::Unimplemented(
            "phase 1 only handles `fn main() -> Int <int_literal>`",
        ));
    };

    let bytes = emit_phase1(program.main_int_literal);
    validate(&bytes)?;
    Ok(bytes)
}

/// Emit a minimal wasm module:
///
/// - `(func $main (result i64) i64.const N)`
/// - `(func $_start (call $main) (drop))` — wasi-style entry, the int
///   itself isn't exported anywhere yet (phase 1 is "does it run").
/// - `(export "_start" (func $_start))`
/// - `(export "main" (func $main))`
///
/// Phase 1 doesn't actually need GC or tail-call features yet — the
/// integer literal is plain `i64.const`. We still validate against the
/// GC + tail-call feature set so the validation harness is exercised
/// from day one.
fn emit_phase1(value: i64) -> Vec<u8> {
    use wasm_encoder::{
        CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
        TypeSection, ValType,
    };

    let mut module = Module::new();

    // type section: () -> () for _start, () -> i64 for main
    let mut types = TypeSection::new();
    types.ty().function([], []); // _start
    types.ty().function([], [ValType::I64]); // main
    module.section(&types);

    // function section: two fns, types 0 and 1
    let mut funcs = FunctionSection::new();
    funcs.function(0); // _start
    funcs.function(1); // main
    module.section(&funcs);

    // export section
    let mut exports = ExportSection::new();
    exports.export("_start", ExportKind::Func, 0);
    exports.export("main", ExportKind::Func, 1);
    module.section(&exports);

    // code section
    let mut codes = CodeSection::new();

    // _start: call main, drop the i64 result
    let mut start = Function::new([]);
    start.instruction(&Instruction::Call(1)); // call $main
    start.instruction(&Instruction::Drop);
    start.instruction(&Instruction::End);
    codes.function(&start);

    // main: i64.const value, end
    let mut main = Function::new([]);
    main.instruction(&Instruction::I64Const(value));
    main.instruction(&Instruction::End);
    codes.function(&main);

    module.section(&codes);
    module.finish()
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
