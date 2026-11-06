//! Reuse a wasm frame for a function's own tail calls.
//!
//! MIR decides which functions run their self tail calls as a loop
//! (`MirFnRepr::self_tail_loop`, `ir/mir/self_tail.rs`); this lowers the
//! marked ones. Once the wasm calling convention is final (including twins),
//! a `return_call` to this same function assigns the incoming arguments and
//! branches to the loop. Other calls retain
//! their calling convention. Arguments are evaluated before any parameter
//! is overwritten, and every defaultable local is reset on loop entry:
//! cached boxes and Big flags belong to the previous invocation. The
//! exceptions are the carried locals, which hold part of a parameter's value
//! from one iteration to the next: the buffer of a `String` parameter the
//! loop grows by appending (`body/from_mir/str_builder.rs`).
//!
//! Outside a `--certify` build a twin's loop runs only while every `Int`
//! param it takes unpacked is a machine word ([`lower_split`]); a call that
//! brings a packed one runs the body once outside the loop.
//!
//! Certification builds run it too: the certificate wall lowers a plan whose
//! body tail-calls its own function to exactly this loop
//! (`GrammarLower.bodyBytes`) and proves that the loop runs as the body does
//! (`InterpreterSequencing.wEnter_loop`).

use wasm_encoder::reencode::{Reencode, RoundtripReencoder};
use wasm_encoder::{AbstractHeapType, BlockType, Function, HeapType, Instruction as I, ValType};

use super::WasmGcError;

#[cfg(all(test, feature = "wasm"))]
mod tests;

pub(super) fn lower(
    body: Function,
    self_index: u32,
    parameter_count: u32,
    carried: &[u32],
) -> Result<Function, WasmGcError> {
    lower_split(body, self_index, parameter_count, carried, &[])
}

/// As [`lower`], for a twin whose `packed` params are the refs of the `Int`
/// params it takes unpacked (`body/from_mir/twin.rs`): null unless the value
/// came packed. The loop runs only while every one of them is null, so on
/// entry to the loop each is the null constant and the body's reads of a
/// packed or Big param fold away. A call with any of them set runs the body
/// once outside the loop, and so does a self tail call that sets one: it
/// stays a `return_call`.
pub(super) fn lower_split(
    body: Function,
    self_index: u32,
    parameter_count: u32,
    carried: &[u32],
    packed: &[u32],
) -> Result<Function, WasmGcError> {
    // A carried local is not passed by a real call: keep the plain loop.
    let packed = if carried.is_empty() { packed } else { &[] };
    let raw = body.clone().into_raw_body();
    let parsed = wasmparser::FunctionBody::new(wasmparser::BinaryReader::new(&raw, 0));
    let invalid =
        |error: &dyn std::fmt::Display| WasmGcError::Validation(format!("self tail loop: {error}"));
    let mut reader = parsed.get_operators_reader().map_err(|e| invalid(&e))?;
    let mut reencoder = RoundtripReencoder;
    let mut instructions = Vec::new();
    while !reader.eof() {
        let op = reader.read().map_err(|e| invalid(&e))?;
        instructions.push(reencoder.instruction(op).map_err(|e| invalid(&e))?);
    }
    // Aver has no exception or continuation instructions. Keep an unfamiliar
    // control-flow construct untouched rather than guessing its label scopes.
    if instructions.iter().any(|i| {
        matches!(
            i,
            I::TryTable(..)
                | I::Try(..)
                | I::Resume { .. }
                | I::ResumeThrow { .. }
                | I::ResumeThrowRef { .. }
        )
    }) {
        // A carried local holds part of a parameter's value, which the
        // self tail call then does not pass.
        if !carried.is_empty() {
            return Err(invalid(
                &"a function with carried locals must run as a loop",
            ));
        }
        return Ok(body);
    }

    let locals = parsed
        .get_locals_reader()
        .map_err(|e| invalid(&e))?
        .into_iter()
        .map(|group| {
            let (count, ty) = group.map_err(|e| invalid(&e))?;
            Ok((count, reencoder.val_type(ty).map_err(|e| invalid(&e))?))
        })
        .collect::<Result<Vec<_>, WasmGcError>>()?;
    let mut out = Function::new(locals.iter().copied());
    let any_packed = |out: &mut Function| {
        for (k, p) in packed.iter().enumerate() {
            out.instruction(&I::LocalGet(*p));
            out.instruction(&I::RefIsNull);
            out.instruction(&I::I32Eqz);
            if k > 0 {
                out.instruction(&I::I32Or);
            }
        }
    };
    if !packed.is_empty() {
        // The body once, with its self tail calls left as calls.
        any_packed(&mut out);
        out.instruction(&I::If(BlockType::Empty));
        let mut depth = 0;
        for instruction in &instructions {
            let mut instruction = instruction.clone();
            match instruction {
                I::Block(_) | I::Loop(_) | I::If(_) => depth += 1,
                I::End if depth == 0 => {
                    out.instruction(&I::Return);
                    out.instruction(&I::End);
                    continue;
                }
                I::End => depth -= 1,
                _ => {}
            }
            shift_function_label(&mut instruction, depth);
            out.instruction(&instruction);
        }
    }
    out.instruction(&I::Loop(BlockType::Empty));
    let mut slot = parameter_count;
    for (count, ty) in locals {
        let zero = match ty {
            ValType::I32 => Some(I::I32Const(0)),
            ValType::I64 => Some(I::I64Const(0)),
            ValType::F32 => Some(I::F32Const(0.0.into())),
            ValType::F64 => Some(I::F64Const(0.0.into())),
            ValType::V128 => Some(I::V128Const(0)),
            ValType::Ref(r) if r.nullable => Some(I::RefNull(r.heap_type)),
            // Validation requires a non-defaultable local to be assigned
            // before its first read, including on entry to the loop.
            ValType::Ref(_) => None,
        };
        for _ in 0..count {
            if let Some(zero) = &zero
                && !carried.contains(&slot)
            {
                out.instruction(zero);
                out.instruction(&I::LocalSet(slot));
            }
            slot += 1;
        }
    }

    let mut depth = 0;
    for mut instruction in instructions {
        match instruction {
            I::ReturnCall(index) if index == self_index => {
                for parameter in (0..parameter_count).rev() {
                    out.instruction(&I::LocalSet(parameter));
                }
                if !packed.is_empty() {
                    any_packed(&mut out);
                    out.instruction(&I::If(BlockType::Empty));
                    for parameter in 0..parameter_count {
                        out.instruction(&I::LocalGet(parameter));
                    }
                    out.instruction(&I::ReturnCall(self_index));
                    out.instruction(&I::End);
                    for p in packed {
                        out.instruction(&I::RefNull(HeapType::Abstract {
                            shared: false,
                            ty: AbstractHeapType::None,
                        }));
                        out.instruction(&I::LocalSet(*p));
                    }
                }
                out.instruction(&I::Br(depth));
                continue;
            }
            I::Block(_) | I::Loop(_) | I::If(_) => depth += 1,
            I::End if depth == 0 => {
                // A plain fallthrough returns the function's results. An
                // empty-result loop needs no new multi-value block type.
                out.instruction(&I::Return);
                out.instruction(&I::End);
                out.instruction(&I::Unreachable);
                out.instruction(&I::End);
                continue;
            }
            I::End => depth -= 1,
            _ => {}
        }
        shift_function_label(&mut instruction, depth);
        out.instruction(&instruction);
    }
    Ok(out)
}

/// Internal labels retain their depth. A branch to the implicit function
/// label crosses the new loop (or `if`) and therefore moves out once.
fn shift_function_label(instruction: &mut I<'_>, depth: u32) {
    let shift = |target: &mut u32| {
        if *target == depth {
            *target += 1;
        }
    };
    match instruction {
        I::Br(target) | I::BrIf(target) | I::BrOnNull(target) | I::BrOnNonNull(target) => {
            shift(target)
        }
        I::BrTable(targets, default) => {
            for target in targets.to_mut() {
                shift(target);
            }
            shift(default);
        }
        I::BrOnCast { relative_depth, .. }
        | I::BrOnCastFail { relative_depth, .. }
        | I::BrOnCastDescEq { relative_depth, .. }
        | I::BrOnCastDescEqFail { relative_depth, .. } => shift(relative_depth),
        _ => {}
    }
}
