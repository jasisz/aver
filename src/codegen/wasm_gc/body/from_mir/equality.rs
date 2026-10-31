//! The three intrinsics of the structural equality functions
//! (`codegen::wasm_gc::eq_fns`): `__eq_same_ref`, `__eq_scope` and
//! `__eq_memo`.

use wasm_encoder::{BlockType, HeapType, RefType};

use super::*;
use crate::codegen::wasm_gc::eq_fns::EQ_BUDGET;
use crate::ir::hir::BuiltinIntrinsic;

fn memo(ctx: &EmitCtx<'_>) -> Result<super::super::EqMemo, WasmGcError> {
    ctx.fn_map.eq_memo.ok_or_else(|| {
        WasmGcError::Validation(
            "a structural equality function walks, but no table of remembered pairs".into(),
        )
    })
}

/// Pushes the key a pair of nodes is filed under: `tag` mixed with every
/// key field a step reads at once, as `body::node_table::emit_node_key`
/// mixes them. The table's lookup and insert spread it.
fn emit_key(
    func: &mut Function,
    tag: &Spanned<MirExpr>,
    keys: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<()>, WasmGcError> {
    let MirExpr::Literal(lit) = &tag.node else {
        return Err(WasmGcError::Validation(
            "__eq_memo takes its tag as an Int literal".into(),
        ));
    };
    let Literal::Int(tag) = lit.node else {
        return Err(WasmGcError::Validation(
            "__eq_memo takes its tag as an Int literal".into(),
        ));
    };
    func.instruction(&Instruction::I32Const(tag as i32));
    let wide = [
        Instruction::I64Const(0x9e37_79b9_7f4a_7c15_u64 as i64),
        Instruction::I64Mul,
        Instruction::I64Const(32),
        Instruction::I64ShrU,
        Instruction::I32WrapI64,
    ];
    for key in keys {
        let ty = aver_to_wasm(&aver_type_str_of(key), Some(ctx.registry))?;
        if emit_mir_expr(func, key, slots, ctx)?.is_none() {
            return Ok(None);
        }
        match ty {
            Some(ValType::I32) => {}
            Some(ValType::I64) => {
                for op in &wide {
                    func.instruction(op);
                }
            }
            Some(ValType::F64) => {
                func.instruction(&Instruction::I64ReinterpretF64);
                for op in &wide {
                    func.instruction(op);
                }
            }
            Some(ValType::Ref(RefType {
                heap_type: HeapType::Concrete(idx),
                ..
            })) if Some(idx) == ctx.registry.aint_struct_idx => {
                func.instruction(&Instruction::StructGet {
                    struct_type_index: idx,
                    field_index: 0,
                });
                for op in &wide {
                    func.instruction(op);
                }
            }
            Some(ValType::Ref(RefType {
                heap_type: HeapType::Concrete(idx),
                ..
            })) if Some(idx) == ctx.registry.string_array_type_idx => {
                func.instruction(&Instruction::ArrayLen);
            }
            _ => {
                return Err(WasmGcError::Validation(format!(
                    "__eq_memo key field of type `{}` has no key",
                    aver_type_str_of(key)
                )));
            }
        }
        func.instruction(&Instruction::I32Xor);
        func.instruction(&Instruction::I32Const(0x0100_0193));
        func.instruction(&Instruction::I32Mul);
    }
    Ok(Some(()))
}

/// Emits one of the three; `Ok(None)` when an operand has no lowering.
pub(super) fn emit_eq_intrinsic(
    func: &mut Function,
    intr: BuiltinIntrinsic,
    args: &[Spanned<MirExpr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<Option<bool>, WasmGcError> {
    let emit = |func: &mut Function, e: &Spanned<MirExpr>| -> Result<Option<()>, WasmGcError> {
        Ok(emit_mir_expr(func, e, slots, ctx)?.map(|_| ()))
    };
    match (intr, args) {
        // `ref.eq`: the same object.
        (BuiltinIntrinsic::EqSameRef, [a, b]) => {
            if emit(func, a)?.is_none() || emit(func, b)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::RefEq);
        }
        // A scope counts its own nodes from zero, as a comparison of its
        // own; the scope it is in counts on from where it was when this one
        // ends. The outermost one (none counted outside it) forgets the
        // remembered pairs when it ends, once it went past the budget.
        (BuiltinIntrinsic::EqScope, [body]) => {
            let memo = memo(ctx)?;
            let saved = slots.fresh_local(ValType::I32);
            func.instruction(&Instruction::GlobalGet(memo.steps));
            func.instruction(&Instruction::LocalSet(saved));
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::GlobalSet(memo.steps));
            if emit(func, body)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::LocalGet(saved));
            func.instruction(&Instruction::I32Eqz);
            func.instruction(&Instruction::GlobalGet(memo.seen));
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32Ne);
            func.instruction(&Instruction::I32And);
            func.instruction(&Instruction::If(BlockType::Empty));
            func.instruction(&Instruction::Call(memo.drop_all));
            func.instruction(&Instruction::End);
            func.instruction(&Instruction::LocalGet(saved));
            func.instruction(&Instruction::GlobalSet(memo.steps));
        }
        // One node more for the scope to count.
        // One node more for the scope to count.
        (BuiltinIntrinsic::EqCount, [body]) => {
            let memo = memo(ctx)?;
            func.instruction(&Instruction::GlobalGet(memo.steps));
            func.instruction(&Instruction::I32Const(1));
            func.instruction(&Instruction::I32Add);
            func.instruction(&Instruction::GlobalSet(memo.steps));
            if emit(func, body)?.is_none() {
                return Ok(None);
            }
        }
        // Whether the scope is past its budget.
        (BuiltinIntrinsic::EqPast, []) => {
            let memo = memo(ctx)?;
            func.instruction(&Instruction::GlobalGet(memo.steps));
            func.instruction(&Instruction::I32Const(EQ_BUDGET));
            func.instruction(&Instruction::I32GtU);
        }
        // A pair found among the remembered ones is equal; one found equal
        // is remembered.
        (BuiltinIntrinsic::EqMemo, [a, b, tag, rest @ ..]) if !rest.is_empty() => {
            let memo = memo(ctx)?;
            let (body, keys) = rest.split_last().expect("checked non-empty");
            let pair = |func: &mut Function| -> Result<Option<()>, WasmGcError> {
                if emit(func, a)?.is_none() || emit(func, b)?.is_none() {
                    return Ok(None);
                }
                emit_key(func, tag, keys, slots, ctx)
            };
            func.instruction(&Instruction::Block(BlockType::Result(ValType::I32)));
            if pair(func)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::Call(memo.lookup));
            func.instruction(&Instruction::If(BlockType::Empty));
            func.instruction(&Instruction::I32Const(1));
            func.instruction(&Instruction::Br(1));
            func.instruction(&Instruction::End);
            if emit(func, body)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
            if pair(func)?.is_none() {
                return Ok(None);
            }
            func.instruction(&Instruction::Call(memo.insert));
            func.instruction(&Instruction::I32Const(1));
            func.instruction(&Instruction::Else);
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::End);
            func.instruction(&Instruction::End);
        }
        _ => {
            return Err(WasmGcError::Validation(format!(
                "{} with {} arguments",
                intr.name(),
                args.len()
            )));
        }
    }
    Ok(Some(true))
}
